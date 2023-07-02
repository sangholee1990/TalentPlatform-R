#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

#================================================
# 요구사항
#================================================
# R을 이용한 디시인사이드 (dcinside)에서 만화가지망생 갤러리 웹 크롤링

# 안녕하세요? https://gall.dcinside.com/mgallery/board/lists/?id=cartoonist <디시인사이드갤러리 만화가지망생갤러리>의 웹 크롤러 견적 문의합니다.
# R로 제작해 주시면 좋겠어요. 데이터(글 제목, 본문, 게시일, 작성자 포함)는 CSV 파일로 다운로드 받을 수 있게 해주시면 될 것 같고요.
# 2년 전에 제가 만들어 둔게 있는데 지금 쓰려니 오류가 너무 나는데, 오류 수정할 시간이 없어 요청드려봅니다.

# 댓글 대댓글 제외한 데이터(제목, 본문, 게시일, 작성자 등) 크롤러는 견적 얼마인가요?

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0407"

if (Sys.info()["sysname"] == "Windows") {
  contextPath = ifelse(env == "local", ".", "E:/04. TalentPlatform/Github/TalentPlatform-R")
} else {
  contextPath = ifelse(env == "local", ".", "/SYSTEMS/PROG/R/PyCharm")
}

if (env == "local") {
  globalVar = list(
    "inpPath" = contextPath
    , "figPath" = contextPath
    , "outPath" = contextPath
    , "tmpPath" = contextPath
    , "logPath" = contextPath
  )
} else {
  # source(here::here(file.path(contextPath, "src"), "InitConfig.R"), encoding = "UTF-8")
  source(file.path(contextPath, "src", "InitConfig.R"))
}

#================================================
# 비즈니스 로직 수행
#================================================
# 라이브러리 읽기
library(rvest)
library(stringr)
library(tidyverse)

# 함수 정의
getXpathText = function(urlDtlInfo) {
  
  urlDtlInfo %>% 
    rvest::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id="container"]/section/article[2]/div[1]/div/div[1]/div[1]/div[2]/p') %>%
    rvest::html_text2() %>%
    paste(collapse = "")
}

# 페이지 정보
pageList = 1:50

# pageInfo = 1
dataL2 = tibble::tibble()
for (pageInfo in pageList) {

  cat(sprintf("[CHECK] progress : %.2f %s", (pageInfo / length(pageList)) * 100.0, "%"), "\n")

  # 만화가지망생 갤러리
  urlInfo = sprintf("https://gall.dcinside.com/mgallery/board/lists/?id=cartoonist&page=%s", pageInfo)

  # **********************************************************************************
  # 기본 정보
  # **********************************************************************************
  data = urlInfo %>%
    rvest::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id="container"]/section[1]/article[2]/div[2]/table') %>%
    rvest::html_table() %>%
    as.data.frame() %>%
    as.tibble()

  # 기본 정보 최소 개수
  if (nrow(data) < 30) { next }

  # **********************************************************************************
  # 상세 정보
  # *********************************************************************************
  urlDtlList = urlInfo %>%
    rvest::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id="container"]/section[1]/article[2]/div[2]/table/tbody/tr[*]/td[3]/a') %>%
    rvest::html_attr("href") %>%
    as.tibble() %>%
    dplyr::filter(
      stringr::str_detect(value, regex("id=cartoonist"))
      , ! stringr::str_detect(value, regex("https"))
    ) %>%
    dplyr::mutate(
      value = ifelse(
        stringr::str_detect(value, regex("https"))
        , value
        , sprintf("https://gall.dcinside.com%s", value)
        )
      , tmp = stringr::str_match(value, "no=*[0-9]*")
      , num = stringr::str_replace_all(tmp, "no=", replacement = "") %>% as.numeric()
    )
  
  # j = 1
  urlDtlData = tibble::tibble()
  for (j in 1:nrow(urlDtlList)) {
    try(
      expr = {
        urlDtlInfo = urlDtlList[j,]

        tmpData = tibble::tibble(
          "번호" = urlDtlInfo$num
          , "본문" = getXpathText(urlDtlInfo$value)
          , "URL" = urlDtlInfo$value
        )

        urlDtlData = dplyr::bind_rows(urlDtlData, tmpData)
      }
      , silent = TRUE
    )
    
    # 시간 지연 설정
    # Sys.sleep(0.5)
    Sys.sleep(1.0)
  }
  
  # 상세 정보 최소 개수
  if (nrow(urlDtlData) < 1) { next }
  
  dataL1 = data %>%
    dplyr::left_join(urlDtlData, by = c("번호" = "번호"))

  # 시간 지연 설정
  # Sys.sleep(0.5)
  Sys.sleep(1.0)

  dataL2 = dplyr::bind_rows(dataL2, dataL1)
}

# 자료 저장
minPage = min(pageList, na.rm = TRUE)
maxPage = max(pageList, na.rm = TRUE)
saveFile = sprintf("%s/%s/%s_%s-%s.csv", globalVar$outPath, serviceName, "dcinside-cartoonist", minPage, maxPage)
dir.create(fs::path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(x = dataL2, file = saveFile)
cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")