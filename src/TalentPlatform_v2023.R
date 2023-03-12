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

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "TEST"

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

# 페이지 정보
pageList = 1:50

# pageInfo = 1
dataL1 = tibble::tibble()
for (pageInfo in pageList) {

  cat(sprintf("[CHECK] progress : %.2f %s", (pageInfo / length(pageList)) * 100.0, "%"), "\n")

  # 만화가지망생 갤러리
  urlInfo = sprintf("https://gall.dcinside.com/mgallery/board/lists/?id=cartoonist&page=%s", pageInfo)

  data = urlInfo %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id="container"]/section[1]/article[2]/div[2]/table') %>%
    rvest::html_table() %>%
    as.data.frame() %>%
    as.tibble()

  isFlag = urlInfo %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id="container"]/section[1]/article[2]/div[2]/table/tbody/tr[*]/td[3]/a') %>%
    rvest::html_attr("href") %>%
    as.tibble() %>%
    dplyr::filter(
      stringr::str_detect(value, regex("https://gall.dcinside.com/mgallery/board"))
    )

    dpl
    stringr::str_detect("https://gall.dcinside.com/mgallery/board")

    urlDtlInfo = "https://gall.dcinside.com/mgallery/board/view/?id=cartoonist&no=149590&t=cv&page=1"

# //*[@id="comment_li_862509"]
urlDtlInfo %>%
    xml2::read_html() %>%
    rvest::html_nodes('.cmt_nickbox') %>%
    # #comment_li_862509 > div > div.cmt_nickbox

    rvest::html_attr("data-nick")
    # rvest::html_attr("href") %>%
    # //*[@id="comment_li_141296"]/div/div[1]/span
    #

    # https://gall.dcinside.com/mgallery/board/view/?id=cartoonist&no=26558&t=cv&page=1
    # dplyr::filter(

    # )
    stringr::str_detect("http")
    # stringr::str_match("http")
    # rvest::html_table() %>%
    # as.data.frame() %>%
    as.tibble()
  # //*[@id="container"]/section[1]/article[2]/div[2]/table
  # //*[@id="container"]/section[1]/article[2]/div[2]/table/tbody/tr[1]/td[3]/a

  #

  # 시간 지연 설정
  Sys.sleep(0.5)

  # 최소 콘텐츠 수
  if (nrow(data) < 30) { next }

  dataL1 = dplyr::bind_rows(dataL1, data)
}

# 결과파일 저장
# write.csv(data_full,"./esfp_txt.csv")
