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
# R을 이용한 데일리 패션 뉴스 크롤링 및 키워드 명사 추출

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0522"

if (Sys.info()[["sysname"]] == "Windows") {
  contextPath = ifelse(env == "local", ".", "C:/SYSTEMS/PROG/R/TalentPlatform-R")
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

# ================================================
# 비즈니스 로직 수행
# ================================================
# 라이브러리 읽기
library(openxlsx)
library(rvest)
library(stringr)
library(tidyverse)
library(RcppMeCab)
library(RmecabKo)

# 명사 추출을 위한 메타 정보
RmecabKo::install_mecab("c:/mecab")

# URL 기본 정보
urlRoot = "https://dafanew.com"

# 페이지 정보
pageList = 1:2

# ================================================
# 데일리 패션 뉴스 크롤링
# ================================================
data = tibble::tibble()
for (pageInfo in pageList) {
  
  cat(sprintf("[CHECK] progress : %.2f %s", (pageInfo / length(pageList)) * 100.0, "%"), "\n")
  
  urlInfo = sprintf("%s/news?page=%s", urlRoot, pageInfo) 
  
  urlDtlData = urlInfo %>%
    rvest::read_html() %>% 
    rvest::html_elements(".post_link_wrap") %>% 
    rvest::html_attr("href") %>% 
    as.tibble() %>% 
    dplyr::mutate(
      urlDtlInfo = sprintf("%s%s", urlRoot, value)
    )
  
  # i = 1
  for (i in 1:nrow(urlDtlData)) {
    urlDtlInfo = urlDtlData[i, ]$urlDtlInfo
    
    # 제목
    title = urlDtlInfo %>%
      rvest::read_html() %>% 
      rvest::html_elements(".view_tit") %>% 
      rvest::html_text2()
    
    date = gsub(".*(\\d{4}년 \\d{1,2}월 \\d{1,2}일).*", "\\1", title) %>% 
      as.Date(format = "%Y년 %m월 %d일")
    
    # 뉴스 내용
    cont = urlDtlInfo %>%
      rvest::read_html() %>% 
      rvest::html_elements(".board_txt_area") %>% 
      rvest::html_text2() %>% 
      gsub("\\s+", " ", .) %>%
      gsub("\"", "", .) %>%
      gsub("출처: http[^[:space:]]*", "", .) %>% 
      gsub("[^[:alnum:][:space:]]", "", .)
    
    
    tmpData = tibble("page" = pageInfo, "date" = date, "title" = title, "cont" = cont)
    data = dplyr::bind_rows(data, tmpData)
  }
}


saveFile = sprintf("%s/%s/%s.csv", globalVar$outPath, serviceName, "데일리 패션 뉴스-크롤링")
dir.create(dirname(saveFile), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(data, file = saveFile)
cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")

# ================================================
# 키워드 명사 추출
# ================================================
dataL1 = data %>% 
  dplyr::mutate(
    sDate = format(date, "%Y%m%d")
  ) %>% 
  dplyr::filter(
    sDate %in% c("20231201", "20231204", "20231206", "20231208")
  )


contAll = paste(dataL1$cont, collapse = " ")

dataL2 = RcppMeCab::pos(utf8::as_utf8(contAll), format = "data.frame") %>%
  dplyr::filter(pos == "NNG") %>%
  dplyr::select(token)

keywordData = dataL2 %>%
  dplyr::group_by(token) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::mutate(len = stringr::str_length(token)) %>% 
  dplyr::filter(
    freq >= 2
    , len >= 2
  ) %>%
  dplyr::arrange(desc(freq)) %>% 
  dplyr::slice(1:50)

saveFile = sprintf("%s/%s/%s.csv", globalVar$outPath, serviceName, "데일리 패션 뉴스-TOP50 키워드")
dir.create(dirname(saveFile), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(keywordData, file = saveFile)
cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")
