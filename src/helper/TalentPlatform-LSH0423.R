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
# R을 이용한 yes24 금주의 주요 신간 10개 도서 수집

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0423"

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
library(tidyverse)
library(rvest)
library(xml2)

# 함수 선언
getUrlText = function(url, xpath) {
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath = xpath) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}


# 웹 페이지의 URL
url = "http://www.yes24.com/24/Category/NewProduct"

# 금주의 주요 신간 추출
getUrlText(url, '//*[@id="topBooksUl_001"]/li[*]/div[2]/p[1]/a')[1:10]

# 홀수
getUrlText(url, '//*[@id="topBooksUl_001"]/li[*]/div[2]/p[1]/a')[seq(1, 10, 2)]

# 짝수
getUrlText(url, '//*[@id="topBooksUl_001"]/li[*]/div[2]/p[1]/a')[seq(0, 10, 2)]

