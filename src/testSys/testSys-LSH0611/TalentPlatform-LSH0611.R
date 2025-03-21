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
# R을 이용한 네이트 랭킹 뉴스 수집 및 엑셀 저장

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0611"

if (Sys.info()[["sysname"]] == "Windows") {
  contextPath = ifelse(env == "local", getwd(), "C:/SYSTEMS/PROG/R/TalentPlatform-R")
} else {
  contextPath = ifelse(env == "local", getwd(), "/SYSTEMS/PROG/R/PyCharm")
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
library(ggplot2)
library(dplyr)
library(fs)
library(httr)
library(rvest)
library(stringr)
library(dplyr)
library(writexl)

srtDate = "2025-03-01"
endDate = "2025-03-07"

# 날짜 벡터 생성 (2025년 3월 1일부터 3월 7일까지)
dateList = strftime(seq(as.Date(srtDate), as.Date(endDate), by = "day"), format = "%Y%m%d")

# 각 날짜별로 뉴스 기사 수집
# dateInfo = dateList[1]
dataL1 = tibble()
for (dateInfo in dateList) {
  
  cat(sprintf("[CHECK] dateInfo : %s", dateInfo), "\n")
  
  # URL 주소
  URL = sprintf("https://news.nate.com/rank/interest?sc=eco&p=day&date=%s", dateInfo)
  
  # html = httr::GET(URL) %>% 
  #   read_html()
  res = read_html(URL)
  
  # 상위 제목
  pattern = '//*[@id="newsContents"]/div/div[2]/div[*]/div/a/span[2]/h2'
  titleTopList = res %>%
    html_nodes(xpath = pattern) %>%
    html_text(trim=TRUE)
  
  # 하단 제목
  pattern = '//*[@id="postRankSubject"]/ul[*]/li[*]/a/h2'
  titleBotList = res %>% 
    html_nodes(xpath = pattern) %>%
    html_text(trim=TRUE)

  if (length(titleTopList) < 1) next
  if (length(titleBotList) < 1) next
    
  # 데이터 병합
  data = tibble(
    date = dateInfo,
    title = c(titleTopList, titleBotList)
  )
  
  dataL1 = dplyr::bind_rows(dataL1, data)
}

saveFile = sprintf("%s/%s.xlsx", globalVar$outPath, "colctData_20250301-20250307")
dir.create(fs::path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
write_xlsx(dataL1, saveFile)
cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")
