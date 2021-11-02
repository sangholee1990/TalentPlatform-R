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
# 

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "bdwide"
serviceName = "PRJ0002"
contextPath = ifelse(env == "local", ".", getwd())

if (env == "local") {
  globalVar = list(
    "inpPath" = contextPath
    , "figPath" = contextPath
    , "outPath" = contextPath
    , "tmpPath" = contextPath
    , "logPath" = contextPath
  )
} else {
  source(here::here(file.path(contextPath, "src"), "InitConfig.R"), encoding = "UTF-8")
}

#================================================
# 비즈니스 로직 수행
#================================================
# 라이브러리 읽기
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(lubridate)
library(MASS)
library(scales)
library(dplyr)
library(openxlsx)
library(vroom)
library(RcppMeCab)
library(RmecabKo)

# 명사 추출을 위한 메타 정보
RmecabKo::install_mecab("c:/mecab")

for (variable in vector) {
  
}

filePattern = paste0("*", "제", 1, "호", "*" ,".xlsx")
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))

# fileInfo = fileList[3]
data = openxlsx::read.xlsx(fileInfo, sheet = 1)

dataL1 = data %>%
  as.tibble() %>% 
  dplyr::filter(
    Modification == "수정결과"
    , ! is.na(Script)
    ) %>% 
  dplyr::select(-c("Index", "Modification")) %>% 
  tibble::rowid_to_column() %>% 
  magrittr::set_colnames(c("index", "speaker", "startTime", "endTime", "content"))


# *******************************************************
# 키워드 추출
# *******************************************************
contentAll = paste(dataL1$content, collapse = "\n\r")

dataL2 = RcppMeCab::pos(utf8::as_utf8(contentAll), format = "data.frame") %>%
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
  dplyr::arrange(desc(freq))


