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

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "MetaData.tsv"))

metaData = readr::read_tsv(file = fileInfo, locale = locale("ko", encoding = "UTF-8")) %>% 
  dplyr::filter(
    type2 == "양주뉴스"
    , ! is.na(sttCsvName)
  )

# i = 1
# i = 3
# i = 150
# i = 1000

dataL3 = tibble::tibble()

for (i in seq(1, 1000, 1)) {
  
  fileKey = paste0("제", i, "호")
  fileKey2 = paste0("식", i, "호")

  if (i > 135) {
    filePattern = paste0("*", fileKey2, "*", ".xlsx")
  } else {
    filePattern = paste0("*", fileKey, "*", ".xlsx")
  }
  
  fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern)) %>% 
    dplyr::last()
  
  if (is.na(fileInfo) || length(fileInfo) < 1) { next }
  
  # *******************************************************
  # 네이버 클로바 STT 파일 읽기
  # *******************************************************
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
  contentAll = paste(dataL1$content, collapse = " ")
  
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
    dplyr::arrange(desc(freq)) %>% 
    dplyr::slice(1:10)
  
  keywordTop10 = stringr::str_c(keywordData$token, collapse = '+')
  keywordTop5= stringr::str_c(keywordData$token[1:5], collapse = '+')
  keywordTop2= stringr::str_c(keywordData$token[1:2], collapse = '+')
  
  # *******************************************************
  # 메타 파일 읽기
  # *******************************************************
  metaDataL1 = metaData %>% 
    dplyr::filter(
      stringr::str_detect(sttCsvName, regex(fileKey))
    )

  if (nrow(metaDataL1) < 1) { next }
  
  newFileName = stringr::str_c(
    metaDataL1$year
    , metaDataL1$id
    , metaDataL1$type2
    , metaDataL1$title
    , keywordTop2
    , sep = "_" 
    ) %>% 
    stringr::str_replace_all(pattern = "-", replacement = " ") %>% 
    stringr::str_replace_all(pattern = "\\+", replacement = ",")

  tmpData = tibble::tibble(
    fileName = newFileName
    , keywordTop5 = keywordTop5
    , keywordTop10 = keywordTop10
    , content = paste(dataL1$content, collapse = "\r\n")
  )
  
  dataL3 = dplyr::bind_rows(dataL3, tmpData)
    
  saveXlsxFile = sprintf("%s/%s/%s.xlsx", globalVar$outPath, serviceName, newFileName)
  
  wb = openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(wb, "Sheet1", dataL1, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
  openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)
}


saveXlsxFile = sprintf("%s/%s/%s.xlsx", globalVar$outPath, serviceName, "MetaData")

wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Sheet1")
openxlsx::writeData(wb, "Sheet1", dataL3, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)


dataL4 = dataL3 %>% 
  dplyr::select(-c("keywordTop5", "keywordTop10"))

saveCsvFile = sprintf("%s/%s/%s.csv", globalVar$outPath, serviceName, "공공데이터포털_STT")
readr::write_csv(dataL4, file = saveCsvFile)


saveXlsxFile = sprintf("%s/%s/%s.xlsx", globalVar$outPath, serviceName, "공공데이터포털_STT")

wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Sheet1")
openxlsx::writeData(wb, "Sheet1", dataL4, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)
