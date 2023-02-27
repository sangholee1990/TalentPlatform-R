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
# R을 이용한 판매량 데이터 예측 모형

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0282"

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
library(tidyverse)
library(readr)
library(httr)
library(rvest)
library(jsonlite)
library(RCurl)
library(dplyr)
library(data.table)
library(Rcpp)
library(philentropy)
library(h2o)
library(readxl)
library(stringr)

fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "발송마스타*.xls"))

dataL2 = tibble::tibble()
for (fileInfo in fileList) {
  cat(sprintf("[CHECK] fileInfo : %s", fileInfo), "\n")
  
  data = readxl::read_excel(fileInfo, sheet = 1, col_names = FALSE) %>% 
    magrittr::set_colnames(c("sDate", "name")) %>% 
    dplyr::select(c("sDate", "name"))
  
  tmp = stringr::str_match(data$name, "[:graph:]*\\-[0-9]{3,3}\\-[0-9]개")
  tmp2 = stringr::str_split_fixed(tmp, "-", 3) %>% as.tibble()
  
  type = stringr::str_replace(tmp2$V1, "\\◎|\\★|ㅁ.", replacement = "")
  size = stringr::str_replace(tmp2$V2, "\\-", replacement = "") %>% as.numeric()
  cnt = stringr::str_replace(tmp2$V3, "개", replacement = "") %>% as.numeric()

  dataL1 = dplyr::bind_cols(data, data.frame(type, cnt, size)) %>% 
    dplyr::filter(
      ! is.na(type), ! is.na(cnt), ! is.na(size)
    ) %>%
    dplyr::mutate(
      dtDate = readr::parse_date(as.character(sDate), "%Y%m%d")
      , dtYear = lubridate::year(dtDate)
      , dtMonth = lubridate::month(dtDate)
    ) %>% 
    readr::type_convert()

  dataL2 = dplyr::bind_rows(dataL2, dataL1)
}


dataL3 = dataL2 %>% 
  dplyr::mutate(
    dtDate = lubridate::make_date(dtYear, dtMonth, 1)
    , dtXran = lubridate::decimal_date(dtDate)
  )


# 1차 가공자료
chkData = dataL3 %>% 
  dplyr::group_by(type) %>%
  dplyr::summarise(
    cnt = sum(cnt, na.rm = TRUE)
  ) %>% 
  dplyr::arrange(desc(cnt))

summary(chkData)

chkDataL1 = chkData %>% 
  dplyr::filter(cnt > mean(cnt, na.rm = TRUE))

# 2차 가공자료
dataL4 = dataL3 %>% 
  dplyr::filter(type %in% chkDataL1$type) %>% 
  dplyr::group_by(dtDate, dtYear, dtMonth, dtXran, type, size) %>%
  dplyr::summarise(
    cnt = sum(cnt, na.rm = TRUE)
  ) %>% 
  dplyr::ungroup()


# 훈련 및 테스트 데이터 셋 설정
minDate = min(dataL4$dtDate, na.rm = TRUE)
maxDate = as.Date("2023-12-31")

testData = tibble(dtDate = seq(minDate, maxDate, "1 month")) %>% 
  dplyr::mutate(
    dtYear = lubridate::year(dtDate)
    , dtMonth = lubridate::month(dtDate)
    , dtXran = lubridate::decimal_date(dtDate)
  ) 


typeList = dataL4$type %>% unique()
sizeList = dataL4$size %>% unique()
for (typeInfo in typeList) {
  for (sizeInfo in sizeList) {
  
    trainData = dataL4 %>% 
      dplyr::filter(
        type == typeInfo
        , size == sizeInfo
        )
    
    # if (nrow(trainData) < 5) { next }
    if (nrow(trainData) < 10) { next }
    
    cat(sprintf("[CHECK] %s-%s : %s", typeInfo, sizeInfo, nrow(trainData)), "\n")
    
    
    # ******************************************************************************
    # 다중선형회귀모형
    # ******************************************************************************
    lmModel = lm(cnt ~ dtXran + dtYear + dtMonth, data = trainData)
    summary(lmModel)

    testData$prdLM = predict(lmModel, newdata = testData)

    # ******************************************************************************
    # 머신러닝 및 딥러닝 모형
    # ******************************************************************************
    # 초기화
    h2o::h2o.init()

    # 모델 학습
    amlModel = h2o::h2o.automl(
      x = c("dtXran", "dtYear", "dtMonth")
      , y = c("cnt")
      , training_frame = h2o::as.h2o(trainData)
      , validation_frame = h2o::as.h2o(trainData)
      , nfolds = 5
      , sort_metric = "RMSE"
      , stopping_metric = "RMSE"
      , seed = 1
      , max_models = 10
      , max_runtime_secs = 60
    )

    testData$prdDL = as.data.frame(h2o::h2o.predict(object = amlModel, newdata = as.h2o(testData)))$predict

    
    # ******************************************************************************
    # 자료 병합
    # ******************************************************************************
    testDataL1 = testData %>% 
      dplyr::left_join(trainData, by = c("dtDate" = "dtDate", "dtYear" = "dtYear", "dtMonth" = "dtMonth", "dtXran" = "dtXran"))
    
    
    # ******************************************************************************
    # 에측 결과 저장
    # ******************************************************************************
    saveXlsxFile = sprintf("%s/%s/AI-ShoesRate-%s-%s.xlsx", globalVar$outPath, serviceName, typeInfo, sizeInfo)
    dir.create(path_dir(saveXlsxFile), showWarnings = FALSE, recursive = TRUE)
    
    wb = openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Sheet1")
    openxlsx::writeData(wb, "Sheet1", testDataL1, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
    openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)
    
    cat(sprintf("[CHECK] saveXlsxFile : %s", saveXlsxFile), "\n")
  }
}


#================================================
# 2023.02.27 이전
#================================================
# 파일 읽기
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "판매량_예측.xlsx"))
# data = openxlsx::read.xlsx(fileInfo, sheet = 2)

# trainData = data %>% 
#   dplyr::mutate(
#     dtDate = readr::parse_date(date, "%Y-%m")
#     , dtYear = lubridate::year(dtDate)
#     , dtMonth = lubridate::month(dtDate)
#     , dtXran = lubridate::decimal_date(dtDate)
#   )
# 
# # testData = tibble(dtDate = seq(as.Date("2018-09-01"), as.Date("2023-02-01"), "1 month")) %>% 
# testData = tibble(dtDate = seq(as.Date("2018-09-01"), as.Date("2023-02-01"), "1 month")) %>% 
#   dplyr::mutate(
#     dtYear = lubridate::year(dtDate)
#     , dtMonth = lubridate::month(dtDate)
#     , dtXran = lubridate::decimal_date(dtDate)
#   ) %>% 
#   dplyr::filter(
#     dtMonth %in% c(9, 10, 11)
#   )
# 
# # ******************************************************************************
# # 다중선형회귀모형
# # ******************************************************************************
# lmModel = lm(value ~ dtXran + dtYear + dtMonth, data = trainData)
# summary(lmModel)
# 
# testData$prdLM = predict(lmModel, newdata = testData)
# 
# # ******************************************************************************
# # 머신러닝 및 딥러닝 모형
# # ******************************************************************************
# # 초기화
# # h2o::h2o.init()
# # 
# # # 모델 학습
# # amlModel = h2o::h2o.automl(
# #   x = c("dtXran", "dtYear", "dtMonth")
# #   , y = c("value")
# #   , training_frame = as.h2o(trainData)
# #   , nfolds = 10
# #   , sort_metric = "RMSE"
# #   , stopping_metric = "RMSE"
# #   , seed = 1
# #   , max_models = 10
# # )
# 
# summary(amlModel)
# 
# testData$prdDL = as.data.frame(h2o::h2o.predict(object = amlModel, newdata = as.h2o(testData)))$predict
# 
# 
# # ******************************************************************************
# # 에측 결과 저장
# # ******************************************************************************
# saveXlsxFile = sprintf("%s/%s_%s.xlsx", globalVar$outPath, serviceName, "판매량_예측결과")
# wb = openxlsx::createWorkbook()
# openxlsx::addWorksheet(wb, "예측 데이터")
# openxlsx::writeData(wb, "예측 데이터", testData, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
# openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)
