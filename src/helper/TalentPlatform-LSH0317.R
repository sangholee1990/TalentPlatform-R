
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
# R을 이용한 지상관측소 별로 자료 병합 및 결측값 처리

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0317"
contextPath = ifelse(env == "local", ".", "E:/04. TalentPlatform/Github/TalentPlatform-R")

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
library(tidyverse)
library(lubridate)
library(openxlsx)
library(dplyr)
library(readxl)
library(plyr)

# setwd("E:/04. TalentPlatform/Github/TalentPlatform-R/resources/input/test/LSH0317")

# stnList = c("강릉", "광주", "군산")
stnList = c("강릉", "광주", "군산", "대구", "대전", "목포", "부산", "서산", "서울", "속초", "여수", "울릉도", "울산"
            , "인천", "전주", "제주", "진주", "천안", "청주", "추풍령", "통영", "포항")

# stnInfo = stnList[1]

dataL1 = tibble::tibble()
for (stnInfo in stnList) {

  cat( sprintf("[CHECK] stnInfo : %s", stnInfo), "\n" )
  
  filePattern = sprintf("%s_FDWQM ACCESS-ESM1-5 historical Each Station.csv", stnInfo)
  fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  
  data = read.csv(file = fileInfo) %>% 
    as.tibble() %>% 
    dplyr::select(probInfo, model, obs, name) %>% 
    dplyr::mutate(
      stn = stnInfo
    ) %>%
    tibble::rowid_to_column()
  
  dataL1 = dplyr::bind_rows(dataL1, data)
}


# summary(dataL1)

# nameList = dataL1$name %>% unique() %>% sort()
nameList = sprintf("OBS%s", 70:99)

# nameInfo = nameList[1]
# nameInfo = nameList[15]
# nameInfo = nameList[5]
# sprintf = stnList[1]
for (nameInfo in nameList) {
  
  # dataL2 = dataL1 %>% 
  #   dplyr::filter(
  #     name == nameInfo
  #   )
  
  # if (nrow(dataL2) < 1) next
  cat( sprintf("[CHECK] nameInfo : %s", nameInfo), "\n" )
  
  dataL4 = tibble::tibble()
  stnInfo = stnList[1]
  for (stnInfo in stnList) {
    
    colObsName = sprintf("obs%s", stnInfo)
    colModelName = sprintf("model%s", stnInfo)
    
    dataL3 = dataL1 %>% 
      dplyr::filter(
        name == nameInfo
        , stn == stnInfo
      ) %>% 
      dplyr::select(model) %>% 
      magrittr::set_colnames(c(colModelName))
    
    # if (nrow(dataL3) < 1) next
    if (nrow(dataL3) < 1) {
      dataL3 = tibble(model = NA, .rows = 10957) %>% 
        magrittr::set_colnames(c(colModelName))
    }
    
    cat( sprintf("[CHECK] stnInfo : %s", stnInfo), "\n" )
    
    if (nrow(dataL4) == 0) {
      dataL4 = dataL3
    } else {
      dataL4 = dplyr::bind_cols(dataL4, dataL3)
    }
  }
  
  saveFile = sprintf("%s/%s_%s_%s", globalVar$outPath, serviceName, nameInfo, "Weibull ACCESS-ESM1-5 model.csv")
  readr::write_csv(dataL4, file = saveFile)
}
