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
# R을 이용한 기업간의 이직의 영향 분석

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0432"

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
# loading packages
library(lmtest)
library(tidyverse)

# Set working directory (adjust this so it points to the directory on your machine 
# where the files are)
# setwd("E:/R")


# creating a new dataset "finaldata"
# finaldata <- read.csv(file = "emba_dataset_team project.csv")
# summary(finaldata)


##### testing the models  ###################################################################
# 파일 읽기
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "emba_dataset_team+project.csv"))

data = readr::read_csv(fileList) %>% 
  readr::type_convert()

summary(data)

# 전체 변수에 대한 다중선형 회귀모형 수행
# 독립변수 : 2019년 기준 직원 이직률 (turnover19) 제외한 전체 변수
# 종속변수 : 2019년 기준 직원 이직률 (turnover19)
lmFit = lm(turnover19 ~ ., data = data)

# 전체 변수에 대한 요약 결과
summary(lmFit)

lmFit$coefficients %>% sort(decreasing = TRUE) %>% round(4)

# 전체 변수에서 변수 선택
stepFit = step(lmFit)

# 유의미한 변수에 대한 요약 결과
summary(stepFit)

stepFit$coefficients %>% sort(decreasing = TRUE) %>% round(4)
