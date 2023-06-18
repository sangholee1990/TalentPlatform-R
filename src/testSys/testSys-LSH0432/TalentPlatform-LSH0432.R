# ================================================
# 요구사항
#=================================================
# R을 이용한 기업간의 이직의 영향 분석

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
setwd(getwd())

##### testing the models  ###################################################################
# 파일 읽기
fileList = Sys.glob(file.path(globalVar$inpPath, "emba_dataset_team+project.csv"))

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