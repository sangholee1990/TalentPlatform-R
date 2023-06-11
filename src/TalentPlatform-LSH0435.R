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
# R을 이용한 인공신경망으로 이차방성식 실근 및 소수 개수 예측

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0435"

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
library(nnet)
library(primes)

# ******************************************************
# 이차방성식 실근
# ******************************************************
# R을 이용한 인공신경망으로 이차방성식 실근 및 소수 개수 예측

# 이차방정식의 계수와 실근을 계산하는 함수를 생성합니다
getData = function(n) {
  a = sample(-5:5, n, replace = TRUE)
  b = sample(-5:5, n, replace = TRUE)
  c = sample(-5:5, n, replace = TRUE)
  root1 = (-b + sqrt(b^2 - 4 * a * c)) / (2 * a)
  root2 = (-b - sqrt(b^2 - 4 * a * c)) / (2 * a)

  return(data.frame(a, b, c, root1, root2))
}

# 학습 데이터 생성
set.seed(123)

trainData = getData(1000000) %>%
  na.omit() %>%
  distinct(a, b, c, root1, root2)

# 인공신경망 학습
nnetModel = nnet(cbind(root1, root2) ~ a + b + c, data = trainData, size = 100, maxit = 5000, linout = TRUE)

# 인공신경망 예측
predict(nnetModel, data.frame(a = 1, b = 3, c = 2))
predict(nnetModel, data.frame(a = 1, b = -5, c = 6))

# ******************************************************
# 소수 개수
# ******************************************************
trainData = data.frame(n = 1:3005)
trainData$cnt = sapply(trainData$n, function(x) sum(primes::is_prime(2:x), na.rm = TRUE))

# 인공신경망 학습
nnetModel = nnet(cnt ~ n, data = trainData, size = 50,, maxit=5000, linout = TRUE)

predict(nnetModel, newdata=data.frame(n=c(2:100))) %>% floor
