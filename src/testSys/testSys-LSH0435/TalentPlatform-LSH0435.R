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
