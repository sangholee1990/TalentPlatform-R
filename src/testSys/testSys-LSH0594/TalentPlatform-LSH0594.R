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
# R을 이용한 선형회귀분석 및 K근접이웃의 검증지표 계산

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0594"

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
load("P1.RData")

# (1) 선형회귀분석(linear regression) 학습모델에 대하여 검증 데이터셋인 P1_test의 평균제곱오차(mean squared error, MSE)를 구하시오. (4점)

# 선형회귀분석 모델 
lmModel = lm(y ~ x1 + x2 + x3, data = P1_train)

# 검증 예측
P1_test$lmPrd = predict(lmModel, newdata = P1_test)

# 오차 계산
lmMse = mean((P1_test$y - P1_test$lmPrd)^2, na.rm = TRUE)
cat(sprintf("[CHECK] lmMse : %s", lmMse), "\n")

# (2) 가중치가 있는 k 근접이웃(weighted k-nearest neighbor, 강의자료 LNML2-3 35 page) 학습모델에 대하여 검증 데이터셋인 P1_test의 MSE를 구하시오. 단, k=17이고, 거리척도는 맨하탄 거리(Manhattan distance)를 사용한다. (6점)

# 가중치 k 근접이웃 모델
k = 17
eps = 1e-9
for (i in 1:nrow(P1_test)) {
  P1_testInfo = P1_test[i, ]
  
  # 학습 데이터와 거리 계산
  distances = abs(P1_train$x1 - P1_testInfo$x1) + abs(P1_train$x2 - P1_testInfo$x2) + abs(P1_train$x3 - P1_testInfo$x3)
  
  # 가장 가까운 k개의 이웃 선택
  nearIdx = order(distances)[1:k]
  nearWeg = 1 / ((distances[nearIdx] + eps)^2)
  nearY = P1_train$y[nearIdx]
  
  # 검증 예측
  P1_test$knnPrd[i] = sum(nearWeg * nearY, na.rm = TRUE) / sum(nearWeg, na.rm = TRUE)
}

# 오차 계산
knnMse = mean((P1_test$y - P1_test$knnPrd)^2, na.rm = TRUE)
cat(sprintf("[CHECK] knnMse : %s", knnMse), "\n")
