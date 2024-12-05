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
# R을 이용한 단층 퍼셉트론 및 분류예측나무 성능

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0592"

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
# ************************************************
# 단층 퍼셉트론
# ************************************************
# 데이터 읽기
data = data.frame(
  x1 = c(0.53, 1.00, 0.18, 1.10, 1.22, 0.54, 0.95, 0.40, 2.51, 2.80, 3.78, 3.84, 3.86, 1.25, 1.74, 3.50, 2.60, 0.88, 1.33, 2.10)
  , x2 = c(0.86, 0.09, 0.50, 0.90, 0.21, 1.46, 0.53, 1.52, 1.57, 2.19, 1.88, 3.84, 1.10, 3.76, 2.69, 2.22, 1.91, 3.57, 3.72, 1.90)
  , y = c(-1, -1, -1, -1, -1, -1, -1, -1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
)

# data$y = as.factor(data$y)

# w0, w1, w2 기본값
w = c(1.2, 3.7, -8.5)

# 학습률 기본값
lambda = 0.23  

# 퍼셉트론 학습
for (i in 1:103) {
  for (j in 1:nrow(data)) {
    
    # 입력값
    x = c(1, data$x1[j], data$x2[j])
    
    # 가중합 계산
    z = sum(w * x, na.rm = TRUE)
    
    # 활성화 함수에 따른 예측값
    prd = ifelse(sum(w * x, na.rm = TRUE) >  -1.5, 1, -1)
    
    # 에러
    error = data$y[j] - prd
    
    # 가중치 업데이트
    w = w + lambda * error * x
  }
}


# w 103의 추정치
bestW = w
cat("w 103의 추정치 : ", round(bestW, 2), "\n")

# 정확도
accYesCnt = 0
for (j in 1:nrow(data)) {
  x = c(1, data$x1[j], data$x2[j])
  prd = ifelse(sum(bestW * x, na.rm = TRUE) > -1.5, 1, -1)
  data$prd[j] = prd
}

prdY = sum(data$prd == data$y, na.rm = TRUE)
accuracy = prdY / nrow(data) * 100
cat("정확도 : ", accuracy, "\n")

# ************************************************
# 분류예측나무 성능
# ************************************************
load("P4.RData")

# P4_test$y = as.factor(P4_test$y)


# 조건에 따른 예측값 계산
P4_test$prd = ifelse(
  P4_test$x2 == 0
  , 0
  , ifelse(
    P4_test$x5 == 0
    , ifelse(
      P4_test$x9 == 0
      , ifelse(
        P4_test$x7 == 0
        , 0
        , 1
        )
      , 1
      )
    , 1
    )
)


prdY = sum(P4_test$prd == P4_test$y, na.rm = TRUE)
accuracy = prdY / nrow(P4_test) * 100
cat("정확도 : ", accuracy, "\n")
