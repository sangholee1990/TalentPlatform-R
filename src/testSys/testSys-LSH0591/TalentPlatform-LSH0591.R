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
# R을 이용한 분리초평면 추정 및 지지벡터 계산

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0591"

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
library(e1071)

# 데이터 읽기
data = data.frame(
  x1 = c(0.53, 1.00, 0.18, 1.10, 1.22, 0.54, 0.95, 0.40, 2.51, 2.80, 3.78, 3.84, 3.86, 1.25, 1.74, 3.50, 2.60, 0.88, 1.33, 2.10)
  , x2 = c(0.86, 0.09, 0.50, 0.90, 0.21, 1.46, 0.53, 1.52, 1.57, 2.19, 1.88, 3.84, 1.10, 3.76, 2.69, 2.22, 1.91, 3.57, 3.72, 1.90)
  , y = c(-1, -1, -1, -1, -1, -1, -1, -1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
)

# y를 factor로 변환
data$y = as.factor(data$y)


# SVM 모델 학습 (linear kernel, cost=1)
svmModel = svm(y ~ ., data = data, kernel = "linear", cost = 1, scale = FALSE)

# 분리 초평면 정보 추출
coef = t(svmModel$coefs) %*% svmModel$SV  # 가중치 계산
intercept = -svm_model$rho                 # 절편

# 서포트 벡터 추출
support_vectors <- svm_model$SV

# 결과 출력
cat("분리 초평면:\n")
cat("0 =", coef[1], "* x1 +", coef[2], "* x2 +", intercept, "\n")

cat("\n서포트 벡터:\n")
print(support_vectors)