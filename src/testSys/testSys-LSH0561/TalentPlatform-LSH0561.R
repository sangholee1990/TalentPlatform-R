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
# R을 이용한 ADAM 알고리즘으로 다중선형회귀 예측모델 추정 및 결정계수 계산

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0561"

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
# 파일 검색
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "p3.RData"))

# 파일 읽기
load(fileInfo)

# 변수 저장 및 절편 (intercept) 추가
# data = p3 %>% 
#   dplyr::mutate(intercept = 1)
data = p3
data$intercept = 1

# 데이터 확인
head(data)

# 독립변수 설정
x = data[, c("x1", "x2", "intercept")]

# 종속변수 설정
y = data[, "y"]

# 비용 함수의 그래디언트 정의
comGrd = function(w, x, y) {
  predictions = as.matrix(x) %*% w
  errors = predictions - y
  gradient = t(as.matrix(x)) %*% errors / length(y)
  return(gradient)
}

# Adam 최적화 함수
adamOpt = function(x, y, alpha = 0.001, beta1 = 0.9, beta2 = 0.999, delta = 1e-8, weg = c(3, 2, 1), max_iter = 10000) {
  
  # 초기 파라미터 설정
  w = matrix(weg, nrow = ncol(x), ncol = 1) 
  
  # 1차 모멘트 벡터 초기화
  m = matrix(0, nrow = ncol(x), ncol = 1)
  
  # 2차 모멘트 벡터 초기화
  v = matrix(0, nrow = ncol(x), ncol = 1)
  t = 0
  
  while (t < max_iter) {
    t = t + 1
    
    # 시점 t에서의 그래디언트 벡터 계산
    g = comGrd(w, x, y) 
    
    # 1차 모멘트 벡터 업데이트
    m = beta1 * m + (1 - beta1) * g 
    
    # 2차 모멘트 벡터 업데이트
    v = beta2 * v + (1 - beta2) * (g^2) 
    
    # 바이어스 수정된 1차 모멘트 벡터 계산
    mHat = m / (1 - beta1^t) 
    
    # 바이어스 수정된 2차 모멘트 벡터 계산
    vHat = v / (1 - beta2^t) 
    
    # 파라미터 업데이트
    w = w - alpha * (mHat / (sqrt(vHat) + delta)) 
    
    # 8712 갱신 후 반복문 중지
    if (t == 8712) {
      break
    }
  }
  
  return(w)
}

# Adam 최적화 적용
optWeg = adamOpt(x, y, max_iter = 8712)

# 최적의 가중치 (회귀 계수) 출력
optWeg

# 예측값 계산
prd = as.matrix(x) %*% optWeg

# 결과를 데이터프레임으로 변환
result = data.frame(act = y, prd = prd)
result

# 결정계수 계산
sse = sum((result$act - result$prd)^2, na.rm = TRUE)
sst = sum((result$act - mean(result$act, na.rm = TRUE))^2, na.rm = TRUE)
r2 = 1 - sse/sst

# 결정계수 출력
cat(sprintf("[CHECK] 결정계수 : %s", r2), "\n")

# 시각화
plot(result$act, result$prd, xlab = "실측", ylab = "예측", main = sprintf("실측 vs 예측 (결정계수 : %s)", round(r2, 2)), col = "blue", pch = 16)
abline(0, 1, col = "red", lty = 2)
