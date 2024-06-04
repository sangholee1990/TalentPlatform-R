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
# R을 이용한 ADAM 알고리즘 최소값 찾기

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0560"

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
# 패키지 읽기
# library(ggplot2)

# 함수 정의
f = function(x) {
  x^4 - 5*x^2 - 3*x
}

# 도함수 정의
fPrime = function(x) {
  4*x^3 - 10*x - 3
}

# Adam 알고리즘 입력 파라미터
beta1 = 0.9
beta2 = 0.999
epsilon = 1e-8

# 학습률
# alpha = 0.01
alpha = 0.001

# 초기값
x = -3 

# Adam 알고리즘 초기화
m = 0
v = 0
t = 0

# 반복 횟수
maxIter = 10000

# 시각화를 위한 입력 데이터 초기화
xHistory = numeric(maxIter)
fHistory = numeric(maxIter)
xHistory[1] = x
fHistory[1] = f(x)

# maxIter 반복문을 통해 Adam 알고리즘 최적화 수행
for (i in 2:maxIter) {
  t = t + 1
  g = fPrime(x)
  m = beta1 * m + (1 - beta1) * g
  v = beta2 * v + (1 - beta2) * g^2
  mHat = m / (1 - beta1^t)
  vHat = v / (1 - beta2^t)
  x = x - alpha * mHat / (sqrt(vHat) + epsilon)
  
  xHistory[i] = x
  fHistory[i] = f(x)
  
  # 수렴 조건
  if (abs(fPrime(x)) < 1e-6) {
    xHistory = xHistory[1:i]
    fHistory = fHistory[1:i]
    
    # 반복문 종료
    break
  }
}

# 최적화 결과 x, f(x) 계산
optimizedX = x
optimizedF = f(x)

# 최적화 결과 출력
cat("최적화 x 값: ", optimizedX, "\n")
cat("최적화 f(x) 값: ", optimizedF, "\n")

# 시각화를 위한 임시 데이터
xValues = seq(-3.5, 3.5, length.out = 400)
yValues = sapply(xValues, f)

df = data.frame(x = xValues, y = yValues)
pathDf = data.frame(x = xHistory, y = fHistory)

plot(df$x, df$y, type = "l", col = "blue", xlab = "x", ylab = "f(x)", main = paste("Adam Optimization on f(x) / 최적화 x 값: ", round(optimizedX, 4), ", 최적화 f(x) 값: ", round(optimizedF, 4)))
points(pathDf$x, pathDf$y, col = "red", pch = 16, cex = 0.5)

# ggplot() +
#   geom_line(data = df, aes(x = x, y = y), color = "blue") +
#   geom_point(data = pathDf, aes(x = x, y = y), color = "red", size = 1) +
#   labs(
#     title = paste("Adam Optimization on f(x) / 최적화된 x 값: ", round(optimizedX, 4), ", 최적화된 f(x) 값: ", round(optimizedF, 4)),
#     x = "x",
#     y = "f(x)"
#   )