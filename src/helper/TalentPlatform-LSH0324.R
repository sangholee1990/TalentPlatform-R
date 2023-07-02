
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
# R을 이용한 응용 시뮬레이션 (적분, 피보나치 수열, 유사 피보나치 수열)

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0324"
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
# 1. 에서 를 구하고자 한다. 이때 구간을 100구간으로 나누어 면적을 구하는 방법을 설명하고 프로그램을 작성하시오. 
# 또한 평균을 이용하여 면적을 구하는 방법을 설명하고 구하시오.

formula = function(x) {
  (x**3) + 4
}

res = integrate(formula, lower=0, upper=2)
cat("val : ", res$value, "\n")


# 2. 1부터 n까지 더하는 프로그램을 작성하고 10부터 1000까지의 합을 구하시오. 
getSum = function(srtVal, endVal) {
  result = sum(srtVal:endVal, na.rm = TRUE)
  return(c(result))
}

res = getSum(10, 1000)
cat("val : ", res, "\n")

# 3 유사피보나치 수열의 일반항은 다음과 같다.
getFibo = function(n) {

  tmpVal = c(1, 1, 1)  

  for (i in 1:n) {
    tmpVal[i + 3] = tmpVal[i] - (2 * tmpVal[i + 1]) + (2 * tmpVal[i + 2])
  }
  
  result = tmpVal
  
  return(c(result))
}


# (1) 유사피보나치 수열에서 을 계산하는 프로그램을 작성하시오.
res = getFibo(10)[10]
cat("val : ", res, "\n")

# (2) 유사피보나치 수열에서 을 프로그램을 작성하시오.
res = sum(getFibo(10), na.rm = TRUE)
cat("val : ", res, "\n")

# (3) 유사피보나치 수열에서 을 프로그램을 작성하시오.
res = sum(getFibo(20)[c(2, 5, 8, 11, 14, 17, 20)], na.rm = TRUE)
cat("val : ", res, "\n")

# 4 의 프로그램을 작성하고의 값을 구하시오. 
getFac = function(n) {
  result = 1
  for (i in 1:n) {
    result = (result * i)
  }
  
  return(c(result))
}

res = getFac(10) / (getFac(10) * getFac(10 - 2))
cat("val : ", res, "\n")


# 5 인 방정식의 해를 이분법을 이용하여 계산하시오. 단 오차의      한계는 0.00001임. 
# 
# 6. 일 때 의 근사값을 구하시오.  
# 
# 7. 피보나치 수열의 일반항은 다음과 같다.(15)
getFiboDefault = function(n) {
  
  tmpVal = c(1, 1)  
  
  for (i in 1:n) {
    tmpVal[i + 2] = tmpVal[i] + tmpVal[i + 1]
  }
  
  result = tmpVal
  
  return(c(result))
}

# (1) 피보나치 수열에서 을 계산하는 프로그램을 작성하시오.(10)
res = getFiboDefault(15)[15]
cat("val : ", res, "\n")

# (2) 피보나치 수열에서 을 계산하시오.(10)
res = sum(getFiboDefault(15), na.rm = TRUE)
cat("val : ", res, "\n")

# (3) 피보나치 수열에서 을 계산하시오.(10)
res = sum(getFiboDefault(30)[seq(3, 30, 3)], na.rm = TRUE)
cat("val : ", res, "\n")

# (4) 피보나치 수열에서 을 계산하시오.
res = sum(getFiboDefault(30)[seq(1, 31, 3)], na.rm = TRUE)
cat("val : ", res, "\n")


# (5) 피보나치 수열에서 을 계산하시오.
res = sum(getFiboDefault(30)[seq(2, 32, 3)], na.rm = TRUE)
cat("val : ", res, "\n")


# 
# 8. f(x)=log를 구하는 프로그램을 작성하시오.(10)
# 
# 
# 9. 에서 랜덤하게 100개를 발생시켜 양의 개수를 카운트하는 
# 프로그램을 작성하시오. rnorm(100) (10)
# 
# 10, 을 구하는 hap1 프로그램을 작성하시오.
res = sum(seq(1, 199, 2), na.rm = TRUE)
cat("val : ", res, "\n")

# 11. 일 때 과 의 값을 for문장과  표본평균 몬테칼로 적분법을 이용하여 구하시오.    


