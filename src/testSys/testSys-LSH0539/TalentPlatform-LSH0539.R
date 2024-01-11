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
# R을 이용한 지역별 보험료 및 BMI 체질량지수 데이터 분석 (분산분석, 회귀분석, 잔차분석)

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0539"

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
library(stats)
library(TukeyC)
library(lmtest)

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "hw4.csv"))
data = read.csv(fileInfo)


# (5점) 데이터 탐색(Data Exploration) 데이터 구조를 확인하여라(str 이용)
str(data)

# Character 변수형태를 factor로 변환하여라
data$region = as.factor(data$region)

# 각 변수의 요약통계량 확인하여라(summary)
summary(data)

# (15점) 분산분석(ANOVA): region(지역)에 따라 cost(보험료)이 차이가 나는지 분석해보고자 한다.
aovRes = aov(cost ~ region, data=data)
summary(aovRes)

# Df    Sum Sq   Mean Sq F value   Pr(>F)    
# region        3 1.889e+09 629610087   5.867 0.000685 ***
#   Residuals   261 2.801e+10 107319503                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# (10점) 귀무가설과 대립가설을 세우고 분산분석표를 통해 유의수준 1%에서 위 가설을 검정하고 결과를 해석하여라.
# P값은 0.000685로서 유의수준 0.01 (1%) 이하보다 작기 때문에 귀무가설을 기각하여 지역 간의 보험료의 차이가 있고 이는 통계적으로 유의미함

# (5점) 지역(region)에 따라 보험료(cost) 차이가 난다면 어떤 지역과 어떤 지역에서 차이가 나는지 다중비교분석(튜키검정)을 하고 그 결과를 해석하여라.
TukeyHSD(aovRes)

# 특정 지역 간의 보험료 차이를 세부적으로 분석한 결과 C-A, C-B, D-C에서 통계적으로 유의미함
# 즉 지역 C의 보혐료는 지역 A와 B에 비해 유의미하게 높음
# 지역 D의 보험료는 지역 C에 비해 유의미하게 낮음
# 반면에 B-A, D-A, D-B에서는 통계적으로 유의미하지 못함

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = cost ~ region, data = data)
# 
# $region
# diff         lwr       upr     p adj
# B-A  -362.0412  -5228.6277  4504.545 0.9974720
# C-A  5136.9118    764.4641  9509.360 0.0138985
# D-A -1174.1708  -6040.7573  3692.416 0.9243636
# C-B  5498.9530    909.8809 10088.025 0.0115273
# D-B  -812.1296  -5874.2386  4249.979 0.9759007
# D-C -6311.0826 -10900.1548 -1722.010 0.0025091


# (30점) 회귀분석(Regression Model): cost(종속변수, 보험료)에 영향을 미치는 변수로 bmi(설명변수, 체질량지수)를 생각하고 있다. 다음 물음에 답하시오
lmFit = lm(cost ~ bmi, data=data)
summary(lmFit)

# Call:
# lm(formula = cost ~ bmi, data = data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -18170.0  -3869.0    367.7   3545.8  16743.4 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -13530.99    1970.30  -6.867  4.7e-11 ***
#   bmi           1453.17      63.38  22.927  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6157 on 263 degrees of freedom
# Multiple R-squared:  0.6665,	Adjusted R-squared:  0.6653 


# (10점) cost(종속변수, 보험료)와 bmi(설명변수, 체질량지수)의 단순회귀분석 결과를 제시하고 설명변수가 유의한지 유의수준 1%에서 판단하여라
# 단순회귀모형의 P값은 2.2e-16 이하로서 유의수준 0.01 (1%) 이하보다 작기 때문에 통계적으로 유의함
# 한편 각 회귀계수에서 설명변수인 BMI의 P값은 2e-16 이하로서 유의수준 0.01 (1%) 이하보다 작기 때문에 유의미한 회귀계수로 나타남

# (3점) 회귀모형식을 쓰고 회귀계수(β)의 의미를 설명하여라
# 회귀모형: cost = -13530.99 + 1453.17 * bmi
# 회귀계수(β)는 절편으로서 BMI 0일 경우  회귀모형의 시작점으로 파악됨

# (4점) 회귀모형에서 설명변수가 종속변수의 변동을 설명하는 비율은 얼마이며, 그 의미는 무엇인가?
# 수정된 결정계수는 0.6653로서 BMI 설명변수는 전체 cost 보험료의 66.65%를 설명할 수 있음

# (10점) 회귀모형의 잔차 가정 만족 여부(정규성, 독립성, 등분산성)를 확인하여라 (Assumptions Check)
# 정규성 만족: QQ 그림은 기준선을 기준으로 고르게 분포되어 정규분포를 따름
resid = residuals(lmFit)

qqnorm(resid)
qqline(resid)


# 독립성 만족: P값은 0.2776로서 유의수준 0.05보다 크기 때문에 귀무가설을 기각하지 못하여 통계적으로 유의미한 자기상관이 없음
lmtest::dwtest(lmFit)


# 등분산성 만족: 예측 결과에 따른 잔차 그림에서 0선을 기준으로 무작위로 분포되어 등분산성을 따름
plot(fitted(lmFit), resid)
abline(h=0, col="red")


# (3점) 어느 사람의 bmi(체질량지수)가 30이었다면, cost(보험료)는 얼마로 추정할 수 있는가?
# 30064.26 추정
predict(lmFit, newdata = data.frame(bmi=30))
