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
# R을 이용한 심리 통계 해석 및 시각화

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0433"

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
# loading packages
library(tidyverse)

# 파일 읽기
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "ChildAggression.dat"))

# Aggression : 아동의 공격성 점수
# Parenting_style : 점수가 높을수록 나쁜 양육 경험
# Computer_games : 점수가 높을수록 컴퓨터 게임을 많이 함
# Sibling_aggression : 점수가 높을수록 언니/누나/형/오빠들이 공격적임
# Television : 점수가 높을수록 TV 시청 시간이 많음
data = read.csv(fileList, sep = " ")

# 1. 산포도(scatter plot)를 그려 Aggression과 관련이 있을 것 같은 변수들을 탐색해보시오.
# 각 변수에 대한 산포도 행렬 및 상관계수를아래와 같이 시각화하였다.
# 그 결과 종속변수 (Aggression)를 기준으로 각 변수들 간의 증가/감소 관계를 나타냄을 확인할 수 있다 (증가: Sibling_Aggression, Television, Computer_Games, Parenting_Style; 감소: Diet).
# 또한 상관계수에서도 Diet -0.01, Sibling_Aggression 0.13, Television 0.16, Computer_Games 0.19, Parenting_Style 0.21순으로 높은 관계성을 보였다.

pairs(data)

# 상관계수
# cor(data)["Aggression", ] %>% sort() %>% round(2)
# Diet      Sibling_Aggression    Television    Computer_Games    Parenting_Style
# -0.01    0.13                 0.16         0.19             0.21

# 2. Parenting_style과 Sibling_aggression으로 아동의 공격성을 예측할 수 있는가?
# 아동의 공격성을 예측 가능하나 성능이 매우 낮음

# 독립변수 (Parenting_style, Sibling_aggression) 및 종속변수 (Aggression)를 설정하여 회귀모형을 적합하였다. 
# 그 결과 전체 변수에서 회귀모형의 수정된 결정계수은 0.050으로서 유의수준  0.05 이하 통계적으로 유의미하다.
# 이러한 회귀모형은 종속 변수의 약 5%만을 설명력을 지니기 때문에 예측 성능이 낮으나 다른 요인들을 고려하여 모형 성능이 향상될 것으로 판단된다.

# 한편 각 변수의 유의성 검정 결과 모든 변수 (Sibling_Aggression, Parenting_Style)는 유의수준 0.05 이하에서 통계적 유의미하다.
# 또한 종속변수 (Aggression)를 기준으로 독립변수들 간의 증가/감소 관계를 나타냄을 확인할 수 있다 (증가: Parenting_Style, Sibling_Aggression; 감소: 없음).
# 즉 증가 관계에서 Parenting_Style, Sibling_Aggression 순으로 종속 변수과의 높은 양의 관계성을 보였고 각각 평균적으로 0.062, 0.093 단위로 상승한다.

# Call:
#   lm(formula = Aggression ~ Parenting_Style + Sibling_Aggression,
#      data = data)
# 
# Residuals:
#   Min          1Q      Median          3Q         Max
# -1.09755305 -0.17180165  0.00091806  0.15404897  1.23036760
# 
# Coefficients:
#   Estimate   Std. Error  t value   Pr(>|t|)
# (Intercept)        -0.005784111  0.012064759 -0.47942   0.631797
# Parenting_Style     0.061983936  0.012256736  5.05713 5.5149e-07 ***
#   Sibling_Aggression  0.093409402  0.037504666  2.49061   0.012996 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3112519 on 663 degrees of freedom
# Multiple R-squared:  0.05324761,	Adjusted R-squared:  0.05039165
# F-statistic: 18.64435 on 2 and 663 DF,  p-value: 1.325469e-08

lmFit = lm(Aggression ~ Parenting_Style + Sibling_Aggression, data = data)
summary(lmFit)

# 3. Parenting_style과 Sibling_aggression이 Aggression에 미치는 영향력을 통제했을 때, tv나 컴퓨터 게임으로 아동의 공격성을 예측할 수 있는가?
# 아동의 공격성을 예측 가능하나 성능은 2번 회귀모형보다 높으나 개선 필요함

# 독립변수 (Parenting_Style, Sibling_Aggression, Television, Computer_Games) 및 종속변수 (Aggression)를 설정하여 회귀모형을 적합하였다. 
# 그 결과 전체 변수에서 회귀모형의 수정된 결정계수은 0.065으로서 유의수준  0.05 이하 통계적으로 유의미하다.
# 이러한 회귀모형은 종속 변수의 약 6.5%만을 설명력을 지니기 때문에 예측 성능이 낮으나 다른 요인들을 고려하여 모형 성능이 향상될 것으로 판단된다.

# 한편 각 변수의 유의성 검정 결과 일부 변수 (Television) 외에 모든 변수는 유의수준 0.01 이하에서 통계적 유의미하다.
# 또한 종속변수 (Aggression)를 기준으로 독립변수들 간의 증가/감소 관계를 나타냄을 확인할 수 있다 (증가: Parenting_Style, Sibling_Aggression, Computer_Games; 감소: 없음).
# 즉 증가 관계에서 Parenting_Style, Sibling_Aggression, Computer_Games 순으로 종속 변수과의 높은 양의 관계성을 보였고 각각 평균적으로 0.047, 0.062, 0.124 단위로 상승한다.

# Call:
#   lm(formula = Aggression ~ Parenting_Style + Sibling_Aggression +
#        Television + Computer_Games, data = data)
# 
# Residuals:
#   Min          1Q      Median          3Q         Max
# -1.06765745 -0.16508913 -0.00520362  0.15194751  1.21467334
# 
# Coefficients:
#   Estimate   Std. Error  t value   Pr(>|t|)
# (Intercept)        -0.005562994  0.012046193 -0.46181 0.64437304
# Parenting_Style     0.047224678  0.014257294  3.31232 0.00097567 ***
#   Sibling_Aggression  0.062206798  0.038386185  1.62055 0.10559073
# Television          0.045470269  0.046097402  0.98640 0.32430000
# Computer_Games      0.124355476  0.036590087  3.39861 0.00071797 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3087566 on 661 degrees of freedom
# Multiple R-squared:  0.07117736,	Adjusted R-squared:  0.06555665
# F-statistic: 12.66341 on 4 and 661 DF,  p-value: 6.1856e-10

lmFit2 = lm(Aggression ~ Parenting_Style + Sibling_Aggression + Television + Computer_Games, data = data)
summary(lmFit2)

# 두 회귀모형 비교
# Model 1: 종속변수 Aggression 및 독립변수 Parenting_Style, Sibling_Aggression
# Model 2: 종속변수 Aggression 및 독립변수 Parenting_Style, Sibling_Aggression, Television, Computer_Games
# 두 회귀모형 간의 통계적으로 검정하는 F검정 결과를 나태냄.
# 그 결과 유의수준 (p-value)은 0.0018로서 유의수준 0.05 이하에서 통계적 유의미한 결과를 보임
# 따라서 Parenting_Style, Sibling_Aggression을 통제했음에도 불구하고 Television, Computer_Games는 아동의 공격성을 예측하는데 추가적인 영향을 미침
anova(lmFit, lmFit2)

# Analysis of Variance Table
# 
# Model 1: Aggression ~ Parenting_Style + Sibling_Aggression
# Model 2: Aggression ~ Parenting_Style + Sibling_Aggression + Television + 
#   Computer_Games
# Res.Df       RSS Df Sum of Sq       F    Pr(>F)   
# 1    663 64.229956                                  
# 2    661 63.013559  2 1.2163969 6.37988 0.0018016 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# 4. 공격성이 평균 이상인 아동들과 평균 미만인 아동들 사이에 컴퓨터 게임 시간에 차이가 있는가?
# 평균 Aggression 계산
meanAgg = mean(data$Aggression, na.rm = TRUE)

# Aggression 평균을 기준으로 두 그룹으로 나눔
data$type = ifelse(data$Aggression >= meanAgg, "meanUp", "meanDown")

# F검정
fTest = var.test(Computer_Games ~ type, data = data, conf.level = 0.95)
# fTest = var.test(Parenting_Style ~ type, data = data, conf.level = 0.95)

# F검정 결과 유의수준 p-value >= 0.05 이상으로서 대립가설이 채택 (두 그룹은 분산 동일)
print(fTest)

# F test to compare two variances
# 
# data:  Computer_Games by type
# F = 0.92591807, num df = 334, denom df = 330, p-value = 0.483382
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.746359272 1.148512239
# sample estimates:
#   ratio of variances
# 0.92591807


# T검정 (등분산 가정 O)
tTest = t.test(Computer_Games ~ type, data = data, conf.level = 0.95, var.equal = TRUE, paired = FALSE)
# tTest = t.test(Parenting_Style ~ type, data = data, conf.level = 0.95, var.equal = TRUE, paired = FALSE)

# T검정 결과 유의수준 p-value < 0.05 이하로서 귀무가설 기각 (두 그룹은 평균 차이)
# 즉 두 그룹 (평균 이상 meanUp, 평균 미만 meanDown)은 컴퓨터 게임 시간에 따라 통계적으로 유의미한 차이를 보임
print(tTest)

# Two Sample t-test
# 
# data:  Computer_Games by type
# t = -2.7990405, df = 664, p-value = 0.005274276
# alternative hypothesis: true difference in means between group meanDown and group meanUp is not equal to 0
# 95 percent confidence interval:
#   -0.12550306093 -0.02201693363
# sample estimates:
#   mean in group meanDown   mean in group meanUp
# -0.02627725124          0.04748274604

# 5. 이상의 결과와 추가적인 분석을 통해 아동의 공격성과 컴퓨터 게임이 관련이 있다고 할 수 있는지에 대해 종합적으로 서술하시오.
# 회귀모형과 F/T검정 분석 결과를 토대로 아동의 공격성과 컴퓨터 게임 사이의 관련성을 종합적으로 서술하면 다음과 같다.
# (회귀모형의 결과) Parenting_Style, Sibling_Aggression, Television, Computer_Games를 포함하는 회귀 모형에서, Computer_Games 변수는 유의수준 0.01 이하에서 통계적으로 유의미하였다. 이는 컴퓨터 게임이 아동의 공격성에 영향을 미칠 수 있다는 것을 나타낸다. 그러나 해당 모형의 수정된 결정계수가 0.065으로 아직 아동의 공격성의 분산의 작은 부분만을 설명하고 있어 다른 중요한 요인들을 추가하여 회귀 모형 개선이 요구된다.
# (평균 컴퓨터 게임 시간에 대한  T검정 결과) 평균 이상의 공격성을 가진 아동 그룹 (meanUp)과 평균 미만인 아동 그룹 (meanDown) 사이에서 컴퓨터 게임 시간에 통계적으로 유의미한 차이를 있었다. 이 결과는 아동의 공격성 수준과 그들이 컴퓨터 게임에 소비하는 시간 사이에 연관성이 있음을 보여준다.
# (총평) 앞선 결과를 종합적으로 해석하면, 컴퓨터 게임이 아동의 공격성에 영향을 미칠 수 있음을 나타낸다. 그러나 이러한 연관성은 다른 여러 요인들 (부모의 양육 스타일, 형제 간의 공격성 등)과 복합적으로 작용함을 유의해야 한다. 또한 이 연구의 설명력이 상대적으로 낮음을 고려할 때, 아동의 공격성을 좀 더 정확하게 예측하기 위해서는 다른 요인 (아동의 성별, 연령, 학교 환경, 친구 관계, 정서적 상태 등)들을 추가로 고려해야 한다.
# 따라서 아동의 공격성과 컴퓨터 게임이 관련이 있다고 할 수 있으나 이는 아동의 공격성을 결정하는 많은 요인 중 하나일 뿐이다. 향후 더 많은 변수를 포함하는 개선 연구가 필요하다.