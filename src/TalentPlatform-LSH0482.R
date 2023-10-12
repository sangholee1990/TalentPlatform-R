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
# R을 이용한 대학원 과제 관련 통계 분석 멘토링

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0482"

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
library(pwr)
library(survival)
library(meta)
library(kernlab)
library(missForest)
library(mice)
library(rpart)
library(caret)

# ***************************************************
# 과제1
# ***************************************************
# 단측 z-검정의 검정력 계산
# 유의 수준: 0.05, 효과 크기: 0.4, 표본 크기: 10
pwr::pwr.norm.test(d = 0.4, n = 10, sig.level = 0.05, alternative = "greater")

# 유의 수준: 0.05, 효과 크기: 0.4, 표본 크기: 40
pwr.norm.test(d = 0.4, n = 40, sig.level = 0.05, alternative = "greater")


result = pwr.norm.test(d = 0.4, n = 10, sig.level = 0.05, alternative = "greater")
print(result)
sprintf("%s", result)
# cat(print(result), "\n")

# base::print(result)

# ***************************************************
# 과제2
# ***************************************************
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "Cochrane.csv"))
# fileList = Sys.glob("C:/SYSTEMS/PROG/R/TalentPlatform-R/resources/input/test/LSH0482/Cochrane.csv")

fileInfo = fileList[1]
# print(fileInfo)
data = readr::read_csv(fileInfo)

# resp.h: 처리군 응답 환자수
# fail.h: 처리군 실패 환자수
# drop.h: 처리군 탈락 환자수

# resp.p: 대조군 응답 환자수
# fail.p: 대조군 실패 환자수
# drop.p: 대조군 탈락 환자수

dataL1 = data %>% 
  dplyr::mutate(
    event.e = resp.h
    , n.e = resp.h + fail.h
    , event.c = resp.p
    , n.c = resp.p + fail.p
  )


# Mantel-Haenszel 방법으로 고정 효과 모델 적용
fixEffModel = meta::metabin(
  event.e = event.e,
  n.e = n.e,
  event.c = event.c,
  n.c = n.c,
  data = dataL1,
  studlab  = authoryear,
  method = "MH", # Mantel-Hasenzel
  # method = "Inverse", # inverse variance
  sm = "RR"
)

# 결과 요약
summary(fixEffModel)

# DerSimonian-Laird 방법으로 무작위 효과 모델 적용
# Inverse 방법은 무작위 효과 모델에 사용할 때 내부적으로 DerSimonian-Laird 추정을 사용
ranEffModel = meta::metabin(
  event.e = event.e,
  n.e = n.e,
  event.c = event.c,
  n.c = n.c,
  studlab  = authoryear,
  data = dataL1,
  method = "Inverse", # inverse variance
  # method = "DL", # inverse variance
  sm = "RR",
  hakn = TRUE
)

# 결과 요약
summary(ranEffModel)

# https://bookdown.org/baba_yoshihiko/Doing_Meta_Analysis_in_R/sem.html

# ***************************************************
# 과제3
# ***************************************************



# ***************************************************
# 과제4
# ***************************************************
# 모의실험에 사용할 계수
set.seed(1)
coefs = rnorm(5)

# Weibull 분포의 shape parameter 설정
k = 2

# 기본
# pbc[ , c("age", "bili")]

# dplyr

 # %>% : <Ctrl> + <Shift> + <M>
# 데이터 읽기
data = pbc %>% 
  dplyr::select(c("age", "bili", "edema", "protime", "albumin")) %>% 
  na.omit() %>% 
  dplyr::mutate(
    X1 = scale(age)
    , X2 = scale(log(bili))
    # , X22 = bili %>% log() %>% scale()
    , X3 = scale(edema)
    , X4 = scale(log(protime))
    , X5 = scale(albumin)
  )

dataL1 = data %>% 
  dplyr::mutate(
    lambda = exp(0 + X1*coefs[1] + X2*coefs[2] + X3*coefs[3] + X4*coefs[4] + X5*coefs[5])
    , simTime = (-log(runif(n())) / lambda)^(1/k)
  )

colnames(dataL1)

cat("[CHECK] coefs : ", coefs, "\n")
# [CHECK] coefs :  -0.6264538107 0.1836433242 -0.8356286124 1.595280802 0.3295077718 

# 모수적 생존모형으로 와이블 모형 적합
weibullFit = survival::survreg(Surv(simTime) ~ X1 + X2 + X3 + X4 + X5, data = dataL1, dist = 'weibull')
summary(weibullFit)
# X1의 경우 0.294로서 X1 변수가 증가할수록 실패 시간 (생존 시간 감소)의 log 위험비율 0.294배 증가
# 이러한 결과는 유의수준 (p value)에서 0.05 이하로서 통계적으로 유의미함 

#   survreg(formula = Surv(simTime) ~ X1 + X2 + X3 + X4 + X5, data = dataL1, 
#           dist = "weibull")
# Value Std. Error         z          p
# (Intercept) -0.0206454  0.0232250  -0.88893   0.374039
# X1           0.2942836  0.0215484  13.65688 < 2.22e-16
# X2          -0.0433760  0.0254171  -1.70657   0.087902
# X3           0.4428880  0.0239870  18.46371 < 2.22e-16
# X4          -0.8019337  0.0225356 -35.58518 < 2.22e-16
# X5          -0.1502026  0.0248869  -6.03540 1.5857e-09
# Log(scale)  -0.7961981  0.0391530 -20.33558 < 2.22e-16
# 
# Scale= 0.451041 
# 
# Weibull distribution
# Loglik(model)= -212.8   Loglik(intercept only)= -489.8
# Chisq= 554.07 on 5 degrees of freedom, p= 1.7e-117 
# Number of Newton-Raphson Iterations: 7 
# n= 416 

# 비례위험모형 적합
coxFit = survival::coxph(Surv(simTime) ~ X1 + X2 + X3 + X4 + X5, data = dataL1)
summary(coxFit)

# 계수가 각 변수에 미치는 상대적인 영향
# 위험비 exp(coef)에서 1보다 크면 위험 증가하나 1보다 작으면 위험 감소
# X1의 경우 0.43로서 X1이 증가할수록 위험 감소
# X3의 경우 2.04로서 X3가 증가할수록 위험 증가

# coxph(formula = Surv(simTime) ~ X1 + X2 + X3 + X4 + X5, data = dataL1)
# 
# n= 416, number of events= 416 
# 
# coef   exp(coef)    se(coef)         z   Pr(>|z|)    
# X1 -0.84671820  0.42881993  0.06369112 -13.29413 < 2.22e-16 ***
#   X2  0.43534607  1.54549782  0.05857596   7.43216 1.0684e-13 ***
#   X3  0.71067729  2.03536932  0.06168279  11.52148 < 2.22e-16 ***
#   X4  0.61214753  1.84438802  0.06438879   9.50705 < 2.22e-16 ***
#   X5 -0.33355669  0.71637128  0.05887427  -5.66558 1.4653e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# exp(coef) exp(-coef) lower .95 upper .95
# X1 0.4288199  2.3319812 0.3784957 0.4858352
# X2 1.5454978  0.6470407 1.3778706 1.7335180
# X3 2.0353693  0.4913113 1.8035938 2.2969297
# X4 1.8443880  0.5421853 1.6257151 2.0924743
# X5 0.7163713  1.3959242 0.6382992 0.8039926
# 
# Concordance= 0.777  (se = 0.011 )
# Likelihood ratio test= 412.45  on 5 df,   p=<2e-16
# Wald test            = 400.19  on 5 df,   p=<2e-16
# Score (logrank) test = 507.1  on 5 df,   p=<2e-16


# ***************************************************
# 과제5
# ***************************************************
data(spam)

set.seed(123)
spam.1 = spam

# 57개의 공변량에 대해 각각 20%의 결측을 생성
# i = 1
for (i in 1:57) {
  colName = colnames(spam)[i]
  if (colName == "type") next
  
  idx = sample(1:nrow(spam.1), size = nrow(spam) * 0.2)
  spam.1[idx, i] = NA
}

# summary(spam)
# summary(spam.1)

# 데이터 분할
# index = sample(1:nrow(spam.1), 0.75 * nrow(spam.1))
index = sample(1:nrow(spam.1), 0.20 * nrow(spam.1))
train_data = spam.1[index, ]
test_data = spam.1[-index, ]

# 1. missForest imputation과 나무 모형에 의한 예측 정확도 평가: 학습 75%, 테스트 25%에 배당한다. 
missForest_imputed = missForest(train_data)
train_data_mf = missForest_imputed$ximp

# 나무 모형 학습 및 예측
# make + address + ... +
tree_model_mf = rpart(type ~ ., data = train_data_mf, method = "class")
pred_mf = predict(tree_model_mf, newdata = test_data, type = "class")

table(test_data$type, pred_mf)
# accuracy_mf = sum(diag(table(test_data$type, pred_mf))) / nrow(test_data)


# 2. mice imputation과 나무 모형에 의한 예측 정확도 평가: 학습에 75%를, 테스트에 배당한다. 이때 imputed dataset은 1개만 쓴다. 
set.seed(123)
mice_imputed <- mice(train_data, method = "cart", m = 1)
train_data_mice <- complete(mice_imputed, 1)

# 나무 모형 학습 및 예측
tree_model_mice <- rpart(type ~ ., data = train_data_mice, method = "class")
pred_mice <- predict(tree_model_mice, newdata = test_data, type = "class")

table(test_data$type, pred_mice)
# accuracy_mice <- sum(diag(table(test_data$type, pred_mice))) / nrow(test_data)


# 20% 학습 / 80% 테스트 임시 결과
# 스팸이 아닌 메시지를 정확하게 분류할 경우 missForest
# 반면에 스팸 식별의 정확성이 중요할 경우 mice 선택

# > table(test_data$type, pred_mf)
# pred_mf
# nonspam spam
# nonspam    2124   94
# spam        440 1023

# > table(test_data$type, pred_mice)
# pred_mice
# nonspam spam
# nonspam    2079  139
# spam        385 1078

