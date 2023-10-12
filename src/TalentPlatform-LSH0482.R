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

# 단측 z-검정의 검정력 계산
# 유의 수준: 0.05, 효과 크기: 0.4, 표본 크기: 10
pwr.norm.test(d = 0.4, n = 10, sig.level = 0.05, alternative = "greater")

# 유의 수준: 0.05, 효과 크기: 0.4, 표본 크기: 40
pwr.norm.test(d = 0.4, n = 40, sig.level = 0.05, alternative = "greater")


# ***************************************************
# 과제1
# ***************************************************


# ***************************************************
# 과제2
# ***************************************************
library(meta)

fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "Cochrane.csv"))

fileInfo = fileList[1]
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
  sm = "RR",
  hakn = TRUE
)

# 결과 요약
summary(ranEffModel)


# ***************************************************
# 과제3
# ***************************************************



# ***************************************************
# 과제4
# ***************************************************
library(survival)

# 모의실험에 사용할 계수
set.seed(1)
coefs = rnorm(5)

# Weibull 분포의 shape parameter 설정
k = 2

# 데이터 읽기
data = pbc %>% 
  dplyr::select(c("age", "bili", "edema", "protime", "albumin")) %>% 
  na.omit() %>% 
  dplyr::mutate(
    X1 = scale(age)
    , X2 = scale(log(bili))
    , X3 = scale(edema)
    , X4 = scale(log(protime))
    , X5 = scale(albumin)
  )

dataL1 = data %>% 
  dplyr::mutate(
    lambda = exp(0 + X1*coefs[1] + X2*coefs[2] + X3*coefs[3] + X4*coefs[4] + X5*coefs[5])
    , simTime = (-log(runif(n())) / lambda)^(1/k)
  )

cat("[CHECK] coefs : ", coefs, "\n")
# [CHECK] coefs :  -0.6264538107 0.1836433242 -0.8356286124 1.595280802 0.3295077718 

# 모수적 생존모형으로 와이블 모형 적합
weibullFit = survreg(Surv(simTime) ~ X1 + X2 + X3 + X4 + X5, data = dataL1, dist = 'weibull')
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
coxFit = coxph(Surv(simTime) ~ X1 + X2 + X3 + X4 + X5, data = dataL1)
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



# # 파일 읽기
# fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "*/*/*/*.xlsx"))
# 
# # fileInfo = fileList[1]
# for (fileInfo in fileList) {
# 
#   cat(sprintf("[CHECK] fileInfo : %s", fileInfo), "\n")
#     
#   orgData = openxlsx::read.xlsx(fileInfo, sheet = 1, startRow = 1)
#   
#   data = orgData %>% 
#     tibble::as.tibble() %>% 
#     dplyr::rename(
#       sDate = "관측일자"
#       , sTime = "관측시간"
#       , alt = "유의파고(m)"
#       , inv = "유의파주기(sec)"
#     ) %>% 
#     readr::type_convert() %>% 
#     dplyr::filter(
#       ! is.na(alt)
#       , ! is.na(inv)
#     ) %>% 
#     dplyr::mutate(across(where(is.character), as.numeric)) %>% 
#     dplyr::mutate(
#       sDateTime = paste(sDate, sTime, sep = " ")
#     ) %>% 
#     dplyr::mutate(
#       dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M:%S")
#     ) %>% 
#     dplyr::filter(
#       dplyr::between(alt, 3, 16)
#       , inv >= 9
#     )
# 
#   if (nrow(data) < 1) { next }
#   
#   coeff = 0.35
#   
#   fileNameNotExt = tools::file_path_sans_ext(fs::path_file(fileInfo))
#   saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, fileNameNotExt)
#   dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#   
#   makePlot = ggplot(data, aes(x=dtDateTime)) +
#     geom_point(aes(y=alt, color = "alt")) +
#     geom_point(aes(y=inv*coeff, color= "inv")) +
#     scale_y_continuous(
#       limits = c(0, 10)
#       , name = "alt"
#       , sec.axis = sec_axis(~./coeff, name="inv")
#     ) +
#     labs(x = "Date Time", y = NULL, color = NULL, subtitle = fileNameNotExt) +
#     scale_color_manual(values = c("orange2", "gray30")) +
#     scale_x_datetime(date_labels = "%Y-%m", date_breaks = "6 month") +
#     theme(
#           text = element_text(size = 16)
#           , legend.position = "top"
#           , axis.text.x = element_text(angle = 45, hjust = 1)
#         )
#   
#   ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
#   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# }