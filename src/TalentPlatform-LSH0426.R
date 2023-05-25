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
# R을 이용한 대중교통 통행량 막대 그래프 시각화

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0426"

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
library(xlsx)

# 파일 읽기
# fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "*.xlsx"))
#
# # fileInfo = fileList[1]
# data = tibble::tibble()
# for (fileInfo in fileList) {
#
#   selData = xlsx::read.xlsx(fileInfo, sheetIndex = 1) %>%
#     tibble::as.tibble() %>%
#     dplyr::filter(
#       ! is.na(통행량)
#     )
#
#   data = dplyr::bind_rows(data, selData)
# }
#
#
# dataL1 = data %>%
#   dplyr::rename(key = 시도.출발.) %>%
#   dplyr::select(c("일자", key, "통행량")) %>%
#   dplyr::arrange(desc(통행량))
#
# statData = dataL1 %>%
#   dplyr::group_by(일자) %>%
#   dplyr::summarise(
#     sumVal = sum(통행량, na.rm = TRUE)
#   )
#
# statDataL1 = dataL1 %>%
#   dplyr::left_join(statData, by = c("일자" = "일자")) %>%
#   dplyr::mutate(
#     perVal = (통행량 / sumVal) * 100.0
#   ) %>%
#   dplyr::arrange(desc(통행량))
#
#
# # 내림차순 정렬
# dataL1$key = forcats::fct_relevel(dataL1$key, unique(dataL1$key))
# statDataL1$key = forcats::fct_relevel(statDataL1$key, unique(statDataL1$key))
#
#
# # 주요 시도에 따른 대중교통 통행량 막대 그래프
# plotSubTitle = sprintf("%s", "주요 시도에 따른 대중교통 통행량 막대 그래프")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
# dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
# ggplot(data = dataL1, aes(x = key, y = 통행량, fill = 일자, label = round(통행량, 0))) +
#   geom_bar(stat = "identity", width = 0.5, position=position_dodge(width = 0.75))  +
#   geom_text(position = position_dodge(width = 0.75), size = 4, vjust = -0.5, hjust = 0.5, color = "black") +
#   labs(title = NULL, x = "주요 시도", y = "통행량", colour = NULL, fill = NULL, subtitle = plotSubTitle) +
#   theme(
#     text = element_text(size = 16)
#     , legend.position = "top"
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
#
#
# # 주요 시도에 따른 대중교통 %통행량 막대 그래프
# plotSubTitle = sprintf("%s", "주요 시도에 따른 대중교통 %통행량 막대 그래프")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
# dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
# ggplot(data = statDataL1, aes(x = key, y = perVal, fill = 일자, label = round(perVal, 1))) +
#   geom_bar(stat = "identity", width = 0.5, position=position_dodge(width = 0.75))  +
#   geom_text(position = position_dodge(width = 0.75), size = 4, vjust = -0.5, hjust = 0.5, color = "black") +
#   labs(title = NULL, x = "주요 시도", y = "%통행량", colour = NULL, fill = NULL, subtitle = plotSubTitle) +
#   theme(
#     text = element_text(size = 16)
#     , legend.position = "top"
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


# 라이브러리 읽기
library(rms)
library(PredictABEL)
library(dplyr)
library(survival)
library(riskRegression)
library(pec)
require(prodlim)
library(lava)
library(ggplot2)
library(cowplot)
library(Rcpp)
library(survminer)
library(moonBook)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(brms)
library(glmmTMB)
library(MASS)
library(lme4)
library(survminer)
library(lm.beta)
library(GGally)
library(scales)
library(ggm)
library(moonBook)
library(MatchIt)
library(optmatch)
library(cobalt)
library(twang)
library(survival)
library(survey)
library(tableone)
library(jskm)
library(tools)
library(twang)
library(ggplot2)
library(survminer)
library(MatchIt)
library(MatchIt)
library(survival)

################ raw data ################
##########################################
##########################################
# data <-read.csv(file="C:/Users/User/Desktop/PTRG_DES/data/PTRG_DES_20220723_update.csv",header=TRUE)
# PTRG <-read.csv(file="C:/Users/User/Desktop/PTRG_DES/data/PTRG_DES_20220723_update.csv",header=TRUE)

# 파일 읽기
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "PTRG_DES_20220723_update.csv"))
fileInfo = fileList[1]

data <- read.csv(file = fileInfo, header = TRUE)
PTRG <- read.csv(file = fileInfo, header = TRUE)

head(PTRG)
str(PTRG)
nrow(PTRG)

### before PSM/IPTW table 1.

mytable(CCB ~ ., data = PTRG, method = 1, catMethod = 0)
mycsv(mytable(CCB ~ ., data = PTRG, method = 1, catMethod = 0), file = "beforePSM.csv")


##### before PSM/IPTW table 2. #####


cox.MACCE = coxph(Surv(MACCE2_d, MACCE2 == 1) ~ CCB, data = PTRG)
summary(cox.MACCE)

cox.MACCE = coxph(Surv(Death_d, CV_DEATH == 1) ~ CCB, data = PTRG)
summary(cox.MACCE)

cox.MACCE = coxph(Surv(MI_d, TOTAL_MI == 1) ~ CCB, data = PTRG)
summary(cox.MACCE)

cox.MACCE = coxph(Surv(ST_d, ST_YN == 1) ~ CCB, data = PTRG)
summary(cox.MACCE)

cox.MACCE = coxph(Surv(CVA_d, CVA == 1) ~ CCB, data = PTRG)
summary(cox.MACCE)

cox.MACCE = coxph(Surv(RR_d, Revascularization == 1) ~ CCB, data = PTRG)
summary(cox.MACCE)

cox.MACCE = coxph(Surv(BL_day, BL_YN == 1) ~ CCB, data = PTRG)
summary(cox.MACCE)


##################### Figure 1. before PSM/IPTW

fit <- survfit(Surv(MACCE2_d, MACCE2 == 1) ~ CCB, data = PTRG)

ggsurvplot(fit, title = "MACCE2", data = PTRG,
           fun = "cumhaz", risk.table = TRUE, conf.int = FALSE,
           legend = "right", legend.labs = c("no CCB", "CCB"), palette = c("gray 39", "orangered"),
           xlim = c(0, 1825), break.time.by = 365, xlab = "Follow-up duration (days)",
           ylim = c(0, 0.2), break.by = 0.05)

fit <- survfit(Surv(Death_d, CV_DEATH == 1) ~ CCB, data = PTRG)
fit
ggsurvplot(fit, title = "CV death", data = PTRG,
           fun = "cumhaz", risk.table = TRUE, conf.int = FALSE,
           legend = "right", legend.labs = c("no CCB", "CCB"), palette = c("gray 39", "orangered"),
           xlim = c(0, 1825), break.time.by = 365, xlab = "Follow-up duration (days)",
           ylim = c(0, 0.1), break.by = 0.05)

fit <- survfit(Surv(MI_d, TOTAL_MI == 1) ~ CCB, data = PTRG)
fit
ggsurvplot(fit, title = "Myocardial infarction", data = PTRG,
           fun = "cumhaz", risk.table = TRUE, conf.int = FALSE,
           legend = "right", legend.labs = c("no CCB", "CCB"), palette = c("gray 39", "orangered"),
           xlim = c(0, 1825), break.time.by = 365, xlab = "Follow-up duration (days)",
           ylim = c(0, 0.1), break.by = 0.05)

fit <- survfit(Surv(ST_d, ST_YN == 1) ~ CCB, data = PTRG)
fit
ggsurvplot(fit, title = "Stent thrombosis", data = PTRG,
           fun = "cumhaz", risk.table = TRUE, conf.int = FALSE,
           legend = "right", legend.labs = c("no CCB", "CCB"), palette = c("gray 39", "orangered"),
           xlim = c(0, 1825), break.time.by = 365, xlab = "Follow-up duration (days)",
           ylim = c(0, 0.1), break.by = 0.05)

fit <- survfit(Surv(CVA_d, CVA == 1) ~ CCB, data = PTRG)
fit
ggsurvplot(fit, title = "Stroke", data = PTRG,
           fun = "cumhaz",
           risk.table = TRUE, conf.int = FALSE,
           legend = "right", legend.labs = c("no CCB", "CCB"), palette = c("gray 39", "orangered"),
           xlim = c(0, 1825), break.time.by = 365, xlab = "Follow-up duration (days)",
           ylim = c(0, 0.1), break.by = 0.05)

fit <- survfit(Surv(BL_day, BL_YN == 1) ~ CCB, data = PTRG)
fit
ggsurvplot(fit, title = "Bleeding", data = PTRG,
           fun = "cumhaz",
           risk.table = TRUE, conf.int = FALSE,
           legend = "right", legend.labs = c("no CCB", "CCB"), palette = c("gray 39", "orangered"),
           xlim = c(0, 1825), break.time.by = 365, xlab = "Follow-up duration (days)",
           ylim = c(0, 0.1), break.by = 0.05)


#### univariable cox

cox.MACCE <- coxph(Surv(MACCE2_d, MACCE2 == 1) ~ TC, data = PTRG)
summary(cox.MACCE)


cox.MACCE <- coxph(Surv(MACCE2_d, MACCE2 == 1) ~ CCB +
  Age +
  BMI +
  DM +
  HTN +
  CKD +
  CHF +
  PRE_CVA +
  HB +
  TC +
  ASP +
  CILO +
  BB +
  STATIN +
  ACCAHA +
  Bifurcation +
  MultivvDz +
  TotalSTL, data = PTRG)
summary(cox.MACCE, max.risk = Inf)


#
# ################ PSM data ################
# ##########################################
# ##########################################
#
# # data <- read.csv(file = "C:/Users/User/Desktop/PTRG_DES/data/PTRG_DES_20220723_update.csv")
# data <- read.csv(file = "./PTRG_DES_20220723_update.csv")
#
# table(data$CCB)
# temp = data[, c("No", "Age", "Sex", "BMI", "Dx", "Smoking", "HTN", "DM", "Dyslipid", "CKD", "PAD", "CHF", "PRE_MI", "PRE_PCI", "PRE_CVA", "VERI_PRU", "LVEF", "HB", "Cr", "LDL", "ACCAHA", "ASP", "CILO", "RASinh", "BB", "CCB", "STATIN", "CHIP_CRITERIA")]
#
#
# temp$Sex = ifelse(temp$Sex == 1, 1, 0)
# temp$Sex = as.factor(temp$Sex)
# temp$Dx = as.factor(temp$Dx)
# temp$Smoking = as.factor(temp$Smoking)
# temp$DM = as.factor(temp$DM)
# temp$HTN = as.factor(temp$HTN)
# temp$CCB = as.factor(temp$CCB)
# str(temp)
#
# temp_1 = na.omit(temp)
# nrow(temp_1)
#
# set.seed(2022)
# p1 = matchit(CCB ~ Age +
#   Sex +
#   BMI +
#   Dx +
#   Smoking +
#   HTN +
#   DM +
#   Dyslipid +
#   CKD +
#   PAD +
#   CHF +
#   PRE_MI +
#   PRE_PCI +
#   PRE_CVA +
#   VERI_PRU +
#   LVEF +
#   HB +
#   Cr +
#   LDL +
#   ACCAHA +
#   ASP +
#   CILO +
#   RASinh +
#   BB +
#   STATIN +
#   CHIP_CRITERIA, data = temp_1, distance = "logit", ratio = 1, method = "optimal")
# summary(p1)
#
# match_data = match.data(p1)
# table(match_data$subclass)
# names(match_data)
#
# temp_2 = merge(match_data[, c("No", "subclass")], data, by = "No")
# summary(temp_2)
# nrow(temp_2)
#
# write.csv(temp_2, "PSMdata.csv")
#
#
# ##### PSM table1 #####
# mytable(CCB ~ ., data = temp_2, method = 1, catMethod = 0)
# AA <- mytable(CCB ~ ., data = temp_2, method = 1, catMethod = 0)
# mycsv(AA, file = "PSMbaseline.csv")
#
#
# ### PSM SMD 구하기###
# library(tableone)
# library(cobalt)
# library(tools)
# install.packages("metafor")
# library(metafor)
#
#
# xvars <- c("No", "ACCAHA", "MultivvDz", "Hba1c", "PRU_over208", "STATIN", "CILO", "CCB", "Age", "Sex", "BMI", "Dx", "AMI", "Smoking", "DM", "HTN", "Dyslipid", "CKD", "PAD", "CHF", "PRE_MI", "PRE_PCI", "PRE_CABG", "PRE_CVA", "VERI_PRU", "LVEF", "WBC", "HB", "PLATELET", "Cr", "TC", "TG", "HDL", "LDL", "TotalNoST", "ASP", "BB", "RASinh", "HBR_score", "CHIP_CRITERIA")
# table <- CreateTableOne(vars = xvars, strata = "CCB", data = temp_2, test = FALSE)
# print(table, smd = TRUE)
#
#
# ##### PSM table2 #####
# cox.MACCE = coxph(Surv(MACCE2_d, MACCE2 == 1) ~ CCB, data = temp_2)
# summary(cox.MACCE)
# cox.death = coxph(Surv(Death_d, CV_DEATH == 1) ~ CCB, data = temp_2)
# summary(cox.death)
# cox.MI = coxph(Surv(MI_d, TOTAL_MI == 1) ~ CCB, data = temp_2)
# summary(cox.MI)
# cox.ST = coxph(Surv(ST_d, ST_YN == 1) ~ CCB, data = temp_2)
# summary(cox.ST)
# cox.CVA = coxph(Surv(CVA_d, CVA == 1) ~ CCB, data = temp_2)
# summary(cox.CVA)
# cox.RR = coxph(Surv(RR_d, Revascularization == 1) ~ CCB, data = temp_2)
# summary(cox.RR)
# cox.BL = coxph(Surv(BL_day, BL_YN == 1) ~ CCB, data = temp_2)
# summary(cox.BL)
#
#
# ###PSM figure2###
#
# fit <- survfit(Surv(MACCE2_d, MACCE2 == 1) ~ CCB, data = temp_2)
# fit
# ggsurvplot(fit, title = "MACCE2", data = temp_2,
#            fun = "cumhaz", risk.table = TRUE, conf.int = FALSE,
#            legend = "right", legend.labs = c("no CCB", "CCB"), palette = c("gray 39", "orangered"),
#            xlim = c(0, 1825), break.time.by = 365, xlab = "Follow-up duration (days)",
#            ylim = c(0, 0.2), break.by = 0.05)
#
# fit <- survfit(Surv(Death_d, CV_DEATH == 1) ~ CCB, data = temp_2)
# fit
# ggsurvplot(fit, title = "CV death", data = temp_2,
#            fun = "cumhaz", risk.table = TRUE, conf.int = FALSE,
#            legend = "right", legend.labs = c("no CCB", "CCB"), palette = c("gray 39", "orangered"),
#            xlim = c(0, 1825), break.time.by = 365, xlab = "Follow-up duration (days)",
#            ylim = c(0, 0.1), break.by = 0.05)
#
# fit <- survfit(Surv(MI_d, TOTAL_MI == 1) ~ CCB, data = temp_2)
# fit
# ggsurvplot(fit, title = "Myocardial infarction", data = temp_2,
#            fun = "cumhaz", risk.table = TRUE, conf.int = FALSE,
#            legend = "right", legend.labs = c("no CCB", "CCB"), palette = c("gray 39", "orangered"),
#            xlim = c(0, 1825), break.time.by = 365, xlab = "Follow-up duration (days)",
#            ylim = c(0, 0.1), break.by = 0.05)
#
# fit <- survfit(Surv(ST_d, ST_YN == 1) ~ CCB, data = temp_2)
# fit
# ggsurvplot(fit, title = "Stent thrombosis", data = temp_2,
#            fun = "cumhaz", risk.table = TRUE, conf.int = FALSE,
#            legend = "right", legend.labs = c("no CCB", "CCB"), palette = c("gray 39", "orangered"),
#            xlim = c(0, 1825), break.time.by = 365, xlab = "Follow-up duration (days)",
#            ylim = c(0, 0.1), break.by = 0.05)
#
# fit <- survfit(Surv(CVA_d, CVA == 1) ~ CCB, data = temp_2)
# fit
# ggsurvplot(fit, title = "Stroke", data = temp_2,
#            fun = "cumhaz",
#            risk.table = TRUE, conf.int = FALSE,
#            legend = "right", legend.labs = c("no CCB", "CCB"), palette = c("gray 39", "orangered"),
#            xlim = c(0, 1825), break.time.by = 365, xlab = "Follow-up duration (days)",
#            ylim = c(0, 0.1), break.by = 0.05)
#
# fit <- survfit(Surv(BL_day, BL_YN == 1) ~ CCB, data = temp_2)
# fit
# ggsurvplot(fit, title = "Bleeding", data = temp_2,
#            fun = "cumhaz",
#            risk.table = TRUE, conf.int = FALSE,
#            legend = "right", legend.labs = c("no CCB", "CCB"), palette = c("gray 39", "orangered"),
#            xlim = c(0, 1825), break.time.by = 365, xlab = "Follow-up duration (days)",
#            ylim = c(0, 0.1), break.by = 0.05)


############## IPTW ################
####################################
####################################
####################################
# data <- read.csv(file = "./PTRG_DES_20220723_update.csv")
data <- read.csv(file = fileInfo)

# table(data$CCB)
# temp = data[, c("No", "Age", "Sex", "BMI", "Dx", "Smoking", "HTN", "DM", "Dyslipid", "CKD", "PAD", "CHF", "PRE_MI", "PRE_PCI", "PRE_CVA", "VERI_PRU", "LVEF", "HB", "Cr", "LDL", "ACCAHA", "ASP", "CILO", "RASinh", "BB", "CCB", "STATIN", "CHIP_CRITERIA")]
# temp = data[, c("No", "Age", "Sex", "BMI", "Dx", "AMI", "Smoking", "DM", "HTN", "Dyslipid", "CKD", "PAD", "CHF", "PRE_MI", "PRE_PCI", "PRE_CABG", "PRE_CVA", "VERI_PRU", "LVEF", "WBC", "HB", "PLATELET", "Cr", "TC", "TG", "HDL", "LDL", "hsCRP", "Hba1c", "ACCAHA", "TotalNoST", "Bifurcation", "CTO", "ASP", "CILO", "RASinh", "BB", "CCB", "STATIN", "PPI", "CHIP_CRITERIA")]


# covariates = c("No", "Age", "Sex", "BMI", "Dx", "AMI", "Smoking", "DM", "HTN", "Dyslipid", "CKD", "PAD", "CHF", "PRE_MI", "PRE_PCI", "PRE_CABG", "PRE_CVA", "VERI_PRU", "LVEF", "WBC", "HB", "PLATELET", "Cr", "TC", "TG", "HDL", "LDL", "hsCRP", "Hba1c", "ACCAHA", "TotalNoST", "Bifurcation", "CTO", "ASP", "CILO", "RASinh", "BB", "CCB", "STATIN", "PPI", "CHIP_CRITERIA")

temp = data %>%
  dplyr::select(c("No", "Age", "Sex", "BMI", "Dx", "AMI", "Smoking", "DM", "HTN", "Dyslipid", "CKD", "PAD", "CHF", "PRE_MI", "PRE_PCI", "PRE_CABG", "PRE_CVA", "VERI_PRU", "LVEF", "WBC", "HB", "PLATELET", "Cr", "TC", "TG", "HDL", "LDL", "hsCRP", "Hba1c", "ACCAHA", "TotalNoST", "Bifurcation", "CTO", "ASP", "CILO", "RASinh", "BB", "CCB", "STATIN", "PPI", "CHIP_CRITERIA")) %>%
  dplyr::mutate(
    CCB = as.integer(CCB)
  ) %>%
  na.omit()


# temp$CCB = as.integer(temp$CCB)
# str(temp)
# table(temp$CCB)
# temp$CCB = ifelse(temp$CCB == 2, 1, 0)


p1 = twang::ps(CCB ~ Age +
  Sex +
  BMI +
  Dx +
  Smoking +
  HTN +
  DM +
  Dyslipid +
  CKD +
  PAD +
  CHF +
  PRE_MI +
  PRE_PCI +
  PRE_CVA +
  VERI_PRU +
  LVEF +
  HB +
  Cr +
  LDL +
  ACCAHA +
  ASP +
  CILO +
  RASinh +
  BB +
  STATIN +
  CHIP_CRITERIA, data = temp, stop.method = c("es.mean"), estimand = "ATE")


# ps_model = glm(CCB ~ Age +
#                  Sex +
#                  BMI +
#                  Dx +
#                  Smoking +
#                  HTN +
#                  DM +
#                  Dyslipid +
#                  CKD +
#                  PAD +
#                  CHF +
#                  PRE_MI +
#                  PRE_PCI +
#                  PRE_CVA +
#                  VERI_PRU +
#                  LVEF +
#                  HB +
#                  Cr +
#                  LDL +
#                  ACCAHA +
#                  ASP +
#                  CILO +
#                  RASinh +
#                  BB +
#                  STATIN +
#                  CHIP_CRITERIA, data = tempL1)

# summary(ps_model)
#
# match_it_obj <- matchit(CCB ~ Age +
#                           Sex +
#                           BMI +
#                           Dx +
#                           Smoking +
#                           HTN +
#                           DM +
#                           Dyslipid +
#                           CKD +
#                           PAD +
#                           CHF +
#                           PRE_MI +
#                           PRE_PCI +
#                           PRE_CVA +
#                           VERI_PRU +
#                           LVEF +
#                           HB +
#                           Cr +
#                           LDL +
#                           ACCAHA +
#                           ASP +
#                           CILO +
#                           RASinh +
#                           BB +
#                           STATIN +
#                           CHIP_CRITERIA, data = tempL1, method = "quick", weights = "iptw", estimand = "ATT", model = ps_model)
#
# summary(match_it_obj)
#
# # 가중치가 적용된 데이터를 얻습니다.
# s <- match.data(match_it_obj)
#
# # weighted_data$
#
# # survival 객체를 생성합니다.
# # 여기서는 time 변수가 생존 시간을, status 변수가 censoring 여부를 나타냅니다.
# surv_obj <- Surv(time = weighted_data$CCB, event = weighted_data$ipw)
#
# # Kaplan-Meier 추정치를 계산합니다.
# km_fit <- survfit(surv_obj ~ treat, data = weighted_data)
#
# # Kaplan-Meier 곡선을 그립니다.
# plot(km_fit, lty = 2:3)


temp$ipw = get.weights(p1, stop.method = "es.mean")

sum(temp$ipw, na.rm = TRUE)

data_iptw = merge(data, temp[, c("No", "ipw")], by = "No")
data_iptw
nrow(data_iptw)


# weighteddata <- svydesign(ids = ~1, data = data_iptw, weights = ~ipw)
# nrow(weighteddata)
# summary(weighteddata)
# 
# 
# # Weighted Table 1
# xvars <- c("No", "ACCAHA", "MultivvDz", "Hba1c", "PRU_over208", "STATIN", "CILO", "CCB", "Age", "Sex", "BMI", "Dx", "AMI", "Smoking", "DM", "HTN", "Dyslipid", "CKD", "PAD", "CHF", "PRE_MI", "PRE_PCI", "PRE_CABG", "PRE_CVA", "VERI_PRU", "LVEF", "WBC", "HB", "PLATELET", "Cr", "TC", "TG", "HDL", "LDL", "TotalNoST", "ASP", "BB", "RASinh", "HBR_score", "CHIP_CRITERIA")
# weightedtable <- svyCreateTableOne(vars = xvars, strata = "CCB", data = weighteddata, test = FALSE)
# 
# # Show Table with SMD
# print(weightedtable, smd = TRUE)
# 

# ##### IPTW table2 #####
# cox.MACCE <- svycoxph(Surv(MACCE2_d, MACCE2 == 1) ~ CCB, design = weighteddata)
# summary(cox.MACCE)
# cox.death <- svycoxph(Surv(Death_d, CV_DEATH == 1) ~ CCB, design = weighteddata)
# summary(cox.death)
# cox.MI <- svycoxph(Surv(MI_d, TOTAL_MI == 1) ~ CCB, design = weighteddata)
# summary(cox.MI)
# cox.ST <- svycoxph(Surv(ST_d, ST_YN == 1) ~ CCB, design = weighteddata)
# summary(cox.ST)
# cox.CVA <- svycoxph(Surv(CVA_d, CVA == 1) ~ CCB, design = weighteddata)
# summary(cox.CVA)
# cox.RR <- svycoxph(Surv(RR_d, Revascularization == 1) ~ CCB, design = weighteddata)
# summary(cox.RR)
# cox.BL <- svycoxph(Surv(BL_day, BL_YN == 1) ~ CCB, design = weighteddata)
# summary(cox.BL)
# 
# 
# weighteddata <- svydesign(ids = ~1, data = data_iptw, weights = ~ipw)
# nrow(weighteddata)
# summary(weighteddata)
# 
# # Cox 비례 위험 모형 적합
# fit <- svycoxph(Surv(MACCE2_d, MACCE2 == 1) ~ CCB, design = weighteddata)
# summary(fit)

# # Kaplan-Meier 곡선 그리기
# ggsurvplot(survfit(fit), title = "Bleeding", data = weighteddata,
#            fun = "cumhaz", risk.table = TRUE, conf.int = FALSE,
#            palette = c("gray 39", "orangered"), xlim = c(0, 1825),
#            break.time.by = 365, xlab = "Follow-up duration (days)",
#            ylim = c(0, 0.1), break.by = 0.05)


# create a survival object
Surv_obj = Surv(data_iptw$MACCE2_d, data_iptw$MACCE2 == 1)

# fit a Cox model
# fit_unweighted = coxph(Surv_obj ~ CCB, data = data_iptw)

# create weights
weights = ifelse(data_iptw$CCB == 1, 1 / temp$ipw, 1)

# fit a weighted Cox model
fit_weighted = coxph(Surv_obj ~ CCB, data = data_iptw, weights = weights)

# generate Kaplan-Meier curves
kmfit_weighted = survfit(fit_weighted)
# kmfit_unweighted = survfit(fit_unweighted)

fits = list("weighted" = kmfit_weighted, "unweighted" = kmfit_unweighted)


# fit <- survfit(Surv(MACCE2_d, MACCE2 == 1) ~ CCB, data = temp_2)
# fit
# ggsurvplot(fit, title = "MACCE2", data = temp_2,
#            fun = "cumhaz", risk.table = TRUE, conf.int = FALSE,
#            legend = "right", legend.labs = c("no CCB", "CCB"), palette = c("gray 39", "orangered"),
#            xlim = c(0, 1825), break.time.by = 365, xlab = "Follow-up duration (days)",
#            ylim = c(0, 0.2), break.by = 0.05)

plotSubTitle = sprintf("%s", "IPTW에 따른 kaplan-meier 곡선")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

makePlot = survminer::ggsurvplot(fits, title = "Bleeding", data = weighteddata,
                                 fun = "cumhaz", risk.table = TRUE, conf.int = FALSE,
                                 palette = c("gray 39", "orangered"), xlim = c(0, 1825),
                                 break.time.by = 365, xlab = "Follow-up duration (days)",
                                 ylim = c(0, 0.1), break.by = 0.05, combine = TRUE)

mainPlot = cowplot::plot_grid(makePlot$plot, makePlot$table, align = "v", ncol = 1, rel_heights = c(3, 1))
ggsave(plot = mainPlot, filename = saveImg, width = 10, height = 8, dpi = 600)