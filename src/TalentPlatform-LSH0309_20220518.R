
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

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0309"
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
library(tidyverse)
library(lubridate)
library(openxlsx)
library(dplyr)
#library(wiqid)
library(dplyr)
library(fitdistrplus)
library(stats)

# install.packages("hydroGOF",dependencies = TRUE)

library(hydroGOF)
# setwd("D:/downdown")

# modprecip=data=read.csv("22_IDW_pr_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_19850101-20141231.csv")[,-1]
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "22_IDW_pr_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_19850101-20141231.csv"))
# modprecip=data=read.csv(fileInfo, encoding = "EUC-KR")[,-1]
modprecip=readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))[,-1]
modprecip = modprecip %>% 
  as.data.frame()

# obsprecip=data1=read.csv("22_real pr 1985-2014 (1).csv")[,-1]
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "22_real+pr+1985-2014.csv"))
# obsprecip=data1=read.csv(fileInfo)[,-1]
obsprecip=readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))[,-1]
obsprecip = obsprecip %>% 
  as.data.frame()

######## Double gamma distribution 
########1%-90%와 90%-100%으로 구분 
Q1<-c()
OBS<-c()
#######

quan1=data.frame(sort(modprecip[,1]))
quan2=data.frame(sort(modprecip[,2]))
quan3=data.frame(sort(modprecip[,3]))
quan4=data.frame(sort(modprecip[,4]))
quan5=data.frame(sort(modprecip[,5]))
quan6=data.frame(sort(modprecip[,6]))
quan7=data.frame(sort(modprecip[,7]))
quan8=data.frame(sort(modprecip[,8]))
quan9=data.frame(sort(modprecip[,9]))
quan10=data.frame(sort(modprecip[,10]))
quan11=data.frame(sort(modprecip[,11]))
quan12=data.frame(sort(modprecip[,12]))
quan13=data.frame(sort(modprecip[,13]))
quan14=data.frame(sort(modprecip[,14]))
quan15=data.frame(sort(modprecip[,15]))
quan16=data.frame(sort(modprecip[,16]))
quan17=data.frame(sort(modprecip[,17]))
quan18=data.frame(sort(modprecip[,18]))
quan19=data.frame(sort(modprecip[,19]))
quan20=data.frame(sort(modprecip[,20]))
quan21=data.frame(sort(modprecip[,21]))
quan22=data.frame(sort(modprecip[,22]))

LL2<-cbind(quan1,quan2,quan3,quan4,quan5,quan6,quan7,quan8,quan9,quan10,quan11,quan12,quan13,quan14,quan15,quan16,quan17,quan18,quan19,quan20,quan21,quan22)

obsquan1=data.frame(sort(obsprecip[,1]))
obsquan2=data.frame(sort(obsprecip[,2]))
obsquan3=data.frame(sort(obsprecip[,3]))
obsquan4=data.frame(sort(obsprecip[,4]))
obsquan5=data.frame(sort(obsprecip[,5]))
obsquan6=data.frame(sort(obsprecip[,6]))
obsquan7=data.frame(sort(obsprecip[,7]))
obsquan8=data.frame(sort(obsprecip[,8]))
obsquan9=data.frame(sort(obsprecip[,9]))
obsquan10=data.frame(sort(obsprecip[,10]))
obsquan11=data.frame(sort(obsprecip[,11]))
obsquan12=data.frame(sort(obsprecip[,12]))
obsquan13=data.frame(sort(obsprecip[,13]))
obsquan14=data.frame(sort(obsprecip[,14]))
obsquan15=data.frame(sort(obsprecip[,15]))
obsquan16=data.frame(sort(obsprecip[,16]))
obsquan17=data.frame(sort(obsprecip[,17]))
obsquan18=data.frame(sort(obsprecip[,18]))
obsquan19=data.frame(sort(obsprecip[,19]))
obsquan20=data.frame(sort(obsprecip[,20]))
obsquan21=data.frame(sort(obsprecip[,21]))
obsquan22=data.frame(sort(obsprecip[,22]))

LL3<-cbind(obsquan1,obsquan2,obsquan3,obsquan4,obsquan5,obsquan6,obsquan7,obsquan8,obsquan9,obsquan10,obsquan11,obsquan12,obsquan13,obsquan14,obsquan15,obsquan16,obsquan17,obsquan18,obsquan19,obsquan20,obsquan21,obsquan22)


BB1<-c()
for (i in 1:22) {
  gg=replace(LL3[,i],which(LL3[,i]<=0.5),0)
  BB1<- cbind(BB1,gg)
}


BL1<-c()
for (i in 1:22) {
  gg=replace(LL2[,i],which(LL2[,i]<=0.5),0)
  BL1<- cbind(BL1,gg)
}


Q1 = data.frame(BL1)
colnames(Q1) <- colnames(obsprecip)[1:22]
OBS = data.frame(BB1)
colnames(OBS) <- colnames(obsprecip)[1:22]



QC1 = max(which(Q1[,1] == 0)) 
MAXQ1=length(Q1[,1])
UZ1=data.frame(Q1[QC1:MAXQ1,1])

QC2 = max(which(Q1[,2] == 0)) 
MAXQ2=length(Q1[,2])
UZ2=data.frame(Q1[QC2:MAXQ2,2])

QC3 = max(which(Q1[,3] == 0)) 
MAXQ3=length(Q1[,3])
UZ3=data.frame(Q1[QC3:MAXQ3,3])

QC4 = max(which(Q1[,4] == 0)) 
MAXQ4=length(Q1[,4])
UZ4=data.frame(Q1[QC4:MAXQ4,4])

QC5 = max(which(Q1[,5] == 0)) 
MAXQ5=length(Q1[,5])
UZ5=data.frame(Q1[QC5:MAXQ5,5])

QC6 = max(which(Q1[,6] == 0)) 
MAXQ6=length(Q1[,6])
UZ6=data.frame(Q1[QC6:MAXQ6,6])

QC7 = max(which(Q1[,7] == 0)) 
MAXQ7=length(Q1[,7])
UZ7=data.frame(Q1[QC7:MAXQ7,7])

QC8 = max(which(Q1[,8] == 0)) 
MAXQ8=length(Q1[,8])
UZ8=data.frame(Q1[QC8:MAXQ8,8])

QC9 = max(which(Q1[,9] == 0)) 
MAXQ9=length(Q1[,9])
UZ9=data.frame(Q1[QC9:MAXQ9,9])

QC10 = max(which(Q1[,10] == 0)) 
MAXQ10=length(Q1[,10])
UZ10=data.frame(Q1[QC10:MAXQ10,10])

QC11 = max(which(Q1[,11] == 0)) 
MAXQ11=length(Q1[,11])
UZ11=data.frame(Q1[QC11:MAXQ11,11])

QC12 = max(which(Q1[,12] == 0)) 
MAXQ12=length(Q1[,12])
UZ12=data.frame(Q1[QC12:MAXQ12,12])

QC13 = max(which(Q1[,13] == 0)) 
MAXQ13=length(Q1[,13])
UZ13=data.frame(Q1[QC13:MAXQ13,13])

QC14 = max(which(Q1[,14] == 0)) 
MAXQ14=length(Q1[,14])
UZ14=data.frame(Q1[QC14:MAXQ14,14])

QC15 = max(which(Q1[,15] == 0)) 
MAXQ15=length(Q1[,15])
UZ15=data.frame(Q1[QC15:MAXQ15,15])

QC16 = max(which(Q1[,16] == 0)) 
MAXQ16=length(Q1[,16])
UZ16=data.frame(Q1[QC16:MAXQ16,16])

QC17 = max(which(Q1[,17] == 0)) 
MAXQ17=length(Q1[,17])
UZ17=data.frame(Q1[QC17:MAXQ17,17])

QC18 = max(which(Q1[,18] == 0)) 
MAXQ18=length(Q1[,18])
UZ18=data.frame(Q1[QC18:MAXQ18,18])

QC19 = max(which(Q1[,19] == 0)) 
MAXQ19=length(Q1[,19])
UZ19=data.frame(Q1[QC19:MAXQ19,19])

QC20 = max(which(Q1[,20] == 0)) 
MAXQ20=length(Q1[,20])
UZ20=data.frame(Q1[QC20:MAXQ20,20])

QC21 = max(which(Q1[,21] == 0)) 
MAXQ21=length(Q1[,21])
UZ21=data.frame(Q1[QC21:MAXQ21,21])

QC22 = max(which(Q1[,22] == 0)) 
MAXQ22=length(Q1[,22])
UZ22=data.frame(Q1[QC22:MAXQ22,22])


#######################

MAXOBS=length(OBS[,1])
OBZ1=data.frame(OBS[QC1:MAXOBS,1])

MAXQ2=length(OBS[,2])
OBZ2=data.frame(OBS[QC2:MAXQ2,2])

MAXQ3=length(OBS[,3])
OBZ3=data.frame(OBS[QC3:MAXQ3,3])

MAXQ4=length(OBS[,4])
OBZ4=data.frame(OBS[QC4:MAXQ4,4])

MAXQ5=length(OBS[,5])
OBZ5=data.frame(OBS[QC5:MAXQ5,5])

MAXQ6=length(OBS[,6])
OBZ6=data.frame(OBS[QC6:MAXQ6,6])

MAXQ7=length(OBS[,7])
OBZ7=data.frame(OBS[QC7:MAXQ7,7])

MAXQ8=length(OBS[,8])
OBZ8=data.frame(OBS[QC8:MAXQ8,8])

MAXQ9=length(OBS[,9])
OBZ9=data.frame(OBS[QC9:MAXQ9,9])

MAXOBS0=length(OBS[,10])
OBZ10=data.frame(OBS[QC10:MAXOBS0,10])

MAXOBS1=length(OBS[,11])
OBZ11=data.frame(OBS[QC11:MAXOBS1,11])

MAXOBS2=length(OBS[,12])
OBZ12=data.frame(OBS[QC12:MAXOBS2,12])

MAXOBS3=length(OBS[,13])
OBZ13=data.frame(OBS[QC13:MAXOBS3,13])

MAXOBS4=length(OBS[,14])
OBZ14=data.frame(OBS[QC14:MAXOBS4,14])

MAXOBS5=length(OBS[,15])
OBZ15=data.frame(OBS[QC15:MAXOBS5,15])

MAXOBS6=length(OBS[,16])
OBZ16=data.frame(OBS[QC16:MAXOBS6,16])

MAXOBS7=length(OBS[,17])
OBZ17=data.frame(OBS[QC17:MAXOBS7,17])

MAXOBS8=length(OBS[,18])
OBZ18=data.frame(OBS[QC18:MAXOBS8,18])

MAXOBS9=length(OBS[,19])
OBZ19=data.frame(OBS[QC19:MAXOBS9,19])

MAXQ20=length(OBS[,20])
OBZ20=data.frame(OBS[QC20:MAXQ20,20])

MAXQ21=length(OBS[,21])
OBZ21=data.frame(OBS[QC21:MAXQ21,21])

MAXQ22=length(OBS[,22])
OBZ22=data.frame(OBS[QC22:MAXQ22,22])


UZ1=data.frame(replace(UZ1[,1],which(UZ1[,1] <= 0.5),0.001))
UZ2=data.frame(replace(UZ2[,1],which(UZ2[,1] <= 0.5),0.001))
UZ3=data.frame(replace(UZ3[,1],which(UZ3[,1] <= 0.5),0.001))
UZ4=data.frame(replace(UZ4[,1],which(UZ4[,1] <= 0.5),0.001))
UZ5=data.frame(replace(UZ5[,1],which(UZ5[,1] <= 0.5),0.001))
UZ6=data.frame(replace(UZ6[,1],which(UZ6[,1] <= 0.5),0.001))
UZ7=data.frame(replace(UZ7[,1],which(UZ7[,1] <= 0.5),0.001))
UZ8=data.frame(replace(UZ8[,1],which(UZ8[,1] <= 0.5),0.001))
UZ9=data.frame(replace(UZ9[,1],which(UZ9[,1] <= 0.5),0.001))
UZ10=data.frame(replace(UZ10[,1],which(UZ10[,1] <= 0.5),0.001))
UZ11=data.frame(replace(UZ11[,1],which(UZ11[,1] <= 0.5),0.001))
UZ12=data.frame(replace(UZ12[,1],which(UZ12[,1] <= 0.5),0.001))
UZ13=data.frame(replace(UZ13[,1],which(UZ13[,1] <= 0.5),0.001))
UZ14=data.frame(replace(UZ14[,1],which(UZ14[,1] <= 0.5),0.001))
UZ15=data.frame(replace(UZ15[,1],which(UZ15[,1] <= 0.5),0.001))
UZ16=data.frame(replace(UZ16[,1],which(UZ16[,1] <= 0.5),0.001))
UZ17=data.frame(replace(UZ17[,1],which(UZ17[,1] <= 0.5),0.001))
UZ18=data.frame(replace(UZ18[,1],which(UZ18[,1] <= 0.5),0.001))
UZ19=data.frame(replace(UZ19[,1],which(UZ19[,1] <= 0.5),0.001))
UZ20=data.frame(replace(UZ20[,1],which(UZ20[,1] <= 0.5),0.001))
UZ21=data.frame(replace(UZ21[,1],which(UZ21[,1] <= 0.5),0.001))
UZ22=data.frame(replace(UZ22[,1],which(UZ22[,1] <= 0.5),0.001))


OBZ1=data.frame(replace(OBZ1[,1],which(OBZ1[,1] <= 0.5), 0.001))
OBZ2=data.frame(replace(OBZ2[,1],which(OBZ2[,1] <= 0.5), 0.001))
OBZ3=data.frame(replace(OBZ3[,1],which(OBZ3[,1] <= 0.5), 0.001))
OBZ4=data.frame(replace(OBZ4[,1],which(OBZ4[,1] <= 0.5), 0.001))
OBZ5=data.frame(replace(OBZ5[,1],which(OBZ5[,1] <= 0.5), 0.001))
OBZ6=data.frame(replace(OBZ6[,1],which(OBZ6[,1] <= 0.5), 0.001))
OBZ7=data.frame(replace(OBZ7[,1],which(OBZ7[,1] <= 0.5), 0.001))
OBZ8=data.frame(replace(OBZ8[,1],which(OBZ8[,1] <= 0.5), 0.001))
OBZ9=data.frame(replace(OBZ9[,1],which(OBZ9[,1] <= 0.5), 0.001))
OBZ10=data.frame(replace(OBZ10[,1],which(OBZ10[,1] <= 0.5), 0.001))
OBZ11=data.frame(replace(OBZ11[,1],which(OBZ11[,1] <= 0.5), 0.001))
OBZ12=data.frame(replace(OBZ12[,1],which(OBZ12[,1] <= 0.5), 0.001))
OBZ13=data.frame(replace(OBZ13[,1],which(OBZ13[,1] <= 0.5), 0.001))
OBZ14=data.frame(replace(OBZ14[,1],which(OBZ14[,1] <= 0.5), 0.001))
OBZ15=data.frame(replace(OBZ15[,1],which(OBZ15[,1] <= 0.5), 0.001))
OBZ16=data.frame(replace(OBZ16[,1],which(OBZ16[,1] <= 0.5), 0.001))
OBZ17=data.frame(replace(OBZ17[,1],which(OBZ17[,1] <= 0.5), 0.001))
OBZ18=data.frame(replace(OBZ18[,1],which(OBZ18[,1] <= 0.5), 0.001))
OBZ19=data.frame(replace(OBZ19[,1],which(OBZ19[,1] <= 0.5), 0.001))
OBZ20=data.frame(replace(OBZ20[,1],which(OBZ20[,1] <= 0.5), 0.001))
OBZ21=data.frame(replace(OBZ21[,1],which(OBZ21[,1] <= 0.5), 0.001))
OBZ22=data.frame(replace(OBZ22[,1],which(OBZ22[,1] <= 0.5), 0.001))


# LSH

iCount = 1
# for (iCount in 1:ncol(obsprecip)) {
for (iCount in 2:2) {
  obzInfo = sprintf("OBZ%s", iCount)
  uzInfo = sprintf("UZ%s", iCount)
  qcInfo = sprintf("QC%s", iCount)
  
  cat(obzInfo, "\n")
  cat(uzInfo, "\n")
  
  OBZ1 = get(obzInfo)
  UZ1 = get(uzInfo)
  QC = get(qcInfo)
  
  
  UU = 1:length(OBZ1[,1])
  loc = data.frame(round(quantile(UU, probs = c(0.7,0.71,0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,0.80,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88,0.89,0.90,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,1.00)), digits = 0))
  
  a1 = loc[1,1]
  a2 = loc[2,1]
  a3 = loc[3,1]
  a4 = loc[4,1]
  a5 = loc[5,1]
  a6 = loc[6,1]
  a7 = loc[7,1]
  a8 = loc[8,1]
  a9 = loc[9,1]
  a10 = loc[10,1]
  a11 = loc[11,1]
  a12 = loc[12,1]
  a13 = loc[13,1]
  a14 = loc[14,1]
  a15 = loc[15,1]
  a16 = loc[16,1]
  a17 = loc[17,1]
  a18 = loc[18,1]
  a19 = loc[19,1]
  a20 = loc[20,1]
  a21 = loc[21,1]
  a22 = loc[22,1]
  a23 = loc[23,1]
  a24 = loc[24,1]
  a25 = loc[25,1]
  a26 = loc[26,1]
  a27 = loc[27,1]
  a28 = loc[28,1]
  a29 = loc[29,1]
  a30 = loc[30,1]
  a31 = loc[31,1]
  
  
  
  
  cl1=data.frame(UZ1[a1:length(OBZ1[,1]),])
  
  cl2=data.frame(UZ1[a2:length(OBZ1[,1]),])
  
  cl3=data.frame(UZ1[a3:length(OBZ1[,1]),])
  
  cl4=data.frame(UZ1[a4:length(OBZ1[,1]),])
  
  cl5=data.frame(UZ1[a5:length(OBZ1[,1]),])
  
  cl6=data.frame(UZ1[a6:length(OBZ1[,1]),])
  
  cl7=data.frame(UZ1[a7:length(OBZ1[,1]),])
  
  cl8=data.frame(UZ1[a8:length(OBZ1[,1]),])
  
  cl9=data.frame(UZ1[a9:length(OBZ1[,1]),])
  
  cl10=data.frame(UZ1[a10:length(OBZ1[,1]),])
  
  cl11=data.frame(UZ1[a11:length(OBZ1[,1]),])
  
  cl12=data.frame(UZ1[a12:length(OBZ1[,1]),])
  
  cl13=data.frame(UZ1[a13:length(OBZ1[,1]),])
  
  cl14=data.frame(UZ1[a14:length(OBZ1[,1]),])
  
  cl15=data.frame(UZ1[a15:length(OBZ1[,1]),])
  
  cl16=data.frame(UZ1[a16:length(OBZ1[,1]),])
  
  cl17=data.frame(UZ1[a17:length(OBZ1[,1]),])
  
  cl18=data.frame(UZ1[a18:length(OBZ1[,1]),])
  
  cl19=data.frame(UZ1[a19:length(OBZ1[,1]),])
  
  cl20=data.frame(UZ1[a20:length(OBZ1[,1]),])
  
  cl21=data.frame(UZ1[a21:length(OBZ1[,1]),])
  
  cl22=data.frame(UZ1[a22:length(OBZ1[,1]),])
  
  cl23=data.frame(UZ1[a23:length(OBZ1[,1]),])
  
  cl24=data.frame(UZ1[a24:length(OBZ1[,1]),])
  
  cl25=data.frame(UZ1[a25:length(OBZ1[,1]),])
  
  cl26=data.frame(UZ1[a26:length(OBZ1[,1]),])
  
  cl27=data.frame(UZ1[a27:length(OBZ1[,1]),])
  
  cl28=data.frame(UZ1[a28:length(OBZ1[,1]),])
  
  cl29=data.frame(UZ1[a29:length(OBZ1[,1]),])
  
  cl30=data.frame(UZ1[a30:length(OBZ1[,1]),])
  
  cl31=data.frame(UZ1[a31:length(OBZ1[,1]),])
  
  
  cll1=data.frame(UZ1[1:a1-1,])
  
  cll2=data.frame(UZ1[1:a2-1,])
  
  cll3=data.frame(UZ1[1:a3-1,])
  
  cll4=data.frame(UZ1[1:a4-1,])
  
  cll5=data.frame(UZ1[1:a5-1,])
  
  cll6=data.frame(UZ1[1:a6-1,])
  
  cll7=data.frame(UZ1[1:a7-1,])
  
  cll8=data.frame(UZ1[1:a8-1,])
  
  cll9=data.frame(UZ1[1:a9-1,])
  
  cll10=data.frame(UZ1[1:a10-1,])
  
  cll11=data.frame(UZ1[1:a11-1,])
  
  cll12=data.frame(UZ1[1:a12-1,])
  
  cll13=data.frame(UZ1[1:a13-1,])
  
  cll14=data.frame(UZ1[1:a14-1,])
  
  cll15=data.frame(UZ1[1:a15-1,])
  
  cll16=data.frame(UZ1[1:a16-1,])
  
  cll17=data.frame(UZ1[1:a17-1,])
  
  cll18=data.frame(UZ1[1:a18-1,])
  
  cll19=data.frame(UZ1[1:a19-1,])
  
  cll20=data.frame(UZ1[1:a20-1,])
  
  cll21=data.frame(UZ1[1:a21-1,])
  
  cll22=data.frame(UZ1[1:a22-1,])
  
  cll23=data.frame(UZ1[1:a23-1,])
  
  cll24=data.frame(UZ1[1:a24-1,])
  
  cll25=data.frame(UZ1[1:a25-1,])
  
  cll26=data.frame(UZ1[1:a26-1,])
  
  cll27=data.frame(UZ1[1:a27-1,])
  
  cll28=data.frame(UZ1[1:a28-1,])
  
  cll29=data.frame(UZ1[1:a29-1,])
  
  cll30=data.frame(UZ1[1:a30-1,])
  
  cll31=data.frame(UZ1[1:a31-1,])
  
  
  qempty1<-c()
  qempty2<-c()
  qempty3<-c()
  qempty4<-c()
  qempty5<-c()
  qempty6<-c()
  qempty7<-c()
  qempty8<-c()
  qempty9<-c()
  qempty10<-c()
  qempty11<-c()
  qempty12<-c()
  qempty13<-c()
  qempty14<-c()
  qempty15<-c()
  qempty16<-c()
  qempty17<-c()
  qempty18<-c()
  qempty19<-c()
  qempty20<-c()
  qempty21<-c()
  qempty22<-c()
  qempty23<-c()
  qempty23<-c()
  qempty24<-c()
  qempty25<-c()
  qempty26<-c()
  qempty27<-c()
  qempty28<-c()
  qempty29<-c()
  qempty30<-c()
  qempty31<-c()
  
  cul1=cl1[,1]
  cc1<-fitdist(cul1,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty1 = data.frame(pgamma(cul1, meanlog, sdlog))
  
  
  cul2=cl2[,1]
  cc1<-fitdist(cul2,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty2 = data.frame(pgamma(cul2, meanlog, sdlog))
  
  cul3=cl3[,1]
  cc1<-fitdist(cul3,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty3 = data.frame(pgamma(cul3, meanlog, sdlog))
  
  cul4=cl4[,1]
  cc1<-fitdist(cul4,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty4 = data.frame(pgamma(cul4, meanlog, sdlog))
  
  cul5=cl5[,1]
  cc1<-fitdist(cul5,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty5 = data.frame(pgamma(cul5, meanlog, sdlog))
  
  cul6=cl6[,1]
  cc1<-fitdist(cul6,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty6 = data.frame(pgamma(cul6, meanlog, sdlog))
  
  cul7=cl7[,1]
  cc1<-fitdist(cul7,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty7 = data.frame(pgamma(cul7, meanlog, sdlog))
  
  cul8=cl8[,1]
  cc1<-fitdist(cul8,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty8 = data.frame(pgamma(cul8, meanlog, sdlog))
  
  cul9=cl9[,1]
  cc1<-fitdist(cul9,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty9 = data.frame(pgamma(cul9, meanlog, sdlog))
  
  cul10=cl10[,1]
  cc1<-fitdist(cul10,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty10 = data.frame(pgamma(cul10, meanlog, sdlog))
  
  cul11=cl11[,1]
  cc1<-fitdist(cul11,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty11 = data.frame(pgamma(cul11, meanlog, sdlog))
  
  
  cul12=cl12[,1]
  cc1<-fitdist(cul12,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty12 = data.frame(pgamma(cul12, meanlog, sdlog))
  
  cul13=cl13[,1]
  cc1<-fitdist(cul13,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty13 = data.frame(pgamma(cul13, meanlog, sdlog))
  
  cul14=cl14[,1]
  cc1<-fitdist(cul14,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty14 = data.frame(pgamma(cul14, meanlog, sdlog))
  
  cul15=cl15[,1]
  cc1<-fitdist(cul15,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty15 = data.frame(pgamma(cul15, meanlog, sdlog))
  
  cul16=cl16[,1]
  cc1<-fitdist(cul16,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty16 = data.frame(pgamma(cul16, meanlog, sdlog))
  
  cul17=cl17[,1]
  cc1<-fitdist(cul17,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty17 = data.frame(pgamma(cul17, meanlog, sdlog))
  
  cul18=cl18[,1]
  cc1<-fitdist(cul18,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty18 = data.frame(pgamma(cul18, meanlog, sdlog))
  
  cul19=cl19[,1]
  cc1<-fitdist(cul19,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty19 = data.frame(pgamma(cul19, meanlog, sdlog))
  
  cul20=cl20[,1]
  cc1<-fitdist(cul20,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty20 = data.frame(pgamma(cul20, meanlog, sdlog))
  
  cul21=cl21[,1]
  cc1<-fitdist(cul21,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty21 = data.frame(pgamma(cul21, meanlog, sdlog))
  
  cul22=cl22[,1]
  cc1<-fitdist(cul22,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty22 = data.frame(pgamma(cul22, meanlog, sdlog))
  
  cul23=cl23[,1]
  cc1<-fitdist(cul23,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty23 = data.frame(pgamma(cul23, meanlog, sdlog))
  
  cul24=cl24[,1]
  cc1<-fitdist(cul24,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty24 = data.frame(pgamma(cul24, meanlog, sdlog))
  
  cul25=cl25[,1]
  cc1<-fitdist(cul25,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty25 = data.frame(pgamma(cul25, meanlog, sdlog))
  
  cul26=cl26[,1]
  cc1<-fitdist(cul26,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty26 = data.frame(pgamma(cul26, meanlog, sdlog))
  
  cul27=cl27[,1]
  cc1<-fitdist(cul27,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty27 = data.frame(pgamma(cul27, meanlog, sdlog))
  
  cul28=cl28[,1]
  cc1<-fitdist(cul28,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty28 = data.frame(pgamma(cul28, meanlog, sdlog))
  
  cul29=cl29[,1]
  cc1<-fitdist(cul29,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty29 = data.frame(pgamma(cul29, meanlog, sdlog))
  
  cul30=cl30[,1]
  cc1<-fitdist(cul30,"gamma", method="mle")
  para=data.frame(cc1$estimate)
  meanlog=para[1,1]
  sdlog=para[2,1]
  qempty30 = data.frame(pgamma(cul30, meanlog, sdlog))
    
  ##########  MODEL Dataset
  
    qqempty1<-c()
    qqempty2<-c()
    qqempty3<-c()
    qqempty4<-c()
    qqempty5<-c()
    qqempty6<-c()
    qqempty7<-c()
    qqempty8<-c()
    qqempty9<-c()
    qqempty10<-c()
    qqempty11<-c()
    qqempty12<-c()
    qqempty13<-c()
    qqempty14<-c()
    qqempty15<-c()
    qqempty16<-c()
    qqempty17<-c()
    qqempty18<-c()
    qqempty19<-c()
    qqempty20<-c()
    qqempty21<-c()
    qqempty22<-c()
    qqempty23<-c()
    qqempty23<-c()
    qqempty24<-c()
    qqempty25<-c()
    qqempty26<-c()
    qqempty27<-c()
    qqempty28<-c()
    qqempty29<-c()
    qqempty30<-c()
    qqempty31<-c()
    
    
    cul1=cll1[,1]
    cc1<-fitdist(cul1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty1 = data.frame(pgamma(cul1, meanlog, sdlog))
    
    
    cul2=cll2[,1]
    cc1<-fitdist(cul2,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty2 = data.frame(pgamma(cul2, meanlog, sdlog))
    
    cul3=cll3[,1]
    cc1<-fitdist(cul3,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty3 = data.frame(pgamma(cul3, meanlog, sdlog))
    
    cul4=cll4[,1]
    cc1<-fitdist(cul4,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty4 = data.frame(pgamma(cul4, meanlog, sdlog))
    
    cul5=cll5[,1]
    cc1<-fitdist(cul5,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty5 = data.frame(pgamma(cul5, meanlog, sdlog))
    
    cul6=cll6[,1]
    cc1<-fitdist(cul6,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty6 = data.frame(pgamma(cul6, meanlog, sdlog))
    
    cul7=cll7[,1]
    cc1<-fitdist(cul7,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty7 = data.frame(pgamma(cul7, meanlog, sdlog))
    
    cul8=cll8[,1]
    cc1<-fitdist(cul8,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty8 = data.frame(pgamma(cul8, meanlog, sdlog))
    
    cul9=cll9[,1]
    cc1<-fitdist(cul9,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty9 = data.frame(pgamma(cul9, meanlog, sdlog))
    
    cul10=cll10[,1]
    cc1<-fitdist(cul10,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty10 = data.frame(pgamma(cul10, meanlog, sdlog))
    
    cul11=cll11[,1]
    cc1<-fitdist(cul11,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty11 = data.frame(pgamma(cul11, meanlog, sdlog))
    
    
    cul12=cll12[,1]
    cc1<-fitdist(cul12,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty12 = data.frame(pgamma(cul12, meanlog, sdlog))
    
    cul13=cll13[,1]
    cc1<-fitdist(cul13,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty13 = data.frame(pgamma(cul13, meanlog, sdlog))
    
    cul14=cll14[,1]
    cc1<-fitdist(cul14,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty14 = data.frame(pgamma(cul14, meanlog, sdlog))
    
    cul15=cll15[,1]
    cc1<-fitdist(cul15,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty15 = data.frame(pgamma(cul15, meanlog, sdlog))
    
    cul16=cll16[,1]
    cc1<-fitdist(cul16,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty16 = data.frame(pgamma(cul16, meanlog, sdlog))
    
    cul17=cll17[,1]
    cc1<-fitdist(cul17,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty17 = data.frame(pgamma(cul17, meanlog, sdlog))
    
    cul18=cll18[,1]
    cc1<-fitdist(cul18,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty18 = data.frame(pgamma(cul18, meanlog, sdlog))
    
    cul19=cll19[,1]
    cc1<-fitdist(cul19,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty19 = data.frame(pgamma(cul19, meanlog, sdlog))
    
    cul20=cll20[,1]
    cc1<-fitdist(cul20,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty20 = data.frame(pgamma(cul20, meanlog, sdlog))
    
    cul21=cll21[,1]
    cc1<-fitdist(cul21,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty21 = data.frame(pgamma(cul21, meanlog, sdlog))
    
    cul22=cll22[,1]
    cc1<-fitdist(cul22,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty22 = data.frame(pgamma(cul22, meanlog, sdlog))
    
    cul23=cll23[,1]
    cc1<-fitdist(cul23,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty23 = data.frame(pgamma(cul23, meanlog, sdlog))
    
    cul24=cll24[,1]
    cc1<-fitdist(cul24,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty24 = data.frame(pgamma(cul24, meanlog, sdlog))
    
    cul25=cll25[,1]
    cc1<-fitdist(cul25,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty25 = data.frame(pgamma(cul25, meanlog, sdlog))
    
    cul26=cll26[,1]
    cc1<-fitdist(cul26,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty26 = data.frame(pgamma(cul26, meanlog, sdlog))
    
    cul27=cll27[,1]
    cc1<-fitdist(cul27,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty27 = data.frame(pgamma(cul27, meanlog, sdlog))
    
    cul28=cll28[,1]
    cc1<-fitdist(cul28,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty28 = data.frame(pgamma(cul28, meanlog, sdlog))
    
    cul29=cll29[,1]
    cc1<-fitdist(cul29,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty29 = data.frame(pgamma(cul29, meanlog, sdlog))
    
    cul30=cll30[,1]
    cc1<-fitdist(cul30,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty30 = data.frame(pgamma(cul30, meanlog, sdlog))
    
  ####################OBS dataset
  
    ct1=data.frame(OBZ1[a1:length(OBZ1[,1]),])
    
    ct2=data.frame(OBZ1[a2:length(OBZ1[,1]),])
    
    ct3=data.frame(OBZ1[a3:length(OBZ1[,1]),])
    
    ct4=data.frame(OBZ1[a4:length(OBZ1[,1]),])
    
    ct5=data.frame(OBZ1[a5:length(OBZ1[,1]),])
    
    ct6=data.frame(OBZ1[a6:length(OBZ1[,1]),])
    
    ct7=data.frame(OBZ1[a7:length(OBZ1[,1]),])
    
    ct8=data.frame(OBZ1[a8:length(OBZ1[,1]),])
    
    ct9=data.frame(OBZ1[a9:length(OBZ1[,1]),])
    
    ct10=data.frame(OBZ1[a10:length(OBZ1[,1]),])
    
    ct11=data.frame(OBZ1[a11:length(OBZ1[,1]),])
    
    ct12=data.frame(OBZ1[a12:length(OBZ1[,1]),])
    
    ct13=data.frame(OBZ1[a13:length(OBZ1[,1]),])
    
    ct14=data.frame(OBZ1[a14:length(OBZ1[,1]),])
    
    ct15=data.frame(OBZ1[a15:length(OBZ1[,1]),])
    
    ct16=data.frame(OBZ1[a16:length(OBZ1[,1]),])
    
    ct17=data.frame(OBZ1[a17:length(OBZ1[,1]),])
    
    ct18=data.frame(OBZ1[a18:length(OBZ1[,1]),])
    
    ct19=data.frame(OBZ1[a19:length(OBZ1[,1]),])
    
    ct20=data.frame(OBZ1[a20:length(OBZ1[,1]),])
    
    ct21=data.frame(OBZ1[a21:length(OBZ1[,1]),])
    
    ct22=data.frame(OBZ1[a22:length(OBZ1[,1]),])
    
    ct23=data.frame(OBZ1[a23:length(OBZ1[,1]),])
    
    ct24=data.frame(OBZ1[a24:length(OBZ1[,1]),])
    
    ct25=data.frame(OBZ1[a25:length(OBZ1[,1]),])
    
    ct26=data.frame(OBZ1[a26:length(OBZ1[,1]),])
    
    ct27=data.frame(OBZ1[a27:length(OBZ1[,1]),])
    
    ct28=data.frame(OBZ1[a28:length(OBZ1[,1]),])
    
    ct29=data.frame(OBZ1[a29:length(OBZ1[,1]),])
    
    ct30=data.frame(OBZ1[a30:length(OBZ1[,1]),])
    
    ct31=data.frame(OBZ1[a31:length(OBZ1[,1]),])
    
    
    ctl1=data.frame(OBZ1[1:a1-1,])
    
    ctl2=data.frame(OBZ1[1:a2-1,])
    
    ctl3=data.frame(OBZ1[1:a3-1,])
    
    ctl4=data.frame(OBZ1[1:a4-1,])
    
    ctl5=data.frame(OBZ1[1:a5-1,])
    
    ctl6=data.frame(OBZ1[1:a6-1,])
    
    ctl7=data.frame(OBZ1[1:a7-1,])
    
    ctl8=data.frame(OBZ1[1:a8-1,])
    
    ctl9=data.frame(OBZ1[1:a9-1,])
    
    ctl10=data.frame(OBZ1[1:a10-1,])
    
    ctl11=data.frame(OBZ1[1:a11-1,])
    
    ctl12=data.frame(OBZ1[1:a12-1,])
    
    ctl13=data.frame(OBZ1[1:a13-1,])
    
    ctl14=data.frame(OBZ1[1:a14-1,])
    
    ctl15=data.frame(OBZ1[1:a15-1,])
    
    ctl16=data.frame(OBZ1[1:a16-1,])
    
    ctl17=data.frame(OBZ1[1:a17-1,])
    
    ctl18=data.frame(OBZ1[1:a18-1,])
    
    ctl19=data.frame(OBZ1[1:a19-1,])
    
    ctl20=data.frame(OBZ1[1:a20-1,])
    
    ctl21=data.frame(OBZ1[1:a21-1,])
    
    ctl22=data.frame(OBZ1[1:a22-1,])
    
    ctl23=data.frame(OBZ1[1:a23-1,])
    
    ctl24=data.frame(OBZ1[1:a24-1,])
    
    ctl25=data.frame(OBZ1[1:a25-1,])
    
    ctl26=data.frame(OBZ1[1:a26-1,])
    
    ctl27=data.frame(OBZ1[1:a27-1,])
    
    ctl28=data.frame(OBZ1[1:a28-1,])
    
    ctl29=data.frame(OBZ1[1:a29-1,])
    
    ctl30=data.frame(OBZ1[1:a30-1,])
    
    ctl31=data.frame(OBZ1[1:a31-1,])
    
    
  ######################
    obsempty1<-c()
    obsempty2<-c()
    obsempty3<-c()
    obsempty4<-c()
    obsempty5<-c()
    obsempty6<-c()
    obsempty7<-c()
    obsempty8<-c()
    obsempty9<-c()
    obsempty10<-c()
    obsempty11<-c()
    obsempty12<-c()
    obsempty13<-c()
    obsempty14<-c()
    obsempty15<-c()
    obsempty16<-c()
    obsempty17<-c()
    obsempty18<-c()
    obsempty19<-c()
    obsempty20<-c()
    obsempty21<-c()
    obsempty22<-c()
    obsempty23<-c()
    obsempty23<-c()
    obsempty24<-c()
    obsempty25<-c()
    obsempty26<-c()
    obsempty27<-c()
    obsempty28<-c()
    obsempty29<-c()
    obsempty30<-c()
    obsempty31<-c()
  
    QUAN1<-c()
    QUAN2<-c()
    QUAN3<-c()
    QUAN4<-c()
    QUAN5<-c()
    QUAN6<-c()
    QUAN7<-c()
    QUAN8<-c()
    QUAN9<-c()
    QUAN10<-c()
    QUAN11<-c()
    QUAN12<-c()
    QUAN13<-c()
    QUAN14<-c()
    QUAN15<-c()
    QUAN16<-c()
    QUAN17<-c()
    QUAN18<-c()
    QUAN19<-c()
    QUAN20<-c()
    QUAN21<-c()
    QUAN22<-c()
    QUAN23<-c()
    QUAN23<-c()
    QUAN24<-c()
    QUAN25<-c()
    QUAN26<-c()
    QUAN27<-c()
    QUAN28<-c()
    QUAN29<-c()
    QUAN30<-c()
    QUAN31<-c()
    
  
  ###################################
  
  
  
    cl1=ct1[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty1 = pgamma(cl1, shape, rate)
    QUAN1= data.frame(qgamma(qempty1[,1], shape, rate))
    
    cl1=ct2[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty2 = pgamma(cl1, shape, rate)
    QUAN2= data.frame(qgamma(qempty2[,1], shape, rate))
    
    
    cl1=ct3[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty3 = pgamma(cl1, shape, rate)
    QUAN3 = data.frame(qgamma(qempty3[,1], shape, rate))
    
    cl1=ct4[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty4 = pgamma(cl1, shape, rate)
    QUAN4= data.frame(qgamma(qempty4[,1], shape, rate))
    
    cl1=ct5[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty5 = pgamma(cl1, shape, rate)
    QUAN5= data.frame(qgamma(qempty5[,1], shape, rate))
    
    cl1=ct6[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty6 = pgamma(cl1, shape, rate)
    QUAN6= data.frame(qgamma(qempty6[,1], shape, rate))
    
    cl1=ct7[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty7 = pgamma(cl1, shape, rate)
    QUAN7= data.frame(qgamma(qempty7[,1], shape, rate))
    
    cl1=ct8[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty8 = pgamma(cl1, shape, rate)
    QUAN8= data.frame(qgamma(qempty8[,1], shape, rate))
    
    cl1=ct9[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty9 = pgamma(cl1, shape, rate)
    QUAN9= data.frame(qgamma(qempty9[,1], shape, rate))
    
    cl1=ct10[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty10 = pgamma(cl1, shape, rate)
    QUAN10= data.frame(qgamma(qempty10[,1], shape, rate))
    
    cl1=ct11[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty11 = pgamma(cl1, shape, rate)
    QUAN11 = data.frame(qgamma(qempty11[,1], shape, rate))
    
    
    cl1=ct12[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty12 = pgamma(cl1, shape, rate)
    QUAN12= data.frame(qgamma(qempty12[,1], shape, rate))
    
    cl1=ct13[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty13 = pgamma(cl1, shape, rate)
    QUAN13= data.frame(qgamma(qempty13[,1], shape, rate))
    
    
    cl1=ct14[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty14 = pgamma(cl1, shape, rate)
    QUAN14= data.frame(qgamma(qempty14[,1], shape, rate))
    
    cl1=ct15[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty15 = pgamma(cl1, shape, rate)
    QUAN15= data.frame(qgamma(qempty15[,1], shape, rate))
    
    cl1=ct16[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty16 = pgamma(cl1, shape, rate)
    QUAN16= data.frame(qgamma(qempty16[,1], shape, rate))
    
    cl1=ct17[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty17 = pgamma(cl1, shape, rate)
    QUAN17= data.frame(qgamma(qempty17[,1], shape, rate))
    
    cl1=ct18[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty18 = pgamma(cl1, shape, rate)
    QUAN18= data.frame(qgamma(qempty18[,1], shape, rate))
    
    cl1=ct19[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty19 = pgamma(cl1, shape, rate)
    QUAN19= data.frame(qgamma(qempty19[,1], shape, rate))
    
    
    cl1=ct20[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty20 = pgamma(cl1, shape, rate)
    QUAN20= data.frame(qgamma(qempty20[,1], shape, rate))
    
    cl1=ct21[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty21 = pgamma(cl1, shape, rate)
    QUAN21= data.frame(qgamma(qempty21[,1], shape, rate))
    
    cl1=ct22[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty22 = pgamma(cl1, shape, rate)
    QUAN22= data.frame(qgamma(qempty22[,1], shape, rate))
    
    
    cl1=ct23[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty23 = pgamma(cl1, shape, rate)
    QUAN23= data.frame(qgamma(qempty23[,1], shape, rate))
    
    cl1=ct24[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty24 = pgamma(cl1, shape, rate)
    QUAN24= data.frame(qgamma(qempty24[,1], shape, rate))
    
    cl1=ct25[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty25 = pgamma(cl1, shape, rate)
    QUAN25= data.frame(qgamma(qempty25[,1], shape, rate))
    
    cl1=ct26[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty26 = pgamma(cl1, shape, rate)
    QUAN26= data.frame(qgamma(qempty26[,1], shape, rate))
    
    cl1=ct27[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty27 = pgamma(cl1, shape, rate)
    QUAN27= data.frame(qgamma(qempty27[,1], shape, rate))
    
    cl1=ct28[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty28 = pgamma(cl1, shape, rate)
    QUAN28= data.frame(qgamma(qempty28[,1], shape, rate))
    
    cl1=ct29[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty29 = pgamma(cl1, shape, rate)
    QUAN29= data.frame(qgamma(qempty29[,1], shape, rate))
    
    cl1=ct30[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty30 = pgamma(cl1, shape, rate)
    QUAN30= data.frame(qgamma(qempty30[,1], shape, rate))
    
  #####################
  
    oobsempty1<-c()
    oobsempty2<-c()
    oobsempty3<-c()
    oobsempty4<-c()
    oobsempty5<-c()
    oobsempty6<-c()
    oobsempty7<-c()
    oobsempty8<-c()
    oobsempty9<-c()
    oobsempty10<-c()
    oobsempty11<-c()
    oobsempty12<-c()
    oobsempty13<-c()
    oobsempty14<-c()
    oobsempty15<-c()
    oobsempty16<-c()
    oobsempty17<-c()
    oobsempty18<-c()
    oobsempty19<-c()
    oobsempty20<-c()
    oobsempty21<-c()
    oobsempty22<-c()
    oobsempty23<-c()
    oobsempty23<-c()
    oobsempty24<-c()
    oobsempty25<-c()
    oobsempty26<-c()
    oobsempty27<-c()
    oobsempty28<-c()
    oobsempty29<-c()
    oobsempty30<-c()
    oobsempty31<-c()
    
    QQUAN1<-c()
    QQUAN2<-c()
    QQUAN3<-c()
    QQUAN4<-c()
    QQUAN5<-c()
    QQUAN6<-c()
    QQUAN7<-c()
    QQUAN8<-c()
    QQUAN9<-c()
    QQUAN10<-c()
    QQUAN11<-c()
    QQUAN12<-c()
    QQUAN13<-c()
    QQUAN14<-c()
    QQUAN15<-c()
    QQUAN16<-c()
    QQUAN17<-c()
    QQUAN18<-c()
    QQUAN19<-c()
    QQUAN20<-c()
    QQUAN21<-c()
    QQUAN22<-c()
    QQUAN23<-c()
    QQUAN23<-c()
    QQUAN24<-c()
    QQUAN25<-c()
    QQUAN26<-c()
    QQUAN27<-c()
    QQUAN28<-c()
    QQUAN29<-c()
    QQUAN30<-c()
    QQUAN31<-c()
  
    cl1=ctl1[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty1 = pgamma(cl1, shape, rate)
    QQUAN1= data.frame(qgamma(qqempty1[,1], shape, rate))
    # testQQUAN1= data.frame(qgamma(oobsempty1, shape, rate))
    
    cl1=ctl2[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty2 = pgamma(cl1, shape, rate)
    QQUAN2= data.frame(qgamma(qqempty2[,1], shape, rate))
    
    
    cl1=ctl3[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty3 = pgamma(cl1, shape, rate)
    QQUAN3 = data.frame(qgamma(qqempty3[,1], shape, rate))
    
    cl1=ctl4[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty4 = pgamma(cl1, shape, rate)
    QQUAN4= data.frame(qgamma(qqempty4[,1], shape, rate))
    
    cl1=ctl5[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty5 = pgamma(cl1, shape, rate)
    QQUAN5= data.frame(qgamma(qqempty5[,1], shape, rate))
    
    cl1=ctl6[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty6 = pgamma(cl1, shape, rate)
    QQUAN6= data.frame(qgamma(qqempty6[,1], shape, rate))
    
    cl1=ctl7[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty7 = pgamma(cl1, shape, rate)
    QQUAN7= data.frame(qgamma(qqempty7[,1], shape, rate))
    
    cl1=ctl8[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty8 = pgamma(cl1, shape, rate)
    QQUAN8= data.frame(qgamma(qqempty8[,1], shape, rate))
    
    cl1=ctl9[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty9 = pgamma(cl1, shape, rate)
    QQUAN9= data.frame(qgamma(qqempty9[,1], shape, rate))
    
    cl1=ctl10[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty10 = pgamma(cl1, shape, rate)
    QQUAN10= data.frame(qgamma(qqempty10[,1], shape, rate))
    
    cl1=ctl11[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty11 = pgamma(cl1, shape, rate)
    QQUAN11 = data.frame(qgamma(qqempty11[,1], shape, rate))
    
    
    cl1=ctl12[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty12 = pgamma(cl1, shape, rate)
    QQUAN12= data.frame(qgamma(qqempty12[,1], shape, rate))
    
    cl1=ctl13[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty13 = pgamma(cl1, shape, rate)
    QQUAN13= data.frame(qgamma(qqempty13[,1], shape, rate))
    
    
    cl1=ctl14[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty14 = pgamma(cl1, shape, rate)
    QQUAN14= data.frame(qgamma(qqempty14[,1], shape, rate))
    
    cl1=ctl15[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty15 = pgamma(cl1, shape, rate)
    QQUAN15= data.frame(qgamma(qqempty15[,1], shape, rate))
    
    cl1=ctl16[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty16 = pgamma(cl1, shape, rate)
    QQUAN16= data.frame(qgamma(qqempty16[,1], shape, rate))
    
    cl1=ctl17[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty17 = pgamma(cl1, shape, rate)
    QQUAN17= data.frame(qgamma(qqempty17[,1], shape, rate))
    
    cl1=ctl18[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty18 = pgamma(cl1, shape, rate)
    QQUAN18= data.frame(qgamma(qqempty18[,1], shape, rate))
    
    cl1=ctl19[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty19 = pgamma(cl1, shape, rate)
    QQUAN19= data.frame(qgamma(qqempty19[,1], shape, rate))
    
    
    cl1=ctl20[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty20 = pgamma(cl1, shape, rate)
    QQUAN20= data.frame(qgamma(qqempty20[,1], shape, rate))
    
    cl1=ctl21[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty21 = pgamma(cl1, shape, rate)
    QQUAN21= data.frame(qgamma(qqempty21[,1], shape, rate))
    
    cl1=ctl22[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty22 = pgamma(cl1, shape, rate)
    QQUAN22= data.frame(qgamma(qqempty22[,1], shape, rate))
    
    
    cl1=ctl23[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty23 = pgamma(cl1, shape, rate)
    QQUAN23= data.frame(qgamma(qqempty23[,1], shape, rate))
    
    cl1=ctl24[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty24 = pgamma(cl1, shape, rate)
    QQUAN24= data.frame(qgamma(qqempty24[,1], shape, rate))
    
    cl1=ctl25[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty25 = pgamma(cl1, shape, rate)
    QQUAN25= data.frame(qgamma(qqempty25[,1], shape, rate))
    
    cl1=ctl26[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty26 = pgamma(cl1, shape, rate)
    QQUAN26= data.frame(qgamma(qqempty26[,1], shape, rate))
    
    cl1=ctl27[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty27 = pgamma(cl1, shape, rate)
    QQUAN27= data.frame(qgamma(qqempty27[,1], shape, rate))
    
    cl1=ctl28[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty28 = pgamma(cl1, shape, rate)
    QQUAN28= data.frame(qgamma(qqempty28[,1], shape, rate))
    
    cl1=ctl29[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty29 = pgamma(cl1, shape, rate)
    QQUAN29= data.frame(qgamma(qqempty29[,1], shape, rate))
    
    
    cl1=ctl30[,1]
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty30 = pgamma(cl1, shape, rate)
    QQUAN30= data.frame(qgamma(qqempty30[,1], shape, rate))
    
    
    
    colnames(QQUAN1) <- colnames(obsprecip)[iCount]
    colnames(QQUAN2) <- colnames(obsprecip)[iCount]
    colnames(QQUAN3) <- colnames(obsprecip)[iCount]
    colnames(QQUAN4) <- colnames(obsprecip)[iCount]
    colnames(QQUAN5) <- colnames(obsprecip)[iCount]
    colnames(QQUAN6) <- colnames(obsprecip)[iCount]
    colnames(QQUAN7) <- colnames(obsprecip)[iCount]
    colnames(QQUAN8) <- colnames(obsprecip)[iCount]
    colnames(QQUAN9) <- colnames(obsprecip)[iCount]
    colnames(QQUAN10) <- colnames(obsprecip)[iCount]
    colnames(QQUAN11) <- colnames(obsprecip)[iCount]
    colnames(QQUAN12) <- colnames(obsprecip)[iCount]
    colnames(QQUAN13) <- colnames(obsprecip)[iCount]
    colnames(QQUAN14) <- colnames(obsprecip)[iCount]
    colnames(QQUAN15) <- colnames(obsprecip)[iCount]
    colnames(QQUAN16) <- colnames(obsprecip)[iCount]
    colnames(QQUAN17) <- colnames(obsprecip)[iCount]
    colnames(QQUAN18) <- colnames(obsprecip)[iCount]
    colnames(QQUAN19) <- colnames(obsprecip)[iCount]
    colnames(QQUAN20) <- colnames(obsprecip)[iCount]
    colnames(QQUAN21) <- colnames(obsprecip)[iCount]
    colnames(QQUAN22) <- colnames(obsprecip)[iCount]
    colnames(QQUAN23) <- colnames(obsprecip)[iCount]
    colnames(QQUAN24) <- colnames(obsprecip)[iCount]
    colnames(QQUAN25) <- colnames(obsprecip)[iCount]
    colnames(QQUAN26) <- colnames(obsprecip)[iCount]
    colnames(QQUAN27) <- colnames(obsprecip)[iCount]
    colnames(QQUAN28) <- colnames(obsprecip)[iCount]
    colnames(QQUAN29) <- colnames(obsprecip)[iCount]
    colnames(QQUAN30) <- colnames(obsprecip)[iCount]
    
    colnames(QUAN1) <- colnames(obsprecip)[iCount]
    colnames(QUAN2) <- colnames(obsprecip)[iCount]
    colnames(QUAN3) <- colnames(obsprecip)[iCount]
    colnames(QUAN4) <- colnames(obsprecip)[iCount]
    colnames(QUAN5) <- colnames(obsprecip)[iCount]
    colnames(QUAN6) <- colnames(obsprecip)[iCount]
    colnames(QUAN7) <- colnames(obsprecip)[iCount]
    colnames(QUAN8) <- colnames(obsprecip)[iCount]
    colnames(QUAN9) <- colnames(obsprecip)[iCount]
    colnames(QUAN10) <- colnames(obsprecip)[iCount]
    colnames(QUAN11) <- colnames(obsprecip)[iCount]
    colnames(QUAN12) <- colnames(obsprecip)[iCount]
    colnames(QUAN13) <- colnames(obsprecip)[iCount]
    colnames(QUAN14) <- colnames(obsprecip)[iCount]
    colnames(QUAN15) <- colnames(obsprecip)[iCount]
    colnames(QUAN16) <- colnames(obsprecip)[iCount]
    colnames(QUAN17) <- colnames(obsprecip)[iCount]
    colnames(QUAN18) <- colnames(obsprecip)[iCount]
    colnames(QUAN19) <- colnames(obsprecip)[iCount]
    colnames(QUAN20) <- colnames(obsprecip)[iCount]
    colnames(QUAN21) <- colnames(obsprecip)[iCount]
    colnames(QUAN22) <- colnames(obsprecip)[iCount]
    colnames(QUAN23) <- colnames(obsprecip)[iCount]
    colnames(QUAN24) <- colnames(obsprecip)[iCount]
    colnames(QUAN25) <- colnames(obsprecip)[iCount]
    colnames(QUAN26) <- colnames(obsprecip)[iCount]
    colnames(QUAN27) <- colnames(obsprecip)[iCount]
    colnames(QUAN28) <- colnames(obsprecip)[iCount]
    colnames(QUAN29) <- colnames(obsprecip)[iCount]
    colnames(QUAN30) <- colnames(obsprecip)[iCount]
    
  #########
    nrow(QUAN1)
    CCQ1=rbind(QQUAN1,QUAN1)
    CCQ2=rbind(QQUAN2,QUAN2)
    CCQ3=rbind(QQUAN3,QUAN3)
    CCQ4=rbind(QQUAN4,QUAN4)
    CCQ5=rbind(QQUAN5,QUAN5)
    CCQ6=rbind(QQUAN6,QUAN6)
    CCQ7=rbind(QQUAN7,QUAN7)
    CCQ8=rbind(QQUAN8,QUAN8)
    CCQ9=rbind(QQUAN9,QUAN9)
    CCQ10=rbind(QQUAN10,QUAN10)
    CCQ11=rbind(QQUAN11,QUAN11)
    CCQ12=rbind(QQUAN12,QUAN12)
    CCQ13=rbind(QQUAN13,QUAN13)
    CCQ14=rbind(QQUAN14,QUAN14)
    CCQ15=rbind(QQUAN15,QUAN15)
    CCQ16=rbind(QQUAN16,QUAN16)
    CCQ17=rbind(QQUAN17,QUAN17)
    CCQ18=rbind(QQUAN18,QUAN18)
    CCQ19=rbind(QQUAN19,QUAN19)
    CCQ20=rbind(QQUAN20,QUAN20)
    CCQ21=rbind(QQUAN21,QUAN21)
    CCQ22=rbind(QQUAN22,QUAN22)
    CCQ23=rbind(QQUAN23,QUAN23)
    CCQ24=rbind(QQUAN24,QUAN24)
    CCQ25=rbind(QQUAN25,QUAN25)
    CCQ26=rbind(QQUAN26,QUAN26)
    CCQ27=rbind(QQUAN27,QUAN27)
    CCQ28=rbind(QQUAN28,QUAN28)
    CCQ29=rbind(QQUAN29,QUAN29)
    CCQ30=rbind(QQUAN30,QUAN30)
    
    
  BBQ1<-c()
  BBQ2<-c()
  BBQ3<-c()
    BBQ4<-c()
    BBQ5<-c()
    BBQ6<-c()
    BBQ7<-c()
    BBQ8<-c()
    BBQ9<-c()
    BBQ10<-c()  
  
  BBQ11<-c()
  BBQ12<-c()
  BBQ13<-c()
  BBQ14<-c()
  BBQ15<-c()
  BBQ16<-c()
  BBQ17<-c()
  BBQ18<-c()
  BBQ19<-c()
  BBQ20<-c()
  
  BBQ21<-c()
  BBQ22<-c()
  BBQ23<-c()
  BBQ24<-c()
  BBQ25<-c()
  BBQ26<-c()
  BBQ27<-c()
  BBQ28<-c()
  BBQ29<-c()
  BBQ30<-c()
  BBQ31<-c()
  
  for (i in 1) {
    
    length(BBQ1)
    gg=replace(CCQ1[,i],which(CCQ1[,i]<=0.5),0)
    BBQ1<- cbind(BBQ1,gg)
    gg=replace(CCQ2[,i],which(CCQ2[,i]<=0.5),0)
    BBQ2<- cbind(BBQ2,gg)
    gg=replace(CCQ3[,i],which(CCQ3[,i]<=0.5),0)
    BBQ3<- cbind(BBQ3,gg)
    gg=replace(CCQ4[,i],which(CCQ4[,i]<=0.5),0)
    BBQ4<- cbind(BBQ4,gg)
    gg=replace(CCQ5[,i],which(CCQ5[,i]<=0.5),0)
    BBQ5<- cbind(BBQ5,gg)
    gg=replace(CCQ6[,i],which(CCQ6[,i]<=0.5),0)
    BBQ6<- cbind(BBQ6,gg)
    gg=replace(CCQ7[,i],which(CCQ7[,i]<=0.5),0)
    BBQ7<- cbind(BBQ7,gg)
    gg=replace(CCQ8[,i],which(CCQ8[,i]<=0.5),0)
    BBQ8<- cbind(BBQ8,gg)
    gg=replace(CCQ9[,i],which(CCQ9[,i]<=0.5),0)
    BBQ9<- cbind(BBQ9,gg)
    gg=replace(CCQ10[,i],which(CCQ10[,i]<=0.5),0)
    BBQ10<- cbind(BBQ10,gg)
    gg=replace(CCQ11[,i],which(CCQ11[,i]<=0.5),0)
    BBQ11<- cbind(BBQ11,gg)
    gg=replace(CCQ12[,i],which(CCQ12[,i]<=0.5),0)
    BBQ12<- cbind(BBQ12,gg)
    gg=replace(CCQ13[,i],which(CCQ13[,i]<=0.5),0)
    BBQ13<- cbind(BBQ13,gg)
    gg=replace(CCQ14[,i],which(CCQ14[,i]<=0.5),0)
    BBQ14<- cbind(BBQ14,gg)
    gg=replace(CCQ15[,i],which(CCQ15[,i]<=0.5),0)
    BBQ15<- cbind(BBQ15,gg)
    gg=replace(CCQ16[,i],which(CCQ16[,i]<=0.5),0)
    BBQ16<- cbind(BBQ16,gg)
    gg=replace(CCQ17[,i],which(CCQ17[,i]<=0.5),0)
    BBQ17<- cbind(BBQ17,gg)
    gg=replace(CCQ18[,i],which(CCQ18[,i]<=0.5),0)
    BBQ18<- cbind(BBQ18,gg)
    gg=replace(CCQ19[,i],which(CCQ19[,i]<=0.5),0)
    BBQ19<- cbind(BBQ19,gg)
    gg=replace(CCQ20[,i],which(CCQ20[,i]<=0.5),0)
    BBQ20<- cbind(BBQ20,gg)
    gg=replace(CCQ21[,i],which(CCQ21[,i]<=0.5),0)
    BBQ21<- cbind(BBQ21,gg)
    gg=replace(CCQ22[,i],which(CCQ22[,i]<=0.5),0)
    BBQ22<- cbind(BBQ22,gg)
    gg=replace(CCQ23[,i],which(CCQ23[,i]<=0.5),0)
    BBQ23<- cbind(BBQ23,gg)
    gg=replace(CCQ24[,i],which(CCQ24[,i]<=0.5),0)
    BBQ24<- cbind(BBQ24,gg)
    gg=replace(CCQ25[,i],which(CCQ25[,i]<=0.5),0)
    BBQ25<- cbind(BBQ25,gg)
    gg=replace(CCQ26[,i],which(CCQ26[,i]<=0.5),0)
    BBQ26<- cbind(BBQ26,gg)
    gg=replace(CCQ27[,i],which(CCQ27[,i]<=0.5),0)
    BBQ27<- cbind(BBQ27,gg)
    gg=replace(CCQ28[,i],which(CCQ28[,i]<=0.5),0)
    BBQ28<- cbind(BBQ28,gg)
    gg=replace(CCQ29[,i],which(CCQ29[,i]<=0.5),0)
    BBQ29<- cbind(BBQ29,gg)
    gg=replace(CCQ30[,i],which(CCQ30[,i]<=0.5),0)
    BBQ30<- cbind(BBQ30,gg)
    
  }
  
  # LSH
  BQ1=data.frame(Q1[1:QC1-1,1])
  # BQ1=data.frame(Q2[1:QC2-1,1])
  colnames(BQ1) <- c('gg')
  EBQ1=rbind(BQ1,BBQ1)
  EBQ2=rbind(BQ1,BBQ2)
  EBQ3=rbind(BQ1,BBQ3)
  EBQ4=rbind(BQ1,BBQ4)
  EBQ5=rbind(BQ1,BBQ5)
  EBQ6=rbind(BQ1,BBQ6)
  EBQ7=rbind(BQ1,BBQ7)
  EBQ8=rbind(BQ1,BBQ8)
  EBQ9=rbind(BQ1,BBQ9)
  EBQ10=rbind(BQ1,BBQ10)
  EBQ11=rbind(BQ1,BBQ11)
  EBQ12=rbind(BQ1,BBQ12)
  EBQ13=rbind(BQ1,BBQ13)
  EBQ14=rbind(BQ1,BBQ14)
  EBQ15=rbind(BQ1,BBQ15)
  EBQ16=rbind(BQ1,BBQ16)
  EBQ17=rbind(BQ1,BBQ17)
  EBQ18=rbind(BQ1,BBQ18)
  EBQ19=rbind(BQ1,BBQ19)
  EBQ20=rbind(BQ1,BBQ20)
  EBQ21=rbind(BQ1,BBQ21)
  EBQ22=rbind(BQ1,BBQ22)
  EBQ23=rbind(BQ1,BBQ23)
  EBQ24=rbind(BQ1,BBQ24)
  EBQ25=rbind(BQ1,BBQ25)
  EBQ26=rbind(BQ1,BBQ26)
  EBQ27=rbind(BQ1,BBQ27)
  EBQ28=rbind(BQ1,BBQ28)
  EBQ29=rbind(BQ1,BBQ29)
  EBQ30=rbind(BQ1,BBQ30)
  
  
  
  #rmse(BBQ2,obsprecip)
  
  colnames(EBQ1) <- colnames(obsprecip)[iCount]
  colnames(EBQ2) <- colnames(obsprecip)[iCount]
  colnames(EBQ3) <- colnames(obsprecip)[iCount]
  colnames(EBQ4) <- colnames(obsprecip)[iCount]
  colnames(EBQ5) <- colnames(obsprecip)[iCount]
  colnames(EBQ6) <- colnames(obsprecip)[iCount]
  colnames(EBQ7) <- colnames(obsprecip)[iCount]
  colnames(EBQ8) <- colnames(obsprecip)[iCount]
  colnames(EBQ9) <- colnames(obsprecip)[iCount]
  colnames(EBQ10) <- colnames(obsprecip)[iCount]
  colnames(EBQ11) <- colnames(obsprecip)[iCount]
  colnames(EBQ12) <- colnames(obsprecip)[iCount]
  colnames(EBQ13) <- colnames(obsprecip)[iCount]
  colnames(EBQ14) <- colnames(obsprecip)[iCount]
  colnames(EBQ15) <- colnames(obsprecip)[iCount]
  colnames(EBQ16) <- colnames(obsprecip)[iCount]
  colnames(EBQ17) <- colnames(obsprecip)[iCount]
  colnames(EBQ18) <- colnames(obsprecip)[iCount]
  colnames(EBQ19) <- colnames(obsprecip)[iCount]
  colnames(EBQ20) <- colnames(obsprecip)[iCount]
  colnames(EBQ21) <- colnames(obsprecip)[iCount]
  colnames(EBQ22) <- colnames(obsprecip)[iCount]
  colnames(EBQ23) <- colnames(obsprecip)[iCount]
  colnames(EBQ24) <- colnames(obsprecip)[iCount]
  colnames(EBQ25) <- colnames(obsprecip)[iCount]
  colnames(EBQ26) <- colnames(obsprecip)[iCount]
  colnames(EBQ27) <- colnames(obsprecip)[iCount]
  colnames(EBQ28) <- colnames(obsprecip)[iCount]
  colnames(EBQ29) <- colnames(obsprecip)[iCount]
  colnames(EBQ30) <- colnames(obsprecip)[iCount]
  
  # write.csv(EBQ1, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/70 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ2, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/71 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ3, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/72 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ4, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/73 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ5, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/74 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ6, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/75 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ7, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/76 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ8, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/77 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ9, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/78 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ10, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/79 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ11, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/80 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ12, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/81 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ13, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/82 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ14, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/83 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ15, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/84 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ16, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/85 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ17, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/86 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ18, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/87 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ19, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/88 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ20, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/89 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ21, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/90 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ22, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/91 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ23, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/92 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ24, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/93 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ25, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/94 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ26, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/95 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ27, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/96 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ28, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/97 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ29, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/98 Gamma NorESM2-MM model.csv")
  # write.csv(EBQ30, "F:/Daily Precipitation/80-100/Combination/Datare/NorESM2-MM/99 Gamma NorESM2-MM model.csv")
  
  ########################### Kullback method
  #####################################
  
  tl1<-c()
  tl2<-c()
  tl3<-c()
  tl4<-c()
  tl5<-c()
  tl6<-c()
  tl7<-c()
  tl8<-c()
  tl9<-c()
  tl10<-c()
  tl11<-c()
  tl12<-c()
  tl13<-c()
  tl14<-c()
  tl15<-c()
  tl16<-c()
  tl17<-c()
  tl18<-c()
  tl19<-c()
  tl20<-c()
  tl21<-c()
  tl22<-c()
  tl23<-c()
  tl24<-c()
  tl25<-c()
  tl26<-c()
  tl27<-c()
  tl28<-c()
  tl29<-c()
  tl30<-c()
  
  
  tll1<-c()
  tll2<-c()
  tll3<-c()
  tll4<-c()
  tll5<-c()
  tll6<-c()
  tll7<-c()
  tll8<-c()
  tll9<-c()
  tll10<-c()
  tll11<-c()
  tll12<-c()
  tll13<-c()
  tll14<-c()
  tll15<-c()
  tll16<-c()
  tll17<-c()
  tll18<-c()
  tll19<-c()
  tll20<-c()
  tll21<-c()
  tll22<-c()
  tll23<-c()
  tll24<-c()
  tll25<-c()
  tll26<-c()
  tll27<-c()
  tll28<-c()
  tll29<-c()
  tll30<-c()
  
  tlll1<-c()
  tlll2<-c()
  tlll3<-c()
  tlll4<-c()
  tlll5<-c()
  tlll6<-c()
  tlll7<-c()
  tlll8<-c()
  tlll9<-c()
  tlll10<-c()
  tlll11<-c()
  tlll12<-c()
  tlll13<-c()
  tlll14<-c()
  tlll15<-c()
  tlll16<-c()
  tlll17<-c()
  tlll18<-c()
  tlll19<-c()
  tlll20<-c()
  tlll21<-c()
  tlll22<-c()
  tlll23<-c()
  tlll24<-c()
  tlll25<-c()
  tlll26<-c()
  tlll27<-c()
  tlll28<-c()
  tlll29<-c()
  tlll30<-c()
  
  tllll1<-c()
  tllll2<-c()
  tllll3<-c()
  tllll4<-c()
  tllll5<-c()
  tllll6<-c()
  tllll7<-c()
  tllll8<-c()
  tllll9<-c()
  tllll10<-c()
  tllll11<-c()
  tllll12<-c()
  tllll13<-c()
  tllll14<-c()
  tllll15<-c()
  tllll16<-c()
  tllll17<-c()
  tllll18<-c()
  tllll19<-c()
  tllll20<-c()
  tllll21<-c()
  tllll22<-c()
  tllll23<-c()
  tllll24<-c()
  tllll25<-c()
  tllll26<-c()
  tllll27<-c()
  tllll28<-c()
  tllll29<-c()
  tllll30<-c()
  
  tlllll1<-c()
  tlllll2<-c()
  tlllll3<-c()
  tlllll4<-c()
  tlllll5<-c()
  tlllll6<-c()
  tlllll7<-c()
  tlllll8<-c()
  tlllll9<-c()
  tlllll10<-c()
  tlllll11<-c()
  tlllll12<-c()
  tlllll13<-c()
  tlllll14<-c()
  tlllll15<-c()
  tlllll16<-c()
  tlllll17<-c()
  tlllll18<-c()
  tlllll19<-c()
  tlllll20<-c()
  tlllll21<-c()
  tlllll22<-c()
  tlllll23<-c()
  tlllll24<-c()
  tlllll25<-c()
  tlllll26<-c()
  tlllll27<-c()
  tlllll28<-c()
  tlllll29<-c()
  tlllll30<-c()
  ########################### 
  
  
  for (i in 1) {
    options(digits = 8)
    # a1 = EBQ1[,1]
    a1 = EBQ2[,1]
    a2 = sort(obsprecip[,iCount])
    
    C1=rmse(EBQ1[,i],sort(obsprecip[,iCount]))
    
    C1=rmse(EBQ1[,i],sort(obsprecip[,iCount]))
    P1=C1/mean(obsprecip[,iCount])
    P2=pbias(EBQ1[,i],sort(obsprecip[,iCount]))
    P3=NSE(EBQ1[,i],sort(obsprecip[,iCount]))
    P4=md(EBQ1[,i],sort(obsprecip[,iCount]))
    P5=KGE(EBQ1[,i],sort(obsprecip[,iCount]))
    
    tl1<-cbind(tl1,P1)
    tll1<-cbind(tll1,P2)
    tlll1<-cbind(tlll1,P3)
    tllll1<-cbind(tllll1,P4)
    tlllll1<-cbind(tlllll1,P5)
    
    
    C1=rmse(EBQ2[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ2[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ2[,i],sort(obsprecip[,i]))
    P4=md(EBQ2[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ2[,i],sort(obsprecip[,i]))
    
    tl2<-cbind(tl2,P1)
    tll2<-cbind(tll2,P2)
    tlll2<-cbind(tlll2,P3)
    tllll2<-cbind(tllll2,P4)
    tlllll2<-cbind(tlllll2,P5)
    
    
    C1=rmse(EBQ3[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ3[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ3[,i],sort(obsprecip[,i]))
    P4=md(EBQ3[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ3[,i],sort(obsprecip[,i]))
    
    tl3<-cbind(tl3,P1)
    tll3<-cbind(tll3,P2)
    tlll3<-cbind(tlll3,P3)
    tllll3<-cbind(tllll3,P4)
    tlllll3<-cbind(tlllll3,P5)
    
    
    C1=rmse(EBQ4[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ4[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ4[,i],sort(obsprecip[,i]))
    P4=md(EBQ4[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ4[,i],sort(obsprecip[,i]))
    
    tl4<-cbind(tl4,P1)
    tll4<-cbind(tll4,P2)
    tlll4<-cbind(tlll4,P3)
    tllll4<-cbind(tllll4,P4)
    tlllll4<-cbind(tlllll4,P5)
    
    C1=rmse(EBQ5[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ5[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ5[,i],sort(obsprecip[,i]))
    P4=md(EBQ5[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ5[,i],sort(obsprecip[,i]))
    
    tl5<-cbind(tl5,P1)
    tll5<-cbind(tll5,P2)
    tlll5<-cbind(tlll5,P3)
    tllll5<-cbind(tllll5,P4)
    tlllll5<-cbind(tlllll5,P5)
    
    
    C1=rmse(EBQ6[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ6[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ6[,i],sort(obsprecip[,i]))
    P4=md(EBQ6[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ6[,i],sort(obsprecip[,i]))
    
    tl6<-cbind(tl6,P1)
    tll6<-cbind(tll6,P2)
    tlll6<-cbind(tlll6,P3)
    tllll6<-cbind(tllll6,P4)
    tlllll6<-cbind(tlllll6,P5)
    
    
    C1=rmse(EBQ7[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ7[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ7[,i],sort(obsprecip[,i]))
    P4=md(EBQ7[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ7[,i],sort(obsprecip[,i]))
    
    tl7<-cbind(tl7,P1)
    tll7<-cbind(tll7,P2)
    tlll7<-cbind(tlll7,P3)
    tllll7<-cbind(tllll7,P4)
    tlllll7<-cbind(tlllll7,P5)
    
    C1=rmse(EBQ8[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ8[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ8[,i],sort(obsprecip[,i]))
    P4=md(EBQ8[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ8[,i],sort(obsprecip[,i]))
    
    tl8<-cbind(tl8,P1)
    tll8<-cbind(tll8,P2)
    tlll8<-cbind(tlll8,P3)
    tllll8<-cbind(tllll8,P4)
    tlllll8<-cbind(tlllll8,P5)
    
    C1=rmse(EBQ9[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ9[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ9[,i],sort(obsprecip[,i]))
    P4=md(EBQ9[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ9[,i],sort(obsprecip[,i]))
    
    tl9<-cbind(tl9,P1)
    tll9<-cbind(tll9,P2)
    tlll9<-cbind(tlll9,P3)
    tllll9<-cbind(tllll9,P4)
    tlllll9<-cbind(tlllll9,P5)
    
    C1=rmse(EBQ10[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ10[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ10[,i],sort(obsprecip[,i]))
    P4=md(EBQ10[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ10[,i],sort(obsprecip[,i]))
    
    tl10<-cbind(tl10,P1)
    tll10<-cbind(tll10,P2)
    tlll10<-cbind(tlll10,P3)
    tllll10<-cbind(tllll10,P4)
    tlllll10<-cbind(tlllll10,P5)
    
    C1=rmse(EBQ11[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ11[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ11[,i],sort(obsprecip[,i]))
    P4=md(EBQ11[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ11[,i],sort(obsprecip[,i]))
    
    tl11<-cbind(tl11,P1)
    tll11<-cbind(tll11,P2)
    tlll11<-cbind(tlll11,P3)
    tllll11<-cbind(tllll11,P4)
    tlllll11<-cbind(tlllll11,P5)
    
    C1=rmse(EBQ12[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ12[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ12[,i],sort(obsprecip[,i]))
    P4=md(EBQ12[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ12[,i],sort(obsprecip[,i]))
    
    tl12<-cbind(tl12,P1)
    tll12<-cbind(tll12,P2)
    tlll12<-cbind(tlll12,P3)
    tllll12<-cbind(tllll12,P4)
    tlllll12<-cbind(tlllll12,P5)
    
    C1=rmse(EBQ13[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ13[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ13[,i],sort(obsprecip[,i]))
    P4=md(EBQ13[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ13[,i],sort(obsprecip[,i]))
    
    tl13<-cbind(tl13,P1)
    tll13<-cbind(tll13,P2)
    tlll13<-cbind(tlll13,P3)
    tllll13<-cbind(tllll13,P4)
    tlllll13<-cbind(tlllll13,P5)
    
    C1=rmse(EBQ14[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ14[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ14[,i],sort(obsprecip[,i]))
    P4=md(EBQ14[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ14[,i],sort(obsprecip[,i]))
    
    tl14<-cbind(tl14,P1)
    tll14<-cbind(tll14,P2)
    tlll14<-cbind(tlll14,P3)
    tllll14<-cbind(tllll14,P4)
    tlllll14<-cbind(tlllll14,P5)
    
    C1=rmse(EBQ15[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ15[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ15[,i],sort(obsprecip[,i]))
    P4=md(EBQ15[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ15[,i],sort(obsprecip[,i]))
    
    tl15<-cbind(tl15,P1)
    tll15<-cbind(tll15,P2)
    tlll15<-cbind(tlll15,P3)
    tllll15<-cbind(tllll15,P4)
    tlllll15<-cbind(tlllll15,P5)
    
    C1=rmse(EBQ16[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ16[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ16[,i],sort(obsprecip[,i]))
    P4=md(EBQ16[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ16[,i],sort(obsprecip[,i]))
    
    tl16<-cbind(tl16,P1)
    tll16<-cbind(tll16,P2)
    tlll16<-cbind(tlll16,P3)
    tllll16<-cbind(tllll16,P4)
    tlllll16<-cbind(tlllll16,P5)
    
    C1=rmse(EBQ17[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ17[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ17[,i],sort(obsprecip[,i]))
    P4=md(EBQ17[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ17[,i],sort(obsprecip[,i]))
    
    tl17<-cbind(tl17,P1)
    tll17<-cbind(tll17,P2)
    tlll17<-cbind(tlll17,P3)
    tllll17<-cbind(tllll17,P4)
    tlllll17<-cbind(tlllll17,P5)
    
    C1=rmse(EBQ18[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ18[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ18[,i],sort(obsprecip[,i]))
    P4=md(EBQ18[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ18[,i],sort(obsprecip[,i]))
    
    tl18<-cbind(tl18,P1)
    tll18<-cbind(tll18,P2)
    tlll18<-cbind(tlll18,P3)
    tllll18<-cbind(tllll18,P4)
    tlllll18<-cbind(tlllll18,P5)
    
    C1=rmse(EBQ19[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ19[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ19[,i],sort(obsprecip[,i]))
    P4=md(EBQ19[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ19[,i],sort(obsprecip[,i]))
    
    tl19<-cbind(tl19,P1)
    tll19<-cbind(tll19,P2)
    tlll19<-cbind(tlll19,P3)
    tllll19<-cbind(tllll19,P4)
    tlllll19<-cbind(tlllll19,P5)
    
    C1=rmse(EBQ20[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ20[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ20[,i],sort(obsprecip[,i]))
    P4=md(EBQ20[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ20[,i],sort(obsprecip[,i]))
    
    tl20<-cbind(tl20,P1)
    tll20<-cbind(tll20,P2)
    tlll20<-cbind(tlll20,P3)
    tllll20<-cbind(tllll20,P4)
    tlllll20<-cbind(tlllll20,P5)
    
    C1=rmse(EBQ21[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ21[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ21[,i],sort(obsprecip[,i]))
    P4=md(EBQ21[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ21[,i],sort(obsprecip[,i]))
    
    tl21<-cbind(tl21,P1)
    tll21<-cbind(tll21,P2)
    tlll21<-cbind(tlll21,P3)
    tllll21<-cbind(tllll21,P4)
    tlllll21<-cbind(tlllll21,P5)
    
    C1=rmse(EBQ22[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ22[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ22[,i],sort(obsprecip[,i]))
    P4=md(EBQ22[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ22[,i],sort(obsprecip[,i]))
    
    tl22<-cbind(tl22,P1)
    tll22<-cbind(tll22,P2)
    tlll22<-cbind(tlll22,P3)
    tllll22<-cbind(tllll22,P4)
    tlllll22<-cbind(tlllll22,P5)
    
    C1=rmse(EBQ23[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ23[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ23[,i],sort(obsprecip[,i]))
    P4=md(EBQ23[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ23[,i],sort(obsprecip[,i]))
    
    tl23<-cbind(tl23,P1)
    tll23<-cbind(tll23,P2)
    tlll23<-cbind(tlll23,P3)
    tllll23<-cbind(tllll23,P4)
    tlllll23<-cbind(tlllll23,P5)
    
    C1=rmse(EBQ24[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ24[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ24[,i],sort(obsprecip[,i]))
    P4=md(EBQ24[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ24[,i],sort(obsprecip[,i]))
    
    tl24<-cbind(tl24,P1)
    tll24<-cbind(tll24,P2)
    tlll24<-cbind(tlll24,P3)
    tllll24<-cbind(tllll24,P4)
    tlllll24<-cbind(tlllll24,P5)
    
    C1=rmse(EBQ25[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ25[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ25[,i],sort(obsprecip[,i]))
    P4=md(EBQ25[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ25[,i],sort(obsprecip[,i]))
    
    tl25<-cbind(tl25,P1)
    tll25<-cbind(tll25,P2)
    tlll25<-cbind(tlll25,P3)
    tllll25<-cbind(tllll25,P4)
    tlllll25<-cbind(tlllll25,P5)
    
    C1=rmse(EBQ26[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ26[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ26[,i],sort(obsprecip[,i]))
    P4=md(EBQ26[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ26[,i],sort(obsprecip[,i]))
    
    tl26<-cbind(tl26,P1)
    tll26<-cbind(tll26,P2)
    tlll26<-cbind(tlll26,P3)
    tllll26<-cbind(tllll26,P4)
    tlllll26<-cbind(tlllll26,P5)
    
    C1=rmse(EBQ27[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ27[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ27[,i],sort(obsprecip[,i]))
    P4=md(EBQ27[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ27[,i],sort(obsprecip[,i]))
    
    tl27<-cbind(tl27,P1)
    tll27<-cbind(tll27,P2)
    tlll27<-cbind(tlll27,P3)
    tllll27<-cbind(tllll27,P4)
    tlllll27<-cbind(tlllll27,P5)
    
    C1=rmse(EBQ28[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ28[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ28[,i],sort(obsprecip[,i]))
    P4=md(EBQ28[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ28[,i],sort(obsprecip[,i]))
    
    tl28<-cbind(tl28,P1)
    tll28<-cbind(tll28,P2)
    tlll28<-cbind(tlll28,P3)
    tllll28<-cbind(tllll28,P4)
    tlllll28<-cbind(tlllll28,P5)
    
    C1=rmse(EBQ29[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ29[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ29[,i],sort(obsprecip[,i]))
    P4=md(EBQ29[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ29[,i],sort(obsprecip[,i]))
    
    tl29<-cbind(tl29,P1)
    tll29<-cbind(tll29,P2)
    tlll29<-cbind(tlll29,P3)
    tllll29<-cbind(tllll29,P4)
    tlllll29<-cbind(tlllll29,P5)
    
    C1=rmse(EBQ30[,i],sort(obsprecip[,i]))
    P1=C1/mean(obsprecip[,i])
    P2=pbias(EBQ30[,i],sort(obsprecip[,i]))
    P3=NSE(EBQ30[,i],sort(obsprecip[,i]))
    P4=md(EBQ30[,i],sort(obsprecip[,i]))
    P5=KGE(EBQ30[,i],sort(obsprecip[,i]))
    
    tl30<-cbind(tl30,P1)
    tll30<-cbind(tll30,P2)
    tlll30<-cbind(tlll30,P3)
    tllll30<-cbind(tllll30,P4)
    tlllll30<-cbind(tlllll30,P5)
    
  }
  
  ccl1 = rbind(tl1,tl2,tl3,tl4,tl5,tl6,tl7,tl8,tl9,tl10,tl11,tl12,tl13,tl14,tl15,tl16,tl17,tl18,tl19,tl20,tl21,tl22,tl23,tl24,tl25,tl26,tl27,tl28,tl29,tl30)
  cu2 = rbind(tll1,tll2,tll3,tll4,tll5,tll6,tll7,tll8,tll9,tll10,tll11,tll12,tll13,tll14,tll15,tll16,tll17,tll18,tll19,tll20,tll21,tll22,tll23,tll24,tll25,tll26,tll27,tll28,tll29,tll30)
  ccl2 = abs(cu2)
  ccl3 = rbind(tlll1,tlll2,tlll3,tlll4,tlll5,tlll6,tlll7,tlll8,tlll9,tlll10,tlll11,tlll12,tlll13,tlll14,tlll15,tlll16,tlll17,tlll18,tlll19,tlll20,tlll21,tlll22,tlll23,tlll24,tlll25,tlll26,tlll27,tlll28,tlll29,tlll30)
  ccl4 = rbind(tllll1,tllll2,tllll3,tllll4,tllll5,tllll6,tllll7,tllll8,tllll9,tllll10,tllll11,tllll12,tllll13,tllll14,tllll15,tllll16,tllll17,tllll18,tllll19,tllll20,tllll21,tllll22,tllll23,tllll24,tllll25,tllll26,tllll27,tllll28,tllll29,tllll30)
  ccl5 = rbind(tlllll1,tlllll2,tlllll3,tlllll4,tlllll5,tlllll6,tlllll7,tlllll8,tlllll9,tlllll10,tlllll11,tlllll12,tlllll13,tlllll14,tlllll15,tlllll16,tlllll17,tlllll18,tlllll19,tlllll20,tlllll21,tlllll22,tlllll23,tlllll24,tlllll25,tlllll26,tlllll27,tlllll28,tlllll29,tlllll30)
  
  # write.csv(ccl1,"F:/Daily Precipitation/80-100/Combination/Performance/Gamma NorESM2-MM NRMSE.csv")
  # write.csv(ccl2,"F:/Daily Precipitation/80-100/Combination/Performance/Gamma NorESM2-MM PBIAS.csv")
  # write.csv(ccl3,"F:/Daily Precipitation/80-100/Combination/Performance/Gamma NorESM2-MM NSE.csv")
  # write.csv(ccl4,"F:/Daily Precipitation/80-100/Combination/Performance/Gamma NorESM2-MM MD.csv")
  # write.csv(ccl5,"F:/Daily Precipitation/80-100/Combination/Performance/Gamma NorESM2-MM KGE.csv")
  
  ################## 표준화
  Final_NRMSE1 <- c()
  Final_PBIAS1 <- c()
  
  Final_NSE1 <- c()
  
  Final_MD1 <- c()
  
  Final_KGE1 <- c()
  
  Final_NRMSE <- c()
  Final_PBIAS <- c()
  Final_NSE <- c()
  Final_MD <- c()
  Final_KGE <- c()
  
  
  for (i in 1:30) {
    NRMSE1=1+((min(ccl1[,1]) - ccl1[i,1])/(max(ccl1[,1])-min(ccl1[,1])))
    PBIAS1=1+((min(ccl1[,1]) - ccl1[i,1])/(max(ccl1[,1])-min(ccl1[,1])))
    NSE1=((ccl1[i,1] - min(ccl1[,1]))/(max(ccl1[,1])-min(ccl1[,1])))
    MD1=((ccl1[i,1] - min(ccl1[,1]))/(max(ccl1[,1])-min(ccl1[,1])))
    KGE1=((ccl1[i,1] - min(ccl1[,1]))/(max(ccl1[,1])-min(ccl1[,1])))
    
    Final_NRMSE1 = rbind(NRMSE1, Final_NRMSE1)
    Final_PBIAS1 = rbind(PBIAS1, Final_PBIAS1)
    Final_NSE1 = rbind(NSE1, Final_NSE1)
    Final_MD1 = rbind(MD1, Final_MD1)
    Final_KGE1 = rbind(KGE1, Final_KGE1)
    
  }
  # 이부분을 1행부터 22행까지 
  
  Final_NRMSE = Final_NRMSE1
  Final_PBIAS = Final_PBIAS1
  Final_NSE = Final_NSE1
  Final_MD = Final_MD1
  Final_KGE = Final_KGE1
  
  
  Total = (Final_NRMSE[,] + Final_PBIAS[,]+ Final_NSE[,]+ Final_MD[,]+Final_KGE[,])/5
  
  
  #### 30까지 평균
  
  uk1<-data.frame(Total)
  
  colnames(uk1) <- colnames(obsprecip)[1]
  rownames(uk1) <- c("OBS70","OBS71","OBS72","OBS73","OBS74","OBS75","OBS76","OBS77","OBS78","OBS79","OBS80","OBS81","OBS82","OBS83","OBS84","OBS85","OBS86","OBS87","OBS88","OBS89","OBS90","OBS91","OBS92","OBS93","OBS94","OBS95","OBS96","OBS97","OBS98","OBS99")
  
  # write.csv(uk1,"F:/Daily Precipitation/80-100/Del/Delta 80-100 Gamma RMSE NorESM2-MM Performance.csv")
  
  ################### 관측소별 분위 결정 
  name_v = rownames(uk1)
  
  OBS_list = list(
    
    OBS70 = "OBS70",
    OBS71 = "OBS71",
    OBS72 = "OBS72",
    OBS73 = "OBS73",
    OBS74 = "OBS74",
    OBS75 = "OBS75",
    OBS76 = "OBS76",
    OBS77 = "OBS77",
    OBS78 = "OBS78",
    OBS79 = "OBS79",
    OBS80 = "OBS80",
    OBS81 = "OBS81",
    OBS82 = "OBS82",
    OBS83 = "OBS83",
    OBS84 = "OBS84",
    OBS85 = "OBS85",
    OBS86 = "OBS86",
    OBS87 = "OBS87",
    OBS88 = "OBS88",
    OBS89 = "OBS89",
    OBS90 = "OBS90",
    OBS91 = "OBS91",
    OBS92 = "OBS92",
    OBS93 = "OBS93",
    OBS94 = "OBS94",
    OBS95 = "OBS95",
    OBS96 = "OBS96",
    OBS97 = "OBS97",
    OBS98 = "OBS98",
    OBS99 = "OBS99"
    )
  
  qempty_list =list(
    
    OBS70 = EBQ1,
    OBS71 = EBQ2,
    OBS72 = EBQ3,
    OBS73 = EBQ4,
    OBS74 = EBQ5,
    OBS75 = EBQ6,
    OBS76 = EBQ7,
    OBS77 = EBQ8,
    OBS78 = EBQ9,
    OBS79 = EBQ10,
    OBS80 = EBQ11,
    OBS81 = EBQ12,
    OBS82 = EBQ13,
    OBS83 = EBQ14,
    OBS84 = EBQ15,
    OBS85 = EBQ16,
    OBS86 = EBQ17,
    OBS87 = EBQ18,
    OBS88 = EBQ19,
    OBS89 = EBQ20,
    OBS90 = EBQ21,
    OBS91 = EBQ22,
    OBS92 = EBQ23,
    OBS93 = EBQ24,
    OBS94 = EBQ25,
    OBS95 = EBQ26,
    OBS96 = EBQ27,
    OBS97 = EBQ28,
    OBS98 = EBQ29,
    OBS99 = EBQ30
    )
  
  
  #u1<-data.frame(quantile(substr(utcl,1,22)[,1],qempty14[,1]))
  
  
  PERIOD<-c()
  Result<-c()
  # i=7
  for (i in 1){
    assign_obsdata = data.frame(OBS_list[name_v[which.max(uk1[,i])]])[,i]
    assign_qempty = data.frame(qempty_list[name_v[which.max(uk1[,i])]])[,i]
    #assign_df = data.frame(quantile(assign_obsdata,assign_qempty))
    #assign(paste0("u",i),assign_df)
    PERIOD = cbind(PERIOD, which.max(uk1[,i]))
    Result = cbind(Result,assign_qempty)
  }
  
  
  # write.csv(PERIOD,"F:/Daily Precipitation/80-100/Total per/Period 80-100 Gamma delta NorESM2-MM.csv")
  # write.csv(OOBT,"Period Fix RMSE NorESM2-MM.csv")
  
  Result
  colnames(Result) <- colnames(obsprecip)[1]
  
  head(Result)
}

summary(Result)
# 강릉         
# Min.   :  0.0000  
# 1st Qu.:  0.0000  
# Median :  0.0000  
# Mean   :  4.0798  
# 3rd Qu.:  0.0000  
# Max.   :339.4162 


################ 시계열
# globalVar_inpConfig = "F:/Daily Precipitation/80-100/Col"



modelData= data.frame(Result)
colnames(Result) <- colnames(obsprecip)[1:22]
obsData= obsprecip
seqList = seq(1, 22)
dataL2 = tibble::tibble()
dataL3 = tibble::tibble()


# LSH
# seqInfo = 2
for (seqInfo in seqList) {
  
  colModelName = colnames(modelData)[seqInfo]
  colObsName = colnames(obsData)[seqInfo]
  
  modelDataL1 = modelData %>%
    dplyr::select(colModelName) %>%
    dplyr::rename("model" = colModelName) %>%
    dplyr::arrange(model)
  
  
  obsDataL1 = obsData %>%
    dplyr::select(colObsName) %>%
    dplyr::rename(
      "obs" = colObsName
    ) %>%
    tibble::rowid_to_column() %>%
    dplyr::arrange(obs) %>%
    dplyr::bind_cols(modelDataL1) %>%
    dplyr::arrange(rowid) %>%
    dplyr::bind_cols(colModelName = colModelName, colObsName = colObsName)
  
  dataL2 = dplyr::bind_rows(dataL2, obsDataL1)
  
  obsDataL2 = obsDataL1 %>%
    dplyr::select(obs, model)
  
  if (seqInfo == 1) {
    dataL3 = obsDataL2
  } else {
    dataL3 = dplyr::bind_cols(dataL3, obsDataL2)
  }
}


saveFile1 = sprintf("%s/%s", globalVar_inpConfig, "R FDGQM NorESM2-MM historical FIX.csv")
readr::write_csv(dataL2, file = saveFile1)

saveFile2 = sprintf("%s/%s", globalVar_inpConfig, "C FDGQM NorESM2-MM historical FIX.csv")
readr::write_csv(dataL3, file = saveFile2)

################### 
setwd("F:/Daily Precipitation/80-100/Col")
dataL1=read.csv("C FDGQM NorESM2-MM historical FIX.csv")
m=dataL1[,seq(from=2, to=44, by=2)]
m = data.frame(m)
colnames(m) <- colnames(obsprecip)[1:22]
write.csv(m,"F:/Daily Precipitation/80-100/Data/FDGQM NorESM2-MM historical.csv")
########

lltc=rmse(m,obsprecip)
write.csv(lltc,"F:/Daily Precipitation/80-100/Per/FDGQM NorESM2-MM historical performance.csv")
