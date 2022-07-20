
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
# R을 이용한 22개 관측소에 대한 감마분포 자료처리 고도화

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
library(dplyr)
library(fitdistrplus)
library(stats)
library(hydroGOF)

# setwd("F:/Daily Precipitation")

# modprecip=data=read.csv("22_IDW_pr_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_19850101-20141231.csv")[,-1]
# modprecip = modprecip
# obsprecip=data1=read.csv("22_real pr 1985-2014.csv")[,-1]
# obsprecip = obsprecip

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "22_IDW_pr_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_19850101-20141231.csv"))
modprecip=readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))[,-1]
modprecip = modprecip %>% 
  as.data.frame()

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "22_real+pr+1985-2014.csv"))
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


UZ1=data.frame(replace(Q1[,1],which(Q1[,1] <= 0.5),0.001))
UZ2=data.frame(replace(Q1[,2],which(Q1[,2] <= 0.5),0.001))
UZ3=data.frame(replace(Q1[,3],which(Q1[,3] <= 0.5),0.001))
UZ4=data.frame(replace(Q1[,4],which(Q1[,4] <= 0.5),0.001))
UZ5=data.frame(replace(Q1[,5],which(Q1[,5] <= 0.5),0.001))
UZ6=data.frame(replace(Q1[,6],which(Q1[,6] <= 0.5),0.001))
UZ7=data.frame(replace(Q1[,7],which(Q1[,7] <= 0.5),0.001))
UZ8=data.frame(replace(Q1[,8],which(Q1[,8] <= 0.5),0.001))
UZ9=data.frame(replace(Q1[,9],which(Q1[,9] <= 0.5),0.001))

# LSH
# UZ40=data.frame(replace(Q1[,10],which(Q1[,10] <= 0.5),0.001))
# UZ41=data.frame(replace(Q1[,11],which(Q1[,11] <= 0.5),0.001))
# UZ42=data.frame(replace(Q1[,12],which(Q1[,12] <= 0.5),0.001))
# UZ43=data.frame(replace(Q1[,13],which(Q1[,13] <= 0.5),0.001))
# UZ44=data.frame(replace(Q1[,14],which(Q1[,14] <= 0.5),0.001))
# UZ45=data.frame(replace(Q1[,15],which(Q1[,15] <= 0.5),0.001))
# UZ46=data.frame(replace(Q1[,16],which(Q1[,16] <= 0.5),0.001))
# UZ47=data.frame(replace(Q1[,17],which(Q1[,17] <= 0.5),0.001))
# UZ48=data.frame(replace(Q1[,18],which(Q1[,18] <= 0.5),0.001))
# UZ49=data.frame(replace(Q1[,19],which(Q1[,19] <= 0.5),0.001))
# UZ40=data.frame(replace(Q1[,20],which(Q1[,20] <= 0.5),0.001))
# UZ41=data.frame(replace(Q1[,21],which(Q1[,21] <= 0.5),0.001))
# UZ42=data.frame(replace(Q1[,22],which(Q1[,22] <= 0.5),0.001))

UZ10=data.frame(replace(Q1[,10],which(Q1[,10] <= 0.5),0.001))
UZ11=data.frame(replace(Q1[,11],which(Q1[,11] <= 0.5),0.001))
UZ12=data.frame(replace(Q1[,12],which(Q1[,12] <= 0.5),0.001))
UZ13=data.frame(replace(Q1[,13],which(Q1[,13] <= 0.5),0.001))
UZ14=data.frame(replace(Q1[,14],which(Q1[,14] <= 0.5),0.001))
UZ15=data.frame(replace(Q1[,15],which(Q1[,15] <= 0.5),0.001))
UZ16=data.frame(replace(Q1[,16],which(Q1[,16] <= 0.5),0.001))
UZ17=data.frame(replace(Q1[,17],which(Q1[,17] <= 0.5),0.001))
UZ18=data.frame(replace(Q1[,18],which(Q1[,18] <= 0.5),0.001))
UZ19=data.frame(replace(Q1[,19],which(Q1[,19] <= 0.5),0.001))
UZ20=data.frame(replace(Q1[,20],which(Q1[,20] <= 0.5),0.001))
UZ21=data.frame(replace(Q1[,21],which(Q1[,21] <= 0.5),0.001))
UZ22=data.frame(replace(Q1[,22],which(Q1[,22] <= 0.5),0.001))



OBZ1=data.frame(replace(OBS[,1],which(OBS[,1] <= 0.5),0.001))
OBZ2=data.frame(replace(OBS[,2],which(OBS[,2] <= 0.5),0.001))
OBZ3=data.frame(replace(OBS[,3],which(OBS[,3] <= 0.5),0.001))
OBZ4=data.frame(replace(OBS[,4],which(OBS[,4] <= 0.5),0.001))
OBZ5=data.frame(replace(OBS[,5],which(OBS[,5] <= 0.5),0.001))
OBZ6=data.frame(replace(OBS[,6],which(OBS[,6] <= 0.5),0.001))
OBZ7=data.frame(replace(OBS[,7],which(OBS[,7] <= 0.5),0.001))
OBZ8=data.frame(replace(OBS[,8],which(OBS[,8] <= 0.5),0.001))
OBZ9=data.frame(replace(OBS[,9],which(OBS[,9] <= 0.5),0.001))

# LSH
# OBZ40=data.frame(replace(OBS[,10],which(OBS[,10] <= 0.5),0.001))
# OBZ41=data.frame(replace(OBS[,11],which(OBS[,11] <= 0.5),0.001))
# OBZ42=data.frame(replace(OBS[,12],which(OBS[,12] <= 0.5),0.001))
# OBZ43=data.frame(replace(OBS[,13],which(OBS[,13] <= 0.5),0.001))
# OBZ44=data.frame(replace(OBS[,14],which(OBS[,14] <= 0.5),0.001))
# OBZ45=data.frame(replace(OBS[,15],which(OBS[,15] <= 0.5),0.001))
# OBZ46=data.frame(replace(OBS[,16],which(OBS[,16] <= 0.5),0.001))
# OBZ47=data.frame(replace(OBS[,17],which(OBS[,17] <= 0.5),0.001))
# OBZ48=data.frame(replace(OBS[,18],which(OBS[,18] <= 0.5),0.001))
# OBZ49=data.frame(replace(OBS[,19],which(OBS[,19] <= 0.5),0.001))
# OBZ40=data.frame(replace(OBS[,20],which(OBS[,20] <= 0.5),0.001))
# OBZ41=data.frame(replace(OBS[,21],which(OBS[,21] <= 0.5),0.001))
# OBZ42=data.frame(replace(OBS[,22],which(OBS[,22] <= 0.5),0.001))
OBZ10=data.frame(replace(OBS[,10],which(OBS[,10] <= 0.5),0.001))
OBZ11=data.frame(replace(OBS[,11],which(OBS[,11] <= 0.5),0.001))
OBZ12=data.frame(replace(OBS[,12],which(OBS[,12] <= 0.5),0.001))
OBZ13=data.frame(replace(OBS[,13],which(OBS[,13] <= 0.5),0.001))
OBZ14=data.frame(replace(OBS[,14],which(OBS[,14] <= 0.5),0.001))
OBZ15=data.frame(replace(OBS[,15],which(OBS[,15] <= 0.5),0.001))
OBZ16=data.frame(replace(OBS[,16],which(OBS[,16] <= 0.5),0.001))
OBZ17=data.frame(replace(OBS[,17],which(OBS[,17] <= 0.5),0.001))
OBZ18=data.frame(replace(OBS[,18],which(OBS[,18] <= 0.5),0.001))
OBZ19=data.frame(replace(OBS[,19],which(OBS[,19] <= 0.5),0.001))
OBZ20=data.frame(replace(OBS[,20],which(OBS[,20] <= 0.5),0.001))
OBZ21=data.frame(replace(OBS[,21],which(OBS[,21] <= 0.5),0.001))
OBZ22=data.frame(replace(OBS[,22],which(OBS[,22] <= 0.5),0.001))

# LSH
options(digits = 10)

# iCount = 1
dataL3 = tibble::tibble()
dataL4 = tibble::tibble()
maxDataL1 = tibble::tibble()
TotalL1 = tibble::tibble()

for (iCount in 1:ncol(obsprecip)) {
  
  obzInfo = sprintf("OBZ%s", iCount)
  uzInfo = sprintf("UZ%s", iCount)
  
  cat(obzInfo, "\n")
  cat(uzInfo, "\n")

  OBZ4 = get(obzInfo)
  UZ4 = get(uzInfo)
  
  probList = seq(0.70, 0.99, 0.01)
  CCQ1 = tibble::tibble()
  EBQ1 = tibble::tibble()
  ccl1 = tibble::tibble()
  
  statDataP1 = tibble::tibble()
  statDataP2 = tibble::tibble()
  statDataP3 = tibble::tibble()
  statDataP4 = tibble::tibble()
  statDataP5 = tibble::tibble()
  
  for (probInfo in probList) {
    
    UU = 1:length(OBZ4[,1])
    loc = data.frame(round(quantile(UU, probs = probInfo), digits = 0))
    a1 = loc[1,1]
    
    cl1=data.frame(UZ4[a1:length(OBZ4[,1]),])
    cul1=cl1[,1]
    if (mean(cul1, na.rm = TRUE) == 0.001) next
    
    cc1<-fitdist(cul1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qempty1 = data.frame(pgamma(cul1, meanlog, sdlog))
    
    cll1=data.frame(UZ4[1:a1-1,])
    cul1=cll1[,1]
    if (mean(cul1, na.rm = TRUE) == 0.001) next
    
    cc1<-fitdist(cul1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    meanlog=para[1,1]
    sdlog=para[2,1]
    qqempty1 = data.frame(pgamma(cul1, meanlog, sdlog))
    
    
    ct1=data.frame(OBZ4[a1:length(OBZ4[,1]),])
    cl1=ct1[,1]
    if (mean(cl1, na.rm = TRUE) == 0.001) next
    
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    obsempty1 = pgamma(cl1, shape = shape, rate = rate)
    QUAN1= data.frame(qgamma(qempty1[,1], shape, rate))
    
    ctl1=data.frame(OBZ4[1:a1-1,])
    cl1=ctl1[,1]
    if (mean(cl1, na.rm = TRUE) == 0.001) next
    
    cc1<-fitdist(cl1,"gamma", method="mle")
    para=data.frame(cc1$estimate)
    shape=para[1,1]
    rate=para[2,1]
    oobsempty1 = pgamma(cl1, shape, rate)
    QQUAN1= data.frame(qgamma(qqempty1[,1], shape, rate))
    
    colnames(QUAN1) = "model"
    colnames(QQUAN1) = "model"
    
    cat("probInfo : ", probInfo, "\n")
    
    # CCQ1=rbind(QQUAN1,QUAN1)
    simData = data.frame(probInfo, rbind(QQUAN1, QUAN1))
    CCQ1 = dplyr::bind_rows(CCQ1, simData)
    
    BBQ1 = simData %>% 
      dplyr::mutate(
        model = ifelse(model <= 0.5, 0, model)
      ) %>% 
      dplyr::bind_cols(obs = obsprecip[, iCount]) %>% 
      dplyr::mutate(
        name = sprintf("OBS%s",  probInfo * 100)
      )

    EBQ1 = dplyr::bind_rows(EBQ1, BBQ1)
    # cat("summary : ", summary(BBQ1), "\n")
    
    C1 = hydroGOF::rmse(BBQ1$model, sort(BBQ1$obs), na.rm = TRUE)
    P1 = C1 / mean(BBQ1$obs, na.rm = TRUE)
    P2 = hydroGOF::pbias(BBQ1$obs, sort(BBQ1$obs), na.rm = TRUE)
    P3 = hydroGOF::NSE(BBQ1$obs, sort(BBQ1$obs), na.rm = TRUE)
    P4 = hydroGOF::md(BBQ1$obs, sort(BBQ1$obs), na.rm = TRUE)
    P5 = hydroGOF::KGE(BBQ1$obs, sort(BBQ1$obs), na.rm = TRUE)
    
    statDataP1 = dplyr::bind_rows(statDataP1, tibble::tibble("prob" = probInfo, "val" = P1))
    statDataP2 = dplyr::bind_rows(statDataP2, tibble::tibble("prob" = probInfo, "val" = abs(P2)))
    statDataP3 = dplyr::bind_rows(statDataP3, tibble::tibble("prob" = probInfo, "val" = P3))
    statDataP4 = dplyr::bind_rows(statDataP4, tibble::tibble("prob" = probInfo, "val" = P4))
    statDataP5 = dplyr::bind_rows(statDataP5, tibble::tibble("prob" = probInfo, "val" = P5))
    
    tmpData = tibble::tibble(
      "NRMSE" = P1
      , "PBIAS" = abs(P2)
      , "NSE" = P3
      , "MD" = P4
      , "KGE" = P5
      , "prob" = probInfo
      , "name" = sprintf("OBS%s",  probInfo * 100)
    )
    
    ccl1 = dplyr::bind_rows(ccl1, tmpData)
  }
  
  saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "statDataP1_NRMSE.csv")
  readr::write_csv(statDataP1, file = saveFile)
  
  saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "statDataP2_PBIAS.csv")
  readr::write_csv(statDataP2, file = saveFile)
  
  saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "statDataP3_NSE.csv")
  readr::write_csv(statDataP3, file = saveFile)
  
  saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "statDataP4_MD.csv")
  readr::write_csv(statDataP4, file = saveFile)
  
  saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "statDataP5_KGE.csv")
  readr::write_csv(statDataP5, file = saveFile)
  
  
  # 표준화
  Final_NRMSE1 <- c()
  Final_PBIAS1 <- c()
  Final_NSE1 <- c()
  Final_MD1 <- c()
  Final_KGE1 <- c()
  selColName = c()
  
  # idx = 1
  for (idx in 1:length(probList)) {
    
    resChk = ccl1 %>%
      tibble::rowid_to_column() %>% 
        dplyr::mutate(prob = paste0(prob)) %>% 
        dplyr::filter(prob == probList[idx])
    
    cat("idx : ", idx, "prob : ", probList[idx], "resChk : ", nrow(resChk), "rowid : ", resChk$rowid, "\n")
    
    if (nrow(resChk) < 1) next
    
    i = resChk$rowid
    # cat("prob : ", probList[i], "\n")
    
    selColName = append(selColName, sprintf("OBS%s",  probList[idx] * 100))
    
    NRMSE1=1+((min(ccl1[,1], na.rm = TRUE) - ccl1[i,1])/(max(ccl1[,1], na.rm = TRUE)-min(ccl1[,1], na.rm = TRUE)))
    PBIAS1=1+((min(ccl1[,2], na.rm = TRUE) - ccl1[i,2])/(max(ccl1[,2], na.rm = TRUE) - min(ccl1[,2], na.rm = TRUE)))
    NSE1=((ccl1[i,3] - min(ccl1[,3], na.rm = TRUE))/(max(ccl1[,3], na.rm = TRUE)-min(ccl1[,3], na.rm = TRUE)))
    MD1=((ccl1[i,4] - min(ccl1[,4], na.rm = TRUE))/(max(ccl1[,4], na.rm = TRUE)-min(ccl1[,4], na.rm = TRUE)))
    KGE1=((ccl1[i,5] - min(ccl1[,5], na.rm = TRUE))/(max(ccl1[,5], na.rm = TRUE)-min(ccl1[,5], na.rm = TRUE)))
    
    Final_NRMSE1 = rbind(NRMSE1, Final_NRMSE1)
    Final_PBIAS1 = rbind(PBIAS1, Final_PBIAS1)
    Final_NSE1 = rbind(NSE1, Final_NSE1)
    Final_MD1 = rbind(MD1, Final_MD1)
    Final_KGE1 = rbind(KGE1, Final_KGE1)
  }
  
  colName = colnames(obsprecip)[iCount]

  Total = data.frame(Final_NRMSE1, Final_PBIAS1, Final_NSE1, Final_MD1, Final_KGE1) %>% 
    rowMeans(na.rm = TRUE) %>% 
    t() %>%
    as.tibble() %>% 
    magrittr::set_colnames(selColName) %>%
    tidyr::gather() %>%
    dplyr::mutate(
      colName = colName
    )
  
  TotalL1 = dplyr::bind_rows(TotalL1, Total)
    
  maxData = Total %>% 
    dplyr::filter(value == max(value, na.rm = TRUE))
  
  maxDataL1 = dplyr::bind_rows(maxDataL1, maxData)
  
  cat("maxData : ", maxData$key, maxData$value, "\n")

  colObsName = sprintf("obs%s", colName)
  colModelName = sprintf("model%s", colName)
  
  dataL2 = EBQ1 %>% 
    dplyr::filter(name == maxData$key) %>% 
    dplyr::select(obs, model) %>% 
    magrittr::set_colnames(c(colObsName, colModelName))
  
  saveFile = sprintf("%s/%s_%s_%s", globalVar$outPath, serviceName, colName, "R FDGQM NorESM2-MM historical FIX.csv")
  readr::write_csv(dataL2, file = saveFile)
  
  if (iCount == 1) {
    dataL3 = dataL2
  } else {
    dataL3 = dplyr::bind_rows(dataL3, dataL2)
  }
  
  rmseVal = hydroGOF::rmse(dataL2[1], dataL2[2]) %>% 
    as.tibble() %>% 
    magrittr::set_colnames(colName)
  
  if (iCount == 1) {
    dataL4 = rmseVal
  } else {
    dataL4 = dplyr::bind_rows(dataL4, rmseVal)
  }
  
}

saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "MaxData.csv")
readr::write_csv(maxDataL1, file = saveFile)

saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "TotalData.csv")
readr::write_csv(TotalL1, file = saveFile)

saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "C FDGQM NorESM2-MM historical FIX.csv")
readr::write_csv(dataL3, file = saveFile)

saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "FDGQM NorESM2-MM historical performance.csv")
readr::write_csv(dataL4, file = saveFile)
