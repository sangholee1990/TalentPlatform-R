
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
# R을 이용한 그룹별 평균 및 순위 산정 그리고 컬럼 검사

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0318"
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
# 라이브러리 읽기
library(tidyverse)
library(readr)
library(lubridate)
library(openxlsx)


#================================================
# change %
#================================================
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "Historical.csv"))
obsdata = readr::read_csv(file = fileInfo)
  
y<-obsdata[,1]
m<-obsdata[,2]

# annual, summer, winter series ##

# 연 합계
for (yr in 1986:2005){
  dat<-obsdata[which(y==yr),]
  if (yr==1986) re<-colSums(dat[,3:ncol(dat)])
  if(yr>1986) re<-rbind(re,colSums(dat[,3:ncol(dat)]))
}
rownames(re)<-seq(1986,2005,1)
colnames(re)<-colnames(obsdata[,3:ncol(obsdata)])
Oan<-re

# 겨울 (12, 1, 2), 여름 (6, 7, 8) 합계
for (yr in 1986:2005){
  sdat<-colSums(obsdata[which(y==yr & m>5 & m<9),3:ncol(obsdata)])
  wdat<-colSums(obsdata[which(y==yr & (m<3 | m>11)),3:ncol(obsdata)])
  if (yr==1986) { sres<-sdat ; wres<-wdat}
  if(yr>1986) {sres<-rbind(sres,sdat); wres<-rbind(wres,wdat) } 
}
colnames(sres)<-colnames(obsdata[,3:ncol(obsdata)])
colnames(wres)<-colnames(obsdata[,3:ncol(obsdata)])
rownames(wres)<-seq(1986,2005,1)
rownames(sres)<-seq(1986,2005,1)
Osum<-sres
Owin<-wres

# 연, 겨울, 여름에 대한 평균
Ores<-rbind(colMeans(Oan),colMeans(Osum),colMeans(Owin))

##############Future#######################################################
fileInfo2 = Sys.glob(file.path(globalVar$inpPath, serviceName, "SSP245 future data.csv"))
futdata = readr::read_csv(file = fileInfo2)

y<-futdata[,1]
m<-futdata[,2]

# annual, summer, winter series ##

# 미래 연 합계
for (yr in 2021:2040){
  dat<-futdata[which(y==yr),]
  if (yr==2021) re<-colSums(dat[,3:ncol(dat)],na.rm = T)
  if(yr>2021) re<-rbind(re,colSums(dat[,3:ncol(dat)],na.rm = T))
}
rownames(re)<-seq(2021,2040,1)
colnames(re)<-colnames(futdata[,3:ncol(futdata)])

# 미래 겨울 (12, 1, 2), 여름 (6, 7, 8) 합계
for (yr in 2021:2040){
  sdat<-colSums(futdata[which(y==yr & m>5 & m<9),3:ncol(futdata)])
  wdat<-colSums(futdata[which(y==yr & (m<3 | m>11)),3:ncol(futdata)])
  if (yr==2021) { sres<-sdat ; wres<-wdat}
  if(yr>2021) {sres<-rbind(sres,sdat); wres<-rbind(wres,wdat) } 
}
colnames(sres)<-colnames(futdata[,3:ncol(futdata)])
colnames(wres)<-colnames(futdata[,3:ncol(futdata)])
rownames(wres)<-seq(2021,2040,1)
rownames(sres)<-seq(2021,2040,1)

# 미래 연, 겨울, 여름에 대한 평균
P1<-rbind(colMeans(re),colMeans(sres),colMeans(wres))  

for (yr in 2051:2070){
  dat<-futdata[which(y==yr),]
  if (yr==2051) re<-colSums(dat[,3:ncol(dat)],na.rm = T)
  if(yr>2051) re<-rbind(re,colSums(dat[,3:ncol(dat)],na.rm = T))
}
rownames(re)<-seq(2051,2070,1)
colnames(re)<-colnames(futdata[,3:ncol(futdata)])

for (yr in 2051:2070){
  sdat<-colSums(futdata[which(y==yr & m>5 & m<9),3:ncol(futdata)],na.rm = T)
  wdat<-colSums(futdata[which(y==yr & (m<3 | m>11)),3:ncol(futdata)],na.rm = T)
  if (yr==2051) { sres<-sdat ; wres<-wdat}
  if(yr>2051) {sres<-rbind(sres,sdat); wres<-rbind(wres,wdat) } 
}
colnames(sres)<-colnames(futdata[,3:ncol(futdata)])
colnames(wres)<-colnames(futdata[,3:ncol(futdata)])
rownames(wres)<-seq(2051,2070,1)
rownames(sres)<-seq(2051,2070,1)

P2<-rbind(colMeans(re,na.rm = T),colMeans(sres,na.rm = T),colMeans(wres,na.rm = T))

for (yr in 2081:2100){
  dat<-futdata[which(y==yr),]
  if (yr==2081) re<-colSums(dat[,3:ncol(dat)],na.rm = T)
  if(yr>2081) re<-rbind(re,colSums(dat[,3:ncol(dat)],na.rm = T))
}
rownames(re)<-seq(2081,2100,1)
colnames(re)<-colnames(futdata[,3:ncol(futdata)])

for (yr in 2081:2100){
  sdat<-colSums(futdata[which(y==yr & m>5 & m<9),3:ncol(futdata)],na.rm = T)
  wdat<-colSums(futdata[which(y==yr & (m<3 | m>11)),3:ncol(futdata)],na.rm = T)
  if (yr==2081) { sres<-sdat ; wres<-wdat}
  if(yr>2081) {sres<-rbind(sres,sdat); wres<-rbind(wres,wdat) } 
}
colnames(sres)<-colnames(futdata[,3:ncol(futdata)])
colnames(wres)<-colnames(futdata[,3:ncol(futdata)])
rownames(wres)<-seq(2081,2100,1)
rownames(sres)<-seq(2081,2100,1)

P3<-rbind(colMeans(re,na.rm = T),colMeans(sres,na.rm = T),colMeans(wres,na.rm = T))

RES<-cbind((P1[1,]-Ores[1,])/Ores[1,],(P1[2,]-Ores[2,])/Ores[2,],(P1[3,]-Ores[3,])/Ores[3,],
           (P2[1,]-Ores[1,])/Ores[1,],(P2[2,]-Ores[2,])/Ores[2,],(P2[3,]-Ores[3,])/Ores[3,],
           (P3[1,]-Ores[1,])/Ores[1,],(P3[2,]-Ores[2,])/Ores[2,],(P3[3,]-Ores[3,])/Ores[3,])

#RES<-cbind((P1[1,]-Ores[1,]),(P1[2,]-Ores[2,]),(P1[3,]-Ores[3,]),
#           (P2[1,]-Ores[1,]),(P2[2,]-Ores[2,]),(P2[3,]-Ores[3,]))
RESPER<-RES*100
colnames(RESPER)<-c("A-P1","S-P1","W-P1","A-P2","S-P2","W-P2","A-P3","S-P3","W-P3")
#setwd("C:\\Users\\mo7am\\Desktop\\SSD\\PhD\\GCM Selection\\Results\\Projection\\Change")
# write.csv(RESPER,"D:/hydroperfor/PROJECTIONrrrr (1)/HadGEM-ES/HadGEM2-ES_pr_changeper.csv")
#if (i==1) RES<-cbind(P1[1,]-Ores[1,],P2[1,]-Ores[1,])
#if(i>1) RES<-cbind(RES,P1[1,]-Ores[1,],P2[1,]-Ores[1,])



#setwd("C:\\Users\\mo7am\\Desktop\\SSD\\PhD\\GCM Selection\\Results\\Projection\\Change")
#write.csv(RES,"Change_8.5_R_amount.csv")
