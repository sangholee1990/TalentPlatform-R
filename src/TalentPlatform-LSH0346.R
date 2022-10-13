
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
# R을 이용한 swmmr 패키지를 활용한 연계 작업 그리고 DEoptim를 통해 유출량 모의

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0346"
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

# ******************************************************************************
# 라이브러리 읽기
# ******************************************************************************
# library(swmmr)
# library(DEoptim)
# library(xts)
# library(hydroGOF)
library(dplyr)

# 난수 정의
set.seed(84)




########################################################################
# rm(list = ls()); gc()

# setting path where the data is
# path <- "F:/REA/Yongsan"
# setwd(path)

# input file name
input_file_name1 <- "historical"
input_file_name2 <- "LSTM SSP245 21-25"


# reading csv
# 1 --> left side input
# 2 --> right side input
# inputt1 <- read.csv(paste0(input_file_name1, ".csv"))
# inputt2 <- read.csv(paste0(input_file_name2, ".csv"))

# 현재 모형 맟 실측
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "historical.csv"))
inputt1 = read.csv(fileInfo)

# 미래 모형
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "LSTM SSP245 21-25.csv"))
inputt2 = read.csv(fileInfo)

inpul1<-inputt1[,-1]
input1<-inpul1[,]
inpul2<-inputt2[,-1]
input2<-inpul2[,]

# how many models using at a time
# 현재 모형 개수
model_num <- ncol(input1)-1

# setting specific indices to select
indices <- 1:60

# deltaT matrix calculation
deltaT_mat <- data.frame(matrix(NA, 1, ncol(input1)))

# 앙상블 모형
colnames(deltaT_mat) <- c(colnames(input1)[1:model_num], "Ensemble")

# 각 모형에 따라 -절대값 (미래의 평균 - 현재의 평균) 차이
for (i in 1:model_num) {
  deltaT_mat[1, i] <- -abs(mean(input2[indices, i]) - mean(input1[indices, i]))
}

# to make calculation easy setting 1st element of R1_avg as deltaT_Ensemble
R1_avg <- deltaT_mat[1, "Ensemble"]

# 평균 수행
deltaT_mat$Ensemble <- mean(as.numeric(deltaT_mat[1, 1:model_num]))

# Bi + Ri together
R_mat <- data.frame(matrix(NA, 1, model_num))
colnames(R_mat) <- paste0("B", 1:model_num)

# B1, 2, 3, 4 calculation
# 동일한 컬럼에 대한 평균 (예측 - 실측) 차이
for (i in 1:model_num) {
  R_mat[1, i] <- mean(input1[indices, i]) - mean(input1[indices, 12])
}

# i = 1
# moving average calculation
# 이동평균
mov_avg_list <- c()
for (i in 1:7306) { # (nrow(input1)-119)
  mov_avg_list1 <- c(mov_avg_list, mean(input1$OBS[i:i+35]))
  mov_avg_list <- na.omit(mov_avg_list1)
}

#  [1]  2.023548387  2.349677419  2.621428571  2.542258065  1.456000000  2.388709677 11.009333330
# [8] 74.761935480 27.454838710  6.099333333  3.538709677  2.628000000  2.906451613  2.471612903
# [15]  8.126785714  5.406451613  7.028000000  8.954516129  6.461000000 54.699677420 67.462258060
# [22] 28.588333330 11.713870970 11.268666670 13.213548390

# 이동평균의 차이
mov_avg_diff <- max(mov_avg_list) - min(mov_avg_list)



#==================================
# Fortran을 이용한 반복 수행
#==================================
# setwd("C:/Users/user/Desktop/TEST/바탕 화면/Test")
# setwd("C:/Users/user/Desktop/TEST")

dimD = tibble(deltaT_mat) %>%
  tidyr::gather(key = "key", value = "val") %>%
  dplyr::select(val)

dimR = tibble(R_mat)%>%
  tidyr::gather(key = "key", value = "val") %>%
  dplyr::select(val)

dimAvg = tibble(R1_avg) %>%
  tidyr::gather(key = "key", value = "val") %>%
  dplyr::select(val)

dimMovAvg = tibble(mov_avg_diff) %>%
  tidyr::gather(key = "key", value = "val") %>%
  dplyr::select(val)

# utils::write.table(dimD, file = "C:/Users/user/Desktop/TEST/input-dimD.dat", col.names = FALSE, row.names = FALSE)
# utils::write.table(dimR, file = "C:/Users/user/Desktop/TEST/input-dimR.dat", col.names = FALSE, row.names = FALSE)
# utils::write.table(dimAvg, file = "C:/Users/user/Desktop/TEST/input-dimAvg.dat", col.names = FALSE, row.names = FALSE)
# utils::write.table(dimMovAvg, file = "C:/Users/user/Desktop/TEST/input-dimMovAvg.dat", col.names = FALSE, row.names = FALSE)
# 
# getwd()
# system(paste(
#   "gfortran"
#   , "./CallFortranInR.f90"
# ))
# 
# system(paste(
#   "./a.exe"
# ))

# a<-read.table("C:/Users/user/Desktop/TEST/result-dimR.dat")
# b<-read.table("C:/Users/user/Desktop/TEST/result-dimD.dat")
# c<-read.table("C:/Users/user/Desktop/TEST/result-dimAvg.dat")
# write.csv(a,"F:/REA/Yongsan/R/LSTM SSP245 21-25 R.csv")
# write.csv(b,"F:/REA/Yongsan/D/LSTM SSP245 21-25 D.csv")
# write.csv(c,"F:/REA/Yongsan/AVG/LSTM SSP245 21-25 AVG.csv")

