
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
# R을 이용한 지상관측소 별로 3개 분포함수 및 최적 평가지표 결정

# 1. 현재 데이터는 3개의 분포함수(Weibull, Gamma, Lnorm)을 사용하였습니다! 
# 평가지표 결과는 NRMSE, Pbias, NSE, MD, KGE 이렇게 뽑아냈고 
# 5개의 평가지표의 최대최소 표준화를 토대로 어떤 분포에서 가장 좋은 구간을 알아내는것입니다!

# 2. 그후 선택된 구간이전에서 가장 좋은 성능을 나타내는 분포함수를 
# 5개의 평가지표로 결정해야 합니다. 결정하는 방법은 1의 방법과 같습니다!

# 3. 마지막으로 두개의 분포함수 결과를 합쳐주시면 됩니다!

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0323"
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

# stnList = c("강릉", "광주", "군산")
stnList = c("강릉", "광주", "군산", "대구", "대전", "목포", "부산", "서산", "서울", "속초", "여수", "울릉도", "울산", "인천", "전주", "제주", "진주", "천안", "청주", "추풍령", "통영", "포항")

probRefData = tibble::tibble(prob = seq(0.70, 0.99, 0.01))

# stnInfo = stnList[1]
dataL1 = tibble::tibble()
for (stnInfo in stnList) {

  cat( sprintf("[CHECK] stnInfo : %s", stnInfo), "\n" )

  # NRMSE
  filePattern = sprintf("%s/%s/%s_ACCESS-ESM1-5 statDataP1_NRMSE.csv", "Performance", "Gamma", stnInfo)
  fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  data1 = readr::read_csv(fileInfo, progress = FALSE, show_col_types = FALSE)
  
  filePattern = sprintf("%s/%s/%s_ACCESS-ESM1-5 statDataP1_NRMSE.csv", "Performance", "Log", stnInfo)
  fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  data2 = readr::read_csv(fileInfo, progress = FALSE, show_col_types = FALSE)
  
  filePattern = sprintf("%s/%s/%s_ACCESS-ESM1-5 statDataP1_NRMSE.csv", "Performance", "Weibull", stnInfo)
  fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  data3 = readr::read_csv(fileInfo, progress = FALSE, show_col_types = FALSE)
  
  # Pbias
  filePattern = sprintf("%s/%s/%s_ACCESS-ESM1-5 statDataP2_Pbias.csv", "Performance", "Gamma", stnInfo)
  fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  data4 = readr::read_csv(fileInfo, progress = FALSE, show_col_types = FALSE)
  
  filePattern = sprintf("%s/%s/%s_ACCESS-ESM1-5 statDataP2_Pbias.csv", "Performance", "Log", stnInfo)
  fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  data5 = readr::read_csv(fileInfo, progress = FALSE, show_col_types = FALSE)
  
  filePattern = sprintf("%s/%s/%s_ACCESS-ESM1-5 statDataP2_Pbias.csv", "Performance", "Weibull", stnInfo)
  fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  data6 = readr::read_csv(fileInfo, progress = FALSE, show_col_types = FALSE)
  
  # NSE
  filePattern = sprintf("%s/%s/%s_ACCESS-ESM1-5 statDataP3_NSE.csv", "Performance", "Gamma", stnInfo)
  fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  data7 = readr::read_csv(fileInfo, progress = FALSE, show_col_types = FALSE)
  
  filePattern = sprintf("%s/%s/%s_ACCESS-ESM1-5 statDataP3_NSE.csv", "Performance", "Log", stnInfo)
  fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  data8 = readr::read_csv(fileInfo, progress = FALSE, show_col_types = FALSE)
  
  filePattern = sprintf("%s/%s/%s_ACCESS-ESM1-5 statDataP3_NSE.csv", "Performance", "Weibull", stnInfo)
  fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  data9 = readr::read_csv(fileInfo, progress = FALSE, show_col_types = FALSE)

  # MD
  filePattern = sprintf("%s/%s/%s_ACCESS-ESM1-5 statDataP4_MD.csv", "Performance", "Gamma", stnInfo)
  fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  data10 = readr::read_csv(fileInfo, progress = FALSE, show_col_types = FALSE)
  
  filePattern = sprintf("%s/%s/%s_ACCESS-ESM1-5 statDataP4_MD.csv", "Performance", "Log", stnInfo)
  fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  data11 = readr::read_csv(fileInfo, progress = FALSE, show_col_types = FALSE)
  
  filePattern = sprintf("%s/%s/%s_ACCESS-ESM1-5 statDataP4_MD.csv", "Performance", "Weibull", stnInfo)
  fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  data12 = readr::read_csv(fileInfo, progress = FALSE, show_col_types = FALSE)
  
  # KGE
  filePattern = sprintf("%s/%s/%s_ACCESS-ESM1-5 statDataP5_KGE.csv", "Performance", "Gamma", stnInfo)
  fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  data13 = readr::read_csv(fileInfo, progress = FALSE, show_col_types = FALSE)
  
  filePattern = sprintf("%s/%s/%s_ACCESS-ESM1-5 statDataP5_KGE.csv", "Performance", "Log", stnInfo)
  fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  data14 = readr::read_csv(fileInfo, progress = FALSE, show_col_types = FALSE)
  
  filePattern = sprintf("%s/%s/%s_ACCESS-ESM1-5 statDataP5_KGE.csv", "Performance", "Weibull", stnInfo)
  fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  data15 = readr::read_csv(fileInfo, progress = FALSE, show_col_types = FALSE)
  
  # NR=rbind(data1,data2,data3)
  # Pb=rbind(data4,data5,data6)
  # NS=rbind(data7,data8,data9)
  # MD=rbind(data10,data11,data12)
  # KG=rbind(data13,data14,data15)
  
  NR = probRefData %>% 
    dplyr::left_join(data1, by = c("prob" = "prob")) %>% 
    dplyr::left_join(data2, by = c("prob" = "prob")) %>% 
    dplyr::left_join(data3, by = c("prob" = "prob")) %>% 
    tidyr::gather(-prob, key = "key", value = "val") %>% 
    dplyr::select(-key)
  
  Pb = probRefData %>% 
    dplyr::left_join(data4, by = c("prob" = "prob")) %>% 
    dplyr::left_join(data5, by = c("prob" = "prob")) %>% 
    dplyr::left_join(data6, by = c("prob" = "prob")) %>% 
    tidyr::gather(-prob, key = "key", value = "val") %>% 
    dplyr::select(-key)
  
  NS = probRefData %>% 
    dplyr::left_join(data7, by = c("prob" = "prob")) %>% 
    dplyr::left_join(data8, by = c("prob" = "prob")) %>% 
    dplyr::left_join(data9, by = c("prob" = "prob")) %>% 
    tidyr::gather(-prob, key = "key", value = "val") %>% 
    dplyr::select(-key)
  
  MD = probRefData %>% 
    dplyr::left_join(data10, by = c("prob" = "prob")) %>% 
    dplyr::left_join(data11, by = c("prob" = "prob")) %>% 
    dplyr::left_join(data12, by = c("prob" = "prob")) %>% 
    tidyr::gather(-prob, key = "key", value = "val") %>% 
    dplyr::select(-key)
  
  KG = probRefData %>% 
    dplyr::left_join(data13, by = c("prob" = "prob")) %>% 
    dplyr::left_join(data14, by = c("prob" = "prob")) %>% 
    dplyr::left_join(data15, by = c("prob" = "prob")) %>% 
    tidyr::gather(-prob, key = "key", value = "val") %>% 
    dplyr::select(-key)
  
  RR=cbind(NR,Pb[,2],NS[,2],MD[,2],KG[,2])
  prob = nrow(NR)
  RNR = c()
  RPb = c()
  RNS = c()
  RMD = c()
  RKG = c()
  for (i in 1:prob) {
    NR1 = 1+(min(RR[,2], na.rm = TRUE)-RR[i,2])/(max(RR[,2], na.rm = TRUE)-min(RR[,2], na.rm = TRUE))  
    RNR = c(RNR,NR1)
    PB1 = 1+(min(abs(RR[,3]), na.rm = TRUE)-abs(RR[i,3])) / (max(abs(RR[,3]), na.rm = TRUE)-min(abs(RR[,3]), na.rm = TRUE))  
    RPb = c(RPb,NR1)
    NS1 = (RR[i,4] - min(RR[,4], na.rm = TRUE)) / (max(RR[,4], na.rm = TRUE)-min(RR[,4], na.rm = TRUE))  
    RNS = c(RNS,NS1)
    MD1 = (RR[i,5] - min(RR[,5], na.rm = TRUE)) / (max(RR[,5], na.rm = TRUE)-min(RR[,5], na.rm = TRUE))  
    RMD = c(RMD,MD1)
    KG1 = (RR[i,6] - min(RR[,6], na.rm = TRUE)) / (max(RR[,6], na.rm = TRUE)-min(RR[,6], na.rm = TRUE))  
    RKG = c(RKG,KG1)
  }
  
  # Final=cbind(RNR,RPb,RNS,RMD,RKG)
  fnlData = cbind(RNR,RPb,RNS,RMD,RKG) %>% 
    rowSums() %>% 
    as.tibble() %>% 
    dplyr::bind_cols(dplyr::bind_rows(data.frame(model = rep("Gamma", 30)), data.frame(model = rep("Log", 30)), data.frame(model = rep("Weibull", 30)))) %>% 
    dplyr::bind_cols(dplyr::bind_rows(probRefData, probRefData, probRefData)) %>% 
    dplyr::mutate(stn = stnInfo)
    
  
  dataL1 = dplyr::bind_rows(dataL1, fnlData)
}


modelList = c("Gamma", "Log", "Weibull")
for (modelInfo in modelList) {
  
  cat( sprintf("[CHECK] modelInfo : %s", modelInfo), "\n" )
  
  dataL2 = dataL1 %>% 
    dplyr::filter(model == modelInfo) %>% 
    tidyr::replace_na(list(value = 0)) %>% 
    dplyr::select(-model) %>% 
    tidyr::spread(prob, key = "stn", value = "value")
  
  saveFile = sprintf("%s/%s_%s_%s", globalVar$outPath, serviceName, modelInfo, "Delta ACCESS-ESM1-5 Performance.csv")
  readr::write_csv(dataL2, file = saveFile)
}
