
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


# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "Weibull", "Delta ACCESS-ESM1-5 Performance.csv"))


#normper=read.csv("Period Fix FDGQM RMSE INM-CM4-8 Performance.csv")
# setwd("D:/downdown/DQM/Weibull")
# weilper=read.csv("Delta ACCESS-ESM1-5 Performance.csv")

filePattern = sprintf("%s_%s_%s", serviceName, "Weibull", "Delta ACCESS-ESM1-5 Performance.csv")
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
weilper = readr:::read_csv(fileInfo) %>% 
  as.data.frame()

# setwd("D:/downdown/DQM/Lognormal")
# Logper=read.csv("Delta ACCESS-ESM1-5 Performance.csv")

filePattern = sprintf("%s_%s_%s", serviceName, "Log", "Delta ACCESS-ESM1-5 Performance.csv")
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
Logper = readr:::read_csv(fileInfo) %>% 
  as.data.frame()

# setwd("D:/downdown/DQM/Gamma")
# Gammaper=read.csv("Delta ACCESS-ESM1-5 Performance.csv")


filePattern = sprintf("%s_%s_%s", serviceName, "Gamma", "Delta ACCESS-ESM1-5 Performance.csv")
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
Gammaper = readr:::read_csv(fileInfo) %>% 
  as.data.frame()


fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "22_real pr 1985-2014.csv"))
# obs=read.csv("22_real pr 1985-2014.csv")
# obs1=read.csv("22_real pr 1985-2014.csv")
obs = readr:::read_csv(fileInfo, locale = locale("ko", encoding = "EUC-KR"))[,-1] %>% 
  as.data.frame()
obs1 = readr:::read_csv(fileInfo, locale = locale("ko", encoding = "EUC-KR"))[,-1] %>% 
  as.data.frame()


#n1=normper[,-1]
#rownames(n1) <- c("OBS80","OBS81","OBS82","OBS83","OBS84","OBS85","OBS86","OBS87","OBS88","OBS89","OBS90","OBS91","OBS92","OBS93","OBS94","OBS95")
#colnames(n1) <- c("NC1","NC2","NC3","NC4","NC5","NC6","NC7","NC8","NC9","NC10","NC11","NC12","NC13","NC14","NC15","NC16","NC17","NC18","NC19","NC20","NC21","NC22")
# w1=weilper[,-1:-2]
w1=weilper[,-1]
rownames(w1) <- c("OBS70","OBS71","OBS72","OBS73","OBS74","OBS75","OBS76","OBS77","OBS78","OBS79","OBS80","OBS81","OBS82","OBS83","OBS84","OBS85","OBS86","OBS87","OBS88","OBS89","OBS90","OBS91","OBS92","OBS93","OBS94","OBS95","OBS96","OBS97","OBS98","OBS99")
colnames(w1) <- c("WC1","WC2","WC3","WC4","WC5","WC6","WC7","WC8","WC9","WC10","WC11","WC12","WC13","WC14","WC15","WC16","WC17","WC18","WC19","WC20","WC21","WC22")

l1=Logper[,-1]
rownames(l1) <- c("OBS70","OBS71","OBS72","OBS73","OBS74","OBS75","OBS76","OBS77","OBS78","OBS79","OBS80","OBS81","OBS82","OBS83","OBS84","OBS85","OBS86","OBS87","OBS88","OBS89","OBS90","OBS91","OBS92","OBS93","OBS94","OBS95","OBS96","OBS97","OBS98","OBS99")
colnames(l1) <- c("LC1","LC2","LC3","LC4","LC5","LC6","LC7","LC8","LC9","LC10","LC11","LC12","LC13","LC14","LC15","LC16","LC17","LC18","LC19","LC20","LC21","LC22")

g1=Gammaper[,-1]
rownames(g1) <- c("OBS70","OBS71","OBS72","OBS73","OBS74","OBS75","OBS76","OBS77","OBS78","OBS79","OBS80","OBS81","OBS82","OBS83","OBS84","OBS85","OBS86","OBS87","OBS88","OBS89","OBS90","OBS91","OBS92","OBS93","OBS94","OBS95","OBS96","OBS97","OBS98","OBS99")
colnames(g1) <- c("GC1","GC2","GC3","GC4","GC5","GC6","GC7","GC8","GC9","GC10","GC11","GC12","GC13","GC14","GC15","GC16","GC17","GC18","GC19","GC20","GC21","GC22")

ll1<-cbind(w1,l1,g1)

UT <-c()
for (i in 1:22) {
  c2=max(w1[,i])
  loc2=which.max(w1[,i])
  c3=max(l1[,i])
  loc3=which.max(l1[,i])
  c4=max(g1[,i])
  loc4=which.max(g1[,i])
  u2<-rbind(c2,loc2)
  u3<-rbind(c3,loc3)
  u4<-rbind(c4,loc4)
  ll1<-cbind(u2,u3,u4)
  lcc1<-which.max(ll1[1,])
  res<-ll1[,lcc1]
  UT<-cbind(UT,res,lcc1)
}

# write.csv(UT,"D:/Daily QM/Combination/R delta/Fix delta ACCESS-ESM1-5.csv")
#############################
# setwd("D:/Daily QM/Combination/Dataset/")


base_fold_name = "ACCESS-ESM1-5/"
list_ = list("1" = "70 ",
             "2" = "71 ",
             "3" = "72 ",
             "4" = "73 ",
             "5" = "74 ",
             "6" = "75 ",
             "7" = "76 ",
             "8" = "77 ",
             "9" = "78 ",
             "10" = "79 ",
             "11" = "80 ",
             "12" = "81 ",
             "13" = "82 ",
             "14" = "83 ",
             "15" = "84 ",
             "16" = "85 ",
             "17" = "86 ",
             "18" = "87 ",
             "19" = "88 ",
             "20" = "89 ",
             "21" = "90 ",
             "22" = "91 ",
             "23" = "92 ",
             "24" = "93 ",
             "25" = "94 ",
             "26" = "95 ",
             "27" = "96 ",
             "28" = "97 ",
             "29" = "98 ",
             "30" = "99 "
)

dist_name = list("1" = "Weibull",
                 "2" = "Lnorm",
                 "3" = "Gamma" )

section = list("1" =  10847:10957,
               "2" =  10738:10957,
               "3" =  10628:10957,
               "4" =  10519:10957,
               "5" =  10409:10957,
               "6" = 10300:10957,
               "7" = 10190:10957,
               "8"= 10081:10957,
               "9" = 9971:10957,
               "10" = 9861:10957,
               "11" = 9752:10957,
               "12" = 9642:10957,
               "13" = 9533:10957,
               "14" = 9423:10957,
               "15" = 9314:10957,
               "16" = 9204:10957,
               "17" = 9094:10957,
               "18" = 8985:10957,
               "19" = 8875:10957,
               "20" = 8766:10957,
               "21" = 8656:10957,
               "22" = 8547:10957,
               "23" = 8437:10957,
               "24" = 8328:10957,
               "25" = 8218:10957,
               "26" = 8108:10957,
               "27" = 7999:10957,
               "28" = 7889:10957,
               "29" = 7780:10957,
               "30" = 7670:10957
)



all_data = c()
Otherp = c()
i = 1
for (i in 1:22){
  
  loc_value = as.integer(UT[2,2*i-1])
  lcc_value = as.integer(UT[1,2*i])
  numb = as.character(list_[loc_value]) %>% stringr::str_trim()
  name = as.character(dist_name[lcc_value]) %>% stringr::str_trim()
  
  # csv_file_name = paste0(base_fold_name,numb,name," ACCESS-ESM1-5 model.csv")
  # Dataset= data.frame(read.csv(csv_file_name)[,-1])
  
  filePattern = sprintf("%s/OBS%s_%s %s", "Dataset", numb, name, "ACCESS-ESM1-5 model.csv")
  csv_file_name = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  Dataset = readr::read_csv(csv_file_name, locale = locale("ko", encoding = "EUC-KR"))[,-1]


  inter = data.frame(section[loc_value])[,1]
  db_set = data.frame(as.character(dist_name[1:3]))
  db_numb = as.character(list_[loc_value]) %>% stringr::str_trim()
  # csv_file_name1 = paste0(base_fold_name,db_numb,db_set[1,]," ACCESS-ESM1-5 model.csv")
  # csv_file_name2 = paste0(base_fold_name,db_numb,db_set[2,]," ACCESS-ESM1-5 model.csv")
  # csv_file_name3 = paste0(base_fold_name,db_numb,db_set[3,]," ACCESS-ESM1-5 model.csv")

  # Dataset2= read.csv(csv_file_name1)[,-1]
  # Dataset3= read.csv(csv_file_name2)[,-1]
  # Dataset4= read.csv(csv_file_name3)[,-1]
  
  
  filePattern = sprintf("%s/OBS%s_%s %s", "Dataset", db_numb, db_set[1,], "ACCESS-ESM1-5 model.csv")
  csv_file_name1 = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  Dataset2 = readr::read_csv(csv_file_name1, locale = locale("ko", encoding = "EUC-KR"))[,-1]
  
  filePattern = sprintf("%s/OBS%s_%s %s", "Dataset", db_numb, db_set[2,], "ACCESS-ESM1-5 model.csv")
  csv_file_name2 = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  Dataset3 = readr::read_csv(csv_file_name2, locale = locale("ko", encoding = "EUC-KR"))[,-1]
  
  filePattern = sprintf("%s/OBS%s_%s %s", "Dataset", db_numb, db_set[3,], "ACCESS-ESM1-5 model.csv")
  csv_file_name3 = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  Dataset4 = readr::read_csv(csv_file_name3, locale = locale("ko", encoding = "EUC-KR"))[,-1]

  sub_dataset = data.frame(Dataset[inter,i])
  colnames(sub_dataset) = c('ul1')
  db_set1 = list("1" = Dataset2,"2" = Dataset3,"3" =Dataset4)
  
  compare = c()
  compare1 = c()
  compare2 = c()
  compare3 = c()
  
  for (j in 1:3){
    assign(paste0("comp",j),data.frame(db_set1[j])[-inter,i])
    
  }
  
  
  for (jj in 1:22){
    obs[,jj] <- sort(obs[,jj])
  }
  
  
  # 확률
  numb
  
  
  # str(comp1)
  comp_obs = obs[-inter,i]
  compare1 = cbind(compare,rmse(comp1,comp_obs))
  compare2 = cbind(compare,rmse(comp2,comp_obs))
  compare3 = cbind(compare,rmse(comp3,comp_obs))
  compare = as.numeric(cbind(compare1,compare2,compare3))
  min_dist = which.min(compare)
  comlist = list("1" =  comp1,
                 "2" =  comp2,
                 "3" =  comp3
  )
  
  
  
  
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
  
  
  
  
  # str(comp1)
  comp_obs = obs[-inter,i]
  compare1 = cbind(compare,rmse(comp1,comp_obs))
  compare2 = cbind(compare,rmse(comp2,comp_obs))
  compare3 = cbind(compare,rmse(comp3,comp_obs))
  compare = as.numeric(cbind(compare1,compare2,compare3))
  min_dist = which.min(compare)
  comlist = list("1" =  comp1,
                 "2" =  comp2,
                 "3" =  comp3
  )
  
  
  
  
  
  Otherp = cbind(min_dist, Otherp)
  Tdata=data.frame(comlist[min_dist])
  colnames(Tdata) = c('ul1')
  make_data = c(Tdata[,1], sub_dataset[,1])
  all_data = cbind(all_data,make_data)
  
}

final_data = all_data %>% 
  as.tibble() %>% 
  magrittr::set_colnames(c(names(obs)))
  # magrittr::set_colnames(c("ul1", names(obs)))

final_data
summary(final_data)

fnlOthData = Otherp %>% 
  as.tibble() %>% 
  magrittr::set_colnames(c(names(obs)))
  # magrittr::set_colnames(c("ul1", names(obs)))

# rmse(final_data[,2],obs[,2])
# write.csv(Otherp,"F:/R/tu/tttt/tut/GCM/Alpha Gamma qunatile mapping/Historical/dataframe/Period1 Fix RMSE Combination INM-CM4-8.csv")
saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "Period1 Fix RMSE Combination INM-CM4-8.csv")
readr::write_csv(fnlOthData, file = saveFile)

################ 시계열
# globalVar_inpConfig = "F:/R/tu/tttt/tut/GCM/Alpha Gamma qunatile mapping/Historical/Bias"


modelData = final_data
# colnames(final_data) <- colnames(obs1)[1:22]
obsData= obs1
seqList = seq(1, 22)
dataL2 = tibble::tibble()
dataL3 = tibble::tibble()

# seqInfo = 1

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
    dplyr::select(obs, model) %>% 
    magrittr::set_colnames(c(sprintf("obs%s", colObsName), sprintf("model%s", colModelName))) %>% 
    as.tibble()
  
  if (seqInfo == 1) {
    dataL3 = obsDataL2
  } else {
    dataL3 = dplyr::bind_cols(dataL3, obsDataL2)
  }
}

# saveFile1 = sprintf("%s/%s", globalVar_inpConfig, "R FDDG INM-CM4-8 historical FIX.csv")
# readr::write_csv(dataL2, file = saveFile1)
# 
# saveFile2 = sprintf("%s/%s", globalVar_inpConfig, "C FDDG INM-CM4-8 historical FIX.csv")
# readr::write_csv(dataL3, file = saveFile2)

saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "R FDDG INM-CM4-8 historical FIX.csv")
readr::write_csv(dataL2, file = saveFile)

saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "C FDDG INM-CM4-8 historical FIX.csv")
readr::write_csv(dataL3, file = saveFile)


################### 
# setwd("F:/R/tu/tttt/tut/GCM/Alpha Gamma qunatile mapping/Historical/Bias")
# dataL1=read.csv("C FDDG INM-CM4-8 historical FIX.csv")
# m=dataL1[,seq(from=2, to=44, by=2)]
# m = data.frame(m)
# #Line=QUAN[,seq(from=2, to=22, by=2)]
# colnames(m) <- colnames(obs)[1:22]
# write.csv(m,"F:/R/tu/tttt/tut/GCM/Alpha Gamma qunatile mapping/Historical/Fix bias/Double1zzzzzzz RMSE Combination INM-CM4-8 historical.csv")
# ########
# uu21<-rmse(m,obs1)
# 
# write.csv(uu21,"F:/R/tu/tttt/tut/GCM/Alpha Gamma qunatile mapping/Historical/New Bias/Fix12zzzzzzz DDGQM RMSE INM-CM4-8 combination.csv")
# 
#   
#   saveFile = sprintf("%s/%s_%s_%s", globalVar$outPath, serviceName, nameInfo, "Weibull ACCESS-ESM1-5 model.csv")
#   readr::write_csv(dataL4, file = saveFile)
# }
