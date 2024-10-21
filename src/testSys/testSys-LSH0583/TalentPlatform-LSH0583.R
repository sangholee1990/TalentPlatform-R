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
# R을 이용한 폭염에 따른 온열질환자 예측 및 분석

# 1)전공에 관련된 데이터에 대해서 problem formulation에 근거하여 예측을 위한 분석 주제를 정하고 분석과정를 계획하고 탐색적 데이터 분석을 실시
# -> 1)리포트 한개
# 2)예측모형을 도출하고 성능을 평가한 뒤 결과의 적용 방안에 대하여 기술하라
# ->1)내용과 2)내용을 포함한 리포트 한개

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0583"

if (Sys.info()[["sysname"]] == "Windows") {
  contextPath = ifelse(env == "local", getwd(), "C:/SYSTEMS/PROG/R/TalentPlatform-R")
} else {
  contextPath = ifelse(env == "local", getwd(), "/SYSTEMS/PROG/R/PyCharm")
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
library(data.table)
library(readr)
library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)
library(ggmap)
library(readxl)
library(ggrepel)
library(metR)
library(solarPos)
library(Metrics)
library(tidyverse)
library(weathermetrics)
library(data.table)
library(callr)
library(devtools)
library(GGally)
library(factoextra)
library(tidyverse)
library(colorRamps)
library(akima)
library(GGally)
library(tidyverse)
library(factoextra)
library(gridExtra)
library(ggcorrplot)
library(MAT)
library(moments)
library(RColorBrewer)
library(dplyr)
library(zoo)
library(ggplot2)
library(gstat)
library(sp)
library(maptools)
library(RNetCDF)
library(leaflet)
library(colorRamps)
library(gpclib)
library(rgeos)
library(mapdata)
library(MASS)
library(neuralnet)
library(h2o)
library(reshape2)
library(MASS)
library(gstat)
library(pROC)
library(ggmap)
library(readxl)
library(ggrepel)


#==================================================================================================
#  파일 읽기
#==================================================================================================
# 기상 자료 읽기
# 지점, 일시, 기온(°C), 기온 QC플래그, 강수량(mm), 강수량 QC플래그, 풍속(m/s), 풍속 QC플래그
# ,풍향(16방위), 풍향 QC플래그, 습도(%), 습도 QC플래그, 증기압(hPa), 이슬점온도(°C), 현지기압(hPa), 현지기압 QC플래그
# ,해면기압(hPa), 해면기압 QC플래그, 일조(hr), 일조 QC플래그, 일사(MJ/m2), 적설(cm), 3시간신적설(cm)
# ,전운량(10분위), 중하층운량(10분위), 운형(운형약어), 최저운고(100m ), 시정(10m), 지면상태(지면상태코드)
# ,현상번호(국내식), 지면온도(°C), 지면온도 QC플래그, 5cm 지중온도(°C), 10cm 지중온도(°C)
# ,20cm 지중온도(°C), 30cm 지중온도(°C)

fileInfo = Sys.glob(file.path(globalVar$inpPath, "INPUT/Big_Data_For_Input_ASOS_2011-2019_QC.inp"))
data = data.table::fread(fileInfo, sep = ",", header = FALSE, stringsAsFactors = FALSE)
# data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))

colnames(data) = c("rowNum", "stationNum", "dateTimeOri", "temp", "tempQc", "prec", "precQc", "ws", "wsQc", "wd", "wdQc", "rh", "rhQc", "waterRh", "dewTemp", "localPres", "localPresQc", "seaPres", "seaPresQc", "daylight", "daylightQc", "sr", "snowfall", "v3hrSnowfall", "allCloudAmount", "middleCloudAmount", "cloudType", "cloudBottomHeight", "vis", "landType", "weatherType", "surfaceTemp", "surfaceTempQc", "surface5mTemp", "surface10mTemp", "surface20mTem", "surface30mTemp", "dateTime", "year", "month", "day", "hour", "minute")

dplyr::tbl_df(data)


# ************************************************************************
# 기상 관측소 정보 읽기
# ************************************************************************
fileInfo = Sys.glob(file.path(globalVar$inpPath, "INPUT/Station_Information_20190714.info"))
stationData = data.table::fread(fileInfo, sep = "\t", header = FALSE)
colnames(stationData) = c("stationNum", "lon", "lat", "hight", "stationName", "metroStationName")

dplyr::tbl_df(stationData)

# ************************************************************************
# 온열질환자 자료 읽기
# ************************************************************************
fileInfo = Sys.glob(paste(globalVar$inpConfig, "Big_Data_For_Validation_QC_L2_2015-2019.val", sep = "/"))
valData = data.table::fread(fileInfo, sep = " ", header = FALSE)
colnames(valData) = c("year", "month", "day", "metroStationName", "hwanja", "death")

dplyr::tbl_df(valData)

# ************************************************************************
# 일평균 수행
# ************************************************************************
dataL1 = data %>%
  dplyr::na_if(-999.0) %>%
  dplyr::select(dateTime, year, month, day, hour, stationNum, temp, ws, rh, sr) %>%
  dplyr::filter(
    dplyr::between(year, 2015, 2019)
    , dplyr::between(month, 5, 9)
    , dplyr::between(hour, 9, 18)
  ) %>%
  dplyr::group_by(year, month, day, stationNum) %>%
  dplyr::summarise(
    maxTemp = max(temp, na.rm = TRUE)
    , meanTemp = mean(temp, na.rm = TRUE)
    , meanRh = mean(rh, na.rm = TRUE)
    , meanWs = mean(ws, na.rm = TRUE)
    , sumSr = sum(sr, na.rm = TRUE)
  )

dplyr::glimpse(dataL1)

# ************************************************************************
# 4종 폭염지수 계산
# ************************************************************************
dataL2 = dataL1 %>%
  dplyr::filter(sumSr > 0) %>%
  dplyr::mutate(
    heatIndex = fnHeatIndex(meanTemp, meanRh)
    , humidIndex = fnHumidIndex(meanTemp, meanRh)
    , appTempIndex = fnAppTempIndex(meanTemp, meanRh, meanWs)
    , appTempRadIndex = fnAppTempRadIndex(meanTemp, meanRh, meanWs, sumSr)
    , wetBulbGolbalTempIndex = fnWetBulbGolbalTempIndex(meanTemp, meanRh, meanWs, sumSr)
  )

dplyr::glimpse(dataL1)

# ************************************************************************
# 자료 병합 (기상 자료, 기상 관측소, 온열질환자 자료)
# ************************************************************************
dataL3 = dataL2 %>%
  dplyr::left_join(stationData, by = c("stationNum" = "stationNum")) %>%
  dplyr::left_join(valData, by = c("metroStationName" = "metroStationName", "year" = "year", "month" = "month", "day" = "day")) %>%
  dplyr::mutate(
    sDate = paste(year, month, day, sep = "-")
    , dtDate = readr::parse_date(sDate, "%Y-%m-%d")
    , jd = lubridate::yday(dtDate)
    , xran = lubridate::decimal_date(dtDate)
  ) %>%
  readr::type_convert()

dplyr::glimpse(dataL3)


# ************************************************************************
# 유의미한 변수 찾기
# ************************************************************************
dataL4 = dataL3 %>%
  na.omit() %>%
  dplyr::select(maxTemp, meanTemp, meanRh, meanWs, sumSr, heatIndex, humidIndex, appTempIndex, appTempRadIndex, wetBulbGolbalTempIndex, jd, hwanja, death)

dplyr::glimpse(dataL4)

# 상관계수 행렬
corMat = cor(dataL4)

ggcorrplot(corMat, outline.col = "white", lab = FALSE) +
  scale_fill_gradientn(colours = colorRamps::matlab.like(10)) +
  labs(fill = "Cor")

# 폭염 기준 상관계수 행렬
dataL5 = dataL4 %>%
  dplyr::filter(
    maxTemp >= 33
    , heatIndex >= 32
  )

corMat = cor(dataL5)

ggcorrplot(corMat, outline.col = "white", lab = FALSE) +
  scale_fill_gradientn(colours = colorRamps::matlab.like(10)) +
  labs(fill = "Cor")

# 각 지점에 따른 다중선형회귀모형/딥러닝 학습
stationNameList = sort(unique(dataL3$metroStationName))
# stationNameInfo = "Busan"

h2o::h2o.init()
dataL6 = data.frame()

for (stationNameInfo in stationNameList) {
  
  dataL4 = dataL3 %>%
    dplyr::filter(
      hwanja >= 0
      , metroStationName == stationNameInfo
    )
  
  if (nrow(dataL4) < 1) { next }
  
  dataL5 = na.omit(dataL4)
  
  # **************************************
  # 데이터 분할
  # **************************************
  # 훈련 및 데이터 셋을 60:40으로 나누기 위한 인덱스 설정
  ind = sample(1:nrow(dataL5), nrow(dataL5) * 0.6)
  
  # 해당 인덱스에 따라 자료 할당
  trainData = dataL5[ind,]
  testData = dataL5[-ind,]
  
  # 표준화 수행
  # trainData = dataL5[-ind,]  %>%
  # dplyr::mutate_each_(funs(scale), vars=c("meanTemp", "meanRh", "sumSr", "meanWs", "wetBulbGolbalTempIndex", "jd"))
  
  # testData = dataL5[ind,]  %>%
  # dplyr::mutate_each_(funs(scale), vars=c("meanTemp", "meanRh", "sumSr", "meanWs", "wetBulbGolbalTempIndex", "jd"))
  
  # 정규화 수행
  # trainData = dataL5[-ind,]  %>%
  #   dplyr::mutate_each_(funs(scales::rescale), vars=c("meanTemp", "meanRh", "sumSr", "meanWs", "wetBulbGolbalTempIndex", "jd"))
  
  # testData = dataL5[ind,]  %>%
  #   dplyr::mutate_each_(funs(scales::rescale), vars=c("meanTemp", "meanRh", "sumSr", "meanWs", "wetBulbGolbalTempIndex", "jd"))
  
  # 훈련 데이터셋 확인
  dplyr::tbl_df(trainData)
  
  # 테스트 데이터셋 확인
  dplyr::tbl_df(testData)
  
  # 동적 회귀식 생성
  # allVar = colnames(trainData)
  # predictorVarList = allVar[!allVar %in% "Churn"]
  # predictorVar = paste(predictorVarList, collapse = "+")
  # form = as.formula(paste("Churn ~", predictorVar, collapse = "+"))
  
  # 수동 회귀식 생성
  # lmForm = hwanja ~ meanTemp + meanRh + heatIndex
  # lmForm = hwanja ~ meanTemp + meanRh + sumSr + meanWs
  form = hwanja ~ meanTemp +
    meanRh +
    sumSr +
    meanWs +
    wetBulbGolbalTempIndex +
    jd
  # lmForm = hwanja ~ wetBulbGolbalTempIndex + meanSurfaceTemp + meanAllCloudAmount
  
  # **************************************
  # 다중선형회귀모형
  # **************************************
  lmFit = lm(form, data = trainData)
  summary(lmFit)
  
  xAxis = predict(lmFit, new = testData)
  yAxis = testData$hwanja
  
  # plot(xAxis, yAxis)
  
  cat(sprintf(
    "MLR [%10s] length : %05s | cor : %05s | Bias : %05s | RMSE : %05s"
    , stationNameInfo
    , length(xAxis)
    , round(cor(xAxis, yAxis), 2)
    , round(Metrics::rmse(xAxis, yAxis), 2)
    , round(Metrics::bias(xAxis, yAxis), 2)
  ), "\n")
  
  # **************************************
  # 딥러닝
  # **************************************
  # activation : 활성화 함수로서 Rectifier 정규화 선형 함수 (즉 Keras의 ReLU 동일)
  # hidden : 숨겨진 레이어의 수와 뉴런 수 (일반적으로 입력 차원의 1/10 or 1/100 단위)
  # epochs : 반복 횟수 (기본 10-40)
  # nfolds : 훈련 반복 수
  
  layerNum = as.integer(nrow(trainData) / 10)
  # layerNum = as.integer(nrow(trainData) / 100)
  
  dlModel = h2o::h2o.deeplearning(
    x = c("meanTemp", "meanRh", "sumSr", "meanWs", "wetBulbGolbalTempIndex", "jd")
    , y = c("hwanja")
    , training_frame = as.h2o(trainData)
    , activation = 'Rectifier'
    , hidden = rep(layerNum, 3)
    , nfolds = 10
    , epochs = 100
  )
  
  xAxis = as.data.frame(h2o::h2o.predict(object = dlModel, newdata = as.h2o(testData)))$predict
  yAxis = testData$hwanja
  
  # plot(xAxis, yAxis)
  
  cat(sprintf(
    "DL [%10s] length : %05s | cor : %05s | Bias : %05s | RMSE : %05s"
    , stationNameInfo
    , length(xAxis)
    , round(cor(xAxis, yAxis), 2)
    , round(Metrics::rmse(xAxis, yAxis), 2)
    , round(Metrics::bias(xAxis, yAxis), 2)
  ), "\n")
  
  
  dataL5$stationNameInfo = stationNameInfo
  dataL5$mlr = predict(lmFit, new = dataL5)
  dataL5$dl = as.data.frame(h2o::h2o.predict(object = dlModel, newdata = as.h2o(dataL5)))$predict
  
  dataL6 = dplyr::bind_rows(dataL6, dataL5)
}

saveFile = sprintf("%s/%s_%s", globalVar$outConfig, serviceName, "Big_Data_For_Output_ASOS_2015-2019.out")
readr::write_csv(dataL6, file = saveFile)

dataL7 = dataL6

#==================================================================================================
# 시각화를 위한 파일 읽기
#==================================================================================================
fileInfo = Sys.glob(paste(globalVar$outConfig, "LSH0079_Big_Data_For_Output_ASOS_2015-2019.out", sep = "/"))
dataL7 = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR")) %>%
  na.omit() %>%
  dplyr::rename(
    "kcdc" = "hwanja"
  ) %>%
  dplyr::filter(
    mlr >= 0
    , dl >= 0
  )

dplyr::glimpse(dataL7)

# *******************************************************
# 일별 사례 : 2018년 07월 23일
# *******************************************************
dayData = dataL7 %>%
  dplyr::filter(
    year == 2018
    , month == 8
    , day == 7
  )


# *******************************************************
# 월별 사례 : 2018년 08월
# *******************************************************
monthData = dataL7 %>%
  dplyr::group_by(stationName, lon, lat, year, month) %>%
  dplyr::summarise(
    cor = cor(mlr, kcdc)
    , bias = bias(mlr, kcdc)
    , rmse = rmse(mlr, kcdc)
    , n = n()
    , meanMlr = mean(mlr, na.rm = TRUE)
    , meanDl = mean(dl, na.rm = TRUE)
    , meanKcdc = mean(kcdc, na.rm = TRUE)
  ) %>%
  dplyr::filter(
    year == 2018
    , month == 8
  )


# *******************************************************
# 연별 사례 : 2018-2019년
# *******************************************************
yearData = dataL7 %>%
  dplyr::filter(dplyr::between(year, 2018, 2019))


# *******************************************************
# 산포도 시각화
# *******************************************************
# 일별 (다중선형회귀 예측 vs 환자 관측값)
X = dayData$mlr
Y = dayData$kcdc

# 일별 (딥러닝 예측 vs 환자 관측값)
X = dayData$dl
Y = dayData$kcdc

# 연별 (다중선형회귀 예측 vs 환자 관측값)
# X = monthData$meanMlr
# Y = monthData$meanKcdc

# 연별 (딥러닝 예측 vs 환자 관측값)
# X = monthData$meanDl
# Y = monthData$meanKcdc

val = fnStats(X, Y)
sprintf("%.3f", val)

xcord = 1
ycord = seq(48, 0, -3)

saveImg = sprintf("%s/Img_%s_%s.png", globalVar$figConfig, serviceName, 1)

ggplot() +
  coord_fixed(ratio = 1) +
  theme_bw() +
  geom_point(aes(X, Y)) +
  # stat_bin2d(binwidth = c(1, 1), aes(X, Y)) +
  # stat_bin2d(binwidth = c(5, 5), aes(X, Y)) +
  # scale_fill_gradientn(colours = cbViridis, limits=c(0, 100), na.value=cbViridis[1]) +
  annotate("text", x = xcord, y = ycord[1], label = paste0("(KCDC) = ", sprintf("%.2f", val[1]), " x (MLR) + ", sprintf("%.2f", val[2])), size = 5, hjust = 0, color = "red", fontface = "bold", family = font) +
  # annotate("text", x=250, y=ycord[2], label=paste0("R = ", sprintf("%.2f",val[12]), " (p < 0.001) | Stdev = ", sprintf("%.2f",val[14])), size=5, hjust=0, color="red", family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[2], label = paste0("R = ", sprintf("%.2f", val[12]), " (p-value < 0.001)"), size = 5, hjust = 0, color = "red", family = font, fontface = "bold") +
  # annotate("text", x=250, y=ycord[3], label=paste0("Stdev = ", sprintf("%.2f",val[14])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=250, y=ycord[4], label=paste0("AMI = ", sprintf("%.3f",val[3]), " | CERES = ", sprintf("%.3f",val[4])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[3], label = paste0("Bias = ", sprintf("%.2f", val[8])), parse = F, size = 5, hjust = 0, family = font, fontface = "bold") +
  # annotate("text", x=220, y=ycord[3], label=paste0("Bias = ", sprintf("%.2f",val[8])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[4], label = paste0("RMSE = ", sprintf("%.2f", val[10])), parse = F, size = 5, hjust = 0, family = font, fontface = "bold") +
  # annotate("text", x=220, y=ycord[4], label=paste0("RMSE = ", sprintf("%.2f",val[10]), " (MPE = ", sprintf("%.2f",val[15])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=2, y=ycord[5], label=paste0("MPE = ", sprintf("%.2f",val[15])," %"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[5], label = paste0("N = ", sprintf("%.0f", val[7])), size = 5, hjust = 0, color = "black", family = font, fontface = "bold") +
  geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
  stat_smooth(method = "lm", color = "red", se = F, aes(X, Y)) +
  scale_x_continuous(minor_breaks = seq(0, 50, by = 10), breaks = seq(0, 50, by = 10), expand = c(0, 0), limits = c(0, 50)) +
  scale_y_continuous(minor_breaks = seq(0, 50, by = 10), breaks = seq(0, 50, by = 10), expand = c(0, 0), limits = c(0, 50)) +
  labs(title = "") +
  labs(x = expression(paste(bold("PHRI From MLR"))),
       y = expression(paste(bold("PHRI From KCDC "))),
       fill = "Count") +
  theme(plot.title = element_text(face = "bold", size = 20, color = "black")) +
  theme(axis.title.x = element_text(face = "bold", size = 19, colour = "black")) +
  theme(axis.title.y = element_text(face = "bold", size = 19, colour = "black", angle = 90)) +
  theme(axis.text.x = element_text(face = "bold", size = 19, colour = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 19, colour = "black")) +
  theme(legend.title = element_text(face = "bold", size = 14, colour = "black")) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 0.96)) +
  theme(legend.key = element_blank()) +
  theme(legend.text = element_text(size = 14, face = "bold")) +
  theme(legend.background = element_blank()) +
  theme(text = element_text(family = font)) +
  theme(plot.margin = unit(c(0, 8, 0, 0), "mm")) +
  ggsave(filename = saveImg, width = 6, height = 6, dpi = 600)

# *******************************************************
# 2차원 빈도분포 산포도 시각화
# *******************************************************
# 일별 (다중선형회귀 예측 vs 환자 관측값)
X = dayData$mlr
Y = dayData$kcdc

# 일별 (딥러닝 예측 vs 환자 관측값)
# X = dayData$dl
# Y = dayData$kcdc

# 연별 (다중선형회귀 예측 vs 환자 관측값)
# X = monthData$meanMlr
# Y = monthData$meanKcdc

# 연별 (딥러닝 예측 vs 환자 관측값)
# X = monthData$meanDl
# Y = monthData$meanKcdc


xcord = 10.5
ycord = seq(48, 0, -3)

val = fnStats(X, Y)
sprintf("%.3f", val)

saveImg = sprintf("%s/Img_%s_%s.png", globalVar$figConfig, serviceName, 2)

ggplot() +
  coord_fixed(ratio = 1) +
  theme_bw() +
  # geom_point(aes(X, Y)) +
  stat_bin2d(binwidth = c(1, 1), aes(X, Y)) +
  # stat_bin2d(binwidth = c(5, 5), aes(X, Y)) +
  scale_fill_gradientn(colours = cbViridis, limits = c(0, 100), na.value = cbViridis[1]) +
  annotate("text", x = xcord, y = ycord[1], label = paste0("(KCDC) = ", sprintf("%.2f", val[1]), " x (MLR) + ", sprintf("%.2f", val[2])), size = 5, hjust = 0, color = "red", fontface = "bold", family = font) +
  # annotate("text", x=xcord, y=ycord[1], label=paste0("(Val) = ", sprintf("%.2f",val[1])," x (Pred) + ", sprintf("%.2f",val[2])), size=5, hjust=0, color="red", fontface="bold", family=font) +
  # annotate("text", x=250, y=ycord[2], label=paste0("R = ", sprintf("%.2f",val[12]), " (p < 0.001) | Stdev = ", sprintf("%.2f",val[14])), size=5, hjust=0, color="red", family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[2], label = paste0("R = ", sprintf("%.2f", val[12]), " (p-value < 0.001)"), size = 5, hjust = 0, color = "red", family = font, fontface = "bold") +
  # annotate("text", x=250, y=ycord[3], label=paste0("Stdev = ", sprintf("%.2f",val[14])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=250, y=ycord[4], label=paste0("AMI = ", sprintf("%.3f",val[3]), " | CERES = ", sprintf("%.3f",val[4])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[3], label=paste0("Bias = ", sprintf("%.2f",val[8]), " (", sprintf("%.2f",val[9])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[3], label = paste0("Bias = ", sprintf("%.2f", val[8])), parse = F, size = 5, hjust = 0, family = font, fontface = "bold") +
  # annotate("text", x=220, y=ycord[3], label=paste0("Bias = ", sprintf("%.2f",val[8])), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=xcord, y=ycord[4], label=paste0("RMSE = ", sprintf("%.2f",val[10]), " (", sprintf("%.2f",val[11])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[4], label = paste0("RMSE = ", sprintf("%.2f", val[10])), parse = F, size = 5, hjust = 0, family = font, fontface = "bold") +
  # annotate("text", x=220, y=ycord[4], label=paste0("RMSE = ", sprintf("%.2f",val[10]), " (MPE = ", sprintf("%.2f",val[15])," %)"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  # annotate("text", x=2, y=ycord[5], label=paste0("MPE = ", sprintf("%.2f",val[15])," %"), parse=F, size=5, hjust=0, family=font, fontface="bold") +
  annotate("text", x = xcord, y = ycord[5], label = paste0("N = ", sprintf("%.0f", val[7])), size = 5, hjust = 0, color = "black", family = font, fontface = "bold") +
  geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
  stat_smooth(method = "lm", color = "red", se = F, aes(X, Y)) +
  scale_x_continuous(minor_breaks = seq(0, 50, by = 10), breaks = seq(0, 50, by = 10), expand = c(0, 0), limits = c(0, 50)) +
  scale_y_continuous(minor_breaks = seq(0, 50, by = 10), breaks = seq(0, 50, by = 10), expand = c(0, 0), limits = c(0, 50)) +
  labs(
    x = expression(paste(bold("PHRI From MLR")))
    , y = expression(paste(bold("PHRI From KCDC ")))
    , fill = "Count"
    , title = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "black")
    , axis.title.x = element_text(face = "bold", size = 19, colour = "black")
    , axis.title.y = element_text(face = "bold", size = 19, colour = "black", angle = 90)
    , axis.text.x = element_text(face = "bold", size = 19, colour = "black")
    , axis.text.y = element_text(face = "bold", size = 19, colour = "black")
    , legend.title = element_text(face = "bold", size = 14, colour = "black")
    , legend.position = c(0, 1), legend.justification = c(0, 0.96)
    , legend.key = element_blank()
    , legend.text = element_text(size = 14, face = "bold")
    , legend.background = element_blank()
    # , text=element_text(family=font)
    , plot.margin = unit(c(0, 8, 0, 0), "mm")
  ) +
  ggsave(filename = saveImg, width = 6, height = 6, dpi = 600)


# *******************************************************
# 지도 매핑
# *******************************************************
# 일별 
mapData = dayData

# 월별
# mapData = monthData

saveImg = sprintf("%s/Img_%s_%s.png", globalVar$figConfig, serviceName, 2)

coordinates(mapData) = ~lon + lat
plot(mapData)

mapDataL1 = mapData %>%
  as.tibble()

globalVar$mapConfig

mapKor = read_sf(paste(globalVar$mapConfig, "gadm36_KOR_shp/gadm36_KOR_1.shp", sep = "/"))
mapPrk = read_sf(paste(globalVar$mapConfig, "gadm36_PRK_shp/gadm36_PRK_1.shp", sep = "/"))
mapJpn = read_sf(paste(globalVar$mapConfig, "gadm36_JPN_shp/gadm36_JPN_1.shp", sep = "/"))

yRange = as.numeric(c(33, 39))    # min/max latitude  of the interpolation area
xRange = as.numeric(c(124, 132))  # min/max longitude of the interpolation area

# expand points to grid
gridData = expand.grid(
  x = seq(from = xRange[1], to = xRange[2], by = 0.01)
  , y = seq(from = yRange[1], to = yRange[2], by = 0.01)
)
coordinates(gridData) = ~x + y
gridded(gridData) = TRUE

# ******************************************
# 공간 내삽
# ******************************************

#+++++++++++++++++++++
# 일별
#+++++++++++++++++++++
# 일별 다중선형회귀 예측
spData = gstat::idw(formula = mlr ~ 1, locations = mapData, newdata = gridData)

# 일별 딥러닝 예측
# spData = gstat::idw(formula = dl ~ 1, locations = mapData, newdata = gridData)

# 일별 환자 관측
# spData = gstat::idw(formula = kcdc ~ 1, locations = mapData, newdata = gridData)

#+++++++++++++++++++++
# 월별
#+++++++++++++++++++++
# 월별 다중선형회귀 예측
# spData = gstat::idw(formula = meanMlr ~ 1, locations = mapData, newdata = gridData)

# 월별 딥러닝 예측
# spData = gstat::idw(formula = meanKcdc ~ 1, locations = mapData, newdata = gridData)

# 월별 딥러닝 예측
# spData = gstat::idw(formula = meanKcdc ~ 1, locations = mapData, newdata = gridData)

# 월별 상관계수 분포
# spData = gstat::idw(formula = cor ~ 1, locations = mapData, newdata = gridData)

# 월별 평균제곱근오차 분포
# spData = gstat::idw(formula = rmse ~ 1, locations = mapData, newdata = gridData)

spDataL1 = spData %>%
  as.data.frame() %>%
  dplyr::rename(
    "lon" = "x"
    , "lat" = "y"
    , "pred" = "var1.pred"
  ) %>%
  dplyr::mutate(
    isMaskLand = metR::MaskLand(lon, lat, mask = "world")
  ) %>%
  dplyr::filter(isMaskLand == TRUE)

saveImg = sprintf("%s/Img_%s_%s.png", globalVar$figConfig, serviceName, 3)

ggplot() +
  coord_fixed(ratio = 1.1) +
  # coord_fixed(1.3) +
  theme_bw() +
  geom_tile(data = spDataL1, aes(x = lon, y = lat, fill = pred)) +
  scale_fill_gradientn(colours = cbMatlab, limits = c(0, 10), breaks = seq(0, 10, 2), na.value = cbMatlab[length(cbMatlab)]) + # Pred, Val
  # scale_fill_gradientn(colours = cbSpectral, limits=c(0, 10), na.value = cbSpectral[length(cbSpectral)], breaks = seq(0, 10, 2)) + # RMSE
  # scale_fill_gradientn(colours = cbPlasma, limits=c(0.4, 1), na.value = cbPlasma[length(cbPlasma)], breaks = seq(0.4, 1, 0.2)) + # R
  # geom_point(aes(x = long, y = lat, colour=obs), data = data_L6, size=5, alpha=0.3, show.legend = FALSE) +
  geom_point(aes(x = lon, y = lat), colour = "black", data = mapDataL1, size = 5, alpha = 0.3, show.legend = FALSE) +
  # geom_text(aes(x=lon, y=lat, label=obs, colour=obs), hjust=-0.25, vjust=0.5, nudge_x=0, nudge_y=0, size=5, colour='black', data=rsReultL1, family = font) +
  ggrepel::geom_text_repel(aes(x = lon, y = lat, label = stationName, colour = stationName), point.padding = 0.25, box.padding = 0.25, nudge_y = 0.1, data = mapDataL1, size = 4, colour = "black") +
  
  # geom_label(aes(x=lon, y=lat, label=obs, colour=obs), hjust=-0.25, vjust=0.5, nudge_x=0, nudge_y=0, size=5,
  # data=data_L4, show.legend = FALSE) +
  # coord_sf() +
  # ggplot(data = world) +
  # geom_sf() +
  # coord_sf(xlim = c(124, 132), ylim = c(33, 39), expand = FALSE)
  # geom_path(aes(x=long, y=lat, group=group), data = world, colour="black") +
  # geom_polygon(data = shore, aes(x=long, y = lat, group = group), color = "black", fill = NA) +
  # geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = NA, color = "red") + 
  # scale_x_continuous(breaks = longitude, labels = x_longitude, expand=c(0,0), limits=c(125, 130)) +
  # scale_y_continuous(breaks = latitude,  labels = y_latitude,  expand=c(0,0), limits=c(33, 39)) +
  # scale_x_continuous(breaks = longitude, labels = x_longitude, expand=c(0,0), limits=c(124, 132)) +
  # scale_y_continuous(breaks = latitude,  labels = y_latitude,  expand=c(0,0), limits=c(33, 39)) +
  # ggplot2::scale_x_continuous(limits=c(124, 132)) +
  # ggplot2::scale_y_continuous(limits=c(33, 39)) +
  # geom_polygon(data = shore, aes(x=long, y = lat, group = group), color = "black", fill = NA) +
  # geom_sf(data=map,  color = "black", fill=NA) +
  geom_sf(data = mapKor, color = "black", fill = NA) +
  geom_sf(data = mapPrk, color = "black", fill = NA) +
  geom_sf(data = mapJpn, color = "black", fill = NA) +
  metR::scale_x_longitude(expand = c(0, 0), breaks = seq(124, 132, 2), limits = c(124, 132)) +
  metR::scale_y_latitude(expand = c(0, 0), breaks = seq(32, 40, 1), limits = c(33, 39)) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "black")
    , axis.title.x = element_text(face = "bold", size = 18, colour = "black")
    , axis.title.y = element_text(face = "bold", size = 18, colour = "black", angle = 90)
    , axis.text.x = element_text(face = "bold", size = 18, colour = "black")
    , axis.text.y = element_text(face = "bold", size = 18, colour = "black")
    , legend.position = c(1, 1), legend.justification = c(1, 1)
    , legend.key = element_blank()
    , legend.text = element_text(size = 14, face = "bold")
    , legend.title = element_text(face = "bold", size = 14, colour = "black")
    , legend.background = element_blank()
    # , text=element_text(family = font)
    , plot.margin = unit(c(0, 8, 0, 0), "mm")
  ) +
  labs(x = NULL, y = NULL, fill = NULL, colour = NULL, title = NULL) +
  ggsave(filename = saveImg, width = 8, height = 10, dpi = 600)


# *******************************************************
# 검증
# *******************************************************
# 일별 (다중선형회귀 예측 vs 환자 관측)
# rocDayData = roc(as.integer(dayData$mlr), dayData$kcdc)
# pROC::ggroc(rocDayData)

# 월별 (다중선형회귀 예측 vs 환자 관측)
# rocMontyData = roc(as.integer(monthData$meanMlr), monthData$meanKcdc)
# pROC::ggroc(rocMontyData)

# 연별 (다중선형회귀 예측 vs 환자 관측)
rocYearData = roc(as.integer(yearData$mlr), yearData$kcdc)
pROC::ggroc(rocYearData)

# 일별 (딥러닝 예측 vs 환자 관측)
# rocDayData = roc(as.integer(dayData$dl), dayData$kcdc)
# pROC::ggroc(rocDayData)

# 월별 (딥러닝 예측 vs 환자 관측)
# rocMontyData = roc(as.integer(monthData$meanDl), monthData$meanKcdc)
# pROC::ggroc(rocMontyData)

# 연별 (딥러닝 예측 vs 환자 관측)
rocYearData = roc(as.integer(yearData$dl), yearData$kcdc)
pROC::ggroc(rocYearData)


# 순번, 지점, 시작일, 지점명, 관리관서, 위도, 경도, 노장해발고도(m)
# data = read.csv("일사관측소_2018.csv", skip=1, header=FALSE)
# colnames(data) = c("num", "station_id", "start_time", "id", "department", "lat", "long", "alt")
# head(data)

# summary(data)

# glimpse(dataL7)
# Figure
korea = c(left = 124, bottom = 32, right = 132, top = 40)    # Korea
map = get_googlemap(korea, zoom = 7, maptype = 'hybrid')

ggmap(map, extent = 'hybrid') +
  # ggmap(map) +
  geom_point(aes(x = log, y = lat, colour = hight), data = dataL7, size = 4) +
  geom_text_repel(aes(x = log, y = lat, label = stationName, colour = hight),
                  point.padding = 0.25, box.padding = 0.25, nudge_y = 0.1,
                  data = dataL7, size = 4, colour = "white", fontface = "bold") +
  labs(colour = "Altitude [m]") +
  scale_colour_gradientn(colours = rainbow(11), limits = c(0, 800)) +
  theme(axis.title.x = element_text(face = "bold", size = 15, colour = "black")) +
  theme(axis.title.y = element_text(face = "bold", size = 15, colour = "black", angle = 90)) +
  theme(axis.text.x = element_text(face = "bold", size = 15, colour = "black")) +
  theme(axis.text.y = element_text(face = "bold", size = 15, colour = "black")) +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1)) +
  theme(legend.key = element_blank()) +
  theme(legend.text = element_text(size = 12, face = "bold", color = "white")) +
  theme(legend.title = element_text(size = 14, face = "bold", colour = "white")) +
  theme(legend.background = element_blank())

+
  ggsave("Map.png", width = 6, height = 6, dpi = 600)