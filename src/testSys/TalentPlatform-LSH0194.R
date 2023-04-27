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
# R을 이용한 머신러닝 및 딥러닝 기반으로 예측 모형 개발

# ================================================
# 초기 환경변수 설정
# ================================================
# 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "local"

# 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
env = "dev"

# 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"


prjName = "test"
serviceName = "LSH0194"
contextPath = ifelse(env == "local", getwd(), "E:/04. TalentPlatform/Github/TalentPlatform-R")

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
# 함수 정의
#================================================
perfEval = function(x, y) {
  
  if (length(x) < 1) { return(sprintf("%s", "x 값 없음")) }
  if (length(y) < 1) { return(sprintf("%s", "y 값 없음")) }
  
  slope = coef(lm(y ~ x))[2]
  interp = coef(lm(y ~ x))[1]
  xMean = mean(x, na.rm = TRUE)
  yMean = mean(y, na.rm = TRUE)
  xSd = sd(x, na.rm = TRUE)
  ySd = sd(y, na.rm = TRUE)
  cnt = length(x)
  bias = mean(x - y, na.rm = TRUE)
  rBias = (bias / yMean) * 100.0
  rmse = sqrt(mean((x - y)^2, na.rm = TRUE))
  rRmse = (rmse / yMean) * 100.0
  r = cor.test(x, y)$estimate
  p = cor.test(x, y)$p.value
  diffMean = mean(x - y, na.rm = TRUE)
  diffSd = sd(x - y, na.rm = TRUE)
  # perDiffMean = mean((x - y) / y, na.rm = TRUE) * 100.0
  
  return(c(slope, interp, xMean, yMean, xSd, ySd, cnt, bias, rBias, rmse, rRmse, r, p, diffMean, diffSd))
}

biasCorr = function(actu, pred, minVal, maxVal, interVal, isPlot = FALSE) {
  
  factorVal = seq(minVal, maxVal, by = interVal)
  
  # RMSE Fitting
  liResult = lapply(1:length(factorVal), function(i) Metrics::rmse(actu, pred * factorVal[i])) %>%
    unlist()
  
  ind = which(liResult == min(liResult, na.rm = TRUE))
  
  if (isPlot == TRUE) {
    plot(liResult)
  }
  
  # Best Factor Index
  ind = which(liResult == min(liResult, na.rm = TRUE))
  
  calibFactor = factorVal[[ind]]
  calPred = calibFactor * pred
  
  meanDiff = mean(actu, na.rm = TRUE) - mean(calPred, na.rm = TRUE)
  newPred = (calPred) + meanDiff
  
  cat(
    sprintf("%s : %.2f", "[보정 X] RMSE", Metrics::rmse(actu, pred))
    , sprintf("%s : %.2f", "[보정 O] RMSE", Metrics::rmse(actu, newPred))
    , "\n"
  )
  
  return(c(newPred))
}


#================================================
# 비즈니스 로직 수행
#================================================
# 라이브러리 읽기
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(lubridate)
library(MASS)
library(scales)
library(dplyr)
library(hrbrthemes)
library(data.table)
library(ggpubr)
library(forcats)
library(lubridate)
library(openxlsx)
library(vroom)
library(RQuantLib)
library(caret)
library(tictoc)
library(caret)
library(glmnet)
library(Metrics)
library(randomForest)
library(mgcv)
library(nima)
library(h2o)
library(stringr)

# 로그 설정
saveLogFile = sprintf("%s/%s_%s_%s_%s.log", globalVar$logPath, Sys.info()["sysname"], Sys.info()["nodename"], prjName, format(Sys.time(), "%Y%m%d"))

log = log4r::create.logger()
log4r::logfile(log) = saveLogFile
log4r::level(log) = "INFO"

# 검증 지수 테이블 생성
rowNum = 1
colNum = 9
perfTable = data.frame(matrix(0, nrow = rowNum * colNum, ncol = 15))
rownames(perfTable) = c("MLR", "RF", "GAM", "SARIMA", "SVM", "GBM", "EL", "DNN", "AML")
# rownames(perfTable) = c(
#   paste0("MLR-", 1:rowNum), paste0("RF-", 1:rowNum), paste0("GAM-", 1:rowNum)
#   , paste0("SARIMA-", 1:rowNum), paste0("SVM-", 1:rowNum), paste0("DNN-", 1:rowNum)
# )
colnames(perfTable) = c("slope", "interp", "xMean", "yMean", "xSd", "ySd", "cnt", "bias", "rBias", "rmse", "rRmse", "r", "pVal", "diffMean", "diffSd")


# 데이터 읽기
fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0194_PAhourlyCHW.csv", sep = "/"))

data = vroom::vroom(
  file = fileInfo
  , col_select = c(YMDH, Uvalue_Wall, Uvalue_Window, Uvalue_Roof, WWR, Height, Year.x, AgeAfterRenov, Equipment, Lighting, Solar, HD, CD, Humidity, Pressure, WindSpeed, type2, CHWEUI)
  , col_names = TRUE
)

# PAHourlyCHW = data.table::fread(file = fileInfo)
# summary(data)
# summary(PAHourlyCHW)

RQuantLib::isBusinessDay("UnitedStates/NYSE", seq(from=lubridate::as_date(min(data$YMDH, na.rm = TRUE)), to=lubridate::as_date(max(data$YMDH, na.rm = TRUE)), by=1))

dataL1 = data %>% 
  dplyr::filter(
    0.01 < WWR & WWR < 0.9
    , type2 %in% c("Education", "Lab", "Lodge", "office ", "public")
  ) %>%
  dplyr::mutate(
    nMonth = lubridate::month(YMDH)
    , nDay = lubridate::day(YMDH)
    , nHour = lubridate::hour(YMDH)
    , refYmd = lubridate::make_date(year = 2000, month = nMonth, day = nDay)
    
    #  교호작용 변수
    , interTerm1 = Uvalue_Wall * WWR
    , interTerm2 = Uvalue_Window * WWR
    , interTerm3 = Year.x * AgeAfterRenov
    
    , isTrainValid = dplyr::between(lubridate::as_date(YMDH), lubridate::date("2015-07-01"), lubridate::date("2016-07-01"))
    
    # 펜실레니아 근처 뉴욕 기준으로 비즈니스 여부 판단
    # , isBizDay = bizdays::is.bizday(YMDH, "QuantLib/UnitedStates/NYSE")
    , isBizDay = RQuantLib::isBusinessDay("UnitedStates/NYSE", lubridate::as_date(YMDH))
    
    , seasonType = dplyr::case_when(
      dplyr::between(refYmd, lubridate::date("2000-01-13"), lubridate::date("2000-05-10")) ~ "spring"
      , dplyr::between(refYmd, lubridate::date("2000-05-11"), lubridate::date("2000-08-25")) ~ "summer"
      , dplyr::between(refYmd, lubridate::date("2000-08-26"), lubridate::date("2000-12-18")) ~ "fall"
      , lubridate::date("2000-12-19") <= refYmd | refYmd <= lubridate::date("2000-01-12") ~ "winter"
    )
    
    , bizDayType =  dplyr::case_when(
      isBizDay == TRUE ~ "Business day"
      , isBizDay == FALSE ~ "non-business day"
    )
    
    , hourType = dplyr::case_when(
      dplyr::between(nHour, 7, 17) ~ "working"
      , 7 < nHour & nHour <= 22 ~ "evening"
      , 22 < nHour | nHour < 7 ~ "night"
    )
  ) %>% 
  dplyr::mutate_if(is.character, as.factor)

# dplyr::select(YMDH, nMonth, nDay, nHour, WWR, refYmd, hourType, businessDay, seasonType)

# dplyr::tbl_df(dataL1)

# summary(data)
summary(dataL1)


# saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "PAhourlyCHW_FNL")
# readr::write_csv(x = dataL1, file = saveFile)
# 
# dataL2 = vroom::vroom(
#   file = saveFile
# )

#*******************************************
# 모형 구성
#*******************************************
dataL2 = dataL1 %>% 
  dplyr::select(-c(YMDH, nMonth, nDay, nHour, refYmd, isTrainValid, isBizDay))

# 선형회귀분석
lmFit = lm(CHWEUI ~ ., data = dataL2)
summary(lmFit)

# plot(lmFit)

# 단계별 소거법
lmFitStep = MASS::stepAIC(lmFit, direction = "both")
summary(lmFitStep)

# 독립변수 및 종속변수 선정
# modelForm = as.formula(lmFit$call$formula)
modelForm = as.formula(lmFitStep$call$formula)

modelFormSep = modelForm %>% paste(sep = " ~ ") 
modelFormY = modelFormSep[2]
modelFormX = modelFormSep[3] %>% stringr::str_split(" \\+ ") %>% unlist()

# 오래 시간 소요
# nima::lm_plot(lmFit)

# Residuals vs Fitted : 선형 회귀분석에서 오차는 평균이 0이고 분산이 일정한 정규분 분포를 가정하였으므로 잔차가 특별한 경향을 보이지 않는 것이 이상적이다.
# Gaussian Q-Q : 잔차가 정규분포를 따르는지 확인. 기울기 1인 직선이 되는 것이 이상적이다.
# Scale-Location : 또한 표준화 잔차가 특별한 경향을 보이지 않는 것이 이상적이다.
# Cook’s distance, Residuals vs Leverage,  Cook’s distance vs Leverage : 영향점의 유무를 검사하는데 유용하다. (중회귀분석 시간에 다시)

#*******************************************
# 훈련/검증/테스트 셋 설정
#*******************************************
set.seed(1)

trainData = dataL1 %>% 
  dplyr::filter(isTrainValid == TRUE) %>% 
  dplyr::select(-c(YMDH, nMonth, nDay, nHour, refYmd, isTrainValid, isBizDay))

# 테스트용
trainData = trainData %>% 
  dplyr::sample_n(100)

# 훈련 데이터셋 확인
dplyr::tbl_df(trainData)

# 훈련 및 데이터 셋을 80:20으로 나누기 위한 인덱스 설정
# idx = caret::createDataPartition(y = dataL3$CHWEUI, p = 0.8, list = FALSE)   

# 해당 인덱스에 따라 자료 할당
# trainData = dataL3[idx, ]
# validData = dataL3[-idx, ]

testData = dataL1 %>% 
  dplyr::filter(isTrainValid == FALSE) %>% 
  dplyr::select(-c(YMDH, nMonth, nDay, nHour, refYmd, isTrainValid, isBizDay))

# 검증 데이터셋 확인
# dplyr::tbl_df(validData)

# 테스트 데이터셋 확인
dplyr::tbl_df(testData)

#**********************************************************
# 학습 파라미터 설정
#**********************************************************
# method : 데이터 샘플링 기법로서  boot(부트스트래핑), boot632(부트스트래핑의 개선된 버전), cv(교차 검증), repeatedcv(교차 검증의 반복), LOOCV(Leave One Out Cross Validation) 
# repeats : 데이터 샘플링 반복 횟수
# number : 분할 횟수 
# controlInfo = caret::trainControl(
#   method = 'repeatedcv'
#   , repeats = 10
#   , number = 10
#   , p = 0.8
# )

controlInfo = caret::trainControl(
  method = 'repeatedcv'
  , repeats = 1
  , number = 10
  , p = 0.8
)


#**********************************************************
# 머신러닝 (MLR, RF, GAM, SARIMA, SVR, GBM)
#**********************************************************

#++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. Multiple Linear Regression (MLR) model
#++++++++++++++++++++++++++++++++++++++++++++++++++
# form : 모델 형식
# data : 모델 적용 데이터
# preProc : 데이터 전처리로서 center (평균이 0이 되게 함), scale (분산이 1이 되게 함), pca(주성분 분석) 설정 가능
# metric : 분류 문제의 경우 정확도(accuracy), 회귀 문제일 경우 RMSE로 자동 지정
# tuneGrid : 하이퍼 파라미터 설정
# trControl : 학습 파라미터 설정

# 모델 학습
mlrModel = caret::train(
  form = modelForm
  , data = trainData
  , method = "lm"
  , preProc = c("center", "scale")
  , metric = "RMSE"
  # , tuneGrid = expand.grid(
  #   intercept = c(TRUE, FALSE)
  #   )
  , trControl = controlInfo
)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "MLR RMSE Results Across Tuning Parameters")

ggplot(mlrModel) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

# 최적 모형의 회귀계수
mlrModel$finalModel 

# 모델 검증
perfTable["MLR", ] = perfEval(
  predict(mlrModel, newdata = testData)
  , testData$CHWEUI
) %>% 
  round(2)


#+++++++++++++++++++++++++++++++++++++++++++
# 5. Random Forest (RF)
#+++++++++++++++++++++++++++++++++++++++++++
# 모델 학습
rfModel = caret::train(
  form = modelForm
  , data = trainData
  , method = "rf"
  , preProc = c("center", "scale")
  , metric = "RMSE"
  # , tuneGrid = expand.grid(
  #   mtry = 1:2
  # )
  , trControl = controlInfo
)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "RF RMSE Results Across Tuning Parameters")

ggplot(rfModel) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

# 최적 모형의 회귀계수
rfModel$finalModel 

# 모델 검증
perfTable["RF", ] = perfEval(
  predict(rfModel, newdata = testData)
  , testData$CHWEUI
) %>% 
  round(2)

#+++++++++++++++++++++++++++++++++++++++++++
# 6. Generalized addictive model (GAM)
#+++++++++++++++++++++++++++++++++++++++++++

# Factor 자료형 제외하여 모델 구성
modelFormExceptFactor = as.formula("CHWEUI ~ Uvalue_Wall + Uvalue_Window + Uvalue_Roof + WWR + Height + Year.x + AgeAfterRenov + Equipment + Lighting + Solar + HD + CD + Humidity + Pressure + WindSpeed + interTerm1 + interTerm2")

# 현재 학습 과정에서 에러 발생 (Factor 자료형 포함 시 에러 발생)
gamModel = caret::train(
  form = modelFormExceptFactor
  , data = trainData
  , method = "gam"
  , preProc = c("center", "scale")
  , metric = "RMSE"
  # , tuneGrid = expand.grid(
  #   method = "GCV.Cp"
  #   , select = c(TRUE, FALSE)
  # )
  , trControl = controlInfo
)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "GAM RMSE Results Across Tuning Parameters")

ggplot(gamModel) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

# 최적 모형의 회귀계수
gamModel$finalModel 

# 모델 검증
perfTable["GAM", ] = perfEval(
  predict(gamModel, newdata = testData)
  , testData$CHWEUI
) %>% 
  round(2)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 7. Seasonal autoregressive integrated moving average (SARIMA)
# 
# 우선적으로 날짜 데이터를 시계열 데이터 변환
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# library(ForecastTB)
# library(predtoolsTS)
# 
# a <- prediction_errors(data = nottem)
# 
# modl.tsToDataFrame(AirPassengers,formula=c(1,3,4,5,6,7))
# modl.tsToDataFrame(AirPassengers,formula=c(1:20))
# 
# Lines <- "Dates   Bajaj_close Hero_close
# 3/14/2013   1854.8  1669.1
# 3/15/2013   1850.3  1684.45
# 3/18/2013   1812.1  1690.5
# 3/19/2013   1835.9  1645.6
# 3/20/2013   1840    1651.15
# 3/21/2013   1755.3  1623.3
# 3/22/2013   1820.65 1659.6
# 3/25/2013   1802.5  1617.7
# 3/26/2013   1801.25 1571.85
# 3/28/2013   1799.55 1542"
# 
# library(zoo)
# z <- read.zoo(text = Lines, header = TRUE, format = "%m/%d/%Y")
# 
# as.ts(read.zoo(Lines, FUN = as.yearmon))
# 
# DF <- read.table(text = Lines, header = TRUE)
# z <- read.zoo(DF, format = "%m/%d/%Y")
# 
# zz <- z
# time(zz) <- seq_along(time(zz))
# 
# 
# as.ts(z)
# as.ts(zz)
# 
# z.m <- as.zooreg(aggregate(z, as.yearmon, mean), freq = 12)
# as.ts(z.m)
# 
# 
# dta <- data.frame(
#   Dates = c("3/14/2013", "3/15/2013", "3/18/2013", "3/19/2013"),
#   Bajaj_close = c(1854.8, 1850.3, 1812.1, 1835.9),
#   Hero_close = c(1669.1, 1684.45, 1690.5, 1645.6)
# )
# 
# dta
# 
# 
# library(tsbox)
# b = ts_ts(ts_long(dta))
# 
# plot(b)
# 
# 
# library(dplyr)
# library(nycflights13)
# dta <- weather %>%
#   select(origin, time = time_hour, temp, humid, precip) %>%
#   ts_long()
# 
# dta %>%
#   filter(id == "temp") %>%
#   ts_trend() %>%
#   ts_plot()


#‘AirPassengers‘ is a sample dataset in CRAN
# prediction_errors(data = AirPassengers)

# 날짜 데이터를 시계열 데이터 변환
# tsData = ts(data$val, start = c(1999, 1), frequency = 12)

# 시계열 안정성 진단 및 검정
# P값이 0.01로서 귀무가설 기각 (정상 시계열)
# adf.test(tsData, alternative = c("stationary"), k = 0)
# Dickey-Fuller = -5.0135178, Lag order = 0, p-value = 0.01

# 시계열로부터 최종선택한 ARIMA 모형 (입력변수 : 시계열)
# bestModel = auto.arima(tsData, trace = TRUE)
# ARIMA(0,1,4)(1,1,2)[12]
# 비계절 : AR(0), 1차 차분, MA(4)
# 계절 : AR(1), 2차 차분 (24개월), MA(2)

# 시계열로부터 최종선택한 ARIMA 모형 (입력변수 : 시계열 + 입력 변수)
# xreg <- data.frame(MaxTemp = elecdaily[, "Temperature"],
#                    MaxTempSq = elecdaily[, "Temperature"]^2,
#                    Workday = elecdaily[, "WorkDay"])
# bestModel = forecast::auto.arima(elecdaily[, "Demand"], xreg = xreg)

# 모형의 통계적 유의성 검정
# X-squared = 0.071471882, df = 1, p-value = 0.7892057
# Box.test(bestModel$residuals, lag = 1, type = "Ljung")

# 잔차 그림 확인
# checkresiduals(bestModel)

# 최종선택한 모형을 사용하여 구한 예측값(월별자료는 향후 12개월)을 나타내는 그래프
# akimaData = forecast::forecast(bestModel, h = 12)

# 필요한 입력 변수에 따른 결과 
# akimaData = forecast::forecast(bestModel, xreg = cbind(rep(26,14), rep(26^2,14),  c(0,1,0,0,1,1,1,1,1,0,0,1,1,1))))

# 성능 비교
# accuracy(akimaData)


#+++++++++++++++++++++++++++++++++++++++++++
# 8. Support Vector Regression (SVM)
#+++++++++++++++++++++++++++++++++++++++++++
# 모델 학습
svmModel = caret::train(
  form = modelForm
  , data = trainData
  , method = "svmLinear"
  , preProc = c("center", "scale")
  , metric = "RMSE"
  # , tuneGrid = expand.grid(
  #   C = 2^(seq(-5, 5, 2))
  # )
  , trControl = controlInfo
)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "SVM RMSE Results Across Tuning Parameters")

ggplot(svmModel) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)


# 최적 모형의 회귀계수
svmModel$finalModel 

# 모델 검증
perfTable["SVM", ] = perfEval(
  predict(svmModel, newdata = testData)
  , testData$CHWEUI
) %>% 
  round(2)

#+++++++++++++++++++++++++++++++++++++++++++
# 12.	Gradient boosting machine (GBM)
#+++++++++++++++++++++++++++++++++++++++++++
# 모델 학습
gbmModel = caret::train(
  form = modelForm
  , data = trainData
  , method = "gbm"
  , preProc = c("center", "scale")
  , metric = "RMSE"
  # , tuneGrid = expand.grid(
  #   interaction.depth = 1:5
  #   , n.trees = (1:6) * 500
  #   , shrinkage = c(0.001, 0.01, 0.1)
  #   , n.minobsinnode = 10
  # )
  , trControl = controlInfo
)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "GBM RMSE Results Across Tuning Parameters")

ggplot(gbmModel) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

# 최적 모형의 회귀계수
gbmModel$finalModel 

# 모델 검증
perfTable["GBM", ] = perfEval(
  predict(gbmModel, newdata = testData)
  , testData$CHWEUI
) %>% 
  round(2)

#+++++++++++++++++++++++++++++++++++++++++++
# Heterogeneous Ensemble Learning (EL)
#+++++++++++++++++++++++++++++++++++++++++++
# 상위 3개 조합
elModelList = caretEnsemble::caretList(
  modelForm
  , data = trainData
  , trControl = controlInfo
  , methodList = c("svmLinear", "rf", "gbm")
)

# 앙상블 모델 결합
elModel = caretEnsemble::caretEnsemble(
  elModelList
  , trControl = controlInfo
  , metric = "RMSE"
)

# 모델 결과
summary(elModel)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "EL RMSE Results Across Tuning Parameters")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
plot(elModel)
dev.off()

# 최적 모형의 회귀계수
elModel$ens_model$finalModel

# 모델 검증
perfTable["EL", ] = perfEval(
  predict(elModel, newdata = testData)
  , testData$CHWEUI
) %>% 
  round(2)

#**********************************************************
# 딥러닝
#**********************************************************

#+++++++++++++++++++++++++++++++++++++++++++
# 11. Deep Neural Network (DNN)
#+++++++++++++++++++++++++++++++++++++++++++ 
# 초기화
h2o::h2o.init()

# activation : 활성화 함수로서 Rectifier 정규화 선형 함수 (즉 Keras의 ReLU 동일)
# hidden : 숨겨진 레이어의 수와 뉴런 수 (일반적으로 입력 차원의 1/10 or 1/100 단위)
# epochs : 반복 횟수 (기본 10-40)
# nfolds : 훈련 반복 수

layerNum = as.integer(nrow(trainData) / 10)
layerInfo = 3
epochsInfo = 1000

# 모델 학습
dnnModel = h2o::h2o.deeplearning(
  x = modelFormX
  , y = modelFormY
  , training_frame = as.h2o(trainData)
  , activation = "Rectifier"
  , hidden = rep(layerNum, layerInfo)
  , nfolds = 10
  , epochs = epochsInfo
  , seed = 1
)


saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "Training-Scoring-History")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
plot(dnnModel, timestep = "epochs", metric = "rmse")
dev.off()

# 모델 검증
perfTable["DNN", ] = perfEval(
  as.data.frame(h2o::h2o.predict(object = dnnModel, newdata = as.h2o(testData)))$predict
  , testData$CHWEUI
) %>% 
  round(2)


#+++++++++++++++++++++++++++++++++++++++++++
# 99. Automated Machine Learning (AML)
#+++++++++++++++++++++++++++++++++++++++++++
# 초기화
h2o::h2o.init()

# 모델 학습
amlModel = h2o::h2o.automl(
  x = modelFormX
  , y = modelFormY
  , training_frame = as.h2o(trainData)
  , nfolds = 10
  , sort_metric = "RMSE"
  , stopping_metric = "RMSE"
  , seed = 1
  # , max_runtime_secs = 60 * 120
  # , max_models = 50
  # , keep_cross_validation_predictions = TRUE
  # , stopping_rounds = 50
  # , stopping_tolerance = 0
)

# 모델 검증
perfTable["AML", ] = perfEval(
  as.data.frame(h2o::h2o.predict(object = amlModel, newdata = as.h2o(testData)))$predict
  , testData$CHWEUI
) %>% 
  round(2)
