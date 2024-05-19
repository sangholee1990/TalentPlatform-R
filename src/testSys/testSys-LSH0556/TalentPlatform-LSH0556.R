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
# R을 이용한 lime 대리 분석을 활용한 일사량 예측 모델링 시각화 및 해석

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0556"

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
library(caret)
library(randomForest)
library(lime)

# 파일 검색
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "SolarPF_184.csv"))

# 파일 읽기
data = read.csv(fileInfo) %>% 
  tibble::as.tibble()

# Day_sin: 날짜의 사인 변환 값 (연속형)
# Day_cos: 날짜의 코사인 변환 값 (연속형)
# T8 - T18: 오전 8시부터 오후 6시까지의 시간별 이진 표시 (이진형)
# W1: 맑은 날씨 표시 (이진형)
# W2: 부분적으로 흐린 날씨 표시 (이진형)
# W3: 대부분 흐린 날씨 표시 (이진형)
# W4: 흐린 날씨 표시 (이진형)
# Temp: 기온 측정 값 (연속형)
# Humi: 습도 측정 값 (연속형)
# Wind_speed: 풍속 측정 값 (연속형)
# D1_Day_sin: 전날의 날짜에 대한 사인 변환 값 (연속형)
# D1_Day_cos: 전날의 날짜에 대한 코사인 변환 값 (연속형)
# D1_W1 - D1_W4: 전날의 날씨 상태 이진 표시 (이진형)
# D1_Temp: 전날의 기온 측정 값 (연속형)
# D1_Humi: 전날의 습도 측정 값 (연속형)
# D1_Wind_speed: 전날의 풍속 측정 값 (연속형)
# D1_Solar: 전날의 일사량 측정 값 (연속형)
# Solar: 측정된 일사량 (연속형)

# 데이터 구조 파악
str(data)

# 결측값 삭제 
# 샘플링 200 또는 1000개 추출
dataL1 = data %>% 
  na.omit() %>% 
  as.tibble() %>% 
  dplyr::slice(1:1000)
  # dplyr::slice(1:200)


# 요약
summary(dataL1)

# ==============================================================================
# 훈련 70% 및 테스트 30% 분할을 위한 인덱스 설정
# ==============================================================================
# 데이터 분할
set.seed(123)

idx = createDataPartition(dataL1$Solar, p = 0.7, list = FALSE)
# idx = sample(1:nrow(dataL1), nrow(dataL1) * 0.7)

trainData = dataL1[idx, ]
testData = dataL1[-idx, ]

# 훈련 데이터셋 확인
head(trainData)

# 테스트 데이터셋 확인
head(testData)

trainDataX = trainData %>% dplyr::select(-Solar)
trainDataY = trainData %>% dplyr::select(Solar)

testDataX = testData %>% dplyr::select(-Solar)
testDataY = testData %>% dplyr::select(Solar)

# ==============================================================================
# 랜덤포레스트 RF 학습/저장/불러오기/예측/검증
# ==============================================================================
saveModel = sprintf("%s/%s/%s.rds", globalVar$outPath, serviceName, "rfModel")
dir.create(fs::path_dir(saveModel), showWarnings = FALSE, recursive = TRUE)

if (file.exists(saveModel) == TRUE) {
  # 학습모델 불러오기
  rfModel = readRDS(saveModel)
} else {
  # 학습모형 수행
  # rfModel = randomForest(Solar ~ ., data = trainData, importance = TRUE)
  controlInfo = caret::trainControl(method = "cv", number = 2)
  rfModel = caret::train(Solar ~ ., data = trainData, method = "rf", trControl = controlInfo)
  
  # 학습모델 저장
  saveRDS(rfModel, file = saveModel)
  cat(sprintf("[CHECK] saveModel : %s", saveModel), "\n")
}

# 랜덤포레스트 RF 모형 성능 시각화
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "RF모델 성능 시각화")
# dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# 
# ggplot(rfModel) +
#   theme(text = element_text(size = 16)) +
#   ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)
# 
# # shell.exec(saveImg)
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# 최적 모형의 회귀계수
rfModel

# 테스트 데이터를 이용하여 검증 결과
obs = testData$Solar
prd = predict(rfModel, newdata = testData)

caret::postResample(pred = prd, obs = obs)

# RF모델 변수 중요도 시각화
impData = varImp(rfModel)

saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "RF모델 변수 중요도 시각화")
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(impData) +
  theme(text = element_text(size = 16)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
  

# ==============================================================================
# lime 대리 분석
# ==============================================================================
limeModel = lime::lime(testDataX, rfModel)
summary(limeModel)

limeRes = lime::explain(testDataX, limeModel, n_features = 1)
print(limeRes)

# lime 시각화
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "RF모델 lime 시각화")
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

lime::plot_explanations(limeRes) +
  theme(text = element_text(size = 10)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
