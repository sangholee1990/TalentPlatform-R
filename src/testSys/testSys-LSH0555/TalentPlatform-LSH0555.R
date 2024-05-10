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
# R을 이용한 와인 품질 데이터 분석 및 분류 모델링 (의사결정나무, 랜덤포레스트) 그리고 한글 보고서

# UC Irvine Machine Learning Repository에 매우 다양한 데이터을 포함하고 있습니다. 
# 10개 이상의 변수, 100개 이상의 관측치의 크기를 가지는 데이터셋을 하나 선택하여 방법론 중 2가지 이상을 적용하여 결과를 도출해 보시기 바랍니다. 
# https://archive.ics.uci.edu/

# 방법론에서 2가지 이상입니다.
# 의사결정나무
# 랜덤포레스트
# 순환신경망
# 다중회귀모형
# 로지스틱회귀분석
# 군집분석
# 주성분분석

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0555"

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
library(rpart)
library(randomForest)

# 파일 검색
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "wine_quality/winequality-red.csv"))

# 파일 읽기
data = readr::read_delim(fileInfo)

# 결측값 삭제
data = na.omit(data)

# 변수 확인
str(data)

# 컬럼 정보 
colnames(data)

# 요약
summary(data)

# 와인 품질 부여 (6 이상 1:Good, 그 외 0:Bad)
# data$quaflag = ifelse(data$quality >= 6, "Good", "Bad")
data$quaflag = ifelse(data$quality >= 7, 1, 0)
# data$quaflag = data$quality

# 변수 제거
dataL1 = data[ , ! names(data) %in% c("quality")]

#=================================================================
# 훈련 및 테스트 셋 설정
#=================================================================
# 훈련 및 데이터 셋을 70:30으로 나누기 위한 인덱스 설정
idx = sample(1:nrow(dataL1), nrow(dataL1) * 0.7)

# 해당 인덱스에 따라 자료 할당
trainData = dataL1[idx, ]
testData = dataL1[-idx, ]

# 훈련 데이터셋 확인
head(trainData)

# 테스트 데이터셋 확인
head(testData)

# ================================================
# 의사결정나무
# ================================================
# 학습모형
treeModel = rpart(quaflag ~ ., data = trainData)

summary(treeModel)

# 변수 중요도
print(treeModel$variable.importance)

# 예측
prd = predict(treeModel, newdata=testData)
prdFlag = ifelse(prd > 0.5, 1, 0)
obsFlag = testData$quaflag

table(prdFlag, obsFlag)

conMatRes = caret::confusionMatrix(data = factor(prdFlag), reference = factor(obsFlag))

# 정확도 : 0.860
round(conMatRes$overall["Accuracy"], 3)

# 민감도 : 0.933  
round(conMatRes$byClass["Sensitivity"], 3)

# 특이도 : 0.391  
round(conMatRes$byClass["Specificity"], 3)

# ROC 커브를 위한 설정
logitRoc = ROCit::rocit(score = prdFlag, class = obsFlag)

# 요약 결과
summary(logitRoc)

# mainTitle = "ROC 곡선-유의미한 변수"
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, mainTitle)
# png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

plot(logitRoc, main = mainTitle)

# dev.off()


# ================================================
# 랜덤포레스트
# ================================================
# 학습모형
rfModel = randomForest(quaflag ~ ., data = trainData)

summary(rfModel)

# 변수 중요도
print(importance(rfModel))

# 예측
prd = predict(rfModel, newdata=testData)
prdFlag = ifelse(prd > 0.5, 1, 0)
obsFlag = testData$quaflag

table(prdFlag, obsFlag)

conMatRes = caret::confusionMatrix(data = factor(prdFlag), reference = factor(obsFlag))

# 정확도 : 0.902
round(conMatRes$overall["Accuracy"], 3)

# 민감도 : 0.974  
round(conMatRes$byClass["Sensitivity"], 3)

# 특이도 : 0.438  
round(conMatRes$byClass["Specificity"], 3)

# ROC 커브를 위한 설정
logitRoc = ROCit::rocit(score = prdFlag, class = obsFlag)

# 요약 결과
summary(logitRoc)

# mainTitle = "ROC 곡선-유의미한 변수"
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, mainTitle)
# png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

plot(logitRoc, main = mainTitle)

# dev.off()
