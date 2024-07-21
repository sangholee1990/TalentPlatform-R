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
# R을 이용한 인공신경망 및 결정트리 기반으로 대출 여부 예측

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0573"

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
# Load necessary libraries
library(caret)
library(openxlsx)
library(tibble)
library(tidyverse)
library(nnet)
library(neuralnet)
library(caret)
library(rpart)

# Load the data
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "UniversalBank.xlsx"))
data = openxlsx::read.xlsx(fileInfo, sheet = 1, startRow = 1) %>% 
  as.tibble()

# ==============================================================================
# Data Prtitioning
# ==============================================================================
# for reproducibility
set.seed(123)

# Accurately partitions the data into training and validation sets with a split of 60% training and 40% validation. 
# trainIdx = sample(c(1:nrow(data)), nrow(data) * 0.6)  
trainIdx = createDataPartition(data$PersonalLoan, p = 0.6, list = FALSE)
trainData = data[trainIdx, ]
validData = data[-trainIdx, ]

# The success class is correctly specified as 1 (loan acceptance), and the default cutoff value of 0.5 is used.

# ==============================================================================
# Data PreProcess
# ==============================================================================
# Remove ID and ZIP Code
trainDataL1 = trainData[ , !(names(trainData) %in% c("ID", "ZIP.Code"))]
validDataL1 = validData[ , !(names(validData) %in% c("ID", "ZIP.Code"))]

# summary(trainDataL1)

# Categorical predictors with more than two categories are appropriately transformed into dummy variables, 
trainDataL1$Family = as.factor(trainDataL1$Family)
trainDataL1$Education = as.factor(trainDataL1$Education)

validDataL1$Family = as.factor(validDataL1$Family)
validDataL1$Education = as.factor(validDataL1$Education)

# str(trainDataL1)

# 범주형 변수
trainDummy = dummyVars(PersonalLoan ~ ., data = trainDataL1)
trainDataL2 = predict(trainDummy, newdata = trainDataL1)

validDummy = dummyVars(PersonalLoan ~ ., data = validDataL1)
validDataL2 = predict(validDummy, newdata = validDataL1)


# 숫자형 변수 스케일링 (0-1 범위로 조정)
trainProc = preProcess(trainDataL2, method = c("range"))
trainDataL3 = predict(trainProc, trainDataL2)

validProc = preProcess(validDataL2, method = c("range"))
validDataL3 = predict(validProc, validDataL2)


# PersonalLoan 열 추가
trainDataL4 = as.data.frame(trainDataL3)
# trainDataL4$PersonalLoan = trainData$PersonalLoan
trainDataL4$PersonalLoan = as.factor(trainData$PersonalLoan)

validDataL4 = as.data.frame(validDataL3)
# validDataL4$PersonalLoan = validData$PersonalLoan
validDataL4$PersonalLoan = as.factor(validData$PersonalLoan)

# str(trainDataL4)
# str(validDataL4)

# ==============================================================================
# Neutral Net Classification
# ==============================================================================
# 신경망 모델 학습
set.seed(123)
nnetModel = caret::train(PersonalLoan ~ ., data = trainDataL4, method = "nnet", linout = FALSE, trace = FALSE)

# 신경망 모델 예측
trainDataL5 = trainDataL4
validDataL5 = validDataL4

trainDataL5$nnetPrd = predict(nnetModel, trainDataL5)
# trainDataL5$nnetPrdBin = ifelse(trainDataL5$nnetPrd > 0.5, 1, 0)

validDataL5$nnetPrd = predict(nnetModel, validDataL5)
# validDataL5$nnetPrdBin = ifelse(validDataL5$nnetPrd > 0.5, 1, 0)

# Confusion Matrix 및 성능 비교
trainNnetConMat = confusionMatrix(trainDataL5$nnetPrd, trainDataL5$PersonalLoan)
validNnetConMat = confusionMatrix(validDataL5$nnetPrd, validDataL5$PersonalLoan)

# ==============================================================================
# Decision Tree Classification
# ==============================================================================
set.seed(124)
dtModel = train(PersonalLoan ~ ., data = trainDataL4, method = "rpart")

# 가지치기
bestCp = dtModel$finalModel$cptable[which.min(dtModel$finalModel$cptable[,"rel error"]), "CP"]
pruneCt = rpart::prune(dtModel$finalModel, cp = bestCp)

# Decision Tree 예측
trainDataL5$dtPrd = predict(pruneCt, newdata = trainDataL5)[, "1"]
trainDataL5$dtPrdBin = ifelse(trainDataL5$dtPrd > 0.5, 1, 0)

validDataL5$dtPrd = predict(pruneCt, newdata = validDataL5)[, "1"]
validDataL5$dtPrdBin = ifelse(validDataL5$dtPrd > 0.5, 1, 0)

# ==============================================================================
# Model Comparison
# ==============================================================================
# Confusion Matrix 및 성능 비교
trainDtConMat = confusionMatrix(as.factor(trainDataL5$dtPrdBin), trainDataL5$PersonalLoan)
validDtConMat = confusionMatrix(as.factor(validDataL5$dtPrdBin), validDataL5$PersonalLoan)


# 검증 데이터를 기준으로 성능 비교
validNnetConMat$table
validDtConMat$table

validNnetConMat$overall %>% round(2)
validDtConMat$overall %>% round(2)

# 그 결과 nnet 모형 (Accuracy = 0.97, Kappa = 0.86)은 rpart 모형 (Accuracy = 0.96, Kappa = 0.76)보다 높은 정확도 및 Kappa 검증 결과를 보임

# ==============================================================================
# Customer Classification
# ==============================================================================
# 고객 데이터 
# Family 4종 분류
# Education 3종 분류
cusData = tibble::tibble(
  Age = 40,
  Experience = 10,
  Income = 84,
  Family.1 = 0,
  Family.2 = 1,
  Family.3 = 0,
  Family.4 = 0,
  CCAvg = 2,
  Education.1 = 0,
  Education.2 = 1,
  Education.3 = 0,
  Mortgage = 0,
  SecuritiesAccount = 0,
  CDAccount = 0,
  Online = 1,
  CreditCard = 1
)

# Age, Experience, Income, CCAvg 스케일링
maxData = apply(trainData, 2, max)
minData = apply(trainData, 2, min)

cusDataL1 = cusData %>% 
  dplyr::mutate(
    Age = (Age - minData["Age"]) / (maxData["Age"] - minData["Age"])
    , Experience = (Experience - minData["Experience"]) / (maxData["Experience"] - minData["Experience"])
    , Income = (Income - minData["Income"]) / (maxData["Income"] - minData["Income"])
    , CCAvg = (CCAvg - minData["CCAvg"]) / (maxData["CCAvg"] - minData["CCAvg"])
  )

str(cusDataL1)

# 개인 대출 예측 결과 거절 (0)로 파악됨
cusDataL1$nnetPrd = predict(nnetModel, cusDataL1)
print(cusDataL1$nnetPrd)

