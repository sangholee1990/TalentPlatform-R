
library(dplyr)
library(tidyr)
library(caret)
library(e1071)
library(randomForest)
library(class)
library(nnet)
library(pROC)
library(MASS)

# 패키지 목록 보기
installed.packages()

# 특정 패키지에서 함수 목록 보기
help(package = caret)

# 특정 함수에서 예제 보기
?caret::createDataPartition


# 
# packList
# for (packInfo in packList) {
#   library(packInfo, character.only = TRUE)
# }
# 
# library(caret)
# ls(package:caret)
# help(package = "caret")
# ??caret
# search()


data = mtcars

summary(data)

idx = caret::createDataPartition(data$mpg, p = 0.6, list = FALSE)
trainData = data[idx, ]
testData = data[-idx, ]

controlInfo = caret::trainControl(
  method = 'cv'
  , number = 10
)


# LM
lmModel = caret::train(
  mpg ~ .
  , data = trainData
  , method = "lm"
  , preProc = c("center", "scale")
  , trControl = controlInfo
)

yObs = testData$mpg
yPrd = predict(lmModel, newdata = testData)

caret::postResample(pred = yPrd, obs = yObs)

# glm
glmModel = caret::train(
  mpg ~ .
  , data = trainData
  , method = "glm"
  , preProc = c("center", "scale")
  , trControl = controlInfo
)

yObs = testData$mpg
yPrd = predict(glmModel, newdata = testData)

caret::postResample(pred = yPrd, obs = yObs)


# SVM
svmModel = caret::train(
  mpg ~ .
  , data = trainData
  , method = "svmLinear"
  , preProc = c("center", "scale")
  , trControl = controlInfo
)

yObs = testData$mpg
yPrd = predict(svmModel, newdata = testData)

caret::postResample(pred = yPrd, obs = yObs)

# RF
rfModel = caret::train(
  mpg ~ .
  , data = trainData
  , method = "rf"
  , preProc = c("center", "scale")
  , trControl = controlInfo
)

yObs = testData$mpg
yPrd = predict(rfModel, newdata = testData)

caret::postResample(pred = yPrd, obs = yObs)

# knn
knnModel = caret::train(
  mpg ~ .
  , data = trainData
  , method = "knn"
  , preProc = c("center", "scale")
  , trControl = controlInfo
)

yObs = testData$mpg
yPrd = predict(knnModel, newdata = testData)

caret::postResample(pred = yPrd, obs = yObs)

# nnet
nnetModel = caret::train(
  mpg ~ .
  , data = trainData
  , method = "nnet"
  , preProc = c("center", "scale")
  , trControl = controlInfo
)

yObs = testData$mpg
yPrd = predict(nnetModel, newdata = testData)

caret::postResample(pred = yPrd, obs = yObs)


# gbm
gbmModel = caret::train(
  mpg ~ .
  , data = trainData
  , method = "gbm"
  , preProc = c("center", "scale")
  , trControl = controlInfo
)

yObs = testData$mpg
yPrd = predict(gbmModel, newdata = testData)

caret::postResample(pred = yPrd, obs = yObs)



# dnn
dnnModel = caret::train(
  mpg ~ .
  , data = trainData
  , method = "dnn"
  , preProc = c("center", "scale")
  , trControl = controlInfo
)

yObs = testData$mpg
yPrd = predict(dnnModel, newdata = testData)

caret::postResample(pred = yPrd, obs = yObs)



####################################################

dataL1 = mtcars
dataL1$vs = as.factor(dataL1$vs)

summary(dataL1)


idx = caret::createDataPartition(dataL1$vs, p = 0.6, list = FALSE)
trainData = dataL1[idx, ]
testData = dataL1[-idx, ]

yObs = testData$vs

controlInfo = caret::trainControl(
  method = 'cv'
  , number = 10
)

# glm
glmModel = caret::train(
  vs ~ .
  , data = trainData
  , method = "glm"
  , preProc = c("center", "scale")
  , trControl = controlInfo
)


yPrd = predict(glmModel, newdata = testData, type="raw")
caret::postResample(pred = yPrd, obs = yObs)

yPrd = predict(glmModel, newdata = testData, type="prob")
caret::postResample(pred = yPrd[ ,1], obs = as.numeric(yObs))

# 혼동행렬
caret::confusionMatrix(data = yPrd, reference = yObs)



# SVM
svmModel = caret::train(
  vs ~ .
  , data = trainData
  , method = "svmLinear"
  , preProc = c("center", "scale")
  , trControl = controlInfo
)

yPrd = predict(svmModel, newdata = testData)

caret::postResample(pred = yPrd, obs = yObs)
caret::confusionMatrix(data = yPrd, reference = yObs)

# RF
rfModel = caret::train(
  vs ~ .
  , data = trainData
  , method = "rf"
  , preProc = c("center", "scale")
  , trControl = controlInfo
)

yPrd = predict(rfModel, newdata = testData)

caret::postResample(pred = yPrd, obs = yObs)
caret::confusionMatrix(data = yPrd, reference = yObs)

# knn
knnModel = caret::train(
  vs ~ .
  , data = trainData
  , method = "knn"
  , preProc = c("center", "scale")
  , trControl = controlInfo
)

yPrd = predict(knnModel, newdata = testData)

caret::postResample(pred = yPrd, obs = yObs)
caret::confusionMatrix(data = yPrd, reference = yObs)

# nnet
nnetModel = caret::train(
  vs ~ .
  , data = trainData
  , method = "nnet"
  , preProc = c("center", "scale")
  , trControl = controlInfo
)

yPrd = predict(nnetModel, newdata = testData)

caret::postResample(pred = yPrd, obs = yObs)
caret::confusionMatrix(data = yPrd, reference = yObs)







