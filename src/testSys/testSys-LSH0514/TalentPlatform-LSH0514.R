#================================================
# 요구사항
#================================================
# R을 이용한 마케팅 데이터 분석 (요약통계량, 히스토그램, 군집분석, 분류나무 분석)

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0514"
contextPath = "."

globalVar = list(
  "inpPath" = contextPath
  , "figPath" = contextPath
  , "outPath" = contextPath
  , "tmpPath" = contextPath
  , "logPath" = contextPath
)

# ================================================
# 비즈니스 로직 수행
# ================================================
# 라이브러리 읽기
library(cluster)
library(rpart)
library(caret)

# 파일 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "sales.csv"))
data = read.csv(fileInfo)

# ==============================================================================
# 1. summary()함수를	이용하여	acctAge,	spendToDate의	평균값을	구하고	그	결과를 해석하여라.
# ==============================================================================
# 거래 기간의 평균은 약 12.84개월로서 1년 이상 회원으로 활동 중임
summary(data$acctAge)["Mean"]

# 누적 총 구매금액의 평균은 약 125.94 USD로서 고객 1명당 약 126 달러의 구매함
summary(data$spendToDate)["Mean"]

# ==============================================================================
# 2. hist ()함수를 이용하여 visitsMonth의 히스토그램을 그리고 그 결과를 해석하여라.
# ==============================================================================
# 쇼핑몰 방문 횟수는 주로 2~10회 범위 내에서 4~8회에 높은 빈도로 집중되어 있는 반면에 그 외의 경우 (10회 이상) 상대적으로 적은 빈도로 측정됨
# 또한 좌우 대칭적인 분포보다는 우측으로 치우친 모양을 띠기 때문에 평균 방문 횟수 (평균 7.18회, 중간값 7.00)보다 더 많은 고객이 방문함 
hist(data$visitsMonth, main = "쇼핑몰 방문회수에 따른 빈도", xlab = "방문 횟수", ylab = "빈도")

# ==============================================================================
# 3. kmeans()함수를 이용하여 주어진 변수 중 1~8번까지의 8개 변수를 사용하여 k=4로  하는 k평균 군집분석을 수행한 뒤, 아래 질문에 답하세요.
# ==============================================================================
set.seed(1)

# k평균 군집분석
kmeansModel = kmeans(data[, 1:8], centers = 4)

# 3-1) 각각의 군집에 몇 명의 고객이 배정되었는가? (hint: table()함수를 이용)
# 군집은 4개 유형으로 형성되며 즉 각각 630, 36, 61, 108로 배정됨
table(kmeansModel$cluster)
#   1   2   3   4 
# 630  36  61 108 

# 3-2) 4개 군집에 속한 고객들의 spendToDate값 평균은 각각 어떻게 되는가? (hint: aggregate()함수 이용)
# 4개 유형을 기준으로 spendToDate의 평균은 각각 42.22, 893.86, 454.23, 172.94임
round(aggregate(data$spendToDate, by = list(kmeansModel$cluster), FUN = mean), 2)
# Group.1      x
# 1       1  42.22
# 2       2 893.86
# 3       3 454.23
# 4       4 172.94

# 3-3) 4개 군집에 속한 고객들의 spendToDate값의 평균이 차이가  있는  지를  확인하기  위한 분산분석을 실행하고, 그 결과를 해석하여라.
# P값은 2.22e-16로서 유의수준 0.05 이하이기 때문에 귀무 가설을 기각하여 군집 간에 spendToDate의 평균에 통계적인 유의미한 차이를 보임
aovRes = aov(spendToDate ~ factor(kmeansModel$cluster), data = data)

summary(aovRes)
#                              Df   Sum Sq  Mean Sq  F value     Pr(>F)    
# factor(kmeansModel$cluster)   3 32457928 10819309 4575.893 < 2.22e-16 ***
# Residuals                   831  1964829     2364                        
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# ==============================================================================
# 4. 분류나무 분석: 835명의 데이터를 학습데이터(80%)와 검증데이터(20%)로 분류한 뒤, purchase를 예측하기 위한 분류나무 기본형을 그리는 분석을 실시하여라.
# ==============================================================================
set.seed(1)

# 자료형 변환
data$purchase = as.factor(data$purchase)

# 데이터 분할
idx = sample(1:nrow(data), 0.8 * nrow(data))
trainData = data[idx, ]
testData = data[-idx, ]

# 4-1) 학습데이터와 검증데이터에는 각각 몇 명의 고객이 배정되었는가?
nrow(trainData)
# 668

nrow(testData)
# 167

# 4-2) 분류나무(기본형)를 그린 결과그림을 아래에 첨부하고, purchase가 1(구매)로 판정되기 위한 의사결정 규칙을 나열하여라.
treeModel = rpart(purchase ~ ., data = trainData, method = "class")

# 1) root 668 21 0 (0.96856287425 0.03143712575)  
#   2) visitsMonth< 12.5 660 16 0 (0.97575757576 0.02424242424)  
#     4) satSite< 7.5 589  6 0 (0.98981324278 0.01018675722) *
#     5) satSite>=7.5 71 10 0 (0.85915492958 0.14084507042)  
#       10) spendMonth< 27 48  0 0 (1.00000000000 0.00000000000) *
#       11) spendMonth>=27 23 10 0 (0.56521739130 0.43478260870)  
#         22) visitsMonth< 7.5 7  0 0 (1.00000000000 0.00000000000) *
#         23) visitsMonth>=7.5 16  6 1 (0.37500000000 0.62500000000) *
# 3) visitsMonth>=12.5 8  3 1 (0.37500000000 0.62500000000) *
treeModel
plot(treeModel)
text(treeModel, use.n = TRUE)

# 4-3) 위에서 그린 분류나무(기본형)로 학습데이터를 예측할 경우 정확도(accuracy)는 얼마인가? (confusion matrix 활용)
trainPrd = predict(treeModel, trainData, type = "class")

# 정확도 0.9775449102 
confusionMatrix(data = trainPrd, reference = trainData$purchase)$overall["Accuracy"]

# 4-4) 위에서 그린 분류나무(기본형)로 검증데이터를 예측할 경우 정확도(accuracy)는 얼마인가? (confusion matrix 활용)
testPrd = predict(treeModel, testData, type = "class")

# 정확도 0.9520958084
confusionMatrix(testPrd, testData$purchase)$overall["Accuracy"]
