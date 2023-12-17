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
# R을 이용한

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0528"

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
library(ggplot2)
library(dplyr)
library(glmnet)
library(rpart)
library(xgboost)
library(caret)
library(Metrics)


# Load the data
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "realtor-data.zip.csv"))

# bank_data = read.csv(fileInfo)
bank_data = readr::read_csv(fileInfo)

# Descriptive Statistics
summary(bank_data)

colnames(bank_data)

# bank_data$status %>% unique() %>% sort()
# bank_data$state %>% unique() %>% sort()
# bank_data$city %>% unique() %>% sort()

# Histogram of Income
ggplot(bank_data, aes(x = Income)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Income", x = "Income", y = "Frequency")


set.seed(123) # For reproducibility
split <- createDataPartition(bank_data$Personal.Loan, p = 0.8, list = FALSE)
train_data <- bank_data[split,]
test_data <- bank_data[-split,]

# Logistic Regression for Personal Loan prediction
logistic_model <- glm(Personal.Loan ~ ., family = binomial(link = 'logit'), data = train_data)
summary(logistic_model)

# Decision Tree
tree_model <- rpart(Personal.Loan ~ ., method = "class", data = train_data)
print(tree_model)

# XGBoost Classifier
labels <- as.numeric(train_data$Personal.Loan) - 1
data_matrix <- as.matrix(train_data[, -which(names(train_data) == "Personal.Loan")])
dtrain <- xgb.DMatrix(data = data_matrix, label = labels)
xgb_model <- xgboost(data = dtrain, nrounds = 100, objective = "binary:logistic")
print(xgb_model)

# 모델 비교 평가
logistic_predictions <- predict(logistic_model, newdata = test_data, type = "response")
logistic_predictions <- ifelse(logistic_predictions > 0.5, 1, 0)
confusion_logistic <- confusionMatrix(as.factor(logistic_predictions), as.factor(test_data$Personal.Loan))
tree_predictions <- predict(tree_model, newdata = test_data, type = "class")
confusion_tree <- confusionMatrix(tree_predictions, test_data$Personal.Loan)
dtest <- xgb.DMatrix(data = as.matrix(test_data[, -which(names(test_data) == "Personal.Loan")]))
xgb_predictions <- predict(xgb_model, dtest)
xgb_predictions <- ifelse(xgb_predictions > 0.5, 1, 0)
confusion_xgb <- confusionMatrix(as.factor(xgb_predictions), as.factor(test_data$Personal.Loan))


logistic_accuracy <- confusion_logistic$overall['Accuracy']
logistic_precision <- confusion_logistic$byClass['Precision']
logistic_recall <- confusion_logistic$byClass['Recall']
logistic_F1 <- 2 * (logistic_precision * logistic_recall) / (logistic_precision + logistic_recall)

tree_accuracy <- confusion_tree$overall['Accuracy']
tree_precision <- confusion_tree$byClass['Precision']
tree_recall <- confusion_tree$byClass['Recall']
tree_F1 <- 2 * (tree_precision * tree_recall) / (tree_precision + tree_recall)

xgb_accuracy <- confusion_xgb$overall['Accuracy']
xgb_precision <- confusion_xgb$byClass['Precision']
xgb_recall <- confusion_xgb$byClass['Recall']
xgb_F1 <- 2 * (xgb_precision * xgb_recall) / (xgb_precision + xgb_recall)

# Creating a summary table
performance_table <- data.frame(
  Model = c("Logistic Regression", "Decision Tree", "XGBoost"),
  Accuracy = c(logistic_accuracy, tree_accuracy, xgb_accuracy),
  Precision = c(logistic_precision, tree_precision, xgb_precision),
  Recall = c(logistic_recall, tree_recall, xgb_recall),
  F1_Score = c(logistic_F1, tree_F1, xgb_F1)
)

print(performance_table)


# # ================================================
# # 연령대별 여행자 비율
# # ================================================
# # 파일 읽기
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "국민 해외관광객 성연령별.csv"))
# sexData = readr::read_csv(fileInfo, locale = locale("ko", encoding = "EUC-KR"))
#
# sexDataL1 = sexData %>%
#   dplyr::select(-c("남성 수", "여성 수")) %>%
#   tidyr::pivot_longer(cols = c(`남성 비율`, `여성 비율`), names_to = "key", values_to = "val")
#
# # 연령대별 여행자 비율
# mainTitle = sprintf("%s", "연령대별 여행자 비율")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
# ggplot(sexDataL1, aes(x = 연령대, y = val, fill = key, group = key, label = round(val, 2))) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_text(position = position_dodge(width = 0.9), vjust = 1.5, size = 5, color = "white") +
#   labs(x = "연렁대", y = "비율 [%]", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
#   theme(
#     text = element_text(size = 16)
#     , legend.position = "top"
#     ) +
#   ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)
#
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#
#
# # ================================================
# # 월별 여행객
# # ================================================
# # 파일 읽기
# fileInfo2 = Sys.glob(file.path(globalVar$inpPath, serviceName, "국민 해외관광객 추이.csv"))
# trendData = readr::read_csv(fileInfo2, locale = locale("ko", encoding = "EUC-KR"))
#
# trendDataL1 = trendData %>%
#   dplyr::mutate(
#     dtDate = readr::parse_date(as.character(기준연월), format = "%Y%m")
#   ) %>%
#   dplyr::rename(
#     val = `국민 해외관광객 수`
#   )
#
# # 월별 여행객
# mainTitle = sprintf("%s", "월별 여행객")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
# ggplot(data = trendDataL1, aes(x = dtDate, y = val)) +
#   geom_line() +
#   geom_point() +
#   scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month", minor_breaks = "1 month") +
#   labs(x = "날짜 [년-월]", y = "해외 여행객", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
#   theme(
#     text = element_text(size = 16)
#     , legend.position = "top"
#     , axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)
#
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
