library(ggplot2)
library(dplyr)
install.packages("Metrics")

# Load the data
bank_data <- read.csv("UniversalBank.csv")

# Descriptive Statistics
summary(bank_data)

# Histogram of Income
ggplot(bank_data, aes(x = Income)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Income", x = "Income", y = "Frequency")

library(glmnet)
library(rpart)
library(xgboost)
library(caret)
library(Metrics)

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
