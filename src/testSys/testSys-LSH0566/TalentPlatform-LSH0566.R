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
# R을 이용한 EUDAMED 사이트로부터 웹 스크래핑 및 장기간 수집

# 선생님 안녕하세요. 견적 요청드리고싶은 건 있어서 다시 연락드립니다. 이번에도 R에서 html 코딩이 필요한 상황입니다.

# 1. https://ec.europa.eu/tools/eudamed/#/screen/search-device?submitted=true 접속 (결과값: 471839 records found)
#   
#   2. 471839건 각각 View action 누르면 나오는 페이지에서 아래 내용을 스크래핑
# 1) Actor/Organisation name
# 2) Actor ID/SRN
# 3) Applicable legislation
# 4) Basic UDI-DU/EUDAMED DI/ Issuing entity
# 5) Risk class
# 6) Device name
# 7) UDI-DI code/Issuing entity
# 8) Status,Nomenclature code(s)
# 9) Name/Trade name(s)
# 10) Member state of the placing on the EU market of the device
# 
# 3. 위 스크래핑해온 10가지 항목을 dataframe으로 만든 후 엑셀로 추출
# 
# 4. 결과: 10 X 471839 크기의 데이터셋

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0566"

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
library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)

allCnt = 464573
# pageSize = 300
pageSize = 25

pageMax = ceiling(allCnt / pageSize)
pageList = 0:pageMax

# 464573 

# https://ec.europa.eu/tools/eudamed/api/devices/udiDiData?page=1&pageSize=25&size=25&iso2Code=en&sort=primaryDi,ASC&sort=versionNumber,DESC&deviceStatusCode=refdata.device-model-status.on-the-market&languageIso2Code=en

# baseUrl = "https://ec.europa.eu/tools/eudamed/api/devices/udiDiData"
# params = list(
#   page = 1,
#   pageSize = pageSize,
#   size = pageSize,
#   iso2Code = "en",
#   sort = c("primaryDi,ASC", "versionNumber,DESC"),
#   deviceStatusCode = "refdata.device-model-status.on-the-market",
#   languageIso2Code = "en"
# )
# 
# 
# response = httr::GET(baseUrl, query = params)

url = sprintf("https://ec.europa.eu/tools/eudamed/api/devices/udiDiData?page=%s&pageSize=%s&size=%s&iso2Code=en&deviceStatusCode=refdata.device-model-status.on-the-market&languageIso2Code=en", 0, pageSize, pageSize)

# API 요청
apiRes = httr::GET(url)
if (httr::status_code(apiRes) != 200) next

# API 응답
resData = httr::content(apiRes, as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON()

if (length(resData) < 1) next

# 데이터프레임으로 변환
resDataL1 = resData$content %>% 
  tibble::as.tibble()

if (nrow(resDataL1) < 1) next

resDataL1$uuid[1]

urlDtl = sprintf("https://ec.europa.eu/tools/eudamed/api/devices/basicUdiData/udiDiData/%s?languageIso2Code=en", resDataL1$uuid[1])
# urlDtl = sprintf("https://ec.europa.eu/tools/eudamed/api/devices/basicUdiData/udiDiData/%s?languageIso2Code=en", resDataL1$uuid[1])

apiResDtl = httr::GET(urlDtl)

# API 응답
resDtlData = httr::content(apiResDtl, as = "text", encoding = "UTF-8") %>%
  jsonlite::fromJSON()

# 데이터프레임으로 변환
# resDtlDataL1 = resDtlData$deviceCertificateInfoListForDisplay %>% 
  # tibble::as.tibble()

# resDtlData$manufacturer

# ******************************************************************************
# Manufacturer details
# ******************************************************************************
manDtlInfo = resDtlData$manufacturer

# 1) Actor/Organisation name
acrOrgName = tryCatch({
  sprintf("%s[%s]", manDtlInfo$name, toupper(manDtlInfo$names$texts$language$isoCode))
}, error = function(e) {NA})


rvest::read_html(urlDtl) %>% 
  html_node(xpath = '//*[@id="cdk-accordion-child-0"]/div/div/dl[1]/dd/div') %>%
  html_text(trim = TRUE)

xml2::read_html(urlDtl) %>%
  html_nodes(".ng-star-inserted") %>%
  html_text(trim = TRUE)
  # html_nodes(xpath = "//td[text()='Actor/Organisation name']/following-sibling::td") %>%
  # html_text(trim = TRUE)


# 2) Actor ID/SRN
actIdSrn = tryCatch({
  sprintf("%s", manDtlInfo$srn)
}, error = function(e) {NA})


# 3) Applicable legislation
# 4) UDI-DU/EUDAMED DI/ Issuing entity
resDtlData$basicUdi$code
resDtlData

# 5) Risk class
# 6) Device name
# 7) UDI-DI code/Issuing entity
# 8) Status,Nomenclature code(s)
# 9) Name/Trade name(s)
# 10) Member state of the placing on the EU market of the device


resDtlData



resDtlDataL1 = resDtlData$deviceCertificateInfoList %>% 
  tibble::as.tibble()

resDtlDataL1

# https://ec.europa.eu/tools/eudamed/api/devices/basicUdiData/udiDiData/f03fd9b0-85dd-4c45-80e8-ac7bdc020d30?languageIso2Code=en


# 1) Actor/Organisation name
# //*[@id="cdk-accordion-child-1"]/div/div/dl[1]

# //*[@id="cdk-accordion-child-1"]/div/div/dl[1]/dd/div
urlDtl %>% 
  read_html() %>% 
  rvest::html_elements("#pma-details") %>%
  rvest::html_table()

urlDtl %>% 
  xml2::read_html() %>% 
  rvest::html_elements("#pma-details")
  # 
  # rvest::html_node(xpath = paste0('//*[@id="cdk-accordion-child-1"]/div/div/dl[1]/*')) %>% 
  # rvest::html_text() 
  # rvest::html_elements(xpath = paste0('//*[@id="cdk-accordion-child-1"]/div/div/dl[1]/dd/div'))
# #cdk-accordion-child-1 > div > div > dl:nth-child(1)
urlDtl %>% 
  xml2::read_html() %>% 
  rvest::html_element("#cdk-accordion-child-1")




# # Load the data
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "realtor-data.zip.csv"))
# 
# # bank_data = read.csv(fileInfo)
# bank_data = readr::read_csv(fileInfo)
# 
# # Descriptive Statistics
# summary(bank_data)
# 
# colnames(bank_data)
# 
# # bank_data$status %>% unique() %>% sort()
# # bank_data$state %>% unique() %>% sort()
# # bank_data$city %>% unique() %>% sort()
# 
# # Histogram of Income
# ggplot(bank_data, aes(x = Income)) +
#   geom_histogram(bins = 30, fill = "blue", color = "black") +
#   theme_minimal() +
#   labs(title = "Histogram of Income", x = "Income", y = "Frequency")
# 
# 
# set.seed(123) # For reproducibility
# split <- createDataPartition(bank_data$Personal.Loan, p = 0.8, list = FALSE)
# train_data <- bank_data[split,]
# test_data <- bank_data[-split,]
# 
# # Logistic Regression for Personal Loan prediction
# logistic_model <- glm(Personal.Loan ~ ., family = binomial(link = 'logit'), data = train_data)
# summary(logistic_model)
# 
# # Decision Tree
# tree_model <- rpart(Personal.Loan ~ ., method = "class", data = train_data)
# print(tree_model)
# 
# # XGBoost Classifier
# labels <- as.numeric(train_data$Personal.Loan) - 1
# data_matrix <- as.matrix(train_data[, -which(names(train_data) == "Personal.Loan")])
# dtrain <- xgb.DMatrix(data = data_matrix, label = labels)
# xgb_model <- xgboost(data = dtrain, nrounds = 100, objective = "binary:logistic")
# print(xgb_model)
# 
# # 모델 비교 평가
# logistic_predictions <- predict(logistic_model, newdata = test_data, type = "response")
# logistic_predictions <- ifelse(logistic_predictions > 0.5, 1, 0)
# confusion_logistic <- confusionMatrix(as.factor(logistic_predictions), as.factor(test_data$Personal.Loan))
# tree_predictions <- predict(tree_model, newdata = test_data, type = "class")
# confusion_tree <- confusionMatrix(tree_predictions, test_data$Personal.Loan)
# dtest <- xgb.DMatrix(data = as.matrix(test_data[, -which(names(test_data) == "Personal.Loan")]))
# xgb_predictions <- predict(xgb_model, dtest)
# xgb_predictions <- ifelse(xgb_predictions > 0.5, 1, 0)
# confusion_xgb <- confusionMatrix(as.factor(xgb_predictions), as.factor(test_data$Personal.Loan))
# 
# 
# logistic_accuracy <- confusion_logistic$overall['Accuracy']
# logistic_precision <- confusion_logistic$byClass['Precision']
# logistic_recall <- confusion_logistic$byClass['Recall']
# logistic_F1 <- 2 * (logistic_precision * logistic_recall) / (logistic_precision + logistic_recall)
# 
# tree_accuracy <- confusion_tree$overall['Accuracy']
# tree_precision <- confusion_tree$byClass['Precision']
# tree_recall <- confusion_tree$byClass['Recall']
# tree_F1 <- 2 * (tree_precision * tree_recall) / (tree_precision + tree_recall)
# 
# xgb_accuracy <- confusion_xgb$overall['Accuracy']
# xgb_precision <- confusion_xgb$byClass['Precision']
# xgb_recall <- confusion_xgb$byClass['Recall']
# xgb_F1 <- 2 * (xgb_precision * xgb_recall) / (xgb_precision + xgb_recall)
# 
# # Creating a summary table
# performance_table <- data.frame(
#   Model = c("Logistic Regression", "Decision Tree", "XGBoost"),
#   Accuracy = c(logistic_accuracy, tree_accuracy, xgb_accuracy),
#   Precision = c(logistic_precision, tree_precision, xgb_precision),
#   Recall = c(logistic_recall, tree_recall, xgb_recall),
#   F1_Score = c(logistic_F1, tree_F1, xgb_F1)
# )
# 
# print(performance_table)


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
