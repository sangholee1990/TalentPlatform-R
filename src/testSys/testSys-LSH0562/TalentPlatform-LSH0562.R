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
# R을 이용한 kaggle 작업환경 구축

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0562"

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
library(dplyr)
library(ggplot2)
library(psych)
library(forecast)

# ================================================
# 1. Data Import & Structure
# ================================================
# 파일 검색
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "zara.csv"))

data = read.csv(fileInfo, sep = ",", quote = "\"", stringsAsFactors = FALSE) %>%
  as.tibble()

head(data)
str(data)

colSums(is.na(data))
dim(data)

# Remove Unnecessary Variables
data = data[, -c(8:9, 11, 14)]
data2 = data

# Variable Type Transformation
data$Product.Position <- as.factor(data$Product.Position)
data$Product.Category <- as.factor(data$Product.Category)
data$brand <- as.factor(data$brand)
data$terms <- as.factor(data$terms)
data$section <- as.factor(data$section)
data$name <- as.factor(data$name)
data$currency <- as.factor(data$currency)

# Data Transform
# Promotion : No - > 0 , Yes -> 1 
data$Promotion <- ifelse(data$Promotion == 'No', 0, ifelse(data$Promotion == 'Yes', 1, 2))

# Seasonal : No - > 0 , Yes -> 1 
data$Seasonal <- ifelse(data$Seasonal == 'No', 0, ifelse(data$Seasonal == 'Yes', 1, 2))

# section : MAN - > 0 , WOMEN -> 1 , 
data$section <- ifelse(data$section == 'MAN', 0, ifelse(data$section == 'WOMEN', 1, 2))

head(data)
str(data)
summary(data)

# ================================================
# 2. Data Visualization
# ================================================
cols = c('Product.Position', 'Promotion', 'Seasonal', 'section')

for (i in cols) {
  # print(ggplot(data2, aes(x=data2[,i],fill = data2[,i])) + geom_bar() + ggtitle(paste(i,'count'))+ xlab(i) + theme_bw() + theme(legend.position = 'none'))

  print(
    ggplot(data2, aes_string(x = i, fill = i)) +
      geom_bar() +
      ggtitle(paste(i, 'count')) +
      xlab(i) +
      theme_bw() +
      theme(legend.position = 'none')
  )
}

for (i in cols) {
  # print(ggplot(data2, aes(x=data2[,i], y=price, fill= data2[,i])) + geom_boxplot() + xlab(i) +ggtitle(paste(i,'vs price'))+ theme_bw() + theme(legend.position = 'none') )
  print(
    ggplot(data2, aes_string(x = i, y = 'price', fill = i)) +
      geom_boxplot() +
      xlab(i) +
      ggtitle(paste(i, 'vs price')) +
      theme_bw() +
      theme(legend.position = 'none')
  )
}

for (i in cols) {
  # print(ggplot(data2, aes(x=data2[,i], y=Sales.Volume, fill= data2[,i])) + geom_boxplot() + xlab(i) +ggtitle(paste(i,'vs Sales.Volume'))+ theme_bw() + theme(legend.position = 'none') )
  print(
    ggplot(data2, aes_string(x = i, y = 'Sales.Volume', fill = i)) +
      geom_boxplot() +
      xlab(i) +
      ggtitle(paste(i, 'vs Sales.Volume')) +
      theme_bw() +
      theme(legend.position = 'none')
  )
}


ggplot(data2, aes(x = price)) +
  geom_density() +
  ggtitle('The density of commodity prices') +
  xlab('price')

ggplot(data2, aes(x = price, y = Sales.Volume)) + geom_smooth(se = F)


# The relationship between price and sales
# Average price by group
data2_promotion <- data2 %>%
  group_by(Promotion) %>%
  summarize(N = n(), avg_price = round(mean(price, na.rm = T)))
data2_promotion

ggplot(data2_promotion, aes(x = Promotion, y = avg_price, fill = Promotion)) +
  geom_col()

# The average price of products whose promotions are 'Yes' is higher.
data2_Seasonal <- data2 %>%
  group_by(Seasonal) %>%
  summarize(N = n(), avg_price = round(mean(price, na.rm = T)))
data2_Seasonal

ggplot(data2_Seasonal, aes(x = Seasonal, y = avg_price, fill = Seasonal)) + 
  geom_col()

# There's not much difference in the average price
# Average calculation according to the number of people allocated
data2_section <- data2 %>%
  group_by(section) %>%
  summarize(N = n(), avg_price = round(mean(price, na.rm = T)))
data2_section

# If pick 30 people and average them
data2_section30 <- data2 %>%
  group_by(section) %>%
  sample_n(size = 30) %>%
  summarize(N = n(), avg_price = round(mean(price, na.rm = T)))
data2_section30

ggplot(data2_section30, aes(x = section, y = avg_price, fill = section)) + 
  geom_col()


# The average price of men product is higher.
data2_promotion2 <- data2 %>%
  group_by(Promotion) %>%
  summarize(N = n(), avg_Sales.Volume = round(mean(Sales.Volume, na.rm = T)))
data2_promotion2

ggplot(data2_promotion2, aes(x = Promotion, y = avg_Sales.Volume, fill = Promotion)) + 
  geom_col()

# There is not much difference in the average sales volume
data2_Seasonal2 <- data2 %>%
  group_by(Seasonal) %>%
  summarize(N = n(), avg_Sales.Volume = round(mean(Sales.Volume, na.rm = T)))
data2_Seasonal2

ggplot(data2_Seasonal2, aes(x = Seasonal, y = avg_Sales.Volume, fill = Seasonal)) +
  geom_col()

# There is not much difference in the average sales volume
# Average calculation according to the number of people allocated
data2_section2 <- data2 %>%
  group_by(section) %>%
  summarize(N = n(), avg_Sales.Volume = round(mean(Sales.Volume, na.rm = T)))
data2_section2

ggplot(data2_section2, aes(x = section, y = avg_Sales.Volume, fill = section)) + 
  geom_col()

# ================================================
# 3. Price & Sales.Volume prediction
# ================================================
md_lr <- lm(price ~ Promotion + Seasonal + section + Sales.Volume, data = data)
summary(md_lr)

step(md_lr, direction = "backward")

# Select variable
md_lr <- lm(price ~ Promotion + section, data = data)
summary(md_lr)

# Regression equation: 86.014 + Promotion * 12.298 + section * -20.819
plot(md_lr)

pred <- 86.014 +
  data$Promotion * 12.298 +
  data$section * -20.819
accuracy(data$price, pred)

md_lr2 <- lm(Sales.Volume ~ price + Promotion + Seasonal + section, data = data)
summary(md_lr2)

# The regression model that predicts Sales.Volume is not statistically significant