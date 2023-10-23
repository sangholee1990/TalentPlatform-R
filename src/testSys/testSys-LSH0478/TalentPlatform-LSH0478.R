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
# R을 이용한 지점별 평균-누적 강수량 내삽 및 등우선도 시각화

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0478"

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
    , "mapPath" = contextPath
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

# 입력 데이터
data = data.frame(
  X1 = c(173, 181, 192, 173, 169, 160, 170, 167, 173, 170, 173, 166, 162, 154, 154, 175, 164, 152, 151, 181)
  , X2 = c(171, 190, 195, 157, 160, 167, 167, 174, 173, 173, 198, 177, 171, 177, 179, 185, 199, 153, 198, 193)
  , X3 = c(187, 182, 187, 150, 167, 170, 174, 173, 178, 173, 162, 174, 154, 179, 164, 163, 156, 163, 167, 151)
  , X4 = c(151, 171, 187, 154, 170, 197, 173, 179, 173, 170, 171, 161, 173, 170, 171, 156, 184, 163, 181, 166)
  , X5 = c(188, 187, 172, 168, 197, 159, 179, 170, 173, 173, 173, 174, 154, 167, 182, 151, 151, 156, 180, 180)
  , X6 = c(181, 177, 164, 174, 174, 174, 167, 167, 173, 198, 198, 177, 177, 174, 174, 184, 198, 167, 199, 184)
  , X7 = c(182, 162, 164, 171, 173, 161, 174, 173, 173, 166, 166, 162, 162, 154, 154, 151, 151, 154, 151, 182)
  , X8 = c(157, 172, 189, 182, 200, 173, 170, 178, 173, 177, 177, 171, 171, 154, 154, 198, 198, 179, 198, 180)
  , X9 = c(162, 188, 179, 200, 181, 179, 167, 173, 178, 173, 173, 154, 154, 175, 175, 152, 152, 175, 152, 181)
  , X10 = c(169, 200, 182, 181, 169, 170, 170, 173, 173, 173, 173, 177, 177, 185, 185, 199, 199, 153, 199, 184)
  )

alpha = 0.20
confLevel = 1.0 - alpha
n = nrow(data)
degree = n - 1
tVal = qt(1.0 - alpha/2, degree)

# 평균, 표준편차
# meanVal = colMeans(data)

meanVal = (colMeans(data) * 0) + 174
stdVal = apply(data, 2, sd)

# 표준오차
error = tVal * stdVal / sqrt(n)

dataL1 = data.frame(
  name = names(meanVal)
  , mean = meanVal
  , std = stdVal
  , error = error
  , ciLower = meanVal - error
  , ciUpper = meanVal + error
)


nameList = dataL1$name %>% unique()

# nameInfo = nameList[1]
for (nameInfo in nameList) {

  dataL2 = dataL1 %>%
    dplyr::filter(name == nameInfo)

  cat(sprintf("[CHECK] nameInfo : %s", nameInfo), "\n")
  cat(sprintf("주어진 평균 174cm에 대한 98%% 신뢰 구간: %0.2f - %0.2f cm", dataL2$ciLower, dataL2$ciUpper), "\n")

  xran = seq(dataL2$mean - 4*dataL2$std, dataL2$mean + 4*dataL2$std, length.out = 1000)

  dataL3 = data.frame(
    x = xran
    , y = dnorm(xran, mean = dataL2$mean, sd = dataL2$std)
  )

  makePlot = ggplot() +
    geom_line(data = dataL3, aes(x=x, y=y), color = "black") +
    geom_vline(data = dataL2, aes(xintercept = mean), color = "blue", size = 1) +
    geom_vline(data = dataL2, aes(xintercept = ciLower), color = "green", size = 0.5) +
    geom_vline(data = dataL2, aes(xintercept = ciUpper), color = "green", size = 0.5) +
    geom_ribbon(data = dataL3, aes(x = x, ymin = 0, ymax = ifelse(x > dataL2$ciLower & x < dataL2$ciUpper, y, 0)), fill = "lightblue", alpha = 0.3) +
    geom_ribbon(data = dataL3, aes(x = x, ymin = 0, ymax = ifelse(x < dataL2$ciLower | x > dataL2$ciUpper, y, 0)), fill = "lightgreen", alpha = 0.3) +
    theme_minimal() +
    labs(title = sprintf("Normal Distribution (name = %s)", dataL2$name), x = "", y = "Density")

  print(makePlot)
}