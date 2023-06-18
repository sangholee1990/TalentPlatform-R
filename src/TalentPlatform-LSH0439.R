# ===============================================================================================
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
# R을 이용한 품목별로 시계열 예측

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0439"

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
library(dplyr)
library(tidyverse)
library(ggpubr)
library(data.table)
library(forecast)
library(fs)

data_2019 = fread("./data_2019.csv",encoding = "UTF-8",sep = ",")
data_2020 = fread("./data_2020.csv",encoding = "UTF-8",sep = "\t",skip = 1)
data_2021 = fread("./data_2021.csv",encoding = "UTF-8",sep = "\t",skip = 1)
data_2022 = fread("./data_2022.csv",encoding = "UTF-8",sep = "\t",skip = 1)
data_2023 = fread("./data_2023.csv",encoding = "UTF-8",sep = ",",skip = 1)

data_2020 = data_2020 %>% dplyr::select(V1,V2)
colnames(data_2020) = c("품명","원화금액")

data_2021 = data_2021 %>% dplyr::select(V1,V2)
colnames(data_2021) = c("품명","원화금액")

data_2022 = data_2022 %>% dplyr::select(V1,V2)
colnames(data_2022) = c("품명","원화금액")

data_2023 = data_2023 %>% dplyr::select(V1,V2)
colnames(data_2023) = c("품명","원화금액")

######
data_2019$품목[data_2019$품명 == "A"] <- "IC소자"
data_2019$품목[data_2019$품명 == "B"] <- "반도체"
data_2019$품목[data_2019$품명 == "C"] <- "공구류"
data_2019$품목[data_2019$품명 == "D"] <- "조립비용"
data_2019$품목[data_2019$품명 == "F"] <- "일반자재"

data_2020$품목[data_2020$품명 == "A"] <- "IC소자"
data_2020$품목[data_2020$품명 == "B"] <- "반도체"
data_2020$품목[data_2020$품명 == "C"] <- "공구류"
data_2020$품목[data_2020$품명 == "D"] <- "조립비용"
data_2020$품목[data_2020$품명 == "F"] <- "일반자재"

data_2021$품목[data_2021$품명 == "A"] <- "IC소자"
data_2021$품목[data_2021$품명 == "B"] <- "반도체"
data_2021$품목[data_2021$품명 == "C"] <- "공구류"
data_2021$품목[data_2021$품명 == "D"] <- "조립비용"
data_2021$품목[data_2021$품명 == "F"] <- "일반자재"

data_2022$품목[data_2022$품명 == "A"] <- "IC소자"
data_2022$품목[data_2022$품명 == "B"] <- "반도체"
data_2022$품목[data_2022$품명 == "C"] <- "공구류"
data_2022$품목[data_2022$품명 == "D"] <- "조립비용"
data_2022$품목[data_2022$품명 == "F"] <- "일반자재"

data_2023$품목[data_2023$품명 == "A"] <- "IC소자"
data_2023$품목[data_2023$품명 == "B"] <- "반도체"
data_2023$품목[data_2023$품명 == "C"] <- "공구류"
data_2023$품목[data_2023$품명 == "D"] <- "조립비용"
data_2023$품목[data_2023$품명 == "F"] <- "일반자재"

data_2019$원화금액 = gsub(",","",data_2019$원화금액)
data_2020$원화금액 = gsub(",","",data_2020$원화금액)
data_2021$원화금액 = gsub(",","",data_2021$원화금액)
data_2022$원화금액 = gsub(",","",data_2022$원화금액)
data_2023$원화금액 = gsub(",","",data_2023$원화금액)

data_2019$year = 2019
data_2020$year = 2020
data_2021$year = 2021
data_2022$year = 2022
data_2023$year = 2023

#########################
data = rbind(data_2019,data_2020,data_2021,data_2022,data_2023)

data = as_tibble(data)
data$원화금액 = as.numeric(data$원화금액)

plot(data$year,data$원화금액)


# 시계열 회귀분석
dataL1 = data %>%
  dplyr::group_by(품목, year) %>%
  dplyr::summarise(
    val = mean(원화금액, na.rm= TRUE)
  )

typeList = dataL1$품목 %>% unique() %>% sort()
for (typeInfo in typeList) {
  cat(sprintf("[CHECK] typeInfo : %s", typeInfo), "\n")

  dataL2 = dataL1 %>%
    dplyr::filter(품목 == typeInfo)

  # 회귀분석
  lmFit = lm(val ~ year, data = dataL2)

  dataL2$prd = predict(lmFit, newdata = dataL2)


  mainTitle = sprintf("%s에 대한 실측 및 예측 산점도", typeInfo)
  saveImg = sprintf("./%s.png", mainTitle)

  makePlot = ggpubr::ggscatter(
    dataL2, x = "prd", y = "val", color = "black"
    , add = "reg.line", conf.int = TRUE
    , add.params = list(color = "blue", fill = "lightblue")
  ) +
  theme_bw() +
    ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 1.0, size = 4) +
    ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.9, size = 4) +
    labs(
      title = NULL
      , x = "예측"
      , y = "실측"
      , subtitle = mainTitle
    ) +
    theme(text = element_text(size = 16))

  ggsave(makePlot, filename = saveImg, width = 6, height = 6, dpi = 600)

  # 시계열 분석
  tsData = ts(dataL2$val, start=c(2019), end=c(2023), frequency = 1)
  bestModel = forecast::auto.arima(tsData, trace = TRUE)

  # 최종선택한 모형을 사용하여 구한 예측값(2년 이후)을 나타내는 그래프
  akimaData = forecast::forecast(bestModel, h = 1)

  # 성능 비교
  # accuracy(akimaData)

  mainTitle = sprintf("%s에 대한 실측 및 예측 시계열", typeInfo)
  saveImg = sprintf("./%s.png", mainTitle)
  dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

  makePlot = autoplot(akimaData) +
    geom_smooth(method = 'lm', se = TRUE) +
    ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 1.0, size = 5) +
    ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.9, size = 5) +
    labs(
      x = "연도"
      , y = "예측"
      , color = NULL
      , fill = NULL
      , subtitle = mainTitle
    ) +
    theme(
      text = element_text(size = 16)
      , legend.position = "bottom"
    )
  ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
}