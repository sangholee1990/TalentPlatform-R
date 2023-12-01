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
# R을 이용한 시간대 및 요일별 범죄대분류 발생횟수 시각화

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0513"

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
library(ggpubr)
library(webr)
library(openxlsx)
library(lubridate)
library(fs)
library(modelr)

# 파일 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "경찰청_범죄+발생+시간대+및+요일_20191231.csv"))

data = readr::read_csv(fileInfo, locale = readr::locale("ko", encoding = "EUC-KR"))
# colnames(data)

# ================================================
# 시간대 범죄대분류 발생횟수 시각화
# ================================================
# 다양한 범죄대분류 및 3시간 간격에 따라 총 발생횟수를 합산/시각화 수행
# 발생 빈도가 높은 교통범죄의 경우 00~23시까지 점차 증가하며 특히 21시 이후 높은 빈도로 발생됨
#  특히 야간에 교통량이 많음에도 불구하고 운전자 피로도 증가 및 음주운전에 쉽게 노출되기 때문임
# 대부분 강력/교통/보건/폭력/풍속 범죄에서는 21시 이후에 높게 발생되는 반면에 기타/노동/마약/병력/선거/안보/절도/지능/특별경제/환경 범죄의 경우 이와 달리 평일 (09~18)에 주로  발생됨
dataL1 = data %>% 
  tidyr::pivot_longer(cols = c("0시00분-02시59분":"21시00분-23시59분"), names_to = "name", values_to = "val") %>% 
  dplyr::group_by(범죄대분류, name) %>% 
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  ) %>% 
  dplyr::mutate(
    key = dplyr::case_when(
      stringr::str_detect(name, regex("0시00분-02시59분")) ~ "00:00-02:59"
      , stringr::str_detect(name, regex("03시00분-05시59분")) ~ "03:00-05:59"
      , stringr::str_detect(name, regex("06시00분-08시59분")) ~ "06:00-08:59"
      , stringr::str_detect(name, regex("09시00분-11시59분")) ~ "09:00-11:59"
      , stringr::str_detect(name, regex("12시00분-14시59분")) ~ "12:00-14:59"
      , stringr::str_detect(name, regex("15시00분-17시59분")) ~ "15:00-17:59"
      , stringr::str_detect(name, regex("18시00분-20시59분")) ~ "18:00-20:59"
      , stringr::str_detect(name, regex("21시00분-23시59분")) ~ "21:00-23:59"
    )
  )
    
# 정렬
dataL1$key = forcats::fct_relevel(dataL1$key, c("00:00-02:59",  "03:00-05:59", "06:00-08:59", "09:00-11:59", "12:00-14:59", "15:00-17:59", "18:00-20:59", "21:00-23:59"))

mainTitle = sprintf("%s", "시간대 범죄대분류 발생횟수 시각화")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL1, aes(x = key, y = sumVal, color = 범죄대분류, group = 범죄대분류)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ 범죄대분류, scales = "free_y") +
  labs(x = NULL, y = "발생 횟수", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  theme(
    text = element_text(size = 14)
    , axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ================================================
# 요일 범죄대분류 발생횟수 시각화
# ================================================
# 다양한 범죄대분류 및 요일에 따라 총 발생횟수를 합산/시각화 수행
# 발생 빈도가 높은 교통범죄의 경우 월~토까지 점차 증가하다가 일요일 낮게 발생됨
# 이는 일요일은 다른 날에 비해 촐퇴근으로 인한 교통량 감소로 판단됨
# 대부분 기타/노동/병력/보건/지능/특별경제/풍속/환경 범죄에서는 평일보다 주말에 낮게 발생되는 반면에 강력/안보/폭력/절도 범죄의 경우 정반대로 주말에 높게 발생됨
dataL1 = data %>% 
  tidyr::pivot_longer(cols = c("일":"토"), names_to = "key", values_to = "val") %>% 
  dplyr::group_by(범죄대분류, key) %>% 
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  )

# 정렬
dataL1$key = forcats::fct_relevel(dataL1$key, c("월", "화", "수", "목", "금", "토", "일"))

mainTitle = sprintf("%s", "요일 범죄대분류 발생횟수 시각화")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL1, aes(x = key, y = sumVal, color = 범죄대분류, group = 범죄대분류)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ 범죄대분류, scales = "free_y") +
  labs(x = NULL, y = "발생 횟수", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  theme(
    text = element_text(size = 14)
  ) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
