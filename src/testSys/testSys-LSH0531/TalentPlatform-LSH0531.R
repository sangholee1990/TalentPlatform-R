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
# R을 이용한 특정 장소별로 많이 일어나는 범죄 유형의 특성

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0528"

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
library(sysfonts)
library(showtext)
library(tidyverse)
library(extrafont)

# 국문 폰트 설정
if (Sys.info()["sysname"] == "Windows") {
  sysfonts::font.add(family = "malgun", regular = "C:/Windows/Fonts/malgun.ttf")
  showtext::showtext_opts(dpi = 100)
  showtext::showtext.auto()
}

## 주제: 특정 장소별로 많이 일어나는 범죄 유형의 특성
## 사용 데이터: 공공데이터 포털에서 ‘경찰청_범죄 발생 장소별 통계’ 22,21,20년도 3개년치 데이터 사용

## 사전 준비
# - 데이터 읽기 및 병합
# - 데이터 전처리 (wide -> long)
dataL1 = tibble::tibble()

# 데이터 읽기 및 병합
fileList = Sys.glob(file.path("LSH0531/*.csv"))
for (fileInfo in fileList) {
  data = readr::read_csv(fileInfo, locale = locale("ko", encoding = "EUC-KR"))
  dataL1 = dplyr:::bind_rows(dataL1, data)
}

# 데이터 전처리
dataL2 = dataL1 %>% 
  tidyr::pivot_longer(cols = c("아파트_연립다세대":"기타"), names_to = "name", values_to = "val")

head(dataL2)

## 분석
### 1. 데이터에서 다루는 범죄 유형 요약
# - 범죄 대유형에 따른 발생 빈도를 막대그래프 시각화함
# - 그 결과 주로 지능/강력/폭력 범죄는 750 이상으로 분포하며 그 외 풍속/교통 범죄 등으로 나타냄
dataL3 = dataL2 %>%
  dplyr::count(범죄대분류) %>%
  dplyr::arrange(desc(n))

head(dataL3)

ggplot(dataL3, aes(x = reorder(범죄대분류, -n), y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25)) +
  labs(x = "범죄 유형", y = "발생 빈도", title = "범죄 유형에 따른 발생 빈도")

### 2. 데이터에서 나타나는 범죄발생 장소 요약
# - 범죄 장소에 따른 발생 빈도를 막대그래프 시각화함
# - 특히 1위 노상 및 2위 기타의 경우 각각 3,313,494 및 2,136,060로서 높은 비율로 차지한 반면에 대부분 낮은 분포를 나타냄
dataL3 = dataL2 %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(
    n = sum(val, na.rm = TRUE)
  ) %>% 
  dplyr::arrange(desc(n))

head(dataL3)

ggplot(dataL3, aes(x = reorder(name, -n), y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25)) +
  labs(x = "범죄 장소", y = "발생 빈도", title = "범죄 장소별 발생 빈도")

### 3. 장소별로 많이 일어나는 범죄의 유형
# - 범죄 장소에 따른 자주 발생되는 범죄 유형을 막대그래프 시각화함
# - 항시 개방된 노상은 교통범죄에서 자주 일어나고 최근 보이스피싱 관련 기타 및 사무실은 지능범죄가 발생함
# - 또한 최근에 아파트/주택 간의 층간 소음으로 인해 폭력범죄가 주로 나타남
dataL3 = dataL2 %>% 
  dplyr::group_by(name, 범죄대분류) %>% 
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  ) %>% 
  dplyr::ungroup() %>%
  dplyr::group_by(name) %>%
  dplyr::arrange(desc(sumVal))

head(dataL3)

ggplot(dataL3, aes(x = reorder(name, -sumVal), y = sumVal, fill = 범죄대분류)) +
  geom_bar(stat = "identity") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25)
    # , text = element_text(size = 14)
  ) +
  labs(x = "범죄 장소", y = "발생 빈도", title = "장소별로 많이 일어나는 범죄의 유형")

### 4. 범죄 유형(강력, 폭력, 지능 등)별로 가장 발생횟수가 빈번한 장소
# - 범죄 유형에 따른 자주 발생하는 횟수를 막대그래프 시각화함
# - 상위 5위 범죄의 경우 교통, 지능, 폭력, 기타, 절도로 나타나고 그 외 기타 범죄 (특별경제범죄, 풍속범죄 등)들은 다소 낮은 빈도로 분포함
# - 특히 상위 범죄 관련하여 원인은 앞서 설명한 "장소별로 많이 일어나는 범죄의 유형"과 동일함
dataL3 = dataL2 %>%
  dplyr::group_by(범죄대분류, name) %>%
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(범죄대분류) %>%
  dplyr::top_n(n = 1) %>% 
  dplyr::arrange(desc(sumVal))

head(dataL3)

ggplot(dataL3, aes(x = reorder(범죄대분류, -sumVal), y = sumVal, fill = 범죄대분류)) +
  geom_bar(stat = "identity") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25)
    # , text = element_text(size = 14)
  ) +
  labs(x = "범죄 유형", y = "발생 빈도", title = "범죄 유형별로 가장 발생횟수가 빈번한 장소")