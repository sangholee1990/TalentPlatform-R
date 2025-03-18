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
# R을 이용한 석사학위 데이터 분석 및 시각화 (토양온도 및 토양수분 변화에 따른 토양호흡 예측 모델)

# 1-1. 토양온도 및 수분과의 상관관계로부터 추정한 비선형회귀식 기반의 토양호흡 결측값 보간
# 토양호흡값에 대해 토양온도 및 토양수분과의 상관관계로부터 추정한 비선형회귀식을 이용하여 토양호흡 결측값을 보간하는 것입니다.
# 우선 토양호흡과 토양온도 간의 회귀식을 이용하여 토양온도의 영향을 최소화한 토양호흡 데이터 Rs_norm(실제 관측한 토양호흡/토양온도로부터 추정한 토양호흡)을 계산하고, 계산된 Rs_norm 데이터를 이용하여 토양수분과 Rs_norm 간의 비선형회귀식 도출 후, RMSE 값을 기준으로 최적의 토양수분-Rs_norm 회귀식을 선정합니다.
# 최종적으로 최적의 토양온도-토양호흡 회귀식과 토양수분-Rs_norm 회귀식을 종합하여 토양온도 및 토양수분 변화에 따른 토양호흡 예측 모델 도출을 문의드립니다.

# 1-2. 그래프 상의 데이터 아래 면적 적분해서 총 토양호흡량 계산(그래프는 필요없고, 값만 필요)

# 2. 토양온도 및 기온의 기존 그림 업데이트
# 이게 우선 2번에 업데이트가 필요한 그래프인데, 기존 코드에 이 그래프를 만드는 코드가 있을겁니다. figure4 R

# 3. 붙임의 자료에서 ln(flux) 데이터로 기존 그래프 업데이트(강우 차단 조건으로 나눈 그래프 1개, 6개 조건으로 나눈 그래프 1개)
# 이건 3번 요청사항에 대한 그래프인데, y축의 토양호흡 데이터를 ln(flux) 값으로, 그리고 토양온도를 별도의 엑셀 파일로 드린 데이터로 사용부탁드립니다.
# 그리고 slope는 y = mx + b형태로 해서 기울기와 절편도 포함 부탁드려요.
# R2 값과 p-value도 부탁드립니다.
# 기본적으로 강우 차단 조건(C: Control, L: Light drought, S: Severe drought)와 온도 조건(N: Non-warming, W: Warming)의 조합으로 6개의 조건이 있는데요. CN, CW, LN, LW, SN, SW 이런 식이고, 반복수는 4개씩입니다. 원래는 수종(K/M)까지 12개의 조건으로 실험을 진행했으나, 수종이 토양호흡에 미치는 영향이 없는 것으로 드러나 수종을 무시하고 6개의 조건으로 데이터를 정리하고 있고요.
# 그리고 변수는 토양호흡과 조건별 토양온도, 기온, 토양수분이 있습니다.

# 4. 토양온도와 MT 자료, 토양수분 자료를 활용하여 기존 그래프 업데이트
# 끝으로 이게 4번에 대한 그래프이고, R코드에서는 figure 9에 해당합니다.
# 이 기존의 그래프는 위에서부터 토양호흡, 토양온도, 토양수분함량을 시간에 따라 나타낸 것인데, 우선 토양호흡은 ln(flux)로 대체, 토양온도와 수분은 엑셀 데이터를 사용해주시면 됩니다. 그리고 기존 그래프는 토양온도와 수분 데이터가 토양호흡을 측정했던 시기만 찍혀있어서 중간중간 값이 직선으로 되어있는데요. 토양호흡은 2023년 3월부터 11월까지 월 1-2회 주기로 11회 측정했지만, 토양온도 및 기온은 5분 단위, 수분 데이터는 30초 단위로 측정을 해서 데이터가 많습니다. 토양수분은 변동이 크지 않아 일단위로 해주셔도 되고, 토양온도 및 기온은 5분이나 1시간 등 정해진 것은 없지만 가동성이 떨어지지 않게 변화가 잘 보이도록 부탁드립니다.

# 이게 전체 내용인데, 기존 자료를 업데이트 하는 것은 제가 코드나 그래프를 드리면 업데이트를 해주셔도 되고, 새로 만들어 주셔도 됩니다. 편하신 방법대로..

# 이것도 엑셀의 토양수분 데이터로 업데이트 부탁드립니다;; y축 이름은 "Precipitation (mm/d)"로 해서요..ㅎㅎ

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0604"

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
library(lubridate)
library(ggplot2)
library(ggpattern)
library(patchwork)
library(openxlsx)


# ================================================
# 1-2. 그래프 상의 데이터 아래 면적 적분해서 총 토양호흡량 계산(그래프는 필요없고, 값만 필요)
# ================================================


# ================================================
# 2. 토양온도 및 기온의 기존 그림 업데이트
# ================================================
##### Figure.2,3,4 토양온도와 대기온도 아웃라이어 제거 후 비교 #####
# temp_compare <- read.csv("soil_vs_air_temp_raw.csv")

# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "soil_vs_air_temp_raw.csv"))
# temp_compare <- read.csv(fileInfo)

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "20250130_2023년3월11월_기온및토양온도_통합V7문의.xlsx"))
orgData = openxlsx::read.xlsx(fileInfo)
temp_compare = orgData %>% 
  dplyr::select(timestamp,Soil_CW1,Soil_CN1,Air_CW1,Air_CN1,Soil_CW2,Soil_CN2,Air_CW2,Air_CN2,Soil_CW3,Soil_CN3,Air_CW3,Air_CN3,Soil_LW1,Soil_LN1,Air_LW1,Air_LN1,Soil_LW2,Soil_LN2,Air_LW2,Air_LN2,Soil_LW3,Soil_LN3,Air_LW3,Air_LN3,Soil_SW1,Soil_SN1,Air_SW1,Air_SN1,Soil_SW2,Soil_SN2,Air_SW2,Air_SN2,Soil_SW3,Soil_SN3,Air_SW3,Air_SN3)

# 시간 문자열 정리
temp_compare$timestamp <- gsub("오전 ", "AM ", temp_compare$timestamp)
temp_compare$timestamp <- gsub("오후 ", "PM ", temp_compare$timestamp)
temp_compare$timestamp <- gsub("시", ":", temp_compare$timestamp)
temp_compare$timestamp <- gsub("분", ":", temp_compare$timestamp)
temp_compare$timestamp <- gsub("초", "", temp_compare$timestamp)

# 공백 정리
temp_compare$timestamp <- gsub("\\s+", " ", temp_compare$timestamp)  # 연속된 공백 제거
temp_compare$timestamp <- gsub(": ", ":", temp_compare$timestamp)   # 잘못된 공백 제거

# 문자열을 datetime 형식으로 변환
temp_compare$timestamp <- parse_date_time(temp_compare$timestamp, orders = "m. d. y p I:M:S ", quiet = TRUE)

# 데이터 불러오기 (예제에서는 temp_compare 데이터 프레임을 가정)
# -20도 이하의 값은 NA로 변환
temp_compare[temp_compare < -20] <- NA

# Soil 데이터만 선택
soil_data <- temp_compare %>% 
  select(timestamp, starts_with("Soil_")) %>% 
  pivot_longer(cols = -timestamp, names_to = "Treatment", values_to = "Temperature")

# Treatment 그룹화 (C, L, S 조합과 N, W 조합을 묶어 6개의 그룹 생성)
soil_data <- soil_data %>%
  mutate(Treatment = case_when(
    grepl("CW", Treatment) ~ "CW",
    grepl("CN", Treatment) ~ "CN",
    grepl("LW", Treatment) ~ "LW",
    grepl("LN", Treatment) ~ "LN",
    grepl("SW", Treatment) ~ "SW",
    grepl("SN", Treatment) ~ "SN",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Treatment))

# 월별 데이터 추가
soil_data <- soil_data %>%
  mutate(Month = format(timestamp, "%m"))

soil_data <- soil_data %>% filter(!is.na(Temperature))

# 아웃라이어 바운드 계산 (평균 ± 3SD 범위)
outlier_bounds <- soil_data %>%
  group_by(Month, Treatment) %>%
  summarise(
    Mean = mean(Temperature, na.rm = TRUE),
    SD = sd(Temperature, na.rm = TRUE),
    Lower_Bound = Mean - 3 * SD,
    Upper_Bound = Mean + 3 * SD,
    .groups = "drop"
  )

# 아웃라이어 제거된 데이터
soil_temp_filtered <- soil_data %>%
  left_join(outlier_bounds, by = c("Month", "Treatment")) %>%
  filter(!is.na(Lower_Bound) & !is.na(Upper_Bound) & Temperature >= Lower_Bound & Temperature <= Upper_Bound)

# Air 데이터만 선택
air_data <- temp_compare %>% 
  select(timestamp, starts_with("Air_")) %>% 
  pivot_longer(cols = -timestamp, names_to = "Treatment", values_to = "Temperature")

# Treatment 그룹화 (C, L, S 조합과 N, W 조합을 묶어 6개의 그룹 생성)
air_data <- air_data %>%
  mutate(Treatment = case_when(
    grepl("CW", Treatment) ~ "CW",
    grepl("CN", Treatment) ~ "CN",
    grepl("LW", Treatment) ~ "LW",
    grepl("LN", Treatment) ~ "LN",
    grepl("SW", Treatment) ~ "SW",
    grepl("SN", Treatment) ~ "SN",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Treatment))

# 월별 데이터 추가
air_data <- air_data %>%
  mutate(Month = format(timestamp, "%m"))

air_data <- air_data %>% filter(!is.na(Temperature))

# 원본 데이터에서 분포와 아웃라이어 바운드 선을 표현한 바이올린 플롯
mainTitle = sprintf("%s", "2.soil_temp_outliers")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(soil_data, aes(x = Month, y = Temperature, fill = Treatment)) +
  geom_violin(alpha = 0.5, scale = "width") +  # 온도의 분포를 보여줌
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.8) +  # 중간 요약 정보 제공
  geom_line(data = outlier_bounds, aes(x = Month, y = Lower_Bound, group = Treatment), linetype = "dashed", color = "black") +
  geom_line(data = outlier_bounds, aes(x = Month, y = Upper_Bound, group = Treatment), linetype = "dashed", color = "black") +
  labs(title = "Soil temperature, full data",
       x = "Month",
       y = "Temperature (°C)") +
  scale_fill_manual(values=c("blue", "red", "blue", "red", "blue", "red")) +
  facet_grid(.~Treatment) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  ggsave(saveImg, units = "cm", height = 15, width = 40)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# 필터링된 데이터에서 분포와 아웃라이어 바운드 선을 표현한 바이올린 플롯
mainTitle = sprintf("%s", "2.soil_outlier_filtered")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(soil_temp_filtered, aes(x = Month, y = Temperature, fill = Treatment)) +
  geom_violin(alpha = 0.5, scale = "width") +  # 온도의 분포를 보여줌
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.8) +  # 중간 요약 정보 제공
  geom_line(data = outlier_bounds, aes(x = Month, y = Lower_Bound, group = Treatment), linetype = "dashed", color = "black") +
  geom_line(data = outlier_bounds, aes(x = Month, y = Upper_Bound, group = Treatment), linetype = "dashed", color = "black") +
  labs(title = "Soil temperature, outlier removed",
       x = "Month",
       y = "Temperature (°C)") +
  scale_fill_manual(values=c("blue", "red", "blue", "red", "blue", "red")) +
  facet_grid(.~Treatment) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  ggsave(saveImg, units = "cm", height = 12, width = 40)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# 원본 데이터에서 분포와 아웃라이어 바운드 선을 표현한 바이올린 플롯
mainTitle = sprintf("%s", "3.air_outlier")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(air_data, aes(x = Month, y = Temperature, fill = Treatment)) +
  geom_violin(alpha = 0.5, scale = "width") +  # 온도의 분포를 보여줌
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.8) +  # 중간 요약 정보 제공
  geom_line(data = outlier_bounds, aes(x = Month, y = Lower_Bound, group = Treatment), linetype = "dashed", color = "black") +
  geom_line(data = outlier_bounds, aes(x = Month, y = Upper_Bound, group = Treatment), linetype = "dashed", color = "black") +
  labs(title = "Air temperature, full data",
       x = "Month",
       y = "Temperature (°C)") +
  scale_fill_manual(values=c("blue", "red", "blue", "red", "blue", "red")) +
  facet_grid(.~Treatment) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  ggsave(saveImg, units = "cm", height = 12, width = 40)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

soil_temp_filtered$group <- "Soil"
air_data$group <- "Air"

temp_compare <- rbind(soil_temp_filtered[,c(1,2,3,4,9)], air_data)

temp_summary <- temp_compare %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(group, date, Treatment) %>%
  summarise(mean = mean(Temperature, na.rm = T))

# 4.soil_vs_air_temp
mainTitle = sprintf("%s", "4.soil_vs_air_temp")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(temp_summary) +
  geom_line(aes(x = date, y = mean, color = group), linewidth = 0.5, alpha = 0.6) + # 연한 색으로 설정
  geom_smooth(aes(x = date, y = mean, color = group, fill = group), 
              method = "loess", se = TRUE, linewidth = 1.2, alpha = 0.3) + # 신뢰 구간 투명도 조정
  labs(
    x = "Month",
    y = "Temperature (°C)",
    color = "Group",
    fill = "Group"
  ) +
  scale_color_manual(values = c("#56A4E9", "#D69F00")) + # 추세선 색상
  scale_fill_manual(values = c("#56A4E9", "#D69F00")) + # 신뢰 구간 색상, 선과 일치
  scale_x_date(date_breaks = "1 month", date_labels = "%m") + # X축 형식
  facet_grid(. ~ Treatment) +
  theme_bw() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  ggsave(saveImg, units = "cm", height = 12, width = 40)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ================================================
# 3. 붙임의 자료에서 ln(flux) 데이터로 기존 그래프 업데이트(강우 차단 조건으로 나눈 그래프 1개, 6개 조건으로 나눈 그래프 1개)
# ================================================
##### Figure.5  토양온도와 토양호흡 비교 #####
# mt <- read.csv("mt.csv")
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "mt.csv"))
# mt <- read.csv(fileInfo)

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "20250130_2023년_마스터테이블_문의.xlsx"))
orgData = openxlsx::read.xlsx(fileInfo)
mt = orgData

mt$Date <- as.Date(mt$Date, origin = "1899-12-30")
mt$season <- factor(mt$season, levels = c("sp", "s", "f"))
mt$Heating[mt$Heating=="NW"] <- "N"
mt$Treatment <- paste0(mt$PPT_control, mt$Heating)

# NA 값 제거 (temp와 flux가 NA가 아닌 값만 사용)
mt_outlier <- mt %>%
  filter(season %in% c("sp", "s", "f"), !is.na(temp), !is.na(flux)) %>%
  select(Date, temp, flux, season, PPT_control, Heating, Treatment)  %>%
  group_by(Date) %>%
  mutate(
    flux_lower = quantile(flux, 0.25, na.rm = TRUE) -
      1.5 * (quantile(flux, 0.75, na.rm = TRUE) - quantile(flux, 0.25, na.rm = TRUE)),
    flux_upper = quantile(flux, 0.75, na.rm = TRUE) +
      1.5 * (quantile(flux, 0.75, na.rm = TRUE) - quantile(flux, 0.25, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate(is_outlier = flux < flux_lower | flux > flux_upper) %>%
  mutate(data_type = ifelse(is_outlier, "removed", "clean"))

mt_filtered <- mt_outlier[mt_outlier$is_outlier == F, c(1:7)]
mt_filtered$logflux <- log(mt_filtered$flux + 0.001)

# 5.mt_outlier
# mainTitle = sprintf("%s", "5.mt_outlier")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# 
# ggplot(mt_outlier, aes(x = temp, y = flux, color = data_type)) +
#   geom_point(size=0.8) +
#   facet_grid(. ~ Date, scale = "free_x") + #scale = "free"
#   scale_color_manual(values = c("clean" = "blue", "removed" = "red")) +
#   labs(
#     x = "Temperature (°C)",
#     y = "CO2 flux (g/m^2/hr)",
#     color = "Data Type"
#   ) +
#   theme_bw() +
#   ggsave(saveImg, units = "cm", height = 8, width = 40)
# 
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# 5.mt_outlier_ln변환
# mainTitle = sprintf("%s", "5.mt_outlier_ln변환")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# 
# ggplot(mt_outlier, aes(x = temp, y = log(flux), color = data_type)) +
#   geom_point(size=0.8) +
#   facet_grid(. ~ Date, scale = "free_x") + #scale = "free"
#   scale_color_manual(values = c("clean" = "blue", "removed" = "red")) +
#   labs(
#     x = "Temperature (°C)",
#     y = "ln (CO2 flux) (g/m^2/hr)",
#     color = "Data Type"
#   ) +
#   theme_bw() + 
#   ggsave(saveImg, units = "cm", height = 8, width = 40)
# 
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ##### 토양온도 vs 토양 호흡 #####
# # 선형 회귀 분석 (각 season별 R^2 및 Slope 계산)
# r2_values <- mt_filtered %>%
#   group_by(season) %>%
#   summarise(
#     r2 = round(summary(lm(flux ~ temp, data = pick(flux, temp)))$r.squared, 3),
#     slope = round(coef(lm(flux ~ temp, data = pick(flux, temp)))[2], 3),
#     x_pos = min(temp, na.rm = TRUE),
#     y_pos = max(flux, na.rm = TRUE) * 0.8
#   )
# 
# # ggplot으로 산점도 + 회귀선 그리기
# mainTitle = sprintf("%s", "6.계절별 토양온도 vs 토양호흡")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# 
# p <- ggplot(mt_filtered, aes(x = temp, y = flux)) +
#   geom_point(alpha = 0.6, color = "blue") +  # 산점도
#   geom_smooth(method = "lm", se = T, color = "red", linetype = "dashed") +  # 회귀선
#   facet_wrap(~ season, scales = "free_x",  # X축을 자유롭게 설정
#              labeller = as_labeller(c(sp = "Spring (3-5)", s = "Summer (6-8)", f = "Fall (9-11)"))) +  
#   labs(x = "Ts (°C)", 
#        y = "ln CO2 flux (g/m^2/hr)") +
#   theme_bw()
# 
# # R^2 값 추가 (위치 및 크기 조정)
# p + geom_text(data = r2_values, aes(x = x_pos, 
#                                     y = c(0.3,0,0.4), 
#                                     label = paste("Slope:", slope, "\nR²:", r2)),
#               inherit.aes = FALSE, color = "black", size = 3, hjust = 0,      show.legend = FALSE ) +
#   ggsave(saveImg, units="cm", width=20,height=8)
# 
# # shell.exec(saveImg)
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# 
# # ggplot으로 산점도 + 회귀선 그리기
# mainTitle = sprintf("%s", "6.계절별 토양온도 vs 토양호흡 다른 버전")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# 
# p <- ggplot(mt_filtered, aes(x = temp, y = flux, color=season, fill=season)) +
#   geom_point(alpha = 0.6) +  # 산점도
#   geom_smooth(method = "lm", se = T, linetype = "dashed") +  # 회귀선
#   labs(x = "Ts (°C)", 
#        y = "ln CO2 flux (g/m^2/hr)") +
#   theme_bw() +
#   scale_color_discrete(labels = c("sp" = "Spring (3-5)",
#                                   "s"  = "Summer (6-8)",
#                                   "f"  = "Fall (9-11)")) +
#   scale_fill_discrete(labels = c("sp" = "Spring (3-5)",
#                                  "s"  = "Summer (6-8)",
#                                  "f"  = "Fall (9-11)"))
# 
# # R^2 값 추가 (위치 및 크기 조정)
# p + 
#   geom_text(
#     data = r2_values,
#     aes(
#       x = c(0, 15, 10),         # r2_values에 미리 계산해 둔 x 위치
#       y = c(0.4, 0.7, 0.5),         # r2_values에 미리 계산해 둔 y 위치
#       label = paste("Slope:", slope, "\nR²:", r2),
#       color = season     # <-- season에 따라 글씨 색상이 달라짐
#     ),
#     inherit.aes = FALSE, # 메인 그래프의 aes 맵핑을 상속받지 않음
#     size = 4,
#     hjust = 0,      show.legend = FALSE 
#   ) +
#   ggsave(saveImg, units="cm", width=15,height=10)
# 
# # shell.exec(saveImg)
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# 
# 
# ##### 토양온도 vs 토양 호흡 log 변환#####
# # 선형 회귀 분석 (각 season별 R^2 및 Slope 계산)
# r2_values <- mt_filtered %>%
#   group_by(season) %>%
#   summarise(
#     r2 = round(summary(lm(logflux ~ temp, data = pick(logflux, temp)))$r.squared, 3),
#     slope = round(coef(lm(logflux ~ temp, data = pick(logflux, temp)))[2], 3),
#     x_pos = min(temp, na.rm = TRUE),
#     y_pos = max(logflux, na.rm = TRUE) * 0.8
#   )
# 
# # ggplot으로 산점도 + 회귀선 그리기
# mainTitle = sprintf("%s", "6.계절별 토양온도 vs 토양호흡 log 변환")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# 
# p <- ggplot(mt_filtered, aes(x = temp, y = logflux)) +
#   geom_point(alpha = 0.6, color = "blue") +  # 산점도
#   geom_smooth(method = "lm", se = T, color = "red", linetype = "dashed") +  # 회귀선
#   facet_wrap(~ season, scales = "free_x",  # X축을 자유롭게 설정
#              labeller = as_labeller(c(sp = "Spring (3-5)", s = "Summer (6-8)", f = "Fall (9-11)"))) +  
#   labs(x = "Ts (°C)", 
#        y = "ln CO2 flux (g/m^2/hr)") +
#   theme_bw()
# 
# # R^2 값 추가 (위치 및 크기 조정)
# p + geom_text(data = r2_values, aes(x = x_pos, 
#                                     y = c(-1,-2,-1), 
#                                     label = paste("Slope:", slope, "\nR²:", r2)),
#               inherit.aes = FALSE, color = "black", size = 3, hjust = 0,      show.legend = FALSE ) +
#   ggsave(saveImg, units="cm", width=20,height=8)
# 
# # shell.exec(saveImg)
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ggplot으로 산점도 + 회귀선 그리기
mainTitle = sprintf("%s", "6.계절별 토양온도 vs 토양호흡 log 변환 다른 버전")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

# summary(mt_filtered$logflux)

p <- ggplot(mt_filtered, aes(x = temp, y = logflux, color=season, fill=season)) +
  geom_point(alpha = 0.6) +  # 산점도
  geom_smooth(method = "lm", se = T, linetype = "dashed") +  # 회귀선
  labs(x = "Ts (°C)", 
       y = "ln CO2 flux (g/m^2/hr)") +
  theme_bw() +
  ylim(-7, 2) +
  scale_color_discrete(labels = c("sp" = "Spring (3-5)",
                                  "s"  = "Summer (6-8)",
                                  "f"  = "Fall (9-11)")) +
  scale_fill_discrete(labels = c("sp" = "Spring (3-5)",
                                 "s"  = "Summer (6-8)",
                                 "f"  = "Fall (9-11)"))

# # 선형 회귀 분석 (각 season별 R^2 및 Slope 계산)
r2_values <- mt_filtered %>%
  group_by(season) %>%
  summarise(
    r2 = round(summary(lm(logflux ~ temp, data = pick(logflux, temp)))$r.squared, 3),
    slope = round(coef(lm(logflux ~ temp, data = pick(logflux, temp)))[2], 3),
    pval = formatC(summary(lm(logflux ~ temp, data = pick(logflux, temp)))$fstatistic %>%
                    {1 - pf(.[1], .[2], .[3])}, format = "f", digits = 2),
    x_pos = min(temp, na.rm = TRUE),
    y_pos = max(logflux, na.rm = TRUE) * 0.8
  )

# R^2 값 추가 (위치 및 크기 조정)
p + 
  geom_text(
    data = r2_values,
    aes(
      x = c(1, 11, 21),         # r2_values에 미리 계산해 둔 x 위치
      y = c(1, 1, 1),         # r2_values에 미리 계산해 둔 y 위치
      label = paste0("Slope: ", slope, "\nR²:", r2, "\n(P < ", pval, ")"),
      color = season     # <-- season에 따라 글씨 색상이 달라짐
    ),
    inherit.aes = FALSE, # 메인 그래프의 aes 맵핑을 상속받지 않음
    size = 4,
    hjust = 0,      show.legend = FALSE 
  ) +
  ggsave(saveImg, units="cm", width=15,height=10)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ================================================
# 4. 토양온도와 MT 자료, 토양수분 자료를 활용하여 기존 그래프 업데이트
# ================================================
# mt <- read.csv("mt.csv")

# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "mt.csv"))
# mt <- read.csv(fileInfo)


fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "20250130_2023년3월11월_기온및토양온도_통합V7문의.xlsx"))
orgData = openxlsx::read.xlsx(fileInfo)
temp_compare = orgData %>% 
  dplyr::select(timestamp,Soil_CW1,Soil_CN1,Air_CW1,Air_CN1,Soil_CW2,Soil_CN2,Air_CW2,Air_CN2,Soil_CW3,Soil_CN3,Air_CW3,Air_CN3,Soil_LW1,Soil_LN1,Air_LW1,Air_LN1,Soil_LW2,Soil_LN2,Air_LW2,Air_LN2,Soil_LW3,Soil_LN3,Air_LW3,Air_LN3,Soil_SW1,Soil_SN1,Air_SW1,Air_SN1,Soil_SW2,Soil_SN2,Air_SW2,Air_SN2,Soil_SW3,Soil_SN3,Air_SW3,Air_SN3)

# 시간 문자열 정리
temp_compare$timestamp <- gsub("오전 ", "AM ", temp_compare$timestamp)
temp_compare$timestamp <- gsub("오후 ", "PM ", temp_compare$timestamp)
temp_compare$timestamp <- gsub("시", ":", temp_compare$timestamp)
temp_compare$timestamp <- gsub("분", ":", temp_compare$timestamp)
temp_compare$timestamp <- gsub("초", "", temp_compare$timestamp)

# 공백 정리
temp_compare$timestamp <- gsub("\\s+", " ", temp_compare$timestamp)  # 연속된 공백 제거
temp_compare$timestamp <- gsub(": ", ":", temp_compare$timestamp)   # 잘못된 공백 제거

# 문자열을 datetime 형식으로 변환
temp_compare$timestamp <- parse_date_time(temp_compare$timestamp, orders = "m. d. y p I:M:S ", quiet = TRUE)

# 데이터 불러오기 (예제에서는 temp_compare 데이터 프레임을 가정)
# -20도 이하의 값은 NA로 변환
temp_compare[temp_compare < -20] <- NA

# Air 데이터만 선택
air_data <- temp_compare %>% 
  select(timestamp, starts_with("Air_")) %>% 
  dplyr::mutate(
    date = as.Date(timestamp)
  ) %>% 
  group_by(date) %>%
  summarise(
    CN = mean(c(Air_CN1, Air_CN2, Air_CN3), na.rm = TRUE),
    CW = mean(c(Air_CW1, Air_CW2, Air_CW3), na.rm = TRUE),
    LN = mean(c(Air_LN1, Air_LN2, Air_LN3), na.rm = TRUE),
    LW = mean(c(Air_LW1, Air_LW2, Air_LW3), na.rm = TRUE),
    SN = mean(c(Air_SN1, Air_SN2, Air_SN3), na.rm = TRUE),
    SW = mean(c(Air_SW1, Air_SW2, Air_SW3), na.rm = TRUE)
  ) %>% 
  pivot_longer(cols = -date, names_to = "variable", values_to = "value") %>%
  dplyr::mutate(
    PPT_control = substr(variable, 1, 1), # 첫 글자로 그룹(C, L, S)
    Heating = substr(variable, 2, 2)  # 두 번째 글자로 유형(N, W)
  )

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "SWC_2023.xlsx"))
orgData = openxlsx::read.xlsx(fileInfo)

# timestamp를 날짜 형식으로 변환
vwc <- orgData %>%
  mutate(
    date = as.Date(as.numeric(TIMESTAMP), origin = "1899-12-30"), 
    across(where(is.character), as.numeric)
  ) %>%
  group_by(date) %>%
  summarise(
    CN = mean(c(CN1, CN2, CN3), na.rm = TRUE),
    CW = mean(c(CW1, CW2, CW3), na.rm = TRUE),
    LN = mean(c(LN1, LN2, LN3), na.rm = TRUE),
    LW = mean(c(LW1, LW2, LW3), na.rm = TRUE),
    SN = mean(c(SN1, SN2, SN3), na.rm = TRUE),
    SW = mean(c(SW1, SW2, SW3), na.rm = TRUE)
  )

# 데이터를 long 형식으로 변환
vwc_long <- vwc %>%
  pivot_longer(cols = -date, names_to = "variable", values_to = "value") %>%
  mutate(
    PPT_control = substr(variable, 1, 1), # 첫 글자로 그룹(C, L, S)
    Heating = substr(variable, 2, 2)  # 두 번째 글자로 유형(N, W)
  ) %>%
  dplyr::mutate(
    Date = date
    , VWC = as.numeric(value)
  ) %>% 
  dplyr::select(
    Date, PPT_control, Heating, VWC
  )

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "20250130_2023년_마스터테이블_문의.xlsx"))
orgData = openxlsx::read.xlsx(fileInfo)
mt = orgData %>% 
  dplyr::select(-VWC)

mt$Date <- as.Date(mt$Date, origin = "1899-12-30")
# mt$VWC <- as.numeric(mt$VWC)
mt$season <- factor(mt$season, levels = c("sp", "s", "f"))
mt$Heating[mt$Heating=="NW"] <- "N"
mt$Treatment <- paste0(mt$PPT_control, mt$Heating)

# NA 값 제거 (temp와 flux가 NA가 아닌 값만 사용)
mt_outlier <- mt %>%
  # dplyr::left_join(vwc_long, by = c("Date", "PPT_control", "Heating")) %>% 
  filter(season %in% c("sp", "s", "f"), !is.na(temp), !is.na(flux)) %>%
  select(Date, temp, VWC, flux, season, PPT_control, Heating, Treatment)  %>%
  dplyr::mutate(
    flux = log(flux)
  ) %>% 
  group_by(Date) %>%
  mutate(
    flux_lower = quantile(flux, 0.25, na.rm = TRUE) -
      1.5 * (quantile(flux, 0.75, na.rm = TRUE) - quantile(flux, 0.25, na.rm = TRUE)),
    flux_upper = quantile(flux, 0.75, na.rm = TRUE) +
      1.5 * (quantile(flux, 0.75, na.rm = TRUE) - quantile(flux, 0.25, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate(is_outlier = 
           flux < flux_lower | flux > flux_upper) %>%
  mutate(data_type = ifelse(is_outlier, "removed", "clean"))

mt_filtered <- mt_outlier[mt_outlier$is_outlier == F, c(1:8)]
mt_filtered$logflux <- log(mt_filtered$flux + 0.001)

# 1) 요약: all-NA인 경우를 NA_real_로 바꾸는 if문 사용
mt_daily <- mt_filtered %>%
  group_by(Date, PPT_control, Heating) %>%
  summarise(
    flux_mean = if (all(is.na(flux))) NA_real_ else mean(flux, na.rm = TRUE),
    flux_min  = if (all(is.na(flux))) NA_real_ else min(flux, na.rm = TRUE),
    flux_max  = if (all(is.na(flux))) NA_real_ else max(flux, na.rm = TRUE),
    
    temp_mean = if (all(is.na(temp))) NA_real_ else mean(temp, na.rm = TRUE),
    temp_min  = if (all(is.na(temp))) NA_real_ else min(temp, na.rm = TRUE),
    temp_max  = if (all(is.na(temp))) NA_real_ else max(temp, na.rm = TRUE),
    
    VWC_mean = if (all(is.na(VWC))) NA_real_ else mean(VWC, na.rm = TRUE),
    VWC_min  = if (all(is.na(VWC))) NA_real_ else min(VWC, na.rm = TRUE),
    VWC_max  = if (all(is.na(VWC))) NA_real_ else max(VWC, na.rm = TRUE),
    
    .groups = "drop"
  )


# 2) ggplot용 설정
shape_vals <- c("C" = 16,  # ●
                "L" = 17,  # ▲
                "S" = 15)  # ■

color_vals <- c("W" = "red",
                "N" = "blue")

# (a) flux plot
mainTitle = sprintf("%s", "9.토양호흡_토양온도_토양수분")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

breaksDateList <- sort(c(
  seq(as.Date("2023-01-01"), as.Date("2023-12-15"), by = "1 month"),
  seq(as.Date("2023-01-10"), as.Date("2023-12-10"), by = "1 month"),
  seq(as.Date("2023-01-20"), as.Date("2023-12-20"), by = "1 month")
))

p_flux <- ggplot(mt_daily,
                 aes(x = Date, 
                     y = flux_mean,
                     # y = log(flux_mean), 
                     shape = PPT_control, 
                     color = Heating,
                     group = interaction(PPT_control, Heating))) +
  geom_line(na.rm = TRUE) +
  geom_pointrange(aes(ymin = flux_min, ymax = flux_max), na.rm = TRUE) +
  # geom_pointrange(aes(ymin = log(flux_min), ymax = log(flux_max)), na.rm = TRUE) +
  scale_shape_manual(values = shape_vals) +
  scale_color_manual(values = color_vals) +
  # scale_x_date(date_breaks = "1 month", date_labels = "%m") +
  scale_x_date(breaks = breaksDateList, date_labels = "%m\n%d") +
  labs(x = NULL, 
       # y = expression(CO[2]~flux~"(" * g~m^-2~hr^-1 * ")")
       y = expression(ln~CO[2]~flux~"(" * g~m^-2~hr^-1 * ")")
  ) +
  theme_classic() +
  theme(
    axis.text.x  = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# (b) temp plot
p_temp <- ggplot(mt_daily,
                 aes(x = Date,
                     y = temp_mean,
                     shape = PPT_control,
                     color = Heating,
                     group = interaction(PPT_control, Heating))) +
  geom_point(na.rm = TRUE) +
  geom_line(data = air_data, aes(x = date, y = value), alpha = 0.1) +
  # geom_pointrange(aes(ymin = temp_min, ymax = temp_max), na.rm = TRUE) +
  scale_shape_manual(values = shape_vals) +
  scale_color_manual(values = color_vals) +
  # scale_x_date(date_breaks = "1 month", date_labels = "%m") +
  scale_x_date(breaks = breaksDateList, date_labels = "%m\n%d") +
  labs(x = NULL, 
       y = "Ts (°C)") +
  theme_classic() +
  theme(
    axis.text.x  = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# (c) VWC plot
p_vwc <- ggplot(mt_daily,
                aes(x = Date,
                    y = VWC_mean,
                    shape = PPT_control,
                    color = Heating,
                    group = interaction(PPT_control, Heating))) +
  geom_point(na.rm = TRUE) +
  geom_line(data = vwc_long, aes(x = Date, y = VWC), alpha = 0.1) +
  # geom_pointrange(aes(ymin = VWC_min, ymax = VWC_max), na.rm = TRUE) +
  scale_shape_manual(values = shape_vals) +
  scale_color_manual(values = color_vals) +
  # scale_x_date(date_breaks = "1 month", date_labels = "%m") +
  scale_x_date(breaks = breaksDateList, date_labels = "%m\n%d") +
  labs(x = "Month", 
       y = "Soil Water Content (\u03B8 %)") +
  theme_classic()

# 3) 3개 플롯을 세로로 결합
combined_plot <- (p_flux / p_temp / p_vwc) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")
# theme(legend.position = "right")

# 4) 출력
combined_plot +
  ggsave(saveImg, units="cm", width=15,height=15)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ================================================
# 이것도 엑셀의 토양수분 데이터로 업데이트 부탁드립니다
# ================================================
# prec <- read.csv("prec.csv")
# vwc <- read.csv("VWC.csv")

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "prec.csv"))
prec <- read.csv(fileInfo)

# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "VWC.csv"))
# vwc <- read.csv(fileInfo)

# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "20250130_2023년_마스터테이블_문의.xlsx"))
# orgData = openxlsx::read.xlsx(fileInfo)
# vwc = orgData %>% 
#   dplyr::mutate(
#     date = as.Date(Date, origin = "1899-12-30")
#     , value = VWC
#   ) %>% 
#   dplyr::select(Date, value, PPT_control, Heating)

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "SWC_2023.xlsx"))
orgData = openxlsx::read.xlsx(fileInfo)

# timestamp를 날짜 형식으로 변환
vwc <- orgData %>%
  mutate(
    date = as.Date(as.numeric(TIMESTAMP), origin = "1899-12-30"), 
    across(where(is.character), as.numeric)
    ) %>%
  group_by(date) %>%
  summarise(
    CN = mean(c(CN1, CN2, CN3), na.rm = TRUE),
    CW = mean(c(CW1, CW2, CW3), na.rm = TRUE),
    LN = mean(c(LN1, LN2, LN3), na.rm = TRUE),
    LW = mean(c(LW1, LW2, LW3), na.rm = TRUE),
    SN = mean(c(SN1, SN2, SN3), na.rm = TRUE),
    SW = mean(c(SW1, SW2, SW3), na.rm = TRUE)
  )

prec <- prec %>%
  mutate(date = as.Date(timestamp, format = "%Y-%m-%d %H:%M")) %>%
  group_by(date) %>%
  summarise(prec = sum(prec, na.rm = TRUE))

# 데이터를 long 형식으로 변환
vwc_long <- vwc %>%
  pivot_longer(cols = -date, names_to = "variable", values_to = "value") %>%
  mutate(
    PPT_control = substr(variable, 1, 1), # 첫 글자로 그룹(C, L, S)
    Heating = substr(variable, 2, 2)  # 두 번째 글자로 유형(N, W)
  )

summary(vwc_long)
summary(prec)

# prec$prec

scaler <- 10

# 10.강우량 vs 강우조건별 토양수분
mainTitle = sprintf("%s", "10.강우량 vs 강우조건별 토양수분")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot() +
  # 강수량 선 그래프 (왼쪽 y축)
  geom_col(data = prec, aes(x = date, y = prec), fill = "dodgerblue2", alpha = 0.6) +
  # 토양 수분 선 그래프 (오른쪽 y축)
  geom_line(data = vwc_long, 
            aes(x = date, y = value * 100 * scaler, color = PPT_control, linetype = Heating), size = 0.7) +
  # 색상과 선형 지정
  scale_color_manual(values = c("C" = "blue", "L" = "darkgreen", "S" = "red")) +
  scale_linetype_manual(values = c("N" = "dotted", "W" = "solid")) +
  # x축 포맷 변경 (Month만 표시)
  scale_x_date(date_labels = "%m", date_breaks = "1 month") +
  # y축 이름과 sec.axis 설정
  scale_y_continuous(name = "Precipitation (mm/d)",
                     sec.axis = sec_axis(~ . / scaler, name = "Soil Water Content (θ%)")) +
  # 레이블과 테마 설정
  labs(x = "Month", color = "PPT_control", linetype = "Heating") +
  theme_bw() +
  theme(
    axis.title.y.right = element_text(color = "black"),
    axis.title.y.left = element_text(color = "black"),
    legend.position = "right"
  ) +
  ggsave(saveImg, units="cm", width=20,height=12)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ================================================
# 공통사항1.R
# ================================================
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "soil_vs_air_temp_raw.csv"))
# temp_compare <- read.csv(fileInfo)

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "20250130_2023년3월11월_기온및토양온도_통합V7문의.xlsx"))
orgData = openxlsx::read.xlsx(fileInfo)
temp_compare = orgData %>% 
  dplyr::select(timestamp,Soil_CW1,Soil_CN1,Air_CW1,Air_CN1,Soil_CW2,Soil_CN2,Air_CW2,Air_CN2,Soil_CW3,Soil_CN3,Air_CW3,Air_CN3,Soil_LW1,Soil_LN1,Air_LW1,Air_LN1,Soil_LW2,Soil_LN2,Air_LW2,Air_LN2,Soil_LW3,Soil_LN3,Air_LW3,Air_LN3,Soil_SW1,Soil_SN1,Air_SW1,Air_SN1,Soil_SW2,Soil_SN2,Air_SW2,Air_SN2,Soil_SW3,Soil_SN3,Air_SW3,Air_SN3)

# 시간 문자열 정리
temp_compare$timestamp <- gsub("오전 ", "AM ", temp_compare$timestamp)
temp_compare$timestamp <- gsub("오후 ", "PM ", temp_compare$timestamp)
temp_compare$timestamp <- gsub("시", ":", temp_compare$timestamp)
temp_compare$timestamp <- gsub("분", ":", temp_compare$timestamp)
temp_compare$timestamp <- gsub("초", "", temp_compare$timestamp)

# 공백 정리
temp_compare$timestamp <- gsub("\\s+", " ", temp_compare$timestamp)  # 연속된 공백 제거
temp_compare$timestamp <- gsub(": ", ":", temp_compare$timestamp)   # 잘못된 공백 제거

# 문자열을 datetime 형식으로 변환
temp_compare$timestamp <- parse_date_time(temp_compare$timestamp, orders = "m. d. y p I:M:S ", quiet = TRUE)

# 데이터 불러오기 (예제에서는 temp_compare 데이터 프레임을 가정)
# -20도 이하의 값은 NA로 변환
temp_compare[temp_compare < -20] <- NA

# Soil 데이터만 선택
soil_data <- temp_compare %>% 
  select(timestamp, starts_with("Soil_")) %>% 
  pivot_longer(cols = -timestamp, names_to = "Treatment", values_to = "Temperature")

# Treatment 그룹화 (C, L, S 조합과 N, W 조합을 묶어 6개의 그룹 생성)
soil_data <- soil_data %>%
  filter(!is.na(Treatment))

soil_data <- soil_data %>%
  mutate(
    timestamp = floor_date(timestamp, unit = "6 hours")
  ) %>%
  group_by(timestamp, Treatment) %>%
  summarise(Temperature = mean(Temperature, na.rm =T))

# Air 데이터만 선택
air_data <- temp_compare %>% 
  select(timestamp, starts_with("Air_")) %>% 
  pivot_longer(cols = -timestamp, names_to = "Treatment", values_to = "Temperature")

# Treatment 그룹화 (C, L, S 조합과 N, W 조합을 묶어 6개의 그룹 생성)
air_data <- air_data %>%
  filter(!is.na(Treatment))

air_data <- air_data %>% filter(!is.na(Temperature))
air_data <- air_data %>%
  mutate(
    timestamp = floor_date(timestamp, unit = "6 hours")
  ) %>%
  group_by(timestamp, Treatment) %>%
  summarise(Temperature = mean(Temperature, na.rm =T))

soil_data$group <- "Soil"
air_data$group <- "Air"

temp_compare <- rbind(soil_data, air_data)
temp_summary <- temp_compare %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(group, date, Treatment) %>%
  summarise(mean = mean(Temperature, na.rm = T))

soil_data$number <- substr(soil_data$Treatment, 8, 8)
soil_data$Treatment <- substr(soil_data$Treatment, 6, 7)

# 공통사항1-1
mainTitle = sprintf("%s", "공통사항1-1")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(soil_data) +
  geom_line(aes(x = timestamp, y = Temperature, color = number), linewidth = 0.5, alpha = 0.6) + # 연한 색으로 설정
  labs(
    x = "Month",
    y = "Temperature (°C)",
    color = "Group",
  ) +
  #  scale_x_date(date_breaks = "1 month", date_labels = "%m") + # X축 형식
  facet_grid(. ~ Treatment) +
  theme_bw() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) + 
  ggsave(saveImg, units="cm", width=50,height=15)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

air_data$number <- substr(air_data$Treatment, 7, 7)
air_data$Treatment <- substr(air_data$Treatment, 5, 6)

# 공통사항1-2
mainTitle = sprintf("%s", "공통사항1-2")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(air_data) +
  geom_line(aes(x = timestamp, y = Temperature, color = number), linewidth = 0.5, alpha = 0.6) + # 연한 색으로 설정
  labs(
    x = "Month",
    y = "Temperature (°C)",
    color = "Group"
  ) +
  #  scale_x_date(date_breaks = "1 month", date_labels = "%m") + # X축 형식
  facet_grid(. ~ Treatment) +
  theme_bw() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) + 
  ggsave(filename = saveImg, units="cm", width=50,height=15)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# vwc <- read.csv("VWC.csv")
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "VWC.csv"))
vwc <- read.csv(fileInfo)
# timestamp를 날짜 형식으로 변환

# 데이터를 long 형식으로 변환
vwc_long <- vwc %>%
  pivot_longer(cols = -timestamp, names_to = "variable", values_to = "value") 
vwc_long$Treatment <- vwc_long$variable

vwc_long <- vwc_long %>%
  mutate(timestamp = parse_date_time(timestamp, orders = "ymd HM"))%>%
  mutate(timestamp = floor_date(timestamp, unit = "6 hours")) %>%
  group_by(timestamp, Treatment) %>%
  summarise(swc = mean(value, na.rm =T))

vwc_long$number <- substr(vwc_long$Treatment, 3, 3)
vwc_long$Treatment <- substr(vwc_long$Treatment, 1, 2)

# 공통사항1-3
mainTitle = sprintf("%s", "공통사항1-3")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot() +
  geom_line(data = vwc_long, 
            aes(x = timestamp, y = swc, color = number), size = 0.7) +
  facet_grid(. ~ Treatment) +
  # x축 포맷 변경 (Month만 표시)
  # y축 이름과 sec.axis 설정
  scale_y_continuous(name = "Soil Water Content (θ%)") + 
  # 레이블과 테마 설정
  labs(x = "Month") +
  theme_bw() +
  ggsave(filename = saveImg, units="cm", width=50,height=15)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")



##### 일평균 #####
# temp_compare <- read.csv("soil_vs_air_temp_raw.csv")

# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "soil_vs_air_temp_raw.csv"))
# temp_compare <- read.csv(fileInfo)

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "20250130_2023년3월11월_기온및토양온도_통합V7문의.xlsx"))
orgData = openxlsx::read.xlsx(fileInfo)
temp_compare = orgData %>% 
  dplyr::select(timestamp,Soil_CW1,Soil_CN1,Air_CW1,Air_CN1,Soil_CW2,Soil_CN2,Air_CW2,Air_CN2,Soil_CW3,Soil_CN3,Air_CW3,Air_CN3,Soil_LW1,Soil_LN1,Air_LW1,Air_LN1,Soil_LW2,Soil_LN2,Air_LW2,Air_LN2,Soil_LW3,Soil_LN3,Air_LW3,Air_LN3,Soil_SW1,Soil_SN1,Air_SW1,Air_SN1,Soil_SW2,Soil_SN2,Air_SW2,Air_SN2,Soil_SW3,Soil_SN3,Air_SW3,Air_SN3)

# 시간 문자열 정리
temp_compare$timestamp <- gsub("오전 ", "AM ", temp_compare$timestamp)
temp_compare$timestamp <- gsub("오후 ", "PM ", temp_compare$timestamp)
temp_compare$timestamp <- gsub("시", ":", temp_compare$timestamp)
temp_compare$timestamp <- gsub("분", ":", temp_compare$timestamp)
temp_compare$timestamp <- gsub("초", "", temp_compare$timestamp)

# 공백 정리
temp_compare$timestamp <- gsub("\\s+", " ", temp_compare$timestamp)  # 연속된 공백 제거
temp_compare$timestamp <- gsub(": ", ":", temp_compare$timestamp)   # 잘못된 공백 제거

# 문자열을 datetime 형식으로 변환
temp_compare$timestamp <- parse_date_time(temp_compare$timestamp, orders = "m. d. y p I:M:S ", quiet = TRUE)

# 데이터 불러오기 (예제에서는 temp_compare 데이터 프레임을 가정)
# -20도 이하의 값은 NA로 변환
temp_compare[temp_compare < -20] <- NA

# Soil 데이터만 선택
soil_data <- temp_compare %>% 
  select(timestamp, starts_with("Soil_")) %>% 
  pivot_longer(cols = -timestamp, names_to = "Treatment", values_to = "Temperature")

# Treatment 그룹화 (C, L, S 조합과 N, W 조합을 묶어 6개의 그룹 생성)
soil_data <- soil_data %>%
  filter(!is.na(Treatment))

soil_data <- soil_data %>%
  mutate(
    timestamp = as.Date(timestamp)
  ) %>%
  group_by(timestamp, Treatment) %>%
  summarise(Temperature = mean(Temperature, na.rm =T))


# Air 데이터만 선택
air_data <- temp_compare %>% 
  select(timestamp, starts_with("Air_")) %>% 
  pivot_longer(cols = -timestamp, names_to = "Treatment", values_to = "Temperature")

# Treatment 그룹화 (C, L, S 조합과 N, W 조합을 묶어 6개의 그룹 생성)
air_data <- air_data %>%
  filter(!is.na(Treatment))

air_data <- air_data %>% filter(!is.na(Temperature))
air_data <- air_data %>%
  mutate(
    timestamp = as.Date(timestamp)
  ) %>%
  group_by(timestamp, Treatment) %>%
  summarise(Temperature = mean(Temperature, na.rm =T))

soil_data$group <- "Soil"
air_data$group <- "Air"

temp_compare <- rbind(soil_data, air_data)

soil_data$number <- substr(soil_data$Treatment, 8, 8)
soil_data$Treatment <- substr(soil_data$Treatment, 6, 7)

# NEW 공통사항1-1
mainTitle = sprintf("%s", "NEW 공통사항1-1")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(soil_data) +
  geom_line(aes(x = timestamp, y = Temperature, color = number), linewidth = 0.5, alpha = 0.6) + # 연한 색으로 설정
  labs(
    x = "Month",
    y = "Temperature (°C)",
    color = "Group",
  ) +
  #  scale_x_date(date_breaks = "1 month", date_labels = "%m") + # X축 형식
  facet_grid(. ~ Treatment) +
  theme_bw() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) + 
  ggsave(saveImg, units="cm", width=50,height=15)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

air_data$number <- substr(air_data$Treatment, 7, 7)
air_data$Treatment <- substr(air_data$Treatment, 5, 6)

# NEW 공통사항1-2
mainTitle = sprintf("%s", "NEW 공통사항1-2")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(air_data) +
  geom_line(aes(x = timestamp, y = Temperature, color = number), linewidth = 0.5, alpha = 0.6) + # 연한 색으로 설정
  labs(
    x = "Month",
    y = "Temperature (°C)",
    color = "Group"
  ) +
  #  scale_x_date(date_breaks = "1 month", date_labels = "%m") + # X축 형식
  facet_grid(. ~ Treatment) +
  theme_bw() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  ggsave(saveImg, units="cm", width=50,height=15)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "VWC.csv"))
# vwc <- read.csv("VWC.csv")
vwc <- read.csv(fileInfo)

# 데이터를 long 형식으로 변환
vwc_long <- vwc %>%
  pivot_longer(cols = -timestamp, names_to = "variable", values_to = "value") 
vwc_long$Treatment <- vwc_long$variable

vwc_long <- vwc_long %>%
  mutate(timestamp = parse_date_time(timestamp, orders = "ymd HM"))%>%
  mutate(timestamp = as.Date(timestamp)) %>%
  group_by(timestamp, Treatment) %>%
  summarise(swc = mean(value, na.rm =T))

vwc_long$number <- substr(vwc_long$Treatment, 3, 3)
vwc_long$Treatment <- substr(vwc_long$Treatment, 1, 2)

# NEW 공통사항1-3
mainTitle = sprintf("%s", "NEW 공통사항1-3")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot() +
  geom_line(data = vwc_long, 
            aes(x = timestamp, y = swc, color = number), size = 0.7) +
  facet_grid(. ~ Treatment) +
  # x축 포맷 변경 (Month만 표시)
  # y축 이름과 sec.axis 설정
  scale_y_continuous(name = "Soil Water Content (θ%)") + 
  # 레이블과 테마 설정
  labs(x = "Month") +
  theme_bw() +
  ggsave(saveImg, units="cm", width=50,height=15)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ================================================
# figure0,1.R
# ================================================
##### Figure 0,1 아웃라이어 제거 #####
##### 플랜트박스 데이터 = 플럭스 IQR 기준으로 아웃라이어 제거 #####
# mt <- read.csv("mt.csv")
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "mt.csv"))
mt <- read.csv(fileInfo)

mt$Date <- as.Date(mt$Date, origin = "1899-12-30")
mt$Heating <- substring(mt$Heating,1,1)
mt$Treatment <- paste0(mt$PPT_control, mt$Heating)

# 그룹별로 IQR 기준으로 이상치 탐지
mt <- mt %>%
  group_by(Treatment, Date) %>%
  mutate(
    Q1 = quantile(flux, 0.25, na.rm = TRUE),
    Q3 = quantile(flux, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR,
    outlier = flux < lower_bound | flux > upper_bound
  ) %>%
  ungroup()

# NEW 공통사항1-1
mainTitle = sprintf("%s", "0.flux_outliers")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(mt) +
  geom_line(aes(x = Date, y = flux, group = Plot_No), color="grey") +
  geom_point(aes(x = Date, y = flux, group = Plot_No), cex=0.8) +
  geom_line(aes(x = Date, y = lower_bound), color = "blue", linewidth = 0.8, linetype = "dashed") +
  geom_line(aes(x = Date, y = upper_bound), color = "blue", linewidth = 0.8, linetype = "dashed") +
  facet_grid(. ~ Treatment) +
  labs(
    x = "Month",
    y = "Flux"
  ) +
  theme_bw() +
  ggsave(saveImg, units = "cm", height = 10, width = 30)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

mt_filtered <- mt[mt$outlier==F,]

# NEW 공통사항1-1
mainTitle = sprintf("%s", "1.flux_outliers_filtered")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(mt_filtered) +
  geom_line(aes(x = Date, y = flux, group = Plot_No), color="grey") +
  geom_point(aes(x = Date, y = flux, group = Plot_No), cex=0.8) +
  geom_line(aes(x = Date, y = lower_bound), color = "blue", linewidth = 0.8, linetype = "dashed") +
  geom_line(aes(x = Date, y = upper_bound), color = "blue", linewidth = 0.8, linetype = "dashed") +
  facet_grid(. ~ Treatment) +
  labs(
    x = "Month",
    y = "Flux"
  ) +
  theme_bw() +
  ggsave(saveImg, units = "cm", height = 10, width = 30)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ================================================
# figure2,3,4.R
# ================================================
##### Figure.2,3,4 토양온도와 대기온도 아웃라이어 제거 후 비교 #####
# temp_compare <- read.csv("soil_vs_air_temp_raw.csv")

# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "soil_vs_air_temp_raw.csv"))
# temp_compare <- read.csv(fileInfo)

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "20250130_2023년3월11월_기온및토양온도_통합V7문의.xlsx"))
orgData = openxlsx::read.xlsx(fileInfo)
temp_compare = orgData %>% 
  dplyr::select(timestamp,Soil_CW1,Soil_CN1,Air_CW1,Air_CN1,Soil_CW2,Soil_CN2,Air_CW2,Air_CN2,Soil_CW3,Soil_CN3,Air_CW3,Air_CN3,Soil_LW1,Soil_LN1,Air_LW1,Air_LN1,Soil_LW2,Soil_LN2,Air_LW2,Air_LN2,Soil_LW3,Soil_LN3,Air_LW3,Air_LN3,Soil_SW1,Soil_SN1,Air_SW1,Air_SN1,Soil_SW2,Soil_SN2,Air_SW2,Air_SN2,Soil_SW3,Soil_SN3,Air_SW3,Air_SN3)

# 시간 문자열 정리
temp_compare$timestamp <- gsub("오전 ", "AM ", temp_compare$timestamp)
temp_compare$timestamp <- gsub("오후 ", "PM ", temp_compare$timestamp)
temp_compare$timestamp <- gsub("시", ":", temp_compare$timestamp)
temp_compare$timestamp <- gsub("분", ":", temp_compare$timestamp)
temp_compare$timestamp <- gsub("초", "", temp_compare$timestamp)

# 공백 정리
temp_compare$timestamp <- gsub("\\s+", " ", temp_compare$timestamp)  # 연속된 공백 제거
temp_compare$timestamp <- gsub(": ", ":", temp_compare$timestamp)   # 잘못된 공백 제거

# 문자열을 datetime 형식으로 변환
temp_compare$timestamp <- parse_date_time(temp_compare$timestamp, orders = "m. d. y p I:M:S ", quiet = TRUE)

# 데이터 불러오기 (예제에서는 temp_compare 데이터 프레임을 가정)
# -20도 이하의 값은 NA로 변환
temp_compare[temp_compare < -20] <- NA

# Soil 데이터만 선택
soil_data <- temp_compare %>% 
  select(timestamp, starts_with("Soil_")) %>% 
  pivot_longer(cols = -timestamp, names_to = "Treatment", values_to = "Temperature")

# Treatment 그룹화 (C, L, S 조합과 N, W 조합을 묶어 6개의 그룹 생성)
soil_data <- soil_data %>%
  mutate(Treatment = case_when(
    grepl("CW", Treatment) ~ "CW",
    grepl("CN", Treatment) ~ "CN",
    grepl("LW", Treatment) ~ "LW",
    grepl("LN", Treatment) ~ "LN",
    grepl("SW", Treatment) ~ "SW",
    grepl("SN", Treatment) ~ "SN",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Treatment))

# 월별 데이터 추가
soil_data <- soil_data %>%
  mutate(Month = format(timestamp, "%m"))

soil_data <- soil_data %>% filter(!is.na(Temperature))

# 아웃라이어 바운드 계산 (평균 ± 3SD 범위)
outlier_bounds <- soil_data %>%
  group_by(Month, Treatment) %>%
  summarise(
    Mean = mean(Temperature, na.rm = TRUE),
    SD = sd(Temperature, na.rm = TRUE),
    Lower_Bound = Mean - 3 * SD,
    Upper_Bound = Mean + 3 * SD,
    .groups = "drop"
  )

# 아웃라이어 제거된 데이터
soil_temp_filtered <- soil_data %>%
  left_join(outlier_bounds, by = c("Month", "Treatment")) %>%
  filter(!is.na(Lower_Bound) & !is.na(Upper_Bound) & Temperature >= Lower_Bound & Temperature <= Upper_Bound)

# Air 데이터만 선택
air_data <- temp_compare %>% 
  select(timestamp, starts_with("Air_")) %>% 
  pivot_longer(cols = -timestamp, names_to = "Treatment", values_to = "Temperature")

# Treatment 그룹화 (C, L, S 조합과 N, W 조합을 묶어 6개의 그룹 생성)
air_data <- air_data %>%
  mutate(Treatment = case_when(
    grepl("CW", Treatment) ~ "CW",
    grepl("CN", Treatment) ~ "CN",
    grepl("LW", Treatment) ~ "LW",
    grepl("LN", Treatment) ~ "LN",
    grepl("SW", Treatment) ~ "SW",
    grepl("SN", Treatment) ~ "SN",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Treatment))

# 월별 데이터 추가
air_data <- air_data %>%
  mutate(Month = format(timestamp, "%m"))

air_data <- air_data %>% filter(!is.na(Temperature))

# 원본 데이터에서 분포와 아웃라이어 바운드 선을 표현한 바이올린 플롯
mainTitle = sprintf("%s", "2.soil_temp_outliers")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(soil_data, aes(x = Month, y = Temperature, fill = Treatment)) +
  geom_violin(alpha = 0.5, scale = "width") +  # 온도의 분포를 보여줌
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.8) +  # 중간 요약 정보 제공
  geom_line(data = outlier_bounds, aes(x = Month, y = Lower_Bound, group = Treatment), linetype = "dashed", color = "black") +
  geom_line(data = outlier_bounds, aes(x = Month, y = Upper_Bound, group = Treatment), linetype = "dashed", color = "black") +
  labs(title = "Soil temperature, full data",
       x = "Month",
       y = "Temperature (°C)") +
  scale_fill_manual(values=c("blue", "red", "blue", "red", "blue", "red")) +
  facet_grid(.~Treatment) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  ggsave(saveImg, units = "cm", height = 15, width = 40)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# 필터링된 데이터에서 분포와 아웃라이어 바운드 선을 표현한 바이올린 플롯
mainTitle = sprintf("%s", "2.soil_outlier_filtered")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(soil_temp_filtered, aes(x = Month, y = Temperature, fill = Treatment)) +
  geom_violin(alpha = 0.5, scale = "width") +  # 온도의 분포를 보여줌
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.8) +  # 중간 요약 정보 제공
  geom_line(data = outlier_bounds, aes(x = Month, y = Lower_Bound, group = Treatment), linetype = "dashed", color = "black") +
  geom_line(data = outlier_bounds, aes(x = Month, y = Upper_Bound, group = Treatment), linetype = "dashed", color = "black") +
  labs(title = "Soil temperature, outlier removed",
       x = "Month",
       y = "Temperature (°C)") +
  scale_fill_manual(values=c("blue", "red", "blue", "red", "blue", "red")) +
  facet_grid(.~Treatment) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  ggsave(saveImg, units = "cm", height = 12, width = 40)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# 원본 데이터에서 분포와 아웃라이어 바운드 선을 표현한 바이올린 플롯
mainTitle = sprintf("%s", "3.air_outlier")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(air_data, aes(x = Month, y = Temperature, fill = Treatment)) +
  geom_violin(alpha = 0.5, scale = "width") +  # 온도의 분포를 보여줌
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.8) +  # 중간 요약 정보 제공
  geom_line(data = outlier_bounds, aes(x = Month, y = Lower_Bound, group = Treatment), linetype = "dashed", color = "black") +
  geom_line(data = outlier_bounds, aes(x = Month, y = Upper_Bound, group = Treatment), linetype = "dashed", color = "black") +
  labs(title = "Air temperature, full data",
       x = "Month",
       y = "Temperature (°C)") +
  scale_fill_manual(values=c("blue", "red", "blue", "red", "blue", "red")) +
  facet_grid(.~Treatment) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  ggsave(saveImg, units = "cm", height = 12, width = 40)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

soil_temp_filtered$group <- "Soil"
air_data$group <- "Air"

temp_compare <- rbind(soil_temp_filtered[,c(1,2,3,4,9)], air_data)

temp_summary <- temp_compare %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(group, date, Treatment) %>%
  summarise(mean = mean(Temperature, na.rm = T))

# 4.soil_vs_air_temp
mainTitle = sprintf("%s", "4.soil_vs_air_temp")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(temp_summary) +
  geom_line(aes(x = date, y = mean, color = group), linewidth = 0.5, alpha = 0.6) + # 연한 색으로 설정
  geom_smooth(aes(x = date, y = mean, color = group, fill = group), 
              method = "loess", se = TRUE, linewidth = 1.2, alpha = 0.3) + # 신뢰 구간 투명도 조정
  labs(
    x = "Month",
    y = "Temperature (°C)",
    color = "Group",
    fill = "Group"
  ) +
  scale_color_manual(values = c("#56A4E9", "#D69F00")) + # 추세선 색상
  scale_fill_manual(values = c("#56A4E9", "#D69F00")) + # 신뢰 구간 색상, 선과 일치
  scale_x_date(date_breaks = "1 month", date_labels = "%m") + # X축 형식
  facet_grid(. ~ Treatment) +
  theme_bw() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  ggsave(saveImg, units = "cm", height = 12, width = 40)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ================================================
# figure5,6,7,8.R
# ================================================
##### Figure.5  토양온도와 토양호흡 비교 #####
# mt <- read.csv("mt.csv")
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "mt.csv"))
mt <- read.csv(fileInfo)

mt$Date <- as.Date(mt$Date, origin = "1899-12-30")
mt$season <- factor(mt$season, levels = c("sp", "s", "f"))
mt$Heating[mt$Heating=="NW"] <- "N"
mt$Treatment <- paste0(mt$PPT_control, mt$Heating)

# NA 값 제거 (temp와 flux가 NA가 아닌 값만 사용)
mt_outlier <- mt %>%
  filter(season %in% c("sp", "s", "f"), !is.na(temp), !is.na(flux)) %>%
  select(Date, temp, flux, season, PPT_control, Heating, Treatment)  %>%
  group_by(Date) %>%
  mutate(
    flux_lower = quantile(flux, 0.25, na.rm = TRUE) -
      1.5 * (quantile(flux, 0.75, na.rm = TRUE) - quantile(flux, 0.25, na.rm = TRUE)),
    flux_upper = quantile(flux, 0.75, na.rm = TRUE) +
      1.5 * (quantile(flux, 0.75, na.rm = TRUE) - quantile(flux, 0.25, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate(is_outlier = 
           flux < flux_lower | flux > flux_upper) %>%
  mutate(data_type = ifelse(is_outlier, "removed", "clean"))

mt_filtered <- mt_outlier[mt_outlier$is_outlier == F, c(1:7)]
mt_filtered$logflux <- log(mt_filtered$flux + 0.001)

# 5.mt_outlier
mainTitle = sprintf("%s", "5.mt_outlier")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(mt_outlier, aes(x = temp, y = flux, color = data_type)) +
  geom_point(size=0.8) +
  facet_grid(. ~ Date, scale = "free_x") + #scale = "free"
  scale_color_manual(values = c("clean" = "blue", "removed" = "red")) +
  labs(
    x = "Temperature (°C)",
    y = "CO2 flux (g/m^2/hr)",
    color = "Data Type"
  ) +
  theme_bw() +
  ggsave(saveImg, units = "cm", height = 8, width = 40)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# 5.mt_outlier_ln변환
mainTitle = sprintf("%s", "5.mt_outlier_ln변환")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(mt_outlier, aes(x = temp, y = log(flux), color = data_type)) +
  geom_point(size=0.8) +
  facet_grid(. ~ Date, scale = "free_x") + #scale = "free"
  scale_color_manual(values = c("clean" = "blue", "removed" = "red")) +
  labs(
    x = "Temperature (°C)",
    y = "ln (CO2 flux) (g/m^2/hr)",
    color = "Data Type"
  ) +
  theme_bw() + 
  ggsave(saveImg, units = "cm", height = 8, width = 40)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

##### 토양온도 vs 토양 호흡 #####
# 선형 회귀 분석 (각 season별 R^2 및 Slope 계산)
r2_values <- mt_filtered %>%
  group_by(season) %>%
  summarise(
    r2 = round(summary(lm(flux ~ temp, data = pick(flux, temp)))$r.squared, 3),
    slope = round(coef(lm(flux ~ temp, data = pick(flux, temp)))[2], 3),
    x_pos = min(temp, na.rm = TRUE),
    y_pos = max(flux, na.rm = TRUE) * 0.8
  )

# ggplot으로 산점도 + 회귀선 그리기
mainTitle = sprintf("%s", "6.계절별 토양온도 vs 토양호흡")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

p <- ggplot(mt_filtered, aes(x = temp, y = flux)) +
  geom_point(alpha = 0.6, color = "blue") +  # 산점도
  geom_smooth(method = "lm", se = T, color = "red", linetype = "dashed") +  # 회귀선
  facet_wrap(~ season, scales = "free_x",  # X축을 자유롭게 설정
             labeller = as_labeller(c(sp = "Spring (3-5)", s = "Summer (6-8)", f = "Fall (9-11)"))) +  
  labs(x = "Ts (°C)", 
       y = "CO2 flux (g/m^2/hr)") +
  theme_bw()

# R^2 값 추가 (위치 및 크기 조정)
p + geom_text(data = r2_values, aes(x = x_pos, 
                                    y = c(0.3,0,0.4), 
                                    label = paste("Slope:", slope, "\nR²:", r2)),
              inherit.aes = FALSE, color = "black", size = 3, hjust = 0,      show.legend = FALSE ) +
  ggsave(saveImg, units="cm", width=20,height=8)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ggplot으로 산점도 + 회귀선 그리기
mainTitle = sprintf("%s", "6.계절별 토양온도 vs 토양호흡 다른 버전")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

p <- ggplot(mt_filtered, aes(x = temp, y = flux, color=season, fill=season)) +
  geom_point(alpha = 0.6) +  # 산점도
  geom_smooth(method = "lm", se = T, linetype = "dashed") +  # 회귀선
  labs(x = "Ts (°C)", 
       y = "CO2 flux (g/m^2/hr)") +
  theme_bw() +
  scale_color_discrete(labels = c("sp" = "Spring (3-5)",
                                  "s"  = "Summer (6-8)",
                                  "f"  = "Fall (9-11)")) +
  scale_fill_discrete(labels = c("sp" = "Spring (3-5)",
                                 "s"  = "Summer (6-8)",
                                 "f"  = "Fall (9-11)"))

# R^2 값 추가 (위치 및 크기 조정)
p + 
  geom_text(
    data = r2_values,
    aes(
      x = c(0, 15, 10),         # r2_values에 미리 계산해 둔 x 위치
      y = c(0.4, 0.7, 0.5),         # r2_values에 미리 계산해 둔 y 위치
      label = paste("Slope:", slope, "\nR²:", r2),
      color = season     # <-- season에 따라 글씨 색상이 달라짐
    ),
    inherit.aes = FALSE, # 메인 그래프의 aes 맵핑을 상속받지 않음
    size = 4,
    hjust = 0,      show.legend = FALSE 
  ) +
  ggsave(saveImg, units="cm", width=15,height=10)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


##### 토양온도 vs 토양 호흡 log 변환#####
# 선형 회귀 분석 (각 season별 R^2 및 Slope 계산)
r2_values <- mt_filtered %>%
  group_by(season) %>%
  summarise(
    r2 = round(summary(lm(logflux ~ temp, data = pick(logflux, temp)))$r.squared, 3),
    slope = round(coef(lm(logflux ~ temp, data = pick(logflux, temp)))[2], 3),
    x_pos = min(temp, na.rm = TRUE),
    y_pos = max(logflux, na.rm = TRUE) * 0.8
  )

# ggplot으로 산점도 + 회귀선 그리기
mainTitle = sprintf("%s", "6.계절별 토양온도 vs 토양호흡 log 변환")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

p <- ggplot(mt_filtered, aes(x = temp, y = logflux)) +
  geom_point(alpha = 0.6, color = "blue") +  # 산점도
  geom_smooth(method = "lm", se = T, color = "red", linetype = "dashed") +  # 회귀선
  facet_wrap(~ season, scales = "free_x",  # X축을 자유롭게 설정
             labeller = as_labeller(c(sp = "Spring (3-5)", s = "Summer (6-8)", f = "Fall (9-11)"))) +  
  labs(x = "Ts (°C)", 
       y = "ln (CO2 flux) (g/m^2/hr)") +
  theme_bw()

# R^2 값 추가 (위치 및 크기 조정)
p + geom_text(data = r2_values, aes(x = x_pos, 
                                    y = c(-1,-2,-1), 
                                    label = paste("Slope:", slope, "\nR²:", r2)),
              inherit.aes = FALSE, color = "black", size = 3, hjust = 0,      show.legend = FALSE ) +
  ggsave(saveImg, units="cm", width=20,height=8)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


##### 계절별, Heating별 토양온도 vs 토양호흡 #####
# 선형 회귀 분석 (각 season별 R^2 및 Slope 계산)
r2_values <- mt_filtered %>%
  group_by(season, Heating) %>%
  summarise(
    r2 = round(summary(lm(flux ~ temp, data = pick(flux, temp)))$r.squared, 3),
    slope = round(coef(lm(flux ~ temp, data = pick(flux, temp)))[2], 3),
    x_pos = min(temp, na.rm = TRUE),
    y_pos = max(flux, na.rm = TRUE) * 0.8
  )

# ggplot으로 산점도 + 회귀선 그리기
mainTitle = sprintf("%s", "7.계절별, Heating별 토양온도 vs 토양호흡")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

p <- ggplot(mt_filtered, aes(x = temp, y = flux, color = Heating, fill=Heating)) +
  geom_point(alpha = 0.6) +  # 산점도
  geom_smooth(method = "lm", se = T, linetype = "dashed") +  # 회귀선
  facet_wrap(. ~season, scales = "free_x",
             labeller = labeller(season=c("sp" = "Spring (3-5)", "s"  = "Summer (6-8)", "f"  = "Fall (9-11)" )))+  
  labs(x = "Ts (°C)", 
       y = "CO2 flux (g/m^2/hr)") +
  theme_bw()  +
  scale_color_manual(values = c("red", "blue")) +
  scale_fill_manual(values = c("red", "blue"))

# R^2 값 추가 (위치 및 크기 조정)
p + 
  geom_text(
    data = r2_values,
    aes(
      x = c(0,0,20,20,0,0),         # r2_values에 미리 계산해 둔 x 위치
      y = c(0.5,0.35,0.1,0.-0.05,0.5,0.35),         # r2_values에 미리 계산해 둔 y 위치
      label = paste("Slope:", slope, "\nR²:", r2),
      color = Heating     # <-- season에 따라 글씨 색상이 달라짐
    ),
    inherit.aes = FALSE, # 메인 그래프의 aes 맵핑을 상속받지 않음
    size = 4,
    hjust = 0,      show.legend = FALSE 
  ) +
  ggsave(saveImg, units="cm", width=24,height=10)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

##### 계절별, Heating별 토양온도 vs 토양호흡 다른 버전#####
mainTitle = sprintf("%s", "7.계절별, Heating별 토양온도 vs 토양호흡 다른 버전")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

# ggplot으로 산점도 + 회귀선 그리기
p <- ggplot(mt_filtered, aes(x = temp, y = flux, color = season)) +
  geom_point(alpha = 0.6) +  # 산점도
  geom_smooth(method = "lm", color = "black", se = T) +  # 회귀선
  facet_grid(season~Heating) +  
  labs(x = "Ts (°C)", 
       y = "CO2 flux (g/m^2/hr)") +
  theme_bw()  +
  scale_color_discrete(labels = c("sp" = "Spring (3-5)",
                                  "s"  = "Summer (6-8)",
                                  "f"  = "Fall (9-11)"))
# R^2 값 추가 (위치 및 크기 조정)
p + 
  geom_text(
    data = r2_values, color = "black",
    aes(
      x = 0,         # r2_values에 미리 계산해 둔 x 위치
      y = 0.7,         # r2_values에 미리 계산해 둔 y 위치
      label = paste("Slope:", slope, "\nR²:", r2),
      color = season     # <-- season에 따라 글씨 색상이 달라짐
    ),
    inherit.aes = FALSE, # 메인 그래프의 aes 맵핑을 상속받지 않음
    size = 4,
    hjust = 0, 
    show.legend = FALSE 
  ) +
  ggsave(saveImg, units="cm", width=15,height=15)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")



##### 계절별, Treatment별 토양온도 vs 토양호흡 #####
# 선형 회귀 분석 (각 season별 R^2 및 Slope 계산)
r2_values <- mt_filtered %>%
  group_by(season, Treatment) %>%
  summarise(
    r2 = round(summary(lm(flux ~ temp, data = pick(flux, temp)))$r.squared, 3),
    slope = round(coef(lm(flux ~ temp, data = pick(flux, temp)))[2], 3),
    x_pos = min(temp, na.rm = TRUE),
    y_pos = max(flux, na.rm = TRUE) * 0.8
  )

# ggplot으로 산점도 + 회귀선 그리기
mainTitle = sprintf("%s", "8.계절별, Treatment별 토양온도 vs 토양호흡")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

p <- ggplot(mt_filtered, aes(x = temp, y = flux, color = season, fill=season)) +
  geom_point(alpha = 0.6) +  # 산점도
  geom_smooth(method = "lm", se = T, linetype = "dashed") +  # 회귀선
  facet_grid(~Treatment) +  
  labs(x = "Ts (°C)", 
       y = "CO2 flux (g/m^2/hr)") +
  theme_bw()  +
  scale_color_discrete(labels = c("sp" = "Spring (3-5)",
                                  "s"  = "Summer (6-8)",
                                  "f"  = "Fall (9-11)")) +
  scale_fill_discrete(labels = c("sp" = "Spring (3-5)",
                                 "s"  = "Summer (6-8)",
                                 "f"  = "Fall (9-11)"))

# R^2 값 추가 (위치 및 크기 조정)
p + 
  geom_text(
    data = r2_values,
    aes(
      x = rep(c(0,24,12),each=6),         # r2_values에 미리 계산해 둔 x 위치
      y = 0.7,         # r2_values에 미리 계산해 둔 y 위치
      label = paste("Slope:", slope, "\nR²:", r2),
      color = season     # <-- season에 따라 글씨 색상이 달라짐
    ),
    inherit.aes = FALSE, # 메인 그래프의 aes 맵핑을 상속받지 않음
    size = 3,
    hjust = 0,      show.legend = FALSE 
  ) + 
  ggsave(saveImg, units="cm", width=45,height=8)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

##### 계절별, Treatment별 토양온도 vs 토양호흡 다른 버전 #####
# ggplot으로 산점도 + 회귀선 그리기
mainTitle = sprintf("%s", "8.계절별, Treatment별 토양온도 vs 토양호흡 다른 버전")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

p <- ggplot(mt_filtered, aes(x = temp, y = flux, color = season)) +
  geom_point(alpha = 0.6) +  # 산점도
  geom_smooth(method = "lm", color = "black", se = T) +  # 회귀선
  facet_grid(season~Treatment) +  
  labs(x = "Ts (°C)", 
       y = "CO2 flux (g/m^2/hr)") +
  theme_bw()  +
  scale_color_discrete(labels = c("sp" = "Spring (3-5)",
                                  "s"  = "Summer (6-8)",
                                  "f"  = "Fall (9-11)"))
# R^2 값 추가 (위치 및 크기 조정)
p + 
  geom_text(
    data = r2_values, color = "black",
    aes(
      x = 0,         # r2_values에 미리 계산해 둔 x 위치
      y = 0.7,         # r2_values에 미리 계산해 둔 y 위치
      label = paste("Slope:", slope, "\nR²:", r2),
      color = season     # <-- season에 따라 글씨 색상이 달라짐
    ),
    inherit.aes = FALSE, # 메인 그래프의 aes 맵핑을 상속받지 않음
    size = 4,
    hjust = 0, 
    show.legend = FALSE 
  ) +
  ggsave(saveImg, units="cm", width=30,height=15)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ================================================
# figure9.R
# ================================================
# mt <- read.csv("mt.csv")

# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "mt.csv"))
# mt <- read.csv(fileInfo)

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "20250130_2023년_마스터테이블_문의.xlsx"))
orgData = openxlsx::read.xlsx(fileInfo)
mt = orgData

mt$Date <- as.Date(mt$Date, origin = "1899-12-30")
mt$VWC <- as.numeric(mt$VWC)
mt$season <- factor(mt$season, levels = c("sp", "s", "f"))
mt$Heating[mt$Heating=="NW"] <- "N"
mt$Treatment <- paste0(mt$PPT_control, mt$Heating)

# NA 값 제거 (temp와 flux가 NA가 아닌 값만 사용)
mt_outlier <- mt %>%
  filter(season %in% c("sp", "s", "f"), !is.na(temp), !is.na(flux)) %>%
  select(Date, temp, VWC, flux, season, PPT_control, Heating, Treatment)  %>%
  dplyr::mutate(
    flux = log(flux)
  ) %>% 
  group_by(Date) %>%
  mutate(
    flux_lower = quantile(flux, 0.25, na.rm = TRUE) -
      1.5 * (quantile(flux, 0.75, na.rm = TRUE) - quantile(flux, 0.25, na.rm = TRUE)),
    flux_upper = quantile(flux, 0.75, na.rm = TRUE) +
      1.5 * (quantile(flux, 0.75, na.rm = TRUE) - quantile(flux, 0.25, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate(is_outlier = 
           flux < flux_lower | flux > flux_upper) %>%
  mutate(data_type = ifelse(is_outlier, "removed", "clean"))

mt_filtered <- mt_outlier[mt_outlier$is_outlier == F, c(1:8)]
mt_filtered$logflux <- log(mt_filtered$flux + 0.001)

# 1) 요약: all-NA인 경우를 NA_real_로 바꾸는 if문 사용
mt_daily <- mt_filtered %>%
  group_by(Date, PPT_control, Heating) %>%
  summarise(
    flux_mean = if (all(is.na(flux))) NA_real_ else mean(flux, na.rm = TRUE),
    flux_min  = if (all(is.na(flux))) NA_real_ else min(flux, na.rm = TRUE),
    flux_max  = if (all(is.na(flux))) NA_real_ else max(flux, na.rm = TRUE),
    
    temp_mean = if (all(is.na(temp))) NA_real_ else mean(temp, na.rm = TRUE),
    temp_min  = if (all(is.na(temp))) NA_real_ else min(temp, na.rm = TRUE),
    temp_max  = if (all(is.na(temp))) NA_real_ else max(temp, na.rm = TRUE),
    
    VWC_mean = if (all(is.na(VWC))) NA_real_ else mean(VWC, na.rm = TRUE),
    VWC_min  = if (all(is.na(VWC))) NA_real_ else min(VWC, na.rm = TRUE),
    VWC_max  = if (all(is.na(VWC))) NA_real_ else max(VWC, na.rm = TRUE),
    
    .groups = "drop"
  )


# 2) ggplot용 설정
shape_vals <- c("C" = 16,  # ●
                "L" = 17,  # ▲
                "S" = 15)  # ■

color_vals <- c("W" = "red",
                "N" = "blue")

# (a) flux plot
mainTitle = sprintf("%s", "9.토양호흡_토양온도_토양수분")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

breaksDateList <- sort(c(
  seq(as.Date("2023-01-01"), as.Date("2023-12-15"), by = "1 month"),
  seq(as.Date("2023-01-10"), as.Date("2023-12-10"), by = "1 month"),
  seq(as.Date("2023-01-20"), as.Date("2023-12-20"), by = "1 month")
))

p_flux <- ggplot(mt_daily,
                 aes(x = Date, 
                     y = flux_mean,
                     # y = log(flux_mean), 
                     shape = PPT_control, 
                     color = Heating,
                     group = interaction(PPT_control, Heating))) +
  geom_line(na.rm = TRUE) +
  geom_pointrange(aes(ymin = flux_min, ymax = flux_max), na.rm = TRUE) +
  # geom_pointrange(aes(ymin = log(flux_min), ymax = log(flux_max)), na.rm = TRUE) +
  scale_shape_manual(values = shape_vals) +
  scale_color_manual(values = color_vals) +
  # scale_x_date(date_breaks = "1 month", date_labels = "%m") +
  scale_x_date(breaks = breaksDateList, date_labels = "%m\n%d") +
  labs(x = NULL, 
       # y = expression(CO[2]~flux~"(" * g~m^-2~hr^-1 * ")")
       y = expression(ln~CO[2]~flux~"(" * g~m^-2~hr^-1 * ")")
  ) +
  theme_classic() +
  theme(
    axis.text.x  = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# (b) temp plot
p_temp <- ggplot(mt_daily,
                 aes(x = Date,
                     y = temp_mean,
                     shape = PPT_control,
                     color = Heating,
                     group = interaction(PPT_control, Heating))) +
  geom_line(na.rm = TRUE) +
  geom_pointrange(aes(ymin = temp_min, ymax = temp_max), na.rm = TRUE) +
  scale_shape_manual(values = shape_vals) +
  scale_color_manual(values = color_vals) +
  # scale_x_date(date_breaks = "1 month", date_labels = "%m") +
  scale_x_date(breaks = breaksDateList, date_labels = "%m\n%d") +
  labs(x = NULL, 
       y = "Ts (°C)") +
  theme_classic() +
  theme(
    axis.text.x  = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# (c) VWC plot
p_vwc <- ggplot(mt_daily,
                aes(x = Date,
                    y = VWC_mean,
                    shape = PPT_control,
                    color = Heating,
                    group = interaction(PPT_control, Heating))) +
  geom_line(na.rm = TRUE) +
  geom_pointrange(aes(ymin = VWC_min, ymax = VWC_max), na.rm = TRUE) +
  scale_shape_manual(values = shape_vals) +
  scale_color_manual(values = color_vals) +
  # scale_x_date(date_breaks = "1 month", date_labels = "%m") +
  scale_x_date(breaks = breaksDateList, date_labels = "%m\n%d") +
  labs(x = "Month", 
       y = "Soil Water Content (\u03B8 %)") +
  theme_classic()

# 3) 3개 플롯을 세로로 결합
combined_plot <- (p_flux / p_temp / p_vwc) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")
  # theme(legend.position = "right")

# 4) 출력
combined_plot +
  ggsave(saveImg, units="cm", width=15,height=15)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ================================================
# figure10,11.R
# ================================================
# prec <- read.csv("prec.csv")
# vwc <- read.csv("VWC.csv")

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "prec.csv"))
prec <- read.csv(fileInfo)

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "VWC.csv"))
vwc <- read.csv(fileInfo)

# timestamp를 날짜 형식으로 변환
vwc <- vwc %>%
  mutate(date = as.Date(timestamp, format = "%Y-%m-%d %H:%M")) %>%
  group_by(date) %>%
  summarise(
    CN = mean(c(CN1, CN2, CN3), na.rm = TRUE),
    CW = mean(c(CW1, CW2, CW3), na.rm = TRUE),
    LN = mean(c(LN1, LN2, LN3), na.rm = TRUE),
    LW = mean(c(LW1, LW2, LW3), na.rm = TRUE),
    SN = mean(c(SN1, SN2, SN3), na.rm = TRUE),
    SW = mean(c(SW1, SW2, SW3), na.rm = TRUE)
  )

prec <- prec %>%
  mutate(date = as.Date(timestamp, format = "%Y-%m-%d %H:%M")) %>%
  group_by(date) %>%
  summarise(prec = sum(prec, na.rm = TRUE))

# 데이터를 long 형식으로 변환
vwc_long <- vwc %>%
  pivot_longer(cols = -date, names_to = "variable", values_to = "value") %>%
  mutate(
    PPT_control = substr(variable, 1, 1), # 첫 글자로 그룹(C, L, S)
    Heating = substr(variable, 2, 2)  # 두 번째 글자로 유형(N, W)
  )

summary(vwc_long$value)
summary(prec)

# prec$prec

scaler <- 10

# 10.강우량 vs 강우조건별 토양수분
mainTitle = sprintf("%s", "10.강우량 vs 강우조건별 토양수분")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot() +
  # 강수량 선 그래프 (왼쪽 y축)
  geom_col(data = prec, aes(x = date, y = prec), fill = "dodgerblue2", alpha = 0.6) +
  # 토양 수분 선 그래프 (오른쪽 y축)
  geom_line(data = vwc_long, 
            aes(x = date, y = value * 100 * scaler, color = PPT_control, linetype = Heating), size = 0.7) +
  # 색상과 선형 지정
  scale_color_manual(values = c("C" = "blue", "L" = "darkgreen", "S" = "red")) +
  scale_linetype_manual(values = c("N" = "dotted", "W" = "solid")) +
  # x축 포맷 변경 (Month만 표시)
  scale_x_date(date_labels = "%m", date_breaks = "1 month") +
  # y축 이름과 sec.axis 설정
  scale_y_continuous(name = "Precipitation (mm)",
                     sec.axis = sec_axis(~ . / scaler, name = "Soil Water Content (θ%)")) +
  # 레이블과 테마 설정
  labs(x = "Month", color = "PPT_control", linetype = "Heating") +
  theme_bw() +
  theme(
    axis.title.y.right = element_text(color = "black"),
    axis.title.y.left = element_text(color = "black"),
    legend.position = "right"
  ) +
  ggsave(saveImg, units="cm", width=20,height=12)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

vwc_summary <- sapply(vwc[,-1], mean) * 100
N <- c(vwc_summary[c(1,3,5)], vwc_summary[3]/vwc_summary[1] * 100, vwc_summary[5]/vwc_summary[1] * 100)
W <- c(vwc_summary[c(2,4,6)], vwc_summary[4]/vwc_summary[2] * 100, vwc_summary[6]/vwc_summary[2] * 100)
NW_mean <- colMeans(rbind(N, W))
NW_result <- rbind(N,W,NW_mean)
colnames(NW_result) <- c("C", "L", "S", "CvsL", "CvsS")

# write.csv(rbind(N,W,NW_mean), "토양수분비교표.csv", fileEncoding = "CP949")

saveFile = sprintf("%s/%s/%s.csv", globalVar$figPath, serviceName, "토양수분비교표")
dir.create(fs::path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
write.csv(rbind(N,W,NW_mean), saveFile, fileEncoding = "CP949")




names(vwc_long)[2] <- "Treatment"
##### 조건별 vwc 분포  #####
mainTitle = sprintf("%s", "11.조건별 토양수분")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(data = vwc_long) +
  geom_boxplot_pattern(aes(x = Treatment, y = value * 100, fill = Treatment, pattern = Treatment),
                       pattern_colour = "white",       # 패턴 색상을 흰색으로 설정
                       pattern_fill = "white",         # 패턴 채우기 색상 설정
                       size = 1, alpha=0.8) +                     # 박스플롯의 선 두께 설정
  scale_fill_manual(values = c("Tair" = "white", 
                               "CW" = "blue", "CN" = "blue", 
                               "LW" = "darkgreen", "LN" = "darkgreen", 
                               "SW" = "red", "SN" = "red")) +
  scale_pattern_manual(values = c("Tair" = "none", 
                                  "CW" = "none", "CN" = "stripe", 
                                  "LW" = "none", "LN" = "stripe", 
                                  "SW" = "none", "SN" = "stripe")) +
  ylab("Soil Water Content (θ%)")+
  theme_bw() +
  theme(legend.position = "none") +
  ggsave(saveImg, units = "cm", height = 15, width = 15)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

##### 조건별 온도 분포  #####
# temp_compare <- read.csv("soil_vs_air_temp_raw.csv")
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "soil_vs_air_temp_raw.csv"))
temp_compare <- read.csv(fileInfo)

# 시간 문자열 정리
temp_compare$timestamp <- gsub("오전 ", "AM ", temp_compare$timestamp)
temp_compare$timestamp <- gsub("오후 ", "PM ", temp_compare$timestamp)
temp_compare$timestamp <- gsub("시", ":", temp_compare$timestamp)
temp_compare$timestamp <- gsub("분", ":", temp_compare$timestamp)
temp_compare$timestamp <- gsub("초", "", temp_compare$timestamp)

# 공백 정리
temp_compare$timestamp <- gsub("\\s+", " ", temp_compare$timestamp)  # 연속된 공백 제거
temp_compare$timestamp <- gsub(": ", ":", temp_compare$timestamp)   # 잘못된 공백 제거

# 문자열을 datetime 형식으로 변환
temp_compare$timestamp <- parse_date_time(temp_compare$timestamp, orders = "m. d. y p I:M:S ")

# 데이터 불러오기 (예제에서는 temp_compare 데이터 프레임을 가정)
# -20도 이하의 값은 NA로 변환
temp_compare[temp_compare < -20] <- NA

# Soil 데이터만 선택
soil_data <- temp_compare %>% 
  select(timestamp, starts_with("Soil_")) %>% 
  pivot_longer(cols = -timestamp, names_to = "Treatment", values_to = "Temperature")

# Treatment 그룹화 (C, L, S 조합과 N, W 조합을 묶어 6개의 그룹 생성)
soil_data <- soil_data %>%
  mutate(Treatment = case_when(
    grepl("CW", Treatment) ~ "CW",
    grepl("CN", Treatment) ~ "CN",
    grepl("LW", Treatment) ~ "LW",
    grepl("LN", Treatment) ~ "LN",
    grepl("SW", Treatment) ~ "SW",
    grepl("SN", Treatment) ~ "SN",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Treatment))

# 월별 데이터 추가
soil_data <- soil_data %>%
  mutate(Month = format(timestamp, "%m")) %>%
  filter(!is.na(Temperature))

# 아웃라이어 바운드 계산 (평균 ± 3SD 범위)
outlier_bounds <- soil_data %>%
  group_by(Month, Treatment) %>%
  summarise(
    Mean = mean(Temperature, na.rm = TRUE),
    SD = sd(Temperature, na.rm = TRUE),
    Lower_Bound = Mean - 3 * SD,
    Upper_Bound = Mean + 3 * SD,
    .groups = "drop"
  )

# 아웃라이어 제거된 데이터
soil_temp_filtered <- soil_data %>%
  left_join(outlier_bounds, by = c("Month", "Treatment")) %>%
  filter(!is.na(Lower_Bound) & !is.na(Upper_Bound) & Temperature >= Lower_Bound & Temperature <= Upper_Bound) %>%
  select(Treatment, Temperature)

# Air 데이터 선택
# air_data <- read.csv("temp.csv")
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "temp.csv"))
air_data <- read.csv(fileInfo)

air_data$timestamp <- "Tair"
names(air_data) <- c("Treatment", "Temperature")

temp_full <- rbind(soil_temp_filtered, air_data)
temp_full <- temp_full %>%
  mutate(Treatment = factor(Treatment, levels = c("Tair", "CN", "CW", "LN", "LW", "SN", "SW")))

# ggplot으로 시각화
mainTitle = sprintf("%s", "11.조건별 토양온도")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(data = temp_full) +
  geom_boxplot_pattern(aes(x = Treatment, y = Temperature, fill = Treatment, pattern = Treatment),
                       pattern_colour = "white",       # 패턴 색상을 흰색으로 설정
                       pattern_fill = "white",         # 패턴 채우기 색상 설정
                       size = 1, alpha = 0.8) +        # 박스플롯의 선 두께 설정
  scale_fill_manual(values = c("Tair" = "white", 
                               "CW" = "blue", "CN" = "blue", 
                               "LW" = "darkgreen", "LN" = "darkgreen", 
                               "SW" = "red", "SN" = "red")) +
  scale_pattern_manual(values = c("Tair" = "none", 
                                  "CW" = "none", "CN" = "stripe", 
                                  "LW" = "none", "LN" = "stripe", 
                                  "SW" = "none", "SN" = "stripe")) +
  ylab("Temperature (°C)") +
  theme_bw() +
  theme(legend.position = "none") +
  ggsave(saveImg, units = "cm", height = 15, width = 15)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# Treatment별 평균 Temperature 계산
treatment_means <- soil_temp_filtered %>%
  group_by(Treatment) %>%         # Treatment별로 그룹화
  summarize(
    Mean_Temperature = mean(Temperature, na.rm = TRUE) # 평균 계산 (결측치 제외)
  )

# 예제 데이터 생성
treatment_means <- data.frame(
  Treatment = c("CN", "CW", "LN", "LW", "SN", "SW"),
  Mean_Temperature = c(17.8, 18.1, 16.9, 18.0, 18.6, 19.4)
)

temp_summary <- treatment_means$Mean_Temperature

C <- c(temp_summary[c(1,2)], temp_summary[2]/temp_summary[1] * 100)
L <- c(temp_summary[c(3,4)], temp_summary[4]/temp_summary[3] * 100)
S <- c(temp_summary[c(5,6)], temp_summary[6]/temp_summary[5] * 100)
CLS_mean <- colMeans(rbind(C,L,S))
CLS_result <- rbind(C,L,S,CLS_mean)
colnames(CLS_result) <- c("N", "W", "NvsW")
# write.csv(CLS_result, "토양온도비교표.csv", fileEncoding = "CP949")

saveFile = sprintf("%s/%s/%s.csv", globalVar$figPath, serviceName, "토양온도비교표")
dir.create(fs::path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
write.csv(CLS_result, saveFile, fileEncoding = "CP949")
