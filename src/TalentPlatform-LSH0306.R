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
# R을 이용한 서울시 아파트 실거래가 회귀분석 및 주택 가격 결정 요인

# 먼저 데이터를 수집합니다 (예 : 거주 국가의 10 년 도시 수준 데이터).
# 데이터 세트에는 주택 가격, 소득, 도시 크기 및 기타 변수가 있어야합니다.
# 그런 다음 회귀를 실행하고 
# 소득을 주택 가격으로 나눈 값으로 측정 한 주택 가격 결정 요인을 테스트 할 수 있습니다.

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0169"
contextPath = ifelse(env == "local", ".", "E:/04. TalentPlatform/Github/TalentPlatform-R")

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
  source(here::here(file.path(contextPath, "src"), "InitConfig.R"), encoding = "UTF-8")
}


#================================================
# Main
#================================================
library(ggplot2)
library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)
library(RCurl)
library(readr)
library(magrittr)
library(ggrepel)
library(colorRamps)
library(ggpubr)
library(lm.beta)
library(ggpmisc)

Sys.setlocale("LC_ALL", "English")

# 서울에서 서울특별시 법정동 코드 읽기
codeInfo = Sys.glob(paste(globalVar$mapPath, "/admCode/법정동코드_전체자료.txt", sep = "/"))

codeList = readr::read_delim(codeInfo, delim = "\t", locale = locale("ko", encoding = "EUC-KR"), col_types = "ccc") %>%
  magrittr::set_colnames(c("EMD_CD", "addr", "isUse")) %>% 
  tidyr::separate(col = "addr", into = c("d1", "d2", "d3", "d4"), sep = " ") %>%
  dplyr::mutate(
    emdCd = stringr::str_sub(EMD_CD, 1, 5)
  ) %>% 
  dplyr::filter(
    stringr::str_detect(d1, regex("서울특별시"))
    , stringr::str_detect(isUse, regex("존재"))
    , is.na(d3)
    , is.na(d4)
  )

codeDistList = codeList %>%
  dplyr::distinct(emdCd)

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0169_가구_특성정보_(+소득정보)_201211.csv", sep = "/"))
costData = readr::read_csv(file = fileInfo) %>%
  dplyr::mutate(
    emdCd = stringr::str_sub(as.character(raw_dn_cd), 1, 5)
  ) %>% 
  dplyr::group_by(emdCd) %>% 
  dplyr::summarise(
    meanCost = mean(avrg_income_amount_am, na.rm = TRUE)
  )

# ***********************************************
# 데이터 전처리
# ***********************************************
Sys.setlocale("LC_ALL", "Korean")

fileInfo = Sys.glob(paste(globalVar$outPath, "LSH0169_seoul apartment transaction.csv", sep = "/"))

dataL2 = readr::read_csv(file = fileInfo) %>% 
  readr::type_convert() %>% 
  dplyr::mutate(
    지번2 = readr::parse_number(지번)
    , emdCd = as.character(emdCd)
  ) %>% 
  dplyr::left_join(codeList, by = c("emdCd" = "emdCd")) %>%
  dplyr::left_join(costData, by = c("emdCd" = "emdCd")) %>% 
  dplyr::mutate(
    addr = stringr::str_trim(paste(d1, d2, 아파트, 지번, seq = ' '))
    , val = 거래금액 / meanCost # 연소득당 거래금액
    , val2 = 거래금액 / 전용면적 # 면적당 거래금액
  )


# *********************************************************
# 그래프 그리기(히스토그램, 상자 수염그림, 산점도 등)
# *********************************************************
# 연소득당 거래금액 따른 히스토그램
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "연소득당 거래금액 따른 히스토그램")

ggplot(dataL2, aes(x = val)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = 0.2) +
  geom_rug(aes(x = val, y = 0), position = position_jitter(height = 0)) +
  labs(x = "연소득당 거래금액", y = "밀도 함수", colour = NULL, fill = NULL, subtitle = "연소득당 거래금액 따른 히스토그램") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 12, height = 6, dpi = 600)

# 법정동에 따른 연소득당 거래금액 히스토그램
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "법정동에 따른 연소득당 거래금액 히스토그램")

ggplot(dataL3, aes(x = d2, y = meanVal, fill = meanVal)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = round(meanVal, 0)), vjust = 1.6, color = "white", size = 4) +
  labs(x = "법정동", y = "연소득당 거래금액", fill = NULL, subtitle = "법정동에 따른 연소득당 거래금액 히스토그램") +
  scale_fill_gradientn(colours = cbMatlab, na.value = NA) +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)


# 법정동에 따른 연소득당 거래금액 상자 그림
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "법정동에 따른 연소득당 거래금액 상자 그림")

ggplot(dataL2, aes(x = d2, y = val, color = d2)) +
  geom_boxplot() +
  labs(x = "법정동", y = "연소득당 거래금액", fill = NULL, subtitle = "법정동에 따른 연소득당 거래금액 상자 그림") +
  # scale_colour_gradientn(colours = cbMatlab, na.value = NA) +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)


# ***********************************************
# 주택 가격 결정 요인을 위한 회귀분석
# ***********************************************
dataL4 = dataL2 %>%
  dplyr::select(건축년도, 전용면적, 층, val2, d2, val)

#+++++++++++++++++++++++++++++++++++++++++++++++
# 전체 아파트
dataL5 = dataL4

# 중형 이상 아파트 (66 m2 이상)
# dataL5 = dataL4 %>%
#   dplyr::filter(전용면적 >= 66) %>%
#   dplyr::select(-전용면적)

# 소형 아파트 (66 m2 미만)
# dataL5 = dataL4 %>%
#   dplyr::filter(전용면적 < 66) %>%
#   dplyr::select(-전용면적)
#+++++++++++++++++++++++++++++++++++++++++++++++

# 선형회귀분석
lmFit = lm(val ~ ., data = dataL5)
summary(lmFit)

# 단계별 소거법
lmFitStep = MASS::stepAIC(lmFit, direction = "both")
summary(lmFitStep)

# Beta 회귀계수
lmBetaFit = lm.beta::lm.beta(lmFitStep)
lmBetaFit$standardized.coefficients %>% round(2) %>% sort() %>% rev()