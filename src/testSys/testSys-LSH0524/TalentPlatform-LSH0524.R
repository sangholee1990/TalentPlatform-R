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
# R을 이용한 대기오염 데이터에서 통합대기환경 지수 계산

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0524"

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
library(readxl)
library(leaflet)
library(dplyr)

# 함수 정의
rename_locations <- function(data) {
  data <- data %>%
    mutate(지역명 = case_when(
      측정소명 %in% c("용담동", "용암동", "가덕면", "사천동", "산남동", "송정동(봉명동)", "오송읍", "오창읍") ~ "청주시",
      측정소명 %in% c("살미면", "중앙탑면", "칠금동", "호암동") ~ "충주시",
      측정소명 %in% c("단성면", "단양읍", "매포읍") ~ "단양군",
      측정소명 %in% c("영천동", "장락동", "청풍면") ~ "제천시",
      측정소명 %in% c("소이면", "음성읍") ~ "음성군",
      측정소명 %in% c("감물면", "괴산읍") ~ "괴산군",
      측정소명 %in% c("증평읍", "도안면") ~ "증평군",
      측정소명 %in% c("황간면", "영동읍") ~ "영동군",
      측정소명 %in% c("덕산읍", "진천읍") ~ "진천군",
      측정소명 == "보은읍" ~ "보은군",
      측정소명 == "옥천읍" ~ "옥천군",
      TRUE ~ "기타"
    ))
  
  return(data)
}


# 함수 정의: 지역별 평균 계산
calculate_averages <- function(data) {
  data %>%
    # group_by(지역명) %>%
    group_by(sDateYm, 지역명) %>%
    summarise(across(c("SO2", "CO", "O3", "NO2", "PM10", "PM2.5"), mean, na.rm = TRUE))
}

# 함수: 인덱스 계산
calculate_index <- function(Cp, BPLO, BPHI, ILO, IHI) {
  
  Cp_adjusted <- pmin(Cp, BPHI)
  
  Ip <- (IHI - ILO) / (BPHI - BPLO) * (Cp_adjusted - BPLO) + ILO
  
  return(Ip)
}

# 함수: 대기질 지수(CAI) 계산
calculate_CAI <- function(data) {
  data %>% 
    rowwise() %>% 
    mutate(
      SO2_index = calculate_index(SO2, 0, 0.02, 0, 50),
      SO2_index = if_else(SO2 > 0.02, calculate_index(SO2, 0.021, 0.05, 51, 100), SO2_index),
      SO2_index = if_else(SO2 > 0.05, calculate_index(SO2, 0.051, 0.15, 101, 250), SO2_index),
      SO2_index = if_else(SO2 > 0.15, calculate_index(SO2, 0.151, 1, 251, 500), SO2_index),
      
      CO_index = calculate_index(CO, 0, 2, 0, 50),
      CO_index = if_else(CO > 2, calculate_index(CO, 2.01, 9, 51, 100), CO_index),
      CO_index = if_else(CO > 9, calculate_index(CO, 9.01, 15, 101, 250), CO_index),
      CO_index = if_else(CO > 15, calculate_index(CO, 15.01, 50, 251, 500), CO_index),
      
      O3_index = calculate_index(O3, 0, 0.03, 0, 50),
      O3_index = if_else(O3 > 0.03, calculate_index(O3, 0.031, 0.09, 51, 100), O3_index),
      O3_index = if_else(O3 > 0.09, calculate_index(O3, 0.091, 0.15, 101, 250), O3_index),
      O3_index = if_else(O3 > 0.15, calculate_index(O3, 0.151, 0.6, 251, 500), O3_index),
      
      NO2_index = calculate_index(NO2, 0, 0.03, 0, 50),
      NO2_index = if_else(NO2 > 0.03, calculate_index(NO2, 0.031, 0.06, 51, 100), NO2_index),
      NO2_index = if_else(NO2 > 0.06, calculate_index(NO2, 0.061, 0.2, 101, 250), NO2_index),
      NO2_index = if_else(NO2 > 0.2, calculate_index(NO2, 0.201, 2, 251, 500), NO2_index),
      
      PM10_index = calculate_index(PM10, 0, 30, 0, 50),
      PM10_index = if_else(PM10 > 30, calculate_index(PM10, 31, 80, 51, 100), PM10_index),
      PM10_index = if_else(PM10 > 80, calculate_index(PM10, 81, 150, 101, 250), PM10_index),
      PM10_index = if_else(PM10 > 150, calculate_index(PM10, 151, 600, 251, 500), PM10_index),
      
      PM25_index = calculate_index(`PM2.5`, 0, 15, 0, 50),
      PM25_index = if_else(`PM2.5` > 15, calculate_index(`PM2.5`, 16, 35, 51, 100), PM25_index),
      PM25_index = if_else(`PM2.5` > 35, calculate_index(`PM2.5`, 36, 75, 101, 250), PM25_index),
      PM25_index = if_else(`PM2.5` > 75, calculate_index(`PM2.5`, 76, 500, 251, 500), PM25_index),
      
      CAI = max(SO2_index, CO_index, O3_index, NO2_index, PM10_index, PM25_index),
      
      num_above_moderate = sum(c(SO2_index, CO_index, O3_index, NO2_index, PM10_index, PM25_index) >= 101),
      
      final_CAI = case_when(
        num_above_moderate >= 2 ~ CAI + 50,
        num_above_moderate >= 3 ~ CAI + 75,
        TRUE ~ CAI
      )
    )
}


# 파일 읽기
air_data3 <- read_excel("충청북도_대기오염_측정자료_202003.xls")
air_data4<- read_excel("충청북도_대기오염_측정자료_202004.xls")
air_data5 <- read_excel("충청북도_대기오염_측정자료_202005.xls")
air_data6 <- read_excel("충청북도_대기오염_측정자료_202006.xls")
air_data7 <- read_excel("충청북도_대기오염_측정자료_202007.xls")
air_data8 <- read_excel("충청북도_대기오염_측정자료_202008.xls")
air_data9 <- read_excel("충청북도_대기오염_측정자료_202009.xls")
air_data10 <- read_excel("충청북도_대기오염_측정자료_202010.xls")


# 데이터 통합
data = dplyr::bind_rows(air_data3, air_data4, air_data5, air_data6, air_data7, air_data8, air_data9, air_data10) %>% 
  dplyr::mutate(
    sDateYm = substr(날짜, 1, 7)
  )

# 여러 데이터셋에 함수 적용
dataL1 = data %>% 
  rename_locations

# dataset_list에 lapply() 함수 적용하여 각 데이터셋에서 평균 계산
dataL2 = dataL1 %>% 
  calculate_averages

# 결과 확인
# 첫 번째 데이터셋에서 지역명별 평균 데이터
dataL2 %>% 
  dplyr::filter(sDateYm %in% c("2020-03"))


#결측값 제거
dataL3 = dataL2 %>% 
  na.omit()


# averages_list에 lapply() 함수 적용하여 각 데이터프레임에서 CAI 계산
dataL4 = dataL3 %>% 
  calculate_CAI


# 결과 확인
# 첫 번째 데이터셋에서 지역명별 CAI 값
dataL4 %>% 
  dplyr::filter(sDateYm %in% c("2020-03"))
