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
# R을 이용한 KOTITI 시계열 비교

# KOTITI 기준값과 비교하여 장비1/2/3은 상이한 경향/분포를 보임

# val: KOTITI 기준값
# meanVal_193037: 장비1 평균값 (1시간 평균)
# meanVal_193044: 장비2 평균값 (1시간 평균)
# meanVal_193049: 장비3 평균값 (1시간 평균)
# cnt_193037: 장비1 총계 (1시간 평균 계산 시 총 개수)
# cnt_193044: 장비2 총계 (1시간 평균 계산 시 총 개수)
# cnt_193049: 장비3 총계 (1시간 평균 계산 시 총 개수)

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "BDWIDE2025"

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
library(ggplot2)
library(dplyr)
library(Metrics)
library(dplyr)
library(openxlsx)
library(readxl)
library(lubridate)

# 함수 정의
calibFactor = function(nActual, nPredicted, nMin, nMax, nInterval, isPlot = FALSE) {
  
  nFactor = seq(nMin, nMax, by = nInterval)
  
  # RMSE Fitting
  liResultTmp = lapply(1:length(nFactor), function(iCount) Metrics::rmse(nActual, nPredicted * nFactor[iCount]))   
  
  liResult = unlist(liResultTmp)
  
  if (isPlot == TRUE) {
    plot(liResult)   
  }
  
  # Best Factor Index
  iIndex = which(liResult == min(liResult, na.rm = TRUE))
  
  slope = nFactor[[iIndex]]
  prd = nPredicted * slope
  offset = mean(nActual - prd, na.rm = TRUE)
  
  return(c(slope = slope, offset = offset))
}


# 등급 판정 함수
determineGrade = function(results) {
  # 평가 결과에서 값 추출
  accuracy = results[["정확도(%)"]]
  rSquared = results[["결정계수(r²)"]]
  acquisitionRate = results[["자료획득률(%)"]]
  
  # 필수 값이 없으면 등급 판정 불가
  if (is.na(accuracy) || is.na(rSquared) || is.na(acquisitionRate)) {
    return("등급 판정 불가 (필수 항목 누락)")
  }
  
  # 각 항목별 등급을 숫자로 변환 (1, 2, 3, 4=등급 외)
  # 
  accuracyGrade = if (accuracy > 80) { 1 } else if (accuracy > 70) { 2 } else if (accuracy > 50) { 3 } else { 4 }
  rSquaredGrade = if (rSquared > 0.8) { 1 } else if (rSquared > 0.7) { 2 } else if (rSquared > 0.6) { 3 } else { 4 }
  acquisitionRateGrade = if (acquisitionRate > 80) { 1 } else { 4 }
  # 
  
  # 가장 낮은 등급(가장 큰 숫자)을 최종 등급으로 결정
  finalNumericGrade = max(accuracyGrade, rSquaredGrade, acquisitionRateGrade)
  
  # 숫자 등급을 문자열로 변환하여 반환
  gradeMap = c("1등급", "2등급", "3등급", "등급 외")
  return(gradeMap[finalNumericGrade])
}

# 성능 평가 함수
evaluateDevicePerformance = function(data, refCol, deviceCols) {
  # --- 데이터 준비 ---
  referenceData = data[[refCol]]
  devicesData = data[, deviceCols, drop = FALSE]
  
  # --- 1. 정확도 (Accuracy) ---
  deviceMean = rowMeans(devicesData, na.rm = TRUE)
  accuracy = mean(1 - abs(referenceData - deviceMean) / referenceData, na.rm = TRUE) * 100
  finalAccuracy = round(accuracy, 1)
  
  # --- 2. 결정계수 (R-squared) ---
  corR = cor(referenceData, deviceMean, use = "complete.obs")
  rSquared = corR^2
  finalRSquared = round(rSquared, 2)
  
  # --- 3. 상대정밀도 (Relative Precision) ---
  if (length(deviceCols) > 1) {
    rsdValues = apply(devicesData, 1, function(row) {
      meanVal = mean(row, na.rm = TRUE)
      if (is.na(meanVal) || meanVal == 0) return(NA)
      sdVal = sd(row, na.rm = TRUE)
      return((sdVal / meanVal) * 100)
    })
    precision = 100 - mean(rsdValues, na.rm = TRUE)
    finalPrecision = round(precision, 1)
  } else {
    finalPrecision = NA
  }
  
  # --- 4. 자료획득률 (Data Acquisition Rate) ---
  refCount = sum(!is.na(referenceData))
  deviceCounts = colSums(!is.na(devicesData))
  acquisitionRates = (deviceCounts / refCount) * 100
  minAcquisitionRate = min(acquisitionRates)
  finalAcquisitionRate = round(minAcquisitionRate, 1)
  
  # --- 5. 기울기(Slope) 및 절편(Intercept) 계산 ---
  model = lm(deviceMean ~ referenceData)
  coeffs = coef(model)
  slope = coeffs[2]
  intercept = coeffs[1]
  finalSlope = round(slope, 2)
  finalIntercept = round(intercept, 2)
  
  # --- 결과 반환 ---
  results = list(
    "정확도(%)" = finalAccuracy,
    "결정계수(r²)" = finalRSquared,
    # "상대정밀도(%)" = finalPrecision, # 상대정밀도는 등급 판정에서 제외됨
    "자료획득률(%)" = finalAcquisitionRate
  )
  
  # 등급 판정 함수 호출
  results[["등급"]] = determineGrade(results)
  
  return(results)
}

# 기준자료
# fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "20250507_KOTITI_PM25/*/기준측정기 데이터_250423-250504.xlsx"))
# fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "20250507_KOTITI_PM25/3차/기준측정기 데이터_*.xlsx"))
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "20250507_KOTITI_PM25/4차/기준측정기 데이터_*.xlsx"))

# fileInfo = fileList[1]
refData = tibble::tibble()
for (fileInfo in fileList) {
  # orgData = openxlsx::read.xlsx(fileInfo, sheet = "Sheet2")
  orgData = openxlsx::read.xlsx(fileInfo, sheet = "Sheet1")
  refData = dplyr::bind_rows(refData, orgData)
}

refDataL1 = refData %>% 
  dplyr::mutate(
    sYmd = stringr::str_c(연, 월, stringr::str_remove(일, "일"), 시간, sep = "-"),
    dtHour = readr::parse_datetime(sYmd, format = "%Y-%m-%d-%H")
  ) %>% 
    dplyr::rename(
      # val = "KOTITI기준측정기.농도"
      val = "KOTITI.기준측정기.데이터"
    )

refDataL2 = refDataL1 %>% 
  dplyr::select(dtHour, val)

# plot(refDataL2$dtYmd, refDataL2$val)


# 측정자료
# 193037, 193044, 193049
# fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "20250507_KOTITI_PM25/*/*_*.csv"))
# fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "20250507_KOTITI_PM25/3차/*_*.csv"))
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "20250507_KOTITI_PM25/4차/*.csv"))

# fileInfo = fileList[1]
# mesData = tibble::tibble()
# for (fileInfo in fileList) {
#   fileNameSplit = fileInfo %>% 
#     fs::path_file() %>% 
#     tools::file_path_sans_ext() %>% 
#     tools::file_path_sans_ext() %>% 
#     stringr::str_split(pattern = "_") %>% 
#     unlist()
#   
#   orgData = readr::read_csv(fileInfo, col_names = FALSE, skip = 1, show_col_types = FALSE) %>% 
#     dplyr::mutate(
#       sYmd = stringr::str_c(fileNameSplit[2], X2, sep = " "),
#       key = fileNameSplit[3]
#     ) %>% 
#     dplyr::mutate(
#       dtYmd = readr::parse_datetime(sYmd, format = "%Y%m%d %H:%M:%S")
#     ) %>% 
#     tidyr::separate(col = X3, into = c("type", "val"), sep = "=", convert = TRUE)
#     
#   mesData = dplyr::bind_rows(mesData, orgData)
# }

fileInfo = fileList[1]
mesData = readr::read_csv(fileInfo, col_names = TRUE, skip = 2, show_col_types = FALSE)

mesDataL1 = mesData %>% 
  dplyr::mutate(
    dtHour = lubridate::floor_date(MeasuredTime, unit = "hour"),
    key = "solarmy",
  ) %>%
  dplyr::group_by(key, dtHour) %>%
  dplyr::summarise(
    meanVal = mean(PM2.5, na.rm = TRUE),
    cnt = n()
  ) %>%
  dplyr::ungroup()

mesDataL2 = mesDataL1 %>%
  tidyr::pivot_wider(
    id_cols = dtHour,
    names_from = key,
    values_from = c(meanVal, cnt)
  )

# mesDataL2 = mesDataL1

# plot(refDataL2$dtHour, refDataL2$val)
# plot(mesDataL2$dtHour, mesDataL2$meanVal_193037)
# plot(mesDataL2$dtHour, mesDataL2$meanVal_193044)
# plot(mesDataL2$dtHour, mesDataL2$meanVal_193049)

# 데이터 병합
data = dplyr::left_join(refDataL2, mesDataL2, by = c("dtHour" = "dtHour"))

dataL1 = data %>%
  dplyr::select(dtHour, val, meanVal_solarmy) %>% 
  na.omit()

dataL2 = dataL1 %>%
  dplyr::select(-starts_with("cnt_")) %>% 
  tidyr::pivot_longer(
    cols = c(val, starts_with("meanVal_")),
    names_to = "key",
    values_to = "val"
  )

mainTitle = sprintf("%s", "KOTITI 시간에 따른 PM25 시계열")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL2, aes(x = dtHour, y = val, color = key, group = key)) +
  geom_line() +
  geom_point() +
  labs(
    # title = "시간에 따른 값 변화",
    x = "시간 [월-일 시]",
    y = "PM2.5",
    color = NULL
  ) +
  theme(
    # plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "top"
  ) +
   scale_x_datetime(breaks = "1 days", date_breaks = "1 days", date_labels = "%m-%d", limits = c(as.POSIXct("2025-06-13 00:00:00", tz="KST"), as.POSIXct("2025-06-27 00:00:00", tz="KST"))) +
  # scale_x_datetime(date_breaks = "1 days", date_labels = "%m-%d") +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# 보정 데이터
calibFactor = getCalibFactor(dataL1$val, dataL1$meanVal_solarmy, -10, 10, 0.001, isPlot=TRUE)


dataL3 = dataL1 %>%
  dplyr::mutate(
    refVal_solarmy = (calibFactor['slope'] * meanVal_solarmy) + calibFactor['offset']
  ) 

# 평균제곱근오차
Metrics::rmse(dataL3$val, dataL3$meanVal_solarmy)
Metrics::rmse(dataL3$val, dataL3$refVal_solarmy)

dataL4 = dataL3 %>% 
  tidyr::pivot_longer(
    cols = c(val, starts_with("meanVal_"), starts_with("refVal_")),
    # cols = c(val, starts_with("refVal_")),
    names_to = "key",
    values_to = "val"
  )

saveFile = sprintf("%s/%s/%s.xlsx", globalVar$outPath, serviceName, "KOTITI 시간에 따른 PM25 시계열")
dir.create(fs::path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Sheet1")
openxlsx::writeData(wb, "Sheet1", dataL3, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
openxlsx::saveWorkbook(wb, file = saveFile, overwrite = TRUE)
cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")


mainTitle = sprintf("%s", "KOTITI 시간에 따른 PM25 시계열")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL4, aes(x = dtHour, y = val, color = key, group = key)) +
  geom_line() +
  geom_point() +
  labs(
    # title = "시간에 따른 값 변화",
    x = "시간 [월-일 시]",
    y = "PM2.5",
    color = NULL
  ) +
  theme(
    # plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "top"
  ) +
  scale_x_datetime(breaks = "1 days", date_breaks = "1 days", date_labels = "%m-%d", limits = c(as.POSIXct("2025-06-13 00:00:00", tz="KST"), as.POSIXct("2025-06-27 00:00:00", tz="KST"))) +
  # scale_x_datetime(date_breaks = "1 days", date_labels = "%m-%d") +
  ggsave(filename = saveImg, width = 12, height = 6, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

dataL3

# 보정 이전 2등급 (정확도 79.9%, 결정계수 0.88, 자료획득률 100%)
evaluateDevicePerformance (
  data = dataL3,
  refCol = "val",
  deviceCols = c("meanVal_solarmy")
)

# 보정 이후 1등급 (정확도 83.5%, 결정계수 0.88, 자료획득률 100%)
evaluateDevicePerformance (
  data = dataL3,
  refCol = "val",
  deviceCols = c("refVal_solarmy")
)
