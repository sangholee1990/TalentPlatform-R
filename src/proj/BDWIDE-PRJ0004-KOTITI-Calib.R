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
fnGetCalibFactor = function(nActual, nPredicted, nMin, nMax, nInterval, isPlot = FALSE) {
  
  nFactor = seq(nMin, nMax, by = nInterval)
  
  # RMSE Fitting
  liResultTmp = lapply(1:length(nFactor), function(iCount) Metrics::rmse(nActual, nPredicted * nFactor[iCount]))   
  
  liResult = unlist(liResultTmp)
  
  if (isPlot == TRUE) {
    plot(liResult)   
  }
  
  # Best Factor Index
  iIndex = which(liResult == min(liResult, na.rm = TRUE))
  
  return (nFactor[[iIndex]])
}


# 기준자료
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "20250507_KOTITI_PM25/*/기준측정기 데이터_250423-250504.xlsx"))

# fileInfo = fileList[1]
refData = tibble::tibble()
for (fileInfo in fileList) {
  orgData = openxlsx::read.xlsx(fileInfo, sheet = "Sheet2")
  refData = dplyr::bind_rows(refData, orgData)
}

refDataL1 = refData %>% 
  dplyr::mutate(
    sYmd = stringr::str_c(연, 월, stringr::str_remove(일, "일"), 시간, sep = "-"),
    dtHour = readr::parse_datetime(sYmd, format = "%Y-%m-%d-%H")
  ) %>% 
    dplyr::rename(
      val = "KOTITI기준측정기.농도"
    )

refDataL2 = refDataL1 %>% 
  dplyr::select(dtHour, val)

# plot(refDataL2$dtYmd, refDataL2$val)


# 측정자료
# 193037, 193044, 193049
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "20250507_KOTITI_PM25/*/*_*.csv"))

# fileInfo = fileList[1]
mesData = tibble::tibble()
for (fileInfo in fileList) {
  fileNameSplit = fileInfo %>% 
    fs::path_file() %>% 
    tools::file_path_sans_ext() %>% 
    tools::file_path_sans_ext() %>% 
    stringr::str_split(pattern = "_") %>% 
    unlist()
  
  orgData = readr::read_csv(fileInfo, col_names = FALSE, skip = 1, show_col_types = FALSE) %>% 
    dplyr::mutate(
      sYmd = stringr::str_c(fileNameSplit[2], X2, sep = " "),
      key = fileNameSplit[3]
    ) %>% 
    dplyr::mutate(
      dtYmd = readr::parse_datetime(sYmd, format = "%Y%m%d %H:%M:%S")
    ) %>% 
    tidyr::separate(col = X3, into = c("type", "val"), sep = "=", convert = TRUE)
    
  mesData = dplyr::bind_rows(mesData, orgData)
}

mesDataL1 = mesData %>% 
  dplyr::mutate(
    dtHour = lubridate::floor_date(dtYmd, unit = "hour")
  ) %>%
  dplyr::group_by(key, dtHour) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE),
    cnt = n()
  ) %>%
  dplyr::ungroup()

mesDataL2 = mesDataL1 %>% 
  tidyr::pivot_wider(
    id_cols = dtHour,
    names_from = key,
    values_from = c(meanVal, cnt)
  )

# plot(refDataL2$dtHour, refDataL2$val)
# plot(mesDataL2$dtHour, mesDataL2$meanVal_193037)
# plot(mesDataL2$dtHour, mesDataL2$meanVal_193044)
# plot(mesDataL2$dtHour, mesDataL2$meanVal_193049)

# 데이터 병합
data = dplyr::left_join(refDataL2, mesDataL2, by = c("dtHour" = "dtHour"))


saveFile = sprintf("%s/%s/%s.xlsx", globalVar$outPath, serviceName, "KOTITI 시간에 따른 PM25 시계열")
dir.create(fs::path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Sheet1")
openxlsx::writeData(wb, "Sheet1", data, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
openxlsx::saveWorkbook(wb, file = saveFile, overwrite = TRUE)
cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")


dataL1 = data %>% 
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
  scale_x_datetime(date_breaks = "12 hour", date_labels = "%m-%d %H") +
  ggsave(filename = saveImg, width = 12, height = 6, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
