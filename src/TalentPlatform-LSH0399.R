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
# R을 이용한 일식 식분도 이미지 데이터 추출 및 평균 식분도 시각화 (163개; 78+85개), 몬테카를로 시뮬레이션 모의

# 제가 첨부해드렸던 신라의 사례로 해보시는것이 좋을듯 한데 어떻게 생각하시는지요? 총 16개의 일식입니다.
# BC 54.05.09 / BC 28.06.19 / BC 26.10.23 / BC 15.03.29 / BC 02.02.05/ AD 02.11.22
# AD 16.08.21 / AD 124.10.25 / AD 127.08.25 / AD 141.11.16 / AD 166.02.18 / AD 186.07.04
# AD 193.02.19 / AD 194.08.04 / AD 200.09.26 / AD 201.03.22

# 영역 설정
# 경도 : 90 ~ 150
# 위도 : 10 ~ 60

# 선에 따른 값 부여
# 청색 : 0.0
# 적색 : 0.2
# 흑색 : 0.3, 0.5, 0.7, 0.9

# 선에 따른 세부값 부여
# O : 점선 여러개 (중앙 1.0 - 상/하 0.98), 점선 1개 (중앙 1.0), 이전과 동일
# X : 없음 (중앙 0.97)

# <경주식분 0.69이상인 것만 통과시키기>
# 1번째 추출 - 78개 모집단에서 무작위 14개를 뽑는다. 평균식분도를 만들어본다. 거기서 경주의 식분이 0.69이상이다. (통과시킴) 그 최대평균값(빨간점)의 위치를 지도에 나타낸다.
# 2번째 추출 - 78개 모집단에서 무작위 14개를 뽑는다. 평균식분도를 만들어본다. 경주의 식분이 0.69미만이다. (탈락시킴) 탈락이므로 최대평균값(빨간점)의 위치는 나타내지 않는다.
# 이런식으로 지도상에 총 만개의 점이 생길때까지 하는것입니다.
#
# 이전에 시간상의 문제로 4백 몇개? 까지만 점을 찍었었는데 이번엔 끝까지 해보는겁니다.

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0399"

if (Sys.info()["sysname"] == "Windows") {
  contextPath = ifelse(env == "local", ".", "E:/04. TalentPlatform/Github/TalentPlatform-R")
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

#================================================
# 비즈니스 로직 수행
#================================================
# 라이브러리 읽기
library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(metR)
library(openxlsx)
library(MBA)
library(noncompliance)
library(colorRamps)
library(sf)
library(beepr)
library(openxlsx)
# library(xlsx)
library(readxl)
library(furrr)
library(sampling)
library(future)

mutate_cond = function(.data, condition, ..., new_init = NA, envir = parent.frame()) {
  # Initialize any new variables as new_init
  new_vars <- substitute(list(...))[-1]
  new_vars %<>% sapply(deparse) %>%
    names %>%
    setdiff(names(.data))
  .data[, new_vars] <- new_init

  condition <- eval(substitute(condition), .data, envir)
  .data[condition,] <- .data %>% filter(condition) %>% mutate(...)
  .data
}

cbMatlab = colorRamps::matlab.like(11)
mapGlobal = sf::st_read(file.path(globalVar$mapPath, "gshhg-shp-2.3.6/GSHHS_shp/i/GSHHS_i_L1.shp"))

xRange = as.numeric(c(90, 150))
yRange = as.numeric(c(10, 60))
# yRange = as.numeric(c(5, 65))

# newLon = seq(from = xRange[1], to = xRange[2], by = 0.1)
# newLat = seq(from = yRange[1], to = yRange[2], by = 0.1)
newLon = seq(from = xRange[1], to = xRange[2], by = 0.2)
newLat = seq(from = yRange[1], to = yRange[2], by = 0.2)
# newLon = seq(from = xRange[1], to = xRange[2], by = 0.5)
# newLat = seq(from = yRange[1], to = yRange[2], by = 0.5)

gridData = noncompliance::expand.grid.DT(
  newLon
  , newLat
  , col.names = c("lon", "lat")
)

# fileInfo = Sys.glob(file.path(globalVar$inpPath, "mapImageToData.xlsx"))
# fileInfo = Sys.glob(file.path(globalVar$inpPath, "LSH0195_일식 식분도 이미지 데이터 추출.xlsx"))
# fileInfo = Sys.glob(file.path(globalVar$inpPath, "LSH0259_일식 식분도 이미지 데이터 추출.xlsx"))
# fileInfo = Sys.glob(file.path(globalVar$inpPath, "LSH0330_일식 식분도 이미지 데이터 추출.xlsx"))
# fileInfo = Sys.glob(file.path(globalVar$inpPath, "LSH0382_일식 식분도 이미지 데이터 추출.xlsx"))
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "일식 식분도 이미지 데이터 추출.xlsx"))
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "LSH0399_일식 식분도 이미지 데이터 추출.xlsx"))


# **************************************************
# 시트 선택
# **************************************************
# 시트 1 : 모집단163개
# sheetInfo = 1

# 시트 2 : 모집단86개
# sheetInfo = 2

# 시트 3 : 양자강집단84개
sheetInfo = 3

sheetName = dplyr::case_when(
  sheetInfo == 1 ~ "모집단163개"
  , sheetInfo == 2 ~ "모집단86개"
  , sheetInfo == 3 ~ "양자강집단84개"
  , TRUE ~ NA_character_
)


# **************************************************
# 단일 이미지 테스트
# **************************************************
# data = openxlsx::read.xlsx(fileInfo, sheet = sheetName)
#
# typeList = data$type %>% unique()
# # typeList = typeList[41:45]
# # typeList = typeList[56:78]
# # typeList = typeList[79]
# # typeList = typeList[79:162]
# typeList = typeList[163:163]
#
# # typeInfo = typeList[1]
# for (typeInfo in typeList) {
#
#   tmpData = data %>%
#     as.tibble() %>%
#     dplyr::filter(
#       type == typeInfo
#       , ! is.na(val)
#     ) %>%
#     dplyr::select(-type)
#
#   dataL1 = MBA::mba.points(tmpData, gridData)
#
#   dataL2 = dataL1 %>%
#     as.data.frame() %>%
#     as.tibble() %>%
#     dplyr::rename(
#       xAxis = xyz.est.x
#       , yAxis = xyz.est.y
#       , zAxis = xyz.est.z
#     ) %>%
#     dplyr::mutate(
#       type = typeInfo
#     )
#
#   idx = which(dataL2$zAxis == max(dataL2$zAxis, na.rm = TRUE))
#   maxData = dataL2[idx, ]
#
#   makePlot = ggplot(data = dataL2, aes(x = xAxis, y = yAxis, fill = zAxis, z = zAxis)) +
#     geom_raster(interpolate = TRUE, na.rm = TRUE) +
#     # metR::geom_contour_fill(na.fill = TRUE, kriging = TRUE) +.
#     # geom_tile() +
#     scale_fill_gradientn(colours = cbMatlab, limits = c(0, 1.1), breaks = c(0, 0.3, 0.5, 0.7, 0.9), na.value = NA) +
#     # metR::geom_contour2(color = "black", alpha = 1.0, breaks = seq(0.3, 0.9, 0.2), show.legend = FALSE) +
#     geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = NA) +
#     metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0, show.legend = FALSE, size = 0.1) +
#     metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.3, show.legend = FALSE, size = 0.5) +
#     metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.5, show.legend = FALSE, size = 1) +
#     metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.7, show.legend = FALSE, size = 2) +
#     metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.9, show.legend = FALSE, size = 4) +
#     geom_point(data = tmpData, aes(x = lon, y = lat, colour = factor(val), fill = NULL, z = NULL)) +
#     geom_point(data = maxData, aes(x = xAxis, y = yAxis, colour = meanVal, fill = NULL, z = NULL), color = "red") +
#     metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = c(0, 0.3, 0.5, 0.7, 0.9), rotate = TRUE, na.rm = TRUE, size = 5) +
#     metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(90, 150), expand = c(0, 0)) +
#     metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(10, 60), expand = c(0, 0)) +
#     labs(
#       subtitle = NULL
#       , x = NULL
#       , y = NULL
#       , fill = NULL
#       , colour = NULL
#       , title = NULL
#     ) +
#     theme(text = element_text(size = 18))
#
#   saveImg = sprintf("%s/%s/%s_%s.png", globalVar$figPath, serviceName, sheetName, typeInfo)
#   dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#   ggsave(plot = makePlot, filename = saveImg, width = 10, height = 10, dpi = 600)
#   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# }

# 마리오 알람 소리
# beepr::beep(sound = 8)


# **************************************************
# 시트에 따른 데이터 병합
# **************************************************
# sheetList = c(1)
# sheetName = "모집단163개"

# sheetList = c(2)
# sheetName = "모집단86개"

sheetList = c(3)
sheetName = "양자강집단84개"

# sheetInfo = sheetList[1]
dataL3 = tibble::tibble()
for (sheetInfo in sheetList) {

  # data = xlsx::read.xlsx(fileInfo, sheetIndex = sheetInfo) %>%
  data = openxlsx::read.xlsx(fileInfo, sheet = sheetInfo) %>%
    tibble::as.tibble()
  
  # typeInfo = typeList[1]
  typeList = data$type %>% unique
  for (typeInfo in typeList) {

    tmpData = data %>%
      dplyr::filter(
        type == typeInfo
        , !is.na(val)
      ) %>%
      dplyr::select(-type)
    
    dataL1 = MBA::mba.points(tmpData, gridData)
    
    dataL2 = dataL1 %>%
      as.data.frame() %>%
      as.tibble() %>%
      dplyr::rename(
        xAxis = xyz.est.x
        , yAxis = xyz.est.y
        , zAxis = xyz.est.z
      ) %>%
      dplyr::mutate(type = typeInfo)
    
    dataL3 = dplyr::bind_rows(dataL3, dataL2)
  }
}


# **************************************************
# 공간 평균
# **************************************************
cat(sprintf("[CHECK] type : %s", dataL3$type %>% unique %>% length), "\n")

# 표본 주사위
# sampleData = openxlsx::read.xlsx(fileInfo, sheet = "그룹정보")

# 신라픽
# selList = c(1, 6, 7, 9, 13, 15, 20, 53, 55, 64, 75, 76, 77, 78)

# 3번픽
# selList = c(60, 8, 32, 72, 24, 47, 45, 7, 37, 75, 38, 57, 21, 9)

# selList = c(79, 155, 46, 22, 117, 114, 26, 123, 107, 128, "27(0.96)", 45, 40, 31, 70, 72)

# sampleData$type %>% unique
# sampleData$sampleType %>% unique
# dataL3$type %>% unique()
# dataL4$type %>% unique()

dataL4 = dataL3 %>%
  # dplyr::left_join(sampleData, by = c("type" = "type")) %>%
  # dplyr::filter(sampleType %in% selList) %>%
  # dplyr::filter(type %in% selList) %>%
  dplyr::group_by(xAxis, yAxis) %>%
  dplyr::summarise(
    meanVal = mean(zAxis, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    meanVal = ifelse(meanVal < 0, 0, meanVal)
  )


# 가공 데이터
# saveXlsxFile = sprintf("%s/%s/%s_%s.xlsx", globalVar$outPath, serviceName, sheetName, "DataProc")
# dir.create(path_dir(saveXlsxFile), showWarnings = FALSE, recursive = TRUE)
# wb = openxlsx::createWorkbook()
# openxlsx::addWorksheet(wb, "모집단 가공")
# openxlsx::writeData(wb, "모집단 가공", dataL3, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
# openxlsx::addWorksheet(wb, "평균 가공")
# openxlsx::writeData(wb, "평균 가공", dataL4, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
# openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)
# cat(sprintf("[CHECK] saveXlsxFile : %s", saveXlsxFile), "\n")
# 
# saveFile = sprintf("%s/%s/%s_%s.csv", globalVar$outPath, serviceName, sheetName, "모집단 가공")
# dir.create(path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
# readr::write_csv(dataL3, saveFile)
# cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")
# 
# saveFile = sprintf("%s/%s/%s_%s.csv", globalVar$outPath, serviceName, sheetName, "평균 가공")
# dir.create(path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
# readr::write_csv(dataL4, saveFile)
# cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")


# dataL4$meanVal = round(dataL4$meanVal, 3)
# cat(sprintf("[CHECK] type : %s", dataL4$type %>% unique %>% length), "\n")

summary(dataL4)

maxData = dataL4 %>%
  dplyr::ungroup() %>%
  dplyr::filter(meanVal == max(meanVal, na.rm = TRUE))

# 경주 지점
posLon = 129.2
posLat = 35.8

posData = dataL4 %>%
  dplyr::ungroup() %>%
  dplyr::filter(xAxis == posLon, yAxis == posLat)

cat(sprintf("[CHECK] maxData : %s", maxData$meanVal), "\n")
cat(sprintf("[CHECK] posData : %s", posData$meanVal), "\n")

setBreakCont = c(seq(0.51, 0, -0.02))
setBreakText = c(seq(0.51, 0.10, -0.02))

# 지점(경도90-위도10) 선택
#    xAxis yAxis meanVal posVal sampleInfo
#    <dbl> <dbl>   <dbl>  <dbl> <chr>
#  1    90    10   0.416 0.291  79-155-46-22-117-114-26-123-107-128-27(0.96)-45-40-31-70-72
# bootData %>%
#   dplyr::filter(xAxis == 90, yAxis == 10)

# 평균식분도 결과 : "20230306_86개 모집단의 평균식분도" 폴더 참조
# 최대평균 : 0.518542217867816
# 경주지점 : 0.468578825517671

# dataL4 %>% 
#   dplyr::filter(yAxis == 50)

saveImg = sprintf("%s/%s/%s_%s.png", globalVar$figPath, serviceName, sheetName, "Mean_Color")

makePlot = ggplot(data = dataL4, aes(x = xAxis, y = yAxis, fill = meanVal, z = meanVal)) +
  geom_raster(interpolate = TRUE, na.rm = TRUE) +
  # metR::geom_contour_fill(na.fill = TRUE, kriging = TRUE) +
  scale_fill_gradientn(colours = cbMatlab, limits = c(0, 1.0), breaks = seq(0, 1.0, 0.2), na.value = NA) +
  geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = NA) +
  metR::geom_contour2(color = "black", alpha = 1.0, breaks = setBreakCont, show.legend = FALSE, size = 0.5) +
  metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = setBreakText, rotate = TRUE, na.rm = TRUE, size = 5) +
  geom_point(data = maxData, aes(x = xAxis, y = yAxis), color = "red") +
  metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(89.99, 150.01), expand = c(0, 0)) +
  metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(9.99, 60), expand = c(0, 0)) +
  labs(
    subtitle = NULL
    , x = NULL
    , y = NULL
    , fill = NULL
    , colour = NULL
    , title = NULL
  ) +
  theme(text = element_text(size = 18))

ggsave(makePlot, filename = saveImg, width = 10, height = 10, dpi = 600)
# ggplot2::last_plot()
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

saveImg = sprintf("%s/%s/%s_%s.png", globalVar$figPath, serviceName, sheetName, "Mean_Black")

makePlot = ggplot(data = dataL4, aes(x = xAxis, y = yAxis, z = meanVal)) +
  # geom_raster(interpolate = TRUE, na.rm = TRUE) +
  # scale_fill_gradientn(colours = cbMatlab, limits = c(0, 1.0), breaks = seq(0, 1.0, 0.2), na.value = NA) +
  # metR::geom_contour_fill(na.fill = TRUE, kriging = TRUE)
  geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = NA) +
  metR::geom_contour2(color = "black", alpha = 1.0, breaks = setBreakCont, show.legend = FALSE, size = 0.5) +
  metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = setBreakText, rotate = TRUE, na.rm = TRUE, size = 5) +
  geom_point(data = maxData, aes(x = xAxis, y = yAxis), color = "red") +
  metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(89.99, 150.01), expand = c(0, 0)) +
  metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(9.99, 60), expand = c(0, 0)) +
  labs(
    subtitle = NULL
    , x = NULL
    , y = NULL
    , fill = NULL
    , colour = NULL
    , title = NULL
  ) +
  theme(text = element_text(size = 18))

ggsave(makePlot, filename = saveImg, width = 10, height = 10, dpi = 600)
# ggplot2::last_plot()
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# **************************************************
# 시뮬레이션 모의
# **************************************************
# 난수 초기값
set.seed(123)

# 프로세스 제거
# ps -ef | grep parallelly | awk '{print $2}' | xargs kill -9

# 프로세스 수행
# conda activate r36
# cd /SYSTEMS/PROG/R/PyCharm/src
# nohup Rscript TalentPlatform-LSH0382.R &

# 표본 주사위
# sampleData = openxlsx::read.xlsx(fileInfo, sheet = "그룹정보")
# # sampleData = openxlsx::read.xlsx(fileInfo, sheet = "그룹정보L2")
#
# sampleDataL1 = dataL3 %>%
#   dplyr::left_join(sampleData, by = c("type" = "type")) %>%
#   dplyr::select(-group, -sampleType)

sampleDataL1 = dataL3

sampleInfo = sampleDataL1$type %>% unique()

# 부트스트랩 횟수
# bootDo = 10
# bootDo = 1000
bootDo = 10000
# bootDo = 100000
# bootDo = 300000

# 경주 지점
posLon = 129.2
posLat = 35.8

# 부트스트랩 주사위 목록
# bostSample = lapply(1:bootDo, function(i) sample(sampleInfo, size = 14, replace = FALSE))
# bostSample = lapply(1:bootDo, function(i) sampling::strata(c("group"), size = c(9, 3, 2), method = "srswor", data=sampleData)$ID_unit)
# bostSample = lapply(1:bootDo, function(i) sampling::strata(c("group"), size = c(27, 9, 6), method = "srswor", data=sampleData)$ID_unit)
# bostSample = lapply(1:bootDo, function(i) sampling::strata(c("group"), size = c(3, 2, 9), method = "srswor", data=sampleData)$ID_unit)

# 부트스트랩 중복검사
# bostSampleL1 = data.frame(t(sapply(bostSample, c))) %>%
#   as.tibble()
#
# bostSampleL2 = tibble::tibble()
# for (i in 1:nrow(bostSampleL1)) {
#   selData = bostSampleL1[i, ] %>%
#     sort() %>%
#     magrittr::set_colnames(c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", 'X13', 'X14'))
#
#   bostSampleL2 = dplyr::bind_rows(bostSampleL2, selData)
# }
#
# bostSampleL3 = bostSampleL2 %>%
#   dplyr::distinct(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14)
#
# saveFile = sprintf("%s/%s/bostSampleL3_%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo)
# dir.create(path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
# readr::write_csv(x = bostSampleL3, file = saveFile)
# cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")

options(future.globals.maxSize = 9999999999999999)
plan(multisession, workers = parallelly::availableCores() - 2)
# plan(multisession, workers = parallelly::availableCores() - 5)
# plan(multisession, workers = parallelly::availableCores() - 10)
# plan(multisession, workers = parallelly::availableCores() - 20)
# future::plan(multisession, workers = 10)
# future::plan(multisession, workers = 1)

# 부트스트랩 추출 개수
# bootNum = 70
# bootNumList = c(14)
bootNumList = c(16)
# bootNumList = c(42)
# bootNumList = c(16, 30)
# bootNumList = c(30, 50, 60, 70)
# bootNumList = c(50, 60, 70)
# bootNumList = c(78)

# 병렬횟수 설정
bootIdxList = seq(1, 1)
# bootIdxList = seq(1, 30)
# bootIdxList = seq(31, 60)
# bootIdxList = seq(1, 300)
# bootIdxList = seq(301, 600)
# bootIdxList = seq(1, 2)

# bootIdx = bootIdxList[1]
# bootNum = bootNumList[1]
for (bootIdx in bootIdxList) {

  cat(sprintf("[CHECK] bootIdx : %s", bootIdx), "\n")

  for (bootNum in bootNumList) {

    # 부스스트랩 주사위 목록
    bostSample = lapply(1:bootDo, function(i) sample(sampleInfo, size = bootNum, replace = FALSE))

    bostSampleL1 = data.frame(t(sapply(bostSample, c)))
    # saveFile = sprintf("%s/%s/bostSampleL1_%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo)
    # saveFile = sprintf("%s/%s/bostSampleL1_%s-%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo, bootIdx)
    saveFile = sprintf("%s/%s/%s_bostSampleL1_%s-%s-%s.csv", globalVar$outPath, serviceName, sheetName, bootNum, bootDo, bootIdx)
    dir.create(path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
    readr::write_csv(x = bostSampleL1, file = saveFile)

    # sampleDataL1 %>%
    #     dplyr::filter(type %in% bostSample[[i]]) %>%
    #     dplyr::select(type) %>%
    #     unique()

    # 부트스트랩을 통해 병렬처리
    bootSelData = furrr::future_map_dfr(1:bootDo, function(i) {
      sampleDataL1 %>%
        dplyr::filter(type %in% bostSample[[i]]) %>%
        dplyr::group_by(xAxis, yAxis) %>%
        dplyr::summarise(
          meanVal = mean(zAxis, na.rm = TRUE)
        ) %>%
        dplyr::mutate(
          meanVal = ifelse(meanVal < 0, 0, meanVal)
        ) %>%
        dplyr::ungroup() %>%
        mutate_cond(xAxis == posLon & yAxis == posLat, posVal = meanVal) %>%
        dplyr::mutate(posVal = na.exclude(posVal)) %>%
        dplyr::filter(meanVal == max(meanVal, na.rm = TRUE))
    })

    # saveFile = sprintf("%s/%s/bootSelData_%s-%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo, bootIdx)
    # saveFile = sprintf("%s/%s/bootSelData_%s-%s-%s_%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo, bootIdx, posLon, posLat)
    saveFile = sprintf("%s/%s/%s_bootSelData_%s-%s-%s_%s-%s.csv", globalVar$outPath, serviceName, sheetName, bootNum, bootDo, bootIdx, posLon, posLat)
    dir.create(path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
    readr::write_csv(x = bootSelData, file = saveFile)
    cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")

    # 부트스트랩을 통해 최대값 추출
    # bootData = future_map_dfr(1:bootDo, function(i) {
    #   sampleDataL1 %>%
    #     dplyr::filter(sampleType %in% bostSample[[i]]) %>%
    #     dplyr::group_by(xAxis, yAxis) %>%
    #     dplyr::summarise(
    #       meanVal = mean(zAxis, na.rm = TRUE)
    #     ) %>%
    #     dplyr::mutate(
    #       meanVal = ifelse(meanVal < 0, 0, meanVal)
    #     ) %>%
    #     dplyr::ungroup() %>%
    #     dplyr::filter(meanVal == max(meanVal, na.rm = TRUE)) %>%
    #     dplyr::mutate(idx = i)
    # })

    # saveFile = sprintf("%s/%s/bootData_%s.csv", globalVar$outPath, serviceName, bootNum)
    # saveFile = sprintf("%s/%s/bootData_%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo)
    # saveFile = sprintf("%s/%s/bootData_%s-%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo, bootIdx)
    # dir.create(path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
    # readr::write_csv(x = bootData, file = saveFile)
    # cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")

    # 부트스트랩을 통해 특정 지점 추출
    # bootPosData = future_map_dfr(1:bootDo, function(i) {
    #   sampleDataL1 %>%
    #     dplyr::filter(sampleType %in% bostSample[[i]]) %>%
    #     dplyr::group_by(xAxis, yAxis) %>%
    #     dplyr::summarise(
    #       meanVal = mean(zAxis, na.rm = TRUE)
    #     ) %>%
    #     dplyr::mutate(
    #       meanVal = ifelse(meanVal < 0, 0, meanVal)
    #     ) %>%
    #     dplyr::ungroup() %>%
    #     dplyr::filter(xAxis == posLon, yAxis == posLat) %>%
    #     dplyr::rename(
    #       "posLon" = "xAxis"
    #       , "posLat" = "yAxis"
    #       , "posVal" = "meanVal"
    #     ) %>%
    #     dplyr::mutate(idx = i)
    # })

    # saveFile = sprintf("%s/%s/bootPosData_%s_%s-%s.csv", globalVar$outPath, serviceName, bootNum, posLon, posLat)
    # saveFile = sprintf("%s/%s/bootPosData_%s-%s_%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo, posLon, posLat)
    # saveFile = sprintf("%s/%s/bootPosData_%s-%s-%s_%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo, bootIdx, posLon, posLat)
    # dir.create(path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
    # readr::write_csv(x = bootPosData, file = saveFile)
    # cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")
  }
}


bootIdxList = seq(1, 1)
# bootIdxList = seq(1, 60)
# bootNumList = c(30, 50, 60, 70)
# bootNumList = c(14)
bootNumList = c(16)
# bootNumList = c(42)
# bootNumList = c(16, 30)
# bootNum = bootNumList[1]
for (bootNum in bootNumList) {

  # bootIdx = bootIdxList[1]
  bootData = tibble::tibble()
  for (bootIdx in bootIdxList) {
    # saveFile = sprintf("%s/%s/bostSampleL1_%s-%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo, bootIdx)
    saveFile = sprintf("%s/%s/%s_bostSampleL1_%s-%s-%s.csv", globalVar$outPath, serviceName, sheetName, bootNum, bootDo, bootIdx)
    fileList = Sys.glob(saveFile)
    if (length(fileList) < 1) { next }

    sampleData = readr::read_csv(file = fileList, show_col_types = FALSE) %>%
      tidyr::unite(sampleInfo, sep = "-") %>%
      dplyr::select(sampleInfo)

    # saveFile = sprintf("%s/%s/bootSelData_%s-%s-%s_%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo, bootIdx, posLon, posLat)
    saveFile = sprintf("%s/%s/%s_bootSelData_%s-%s-%s_%s-%s.csv", globalVar$outPath, serviceName, sheetName, bootNum, bootDo, bootIdx, posLon, posLat)
    fileList = Sys.glob(saveFile)
    if (length(fileList) < 1) { next }

    cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")

    selData = readr::read_csv(file = fileList, show_col_types = FALSE) %>%
      dplyr::bind_cols(sampleData)

    bootData = dplyr::bind_rows(bootData, selData)
  }

  # # A tibble: 43 × 5
  #    xAxis yAxis meanVal posVal sampleInfo
  #    <dbl> <dbl>   <dbl>  <dbl> <chr>
  #  1    90    10   0.416 0.291  79-155-46-22-117-114-26-123-107-128-27(0.96)-45-40-31-70-72
  # bootData %>%
  #   dplyr::filter(xAxis == 90, yAxis == 10)

  # 경주지점 0.68 이상
  # 경주지점 0.69 이상
  bootDataL2 = bootData %>%
    dplyr::distinct(xAxis, yAxis, meanVal, posVal, keep_all = TRUE) %>%
    # dplyr::distinct(xAxis, yAxis, meanVal, posVal, sampleInfo, keep_all = TRUE) %>%
    # dplyr::filter(
      # posVal >= 0.68
      # posVal >= 0.69
    # ) %>%
    dplyr::slice(1:10000)

  # 평균식분도 최대값 0.78 이상
  # 붉은점 0.78 이상
  # 붉은점 0.75 이상
  bootDataL3 = bootDataL2 %>%
    dplyr::distinct(xAxis, yAxis, meanVal, posVal, keep_all = TRUE) %>%
    # dplyr::distinct(xAxis, yAxis, meanVal, posVal, sampleInfo, keep_all = TRUE) %>%
    dplyr::filter(
      # meanVal >= 0.78
      # meanVal >= 0.75
      meanVal >= 0.70
    )

  plotData = bootDataL2
  # plotData = bootDataL3

  saveImg = sprintf("%s/%s/%s-%s_%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Hist", bootNum, bootDo, posLon, posLat)
  dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
  png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

  histData = hist(plotData$meanVal)
  hist(plotData$meanVal, main = NULL, xlab = NULL)
  text(histData$mids, histData$counts, pos = 3, labels = histData$counts)

  dev.off()
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

  # saveImg = sprintf("%s/%s/%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Color Overlay", bootNum)
  # saveImg = sprintf("%s/%s/%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Color Overlay", bootNum, bootDo)
  # saveImg = sprintf("%s/%s/%s_%s-%s-%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Color Overlay", bootNum, posLon, posLat)
  saveImg = sprintf("%s/%s/%s-%s_%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Color Overlay", bootNum, bootDo, posLon, posLat)
  dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

  # 1. 만개의 점이 찍혀있는 위도 경도 그래프
  makePlot = ggplot(data = plotData, aes(x = xAxis, y = yAxis, color = meanVal)) +
    # geom_point(size = 2, show.legend = TRUE) +
    geom_point(size = 1, show.legend = TRUE, alpha = 0.3) +
    # geom_point(size = 1, show.legend = TRUE) +
    scale_color_gradientn(colours = cbMatlab) +
    geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = NA) +
    metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(89.99, 150.01), expand = c(0, 0)) +
    metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(9.99, 60), expand = c(0, 0)) +
    labs(
      subtitle = NULL
      , x = NULL
      , y = NULL
      , fill = NULL
      , colour = NULL
      , title = NULL
    ) +
    theme(text = element_text(size = 18))

  ggsave(makePlot, filename = saveImg, width = 10, height = 10, dpi = 600)
  # ggplot2::last_plot()
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

  # saveImg = sprintf("%s/%s/%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Black Overlay", bootNum)
  # saveImg = sprintf("%s/%s/%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Black Overlay", bootNum, bootDo)
  #  saveImg = sprintf("%s/%s/%s_%s-%s-%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Black Overlay", bootNum, posLon, posLat)
  saveImg = sprintf("%s/%s/%s-%s_%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Black Overlay", bootNum, bootDo, posLon, posLat)
  dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

  makePlot = ggplot(data = plotData, aes(x = xAxis, y = yAxis, color = meanVal)) +
    # geom_point(size = 2, color = "black", show.legend = FALSE) +
    geom_point(size = 1, color = "black", show.legend = FALSE, alpha = 0.3) +
    # geom_point(size = 1, color = "black", show.legend = FALSE) +
    # scale_color_gradientn(colours = cbMatlab) +
    geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = NA) +
    metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(89.99, 150.01), expand = c(0, 0)) +
    metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(9.99, 60), expand = c(0, 0)) +
    labs(
      subtitle = NULL
      , x = NULL
      , y = NULL
      , fill = NULL
      , colour = NULL
      , title = NULL
    ) +
    theme(text = element_text(size = 18))

  ggsave(makePlot, filename = saveImg, width = 10, height = 10, dpi = 600)
  # ggplot2::last_plot()
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

  saveImg = sprintf("%s/%s/%s-%s_%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Density Color Overlay", bootNum, bootDo, posLon, posLat)
  dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

  makePlot = ggplot(data = plotData, aes(x = xAxis, y = yAxis, color = meanVal)) +
    stat_density2d(aes(fill = ..density..), n = 100 * 3, contour = FALSE, geom = "raster") +
    scale_fill_gradientn(colours = cbMatlab) +
    geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "white", fill = NA) +
    metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(89.99, 150.01), expand = c(0, 0)) +
    metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(9.99, 60), expand = c(0, 0)) +
    labs(
      subtitle = NULL
      , x = NULL
      , y = NULL
      , fill = NULL
      , colour = NULL
      , title = NULL
    ) +
    theme(text = element_text(size = 18))

  ggsave(makePlot, filename = saveImg, width = 10, height = 10, dpi = 600)
  # ggplot2::last_plot()
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

  saveImg = sprintf("%s/%s/%s-%s_%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Density Black Overlay", bootNum, bootDo, posLon, posLat)
  dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

  makePlot = ggplot(data = plotData, aes(x = xAxis, y = yAxis, color = meanVal)) +
    stat_density2d(aes(fill = ..density..), n = 100 * 3, contour = FALSE, geom = "raster") +
    scale_fill_distiller(palette = "Greys") +
    geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "yellow", fill = NA) +
    metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(89.99, 150.01), expand = c(0, 0)) +
    metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(9.99, 60), expand = c(0, 0)) +
    labs(
      subtitle = NULL
      , x = NULL
      , y = NULL
      , fill = NULL
      , colour = NULL
      , title = NULL
    ) +
    theme(text = element_text(size = 18))

  ggsave(makePlot, filename = saveImg, width = 10, height = 10, dpi = 600)
  # ggplot2::last_plot()
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

  # 2. 위도 28~34/경도 110~116 구역안의 점 개수
  # statData = plotData %>%
  #   dplyr::distinct(xAxis, yAxis, meanVal, posVal, keep_all = TRUE) %>%
  #   dplyr::filter(
  #     dplyr::between(xAxis, 110, 116)
  #     , dplyr::between(yAxis, 28, 34)
  #   ) %>%
  #   dplyr::summarise(cnt = n())

  # 3. 위도 34~42/경도 124~130 구역안의 점 개수
  # statDataL1 = plotData %>%
  #   dplyr::distinct(xAxis, yAxis, meanVal, posVal, keep_all = TRUE) %>%
  #   dplyr::filter(
  #     dplyr::between(xAxis, 124, 130)
  #     , dplyr::between(yAxis, 34, 42)
  #   ) %>%
  #   dplyr::summarise(cnt = n())

  cat(sprintf("[CHECK] bootData : %s", nrow(plotData)), "\n")
  # cat(sprintf("[CHECK] statData : %s", statData), "\n")
  # cat(sprintf("[CHECK] statDataL1 : %s", statDataL1), "\n")

}

# statDataL2 = bootData %>%
#   dplyr::filter(
#     meanVal >= 0.7
#   ) %>%
#   dplyr::summarise(cnt = n())

# 빈도분포 그래프 : "20230205_붉은점0.75이상_빈도분포" 폴더 참조
# https://drive.google.com/drive/folders/1WFYlYk71aQNaXm4MlsLd9A_MWe7ijSjq?usp=sharing

# 콘솔화면, 막대그래프와 밀집그래프 : "20230320_84개모집단-16개추출-붉은점" 폴더 참조
# https://drive.google.com/drive/folders/1WFtFfjP6hOCp737qJciNDMvhSlQQpaKc?usp=sharing

# ********************************************************************************************
# 20221225_부스스트랩 주사위 목록
# ********************************************************************************************
# 경주 이런거 다 필요없고 78개에서 단순 무작위로 14개 추출해서 평균식분도를 그려보고
# 거기서 붉은점(최대평균값) 을 지도에 점으로 나타내는 작업을 1만번 시행하는겁니다.

# bootIdxList = seq(1, 1)
# bootNumList = c(14)
#
# # bootNum = bootNumList[1]
# # bootIdx = bootIdxList[1]
# for (bootNum in bootNumList) {
#
#   bootData = tibble::tibble()
#   for (bootIdx in bootIdxList) {
#     saveFile = sprintf("%s/%s/bootSelData_%s-%s-%s_%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo, bootIdx, posLon, posLat)
#     fileList = Sys.glob(saveFile)
#     if (length(fileList) < 1) { next }
#
#     cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")
#
#     selData = readr::read_csv(file = fileList, show_col_types = FALSE)
#
#     # selDataL2 = selData %>%
#     #   dplyr::distinct(xAxis, yAxis, meanVal, posVal, .keep_all=TRUE)
#     # cat(sprintf("[CHECK] unique cnt : %s", nrow(selData) - nrow(selDataL2)), "\n")
#
#     bootData = dplyr::bind_rows(bootData, selData)
#   }
#
#   # 경주지점 0.69 이상
#   # bootDataL2 = bootData %>%
#   #   dplyr::distinct(xAxis, yAxis, meanVal, posVal, keep_all = TRUE) %>%
#   #   dplyr::filter(
#   #     posVal >= 0.69
#   #   ) %>%
#   #   dplyr::slice(1:10000)
#
#   # 평균식분도 최대값 0.78 이상
#   # 붉은점 0.78 이상
#   bootDataL3 = bootData %>%
#     dplyr::distinct(xAxis, yAxis, meanVal, posVal, keep_all = TRUE) %>%
#     dplyr::filter(
#       meanVal >= 0.78
#     )
#
#   # plotData = bootData
#   # plotData = bootDataL2
#   plotData = bootDataL3
#
#   # saveImg = sprintf("%s/%s/%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Color Overlay", bootNum)
#   saveImg = sprintf("%s/%s/%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Color Overlay", bootNum, bootDo)
#   # saveImg = sprintf("%s/%s/%s_%s-%s-%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Color Overlay", bootNum, posLon, posLat)
#   # saveImg = sprintf("%s/%s/%s-%s_%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Color Overlay", bootNum, bootDo, posLon, posLat)
#   dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
#   # 1. 만개의 점이 찍혀있는 위도 경도 그래프
#   makePlot = ggplot(data = plotData, aes(x = xAxis, y = yAxis, color = meanVal)) +
#     # geom_point(size = 2, show.legend = TRUE) +
#     geom_point(size = 1, show.legend = TRUE, alpha = 0.3) +
#     # geom_point(size = 1, show.legend = TRUE) +
#     scale_color_gradientn(colours = cbMatlab) +
#     geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = NA) +
#     metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(89.99, 150.01), expand = c(0, 0)) +
#     metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(9.99, 60), expand = c(0, 0)) +
#     labs(
#       subtitle = NULL
#       , x = NULL
#       , y = NULL
#       , fill = NULL
#       , colour = NULL
#       , title = NULL
#     ) +
#     theme(text = element_text(size = 18))
#
#   ggsave(makePlot, filename = saveImg, width = 10, height = 10, dpi = 600)
#   # ggplot2::last_plot()
#   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#
#   # saveImg = sprintf("%s/%s/%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Black Overlay", bootNum)
#   saveImg = sprintf("%s/%s/%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Black Overlay", bootNum, bootDo)
#   #  saveImg = sprintf("%s/%s/%s_%s-%s-%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Black Overlay", bootNum, posLon, posLat)
#   # saveImg = sprintf("%s/%s/%s-%s_%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Black Overlay", bootNum, bootDo, posLon, posLat)
#   dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
#   makePlot = ggplot(data = plotData, aes(x = xAxis, y = yAxis, color = meanVal)) +
#     # geom_point(size = 2, color = "black", show.legend = FALSE) +
#     geom_point(size = 1, color = "black", show.legend = FALSE, alpha = 0.3) +
#     # geom_point(size = 1, color = "black", show.legend = FALSE) +
#     # scale_color_gradientn(colours = cbMatlab) +
#     geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = NA) +
#     metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(89.99, 150.01), expand = c(0, 0)) +
#     metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(9.99, 60), expand = c(0, 0)) +
#     labs(
#       subtitle = NULL
#       , x = NULL
#       , y = NULL
#       , fill = NULL
#       , colour = NULL
#       , title = NULL
#     ) +
#     theme(text = element_text(size = 18))
#
#   ggsave(makePlot, filename = saveImg, width = 10, height = 10, dpi = 600)
#   # ggplot2::last_plot()
#   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#
#   saveImg = sprintf("%s/%s/%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Density Color Overlay", bootNum, bootDo)
#   # saveImg = sprintf("%s/%s/%s-%s_%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Density Color Overlay", bootNum, bootDo, posLon, posLat)
#   dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
#   makePlot = ggplot(data = plotData, aes(x = xAxis, y = yAxis, color = meanVal)) +
#     stat_density2d(aes(fill = ..density..), n = 100 * 3, contour = FALSE, geom = "raster") +
#     scale_fill_gradientn(colours = cbMatlab) +
#     geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "white", fill = NA) +
#     metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(89.99, 150.01), expand = c(0, 0)) +
#     metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(9.99, 60), expand = c(0, 0)) +
#     labs(
#       subtitle = NULL
#       , x = NULL
#       , y = NULL
#       , fill = NULL
#       , colour = NULL
#       , title = NULL
#     ) +
#     theme(text = element_text(size = 18))
#
#   ggsave(makePlot, filename = saveImg, width = 10, height = 10, dpi = 600)
#   # ggplot2::last_plot()
#   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#
#   saveImg = sprintf("%s/%s/%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Density Black Overlay", bootNum, bootDo)
#   # saveImg = sprintf("%s/%s/%s-%s_%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Density Black Overlay", bootNum, bootDo, posLon, posLat)
#   dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
#   makePlot = ggplot(data = plotData, aes(x = xAxis, y = yAxis, color = meanVal)) +
#     stat_density2d(aes(fill = ..density..), n = 100 * 3, contour = FALSE, geom = "raster") +
#     scale_fill_distiller(palette = "Greys") +
#     geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "yellow", fill = NA) +
#     metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(89.99, 150.01), expand = c(0, 0)) +
#     metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(9.99, 60), expand = c(0, 0)) +
#     labs(
#       subtitle = NULL
#       , x = NULL
#       , y = NULL
#       , fill = NULL
#       , colour = NULL
#       , title = NULL
#     ) +
#     theme(text = element_text(size = 18))
#
#   ggsave(makePlot, filename = saveImg, width = 10, height = 10, dpi = 600)
#   # ggplot2::last_plot()
#   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#
#   # 2. 위도 28~34/경도 110~116 구역안의 점 개수
#   statData = plotData %>%
#     dplyr::distinct(xAxis, yAxis, meanVal, posVal, keep_all = TRUE) %>%
#     dplyr::filter(
#       dplyr::between(xAxis, 110, 116)
#       , dplyr::between(yAxis, 28, 34)
#     ) %>%
#     dplyr::summarise(cnt = n())
#
#   # 3. 위도 34~42/경도 124~130 구역안의 점 개수
#   statDataL1 = plotData %>%
#     dplyr::distinct(xAxis, yAxis, meanVal, posVal, keep_all = TRUE) %>%
#     dplyr::filter(
#       dplyr::between(xAxis, 124, 130)
#       , dplyr::between(yAxis, 34, 42)
#     ) %>%
#     dplyr::summarise(cnt = n())
#
#   cat(sprintf("[CHECK] plotData : %s", nrow(plotData)), "\n")
#   cat(sprintf("[CHECK] statData : %s", statData), "\n")
#   cat(sprintf("[CHECK] statDataL1 : %s", statDataL1), "\n")
#
# }

# 1. 위도 경도 그래프 : "20230205_단순무작위14개평균식분도" 폴더 참조 (점 개수 : 10000)
# 2. 위도 28~34/경도 110~116 구역안의 점 개수 : 200
# 3. 위도 34~42/경도 124~130 구역안의 점 개수 : 1472

# 1. 위도 경도 그래프 : "20230205_단순무작위14개평균식분도_붉은점0.78이상" 폴더 참조 (점 개수 : 55)


# ********************************************************************************************
# 20221225_부스스트랩 주사위 목록
# ********************************************************************************************
# bostSample = lapply(1:bootDo, function(i) sample(sampleInfo, size=bootNum, replace = FALSE))
#
# # 부스스트랩을 통해 최대값 추출
# # plan(multisession, workers = availableCores() - 1)
# # plan(multisession, workers = availableCores() - 16)
# plan(multisession, workers = availableCores() - 6)
# # options(future.globals.maxSize= 891289600)
# options(future.globals.maxSize= 99999999999999)
#
# bootData = future_map_dfr(1:bootDo, function(i) {
#
#   dataL3 %>%
#       dplyr::filter(type %in% bostSample[[i]]) %>%
#       dplyr::filter(zAxis > 0) %>%
#       dplyr::group_by(xAxis, yAxis) %>%
#       dplyr::summarise(
#           meanVal = mean(zAxis, na.rm = TRUE)
#       ) %>%
#       dplyr::mutate(
#         meanVal = ifelse(meanVal < 0, 0, meanVal)
#       ) %>%
#       dplyr::ungroup() %>%
#       dplyr::filter(meanVal == max(meanVal, na.rm = TRUE)) %>%
#       dplyr::mutate(idx = i)
# })
#
# bootData = tibble::tibble()
# for (i in 1:length(bostSample)) {
#   cat(sprintf("[CHECK] 진행률 : %.2f", i / length(bostSample) * 100.0), "\n")
#
#   dataL4 = dataL3 %>%
#     dplyr::filter(type %in% bostSample[[i]]) %>%
#     dplyr::filter(zAxis > 0) %>%
#     dplyr::group_by(xAxis, yAxis) %>%
#     dplyr::summarise(
#         meanVal = mean(zAxis, na.rm = TRUE)
#     ) %>%
#     dplyr::ungroup() %>%
#     dplyr::filter(meanVal == max(meanVal, na.rm = TRUE)) %>%
#     dplyr::mutate(idx = i)
#
#   bootData = dplyr::bind_rows(bootData, dataL4)
# }