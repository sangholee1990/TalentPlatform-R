
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
# R을 이용한 일식 식분도 이미지 데이터 추출 및 평균 식분도 시각화 (78개), 몬테카를로 시뮬레이션 모의

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
serviceName = "LSH0382"

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
  source(here::here(file.path(contextPath, "src"), "InitConfig.R"), encoding = "UTF-8")
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

mutate_cond <- function(.data, condition, ..., new_init = NA, envir = parent.frame()) {
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

newLon = seq(from = xRange[1], to = xRange[2], by = 0.1)
newLat = seq(from = yRange[1], to = yRange[2], by = 0.1)

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
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "일식 식분도 이미지 데이터 추출.xlsx"))

# **************************************************
# 시트 선택
# **************************************************
# 시트 1 : 테스트
# sheetInfo = 1

# 시트 2 : 초기신라(16)
# sheetInfo = 2

# 시트 3 : 후기신라(9)
# sheetInfo = 3

# 시트 4 : 전한(43)
# sheetInfo = 4

# 시트 5 : 당나라(14)
# sheetInfo = 5

# 시트 6 : 최종(8)
# sheetInfo = 6

# 시트 7 : 청온리(18)
# sheetInfo = 7

# 시트 8 : 청-조선공통(92)
# sheetInfo = 8

# 시트 9 : 조선온리(20)
# sheetInfo = 9

# 시트 10 : 북명+조선공통(74)
# sheetInfo = 10

# 시트 11 : 남명+조선공통(6)
# sheetInfo = 11

# 시트 12 : 개경조선온리(1)
# sheetInfo = 12

# 시트 13 : 남명+개경조선(2)
# sheetInfo = 13

# 시트 14 : 남명온리(17)
# sheetInfo = 14

# 시트 15 : 북명온리(14)
# sheetInfo = 15

# 시트 16 : 남원+남송공통(4)
# sheetInfo = 16

# 시트 17 : 남원온리(39)
# sheetInfo = 17

# 시트 18 : 남송+선금공통(6)
# sheetInfo = 18

# 시트 19 : 남송+중금공통(19)
# sheetInfo = 19

# 시트 20 : 남송+후금공통(6)
# sheetInfo = 20

# 시트 21 : 남송온리(17)
# sheetInfo = 21

# 시트 22 : 북원+남송공통(5)
# sheetInfo = 22

# 시트 23 : 후금온리(1)
# sheetInfo = 23

# 시트 24 : 중금온리(2)
# sheetInfo = 24

# 시트 25 : 선금+북송공통(3)
# sheetInfo = 25

# 시트 26 : 선금+요+공통(1)
# sheetInfo = 26

# 시트 27 : 북송+요+공통(17)
# sheetInfo = 27

# 시트 28 : 북송온리(49)
# sheetInfo = 28

# 시트 29 : 요+후당공통(2)
# sheetInfo = 29

# 시트 30 : 요온리(7)
# sheetInfo = 30

# 시트 31 : 후진온리(8)
# sheetInfo = 31

# 시트 32 : 후당온리(4)
# sheetInfo = 32

# 시트 33 : 후한온리(3)
# sheetInfo = 33

# 시트 34 : 낙양당온리(11)
# sheetInfo = 34

# 시트 35 : 시안당온리(67)
# sheetInfo = 35

# 시트 36 : 진+수+공통(1)
# sheetInfo = 36

# 시트 37 : 수온리(3)
# sheetInfo = 37

# 시트 38 : 진+북주+공통(7)
# sheetInfo = 38

# 시트 39 : 진온리(1)
# sheetInfo = 39

# 시트 40 : 양+동위+공통(3)
# sheetInfo = 40

# 시트 41 : 동위온리(1)
# sheetInfo = 41

# 시트 42 : 양+후북위+공통(15)
# sheetInfo = 42

# 시트 43 : 남제온리(1)
# sheetInfo = 43

# 시트 44 : 후북위+남제+공통(3)
# sheetInfo = 44

# 시트 45 : 선북위+남제+공통(8)
# sheetInfo = 45

# sheetName = dplyr::case_when(
#   sheetInfo == 1 ~ "테스트"
#   , sheetInfo == 2 ~ "초기신라(16)"
#   , sheetInfo == 3 ~ "후기신라(9)"
#   , sheetInfo == 4 ~ "전한(43)"
#   , sheetInfo == 4 ~ "전한(43)+수정"
#   , sheetInfo == 5 ~ "당나라(14)"
#   , sheetInfo == 6 ~ "최종(8)"
#   , sheetInfo == 7 ~ "청온리(18)"
#   , sheetInfo == 8 ~ "청-조선공통(92)"
#   , sheetInfo == 9 ~ "조선온리(20)"
#   , sheetInfo == 10 ~ "북명+조선공통(74)"
#   , sheetInfo == 11 ~ "남명+조선공통(6)"
#   , sheetInfo == 12 ~ "개경조선온리(1)"
#   , sheetInfo == 13 ~ "남명+개경조선(2)"
#   , sheetInfo == 14 ~ "남명온리(17)"
#   , sheetInfo == 15 ~ "북명온리(14)"
#   , sheetInfo == 16 ~ "남원+남송공통(4)"
#   , sheetInfo == 17 ~ "남원온리(39)"
#   , sheetInfo == 18 ~ "남송+선금공통(6)"
#   , sheetInfo == 19 ~ "남송+중금공통(19)"
#   , sheetInfo == 20 ~ "남송+후금공통(6)"
#   , sheetInfo == 21 ~ "남송온리(17)"
#   , sheetInfo == 22 ~ "북원+남송공통(5)"
#   , sheetInfo == 23 ~ "후금온리(1)"
#   , sheetInfo == 24 ~ "중금온리(2)"
#   , sheetInfo == 25 ~ "선금+북송공통(3)"
#   , sheetInfo == 26 ~ "선금+요+공통(1)"
#   , sheetInfo == 27 ~ "북송+요+공통(17)"
#   , sheetInfo == 28 ~ "북송온리(49)"
#   , sheetInfo == 29 ~ "요+후당공통(2)"
#   , sheetInfo == 30 ~ "요온리(7)"
#   , sheetInfo == 31 ~ "후진온리(8)"
#   , sheetInfo == 32 ~ "후당온리(4)"
#   , sheetInfo == 33 ~ "후한온리(3)"
#   , sheetInfo == 34 ~ "낙양당온리(11)"
#   , sheetInfo == 35 ~ "시안당온리(67)"
#   , sheetInfo == 36 ~ "진+수+공통(1)"
#   , sheetInfo == 37 ~ "수온리(3)"
#   , sheetInfo == 38 ~ "진+북주+공통(7)"
#   , sheetInfo == 39 ~ "진온리(1)"
#   , sheetInfo == 40 ~ "양+동위+공통(3)"
#   , sheetInfo == 41 ~ "동위온리(1)"
#   , sheetInfo == 42 ~ "양+후북위+공통(15)"
#   , sheetInfo == 43 ~ "남제온리(1)"
#   , sheetInfo == 44 ~ "후북위+남제+공통(3)"
#   , sheetInfo == 45 ~ "선북위+남제+공통(8)"
#   , TRUE ~ NA_character_
# )

# 시트 1 : 유송온리(3)
# sheetInfo = 1

# 시트 2 : 선북위+유송공통(15)
# sheetInfo = 2

# 시트 3 : 동진+북위공통(3)
# sheetInfo = 3

# 시트 4 : 동진온리(21)
# sheetInfo = 4

# 시트 5 : 선북위+남제+공통(8)
# sheetInfo = 5

# 시트 6 : 선서진온리(15)
# sheetInfo = 6

# 시트 7 : 위온리(13)
# sheetInfo = 7

# 시트 8 : BC진온리(1)
# sheetInfo = 8

# 시트 9 : 동주+BC진+공통(6)
# sheetInfo = 9

# 시트 10 : 온리동주(2)
# sheetInfo = 10

# 시트 11 : 동주+노+공통(33)
# sheetInfo = 11

# 시트 12 : 초기신라(16)
# sheetInfo = 12

# sheetName = dplyr::case_when(
#   sheetInfo == 1 ~ "유송온리(3)"
#   , sheetInfo == 2 ~ "선북위+유송공통(15)"
#   , sheetInfo == 3 ~ "동진+북위공통(3)"
#   , sheetInfo == 4 ~ "동진온리(21)"
#   , sheetInfo == 5 ~ "선북위+남제+공통(8)"
#   , sheetInfo == 6 ~ "선서진온리(15)"
#   , sheetInfo == 7 ~ "위온리(13)"
#   , sheetInfo == 8 ~ "BC진온리(1)"
#   , sheetInfo == 9 ~ "동주+BC진+공통(6)"
#   , sheetInfo == 10 ~ "온리동주(2)"
#   , sheetInfo == 11 ~ "동주+노+공통(33)"
#   , sheetInfo == 12 ~ "초기신라(16)"
#   , TRUE ~ NA_character_
# )

# 시트 1 : 낙양당첫번째(9)
# sheetInfo = 1

# 시트 2 : 낙양당두번째(2)
# sheetInfo = 2

# 시트 3 : 시안당첫번째(25)
# sheetInfo = 3

# 시트 4 : 시안당두번째(42)
# sheetInfo = 4


# sheetName = dplyr::case_when(
#   sheetInfo == 1 ~ "낙양당첫번째(9)"
#   , sheetInfo == 2 ~ "낙양당두번째(2)"
#   , sheetInfo == 3 ~ "시안당첫번째(25)"
#   , sheetInfo == 4 ~ "시안당두번째(42)"
#   , TRUE ~ NA_character_
# )

# 시트 1 : 모집단78개
sheetInfo = 1

sheetName = dplyr::case_when(
  sheetInfo == 1 ~ "모집단78개"
  , TRUE ~ NA_character_
)


# **************************************************
# 단일 이미지 테스트
# **************************************************
# openxlsx::read.xlsx(fileInfo2, sheetIndex = sheetInfo)
# data = openxlsx::read.xlsx(fileInfo, sheet = sheetName)

# typeList = data$type %>% unique()
# typeList = typeList[41:45]
# typeList = typeList[56:78]

# typeList = typeList[77:78]

# typeInfo = typeList[1]
# for (typeInfo in typeList) {
#
#   tmpData = data %>%
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
#
# }

# 마리오 알람 소리
# beepr::beep(sound = 8)


# **************************************************
# 시트에 따른 데이터 병합
# **************************************************
sheetList = c(1)
sheetName = "모집단78개"

# sheetInfo = sheetList[1]

dataL3 = tibble::tibble()
for (sheetInfo in sheetList) {
  
  # data = xlsx::read.xlsx(fileInfo, sheetIndex = sheetInfo) %>%
  data = openxlsx::read.xlsx(fileInfo, sheet = sheetInfo) %>%
    tibble::as.tibble()
  
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
# cat(sprintf("[CHECK] type : %s",dataL3$type %>% unique %>% length), "\n")
#
# # selList = c(1, 6, 7, 9, 13, 15, 20, 53, 55, 64, 75, 76, 77, 78)
#
# dataL4 = dataL3 %>%
#   # dplyr::filter(type %in% selList) %>%
#   dplyr::group_by(xAxis, yAxis) %>%
#   dplyr::summarise(
#     meanVal = mean(zAxis, na.rm = TRUE)
#   ) %>%
#   dplyr::mutate(
#     meanVal = ifelse(meanVal < 0, 0, meanVal)
#   )
#
# summary(dataL4)
#
# maxData = dataL4 %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(meanVal == max(meanVal, na.rm = TRUE))
#
#
# # 경주 지점
# posLon = 129.2
# posLat = 35.8
#
# posData = dataL4 %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(xAxis == posLon, yAxis == posLat)
#
# # setBreakCont = c(seq(0.70, 0, -0.04))
# # setBreakText = c(seq(0.70, 0.10, -0.04))
#
# setBreakCont = c(seq(0.78, 0, -0.04))
# setBreakText = c(seq(0.78, 0.10, -0.04))
#
# # setBreakCont = c(seq(0.58, 0, -0.04))
# # setBreakText = c(seq(0.58, 0.10, -0.04))
#
# # setBreakCont = c(seq(0.72, 0, -0.04))
# # setBreakText = c(seq(0.72, 0.10, -0.04))
# #
# # setBreakCont = c(seq(0.44, 0, -0.02))
# # setBreakText = c(seq(0.44, 0.10, -0.02))
# #
# # setBreakCont = c(seq(0.47, 0, -0.02))
# # setBreakText = c(seq(0.47, 0.10, -0.02))
#
# # 평균식분도에 대한 최대값입니다.
# # 모집단78개 : 0.7816083
#
# saveImg = sprintf("%s/%s/%s_%s.png", globalVar$figPath, serviceName, sheetName, "Mean_Color")
#
# ggplot(data = dataL4, aes(x = xAxis, y = yAxis, fill = meanVal, z = meanVal)) +
#   geom_raster(interpolate = TRUE, na.rm = TRUE) +
#   # metR::geom_contour_fill(na.fill = TRUE, kriging = TRUE) +
#   scale_fill_gradientn(colours = cbMatlab, limits = c(0, 1.0), breaks = seq(0, 1.0, 0.2), na.value = NA) +
#   geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = NA) +
#   metR::geom_contour2(color = "black", alpha = 1.0, breaks = setBreakCont, show.legend = FALSE, size = 0.5) +
#   metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = setBreakText, rotate = TRUE, na.rm = TRUE, size = 5) +
#   geom_point(data = maxData, aes(x = xAxis, y = yAxis), color = "red") +
#   metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(89.99, 150.01), expand = c(0, 0)) +
#   metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(9.99, 60), expand = c(0, 0)) +
#   labs(
#     subtitle = NULL
#     , x = NULL
#     , y = NULL
#     , fill = NULL
#     , colour = NULL
#     , title = NULL
#   ) +
#   theme(text = element_text(size = 18)) +
#   ggsave(filename = saveImg, width = 10, height = 10, dpi = 600)
#
# saveImg = sprintf("%s/%s/%s_%s.png", globalVar$figPath, serviceName, sheetName, "Mean_Black")
#
# ggplot(data = dataL4, aes(x = xAxis, y = yAxis, fill = meanVal, z = meanVal)) +
#   # geom_raster(interpolate = TRUE, na.rm = TRUE) +
#   # scale_fill_gradientn(colours = cbMatlab, limits = c(0, 1.0), breaks = seq(0, 1.0, 0.2), na.value = NA) +
#   # metR::geom_contour_fill(na.fill = TRUE, kriging = TRUE)
#   geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = NA) +
#   metR::geom_contour2(color = "black", alpha = 1.0, breaks = setBreakCont, show.legend = FALSE, size = 0.5) +
#   metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = setBreakText, rotate = TRUE, na.rm = TRUE, size = 5) +
#    geom_point(data = maxData, aes(x = xAxis, y = yAxis), color = "red") +
#   metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(89.99, 150.01), expand = c(0, 0)) +
#   metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(9.99, 60), expand = c(0, 0)) +
#   labs(
#     subtitle = NULL
#     , x = NULL
#     , y = NULL
#     , fill = NULL
#     , colour = NULL
#     , title = NULL
#   ) +
#   theme(text = element_text(size = 18)) +
#   ggsave(filename = saveImg, width = 10, height = 10, dpi = 600)


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
sampleData = openxlsx::read.xlsx(fileInfo, sheet = "그룹정보")

sampleDataL1 = dataL3 %>%
  dplyr::left_join(sampleData, by = c("type" = "type")) %>%
  dplyr::select(-group, -type)

# sampleInfo = dataL3$type %>% unique()
sampleInfo = sampleData$type %>% unique()

# 부트스트랩 횟수
# bootDo = 10
# bootDo = 1000
bootDo = 10000
# bootDo = 300000

# 경주 지점
posLon = 129.2
posLat = 35.8

# 부스스트랩 주사위 목록
# bostSample = lapply(1:bootDo, function(i) sample(sampleInfo, size = 14, replace = FALSE))
# bostSample = lapply(1:bootDo, function(i) sampling::strata(c("group"), size = c(9, 3, 2), method = "srswor", data=sampleData)$ID_unit)
# bostSample = lapply(1:bootDo, function(i) sampling::strata(c("group"), size = c(27, 9, 6), method = "srswor", data=sampleData)$ID_unit)
# bostSample = lapply(1:bootDo, function(i) sampling::strata(c("group"), size = c(3, 2, 9), method = "srswor", data=sampleData)$ID_unit)

# options(future.globals.maxSize = 9999999999999999)
# plan(multisession, workers = parallelly::availableCores() - 5)
# plan(multisession, workers = parallelly::availableCores() / 2)
# plan(multisession, workers = parallelly::availableCores() - 10)
# plan(multisession, workers = parallelly::availableCores() - 15)
# plan(multisession, workers = parallelly::availableCores() - 20)

# 부트스트랩 추출 개수
# bootNum = 70
# bootNumList = c(16)
bootNumList = c(14)
# bootNumList = c(42)
# bootNumList = c(16, 30)
# bootNumList = c(30, 50, 60, 70)
# bootNumList = c(50, 60, 70)
# bootNumList = c(78)

# 병렬횟수 설정
bootIdxList = seq(1, 30)
# bootIdxList = seq(31, 60)
# bootIdxList = seq(1, 300)
# bootIdxList = seq(301, 600)
# bootIdxList = seq(1, 5)
# bootIdx = bootIdxList[1]
# bootNum = bootNumList[1]

for (bootIdx in bootIdxList) {

  cat(sprintf("[CHECK] bootIdx : %s", bootIdx), "\n")

  # 부스스트랩 주사위 목록
  bostSample = lapply(1:bootDo, function(i) sample(sampleInfo, size = 14, replace = FALSE))

  options(future.globals.maxSize = 9999999999999999)
  # plan(multisession, workers = parallelly::availableCores() - 5)
  plan(multisession, workers = parallelly::availableCores() - 10)
  # plan(multisession, workers = parallelly::availableCores() - 20)
  # plan(multisession, workers = 10)

  for (bootNum in bootNumList) {

    bostSampleL1 = data.frame(t(sapply(bostSample, c)))
    # saveFile = sprintf("%s/%s/bostSampleL1_%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo)
    saveFile = sprintf("%s/%s/bostSampleL1_%s-%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo, bootIdx)
    dir.create(path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
    readr::write_csv(x = bostSampleL1, file = saveFile)

    # 부트스트랩을 통해 병렬처리
    bootSelData = furrr::future_map_dfr(1:bootDo, function(i) {
      sampleDataL1 %>%
        dplyr::filter(sampleType %in% bostSample[[i]]) %>%
        dplyr::group_by(xAxis, yAxis) %>%
        dplyr::summarise(
          meanVal = mean(zAxis, na.rm = TRUE)
        ) %>%
        dplyr::mutate(
          meanVal = ifelse(meanVal < 0, 0, meanVal)
          # , idx = i
        ) %>%
        dplyr::ungroup() %>%
        mutate_cond(xAxis == posLon & yAxis == posLat, posVal = meanVal) %>%
        dplyr::mutate(posVal = na.exclude(posVal)) %>%
        dplyr::filter(meanVal == max(meanVal, na.rm = TRUE)) %>%
        dplyr::mutate(idx = i)
    })

    # saveFile = sprintf("%s/%s/bootSelData_%s-%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo, bootIdx)
    saveFile = sprintf("%s/%s/bootSelData_%s-%s-%s_%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo, bootIdx, posLon, posLat)
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

# # bootNumList = c(30, 50, 60, 70)
# bootNumList = c(14)
# # bootNumList = c(42)
# # bootNumList = c(16, 30)
# # bootNum = bootNumList[1]
# for (bootNum in bootNumList) {
#
#   # saveFile = sprintf("%s/%s/bootData_%s.csv", globalVar$outPath, serviceName, bootNum)
#   # saveFile = sprintf("%s/%s/bootData_%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo)
#   # bootMaxData = readr::read_csv(file = saveFile)
#
#   # saveFile = sprintf("%s/%s/bootPosData_%s_%s-%s.csv", globalVar$outPath, serviceName, bootNum, posLon, posLat)
#   # saveFile = sprintf("%s/%s/bootPosData_%s-%s_%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo, posLon, posLat)
#   # bootPosData = readr::read_csv(file = saveFile)
#
#   # bootIdx = bootIdxList[1]
#   #
#   # bootData = tibble::tibble()
#   # for (bootIdx in bootIdxList) {
#   #   saveBootFile = sprintf("%s/%s/bootData_%s-%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo, bootIdx)
#   #   bootMaxData = readr::read_csv(file = Sys.glob(saveBootFile))
#   #
#   #   saveBootPosFile = sprintf("%s/%s/bootPosData_%s-%s-%s_%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo, bootIdx, posLon, posLat)
#   #   bootPosData = readr::read_csv(file = Sys.glob(saveBootPosFile))
#   #
#   #   selData = bootMaxData %>%
#   #     dplyr::left_join(bootPosData, by = c("idx" = "idx"))
#   #
#   #   bootData = dplyr::bind_rows(bootData, selData)
#   # }
#
#   # bootData = bootData %>%
#   #   dplyr::filter(posVal >= 0.69)
#
#   # saveFile = sprintf("%s/%s/bootData_%s-%s-*.csv", globalVar$outPath, serviceName, bootNum, bootDo)
#   # fileList = Sys.glob(saveFile)
#   #
#   # bootMaxData = fileList %>%
#   #   purrr::map(readr::read_csv) %>%
#   #   purrr::reduce(dplyr::bind_rows)
#   #
#   # saveFile = sprintf("%s/%s/bootPosData_%s-%s-*_%s-%s.csv", globalVar$outPath, serviceName, bootNum, bootDo, posLon, posLat)
#   # fileList = Sys.glob(saveFile)
#   #
#   # bootPosData = fileList %>%
#   #   purrr::map(readr::read_csv) %>%
#   #   purrr::reduce(dplyr::bind_rows)
#
#
#   cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")
#
#   # bootData = bootMaxData
#
#   # bootData = bootMaxData %>%
#   #   dplyr::filter(meanVal >= 0.78)
#
#   # bootData = bootMaxData %>%
#   #   dplyr::left_join(bootPosData, by = c("idx" = "idx")) %>%
#   #   dplyr::filter(posVal >= 0.69)
#
#
#   # summary(bootData)
#
#   # saveImg = sprintf("%s/%s/%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Color Overlay", bootNum)
#   # saveImg = sprintf("%s/%s/%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Color Overlay", bootNum, bootDo)
#   saveImg = sprintf("%s/%s/%s-%s_%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Color Overlay", bootNum, bootDo, posLon, posLat)
#   # saveImg = sprintf("%s/%s/%s_%s-%s-%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Color Overlay", bootNum, posLon, posLat)
#   dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
#   # 1. 만개의 점이 찍혀있는 위도 경도 그래프
#   makePlot = ggplot(data = bootData, aes(x = xAxis, y = yAxis, color = meanVal)) +
#     # geom_point(size = 2, show.legend = TRUE) +
#     geom_point(size = 1, show.legend = TRUE, alpha=0.3) +
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
#   ggplot2::last_plot()
#   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#
#
#
#   # saveImg = sprintf("%s/%s/%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Black Overlay", bootNum)
#   # saveImg = sprintf("%s/%s/%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Black Overlay", bootNum, bootDo)
#   saveImg = sprintf("%s/%s/%s-%s_%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Black Overlay", bootNum, bootDo, posLon, posLat)
#   #  saveImg = sprintf("%s/%s/%s_%s-%s-%s-%s.png", globalVar$figPath, serviceName, sheetName, "Mean Black Overlay", bootNum, posLon, posLat)
#   dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
#   makePlot = ggplot(data = bootData, aes(x = xAxis, y = yAxis, color = meanVal)) +
#     # geom_point(size = 2, color = "black", show.legend = FALSE) +
#     geom_point(size = 1, color = "black", show.legend = FALSE, alpha=0.3) +
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
#   ggsave(makePlot, filename = saveImg, width = 10, height = 10, dpi = 600)
#   ggplot2::last_plot()
#   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#
#   # 2. 위도 28~34/경도 110~116 구역안의 점 개수
#   statData = bootData %>%
#     dplyr::filter(
#       dplyr::between(xAxis, 110, 116)
#       , dplyr::between(yAxis, 28, 34)
#     ) %>%
#     dplyr::summarise(cnt = n())
#
#   # 3. 위도 34~42/경도 124~130 구역안의 점 개수
#   statDataL1 = bootData %>%
#     dplyr::filter(
#       dplyr::between(xAxis, 124, 130)
#       , dplyr::between(yAxis, 34, 42)
#     ) %>%
#     dplyr::summarise(cnt = n())
#
#     cat(sprintf("[CHECK] statData : %s", statData), "\n")
#     cat(sprintf("[CHECK] statDataL1 : %s", statDataL1), "\n")
# }
#
# statDataL2 = bootData %>%
#   dplyr::filter(
#     meanVal >= 0.7
#   ) %>%
#   dplyr::summarise(cnt = n())
#
# 1. 만개의 점이 찍혀있는 위도 경도 그래프 : 2. 위도 경도 그래프 폴더 참조
# 2. 위도 28~34/경도 110~116 구역안의 점 개수 : 171개
# 3. 위도 34~42/경도 124~130 구역안의 점 개수 : 1761개
#
# 1. 만개의 점이 찍혀있는 위도 경도 그래프 : 20221228_결과 폴더 참조
# 2. 위도 28~34/경도 110~116 구역안의 점 개수 : 240
# 3. 위도 34~42/경도 124~130 구역안의 점 개수 : 607
#
#
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