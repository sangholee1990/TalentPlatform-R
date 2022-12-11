
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
# read.xlsx(fileInfo2, sheetIndex = sheetInfo)
# get_personal_onedrive()

library(Microsoft365R)
options(browser="google-chrome")
Microsoft365R::get_business_onedrive()
fileInfo2 = "https://gwnuackr-my.sharepoint.com/:x:/g/personal/20155194_gwnu_ac_kr/EVXebuIGC89EoNVpnfC5frwBmuI3X_T8HZ9Nel2FqI7vzw?e=2qJdaw"
data = openxlsx::read.xlsx(fileInfo2) %>%
  as.tibble()

# data = readxl::read_excel(fileInfo, sheet = sheetInfo) %>%
#   as.tibble()

typeList = data$type %>% unique() %>% sort()

# typeInfo = typeList[1]
for (typeInfo in typeList) {

  tmpData = data %>%
    dplyr::filter(
      type == typeInfo
      , ! is.na(val)
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
    dplyr::mutate(
      type = typeInfo
    )

  idx = which(dataL2$zAxis == max(dataL2$zAxis, na.rm = TRUE))
  maxData = dataL2[idx, ]

  makePlot = ggplot(data = dataL2, aes(x = xAxis, y = yAxis, fill = zAxis, z = zAxis)) +
    geom_raster(interpolate = TRUE, na.rm = TRUE) +
    # metR::geom_contour_fill(na.fill = TRUE, kriging = TRUE) +.
    # geom_tile() +
    scale_fill_gradientn(colours = cbMatlab, limits = c(0, 1.1), breaks = c(0, 0.3, 0.5, 0.7, 0.9), na.value = NA) +
    # metR::geom_contour2(color = "black", alpha = 1.0, breaks = seq(0.3, 0.9, 0.2), show.legend = FALSE) +
    geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = NA) +
    metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0, show.legend = FALSE, size = 0.1) +
    metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.3, show.legend = FALSE, size = 0.5) +
    metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.5, show.legend = FALSE, size = 1) +
    metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.7, show.legend = FALSE, size = 2) +
    metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.9, show.legend = FALSE, size = 4) +
    geom_point(data = tmpData, aes(x = lon, y = lat, colour = factor(val), fill = NULL, z = NULL)) +
    geom_point(data = maxData, aes(x = xAxis, y = yAxis, colour = meanVal, fill = NULL, z = NULL), color = "red") +
    metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = c(0, 0.3, 0.5, 0.7, 0.9), rotate = TRUE, na.rm = TRUE, size = 5) +
    metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(90, 150), expand = c(0, 0)) +
    metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(10, 60), expand = c(0, 0)) +
    labs(
      subtitle = NULL
      , x = NULL
      , y = NULL
      , fill = NULL
      , colour = NULL
      , title = NULL
    ) +
    theme(text = element_text(size = 18))

  saveImg = sprintf("%s/%s_%s_%s.png", globalVar$figPath, serviceName, sheetName, typeInfo)
  ggsave(plot = makePlot, filename = saveImg, width = 10, height = 10, dpi = 600)

}

# 마리오 알람 소리
beepr::beep(sound = 8)


# **************************************************
# 시트에 따른 데이터 병합
# **************************************************
# sheetList = c(sheetInfo)
# sheetName = "후기신라(9)"

sheetList = c(16, 18, 19, 20, 21, 22)
sheetName = "남송(57)"

# sheetInfo = sheetList[1]

dataL3 = tibble::tibble()
for (sheetInfo in sheetList) {
  
  data = xlsx::read.xlsx(fileInfo, sheetIndex = sheetInfo) %>%
    as.tibble()
  
  typeList = data$type %>% unique %>% sort
  
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
cat(
  sprintf("%s : %s", "Type Length : ", dataL3$type %>% unique %>% length)
  , "\n"
)


dataStatL3 = dataL3 %>%
  dplyr::filter(
    zAxis > 0
    # , type %in% c(3, 4)
  ) %>% 
  dplyr::mutate(
    cnt = ifelse(zAxis <= 0, 0, 1)
  ) %>% 
  dplyr::group_by(xAxis, yAxis) %>%
  dplyr::summarise(
    meanVal = mean(zAxis, na.rm = TRUE)
    , sumVal = sum(cnt, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    meanVal = ifelse(meanVal < 0, NA, meanVal)
  ) %>%
  dplyr::arrange(desc(sumVal))


dataL4 = dataStatL3 %>% 
  dplyr::filter(
    meanVal > 0
    , sumVal == max(dataStatL3$sumVal, na.rm = TRUE)
    # , sumVal == 49
    # , sumVal == 48
    # , sumVal == 0
    # , sumVal < 2
    # , sumVal > 0
    )


# 에측 결과 저장
saveFile = sprintf("%s/%s_%s.xlsx", globalVar$outPath, serviceName, "일식 식분도 이미지 데이터 추출 및 중첩 일식 식분도 빈도수 시각화")
wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Sheet")
openxlsx::writeData(wb, "Sheet", dataL4, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
openxlsx::saveWorkbook(wb, file = saveFile, overwrite = TRUE)


summary(dataL4)

idx = which(dataL4$meanVal == max(dataL4$meanVal, na.rm = TRUE))
maxData = dataL4[idx, ]

# setBreakCont = c(seq(0.51, 0, -0.04))
# setBreakText = c(seq(0.51, 0.10, -0.04))
# setBreakCont = c(seq(0.53, 0, -0.001))
# setBreakText = c(seq(0.53, 0.10, -0.001))

setBreakCont = c(seq(0.52, 0, -0.001))
setBreakText = c(seq(0.52, 0.10, -0.001))

# 0.954

# 평균식분도에 대한 최대값입니다.
# 낙양당첫번째(9) : 0.954

saveImg = sprintf("%s/%s_%s_%s.png", globalVar$figPath, serviceName, sheetName, "Mean Color Overlay")

ggplot(data = dataL4, aes(x = xAxis, y = yAxis, fill = meanVal, z = meanVal)) +
  geom_point(color = "black", size = 0.05, show.legend = FALSE) +
  geom_raster(interpolate = TRUE, na.rm = TRUE) +
  # geom_tile(interpolate = TRUE, na.rm = TRUE) +
  # metR::geom_contour_fill(na.fill = TRUE, kriging = TRUE) +
  # scale_fill_gradientn(colours = cbMatlab, limits = c(0, 1.0), breaks = seq(0, 1.0, 0.2), na.value = NA) +
  scale_fill_gradientn(colours = "black", limits = c(0, 1.0), breaks = seq(0, 1.0, 0.2), na.value = NA) +
  geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = NA) +
  metR::geom_contour2(color = "black", alpha = 1.0, breaks = setBreakCont, show.legend = FALSE, size = 0.5) +
  # metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = setBreakText, rotate = TRUE, na.rm = TRUE, size = 5) +
  # metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = setBreakText, rotate = TRUE, na.rm = TRUE, size = 2) +
  geom_point(data = maxData, aes(x = xAxis, y = yAxis, colour = meanVal, fill = NULL, z = NULL), color = "red") +
  metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(90, 150), expand = c(0, 0)) +
  metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(10, 60), expand = c(0, 0)) +
  labs(
    subtitle = NULL
    , x = NULL
    , y = NULL
    , fill = NULL
    , colour = NULL
    , title = NULL
  ) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 10, dpi = 600)

saveImg = sprintf("%s/%s_%s_%s.png", globalVar$figPath, serviceName, sheetName, "Mean Black Overlay")

ggplot(data = dataL4, aes(x = xAxis, y = yAxis, z = meanVal)) +
  geom_point(color = "black", size = 0.05, show.legend = FALSE) +
  # geom_raster(interpolate = TRUE, na.rm = TRUE) +
  # scale_fill_gradientn(colours = cbMatlab, limits = c(0, 1.0), breaks = seq(0, 1.0, 0.2), na.value = NA) +
  # metR::geom_contour_fill(na.fill = TRUE, kriging = TRUE)
  geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = NA) +
  metR::geom_contour2(color = "black", alpha = 1.0, breaks = setBreakCont, show.legend = FALSE, size = 0.5) +
  # metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = setBreakText, rotate = TRUE, na.rm = TRUE, size = 5) +
  # metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = setBreakText, rotate = TRUE, na.rm = TRUE, size = 2) +
  geom_point(data = maxData, aes(x = xAxis, y = yAxis, colour = meanVal, fill = NULL, z = NULL), color = "red") +
  metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(90, 150), expand = c(0, 0)) +
  metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(10, 60), expand = c(0, 0)) +
  labs(
    subtitle = NULL
    , x = NULL
    , y = NULL
    , fill = NULL
    , colour = NULL
    , title = NULL
  ) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 10, dpi = 600)


# 마리오 알람 소리
beepr::beep(sound = 8)

