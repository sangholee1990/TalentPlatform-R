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
# R을 이용한 일반음식점 데이터 기반으로 지도 및 막대그래프 시각화

# https://www.localdata.go.kr/devcenter/dataDown.do?menuNo=20001
# 기존 좌표계와 동일한 중부원점TM(EPSG:2097)

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0487"

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
library(openxlsx)
library(lubridate)
library(fs)
library(sf)
library(forcats)
library(showtext)
library(ggplot2)
library(sf)

showtext::showtext_opts(dpi = 600)
showtext::showtext.auto()

# 파일 읽기
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "07_24_04_P_일부.csv"))
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "07_24_04_P.csv"))

# data = readr::read_csv(fileInfo, locale = readr::locale(encoding = "EUC-KR"))
data = readr::read_csv(fileInfo, locale = readr::locale(encoding = "UTF-8"))

# 업종별 개폐업현황
dataL1 = data %>% 
  dplyr::rename(
    geoX = "좌표정보(X)"
    , geoY = "좌표정보(Y)"
  ) %>% 
  dplyr::select(영업상태명, 업태구분명, 인허가일자, geoX, geoY) %>% 
  na.omit()

# 좌표 변환
sfData = st_as_sf(dataL1, coords = c("geoX", "geoY"), crs = 2097)
sfDataL1 = st_transform(sfData, 4326)

dataL2 = sfDataL1 %>%
  dplyr::mutate(
    lon = st_coordinates(.)[,1]
    , lat = st_coordinates(.)[,2]
  ) %>%
  st_set_geometry(NULL) 

# summary(dataL2)

# 연도별 일반음식점 (전체) 개폐업 현황
dataL3 = dataL2 %>% 
  dplyr::mutate(
    dtYear = format(인허가일자, "%Y") %>% as.numeric()
  ) %>% 
  dplyr::group_by(dtYear, 영업상태명) %>%
  dplyr::summarise(
    cnt = n()
  )

mainTitle = sprintf("연도별 일반음식점 (%s) 개폐업 현황", "전체")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL3, aes(x = dtYear, y = cnt, fill = 영업상태명)) +
  geom_bar(stat = "identity", width = 1.0, position = position_dodge(width = 1.0)) +
  labs(x = "연도 ", y = "개수", fill = NULL, title = NULL, subtitle = mainTitle) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 연도별 일반음식점 (업태구분명) 개폐업 현황
typeList = dataL2$업태구분명 %>% unique() %>% sort()
for (typeInfo in typeList) {

  dataL4 = dataL2 %>% 
    dplyr::filter(
      업태구분명 == typeInfo
    ) %>% 
    dplyr::mutate(
      dtYear = format(인허가일자, "%Y") %>% as.numeric()
    ) %>% 
    dplyr::group_by(dtYear, 영업상태명) %>%
    dplyr::summarise(
      cnt = n()
    )
  
  mainTitle = sprintf("연도별 일반음식점 (%s) 개폐업 현황", typeInfo)
  saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
  dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
  
  makePlot = ggplot(dataL4, aes(x = dtYear, y = cnt, fill = 영업상태명)) +
    # geom_bar(stat = "identity", width = 0.5, position=position_dodge(width = 0.5)) +
    # geom_bar(stat = "identity", position=position_dodge(width = 0.5)) +
    geom_bar(stat = "identity", width = 1.0, position = position_dodge(width = 1.0)) +
    labs(x = "연도 ", y = "개수", fill = NULL, title = NULL, subtitle = mainTitle) +
    theme(
      text = element_text(size = 16)
      , legend.position = "top"
    )
  
  ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
  
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
}

# 지도 시각화
# 대한민국 지리 데이터
mapKor = sf::st_read(file.path(globalVar$mapPath, "gadm36_KOR_shp/gadm36_KOR_1.shp"))
mapKor2 = sf::st_read(file.path(globalVar$mapPath, "gadm36_KOR_shp/gadm36_KOR_2.shp"))

typeList = dataL2$영업상태명 %>% unique() %>% sort()
for (typeInfo in typeList) {
  
  dataL5 = dataL2 %>%
    dplyr::filter(영업상태명 == typeInfo) %>%
    dplyr::select(lon, lat)

  mainTitle = sprintf("일반음식점 %s 지도", typeInfo)
  saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
  dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

  makePlot = ggplot() +
    geom_sf(data = mapKor2, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = "white") +
    geom_sf(data = mapKor, aes(x = NULL, y = NULL, fill = NULL, z = NULL), lwd = 0.5, color = "black", fill = NA) +
    # geom_point(data = dataL5, aes(x = lon, y = lat), alpha = 0.1) +
    stat_density_2d(data = dataL5, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = "polygon") +
    scale_fill_gradient(low = "green", high = "red") +
    scale_alpha(range = c(0, 1.0), guide = FALSE) +
    metR::scale_x_longitude(breaks = seq(125, 131, 1), limits = c(125, 131), expand = c(0, 0)) +
    metR::scale_y_latitude(breaks = seq(33, 39, 1), limits = c(33, 39), expand = c(0, 0)) +
    labs(subtitle = mainTitle, x = NULL, y = NULL, fill = NULL, colour = NULL, title = NULL)
  
  ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)

  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
}
