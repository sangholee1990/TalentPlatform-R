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
# R을 이용한 지점별 평균-누적 강수량 내삽 및 등우선도 시각화

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0421"

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
    , "mapPath" = contextPath
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
library(here)
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
library(readxl)
library(furrr)
library(sampling)
library(future)
library(maps)
library(sp)
library(sf)


xRange = as.numeric(c(120, 135))
yRange = as.numeric(c(30, 40))

newLon = seq(from = xRange[1], to = xRange[2], by = 0.1)
newLat = seq(from = yRange[1], to = yRange[2], by = 0.1)

gridData = noncompliance::expand.grid.DT(
  newLon
  , newLat
  , col.names = c("lon", "lat")
) %>% 
  tibble::as.tibble()

# 지도 읽기
# mapGlobal = sf::st_read(file.path(globalVar$mapPath, "gshhg-shp-2.3.6/GSHHS_shp/i/GSHHS_i_L1.shp"))
mapKor = sf::st_read(file.path(globalVar$mapPath, serviceName, "gadm36_KOR_shp/gadm36_KOR_1.shp"))
mapKor2 = sf::st_read(file.path(globalVar$mapPath, serviceName, "gadm36_KOR_shp/gadm36_KOR_2.shp"))
mapJpn = sf::st_read(file.path(globalVar$mapPath, serviceName, "gadm36_JPN_shp/gadm36_JPN_1.shp"))
mapPrk = sf::st_read(file.path(globalVar$mapPath, serviceName, "gadm36_PRK_shp/gadm36_PRK_1.shp"))


# 컬러바 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "사용자 컬러바.xlsx"))
cbAcc = openxlsx::read.xlsx(fileInfo, sheet = "누적") %>% 
  dplyr::mutate(color = rgb(r, g, b, maxColorValue = 255))

cbMean = openxlsx::read.xlsx(fileInfo, sheet = "평균") %>% 
  dplyr::mutate(color = rgb(r, g, b, maxColorValue = 255))


# 지점 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "sta_molit_kma_좌표.csv"))
posData = readr::read_csv(fileInfo, col_names = c("posId", "lat", "lon")) %>% 
  readr::type_convert()

# 평균/누적 읽기
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "2021_*.out"))

# fileInfo = fileList[1]
for (fileInfo in fileList) {
  
  data = readr::read_delim(fileInfo, delim = " ", col_names = c("posId", "val")) %>% 
    dplyr::mutate(posId = as.character(posId))
  
  # summary(data)

  fileNameNotExt = tools::file_path_sans_ext(fs::path_file(fileInfo))
  isType = stringr::str_detect(fileNameNotExt, regex("평균"))
  
  if (isType) {
    cbColor = cbMean$color
    setBreak = c(seq(20, 200, 10))
    setBreakContour = setBreak[seq_along(setBreak) %% 2 == 1]
    setMax = 200
  } else {
    cbColor = cbAcc$color
    setBreak = c(seq(0, 950, 50), seq(1000, 2000, 100))
    setBreakContour = setBreak[seq_along(setBreak) %% 2 == 1]
    setMax = 2000
  }
  
  # 데이터 병합
  dataL1 = data %>% 
    dplyr::left_join(posData, by = c("posId" = "posId")) %>%
    dplyr::select(-posId) %>% 
    dplyr::select(lon, lat, val) %>% 
    na.omit()
  
  # 공간 내삽 및 육지 마스킹
  dataL2 = MBA::mba.points(dataL1, gridData) %>% 
    as.data.frame() %>%
    as.tibble() %>%
    dplyr::rename(
      xAxis = xyz.est.x
      , yAxis = xyz.est.y
      , zAxis = xyz.est.z
    ) %>%
    dplyr::mutate(
      isMaskLand = metR::MaskLand(xAxis, yAxis, mask = "world")
    ) %>% 
    dplyr::filter(isMaskLand == TRUE)
  
    # 시각화
  makePlot = ggplot(data = dataL2, aes(x = xAxis, y = yAxis, fill = zAxis, z = zAxis)) +
    geom_raster(interpolate = TRUE, na.rm = TRUE) +
    geom_point(data = dataL1, aes(x = lon, y = lat, color = NULL, fill = NULL, z = NULL), color = "black", alpha = 0.5) +
    geom_sf(data = mapKor2, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = NA) +
    geom_sf(data = mapKor, aes(x = NULL, y = NULL, fill = NULL, z = NULL), lwd = 1, color = "black", fill = NA) +
    metR::geom_contour2(color = "red", alpha = 1.0, breaks = setBreakContour, show.legend = FALSE, size = 0.5) +
    metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = setBreakContour, rotate = TRUE, na.rm = TRUE, size = 5) +
    scale_fill_gradientn(colours = cbColor, limits = c(0, setMax), breaks = setBreak, na.value = NA) +
    geom_sf(data = mapJpn, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = "white") +
    geom_sf(data = mapPrk, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = "white") +
    metR::scale_x_longitude(breaks = seq(125, 131, 1), limits = c(125, 131), expand = c(0, 0)) +
    metR::scale_y_latitude(breaks = seq(33, 39, 1), limits = c(33, 39), expand = c(0, 0)) +
    labs(subtitle = NULL, x = NULL, y = NULL, fill = NULL, colour = NULL, title = NULL) +
    theme(
      text = element_text(size = 18)
      , legend.direction = "vertical"
      , legend.position = "right"
      , legend.key.height = unit(4.5, "cm")
    )
  
  # saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, fileNameNotExt)
  saveImg = sprintf("%s/%s.png", globalVar$figPath, fileNameNotExt)
  dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
  ggsave(plot = makePlot, filename = saveImg, width = 10, height = 10, dpi = 600)
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
}
