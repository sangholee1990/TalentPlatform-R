
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
# R을 이용한 MODIS (MCD13A2) TIFF 자료 처리 및 시각화

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0381"

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
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(magrittr)
library(openair)
library(lattice)
library(raster)
library(rgdal)
library(sp)

# *****************************************************
# 2022년 강원도 미세먼지 일평균 캘린더
# *****************************************************
# Sys.setlocale("LC_ALL", "ko_KR.UTF-8")

inpFile = Sys.glob(file.path(globalVar$inpPath, serviceName, "MCD13A2_8day_Phenology_US5_NDVI_Klosterman_2003.tif"))
data = raster::raster(inpFile)
spts <- rasterToPoints(data, spatial = TRUE)

dataL1 = sp::spTransform(spts, sp::CRS("+proj=longlat"))

dataL2 = as.data.frame(dataL1, xy=TRUE) %>%
  dplyr::rename("val" = "MCD13A2_8day_Phenology_US5_NDVI_Klosterman_2003")


head(dataL2)
ggplot(data = dataL2, aes(x = x, y = y, color = val, fill = val)) +
  geom_point()
  # geom_raster(interpolate = TRUE)
  # geom_tile()


plot(data)

summary(data)


# extQcData = raster::extract(data, df = TRUE, na.rm = FALSE) %>%
#   dplyr::select(-ID) %>%
#   t() %>%
#   as.tibble() %>%
#   magrittr::set_colnames("qc")

# raster
# str_name<-'MOD16A2_ET_0.05deg_GEO_2008M01.tif'
# imported_raster=raster(str_name)


data = openxlsx::read.xlsx(inpFile, sheet = 1) %>%
  as.tibble() %>%
  dplyr::mutate(
    date = readr::parse_datetime(sDate, "%Y-%m-%d")
  ) %>%
  dplyr::rename(
    "pm25" = "강원권.PM2.5"
  )

summary(data)

# data %>%
#   dplyr::filter(date == as.Date("2022-09-06"))

# plotSubTitle = sprintf("%s", "2022년 강원도 미세먼지 일평균 캘린더 시각화")
plotSubTitle = sprintf("%s", "강원권 PM2.5 농도")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
# ata, cuts = format(date, "%B-%Y"),

makeCalendarPlot(
  data
  , pollutant = "pm25"
  , year = 2021:2022
  , month = 1:12
  , annotate = "value"
  , breaks = c(0, 16, 36, 76, 500)
  , labels = c("좋음 (0~15)", "보통 (16~35)", "나쁨 (36~75)", "매우 나쁨 (76~)")
  , statistic = "mean"
  , cols = c("#518EF8", "#1CEE37", "#FFE81A", "#F13B61")
  , key.position = "bottom"
  , main = plotSubTitle
  , names = 'aa'
  , w.shift = 1
  , cuts = format(date, "%Y-%B")
  ) # +
 # ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

dev.off()

