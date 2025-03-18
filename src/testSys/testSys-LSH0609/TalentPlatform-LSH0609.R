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
# R을 이용한 전지구 해저면 평균 온도 처리 및 시각화

# 홈페이지에있는 코드를 따라해보고 있지만 다음과 같은 오류가 발생해서 해결하지못하고있습니다.
# 라이브러리 패키지는 tool-install packages에서 입력해서 설치하였습니다.
# library(insol), library(RWalc)에서 오류가 발생했습니다.
# 해결 방법 좀 부탁드립니다.

# 추가적으로 if ( x==""lx==".")은 수정해서 처리해봤습니다.
# 이 후 객체 'raData'를 찾을 수 없다는 오류가 발생했습니다.
# 이는 어떤 문제인지 알 수 있을까욤?
  
# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0609"

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
library(extrafont)
library(ncdf4)
library(tidyverse)
library(ncdump)
library(RNetCDF)
library(tidyverse)
library(lubridate)
library(gganimate)
# library(insol)
library(spData)
library(raster)

# Set Option
# memory.limit(size = 9999999999999)
# options(digits = 10)
# Sys.setlocale("LC_TIME", "english")

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "cmems_mod_glo_phy_anfc_0.083deg_P1D-m_1741320350983.nc"))

sFileDirName = Sys.glob(fileInfo)
liNcInfo = ncdump::NetCDF(sFileDirName)
liNcInfo

liNcOpen = ncdf4::nc_open(sFileDirName)
liNcOpen

# spatial components
# nLon = ncdf4::ncvar_get(liNcOpen, "lon")
# nLat = ncdf4::ncvar_get(liNcOpen, "lat")
nLon = ncdf4::ncvar_get(liNcOpen, "longitude")
nLat = ncdf4::ncvar_get(liNcOpen, "latitude")

# temporal component
nTime = ncdf4::ncvar_get(liNcOpen, "time")

# sea surface temperature
# nVal = ncdf4::ncvar_get(liNcOpen, "sst")
nVal = ncdf4::ncvar_get(liNcOpen, "tob")

# "days since 1800-1-1 00:00:00"
# liNcInfo$unlimdims$units
timeUnits = ncatt_get(liNcOpen, "time", "units")$value

# convert time original (to) to julian 
# nTo = insol::JDymd(year = 1800, month = 1, day = 1)
nTo = lubridate::parse_date_time(sub("days since ", "", timeUnits), orders = "ymd HMS")

dtDate = nTo + nTime

dplyr::tbl_df(
  data.frame(nTime, dtDate)
)


dfData = NULL

iCount = 1
# for (iCount in 1:length(dtDate)) {
for (iCount in 1:1) {
  
  dfTmpData = data.frame(
    dtDate = dtDate[iCount]
    , nLat = rep(nLat, each = length(nLon))
    , nLon = c(nLon)
    # , nVal = c(nVal[ , ,iCount])
    , nVal = c(nVal[ , ])
  )
  
  dfData = dplyr::bind_rows(dfData, dfTmpData)
}


dplyr::tbl_df(dfData)


iTime = length(dtDate)
# iTime = 1

dfData = data.frame(
  noncompliance::expand.grid.DT(dtDate[1:iTime], nLat, nLon)
  # , c(nVal[ , , 1:iTime])
  , c(nVal[ , ])
)
colnames(dfData) = c("dtDate", "nLat", "nLon", "nVal")
dplyr::tbl_df(dfData)


# 1.0 또는 0.5도 간격
geoInv = 0.5
# geoInv = 1.0

# L1 Processing Using Data Frame
# 불규칙 격자를 통해 규칙 격자화
dfDataL1 = dfData %>% 
  dplyr::mutate(
    geoLat = round(nLat / geoInv) * geoInv, 
    geoLon = round(nLon / geoInv) * geoInv,
  ) %>% 
  dplyr::group_by(geoLon, geoLat) %>%
  dplyr::summarise(nMeanVal = mean(nVal, na.rm = TRUE)) %>% 
  dplyr::rename(
    nLat = geoLat,
    nLon = geoLon,
  )

# 규칙 격자
# dfDataL1 = dfData %>%
#   dplyr::group_by(nLon, nLat) %>%
#   dplyr::summarise(nMeanVal = mean(nVal, na.rm = TRUE))

dplyr::tbl_df(dfDataL1)

# L2 Processing Using Data Frame L1
dfDataL2 = dfDataL1 %>%
  dplyr::mutate(nLon180 = metR::ConvertLongitude(nLon, from = 360))

summary(dfDataL2)

# Set Value for Visualization
font = "Palatino Linotype"
mapData = spData::world
cbOcean = oce::oceColors9A(120)

mainTitle = sprintf("%s", "Mean_Sea_Surface_Temperature")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

# Visualization Using ggplot2
ggplot() +
  theme_bw() +
  geom_tile(data = dfDataL2, aes(x = nLon180, y = nLat, fill = nMeanVal)) +
  # 속도 저하
  # metR::geom_text_contour(data = dfDataL2, aes(x = nLon180, y = nLat, z = nMeanVal), stroke = 0.2, check_overlap = TRUE, rotate = TRUE, na.rm = TRUE) +
  metR::geom_contour2(data = dfDataL2, aes(x = nLon180, y = nLat, z = nMeanVal), color = "black", alpha = 0.3) +
  scale_fill_gradientn(colours = cbOcean, limits=c(-10, 40), breaks = seq(-10, 40, 10), na.value = "transparent") +
  geom_sf(data = mapData, fill = "grey100", col = "black") +
  metR::scale_x_longitude(expand = c(0, 0), breaks = seq(-180, 180, 60), limits = c(-180, 180)) +
  metR::scale_y_latitude(expand = c(0, 0), breaks = seq(-90, 90, 30), limits = c(-90, 90)) +
  labs(
    x = ""
    , y = ""
    # , fill = "Mean Sea Surface Temperature [℃]"
    , fill = "Mean Sea Surface Temperature"
    , colour = ""
    , title  = "NOAA Optimum Interpolation (OI) SST V2"
    , subtitle = "Period : December 31, 1989 - February 20, 2020"
    , caption = "Source : NCEP Climate Modeling Branch"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "black")
    , axis.title.x = element_text(face = "bold", size = 18, colour = "black")
    , axis.title.y = element_text(face = "bold", size =18, colour = "black", angle=90)
    , axis.text.x  = element_text(face = "bold", size = 18, colour = "black")
    , axis.text.y  = element_text(face = "bold", size = 18, colour = "black")
    , legend.title = element_text(face = "bold", size = 14, colour = "white")
    , legend.position = c(0, 1)
    , legend.justification = c(0, 0.96)
    , legend.key = element_blank()
    , legend.text = element_text(size = 14, face = "bold", colour = "white")
    , legend.background = element_blank()
    , text = element_text(family = font)
    , plot.margin = unit(c(0, 8, 0, 0), "mm")
  ) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")