
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
# R을 이용한 NetCDF 자료 처리 고도화 및 시각화

# [문의사항]
# 의뢰내용 보냅니다. 생각해보니 2번 먼저 하시는게 좋을것 같아요, byte자료 처리부분이 있는데 
# 이거 r로 가능한지 먼저 말씀을 해주시고 작업을 들어가시는게 좋을것 같아요.
# 
# 1번,2번 자료들이 커서 링크에서 다운받으시는게 빠를것 같아서 링크를 첨부했어요, 
# 일부만 받아서 테스트하셔도 되고요. 3번자료가 총 14기가 정도 되는데 올라가겠죠?
#   
# 3개 자료에서 하려는 작업내용이 중복되는거라서 2번자료로 하시고 1,3번은 기간
# ,변수 선택하는 것만 해주시면 나머지는 제가 해보면서 안되는 부분을 질문하는걸로 하겠습니다.
# 일단 의뢰내용 확인하시고 오늘 연락한번 부탁드려요. 전화주셔도 되요.
# 
# 아. FluxSat GPP는 다 받아서 테스트를 해주시면 좋겠어요 
# 그래야 대용량 자료처리 가능한지 알수있을거 같아요
# 그리고 RFGPP자료는 방금 다 올렸습니다
# 
# 죄송한데 요청사항 다시 수정할께요ㅠㅠ
# FluxSat GPP자료에서,
# 8b (missing_data_zero_filled_low_PAR) 만 제외한 
# 모든 flag_values를 다 쓰도록 해주시면 된다고 했는데요
# all_magnitude_inversions_or_50_percent_or_less_fill_values (4b)로 된거만 써도 될거같아요! 
# GPP값이 4b인것만 갖고 통계값 계산해주시면 될거같아요.

# 의뢰내용: R을 이용한 netcdf 파일 자료처리 및 시각화
# 작성일: 2022.4.22.
# 초안: 작업시작일부터 4일내,
# 전체 작업 완료기간: 2022.5.1
# 
# - 작업환경: 리눅스centos 7, R4.0.5 
# - 데이터: 구글드라이브에 의뢰인 제공 또는 직접 다운로드
# - 자료처리 및 시각화 영역: 북반구, 자료에 윤년 포함.
# - 사용희망 패키지: 언급한 패키지보다 계산속도 면에서 효율적인 패키지가 있다면 추천희망
# https://ropensci.org/blog/2019/11/05/tidync/
#   lubridate, tibble, zpp, ggplot2, raster, rgdal 등
# 
# < 1. VODCA2GPP 에 대한 상세 작업내용 >
#   - 사용할 변수: GPP(time, lat, lon)
# - 북반구 지역 값의 범위: 0~15 gC/m2/day 
# - 값이 8일간격으로 있음.
# - 다운로드: https://researchdata.tuwien.ac.at/records/1k7aj-bdz35

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0307"
contextPath = ifelse(env == "local", ".", "E:/04. TalentPlatform/Github/TalentPlatform-R")

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
library(leaflet)
library(jsonlite)
library(RCurl)
library(readr)
library(raster)
library(sf)
library(tidync)
library(fs)
library(ncmeta)
library(rgdal)                                                                                                      
library(raster)
library(ggplot2)
library(ggOceanMaps)
library(RColorBrewer)
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(metR)
library(PlotSvalbard)
# devtools::install_github("MikkoVihtakari/PlotSvalbard", upgrade = "never")

world = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
cbSpectral = rev(RColorBrewer::brewer.pal(11, "Spectral"))

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "GPP.2018.ver.2020.2.nc"))
ncData = tidync::tidync(fileInfo)
ncVarInfo = ncData %>% tidync::hyper_tbl_cube()
doyInfo = ncVarInfo$dims$doy
lonInfo = ncVarInfo$dims$lon
latInfo = ncVarInfo$dims$lat


# ============================================================================================
# 1) 전체 기간에 대해서, 60.02N, 89.21E 지점에 대한 모든 값을 출력하여 csv로 저장
# (yyyy, mm, doy, gpp).
# ============================================================================================
posLon = 89.21
posLat = 60.02

posGeoData = tibble::tibble(
  noncompliance::expand.grid.DT(
    posLon
    , posLat
    , col.names = c("lon", "lat"))
) 


srtDate = "2013-01-01"
endDate = "2015-01-01"
dtDateList = seq(lubridate::ymd(srtDate), lubridate::ymd(endDate), by = "1 year")

extDataL1 = tibble::tibble()
# dtDateInfo = dtDateList[1]
for (i in 1:length(dtDateList)) {
  dtDateInfo = dtDateList[i]
  nYear = lubridate::year(dtDateInfo)
  
  cat(sprintf("nYear : %s", nYear), "\n")
  
  filePattern = sprintf("GPP.%s.ver.2020.2.nc", nYear)
  fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
 
  # fileInfo = fileList[1]
  for (fileInfo in fileList) {
    gppData = raster::brick(fileInfo, hurrname = "GPP")
    
    # 선정 위경도 (areaGeoData)에 따른 값 추출 (행: 위경도, 열: doy)
    # 해당되는 월에 대한 doy 선택
    # 위경도에 따른 합계/평균 수행
    # gpp 결측값 제거
    extData = raster::extract(gppData, posGeoData, df = TRUE, na.rm = FALSE) %>%
      dplyr::select(-ID) %>% 
      magrittr::set_colnames(doyInfo) %>% 
      t() %>% 
      as.tibble() %>% 
      magrittr::set_colnames("gpp") %>% 
      dplyr::mutate(
        year = nYear
        , doy = doyInfo
        , dtDate = strptime(paste(year, doy, sep = "-"), "%Y-%j")
        , month = lubridate::month(dtDate)
      ) %>% 
      dplyr::select(year, month, doy, gpp)
    
    extDataL1 = dplyr::bind_rows(extDataL1, extData)
  }
}

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "55-65N_80-100E_지점")
readr::write_csv(extDataL1, file = saveFile)


# ============================================================================================
# 2) 1번과 동일한 작업이나 gpp값이 missing value에서 양수가 되는 날(gpp_onset_doy)과 
# 그때의 gpp값(gpp_onset), 양수가 되었다가 missing value가 되는 날(gpp_offset_doy)과 
# 그때의 gpp값(gpp_offset)을 출력하여 csv로 저장. 
# yyyy, mm, gpp_onset_doy, gpp_onset, gpp_offset_doy, gpp_offset 형태로 나오게 
# ============================================================================================

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "55-65N_80-100E_지점")
extDataL1 = read.csv(file = saveFile)

# 결측값 (NA)가 없기 때문에 임의로 생성
extDataL1[2, "gpp"] = NA
extDataL1[5, "gpp"] = NA

extDataL2 = extDataL1 %>% 
  dplyr::mutate(
    bef = lag(gpp)
    , aft  = lead(gpp)
  ) %>% 
  dplyr::mutate(
    gpp_onset_doy = dplyr::case_when(
      is.na(gpp) & aft > 0 ~ doy
    )
    , gpp_onset = dplyr::case_when(
      is.na(gpp) & aft > 0 ~ aft
    )
    , gpp_offset_doy = dplyr::case_when(
      is.na(gpp) & bef > 0 ~ doy
    )
    , gpp_offset = dplyr::case_when(
      is.na(gpp) & bef > 0 ~ bef
    )
  ) %>%
  dplyr::filter(is.na(gpp)) %>% 
  dplyr::select(year, month, gpp_onset_doy, gpp_onset, gpp_offset_doy, gpp_offset)

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "55-65N_80-100E_지점_결측검사")
readr::write_csv(extDataL2, file = saveFile)


# ============================================================================================
# 3) 전체 기간에 대해서, 55-65N, 80-100E 영역내 각 격자의 gpp값을 모두 찾은 후 
# 각 날에 대해서 영역평균하여 csv로 저장. 
# 1)과 동일한 길이의 자료가 될것이고 출력 형태도 동일.
# ============================================================================================
areaLon = seq(80, 100, 0.1)
areaLat = seq(55, 65, 0.1)

areaGeoData = tibble::tibble(
  noncompliance::expand.grid.DT(
    areaLon
    , areaLat
    , col.names = c("lon", "lat"))
  ) 

srtDate = "2013-01-01"
endDate = "2015-01-01"
dtDateList = seq(lubridate::ymd(srtDate), lubridate::ymd(endDate), by = "1 year")

extDataL3 = tibble::tibble()
# dtDateInfo = dtDateList[1]
for (i in 1:length(dtDateList)) {
  dtDateInfo = dtDateList[i]
  nYear = lubridate::year(dtDateInfo)
  
  cat(sprintf("nYear : %s", nYear), "\n")
  
  filePattern = sprintf("GPP.%s.ver.2020.2.nc", nYear)
  fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  
  # fileInfo = fileList[1]
  for (fileInfo in fileList) {
    gppData = raster::brick(fileInfo, hurrname = "GPP")
    
    # 선정 위경도 (areaGeoData)에 따른 값 추출 (행: 위경도, 열: doy)
    # 해당되는 월에 대한 doy 선택
    # 위경도에 따른 합계/평균 수행
    # gpp 결측값 제거
    extData = raster::extract(gppData, areaGeoData, df = TRUE, na.rm = FALSE) %>%
      dplyr::select(-ID) %>% 
      colMeans(na.rm = TRUE) %>%
      as.tibble() %>% 
      magrittr::set_colnames("gpp") %>% 
      dplyr::mutate(
        year = nYear
        , doy = doyInfo
        , dtDate = strptime(paste(year, doy, sep = "-"), "%Y-%j")
        , month = lubridate::month(dtDate)
      ) %>% 
      dplyr::select(year, month, doy, gpp)
    
    extDataL3 = dplyr::bind_rows(extDataL3, extData)
  }
}

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "55-65N_80-100E_영역 평균")
readr::write_csv(extDataL3, file = saveFile)


# ============================================================================================
# 4) 북반구 전체에 대해서 gpp의 격자별 월간 총합 (sum 함수 사용, 단위 gC/m2/month)을 구하고 
# ============================================================================================
# 원하는 기간에 대한 평균장 및 아노말리 장을 map으로 그리기
# - map 형태는 일반적인 mercator와 polar map(https://stackoverflow.com/questions/48816773/polar-stereographic-map-in-r)
# - map 위에 60.02N, 89.21E을 closed circle로 표시하기

# ************************************************************************************
# 북반구 전체에 대해서 gpp의 격자별 월간 총합 (sum 함수 사용, 단위 gC/m2/month)
# ************************************************************************************
# areaLon = ncVarInfo$dims$lon
# areaLat = seq(0, 90, 0.1)
areaLon = seq(-179, 180, 1)
areaLat = seq(0, 90, 1)

areaGeoData = tibble::tibble(
  noncompliance::expand.grid.DT(
    areaLon
    , areaLat
    , col.names = c("lon", "lat"))
)

srtDate = "2013-01-01"
endDate = "2014-01-01"
dtDateList = seq(lubridate::ymd(srtDate), lubridate::ymd(endDate), by = "1 month")

# 앞서 연/월 지점 파일 가져오기
saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "55-65N_80-100E_지점")
posStatData = read.csv(file = saveFile)

# dtDateInfo = dtDateList[1]
for (i in 1:length(dtDateList)) {
  dtDateInfo = dtDateList[i]
  
  nYear = lubridate::year(dtDateInfo)
  nMonth = lubridate::month(dtDateInfo)
  sDateFmt = format(dtDateInfo, "%Y-%m")
  
  # 앞서 연/월 지점 결과를 평균 수행
  saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "55-65N_80-100E_지점")
  posStatData = read.csv(file = saveFile)
  
  posStatDataL1 = posStatData %>% 
    dplyr::filter(
      year == nYear
      , month == nMonth
    ) %>% 
    dplyr::summarise(
      meanVal = mean(gpp, na.rm = TRUE)
    ) %>% 
    dplyr::bind_cols(posGeoData)
  
  # 연, 월에 해당하는 doy 추출
  selDoyData = doyInfo %>%
    as.tibble() %>% 
    magrittr::set_colnames("doy") %>% 
    dplyr::mutate(
      year = nYear
      , dtDate = strptime(paste(year, doy, sep = "-"), "%Y-%j")
      , month = lubridate::month(dtDate)
    ) %>% 
    tibble::rowid_to_column() %>%
    dplyr::filter(
      dplyr::between(month, nMonth, nMonth)
    )

  cat(sprintf("nYear : %s / nMonth : %s", nYear, nMonth), "\n")

  filePattern = sprintf("GPP.%s.ver.2020.2.nc", nYear)
  fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  
  # fileInfo = fileList[1]
  for (fileInfo in fileList) {
    gppData = raster::brick(fileInfo, hurrname = "GPP")
  
    # 선정 위경도 (areaGeoData)에 따른 값 추출 (행: 위경도, 열: doy)
    # 해당되는 월에 대한 doy 선택
    # 위경도에 따른 합계/평균 수행
    # gpp 결측값 제거
    extData = raster::extract(gppData, areaGeoData, df = TRUE, na.rm = FALSE) %>%
      dplyr::select(-ID) %>% 
      as.tibble() %>% 
      magrittr::set_colnames(doyInfo) %>%
      dplyr::select(c(selDoyData$rowid)) %>%
      # rowMeans(na.rm = TRUE) %>%
      rowSums(na.rm = TRUE) %>%
      as.tibble() %>% 
      magrittr::set_colnames("gpp") %>% 
      dplyr::bind_cols(areaGeoData) %>% 
      dplyr::filter(! is.na(gpp))
    
    
    extDataL1 = extData %>% 
      dplyr::filter(
        gpp != 0
        , lat >= 30
      )

    plotSubTitle = sprintf("[%s] %s (%s-%s)", sDateFmt, "전구 전체 월간 총합", round(min(extData$gpp), 0), round(max(extData$gpp), 0))
    saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
    
    makemMecarPlot = ggplot() +
      coord_fixed(ratio = 1) +
      geom_tile(data = extData, aes(x = lon, y = lat, fill = gpp)) +
      geom_sf(data = world, fill = NA, color = "white") +
      coord_sf(ylim = c(0, 90)) +
      scale_fill_distiller(palette = "Spectral") +
      theme_bw() +
      labs(title = NULL, x = NULL, y = NULL, colour = NULL, fill = NULL, subtitle = plotSubTitle) +
      metR::scale_x_longitude(breaks = seq(-180, 180, 30), limits = c(-180, 180), expand = c(0, 0)) +
      metR::scale_y_latitude(breaks = seq(0, 90, 15), limits = c(0, 90), expand = c(0, 0)) +
      theme(
        text = element_text(size = 18)
        , legend.position = "bottom"
        , legend.key.width = unit(4, "cm")
      )
    
    ggsave(makemMecarPlot, filename = saveImg, width = 12, height = 6, dpi = 600)
    
    
    plotSubTitle = sprintf("[%s] %s (%s-%s)", sDateFmt, "북반구 전체 월간 총합", round(min(extDataL1$gpp), 0), round(max(extDataL1$gpp), 0))
    saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
    
    makePlot = PlotSvalbard::basemap(limits = 30, grid.size = 0.2, label.font = 16) +
      geom_point(
        data = ggOceanMaps::transform_coord(extDataL1, lon = "lon", lat = "lat", bind = TRUE)
        , aes(x = lon.proj, y = lat.proj, colour = gpp, fill = gpp)
        , shape = 15
      ) +
      geom_point(
        data = ggOceanMaps::transform_coord(posStatDataL1, lon = "lon", lat = "lat", bind = TRUE)
        , aes(x = lon.proj, y = lat.proj), color = "red"
      ) +
      scale_fill_distiller(palette = "Spectral") +
      scale_colour_distiller(palette = "Spectral") +
      theme_bw() +
      labs(title = NULL, x = NULL, y = NULL, colour = NULL, fill = NULL, subtitle = plotSubTitle) +
      theme(
        text = element_text(size = 18)
        , panel.grid.major.x = element_blank()
        , panel.grid.major.y = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , panel.border = element_blank()
        , panel.grid = element_blank()
        , axis.text.x = element_blank()
        , axis.ticks.x = element_blank()
        , axis.title.x = element_blank()
        , axis.text.y = element_blank()
        , axis.ticks.y = element_blank()
        , axis.title.y = element_blank()
        , legend.position = "bottom"
        , legend.key.width = unit(3.5, "cm")
      )
  
    ggsave(makePlot, filename = saveImg, width = 10, height = 10, dpi = 600)
    
  }
}


# ****************************************************************************************
# 원하는 기간에 대한 평균장 및 아노말리 장을 map으로 그리기
# - 원하는 기간: 2001-2019년 6-7월만 평균
# 2000.11-2020.3월 11-3월 평균 (즉, 2001년 11-3월평균은 2000.11-12, 2001.1-3월부터 시작).
# ****************************************************************************************
areaLon = seq(-179, 180, 1)
areaLat = seq(0, 90, 1)

areaGeoData = tibble::tibble(
  noncompliance::expand.grid.DT(
    areaLon
    , areaLat
    , col.names = c("lon", "lat"))
)

srtDate = "2013-01-01"
endDate = "2016-01-01"
dtDateList = seq(lubridate::ymd(srtDate), lubridate::ymd(endDate), by = "1 year")

srtMonth = 6
endMonth = 7

# dtDateInfo = dtDateList[1]
meanData = tibble::tibble()
for (i in 1:length(dtDateList)) {
  dtDateInfo = dtDateList[i]
  
  nYear = lubridate::year(dtDateInfo)
  sDateFmt = format(dtDateInfo, "%Y")
  
  # 연, 월에 해당하는 doy 추출
  selDoyData = doyInfo %>%
    as.tibble() %>% 
    magrittr::set_colnames("doy") %>% 
    dplyr::mutate(
      year = nYear
      , dtDate = strptime(paste(year, doy, sep = "-"), "%Y-%j")
      , month = lubridate::month(dtDate)
    ) %>% 
    tibble::rowid_to_column() %>%
    dplyr::filter(
      dplyr::between(month, srtMonth, endMonth)
    )
  
  cat(sprintf("nYear : %s", nYear), "\n")
  
  filePattern = sprintf("GPP.%s.ver.2020.2.nc", nYear)
  fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  
  # fileInfo = fileList[1]
  for (fileInfo in fileList) {
    gppData = raster::brick(fileInfo, hurrname = "GPP")
    
    # 선정 위경도 (areaGeoData)에 따른 값 추출 (행: 위경도, 열: doy)
    # 해당되는 월에 대한 doy 선택
    # 위경도에 따른 합계/평균 수행
    # gpp 결측값 제거
    extData = raster::extract(gppData, areaGeoData, df = TRUE, na.rm = FALSE) %>%
      dplyr::select(-ID) %>% 
      as.tibble() %>% 
      magrittr::set_colnames(doyInfo) %>%
      dplyr::select(c(selDoyData$rowid)) %>%
      rowMeans(na.rm = TRUE) %>%
      as.tibble() %>% 
      magrittr::set_colnames(nYear)
    
    if (nrow(meanData) == 0) {
      meanData = extData
    } else {
      meanData = dplyr::bind_cols(meanData, extData)
    }
    
  }
}

meanDataL1 = meanData %>% 
  rowMeans(na.rm = TRUE) %>% 
  as.tibble() %>% 
  magrittr::set_colnames("gpp") %>% 
  dplyr::bind_cols(areaGeoData)

summary(meanDataL1)




# ****************************************************************************************
# - 아노말리 계산시 2001-2019년을 기후값으로 계산
# 예를 들어 2015년 5월 gpp 아노말리장은 2001-2019.5월 평균에서 2015.5월을 뺀 값으로 정의
# ****************************************************************************************
# areaLon = ncVarInfo$dims$lon
# areaLat = seq(0, 90, 0.1)
areaLon = seq(-179, 180, 1)
areaLat = seq(0, 90, 1)

areaGeoData = tibble::tibble(
  noncompliance::expand.grid.DT(
    areaLon
    , areaLat
    , col.names = c("lon", "lat"))
)

srtDate = "2013-01-01"
endDate = "2019-01-01"
dtDateList = seq(lubridate::ymd(srtDate), lubridate::ymd(endDate), by = "1 year")

srtMonth = 5
endMonth = 5

# dtDateInfo = dtDateList[1]
clmData = tibble::tibble()
for (i in 1:length(dtDateList)) {
  dtDateInfo = dtDateList[i]
  
  nYear = lubridate::year(dtDateInfo)
  sDateFmt = format(dtDateInfo, "%Y")
  
  # 연, 월에 해당하는 doy 추출
  selDoyData = doyInfo %>%
    as.tibble() %>% 
    magrittr::set_colnames("doy") %>% 
    dplyr::mutate(
      year = nYear
      , dtDate = strptime(paste(year, doy, sep = "-"), "%Y-%j")
      , month = lubridate::month(dtDate)
    ) %>% 
    tibble::rowid_to_column() %>%
    dplyr::filter(
      dplyr::between(month, srtMonth, endMonth)
    )
  
  cat(sprintf("nYear : %s", nYear), "\n")
  
  filePattern = sprintf("GPP.%s.ver.2020.2.nc", nYear)
  fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  
  # fileInfo = fileList[1]
  for (fileInfo in fileList) {
    gppData = raster::brick(fileInfo, hurrname = "GPP")
    
    # 선정 위경도 (areaGeoData)에 따른 값 추출 (행: 위경도, 열: doy)
    # 해당되는 월에 대한 doy 선택
    # 위경도에 따른 합계/평균 수행
    # gpp 결측값 제거
    extData = raster::extract(gppData, areaGeoData, df = TRUE, na.rm = FALSE) %>%
      dplyr::select(-ID) %>% 
      as.tibble() %>% 
      magrittr::set_colnames(doyInfo) %>%
      dplyr::select(c(selDoyData$rowid)) %>%
      rowMeans(na.rm = TRUE) %>%
      as.tibble() %>% 
      magrittr::set_colnames(nYear)
    
    if (nrow(clmData) == 0) {
      clmData = extData
    } else {
      clmData = dplyr::bind_cols(clmData, extData)
    }
    
  }
}

clmDataL1 = clmData %>% 
  rowMeans(na.rm = TRUE) %>% 
  as.tibble() %>% 
  magrittr::set_colnames("clmGpp") %>% 
  dplyr::bind_cols(areaGeoData)

summary(clmDataL1)


# ============================================================================================
# 5) 3)에서 그린 결과들에서 60.02N, 89.21E 지점값을 출력. 
# 1)번내용과 동일한 방식으로 되는거면 생략 가능. 
# ============================================================================================
# 4) 북반구 전체 월간 총합에서 활용


# ============================================================================================
# 6) 북반구 전체에 대해서 gpp의 격자별 연간총합(단위 gC/m2/year)을 구하고 
# 그림을 매년에 대해서 그리기. map 형태는 3)과 동일. 2001-2019년에 대해서 
# map 이 한 장씩 총 19장이 나와야 함. 
# ============================================================================================
areaLon = seq(-179, 180, 1)
areaLat = seq(0, 90, 1)

areaGeoData = tibble::tibble(
  noncompliance::expand.grid.DT(
    areaLon
    , areaLat
    , col.names = c("lon", "lat"))
)

srtDate = "2001-01-01"
endDate = "2019-01-01"
dtDateList = seq(lubridate::ymd(srtDate), lubridate::ymd(endDate), by = "1 year")

# dtDateInfo = dtDateList[1]
for (i in 1:length(dtDateList)) {
  dtDateInfo = dtDateList[i]
 
  nYear = lubridate::year(dtDateInfo)
  sDateFmt = format(dtDateInfo, "%Y")  
  
  # 연도에 해당하는 doy 추출
  selDoyData = doyInfo %>%
    as.tibble() %>% 
    magrittr::set_colnames("doy") %>% 
    dplyr::mutate(
      year = nYear
      , dtDate = strptime(paste(year, doy, sep = "-"), "%Y-%j")
      , month = lubridate::month(dtDate)
    ) %>% 
    tibble::rowid_to_column() %>%
    dplyr::filter(
      dplyr::between(month, 1, 12)
    )
  
  cat(sprintf("nYear : %s", nYear), "\n")
  
  filePattern = sprintf("GPP.%s.ver.2020.2.nc", nYear)
  fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  
  extDataL3 = tibble::tibble()
  # fileInfo = fileList[1]
  for (fileInfo in fileList) {
    gppData = raster::brick(fileInfo, hurrname = "GPP")
    
    # 선정 위경도 (areaGeoData)에 따른 값 추출 (행: 위경도, 열: doy)
    # 해당되는 월에 대한 doy 선택
    # 위경도에 따른 합계/평균 수행
    # gpp 결측값 제거
    extData = raster::extract(gppData, areaGeoData, df = TRUE, na.rm = FALSE) %>%
      dplyr::select(-ID) %>% 
      as.tibble() %>% 
      magrittr::set_colnames(doyInfo) %>%
      dplyr::select(c(selDoyData$rowid)) %>%
      # rowMeans(na.rm = TRUE) %>%
      rowSums(na.rm = TRUE) %>%
      as.tibble() %>% 
      magrittr::set_colnames("gpp") %>% 
      dplyr::bind_cols(areaGeoData) %>% 
      dplyr::filter(! is.na(gpp))
    
    extDataL1 = extData %>% 
      dplyr::filter(
        gpp != 0
        , lat >= 30
      )
    
    plotSubTitle = sprintf("[%s] %s (%s-%s)", sDateFmt, "북반구 전체 월간 총합", round(min(extDataL1$gpp), 0), round(max(extDataL1$gpp), 0))
    saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
    
    makePlot = PlotSvalbard::basemap(limits = 30, grid.size = 0.2, label.font = 16) +
      geom_point(
        data = ggOceanMaps::transform_coord(extDataL1, lon = "lon", lat = "lat", bind = TRUE)
        , aes(x = lon.proj, y = lat.proj, colour = gpp, fill = gpp)
        , shape = 15
      ) +
      scale_fill_distiller(palette = "Spectral") +
      scale_colour_distiller(palette = "Spectral") +
      theme_bw() +
      labs(title = NULL, x = NULL, y = NULL, colour = NULL, fill = NULL, subtitle = plotSubTitle) +
      theme(
        text = element_text(size = 18)
        , panel.grid.major.x = element_blank()
        , panel.grid.major.y = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , panel.border = element_blank()
        , panel.grid = element_blank()
        , axis.text.x = element_blank()
        , axis.ticks.x = element_blank()
        , axis.title.x = element_blank()
        , axis.text.y = element_blank()
        , axis.ticks.y = element_blank()
        , axis.title.y = element_blank()
        , legend.position = "bottom"
        , legend.key.width = unit(3.5, "cm")
      )
    
    ggsave(makePlot, filename = saveImg, width = 10, height = 10, dpi = 600)
    
  }
}


# ============================================================================================
# 7) 4번 결과로 아노말리 map 그리기. 예)2015년 연평균 아노말리.
# ============================================================================================


srtDate = "2013-01-01"
endDate = "2019-01-01"
dtDateList = seq(lubridate::ymd(srtDate), lubridate::ymd(endDate), by = "1 month")

# dtDateInfo = dtDateList[1]
for (i in 1:length(dtDateList)) {
  dtDateInfo = dtDateList[i]
  
  nYear = lubridate::year(dtDateInfo)
  nMonth = lubridate::month(dtDateInfo)
  sDateFmt = format(dtDateInfo, "%Y-%m")
  
  # 연, 월에 해당하는 doy 추출
  selDoyData = doyInfo %>%
    as.tibble() %>% 
    magrittr::set_colnames("doy") %>% 
    dplyr::mutate(
      year = nYear
      , dtDate = strptime(paste(year, doy, sep = "-"), "%Y-%j")
      , month = lubridate::month(dtDate)
    ) %>% 
    tibble::rowid_to_column() %>%
    dplyr::filter(
      dplyr::between(month, nMonth, nMonth)
    )
  
  cat(sprintf("nYear : %s / nMonth : %s", nYear, nMonth), "\n")
  
  filePattern = sprintf("GPP.%s.ver.2020.2.nc", nYear)
  fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, filePattern))
  
  # fileInfo = fileList[1]
  for (fileInfo in fileList) {
    gppData = raster::brick(fileInfo, hurrname = "GPP")
    
    # 선정 위경도 (areaGeoData)에 따른 값 추출 (행: 위경도, 열: doy)
    # 해당되는 월에 대한 doy 선택
    # 위경도에 따른 합계/평균 수행
    # gpp 결측값 제거
    extData = raster::extract(gppData, areaGeoData, df = TRUE, na.rm = FALSE) %>%
      dplyr::select(-ID) %>% 
      as.tibble() %>% 
      magrittr::set_colnames(doyInfo) %>%
      dplyr::select(c(selDoyData$rowid)) %>%
      rowMeans(na.rm = TRUE) %>%
      as.tibble() %>% 
      magrittr::set_colnames("orgGpp")
    
    
    clmDataL2 = dplyr::bind_cols(clmDataL1, extData) %>% 
      dplyr::mutate(gpp = orgGpp - clmGpp)
    
    summary(clmDataL2)
    
    clmDataL3 = clmDataL2 %>% 
      dplyr::filter(
        gpp != 0
        , lat >= 30
      )
    
    plotSubTitle = sprintf("[%s] %s (%s-%s)", sDateFmt, "전구 전체 월간 아노말리", round(min(clmDataL2$gpp, na.rm = TRUE), 0), round(max(clmDataL2$gpp, na.rm = TRUE), 0))
    saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
    
    makemMecarPlot = ggplot() +
      coord_fixed(ratio = 1) +
      geom_tile(data = clmDataL2, aes(x = lon, y = lat, fill = gpp)) +
      geom_sf(data = world, fill = NA, color = "white") +
      coord_sf(ylim = c(0, 90)) +
      scale_fill_distiller(palette = "Spectral") +
      theme_bw() +
      labs(title = NULL, x = NULL, y = NULL, colour = NULL, fill = NULL, subtitle = plotSubTitle) +
      metR::scale_x_longitude(breaks = seq(-180, 180, 30), limits = c(-180, 180), expand = c(0, 0)) +
      metR::scale_y_latitude(breaks = seq(0, 90, 15), limits = c(0, 90), expand = c(0, 0)) +
      theme(
        text = element_text(size = 18)
        , legend.position = "bottom"
        , legend.key.width = unit(4, "cm")
      )
    
    ggsave(makemMecarPlot, filename = saveImg, width = 12, height = 6, dpi = 600)
    
    
    plotSubTitle = sprintf("[%s] %s (%s-%s)", sDateFmt, "북반구 전체 월간 아노말리", round(min(clmDataL3$gpp, na.rm = TRUE), 0), round(max(clmDataL3$gpp, na.rm = TRUE), 0))
    saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
    
    makePlot = PlotSvalbard::basemap(limits = 30, grid.size = 0.2, label.font = 16) +
      geom_point(
        data = ggOceanMaps::transform_coord(clmDataL3, lon = "lon", lat = "lat", bind = TRUE)
        , aes(x = lon.proj, y = lat.proj, colour = gpp, fill = gpp)
        , shape = 15
      ) +
      scale_fill_distiller(palette = "Spectral") +
      scale_colour_distiller(palette = "Spectral") +
      theme_bw() +
      labs(title = NULL, x = NULL, y = NULL, colour = NULL, fill = NULL, subtitle = plotSubTitle) +
      theme(
        text = element_text(size = 18)
        , panel.grid.major.x = element_blank()
        , panel.grid.major.y = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , panel.border = element_blank()
        , panel.grid = element_blank()
        , axis.text.x = element_blank()
        , axis.ticks.x = element_blank()
        , axis.title.x = element_blank()
        , axis.text.y = element_blank()
        , axis.ticks.y = element_blank()
        , axis.title.y = element_blank()
        , legend.position = "bottom"
        , legend.key.width = unit(3.5, "cm")
      )
    
    ggsave(makePlot, filename = saveImg, width = 10, height = 10, dpi = 600)
  }
}



# ============================================================================================
# 8) 각 격자에 대해서 1999-2020년 6-7월 평균 또는 6월 15일-7월 10일 사이의 평균 gpp 를 구한 후 
# Theil-Sen slope를 구하고 유의수준이 90% 이상인 것만 표출하여 map으로 나타내기
# ============================================================================================
# clmDataL1
# meanDataL1

# 앞선 방법과 유사하여 전처리 생략
library(robslopes)

resData = robslopes::TheilSen(clmDataL1$clmGpp, meanDataL1$gpp, NULL, FALSE)

# 기울기
resData$slope

# 절편
resData$intercept


# < 2. FluxSat GPP에 대한 상세 작업내용 >
# 에러 발생
#   - 다운로드: https://avdc.gsfc.nasa.gov/pub/tmp/FluxSat_GPP/Climate_Modelling_Grid
# - 사용할 변수: byte BRDF_Quality(time, lat, lon) 값이 best_quality_100_percent_with_full_inversions인 GPP(time, lat, lon)만 사용
# - 값이 일자료
# - VODCA2GPP 작업내용과 동일한 내용이므로 파일 읽고 원하는 기간의 변수 불러오는 부분만 테스트. 
# 
# < 3. RF GPP에 대한 상세 작업내용 >
#   - 사용할 변수: GPP(doy, lat, lon)
# - 자료는 doy가 10일 간격이고 윤년포함.
# - 파일이름에 년도가 있고 날짜정보는 파일 doy에 있음. 
# - 작업내용이 VODCA2GPP와 동일하므로 파일 읽고 변수 불러오는 부분만 우선 테스트
# 























# r로 nc 자료 읽을때랑 theil sen slope 구할때 썼던 스크립트 일부 보내드리니 참고하세요
#######################
## GOSIF GPP
## annual, g C m-2 yr-1
#######################
library(raster)
library(tidyverse)
library(rgdal)
library(dplyr)
library(ggplot2)


### read multiple .tif files
path = "D:/spark/work/2_data/satellite/GOSIF-GPP/"
rastlist <- list.files(path=path, pattern = glob2rx('*tif$'), full.names=TRUE)
head(rastlist)
tail(rastlist)

#lapply(rastlist, raster)
gosif_gpp_all <- raster::stack(rastlist)
gosif_gpp_all_df <- as.data.frame(gosif_gpp_all, xy = TRUE)
str(gosif_gpp_all_df)
head(gosif_gpp_all_df)
rast(gosif_gpp_all_df)

# Create a time series raster stack
rastlist_stack <- stack(rastlist)
# class : RasterStack
# dimensions : 3600, 7200, 25920000, 2 (nrow, ncol, ncell, nlayers)
# resolution : 0.05, 0.05 (x, y)
# extent : -180, 180, -90, 90 (xmin, xmax, ymin, ymax)
# crs : +proj=longlat +datum=WGS84 +no_defs
# names : GOSIF_GPP_2007_Mean, GOSIF_GPP_2010_Mean
# min values : 0, 0
# max values : 65535, 65535

#apply scale factor
#rastlist_stack <- rastlist_stack*0.1

gosif_gpp_all_df[gosif_gpp_all_df > 60000] <- NA
mean(gosif_gpp_all_df$GOSIF_GPP_2007_Mean*0.1, na.rm = TRUE) # 918.1669
median(gosif_gpp_all_df$GOSIF_GPP_2007_Mean*0.1, na.rm = TRUE) # 603.3
range(gosif_gpp_all_df$GOSIF_GPP_2007_Mean*0.1, na.rm = TRUE) # 0.0 5341.9

hist(gosif_gpp_all_df$GOSIF_GPP_2007_Mean*0.1)


colnames(gosif_gpp_all_df) <- c("longitude", "latitude",
                                "GOSIF_GPP_2001_Mean", "GOSIF_GPP_2002_Mean",
                                "GOSIF_GPP_2003_Mean", "GOSIF_GPP_2004_Mean",
                                "GOSIF_GPP_2005_Mean", "GOSIF_GPP_2006_Mean",
                                "GOSIF_GPP_2007_Mean", "GOSIF_GPP_2008_Mean",
                                "GOSIF_GPP_2009_Mean", "GOSIF_GPP_2010_Mean",
                                "GOSIF_GPP_2011_Mean", "GOSIF_GPP_2012_Mean",
                                "GOSIF_GPP_2013_Mean", "GOSIF_GPP_2014_Mean",
                                "GOSIF_GPP_2015_Mean", "GOSIF_GPP_2016_Mean",
                                "GOSIF_GPP_2017_Mean", "GOSIF_GPP_2018_Mean",
                                "GOSIF_GPP_2019_Mean", "GOSIF_GPP_2020_Mean")

gosif_gpp_all_df_csib <- gosif_gpp_all_df %>%
  filter(latitude > 58 & latitude < 62) %>%
  filter(longitude > 88 & longitude < 92)

range(gosif_gpp_all_df_csib$GOSIF_GPP_2012_Mean, na.rm = TRUE) # 2450 11653
range(gosif_gpp_all_df_csib$GOSIF_GPP_2020_Mean, na.rm = TRUE) # 2594 11942

med.gpp <- mapply(median,gosif_gpp_all_df_csib[,-c(1,2)], na.rm = TRUE)
mean.gpp <- mapply(mean,gosif_gpp_all_df_csib[,-c(1,2)], na.rm = TRUE)
max.gpp <- mapply(max,gosif_gpp_all_df_csib[,-c(1,2)], na.rm = TRUE)
min.gpp <- mapply(min,gosif_gpp_all_df_csib[,-c(1,2)], na.rm = TRUE)

med.gpp <- data.frame(med.gpp)
mean.gpp <- data.frame(mean.gpp)
max.gpp <- data.frame(max.gpp)
min.gpp <- data.frame(min.gpp)

gosif_gpp_all_df_csib_stats <- data.frame(med.gpp$med.gpp, mean.gpp$mean.gpp, max.gpp$max.gpp, min.gpp$min.gpp)
colnames(gosif_gpp_all_df_csib_stats) <- c("Med.GPP", "Mean.GPP", "Max.GPP", "Min.GPP")
gosif_gpp_all_df_csib_stats_sel <- gosif_gpp_all_df_csib_stats[-21,]

# gosif_gpp_all_df_csib_stats[21,]
# Med.GPP Mean.GPP Max.GPP Min.GPP
# 7220.9 7605.323 12423 2996.6

head(gosif_gpp_all_df_csib_stats)
tail(gosif_gpp_all_df_csib_stats)

gosif_gpp_all_df_csib_stats_sel$Year <- seq(2001,2020,by=1)
head(gosif_gpp_all_df_csib_stats_sel)
tail(gosif_gpp_all_df_csib_stats_sel)

gosif_gpp_all_df_csib_stats_sel <- gosif_gpp_all_df_csib_stats_sel[,c(5,1,2,3,4)]
head(gosif_gpp_all_df_csib_stats_sel)
tail(gosif_gpp_all_df_csib_stats_sel)

gosif_gpp_all_df_csib_stats_sel$Mean.GPP.ano = gosif_gpp_all_df_csib_stats_sel$Mean.GPP-7605.323
head(gosif_gpp_all_df_csib_stats_sel)
tail(gosif_gpp_all_df_csib_stats_sel)


boxplot(gosif_gpp_all_df_csib_stats_sel$Med.GPP,
        gosif_gpp_all_df_csib_stats_sel$Mean.GPP,
        # gosif_gpp_all_df_csib_stats_sel$Max.GPP,
        # gosif_gpp_all_df_csib_stats_sel$Min.GPP,
        # names=c("Med.GPP","Mean.GPP","Max.GPP"),
        names=c("Med.GPP","Mean.GPP"),
        ylab = "Annual GPP stats distribution")


tt <- gosif_gpp_all_df_csib_stats_sel %>%
  filter(Year >= 2003 & Year <= 2015)
simple.fit = lm(Med.GPP~Year, data=tt)
summary(simple.fit) # intercept 67925.81, slope = -30.23, r2 = 0.1521, p-value = 0.1878

ggplot(tt, aes(x=Year, y=Med.GPP)) +
  geom_point() +
  geom_smooth(method=lm)

sens.slope(tt$Med.GPP, conf.level = 0.95)

# Sen's slope
#
# data: tt$Med.GPP
# z = -1.0372, n = 13, p-value = 0.2997
# alternative hypothesis: true z is not equal to 0
# 95 percent confidence interval:
# -91.00 31.71
# sample estimates:
# Sen's slope
# -31.39583 (decreasing trend)


sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}

library(mblm)
tt %>%
  ggplot(aes(Year, Med.GPP)) +
  geom_point() +
  geom_smooth(method = sen)


