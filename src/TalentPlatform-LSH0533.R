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
# R을 이용한 식분도 부트스트랩 결과로부터 밀도함수 정규분포 및 군집화 분석

# 늦게 보내드려서 죄송합니다.
# 초안을 보내드리오니 확인 부탁드립니다.
# https://drive.google.com/drive/folders/1iWzzl3Zm0BfWRS-FowCu2OQ1791Qglm_?usp=sharing

# 보내주신 데이터 (모집단78개_bootDataL2_14-10000-30_129.2-35.8.csv)를 기준으로 다음과 같이 분석하였습니다.
# 01. 밀도함수에 따른 빈도분포
# 기존 10,000개 데이터 (위도 xAxis, 경도 yAxis, 평균식분값 meanVal)를 기준으로 밀도함수 (density)를 계산하여 최종적인 데이터를 구성하였습니다. 
# 그 결과 8,753개의 의미있는 데이터 (모집단78개-plotDataL2_10000-30_129.2-35.8.csv)를 구성하여 다음과 같이 지도 기반으로 시각화 및 빈도분포로 나타내었습니다.
# 각 범례마다 평균식분도를 빈도분포로 나타냄 (모집단78개-HistDen-*_10000-30_129.2-35.8.png)

# 02. 밀도함수 기반으로 군집화
# 01. 변수 3종 (위도,경도,밀도함수)
# 군집화 수행 결과 9개 설정 시 9번 군집에서 나타남

# 02. 변수 4종 (위도,경도,평균식분도,밀도함수)
# 군집화 수행 결과 10개 설정 시 6번 군집에서 나타남

# 여기서 과연 초기신라처럼 중원 남부지역으로 도출될 확률이 얼마인지 계산하기 위해 어떻게 하는것이 좋을지 잠시 토론한적이 있었습니다.

# 실례지만 전반적으로 어떤 의미인지 이해가 가지 않네요.
# 핵심은 과연 초기신라의 최대점(위도 31.7 경도 116.6) 처럼 나올 확률이 얼마인지를 계산해보는것인데, 이것과 결부시켜 해당 자료들을 다시 해석해주실수 있나요?

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0533"

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
library(purrr)
library(amap)
library(broom)
library(grt)
library(geosphere)

unscale = function(value, data, column) {
  scale_attr <- attr(data[[column]], "scaled:scale")
  center_attr <- attr(data[[column]], "scaled:center")
  
  if (is.null(scale_attr) || is.null(center_attr)) {
    stop(paste("속성이 존재하지 않음:", column))
  }
  
  return (value * scale_attr + center_attr)
}


cbMatlab = colorRamps::matlab.like(11)
mapGlobal = sf::st_read(file.path(globalVar$mapPath, "gshhg-shp-2.3.6/GSHHS_shp/i/GSHHS_i_L1.shp"))

xRange = as.numeric(c(90, 150))
yRange = as.numeric(c(10, 60))

newLon = seq(from = xRange[1], to = xRange[2], by = 0.2)
newLat = seq(from = yRange[1], to = yRange[2], by = 0.2)

gridData = noncompliance::expand.grid.DT(
  newLon
  , newLat
  , col.names = c("lon", "lat")
) %>% 
  as.tibble() %>% 
  dplyr::mutate(across(c(lon, lat), as.numeric))


sheetName = "모집단78개"
bootNum = 10000
bootDo = 30
posLon = 129.2
posLat = 35.8

# **************************************************
# 단일 이미지 테스트
# **************************************************
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "LSH0382_일식 식분도 이미지 데이터 추출.xlsx"))
# 
# sheetList = c(1)
# sheetName = "모집단78개"
# 
# dataL3 = tibble::tibble()
# for (sheetInfo in sheetList) {
# 
#   data = openxlsx::read.xlsx(fileInfo, sheet = sheetInfo) %>%
#     tibble::as.tibble()
# 
#   typeList = data$type %>% unique
# 
#   for (typeInfo in typeList) {
# 
#     tmpData = data %>%
#       dplyr::filter(
#         type == typeInfo
#         , !is.na(val)
#       ) %>%
#       dplyr::select(-type)
# 
#     dataL1 = MBA::mba.points(tmpData, gridData)
# 
#     dataL2 = dataL1 %>%
#       as.data.frame() %>%
#       as.tibble() %>%
#       dplyr::rename(
#         xAxis = xyz.est.x
#         , yAxis = xyz.est.y
#         , zAxis = xyz.est.z
#       ) %>%
#       dplyr::mutate(type = typeInfo)
# 
#     dataL3 = dplyr::bind_rows(dataL3, dataL2)
#   }
# }
# 
# typeList = dataL3$type %>% unique()
# cat(sprintf("[CHECK] typeList : %s", length(typeList)), "\n")
# print(typeList)
# 
# dataL4 = dataL3 %>%
#   dplyr::group_by(xAxis, yAxis) %>%
#   dplyr::summarise(
#     meanVal = mean(zAxis, na.rm = TRUE)
#   ) %>%
#   dplyr::mutate(
#     meanVal = ifelse(meanVal < 0, 0, meanVal)
#   )
# 
# maxData = dataL4 %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(meanVal == max(dataL4$meanVal, na.rm = TRUE))
# 
# # 경주 지점
# # posLon = 129.2
# # posLat = 35.8
# 
# 초기신라 지점
# posLon = 116.6
# posLat = 31.8

# posData = dataL4 %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(xAxis == posLon, yAxis == posLat)
# 
# cat(sprintf("[CHECK] maxData : %s", maxData$meanVal), "\n")
# cat(sprintf("[CHECK] posData : %s", posData$meanVal), "\n")

# **************************************************
# 시뮬레이션 테스트
# **************************************************
# 파일 검색
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "모집단78개_bootDataL2_14-10000-30_129.2-35.8.csv"))

# 파일 읽기
plotData = readr::read_csv(fileInfo)

# 밀도함수 시각화
makePlot = ggplot(data = plotData, aes(x = xAxis, y = yAxis, color = meanVal)) +
  stat_density2d(aes(fill = ..density..), n = 100 * 10, contour = FALSE, geom = "raster") +
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

# 밀도함수 데이터 추출
plotDataL1 = ggplot_build(makePlot)$data[[1]]

# 격자 데이터 기반으로 밀도함수 내삽
mbaData = plotDataL1 %>%
  dplyr::select(x, y, density) %>% 
  MBA::mba.points(., gridData) %>% 
  as.data.frame() %>%
  as.tibble() %>%
  dplyr::rename(
    xAxis = xyz.est.x
    , yAxis = xyz.est.y
    , density = xyz.est.z
  ) %>% 
  dplyr::mutate(
    xyAxis = sprintf("%s-%s", xAxis, yAxis)
  )
  # dplyr::mutate(across(c(xAxis, yAxis), as.numeric))


# ggplot(data = mbaData, aes(x = xAxis, y = yAxis, fill = density, color = density)) +
#   geom_raster(interpolate = TRUE, na.rm = TRUE) 

plotDataL2 = plotData %>% 
  # dplyr::mutate(across(c(xAxis, yAxis), as.numeric)) %>% 
  dplyr::mutate(
    xyAxis = sprintf("%s-%s", xAxis, yAxis)
  ) %>% 
  # dplyr::bind_cols(plotData)
  # dplyr::mutate(
  #   xAxis = round(x, 1)
  #   , yAxis = round(y, 1)
    # xVal = round(x, 1)
    # , yVal = round(y, 1)
    # , xAxis = ifelse(round(xVal * 10) %% 2 == 0, xVal, xVal - 0.1)
    # , yAxis = ifelse(round(yVal * 10) %% 2 == 0, yVal, yVal - 0.1)
  # ) %>%
  # dplyr::distinct(xAxis, yAxis, .keep_all = TRUE) %>%
  # dplyr::left_join(mbaData, by = c("xAxis" = "xAxis", "yAxis" = "yAxis"))
  dplyr::left_join(mbaData, by = c("xyAxis" = "xyAxis"), suffix = c("", ".y")) %>% 
  dplyr::select(-xAxis.y, -yAxis.y, keep_all)


# 초기신라 지점
stnLon = 116.6
stnLat = 31.8

plotDataL3 = plotDataL2 %>% 
  dplyr::mutate(
    distKm = geosphere::distHaversine(cbind(xAxis, yAxis), cbind(stnLon, stnLat)) / 1000.0
  ) %>% 
  dplyr::arrange(distKm)

head(plotDataL3)

mbaDataL1 = mbaData %>% 
  dplyr::mutate(
    distKm = geosphere::distHaversine(cbind(xAxis, yAxis), cbind(stnLon, stnLat)) / 1000.0
  ) %>% 
  dplyr::arrange(distKm)

# 1  117.  31.8 0.000802 116.6-31.8    0  
head(mbaDataL1)

# 파일 저장
saveFile = sprintf("%s/%s/%s-%s_%s-%s_%s-%s.csv", globalVar$outPath, serviceName, sheetName, "plotDataL2", bootNum, bootDo, posLon, posLat)
dir.create(path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(x = plotDataL2, file = saveFile)
cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")


# 밀도함수 시각화 
saveImg = sprintf("%s/%s/%s-%s_%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "ColorDen", bootNum, bootDo, posLon, posLat)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

makePlot = ggplot(data = plotDataL2, aes(x = xAxis, y = yAxis, fill = density, color = density)) +
  geom_raster(interpolate = TRUE, na.rm = TRUE) +
  geom_point(aes(x = stnLon, y = stnLat), color = "green", shape = 15, alpha = 0.25) +
  scale_fill_gradientn(colours = cbMatlab) +
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
# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# ==============================================================================
# 밀도함수에 따른 빈도분포
# ==============================================================================
plotDataL3 = plotDataL2 %>% 
  dplyr::filter(! is.na(density))

saveImg = sprintf("%s/%s/%s-%s_%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "HistDen", bootNum, bootDo, posLon, posLat)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
mainTitle = sprintf("Count : %s", length(plotDataL3$density))

# breakList = seq(0, max(plotDataL3$density, na.rm = TRUE) + 0.001, by = 0.001)
# tickList = seq(0, max(plotDataL3$density, na.rm = TRUE) + 0.001, by = 0.001)
breakList = seq(0, max(plotDataL3$density, na.rm = TRUE) + 0.0004, by = 0.0004)
tickList = seq(0, max(plotDataL3$density, na.rm = TRUE) + 0.0004, by = 0.0004)
histData = hist(plotDataL3$density, breaks = breakList)
hist(plotDataL3$density, main = mainTitle, xlab = NULL, breaks = breakList, xaxt = "n")
axis(side = 1, at = tickList, las = 2)
text(histData$mids, histData$counts, pos = 3, labels = histData$counts)

dev.off()
# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# i = 2
tickList = seq(0, max(plotDataL3$density, na.rm = TRUE) + 0.0001, by = 0.0001)
for (i in 1:length(tickList)) {

  if (is.na(tickList[i])) next
  if (is.na(tickList[i + 1])) next
  
  cat(sprintf("[CHECK] tick : %s", tickList[i]), "\n")
  
  tickInfo = tickList[i]
    
  plotDataL4 = plotDataL3 %>% 
    dplyr::filter(dplyr::between(density, tickInfo, tickList[i + 1]))
  
  if (nrow(plotDataL4) < 1) next
  
  hist(plotDataL4$meanVal)
  
  saveImg = sprintf("%s/%s/%s-%s-%.4f_%s-%s_%s-%s.png", globalVar$figPath, serviceName, sheetName, "HistDen", tickInfo, bootNum, bootDo, posLon, posLat)
  dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
  png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
  mainTitle = sprintf("Tick : %.4f / Count : %s", tickInfo, length(plotDataL4$density))
  
  # min(plotDataL3$meanVal, na.rm = TRUE)
  # max(plotDataL3$meanVal, na.rm = TRUE)
  
  breaks = seq(0.60, max(plotDataL4$meanVal, na.rm = TRUE) + 0.05, by = 0.05)
  ticks = seq(0.60, max(plotDataL4$meanVal, na.rm = TRUE) + 0.05, by = 0.05)
  
  histData = hist(plotDataL4$meanVal, breaks = breaks)
  hist(plotDataL4$meanVal, main = mainTitle, xlab = NULL, breaks = breaks, xaxt = "n")
  axis(side = 1, at = ticks)
  text(histData$mids, histData$counts, pos = 3, labels = histData$counts)
  
  dev.off()
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
}

# shell.exec(saveImg)

# ==============================================================================
# 밀도함수 기반으로 군집화
# ==============================================================================
# kmeans 다중 클러스터링 (데이터 표준화 O)
plotDataL4 = plotDataL3 %>%
  dplyr::select(xAxis, yAxis, density)

# plotDataL4 = plotDataL3 %>%
#   dplyr::select(xAxis, yAxis, meanVal)

# plotDataL4 = plotDataL3 %>%
#   dplyr::select(xAxis, yAxis, meanVal, density)

plotDataL5 = plotDataL4 %>%
  dplyr::mutate(across(everything(), ~scale(.)))

# plotDataL5 = plotDataL4 %>%
#   dplyr::mutate_each(
#     funs(scale)
#     , vars = c(colnames(plotDataL3))
#   )

# nClu = 3

statDataL3 = tibble::tibble()
for (nClu in 1:15) {
  
  cat(sprintf("[CHECK] nClu : %s", nClu), "\n")
  
  # 클러스터링 모형
  kcluModel = plotDataL5 %>% 
    purrr::keep(is.numeric) %>% 
    amap::Kmeans(centers = nClu, method = "euclidean")
  
  # 원시 데이터 + 클러스터링 결과
  pointAssignments = broom::augment(kcluModel, plotDataL5) %>% 
    dplyr::mutate(across(colnames(plotDataL5), ~grt::unscale(.)))
  
  # 최근접 찾기
  statData = pointAssignments %>% 
    dplyr::mutate(
      distKm = geosphere::distHaversine(cbind(xAxis, yAxis), cbind(stnLon, stnLat)) / 1000.0
    ) %>% 
    dplyr::arrange(distKm) %>% 
    dplyr::slice(1) %>% 
    dplyr::mutate(
      isFlag = TRUE
    )
  
  statDataL1 = statData %>% 
    dplyr::select(.cluster, isFlag)
  
  statDataL2 = pointAssignments %>% 
    dplyr::group_by(.cluster) %>% 
    dplyr::summarise(
      cnt = n()
      , rat = cnt / nrow(pointAssignments) * 100.0
    ) %>% 
    dplyr::left_join(statDataL1, by = c(".cluster" = ".cluster")) %>% 
    dplyr::mutate(
      nClu = nClu
    )
  
  statDataL3 = dplyr::bind_rows(statDataL3, statDataL2)
  
  saveImg = sprintf("%s/%s/%s-Normal%s.png", globalVar$figPath, serviceName, "Kmeans-hist-var3", nClu)
  # saveImg = sprintf("%s/%s/%s-Normal%s.png", globalVar$figPath, serviceName, "Kmeans-hist-var4", nClu)
  mainTitle = sprintf("%s개 군집에 따른 빈도수 (초기신라 : %s번)", nClu, statData$.cluster)
  dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
  
  # makePlot = ggplot(pointAssignments, aes(x = .cluster, fill = .cluster)) +
  #   geom_histogram(stat = "count", alpha = 0.6) +
  #   geom_text(stat = "count", aes(label = ..count..), size = 5.0, vjust = 2.0, color = "black") +
  #   labs(x = "군집", y = "빈도수", fill = NULL, title = NULL, subtitle = mainTitle) +
  #   theme(
  #     text = element_text(size = 16)
  #     , legend.position = "top"
  #   )
  
  makePlot = ggplot(statDataL2, aes(x = .cluster, y = cnt, fill = .cluster, label = cnt)) +
    geom_bar(stat="identity", alpha = 0.6) +
    geom_text(size = 5.0, vjust = 2.0, color = "black") +
    labs(x = "군집", y = "빈도수", fill = NULL, title = NULL, subtitle = mainTitle) +
    guides(color = guide_legend(nrow = 1)) +
    theme(
      text = element_text(size = 16)
      , legend.position = "top"
    )
  ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
  
  # shell.exec(saveImg)
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
  
  
  # pointAssignments
  
  # 클러스터링 결과
  clusterInfo = broom::tidy(kcluModel) %>% 
    dplyr::mutate(
      xAxis = unscale(xAxis, plotDataL5, "xAxis")
      , yAxis = unscale(yAxis, plotDataL5, "yAxis")
      # , meanVal = unscale(meanVal, plotDataL5, "meanVal")
      , density = unscale(density, plotDataL5, "density")
    )
  # dplyr::mutate(across(colnames(plotDataL5), ~grt::unscale(.)))
  # dplyr::mutate_each_(
  #   funs(grt::unscale)
  #   , vars = colnames(plotDataL5)
  # )
  
  # clusterInfo
  
  # 클러스터링 통계 결과 (amap::Kmean 라이브러리 이용 시 불가)
  # totWithinss = sum(kcluModel$withinss, na.rm = TRUE)
  
  # 시각화
  saveImg = sprintf("%s/%s/%s-Normal%s.png", globalVar$figPath, serviceName, "Kmeans-Cluster-var3", nClu)
  # saveImg = sprintf("%s/%s/%s-Normal%s.png", globalVar$figPath, serviceName, "Kmeans-Cluster-var4", nClu)
  dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
  
  makePlot = ggplot(data = pointAssignments, aes(x = xAxis , y = yAxis, color = .cluster)) +
    geom_point(shape = 16, alpha = 0.5) + 
    geom_label(data = clusterInfo, aes(x = xAxis , y = yAxis, label = cluster, fill = factor(cluster)), size = 8, colour = "white", fontface = "bold", show.legend = FALSE) +
    geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = NA) +
    metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(89.99, 150.01), expand = c(0, 0)) +
    metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(9.99, 60), expand = c(0, 0)) +
    guides(color = guide_legend(nrow = 1)) +
    labs(subtitle = NULL, x = NULL, y = NULL, fill = NULL, colour = NULL, title = NULL, size = NULL) +
    theme(
      text = element_text(size = 16)
      , legend.position = "top"
    )
  
  ggsave(makePlot, filename = saveImg, width = 10, height = 10, dpi = 600)
  
  # shell.exec(saveImg)
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
}

statDataL4 = statDataL3 %>% 
  dplyr::filter(isFlag == TRUE)

saveFile = sprintf("%s/%s/%s.csv", globalVar$outPath, serviceName, "Kmeans-Cluster-var3")
# saveFile = sprintf("%s/%s/%s.csv", globalVar$outPath, serviceName, "Kmeans-Cluster-var4")
dir.create(path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(x = statDataL4, file = saveFile)
cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")



# ****************************************************************
# kmeans 다중 클러스터링 (데이터 표준화 O)
# ****************************************************************
# kcluModelList = dplyr::tibble(nClu = 1:12) %>%
#   dplyr::mutate(
#     kcluModel = purrr::map(
#       nClu
#       , ~ amap::Kmeans(plotDataL5, centers = .x, method = "euclidean")
#     )
#     , augmented = purrr::map(kcluModel, broom::augment, plotDataL5)
#     , tidied = purrr::map(kcluModel, broom::tidy)
#     # 클러스터링 통계 결과 (amap::Kmean 라이브러리 이용 시 불가)
#     # , glanced = purrr::map(kcluModel, broom::glance)
#     # , tot.withinss = purrr::map(kcluModel, ~ sum(.x$withinss, na.rm = TRUE))
#   ) 
# 
# 
# # 원시 데이터+ 클러스터링 결과 
# pointAssignments = kcluModelList %>%
#   dplyr::select(nClu, augmented) %>%
#   tidyr::unnest(augmented) %>%
#   dplyr::mutate(
#     xAxis = unscale(xAxis, plotDataL5, "xAxis")
#     , yAxis = unscale(yAxis, plotDataL5, "yAxis")
#     # , meanVal = unscale(meanVal, plotDataL5, "meanVal")
#     , density = unscale(density, plotDataL5, "density")
#   )
#   # dplyr::mutate(across(colnames(plotDataL5), ~grt::unscale(.)))
# 
# # 클러스터링 결과
# clusterInfo = kcluModelList %>%
#   dplyr::select(nClu, tidied) %>% 
#   tidyr::unnest(tidied) %>% 
#   dplyr::mutate(
#     xAxis = unscale(xAxis, plotDataL5, "xAxis")
#     , yAxis = unscale(yAxis, plotDataL5, "yAxis")
#     # , meanVal = unscale(meanVal, plotDataL5, "meanVal")
#     , density = unscale(density, plotDataL5, "density")
#   )
# 
# 
# # 시각화
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "Kmeans-Cluster-var3-Multi")
# # saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "Kmeans-Cluster-var4-Multi")
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# 
# ggplot(data = pointAssignments, aes(x = xAxis , y = yAxis, color = .cluster)) +
#   geom_point(size = 0.2, shape = 16, alpha = 0.5) + 
#   geom_label(data = clusterInfo, aes(x = xAxis , y = yAxis, label = cluster, fill = factor(cluster)), size = 3, colour = "white", fontface = "bold", show.legend = FALSE) +
#   geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = NA) +
#   metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(89.99, 150.01), expand = c(0, 0)) +
#   metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(9.99, 60), expand = c(0, 0)) +
#   guides(color = guide_legend(nrow = 1)) +
#   facet_wrap(~ nClu) +
#   labs(
#     subtitle = NULL
#     , x = NULL
#     , y = NULL
#     , fill = NULL
#     , colour = NULL
#     , title = NULL
#     , size = NULL
#   ) +
#   theme(
#     text = element_text(size = 16)
#     , legend.position = "top"
#     , legend.box = "horizontal"
#     , axis.line = element_blank()
#     , axis.text = element_blank()
#     , axis.ticks = element_blank()
#     , plot.margin = unit(c(0, 0, 0, 0), 'lines')
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 10, dpi = 600)
# 
# # shell.exec(saveImg)
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
