
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

# Shape파일은 단순이 미국 주 구별하기위한 mask파일이라고 보시면되요! 특정 주만 필터하기위해서요. 저는 아이오와 일리노이 두 개 주만 대상으로 하기때문에 그 특정주에 부여된 ID number가 각각 17,19입니다.
# 그리고 mcd13자료가 추정된 결과값인데, 4밴드로 작성되어 있어요. 그 밴드중에 pos, eos밴드만 사용하시면되요.
# CDL자료는 대상 작물형(콩,옥수수) mask하기위한 자료이구요.
# Mcd13자료에서 shape파일과 cdl자료를 이용해서 대상 주(state)와 대상 작물 픽셀만 mask한 뒤 heatmap으로 시각화하면 결과1입니다.

# 결과2에서 대상변수(x)는 위 결과1의 픽셀을 대상으로 cv값을 계산해야해요. 그리고 실측 csv파일의 cv%값이랑 비교하면됩니다.
# procrustes distance는 R자체에서 통계값으로 구할수 잇는걸로ㅠ알고있는데..
# 이 부분은 저도 잘 모르는 통계값이라서요. 따로 알아보겠습니다.

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
library(maptools)
library(stars)
library(rgeos)
library(terra)
library(tidyterra)
library(ggplot2)
library(Metrics)
library(colorRamps)
library(ggh4x)
library(scales)

cbMatlab = colorRamps::matlab.like(11)

# shapes::procdist 계산을 위해서 패키지 읽기 과정에서 오류
# library(shapes)

# 시작일/종료일 설정
srtDate = "2003-01-01"
endDate = "2020-01-01"
dtDateList = seq(lubridate::ymd(srtDate), lubridate::ymd(endDate), by = "1 year")

# shp 파일 읽기
shpFileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "US_states/s_11au16.shp"))
shpFileInfo = shpFileList[1]
shpData = shapefile(shpFileInfo)
# shpDataL1 = subset(shpData, FIPS %in% c(17, 19))
# plot(shpDataL1)

# 규칙 격자 생성
gridData = noncompliance::expand.grid.DT(
  2003:2020
  , 1:365
  , col.names = c("dtYear", "doy")
)

# *********************************************************************************
# MODIS 및 CDL 자료 가공
# *********************************************************************************
dataL2 = tibble::tibble()
# dtDateInfo = dtDateList[3]
for (i in 1:length(dtDateList)) {
  dtDateInfo = dtDateList[i]

  dtYear = lubridate::year(dtDateInfo)
  dtMonth = lubridate::month(dtDateInfo)

  modisFilePattern = sprintf("MCD13A2_8day_Phenology_US5_NDVI_Klosterman_%s.tif", dtYear)
  fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "Phenology_US5_MODIS", modisFilePattern))
  # modisFilePattern = sprintf("MCD13A2_8day_Phenology_NDVI_%s.tif", dtYear)
  # fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "Phenology_US5_MODIS-20221224", modisFilePattern))

  cornFilePattern = sprintf("Merge_IMG/CDL_fraction_corn_%s_ease.img", dtYear)
  cornFileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "CDL_fraction_US5", cornFilePattern))

  soyFilePattern = sprintf("Merge_IMG/CDL_fraction_soy_%s_ease.img", dtYear)
  soyFileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "CDL_fraction_US5", soyFilePattern))

  if (length(modisFilePattern) < 1) { next }
  if (length(cornFileList) < 1) { next }
  if (length(soyFileList) < 1) { next }

  cat(sprintf("[CHECK] dtDateInfo : %s", dtDateInfo), "\n")

  # MODIS 파일 읽기
  modisData = terra::rast(fileList[1])
  modisDataL1 = terra::project(modisData, "+proj=longlat +datum=WGS84")

  # plot(modisDataL1)

  # corn 및 soy 파일 읽기
  cornData = terra::rast(cornFileList[1])
  crs(cornData) = crs(modisData)
  cornData = terra::project(cornData, "+proj=longlat +datum=WGS84")

  soyData = terra::rast(soyFileList[1])
  crs(soyData) = crs(modisData)
  soyData = terra::project(soyData, "+proj=longlat +datum=WGS84")

  # 각각의 CDL Fraction 값이 0.5 이상이면 corn과 soybean(soy)로 가정함.
  cornData[cornData < 0.5] = NA
  soyData[soyData < 0.5] = NA

  # plot(cornData)
  # plot(soyData)

  # j = 17
  for (j in c(17, 19)) {

    shpDataL1 = subset(shpData, FIPS %in% j)

    cat(sprintf("[CHECK] name : %s", shpDataL1$NAME), "\n")

    # modisDataL1를 기준으로 마스킹 (cornData) 및 영역 자르기 (shpDataL1)  수행
    cornDataL1 = modisDataL1 %>%
      mask(cornData) %>%
      crop(shpDataL1)

    # 서로간의 범위가 다를 경우 에러 발생
    # 마스킹 레이어 (cornData) 및 기준 레이어 (modisDataL1) 리샘플링
    # cornRefData = terra::resample(cornData, modisDataL1)

    # cornDataL1 = modisDataL1 %>%
    #   mask(cornRefData) %>%
    #   crop(shpDataL1)

    # 대상 계절일: "POS", " EOS " ; Band4 inform: c("SOS", "POS", "Senescence", "EOS")
    cornDataL2 = cornDataL1 %>%
      as.data.frame(xy = TRUE) %>%
      tibble::as.tibble() %>%
      magrittr::set_colnames(c("lon", "lat", "SOS", "POS", "Senescence", "EOS")) %>%
      dplyr::select(lon, lat, POS, EOS) %>%
      dplyr::mutate(
        dtYear = dtYear
        , key = "Corn"
        , name = shpDataL1$NAME
      )

   # modisDataL1를 기준으로 마스킹 (soyData) 및 영역 자르기 (shpDataL1)  수행
    soyDataL1 = modisDataL1 %>%
      mask(soyData) %>%
      crop(shpDataL1)

    # plot(soyDataL1)

    soyDataL2 = soyDataL1 %>%
      as.data.frame(xy = TRUE) %>%
      tibble::as.tibble() %>%
      magrittr::set_colnames(c("lon", "lat", "SOS", "POS", "Senescence", "EOS")) %>%
      dplyr::select(lon, lat, POS, EOS) %>%
      dplyr::mutate(
        dtYear = dtYear
        , key = "Soybeans"
        , name = shpDataL1$NAME
      )

    dataL2 = dplyr::bind_rows(dataL2, cornDataL2, soyDataL2)
  }
}

# *********************************************************************************
# 결과1: 각 대상 작물별/주별 추정식물계절일의 분포(Heatmap)
# *********************************************************************************
dataL3 = dataL2 %>%
  tidyr::gather(-c(lon, lat, dtYear, key, name), key = "type", value = "val") %>%
  dplyr::mutate(
    doy = as.integer(val)
    , typeName = ifelse(type == "POS", "Date of Max. growth", "Date of Senescence")
    , group = sprintf("%s (%s, %s)", typeName, key, name)
  )

mainTitle = sprintf("각 대상 작물별-주별 추정식물계절일의 분포4(Heatmap)")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

dataL4 = dataL3 %>%
  dplyr::group_by(dtYear, doy, group) %>%
  dplyr::summarize(
    cnt = n()
  ) %>%
  dplyr::mutate(
    sDate = paste(dtYear, doy, sep = "-")
    , dtDate = lubridate::parse_date_time(sDate, "%Y-%j")
  )

summary(dataL4)


groupList = dataL4$group %>% unique() %>% sort()
# groupInfo = groupList[1]

dataL5 = tibble::tibble()
for (groupInfo in groupList) {

  selData = dataL4 %>%
    dplyr::filter(group == groupInfo) %>%
    dplyr::select(dtYear, doy, cnt) %>%
    MBA::mba.points(gridData) %>%
    as.data.frame() %>%
    as.tibble() %>%
    dplyr::rename(
      dtYear = xyz.est.x
      , doy = xyz.est.y
      , cnt = xyz.est.z
    ) %>%
    dplyr::mutate(
      group = groupInfo
      # dens = cnt / max(cnt, na.rm = TRUE)
      # , dens2 = cnt / sum(cnt, na.rm = TRUE)
      , dens3 = scales::rescale(cnt)

    )

  dataL5 = dplyr::bind_rows(dataL5, selData)
}

summary(dataL5)

ggplot(data = dataL5, aes(x = doy, y = dtYear, fill = dens3)) +
  geom_raster(interpolate = FALSE, na.rm = TRUE) +
  # geom_tile(na.rm = TRUE) +
  # scale_x_continuous(expand = c(0, 0), limits = c(160, 280), minor_breaks = seq(160, 280, 20), breaks = seq(160, 280, 20)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 365), minor_breaks = seq(0, 365, 40), breaks = seq(0, 365, 40)) +
  scale_y_continuous(expand = c(0, 0), limits = c(2002.5, 2019.5), minor_breaks = seq(2000, 2022, 1), breaks = seq(2000, 2022, 1)) +
  # scale_fill_gradientn(colours = cbMatlabva, limits = c(0, 5000), na.value = cbMatlab[length(cbMatlab)]) +
  # scale_fill_gradientn(colours = cbMatlab, limits = c(0, 0.2), na.value = cbMatlab[length(cbMatlab)]) +
  scale_fill_gradientn(colours = cbMatlab, limits = c(0, 0.2), na.value = cbMatlab[length(cbMatlab)]) +
  labs(
    # title = mainTitle
    x = "DOY"
    , y = "Year"
    , color = NULL
    , fill = "density"
  ) +
  theme(
    text = element_text(size = 11)
    # , axis.text.x = element_text(size = 12, angle = 90, hjust = 1)
    # , axis.text.x = element_text(size = 12, angle = 0, hjust = 1)
    , legend.position = "top"
    , legend.key.width = unit(2, "cm")
    , plot.margin = unit(c(0, 4, 0, 0), "mm")
  ) +
  facet_wrap(~group, ncol = 4) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 1000)
  # ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# *********************************************************************************
# 실측 통계자료와 비교
# *********************************************************************************
# CSV파일 보실때요. 예) MODIS_corn_progress_IA.csv
# header 설명이 부족했는데,
# "year stage cv DOY NASS_DOY "
# 여기서 Stage 1은 POS, 2는 EOS예요.
# 산점도에서 두개를 각각 색깔로 구분해주시면 되요.
# 제가 드린 예비결과 포멧에는 POS(빨간원형), EOS(연두색원형) 이렇게 표현되어 있어요.
# DOY 추정값
# NASS_DOY 실측값

fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "comparison_CV/MODIS_*_progress_*.csv"))

valData = tibble::tibble()
# fileInfo = fileList[1]
for (fileInfo in fileList) {

  fileName = tools::file_path_sans_ext(fs::path_file(fileInfo))
  csvData = readr::read_csv(fileInfo)

  csvDataL1 = csvData %>%
    dplyr::mutate(
      key = ifelse(stringr::str_detect(fileName, regex("corn")), "Corn", "Soybeans")
      , stageName = ifelse(stage == 1, "POS", "EOS")
      # , typeName = ifelse(stageName == "POS", "Date of Max. growth", "Date of Senescence")
      , typeName = ifelse(stageName == "POS", "Max. growth", "Senescence")
      , type = ifelse(stringr::str_detect(fileName, regex("IL")), "Illinois", "Iowa")
      , group = sprintf("%s (%s)", key, typeName)
      , group2 = sprintf("%s in %s", key, type)
    )

   # 검증 데이터 생성
  valData = dplyr::bind_rows(valData, csvDataL1)
}


groupList = valData$group %>% unique() %>% sort()
yearList = valData$year %>% unique() %>% sort()

# 그림 2를 위한 검증 데이터 생성
valDataL2 = tibble::tibble()
for (groupInfo in groupList) {
   cat(sprintf("[CHECK] %s", groupInfo), "\n")

   for (yearInfo in yearList) {

    selData = valData %>%
      dplyr::filter(
        group == groupInfo
        , year == yearInfo
      )

      selDataL2 = data.frame(
        group = groupInfo
        , year = yearInfo
        , Bias = Metrics::bias(selData$NASS_DOY, selData$DOY)
        , RMSE = Metrics::rmse(selData$NASS_DOY, selData$DOY)
      )

     valDataL2 = dplyr::bind_rows(valDataL2, selDataL2)
  }
}

# 결과2: 통계자료 Bias/RMSE/Procrustes distance에 대한 Boxpolt
valDataL3 = valDataL2 %>%
  dplyr::select(group, Bias, RMSE) %>%
  tidyr::gather(-group, key = "key", value = "val")

# 정렬
# valDataL3$group = forcats::fct_relevel(valDataL3$group, c("Corn (Date of Senescence)", "Soybeans (Date of Senescence)", "Corn (Date of Max. growth)", "Soybeans (Date of Max. growth)"))
valDataL3$group = forcats::fct_relevel(valDataL3$group, c("Corn (Senescence)", "Soybeans (Senescence)", "Corn (Max. growth)", "Soybeans (Max. growth)"))

mainTitle = sprintf("통계자료 %s에 대한 상자 그림", "bias-RMSE")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

# ggplot(valDataL3, aes(x = group, y = val, color = group)) +
ggplot(valDataL3, aes(x = group, y = val)) +
  geom_boxplot(show.legend = FALSE) +
  geom_abline(intercept = 0, slope = 0, linetype = 2, color = "gray", size = 0.5) +
  labs(x = NULL, y = "Day", color = NULL, fill = NULL, subtitle = NULL) +
  theme_bw() +
  theme(
    text = element_text(size = 16)
    # , legend.position = "top"
    , axis.text.x = element_text(size = 12, angle = 90, hjust = 1)
    # , legend.text = element_text(size = 10)
    , panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
  ) +
  facet_wrap(~key, scale = "free_y") +
  ggh4x::facetted_pos_scales(
    y = list(
      key == "Bias" ~ scale_y_continuous(breaks=seq(-100, 100, 20), limits=c(-100, 60))
      , key == "RMSE" ~ scale_y_continuous(breaks=seq(0, 100, 10), limits=c(0, 100))
    )
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# 결과3 실측 통계자료와 비교한 산점도 결과
valDataL4 = valData %>%
  dplyr::select(stageName, group2, DOY, NASS_DOY)

mainTitle = sprintf("%s", '실측 통계자료와 비교한 산점도 결과')
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

# summary(valDataL1)

valDataL4$stageName = forcats::fct_relevel(valDataL4$stageName, c("POS", "EOS"))
valDataL4$group2 = forcats::fct_relevel(valDataL4$group2, c("Corn in Iowa", "Corn in Illinois", "Soybeans in Iowa", "Soybeans in Illinois"))

ggplot(data = valDataL4, aes(x = DOY, y = NASS_DOY, color = stageName, alpha=NASS_DOY)) +
  geom_point() +
  scale_alpha(guide = "none") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black", size = 0.5) +
  theme_bw() +
  scale_x_continuous(limits = c(180, 360), breaks=seq(180, 360, 40)) +
  scale_y_continuous(limits = c(180, 360), breaks=seq(180, 360, 40)) +
  # scale_colour_manual(values = c("red", "green")) +
  # scale_colour_manual(values = c("#F8766D", "#00BA38")) +
  scale_colour_manual(values = c("red", "#00BA38")) +
  labs(
    title = NULL
    , subtitle = NULL
    , x = "Obs. day of percent crop progress (DOY)"
    , y = "Est. day of percent crop progress (DOY)"
    , colour = NULL
    , fill = NULL
    , alpha = NULL
  ) +
  theme(
    text = element_text(size = 18)
    # , legend.position = "top"
    , legend.position = c(0.94, 0.06)
    , panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
  ) +
  facet_wrap(~group2, ncol=2) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ggplot2::last_plot()

# statData = dataL2 %>%
#   tidyr::gather(-c(lon, lat, dtYear, key, name), key = "type", value = "val") %>%
#   dplyr::group_by(dtYear, key, name, type) %>%
#   dplyr::summarize(
#      meanVal = mean(val, na.rm = TRUE)
#     , sdVal = sd(val, na.rm = TRUE)
#     , cvVal = (sdVal / meanVal) * 100.0
#     # meanPos = mean(POS, na.rm = TRUE)
#     # , sdPos = sd(POS, na.rm = TRUE)
#     # , cvPos = (sdPos / meanPos) * 100.0
#     # , meanEos = mean(EOS, na.rm = TRUE)
#     # , sdEos = sd(EOS, na.rm = TRUE)
#     # , cvEos = (sdEos / meanEos) * 100.0
#   )
#
# typeList = statData$type %>% unique() %>% sort()
# nameList = statData$name %>% unique() %>% sort()
# keyList = statData$key %>% unique() %>% sort()
# stageList = c("Mature_progress", "Silking_progress", "Dropping_leaves_progress", "SettingPod_progress")
#
# # 검증 데이터 생성
# valDataL1 = tibble::tibble()
# # 그림 2를 위한 검증 데이터 생성
# valDataL2 = tibble::tibble()
#
# for (typeInfo in typeList) {
# for (nameInfo in nameList) {
# for (keyInfo in keyList) {
#   cat(sprintf("[CHECK] %s : %s : %s", typeInfo, nameInfo, keyInfo), "\n")
#
#   type = ifelse(nameInfo == "Illinois", "IL", "IA")
#   key = ifelse(keyInfo == "Corn", "corn", "soy")
#
#   statDataL1 = statData %>%
#     dplyr::filter(
#       type == typeInfo
#       , key == keyInfo
#       , name == nameInfo
#     )
#
#   # stageInfo = stageList[1]
#   for (stageInfo in stageList) {
#     # csv 파일 읽기
#      csvFilePattern = sprintf("%s_%s_%s.csv", key, stageInfo, type)
#     csvFileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "NASS_prograss_for_eachcrops", csvFilePattern))
#     if (length(csvFileList) < 1) { next }
#     csvFileInfo = csvFileList[1]
#     csvData = readr::read_csv(csvFileInfo)
#
#     # 검증 데이터 생성
#     valData = statDataL1 %>%
#       dplyr::left_join(csvData, by = c("dtYear" = "Year")) %>%
#       dplyr::mutate(
#         stage = stageInfo
#       )
#
#     valDataL1 = dplyr::bind_rows(valDataL1, valData)
#
#     typeName = ifelse(typeInfo == "POS", "Date of Max. growth", "Date of Senescence")
#
#     tmpData = data.frame(
#       id = sprintf("%s (%s)",keyInfo, typeName)
#       , key = keyInfo
#       , name = nameInfo
#       , type = typeInfo
#       , stage = stageInfo
#       , Bias = Metrics::bias(valData$cvVal, valData$Value)
#       , RMSE = Metrics::rmse(valData$cvVal, valData$Value)
#     )
#
#    valDataL2 = dplyr::bind_rows(valDataL2, tmpData)
#
#   }
# }}}
#
#
# # 결과2: 통계자료 Bias/RMSE/Procrustes distance에 대한 Boxpolt
# valDataL3 = valDataL2 %>%
#   dplyr::select(id, Bias, RMSE) %>%
#   tidyr::gather(-id, key = "key", value = "val")
#
# # 정렬
# valDataL3$id = forcats::fct_relevel(valDataL3$id, c("Corn (Date of Senescence)", "Soybeans (Date of Senescence)", "Corn (Date of Max. growth)", "Soybeans (Date of Max. growth)"))
#
# mainTitle = sprintf("통계자료 %s에 대한 상자 그림", "bias-RMSE")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
# ggplot(valDataL3, aes(x = id, y = val, color = id)) +
#   geom_boxplot(show.legend = TRUE) +
#   labs(x = NULL, y = "Bias/RMSE", color = NULL, fill = NULL, subtitle = NULL) +
#   theme(
#     text = element_text(size = 16)
#     , legend.position = "top"
#     , axis.text.x = element_text(size = 10, angle = 45, hjust = 1)
#     , legend.text = element_text(size = 10)
#   ) +
#   facet_wrap(~key, scale = "free_y") +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
#
#
# # 결과3: 실측 통계자료와 비교한 산점도 결과 (progress(0-100%) 값으로 산출하여 비교, density color로 표현)
# nameList = valDataL1$name %>% unique() %>% sort()
# keyList = valDataL1$key %>% unique() %>% sort()
#
# for (nameInfo in nameList) {
# for (keyInfo in keyList) {
#
#   valDataL4 = valDataL1 %>%
#     dplyr::filter(
#       name == nameInfo
#       , key == keyInfo
#     )
#
#   mainTitle = sprintf("%s in %s", keyInfo, nameInfo)
#   saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
#   dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
#   makePlot = ggpubr::ggscatter(valDataL4, x = "Value", y = "cvVal", color = "type", alpha = "Value", conf.int = FALSE, cor.coef = FALSE, add.params = list(color = "black", fill = "lightgray")) +
#     geom_smooth(aes(color = type), method = "lm", se = FALSE, show.legend = FALSE) +
#     ggpubr::stat_regline_equation(aes(color = type), method = "lm", parse = TRUE, label.x.npc = 0.02, label.y.npc = 0.98, size = 6, show.legend = FALSE) +
#     ggpubr::stat_cor(aes(color = type), label.x.npc = 0.52, label.y.npc = 0.98, p.accuracy = 0.01, r.accuracy = 0.01, size = 6, show.legend = FALSE) +
#     geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black", size = 0.5) +
#     theme_bw() +
#     scale_x_continuous(limits = c(0, 100)) +
#     scale_y_continuous(limits = c(0, 100)) +
#     labs(
#       title = mainTitle
#       , subtitle = NULL
#       , x = "Obs. day of percent crop progress (DOY)"
#       , y = "Est. day of percent crop progress (DOY)"
#       , colour = NULL
#       , fill = NULL
#       , alpha = NULL
#     ) +
#     theme(
#       text = element_text(size = 18)
#       , legend.position = "top"
#     )
#
#   ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
#   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# }}