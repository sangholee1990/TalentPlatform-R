#=====================================
# Usage
#=====================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# # env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# # env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정
#
# prjName = "test"
# serviceName = "LSH0339"
#
# if (Sys.info()["sysname"] == "Windows") {
#   contextPath = ifelse(env == "local", ".", "E:/04. TalentPlatform/Github/TalentPlatform-R")
# } else {
#   contextPath = ifelse(env == "local", ".", "/SYSTEMS/PROG/R/PyCharm")
# }
#
# if (env == "local") {
#   globalVar = list(
#     "inpPath" = contextPath
#     , "figPath" = contextPath
#     , "outPath" = contextPath
#     , "tmpPath" = contextPath
#     , "logPath" = contextPath
#   )
# } else {
#   source(here::here(file.path(contextPath, "src"), "InitConfig.R"), encoding = "UTF-8")
# }

#=====================================
# Install
#=====================================

# ******************************************************************************
# rtools 설치
# ******************************************************************************

# ******************************************************************************
# Rtools MinGW 64-bit 실행
# ******************************************************************************
# pacman -Syu
# pacman -Su
# exit

# ******************************************************************************
# 시스템 환경변수 설정
# (내컴퓨터 > 속성 > 고급 시스템 설정 > 환경 변수 > 계정에 대한 사용자 변수 선택)
# ******************************************************************************

# MSYSTEM : MINGW64
# PKG_CONFIG_PATH : /mingw64/lib/pkgconfig:/mingw64/share/pkgconfig
# Path :
  # C:\rtools40\mingw64\bin
  # C:\rtools40\usr\local\bin
  # C:\rtools40\usr\bin
  # C:\rtools40\bin

# ******************************************************************************
# cmd 창에서 경로 확인
# ******************************************************************************
# where bash
# where ls
# where gcc
# where g++
# where gfortran
# where make


#=====================================
# Init Library
#=====================================
if (Sys.info()["sysname"] == "Windows") {
  .libPaths("E:/04. TalentPlatform/Github/TalentPlatform-R/resources/lib")
} else {
  Sys.setenv(PROJ_LIB = "/usr/local/anaconda3/envs/r36/share/proj")
}

# C:\Users\saima\Documents
# C:/Users/saima/OneDrive/Documents/R/win-library/4.1
# 라이브러리 경로
# .libPaths()

# C:/Users/sangh/Documents/R/win-library/4.1
# E:/04. TalentPlatform/Github/TalentPlatform-R/resources/lib
# .libPaths("E:/04. TalentPlatform/Github/TalentPlatform-R/resources/lib")
# .libPaths(c("C:/Users/saima/OneDrive/Documents/R/win-library/4.1", .libPaths()))
#
# .libPaths("C:/R/R-4.0.2/library/")
# .libPaths()C

## [1] "C:/Users/yoonani/Documents/R/win-library/3.2"
## [2] "C:/Program Files/R/R-3.2.2/library"

# install_load_package = function(packages_v){
#   #
#   new.packages <- packages_v[!(packages_v %in% installed.packages()[,"Package"])]
#   if(length(new.packages) > 0) { install.packages(new.packages) } 
#   lapply(packages_v, require, character.only = TRUE)
#   invisible(capture.output())
#   cat("--------------- \n")
#   lapply(packages_v, function(x) {cat(x, "are loaded! \n")})
#   cat("--------------- \n")
#   #
# }

#=====================================
# Init Env
#=====================================
rm(list = setdiff(ls(), c("env", "prjName", "serviceName", "contextPath")))

# 시스템 환경변수 설정 (내컴퓨터 > 속성 > 고급 시스템 설정 > 환경 변수 > 계정에 대한 사용자 변수 선택)
# Sys.getenv("HOME")
# Sys.getenv("R_USER")
# Sys.getenv("R_LIBS_USER")
# Sys.getenv("R_LIBS")

# HOME="C:/Users/sangh"
# R_USER="C:/Users/sangh"
# R_LIBS="C:/Users/sangh/R/win-library"
# R_LIBS_USER="C:Program FilesR/R-4.1.2/library"

#=====================================
# Set Env
#=====================================
# 인코딩 초기 설정
# Sys.setlocale("LC_ALL", "en_US.UTF-8")
# Sys.setlocale("LC_ALL", "ko_KR.UTF-8")

# 인코딩 정보 확인
# Sys.getlocale()

# Sys.setlocale("LC_ALL", "C")
# options(encoding = "UTF-8")
# Sys.setenv(LANG = "ko_KR.UTF-8")

# Sys.setlocale("LC_ALL", "Korean")
# options(encoding = "UTF-8")
# Sys.setenv(LANG = "ko_KR.UTF-8")

# Sys.setlocale("LC_ALL", "English")
# options(encoding = "UTF-8")
# Sys.setenv(LANG = "en_US.UTF-8")

# Sys.setlocale("LC_CTYPE", ".1251")

# Open API Key
configInfo = yaml::yaml.load_file(file.path(contextPath, "resources", "config", "system.cfg"))

globalVar = list(
  # 사용자 옵션
  "optDig" = 10
  # , "memLimit" = 9999999

  # 환경변수 경로  
  , "contextPath" = contextPath
  # , "initResPath" = file.path(contextPath, "InitResource")
  , "srcPath" = file.path(contextPath, "src")
  , "resPath" = file.path(contextPath, "resources")
  , "cfgPath" = file.path(contextPath, "resources", "config")
  , "inpPath" = ifelse(Sys.info()["sysname"] == "Windows", file.path(contextPath, "resources", "input", prjName), "/DATA/INPUT")
  , "figPath" = ifelse(Sys.info()["sysname"] == "Windows", file.path(contextPath, "resources", "fig", prjName), "/DATA/FIG")
  , "outPath" = ifelse(Sys.info()["sysname"] == "Windows", file.path(contextPath, "resources", "output", prjName), "/DATA/OUTPUT")
  , "logPath" = file.path(contextPath, "resources", "log", prjName)
  , "tmpPath" = file.path(contextPath, "resources", "tmp", prjName)
  , "mapPath" = file.path(contextPath, "resources", "config", "mapInfo")
  , "dbPath" = file.path(contextPath, "resources", "config", "db")
  , "systemPath" = file.path(contextPath, "resources", "config", "system.cfg")
  , "seleniumPath" = file.path(contextPath, "resources", "config", "selenium")
  , "fontPath" = file.path(contextPath, "resources", "config", "fontInfo")
  
  # 오픈 API키
  , "googleKey" = configInfo$default$googleKey
  , "dataKey" = configInfo$default$dataKey
  , "naverKeyId" = configInfo$default$naverKeyId
  , "naverKeyPw" = configInfo$default$naverKeyPw
  , "kakaoRestApiKey" = configInfo$default$kakaoRestApiKey
  , "gyeonggiDataKey" = configInfo$default$gyeonggiDataKey
  , "naverApigwApiKeyId" = configInfo$default$naverApigwApiKeyId
  , "naverApigwApiKey" = configInfo$default$naverApigwApiKey
)

utils::ls.str(globalVar)


# 기본 설정 복사
# isResDir = dir.exists(path = globalVar$resPath)
# isInitResDir = dir.exists(path = globalVar$initResPath)
#
# if (isResDir == FALSE & isInitResDir == TRUE) {
#   sprintf("[복사] 기본 설정 : cp %s %s", globalVar$initResPath, globalVar$resPath)
#
#   dir.create(globalVar$resPath, recursive = TRUE)
#   file.copy(globalVar$initResPath, globalVar$resPath, recursive = TRUE)
# }

# 디렉터리 생성
for (i in 1:length(globalVar)) {
  key = names(globalVar)[i]
  val = globalVar[[key]]

  isKey = stringr::str_detect(key, stringr::regex("Path"))
  if (isKey == FALSE) next

  isDir = dir.exists(path = val)
  if (isDir == TRUE) next

  isFile = file.exists(path = val)
  if (isFile == TRUE) next

  cat(sprintf("[CHECK] %s : %s", key, val), "\n")
  dir.create(val, recursive = TRUE)
}

# Rtools
# writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
# Sys.which("make")

#=====================================
# System Info
#=====================================
# .Platform
# R.version
# Sys.info()
# sessionInfo()
# RStudio.Version()

#=====================================
# Set Library
#=====================================
# library(ggmap)
# library(showtext)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)

#=====================================
# Set Fun
#=====================================
saveLogFile = sprintf("%s/%s_%s_%s_%s.log", globalVar$logPath, Sys.info()["sysname"], Sys.info()["nodename"], prjName, format(Sys.time(), "%Y%m%d"))

log = log4r::create.logger()
log4r::logfile(log) = saveLogFile
log4r::level(log) = "INFO"

tryCatch(

  expr = {
    log4r::info(log, sprintf("%s", "[START] Main R"))
  }

  , warning = function(warning) {
    log4r::warn(log, warning)
  }

  , error = function(error) {
    log4r::error(log, error)
  }

  , finally = {
    log4r::info(log, sprintf("%s", "[END] Main R"))
  }
)

perfEval = function(x, y) {

  if (length(x) < 1) { return(sprintf("%s", "x 값 없음")) }
  if (length(y) < 1) { return(sprintf("%s", "y 값 없음")) }

  slope = coef(lm(y ~ x))[2]
  interp = coef(lm(y ~ x))[1]
  xMean = mean(x, na.rm = TRUE)
  yMean = mean(y, na.rm = TRUE)
  xSd = sd(x, na.rm = TRUE)
  ySd = sd(y, na.rm = TRUE)
  cnt = length(x)
  bias = mean(x - y, na.rm = TRUE)
  rBias = (bias / yMean) * 100.0
  rmse = sqrt(mean((x - y)^2, na.rm = TRUE))
  rRmse = (rmse / yMean) * 100.0
  r = cor.test(x, y)$estimate
  p = cor.test(x, y)$p.value
  diffMean = mean(x - y, na.rm = TRUE)
  diffSd = sd(x - y, na.rm = TRUE)
  # perDiffMean = mean((x - y) / y, na.rm = TRUE) * 100.0

  return(c(slope, interp, xMean, yMean, xSd, ySd, cnt, bias, rBias, rmse, rRmse, r, p, diffMean, diffSd))
}

biasCorr = function(actu, pred, minVal, maxVal, interVal, isPlot = FALSE) {

  factorVal = seq(minVal, maxVal, by = interVal)

  # RMSE Fitting
  liResult = lapply(1:length(factorVal), function(i) Metrics::rmse(actu, pred * factorVal[i])) %>%
    unlist()

  ind = which(liResult == min(liResult, na.rm = TRUE))

  if (isPlot == TRUE) {
    plot(liResult)
  }

  # Best Factor Index
  ind = which(liResult == min(liResult, na.rm = TRUE))

  calibFactor = factorVal[[ind]]
  calPred = calibFactor * pred

  meanDiff = mean(actu, na.rm = TRUE) - mean(calPred, na.rm = TRUE)
  newPred = (calPred) + meanDiff

  cat(
    sprintf("%s : %.2f", "[보정 X] RMSE", Metrics::rmse(actu, pred))
    , sprintf("%s : %.2f", "[보정 O] RMSE", Metrics::rmse(actu, newPred))
    , "\n"
  )

  return(c(newPred))
}

fnHeatIndex = function(temp, rh) {

  temp = (temp * 1.8) + 32

  alpha = 61 + ((temp - 68) * 1.2) + (rh * 0.094)
  hi = 0.5 * (temp + alpha)

  if (hi > 79) {
    hi = -42.379 + 2.04901523 * temp + 10.14333127 * rh -
      0.22475541 * temp * rh -
      0.00683783 * (temp^2) -
      0.05481717 * (rh^2) +
      0.00122874 * (temp^2) * rh +
      0.00085282 * temp * (rh^2) - 0.00000199 * (temp^2) * (rh^2)
    if (rh <= 13 && temp >= 80 && temp <= 112) {
      adjustment1 = (13 - rh) / 4
      adjustment2 = sqrt((17 - abs(temp - 95)) / 17)
      total.adjustment = adjustment1 * adjustment2
      hi = hi - total.adjustment
    } else if (rh > 85 && temp >= 80 && temp <= 87) {
      adjustment1 = (rh - 85) / 10
      adjustment2 = (87 - temp) / 5
      total.adjustment = adjustment1 * adjustment2
      hi = hi + total.adjustment
    }
  }

  heatIndex = (hi - 32) / 1.8

  return(heatIndex)
}


fnHumidIndex = function(temp, rh) {

  vp = rh / 100 *
    6.105 *
    exp(17.27 * temp / (237.7 + temp))
  humidIndex = temp + 0.5555 * (vp - 10)

  return(humidIndex)
}

fnAppTempIndex = function(temp, rh, ws) {

  vp = rh / 100 *
    6.105 *
    exp(17.27 * temp / (237.7 + temp))
  appTempIndex = temp + 0.33 * vp - 0.7 * ws + 4.0

  return(appTempIndex)
}

fnAppTempRadIndex = function(temp, rh, ws, sr) {

  sr = sr * 86400 / (10^6)
  alb = 0.2
  vp = rh / 100 *
    6.105 *
    exp(17.27 * temp / (237.7 + temp))
  appTempRadIndex = temp + 0.33 * vp - 0.7 * ws + 0.7 * (sr * (1 - alb))

  return(appTempRadIndex)
}

fnWetBulbGolbalTempIndex = function(temp, rh, ws, sr) {

  sr = sr * (10^3) / 86400
  wetBulbGolbalTempIndex = 0.735 * temp +
    0.0374 * rh +
    0.00292 * temp * rh +
    7.619 * sr -
    4.557 * (sr^2) -
    0.0572 * ws -
    4.064

  return(wetBulbGolbalTempIndex)
}

getUrlTagHref = function(url, tag) {
  xml2::read_html(url) %>%
    rvest::html_nodes(tag) %>%
    rvest::html_attr("href")
}

#=====================================
# Set Data
#=====================================
options(digits = globalVar$optDig)
options(java.parameters = "-Xmx8192m")
# memory.limit(size = globalVar$memLimit)

library(ggmap)
ggmap::register_google(key = globalVar$googleKey)

#=====================================
# Set Font
#=====================================
# 원도우에서 설치된 폰트 확인
# sysfonts::font_files() %>%
#   tibble::as.tibble() %>%
#   dplyr::filter(
#     stringr::str_detect(family, regex("KoPub|Century|Palatino"))
#     , stringr::str_detect(face, regex("Regular|Medium"))
#   )

# ******************************
# 인코딩으로 인해 오류 발생
# ******************************
# # 폰트 추가
# extrafont::font_import(paths = globalVar$fontPath, pattern = "NewCenturySchoolbook.ttf", prompt = FALSE)
# extrafont::font_import(paths = globalVar$fontPath, pattern = "pala.ttf", prompt = FALSE)

# # 폰트 확인
# extrafont::fonts()

# ******************************
# 인코딩으로 인해 오류 발생
# ******************************
# 인터넷 환경에서 구글 폰트 추가
# font.add.google("Gochi Hand", "gochi")

# 오프라인 환경에서 특정 경로에서 국/영문 폰트 추가

# 영문 폰트
# sysfonts::font.add(family = "New Century Schoolbook", regular = file.path(globalVar$fontPath, "NewCenturySchoolbook.ttf"))
# sysfonts::font.add(family = "Palatino Linotype", regular = file.path(globalVar$fontPath, "pala.ttf"))
#
# # 국문 폰트
# sysfonts::font.add(family = "KoPubWorld Dotum Medium", regular = file.path(globalVar$fontPath, "KoPubWorld Dotum Medium.ttf"))
#
# # 폰트 읽기
# showtext::showtext_opts(dpi = 600)
#
# if (Sys.info()["sysname"] == "Windows") {
#  showtext::showtext.auto()
# }
#
# # 폰트 확인
# sysfonts::font_families()

font = "New Century Schoolbook"
fontKor = "KoPubWorld Dotum Medium"
fontEng = "Palatino Linotype"

# font, colorbar
# font = "New Century Schoolbook"
# font = "Palatino Linotype"
# font = "Time Roman"
# font = "Comic Sans MS"
# font = "Helvetica"
# font = "NanumBarunGothic"
# font = "Times New Roman"

cbSpectral = rev(RColorBrewer::brewer.pal(11, "Spectral"))
cbViridis = viridis::viridis(11)
cbMatlab = colorRamps::matlab.like(11)
cbMatlab2 = colorRamps::matlab.like2(11)
cbDiverge = colorspace::diverge_hcl(11)
cbPlasma = rev(viridis::plasma(11))

# 패키지 업데이트
# update.packages(ask = FALSE)

# 주석 단계
# ====
# ****
# ++++

#===============================
# R에서 Anaconda3 불러오기
#===============================
# library(reticulate)
# 
# # 환경변수 설정
# if (.Platform$OS.type == "windows") {
#   Sys.setenv(RETICULATE_PYTHON = 'C:/ProgramData/Anaconda3/python.exe')
#   Sys.setenv(PATH = paste("C:/ProgramData/Anaconda3/Library/bin", Sys.getenv()["PATH"], sep = ";"))
# }

# reticulate::py_discover_config()
# 
# reticulate::conda_list()
# name                                 python
# 1 Anaconda3 C:\\ProgramData\\Anaconda3\\python.exe

# 임시 conda 삭제
# reticulate::conda_remove("PyCharm")

# reticulate::py_config()
# python:         C:/ProgramData/Anaconda3/python.exe
# libpython:      C:/ProgramData/Anaconda3/python37.dll
# pythonhome:     C:/ProgramData/Anaconda3
# version:        3.7.8 | packaged by conda-forge | (default, Jul 31 2020, 01:53:57) [MSC v.1916 64 bit (AMD64)]
# Architecture:   64bit
# numpy:          C:/ProgramData/Anaconda3/Lib/site-packages/numpy
# numpy_version:  1.18.5

# reticulate::use_python("C:\\ProgramData\\Anaconda3\\python.exe", required = TRUE)

# 라이브러리 읽기
# from pykospacing import spacing
# pykospacing = reticulate::import("pykospacing")

# pykospacing$spacing(stringr::str_remove_all("친애하는 지도자동지께서 주체의 사회주의경제관리리론 전반을  관통하고있는 기본원리를 새롭 게 정식 화 히 심 으 로 써 주체 의 사회주의경제 관리 리론이 의거하고있는 사상리론적 , 방법론적  기초가 뚜렷이 밝혀지게 되였으며 이 기본원리에 의거하여 사회주의경제관리리론을  더욱 과학적으로 체계 화할 수 있 게 되 였 다", " "))

#===============================
# R에서 H2O 업데이트
#===============================
# if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
# if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
# 
# pkgs = c("RCurl","jsonlite")
# for (pkg in pkgs) {
#   if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
# }
# 
# install.packages("h2o", type="source", repos="https://h2o-release.s3.amazonaws.com/h2o/rel-zipf/1/R")
# 
# library(h2o)
# h2o::h2o.init()

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
# 전에 드린 mod값을 obs에 일치시키는 랜덤포레스트와 svm을 구축하는것입니다!
# 구축하는데 있어서 랜덤포레스트는 매개변수 최적값을 산정해주실수 있는지 궁금합니다!
# 앙상블 형태로 7 3으로 분할부탁드립니다!
# 검증 지수 : 상관계수 (R), 유의수준 (p-Value), 편이 (Bias), 평균제곱근오차 (RMSE), %Bias, %RMSE로 하면 될까요?

# serviceName = "LSH0000"

# log = log4r::create.logger()
# log4r::logfile(log) = paste0(globalVar$logPath, "/log4r_", format(Sys.time(), "%Y%m%d"), ".log")
# log4r::level(log) = "INFO"

# tryCatch(
#   expr = {
#     # 주 소스 코드
#     log4r::info(log, sprintf("%s", "[START] Main R"))
#     
#   }
#   , warning = function(warning) { log4r::warn(log, warning) }
#   , error = function(error) { log4r::error(log, error) }
#   , finally = {
#     log4r::info(log, sprintf("%s", "[END] Main R"))
#   }
# )


# try(
#   expr = {
#     urlDtlInfo = urlDtlList[j,]
#     
#     tmpData = tibble::tibble(
#       "번호" = urlDtlInfo$num
#       , "본문" = getXpathText(urlDtlInfo$value)
#       , "URL" = urlDtlInfo$value
#     )
#     
#     urlDtlData = dplyr::bind_rows(urlDtlData, tmpData)
#     
#     # 시간 지연 설정
#     Sys.sleep(0.5)
#   }
#   , silent = TRUE
# )

# library(tidyverse)
# library(ggplot2)
# library(lubridate)
# library(openxlsx)
# library(fs)

#================================================
# Set Env
#================================================
# globalVar = new.env()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."
# globalVar$logPath = "."

#================================================
# Set Fun
#================================================
# perfEval = function(x, y) {
#   
#   if (length(x) < 1) { return( sprintf("%s", "x 값 없음") ) }
#   if (length(y) < 1) { return( sprintf("%s", "y 값 없음") ) }
#   
#   slope = coef(lm(y ~ x))[2]
#   interp = coef(lm(y ~ x))[1]
#   xMean = mean(x, na.rm = TRUE)
#   yMean = mean(y, na.rm = TRUE)
#   xSd = sd(x, na.rm = TRUE)
#   ySd = sd(y, na.rm = TRUE)
#   cnt = length(x)
#   bias = mean(x - y, na.rm = TRUE)
#   rBias = (bias / yMean) * 100.0
#   rmse = sqrt(mean((x - y)^2, na.rm = TRUE))
#   rRmse = (rmse / yMean) * 100.0
#   r = cor.test(x, y)$estimate
#   p = cor.test(x, y)$p.value
#   diffMean = mean(x - y, na.rm = TRUE)
#   diffSd = sd(x - y, na.rm = TRUE)
#   # perDiffMean = mean((x - y) / y, na.rm = TRUE) * 100.0
#   
#   return( c(slope, interp, xMean, yMean, xSd, ySd, cnt, bias, rBias, rmse, rRmse, r, p, diffMean, diffSd) )
# }

# 검증 지수 테이블 생성
# perfTable = data.frame(matrix(0, nrow = 2, ncol = 15))
# rownames(perfTable) = c("RF", "SVM")
# colnames(perfTable) = c("slope", "interp", "xMean", "yMean", "xSd", "ySd", "cnt", "bias", "rBias", "rmse", "rRmse", "r", "r2", "diffMean", "diffSd")

# perfTable[1, ] = round(perfEval(yHat, yObs), 2)

# ************************************************
# [openxlsx] Read
# ************************************************
# data = openxlsx::read.xlsx(fileList, sheet = 1)

# ************************************************
# [openxlsx] Write
# ************************************************
# wb = openxlsx::createWorkbook()
# 
# openxlsx::addWorksheet(wb, "ggData")
# openxlsx::writeData(wb, "ggData", ggData, startRow = 1, startCol = 1)

# openxlsx::saveWorkbook(wb, file = paste0(globalVar$outPath, "/Survery_LSH0078.xlsx"), overwrite = TRUE)

# ************************************************
# File Info
# ************************************************
# fileInfo = Sys.glob(paste(globalVar$inpPath, "play.csv", sep = "/"))
# data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "UTF-8"))

# nameList = sort(unique(geoData_L1$sigungu_name))
# fileName = tools::file_path_sans_ext(fs::path_file(fileInfo))
# if (nrow(dataL2) < 1) { next }


# saveImg = sprintf("%s/%s_%02d_%s.png", globalVar$figPath, serviceName, 3, "관계 시각화")
# saveImg = sprintf("%s/TMP3/Img_%s_%05d_%s_%s.png", globalVar$figPath, serviceName, 3, "충청남도 시군구별 자원 분포도", nameInfo)

# isDir = dir.exists(path = fs::path_dir(saveFile))
# if (isDir == FALSE) { file.create(fs::path_dir(saveFile)) }
# isFile = file.exists(path = saveFile)
# if (isFile == TRUE) { file.remove(path = saveFile) }

# ************************************************
# Data Info
# ************************************************
# dplyr::mutate(
#   backColor = dplyr::case_when(
#     stringr::str_detect(sigungu_name, regex("태안군|서산시|당진시")) ~ "1"
#     , TRUE ~ "NA"
#   )
# )

# ************************************************
# cat sprintf
# ************************************************
# cat(sprintf(
#   "dtDate : %10s | code : %5s"
#   , dtDateList[i]
#   , codeList[j, 'emdCd']
# ), "\n")

# cat(sprintf("nYear : %s / nMonth : %s", nYear, nMonth), "\n")


# ************************************************
# 데이터 병합
# ************************************************
# fileList = Sys.glob(file.path(globalVar$outPath, "LSH0132_obs-to-idw_*.csv"))
# 
# dataL4 = fileList %>%
#   purrr::map(read.csv) %>%
#   purrr::reduce(dplyr::bind_rows)
# 
# saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "obs-to-idw")
# readr::write_csv(x = dataL4, file = saveFile)


# **************************************************
# 데이터 좌측 조인
# # ************************************************
# dataL2 = data %>%
#   dplyr::left_join(dataL1, by = c("type" = "type", "Var1" = "Var1")) 


# **************************************************
# 자료 저장
# **************************************************
# saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "수목 및 기상관측소 데이터")
# saveTmp = tempfile(fileext = "csv")
# 
# readr::write_excel_csv(x = dataL2, file = saveTmp)
# fs::file_copy(saveTmp, saveFile, overwrite = TRUE)

# **************************************************
# 8 비트 계산
# **************************************************
# library(compositions)
# bit = compositions::binary(5, mb=8)
# subBit = stringr::str_sub(bit, 1, 1)

#=====================================
# 파일/변수 검사
#=====================================
# if (is.null(jsonFile) || length(jsonFile) < 1) next

#=====================================
# 디렉터리 생성
#=====================================
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)