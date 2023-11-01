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
# R을 이용한 대한민국 기상청 레이더 자료처리 및 다양한 자료 저장

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0485"

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
library(devtools)
library(sp)
library(remotes)
library(BiocManager)
library(rhdf5)
library(sf)
# library(rgdal)
# devtools::install_github("adokter/bioRad")
library(bioRad)

# 최신 r 4.3.2, rstudio 2023.09.1, rtools43 필요
# Sys.setenv(PATH = paste("c:/rtools43/usr/bin", Sys.getenv("PATH"), sep=";"))
# install.packages('https://cran.r-project.org/src/contrib/Archive/rgdal/rgdal_1.6-7.tar.gz',repos=NULL,type="source")
library(rgdal)
library(terra)

# 파일 검색
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "GDK_230209-10/RDR_GDK_FQC_*.uf"))

fileInfo = fileList[1]
for (fileInfo in fileList) {
  cat(sprintf("[CHECK] fileInfo : %s", fileInfo), "\n")
  
  # 파일 읽기
  data = bioRad::read_pvolfile(fileInfo)
  
  # 관측 시간
  dtYmdHm = data$datetime %>% format("%Y%m%d%H%M")
  
  # i = 2
  scanList = data$scans
  for (i in 1:length(scanList)) {
    
    scanInfo = bioRad::get_scan(data, as.numeric(i))
    
    paramsList = scanInfo$params %>% names()
    for (params in paramsList) {

      mainTitle = sprintf("%s_%s_%s", "RDR_GDK_FQC", params, dtYmdHm)
      saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
      dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
      
      makePlot = plot(scanInfo, param = params) +
        labs(subtitle = mainTitle) +
        theme(text = element_text(size = 16))
      
      ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
      cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
    }
    
    # 레이더 스캔을 PPI 형식으로 변환
    ppiInfo = bioRad::project_as_ppi(scanInfo)
    
    # PPI 데이터 가져오기
    ppiData = ppiInfo$data %>% 
      as.data.frame() %>% 
      as.tibble()
    
    mainTitle = sprintf("%s_%s-%s_%s", "RDR_GDK_FQC", "MAP", "DBZH", dtYmdHm)
    saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
    dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
    
    map(ppiInfo, map = "osm", param = "DBZH") +
      labs(subtitle = mainTitle) +
      theme(text = element_text(size = 16))
    
    ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
    cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
    
    # 도플라 비율
    drInfo = calculate_param(ppiInfo, DR = 10 * log10((1+ ZDR - 2 * (ZDR^0.5) * RHOHV) / (1 + ZDR+ 2 * (ZDR^0.5) * RHOHV)))
    
    mainTitle = sprintf("%s_%s-%s_%s", "RDR_GDK_FQC", "MAP", "DR", dtYmdHm)
    saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
    dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
    
    map(drInfo, map = "osm", param = "DR", zlim=c(-25,-5), palette = viridis::viridis(100)) +
      labs(subtitle = mainTitle) +
      theme(text = element_text(size = 16))
    
    ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
    cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
  }
  
  # 연직 분포
  # vpObj = bioRad::calculate_vp(data)
  # plot(vpObj)
  # vpData = vpObj$data
  # 
  # 
  # bindVpts = bioRad::bind_into_vpts(vpObj)
  # plot(bindVpts)
}
