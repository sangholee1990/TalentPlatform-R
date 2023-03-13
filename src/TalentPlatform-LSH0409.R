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
# R을 이용한 맵 종류 및 확대 레벨에 따른 연구 도메인 시각화

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0409"

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
  # source(here::here(file.path(contextPath, "src"), "InitConfig.R"), encoding = "UTF-8")
  source(file.path(contextPath, "src", "InitConfig.R"))
}

#================================================
# 비즈니스 로직 수행
#================================================
# 라이브러리 읽기
library(ggmap)
library(tidyverse)
# ggmap::register_google(key = "")

# 강원도 춘천 영역
posLon = 127.7357
posLat = 37.90262
posInv = 0.075

roi = c(left = posLon - posInv, right = posLon + posInv, bottom = posLat - posInv, top = posLat + posInv)

zoomList = 0:21
maptypeList = c("terrain", "terrainbackground", "satellite", "roadmap", "hybrid", "terrain", "watercolor", "toner")

for (maptypeInfo in maptypeList) {
  for (zoomInfo in zoomList) {
    cat(sprintf("[CHECK] %s %s", maptypeInfo, zoomInfo), "\n")

    try(
      expr = {
        map = ggmap::get_map(
          location = c(lon = posLon, lat = posLat)
          , zoom = zoomInfo
          , maptype = maptypeInfo
        )

        makePlot = ggmap(map, extent = "device") +
          geom_rect(aes(xmin = roi[1], xmax = roi[2], ymin = roi[3], ymax = roi[4]), fill = "transparent", color = "red")

        mainTitle = sprintf("%s-%s_%s_%s", posLon, posLat, maptypeInfo, zoomInfo)
        saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
        dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
        ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
        cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
      }
      , silent = TRUE
    )
  }
}
