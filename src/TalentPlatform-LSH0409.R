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

# ================================================
# 비즈니스 로직 수행
# ================================================
# 라이브러리 읽기
library(ggmap)
library(tidyverse)
library(sf)
library(sp)

# 검색어
addrName = "강원도"
addrDtlName = "춘천시"

# ****************************************************
# 법정동 코드 읽기
# ****************************************************
codeInfo = Sys.glob(file.path(globalVar$mapPath, "admCode/admCode.xlsx"))
codeData = openxlsx::read.xlsx(codeInfo, sheet = 1, startRow = 2)

codeDataL1 = codeData %>%
  dplyr::filter(
    grepl(addrName, 시도명칭)
  )

codeDataL2 = codeData %>%
  dplyr::filter(
    grepl(addrName, 시도명칭)
    , grepl(addrDtlName, 시군구명칭)
  )


# ****************************************************
# 도 지도 읽기
# ****************************************************
ctpMapInfo = Sys.glob(file.path(globalVar$mapPath, "koreaInfo/TL_SCCO_CTPRVN.shp")) %>%
  sf::st_read(quiet = TRUE, options = "ENCODING=EUC-KR") %>%
  sf::st_transform(sp::CRS("+proj=longlat"))

# makePlot = ggplot(data = ctpMapInfo, aes(x = NULL, y = NULL, fill = NULL, z = NULL, label = CTP_KOR_NM)) +
#   theme_bw() +
#   coord_fixed(ratio = 1) +
#   labs(x = NULL, y = NULL) +
#   geom_sf(color = "black", fill = NA) +
#   geom_sf_text(color = "black", size = 3)
#
# mainTitle = sprintf("%s", "ctpMapInfo")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

makePlot = ggplot(data = ctpMapInfo, aes(x = NULL, y = NULL, fill = NULL, z = NULL, label = CTP_KOR_NM)) +
  theme_bw() +
  coord_fixed(ratio = 1) +
  labs(x = NULL, y = NULL) +
  geom_sf(color = "black", fill = NA) +
  ggspatial::annotation_scale(location = "br", bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", width_hint = 0.2) +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"), style = ggspatial::north_arrow_nautical(fill = c("grey40", "white"), line_col = "grey20", text_family = "ArcherPro Book"))
  # geom_sf_text(color = "black", size = 3)

mainTitle = sprintf("%s", "ctpMapInfo-noLabel")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ****************************************************
# 시군구 지도 읽기
# ****************************************************
sigMapInfo = Sys.glob(file.path(globalVar$mapPath, "koreaInfo/TL_SCCO_SIG.shp")) %>%
  sf::st_read(quiet = TRUE, options = "ENCODING=EUC-KR") %>%
  sf::st_transform(sp::CRS("+proj=longlat"))

# makePlot = ggplot(data = sigMapInfo, aes(x = NULL, y = NULL, fill = NULL, z = NULL, label = SIG_KOR_NM)) +
#   theme_bw() +
#   coord_fixed(ratio = 1) +
#   labs(x = NULL, y = NULL) +
#   geom_sf(color = "black", fill = NA) +
#   geom_sf_text(color = "black", size = 2)
#
# mainTitle = sprintf("%s", "sigMapInfo")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

makePlot = ggplot(data = sigMapInfo, aes(x = NULL, y = NULL, fill = NULL, z = NULL, label = SIG_KOR_NM)) +
  theme_bw() +
  coord_fixed(ratio = 1) +
  labs(x = NULL, y = NULL) +
  geom_sf(color = "black", fill = NA) +
  ggspatial::annotation_scale(location = "br", bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", width_hint = 0.2) +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"), style = ggspatial::north_arrow_nautical(fill = c("grey40", "white"), line_col = "grey20", text_family = "ArcherPro Book"))
# geom_sf_text(color = "black", size = 2)

mainTitle = sprintf("%s", "sigMapInfo-noLabel")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ****************************************************
# 읍면동 지도 읽기
# ****************************************************
dongMapInfo = Sys.glob(file.path(globalVar$mapPath, "koreaInfo/bnd_dong_00_2019_2019_2Q.shp")) %>%
  sf::st_read(quiet = TRUE, options = "ENCODING=EUC-KR") %>%
  sf::st_transform(sp::CRS("+proj=longlat"))

dongMapInfoL1 = dongMapInfo %>%
  dplyr::inner_join(codeDataL1, by = c("adm_dr_cd" = "읍면동코드"))

dongMapInfoL2 = dongMapInfo %>%
  dplyr::inner_join(codeDataL2, by = c("adm_dr_cd" = "읍면동코드"))

makePlot = ggplot(data = dongMapInfo, aes(x = NULL, y = NULL, fill = NULL, z = NULL, label = adm_dr_nm)) +
  theme_bw() +
  coord_fixed(ratio = 1) +
  labs(x = NULL, y = NULL) +
  geom_sf(color = "black", fill = NA) +
  ggspatial::annotation_scale(location = "br", bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", width_hint = 0.2) +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"), style = ggspatial::north_arrow_nautical(fill = c("grey40", "white"), line_col = "grey20", text_family = "ArcherPro Book"))
# geom_sf_text(color = "black", size = 5)

mainTitle = sprintf("%s", "dongMapInfo-noLabel")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# makePlot = ggplot(data = dongMapInfoL1, aes(x = NULL, y = NULL, fill = NULL, z = NULL, label = adm_dr_nm)) +
#   theme_bw() +
#   coord_fixed(ratio = 1) +
#   labs(x = NULL, y = NULL) +
#   geom_sf(color = "black", fill = NA) +
#   geom_sf_text(color = "black", size = 2)
#
# mainTitle = sprintf("%s", "dongDtlMapInfo")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

makePlot = ggplot(data = dongMapInfoL1, aes(x = NULL, y = NULL, fill = NULL, z = NULL, label = adm_dr_nm)) +
  theme_bw() +
  coord_fixed(ratio = 1) +
  labs(x = NULL, y = NULL) +
  geom_sf(color = "black", fill = NA) +
  ggspatial::annotation_scale(location = "br", bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", width_hint = 0.2) +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"), style = ggspatial::north_arrow_nautical(fill = c("grey40", "white"), line_col = "grey20", text_family = "ArcherPro Book"))
# geom_sf_text(color = "black", size = 5)

mainTitle = sprintf("%s", "dongDtlMapInfo-noLabel")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# makePlot = ggplot(data = dongMapInfoL2, aes(x = NULL, y = NULL, fill = NULL, z = NULL, label = adm_dr_nm)) +
#   theme_bw() +
#   coord_fixed(ratio = 1) +
#   labs(x = NULL, y = NULL) +
#   geom_sf(color = "black", fill = NA) +
#   geom_sf_text(color = "black", size = 4)
#
# mainTitle = sprintf("%s", "dongDtl2MapInfo")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

makePlot = ggplot(data = dongMapInfoL2, aes(x = NULL, y = NULL, fill = NULL, z = NULL, label = adm_dr_nm)) +
  theme_bw() +
  coord_fixed(ratio = 1) +
  labs(x = NULL, y = NULL) +
  geom_sf(color = "black", fill = NA) +
  ggspatial::annotation_scale(location = "br", bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", width_hint = 0.2) +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"), style = ggspatial::north_arrow_nautical(fill = c("grey40", "white"), line_col = "grey20", text_family = "ArcherPro Book"))
  # geom_sf_text(color = "black", size = 4)

mainTitle = sprintf("%s", "dongDtl2MapInfo-noLabel")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# ================================================
# 20230314 특정 지역 선정
# ================================================
# # 강원도 춘천 영역
# posLon = 127.7357
# posLat = 37.90262
# posInv = 0.075
#
# roi = c(left = posLon - posInv, right = posLon + posInv, bottom = posLat - posInv, top = posLat + posInv)
#
# zoomList = 0:21
# maptypeList = c("terrain", "terrainbackground", "satellite", "roadmap", "hybrid", "terrain", "watercolor", "toner")
#
# for (maptypeInfo in maptypeList) {
#   for (zoomInfo in zoomList) {
#     cat(sprintf("[CHECK] %s %s", maptypeInfo, zoomInfo), "\n")
#
#     try(
#       expr = {
#         map = ggmap::get_map(
#           location = c(lon = posLon, lat = posLat)
#           , zoom = zoomInfo
#           , maptype = maptypeInfo
#         )
#
#         makePlot = ggmap(map, extent = "device") +
#           geom_rect(aes(xmin = roi[1], xmax = roi[2], ymin = roi[3], ymax = roi[4]), fill = "transparent", color = "red")
#
#         mainTitle = sprintf("%s-%s_%s_%s", posLon, posLat, maptypeInfo, zoomInfo)
#         saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
#         dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#         ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
#         cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#       }
#       , silent = TRUE
#     )
#   }
# }
