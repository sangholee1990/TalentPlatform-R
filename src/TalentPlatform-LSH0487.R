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
# R을 이용한 일반음식점 데이터 기반으로 지도 및 막대그래프 시각화

# https://www.localdata.go.kr/devcenter/dataDown.do?menuNo=20001
# 기존 좌표계와 동일한 중부원점TM(EPSG:2097)

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0487"

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
library(ggplot2)
library(openxlsx)
library(lubridate)
library(fs)
library(sf)
library(forcats)
library(showtext)
library(ggplot2)
library(sf)
library(scales)
library(ggrepel)
library(lubridate)

# showtext::showtext_opts(dpi = 100)
showtext::showtext_opts(dpi = 600)
showtext::showtext.auto()

# 대한민국 지리 데이터
mapKor = sf::st_read(file.path(globalVar$mapPath, "gadm36_KOR_shp/gadm36_KOR_1.shp"))

# 파일 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "07_24_04_P.csv"))
data = readr::read_csv(fileInfo, locale = readr::locale(encoding = "UTF-8"))

dataL1 = data %>%
  tidyr::separate(col = "도로명전체주소", into = c("sido"), sep = " ") %>%
  dplyr::mutate(
    dtYear = format(인허가일자, "%Y")
    , sidoAddr = dplyr::case_when(
      stringr::str_detect(sido, regex("부산광역시")) ~ "부산"
      , stringr::str_detect(sido, regex("충청북도")) ~ "충북"
      , stringr::str_detect(sido, regex("충청남도")) ~ "충남"
      , stringr::str_detect(sido, regex("대구광역시")) ~ "대구"
      , stringr::str_detect(sido, regex("대전광역시")) ~ "대전"
      , stringr::str_detect(sido, regex("강원특별자치도")) ~ "강원"
      , stringr::str_detect(sido, regex("광주광역시")) ~ "광주"
      , stringr::str_detect(sido, regex("경기도")) ~ "경기"
      , stringr::str_detect(sido, regex("경상북도")) ~ "경북"
      , stringr::str_detect(sido, regex("경상남도")) ~ "경남"
      , stringr::str_detect(sido, regex("인천광역시")) ~ "인천"
      , stringr::str_detect(sido, regex("제주특별자치도")) ~ "제주"
      , stringr::str_detect(sido, regex("전라북도")) ~ "전북"
      , stringr::str_detect(sido, regex("전라남도")) ~ "전남"
      , stringr::str_detect(sido, regex("세종특별자치시")) ~ "세종"
      , stringr::str_detect(sido, regex("서울특별시")) ~ "서울"
      , stringr::str_detect(sido, regex("울산광역시")) ~ "울산"
    )
  )

# =======================================================================================================
# 1. 전국 업종별 일반음식점 사업체 수(2023년 기준)
# =======================================================================================================
# 비중이 높은 순으로 10개 업종만 막대그래프로 시각화
dataL2 = dataL1 %>%
  dplyr::filter(
    인허가일자 >= as.Date("2023-01-01")
  ) %>%
  dplyr::group_by(업태구분명) %>%
  dplyr::summarise(
    cnt = n()
  ) %>%
  na.omit() %>%
  dplyr::arrange(desc(cnt)) %>%
  dplyr::slice(1:10)

# 정렬
dataL2$업태구분명 = forcats::fct_relevel(dataL2$업태구분명, dataL2$업태구분명)

mainTitle = sprintf("2023년 전국 업종별 일반음식점 사업체 %s", "막대그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL2, aes(x = 업태구분명, y = cnt, fill = 업태구분명)) +
  geom_bar(stat = "identity", width = 1.0, position = position_dodge(width = 1.0), alpha = 0.6) +
  labs(x = "10개 업종", y = "사업체 수", fill = NULL, title = NULL, subtitle = mainTitle) +
  ggrepel::geom_text_repel(aes(label = scales::comma(cnt)), position = position_dodge(width = 1.0), color = "white", vjust = -0.5, size = 4) +
  theme(
    text = element_text(size = 16)
    , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "none"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# 전국업종별 분포를 원형그래프로 시각화
dataL3 = dataL2 %>%
  dplyr::mutate(per = (cnt / sum(dataL2$cnt, na.rm = TRUE)) * 100) %>%
  dplyr::mutate(name = sprintf("%s\n(%s %%)", 업태구분명, round(per, 1)))

mainTitle = sprintf("2023년 전국 업종별 일반음식점 사업체 %s", "원형그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL3, aes(x = '', y = per, fill = 업태구분명)) +
  geom_bar(stat = 'identity', alpha = 0.6) +
  coord_polar('y', start = 0) +
  ggrepel::geom_label_repel(aes(label = name), position = position_stack(vjust = 0.5), color = "white", size = 4, alpha = 0.8) +
  labs(x = NULL, y = NULL, fill = NULL, title = NULL, subtitle = mainTitle) +
  theme_minimal() +
  theme_classic() +
  theme(
    text = element_text(size = 16)
    , axis.line = element_blank()
    , axis.text = element_blank()
    , axis.ticks = element_blank()
    , legend.position = "none"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 총 사업체 수 기준으로 지도 시각화
dataL4 = dataL1 %>%
  dplyr::group_by(sido, sidoAddr) %>%
  dplyr::summarise(
    cnt = n()
  ) %>%
  na.omit() %>%
  dplyr::arrange(desc(cnt))

dataL5 = dataL4 %>%
  dplyr::mutate(
    per = (cnt / sum(dataL4$cnt, na.rm = TRUE)) * 100
  ) %>%
  dplyr::mutate(
    GID_1 = dplyr::case_when(
      stringr::str_detect(sido, regex("부산광역시")) ~ "KOR.1_1"
      , stringr::str_detect(sido, regex("충청북도")) ~ "KOR.2_1"
      , stringr::str_detect(sido, regex("충청남도")) ~ "KOR.3_1"
      , stringr::str_detect(sido, regex("대구광역시")) ~ "KOR.4_1"
      , stringr::str_detect(sido, regex("대전광역시")) ~ "KOR.5_1"
      , stringr::str_detect(sido, regex("강원특별자치도")) ~ "KOR.6_1"
      , stringr::str_detect(sido, regex("광주광역시")) ~ "KOR.7_1"
      , stringr::str_detect(sido, regex("경기도")) ~ "KOR.8_1"
      , stringr::str_detect(sido, regex("경상북도")) ~ "KOR.9_1"
      , stringr::str_detect(sido, regex("경상남도")) ~ "KOR.10_1"
      , stringr::str_detect(sido, regex("인천광역시")) ~ "KOR.11_1"
      , stringr::str_detect(sido, regex("제주특별자치도")) ~ "KOR.12_1"
      , stringr::str_detect(sido, regex("전라북도")) ~ "KOR.13_1"
      , stringr::str_detect(sido, regex("전라남도")) ~ "KOR.14_1"
      , stringr::str_detect(sido, regex("세종특별자치시")) ~ "KOR.15_1"
      , stringr::str_detect(sido, regex("서울특별시")) ~ "KOR.16_1"
      , stringr::str_detect(sido, regex("울산광역시")) ~ "KOR.17_1"
    )
    # , name = sprintf("%s\n%s / %s %%", sidoAddr, scales::comma(cnt), round(per, 1))
    , name = sprintf("%s\n%s\n%s%%", sidoAddr, scales::comma(cnt), round(per, 1))
  )

dataL6 = mapKor %>%
  dplyr::left_join(dataL5, by = c("GID_1" = "GID_1")) %>%
  dplyr::mutate(
    centroids = sf::st_centroid(geometry)
  ) %>%
  dplyr::mutate(
    lon = sf::st_coordinates(centroids)[, 1],
    lat = sf::st_coordinates(centroids)[, 2]
  )

# 지도 시각화
mainTitle = sprintf("2023년 전국 업종별 일반음식점 사업체 %s", "지도")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(data = dataL6) +
  geom_sf(aes(geometry = geometry, fill = cnt, group = sido), alpha = 0.6) +
  ggrepel::geom_label_repel(aes(x = lon, y = lat, label = name), size = 3.5, box.padding = 0.0, label.padding = 0.2, fill = scales::alpha("white", 0.7), arrow = grid::arrow(length = unit(0.0, "inches"))) +
  scale_fill_continuous(trans = "reverse") +
  metR::scale_x_longitude(breaks = seq(125, 131, 1), limits = c(125, 131), expand = c(0, 0)) +
  metR::scale_y_latitude(breaks = seq(33, 39, 1), limits = c(33, 39), expand = c(0, 0)) +
  labs(subtitle = mainTitle, x = NULL, y = NULL, fill = NULL, colour = NULL, title = NULL) +
  theme(legend.position = "none") +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# =======================================================================================================
# 2. 10대 또는 5대 업종의 지역별 분포
# 지역별 비교가 목적이기 때문에, 2023년 기준 17개 시도 지역별 업종의 비중 비교(원형그래프 시각화)
# =======================================================================================================
# sidoAddrInfo = sidoAddrList[1]
# sidoAddrInfo = "대전"
sidoAddrList = dataL1$sidoAddr %>% unique() %>% sort()
for (sidoAddrInfo in sidoAddrList) {
  
  dataL2 = dataL1 %>%
    dplyr::filter(
      인허가일자 >= as.Date("2023-01-01")
    ) %>%
    dplyr::filter(sidoAddr == sidoAddrInfo) %>%
    dplyr::group_by(업태구분명) %>%
    dplyr::summarise(
      cnt = n()
    ) %>%
    na.omit() %>%
    dplyr::arrange(desc(cnt)) %>%
    dplyr::slice(1:10)
  
  # 정렬
  dataL2$업태구분명 = forcats::fct_relevel(dataL2$업태구분명, dataL2$업태구분명)
  
  dataL3 = dataL2 %>%
    dplyr::mutate(per = (cnt / sum(dataL2$cnt, na.rm = TRUE)) * 100) %>%
    dplyr::mutate(name = sprintf("%s\n(%s %%)", 업태구분명, round(per, 1)))
  
  mainTitle = sprintf("2023년 %s 업종별 일반음식점 사업체 %s", sidoAddrInfo, "원형그래프")
  saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
  dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
  
  makePlot = ggplot(dataL3, aes(x = '', y = per, fill = 업태구분명)) +
    geom_bar(stat = 'identity', alpha = 0.6) +
    coord_polar('y', start = 0) +
    ggrepel::geom_label_repel(aes(label = name), max.iter = 1000000, position = position_stack(vjust = 0.5), color = "white", size = 4, alpha = 0.8) +
    labs(x = NULL, y = NULL, fill = NULL, title = NULL, subtitle = mainTitle) +
    theme_minimal() +
    theme_classic() +
    theme(
      text = element_text(size = 16)
      , axis.line = element_blank()
      , axis.text = element_blank()
      , axis.ticks = element_blank()
      , legend.position = "none"
    )
  
  ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
}


# =======================================================================================================
# 3. 시계열자료로는 특정 세부업종으로, 치킨통닭과 김밥(도시락) 냉면집 이렇게 3개가 필요한데요,
# 최근 10년간(2013년~2023년) 개폐업 현황을 막대그래프로 시각화
# 단위는 전국, 서울, 대전, 세종 이렇게 가능할지 검토 부탁드립니다.
# 3번 관련해서는, CSV자료에 영업기간도 도출할 수 있는데,
# 통닭(치킨), 김밥(도시락), 냉면집
# 서울, 대전, 세종, 전국
# =======================================================================================================
typeList = c("통닭(치킨)", "김밥(도시락)", "냉면집")
sidoList = c("서울", "대전", "세종")

# typeInfo = typeList[1]
# sidoInfo = sidoList[1]
for (typeInfo in typeList) {
  for (sidoInfo in sidoList) {
    
    dataL2 = dataL1 %>%
      dplyr::filter(
        업태구분명 == typeInfo
        , sidoAddr == sidoInfo
        , dplyr::between(인허가일자, as.Date("2013-01-01"), as.Date("2023-12-31"))
      ) %>%
      dplyr::mutate(
        dtYear = format(인허가일자, "%Y") %>% as.numeric()
      ) %>%
      dplyr::group_by(dtYear, 영업상태명, 업태구분명) %>%
      dplyr::summarise(
        cnt = n()
      ) %>%
      na.omit() %>%
      dplyr::arrange(dtYear)
    
    mainTitle = sprintf("2013~2023년 %s (%s) 개폐업 현황", typeInfo, sidoInfo)
    saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
    dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
    
    makePlot = ggplot(dataL2, aes(x = dtYear, y = cnt, color = 영업상태명)) +
      geom_line() +
      geom_point() +
      ggrepel::geom_text_repel(aes(label = scales::comma(cnt)), vjust = -1.0, size = 4, show.legend = FALSE) +
      scale_x_continuous(minor_breaks = 2013:2023, breaks = 2013:2023) +
      labs(x = "연도 ", y = "개폐업 수", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
      theme(
        text = element_text(size = 16)
        , legend.position = "top"
      )
    
    ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
    cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
  }
}


for (typeInfo in typeList) {
  dataL2 = dataL1 %>%
    dplyr::filter(
      업태구분명 == typeInfo
      , dplyr::between(인허가일자, as.Date("2013-01-01"), as.Date("2023-12-31"))
    ) %>%
    dplyr::mutate(
      dtYear = format(인허가일자, "%Y") %>% as.numeric()
    ) %>%
    dplyr::group_by(dtYear, 영업상태명, 업태구분명) %>%
    dplyr::summarise(
      cnt = n()
    ) %>%
    na.omit() %>%
    dplyr::arrange(dtYear)
  
  mainTitle = sprintf("2013~2023년 %s (%s) 개폐업 현황", typeInfo, "전국")
  saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
  dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
  
  makePlot = ggplot(dataL2, aes(x = dtYear, y = cnt, color = 영업상태명)) +
    geom_line() +
    geom_point() +
    ggrepel::geom_text_repel(aes(label = scales::comma(cnt)), vjust = -1.0, size = 4, show.legend = FALSE) +
    scale_x_continuous(minor_breaks = 2013:2023, breaks = 2013:2023) +
    labs(x = "연도 ", y = "개폐업 수", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
    theme(
      text = element_text(size = 16)
      , legend.position = "top"
    )
  
  ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
}

# =======================================================================================================
# 혹시 세부업종+특정 지역 결합해서 그 지역의 폐업업종 중 영업기간으로 분류하여 
# 시각화할 수 있을지도 검토 부탁드립니다
# =======================================================================================================
for (typeInfo in typeList) {
  for (sidoInfo in sidoList) {
    
    dataL2 = dataL1 %>%
      dplyr::filter(
        업태구분명 == typeInfo
        # , sidoAddr == sidoInfo
        , 영업상태명 == "폐업"
        , dplyr::between(인허가일자, as.Date("2013-01-01"), as.Date("2023-12-31"))
      ) %>%
      dplyr::mutate(
        dtYear = format(인허가일자, "%Y") %>% as.numeric()
        , diffYear = lubridate::interval(인허가일자, 폐업일자) / lubridate::dyears(1)
        , name = dplyr::case_when(
          diffYear < 1 ~ "1년 미만"
          , diffYear >= 1 & diffYear < 3 ~ "1-3년"
          , diffYear >= 3 & diffYear < 5 ~ "3-5년"
          , diffYear >= 5 & diffYear < 7 ~ "5-7년"
          , diffYear >= 7 & diffYear < 9 ~ "7-9년"
          , TRUE ~ "9년 이상"
        )
      ) %>%
      dplyr::group_by(영업상태명, name) %>%
      dplyr::summarise(
        cnt = n()
      ) %>%
      na.omit() %>%
      dplyr::arrange(name)
    
    mainTitle = sprintf("2013~2023년 %s (%s) 폐업기간 분류", typeInfo, sidoInfo)
    saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
    dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
    
    # 정렬
    dataL2$name = forcats::fct_relevel(dataL2$name, c("1년 미만", "1-3년", "3-5년", "5-7년", "7-9년", "9년 이상"))
    
    makePlot = ggplot(dataL2, aes(x = name, y = cnt, fill = name)) +
      geom_bar(stat = "identity", width = 0.75, position = position_dodge(width = 1.0), alpha = 0.6, show.legend = FALSE) +
      ggrepel::geom_text_repel(aes(label = scales::comma(cnt)), position = position_dodge(width = 1.0), color = "white", vjust = 0.0, size = 5) +
      labs(x = "폐업 기간 ", y = "개폐업 수", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
      theme(
        text = element_text(size = 16)
        , legend.position = "top"
      ) +
      guides(color = guide_legend(nrow = 1))
    
    ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
    cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
  }
}

# ==============================================================
# 2023.10.22
# ==============================================================
# # 파일 읽기
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "07_24_04_P.csv"))
# data = readr::read_csv(fileInfo, locale = readr::locale(encoding = "UTF-8"))
# 
# # 업종별 개폐업현황
# dataL1 = data %>% 
#   dplyr::rename(
#     geoX = "좌표정보(X)"
#     , geoY = "좌표정보(Y)"
#   ) %>% 
#   dplyr::select(영업상태명, 업태구분명, 인허가일자, geoX, geoY) %>% 
#   na.omit()
# 
# # 좌표 변환
# sfData = st_as_sf(dataL1, coords = c("geoX", "geoY"), crs = 2097)
# sfDataL1 = st_transform(sfData, 4326)
# 
# dataL2 = sfDataL1 %>%
#   dplyr::mutate(
#     lon = st_coordinates(.)[,1]
#     , lat = st_coordinates(.)[,2]
#   ) %>%
#   st_set_geometry(NULL) 
# 
# # summary(dataL2)
# 
# # 연도별 일반음식점 (전체) 개폐업 현황
# dataL3 = dataL2 %>% 
#   dplyr::mutate(
#     dtYear = format(인허가일자, "%Y") %>% as.numeric()
#   ) %>% 
#   dplyr::group_by(dtYear, 영업상태명) %>%
#   dplyr::summarise(
#     cnt = n()
#   )
# 
# mainTitle = sprintf("연도별 일반음식점 (%s) 개폐업 현황", "전체")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# 
# ggplot(dataL3, aes(x = dtYear, y = cnt, fill = 영업상태명)) +
#   geom_bar(stat = "identity", width = 1.0, position = position_dodge(width = 1.0)) +
#   labs(x = "연도 ", y = "개수", fill = NULL, title = NULL, subtitle = mainTitle) +
#   theme(
#     text = element_text(size = 16)
#     , legend.position = "top"
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
# 
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# 
# 
# # 연도별 일반음식점 (업태구분명) 개폐업 현황
# typeList = dataL2$업태구분명 %>% unique() %>% sort()
# for (typeInfo in typeList) {
#   
#   dataL4 = dataL2 %>% 
#     dplyr::filter(
#       업태구분명 == typeInfo
#     ) %>% 
#     dplyr::mutate(
#       dtYear = format(인허가일자, "%Y") %>% as.numeric()
#     ) %>% 
#     dplyr::group_by(dtYear, 영업상태명) %>%
#     dplyr::summarise(
#       cnt = n()
#     )
#   
#   mainTitle = sprintf("연도별 일반음식점 (%s) 개폐업 현황", typeInfo)
#   saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
#   dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#   
#   makePlot = ggplot(dataL4, aes(x = dtYear, y = cnt, fill = 영업상태명)) +
#     # geom_bar(stat = "identity", width = 0.5, position=position_dodge(width = 0.5)) +
#     # geom_bar(stat = "identity", position=position_dodge(width = 0.5)) +
#     geom_bar(stat = "identity", width = 1.0, position = position_dodge(width = 1.0)) +
#     labs(x = "연도 ", y = "개수", fill = NULL, title = NULL, subtitle = mainTitle) +
#     theme(
#       text = element_text(size = 16)
#       , legend.position = "top"
#     )
#   
#   ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
#   
#   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# }
# 
# # 지도 시각화
# # 대한민국 지리 데이터
# mapKor = sf::st_read(file.path(globalVar$mapPath, "gadm36_KOR_shp/gadm36_KOR_1.shp"))
# mapKor2 = sf::st_read(file.path(globalVar$mapPath, "gadm36_KOR_shp/gadm36_KOR_2.shp"))
# 
# typeList = dataL2$영업상태명 %>% unique() %>% sort()
# for (typeInfo in typeList) {
#   
#   dataL5 = dataL2 %>%
#     dplyr::filter(영업상태명 == typeInfo) %>%
#     dplyr::select(lon, lat)
#   
#   mainTitle = sprintf("일반음식점 %s 지도", typeInfo)
#   saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
#   dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#   
#   makePlot = ggplot() +
#     geom_sf(data = mapKor2, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = "white") +
#     geom_sf(data = mapKor, aes(x = NULL, y = NULL, fill = NULL, z = NULL), lwd = 0.5, color = "black", fill = NA) +
#     # geom_point(data = dataL5, aes(x = lon, y = lat), alpha = 0.1) +
#     stat_density_2d(data = dataL5, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = "polygon") +
#     scale_fill_gradient(low = "green", high = "red") +
#     scale_alpha(range = c(0, 1.0), guide = FALSE) +
#     metR::scale_x_longitude(breaks = seq(125, 131, 1), limits = c(125, 131), expand = c(0, 0)) +
#     metR::scale_y_latitude(breaks = seq(33, 39, 1), limits = c(33, 39), expand = c(0, 0)) +
#     labs(subtitle = mainTitle, x = NULL, y = NULL, fill = NULL, colour = NULL, title = NULL)
#   
#   ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
#   
#   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# }