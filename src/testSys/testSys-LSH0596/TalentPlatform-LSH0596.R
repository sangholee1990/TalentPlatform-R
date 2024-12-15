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
# R을 이용한 코딩페스티벌 보고서 제작 (일반음식점 데이터 기반으로 다양한 시각화)

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0596"

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

showtext::showtext_opts(dpi = 600)
showtext::showtext.auto()

# 파일 검색
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "07_24_04_P.csv"))

# 파일 읽기
data = readr::read_csv(fileInfo, locale = readr::locale(encoding = "EUC-KR"))

# 파일 전처리
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
# 2023년 전국 업종별 일반음식점 사업체 막대그래프
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

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# =======================================================================================================
# 2023년 서울 업종별 일반음식점 사업체 원형그래프
# =======================================================================================================
# sidoAddrInfo = sidoAddrList[1]
# sidoAddrInfo = "대전"
sidoAddrList = dataL1$sidoAddr %>% unique() %>% sort()
for (sidoAddrInfo in sidoAddrList) {
  
  if (sidoAddrInfo != "서울") next
  
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
  
  if (nrow(dataL2) < 1) next
  
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

# shell.exec(saveImg)

# =======================================================================================================
# 2013~2023년 한식 (서울) 개폐업 현황
# =======================================================================================================
typeList = c("한식")
sidoList = c("서울")

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
    
    if (nrow(dataL2) < 1) next
    
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


# =======================================================================================================
# 2013~2023년 한식 (서울) 폐업기간 분류
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
