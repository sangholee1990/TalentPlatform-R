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
# 학사 학위논문 (코로나19 전후에 따른 서울시 자치구 대기질 농도 (PM10, PM2.5) 비교 분석)

# 연구배경: 최근 서울시 대기 청정도는 인근 국가로부터 발생한 고농도의 장거리 수송 미세먼지와 더불어 자치구 내의 미세먼지로 인해 악화되고 (김관철 등, 2016) 전 세계적으로 관심이 집중됨 (박애경 등, 2011)
# 특히 코로나19 팬데믹로 인하여 교통체증, 공장 가동 등 요소가 서울시의 대기질 농도 (PM2.5, PM10)에 대한 객관적 분석자료 필요

# 연구목적: 서울시 25개의 자치구를 대상으로 코로나19 팬데믹 전후에 따른 대기질 농도 (PM2.5, PM10)에 전후 비교분석

# 서울시 미세먼지(PM10) 및 초미세먼지 관련 논문 조사
# 관련 연구사례를 통해 다각도 비교 분석을 통해 연구 방법 제시

# 2018~2022년에 대한 서울시 자치구 대기질(PM2.5, PM10)를 이용하여 코로나 전후 농도에 미치는 영향요인 비교 분석
# 인구밀도가 가장 높은 서울특별시 서초구와 강남구에 PM2.5, PM10 농도가 가장 짙을 것으로 예상하며, 높은 교통체증 때문일 것으로 판단 (오장욱과 임태진, 2019)

# 코로나 시대의 서울시 자치구의 대기질 농도에 관한 영향평가에 대한 시사점 도출
# 그러나 서울시 특정 25개 자치구를 국한하였기 때문에 향후 전국적인 대기질 자료를 통해 시공간 한계를 극복한 분석 결과 필요
# 그럼에도 불구하고 이러한 결과는 서울시의 대기질 및 한반도의 기후변화의 문제 해결을 위한 기초 자료로 활용될 것으로 사료된다.

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0356"

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
# 라이브러리 읽기
library(tidyverse)
library(lubridate)
library(openxlsx)
library(stats)
library(hydroGOF)
library(RColorBrewer)
library(forcats)
library(ggpubr)
library(scales)
library(openxlsx)
library(ggpubr)
library(ggplot2)
library(sf)
library(raster)
library(sf)

cbSpectral = rev(RColorBrewer::brewer.pal(11, "Spectral"))
cbMatlab2 = colorRamps::matlab.like2(11)


sysOpt = list(
  # 시작일/종료일 설정
  # , "endDateTime" = "2018-01-02 00:00"
  "srtDateTime" = "2018-01-01 00:00"
  , "endDateTime" = "2022-01-01 00:00"

  # 요청 URL
  , "reqRootUrl" = "http://openAPI.seoul.go.kr:8088"

  # API키
   , "reqKey" = "664250584973686c3637566f464a68"
)

Sys.setenv(PROJ_LIB = "/usr/local/anaconda3/envs/r36/share/proj")

# ******************************************************************************
# 서울시 열린데이터광장 오픈 API (서울시 기간별 시간평균 대기환경 정보)
# ******************************************************************************
# # 요청 목록
# data = seq(lubridate::ymd_hm(sysOpt$srtDateTime, tz="Asia/Seoul"), lubridate::ymd_hm(sysOpt$endDateTime, tz="Asia/Seoul"), by = "1 hour") %>%
#   as.tibble() %>%
#   dplyr::mutate(
#     sDateTime = format(value, "%Y%m%d%H%M")
#     , reqUrl = sprintf("%s/%s/json/TimeAverageCityAir/1/999/%s", sysOpt$reqRootUrl, stringr::str_conv(sysOpt$reqKey, encoding = "UTF-8"), sDateTime)
#   )
#
# dataL1 = tibble::tibble()
# for (i in 1:nrow(data)) {
#
#   rowData = data[i, ]
#   if (is.null(rowData) || nrow(rowData) < 1) next
#
#   jsonFile = NULL
#
#   tryCatch(
#     expr = {
#         jsonFile = RCurl::getURL(rowData$reqUrl)
#     }
#     , warning = function(warning) { cat(sprintf("[WARN] File Not Found (dtDateTime) : %s", rowData$value), "\n") }
#     , error = function(error) { cat(sprintf("[ERROR] File Not Found (dtDateTime) : %s", rowData$value), "\n")}
#   )
#   if (is.null(jsonFile) || length(jsonFile) < 1) next
#
#   # json 형식을 가시적으로 볼수 있도록 변환
#   jsonData = jsonlite::fromJSON(rowData$reqUrl)$TimeAverageCityAir
#   if (is.null(jsonData) || jsonData$list_total_count < 1) next
#
#   cat(sprintf("[CHECK] %s : %s %%",  rowData$value, round(i/nrow(data) * 100, 2)), "\n")
#
#   # 데이터 가져오기
#   dataL1 = dplyr::bind_rows(dataL1, jsonData$row)
# }
#
# dataL2 = dataL1 %>%
#   dplyr::mutate(
#     sDateTimeKst = MSRDT
#     , dtDateTimeKst = lubridate::ymd_hm(sDateTimeKst)
#   )
#
# minDate = min(dataL2$dtDateTimeKst, na.rm = TRUE) %>% format("%Y%m%d")
# maxDate = max(dataL2$dtDateTimeKst, na.rm = TRUE) %>% format("%Y%m%d")
#
# saveCsvFile = sprintf("%s/%s/%s_%s-%s.csv", globalVar$outPath, serviceName, "OrgData", minDate, maxDate)
# fs::dir_create(fs::path_dir(saveCsvFile), showWarnings = FALSE)
# readr::write_csv(dataL2, file = saveCsvFile)

#================================================
# 파일 읽기
#================================================
fileInfo = Sys.glob(file.path(globalVar$outPath, serviceName, "OrgData_*.csv"))
data = readr::read_csv(file = fileInfo)

# summary(data)

dataL1 = data %>%
  dplyr::mutate(
    dtYear = lubridate::year(dtDateTimeKst)
    , dtMonth = lubridate::month(dtDateTimeKst)
    , dtDay = lubridate::day(dtDateTimeKst)
    , dtXran = lubridate::decimal_date(dtDateTimeKst)
    , dtDoy =  as.numeric(format(dtDateTimeKst, "%j"))
    , season = dplyr::case_when(
      dtMonth == 1 | dtMonth == 2 | dtMonth == 12 ~ "Winter"
      , dtMonth == 3 | dtMonth == 4 | dtMonth == 5 ~ "Spring"
      , dtMonth == 6 | dtMonth == 7 | dtMonth == 8 ~ "Summer"
      , dtMonth == 9 | dtMonth == 10 | dtMonth == 11  ~ "Autumn"
      )
    , covidYn = ifelse(dtDateTimeKst >= as.Date("2020-01-20"), "코로나 이후", "코로나 이전")
    # , covidYn = ifelse(dtDateTimeKst >= as.Date("2020-01-20"), "Y", "N")
  ) %>%
  dplyr::filter(
    dplyr::between(PM10, 0, 500)
    , dplyr::between(PM25, 0, 500)
  )

summary(dataL1)

# plot(dataL1$dtDateTimeKst, dataL1$PM10)
# plot(dataL1$dtDateTimeKst, dataL1$PM25)

# **************************************************************************************
# 코로나 전후 월별 통계 데이터에 대한 막대 그래프 -> 전/후 (평균+표준편차)
# **************************************************************************************
dataL2 = dataL1 %>%
  dplyr::select(c("covidYn", "dtMonth", "PM10", "PM25")) %>%
  tidyr::gather(-covidYn, -dtMonth, key = "key", value = "val") %>%
  dplyr::group_by(covidYn, dtMonth, key) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
    , sdVal = sd(val, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    type = dplyr::case_when(
      key == "PM10" ~ sprintf('PM[10]')
      , key == "PM25"~ sprintf('PM[2.5]')
      , TRUE ~ NA_character_
    )
  )

# 계절별 평균 미세먼지 막대 그래프
plotSubTitle = sprintf("%s", "코로나 전후 월별 통계 데이터에 대한 막대 그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(data = dataL2, aes(x = dtMonth, y = meanVal, color = covidYn, group = covidYn)) +
  geom_bar(stat = "identity", width = 0.4, position=position_dodge(width = 0.5), fill = "white") +
  geom_errorbar(width = 0.3, aes(ymin=meanVal - sdVal, ymax=meanVal + sdVal, group = covidYn), position = position_dodge(0.5), show.legend = FALSE) +
  labs(title = NULL, x = "Month", y = bquote('PM  ['*ug/m^3*']'), colour = NULL, fill = NULL, subtitle = plotSubTitle) +
  # scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  scale_x_continuous(minor_breaks = seq(1, 12, 1), breaks=seq(1, 12, 1), limits=c(1,  12)) +
  # scale_y_continuous(minor_breaks = seq(-0.1, 20, 5), breaks=seq(-0.5, 20, 5), limits=c(-0.5, 20)) +
  theme(
    text = element_text(size = 16)
    # , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  # facet_wrap(~season, scale = "free_x") +
  facet_wrap(~type, scale = "free_y", ncol = 1, labeller = label_parsed) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)

ggplot2::last_plot()


# **************************************************************************************
# 코로나 전후 계절별 통계 데이터에 대한 막대 그래프 -> 전/후 (평균+표준편차)
# **************************************************************************************
dataL2 = dataL1 %>%
  dplyr::select(c("covidYn", "season", "PM10", "PM25")) %>%
  tidyr::gather(-covidYn, -season, key = "key", value = "val") %>%
  dplyr::group_by(covidYn, season, key) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
    , sdVal = sd(val, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    type = dplyr::case_when(
      key == "PM10" ~ sprintf('PM[10]')
      , key == "PM25"~ sprintf('PM[2.5]')
      , TRUE ~ NA_character_
    )
  )

# 정렬
dataL2$season = forcats::fct_relevel(dataL2$season, c("Spring", "Summer", "Autumn", "Winter"))

# 계절별 평균 미세먼지 막대 그래프
plotSubTitle = sprintf("%s", "코로나 전후 계절별 통계 데이터에 대한 막대 그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(data = dataL2, aes(x = season, y = meanVal, color = covidYn, group = covidYn)) +
  geom_bar(stat = "identity", width = 0.4, position=position_dodge(width = 0.5), fill = "white") +
  geom_errorbar(width = 0.3, aes(ymin=meanVal - sdVal, ymax=meanVal + sdVal, group = covidYn), position = position_dodge(0.5), show.legend = FALSE) +
  labs(title = NULL, x = "Season", y = bquote('PM  ['*ug/m^3*']'), colour = NULL, fill = NULL, subtitle = plotSubTitle) +
  # scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  # scale_x_continuous(minor_breaks = seq(1, 12, 1), breaks=seq(1, 12, 1), limits=c(1,  12)) +
  # scale_y_continuous(minor_breaks = seq(-0.1, 20, 5), breaks=seq(-0.5, 20, 5), limits=c(-0.5, 20)) +
  theme(
    text = element_text(size = 16)
    # , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  # facet_wrap(~season, scale = "free_x") +
  facet_wrap(~type, scale = "free_y", ncol = 1, labeller = label_parsed) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)

ggplot2::last_plot()


# **************************************************************************************
# 코로나 전후 월별 지역별 평균 PM10 데이터에 대한 시계열 그래프
# 코로나 전후 월별 지역별 평균 PM25 데이터에 대한 시계열 그래프
# **************************************************************************************
dataL2 = dataL1 %>%
  # dplyr::select(c("covidYn", "dtMonth", "MSRSTE_NM", "PM10", "PM25")) %>%
  dplyr::select(c("covidYn", "dtMonth", "MSRSTE_NM", "PM25", "PM10")) %>%
  tidyr::gather(-covidYn, -dtMonth, -MSRSTE_NM, key = "key", value = "val") %>%
  dplyr::group_by(covidYn, dtMonth, MSRSTE_NM, key) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
    , sdVal = sd(val, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    type = dplyr::case_when(
      key == "PM10" ~ sprintf('PM[10]')
      , key == "PM25"~ sprintf('PM[2.5]')
      , TRUE ~ NA_character_
    )
  )

dataL3 = dataL2 %>%
  dplyr::filter(
    key == "PM10"
  )

# 정렬
# dataL2$season = forcats::fct_relevel(dataL2$season, c("Spring", "Summer", "Autumn", "Winter"))

# 코로나 전후 월별 지역별 평균 PM10 데이터에 대한 시계열 그래프
plotSubTitle = sprintf("%s", "코로나 전후 월별 지역별 평균 PM10 데이터에 대한 시계열 그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(data = dataL3, aes(x = dtMonth, y = meanVal, color = covidYn, group = covidYn)) +
  geom_line() +
  geom_point() +
  # ggpubr::stat_cor(label.x.npc = 0.45, label.y.npc = 1.0, p.accuracy  =  0.01,  r.accuracy  =  0.01, size = 4) +
  ggpubr::stat_cor(label.x.npc = 0.4, label.y.npc = 1.0, p.accuracy  =  0.01,  r.accuracy  =  0.01, size = 4.5, show.legend = FALSE) +
  labs(title = NULL, x = "Month", y = bquote('PM' ['10'] *'  ['*ug/m^3*']'), colour = NULL, fill = NULL, subtitle = plotSubTitle) +
  # scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  scale_x_continuous(minor_breaks = seq(1, 12, 1), breaks=seq(1, 12, 1), limits=c(1,  12)) +
  # scale_y_continuous(minor_breaks = seq(0, 80, 20), breaks=seq(0, 80, 20), limits=c(0, 80)) +
  theme(
    text = element_text(size = 16)
    # , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  facet_wrap(~MSRSTE_NM, scale = "free_x", ncol = 5, labeller = label_parsed) +
  ggsave(filename = saveImg, width = 15, height = 10, dpi = 600)

ggplot2::last_plot()

# ggpubr::ggscatter(dataL3, x = "dtMonth", y = "meanVal", color = "covidYn", add = "reg.line", conf.int = TRUE, cor.coef = FALSE, add.params = list(color = "black", fill = "lightgray")) +
#   # ggpubr::stat_regline_equation(label.x.npc = 0.025, label.y.npc = 1.0, size = 4, aes(color=covidYn, label = ..eq.label..), color = "black", parse = TRUE) +
#   # ggpubr::stat_cor(label.x.npc = 0.025, label.y.npc = 0.90, size = 4, color = "black") +
#   # ggpubr::stat_regline_equation(aes(color = covidYn), label.x.npc = 0.0, label.y.npc = 1.0, size = 5) +
#   # ggpubr::stat_cor(aes(color = covidYn), label.x.npc = 0.5, label.y.npc = 1.0, p.accuracy  =  0.01,  r.accuracy  =  0.01, size = 5) +
#   scale_x_continuous(minor_breaks = seq(1, 12, 1), breaks=seq(1, 12, 1), limits=c(1,  12)) +
#   scale_y_continuous(minor_breaks = seq(0, 80, 20), breaks=seq(0, 80, 20), limits=c(0, 80)) +
#   labs(title = NULL, x = "Month", y = bquote('PM' ['10'] *'  ['*ug/m^3*']'), colour = NULL, fill = NULL, subtitle = plotSubTitle) +
#   theme_bw() +
#   theme(
#     text = element_text(size = 16)
#     # , axis.text.x = element_text(angle = 45, hjust = 1)
#     , legend.position = "top"
#   ) +
#   facet_wrap(~MSRSTE_NM, scale = "free_x", ncol = 5, labeller = label_parsed) +
#   ggsave(filename = saveImg, width = 15, height = 10, dpi = 600)

# ggplot2::last_plot()

# 코로나 전후 월별 지역별 평균 PM25 데이터에 대한 시계열 그래프
dataL3 = dataL2 %>%
  dplyr::filter(
    key == "PM25"
  )

# 코로나 전후 월별 지역별 평균 PM25 데이터에 대한 시계열 그래프
plotSubTitle = sprintf("%s", "코로나 전후 월별 지역별 평균 PM25 데이터에 대한 시계열 그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(data = dataL3, aes(x = dtMonth, y = meanVal, color = covidYn, group = covidYn)) +
  geom_line() +
  geom_point() +
   ggpubr::stat_cor(label.x.npc = 0.4, label.y.npc = 1.0, p.accuracy  =  0.01,  r.accuracy  =  0.01, size = 4.5, show.legend = FALSE) +
  labs(title = NULL, x = "Month", y = bquote('PM' ['2.5'] *'  ['*ug/m^3*']'), colour = NULL, fill = NULL, subtitle = plotSubTitle) +
  # scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  scale_x_continuous(minor_breaks = seq(1, 12, 1), breaks=seq(1, 12, 1), limits=c(1,  12)) +
  # scale_y_continuous(minor_breaks = seq(-0.1, 20, 5), breaks=seq(-0.5, 20, 5), limits=c(-0.5, 20)) +
  theme(
    text = element_text(size = 16)
    # , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  facet_wrap(~MSRSTE_NM, scale = "free_x", ncol = 5, labeller = label_parsed) +
  ggsave(filename = saveImg, width = 15, height = 10, dpi = 600)

ggplot2::last_plot()

# **************************************************************************************
# 코로나 전후 계절별 지역별 평균 데이터에 대한 시계열 그래프 -> 전/후
# **************************************************************************************
dataL2 = dataL1 %>%
  dplyr::select(c("covidYn", "season", "MSRSTE_NM", "PM25", "PM10")) %>%
  tidyr::gather(-covidYn, -season, -MSRSTE_NM, key = "key", value = "val") %>%
  dplyr::group_by(covidYn, season, MSRSTE_NM, key) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
    , sdVal = sd(val, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    type = dplyr::case_when(
      key == "PM10" ~ sprintf('PM[10]')
      , key == "PM25"~ sprintf('PM[2.5]')
      , TRUE ~ NA_character_
    )
  )

# 정렬
dataL2$season = forcats::fct_relevel(dataL2$season, c("Spring", "Summer", "Autumn", "Winter"))

dataL3 = dataL2 %>%
  dplyr::filter(
    key == "PM10"
  )

# 코로나 전후 계절별 지역별 평균 PM10 데이터에 대한 시계열 그래프
plotSubTitle = sprintf("%s", "코로나 전후 계절별 지역별 평균 PM10 데이터에 대한 시계열 그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(data = dataL3, aes(x = season, y = meanVal, color = covidYn, group = covidYn)) +
  geom_line() +
  geom_point() +
  labs(title = NULL, x = "Season", y = bquote('PM' ['10'] *'  ['*ug/m^3*']'), colour = NULL, fill = NULL, subtitle = plotSubTitle) +
  # scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  # scale_x_continuous(minor_breaks = seq(1, 12, 1), breaks=seq(1, 12, 1), limits=c(1,  12)) +
  # scale_y_continuous(minor_breaks = seq(-0.1, 20, 5), breaks=seq(-0.5, 20, 5), limits=c(-0.5, 20)) +
  theme(
    text = element_text(size = 16)
    , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  facet_wrap(~MSRSTE_NM, ncol = 5, labeller = label_parsed) +
  ggsave(filename = saveImg, width = 15, height = 10, dpi = 600)

ggplot2::last_plot()

dataL3 = dataL2 %>%
  dplyr::filter(
    key == "PM25"
  )

# 코로나 전후 계절별 지역별 평균 PM25 데이터에 대한 시계열 그래프
plotSubTitle = sprintf("%s", "코로나 전후 계절별 지역별 평균 PM25 데이터에 대한 시계열 그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(data = dataL3, aes(x = season, y = meanVal, color = covidYn, group = covidYn)) +
  geom_line() +
  geom_point() +
  labs(title = NULL, x = "Season", y = bquote('PM' ['2.5'] *'  ['*ug/m^3*']'), colour = NULL, fill = NULL, subtitle = plotSubTitle) +
  # scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  # scale_x_continuous(minor_breaks = seq(1, 12, 1), breaks=seq(1, 12, 1), limits=c(1,  12)) +
  # scale_y_continuous(minor_breaks = seq(-0.1, 20, 5), breaks=seq(-0.5, 20, 5), limits=c(-0.5, 20)) +
  theme(
    text = element_text(size = 16)
    , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  facet_wrap(~MSRSTE_NM, ncol = 5, labeller = label_parsed) +
  ggsave(filename = saveImg, width = 15, height = 10, dpi = 600)

ggplot2::last_plot()


# **************************************************************************************
# 코로나 전후 전체 지역별 데이터에 대한 지도 시각화(전체 제목) -> 전/후
# **************************************************************************************
# SHP 파일 읽기
mapInfo = Sys.glob(file.path(globalVar$mapPath, "koreaInfo/TL_SCCO_SIG.shp"))
# mapInfo = Sys.glob(file.path(globalVar$mapPath, "koreaInfo/bnd_dong_00_2019_2019_2Q.shp"))
# mapInfo = Sys.glob(file.path(globalVar$mapPath, "koreaInfo/TL_SCCO_CTPRVN.shp"))

# shpData = raster::shapefile(mapInfo, encoding = "UTF-8")
# geo = sp::spTransform(shpData, CRS("+proj=longlat"))
# mapData = ggplot2::fortify(geo, region = 'SIG_CD', region2 = "SIG_KOR_NM")

mapData = sf::st_read(mapInfo, options = "ENCODING=EUC-KR") %>%
  sf::st_transform(sp::CRS("+proj=longlat")) %>%
  dplyr::mutate(
    sigCdOpt = stringr::str_sub(SIG_CD, 1, 2)
  ) %>%
  dplyr::filter(
    sigCdOpt == 11
  )

# 법정동 코드 읽기
codeInfo = Sys.glob(file.path(globalVar$mapPath, "admCode/admCode.xlsx"))
codeData = openxlsx::read.xlsx(codeInfo, sheet = 1, startRow = 2)

codeDataL1 = codeData %>%
  dplyr::select("시도코드", "시도명칭", "시군구코드", "시군구명칭", "읍면동코드", "읍면동명칭") %>%
  # dplyr::select("시도코드", "시도명칭", "시군구코드", "시군구명칭") %>%
  dplyr::filter(
    stringr::str_detect(시도명칭, regex("서울특별시"))
  ) %>%
  dplyr::distinct(시군구코드, 시군구명칭)

dataL2 = dataL1 %>%
  dplyr::select(c("covidYn", "MSRSTE_NM", "PM25", "PM10")) %>%
  tidyr::gather(-covidYn, -MSRSTE_NM, key = "key", value = "val") %>%
  dplyr::group_by(covidYn, MSRSTE_NM, key) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
    , sdVal = sd(val, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    type = dplyr::case_when(
      key == "PM10" ~ sprintf('PM[10]')
      , key == "PM25"~ sprintf('PM[2.5]')
      , TRUE ~ NA_character_
    )
  )

summary(dataL2)

keyList = dataL2$key %>% unique()
covidYnList = dataL2$covidYn %>% unique()

for (keyInfo in keyList) {
  for (covidYnInfo in covidYnList) {

    cat(sprintf("[CHECK] keyInfo / covidYnInfo : %s / %s", keyInfo, covidYnInfo), "\n")

    dataL3 = dataL2 %>%
      dplyr::filter(
        key == keyInfo
        , covidYn == covidYnInfo
      )

    print(summary(dataL3$meanVal))

    statData = dataL2 %>%
      dplyr::filter(
        key == keyInfo
      )

    maxVal = max(statData$meanVal, na.rm = TRUE) %>% ceiling()
    minVal = min(statData$meanVal, na.rm = TRUE) %>% floor()

    if (nrow(dataL3) < 1) { next }

    # 통합 데이터셋
    dataL4 = mapData %>%
      dplyr::inner_join(codeDataL1, by = c("SIG_KOR_NM" = "시군구명칭")) %>%
      dplyr::left_join(dataL3, by = c("SIG_KOR_NM" = "MSRSTE_NM"))

    mainTitle = sprintf("%s 전체 지역별 %s 데이터에 대한 지도 시각화", covidYnInfo, keyInfo)
    saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)

    # subTitle = sprintf("[%s] %s", covidYnInfo, bquote('PM' ['2.5'] *'  ['*ug/m^3*']'))


 # bquote(eval(covidYnInfo) * 'PM' ['2.5'] *'  ['*ug/m^3*']')
    makePlot = ggplot(data = dataL4, aes(fill = meanVal, label = SIG_KOR_NM)) +
      theme_bw() +
      coord_fixed(ratio = 1) +
      geom_sf(color = "white") +
      geom_sf_text(color = "black") +
      scale_fill_gradientn(colours = cbSpectral, limits = c(minVal, maxVal), na.value = NA) +
      labs(title = NULL, x = NULL, y = NULL, colour = NULL, fill = NULL) +
      theme(
        text = element_text(size = 16)
        , panel.grid.major.x = element_blank()
        , panel.grid.major.y = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , panel.border = element_blank()
        , axis.text.x = element_blank()
        , axis.ticks.x = element_blank()
        , axis.title.x = element_blank()
        , axis.text.y = element_blank()
        , axis.ticks.y = element_blank()
        , axis.title.y = element_blank()
        , plot.subtitle = element_text(hjust = 1)
        # , legend.position = "bottom"
        , legend.position = c(0.25, 0.9)
        , legend.key.width = unit(1.75, "cm")
        , legend.direction = "horizontal"
      )

    ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)

  }
}

# 21	용산구	32.8643	34.2817	-1.41737
# 24	중구	36.7233	36.9017	-0.178368
# 25	중랑구	34.0691	34.9448	-0.875648
# keyInfo = "PM10"
# 코로나 전후 전체 지역별 PM25 편차 데이터에 대한 지도 시각화
for (keyInfo in keyList) {

    cat(sprintf("[CHECK] keyInfo : %s", keyInfo), "\n")

    dataL3 = dataL2 %>%
      dplyr::filter(
        key == keyInfo
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(covidYn, MSRSTE_NM, meanVal) %>%
      tidyr::spread( key = "covidYn", value = "meanVal") %>%
      dplyr::mutate(
        # meanVal = `코로나 이후` - `코로나 이전`
        meanVal = `코로나 이전` - `코로나 이후`
      )

    print(summary(dataL3$meanVal))

    maxVal = max(dataL3$meanVal, na.rm = TRUE) %>% ceiling()
    # minVal = min(dataL3$meanVal, na.rm = TRUE) %>% floor()
    minVal = -c(maxVal)

    if (nrow(dataL3) < 1) { next }

    # 통합 데이터셋
    dataL4 = mapData %>%
      dplyr::inner_join(codeDataL1, by = c("SIG_KOR_NM" = "시군구명칭")) %>%
      dplyr::left_join(dataL3, by = c("SIG_KOR_NM" = "MSRSTE_NM"))

    mainTitle = sprintf("%s 전체 지역별 %s 편차 데이터에 대한 지도 시각화", '코로나 전후', keyInfo)
    saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)

    # subTitle = sprintf("[%s] %s", covidYnInfo, bquote('PM' ['2.5'] *'  ['*ug/m^3*']'))


 # bquote(eval(covidYnInfo) * 'PM' ['2.5'] *'  ['*ug/m^3*']')
    makePlot = ggplot(data = dataL4, aes(fill = meanVal, label = SIG_KOR_NM)) +
      theme_bw() +
      coord_fixed(ratio = 1) +
      geom_sf(color = "white") +
      geom_sf_text(color = "black") +
      # scale_fill_gradientn(colours = cbSpectral, limits = c(minVal, maxVal), na.value = NA) +
      scale_fill_gradient2(limits = c(minVal, maxVal), na.value = NA) +
      labs(title = NULL, x = NULL, y = NULL, colour = NULL, fill = NULL) +
      theme(
        text = element_text(size = 16)
        , panel.grid.major.x = element_blank()
        , panel.grid.major.y = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , panel.border = element_blank()
        , axis.text.x = element_blank()
        , axis.ticks.x = element_blank()
        , axis.title.x = element_blank()
        , axis.text.y = element_blank()
        , axis.ticks.y = element_blank()
        , axis.title.y = element_blank()
        , plot.subtitle = element_text(hjust = 1)
        # , legend.position = "bottom"
        , legend.position = c(0.25, 0.9)
        , legend.key.width = unit(1.75, "cm")
        , legend.direction = "horizontal"
      )

    ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)

  }
}