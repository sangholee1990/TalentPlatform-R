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
# R을 이용한 서울시 아파트 실거래가 회귀분석 및 주택 가격 결정 요인

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0306"

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
    , "mapPath" = contextPath
  )
} else {
  # source(here::here(file.path(contextPath, "src"), "InitConfig.R"), encoding = "UTF-8")
  source(file.path(contextPath, "src", "InitConfig.R"))
}

# ================================================
# 비즈니스 로직 수행
# ================================================
# 라이브러리 읽기
library(ggplot2)
library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)
library(RCurl)
library(readr)
library(magrittr)
library(ggrepel)
library(colorRamps)
library(ggpubr)
library(lm.beta)
library(ggmap)

# 인코딩 설정
Sys.setlocale("LC_ALL", "English")

cbMatlab = colorRamps::matlab.like(11)

# 공공데이터포털 API키
# reqDataKey = globalVar$dataKey
reqDataKey = "Ftj0WhfmnXN86rrVCPTGvlQJ%oJs9l+ZQjJzPgtc37yVPWuXs8UOP3kD2lTyy9DFInQZj2VvYFH1+Uh7gNgTLLA=="

# 요청 URL
reqUrl = "http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade"

# 구글 인증키
ggmap::register_google(key = "AIzaSyCOfommvdtr_CVmQyL-5wFz31G21CklUE4")


# 요청 API
reqKey = stringr::str_c("?serviceKey=", RCurl::curlEscape(stringr::str_conv(reqDataKey, encoding = "UTF-8")))

# 서울에서 서울특별시 법정동 코드 읽기
codeInfo = Sys.glob(file.path(globalVar$mapPath, serviceName, "/admCode/법정동코드_전체자료.txt"))
codeList = readr::read_delim(codeInfo, delim = "\t", locale = locale("ko", encoding = "EUC-KR"), col_types = "ccc") %>%
  magrittr::set_colnames(c("EMD_CD", "addr", "isUse")) %>% 
  tidyr::separate(col = "addr", into = c("d1", "d2", "d3", "d4"), sep = " ") %>%
  dplyr::mutate(
    emdCd = stringr::str_sub(EMD_CD, 1, 5)
  ) %>% 
  dplyr::filter(
    stringr::str_detect(d1, regex("서울특별시"))
    , stringr::str_detect(isUse, regex("존재"))
    , is.na(d3)
    , is.na(d4)
  )

codeDistList = codeList %>%
  dplyr::distinct(emdCd)

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "가구_특성정보_(+소득정보)_201211.csv"))

costData = readr::read_csv(file = fileInfo) %>%
  dplyr::mutate(
    emdCd = stringr::str_sub(as.character(raw_dn_cd), 1, 5)
  ) %>% 
  dplyr::group_by(emdCd) %>% 
  dplyr::summarise(
    meanCost = mean(avrg_income_amount_am, na.rm = TRUE)
  )

# ***********************************************
# 공공데이터포털 API (자료 수집)
# ***********************************************
# dataL1 = tibble::tibble()
# 
# for (i in 1:length(dtDateList)) {
#   for (j in 1:nrow(codeDistList)) {
#     
#     sDate = format(dtDateList[i], "%Y%m")
#     
#     # 요청 법정동
#     reqLawdCd = stringr::str_c("&LAWD_CD=", codeDistList[j, 'emdCd'])
#     
#     # 요청 날짜
#     reqYmd = stringr::str_c("&DEAL_YMD=", sDate)
#     
#     resData = httr::GET(
#       stringr::str_c(reqUrl, reqKey, reqLawdCd, reqYmd)
#     ) %>%
#       httr::content(as = "text", encoding = "UTF-8") %>%
#       jsonlite::fromJSON()
#     
#     resCode = resData$response$header$resultCode
#     if (resCode != "00") { next }
#     
#     resItems = resData$response$body$items
#     if (resItems == "") { next }
#     
#     cat(sprintf(
#       "dtDate : %10s | code : %5s"
#       , sDate
#       , codeList[j, 'emdCd']
#     ), "\n")
#     
#     resItem = resItems$item %>%
#       as.data.frame()
#     # readr::type_convert()
#     
#     dataL1 = dplyr::bind_rows(
#       dataL1
#       , data.frame(
#         'dtYm' = sDate
#         , 'emdCd' = codeDistList[j, 'emdCd']
#         , resItem
#       )
#     )
#   }
# }

# ***********************************************
# 자료 저장
# ***********************************************
# saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "seoul apartment transaction.csv")
# readr::write_csv(x = dataL1, file = saveFile)

# ***********************************************
# 데이터 전처리
# ***********************************************
# 인코딩 설정
Sys.setlocale("LC_ALL", "Korean")

fileInfo = Sys.glob(file.path(globalVar$outPath, serviceName, "seoul apartment transaction.csv"))

dataL2 = readr::read_csv(file = fileInfo) %>% 
  readr::type_convert() %>% 
  dplyr::mutate(
    지번2 = readr::parse_number(지번)
    , emdCd = as.character(emdCd)
  ) %>% 
  dplyr::left_join(codeList, by = c("emdCd" = "emdCd")) %>%
  dplyr::left_join(costData, by = c("emdCd" = "emdCd")) %>% 
  dplyr::mutate(
    addr = stringr::str_trim(paste(d1, d2, 아파트, 지번, seq = ' '))
    , val = 거래금액 / meanCost # 연소득당 거래금액
    , val2 = 거래금액 / 전용면적 # 면적당 거래금액
  )

dataL3 = dataL2 %>% 
  dplyr::group_by(d2) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  )

# ***********************************************
# 통계 분석
# ***********************************************
# 연소득당 거래금액 따른 기초 통계량
dataL2 %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE) # 평균값
    , medianVal = median(val, na.rm = TRUE) # 중앙값
    , sdVal = sd(val, na.rm = TRUE) # 표준편차
    , maxVal = max(val, na.rm = TRUE) # 최대값
    , minVal = min(val, na.rm = TRUE) # 최소값
    , cnt = n() # 개수
  ) %>%
  dplyr::arrange(desc(meanVal))

# 법정동에 따른 연소득당 거래금액 따른 기초 통계량
dataL2 %>%
  dplyr::group_by(d2) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE) # 평균값
    , medianVal = median(val, na.rm = TRUE) # 중앙값
    , sdVal = sd(val, na.rm = TRUE) # 표준편차
    , maxVal = max(val, na.rm = TRUE) # 최대값
    , minVal = min(val, na.rm = TRUE) # 최소값
    , cnt = n() # 개수
  ) %>%
  dplyr::arrange(desc(meanVal))


# ***********************************************
# 그래프 그리기(히스토그램, 상자 수염그림, 산점도 등)
# ***********************************************
# 연소득당 거래금액 따른 히스토그램
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "연소득당 거래금액 따른 히스토그램")

ggplot(dataL2, aes(x = val)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = 0.2) +
  geom_rug(aes(x = val, y = 0), position = position_jitter(height = 0)) +
  labs(x = "연소득당 거래금액", y = "밀도 함수", colour = NULL, fill = NULL, subtitle = "연소득당 거래금액 따른 히스토그램") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 12, height = 6, dpi = 600)

# 법정동에 따른 연소득당 거래금액 히스토그램
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "법정동에 따른 연소득당 거래금액 히스토그램")

ggplot(dataL3, aes(x = d2, y = meanVal, fill = meanVal)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = round(meanVal, 0)), vjust = 1.6, color = "white", size = 4) +
  labs(x = "법정동", y = "연소득당 거래금액", fill = NULL, subtitle = "법정동에 따른 연소득당 거래금액 히스토그램") +
  scale_fill_gradientn(colours = cbMatlab, na.value = NA) +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)


# 연소득당 거래금액 따른 상자 그림
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "연소득당 거래금액 따른 상자 그림")

ggplot(dataL2, aes(y = val)) +
  geom_boxplot() +
  labs(x = NULL, y = "연소득당 거래금액", colour = NULL, fill = NULL, subtitle = "연소득당 거래금액 따른 상자 그림") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 12, height = 6, dpi = 600)

# 법정동에 따른 연소득당 거래금액 상자 그림
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "법정동에 따른 연소득당 거래금액 상자 그림")

ggplot(dataL2, aes(x = d2, y = val, color = d2)) +
  geom_boxplot() +
  labs(x = "법정동", y = "연소득당 거래금액", fill = NULL, subtitle = "법정동에 따른 연소득당 거래금액 상자 그림") +
  # scale_colour_gradientn(colours = cbMatlab, na.value = NA) +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)

# 연소득당 거래금액 산점도
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "연소득당 거래금액 산점도")

ggpubr::ggscatter(
  dataL2, x = "meanCost", y = "거래금액"
  , add = "reg.line", conf.int = TRUE, scales = "free_x"
  # , facet.by = "전체 법정동"
  , add.params = list(color = "blue", fill = "lightblue")
) +
  labs(
    title = NULL
    , x = "연소득"
    , y = "거래금액"
    , color = NULL
    , subtitle = "연소득당 거래금액 산점도"
  ) +
  theme_bw() +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 1.0, size = 5) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.90, p.accuracy  =  0.01,  r.accuracy  =  0.01, size = 5) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 8, height = 8, dpi = 600)

# # 법정동에 따른 연소득당 거래금액 산점도
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "법정동에 따른 연소득당 거래금액 산점도")
# 
# ggpubr::ggscatter(
#   dataL2, x = "meanCost", y = "거래금액", color = "d2"
#   , add = "reg.line", conf.int = TRUE, scales = "free_x"
#   , facet.by = "d2"
#   , add.params = list(color = "black", fill = "lightgray")
# ) +
#   labs(
#     title = NULL
#     , x = "연소득"
#     , y = "거래금액"
#     , color = NULL
#     , subtitle = "법정동에 따른 연소득당 거래금액 산점도"
#   ) +
#   ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 0.95) +
#   ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.85, p.accuracy  =  0.01,  r.accuracy  =  0.01) +
#   theme(text = element_text(size = 14)) +
#   ggsave(filename = saveImg, width = 12, height = 15, dpi = 600)

# ***********************************************
# 지도 그리기
# ***********************************************
addrList = dataL2$addr %>% unique() %>% sort() %>%
  as.tibble()

# 구글 API 하루 제한
# addrData = ggmap::mutate_geocode(addrList, value, source = "google")

# 각 주소에 따라 위/경도 반환
# for (i in 1:nrow(addrList)) {
#   addrData = ggmap::mutate_geocode(addrList[i, 'value'], value, source = "google")
# 
#   if (nrow(addrData) < 1) { next }
# 
#   readr::write_csv(x = addrData, file = saveFile, append = TRUE)
# }

saveFile = sprintf("%s/%s/%s.csv", globalVar$outPath, serviceName, "seoul apartment transaction-addrData")
addrData =  readr::read_csv(file = saveFile, col_names = c("value", "lon", "lat"))

dataL4 = dataL2 %>% 
  dplyr::left_join(addrData, by = c("addr" = "value")) %>% 
  dplyr::filter(
    ! is.na(lon)
    , ! is.na(lat)
    , dplyr::between(lon, 120, 130)
    , dplyr::between(lat, 30, 40)
  ) %>% 
  dplyr::group_by(lon, lat, addr) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  )

map = ggmap::get_map(
  location = c(lon = mean(dataL4$lon, na.rm = TRUE), lat = mean(dataL4$lat, na.rm = TRUE))
  , zoom = 12
)

saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "연소득당 거래금액 지도 매핑")

ggmap::ggmap(map, extent = "device") +
  geom_point(data = dataL4, aes(x = lon, y = lat, color = meanVal, size = meanVal, alpha = 0.3)) +
  scale_color_gradientn(colours = cbMatlab, na.value = NA) +
  labs(
    subtitle = NULL
    , x = NULL
    , y = NULL
    , fill = NULL
    , colour = NULL
    , title = NULL
    , size = NULL
  ) +
  scale_alpha(guide = 'none') +
  theme(
    text = element_text(size = 18)
  ) +
  ggsave(filename = saveImg, width = 10, height = 10, dpi = 600)


 # ***********************************************
# 주택 가격 결정 요인을 위한 회귀분석
# ***********************************************
dataL4 = dataL2 %>%
  dplyr::select(건축년도, 전용면적, 층, val2, d2, val)

#+++++++++++++++++++++++++++++++++++++++++++++++
# 전체 아파트
dataL5 = dataL4

# 중형 이상 아파트 (66 m2 이상)
dataL5 = dataL4 %>% 
  dplyr::filter(전용면적 >= 66) %>% 
  dplyr::select(-전용면적)

# 소형 아파트 (66 m2 미만)
dataL5 = dataL4 %>% 
  dplyr::filter(전용면적 < 66) %>% 
  dplyr::select(-전용면적)
#+++++++++++++++++++++++++++++++++++++++++++++++

# 선형회귀분석
lmFit = lm(val ~ ., data = dataL5)
summary(lmFit)

# 단계별 소거법
lmFitStep = MASS::stepAIC(lmFit, direction = "both")
summary(lmFitStep)

# Beta 회귀계수
lmBetaFit = lm.beta::lm.beta(lmFitStep)
lmBetaFit$standardized.coefficients %>% round(2) %>% sort() %>% rev()

# 산점도 그림
validData = data.frame(
  xAxis = predict(lmFitStep)
  , yAxis = dataL5$val
  # , type = "전체 아파트"
  # , type = "중형 아파트"
  , type = "소형 아파트"
  
)

# corVal = cor(validData$xAxis, validData$yAxis)
biasVal = Metrics::bias(validData$xAxis, validData$yAxis)
rmseVal = Metrics::rmse(validData$xAxis, validData$yAxis)

# 전체 아파트에 대한 주택가격 결정요인 (연소득당 거래금액) 예측 산점도
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "전체 아파트에 대한 주택가격 결정요인 예측 산점도")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "중형 아파트에 대한 주택가격 결정요인 예측 산점도")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "소형 아파트에 대한 주택가격 결정요인 예측 산점도")

ggscatter(
  validData, x = "xAxis", y = "yAxis", color = "black"
  , add = "reg.line", conf.int = TRUE
  , facet.by = "type"
  , add.params = list(color = "blue", fill = "lightblue")
) +
  theme_bw() +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 1.0, size = 4) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.9, size = 4) +
  ggpp::annotate("text_npc", npcx = 0.05, npcy = 0.8, label = sprintf("Bias = %s", round(biasVal, 2)), hjust = 0, size = 4) +
  ggpp::annotate("text_npc", npcx = 0.05, npcy = 0.7, label = sprintf("RMSE = %s", round(rmseVal, 2)), hjust = 0, size = 4) +
  # ggpp::annotate("text_npc", npcx = 0.05, npcy = 0.60, label = sprintf("Bias = %s", round(biasVal, 2)), hjust = 0, size = 4) +
  # ggpp::annotate("text_npc", npcx = 0.05, npcy = 0.55, label = sprintf("RMSE = %s", round(rmseVal, 2)), hjust = 0, size = 4) +
  labs(
    title = NULL
    , x = "예측"
    , y = "실측"
    , subtitle = "전체 아파트에 대한 주택가격 결정요인 예측 산점도"
    # , subtitle = "중형 아파트에 대한 주택가격 결정요인 예측 산점도"
    # , subtitle = "소형 아파트에 대한 주택가격 결정요인 예측 산점도"
  ) +
  theme(text = element_text(size = 16)) +
  ggsave(filename = saveImg, width = 6, height = 6, dpi = 600)


# 주택 가격 결정 요인을 위한 관계성
# 오래 소요
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "주택 가격 결정 요인을 위한 관계성")
# 
# dataL2 %>%
#   dplyr::select(건축년도, 전용면적, 층, val2, val) %>% 
#   dplyr::rename(
#     "면적당거래금액" = val2
#     , "연소득당거래금액" = val
#   ) %>% 
#   GGally::ggpairs(.) +
#   theme(text = element_text(size = 18))
# 
# ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)
