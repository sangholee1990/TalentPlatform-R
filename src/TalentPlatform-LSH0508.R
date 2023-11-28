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
# R을 이용한 제품전후 전력량 비교 및 통계 검정 보고서 (08.08일 기준으로 이전 및 이후)

# 8월 8일 기준 이전 데이타와 이후 데이타의 값이 얼마나 감소 했나? 입니다.
# 이데이타는 컨테이너 항에 있는 크레인의 전력 사용량 값입니다.
# 크레인의 가동에 따라 전력값이 변화 하기때문에 이 데이타 값을 어떻게 기준을 잡아야 할지를 이데이타에서 확인 해야 합니다.
# 감사합니다.

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0508"

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
library(ggpubr)
library(webr)
library(openxlsx)
library(lubridate)
library(fs)

# 파일 읽기
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "K.TAEAN호설치전후평가_230405.xlsx"))
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "*.xlsx"))

# fileInfo = fileList[1]
orgDataL2 = tibble::tibble()
for (fileInfo in fileList) {
  cat(sprintf("[CHECK] fileInfo : %s", fileInfo), "\n")
    
  orgData = openxlsx::read.xlsx(fileInfo, sheet = 1, startRow = 0)
  
  orgDataL1 = orgData %>% 
    tibble::as.tibble() %>% 
    tidyr::gather(-X1, key = "key", value = "val") %>% 
    dplyr::mutate(
      sDateTime = sprintf("%s %s", key, X1)
    ) %>% 
    dplyr::select(-c("X1", "key")) %>% 
    dplyr::mutate(
      dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
      , dtDate = lubridate::date(dtDateTime)
    ) %>% 
    dplyr::mutate(
      label = dplyr::case_when(
        dtDate < as.Date("2023-08-08")~ "before"
        , dtDate > as.Date("2023-08-23") ~ "after"
      )
    ) %>% 
    dplyr::filter(
      ! is.na(val)
      , ! is.na(dtDate)
      , ! is.na(label)
    )
 
  orgDataL2 = dplyr::bind_rows(orgDataL2, orgDataL1)
}

data = orgDataL2 %>% 
  dplyr::mutate(
    # dtYear = lubridate::year(dtDateTime)
    # , dtMonth = lubridate::month(dtDateTime)
    # , dtDay = lubridate::day(dtDateTime)
    dtYmd = format(dtDateTime, "%Y%m%d")
  ) #%>% 

dataL1 = data %>%
  dplyr::mutate(
    dtDate = readr::parse_date(dtYmd, "%Y%m%d")
    , dtXran = lubridate::decimal_date(dtDate)
  )

dataL1 %>% 
  dplyr::ungroup() %>% 
  # dplyr::select(label, sumVal) %>%
  dplyr::select(label, val) %>%
  dplyr::group_by(label) %>%
  dplyr::summarise_all(list(
    sum = ~sum(., na.rm = TRUE),
    mean = ~mean(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE)
  ))


prdData = data.frame(dtXran = c(2023, 2024))

befData = dataL1 %>% 
  dplyr::filter(label == "before")

befModel = lm(val ~ dtXran, data = befData)
predict(befModel, newdata = prdData) %>% round(2)

# 100 - (6668.58 / 4493.68 * 100)
# -48.39908494

aftData = dataL1 %>% 
  dplyr::filter(label == "after")

aftModel = lm(val ~ dtXran, data = aftData)
predict(aftModel, newdata = prdData) %>% round(2)
# 100 - (1321.00 / 9586.27 * 100)
# 86.21987488

# *****************************************
# 시각화
# *****************************************
plotSubTitle = sprintf("%s", "제품전후 전력량에 따른 비교추이")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

makePlot = ggpubr::ggscatter(
  data = dataL1, x = "dtXran", y = "val", color = "label"
  , add = "reg.line", alpha = 0.1, palette = c("#00AFBB", "#E7B800")
) +
  labs(title = NULL, x = "연도", y = "전력 사용량", color = NULL, subtitle = plotSubTitle) +
  theme_bw() +
  ggpubr::stat_regline_equation(aes(color = label), label.x.npc = 0.0, label.y.npc = 0.95, size = 6, show.legend = FALSE) +
  ggpubr::stat_cor(aes(color = label), label.x.npc = 0.5, label.y.npc = 0.95, p.accuracy = 0.01, r.accuracy = 0.01, size = 6, show.legend = FALSE) +
  scale_x_continuous(minor_breaks = c(2023 + ((1:20) * 0.05)), breaks= c(2023 + ((1:20) * 0.05)),  limits=c(2023.45, 2023.90)) +
  scale_y_continuous(minor_breaks = seq(0, 20000, 5000), breaks=seq(0, 20000, 5000), limits=c(0, 20000)) +
  theme(
    text = element_text(size = 18)
    , legend.position = "top"
  )

ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# ggplot2::last_plot()
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# plotSubTitle = sprintf("%s", "제품전후 전력량에 따른 절감율 추이")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# 
# makePlot = ggpubr::ggscatter(
#   data = dataL2, x = "id", y = "rat"
#   , add = "reg.line", alpha = 0.3
# ) +
#   labs(title = NULL, x = "인덱스", y = "절감 비율", fill = NULL, color = NULL, subtitle = plotSubTitle) +
#   theme_bw() +
#   ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 0.95, size = 6) +
#   ggpubr::stat_cor(label.x.npc = 0.5, label.y.npc = 0.95, p.accuracy = 0.01, r.accuracy = 0.01, size = 6) +
#   theme(
#     text = element_text(size = 18)
#     # , legend.position = "top"
#     , legend.position = "none"
#   )
# 
# ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# ggplot2::last_plot()
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


plotSubTitle = sprintf("%s", "제품전후 전력량에 따른 밀도함수")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

makePlot = ggpubr::ggdensity(
  data = dataL1, x = "val", add = "mean", rug = TRUE,
  color = "label", fill = "label", palette = c("#00AFBB", "#E7B800")
) +
  labs(title = NULL, x = "전력 사용량", y = "밀도함수", fill = NULL, color = NULL, subtitle = plotSubTitle) +
  theme(
    text = element_text(size = 18)
    , legend.position = "top"
  )

ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# ggplot2::last_plot()
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

plotSubTitle = sprintf("%s", "제품전후 전력량에 따른 빈도분포")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

makePlot = ggpubr::gghistogram(
  data = dataL1, x = "val", add = "mean", rug = TRUE,
  color = "label", fill = "label", palette = c("#00AFBB", "#E7B800")
) +
  labs(title = NULL, x = "전력 사용량", y = "밀도함수", fill = NULL, color = NULL, subtitle = plotSubTitle) +
  theme(
    text = element_text(size = 18)
    , legend.position = "top"
  )

ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# ggplot2::last_plot()
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


plotSubTitle = sprintf("%s", "제품전후 전력량에 따른 상자그림")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

makePlot = ggpubr::ggboxplot(
  dataL1, x = "label", y = "val", color = "label", palette =c("#00AFBB", "#E7B800"),
  add = "jitter", shape = "label", alpah = 0.01
) +
  ggpubr::stat_compare_means(comparisons = list( c("before", "after")), show.legend = FALSE) +
  labs(title = NULL, x = "전력 사용량", y = "밀도함수", color = NULL, subtitle = plotSubTitle) +
  theme(
    text = element_text(size = 18)
    # , legend.position = "top"
    , legend.position = "none"
  )

ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# ggplot2::last_plot()
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# *****************************************
# 통계 검정
# *****************************************
# F 검정
fTest = var.test(val ~ label, data = dataL1, conf.level = 0.95)
print(fTest)

# F 검정에서 P값은 2.2204e-16로서 유의수준 0.05 이하이기 때문에 귀무가설이 기각되어 두 그룹 간의 분산 차이 (등분산 X)
mainTitle = sprintf("%s", "제품전후 간의 F 검정")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

plot(fTest) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

ggplot2::last_plot()
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# T 검정
# 등분산 가정 O
# tTest = t.test(val ~ label, data = dataL1, conf.level = 0.95, var.equal = TRUE, paired = FALSE)

# 등분산 가정 X
tTest = t.test(val ~ label, data = dataL1, conf.level = 0.95, var.equal = FALSE, paired = FALSE)

# T 검정에서 P값은 2.2204e-16로서 유의수준 0.05 이하이기 때문에 귀무가설이 기각되어 두 그룹 간의 평균 차이
print(tTest)

mainTitle = sprintf("%s", "제품전후 간의 T 검정")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

plot(tTest) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")