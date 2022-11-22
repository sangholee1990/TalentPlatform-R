
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
# R을 이용한 광학 및 화학 미세먼지 시각화 (2D 빈도분포 산점도, 시계열, 박스플롯, 시정예측, 영향도, 3D 시각화)

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0361"

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
library(openxlsx)
# library(fitdistrplus)
library(stats)
library(hydroGOF)
library(RColorBrewer)
library(forcats)
library(ggpubr)
# library(h2o)
library(scales)
library(openxlsx)
library(htmlwidgets)

ggplotDefaultColor = scales::hue_pal()(2)

perfEval = function(x, y) {

  if (length(x) < 1) { return( sprintf("%s", "x 값 없음") ) }
  if (length(y) < 1) { return( sprintf("%s", "y 값 없음") ) }

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

  return( c(slope, interp, xMean, yMean, xSd, ySd, cnt, bias, rBias, rmse, rRmse, r, p, diffMean, diffSd) )
}


cbSpectral = rev(RColorBrewer::brewer.pal(11, "Spectral"))
cbMatlab2 = colorRamps::matlab.like2(11)

#================================================
# 파일 읽기
#================================================
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "IMPROVE 시정 계산_겨울철.xlsx"))
# fileInfo2 = Sys.glob(file.path(globalVar$inpPath, serviceName, "최종수정 및 추가 데이터.xlsx"))
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "미세먼지 화학성분을 이용하여 IMPROVE 시정 계산_220930.xlsx"))
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "LSH0361_data.xlsx"))


# ******************************************************************************
# 화학성분 PM2.5 recon과 총소멸계수
# ******************************************************************************
# data = openxlsx::read.xlsx(fileInfo, sheet = 6)
# data = openxlsx::read.xlsx(fileInfo, sheet = 4)
data = readxl::read_excel(fileInfo, sheet = 4)

dataL1 = data %>% 
  as.tibble() %>%
  dplyr::rename(
    "dtDateTime" = `Date time`
  ) %>% 
  dplyr::mutate(
    # dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
    dtYear = lubridate::year(dtDateTime)
    , dtMonth = lubridate::month(dtDateTime)
    , dtXran = lubridate::decimal_date(dtDateTime)
  )

summary(dataL1)

# colnames(dataL1)
colList = c("dtDateTime", "총소멸계수(Bext)", "SO42-/PM2.5_recon", "NO3-/PM2.5_recon", "Cl-/PM2.5_recon", "OM/PM2.5_recon", "EC/PM2.5_recon", "FS/PM2.5_recon")

dataL2 = dataL1 %>% 
  # dplyr::select(-c("dtDateTime", "sDateTime", "dtYear", "dtMonth", "dtXran", "PM2.5_reconstruct", "To_ext", "NHSO", "NHNO", "SS", "OMC", "EC", "FS")) %>% 
  dplyr::select(colList) %>% 
  tidyr::gather(-dtDateTime, -"총소멸계수(Bext)", key = "key", value = "val") %>% 
  dplyr::mutate(
    type = dplyr::case_when(
      # key == "SO42-/PM2.5_recon" ~ sprintf('SO42/PM["2.5_reconstruct"]')
      key == "SO42-/PM2.5_recon" ~ sprintf('SO[4]^"2-"/PM["2.5_reconstruct"]')
      # , key == "NO3-/PM2.5_recon"~ sprintf('NO3/PM["2.5_reconstruct"]')
      , key == "NO3-/PM2.5_recon"~ sprintf('NO[3]^"-"/PM["2.5_reconstruct"]')
      # , key == "Cl-/PM2.5_recon" ~ sprintf('Cl/PM["2.5_reconstruct"]')
      , key == "Cl-/PM2.5_recon" ~ sprintf('Cl^"-"/PM["2.5_reconstruct"]')
      , key == "OM/PM2.5_recon" ~ sprintf('OM/PM["2.5_reconstruct"]')
      , key == "EC/PM2.5_recon" ~ sprintf('EC/PM["2.5_reconstruct"]')
      , key == "FS/PM2.5_recon" ~ sprintf('FS/PM["2.5_reconstruct"]')
      , TRUE ~ NA_character_
    )
  )

summary(dataL2)

subTitle = sprintf("%s", "총 소멸계수에 따른 PM25 영향")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, subTitle)

ggplot(data = dataL2, aes(x = `총소멸계수(Bext)`, y = val, color = val)) +
  # geom_point(size = 2, show.legend = FALSE) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm', se = TRUE, color = "black") +
  ggpubr::stat_regline_equation(label.x.npc = 0.025, label.y.npc = 1.0, size = 4, aes(label = ..eq.label..), color = "black", parse = TRUE) +
  ggpubr::stat_cor(label.x.npc = 0.025, label.y.npc = 0.90, size = 4, color = "black") +
  scale_color_gradientn(colours = cbMatlab2, limits = c(0, 1), na.value = NA) +
  # scale_x_continuous(minor_breaks = seq(0, 1000, 200), breaks=seq(0, 1000, 200), limits=c(0, 1000)) +
  scale_y_continuous(minor_breaks = seq(0, 1.2, 0.2), breaks=seq(0, 1.2, 0.2), limits=c(0, 1.2)) +
  labs(
    title = NULL
    , x = bquote(B[ext]* '  ['*Mm^-1*']')
    , y = bquote('Chemical  compositions  in  '*PM[2.5*'_'*reconstruct])
    , color = bquote('Chemical  compositions / '*PM[2.5*'_'*reconstruct])
    , fill = NULL
  ) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
    , legend.key.width = unit(2, "cm")
  ) +
  facet_wrap(~type, scale = "free_x", labeller = label_parsed) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

ggplot2::last_plot()

# ******************************************************************************
# 상대습도에 따른 소멸계수 영향 (PM2.5_reconstruct)
# ******************************************************************************
# data = openxlsx::read.xlsx(fileInfo, sheet = 7)
# data = openxlsx::read.xlsx(fileInfo, sheet = 5)
data = readxl::read_excel(fileInfo, sheet = 5)

summary(data)

dataL1 = data %>% 
  as.tibble() %>% 
  dplyr::rename(
    "dtDateTime" = "Date time"
    , "Bext" = "총소멸계수(y축)"
    , "PM2.5_reconstruct" = "PM2.5_reconstruct(색)"
    , "RH" = "RH(x축)"
  ) %>% 
  dplyr::mutate(
    # dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
    dtYear = lubridate::year(dtDateTime)
    , dtMonth = lubridate::month(dtDateTime)
    , dtXran = lubridate::decimal_date(dtDateTime)
  ) 

summary(dataL1)

colList = c("dtDateTime", "Bext", "RH", "PM2.5_reconstruct")

dataL2 = dataL1 %>% 
  dplyr::select(colList) %>%
  # tidyr::gather(-dtDateTime, -"Bext", -RH, key = "key", value = "val") %>%
  na.omit()

# PM2.5_reconstruct
# dataL2$key %>% unique()

summary(dataL2)

subTitle = sprintf("%s", "상대습도에 따른 소멸계수 영향 (PM2.5_reconstruct)")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, subTitle)

lmFor = y ~ poly(x, 2, raw = TRUE)

ggplot(data = dataL2, aes(x = RH, y = Bext, color = PM2.5_reconstruct)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm', formula = lmFor, se = TRUE, color = "black") +
  ggpubr::stat_regline_equation(label.x.npc = 0.075, label.y.npc = 0.96, size = 5.5, aes(label = ..eq.label..), color = "black", formula = lmFor, parse = TRUE) +
  ggpubr::stat_cor(label.x.npc = 0.075, label.y.npc = 0.90, size = 5.5, color = "black") +
  scale_color_gradientn(colours = rev(cbMatlab2), limits = c(0, 100), na.value = cbMatlab2[11]) +
  scale_x_continuous(minor_breaks = seq(20, 100, 20), breaks=seq(20, 100, 20),  limits=c(20, 100)) +
  scale_y_continuous(minor_breaks = seq(0, 1500, 300), breaks=seq(0, 1500, 300), limits=c(0, 1500)) +
  labs(
      # , y = bquote('Reconstructed  ' *b[ext]* '  of  IMPROVE_2005  ['*Mm^-1*']')
    title = NULL
    , x = "Relative  humidity  [%]"
    , y = bquote('B' ['ext'] * '  ['*Mm^-1*']')
    , color = bquote('PM' ['2.5_reconstruction'] * '  ['*mu*g/m^3*']')
    , fill = NULL
    ) +
  theme(
    text = element_text(size = 18)
    # , legend.position = "top"
    , legend.position = c(0, 1), legend.justification = c(0, 0.96)
    , legend.key=element_blank()
    , legend.text=element_text(size=16)
    , legend.background=element_blank()
  ) +
  # facet_wrap(~type, scale = "free") +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

ggplot2::last_plot()

# ******************************************************************************
# PM2.5_reconstruct에 따른 소멸계수 영향
# ******************************************************************************
# data = openxlsx::read.xlsx(fileInfo, sheet = 7)
# data = openxlsx::read.xlsx(fileInfo, sheet = 5)
data = readxl::read_excel(fileInfo, sheet = 5)

dataL1 = data %>% 
  as.tibble() %>% 
  dplyr::rename(
    "dtDateTime" = "Date time"
    , "Bext" = "총소멸계수(y축)"
    , "PM2.5_reconstruct" = "PM2.5_reconstruct(색)"
    , "RH" = "RH(x축)"
  ) %>% 
  dplyr::mutate(
    # dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
    dtYear = lubridate::year(dtDateTime)
    , dtMonth = lubridate::month(dtDateTime)
    , dtXran = lubridate::decimal_date(dtDateTime)
  ) 

summary(dataL1)

colList = c("dtDateTime", "Bext", "RH", "PM2.5_reconstruct")

dataL2 = dataL1 %>% 
  # dplyr::select(-c("Date.time", "sDateTime", "dtYear", "dtMonth", "dtXran")) %>% 
  dplyr::select(colList) %>% 
  # tidyr::gather(-dtDateTime, -"Bext", -RH, key = "key", value = "val") %>% 
  na.omit()

summary(dataL2)

subTitle = sprintf("%s", "PM2.5_reconstruct에 따른 소멸계수 영향")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, subTitle)

lmFor = y ~ poly(x, 2, raw = TRUE)

ggplot(data = dataL2, aes(x = PM2.5_reconstruct, y = Bext, color = RH)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm', formula = lmFor, se = TRUE, color = "black") +
  ggpubr::stat_regline_equation(label.x.npc = 0.075, label.y.npc = 0.96, size = 5.5, aes(label = ..eq.label..), color = "black", formula = lmFor, parse = TRUE) +
  ggpubr::stat_cor(label.x.npc = 0.075, label.y.npc = 0.90, size = 5.5, color = "black") +
  scale_color_gradientn(colours = cbMatlab2, limits = c(0, 100), na.value = cbMatlab2[11]) +
  # scale_x_continuous(minor_breaks = seq(20, 100, 20), breaks=seq(20, 100, 20),  limits=c(20, 100)) +
  # scale_y_continuous(minor_breaks = seq(0, 1600, 400), breaks=seq(0, 1600, 400), limits=c(0, 1600)) +
  labs(
    title = NULL
    # , x = "Relative  humidity  [%]"
    # , y = bquote('Reconstructed  ' *b[ext]* '  of  IMPROVE_2005  ['*Mm^-1*']')
    # , color = bquote('PM' ['2.5']* ' concentration ['*mu*g/m^3*']')
    # , y = bquote('Reconstructec b' ['ext'] * 'of IMS_95 ['*Mm^-1*']')
    , x = bquote('PM' ['2.5_reconstruction'] * '  ['*mu*g/m^3*']')
    , y = bquote('B' ['ext'] * '  ['*Mm^-1*']')
    , color = "Relative  humidity  [%]"
    , fill = NULL
  ) +
  theme(
    text = element_text(size = 18)
    # , legend.position = "top"
    , legend.position = c(0, 1), legend.justification = c(0, 0.96)
    , legend.key=element_blank()
    , legend.text=element_text(size=16)
    , legend.background=element_blank()
  ) +
  # facet_wrap(~type, scale = "free") +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

ggplot2::last_plot()

# ******************************************************************************
# 3D 시각화
# ******************************************************************************
# data = openxlsx::read.xlsx(fileInfo, sheet = 7)
# data = openxlsx::read.xlsx(fileInfo, sheet = 5)
data = readxl::read_excel(fileInfo, sheet = 5)

dataL1 = data %>% 
  as.tibble() %>% 
  dplyr::rename(
    "dtDateTime" = "Date time"
    , "Bext" = "총소멸계수(y축)"
    , "PM2.5_reconstruct" = "PM2.5_reconstruct(색)"
    , "RH" = "RH(x축)"
  ) %>% 
  dplyr::mutate(
    # dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
    dtYear = lubridate::year(dtDateTime)
    , dtMonth = lubridate::month(dtDateTime)
    , dtXran = lubridate::decimal_date(dtDateTime)
  ) 

summary(dataL1)

colList = c("dtDateTime", "Bext", "RH", "PM2.5_reconstruct")

dataL2 = dataL1 %>% 
  # dplyr::select(-c("Date.time", "sDateTime", "dtYear", "dtMonth", "dtXran")) %>% 
  dplyr::select(colList) %>% 
  # tidyr::gather(-dtDateTime, -"Bext", -RH, key = "key", value = "val") %>% 
  na.omit()

summary(dataL2)

# 상대습도 및 소멸계수에 따른 3D PM2.5_reconstruct 시각화
makePlot3D = plotly::plot_ly(
  type = "mesh3d"
  , data = dataL2
  , x = ~RH
  , y = ~Bext
  , z = ~PM2.5_reconstruct
  , intensity = ~PM2.5_reconstruct
  , colors = cbMatlab2
  , showlegend = FALSE
  , colorbar = list(title = NULL)
  ) %>% 
  plotly::layout(
    title = NULL
    , scene = list(
      xaxis = list(title = "Relative humidity [%]")
      , yaxis = list(title = "b<sub>ext</sub> of IMPROVE<sub>2005</sub> [Mm<sup>-1</sup>]")
      , zaxis = list(title = "PM<sub>2.5</sub> concentration [µg/m<sup>3</sup>]")
    ) 
    , autosize = TRUE
  )

makePlot3D

subTitle = sprintf("%s", "상대습도 및 소멸계수에 따른 3D PM2.5_reconstruct 시각화")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, subTitle)
saveHtml = sprintf("%s/%s/%s.html", globalVar$figPath, serviceName, subTitle)
tmpImg = tempfile(fileext = ".png")
tmpHtml= tempfile(fileext = ".html")

# html 저장
htmlwidgets::saveWidget(makePlot3D, file = tmpHtml)
fs::file_move(tmpHtml, saveHtml)




# 상대습도 및 PM2.5_reconstruct에 따른 3D 소멸계수 시각화
makePlot3D = plotly::plot_ly(
  type = "mesh3d"
  , data = dataL2
  , x = ~RH
  , y = ~PM2.5_reconstruct
  , z = ~Bext
  , intensity = ~Bext
  , colors = cbMatlab2
  , showlegend = FALSE
  , colorbar = list(title = NULL)
) %>% 
  plotly::layout(
    title = NULL
    , scene = list(
      xaxis = list(title = "Relative humidity [%]")
      , yaxis = list(title = "PM<sub>2.5</sub> concentration [µg/m<sup>3</sup>]")
      , zaxis = list(title = "b<sub>ext</sub> of IMPROVE<sub>2005</sub> [Mm<sup>-1</sup>]")
    ) 
    , autosize = TRUE
  )

makePlot3D

subTitle = sprintf("%s", "상대습도 및 PM2.5_reconstruct에 따른 3D 소멸계수 시각화")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, subTitle)
saveHtml = sprintf("%s/%s/%s.html", globalVar$figPath, serviceName, subTitle)
tmpImg = tempfile(fileext = ".png")
tmpHtml= tempfile(fileext = ".html")

# html 저장
htmlwidgets::saveWidget(makePlot3D, file = tmpHtml)
fs::file_move(tmpHtml, saveHtml)


# ******************************************************************************
# PM2.5에 따른 Visibility 산점도 시각화
# PM2.5_recon에 따른 Visibility 산점도 시각화
# 2D 빈도분포 산점도 (PM2.5_recon vs PM2.5)
# 전체 기간에 대한 미세먼지 시계열 (PM2.5, PM2.5_reconstruct)
# ******************************************************************************
# data = openxlsx::read.xlsx(fileInfo, sheet = 5)
data = readxl::read_excel(fileInfo, sheet = 6)

dataL1 = data %>% 
  as.tibble() %>% 
  dplyr::rename(
    "dtDateTime" = "Date time"
    # , "Bext" = "총소멸계수(y축)"
    # , "PM2.5_reconstruct" = "PM2.5_reconstruct(색)"
    # , "RH" = "RH(x축)"
  ) %>% 
  dplyr::mutate(
    # dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
    dtYear = lubridate::year(dtDateTime)
    , dtMonth = lubridate::month(dtDateTime)
    , dtXran = lubridate::decimal_date(dtDateTime)
  ) %>% 
  dplyr::filter(
    PM2.5 < 95
  )

summary(dataL1)

dataL2 = dataL1 %>% 
  dplyr::select(dtDateTime, PM2.5, PM2.5_recon) %>% 
  tidyr::gather(-dtDateTime, key = "key", value = "val")

# PM2.5에 따른 Visibility 산점도 시각화
plotSubTitle = sprintf("%s", "PM2.5에 따른 Visibility 산점도 시각화")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

lmFor = y ~ poly(x, 6, raw = TRUE)

ggplot(data = dataL1, aes(x = PM2.5, y = Visibility, color = Visibility)) +
  # coord_fixed(ratio = 1) +
  theme_bw() +
  # stat_bin2d(aes(xAxis, yAxis), binwidth = c(2, 2)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm', formula = lmFor, se = TRUE, color = "black") +
  ggpubr::stat_regline_equation(label.x.npc = 0.075, label.y.npc = 0.96, size = 5, aes(label = ..eq.label..), color = "black", formula = lmFor, parse = TRUE) +
  ggpubr::stat_cor(label.x.npc = 0.075, label.y.npc = 0.90, size = 5, color = "black") +
  scale_color_gradientn(colours = cbMatlab2, limits = c(0, 300), na.value = NA) +
  scale_x_continuous(minor_breaks = seq(0, 100, 20), breaks=seq(0, 100, 20),  limits=c(0, 100)) +
  scale_y_continuous(minor_breaks = seq(0, 400, 100), breaks=seq(0, 400, 100), limits=c(0, 400)) +
  labs(
    title = NULL
    , x = bquote('PM' ['2.5'] * ' Concentration ['*mu*g/m^3*']')
    , y = "Visibility [km]"
    # , x = bquote('PM' ['2.5_reconstruct'] * ' Concentration ['*mu*g/m^3*']')
    # , y = bquote('Reconstructec b' ['ext'] * 'of IMS_95 ['*Mm^-1*']')
    # , y = "PM2.5_reconstruct"
    , colour = "Visibility"
    , fill = NULL
  ) +
  theme(
    text = element_text(size = 16)
    , legend.position = c(0, 1), legend.justification = c(0, 0.96)
    , legend.key=element_blank()
    # , legend.text=element_text(ssize=14)
    , legend.background=element_blank()
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

ggplot2::last_plot()



# PM2.5_recon에 따른 Visibility 산점도 시각화
plotSubTitle = sprintf("%s", "PM2.5_recon에 따른 Visibility 산점도 시각화")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

lmFor = y ~ poly(x, 6, raw = TRUE)

ggplot(data = dataL1, aes(x = PM2.5_recon, y = Visibility, color = Visibility)) +
  # coord_fixed(ratio = 1) +
  theme_bw() +
  # stat_bin2d(aes(xAxis, yAxis), binwidth = c(2, 2)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm', formula = lmFor, se = TRUE, color = "black") +
  ggpubr::stat_regline_equation(label.x.npc = 0.075, label.y.npc = 0.96, size = 5, aes(label = ..eq.label..), color = "black", formula = lmFor, parse = TRUE) +
  ggpubr::stat_cor(label.x.npc = 0.075, label.y.npc = 0.90, size = 5, color = "black") +
  scale_color_gradientn(colours = cbMatlab2, limits = c(0, 300), na.value = NA) +
  scale_x_continuous(minor_breaks = seq(0, 100, 20), breaks=seq(0, 100, 20),  limits=c(0, 100)) +
  scale_y_continuous(minor_breaks = seq(0, 400, 100), breaks=seq(0, 400, 100), limits=c(0, 400)) +
  labs(
    title = NULL
    # , x = bquote('PM' ['2.5'] * ' Concentration ['*mu*g/m^3*']')
    , x = bquote('PM' ['2.5_reconstruct'] * ' Concentration ['*mu*g/m^3*']')
    , y = "Visibility [km]"
    # , y = bquote('Reconstructec b' ['ext'] * 'of IMS_95 ['*Mm^-1*']')
    # , y = "PM2.5_reconstruct"
    , colour = "Visibility"
    , fill = NULL
  ) +
  theme(
    text = element_text(size = 16)
    , legend.position = c(0, 1), legend.justification = c(0, 0.96)
    , legend.key=element_blank()
    # , legend.text=element_text(size=16)
    , legend.background=element_blank()
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

ggplot2::last_plot()


# 2D 빈도분포 산점도 (PM2.5_recon vs PM2.5)
plotSubTitle = sprintf("%s", "2D 빈도분포 산점도 (PM2.5_recon vs PM2.5)")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

xAxis = dataL1$PM2.5_recon
yAxis = dataL1$PM2.5

xcord = 15
# ycord = seq(1600, 0, -85)
ycord = seq(100, 0, -5.5)
# ycord = seq(100, 0, -4.0)
cbSpectral = rev(RColorBrewer::brewer.pal(11, "Spectral"))

perfEvalInfo = perfEval(xAxis, yAxis)
sprintf("%.3f", perfEvalInfo)

ggplot() +
  coord_fixed(ratio = 1) +
  theme_bw() +
  stat_bin2d(aes(xAxis, yAxis), binwidth = c(2, 2)) +
  scale_fill_gradientn(colours = cbSpectral, limits = c(0, 80), na.value = NA) +
  annotate("text", x = xcord, y = ycord[1], label = paste0("Y = ", sprintf("%.2f", perfEvalInfo[1])," X + ", sprintf("%.2f", perfEvalInfo[2])), size = 5.5, hjust = 0, color = "red") +
  annotate("text", x = xcord, y = ycord[2], label = paste0("R = ", sprintf("%.2f", perfEvalInfo[12]), " (p-value < ", sprintf("%.3f", perfEvalInfo[13]), ")"), size = 5.5, hjust = 0, color = "red") +
  annotate("text", x = xcord, y = ycord[3], label = paste0("Bias = ", sprintf("%.2f", perfEvalInfo[8]), " (", sprintf("%.2f", perfEvalInfo[9])," %)"), parse = FALSE, size = 5.5, hjust = 0) +
  annotate("text", x = xcord, y = ycord[4], label = paste0("RMSE = ", sprintf("%.2f", perfEvalInfo[10]), " (", sprintf("%.2f", perfEvalInfo[11])," %)"), parse = FALSE, size = 5.5, hjust = 0) +
  annotate("text", x = xcord, y = ycord[5], label=paste0("N = ", format(perfEvalInfo[7], big.mark = ",", scientific = FALSE)), size = 5.5, hjust = 0, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
  stat_smooth(aes(xAxis, yAxis), method = "lm", color = "red", se = FALSE) +
  # scale_x_continuous(minor_breaks = seq(0, 160, 20), breaks=seq(0, 160, 20),  expand=c(0,0), limits=c(0,  160)) +
  # scale_y_continuous(minor_breaks = seq(0, 1600, 200), breaks=seq(0, 1600, 200),  expand=c(0,0), limits=c(0,  1600)) +
  scale_x_continuous(minor_breaks = seq(0, 100, 20), breaks=seq(0, 100, 20), limits=c(0, 100)) +
  scale_y_continuous(minor_breaks = seq(0, 100, 20), breaks=seq(0, 100, 20), limits=c(0, 100)) +
  labs(
    title = NULL
    , x = bquote('PM' ['2.5'] * ' Concentration ['*mu*g/m^3*']')
    , y = bquote('PM' ['2.5_reconstruct'] * ' Concentration ['*mu*g/m^3*']')
    , y = bquote('Reconstructec b' ['ext'] * 'of IMS_95 ['*Mm^-1*']')
    # , x = "PM2.5"
    # , y = "PM2.5_reconstruct"
    , fill = NULL
  ) +
    theme(
      text = element_text(size = 18)
      , legend.position = c(0, 1), legend.justification = c(0, 0.96)
      , legend.key=element_blank()
      , legend.background=element_blank()
    ) +
    ggsave(filename = saveImg, width = 6, height = 6, dpi = 600)

ggplot2::last_plot()


# 전체 기간에 대한 미세먼지 시계열 (PM2.5, PM2.5_reconstruct)
plotSubTitle = sprintf("%s", "전체 기간에 대한 미세먼지 시계열 (PM2.5, PM2.5_reconstruct)")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

# 정렬
dataL2$key = forcats::fct_relevel(dataL2$key, c("PM2.5", "PM2.5_recon"))

ggplot(data = dataL2, aes(x = dtDateTime, y = val, color = key)) +
  geom_line() +
  labs(title = NULL, x = "Date [Year-Month-Day]", y = bquote('Concentration  ['*µg/m^3*']'), colour = NULL, fill = NULL, subtitle = plotSubTitle) +
  scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  scale_y_continuous(minor_breaks = seq(0, 100, 20), breaks=seq(0, 100, 20), limits=c(0,  100)) +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

ggplot2::last_plot()


# ******************************************************************************
# PM2.5에 따른 Visibility 산점도 시각화
# PM2.5_recon에 따른 Visibility 산점도 시각화
# 2D 빈도분포 산점도 (PM2.5_recon vs PM2.5)
# 전체 기간에 대한 미세먼지 시계열 (PM2.5, PM2.5_reconstruct)
# ******************************************************************************
# data = openxlsx::read.xlsx(fileInfo, sheet = 5)
data = readxl::read_excel(fileInfo, sheet = 6)

dataL1 = data %>% 
  as.tibble() %>% 
  dplyr::rename(
    "dtDateTime" = "Date time"
  ) %>% 
  dplyr::mutate(
    # dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
    dtYear = lubridate::year(dtDateTime)
    , dtMonth = lubridate::month(dtDateTime)
    , dtXran = lubridate::decimal_date(dtDateTime)
    # , dtDoy = format(dtDateTime, "%j") %>% as.numeric()
    , season = dplyr::case_when(
      dtMonth == 1 | dtMonth == 2 | dtMonth == 12 ~ "Winter"
      , dtMonth == 3 | dtMonth == 4 | dtMonth == 5 ~ "Spring"
      , dtMonth == 6 | dtMonth == 7 | dtMonth == 8 ~ "Summer"
      , dtMonth == 9 | dtMonth == 10 | dtMonth == 11  ~ "Autumn"
      )
    )

# dataL2 = dataL1 %>%
#   dplyr::select(dtMonth, PM2.5, PM2.5_recon) %>%
#   tidyr::gather(-dtMonth, key = "key", value = "val") %>%
#   dplyr::group_by(dtMonth, key) %>%
#   dplyr::summarise(
#     meanVal = mean(val, na.rm = TRUE)
#     , sdVal = sd(val, na.rm = TRUE)
#   )

dataL2 = dataL1 %>%
  dplyr::select(dtYear, dtMonth, PM2.5, PM2.5_recon) %>%
  tidyr::gather(-dtYear, -dtMonth, key = "key", value = "val") %>%
  dplyr::group_by(dtYear, dtMonth, key) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
    , sdVal = sd(val, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    tmpDate = stringr::str_c(dtYear, dtMonth, 1, sep = "-")
    , dtDate = readr::parse_date(tmpDate, "%Y-%m-%d")
  )



# 월별 평균 미세먼지 점선 그래프
plotSubTitle = sprintf("%s", "월별 평균 미세먼지 점선 그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggplot(data = dataL2, aes(x = dtDate, y = meanVal, color = key, group = key)) +
#   geom_point(position = position_dodge(0.5), size = 2) +
  geom_point(position = position_dodge(20), size = 2) +
  # geom_errorbar(width = 0.25, aes(ymin=meanVal - sdVal, ymax=meanVal + sdVal, group = key), position = position_dodge(0.5)) +
  geom_errorbar(width = 10, aes(ymin=meanVal - sdVal, ymax=meanVal + sdVal, group = key), position = position_dodge(20)) +
  labs(title = NULL, x = "Month", y = bquote('Concentration  ['*µg/m^3*']'), colour = NULL, fill = NULL, subtitle = plotSubTitle) +
  # scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  # scale_x_continuous(minor_breaks = seq(1, 12, 1), breaks=seq(1, 12, 1), limits=c(1,  12)) +
  scale_x_date(date_breaks = "1 months", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), paste(month(x, label = TRUE), "\n", year(x)), paste(month(x, label = TRUE)))) +
  scale_y_continuous(minor_breaks = seq(0, 60, 10), breaks=seq(0, 60, 10), limits=c(0, 60)) +
  scale_color_discrete(labels = c("PM2.5" =  bquote('PM' ['2.5']), "PM2.5_recon" =  bquote('PM' ['2.5_reconstruct']))) +
  theme(
    text = element_text(size = 18)
    # , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

ggplot2::last_plot()

# 월별 평균 미세먼지 막대 그래프
plotSubTitle = sprintf("%s", "월별 평균 미세먼지 막대 그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggplot(data = dataL2, aes(x = dtDate, y = meanVal, color = key, fill = key, group = key)) +
  geom_bar(stat = "identity", width = 15, position=position_dodge(width = 20), fill = "white") +
  geom_errorbar(width = 10, aes(ymin=meanVal - sdVal, ymax=meanVal + sdVal, group = key), position = position_dodge(20)) +
  # geom_bar(stat = "identity",  width = 0.4, position=position_dodge(width = 0.5)) +
  labs(title = NULL, x = "Month", y = bquote('Concentration  ['*µg/m^3*']'), colour = NULL, fill = NULL, subtitle = plotSubTitle) +
  # scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  # scale_x_continuous(minor_breaks = seq(1, 12, 1), breaks=seq(1, 12, 1), limits=c(1, 12)) +
  scale_x_date(date_breaks = "1 months", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), paste(month(x, label = TRUE), "\n", year(x)), paste(month(x, label = TRUE)))) +
  scale_y_continuous(minor_breaks = seq(0, 60, 10), breaks=seq(0, 60, 10), limits=c(0, 60)) +
  scale_color_discrete(labels = c("PM2.5" =  bquote('PM' ['2.5']), "PM2.5_recon" =  bquote('PM' ['2.5_reconstruct']))) +
  theme(
    text = element_text(size = 18)
    # , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

ggplot2::last_plot()

dataL3 = dataL1 %>% 
  dplyr::select(season, PM2.5, PM2.5_recon) %>%
  tidyr::gather(-season, key = "key", value = "val") %>%
  dplyr::group_by(season, key) %>%
  dplyr::summarise(
    maenVal = mean(val, na.rm = TRUE)
    , sdVal = sd(val, na.rm = TRUE)
  )

# 정렬
dataL3$season = forcats::fct_relevel(dataL3$season, c("Spring", "Summer", "Autumn", "Winter"))

# 계절별 평균 미세먼지 점선 그래프
plotSubTitle = sprintf("%s", "계절별 평균 미세먼지 점선 그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggplot(data = dataL3, aes(x = season, y = maenVal, color = key, group = key)) +
  geom_point(position = position_dodge(0.5), size = 2) +
   geom_errorbar(width = 0.25, aes(ymin=maenVal - sdVal, ymax=maenVal + sdVal, group = key), position = position_dodge(0.5)) +
  labs(title = NULL, x = "Season", y = bquote('Concentration  ['*µg/m^3*']'), colour = NULL, fill = NULL, subtitle = plotSubTitle) +
  # scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  # scale_x_continuous(minor_breaks = seq(1, 12, 1), breaks=seq(1, 12, 1), limits=c(1,  12)) +
  scale_y_continuous(minor_breaks = seq(0, 50, 10), breaks=seq(0, 50, 10), limits=c(0, 50)) +
  scale_color_discrete(labels = c("PM2.5" =  bquote('PM' ['2.5']), "PM2.5_recon" =  bquote('PM' ['2.5_reconstruct']))) +
  theme(
    text = element_text(size = 18)
    # , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

ggplot2::last_plot()

# 계절별 평균 미세먼지 막대 그래프
plotSubTitle = sprintf("%s", "계절별 평균 미세먼지 막대 그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggplot(data = dataL3, aes(x = season, y = maenVal, pattern = key, color = key, group = key)) +
  geom_bar(stat = "identity", width = 0.4, position=position_dodge(width = 0.5), fill = "white") +
  # geom_bar_pattern(stat = "identity", width = 0.4, position=position_dodge(width = 0.5)) +
  # geom_errorbar(aes(ymin=maenVal - sdVal, ymax=maenVal + sdVal), width=.2,  position=position_dodge(.9)) +
  geom_errorbar(width = 0.25, aes(ymin=maenVal - sdVal, ymax=maenVal + sdVal, group = key), position = position_dodge(0.5)) +
  labs(title = NULL, x = "Season", y = bquote('Concentration  ['*µg/m^3*']'), colour = NULL, fill = NULL, subtitle = plotSubTitle) +
  # scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  # scale_x_continuous(minor_breaks = seq(1, 12, 1), breaks=seq(1, 12, 1), limits=c(1,  12)) +
  scale_y_continuous(minor_breaks = seq(0, 50, 10), breaks=seq(0, 50, 10), limits=c(0, 50)) +
  scale_color_discrete(labels = c("PM2.5" =  bquote('PM' ['2.5']), "PM2.5_recon" =  bquote('PM' ['2.5_reconstruct']))) +
  theme(
    text = element_text(size = 18)
    # , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

ggplot2::last_plot()


# ******************************************************************************
# 계절별 평균 화학성분 막대 그래프
# ******************************************************************************
data = readxl::read_excel(fileInfo, sheet = 4)

dataL1 = data %>%
  as.tibble() %>%
  dplyr::rename(
    "dtDateTime" = "Date time"
    , "SO42-" = "Sulfate\r\n(SO42-)"
    , "NO3-" = "Nitrate\r\n(NO3-)"
  ) %>%
  dplyr::select(c("dtDateTime", "SO42-", "NO3-", "Cl-", "OM", "EC", "FS")) %>%
  dplyr::mutate(
    # dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
    dtYear = lubridate::year(dtDateTime)
    , dtMonth = lubridate::month(dtDateTime)
    , dtXran = lubridate::decimal_date(dtDateTime)
    # , dtDoy = format(dtDateTime, "%j") %>% as.numeric()
    , season = dplyr::case_when(
      dtMonth == 1 | dtMonth == 2 | dtMonth == 12 ~ "Winter"
      , dtMonth == 3 | dtMonth == 4 | dtMonth == 5 ~ "Spring"
      , dtMonth == 6 | dtMonth == 7 | dtMonth == 8 ~ "Summer"
      , dtMonth == 9 | dtMonth == 10 | dtMonth == 11 ~ "Autumn"
    )
  )

dataL3 = dataL1 %>%
  dplyr::select(c("season", "SO42-", "NO3-", "Cl-", "OM", "EC", "FS")) %>%
  tidyr::gather(-season, key = "key", value = "val") %>%
  dplyr::group_by(season, key) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
    , sdVal = sd(val, na.rm = TRUE)
    , minMeanSdVal = meanVal - sdVal
    , maxMeanSdVal = meanVal + sdVal
  )

# 정렬
dataL3$season = forcats::fct_relevel(dataL3$season, c("Spring", "Summer", "Autumn", "Winter"))

# 계절별 평균 미세먼지 막대 그래프
plotSubTitle = sprintf("%s", "계절별 평균 화학성분 막대 그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

# dataL3$key %>% unique()

ggplot(data = dataL3, aes(x = key, y = meanVal, color = key, group = key)) +
  geom_bar(stat = "identity", width = 0.5, position=position_dodge(width = 0.5), fill = "white", show.legend = FALSE) +
  # geom_bar_pattern(stat = "identity", width = 0.4, position=position_dodge(width = 0.5)) +
  # geom_errorbar(aes(ymin=maenVal - sdVal, ymax=maenVal + sdVal), width=.2,  position=position_dodge(.9)) +
  geom_errorbar(width = 0.3, aes(ymin=meanVal - sdVal, ymax=meanVal + sdVal, group = key), position = position_dodge(0.5), show.legend = FALSE) +
  labs(title = NULL, x = "Season", y = bquote('Concentration  ['*µg/m^3*']'), colour = NULL, fill = NULL, subtitle = plotSubTitle) +
  # scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  # scale_x_continuous(minor_breaks = seq(1, 12, 1), breaks=seq(1, 12, 1), limits=c(1,  12)) +
  # scale_y_continuous(minor_breaks = seq(-0.1, 20, 5), breaks=seq(-0.5, 20, 5), limits=c(-0.5, 20)) +
  scale_y_continuous(limits=c(-1.36, 20)) +
  scale_x_discrete(labels = c("Cl-" =  bquote(CI^'-'), "SO42-" =  bquote(SO[4]^'2-'), "NO3-" = bquote(NO[3]^'-'))) +
  theme(
    text = element_text(size = 18)
    # , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  # facet_wrap(~season, scale = "free_x") +
  facet_wrap(~season) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

ggplot2::last_plot()




# ******************************************************************************
# PM2.5_reconstruct에 따른 소멸계수 영향-상대습도 특성
# ******************************************************************************
data = readxl::read_excel(fileInfo, sheet = 5)

dataL1 = data %>%
  as.tibble() %>%
  dplyr::rename(
    "dtDateTime" = "Date time"
    , "Bext" = "총소멸계수(y축)"
    , "PM2.5_reconstruct" = "PM2.5_reconstruct(색)"
    , "RH" = "RH(x축)"
  ) %>%
  dplyr::mutate(
    # dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
    dtYear = lubridate::year(dtDateTime)
    , dtMonth = lubridate::month(dtDateTime)
    , dtXran = lubridate::decimal_date(dtDateTime)
    , type = dplyr::case_when(
      RH <= 23 ~ "23% ≤ RH"
      , 23 < RH & RH <= 40 ~ "23% < RH ≤ 40%"
      , 40 < RH & RH <= 60 ~ "40% < RH ≤ 60%"
      , 60 < RH & RH <= 80 ~ "60% < RH ≤ 80%"
      , 80 < RH & RH <= 90 ~ "80% < RH ≤ 90%"
      , 90 < RH & RH <= 100 ~ "90% < RH ≤ 100%"
      , TRUE ~ NA_character_
    )
  )

summary(dataL1)

colList = c("dtDateTime", "Bext", "RH", "PM2.5_reconstruct", "type")
dataL2 = dataL1 %>%
  dplyr::select(colList) %>%
  na.omit()

summary(dataL2)

dataL2$type = factor(dataL2$type)

subTitle = sprintf("%s", "PM2.5_reconstruct에 따른 소멸계수 영향-상대습도 특성")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, subTitle)

lmFor = y ~ poly(x, 2, raw = TRUE)

ggpubr::ggscatter(dataL2, x = "PM2.5_reconstruct", y = "Bext", color = "type", conf.int = FALSE, cor.coef = FALSE, add.params = list(color = "black", fill = "lightgray")) +
  geom_smooth(aes(color = type), method = 'lm', formula = lmFor, se = TRUE, show.legend = FALSE) +
  ggpubr::stat_regline_equation(aes(color = type), method = 'lm', formula = lmFor, parse = TRUE, label.x.npc = 0.01, label.y.npc = 1.0, size = 5, show.legend = FALSE) +
  ggpubr::stat_cor(aes(color = type, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), formula = lmFor, parse = TRUE, label.x.npc = 0.4, label.y.npc = 1.0, p.accuracy  =  0.01,  r.accuracy  =  0.01, size = 5, show.legend = FALSE) +
  theme_bw() +
    labs(
    title = NULL
    , x = bquote('PM' ['2.5_reconstruction'] * '  ['*mu*g/m^3*']')
    , y = bquote('B' ['ext'] * '  ['*Mm^-1*']')
    , color = "Relative  humidity  [%]"
    , fill = NULL
  ) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)



# ******************************************************************************
# PM2.5_recon에 따른 Visibility 산점도 시각화-상대습도 특성
# PM2.5에 따른 Visibility 산점도 시각화-상대습도 특성
# ******************************************************************************
inpData = readxl::read_excel(fileInfo, sheet = 5)
inpData2 = readxl::read_excel(fileInfo, sheet = 6)

data = inpData %>%
   dplyr::left_join(inpData2, by = c("Date time" = "Date time"))

dataL1 = data %>%
  as.tibble() %>%
  dplyr::rename(
    "dtDateTime" = "Date time"
    , "Bext" = "총소멸계수(y축)"
    , "PM2.5_reconstruct" = "PM2.5_reconstruct(색)"
    , "RH" = "RH(x축)"
  ) %>%
  dplyr::mutate(
    # dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
    dtYear = lubridate::year(dtDateTime)
    , dtMonth = lubridate::month(dtDateTime)
    , dtXran = lubridate::decimal_date(dtDateTime)
    , type = dplyr::case_when(
      RH <= 23 ~ "23% ≤ RH"
      , 23 < RH & RH <= 40 ~ "23% < RH ≤ 40%"
      , 40 < RH & RH <= 60 ~ "40% < RH ≤ 60%"
      , 60 < RH & RH <= 80 ~ "60% < RH ≤ 80%"
      , 80 < RH & RH <= 90 ~ "80% < RH ≤ 90%"
      , 90 < RH & RH <= 100 ~ "90% < RH ≤ 100%"
      , TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(
    PM2.5 < 95
  )

summary(dataL1)


colList = c("PM2.5", "PM2.5_recon", "type", "Visibility")

dataL2 = dataL1 %>%
  dplyr::select(colList) %>%
  na.omit()

dataL2$type = factor(dataL2$type)

# PM2.5_recon에 따른 Visibility 산점도 시각화-상대습도 특성
plotSubTitle = sprintf("%s", "PM2.5_recon에 따른 Visibility 산점도 시각화-상대습도 특성")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

lmFor = y ~ poly(x, 2, raw = TRUE)

ggpubr::ggscatter(dataL2, x = "PM2.5_recon", y = "Visibility", color = "type", conf.int = FALSE, cor.coef = FALSE, add.params = list(color = "black", fill = "lightgray")) +
  geom_smooth(aes(color = type), method = "lm", formula = lmFor, se = FALSE, show.legend = FALSE) +
  ggpubr::stat_regline_equation(aes(color = type), method = "lm", formula = lmFor, parse = TRUE, label.x.npc = 0.05, label.y.npc = 1.0, size = 5, show.legend = FALSE) +
  ggpubr::stat_cor(aes(color = type, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x.npc = 0.5, label.y.npc = 1.0, p.accuracy  =  0.01,  r.accuracy  =  0.01, size = 5, show.legend = FALSE) +
  theme_bw() +
    labs(
    title = NULL
    , x = bquote('PM' ['2.5_reconstruct'] * ' Concentration ['*mu*g/m^3*']')
    , y = "Visibility [km]"
    , colour = "Visibility"
    , fill = NULL
  ) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)



# PM2.5에 따른 Visibility 산점도 시각화-상대습도 특성
plotSubTitle = sprintf("%s", "PM2.5에 따른 Visibility 산점도 시각화-상대습도 특성")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

lmFor = y ~ poly(x, 2, raw = TRUE)

ggpubr::ggscatter(dataL2, x = "PM2.5", y = "Visibility", color = "type", conf.int = FALSE, cor.coef = FALSE, add.params = list(color = "black", fill = "lightgray")) +
  geom_smooth(aes(color = type), method = "lm", formula = lmFor, se = FALSE, show.legend = FALSE) +
  ggpubr::stat_regline_equation(aes(color = type), method = "lm", formula = lmFor, parse = TRUE, label.x.npc = 0.1, label.y.npc = 1.0, size = 5, show.legend = FALSE) +
  ggpubr::stat_cor(aes(color = type, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x.npc = 0.5, label.y.npc = 1.0, p.accuracy  =  0.01,  r.accuracy  =  0.01, size = 5, show.legend = FALSE) +
  theme_bw() +
    labs(
    title = NULL
    , x = bquote('PM' ['2.5'] * ' Concentration ['*mu*g/m^3*']')
    , y = "Visibility [km]"
    , colour = "Visibility"
    , fill = NULL
  ) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)



# # ******************************************************************************
# # PM2.5에 따른 Visibility 산점도 시각화
# # PM2.5_recon에 따른 Visibility 산점도 시각화
# # 2D 빈도분포 산점도 (PM2.5_recon vs PM2.5)
# # 전체 기간에 대한 미세먼지 시계열 (PM2.5, PM2.5_reconstruct)
# # ******************************************************************************
# # data = openxlsx::read.xlsx(fileInfo, sheet = 5)
# data = readxl::read_excel(fileInfo, sheet = 6)
#
# dataL1 = data %>%
#   as.tibble() %>%
#   dplyr::rename(
#     "dtDateTime" = "Date time"
#   ) %>%
#   dplyr::mutate(
#     # dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
#     dtYear = lubridate::year(dtDateTime)
#     , dtMonth = lubridate::month(dtDateTime)
#     , dtXran = lubridate::decimal_date(dtDateTime)
#     # , dtDoy = format(dtDateTime, "%j") %>% as.numeric()
#     , season = dplyr::case_when(
#       dtMonth == 1 | dtMonth == 2 | dtMonth == 12 ~ "Winter"
#       , dtMonth == 3 | dtMonth == 4 | dtMonth == 5 ~ "Spring"
#       , dtMonth == 6 | dtMonth == 7 | dtMonth == 8 ~ "Summer"
#       , dtMonth == 9 | dtMonth == 10 | dtMonth == 11  ~ "Autumn"
#       )
#     )
#
# # summary(dataL1)
#
# dataL2 = dataL1 %>%
#   dplyr::group_by(dtMonth) %>%
#   dplyr::summarise(
#     PM2.5 = mean(PM2.5, na.rm = TRUE)
#      ,  PM2.5_recon = mean(PM2.5_recon, na.rm = TRUE)
#   ) %>%
#   tidyr::gather(-dtMonth, key = "key", value = "val")
#
# # 월별 평균 미세먼지 점선 그래프
# plotSubTitle = sprintf("%s", "월별 평균 미세먼지 점선 그래프")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
#
# ggplot(data = dataL2, aes(x = dtMonth, y = val, color = key, group = key)) +
#   geom_point() +
#   geom_line() +
#   labs(title = NULL, x = "Month", y = bquote('Concentration  ['*µg/m^3*']'), colour = NULL, fill = NULL, subtitle = plotSubTitle) +
#   # scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
#   scale_x_continuous(minor_breaks = seq(1, 12, 1), breaks=seq(1, 12, 1), limits=c(1,  12)) +
#   scale_y_continuous(minor_breaks = seq(0, 40, 10), breaks=seq(0, 40, 10), limits=c(0,  40)) +
#   theme(
#     text = element_text(size = 18)
#     # , axis.text.x = element_text(angle = 45, hjust = 1)
#     , legend.position = "top"
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
#
# # 월별 평균 미세먼지 막대 그래프
# plotSubTitle = sprintf("%s", "월별 평균 미세먼지 막대 그래프")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
#
# ggplot(data = dataL2, aes(x = dtMonth, y = val, color = key, fill = key)) +
#   geom_bar(stat = "identity",  width = 0.4, position=position_dodge(width = 0.5)) +
#   labs(title = NULL, x = "Month", y = bquote('Concentration  ['*µg/m^3*']'), colour = NULL, fill = NULL, subtitle = plotSubTitle) +
#   # scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
#   scale_x_continuous(minor_breaks = seq(1, 12, 1), breaks=seq(1, 12, 1), limits=c(1,  12)) +
#   scale_y_continuous(minor_breaks = seq(0, 40, 10), breaks=seq(0, 40, 10), limits=c(0,  40)) +
#   theme(
#     text = element_text(size = 18)
#     # , axis.text.x = element_text(angle = 45, hjust = 1)
#     , legend.position = "top"
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
#
#
# dataL3 = dataL1 %>%
#   dplyr::group_by(season) %>%
#   dplyr::summarise(
#     PM2.5 = mean(PM2.5, na.rm = TRUE)
#     ,  PM2.5_recon = mean(PM2.5_recon, na.rm = TRUE)
#   ) %>%
#   tidyr::gather(-season, key = "key", value = "val")
#
# # 정렬
# dataL3$season = forcats::fct_relevel(dataL3$season, c("Spring", "Summer", "Autumn", "Winter"))
#
# # 계절별 평균 미세먼지 점선 그래프
# plotSubTitle = sprintf("%s", "계절별 평균 미세먼지 점선 그래프")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
#
# ggplot(data = dataL3, aes(x = season, y = val, color = key, group = key)) +
#   geom_point() +
#   geom_line() +
#   labs(title = NULL, x = "Season", y = bquote('Concentration  ['*µg/m^3*']'), colour = NULL, fill = NULL, subtitle = plotSubTitle) +
#   # scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
#   # scale_x_continuous(minor_breaks = seq(1, 12, 1), breaks=seq(1, 12, 1), limits=c(1,  12)) +
#   scale_y_continuous(minor_breaks = seq(0, 40, 10), breaks=seq(0, 40, 10), limits=c(0,  40)) +
#   theme(
#     text = element_text(size = 18)
#     # , axis.text.x = element_text(angle = 45, hjust = 1)
#     , legend.position = "top"
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
#
# # 계절별 평균 미세먼지 막대 그래프
# plotSubTitle = sprintf("%s", "계절별 평균 미세먼지 막대 그래프")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
#
# ggplot(data = dataL3, aes(x = season, y = val, color = key, fill = key)) +
#   geom_bar(stat = "identity",  width = 0.4, position=position_dodge(width = 0.5)) +
#   labs(title = NULL, x = "Season", y = bquote('Concentration  ['*µg/m^3*']'), colour = NULL, fill = NULL, subtitle = plotSubTitle) +
#   # scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
#   # scale_x_continuous(minor_breaks = seq(1, 12, 1), breaks=seq(1, 12, 1), limits=c(1,  12)) +
#   scale_y_continuous(minor_breaks = seq(0, 40, 10), breaks=seq(0, 40, 10), limits=c(0,  40)) +
#   theme(
#     text = element_text(size = 18)
#     # , axis.text.x = element_text(angle = 45, hjust = 1)
#     , legend.position = "top"
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)