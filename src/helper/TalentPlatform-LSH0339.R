
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
# R을 이용한 plotly 3D 상대습도 영향도 시각화

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0339"

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
library(dplyr)
library(dplyr)
library(fitdistrplus)
library(stats)
library(hydroGOF)
library(RColorBrewer)
library(forcats)
library(ggpubr)
library(h2o)
library(scales)
library(openxlsx)
library(ggplot2)
library(viridis)
library(rayshader)
library(plotly)
library(plotly)

cbSpectral = rev(RColorBrewer::brewer.pal(11, "Spectral"))
cbMatlab2 = rev(colorRamps::matlab.like2(11))

# 파일 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "testData.xlsx"))


# ******************************************************************************
# 상대습도와 PM2.5 recon
# ******************************************************************************
data = readxl::read_excel(fileInfo, sheet = 5)

dataL1 = data %>% 
  as.tibble() %>% 
  dplyr::mutate(
    dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
    , dtYear = lubridate::year(dtDateTime)
    , dtMonth = lubridate::month(dtDateTime)
    , dtXran = lubridate::decimal_date(dtDateTime)
  ) 

summary(dataL1)

dataL2 = dataL1 %>% 
  # dplyr::select(-c("Date.time", "sDateTime", "dtYear", "dtMonth", "dtXran")) %>% 
  dplyr::select(-c("Date time", "sDateTime", "dtYear", "dtMonth", "dtXran")) %>% 
  dplyr::rename(
    bExt = `총소멸계수(Bext)`
  ) %>% 
  tidyr::gather(-dtDateTime, -bExt, -RH, key = "key", value = "val") %>% 
  na.omit()

summary(dataL2)

# ******************************************************************************
# 2D 상대습도와 PM2.5 recon
# ******************************************************************************

subTitle = sprintf("%s", "상대습도에 따른 시정 영향 (PM2.5_reconstruct)")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)

lmFormula = y ~ poly(x, 2, raw = TRUE)

# ggplot(data = dataL2, aes(x = RH, y = bExt, color = val)) +
#   geom_point(size = 2) +
#   geom_smooth(method = 'lm', formula = lmFormula, se = TRUE, color = "black") +
#   ggpubr::stat_regline_equation(label.x.npc = 0.075, label.y.npc = 0.96, size = 5.5, aes(label = ..eq.label..), color = "black", formula = lmFormula, parse = TRUE) +
#   ggpubr::stat_cor(label.x.npc = 0.075, label.y.npc = 0.90, size = 5.5, color = "black") +
#   scale_color_gradientn(colours = cbMatlab2, limits = c(0, 100), na.value = cbMatlab2[11]) +
#   scale_x_continuous(minor_breaks = seq(20, 100, 20), breaks=seq(20, 100, 20),  limits=c(20, 100)) +
#   scale_y_continuous(minor_breaks = seq(0, 1600, 400), breaks=seq(0, 1600, 400), limits=c(0, 1600)) +
#   labs(
#     title = NULL
#     , x = "Relative  humidity  [%]"
#     , y = bquote('Reconstructed  ' *b[ext]* '  of  IMPROVE_2005  ['*Mm^-1*']')
#     , color = bquote('PM' ['2.5']* ' concentration ['*mu*g/m^3*']')
#     , fill = NULL
#     ) +
#   theme(
#     text = element_text(size = 18)
#     # , legend.position = "top"
#     , legend.position = c(0, 1), legend.justification = c(0, 0.96)
#     , legend.key=element_blank()
#     , legend.text=element_text(size=16)
#     , legend.background=element_blank()
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
# 
# 
# 
# 

# ******************************************************************************
# 3D 상대습도와 PM2.5 recon
# ******************************************************************************

makePlot3D = plotly::plot_ly(
  type = "mesh3d"
  , data = dataL2
  , x = ~RH
  , y = ~bExt
  , z = ~val
  , intensity = ~val
  , colors = cbMatlab2
  , showlegend = FALSE
  ) %>% 
  layout(
    title = NULL
    , scene = list(
      xaxis = list(title = "Relative humidity [%]")
      , yaxis = list(title = "b<sub>ext</sub> of IMPROVE<sub>2005</sub> [Mm<sup>-1</sup>]")
      , zaxis = list(title = "PM<sub>2.5</sub> concentration [ug/m<sup>3</sup>]")
      ) 
    )

makePlot3D

subTitle = sprintf("%s", "3D 상대습도에 따른 시정 영향 (PM2.5_reconstruct)")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)
saveHtml = sprintf("%s/%s_%s.html", globalVar$figPath, serviceName, subTitle)
tmpImg = tempfile(fileext = ".png")
tmpHtml= tempfile(fileext = ".html")

# html 저장
htmlwidgets::saveWidget(makePlot3D, file = tmpHtml)
fs::file_move(tmpHtml, saveHtml)

# html을 이용해서 png 저장
# webshot::webshot(tmpHtml, tmpImg, vwidth = 775, vheight = 550, delay = 10)
# fs::file_move(tmpImg, saveImg)
