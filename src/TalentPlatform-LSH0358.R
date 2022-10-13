
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
# R을 이용한 3차원 시각화 동영상 제작

# [입력 데이터] 변경
# A_Group data_final_version3.0.xlsx
# > 20221016_A_Group data_final_version3.0.xlsx
# 
# 3D plot
# 지금 보내드리는 엑셀의 첫번째 탭, Aunknown은 기존에 보내느리는 것과 동일함. 2번째, 3번째 탭의 데이터도 구조는 동일.
# 1. 3D plot :
#   - 기존의 첫번째 탭의 데이터로 z 축만 G 컬럼의 Total physical activity volume (MET * minutes / week)로 바꾸어 실행.
# - 엑셀의 두번째 탭의 데이터로 3D plot을 reproduce. CSF p-tau (F컬럼)- Parental EYO (E컬럼) Collateral-reported conscientiousness (H 컬럼)입니다. 점은 파란색입니다.
# CSF p-tau (F컬럼)- Parental EYO (E컬럼) Total physical activity volume (MET * minutes / week) (G 컬럼)입니다. 점은 보라입니다.
# 
# [공통] x축 (p-tau), y축 (Parental EYO)
# [1번 시트] z축 (Total physical activity volume (MET * minutes / week)), z축 (Collateral-reported conscientiousness)
# [2번 시트] z축 (Total physical activity volume (MET * minutes / week)), z축 (Collateral-reported conscientiousness)
# [결과] 동영상 4개
# 
# 2D plot
# 
# - 그림 1의 Parental EYO - CSF p-tau 2D plot spagatetti plot plotting (엑셀의 첫번째 탭)에  추가하여 2번째, 3번째 tab 데이터에 대한 sphagatti plot을 추가하여 얹어주세요. 2번쨰 탭은 E 컬럼과, F 컬럼 / 3번째 탭은 D 컬럼과 E 컬럼의 데이터를 쓰시면 되는데,  기존에 했던 방식대로 A-C 컬럼에 있는 한 개인의 multiple visit 임을 반영해서 dot conneting sphagetti plot 형식으로 (serial longitudnianl visit data임) 구현. 첫번째 탭의 데이터는 빨강, 2번째 탭의 데이터는 파랑, 3번째 탭의 데이터는 초록으로 선명하고 진한 색상으로 구현.
# 
# - 그림 2 (CSF p-tau - Collateral-reported conscientiousness),  2D plot spagatetti plot plotting (엑셀의 첫번째 탭)에 추가하여 2번째 tab 데이터에 대한 sphagatti plot을 추가하여 얹어주세요. 
# 2번쨰 탭은 F & H 컬럼의 의 데이터를 쓰면 됨. 
# 
# - 그림 3 (CSF p-tau - Total physical activity volume (MET * minutes / week)  2D plot spagatetti plot plotting (엑셀의 첫번째 탭)에 추가하여 2번째 tab 데이터에 대한 sphagatti plot을 추가하여 얹어주세요. 
#         2번쨰 탭은 F & G 컬럼의 의 데이터를 쓰시면 됨. 
#         기존에 했던 방식대로 A-C 컬럼에 있는 한 개인의 multiple visit 임을 반영해서 dot conneting sphagetti plot 형식으로 (serial longitudnianl visit data임) 구현. 
#         첫번째 탭의 데이터는 빨강, 2번째 탭의 데이터는 파랑으로 선명하고 진한 색상으로 구현.
#         
#         그리고 논문의 method에 기술해야 해서 아래와 같이 이전에 작업해 주신 R의 version과 package 이름 좀 여쭈어 봐도 될까요?
#           
#           ex ) R version 3.6.1 with ggplot2, ggpubr, lme4, psych, and dplyr packages.
# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0358"
contextPath = ifelse(env == "local", ".", "E:/04. TalentPlatform/Github/TalentPlatform-R")

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
# 라이브러 설정
library(tidyverse)
library(lubridate)
library(ggplot2)
library(openxlsx)
library(plotly)
library(readxl)
library(rayshader)
library(rayrender)
library(doParallel)
library(magick)
library(webshot2)
library(av)
library(noncompliance)
library(MBA)

# 초기 설정
xRange = as.numeric(c(-40, 0))
yRange = as.numeric(c(10, 165))

newLon = seq(from = xRange[1], to = xRange[2], by = 1)
newLat = seq(from = yRange[1], to = yRange[2], by = 1)

# newLon = seq(from = xRange[1], to = xRange[2], by = 0.5)
# newLat = seq(from = yRange[1], to = yRange[2], by = 0.5)

# newLon = seq(from = xRange[1], to = xRange[2], by = 0.4)
# newLat = seq(from = yRange[1], to = yRange[2], by = 0.4)
# newLon = seq(from = xRange[1], to = xRange[2], by = 0.3)
# newLat = seq(from = yRange[1], to = yRange[2], by = 0.3)


gridData = noncompliance::expand.grid.DT(
  newLon
  , newLat
  , col.names = c("lon", "lat")
)

accumulate_by = function(dat, var) {
  var = lazyeval::f_eval(var, dat)
  lvls = plotly:::getLevels(var)
  dats = lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}


# 파일 읽기
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "A_Group+data_의뢰_final_version3.0.xlsx"))
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "A_Group data_final_version3.0.xlsx"))
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "20221016_A_Group data_final_version3.0.xlsx"))

# inpData = openxlsx::read.xlsx(fileInfo, sheet = 1, startRow = 5)
# inpData = readxl::read_excel(fileInfo, sheet = 1)
# colList = names(inpData) %>% unique()


# ******************************************************************************
# 기존 데이터 전처리 (박진만 연구원)
# ******************************************************************************
# df=df[-c(1:4), ]
# df$CRFNAME=str_split_fixed(df$CRFNAME, "-", 2)[, 1]
# df$CRFNAME=as.factor(df$CRFNAME)
# df$...4=as.factor(df$...4)
# df=df %>% mutate_if(is.character,as.numeric)

# 1-2)
# df %>% mutate(...4="Asymptomatic individuals") %>% 
#   ggplot(aes(x=parental_eyo, y=CSF_xMAP_ptau, group=CRFNAME, color=...4))+ 
#   geom_point()+
#   geom_line()+
#   scale_colour_manual(values=c("#FF3333"))+
#   theme_light()+
#   theme(legend.title=element_blank(), legend.position = "top")+
#   scale_fill_discrete(labels=c("Asymptomatic individuals"))+
#   labs(x="Parental estimated years from expected symptom onset (Parental EYO)", 
#        y="CSF p-tau")
# 
# # 2-1-2)
# df %>% filter(...4=="Aunkown") %>% 
#   group_by(CRFNAME) %>% 
#   plot_ly(x = ~parental_eyo, y = ~CSF_xMAP_ptau, z = ~C_CONSCIEN...8) %>%
#   add_markers(color= ~...4, size=1, colors=c("#FF3333")) %>% 
#   add_lines(color= ~...4, colors=c("#FF3333"), showlegend=F) %>% 
#   layout(scene = list(aspectmode = "manual", aspectratio = list(x=1, y=1, z=1),
#                       xaxis=list(title="Parental estimated years from expected symptom onset (Parental EYO)"),
#                       yaxis=list(title="CSF p-tau"),
#                       zaxis=list(title="Collateral reported conscientiousness")),
#          showlegend=T)
# 
# Error in eval(expr, data, expr_env) : object 'DIAN_EYO' not found
# # 2-3-2)
# df %>% filter(...4=="Aunkown") %>%
#   group_by(CRFNAME) %>%
#   plot_ly(x = ~DIAN_EYO, y = ~CSF_xMAP_AB42, z = ~C_CONSCIEN...15) %>%
#   add_markers(color= ~...4, size=1, colors=c("#6600CC")) %>%
#   add_lines(color= ~...4, colors=c("#6600CC"), showlegend=F) %>%
#   layout(scene = list(aspectmode = "manual", aspectratio = list(x=1, y=1, z=1),
#                       xaxis=list(title="Estimated years to symptom onset (years)"),
#                       yaxis=list(title=paste("CSF A", "β", "1-42", sep="")),
#                       zaxis=list(title="Collateral reported conscientiousness")),
#          showlegend=T)


# ******************************************************************************
# 2D 시각화
# ******************************************************************************
# 그림 1
# dataL1 = tibble::tibble()
# for (sheetInfo in 1:3) {
#   data = readxl::read_excel(fileInfo, sheet = sheetInfo) %>% 
#     tidyr::separate(col = ID_Visit, into = c("key", "keyNum"), sep = "-") %>% 
#     dplyr::rename(
#       "eyo" = "Parental EYO"
#       , "tau" = "CSF p-tau"
#     ) %>% 
#     dplyr::select(eyo, tau, key) %>% 
#     dplyr::mutate(type = sheetInfo)
#   
#   dataL1 = dplyr::bind_rows(dataL1, data)
# }
# 
# summary(dataL1)
# 
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "Fig1_Parental EYO-CSF p-tau_2D plot")
# 
# ggplot(data = dataL1, aes(x = eyo, y = tau, group = key, colour = factor(type))) +
#   geom_point(show.legend = FALSE)+
#   geom_line(show.legend = FALSE)+
#   scale_colour_manual(values = c("red", "blue", "dark green")) +
#   labs(
#     x = "Parental EYO"
#     , y = "CSF p-tau"
#     , color = NA
#     ) +
#   theme(
#     text = element_text(size = 18)
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
# 
# dataL1 = tibble::tibble()
# for (sheetInfo in 1:2) {
#   data = readxl::read_excel(fileInfo, sheet = sheetInfo) %>% 
#     tidyr::separate(col = ID_Visit, into = c("key", "keyNum"), sep = "-") %>% 
#     dplyr::rename(
#       "tpav" = "Total physical activity volume (MET * minutes / week)"
#       , "crc" = "Collateral-reported conscientiousness"
#       , "tau" = "CSF p-tau"
#     ) %>% 
#     dplyr::select(tpav, tau, crc, key) %>% 
#     dplyr::mutate(type = sheetInfo)
#   
#   dataL1 = dplyr::bind_rows(dataL1, data)
# }
# 
# dataL2 = dataL1 %>% 
#   dplyr::filter(tpav < 50000)
# 
# 
# # 그림 2
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "Fig2_CSF p-tau-Collateral-reported conscientiousness_2D plot")
# 
# ggplot(data = dataL1, aes(x = tau, y = crc, group = key, colour = factor(type))) +
#   geom_point(show.legend = FALSE)+
#   geom_line(show.legend = FALSE)+
#   scale_colour_manual(values = c("red", "blue", "dark green")) +
#   labs(
#     x = "CSF p-tau"
#     , y = "Collateral-reported conscientiousness"
#     , color = NA
#   ) +
#   theme(
#     text = element_text(size = 18)
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
# 
# 
# # 그림 3
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "Fig3_CSF p-tau-Total physical activity volume_2D plot")
# 
# ggplot(data = dataL2, aes(x = tau, y = tpav, group = key, colour = factor(type))) +
#   geom_point(show.legend = FALSE)+
#   geom_line(show.legend = FALSE)+
#   scale_colour_manual(values = c("red", "blue", "dark green")) +
#   labs(
#     x = "CSF p-tau"
#     , y = "Total physical activity volume"
#     , color = NA
#   ) +
#   theme(
#     text = element_text(size = 18)
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


# ******************************************************************************
# 사용자 편의성 시각화 (2D, 3D, 동영상)
# ******************************************************************************
inpData = readxl::read_excel(fileInfo, sheet = 1)
colList = names(inpData) %>% unique()

data = inpData %>% 
  dplyr::select(colList) %>% 
  tidyr::separate(col = ID_Visit, into = c("key", "keyNum"), sep = "-") %>% 
  dplyr::rename(
    "eyo" = "Parental EYO"
    , "tau" = "CSF p-tau"
    , "tpav" = "Total physical activity volume (MET * minutes / week)"
    , "crc" = "Collateral-reported conscientiousness"
  ) %>% 
  dplyr::mutate(
    type = str_trim(...4)
  ) %>% 
  dplyr::filter(
    tpav < 50000
    ) %>%
  dplyr::select(eyo, tau, crc, tpav, key, type) %>%
  na.omit() %>% 
  readr::type_convert()

summary(data)

# 2D 시각화
# makePlot = ggplot(data = data, aes(x = eyo, y = tau, colour = crc, group = key)) +
makePlot = ggplot(data = data, aes(x = tau, y = eyo, colour = crc, group = key)) +
  geom_point() +
  geom_line() +
  scale_color_gradientn(colours = colorRamps::matlab.like(11), na.value = NA) +
  labs(
    # x = "Parental estimated years from expected symptom onset (Parental EYO)"
    #x = "Parental EYO"
    #, y = "CSF p-tau"
    x = "CSF p-tau"
    , y = "Parental EYO"
    , colour = NULL
  ) +
  facet_grid(~ type)


makePlot

# 3D 동영상
# cnt = 180
# cnt = 1080
cnt = 360

phivechalf = 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = cnt/2)/2))
phivecfull = c(phivechalf, rev(phivechalf))
thetavec = 0 + 45 * sin(seq(0,359,length.out = cnt) * pi/180)
zoomvec = 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = cnt/2)))
zoomvecfull = c(zoomvec, rev(zoomvec))

# rayshader::plot_gg(makePlot, multicore=TRUE, width=10, height=10, fov=70, scale=500)
rayshader::plot_gg(makePlot, multicore=TRUE, width=10, height=10, fov=70, scale=1000)
saveMp4 = sprintf("%s/%s_%s_%s.mp4", globalVar$figPath, serviceName, "3D_tau-eyo-crc", format(Sys.time(), "%Y%m%d%H%M%S"))
file.create(fs::path_dir(saveMp4), showWarnings = FALSE)
rayshader::render_movie(saveMp4, type = "custom", frames = cnt,  phi = phivecfull, zoom = zoomvecfull, theta = thetavec)
rgl::rgl.close()


# # 데이터 내삽
# dataL1 = data %>% 
#   dplyr::select(eyo, tau, cons) %>% 
#   MBA::mba.points(., gridData, extend = TRUE)
# 
# dataL2 = dataL1 %>%
#   as.data.frame() %>%
#   as.tibble() %>%
#   dplyr::rename(
#     xAxis = xyz.est.x
#     , yAxis = xyz.est.y
#     , zAxis = xyz.est.z
#   ) %>%
#   accumulate_by(~xAxis)


# 2D 시각화
# makePlot = ggplot(data = dataL2, aes(x = xAxis, y = yAxis, colour = zAxis, z = zAxis, frame = frame)) +
#   geom_point() +
#   # stat_density_2d(aes(fill = zAxis), geom = "polygon", n = 100, contour = TRUE) +
#   # geom_raster(interpolate = TRUE, na.rm = TRUE) +
#   # geom_raster(interpolate = TRUE, na.rm = TRUE) +
#   # scale_fill_gradientn(colours = colorRamps::matlab.like(11), na.value = NA) +
#   scale_color_gradientn(colours = colorRamps::matlab.like(11), na.value = NA) +
#   labs(
#     x = "Parental estimated years from expected symptom onset (Parental EYO)"
#     , y = "CSF p-tau"
#     , fill = NULL
#     , colour = NULL
#   )

# 2D 시각화
# makePlotPlay = plotly::ggplotly(makePlot) %>%
#   plotly::animation_opts(
#     frame = 100
#     , transition = 0
#     , redraw = FALSE
#   )
#
# subTitle = sprintf("%s", "2D CON-8 시각화")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)
# saveHtml = sprintf("%s/%s_%s.html", globalVar$figPath, serviceName, subTitle)
# tmpImg = tempfile(fileext = ".png")
# tmpHtml= tempfile(fileext = ".html")

# html 저장
# htmlwidgets::saveWidget(makePlotPlay, file = tmpHtml)
# fs::file_move(tmpHtml, saveHtml)


# 3D 시각화
# dataL3 = dataL2 %>%
#   dplyr::mutate(rowNum = dplyr::row_number())
#     
# makePlot3D = plotly::plot_ly(
#   type = "mesh3d"
#   , data = dataL3
#   , x = ~xAxis
#   , y = ~yAxis
#   , z = ~zAxis
#   # , frame = ~rowNum
#   , intensity = ~zAxis
#   , colors = colorRamps::matlab.like(11)
#   , showlegend = FALSE
#   , colorbar = list(title = NULL)
# ) %>% 
#   layout(
#     title = NULL
#     , scene = list(
#       # xaxis = list(title = "Relative humidity [%]")
#       # , yaxis = list(title = "b<sub>ext</sub> of IMPROVE<sub>2005</sub> [Mm<sup>-1</sup>]")
#       # , zaxis = list(title = "PM<sub>2.5</sub> concentration [ug/m<sup>3</sup>]")
#       xaxis = list(title = "Parental estimated years from expected symptom onset (Parental EYO)")
#       , yaxis = list(title = "CSF p-tau")
#       , zaxis = list(title = "Collateral reported conscientiousness")
#     )
#     , autosize = TRUE
#   )
# 
# makePlot3D
# 
# subTitle = sprintf("%s", "3D CON-8 시각화")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)
# saveHtml = sprintf("%s/%s_%s.html", globalVar$figPath, serviceName, subTitle)
# tmpImg = tempfile(fileext = ".png")
# tmpHtml= tempfile(fileext = ".html")
# 
# # html 저장
# htmlwidgets::saveWidget(makePlot3D, file = tmpHtml)
# fs::file_move(tmpHtml, saveHtml)