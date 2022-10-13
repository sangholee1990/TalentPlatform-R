
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

# newLon = seq(from = xRange[1], to = xRange[2], by = 1)
# newLat = seq(from = yRange[1], to = yRange[2], by = 1)

newLon = seq(from = xRange[1], to = xRange[2], by = 0.5)
newLat = seq(from = yRange[1], to = yRange[2], by = 0.5)

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
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "A_Group data_final_version3.0.xlsx"))


inpData = openxlsx::read.xlsx(fileInfo, sheet = 1, startRow = 5)
colList = names(inpData) %>% unique()

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
# 2-3-1) C_CONSCIEN...8
# ******************************************************************************
data = inpData %>% 
  dplyr::select(colList) %>% 
  tidyr::separate(col = ID_Visit, into = c("key", "keyNum"), sep = "-") %>% 
  dplyr::rename(
    "eyo" = "Parental.EYO"
    , "tau" = "CSF.p-tau"
    , "cons" = "Collateral-reported.conscientiousness"
  ) %>% 
  dplyr::mutate(
    type = str_trim(X4)
  ) %>% 
  dplyr::filter(type =="Aunkown") %>%
  dplyr::select(eyo, tau, cons, key, type) %>%
  na.omit() %>% 
  readr::type_convert()

# summary(data)
# 
# dataL1 = MBA::mba.points(data, gridData, extend = TRUE)
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

#makePlot = ggplot(data = data, aes(x = eyo, y = tau, colour = cons, group=key)) +
makePlot = ggplot(data = data, aes(x = tau, y = eyo, colour = cons, group=key)) +
  geom_point() +
  geom_line() +
  # stat_density_2d(aes(fill = C_CONSCIEN...8), geom = "polygon", n = 100, contour = TRUE) +
  scale_color_gradientn(colours = colorRamps::matlab.like(11), na.value = NA) +
  labs(
    # x = "Parental estimated years from expected symptom onset (Parental EYO)"
    #x = "Parental EYO"
    #, y = "CSF p-tau"
    y = "Parental EYO"
    , x = "CSF p-tau"
    , colour = NULL
  ) +
  facet_grid(~ type)



makePlot


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
# 
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
saveMp4 = sprintf("%s/%s_%s_%s.mp4", globalVar$figPath, serviceName, "cons", format(Sys.time(), "%Y%m%d%H%M%S"))
file.create(fs::path_dir(saveMp4), showWarnings = FALSE)
rayshader::render_movie(saveMp4, type = "custom", frames = cnt,  phi = phivecfull, zoom = zoomvecfull, theta = thetavec)
rgl::rgl.close()

dataL3 = dataL2 %>%
  dplyr::mutate(rowNum = dplyr::row_number())
    

makePlot3D = plotly::plot_ly(
  type = "mesh3d"
  , data = dataL3
  , x = ~xAxis
  , y = ~yAxis
  , z = ~zAxis
  # , frame = ~rowNum
  , intensity = ~zAxis
  , colors = colorRamps::matlab.like(11)
  , showlegend = FALSE
  , colorbar = list(title = NULL)
) %>% 
  layout(
    title = NULL
    , scene = list(
      # xaxis = list(title = "Relative humidity [%]")
      # , yaxis = list(title = "b<sub>ext</sub> of IMPROVE<sub>2005</sub> [Mm<sup>-1</sup>]")
      # , zaxis = list(title = "PM<sub>2.5</sub> concentration [ug/m<sup>3</sup>]")
      xaxis = list(title = "Parental estimated years from expected symptom onset (Parental EYO)")
      , yaxis = list(title = "CSF p-tau")
      , zaxis = list(title = "Collateral reported conscientiousness")
    )
    , autosize = TRUE
  )

makePlot3D

subTitle = sprintf("%s", "3D CON-8 시각화")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)
saveHtml = sprintf("%s/%s_%s.html", globalVar$figPath, serviceName, subTitle)
tmpImg = tempfile(fileext = ".png")
tmpHtml= tempfile(fileext = ".html")

# html 저장
htmlwidgets::saveWidget(makePlot3D, file = tmpHtml)
fs::file_move(tmpHtml, saveHtml)


# ******************************************************************************
# 2-3-2) C_CONSCIEN...15
# ******************************************************************************
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



colnames(df)

data = df %>% filter(...4=="Aunkown") %>%
  # group_by(CRFNAME) %>%
  dplyr::select(parental_eyo, CSF_xMAP_ptau, C_CONSCIEN...15) %>%
  na.omit() %>% 
  readr::type_convert()

summary(data)

dataL1 = MBA::mba.points(data, gridData, extend = TRUE)

dataL2 = dataL1 %>%
  as.data.frame() %>%
  as.tibble() %>%
  dplyr::rename(
    xAxis = xyz.est.x
    , yAxis = xyz.est.y
    , zAxis = xyz.est.z
  )


# makePlot = ggplot(data = dataL1, aes(x = parental_eyo, y = CSF_xMAP_ptau, colour = C_CONSCIEN...8)) +
# ggplot(data = data, aes(x = parental_eyo, y = CSF_xMAP_ptau, colour = C_CONSCIEN...8)) +
#   geom_point() +
#   # stat_density_2d(aes(fill = C_CONSCIEN...8), geom = "polygon", n = 100, contour = TRUE) +
#   scale_color_viridis_c(option = "A") +
#   scale_color_gradientn(colours = colorRamps::matlab.like(11), na.value = NA) +
#   labs(
#     x = "Parental estimated years from expected symptom onset (Parental EYO)"
#     , y = "CSF p-tau"
#     , colour = "CSF p-tau"
#   )



makePlot = ggplot(data = dataL2, aes(x = xAxis, y = yAxis, colour = zAxis, fill = zAxis, z = zAxis)) +
  # geom_point() +
  stat_density_2d(geom = "polygon", n = 100, contour = TRUE) +
  # geom_raster(interpolate = TRUE, na.rm = TRUE) +
  # geom_raster(interpolate = TRUE, na.rm = TRUE) +
  scale_fill_gradientn(colours = colorRamps::matlab.like(11), na.value = NA) +
  scale_color_gradientn(colours = colorRamps::matlab.like(11), na.value = NA) +
  labs(
    x = "Parental estimated years from expected symptom onset (Parental EYO)"
    , y = "CSF p-tau"
    , fill = "CSF p-tau"
    , colour = "CSF p-tau"
  )

makePlot


# cnt = 180
# cnt = 1080
cnt = 360

phivechalf = 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = cnt/2)/2))
phivecfull = c(phivechalf, rev(phivechalf))
thetavec = 0 + 45 * sin(seq(0,359,length.out = cnt) * pi/180)
zoomvec = 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = cnt/2)))
zoomvecfull = c(zoomvec, rev(zoomvec))

rayshader::plot_gg(makePlot, multicore=TRUE, width=10, height=10, fov=70, scale=300)
saveMp4 = sprintf("%s/%s_%s_%s.mp4", globalVar$figPath, serviceName, "con8_interp", format(Sys.time(), "%Y%m%d%H%M%S"))
file.create(fs::path_dir(saveMp4), showWarnings = FALSE)
rayshader::render_movie(saveMp4, type = "custom", frames = cut,  phi = phivecfull, zoom = zoomvecfull, theta = thetavec)
rgl::rgl.close()



# 
# 
# # ******************************************************************************
# # 상대습도와 PM2.5 recon
# # ******************************************************************************
# data = readxl::read_excel(fileInfo, sheet = 5)
# 
# dataL1 = data %>% 
#   as.tibble() %>% 
#   dplyr::mutate(
#     dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
#     , dtYear = lubridate::year(dtDateTime)
#     , dtMonth = lubridate::month(dtDateTime)
#     , dtXran = lubridate::decimal_date(dtDateTime)
#   ) 
# 
# summary(dataL1)
# 
# dataL2 = dataL1 %>% 
#   # dplyr::select(-c("Date.time", "sDateTime", "dtYear", "dtMonth", "dtXran")) %>% 
#   dplyr::select(-c("Date time", "sDateTime", "dtYear", "dtMonth", "dtXran")) %>% 
#   dplyr::rename(
#     bExt = `총소멸계수(Bext)`
#   ) %>% 
#   tidyr::gather(-dtDateTime, -bExt, -RH, key = "key", value = "val") %>% 
#   na.omit()
# 
# summary(dataL2)
# 
# # ******************************************************************************
# # 2D 상대습도와 PM2.5 recon
# # ******************************************************************************
# 
# subTitle = sprintf("%s", "상대습도에 따른 시정 영향 (PM2.5_reconstruct)")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)
# 
# lmFormula = y ~ poly(x, 2, raw = TRUE)
# 
# # ggplot(data = dataL2, aes(x = RH, y = bExt, color = val)) +
# #   geom_point(size = 2) +
# #   geom_smooth(method = 'lm', formula = lmFormula, se = TRUE, color = "black") +
# #   ggpubr::stat_regline_equation(label.x.npc = 0.075, label.y.npc = 0.96, size = 5.5, aes(label = ..eq.label..), color = "black", formula = lmFormula, parse = TRUE) +
# #   ggpubr::stat_cor(label.x.npc = 0.075, label.y.npc = 0.90, size = 5.5, color = "black") +
# #   scale_color_gradientn(colours = cbMatlab2, limits = c(0, 100), na.value = cbMatlab2[11]) +
# #   scale_x_continuous(minor_breaks = seq(20, 100, 20), breaks=seq(20, 100, 20),  limits=c(20, 100)) +
# #   scale_y_continuous(minor_breaks = seq(0, 1600, 400), breaks=seq(0, 1600, 400), limits=c(0, 1600)) +
# #   labs(
# #     title = NULL
# #     , x = "Relative  humidity  [%]"
# #     , y = bquote('Reconstructed  ' *b[ext]* '  of  IMPROVE_2005  ['*Mm^-1*']')
# #     , color = bquote('PM' ['2.5']* ' concentration ['*mu*g/m^3*']')
# #     , fill = NULL
# #     ) +
# #   theme(
# #     text = element_text(size = 18)
# #     # , legend.position = "top"
# #     , legend.position = c(0, 1), legend.justification = c(0, 0.96)
# #     , legend.key=element_blank()
# #     , legend.text=element_text(size=16)
# #     , legend.background=element_blank()
# #   ) +
# #   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
# # 
# # 
# # 
# # 
# 
# # ******************************************************************************
# # 3D 상대습도와 PM2.5 recon
# # ******************************************************************************
# 
# makePlot3D = plotly::plot_ly(
#   type = "mesh3d"
#   , data = dataL2
#   , x = ~RH
#   , y = ~bExt
#   , z = ~val
#   , intensity = ~val
#   , colors = cbMatlab2
#   , showlegend = FALSE
#   ) %>% 
#   layout(
#     title = NULL
#     , scene = list(
#       xaxis = list(title = "Relative humidity [%]")
#       , yaxis = list(title = "b<sub>ext</sub> of IMPROVE<sub>2005</sub> [Mm<sup>-1</sup>]")
#       , zaxis = list(title = "PM<sub>2.5</sub> concentration [ug/m<sup>3</sup>]")
#       ) 
#     )
# 
# makePlot3D
# 
# subTitle = sprintf("%s", "3D 상대습도에 따른 시정 영향 (PM2.5_reconstruct)")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)
# saveHtml = sprintf("%s/%s_%s.html", globalVar$figPath, serviceName, subTitle)
# tmpImg = tempfile(fileext = ".png")
# tmpHtml= tempfile(fileext = ".html")
# 
# # html 저장
# htmlwidgets::saveWidget(makePlot3D, file = tmpHtml)
# fs::file_move(tmpHtml, saveHtml)
# 
# # html을 이용해서 png 저장
# # webshot::webshot(tmpHtml, tmpImg, vwidth = 775, vheight = 550, delay = 10)
# # fs::file_move(tmpImg, saveImg)



















# library(rayshader)
# volcano %>%
#   sphere_shade() %>%
#   # add_shadow(ray_shade(volcano,zscale=3),0.3) %>%
#   plot_3d(volcano, zscale=3, fov=30)
# render_movie("basic_video.mp4", title_text = "Basic rayshader plot",
#              title_bar_color = "red", title_bar_alpha = 0.3)
# rgl::rgl.close()




# gg = ggplot(diamonds, aes(x = x, y = depth)) +
#   stat_density_2d(aes(fill = stat(nlevel)), 
#                   geom = "polygon",
#                   n = 100,bins = 10,contour = TRUE) +
#   # facet_wrap(clarity~.) +
#   scale_fill_viridis_c(option = "A")


# rayshader::plot_gg(gg, multicore=TRUE,  raytrace = TRUE, windowsize = c(1000, 1000), zoom = 0.8, width=5, height=5, scale=250)

# saveMp4 = sprintf("%s/%s_%s.mp4", globalVar$figPath, serviceName, "movie")
# # saveMp4 = sprintf("%s.mp4", "movie2")
# rayshader::plot_gg(gg, multicore=TRUE, raytrace = TRUE, zoom = 0.8, width=5, height=5, scale=250)
# rayshader::render_movie(saveMp4)
# rgl::rgl.close()



# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "img")
# render_snapshot(clear = TRUE, filename=saveImg)
# rgl::rgl.close()


# saveMp4 = sprintf("%s/%s_%s.mp4", globalVar$figPath, serviceName, "movie")
# rayshader::render_movie(saveMp4)
# rgl::rgl.close()
# rayshader::plot_gg(gg, multicore=TRUE, raytrace = TRUE, width=5, height=5)
# rayshader::render_movie(saveMp4, frames = 260, fps=30, zoom=1, fov = 30)



# saveImgList = sprintf("%s/%s_*.png", globalVar$figPath, serviceName, "img")
# mapmate::ffmpeg(pattern = saveImgList, output = saveMp4, delay = 1/10, overwrite = TRUE)

# system(command = "ffmpeg")
# system(command = 'ffmpeg -y -framerate 45 -i"E:/04. TalentPlatform/Github/TalentPlatform-R/resources/fig/test/LSH0358_img_%05d.png" -c:v libx264 -vf fps=45 -pix_fmt yuv420p  "E:/04. TalentPlatform/Github/TalentPlatform-R/resources/fig/test/LSH0358_movie.mp4"')
# system(command = 'ffmpeg -y -framerate 45 -i"E:/04. TalentPlatform/Github/TalentPlatform-R/resources/fig/test/LSH0358_img_%05d.png" -c:v libx264 -vf fps=45 -pix_fmt yuv420p  "E:/04. TalentPlatform/Github/TalentPlatform-R/resources/fig/test/LSH0358_movie.mp4"')
# system(command = 'ffmpeg -y -framerate 45 -f image2 -pattern_type glob -i "E:/04. TalentPlatform/Github/TalentPlatform-R/resources/fig/test/LSH0358_img_*.png" -vcodec libx264 -r 30 -pix_fmt yuv420p "E:/04. TalentPlatform/Github/TalentPlatform-R/resources/fig/test/LSH0358_movie.mp4"')
# system(command = 'ffmpeg -y -framerate 45 -f image2 -i "E:/04. TalentPlatform/Github/TalentPlatform-R/resources/fig/test/LSH0358_img_%d.png" -vcodec libx264 -pix_fmt yuv420p "E:/04. TalentPlatform/Github/TalentPlatform-R/resources/fig/test/LSH0358_movie.mp4"')


# rayshader::plot_gg(gg, multicore=TRUE, width=5, height=5, scale=250)
# Sys.sleep(0.2)
# render_depth(focallength = 100, clear=TRUE)


# 1-2)
# df %>% mutate(...4="Asymptomatic individuals") %>%
#   ggplot(aes(x=parental_eyo, y=CSF_xMAP_ptau, group=CRFNAME, color=...4))+
#   geom_point()+
#   # geom_line()+
#   scale_colour_manual(values=c("#FF3333"))+
#   theme_light()+
#   theme(legend.title=element_blank(), legend.position = "top")+
#   scale_fill_discrete(labels=c("Asymptomatic individuals"))+
#   labs(x="Parental estimated years from expected symptom onset (Parental EYO)",
#        y="CSF p-tau")


# gg = ggplot(diamonds, aes(x = x, y = depth)) +
#   stat_density_2d(aes(fill = stat(nlevel)), 
#                   geom = "polygon",
#                   n = 100,bins = 10,contour = TRUE) +
#   # facet_wrap(clarity~.) +
#   scale_fill_viridis_c(option = "A")

# 2-1-2)
# stat(makePlot$C_CONSCIEN...8)

# makePlot = df %>% filter(...4=="Aunkown") %>%
#   # group_by(CRFNAME) %>%
#   dplyr::select(parental_eyo, CSF_xMAP_ptau, C_CONSCIEN...8) %>%
#   na.omit() %>%
#   readr::type_convert() %>%
#   ggplot(aes(x = parental_eyo, y = CSF_xMAP_ptau, colour = C_CONSCIEN...8)) +
#   geom_point() +
#   # stat_density_2d(aes(fill = C_CONSCIEN...8), geom = "polygon", n = 100, contour = TRUE) +
#   # scale_color_viridis_c(option = "A") +
#   scale_color_gradientn(colours = colorRamps::matlab.like(11), na.value = NA) +
#   labs(
#     x = "Parental estimated years from expected symptom onset (Parental EYO)"
#     , y = "CSF p-tau"
#     , colour = "CSF p-tau"
#     )
# 
# rayshader::plot_gg(makePlot, multicore=TRUE, raytrace = TRUE, zoom = 0.8, width=5, height=5, scale=250)
# saveMp4 = sprintf("%s/%s_%s.mp4", globalVar$figPath, serviceName, "consciousness-CSFptau_2D_plot")
# file.create(fs::path_dir(saveMp4), showWarnings = FALSE)
# rayshader::render_movie(saveMp4)
# rgl::rgl.close()
# 

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
# df %>% filter(...4=="Aunkown")  %>%
#   # group_by(CRFNAME) %>%
#   dplyr::select(parental_eyo, CSF_xMAP_ptau, C_CONSCIEN...8) %>%
#   na.omit() %>%
#   readr::type_convert() %>%
#   ggplot(aes(x = parental_eyo, y = CSF_xMAP_ptau, colour = C_CONSCIEN...8, fill = C_CONSCIEN...8)) +
#   geom_point() +
#   geom_tile(interpolate = TRUE) +
#   # scale_color_viridis_c(option = "A") +
#   scale_color_gradientn(colours = colorRamps::matlab.like(11), na.value = NA) +
#   labs(
#     x = "Parental estimated years from expected symptom onset (Parental EYO)"
#     , y = "CSF p-tau"
#     , colour = "CSF p-tau"
#   )

# 
# rayshader::plot_3d
# 
# plot_3d(volcano)
# plot_3d(volcano, zscale=3, fov=30)
# 
# volcano %>%
#   sphere_shade() %>%
#   plot_3d(volcano, zscale=3, fov=30)
# 
# montereybay %>%
#   sphere_shade(texture="imhof2") %>%
#   plot_3d(montereybay, zscale=50, water = TRUE, watercolor="imhof2",
#           waterlinecolor="white", waterlinealpha=0.5)
# 
# library(raster)
# 
# dataL3 = dataL2
# coordinates(dataL3) <- ~ xAxis + yAxis
# gridded(dataL3) <- TRUE
# 
# dataL3 = dataL2 %>% 
#   rasterFromXYZ()
# plot(dataL3)
# 
# dataL3 %>%
#   # sphere_shade(texture="imhof2") %>%
#   plot_3d(dataL3, zscale=50)
# 
# 
# 
# # dfr <- rasterFromXYZ(df)  #Convert first two columns as lon-lat and third as value                
# # plot(dfr)
# 
# rgl::rgl.close()
# 
# library(rayshader)
# volcano %>%
#   sphere_shade() %>%
#   # add_shadow(ray_shade(volcano,zscale=3),0.3) %>%
#   plot_3d(volcano, zscale=3, fov=30)
# render_movie("basic_video.mp4", title_text = "Basic rayshader plot",
#              title_bar_color = "red", title_bar_alpha = 0.3)
# rgl::rgl.close()

