
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
# R을 이용한 광학 및 화학 시정 시각화 (2D 빈도분포 산점도, 시계열, 박스플롯, 시정예측, 영향도)

# 1. 산점도 -> 2차원 빈도분포 산점도 표시
# 2. 전체 기간에 대한 시계열
# 3. 기상청 시정 등급 (좋음, 보통, 나쁨, 매우나쁨)에 따른 화학-광학 시정 박스플롯 (+평균)
# 4. 광학시정 / 화학시정으로 기상청 시정예측
# 5. 상대습도 (or 다른성분이) 시정에 미치는 영향

# 상대습도에_따른_시정_영향_(PM2.5_reconstruct).png
# 시정 영향 (소멸계수 b ext, PM25 농도)을 파악하기 위해서
# 상대습도에 따른 소멸계수 (b ext) 산포도와 PM25 농도를 컬러 색으로 나타내었다.
# 그 결과 상대습도와 소멸계수의 경우 상관계수는 0.49로서 양의 관계를 띠며 통계적으로 유의함 (P값 < 0.01 이하)을 볼 수 있다.
# 반면에 상대습도와 PM25에서는 뚜렷한 특징을 보이지 않는다.
# 이러한 원인은 --- 논문에서와 같이 --로 판단된다.
#
# 한편 소멸계수와 PM25 농도의 경우 소멸계수가 낮을 경우 대체로 PM25 농도가 낮게 분포함을 확인할 수 있다.
# 이는 ---으로 사료된다.
#
# 총_소멸계수에_따른_PM25_영향.png
# PM25 영향을 파악하기 위해서
# 소멸계수에 따른 PM25 농도의 산포도 및 선형회귀곡선 및 유의성검정을 수행하였다.
# 즉 NHNO의 경우 소멸계수와 PM25의 농도는 양의 상관계수 (R = 0.57)를 띠는 반면
# OMC, NHSO에서는 각각 -0.35, -0.23로서 뚜렷한 음의 상관계수를 보인다.
# 이는 ---으로 때문으로 판단된다 (참조 논문).

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0315"
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
cbMatlab2 = rev(colorRamps::matlab.like2(11))


#================================================
# 2020.06.22 요청사항
#================================================
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "IMPROVE 시정 계산_겨울철.xlsx"))
fileInfo2 = Sys.glob(file.path(globalVar$inpPath, serviceName, "최종수정 및 추가 데이터.xlsx"))


# ******************************************************************************
# 시정 비교
# ******************************************************************************
data = openxlsx::read.xlsx(fileInfo, sheet = 4)

dataL1 = data %>% 
  as.tibble() %>% 
  dplyr::mutate(
    dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
    , dtYear = lubridate::year(dtDateTime)
    , dtMonth = lubridate::month(dtDateTime)
    , dtXran = lubridate::decimal_date(dtDateTime)
  ) 

summary(dataL1)


# 2D 빈도분포 산점도
xAxis = dataL1$Chemical
yAxis = dataL1$Optical

xcord = 22.5
ycord = seq(155, 0, -7.5)
cbSpectral = rev(RColorBrewer::brewer.pal(11, "Spectral"))

perfEvalInfo = perfEval(xAxis, yAxis)
sprintf("%.3f", perfEvalInfo)

plotSubTitle = sprintf("%s", "2D 빈도분포 시정 산점도 (Chemical visibility vs Optical visibility)")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggplot() +
  coord_fixed(ratio = 1) +
  theme_bw() +
  stat_bin2d(aes(xAxis, yAxis), binwidth = c(2, 2)) +
  scale_fill_gradientn(colours = cbSpectral, limits = c(0, 10), na.value = cbSpectral[11]) +
  annotate("text", x = xcord, y = ycord[1], label = paste0("(Optical) = ", sprintf("%.2f", perfEvalInfo[1])," x (Chemical) + ", sprintf("%.2f", perfEvalInfo[2])), size = 7, hjust = 0, color = "red") +
  annotate("text", x = xcord, y = ycord[2], label = paste0("R = ", sprintf("%.2f", perfEvalInfo[12]), " (p-value < ", sprintf("%.3f", perfEvalInfo[13]), ")"), size = 7, hjust = 0, color = "red") +
  annotate("text", x = xcord, y = ycord[3], label = paste0("Bias = ", sprintf("%.2f", perfEvalInfo[8]), " (", sprintf("%.2f", perfEvalInfo[9])," %)"), parse = FALSE, size = 7, hjust = 0) +
  annotate("text", x = xcord, y = ycord[4], label = paste0("RMSE = ", sprintf("%.2f", perfEvalInfo[10]), " (", sprintf("%.2f", perfEvalInfo[11])," %)"), parse = FALSE, size = 7, hjust = 0) +
  annotate("text", x = xcord, y = ycord[5], label=paste0("N = ", format(perfEvalInfo[7], big.mark = ",", scientific = FALSE)), size = 7, hjust = 0, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
  stat_smooth(aes(xAxis, yAxis), method = "lm", color = "red", se = FALSE) +
  scale_x_continuous(minor_breaks = seq(0, 160, 20), breaks=seq(0, 160, 20),  expand=c(0,0), limits=c(0,  160)) +
  scale_y_continuous(minor_breaks = seq(0, 160, 20), breaks=seq(0, 160, 20),  expand=c(0,0), limits=c(0,  160)) +
  labs(
    title = NULL
    , x = "Chemical  visibility  [km]"
    , y = "Optical  visibility  [km]"
    , fill = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 24, color = "black")
    , axis.title.x = element_text(size = 24, colour = "black")
    , axis.title.y = element_text(size = 24, colour = "black")
    , axis.text.x = element_text(size = 19, colour = "black")
    , axis.text.y = element_text(size = 19, colour = "black")
    , legend.position = c(0, 1), legend.justification = c(0, 0.96)
    , legend.key=element_blank()
    , legend.text=element_text(size=19)
    , legend.background=element_blank()
    , plot.margin=unit(c(0,4,0,1), "mm")
  ) +
  ggsave(filename = saveImg, width = 6, height = 6, dpi = 1000)


# 전체 기간에 대한 시계열
dataL2 = dataL1 %>% 
  dplyr::select(-c("Date.time", "sDateTime", "dtYear", "dtMonth", "dtXran")) %>% 
  tidyr::gather(-dtDateTime, key = "key", value = "val")

summary(dataL2)

# 정렬
dataL2$key = forcats::fct_relevel(dataL2$key, c("Optical", "Chemical"))

subTitle = sprintf("%s", "전체 기간에 대한 시정 시계열 (Chemical visibility, Optical visibility)")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)

ggplot(data = dataL2, aes(x = dtDateTime, y = val, color = key)) +
  geom_line() +
  scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "5 day") +
  scale_y_continuous(minor_breaks = seq(0, 160, 20), breaks=seq(0, 160, 20),  expand=c(0,0), limits=c(0,  160)) +
  labs(title = NULL, x = "Date", y = "Visibility  [km]", colour = NULL, fill = NULL, subtitle = subTitle) +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


# ******************************************************************************
# PM2.5 비교
# ******************************************************************************
data = openxlsx::read.xlsx(fileInfo, sheet = 5)

dataL1 = data %>% 
  as.tibble() %>% 
  dplyr::mutate(
    dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
    , dtYear = lubridate::year(dtDateTime)
    , dtMonth = lubridate::month(dtDateTime)
    , dtXran = lubridate::decimal_date(dtDateTime)
  ) 

summary(dataL1)


# 2D 빈도분포 산점도
xAxis = dataL1$PM2.5
yAxis = dataL1$PM2.5_reconstruct

xcord = 22.5
ycord = seq(155, 0, -7.5)
cbSpectral = rev(RColorBrewer::brewer.pal(11, "Spectral"))

perfEvalInfo = perfEval(xAxis, yAxis)
sprintf("%.3f", perfEvalInfo)

plotSubTitle = sprintf("%s", "2D 빈도분포 시정 산점도 (PM2.5 vs PM2.5_reconstruct)")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggplot() +
  coord_fixed(ratio = 1) +
  theme_bw() +
  stat_bin2d(aes(xAxis, yAxis), binwidth = c(2, 2)) +
  scale_fill_gradientn(colours = cbSpectral, limits = c(0, 10), na.value = cbSpectral[11]) +
  annotate("text", x = xcord, y = ycord[1], label = paste0("Y = ", sprintf("%.2f", perfEvalInfo[1])," X + ", sprintf("%.2f", perfEvalInfo[2])), size = 7, hjust = 0, color = "red") +
  annotate("text", x = xcord, y = ycord[2], label = paste0("R = ", sprintf("%.2f", perfEvalInfo[12]), " (p-value < ", sprintf("%.3f", perfEvalInfo[13]), ")"), size = 7, hjust = 0, color = "red") +
  annotate("text", x = xcord, y = ycord[3], label = paste0("Bias = ", sprintf("%.2f", perfEvalInfo[8]), " (", sprintf("%.2f", perfEvalInfo[9])," %)"), parse = FALSE, size = 7, hjust = 0) +
  annotate("text", x = xcord, y = ycord[4], label = paste0("RMSE = ", sprintf("%.2f", perfEvalInfo[10]), " (", sprintf("%.2f", perfEvalInfo[11])," %)"), parse = FALSE, size = 7, hjust = 0) +
  annotate("text", x = xcord, y = ycord[5], label=paste0("N = ", format(perfEvalInfo[7], big.mark = ",", scientific = FALSE)), size = 7, hjust = 0, color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
  stat_smooth(aes(xAxis, yAxis), method = "lm", color = "red", se = FALSE) +
  scale_x_continuous(minor_breaks = seq(0, 160, 20), breaks=seq(0, 160, 20),  expand=c(0,0), limits=c(0,  160)) +
  scale_y_continuous(minor_breaks = seq(0, 160, 20), breaks=seq(0, 160, 20),  expand=c(0,0), limits=c(0,  160)) +
  labs(
    title = NULL
    , x = bquote('PM' ['2.5'])
    , y = bquote('PM' ['2.5_reconstruct'])
    # , x = "PM2.5"
    # , y = "PM2.5_reconstruct"
    , fill = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 24, color = "black")
    , axis.title.x = element_text(size = 24, colour = "black")
    , axis.title.y = element_text(size = 24, colour = "black", angle = 90)
    , axis.text.x = element_text( size = 19, colour = "black")
    , axis.text.y = element_text(size = 19, colour = "black")
    , legend.position = c(0, 1), legend.justification = c(0, 0.96)
    , legend.key=element_blank()
    , legend.text=element_text(size=19)
    , legend.background=element_blank()
    , plot.margin=unit(c(0,4,0,1), "mm")
  ) +
  ggsave(filename = saveImg, width = 6, height = 6, dpi = 1000)


# 전체 기간에 대한 시계열
dataL2 = dataL1 %>% 
  dplyr::select(-c("Date.time", "sDateTime", "dtYear", "dtMonth", "dtXran")) %>% 
  tidyr::gather(-dtDateTime, key = "key", value = "val")

summary(dataL2)

# 정렬
dataL2$key = forcats::fct_relevel(dataL2$key, c("PM2.5", "PM2.5_reconstruct"))

subTitle = sprintf("%s", "전체 기간에 대한 시정 시계열 (PM2.5, PM2.5_reconstruct)")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)

ggplot(data = dataL2, aes(x = dtDateTime, y = val, color = key)) +
  geom_line() +
  scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "5 day") +
  labs(
    title = NULL
    , x = "Date"
    , y = bquote('Concentration  ['*ug/m^3*']')
    , colour = NULL
    , fill = NULL
    , subtitle = NULL
  ) +
  scale_colour_manual(
    name = NULL
    , na.value = "transparent"
    , values = c("PM2.5" = ggplotDefaultColor[1], "PM2.5_reconstruct" = ggplotDefaultColor[2])
    , labels = c(parse(text = sprintf('PM["2.5"]')), parse(text = sprintf('PM["2.5_reconstruct"]')))
  ) +
  scale_y_continuous(minor_breaks = seq(0, 160, 20), breaks=seq(0, 160, 20),  expand=c(0,0), limits=c(0,  160)) +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


# ******************************************************************************
# 화학성분PM2.5 recon과 총소멸계수
# ******************************************************************************
# data = openxlsx::read.xlsx(fileInfo, sheet = 6)
data = openxlsx::read.xlsx(fileInfo2, sheet = 4)

dataL1 = data %>% 
  as.tibble() %>%
  dplyr::mutate(
    dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
    , dtYear = lubridate::year(dtDateTime)
    , dtMonth = lubridate::month(dtDateTime)
    , dtXran = lubridate::decimal_date(dtDateTime)
  ) 

summary(dataL1)

colnames(dataL1)

colnames(dataL2)

dataL2 = dataL1 %>% 
  dplyr::select(-c("Date.time", "sDateTime", "dtYear", "dtMonth", "dtXran", "PM2.5_reconstruct", "To_ext", "NHSO", "NHNO", "SS", "OMC", "EC", "FS")) %>% 
  tidyr::gather(-dtDateTime, -`총소멸계수(Bext)`, key = "key", value = "val") %>% 
  dplyr::mutate(
    type = dplyr::case_when(
      key == "NHSO/PM2.5_recon" ~ sprintf('NHSO/PM["2.5_reconstruct"]')
      , key == "NHNO/PM2.5_recon"~ sprintf('NHNO/PM["2.5_reconstruct"]')
      , key == "SS/PM2.5_recon" ~ sprintf('SS/PM["2.5_reconstruct"]')
      , key == "OMC/PM2.5_recon" ~ sprintf('OMC/PM["2.5_reconstruct"]')
      , key == "EC/PM2.5_recon" ~ sprintf('EC/PM["2.5_reconstruct"]')
      , key == "FS/PM2.5_recon" ~ sprintf('FS/PM["2.5_reconstruct"]')
      , TRUE ~ NA_character_
    )
  )

summary(dataL2)

subTitle = sprintf("%s", "총 소멸계수에 따른 PM25 영향")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)

ggplot(data = dataL2, aes(x = `총소멸계수(Bext)`, y = val, color = val)) +
  geom_point(size = 2, show.legend = FALSE) +
  geom_smooth(method = 'lm', se = TRUE, color = "black") +
  ggpubr::stat_regline_equation(label.x.npc = 0.025, label.y.npc = 1.0, size = 4, aes(label = ..eq.label..), color = "black", parse = TRUE) +
  ggpubr::stat_cor(label.x.npc = 0.025, label.y.npc = 0.90, size = 4, color = "black") +
  scale_color_gradientn(colours = cbMatlab2, na.value = cbMatlab2[11]) +
  scale_x_continuous(minor_breaks = seq(0, 1000, 200), breaks=seq(0, 1000, 200), limits=c(0, 1000)) +
  scale_y_continuous(minor_breaks = seq(0, 1.0, 0.2), breaks=seq(0, 1.0, 0.2), limits=c(0, 1.0)) +
  labs(
    title = NULL
    , x = bquote(b[ext_m]* '  ['*Mm^-1*']')
    , y = bquote('Chemical  compositions  in  '*PM[2.5*'_'*reconstruct])
    , color = NULL
    , fill = NULL
  ) +
  theme(
    text = element_text(size = 16)
  ) +
  facet_wrap(~type, scale = "free_x", labeller = label_parsed) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


# ******************************************************************************
# 상대습도와 PM2.5 recon
# ******************************************************************************
# data = openxlsx::read.xlsx(fileInfo, sheet = 7)
data = openxlsx::read.xlsx(fileInfo2, sheet = 5)


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
  dplyr::select(-c("Date.time", "sDateTime", "dtYear", "dtMonth", "dtXran")) %>% 
  tidyr::gather(-dtDateTime, -`총소멸계수(Bext)`, -RH, key = "key", value = "val") %>% 
  na.omit()

summary(dataL2)

subTitle = sprintf("%s", "상대습도에 따른 시정 영향 (PM2.5_reconstruct)")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)

lmFormula = y ~ poly(x, 2, raw = TRUE)

ggplot(data = dataL2, aes(x = RH, y = `총소멸계수(Bext)`, color = val)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm', formula = lmFormula, se = TRUE, color = "black") +
  ggpubr::stat_regline_equation(label.x.npc = 0.075, label.y.npc = 0.96, size = 5.5, aes(label = ..eq.label..), color = "black", formula = lmFormula, parse = TRUE) +
  ggpubr::stat_cor(label.x.npc = 0.075, label.y.npc = 0.90, size = 5.5, color = "black") +
  scale_color_gradientn(colours = cbMatlab2, limits = c(0, 100), na.value = cbMatlab2[11]) +
  scale_x_continuous(minor_breaks = seq(20, 100, 20), breaks=seq(20, 100, 20),  limits=c(20, 100)) +
  scale_y_continuous(minor_breaks = seq(0, 1600, 400), breaks=seq(0, 1600, 400), limits=c(0, 1600)) +
  labs(
    title = NULL
    , x = "Relative  humidity  [%]"
    , y = bquote('Reconstructed  ' *b[ext]* '  of  IMPROVE_2005  ['*Mm^-1*']')
    , color = bquote('PM' ['2.5']* ' concentration ['*mu*g/m^3*']')
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


# ******************************************************************************
# Case1-상위20% - Best case
# ******************************************************************************
data = openxlsx::read.xlsx(fileInfo2, sheet = 7)

# 4. 마지막으로 추가할 것은 말씀드린대로 계산한 화학시정거리 값을 가지고 상위 20%, 중간 60%, 하위20%로 나누어 보았습니다.
# 밑의 예시사진처럼 각 case 별로 원 그림으로 어떤 성부이 얼마나 차지하지는지랑 
# 총소멸계수에 얼마나 영향을 미치는지 보고싶습니다!! 영향을 미치는것도 그래프로 가능할지는 모르겠는데 한번 확인해봐주시면 감사하겠습니다!!!
# (즉 저는 케이스별로 NHSO, NHNO, SS, OMC, EC, FS 가 총소멸계수에 미치는 포션그래프와 영향을 보고싶습니다!)

dataL1 = data %>% 
  as.tibble() %>% 
  dplyr::mutate(
    dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
    , dtYear = lubridate::year(dtDateTime)
    , dtMonth = lubridate::month(dtDateTime)
    , dtXran = lubridate::decimal_date(dtDateTime)
  ) %>% 
  dplyr::filter(
   ! is.na(sDateTime) 
  )


summary(dataL1)

dataL2 = dataL1 %>% 
  dplyr::select(-c("date", "sDateTime", "dtYear", "dtMonth", "dtXran")) %>% 
  tidyr::gather(-dtDateTime, -`총소멸계수(Bext)`, -화학적.시정거리, key = "key", value = "val")

summary(dataL2)

dataL3 = dataL2 %>%
  dplyr::group_by(key) %>% 
  dplyr::summarise(
    cnt = n()
    , meanExt = mean(`총소멸계수(Bext)`, na.rm = TRUE)
    , meanVis = mean(화학적.시정거리, na.rm = TRUE)
    , meanVal = mean(val, na.rm = TRUE)
  ) %>% 
  dplyr::mutate(
    # per = cnt / sum(cnt, na.rm = TRUE) * 100
    per = meanVal / sum(meanVal, na.rm = TRUE) * 100
  )

subTitle = sprintf("%s", "상위20에 대한 파이차트")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)

dataL3$key = factor(dataL3$key)

ggplot(dataL3, aes(x = "", y = per, fill = key, label = paste0(round(per, 0), "%")) ) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), size = 5, color = "white") +
  labs(x = NULL, y = NULL, fill = NULL, subtitle = NULL) +
  coord_polar(theta = "y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
  # theme_minimal() +
  theme_void() +
  theme(
    text = element_text(size = 18)
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

subTitle = sprintf("%s", "상위20에 대한 통계 결과")
saveFile = sprintf("%s/%s_%s.xlsx", globalVar$outPath, serviceName, subTitle)

wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "상위20")
openxlsx::writeData(wb, "상위20", dataL3, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)



# ******************************************************************************
# Case2 - 중간값60% - Middle case
# ******************************************************************************
data = openxlsx::read.xlsx(fileInfo2, sheet = 8)

dataL1 = data %>% 
  as.tibble() %>% 
  dplyr::mutate(
    dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
    , dtYear = lubridate::year(dtDateTime)
    , dtMonth = lubridate::month(dtDateTime)
    , dtXran = lubridate::decimal_date(dtDateTime)
  ) %>% 
  dplyr::filter(
    ! is.na(sDateTime) 
  )

summary(dataL1)

dataL2 = dataL1 %>% 
  dplyr::select(-c("date", "sDateTime", "dtYear", "dtMonth", "dtXran")) %>% 
  tidyr::gather(-dtDateTime, -`총소멸계수(Bext)`, -화학적.시정거리, key = "key", value = "val") %>% 
  na.omit()

summary(dataL2)

dataL3 = dataL2 %>%
  dplyr::group_by(key) %>% 
  dplyr::summarise(
    cnt = n()
    , meanExt = mean(`총소멸계수(Bext)`, na.rm = TRUE)
    , meanVis = mean(화학적.시정거리, na.rm = TRUE)
    , meanVal = mean(val, na.rm = TRUE)
  ) %>% 
  dplyr::mutate(
    # per = cnt / sum(cnt, na.rm = TRUE) * 100
    per = meanVal / sum(meanVal, na.rm = TRUE) * 100
  )

subTitle = sprintf("%s", "중간60에 대한 파이차트")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)

dataL3$key = factor(dataL3$key)

ggplot(dataL3, aes(x = "", y = per, fill = key, label = paste0(round(per, 0), "%")) ) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), size = 5, color = "white") +
  labs(x = NULL, y = NULL, fill = NULL, subtitle = NULL) +
  coord_polar(theta = "y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
  # theme_minimal() +
  theme_void() +
  theme(
    text = element_text(size = 18)
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

openxlsx::addWorksheet(wb, "중간60")
openxlsx::writeData(wb, "중간60", dataL3, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)



# ******************************************************************************
# Case3-하위 20% - Worst case
# ******************************************************************************
data = openxlsx::read.xlsx(fileInfo2, sheet = 9)

dataL1 = data %>% 
  as.tibble() %>% 
  dplyr::mutate(
    dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
    , dtYear = lubridate::year(dtDateTime)
    , dtMonth = lubridate::month(dtDateTime)
    , dtXran = lubridate::decimal_date(dtDateTime)
  ) %>% 
  dplyr::filter(
    ! is.na(sDateTime) 
  )

summary(dataL1)

dataL2 = dataL1 %>% 
  dplyr::select(-c("date", "sDateTime", "dtYear", "dtMonth", "dtXran")) %>% 
  tidyr::gather(-dtDateTime, -`총소멸계수(Bext)`, -화학적.시정거리, key = "key", value = "val") %>% 
  na.omit()

summary(dataL2)

dataL3 = dataL2 %>%
  dplyr::group_by(key) %>% 
  dplyr::summarise(
    cnt = n()
    , meanExt = mean(`총소멸계수(Bext)`, na.rm = TRUE)
    , meanVis = mean(화학적.시정거리, na.rm = TRUE)
    , meanVal = mean(val, na.rm = TRUE)
  ) %>% 
  dplyr::mutate(
    # per = cnt / sum(cnt, na.rm = TRUE) * 100
    per = meanVal / sum(meanVal, na.rm = TRUE) * 100
  )

subTitle = sprintf("%s", "하위20에 대한 파이차트")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)

dataL3$key = factor(dataL3$key)

ggplot(dataL3, aes(x = "", y = per, fill = key, label = paste0(round(per, 0), "%")) ) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), size = 5, color = "white") +
  labs(x = NULL, y = NULL, fill = NULL, subtitle = NULL) +
  coord_polar(theta = "y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
  # theme_minimal() +
  theme_void() +
  theme(
    text = element_text(size = 18)
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

openxlsx::addWorksheet(wb, "하위20")
openxlsx::writeData(wb, "하위20", dataL3, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)


saveFile = sprintf("%s/%s_%s.xlsx", globalVar$outPath, serviceName, "상중하에 따른 통계 결과")
openxlsx::saveWorkbook(wb, file = saveFile, overwrite = TRUE)


# #================================================
# # 2020.06.16 요청사항
# #================================================
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "IMPROVE 시정 계산_겨울철.xlsx"))
# 
# 
# # ******************************************************************************
# # 시정 비교
# # ******************************************************************************
# data = openxlsx::read.xlsx(fileInfo, sheet = 4)
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
# 
# # 2D 빈도분포 산점도
# xAxis = dataL1$Chemical
# yAxis = dataL1$Optical
# 
# xcord = 22.5
# ycord = seq(155, 0, -7.5)
# cbSpectral = rev(RColorBrewer::brewer.pal(11, "Spectral"))
# 
# perfEvalInfo = perfEval(xAxis, yAxis)
# sprintf("%.3f", perfEvalInfo)
# 
# plotSubTitle = sprintf("%s", "2D 빈도분포 시정 산점도 (Chemical visibility vs Optical visibility)")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
# 
# ggplot() +
#   coord_fixed(ratio = 1) +
#   theme_bw() +
#   stat_bin2d(aes(xAxis, yAxis), binwidth = c(2, 2)) +
#   scale_fill_gradientn(colours = cbSpectral, limits = c(0, 10), na.value = cbSpectral[11]) +
#   annotate("text", x = xcord, y = ycord[1], label = paste0("(Optical) = ", sprintf("%.2f", perfEvalInfo[1])," x (Chemical) + ", sprintf("%.2f", perfEvalInfo[2])), size = 7, hjust = 0, color = "red") +
#   annotate("text", x = xcord, y = ycord[2], label = paste0("R = ", sprintf("%.2f", perfEvalInfo[12]), " (p-value < ", sprintf("%.3f", perfEvalInfo[13]), ")"), size = 7, hjust = 0, color = "red") +
#   annotate("text", x = xcord, y = ycord[3], label = paste0("Bias = ", sprintf("%.2f", perfEvalInfo[8]), " (", sprintf("%.2f", perfEvalInfo[9])," %)"), parse = FALSE, size = 7, hjust = 0) +
#   annotate("text", x = xcord, y = ycord[4], label = paste0("RMSE = ", sprintf("%.2f", perfEvalInfo[10]), " (", sprintf("%.2f", perfEvalInfo[11])," %)"), parse = FALSE, size = 7, hjust = 0) +
#   annotate("text", x = xcord, y = ycord[5], label=paste0("N = ", format(perfEvalInfo[7], big.mark = ",", scientific = FALSE)), size = 7, hjust = 0, color = "black") +
#   geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
#   stat_smooth(aes(xAxis, yAxis), method = "lm", color = "red", se = FALSE) +
#   scale_x_continuous(minor_breaks = seq(0, 160, 20), breaks=seq(0, 160, 20),  expand=c(0,0), limits=c(0,  160)) +
#   scale_y_continuous(minor_breaks = seq(0, 160, 20), breaks=seq(0, 160, 20),  expand=c(0,0), limits=c(0,  160)) +
#   labs(
#     title = plotSubTitle
#     , x = "Chemical  visibility  [km]"
#     , y = "Optical  visibility  [km]"
#     , fill = NULL
#   ) +
#   theme(
#     plot.title = element_text(face = "bold", size = 24, color = "black")
#     , axis.title.x = element_text(size = 24, colour = "black")
#     , axis.title.y = element_text(size = 24, colour = "black")
#     , axis.text.x = element_text(size = 19, colour = "black")
#     , axis.text.y = element_text(size = 19, colour = "black")
#     , legend.position = c(0, 1), legend.justification = c(0, 0.96)
#     , legend.key=element_blank()
#     , legend.text=element_text(size=19)
#     , legend.background=element_blank()
#   ) +
#   ggsave(filename = saveImg, width = 6, height = 6, dpi = 1200)
# 
# 
# # 전체 기간에 대한 시계열
# dataL2 = dataL1 %>% 
#   dplyr::select(-c("Date.time", "sDateTime", "dtYear", "dtMonth", "dtXran")) %>% 
#   tidyr::gather(-dtDateTime, key = "key", value = "val")
# 
# summary(dataL2)
# 
# # 정렬
# dataL2$key = forcats::fct_relevel(dataL2$key, c("Optical", "Chemical"))
# 
# subTitle = sprintf("%s", "전체 기간에 대한 시정 시계열 (Chemical visibility, Optical visibility)")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)
# 
# ggplot(data = dataL2, aes(x = dtDateTime, y = val, color = key)) +
#   geom_line() +
#   scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "5 day") +
#   scale_y_continuous(minor_breaks = seq(0, 160, 20), breaks=seq(0, 160, 20),  expand=c(0,0), limits=c(0,  160)) +
#   labs(title = NULL, x = "Date", y = "Visibility  [km]", colour = NULL, fill = NULL, subtitle = subTitle) +
#   theme(
#     text = element_text(size = 18)
#     , axis.text.x = element_text(angle = 45, hjust = 1)
#     , legend.position = "top"
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
# 
# 
# # ******************************************************************************
# # PM2.5 비교
# # ******************************************************************************
# data = openxlsx::read.xlsx(fileInfo, sheet = 5)
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
# 
# # 2D 빈도분포 산점도
# xAxis = dataL1$PM2.5
# yAxis = dataL1$PM2.5_reconstruct
# 
# xcord = 22.5
# ycord = seq(155, 0, -7.5)
# cbSpectral = rev(RColorBrewer::brewer.pal(11, "Spectral"))
# 
# perfEvalInfo = perfEval(xAxis, yAxis)
# sprintf("%.3f", perfEvalInfo)
# 
# plotSubTitle = sprintf("%s", "2D 빈도분포 시정 산점도 (PM2.5 vs PM2.5_reconstruct)")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
# 
# ggplot() +
#   coord_fixed(ratio = 1) +
#   theme_bw() +
#   stat_bin2d(aes(xAxis, yAxis), binwidth = c(2, 2)) +
#   scale_fill_gradientn(colours = cbSpectral, limits = c(0, 10), na.value = cbSpectral[11]) +
#   annotate("text", x = xcord, y = ycord[1], label = paste0("(PM2.5_reconstruct) = ", sprintf("%.2f", perfEvalInfo[1])," x (PM2.5) + ", sprintf("%.2f", perfEvalInfo[2])), size = 7, hjust = 0, color = "red") +
#   annotate("text", x = xcord, y = ycord[2], label = paste0("R = ", sprintf("%.2f", perfEvalInfo[12]), " (p-value < ", sprintf("%.3f", perfEvalInfo[13]), ")"), size = 7, hjust = 0, color = "red") +
#   annotate("text", x = xcord, y = ycord[3], label = paste0("Bias = ", sprintf("%.2f", perfEvalInfo[8]), " (", sprintf("%.2f", perfEvalInfo[9])," %)"), parse = FALSE, size = 7, hjust = 0) +
#   annotate("text", x = xcord, y = ycord[4], label = paste0("RMSE = ", sprintf("%.2f", perfEvalInfo[10]), " (", sprintf("%.2f", perfEvalInfo[11])," %)"), parse = FALSE, size = 7, hjust = 0) +
#   annotate("text", x = xcord, y = ycord[5], label=paste0("N = ", format(perfEvalInfo[7], big.mark = ",", scientific = FALSE)), size = 7, hjust = 0, color = "black") +
#   geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
#   stat_smooth(aes(xAxis, yAxis), method = "lm", color = "red", se = FALSE) +
#   scale_x_continuous(minor_breaks = seq(0, 160, 20), breaks=seq(0, 160, 20),  expand=c(0,0), limits=c(0,  160)) +
#   scale_y_continuous(minor_breaks = seq(0, 160, 20), breaks=seq(0, 160, 20),  expand=c(0,0), limits=c(0,  160)) +
#   labs(
#     title = plotSubTitle
#     , x = "PM2.5"
#     , y = "PM2.5_reconstruct"
#     , fill = NULL
#   ) +
#   theme(
#     plot.title = element_text(face = "bold", size = 24, color = "black")
#     , axis.title.x = element_text(size = 24, colour = "black")
#     , axis.title.y = element_text(size = 24, colour = "black", angle = 90)
#     , axis.text.x = element_text( size = 19, colour = "black")
#     , axis.text.y = element_text(size = 19, colour = "black")
#     , legend.position = c(0, 1), legend.justification = c(0, 0.96)
#     , legend.key=element_blank()
#     , legend.text=element_text(size=19)
#     , legend.background=element_blank()
#   ) +
#   ggsave(filename = saveImg, width = 6, height = 6, dpi = 1200)
# 
# 
# # 전체 기간에 대한 시계열
# dataL2 = dataL1 %>% 
#   dplyr::select(-c("Date.time", "sDateTime", "dtYear", "dtMonth", "dtXran")) %>% 
#   tidyr::gather(-dtDateTime, key = "key", value = "val")
# 
# summary(dataL2)
# 
# # 정렬
# dataL2$key = forcats::fct_relevel(dataL2$key, c("PM2.5", "PM2.5_reconstruct"))
# 
# subTitle = sprintf("%s", "전체 기간에 대한 시정 시계열 (PM2.5, PM2.5_reconstruct)")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)
# 
# ggplot(data = dataL2, aes(x = dtDateTime, y = val, color = key)) +
#   geom_line() +
#   scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "5 day") +
#   labs(title = NULL, x = "Date", y = bquote('Concentration  ['*ug/m^3*']'), colour = NULL, fill = NULL, subtitle = subTitle) +
#   scale_y_continuous(minor_breaks = seq(0, 160, 20), breaks=seq(0, 160, 20),  expand=c(0,0), limits=c(0,  160)) +
#   theme(
#     text = element_text(size = 18)
#     , axis.text.x = element_text(angle = 45, hjust = 1)
#     , legend.position = "top"
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
# 
# 
# # ******************************************************************************
# # 화학성분PM2.5 recon과 총소멸계수
# # ******************************************************************************
# data = openxlsx::read.xlsx(fileInfo, sheet = 6)
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
# colnames(dataL2)
# 
# dataL2 = dataL1 %>% 
#   dplyr::select(-c("Date.time", "sDateTime", "dtYear", "dtMonth", "dtXran", "PM2.5_reconstruct", "To_ext", "Sulfate.(SO42-)", "Nitrate.(NO3-)", "Cl-", "OM", "EC", "FS")) %>% 
#   tidyr::gather(-dtDateTime, -`총소멸계수(Bext)`, key = "key", value = "val") %>% 
#   dplyr::mutate(
#     type = dplyr::case_when(
#       key == "Cl-/PM2.5_recon" ~ "Cl-"
#       , key == "EC/PM2.5_recon"~ "EC"
#       , key == "FS/PM2.5_recon" ~ "FS"
#       , key == "NH3-/PM2.5_recon" ~ "NH3-"
#       , key == "OM/PM2.5_recon" ~ "OM"
#       , key == "SO42-/PM2.5_recon" ~ "SO42-"
#       , TRUE ~ NA_character_
#     )
#   ) %>% 
#   na.omit()
# 
# summary(dataL2)
# 
# subTitle = sprintf("%s", "총 소멸계수에 따른 PM25 영향")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)
# 
# ggplot(data = dataL2, aes(x = `총소멸계수(Bext)`, y = val, color = val)) +
#   geom_point(size = 2, show.legend = FALSE) +
#   geom_smooth(method = 'lm', se = TRUE, color = "black") +
#   ggpubr::stat_regline_equation(label.x.npc = 0.025, label.y.npc = 1.0, size = 4, aes(label = ..eq.label..), color = "black", parse = TRUE) +
#   ggpubr::stat_cor(label.x.npc = 0.025, label.y.npc = 0.90, size = 4, color = "black") +
#   scale_color_gradientn(colours = cbMatlab2, na.value = cbMatlab2[11]) +
#   scale_x_continuous(minor_breaks = seq(0, 600, 200), breaks=seq(0, 600, 200), limits=c(0, 600)) +
#   scale_y_continuous(minor_breaks = seq(0, 0.5, 0.1), breaks=seq(0, 0.5, 0.1), limits=c(0, 0.5)) +
#   labs(title = subTitle, x = bquote(b[ext_m]* '  ['*Mm^-1*']'), y = bquote('Chemical  compositions  in  '*PM[2.5*'_'*reconstruct]), color = NULL, fill = NULL) +
#   theme(
#     text = element_text(size = 16)
#   ) +
#   facet_wrap(~type, scale = "free_x") +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
# 
# 
# # ******************************************************************************
# # 상대습도와 PM2.5 recon
# # ******************************************************************************
# data = openxlsx::read.xlsx(fileInfo, sheet = 7)
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
#   dplyr::select(-c("Date.time", "sDateTime", "dtYear", "dtMonth", "dtXran")) %>% 
#   tidyr::gather(-dtDateTime, -RH, key = "key", value = "val") %>% 
#   na.omit()
# 
# summary(dataL2)
# 
# subTitle = sprintf("%s", "상대습도에 따른 시정 영향 (PM2.5_reconstruct)")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)
# 
# lmFormula = y ~ poly(x, 2, raw = TRUE)
# 
# ggplot(data = dataL2, aes(x = RH, y = val, color = val)) +
#   geom_point(size = 2) +
#   geom_smooth(method = 'lm', formula = lmFormula, se = TRUE, color = "black") +
#   ggpubr::stat_regline_equation(label.x.npc = 0.075, label.y.npc = 1.0, size = 5, aes(label = ..eq.label..), color = "black", formula = lmFormula, parse = TRUE) +
#   ggpubr::stat_cor(label.x.npc = 0.075, label.y.npc = 0.90, size = 5, color = "black") +
#   scale_color_gradientn(colours = cbMatlab2, limits = c(0, 200), na.value = cbMatlab2[11]) +
#   scale_x_continuous(minor_breaks = seq(20, 100, 20), breaks=seq(20, 100, 20),  limits=c(20, 100)) +
#   scale_y_continuous(minor_breaks = seq(0, 160, 40), breaks=seq(0, 160, 40), limits=c(0, 160)) +
#   labs(title = subTitle, x = "Relative  humidity  [%]", y = bquote('Reconstructed  ' *b[ext]* '  of  IMPROVE_2005  ['*Mm^-1*']'), color = NULL, fill = NULL) +
#   theme(
#     text = element_text(size = 18)
#     # , legend.position = "top"
#     , legend.position = c(0, 1), legend.justification = c(0, 0.96)
#     , legend.key=element_blank()
#     , legend.text=element_text(size=15)
#     , legend.background=element_blank()
#   ) +
#   # facet_wrap(~type, scale = "free") +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)














# #================================================
# # 2020.06.03 요청사항
# #================================================
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "광학vs화학시정 통계문의.xlsx"))
# 
# # 시정 비교
# data = openxlsx::read.xlsx(fileInfo, sheet = 4)
# 
# # 부가정보 (상대습도)
# rhData = openxlsx::read.xlsx(fileInfo, sheet = 6) %>% 
#   as.tibble() %>% 
#   dplyr::mutate(
#     dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
#   ) %>% 
#   dplyr::select(dtDateTime, rh)
# 
# dataL1 = data %>% 
#   as.tibble() %>% 
#   dplyr::mutate(
#     dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M")
#     , dtYear = lubridate::year(dtDateTime)
#     , dtMonth = lubridate::month(dtDateTime)
#     , dtXran = lubridate::decimal_date(dtDateTime)
#   ) %>% 
#   dplyr::rename(
#     val = 화학시정
#     , val2 = 광학.시정
#     , ref = 기상청.시정
#   ) %>% 
#   dplyr::left_join(rhData, by = c("dtDateTime" = "dtDateTime"))
# 
# summary(dataL1)
# 
# 
# 
# 
# # ==============================================================================
# # 1. 산점도 -> 2D 빈도분포 산점도 표시
# # ==============================================================================
# # 시정 20 km 내에서 자료 필터
# dataL2 = dataL1 %>% 
#   dplyr::filter(
#     dplyr::between(val, 0, 20)
#     , dplyr::between(val2, 0, 20)
#     , dplyr::between(ref, 0, 20)
#   )
# 
# summary(dataL2)
# 
# # *********************************************
# # 화학 시정 vs 기상청 시정
# # *********************************************
# xAxis = dataL2$val
# yAxis = dataL2$ref
# 
# xcord = 2.5
# ycord = seq(19.5, 0, -1.0)
# cbSpectral = rev(RColorBrewer::brewer.pal(11, "Spectral"))
# 
# perfEvalInfo = perfEval(xAxis, yAxis)
# sprintf("%.3f", perfEvalInfo)
# 
# plotSubTitle = sprintf("%s", "2D 빈도분포 시정 산점도 (화학 vs 기상청)")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
# 
# ggplot() +
#   coord_fixed(ratio = 1) +
#   theme_bw() +
#   stat_bin2d(aes(xAxis, yAxis), binwidth = c(0.5, 0.5)) +
#   scale_fill_gradientn(colours = cbSpectral, limits = c(0, 10), na.value = cbSpectral[11]) +
#   annotate("text", x = xcord, y = ycord[1], label = paste0("(기상청) = ", sprintf("%.2f", perfEvalInfo[1])," x (화학) + ", sprintf("%.2f", perfEvalInfo[2])), size = 6, hjust = 0, color = "red") +
#   annotate("text", x = xcord, y = ycord[2], label = paste0("R = ", sprintf("%.2f", perfEvalInfo[12]), " (p-value < ", sprintf("%.3f", perfEvalInfo[13]), ")"), size = 6, hjust = 0, color = "red") +
#   annotate("text", x = xcord, y = ycord[3], label = paste0("Bias = ", sprintf("%.2f", perfEvalInfo[8]), " (", sprintf("%.2f", perfEvalInfo[9])," %)"), parse = FALSE, size = 6, hjust = 0) +
#   annotate("text", x = xcord, y = ycord[4], label = paste0("RMSE = ", sprintf("%.2f", perfEvalInfo[10]), " (", sprintf("%.2f", perfEvalInfo[11])," %)"), parse = FALSE, size = 6, hjust = 0) +
#   annotate("text", x = xcord, y = ycord[5], label=paste0("N = ", format(perfEvalInfo[7], big.mark = ",", scientific = FALSE)), size = 6, hjust = 0, color = "black") +
#   geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
#   stat_smooth(aes(xAxis, yAxis), method = "lm", color = "red", se = FALSE) +
#   scale_x_continuous(minor_breaks = seq(0, 20, 2), breaks=seq(0, 20, 2),  expand=c(0,0), limits=c(0,  20)) +
#   scale_y_continuous(minor_breaks = seq(0, 20, 2), breaks=seq(0, 20, 2),  expand=c(0,0), limits=c(0,  20)) +
#   labs(
#     title = plotSubTitle
#     , x = "화학  시정 [km]"
#     , y = "기상청  시정 [km]"
#     , fill = NULL
#   ) +
#   theme(
#     plot.title = element_text(face = "bold", size = 20, color = "black")
#     , axis.title.x = element_text(face = "bold", size = 19, colour = "black")
#     , axis.title.y = element_text(face = "bold", size = 19, colour = "black", angle = 90)
#     , axis.text.x = element_text( size = 19, colour = "black")
#     , axis.text.y = element_text(size = 19, colour = "black")
#     , legend.position = c(0, 1), legend.justification = c(0, 0.96)
#     , legend.key=element_blank()
#     , legend.text=element_text(size=15)
#     , legend.background=element_blank()
#   ) +
#   ggsave(filename = saveImg, width = 6, height = 6, dpi = 1200)
# 
# 
# 
# # *********************************************
# # 광학 시정 vs 기상청 시정
# # *********************************************
# xAxis = dataL2$val2
# yAxis = dataL2$ref
# 
# xcord = 2.5
# ycord = seq(19.5, 0, -1.0)
# cbSpectral = rev(RColorBrewer::brewer.pal(11, "Spectral"))
# 
# perfEvalInfo = perfEval(xAxis, yAxis)
# sprintf("%.3f", perfEvalInfo)
# 
# plotSubTitle = sprintf("%s", "2D 빈도분포 시정 산점도 (광학 vs 기상청)")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
# 
# ggplot() +
#   coord_fixed(ratio = 1) +
#   theme_bw() +
#   stat_bin2d(aes(xAxis, yAxis), binwidth = c(0.5, 0.5)) +
#   scale_fill_gradientn(colours = cbSpectral, limits = c(0, 10), na.value = cbSpectral[11]) +
#   annotate("text", x = xcord, y = ycord[1], label = paste0("(기상청) = ", sprintf("%.2f", perfEvalInfo[1])," x (광학) + ", sprintf("%.2f", perfEvalInfo[2])), size = 6, hjust = 0, color = "red") +
#   annotate("text", x = xcord, y = ycord[2], label = paste0("R = ", sprintf("%.2f", perfEvalInfo[12]), " (p-value < ", sprintf("%.3f", perfEvalInfo[13]), ")"), size = 6, hjust = 0, color = "red") +
#   annotate("text", x = xcord, y = ycord[3], label = paste0("Bias = ", sprintf("%.2f", perfEvalInfo[8]), " (", sprintf("%.2f", perfEvalInfo[9])," %)"), parse = FALSE, size = 6, hjust = 0) +
#   annotate("text", x = xcord, y = ycord[4], label = paste0("RMSE = ", sprintf("%.2f", perfEvalInfo[10]), " (", sprintf("%.2f", perfEvalInfo[11])," %)"), parse = FALSE, size = 6, hjust = 0) +
#   annotate("text", x = xcord, y = ycord[5], label=paste0("N = ", format(perfEvalInfo[7], big.mark = ",", scientific = FALSE)), size = 6, hjust = 0, color = "black") +
#   geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
#   stat_smooth(aes(xAxis, yAxis), method = "lm", color = "red", se = FALSE) +
#   scale_x_continuous(minor_breaks = seq(0, 20, 2), breaks=seq(0, 20, 2),  expand=c(0,0), limits=c(0,  20)) +
#   scale_y_continuous(minor_breaks = seq(0, 20, 2), breaks=seq(0, 20, 2),  expand=c(0,0), limits=c(0,  20)) +
#   labs(
#     title = plotSubTitle
#     , x = "광학  시정 [km]"
#     , y = "기상청  시정 [km]"
#     , fill = NULL
#   ) +
#   theme(
#     plot.title = element_text(face = "bold", size = 20, color = "black")
#     , axis.title.x = element_text(face = "bold", size = 19, colour = "black")
#     , axis.title.y = element_text(face = "bold", size = 19, colour = "black", angle = 90)
#     , axis.text.x = element_text( size = 19, colour = "black")
#     , axis.text.y = element_text(size = 19, colour = "black")
#     , legend.position = c(0, 1), legend.justification = c(0, 0.96)
#     , legend.key=element_blank()
#     , legend.text=element_text(size=15)
#     , legend.background=element_blank()
#   ) +
#   ggsave(filename = saveImg, width = 6, height = 6, dpi = 1200)
# 
# 
# # ==============================================================================
# # 2. 전체 기간에 대한 시계열
# # ==============================================================================
# dataL3 = dataL1 %>% 
#   dplyr::select(-c("Date.time", "sDateTime", "rh", "dtYear", "dtMonth", "dtXran")) %>% 
#   tidyr::gather(-dtDateTime, key = "key", value = "val") %>% 
#   dplyr::mutate(
#     type = dplyr::case_when(
#       key == "val" ~ "화학"
#       , key == "val2"~ "광학"
#       , key == "ref" ~ "기상청"
#       , TRUE ~ NA_character_
#     )
#   )
# 
# summary(dataL3)
# 
# # 정렬
# dataL3$type = forcats::fct_relevel(dataL3$type, c("광학", "화학", "기상청"))
# 
# subTitle = sprintf("%s", "전체 기간에 대한 시정 시계열 (광학, 화학, 기상청)")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)
# 
# ggplot(data = dataL3, aes(x = dtDateTime, y = val, color = type)) +
#   geom_line() +
#   labs(title = NULL, x = "날짜", y = "시정 [km]", colour = NULL, fill = NULL, subtitle = subTitle) +
#   theme(
#     text = element_text(size = 18)
#     , legend.position = "top"
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
# 
# # ==============================================================================
# # 3. 기상청 시정 등급에 따른 화학-광학 시정 박스플롯 (+평균)
# # ==============================================================================
# dataL4 = dataL1 %>% 
#   dplyr::select(-c("Date.time", "sDateTime", "rh", "dtYear", "dtMonth", "dtXran")) %>% 
#   tidyr::gather(-dtDateTime, key = "key", value = "val") %>% 
#   dplyr::mutate(
#     type = dplyr::case_when(
#       key == "val" ~ "화학"
#       , key == "val2"~ "광학"
#       , key == "ref" ~ "기상청"
#       , TRUE ~ NA_character_
#     )
#     , visType = dplyr::case_when(
#       val <= 0.04 ~ "Dense fog"
#       , 0.04 < val & val <= 0.2 ~ "Thick fog"
#       , 0.2 < val & val <= 1 ~ "Fog"
#       , 1 < val & val <= 2 ~ "Mist"
#       , 2 < val & val <= 4 ~ "Haze"
#       , 4 < val & val <= 10 ~ "Poor"
#       , 10 < val & val < 40 ~ "Good"
#       , 40 <= val ~ "Excellent"
#     )
#   ) %>% 
#   na.omit()
# 
# summary(dataL4)
# 
# 
# # 표출순서 정렬
# dataL4$visType = forcats::fct_relevel(dataL4$visType, c("Dense fog", "Thick fog", "Fog", "Mist", "Haze", "Poor", "Good", "Excellent"))
# dataL4$type = forcats::fct_relevel(dataL4$type, c("광학", "화학", "기상청"))
# 
# 
# subTitle = sprintf("%s", "기상청 시정 등급에 따른 박스플롯 (광학, 화학, 기상청)")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)
# 
# ggplot(data = dataL4, aes(x = visType, y = val, color = visType)) +
#   geom_boxplot() +
#   stat_summary(fun.y = mean, geom="point", colour="black", size=2) +
#   stat_summary(fun.y = mean, colour="black", geom="text", show_guide = FALSE, vjust=-0.7, aes(label = round(..y..,1))) +
#   labs(title = NULL, x = "시정 등급", y = "시정 [km]", color = NULL, fill = NULL, subtitle = subTitle) +
#   theme(
#     text = element_text(size = 18)
#     , legend.position = "top"
#     , axis.text.x = element_text(angle = 45, hjust = 1)
#   ) +
#   facet_wrap(~type, scale = "free") +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
# 
# 
# # ==============================================================================
# # 4. 광학시정 / 화학시정으로 기상청 시정예측
# # ==============================================================================
# rowInfo = dataL1 %>% 
#   tibble::rowid_to_column() %>% 
#   dplyr::filter(
#     dtDateTime <= as.Date("2022-01-01")
#   ) %>% 
#   dplyr::arrange(dtDateTime)
# 
# trainData = dataL1[rowInfo$rowid, ] %>% 
#   na.omit()
# 
# testData = dataL1[-rowInfo$rowid, ] %>% 
#   na.omit()
# 
# 
# # ******************************************************************************
# # 화학 시정으로 기상청 시정 예측
# # ******************************************************************************
# # 모형 학습
# saveModel = sprintf("%s/%s-%s-%s-%s.model", globalVar$outPath, serviceName, 'final', 'h2o', 'val')
# 
# # 초기화
# h2o::h2o.init()
# 
# # 모형 학습이 있을 경우
# if (fs::file_exists(saveModel)) {
#   # amlModel = h2o::h2o.loadModel(saveModel)
#   amlModel = h2o::h2o.import_mojo(saveModel)
# } else {
#   # 모형 학습
#   amlModel = h2o::h2o.automl(
#     x = c("dtXran", "dtYear", "dtMonth", "val")
#     , y = c("ref")
#     , training_frame = as.h2o(trainData)
#     , nfolds = 10
#     , sort_metric = "RMSE"
#     , stopping_metric = "RMSE"
#     , seed = 123
#     , max_models = 10
#   )
#   
#   amlBestModel = h2o.get_best_model(amlModel)
#   # h2o::h2o.saveModel(object = amlBestModel, path = fs::path_dir(saveModel), filename = fs::path_file(saveModel), force = TRUE)
#   h2o::h2o.save_mojo(object = amlBestModel, path = fs::path_dir(saveModel), filename = fs::path_file(saveModel), force = TRUE)
# }
# 
# summary(amlModel)
# 
# # 예측
# testData$prdDL = as.data.frame(h2o::h2o.predict(object = amlModel, newdata = as.h2o(testData)))$predict
# 
# # 시각화
# xAxis = testData$prdDL
# yAxis = testData$ref
# 
# xcord = 2.5
# ycord = seq(19.5, 0, -1.0)
# cbSpectral = rev(RColorBrewer::brewer.pal(11, "Spectral"))
# 
# perfEvalInfo = perfEval(xAxis, yAxis)
# sprintf("%.3f", perfEvalInfo)
# 
# plotSubTitle = sprintf("%s", "딥러닝을 이용한 2D 빈도분포 시정 산점도 (예측 화학 vs 실측 기상청)")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
# 
# ggplot() +
#   coord_fixed(ratio = 1) +
#   theme_bw() +
#   stat_bin2d(aes(xAxis, yAxis), binwidth = c(0.5, 0.5)) +
#   scale_fill_gradientn(colours = cbSpectral, limits = c(0, 10), na.value = cbSpectral[11]) +
#   annotate("text", x = xcord, y = ycord[1], label = paste0("(실측) = ", sprintf("%.2f", perfEvalInfo[1])," x (예측) + ", sprintf("%.2f", perfEvalInfo[2])), size = 6, hjust = 0, color = "red") +
#   annotate("text", x = xcord, y = ycord[2], label = paste0("R = ", sprintf("%.2f", perfEvalInfo[12]), " (p-value < ", sprintf("%.3f", perfEvalInfo[13]), ")"), size = 6, hjust = 0, color = "red") +
#   annotate("text", x = xcord, y = ycord[3], label = paste0("Bias = ", sprintf("%.2f", perfEvalInfo[8]), " (", sprintf("%.2f", perfEvalInfo[9])," %)"), parse = FALSE, size = 6, hjust = 0) +
#   annotate("text", x = xcord, y = ycord[4], label = paste0("RMSE = ", sprintf("%.2f", perfEvalInfo[10]), " (", sprintf("%.2f", perfEvalInfo[11])," %)"), parse = FALSE, size = 6, hjust = 0) +
#   annotate("text", x = xcord, y = ycord[5], label=paste0("N = ", format(perfEvalInfo[7], big.mark = ",", scientific = FALSE)), size = 6, hjust = 0, color = "black") +
#   geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
#   stat_smooth(aes(xAxis, yAxis), method = "lm", color = "red", se = FALSE) +
#   scale_x_continuous(minor_breaks = seq(0, 20, 2), breaks=seq(0, 20, 2),  expand=c(0,0), limits=c(0,  20)) +
#   scale_y_continuous(minor_breaks = seq(0, 20, 2), breaks=seq(0, 20, 2),  expand=c(0,0), limits=c(0,  20)) +
#   labs(
#     title = plotSubTitle
#     , x = "예측  화학  시정 [km]"
#     , y = "실측  기상청  시정 [km]"
#     , fill = NULL
#   ) +
#   theme(
#     plot.title = element_text(face = "bold", size = 20, color = "black")
#     , axis.title.x = element_text(face = "bold", size = 19, colour = "black")
#     , axis.title.y = element_text(face = "bold", size = 19, colour = "black", angle = 90)
#     , axis.text.x = element_text( size = 19, colour = "black")
#     , axis.text.y = element_text(size = 19, colour = "black")
#     , legend.position = c(0, 1), legend.justification = c(0, 0.96)
#     , legend.key=element_blank()
#     , legend.text=element_text(size=15)
#     , legend.background=element_blank()
#   ) +
#   ggsave(filename = saveImg, width = 6, height = 6, dpi = 1200)
# 
# 
# # *********************************************
# # 광학 시정으로 기상청 시정 예측
# # *********************************************
# # 모형 학습
# saveModel = sprintf("%s/%s-%s-%s-%s.model", globalVar$outPath, serviceName, 'final', 'h2o', 'val2')
# 
# # 초기화
# h2o::h2o.init()
# 
# # 모형 학습이 있을 경우
# if (fs::file_exists(saveModel)) {
#   # amlModel = h2o::h2o.loadModel(saveModel)
#   amlModel = h2o::h2o.import_mojo(saveModel)
# } else {
#   # 모형 학습
#   amlModel = h2o::h2o.automl(
#     x = c("dtXran", "dtYear", "dtMonth", "val2")
#     , y = c("ref")
#     , training_frame = as.h2o(trainData)
#     , nfolds = 10
#     , sort_metric = "RMSE"
#     , stopping_metric = "RMSE"
#     , seed = 123
#     , max_models = 10
#   )
#   
#   amlBestModel = h2o.get_best_model(amlModel)
#   # h2o::h2o.saveModel(object = amlBestModel, path = fs::path_dir(saveModel), filename = fs::path_file(saveModel), force = TRUE)
#   h2o::h2o.save_mojo(object = amlBestModel, path = fs::path_dir(saveModel), filename = fs::path_file(saveModel), force = TRUE)
# }
# 
# summary(amlModel)
# 
# # 예측
# testData$prdDL2 = as.data.frame(h2o::h2o.predict(object = amlModel, newdata = as.h2o(testData)))$predict
# 
# # 시각화
# xAxis = testData$prdDL2
# yAxis = testData$ref
# 
# xcord = 2.5
# ycord = seq(19.5, 0, -1.0)
# cbSpectral = rev(RColorBrewer::brewer.pal(11, "Spectral"))
# 
# perfEvalInfo = perfEval(xAxis, yAxis)
# sprintf("%.3f", perfEvalInfo)
# 
# plotSubTitle = sprintf("%s", "딥러닝을 이용한 2D 빈도분포 시정 산점도 (예측 광학 vs 실측 기상청)")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
# 
# ggplot() +
#   coord_fixed(ratio = 1) +
#   theme_bw() +
#   stat_bin2d(aes(xAxis, yAxis), binwidth = c(0.5, 0.5)) +
#   scale_fill_gradientn(colours = cbSpectral, limits = c(0, 10), na.value = cbSpectral[11]) +
#   annotate("text", x = xcord, y = ycord[1], label = paste0("(실측) = ", sprintf("%.2f", perfEvalInfo[1])," x (예측) + ", sprintf("%.2f", perfEvalInfo[2])), size = 6, hjust = 0, color = "red") +
#   annotate("text", x = xcord, y = ycord[2], label = paste0("R = ", sprintf("%.2f", perfEvalInfo[12]), " (p-value < ", sprintf("%.3f", perfEvalInfo[13]), ")"), size = 6, hjust = 0, color = "red") +
#   annotate("text", x = xcord, y = ycord[3], label = paste0("Bias = ", sprintf("%.2f", perfEvalInfo[8]), " (", sprintf("%.2f", perfEvalInfo[9])," %)"), parse = FALSE, size = 6, hjust = 0) +
#   annotate("text", x = xcord, y = ycord[4], label = paste0("RMSE = ", sprintf("%.2f", perfEvalInfo[10]), " (", sprintf("%.2f", perfEvalInfo[11])," %)"), parse = FALSE, size = 6, hjust = 0) +
#   annotate("text", x = xcord, y = ycord[5], label=paste0("N = ", format(perfEvalInfo[7], big.mark = ",", scientific = FALSE)), size = 6, hjust = 0, color = "black") +
#   geom_abline(intercept = 0, slope = 1, linetype = 1, color = "black", size = 1.0) +
#   stat_smooth(aes(xAxis, yAxis), method = "lm", color = "red", se = FALSE) +
#   scale_x_continuous(minor_breaks = seq(0, 20, 2), breaks=seq(0, 20, 2),  expand=c(0,0), limits=c(0,  20)) +
#   scale_y_continuous(minor_breaks = seq(0, 20, 2), breaks=seq(0, 20, 2),  expand=c(0,0), limits=c(0,  20)) +
#   labs(
#     title = plotSubTitle
#     , x = "예측  광학  시정 [km]"
#     , y = "실측  기상청  시정 [km]"
#     , fill = NULL
#   ) +
#   theme(
#     plot.title = element_text(face = "bold", size = 20, color = "black")
#     , axis.title.x = element_text(face = "bold", size = 19, colour = "black")
#     , axis.title.y = element_text(face = "bold", size = 19, colour = "black", angle = 90)
#     , axis.text.x = element_text( size = 19, colour = "black")
#     , axis.text.y = element_text(size = 19, colour = "black")
#     , legend.position = c(0, 1), legend.justification = c(0, 0.96)
#     , legend.key=element_blank()
#     , legend.text=element_text(size=15)
#     , legend.background=element_blank()
#   ) +
#   ggsave(filename = saveImg, width = 6, height = 6, dpi = 1200)
# 
# # ==============================================================================
# # 5. 상대습도 (or 다른성분이) 시정에 미치는 영향
# # ==============================================================================
# dataL6 = dataL1 %>% 
#   dplyr::select(-c("Date.time", "sDateTime", "dtYear", "dtMonth", "dtXran")) %>% 
#   tidyr::gather(-dtDateTime, -rh, key = "key", value = "val") %>% 
#   dplyr::mutate(
#     type = dplyr::case_when(
#       key == "val" ~ "화학"
#       , key == "val2"~ "광학"
#       , key == "ref" ~ "기상청"
#       , TRUE ~ NA_character_
#     )
#   ) %>% 
#   na.omit()
#   
# 
# summary(dataL6)
# 
# subTitle = sprintf("%s", "상대습도에 따른 시정 영향 (광학, 화학, 기상청)")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, subTitle)
# 
# # 정렬
# dataL6$type = forcats::fct_relevel(dataL6$type, c("광학", "화학", "기상청"))
# 
# lmFormula = y ~ poly(x, 2, raw = TRUE)
# 
# ggplot(data = dataL6, aes(x = rh, y = val, color = val)) +
#   geom_point(size = 2, alpha = 0.1) +
#   geom_smooth(method = 'lm', formula = lmFormula, se = TRUE) +
#   ggpubr::stat_regline_equation(aes(label = ..eq.label..), color = "red", formula = lmFormula, parse = TRUE, label.x.npc = 0.0, label.y.npc = 0.75) +
#   ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.70, color = "red") +
#   labs(title = subTitle, x = "상대 습도 [%]", y = "시정 [km]", color = NULL, fill = NULL) +
#   theme(
#     text = element_text(size = 16)
#     # , legend.position = "top"
#   ) +
#   facet_wrap(~type, scale = "free") +
#   ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)
