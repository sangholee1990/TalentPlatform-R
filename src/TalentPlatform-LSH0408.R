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
# R을 이용한 학술논문 (지리적 프로파일링 기반으로 Arthur Conan Doyle의 소설 분석) 자료 처리 및 시각화
# suspect가 가맹정이 있는 좌표이고, realpoint가 유동인구가 방문한 좌표입니다.^^
# 데이터는 건대입구역 상권 데이터인데... 가맹점이 있는 위치좌표와 실제 유동인구가 방문한 방문좌표입니다.

# https://github.com/bobverity/Rgeoprofile
# https://www.tandfonline.com/doi/full/10.1080/00330124.2020.1758575

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0408"

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
  # source(here::here(file.path(contextPath, "src"), "InitConfig.R"), encoding = "UTF-8")
  source(file.path(contextPath, "src", "InitConfig.R"))
}

#================================================
# 비즈니스 로직 수행
#================================================
# 라이브러리 읽기
library(ggmap)
library(RgeoProfile)
library(gridExtra)
library(cowplot)
library(viridis)
library(tidyverse)
library(RANN)
library(gridExtra)
library(multcomp)
library(geosphere)
# ggmap::register_google(key = "")

# ------------------------------------------------------------------
# ANALYSIS FOR "INVESTIGATING SHERLOCK HOLMES: USING GEOGRAPHIC
# PROFILING TO ANALYSE THE NOVELS OF ARTHUR CONAN DOYLE"
# Stevens et al. 2020
# ------------------------------------------------------------------

# ------------------------------------------------------------------
# INSTALLATION AND PACKAGE LOADING
# ------------------------------------------------------------------
# install.packages("devtools")
# install.packages("gridExtra")
# install.packages("cowplot")
# devtools::install_github("dkahle/ggmap")
# devtools::install_github("bobverity/Rgeoprofile", ref = "version2.1")
# load packages
# library(ggmap)
# library(Rgeoprofile)
# library(gridExtra)
# library(cowplot)
# library(tidyverse)
# ggmap::register_google(key = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")


# ------------------------------------------------------------------
# IMPORT DATA AND SET PARAMETERS
# ------------------------------------------------------------------
# 건대입구역 상권 데이터

# # 유동인구가 방문한 좌표
# criFileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "realpoint.csv"))
# crime_data = readr::read_csv(criFileInfo)
#
# # 가맹정이 있는 좌표
# srcFileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "suspect.csv"))
# source_data = readr::read_csv(srcFileInfo)

# 유동인구가 방문한 좌표
criFileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "suspect.csv"))
crime_data = readr::read_csv(criFileInfo)

# 가맹정이 있는 좌표
srcFileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "realpoint.csv"))
source_data = readr::read_csv(srcFileInfo)

summary(crime_data)
summary(source_data)


# should be in three columns, for decimal longitude and decomal latitude, and one indicating whether site relates to ACD or other
# crime_data <- read.table("XXXXXXXXXXXXXXXXX", header = FALSE)
# source_data <- read.table("XXXXXXXXXXXXXXXXX", header = FALSE)
head(source_data)
tail(source_data)

# area limits (to limit sources to area studied)
# rangeLon <- c(-0.38, 0.08)
# rangeLat <- c(51.36, 51.61)
rangeLon <- c(127, 128)
rangeLat <- c(37, 38)

# convert data to list format for geoMCMC().
# d <- RgeoProfile::geoData(crime_data[, 1], crime_data[, 2])
# d <- RgeoProfile::geoData(crime_data[, 2], crime_data[, 3])
d <- RgeoProfile::geoData(crime_data$X, crime_data$Y)

# set model and MCMC parameters
p = RgeoProfile::geoParams(data = d, sigma_mean = 1, sigma_squared_shape = 2, sigma_var = NULL, chains = 20, burnin = 1e3, samples = 1e5, longitude_cells = 500, latitude_cells = 500)

# remove suspects outside of the sudy area and coverts sources to correct format
# suspectRowsToInclude <- intersect(
#   which(source_data[, 1] > rangeLon[1] & source_data[, 1] < rangeLon[2]),
#   which(source_data[, 2] > rangeLat[1] & source_data[, 2] < rangeLat[2])
# )

suspectRowsToInclude <- intersect(
  which(source_data$X > rangeLon[1] & source_data$X < rangeLon[2]),
  which(source_data$Y > rangeLat[1] & source_data$Y < rangeLat[2])
)


source_data <- source_data[suspectRowsToInclude,]
# sourceNames <- source_data[, 3]
sourceNames <- source_data$ID
# s <- geoDataSource(source_data[, 1], source_data[, 2])
s <- RgeoProfile::geoDataSource(source_data$X, source_data$Y)
length(sourceNames)

# ------------------------------------------------------------------
# RUN MCMC
# ------------------------------------------------------------------
# 유동 인구
m = RgeoProfile::geoMCMC(data = d, params = p)

# ------------------------------------------------------------------
# Figure 1a Full geoprofile
# NB hard coded to plot only first three source names (eg just ACD)
# ------------------------------------------------------------------

map1 <- RgeoProfile::geoPlotMap(params = p,
                                data = NULL,
                                source = NULL,
                                surface = m$geoProfile,
                                mapSource = "google",
                                mapType = "terrain",
                                breakPercent = c(0, 1, 10, 50, 100), #seq(0,100,l=10),
                                opacity = 0.5,
                                plotContours = TRUE,
                                smoothScale = TRUE,
                                sourceCex = 0.5,
                                crimeCex = 0.5,
                                contourCol = "gray40",
                                sourceCol = "blue",
                                crimeCol = "black")

# add crimes
# 유동인구
map2 <- map1 + geom_point(aes(x = d$longitude, y = d$latitude), col = "black", size = 1, pch = 20)
# add sources

# 가맹점
# map3 <- map2 + geom_point(aes(x = s$longitude[1:3], y = s$latitude[1:3]), col = "red", pch = 15, size = 1.5)
map3 <- map2 + geom_point(aes(x = s$longitude, y = s$latitude), col = "red", pch = 15, size = 1.5)

# add legend and change size
map4 <- map3 +
  scale_fill_gradientn(name = "Hit score\npercentage",
                       colours = rev(viridisLite::plasma(10, alpha = 0.5)),
                       breaks = c(5, 50, 99),
                       labels = c("5", "50", "100")) +
  theme(legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6)) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 7.5)) +
  ggtitle("A")

# fig 1a
fig1a <- map4

mainTitle = sprintf("%s", "fig1a")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
ggsave(fig1a, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# ************************************************************************
# 최근접 화소 찾기
# ************************************************************************
plotData = fig1a$layers[[3]]$data %>%
  as.tibble()

# 유동 인구 : crime_data
nearData = RANN::nn2(
  data = plotData %>% dplyr::select(x, y)
  , query = crime_data %>% dplyr::select(X, Y) %>% dplyr::rename(x = X, y = Y)
  , k = 1
)

plotDataL1 = dplyr::bind_cols(crime_data, plotData[nearData$nn.idx, ])

# plotDataL1$cut %>%  unique() %>% sort()
plotDataL2 = plotDataL1 %>%
  dplyr::select(ID, X, Y, z, cut, col) %>%
  dplyr::rename(hitScore = z) %>%
  dplyr::mutate(
    type = dplyr::case_when(
      0 <= hitScore & hitScore < 10 ~ "low"
      , 10 <= hitScore & hitScore < 50 ~ "medium"
      , 50 <= hitScore & hitScore <= 100 ~ "high"
    )
  ) %>%
  dplyr::arrange(desc(hitScore))

saveFile = sprintf("%s/%s/%s 프로파일 등고선.csv", globalVar$outPath, serviceName, "fig1a_유동인구")
dir.create(path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(x = plotDataL2, file = saveFile)
cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")

plotDataL3 = plotDataL2 %>%
  dplyr::group_by(cut) %>%
  dplyr::summarise(
    meanVal = mean(hitScore, na.rm = TRUE)
    , sdVal = sd(hitScore, na.rm = TRUE)
  )

mainTitle = sprintf("%s 프로파일 등고선", "fig1a_유동인구")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

makePlot = ggplot(data = plotDataL3, aes(x = cut, y = meanVal, color = cut, group = cut)) +
   geom_point(size = 2.5, pch = c(22, 22, 15, 15), stroke = 1) +
  geom_errorbar(width = 0.3, aes(ymin=meanVal - sdVal, ymax=meanVal + sdVal, group = cut), position = position_dodge(0.5), show.legend = FALSE) +
  labs(x = "priority", y = "hit score percentage", fill = NULL, subtitle = mainTitle) +
  scale_color_manual(values = c("black", "gray", "blue", "red")) +
  coord_flip() +
  theme(
    text = element_text(size = 18)
    # , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "none"
  )

ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")



# # 가맹점 : source_data
# nearData = RANN::nn2(
#   data = plotData %>% dplyr::select(x, y)
#   , query = source_data %>% dplyr::select(X, Y) %>% dplyr::rename(x = X, y = Y)
#   , k = 1
# )
#
# plotDataL1 = dplyr::bind_cols(source_data, plotData[nearData$nn.idx, ])
#
# plotDataL2 = plotDataL1 %>%
#   dplyr::select(ID, X, Y, z, cut, col) %>%
#   dplyr::rename(hitScore = z) %>%
#   dplyr::arrange(desc(hitScore))
#
# saveFile = sprintf("%s/%s/%s 프로파일 등고선.csv", globalVar$outPath, serviceName, "fig1a_가맹점")
# dir.create(path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
# readr::write_csv(x = plotDataL2, file = saveFile)
# cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")
#
# plotDataL3 = plotDataL2 %>%
#   dplyr::group_by(cut) %>%
#   dplyr::summarise(
#     meanVal = mean(hitScore, na.rm = TRUE)
#     , sdVal = sd(hitScore, na.rm = TRUE)
#   )
#
# mainTitle = sprintf("%s 프로파일 등고선", "fig1a_가맹점")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
# makePlot = ggplot(data = plotDataL3, aes(x = cut, y = meanVal, color = cut, group = cut)) +
#    geom_point(size = 2.5, pch = c(22, 22, 15, 15), stroke = 1) +
#   geom_errorbar(width = 0.3, aes(ymin=meanVal - sdVal, ymax=meanVal + sdVal, group = cut), position = position_dodge(0.5), show.legend = FALSE) +
#   labs(x = "priority", y = "hit score percentage", fill = NULL, subtitle = mainTitle) +
#   scale_color_manual(values = c("black", "gray", "blue", "red")) +
#   coord_flip() +
#   theme(
#     text = element_text(size = 18)
#     # , axis.text.x = element_text(angle = 45, hjust = 1)
#     , legend.position = "none"
#   )
#
# ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# ------------------------------------------------------------------
# HITSCORE PERCENTAGES
# NB hard coded to use first three source names (eg just ACD)
# ------------------------------------------------------------------
# blue plaques
# ------------------------------------------------------------------

# get hitscores for blue plaques
# hs <- RgeoProfile::geoReportHitscores(params = p, source = s, surface = m$geoProfile)
hs <- cbind(RgeoProfile::geoReportHitscores(params = p, source = s, surface = m$geoProfile), sourceNames)
head(hs)

# where do ACD sites rank in list?
ordered <- hs[order(hs[, 3]),]
# which(ordered[, 4] == "ACD")
# which(ordered[, 4] == "ACD") / nrow(ordered)

# what is lowest hitscore?
lowestHS <- which(hs[, 3] == min(hs[, 3]))
hs[lowestHS,]

# ------------------------------------------------------------------
# 221b baker street
# ------------------------------------------------------------------
# baker_street221 <- c(-0.158557, 51.523753)

baker_street221 <- c(mean(source_data$X, na.rm = TRUE), mean(source_data$Y, na.rm = TRUE))
baker_street221 <- RgeoProfile::geoDataSource(baker_street221[1], baker_street221[2])
sherlock_hs <- RgeoProfile::geoReportHitscores(params = p, source = baker_street221, surface = m$geoProfile)
sherlock_hs
# where would sherlock rank in this list?
length(which(sort(hs[, 3]) < sherlock_hs[, 3]))

# ------------------------------------------------------------------
# authors only
# ------------------------------------------------------------------
# should be in four columns, for decimal longitude and decomal latitude, one indicating name of author, and a short author code
# author_source <- read.table("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", header = FALSE)
# author_source <- read.table(srcFileInfo, header = FALSE)
# author_source = readr::read_csv(srcFileInfo)
author_source = readr::read_csv(criFileInfo)

# remove suspects outside of the sudy area and coverts sources to correct format
# suspectRowsToInclude <- intersect(
#   which(author_source[, 1] > rangeLon[1] & author_source[, 1] < rangeLon[2]),
#   which(author_source[, 2] > rangeLat[1] & author_source[, 2] < rangeLat[2])
# )

suspectRowsToInclude <- intersect(
  which(author_source$X > rangeLon[1] & author_source$X < rangeLon[2]),
  which(author_source$Y > rangeLat[1] & author_source$Y < rangeLat[2])
)
author_source <- author_source[suspectRowsToInclude,]

# author_names <- author_source[, 3]
# author_code <- author_source[, 4]

author_names <- author_source$ID
author_code <- author_source$ID
# author_source <- geoDataSource(author_source[, 1], author_source[, 2])
author_sourceL1 <- geoDataSource(author_source$X, author_source$Y)

# calculate hitscores
authorHS <- cbind(RgeoProfile::geoReportHitscores(params = p, source = author_sourceL1, surface = m$geoProfile), author_names, author_code)
authorHS <- authorHS[order(authorHS[, 3]),]
authorHS


# ------------------------------------------------------------------
# acd sites only
# ------------------------------------------------------------------
# should be in four columns, for decimal longitude and decomal latitude, apriority and description
# acd <- read.table("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", header = FALSE)
# acd = readr::read_csv(srcFileInfo)
# acd = readr::read_csv(srcFileInfo)
# acd = readr::read_csv(criFileInfo)


saveFile = sprintf("%s/%s/%s 프로파일 등고선.csv", globalVar$outPath, serviceName, "fig1a_유동인구")
acd = readr::read_csv(saveFile)
cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")

acd_df <- acd
# acdCode <- acd[, 3]
acdCode <- acd$ID
acdDesc <- acd[,]
# acd <- geoDataSource(acd[, 1], acd[, 2])
acd <- geoDataSource(acd$X, acd$Y)
# acd <- geoDataSource(acd$longitude, acd$latitude)
# acdHS <- data.frame(geoReportHitscores(params = p, source = acd, surface = m$geoProfile), acdCode, acdDesc[, 4])
acdHS <- data.frame(geoReportHitscores(params = p, source = acd, surface = m$geoProfile), acdCode, acdDesc$type)
acdHS

# ------------------------------------------------------------------
# Figure 1b Zoom central London
# ------------------------------------------------------------------

# set master zoom values if required
# zoomLon = c(-0.17, -0.12)
# zoomLat = c(51.50, 51.53)
# zoomLon <- c(126, 128)
# zoomLat <- c(37, 38)
zoomLon <- c(min(acdHS$lon, na.rm = TRUE), max(acdHS$lon, na.rm = TRUE))
zoomLat <- c(min(acdHS$lat, na.rm = TRUE), max(acdHS$lat, na.rm = TRUE))

# rerun model, restricting to limits above
zoom_p <- p
zoom_p$output$longitude_minMax <- zoomLon
zoom_p$output$latitude_minMax <- zoomLat
m2 = geoMCMC(data = d, params = zoom_p)

map5 <- geoPlotMap(params = zoom_p,
                   data = NULL,
                   source = NULL,
                   surface = m2$geoProfile,
                   mapSource = "google",
                   mapType = "terrain",
                   breakPercent = c(0, 1, 10, 50, 100), #seq(0, 100,l = 10),
                   opacity = 0.5,
                   plotContours = TRUE,
                   smoothScale = TRUE,
                   sourceCex = 0.5,
                   crimeCex = 0.5,
                   contourCol = "gray40",
                   sourceCol = "blue",
                   crimeCol = "black",
                   gpLegend = TRUE)

# map6 <- map5 + geom_point(aes(x = d$longitude, y = d$latitude), col = "black", size = 0.5, pch = 20)
map6 <- map5 + geom_point(aes(x = d$longitude, y = d$latitude), col = "black", size = 2, pch = 20)

# add sources
# map7 <- map6 + geom_point(aes(x = s$longitude[1:3], y = s$latitude[1:3]), col = "red", size = 1.5, pch = 15)
map7 <- map6 + geom_point(aes(x = s$longitude, y = s$latitude), col = "red", size = 1.5, pch = 15)


# add legend and change sizes
map8 <- map7 +
  scale_fill_gradientn(name = "Hit score\npercentage",
                       colours = rev(plasma(10)),
                       breaks = c(5, 50, 99),
                       labels = c("5", "50", "100")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6)) +
  ggtitle("B")


# fig 1b
fig1b <- map8

mainTitle = sprintf("%s", "fig1b")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
ggsave(fig1b, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# ------------------------------------------------------------------
# Figure 1c Zoom Norwood
# ------------------------------------------------------------------
# set master zoom values if required
# zoomLon = c(-0.1, -0.05)
# zoomLat = c(51.37, 51.40)
zoomLon <- c(min(acdHS$lon, na.rm = TRUE), max(acdHS$lon, na.rm = TRUE))
zoomLat <- c(min(acdHS$lat, na.rm = TRUE), max(acdHS$lat, na.rm = TRUE))


# rerun model, restricting to limits above
zoom_p2 <- p
zoom_p2$output$longitude_minMax <- zoomLon
zoom_p2$output$latitude_minMax <- zoomLat
m3 = geoMCMC(data = d, params = zoom_p2)

map9 <- geoPlotMap(params = zoom_p2,
                   data = NULL,
                   source = NULL,
                   surface = m3$geoProfile,
                   mapSource = "google",
                   mapType = "terrain",
                   breakPercent = c(0, 1, 5, 50, 100), #seq(0, 100, l = 10),
                   opacity = 0.5,
                   plotContours = TRUE,
                   smoothScale = TRUE,
                   sourceCex = 0.5,
                   crimeCex = 0.5,
                   contourCol = "gray40",
                   sourceCol = "blue",
                   crimeCol = "black",
                   gpLegend = TRUE)

# map10 <- map9 + geom_point(aes(x = d$longitude, y = d$latitude), col = "black", size = 0.5, pch = 20)
map10 <- map9 + geom_point(aes(x = d$longitude, y = d$latitude), col = "black", size = 2, pch = 20)
# add sources
# map11 <- map10 + geom_point(aes(x = s$longitude[1:3], y = s$latitude[1:3]), col = "red", size = 0.5 + 1, pch = 15)
# map11 <- map10 + geom_point(aes(x = s$longitude[1:3], y = s$latitude[1:3]), col = "red", size = 5, pch = 15)
map11 <- map10 + geom_point(aes(x = s$longitude, y = s$latitude), col = "red", size = 1.5, pch = 15)

# add legend and change size
map12 <- map11 +
  scale_fill_gradientn(name = "Hit score\npercentage",
                       colours = rev(plasma(10)),
                       breaks = c(5, 50, 99),
                       labels = c("5", "50", "100")) +
  theme(legend.position = "none",
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6)) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 8)) +
  ggtitle("C")

# fig 1c
fig1c <- map12

mainTitle = sprintf("%s", "fig1c")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
ggsave(fig1c, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# combine and save figure 1
margin = theme(plot.margin = unit(c(0.1, 0.5, 0.1, 0.1), "cm"))

figure1 <- grid.arrange(grobs = lapply(list(fig1a, fig1b, fig1c), "+", margin), layout_matrix = matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))

mainTitle = sprintf("%s", "figure1")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
ggsave(figure1, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# ------------------------------------------------------------------
# Figure 2 authors
# ------------------------------------------------------------------
# fig 2 authors only
# map13 <- map2 + geom_point(aes(x = author_source$longitude, y = author_source$latitude), col = "red", pch = 15, size = 0.5)
map13 <- map2 + geom_point(aes(x = author_source$X, y = author_source$Y), col = "red", pch = 15, size = 2)

# add labels
text_nudge <- rep(0.005, 30)
text_nudge[c(7, 14, 17, 19, 25:27, 29)] <- -0.005

# map14 <- map13 + geom_text(aes(x = author_source$longitude, y = author_source$latitude, label = author_code),
map14 <- map13 + geom_text(aes(x = author_source$X, y = author_source$Y, label = author_code),
                           nudge_y = text_nudge,
                           col = "red",
                           check_overlap = FALSE,
                           size = 3)

# add legend and change size
map15 <- map14 +
  scale_fill_gradientn(name = "Hit score\npercentage",
                       colours = rev(plasma(10, alpha = 0.5)),
                       breaks = c(5, 50, 99),
                       labels = c("5", "50", "100")) +
  theme(legend.position = c(0.93, 0.8),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6)) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 6))

# fig 2
fig2 <- map15

mainTitle = sprintf("%s", "fig2")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
ggsave(fig2, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ------------------------------------------------------------------
# Figure 3a acd sites
# ------------------------------------------------------------------
# fig 3a
# highRows <- which(acd_df[, 3] == "high")
# medRows <- which(acd_df[, 3] == "medium")
# lowRows <- which(acd_df[, 3] == "low")
highRows <- which(acd_df$type == "high")
medRows <- which(acd_df$type == "medium")
lowRows <- which(acd_df$type == "low")

# highS <- geoDataSource(acd_df[highRows, 1], acd_df[highRows, 2])
# medS <- geoDataSource(acd_df[medRows, 1], acd_df[medRows, 2])
# lowS <- geoDataSource(acd_df[lowRows, 1], acd_df[lowRows, 2])
highS <- RgeoProfile::geoDataSource(acd_df[highRows, ]$X, acd_df[highRows, ]$Y)
medS <- RgeoProfile::geoDataSource(acd_df[medRows, ]$X, acd_df[medRows, ]$Y)
lowS <- RgeoProfile::geoDataSource(acd_df[lowRows, ]$X, acd_df[lowRows, ]$Y)

# open squares for low priority
plusLow <- map1 + geom_point(aes(x = lowS$longitude, y = lowS$latitude), col = "black", size = 1.5, pch = 22)

# add gray squares plus additional black borders for medium priority
plusMed <- plusLow +
  geom_point(aes(x = medS$longitude, y = medS$latitude), col = "darkgray", size = 1.5, pch = 15) +
  geom_point(aes(x = medS$longitude, y = medS$latitude), col = "black", size = 1.5, pch = 22)
# add red squares for high priority again with larger duplicates with black outline

plusHigh <- plusMed +
  geom_point(aes(x = highS$longitude, y = highS$latitude), col = "red", size = 1.5, pch = 15) +
  geom_point(aes(x = highS$longitude, y = highS$latitude), col = "black", size = 1.5, pch = 22)

# add legend and change size
plusHigh <- plusHigh +
  scale_fill_gradientn(name = "Hit score\npercentage",
                       colours = rev(plasma(10, alpha = 0.5)),
                       breaks = c(5, 50, 99),
                       labels = c("5", "50", "100")) +
  theme(legend.position = c(0.91, 0.79),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6)) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 4)) +
  ggtitle("A")

# fig 5
fig3a <- plusHigh

mainTitle = sprintf("%s", "fig3a")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
ggsave(fig3a, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ------------------------------------------------------------------
# Statistical Analysis
# ------------------------------------------------------------------

# linear model (full data)
response <- acdHS[, 3]
# treatment <- acdHS[, 4]
treatment = as.factor(acdHS[, 5])

# relevel treatment so order i low/med/high, here "treatment" simply refers
# to what kind of category we place on the location associated with ACD be that
# low, medium or high

treatment <- relevel(treatment, "medium")
treatment <- relevel(treatment, "low")

mod1 <- lm(response ~ treatment)
summary(mod1)

# pairwise comparisons
# install.packages("multcomp")
library(multcomp)
g1 <- glht(mod1, linfct = mcp(treatment = "Tukey"))
summary(g1)

# ------------------------------------------------------------------
# Figure 3b error plot of hs for acd sites
# ------------------------------------------------------------------
means <- c(mean(response[treatment == "low"]), mean(response[treatment == "medium"]), mean(response[treatment == "high"]))
sds <- c(sd(response[treatment == "low"]), sd(response[treatment == "medium"]), sd(response[treatment == "high"]))
lowers <- means - sds
#lowers[3] <- 0
errorBarDF <- data.frame(Priority = factor(c("low", "med", "high"), c("low", "med", "high")), means = means, lower = lowers, upper = means + sds)
# error bars
fig3b <- ggplot(data = errorBarDF,
                mapping = aes(x = Priority,
                              ymin = lower,
                              y = means,
                              ymax = upper,
                              col = Priority,
                              shape = Priority)) +
  geom_errorbar() +
  geom_point(size = 2.5, pch = c(22, 15, 15), stroke = 1) +
  xlab("priority") +
  ylab("hit score percentage") +
  theme(legend.position = "none") +
  scale_color_manual(values = c("black", "darkgray", "red")) +
  coord_flip() +
  ggtitle("B") +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 6))


mainTitle = sprintf("%s", "fig3b")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
ggsave(fig3b, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# fig 3b

# create and save fig 3
figure3 <- grid.arrange(fig3a, fig3b, ncol = 1)

mainTitle = sprintf("%s", "figure3")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
ggsave(figure3, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ------------------------------------------------------------------
# distances between sites
# ------------------------------------------------------------------
# all pairwise distances
# library(geosphere)
# lowP <- acdHS[which(acdHS[, 4] == "low"), 2:1]
# mediumP <- acdHS[which(acdHS[, 4] == "medium"), 2:1]
# highP <- acdHS[which(acdHS[, 4] == "high"), 2:1]
lowP <- acdHS[which(acdHS[, 5] == "low"), 2:1]
mediumP <- acdHS[which(acdHS[, 5] == "medium"), 2:1]
highP <- acdHS[which(acdHS[, 5] == "high"), 2:1]

# mean distance from low to nearest high
lowPminima <- rep(NA, nrow(lowP))
for (i in 1:length(lowPminima))
{ lowPminima[i] <- min(distGeo(lowP[i,], highP)) }
lowPminima
mean(lowPminima)
sd(lowPminima)

# mean distance from medium to nearest high
mediumPminima <- rep(NA, nrow(mediumP))
for (i in 1:length(mediumPminima))
{ mediumPminima[i] <- min(distGeo(mediumP[i,], highP)) }
mediumPminima
mean(mediumPminima)
sd(mediumPminima)

# mean distance from low/medium to nearest high
mean(c(lowPminima, mediumPminima))
sd(c(lowPminima, mediumPminima))

# how many low and medmium sites fall within six sigma of a high priority site?
length(which(c(lowPminima, mediumPminima) < (6 * 1000 * mean(m$sigma))))
# total low and medium priority sites
length(c(lowPminima, mediumPminima))

# ------------------------------------------------------------------
# marginal distribution of the number of source locations
# ------------------------------------------------------------------
# n_sources_frequency <- as.data.frame(table(m$unique_groups))
# names(n_sources_frequency) <- c("Sources", "Frequency")
# ggplot(n_sources_frequency, aes(x = Sources, y = Frequency)) + geom_bar(stat = "identity", fill = "#FF6666")
# mean(m$unique_groups)
# sd(m$unique_groups)