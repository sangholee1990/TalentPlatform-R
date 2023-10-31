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
# R을 이용한 대한민국 기상청 레이더 자료처리 및 다양한 자료 저장

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0485"

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
library(devtools)
library(sp)
library(remotes)

library(BiocManager)
# BiocManager::install("rhdf5")
# browseVignettes("rhdf5")
library(rhdf5)
# devtools::install_github("adokter/bioRad")
library(bioRad)


# 파일 읽기
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "GDK_230209-10/RDR_GDK_FQC_*.uf"))
fileInfo = fileList[1]


data = bioRad::read_pvolfile(fileInfo)


# my_scan <- get_scan(data, 0.5)
my_scan <- get_scan(data, 2)
my_scan
plot(my_scan, param = "DBZH")
# plot(my_scan, param = "ZDR")

# 레이더 스캔을 PPI 형식으로 변환
my_ppi <- bioRad::project_as_ppi(my_scan)

plot(my_ppi, param = "DBZH")
# plot(my_ppi, param = "DBZH")

ppiData = my_ppi$data

df <- ppiData %>% 
  as.data.frame() %>% 
  as.tibble()



data_lonlat <- sp::spTransform(ppiData, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
df2 = data_lonlat %>% 
  as.data.frame() %>% 
  as.tibble()

library(sf)
sf_object <- sf::st_as_sf(data_lonlat)


basemap = "osm"
map(my_ppi, map = basemap, param = "DBZH", zlim = c(-20, 40))
# map(data_lonlat, map = basemap, param = "DBZH", zlim = c(-20, 40))
# map(sf_object, map = basemap, param = "DBZH", zlim = c(-20, 40))



# Screen out the reflectivity areas with RHOHV < 0.95
my_ppi_clean <- calculate_param(my_ppi, DBZH = ifelse(RHOHV > 0.95, NA, DBZH))
# plot the original and cleaned up reflectivity:
map(my_ppi, map = basemap, param = "DBZH", zlim = c(-20, 40))
map(my_ppi_clean, map = basemap, param = "DBZH", zlim = c(-20, 40))


# 도플라 비율
my_ppi <- calculate_param(my_ppi, DR = 10 * log10((1+ ZDR - 2 * (ZDR^0.5) * RHOHV) /
                                                    (1 + ZDR+ 2 * (ZDR^0.5) * RHOHV)))

map(my_ppi, map = basemap, param = "DR", zlim=c(-25,-5), palette = viridis::viridis(100))




# my_ppi <- project_as_ppi(my_scan)
# plot(my_ppi)



bioRad::beam_profile_overlap(data, elev = 2)
bioRad::calculate_param(data) %>% 
  get_scan(7) %>%
  # project_as_ppi() %>%
  plot()

# 연직 분포
vpObj = calculate_vp(data)
plot(vpObj)
vpData = vpObj$data


my_vpts <- bind_into_vpts(vpObj)
plot(my_vpts)



my_vpi <- integrate_profile(my_vpts)




my_vpi <- integrate_profile(vpObj)
plot(my_vpi, quantity = "mtr")

vpObj %>%
  regularize_vpts() %>%
  plot()


data %>% 
  read_pvolfile() %>%
  get_scan(3) %>%
  project_as_ppi() %>%
  plot(param = "VRADH")



scan <- get_scan(data, elev = 1)
scan <- get_scan(data, elev = 2)
plot(scan)

data %>%
  get_scan(2) %>%
  # project_as_ppi() %>%
  plot()



vp <- integrate_to_vp(data)
# print(vp)

# plot(vp)

data$geo$lat
data$geo$lon
data$geo$height

a = data$scans
a

data$scans



scanList = data$scans

# i = 1
for (i in 1:length(scanList)) {
 
   scanInfo = scanList[[i]]
  
  
  # scanInfo$radar
  # scanInfo$datetime
  # scanInfo$params
  # scanInfo$attributes
  # scanInfo$geo
  
  scanInfo$params
  length(scanInfo$params)
  
  scanInfo$params$RHOHV
  
  scanInfo
  
  
  
}

add_expected_eta_to_scan(scanInfo)

a = data$scans[[2]]$params$DBZH

vp_df <- as.data.frame(a)
vp_df

summary(vp_df)

dataL1 = as.data.frame(data)
as.data.frame(data)

as.data.frame(scanInfo)

# bioRad::add_expected_eta_to_scan(scanInfo)
# a = scanInfo$params$RHOHV
a = scanInfo
summary(a)
plot(a)
ppi <- project_as_ppi(a)
plot(ppi)
plot(ppi, param = "DBZH")



bpo <- beam_profile_overlap(
  data,
  get_elevation_angles(data), seq(0, 100000, 1000)
)

bioRad::beam_profile_overlap(data)

# ppis = lapply(data$scans, project_as_ppi,grid_size=1000)
# composite = composite_ppi(ppis,method="max",res=1000)
# map(composite)

data$radar
data$datetime
data$geo
data$attributes
data$scans
length(data)

a = data$scans[[1]]

name(data)
a$radar
a$datetime
a$params
a$attributes
a$geo


plot(data)


# RadAR::
radar_data <- RadAR(fileInfo)

# data = openxlsx::read.xlsx(fileInfo, sheet = 1, startRow = 1)
# data = readxl::read_excel(fileInfo)


# for (fileInfo in fileList) {
# 
#   cat(sprintf("[CHECK] fileInfo : %s", fileInfo), "\n")
#     
#   orgData = openxlsx::read.xlsx(fileInfo, sheet = 1, startRow = 1)
#   
#   data = orgData %>% 
#     tibble::as.tibble() %>% 
#     dplyr::rename(
#       sDate = "관측일자"
#       , sTime = "관측시간"
#       , alt = "유의파고(m)"
#       , inv = "유의파주기(sec)"
#     ) %>% 
#     readr::type_convert() %>% 
#     dplyr::filter(
#       ! is.na(alt)
#       , ! is.na(inv)
#     ) %>% 
#     dplyr::mutate(across(where(is.character), as.numeric)) %>% 
#     dplyr::mutate(
#       sDateTime = paste(sDate, sTime, sep = " ")
#     ) %>% 
#     dplyr::mutate(
#       dtDateTime = readr::parse_datetime(sDateTime, format = "%Y-%m-%d %H:%M:%S")
#     ) %>% 
#     dplyr::filter(
#       dplyr::between(alt, 3, 16)
#       , inv >= 9
#     )
# 
#   if (nrow(data) < 1) { next }
#   
#   coeff = 0.35
#   
#   fileNameNotExt = tools::file_path_sans_ext(fs::path_file(fileInfo))
#   saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, fileNameNotExt)
#   dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#   
#   makePlot = ggplot(data, aes(x=dtDateTime)) +
#     geom_point(aes(y=alt, color = "alt")) +
#     geom_point(aes(y=inv*coeff, color= "inv")) +
#     scale_y_continuous(
#       limits = c(0, 10)
#       , name = "alt"
#       , sec.axis = sec_axis(~./coeff, name="inv")
#     ) +
#     labs(x = "Date Time", y = NULL, color = NULL, subtitle = fileNameNotExt) +
#     scale_color_manual(values = c("orange2", "gray30")) +
#     scale_x_datetime(date_labels = "%Y-%m", date_breaks = "6 month") +
#     theme(
#           text = element_text(size = 16)
#           , legend.position = "top"
#           , axis.text.x = element_text(angle = 45, hjust = 1)
#         )
#   
#   ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
#   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# }