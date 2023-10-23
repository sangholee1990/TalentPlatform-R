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
# BiocManager::install("biocViews")
# devtools::install_github("cgplab/RadAR")
# devtools::install_github("cgplab/RadAR", build_vignettes = TRUE)
library(RadAR)



# library(bioRad)
# library(rhdf5)

library(remotes)
library(RCurl)
library(rdwd)
rdwd::updateRdwd()

library(dwdradar)


# 파일 읽기
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "GDK_230209-10/RDR_GDK_FQC_*.uf"))
fileInfo = fileList[1]

# 에러 발생
# data = dwdradar::readRadarFile(fileInfo)

library(remotes)
rdwd::updateRdwd()

rdwd::dataDWD()
data = rdwd::readDWD(fileInfo)
rad <- readDWD(fileInfo)


library(BiocManager)
# BiocManager::install("rhdf5")
# browseVignettes("rhdf5")
library(rhdf5)
library(bioRad)

data = bioRad::read_pvolfile(fileInfo)

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