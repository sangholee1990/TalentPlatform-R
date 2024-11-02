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
# R을 이용한 광학시정 bext 시각화

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0585"

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
library(openxlsx)

# 데이터 검색/읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "예시파일.xlsx"))
data = openxlsx::read.xlsx(fileInfo, sheet = 1, startRow = 1)

dataL1 = data %>% 
  as.tibble() %>% 
  dplyr::rename(
    bext = "광학시정(Bext)"
  ) %>% 
  dplyr::filter(
    ! is.na(RH)
    , ! is.na(bext)
  )

summary(dataL1)

# 데이터 시각화
mainTitle = sprintf("%s", "광학시정")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL1, aes(x = RH, y = bext)) +
  geom_point(aes(size = 3, color = PM2.5), alpha = 0.8) + 
  # scale_color_gradient2(low = "blue", mid = "#50F8F5", high = "red", midpoint = 60, name = "PM2.5", limits = c(0, 120), breaks = seq(0, 120, by = 30), na.value = NA) +
  scale_color_gradient2(low = "blue", mid = "#50F8F5", high = "red", midpoint = 60, name = "PM2.5", limits = c(0, 120), breaks = seq(0, 120, by = 30)) +
  labs(
    x = "RH (%)",
    y = "광학시정 bext (Mm⁻¹)",
    color = "PM2.5",
    size = "size"
  ) +
  theme_classic() +
  theme(
    , text = element_text(size = 16)
    , legend.position = c(0.06, 0.75)
    , legend.title = element_text(face = "bold", size = 16, color="black")
    , legend.text = element_text(size = 12)
    , axis.title = element_text(face = "bold", size=18, color="black")
    , axis.text = element_text(face = "bold", size=18, color="black")
    , axis.line = element_line(size = 1.0)
    , panel.grid.major = element_line(size = 0.2, color = "gray90")
    , panel.grid.minor = element_line(size = 0.2, color = "gray90") 
  ) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
