# ===============================================================================================
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
# R을 이용한 AERONET 옹스트롬 지수 계산

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0445"

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
library(dplyr)
library(tidyverse)
library(lubridate)

# 파일 조회
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "aeronet.txt"))
fileList2 = Sys.glob(file.path(globalVar$inpPath, serviceName, "site.csv"))

# 데이터 읽기
orgData = read.csv(fileList, header = TRUE, sep = ",")
siteData = read.csv(fileList2, header = TRUE, sep = ",")

# 데이터 병합
data = dplyr::left_join(orgData, siteData, by = c("AERONET_Site" = "AERONET_Site")) %>% 
  dplyr::mutate(
    dtDate = readr::parse_date(Date.dd.mm.yyyy., format = "%d:%m:%Y")
    , dtMonth = lubridate::month(dtDate)
    , dtYear = lubridate::year(dtDate)
  )

# (1) 아래 식을 이용하여 Angstrom exponent를 계산하시오.
# AE = -ln(AOD_440nm/AOD_870nm) / ln(440/870)
# AE 값의 범위는 0~3
data$AE = -log(data$AOD_440nm/data$AOD_870nm) / log(440/870)
summary(data$AE)

# (2) 측정소별 월평균 AOD 440 nm와 AE의 boxplot을 각 값의 오름차순 (혹은 내림차순)으로 그리시오.
# >> 연도에 따른 월평균을 구한 후, boxplot을 그릴 것!
dataL1 = data %>% 
  dplyr::group_by(AERONET_Site, dtYear, dtMonth) %>% 
  dplyr::summarise(
    meanVal = mean(AOD_440nm, na.rm = TRUE)
    , meanVal2 = mean(AE, na.rm = TRUE)
  )

dataL1$dtMonth = as.factor(dataL1$dtMonth)

# 월평균 AOD 440 nm
mainTitle = "월평균 AOD 440 nm boxplot"
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL1, aes(x = dtMonth, y = meanVal)) +
  geom_boxplot() +
  labs(x = "월", y = "월평균 AOD 440 nm", fill = NULL, subtitle = mainTitle) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
  ) +
  facet_wrap(~AERONET_Site) + 
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# 월평균 AE
mainTitle = "월평균 AE boxplot"
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL1, aes(x = dtMonth, y = meanVal2)) +
  geom_boxplot() +
  labs(x = "월", y = "월평균 AOD 440 nm", fill = NULL, subtitle = mainTitle) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
  ) +
  facet_wrap(~AERONET_Site) + 
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
