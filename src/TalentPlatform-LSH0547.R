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
# Python을 이용한 시간별 재분석 ERA5 모델 (Grib)로부터 통계 분석 그리고 MK 검정 (Mann-Kendall)

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0547"

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
library(openxlsx)
library(tidyverse)
library(openxlsx)
library(forcats)
library(ggpubr)

# 파일 검색
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "POS/REANALY-ECMWF-1M-GW-t2m-pos.xlsx"))

# 파일 읽기
data = openxlsx::read.xlsx(fileInfo, sheet = 1, startRow = 1)

dataL1 = data %>% 
  dplyr::select("sDate", "ecmwf-org", "ecmwf-mov", "kma-org") %>% 
  readr::type_convert() %>% 
  tibble::as.tibble() %>% 
  dplyr::mutate(
    dtDate = readr::parse_datetime(as.character(sDate), format = "%Y-%m-%d")
  ) %>% 
  dplyr::select(-sDate)

dataL2 = dataL1 %>% 
  tidyr::gather(-dtDate, key = "key", value = "val")

# summary(dataL2)

dataL2$key = forcats::fct_relevel(dataL2$key, c("ecmwf-org", "ecmwf-mov", "kma-org"))

plotSubTitle = sprintf("%s", "기온 t2m 시계열 그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

makePlot = ggpubr::ggline(
    data = dataL2, x = "dtDate", y = "val", color = "key"
    , add = "reg.line", alpha = 0.1
  ) +
  geom_smooth(method = "lm", se = TRUE, aes(x = dtDate, y = val, color = key, fill = key), alpha = 0.2, show.legend = FALSE) +
  labs(title = NULL, x = "Date [Month]", y = "Temperature [℃]", color = NULL, subtitle = plotSubTitle) +
  theme_bw() +
  ggpubr::stat_regline_equation(aes(color = key, label =  paste0(..eq.label.., "~~(", ..rr.label.., ")")), label.x.npc = 0.0, label.y.npc = 0.98, size = 6, show.legend = FALSE) +
  scale_y_continuous(minor_breaks = seq(-5, 40, 5), breaks=seq(-5, 40, 5), limits=c(-5, 40)) +
  theme(
    text = element_text(size = 18)
    , legend.position = "top"
  )

ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# shell.exec(saveImg)


# 파일 검색
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "POS/REANALY-ECMWF-1M-GW-IDX-hd-pos-북구-광주지방기상청.xlsx"))

# 파일 읽기
data = openxlsx::read.xlsx(fileInfo, sheet = 1, startRow = 1)

dataL1 = data %>% 
  dplyr::select("sDate", "ecmwf-org", "ecmwf-mov", "kma-org") %>% 
  readr::type_convert() %>% 
  tibble::as.tibble() %>% 
  dplyr::mutate(
    dtDate = readr::parse_datetime(as.character(sDate), format = "%Y-%m-%d")
  ) %>% 
  dplyr::select(-sDate)

dataL2 = dataL1 %>% 
  tidyr::gather(-dtDate, key = "key", value = "val")

# summary(dataL2)

dataL2$key = forcats::fct_relevel(dataL2$key, c("ecmwf-org", "ecmwf-mov", "kma-org"))

plotSubTitle = sprintf("%s", "폭염일수 hd 시계열 그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

makePlot = ggpubr::ggline(
  data = dataL2, x = "dtDate", y = "val", color = "key"
  , add = "reg.line", alpha = 0.1
) +
  geom_smooth(method = "lm", se = TRUE, aes(x = dtDate, y = val, color = key, fill = key), alpha = 0.2, show.legend = FALSE) +
  labs(title = NULL, x = "Date [Year]", y = "Heat Index", color = NULL, subtitle = plotSubTitle) +
  theme_bw() +
  ggpubr::stat_regline_equation(aes(color = key, label =  paste(..eq.label.., "~~(", ..rr.label.., ")")), label.x.npc = 0.0, label.y.npc = 0.98, size = 6, show.legend = FALSE) +
  # ggpubr::stat_cor(aes(color = key), label.x.npc = 0.5, label.y.npc = 0.98, p.accuracy = 0.01, r.accuracy = 0.01, size = 6, show.legend = FALSE) +
  scale_y_continuous(minor_breaks = seq(0, 50, 5), breaks=seq(0, 50, 5), limits=c(0, 50)) +
  theme(
    text = element_text(size = 18)
    , legend.position = "top"
  )

ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# shell.exec(saveImg)
