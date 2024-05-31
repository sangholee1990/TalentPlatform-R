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
# R을 이용한 3개 그래프 병합 및 부가 설정 (축 마진 제거 등)

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0559"

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
library(openxlsx)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(fs)
library(scales)

# 파일 조회
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "dom_1st.xlsx"))

# 파일 읽기
data = openxlsx::read.xlsx(fileInfo, sheet = 1, startRow = 1) %>%
  tibble::as.tibble()

# 요약
summary(dom_1st)

# 범위 설정
priRange = range(data$SEN_1st, na.rm = TRUE)
secRange = range(data$SEN_MC_pred, na.rm = TRUE)
triRange = range(data$SEN_CR_pred, na.rm = TRUE)

domData = data %>%
  dplyr::mutate(
    SEN_MC_pred2 = rescale(SEN_MC_pred, to = triRange)
    , SEN_CR_pred2 = rescale(SEN_CR_pred, to = triRange)
    )

mainTitle = sprintf("%s", "mergeVis")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(data = domData) +
  # 녹색 선
  geom_vline(xintercept = 8, color = "green", size = 2) +
  # 좌측
  geom_line(aes(x = DOM, y = SEN_1st), color = "black", size = 1) +

  # 좌측
  geom_line(aes(x = DOM, y = SEN_CR_pred), color = "red", size = 1) +
  geom_ribbon(aes(x = DOM, y = domData$SEN_MC_pred, ymin = -Inf, ymax = SEN_CR_pred), fill = "red", alpha = 0.2) +
  
  # 우측
  geom_line(aes(x = DOM, y = SEN_MC_pred2), color = "blue", size = 1, alpha = 0.5) +
  geom_ribbon(aes(x = DOM, y = SEN_MC_pred2, ymin = domData$SEN_MC_pred2, ymax = max(domData$SEN_MC_pred2, na.rm = TRUE)), fill = "blue", alpha = 0.2) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 24), breaks = seq(0, 24, 6)) +
  scale_y_continuous(expand = c(0, 0), name = "SEN_1st / SEN_CR_pred", sec.axis = sec_axis(trans = ~ rescale(., to = secRange), name = "SEN_MC_pred")) +
  theme(
    text = element_text(size = 14)
  ) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
  
  
# ******************************************************************************
# 참조 코드
# ******************************************************************************
# #1st미분 증가치
# RUM_1st <- ggplot(data = dom_1st, aes(x = DOM, y = RUM_1st)) +
#   geom_vline(xintercept = 8, color = "green", size = 2) +
#   geom_line(aes(y = RUM_1st), color = "black", size = 1)
# 
# 
# SEN_1st <- ggplot(data = dom_1st, aes(x = DOM, y = SEN_1st)) +
#   geom_vline(xintercept = 8, color = "green", size = 2) +
#   geom_line(aes(y = SEN_1st), color = "black", size = 1)
# 
# 
# # 수분
# RUM_MC_pred <- ggplot(data = dom_1st, aes(x = DOM, y = RUM_MC_pred)) +
#   geom_line(color = "blue", size = 1, alpha = 0.5) +
#   geom_ribbon(aes(ymin = RUM_MC_pred, ymax = max(dom_1st$RUM_MC_pred)), fill = "blue", alpha = 0.2) +
#   labs(title = "", y = "") +
#   geom_vline(xintercept = 8, color = "green", size = 2) +
#   theme(axis.text.x = element_blank(),
#         axis.title.y = element_text(hjust = 0),
#         axis.text.y.right = element_text(hjust = 0),
#         axis.title.y.right = element_text(hjust = 0),
#         axis.line.y.right = element_line(color = "black"),
#         axis.ticks.y.right = element_line(color = "blue"),
#         axis.ticks.length.y.right = unit(0.2, "cm"),
#         panel.border = element_blank(),
#         plot.margin = unit(c(1, 1, 1, 3), "lines")) +
#   scale_y_continuous(position = "right", expand = c(0, 0))
# 
# SEN_MC_pred <- ggplot(data = dom_1st, aes(x = DOM, y = SEN_MC_pred)) +
#   geom_line(color = "blue", size = 1, alpha = 0.5) +
#   geom_ribbon(aes(ymin = SEN_MC_pred, ymax = max(dom_1st$SEN_MC_pred)), fill = "blue", alpha = 0.2) +
#   labs(title = "", y = "") +
#   geom_vline(xintercept = 8, color = "green", size = 2) +
#   theme(axis.text.x = element_blank(),
#         axis.title.y = element_text(hjust = 0),
#         axis.text.y.right = element_text(hjust = 0),
#         axis.title.y.right = element_text(hjust = 0),
#         axis.line.y.right = element_line(color = "black"),
#         axis.ticks.y.right = element_line(color = "blue"),
#         axis.ticks.length.y.right = unit(0.2, "cm"),
#         panel.border = element_blank(),
#         plot.margin = unit(c(1, 1, 1, 3), "lines")) +
#   scale_y_continuous(position = "right", expand = c(0, 0))
# 
# 
# # 꺠진쌀
# 
# RUM_CR_pred <- ggplot(data = dom_1st, aes(x = DOM, y = RUM_CR_pred)) +
#   geom_line(color = "red", size = 1) +
#   geom_ribbon(aes(ymin = -Inf, ymax = RUM_CR_pred), fill = "red", alpha = 0.2) + # 아래 영역을 채우는 코드
#   labs(title = "", y = "") +
#   geom_vline(xintercept = 8, color = "green", size = 2) +
#   theme(axis.text.x = element_blank(),
#         axis.title.y = element_text(hjust = 0),
#         axis.text.y.right = element_text(hjust = 0),
#         axis.title.y.right = element_text(hjust = 0),
#         axis.line.y.right = element_line(color = "black"),
#         axis.ticks.y.right = element_line(color = "red"),
#         axis.ticks.length.y.right = unit(0.2, "cm"),
#         panel.border = element_blank(),
#         plot.margin = unit(c(1, 1, 1, 3), "lines")) +
#   scale_y_continuous(position = "right", expand = c(0, 0))
# 
# 
# SEN_CR_pred <- ggplot(data = dom_1st, aes(x = DOM, y = SEN_CR_pred)) +
#   geom_line(color = "red", size = 1) +
#   geom_ribbon(aes(ymin = -Inf, ymax = SEN_CR_pred), fill = "red", alpha = 0.2) + # 아래 영역을 채우는 코드
#   labs(title = "", y = "") +
#   geom_vline(xintercept = 8, color = "green", size = 2) +
#   theme(axis.text.x = element_blank(),
#         axis.title.y = element_text(hjust = 0),
#         axis.text.y.right = element_text(hjust = 0),
#         axis.title.y.right = element_text(hjust = 0),
#         axis.line.y.right = element_line(color = "black"),
#         axis.ticks.y.right = element_line(color = "red"),
#         axis.ticks.length.y.right = unit(0.2, "cm"),
#         panel.border = element_blank(),
#         plot.margin = unit(c(1, 1, 1, 3), "lines")) +
#   scale_y_continuous(position = "right", expand = c(0, 0))
# 
# 
# # 그래프 합치기
# combined_plot <- SEN_1st / SEN_MC_pred / SEN_CR_pred
# 
# # 그래프 출력
# print(combined_plot)