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
# R을 이용한 상자그림 및 바이올린 융합 시각화 (기술 통계, 통계 검정)

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0548"

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
library(openxlsx)
library(rlang)
library(magrittr)
library(scales)
library(fs)
library(ggpubr)
library(forcats)

# ================================================
# d1 그래프 레이아웃 
# ================================================
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "d1 (2 conditions).csv"))

data = readr::read_csv(fileInfo)
data$Culture = factor(data$Culture, levels=c(0, 1), labels = c("South Koreans", "Americans"))
data$Actual_behavior = factor(data$Actual_behavior, levels=c(0, 1), labels = c("No", "Yes"))

summary(data)


# violin 
saveImg = sprintf("%s/%s/%s_%s.png", globalVar$figPath, serviceName, "d1", "violin")
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

makePlot = ggpubr::ggsummarystats(data, x = "Culture", y = "Percentage_1", summaries = c("mean", "sd"), ggfunc = ggviolin, alpha = 0.5, error.plot = c("errorbar"), add = c("mean", "mean_sd"), fill = "Culture", facet.by = c("Actual_behavior"), digits = 2)
  
ggsave(print(makePlot), filename = saveImg, width = 10, height = 8, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# boxplot 
saveImg = sprintf("%s/%s/%s_%s.png", globalVar$figPath, serviceName, "d1", "boxplot")
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(data, aes(x = Culture, y = Percentage_1, color = Culture, fill = Culture)) +
  geom_boxplot(color = "black", alpha = 0.1, show.legend = FALSE) +
  geom_jitter(alpha  = 0.5, size = 2, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 2, shape = 16, show.legend = FALSE) +
  stat_anova_test(label.x.npc = 0.3, label.y.npc = 0.90, label = "{method} p = {p.format} {p.signif}") +
  facet_wrap(~Actual_behavior) +
  theme_bw() +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
  
# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# ================================================
# d2 그래프 레이아웃 
# ================================================
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "d2-* (2x2 conditions).csv"))
for (fileInfo in fileList) {
  cat(sprintf("[CHECK] fileInfo : %s", fileInfo), "\n")
  
  fileName = tools::file_path_sans_ext(fs::path_file(fileInfo))
  fileNameKey = stringr::str_split_1(fileName, " \\(")[1]
  
  data = readr::read_csv(fileInfo)
  data$Culture = factor(data$Culture, levels=c(0, 1), labels = c("South Koreans", "Americans"))
  data$Pursuit = factor(data$Pursuit, levels=c(0, 1), labels = c("True-Self", "Competence"))
  
  summary(data)
  
  # violin 
  saveImg = sprintf("%s/%s/%s_%s.png", globalVar$figPath, serviceName, fileNameKey, "violin")
  dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
  
  makePlot = ggpubr::ggsummarystats(data, x = "Culture", y = "DV_Intention", summaries = c("mean", "sd"), ggfunc = ggviolin, alpha = 0.5, error.plot = c("errorbar"), add = c("mean", "mean_sd"), fill = "Culture", facet.by = c("Pursuit"), digits = 2)
  
  ggsave(print(makePlot), filename = saveImg, width = 10, height = 8, dpi = 600)
  
  # shell.exec(saveImg)
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
  
  # boxplot 
  saveImg = sprintf("%s/%s/%s_%s.png", globalVar$figPath, serviceName, fileNameKey, "boxplot")
  dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
  
  makePlot = ggplot(data, aes(x = Culture, y = DV_Intention, color = Culture, fill = Culture)) +
    geom_boxplot(color = "black", alpha = 0.1, show.legend = FALSE) +
    geom_jitter(alpha  = 0.5, size = 2, show.legend = FALSE) +
    stat_summary(fun = mean, geom = "point", color = "black", size = 2, shape = 16, show.legend = FALSE) +
    stat_anova_test(label.x.npc = 0.3, label.y.npc = 0.90, label = "{method} p = {p.format} {p.signif}") +
    facet_wrap(~ Pursuit) +
    theme_bw()
  
  ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
  
  # shell.exec(saveImg)
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
  
}

# ================================================
# d3 그래프 레이아웃 
# ================================================
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "d3 (2x2x2 conditions).csv"))

data = readr::read_csv(fileInfo)
data$Culture = factor(data$Culture, levels=c(0, 1), labels = c("South Koreans", "Americans"))
data$Pursuit = factor(data$Pursuit, levels=c(0, 1), labels = c("True-Self", "Competence"))
data$Status = factor(data$Status, levels=c(0, 1), labels = c("Self-Employed", "Employed"))

# summary(data)

statusList = data$Status %>% unique() %>% sort()
for (status in statusList) {
  cat(sprintf("[CHECK] status : %s", status), "\n")
  
  dataL1 = data %>% 
    dplyr::filter(Status == status)
  
  # violin 
  saveImg = sprintf("%s/%s/%s_%s_%s.png", globalVar$figPath, serviceName, "d3", status, "violin")
  dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
  
  makePlot = ggpubr::ggsummarystats(dataL1, x = "Culture", y = "DV_Intention", summaries = c("mean", "sd"), ggfunc = ggviolin, alpha = 0.5, error.plot = c("errorbar"), add = c("mean", "mean_sd"), fill = "Culture", facet.by = c("Pursuit"), digits = 2)
  
  ggsave(print(makePlot), filename = saveImg, width = 10, height = 8, dpi = 600)
  
  # shell.exec(saveImg)
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
  
  # boxplot 
  saveImg = sprintf("%s/%s/%s_%s_%s.png", globalVar$figPath, serviceName, "d3", status, "boxplot")
  dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
  
  makePlot = ggplot(dataL1, aes(x = Culture, y = DV_Intention, color = Culture, fill = Culture)) +
    geom_boxplot(color = "black", alpha = 0.1, show.legend = FALSE) +
    geom_jitter(alpha  = 0.5, size = 2, show.legend = FALSE) +
    stat_summary(fun = mean, geom = "point", color = "black", size = 2, shape = 16, show.legend = FALSE) +
    stat_anova_test(label.x.npc = 0.3, label.y.npc = 0.90, label = "{method} p = {p.format} {p.signif}") +
    facet_wrap(~ Pursuit) +
    theme_bw()
  
  ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
  
  # shell.exec(saveImg)
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
}

