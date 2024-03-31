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
library(patchwork)

# ================================================
# d1 그래프 레이아웃 
# ================================================
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "d1 (2 conditions).csv"))

data = readr::read_csv(fileInfo) %>% 
  dplyr::filter(Actual_behavior == 1)

data$Culture = factor(data$Culture, levels=c(0, 1), labels = c("South Koreans", "Americans"))
data$Actual_behavior = factor(data$Actual_behavior, levels=c(0, 1), labels = c("No", "Yes"))

# 정렬
data$Culture = forcats::fct_relevel(data$Culture, c("Americans", "South Koreans"))
# dataL1$Pursuit = forcats::fct_relevel(dataL1$Pursuit, c("True-Self",  "Competence"))

# summary(data)

statData = data %>% 
  dplyr::group_by(Culture) %>% 
  dplyr::summarize(
    cnt = n()
    , meanVal = mean(Percentage_1, na.rm = TRUE)
    , sdVal = sd(Percentage_1, na.rm = TRUE)
    # , sdVal = sd(Percentage_1, na.rm = TRUE)/sqrt(cnt)
    # , upper = meanVal + (2 * sdVal)
    # , lower = meanVal - (2 * sdVal)
    , upper = meanVal + sdVal
    , lower = meanVal - sdVal
    , freq = cnt / nrow(data) * 100
  )

# violin 
saveImg = sprintf("%s/%s/%s_%s.png", globalVar$figPath, serviceName, "d1", "violin")
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

# makePlot = ggpubr::ggsummarystats(data, x = "Culture", y = "Percentage_1", summaries = c("mean", "sd"), ggfunc = ggviolin, alpha = 0.5, error.plot = c("errorbar"), add = c("mean", "mean_sd"), fill = "Culture", facet.by = c("Actual_behavior"), digits = 2, free.panels=TRUE)

p1 = ggplot(data, aes(x = Culture, y = Percentage_1, color = Culture, fill = Culture)) +
  geom_violin(alpha = 0.5, show.legend = FALSE, trim=FALSE) +
  geom_point(data=statData, aes(x = Culture, y = meanVal), color = "black", size=3, show.legend = FALSE) +
  geom_errorbar(data=statData, aes(x=Culture, y=NULL, ymax=upper, ymin=lower), stat='identity', width=0.1,color = "black", show.legend = FALSE) +
  scale_y_continuous(minor_breaks=seq(0, 100, 20), breaks=seq(0, 100, 20), labels=sprintf("%s%%", seq(0, 100, 20)), expand=c(0,0), limits=c(0, 100)) +
  # scale_y_continuous(minor_breaks=seq(0, 100, 20), breaks=seq(0, 100, 20), labels=sprintf("%s%%", seq(0, 100, 20)), expand=c(0,0)) +
  labs(y = "Actual behavior") +
  facet_wrap(~Actual_behavior) +
  theme_bw()

p2 = statData %>% 
  dplyr::rename(
    Frequency = freq
  ) %>% 
  ggpubr::ggsummarytable(
    x = "Culture", y = c("Frequency"), digits = 1, size = 3, ggtheme = theme_bw()
  ) + 
  labs(x = NULL) + 
  theme(
    axis.line.x = element_blank()
    , axis.text.x = element_blank()
    , panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
    , panel.background = element_blank()
    , axis.ticks.x = element_blank()
    )

makePlot = (p1 / p2) + plot_layout(heights = c(9, 1))
ggsave(print(makePlot), filename = saveImg, width = 10, height = 8, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# boxplot
saveImg = sprintf("%s/%s/%s_%s.png", globalVar$figPath, serviceName, "d1", "boxplot")
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

# ggplot(data, aes(x = Culture, y = Percentage_1, color = Culture, fill = Culture)) +
#   geom_boxplot(color = "black", alpha = 0.1, show.legend = FALSE) +
#   geom_jitter(alpha  = 0.5, size = 2, show.legend = FALSE) +
#   stat_summary(fun = mean, geom = "point", color = "black", size = 2, shape = 16, show.legend = FALSE) +
#   stat_anova_test(label.x.npc = 0.3, label.y.npc = 0.90, label = "{method} p = {p.format} {p.signif}") +
#   facet_wrap(~Actual_behavior) +
#   theme_bw() +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


p1 = ggplot(data, aes(x = Culture, y = Percentage_1, color = Culture, fill = Culture)) +
  geom_boxplot(color = "black", alpha = 0.1, show.legend = FALSE) +
  geom_jitter(alpha  = 0.5, size = 2, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 2, shape = 16, show.legend = FALSE) +
  # ggpubr::stat_anova_test(comparisons = list(c("South Koreans", "Americans")), label.x.npc = 0.3, label.y.npc = 0.90, label = "{method} p = {p.format} {p.signif}") +
  ggpubr::geom_pwc(label = "{p.format} {p.signif}", show.legend = FALSE) +
  scale_y_continuous(minor_breaks=seq(0, 100, 20), breaks=seq(0, 100, 20), labels=sprintf("%s%%", seq(0, 100, 20)), expand=c(0,0), limits=c(0, 110)) +
  # scale_y_continuous(minor_breaks=seq(0, 100, 20), breaks=seq(0, 100, 20), labels=sprintf("%s%%", seq(0, 100, 20)), expand=c(0,0)) +
  labs(x = NULL, y = "Actual behavior") +
  facet_wrap(~Actual_behavior) +
  theme_bw()

p2 = statData %>% 
  dplyr::rename(
    Frequency = freq
  ) %>% 
  ggpubr::ggsummarytable(
    x = "Culture", y = c("Frequency"), digits = 1, size = 3, ggtheme = theme_bw()
  ) + 
  labs(x = NULL) + 
  theme(
    axis.line.x = element_blank()
    , axis.text.x = element_blank()
    , panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
    , panel.background = element_blank()
    , axis.ticks.x = element_blank()
  )

makePlot = (p1 / p2) + plot_layout(heights = c(9, 1))
ggsave(print(makePlot), filename = saveImg, width = 10, height = 8, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# ================================================
# d2 그래프 레이아웃 
# ================================================
# fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "d2-* (2x2 conditions).csv"))
# for (fileInfo in fileList) {
#   cat(sprintf("[CHECK] fileInfo : %s", fileInfo), "\n")
#   
#   fileName = tools::file_path_sans_ext(fs::path_file(fileInfo))
#   fileNameKey = stringr::str_split_1(fileName, " \\(")[1]
#   
#   data = readr::read_csv(fileInfo)
#   data$Culture = factor(data$Culture, levels=c(0, 1), labels = c("South Koreans", "Americans"))
#   data$Pursuit = factor(data$Pursuit, levels=c(0, 1), labels = c("True-Self", "Competence"))
#   
#   # violin 
#   saveImg = sprintf("%s/%s/%s_%s.png", globalVar$figPath, serviceName, fileNameKey, "violin")
#   dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#   
#   makePlot = ggpubr::ggsummarystats(data, x = "Culture", y = "DV_Intention", summaries = c("mean", "sd"), ggfunc = ggviolin, alpha = 0.5, error.plot = c("errorbar"), add = c("mean", "mean_sd"), fill = "Culture", facet.by = c("Pursuit"), digits = 2)
#   
#   ggsave(print(makePlot), filename = saveImg, width = 10, height = 8, dpi = 600)
#   
#   # shell.exec(saveImg)
#   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#   
#   # boxplot 
#   saveImg = sprintf("%s/%s/%s_%s.png", globalVar$figPath, serviceName, fileNameKey, "boxplot")
#   dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#   
#   makePlot = ggplot(data, aes(x = Culture, y = DV_Intention, color = Culture, fill = Culture)) +
#     geom_boxplot(color = "black", alpha = 0.1, show.legend = FALSE) +
#     geom_jitter(alpha  = 0.5, size = 2, show.legend = FALSE) +
#     stat_summary(fun = mean, geom = "point", color = "black", size = 2, shape = 16, show.legend = FALSE) +
#     stat_anova_test(label.x.npc = 0.3, label.y.npc = 0.90, label = "{method} p = {p.format} {p.signif}") +
#     facet_wrap(~ Pursuit) +
#     theme_bw()
#   
#   ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
#   
#   # shell.exec(saveImg)
#   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# }

dataL1 = tibble::tibble()
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "d2-* (2x2 conditions).csv"))
for (fileInfo in fileList) {
  cat(sprintf("[CHECK] fileInfo : %s", fileInfo), "\n")
  
  fileName = tools::file_path_sans_ext(fs::path_file(fileInfo))
  fileNameKey = stringr::str_split_1(fileName, " \\(")[1]
  
  data = readr::read_csv(fileInfo)
  data$Culture = factor(data$Culture, levels=c(0, 1), labels = c("South Koreans", "Americans"))
  data$Pursuit = factor(data$Pursuit, levels=c(0, 1), labels = c("True-Self", "Competence"))
  data$type = dplyr::case_when(
    fileNameKey == "d2-1" ~ "Study 3a"
    , fileNameKey == "d2-2" ~ "Study 3b"
    , fileNameKey == "d2-3" ~ "Study 3c"
  )
  
  dataL1 = dplyr::bind_rows(dataL1, data)
}


# dataL1$Culture %>% unique()
# dataL1$type %>% unique()
# dataL1$Pursuit %>% unique()

# 정렬
dataL1$Culture = forcats::fct_relevel(dataL1$Culture, c("Americans", "South Koreans"))
dataL1$Pursuit = forcats::fct_relevel(dataL1$Pursuit, c("True-Self",  "Competence"))

statData = dataL1 %>% 
  dplyr::group_by(Culture, type, Pursuit) %>% 
  dplyr::summarize(
    cnt = n()
    , meanVal = mean(DV_Intention, na.rm = TRUE)
    , sdVal = sd(DV_Intention, na.rm = TRUE)
    # , sdVal = sd(Percentage_1, na.rm = TRUE)/sqrt(cnt)
    # , upper = meanVal + (2 * sdVal)
    # , lower = meanVal - (2 * sdVal)
    , upper = meanVal + sdVal
    , lower = meanVal - sdVal
    , freq = cnt / nrow(data) * 100
  )

# violin 
saveImg = sprintf("%s/%s/%s_%s.png", globalVar$figPath, serviceName, "d2", "violin")
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

p1 = ggplot(dataL1, aes(x = Culture, y = DV_Intention, color = Pursuit, fill = Pursuit)) +
  geom_violin(alpha = 0.5, show.legend = FALSE, trim=FALSE, position=position_dodge(1.0)) +
  # geom_violin(alpha = 0.5, show.legend = FALSE, trim=FALSE) +
  geom_point(data=statData, aes(x = Culture, y = meanVal), position=position_dodge(1.0), color = "black", size=3, show.legend = FALSE) +
  geom_errorbar(data=statData, aes(x=Culture, y=NULL, ymax=upper, ymin=lower), stat='identity', width=0.1,color = "black", show.legend = FALSE, position=position_dodge(1.0)) +
  # scale_y_continuous(minor_breaks=seq(0, 100, 2), breaks=seq(0, 100, 2)), expand=c(0,0), limits=c(1, 10)) +
  labs(x = "Pursuit Condition", y = "Transition Willingness") +
  facet_wrap( ~ type, scales = "fixed", nrow = 1) +
  theme_bw()
# p1

p2 = statData %>% 
  dplyr::rename(
    Frequency = freq
  ) %>% 
  ggpubr::ggsummarytable(
    x = "Culture", y = c("Frequency"), facet.by = c("type"), digits = 1, size = 3, ggtheme = theme_bw(), position = position_dodge2(1.0)
  ) + 
  labs(x = NULL) + 
  theme(
    axis.line.x = element_blank()
    , axis.text.x = element_blank()
    , panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
    , panel.background = element_blank()
    , strip.text.x = element_blank()
    , axis.ticks.x = element_blank()
  )
# p2

makePlot = (p1 / p2) + plot_layout(heights = c(9, 1))
ggsave(print(makePlot), filename = saveImg, width = 10, height = 8, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# boxplot
saveImg = sprintf("%s/%s/%s_%s.png", globalVar$figPath, serviceName, "d2", "boxplot")
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

p1 = ggplot(dataL1, aes(x = Culture, y = DV_Intention, color = Pursuit, fill = Pursuit)) +
  geom_boxplot(color = "black", alpha = 0.1, show.legend = FALSE, position=position_dodge(1.0)) +
  geom_jitter(alpha  = 0.5, size = 2, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 2, shape = 16, show.legend = FALSE, position=position_dodge(1.0)) +
  ggpubr::geom_pwc(label = "{p.format} {p.signif}", show.legend = FALSE) +
  # labs(x = "Pursuit Condition", y = "Transition Willingness") +
  labs(x = NULL, y = "Transition Willingness") +
  facet_wrap( ~ type, scales = "fixed", nrow = 1) +
  theme_bw()
p1

p2 = statData %>% 
  dplyr::rename(
    Frequency = freq
  ) %>% 
  ggpubr::ggsummarytable(
    x = "Culture", y = c("Frequency"), facet.by = c("type"), digits = 1, size = 3, ggtheme = theme_bw(), position = position_dodge2(1.0)
  ) + 
  labs(x = NULL) + 
  theme(
    axis.line.x = element_blank()
    , axis.text.x = element_blank()
    , panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
    , panel.background = element_blank()
    , strip.text.x = element_blank()
    , axis.ticks.x = element_blank()
  )
# p2

makePlot = (p1 / p2) + plot_layout(heights = c(9, 1))
ggsave(print(makePlot), filename = saveImg, width = 10, height = 8, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ================================================
# d3 그래프 레이아웃 
# ================================================
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "d3 (2x2x2 conditions).csv"))
# 
# data = readr::read_csv(fileInfo)
# data$Culture = factor(data$Culture, levels=c(0, 1), labels = c("South Koreans", "Americans"))
# data$Pursuit = factor(data$Pursuit, levels=c(0, 1), labels = c("True-Self", "Competence"))
# data$Status = factor(data$Status, levels=c(0, 1), labels = c("Self-Employed", "Employed"))
# 
# # summary(data)
# 
# statusList = data$Status %>% unique() %>% sort()
# for (status in statusList) {
#   cat(sprintf("[CHECK] status : %s", status), "\n")
#   
#   dataL1 = data %>% 
#     dplyr::filter(Status == status)
#   
#   # violin 
#   saveImg = sprintf("%s/%s/%s_%s_%s.png", globalVar$figPath, serviceName, "d3", status, "violin")
#   dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#   
#   makePlot = ggpubr::ggsummarystats(dataL1, x = "Culture", y = "DV_Intention", summaries = c("mean", "sd"), ggfunc = ggviolin, alpha = 0.5, error.plot = c("errorbar"), add = c("mean", "mean_sd"), fill = "Culture", facet.by = c("Pursuit"), digits = 2)
#   
#   ggsave(print(makePlot), filename = saveImg, width = 10, height = 8, dpi = 600)
#   
#   # shell.exec(saveImg)
#   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#   
#   # boxplot 
#   saveImg = sprintf("%s/%s/%s_%s_%s.png", globalVar$figPath, serviceName, "d3", status, "boxplot")
#   dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#   
#   makePlot = ggplot(dataL1, aes(x = Culture, y = DV_Intention, color = Culture, fill = Culture)) +
#     geom_boxplot(color = "black", alpha = 0.1, show.legend = FALSE) +
#     geom_jitter(alpha  = 0.5, size = 2, show.legend = FALSE) +
#     stat_summary(fun = mean, geom = "point", color = "black", size = 2, shape = 16, show.legend = FALSE) +
#     stat_anova_test(label.x.npc = 0.3, label.y.npc = 0.90, label = "{method} p = {p.format} {p.signif}") +
#     facet_wrap(~ Pursuit) +
#     theme_bw()
#   
#   ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
#   
#   # shell.exec(saveImg)
#   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# }
