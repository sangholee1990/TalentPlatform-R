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
# R을 이용한 2차원 (X, Y, Z 평면) 내 산점도 및 신뢰구간 95% 시각화
# R을 이용한 상자그림 및 바이올린 융합 시각화 (기술 통계, 통계 검정)

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
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

# ggplotDefaultColor = scales::hue_pal()(3)

# ================================================
# 파일 읽기
# d1 그래프 레이아웃 
# ================================================
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "d1 (2 conditions).csv"))

data = readr::read_csv(fileInfo)
  

data$Culture = factor(data$Culture, levels=c(0, 1), labels = c("South Koreans", "Americans"))
data$Actual_behavior = factor(data$Actual_behavior, levels=c(0, 1), labels = c("No", "Yes"))

summary(data)

ggpubr::ggsummarystats(
  data, x = "Culture", y = "Percentage_1", summaries = c("mean", "sd"), 
  ggfunc = ggviolin
  , error.plot = c("errorbar"), add = c("mean", "mean_sd"),  fill = "Culture"
)

# facet.by="Actual_behavior"
# violin 
saveImg = sprintf("%s/%s/%s_%s.png", globalVar$figPath, serviceName, "d1", "violin")
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

# ggplot(data, aes(x = "Culture", y = "Percentage_1")) +
#   geom_boxplot()

ggpubr::ggboxplot(data, x = "Culture", y = "Percentage_1", color = "Culture", add = c("mean", "mean_sd", "jitter"), facet.by="Actual_behavior") + 
  stat_anova_test(label.x.npc = 0.3, label.y.npc = 0.90, label = "{method} p = {p.format} {p.signif}")
makePlot = ggpubr::ggsummarystats(data, x = "Culture", y = "Percentage_1", summaries = c("mean", "sd"), ggfunc = ggviolin, alpha = 0.5, error.plot = c("errorbar"), add = c("mean", "mean_sd"), fill = "Culture", facet.by = c("Actual_behavior"), digits = 2)
  
ggsave(print(makePlot), filename = saveImg, width = 10, height = 8, dpi = 600)

# + 
  # ggscatter(data,  x = "Culture", y = "Percentage_1", color = "Culture")
  
  # geom_point(data = data, aes(x = "Culture", y = "Percentage_1", colour = "Culture"))  
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

  # labs(
  #   title = NULL
  #   , fill = NULL
  #   # , x = "예측"
  #   # , y = "실측"
  #   # , subtitle = mainTitle
  # ) +
  # theme(text = element_text(size = 16))

# ggsave(makePlot, filename = saveImg, width = 6, height = 6, dpi = 600)

# ================================================
# d2 그래프 레이아웃 
# ================================================
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "d2-* (2x2 conditions).csv"))
for (fileInfo in fileList) {
  cat(sprintf("[CHECK] fileInfo : %s", fileInfo), "\n")
  
  fileName = tools::file_path_sans_ext(fs::path_file(fileInfo))
  fileNameKey = stringr::str_split_1(fileName, " \\(")[1]
  




# 정렬


data

data$Culture = as.factor(data$Culture)

summary.stats = data %>%
  group_by(Culture) %>%
  get_summary_stats(type = "common") %>% 
  dplyr::filter(
    variable == "Percentage_1"
  )

ggsummarytable(
  summary.stats, x = "Culture", y = c("n", "mean", "sd"),
  ggtheme = theme_bw()
)

# ggggboxplot(data, x = "Culture", y = "Percentage_1",
#                color = "Culture", add = "jitter", shape = "Culture")
# 
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

as.factor(c(0, 1))
# data$Culture
# my_comparisons <- list( c(0, 1) )
my_comparisons <- list( as.factor(c(0, 1)) )
ggsummarystats(
  data, x = "Culture", y = "Percentage_1", summaries = c("mean", "sd"), 
  ggfunc = ggboxplot, add = "jitter",  color = "Culture", shape = "Culture"
)
  ) +
  # stat_compare_means(label.y = 50)
  # stat_compare_means(comparisons = my_comparisons)
# ================================================
# d3 그래프 레이아웃 
# ================================================
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "d3 (2x2x2 conditions).csv"))

data = readr::read_csv(fileInfo)
data$Culture = factor(data$Culture, levels=c(0, 1), labels = c("South Koreans", "Americans"))
data$Pursuit = factor(data$Pursuit, levels=c(0, 1), labels = c("True-Self", "Competence"))
data$Status = factor(data$Status, levels=c(0, 1), labels = c("Self-Employed", "Employed"))

# data = openxlsx::read.xlsx(fileInfo, sheet = 1)
# data$group = as.factor(data$group)
# 
# # colInfo = c("X", "Y")
# # typeInfo = 21
# typeList = data$type %>% unique() %>% sort()
# colList = list(c("X", "Y"), c("X", "Z"), c("Z", "Y"))
# for (typeInfo in typeList) {
#   for (colInfo in colList) {
#     
#     dataL1 = data %>%
#       dplyr::filter(
#         type == typeInfo
#       ) %>% 
#       dplyr::rename(
#         X = x
#         , Y = y
#         , Z = z
#       )
#     
#     statData = dataL1 %>% 
#       dplyr::group_by(group) %>% 
#       dplyr::summarise(
#         meanX = mean(X, na.rm = TRUE)
#         , meanY = mean(Y, na.rm = TRUE)
#         , meanZ = mean(Z, na.rm = TRUE)
#         , sdX = sd(X, na.rm = TRUE)
#         , sdY = sd(Y, na.rm = TRUE)
#         , sdZ = sd(Z, na.rm = TRUE)
#       )
#     
#     
#     # i = 1
#     # groupInfo = 0
#     statDataL1 = tibble::tibble()
#     statDataL2 = tibble::tibble()
#     groupList = statData$group %>% unique() %>% sort()
#     for (groupInfo in groupList) {
#       
#       statDataX = statData %>% 
#         dplyr::filter(group == groupInfo) %>% 
#         dplyr::select(dplyr::ends_with(colInfo[[1]])) %>%
#         magrittr::set_colnames(c("mean", "sd"))
#       
#       statDataY = statData %>% 
#         dplyr::filter(group == groupInfo) %>% 
#         dplyr::select(dplyr::ends_with(colInfo[[2]])) %>%
#         magrittr::set_colnames(c("mean", "sd"))
#       
#       x = cos(seq(0, 5 * pi, length.out = 100)) * (1.96 * statDataX$sd) + statDataX$mean
#       y = sin(seq(0, 5 * pi, length.out = 100)) * (1.96 * statDataY$sd) + statDataY$mean
#       
#       tmpData = tibble::tibble(group = groupInfo, x = x, y = y) %>% 
#         dplyr::mutate(angle = atan2(y, x)) %>%
#         dplyr::arrange(angle)
#       
#       statDataL1 = dplyr::bind_rows(statDataL1, tibble::tibble(group = groupInfo, x = statDataX$mean, y = statDataY$mean) )
#       statDataL2 = dplyr::bind_rows(statDataL2, tmpData)
#     }
#     
#     plotSubTitle = sprintf("%s_%s%s-axis-error", typeInfo, colInfo[[1]], colInfo[[2]])
#     # saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
#     saveImg = sprintf("%s/%s/%s.tiff", globalVar$figPath, serviceName, plotSubTitle)
#     dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#     
#     makePlot = ggplot() +
#       geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") + 
#       geom_vline(xintercept = 0, colour = "grey", linetype = "dashed") +
#       
#       # geom_point(data = dataL1, aes(colInfo[[1]], colInfo[[2]], colour = group)) +
#       geom_point(data = dataL1, aes(x = !!sym(colInfo[[1]]), y = !!sym(colInfo[[2]]), colour = group)) +
#       
#       # geom_point(data = statDataL1, aes(x, y, color = group), shape=17, size=3, show.legend = FALSE) +
#       geom_point(data = statDataL1 %>% dplyr::filter(group == 0), aes(x, y), color = "red", shape=17, size=3, show.legend = FALSE) +
#       geom_point(data = statDataL1 %>% dplyr::filter(group == 1), aes(x, y), color = "blue", shape=17, size=3, show.legend = FALSE) +
#       geom_point(data = statDataL1 %>% dplyr::filter(group == 2), aes(x, y), color = "green", shape=17, size=3, show.legend = FALSE) +
# 
#       # geom_path(data = statDataL2, aes(x, y, colour = group), size = 0.5, linetype = 2, show.legend = FALSE) +
#       geom_path(data = statDataL2 %>% dplyr::filter(group == 0), aes(x, y), color = "red", size = 0.5, linetype = 2, show.legend = FALSE) +
#       geom_path(data = statDataL2 %>% dplyr::filter(group == 1), aes(x, y), color = "blue", size = 0.5, linetype = 2, show.legend = FALSE) +
#       geom_path(data = statDataL2 %>% dplyr::filter(group == 2), aes(x, y), color = "green", size = 0.5, linetype = 2, show.legend = FALSE) +
#       
#       labs(title = NULL, x = sprintf("%s-axis error (mm)", colInfo[[1]]), y =  sprintf("%s-axis error (mm)", colInfo[[2]]), color = "group") +
#       # xlim(-5, 5) +
#       # ylim(-5, 5) +
#       scale_x_continuous(minor_breaks = seq(-4, 4, 2), breaks=seq(-4, 4, 2), limits=c(-5, 5)) +
#       scale_y_continuous(minor_breaks = seq(-4, 4, 2), breaks=seq(-4, 4, 2), limits=c(-5, 5)) +
#       theme_classic() +
#       theme(
#         panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
#         , legend.background = element_rect(colour = "black", fill = NA, size = 0.5)
#         , legend.position = c(0.98, 0.98)
#         , legend.justification = c("right", "top")
#       ) +
#       scale_color_manual(
#         name = NULL
#         , na.value = "transparent"
#         , values = c("0" = ggplotDefaultColor[1], "1" = ggplotDefaultColor[3], "2" = ggplotDefaultColor[2])
#         , labels = c("Maxilla-first", "Mandible-first", "Mandible only")
#       )
#     
#     ggsave(makePlot, filename = saveImg, width = 5, height = 5, dpi = 600)
#     
#     # shell.exec(saveImg)
#     cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#   }
# }
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


