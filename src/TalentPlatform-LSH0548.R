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

# ggplotDefaultColor = scales::hue_pal()(3)

# ================================================
# 파일 읽기
# ================================================
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "d1 (2 conditions).csv"))

data = readr::read_csv(fileInfo)
  

data$Culture = factor(data$Culture, levels=c(0, 1), labels = c("South Koreans", "Americans"))

summary(data)

ggpubr::ggsummarystats(
  data, x = "Culture", y = "Percentage_1", summaries = c("mean", "sd"), 
  ggfunc = ggviolin
  , error.plot = c("errorbar"), add = c("mean", "mean_sd"),  fill = "Culture"
)

# facet.by="Actual_behavior"

# ggplot(data, aes(x = "Culture", y = "Percentage_1")) +
#   geom_boxplot()

ggpubr::ggboxplot(data, x = "Culture", y = "Percentage_1", color = "Culture", add = c("mean", "mean_sd", "jitter"), facet.by="Actual_behavior") + 
  stat_anova_test(label.x.npc = 0.3, label.y.npc = 0.90, label = "{method} p = {p.format} {p.signif}")

# + 
  # ggscatter(data,  x = "Culture", y = "Percentage_1", color = "Culture")
  
  # geom_point(data = data, aes(x = "Culture", y = "Percentage_1", colour = "Culture"))  



  # labs(
  #   title = NULL
  #   , fill = NULL
  #   # , x = "예측"
  #   # , y = "실측"
  #   # , subtitle = mainTitle
  # ) +
  # theme(text = element_text(size = 16))

# ggsave(makePlot, filename = saveImg, width = 6, height = 6, dpi = 600)

  




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

