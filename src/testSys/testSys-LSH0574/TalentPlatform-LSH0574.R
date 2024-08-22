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
# R을 이용한 중국-인도 및 태양광-풍력 데이터 누적 막대 차트 시각화

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0574"

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
library(ggrepel) 
library(readxl)
library(scales)
library(gridExtra)
library(cowplot)
library(patchwork)
library(zoo)
library(treemapify)
library(pals)

# Load the data
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "Employment_Results.xlsx"))

# ==============================================================================
# FIG 3ab
# ==============================================================================
# Import data
data <- read_excel(fileInfo, sheet = "Fig 2")

dataL1 = data %>% 
  dplyr::mutate(key = sprintf("%s %s", Country, Type))

statData = dataL1 %>% 
  dplyr::group_by(key) %>% 
  dplyr::summarise(
    maxJob = max(Jobs_ths, na.rm = TRUE)
    , maxCap = max(Capacity_GW, na.rm = TRUE)
  )

dataL2 = dplyr::left_join(dataL1, statData, by = c("key" = "key")) %>% 
  dplyr::mutate(
    val = (Capacity_GW * maxJob / maxCap) + (maxJob * 0.05)
  )

# dataL2$key %>% unique() %>% sort()

# ggplot(dataL1, aes(x = as.factor(Year), fill = key)) +
#   geom_bar(aes(y = Jobs_ths, group = interaction(Country)), stat = "identity", position = position_dodge(width = 0.9), alpha = 1)
  
# cbCoolwarm = pals::warmcool(n = 4)

capacityScale = max(dataL2$Jobs_ths, na.rm = TRUE) / max(dataL2$Capacity_GW, na.rm = TRUE)
lineOffset = max(dataL2$Jobs_ths, na.rm = TRUE) * 0.05

mainTitle = sprintf("%s", "China-India_Jobs")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL2, aes(x = Year, color = key, fill = key)) +
  geom_bar(aes(y = Jobs_ths, color = NULL, group = interaction(Country)), stat = "identity", position = position_dodge(width = 0.9), alpha = 1) +
  geom_line(aes(y = val), linetype = "solid", size = 1, position = position_dodge(width = 0.9), show.legend = FALSE) +
  geom_point(aes(y = val), size = 3, shape = 18, position = position_dodge(width = 0.9), show.legend = FALSE) +
  scale_x_continuous(minor_breaks=seq(2010, 2020, 1), breaks=seq(2010, 2020, 1), limits=c(2014.5, 2020.5)) +
  scale_y_continuous(
    name = "Jobs (thousands)",
    breaks = pretty_breaks(),
    sec.axis = sec_axis(~ (. - lineOffset) / capacityScale, name = "Newly installed capacity (GW)", breaks = pretty_breaks())
  ) +
  # labs(x = "Year", title = mainTitle) +
  labs(x = NULL, title = NULL) +
  theme_bw() +
  theme(panel.border = element_rect(linewidth = 1.0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.1, 0.9), 
        legend.key.size = unit(0.6, 'cm'),
        legend.margin = margin(0, 0, 0, 0),
        legend.title = element_blank(),
        text = element_text(size = 14, family = "serif", face = "bold")) +
  # scale_fill_manual(values = grDevices::colorRampPalette(c("#d65d48", "#599CB4"))(4)) +
  # scale_color_manual(values = grDevices::colorRampPalette(c("#EF8B67", "#92B5CA"))(4)) +
  scale_fill_manual(values = c("China Solar" = "#ED7D31", "China Wind" = "#C55A11", "India Solar" = "#4472C4", "India Wind" = "#2F5597"), name = NULL, na.value = NA) +
  scale_color_manual(values = c("China Solar" = "#F6BE98", "China Wind" = "#D68B58", "India Solar" = "#A1B8E1", "India Wind" = "#6D88B6"), name = NULL, na.value = NA) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ==============================================================================
# FIG 3cd
# ==============================================================================
statData = dataL1 %>% 
  dplyr::group_by(key) %>% 
  dplyr::summarise(
    maxJob = max(Earnings_billion, na.rm = TRUE)
    , maxCap = max(Earning_perGW, na.rm = TRUE)
  )

dataL2 = dplyr::left_join(dataL1, statData, by = c("key" = "key")) %>% 
  dplyr::mutate(
    val = (Capacity_GW * maxJob / maxCap) + (maxJob * 0.05)
  )

# cbCoolwarm = pals::warmcool(n = 4)

capacityScale = max(dataL2$Earnings_billion, na.rm = TRUE) / max(dataL2$Earning_perGW, na.rm = TRUE)
lineOffset = max(dataL2$Earning_perGW, na.rm = TRUE) * 0.05

mainTitle = sprintf("%s", "China-India_Ear")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL2, aes(x = Year, color = key, fill = key)) +
  geom_bar(aes(y = Earnings_billion, group = interaction(Country), color = NULL), stat = "identity", position = position_dodge(width = 0.9), alpha = 1) +
  geom_line(aes(y = val), linetype = "solid", size = 1, position = position_dodge(width = 0.9), show.legend = FALSE) +
  geom_point(aes(y = val), size = 3, shape = 18, position = position_dodge(width = 0.9), show.legend = FALSE) +
  scale_x_continuous(minor_breaks=seq(2010, 2020, 1), breaks=seq(2010, 2020, 1), limits=c(2014.5, 2020.5)) +
  scale_y_continuous(
    name = "Job earnings (billion US$)",
    breaks = pretty_breaks(),
    sec.axis = sec_axis(~ (. - lineOffset) / capacityScale, name = "Job earnings per capacity (million US$/GW)", breaks = pretty_breaks())
  ) +
  # labs(x = "Year", title = mainTitle) +
  labs(x = NULL, title = NULL) +
  theme_bw() +
  theme(panel.border = element_rect(linewidth = 1.0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.1, 0.9), 
        legend.key.size = unit(0.6, 'cm'),
        legend.margin = margin(0, 0, 0, 0),
        legend.title = element_blank(),
        text = element_text(size = 14, family = "serif", face = "bold")) +
  # scale_fill_manual(values = grDevices::colorRampPalette(c("#d65d48", "#599CB4"))(4)) +
  # scale_color_manual(values = grDevices::colorRampPalette(c("#EF8B67", "#92B5CA"))(4)) +
  scale_fill_manual(values = c("China Solar" = "#ED7D31", "China Wind" = "#C55A11", "India Solar" = "#4472C4", "India Wind" = "#2F5597"), name = NULL, na.value = NA) +
  scale_color_manual(values = c("China Solar" = "#F6BE98", "China Wind" = "#D68B58", "India Solar" = "#A1B8E1", "India Wind" = "#6D88B6"), name = NULL, na.value = NA) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ==============================================================================
# 원본 코드
# ==============================================================================
# # Function to generate chart for Jobs_ths
# plot_fig_jobs <- function(df, country_name, fig_title) {
#   # Filter data
#   df_filtered <- df %>%
#     filter(Country == country_name)
# 
#   # Calculate scale for secondary y-axis
#   capacity_scale <- max(df_filtered$Jobs_ths, na.rm = TRUE) / max(df_filtered$Capacity_GW, na.rm = TRUE)
# 
#   # Set height offset for line chart
#   line_offset <- max(df_filtered$Jobs_ths, na.rm = TRUE) * 0.05
# 
#   # Create plot
#   ggplot(df_filtered, aes(x = as.factor(Year), group = Type)) +
#     geom_bar(aes(y = Jobs_ths, fill = Type), stat = "identity", position = position_dodge(width = 0.9), alpha = 1) +
#     geom_line(aes(y = (Capacity_GW * capacity_scale) + line_offset, color = Type), linetype = "solid", size = 1, position = position_dodge(width = 0.9), show.legend = FALSE) +
#     geom_point(aes(y = (Capacity_GW * capacity_scale) + line_offset, color = Type), size = 3, shape = 18, position = position_dodge(width = 0.9), show.legend = FALSE) +
#     scale_y_continuous(
#       name = "Jobs (thousands)",
#       breaks = pretty_breaks(),
#       sec.axis = sec_axis(~ (. - line_offset) / capacity_scale, name = "Newly installed capacity (GW)", breaks = pretty_breaks())
#     ) +
#     labs(x = "Year", title = fig_title) +
#     theme_bw() +
#     theme(panel.border = element_rect(linewidth = 1.5),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           legend.position = c(0.1, 0.9),
#           legend.key.size = unit(0.6, 'cm'),
#           legend.margin = margin(0, 0, 0, 0),
#           legend.title = element_blank(),
#           text = element_text(size = 14, family = "serif", face = "bold")) +
#     scale_fill_manual(values = c("#d65d48", "#599CB4")) +
#     scale_color_manual(values = c("#EF8B67", "#92B5CA"))
# }
# 
# # Function to generate chart for Earnings_billion
# plot_fig_earnings_pw <- function(df, country_name, fig_title) {
#   # Filter data
#   df_filtered <- df %>%
#     filter(Country == country_name)
# 
#   # Calculate scale for secondary y-axis
#   capacity_scale <- max(df_filtered$Earnings_billion, na.rm = TRUE) / max(df_filtered$Earning_perGW, na.rm = TRUE)
# 
#   # Set height offset for line chart
#   line_offset <- max(df_filtered$Earnings_billion, na.rm = TRUE) * 0.05
# 
#   # Create plot
#   ggplot(df_filtered, aes(x = as.factor(Year), group = Type)) +
#     geom_bar(aes(y = Earnings_billion, fill = Type), stat = "identity", position = position_dodge(width = 0.9), alpha = 1) +
#     geom_line(aes(y = (Earning_perGW * capacity_scale) + line_offset, color = Type), linetype = "solid", size = 1, position = position_dodge(width = 0.9), show.legend = FALSE) +
#     geom_point(aes(y = (Earning_perGW * capacity_scale) + line_offset, color = Type), size = 3, shape = 18, position = position_dodge(width = 0.9), show.legend = FALSE) +
#     scale_y_continuous(
#       name = "Job earnings (billion US$)",
#       breaks = pretty_breaks(),
#       sec.axis = sec_axis(~ (. - line_offset) / capacity_scale, name = "Job earnings per capacity (million US$/GW)", breaks = pretty_breaks())
#     ) +
#     labs(x = "Year", title = fig_title) +
#     theme_bw() +
#     theme(panel.border = element_rect(linewidth = 1.5),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           legend.position = c(0.1, 0.7),
#           legend.key.size = unit(0.5, 'cm'),
#           legend.margin = margin(0, 0, 0, 0),
#           legend.title = element_blank(),
#           text = element_text(size = 14, family = "serif", face = "bold")) +
#     scale_fill_manual(values = c("#d65d48", "#599CB4")) +
#     scale_color_manual(values = c("#EF8B67", "#92B5CA"))
# }
# 
# # Create chart Fig 2a - China (Jobs_ths)
# fig2a <- plot_fig_jobs(data, "China", "China")
# fig2a
# 
# # Create chart Fig 2b - India (Jobs_ths)
# fig2b <- plot_fig_jobs(data, "India", "India")
# fig2b
# 
# # Create chart Fig 2c - China (Earnings_billion)
# fig2c <- plot_fig_earnings_pw(data, "China", "China")
# fig2c
# 
# # Create chart Fig 2d - India (Earnings_billion)
# fig2d <- plot_fig_earnings_pw(data, "India", "India")
# fig2d
# 
# ggsave("Fig2a.png", fig2a, width = 6.1, height = 4, dpi=600)
# ggsave("Fig2b.png", fig2b, width = 6.1, height = 4, dpi=600)
# ggsave("Fig2c.png", fig2c, width = 6.1, height = 4, dpi=600)
# ggsave("Fig2d.png", fig2d, width = 6.1, height = 4, dpi=600)