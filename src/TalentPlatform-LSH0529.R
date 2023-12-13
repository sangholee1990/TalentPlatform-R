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
# R을 이용한 학위 논문 표 및 그림 시각화

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0529"

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
# devtools::install_github("haozhu233/kableExtra")
library(tidyverse)
library(kableExtra)
library(formattable)
library(webshot)
library(openxlsx)
library(forcats)


# 파일 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "LSH0529. R을 이용한 학위 논문 표 및 그림 시각화.xlsx"))

# 그림 생성
figData = openxlsx::read.xlsx(fileInfo, sheet = "그림") %>% 
  as.tibble()


# figData$name %>% unique()
# 난방 총 용량의 건물에너지성능 영향정도
# 냉난방설비 종류의 건물에너지성능 영향정도
# 신재생에너지 종류의 건물에너지성능 영향정도
# 태양광 효율의 건물에너지성능 영향정도
# EHP의 난방 COP값의 건물에너지성능 영향정도
# 전열교환기 난방효율의 건물에너지성능 영향정도

# 외벽 열관류율의 냉난방에너지 영향정도
# 지붕 열관류율의 냉난방에너지 영향정도
# 바닥 열관류율의 냉난방에너지 영향정도
# 창호 열관류율의 냉난방에너지 영향정도
# 창면적비의 냉난방에너지 영향정도
figDataL1 = figData %>% 
  dplyr::filter(name2 %in% c("외벽", "지붕", "바닥", "창호", "창면적비")) %>% 
  dplyr::mutate(
    key2 = ifelse(is.na(key2), "", key2)
    # , leg = sprintf("%s\n%s", key2, key)
    , leg = sprintf("%s (%s)", key2, key)
  ) %>% 
  dplyr::select(leg, name2, Heating, Cooling) %>%
  tidyr::pivot_longer(cols = c("Heating", "Cooling"), names_to = "key", values_to = "val")

legList = figDataL1$leg %>% unique() %>% sort(decreasing = TRUE)

# 정렬
figDataL1$key = forcats::fct_relevel(figDataL1$key, c("Heating", "Cooling"))
figDataL1$leg = forcats::fct_relevel(figDataL1$leg, legList)

mainTitle = sprintf("%s", "열관류율의 냉난방에너지 영향정도")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(figDataL1, aes(x = leg, y = val, color = key, group = key, label = round(val, 2))) + 
  geom_line() +
  geom_point() +
  labs(x = NULL, y = "냉난방에너지 영향정도", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  # scale_y_continuous(minor_breaks = seq(0, 50, 5), breaks=seq(0, 40, 5), limits = c(30, 40)) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
    , axis.text.x = element_text(angle = 45, hjust = 1, size = 11)
  ) +
  facet_wrap(~ name2, scale = "free_x", ncol = 2) + 
  ggsave(filename = saveImg, width = 8, height = 10, dpi = 600)

 cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 난방 총 용량의 건물에너지성능 영향정도
figDataL1 = figData %>% 
  dplyr::filter(name == "난방 총 용량의 건물에너지성능 영향정도") %>% 
  dplyr::mutate(
    key2 = ifelse(is.na(key2), "", key2)
    # , leg = sprintf("%s\n%s", key2, key)
    , leg = sprintf("%s (%s)", key2, key)
  ) %>% 
  dplyr::select(leg, name2, Heating, Cooling) %>%
  tidyr::pivot_longer(cols = c("Heating", "Cooling"), names_to = "key", values_to = "val")

legList = figDataL1$leg %>% unique() %>% sort(decreasing = TRUE)

# 정렬
figDataL1$key = forcats::fct_relevel(figDataL1$key, c("Heating", "Cooling"))
figDataL1$leg = forcats::fct_relevel(figDataL1$leg, legList)

mainTitle = sprintf("%s", "난방 총 용량의 건물에너지성능 영향정도")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(figDataL1, aes(x = leg, y = val, color = key, group = key, label = round(val, 2))) + 
  geom_line() +
  geom_point() +
  labs(x = NULL, y = "건물에너지성능 영향정도", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  scale_y_continuous(minor_breaks = seq(0, 50, 5), breaks=seq(0, 50, 5), limits = c(30, 50)) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
    , axis.text.x = element_text(angle = 45, hjust = 1, size = 14)
  ) +
  facet_wrap(~ name2, scale = "free_x", ncol = 2) + 
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")



tabData = openxlsx::read.xlsx(fileInfo, sheet = "표") %>% 
  as.tibble()






# 
# my_table = mtcars[1:8, 1:8] %>%
#   dplyr::mutate(
#     hp = color_bar("lightblue")(hp)
#     , wt = cell_spec(wt, "html", color = ifelse(wt > 2, "red", "blue"))
#     , vs = cell_spec(vs, "html", color = "white", align = "c", background = factor(vs, c(0, 1), c("#666666", "#BBBBBB")))
#     # , cyl = color_tile(c(rainbow(3)d))(cyl)
#     , cyl = color_tile("white", "orange")(cyl)
#     ) %>% 
#   kbl(escape = FALSE) %>%
#   kable_paper(c("striped"), full_width = FALSE) %>%
#   column_spec(2, color = spec_color(mtcars$mpg[1:8])) %>%
#   column_spec(6, color = "white",
#               background = spec_color(mtcars$drat[1:8], end = 0.7),
#               popover = paste("am:", mtcars$am[1:8]))
# my_table
# 
# 
# html_table <- "my_table.html"
# writeLines(as.character(my_table), html_table)
# webshot(html_table, "my_table.png", delay = 2) 


