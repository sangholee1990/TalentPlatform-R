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
# R을 이용한 제품전후 전력량 비교 및 통계 검정

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0414"

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
library(ggplot2)
library(ggpubr)
library(webr)

# 파일 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "K.TAEAN호설치전후평가_230405.xlsx"))
data = openxlsx::read.xlsx(fileInfo, sheet = 1)

dataL1 = data %>%
  as.tibble() %>%
  dplyr::filter(
    ! is.na(before)
    , ! is.na(after)
  ) %>%
  dplyr::mutate(
    id = dplyr::row_number()
  ) %>%
  tidyr::gather(-id, key = "key", value = "val") %>% 
  dplyr::filter(key %in% c("before", "after")) 

dataL2 = dataL1 %>% 
  tidyr::pivot_wider(names_from = key, values_from = val) %>% 
  dplyr::mutate(rat = 100 - (after / before * 100)) %>% 
  dplyr::mutate(key = "ratio")

dataL3 = dataL2 %>%
  dplyr::select(-id, -before, -after) %>% 
  dplyr::summarise_all(list(
    sum = ~sum(., na.rm = TRUE),
    mean = ~mean(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE)
  ))

# *****************************************
# 시각화
# *****************************************
plotSubTitle = sprintf("%s", "제품전후 전력량에 따른 비교추이")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

makePlot = ggpubr::ggscatter(
  data = dataL1, x = "id", y = "val", color = "key"
  , add = "reg.line", alpha = 0.3, palette = c("#00AFBB", "#E7B800")
) +
  labs(title = NULL, x = "인덱스", y = "전력 사용량", color = NULL, subtitle = plotSubTitle) +
  theme_bw() +
  ggpubr::stat_regline_equation(aes(color = key), label.x.npc = 0.0, label.y.npc = 0.95, size = 6) +
  ggpubr::stat_cor(aes(color = key), label.x.npc = 0.5, label.y.npc = 0.95, p.accuracy = 0.01, r.accuracy = 0.01, size = 6) +
  theme(
    text = element_text(size = 18)
    , legend.position = "top"
  )

ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
ggplot2::last_plot()
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


plotSubTitle = sprintf("%s", "제품전후 전력량에 따른 절감율 추이")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

makePlot = ggpubr::ggscatter(
  data = dataL2, x = "id", y = "rat"
  , add = "reg.line", alpha = 0.3
) +
  labs(title = NULL, x = "인덱스", y = "절감 비율", fill = NULL, color = NULL, subtitle = plotSubTitle) +
  theme_bw() +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 0.95, size = 6) +
  ggpubr::stat_cor(label.x.npc = 0.5, label.y.npc = 0.95, p.accuracy = 0.01, r.accuracy = 0.01, size = 6) +
  theme(
    text = element_text(size = 18)
    # , legend.position = "top"
    , legend.position = "none"
  )

ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
ggplot2::last_plot()
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


plotSubTitle = sprintf("%s", "제품전후 전력량에 따른 밀도함수")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

makePlot = ggpubr::ggdensity(
  data = dataL1, x = "val", add = "mean", rug = TRUE,
  color = "key", fill = "key", palette = c("#00AFBB", "#E7B800")
  ) +
  labs(title = NULL, x = "전력 사용량", y = "밀도함수", color = NULL, subtitle = plotSubTitle) +
  theme(
    text = element_text(size = 18)
    , legend.position = "top"
  )

ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
ggplot2::last_plot()
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


plotSubTitle = sprintf("%s", "제품전후 전력량에 따른 빈도분포")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

makePlot = ggpubr::gghistogram(
  data = dataL1, x = "val", add = "mean", rug = TRUE,
  color = "key", fill = "key", palette = c("#00AFBB", "#E7B800")
) +
  labs(title = NULL, x = "전력 사용량", y = "밀도함수", color = NULL, subtitle = plotSubTitle) +
  theme(
    text = element_text(size = 18)
    , legend.position = "top"
  )

ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
ggplot2::last_plot()
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


plotSubTitle = sprintf("%s", "제품전후 전력량에 따른 상자그림")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

makePlot = ggpubr::ggboxplot(
  dataL1, x = "key", y = "val", color = "key", palette =c("#00AFBB", "#E7B800"),
  add = "jitter", shape = "key", alpah = 0.1
) +
  ggpubr::stat_compare_means(comparisons = list( c("before", "after"))) +
  labs(title = NULL, x = "전력 사용량", y = "밀도함수", color = NULL, subtitle = plotSubTitle) +
  theme(
    text = element_text(size = 18)
    , legend.position = "top"
  )

ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
ggplot2::last_plot()
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# *****************************************
# 통계 검정
# *****************************************
# F 검정
fTest = var.test(val ~ key, data = dataL1, conf.level = 0.95)
print(fTest)

# F 검정에서 유의수준 p-value < 0.05 이하로서 귀무가설이 기각 (두 그룹은 분산 차이)
mainTitle = sprintf("%s", "제품전후 간의 F 검정")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

plot(fTest) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# T 검정
# 등분산 가정 O
# tTest = t.test(val ~ key, data = dataL1, conf.level = 0.95, var.equal = TRUE, paired = FALSE)

# 등분산 가정 X
tTest = t.test(val ~ key, data = dataL1, conf.level = 0.95, var.equal = FALSE, paired = FALSE)

# T 검정에서 유의수준 p-value는 0.01 이하로서 귀무가설 기각 (두 그룹은 평균 차이)
print(tTest)

mainTitle = sprintf("%s", "제품전후 간의 T 검정")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

plot(tTest) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")