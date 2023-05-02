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
# R을 이용한 설문조사 가공 및 다양한 시각화

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0419"

if (Sys.info()["sysname"] == "Windows") {
  contextPath = ifelse(env == "local", ".", "E:/04. TalentPlatform/Github/TalentPlatform-R")
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
library(mgcv)
library(forcats)
library(knitr)
library(lattice)
library(tidyverse)
library(likert)
library(MASS)
library(psych)
library(viridis)
library(ggplot2)
library(here)
library(flextable)
library(likert)
library(naniar)
library(likert)
library(mirt)
library(ggmirt)
library(grid)
library(UpSetR)
library(ComplexUpset)

itemPersonMap = function(mod) {
  require("mirt")
  require("dplyr")
  require("reshape2")
  require("ggplot2")
  require("cowplot")

  if (unique(mod@Model$itemtype) != "Rasch") {
    stop('You must select itemtype = "Rasch" for all items')
  } else {

    pars <- as.data.frame(coef(mod, IRTpars = TRUE, simplify = TRUE)$items)

    pars <- pars %>%
      dplyr::select(-a) %>%
      mutate(item = row.names(pars)) %>%
      melt(data = .,
           id.vars = "item",
           variable.name = "threshold",
           value.name = "parameter")

    pars_mean <- pars %>%
      group_by(item) %>%
      summarise(mean_threshold = mean(parameter))

    theta <- as.data.frame(fscores(mod)) %>%
      rename(theta = F1)

    # Histogram of latent trait distribution
    p1 <- ggplot(data = theta,
                 aes(x = theta)) +
      geom_histogram(bins = 30, fill = "royalblue2", colour = "lightgray") +
      theme_bw(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45)) +
      labs(x = "Latent Trait", y = "") +
      scale_x_continuous(limits = c(-5, 5), breaks = seq(-5, 5, by = 1)) +
      coord_flip() +
      scale_y_reverse()

    # Dot and line plot of item thresholds
    p2 <- ggplot(data = pars,
                 aes(x = item, y = parameter)) +
      geom_line() +
      geom_point(aes(shape = threshold), size = 3, colour = "indianred1") +
      geom_point(data = pars_mean, aes(x = item, y = mean_threshold), size = 3,
                 colour = "black", shape = 8) +
      theme_bw(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none") +
      labs(x = "", y = "Item Thresholds", shape = "Threshold") +
      scale_y_continuous(position = "right", limits = c(-5, 5), breaks = seq(-5, 5, by = 1))

    # Combine the plots together
    cowplot::plot_grid(p1, p2, align = "h")
  }
}

# 파일 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "컨설팅+분석의뢰서_+shlee1990_+20230425_3.xlsx"))
allData = openxlsx::read.xlsx(fileInfo, sheet = 3, startRow = 8) %>%
  as.tibble()

subData = openxlsx::read.xlsx(fileInfo, sheet = 4, startRow = 8) %>%
  as.tibble()

# ****************************************************************************
# 1번
# ****************************************************************************
colnames(allData)

data = allData %>%
  dplyr::select("생활적응", "심리적응", "보호요인") %>%
  na.omit()

bot30 = quantile(data$`생활적응`, probs = 0.30)
top30 = quantile(data$`생활적응`, probs = 0.70)

dataL1 = data %>% dplyr::mutate(
  type = dplyr::case_when(
    생활적응 <= bot30 ~ "부적응군",
    생활적응 > bot30 & 생활적응 < top30 ~ "적응군",
    생활적응 >= top30 ~ "우수군"
  )
  , key = dplyr::case_when(
    보호요인 < 35 ~ "낮음",
    보호요인 >= 35 & 보호요인 < 65 ~ "보통",
    보호요인 >= 65 ~ "높음"
  )
  , key2 = dplyr::case_when(
    심리적응 < 35 ~ "낮음",
    심리적응 >= 35 & 심리적응 < 65 ~ "보통",
    심리적응 >= 65 ~ "높음"
  )
)


dataL1$type = forcats::fct_relevel(dataL1$type, c("부적응군", "우수군", "적응군"))
dataL1$key = forcats::fct_relevel(dataL1$key, c("낮음", "보통", "높음"))
dataL1$key2 = forcats::fct_relevel(dataL1$key2, c("낮음", "보통", "높음"))

plotSubTitle = sprintf("%s", "생활적응에 따른 보호요인 및 심리적용 상자그림")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

makePlot = ggplot(data = dataL1, aes(x = key, y = 심리적응)) +
  geom_boxplot(size = 0.5) +
  geom_jitter(alpha = 0.2) +
  facet_grid(key2 ~ type) +
  theme_bw() +
  labs(x = "보호 요인", y = "심리 적응", subtitle = plotSubTitle) +
  theme(text = element_text(size = 16))

ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ****************************************************************************
# 2번
# ****************************************************************************
dataList = list(allData = allData, subData = subData)

nameInfo = "allData"
for (nameInfo in names(dataList)) {

  data = get(nameInfo, dataList) %>%
    dplyr::rename(
      "Q5" = "지난.학기.지도교수와.면담을.한.횟수는.어느.정도인지.체크해주세요."
    ) %>%
    dplyr::select("생활적응", "Q5") %>%
    dplyr::mutate(
      type = dplyr::case_when(
        생활적응 <= bot30 ~ "부적응군",
        생활적응 > bot30 & 생활적응 < top30 ~ "적응군",
        생활적응 >= top30 ~ "우수군"
      )
    ) %>%
    dplyr::select(-c("생활적응")) %>%
    na.omit() %>%
    tibble::rowid_to_column()

  selData = data %>%
    dplyr::filter(type == "우수군") %>%
    dplyr::select(-c("type")) %>%
    dplyr::rename(
      "Q5(우수군)" = "Q5"
    )

  dataL1 = data %>%
    dplyr::left_join(selData, by = c("rowid" = "rowid")) %>%
    dplyr::select(-c("type", "rowid")) %>%
    dplyr::mutate_if(is.character, factor) %>%
    as.data.frame()

  plotSubTitle = sprintf("%s (%s)", "Q5 전체 및 우수군 집단에 따른 항목별 점수 분포", nameInfo)
  saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

  # plot(likert::likert(dataL1))
  makePlot = sjPlot::plot_likert(dataL1) +
    labs(subtitle = plotSubTitle) +
    theme(
      text = element_text(size = 16)
      , legend.position = "top"
    )

  ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
}


# ****************************************************************************
# 3번
# ****************************************************************************
colnames(allData)

data = allData %>%
  dplyr::rename(
    "Q7.1" = "이전에.학교중단에.대한.생각을.했으나,.학교를.더.다니게.된.이유는.무엇입니까?"
  ) %>%
  dplyr::select("생활적응", "Q7.1") %>%
  dplyr::mutate(
    type = dplyr::case_when(
      생활적응 <= bot30 ~ "부적응군",
      생활적응 > bot30 & 생활적응 < top30 ~ "적응군",
      생활적응 >= top30 ~ "우수군"
    )
  ) %>%
  dplyr::select(-c("생활적응")) %>%
  dplyr::filter(
    !is.na(Q7.1)
    , Q7.1 != "1학년"
  ) %>%
  dplyr::mutate(
    A1 = ifelse(stringr::str_detect(Q7.1, regex("거리가 가까워서")), 1, 0)
    , A2 = ifelse(stringr::str_detect(Q7.1, regex("학비가 저렴하거나 장학금 혜택 때문에")), 1, 0)
    , A3 = ifelse(stringr::str_detect(Q7.1, regex("전공에 비전이 있다고 생각해서")), 1, 0)
    , A4 = ifelse(stringr::str_detect(Q7.1, regex("동기 및 선후배들이 좋아서")), 1, 0)
    , A5 = ifelse(stringr::str_detect(Q7.1, regex("대안이 없어서")), 1, 0)
    , A6 = ifelse(stringr::str_detect(Q7.1, regex("기타")), 1, 0)
  ) %>%
  tibble::rowid_to_column()

dataL1 = data %>%
  dplyr::select(A1, A2, A3, A4, A5)

modelRes = mirt::mirt(dataL1, model = 1, itemtype = "Rasch")
summary(modelRes)

ggmirt::itempersonMap(modelRes)

plotSubTitle = sprintf("%s (%s)", "Q7.1 전체에 따른 항목별 점수 분포", nameInfo)
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

makePlot = itemPersonMap(modelRes) +
  labs(subtitle = plotSubTitle) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
  )

ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# plot(modelRes)

# dataL2 = data %>%
#   dplyr::mutate(
#     ANS7.1 = dplyr::case_when(
#       stringr::str_detect(Q7.1, regex("거리가 가까워서")) ~ 1
#       , stringr::str_detect(Q7.1, regex("학비가 저렴하거나 장학금 혜택 때문에")) ~ 2
#       , stringr::str_detect(Q7.1, regex("전공에 비전이 있다고 생각해서")) ~ 3
#       , stringr::str_detect(Q7.1, regex("동기 및 선후배들이 좋아서")) ~ 4
#       , stringr::str_detect(Q7.1, regex("대안이 없어서")) ~ 5
#       , stringr::str_detect(Q7.1, regex("기타")) ~ 6
#     )
#   ) %>%
#   tidyr::gather(-rowid, -Q7.1, -ANS7.1, -type, key = "key", value = "val")



# 설문 조사 응답이 다원적이기 때문에 항목 임계값을 추정하기 위해 PCM이 선택됩니다. technical = list(removeEmptyRows=TRUE)설문 조사의 모든 항목을 건너뛴 응답자가 79명이므로 추가합니다 .
# 따라서 모델을 추정하기 전에 제거해야 합니다.
# 모델 추정 결과를 "mod"로 저장하고 있습니다.
# 모델이 제대로 수렴되면 항목-사람 맵을 만드는 데 필요한 모든 정보가 이 개체에 저장됩니다
# mirt. itempersonmap(mod)추정이 완료되면 항목-사람 맵을 만드는 데 사용할 수 있습니다 .
# 도표에서 빨간색 점은 각 항목에 대한 항목 임계값(4개의 응답 범주를 구분하는 3개의 임계값)을 나타내고
# 별표는 각 항목에 대한 임계값의 평균값을 나타냅니다. 이 임계값이 높을수록 더 많은 "팀워크"가 필요합니다. 플롯의 왼쪽에는 잠재 특성(즉, 팀워크 구성)의 분포도 표시됩니다.


# 1. 거리가 가까워서
# 2. 학비가 저렴하거나 장학금 혜택 때문에
# 3. 전공에 비전이 있다고 생각해서
# 4. 동기 및 선후배들이 좋아서
# 5. 별대른 대안이 없어서
# 6. 기타

# plotSubTitle = sprintf("%s (%s)", "Q7.1 전체 및 우수군 집단에 따른 항목별 점수 분포", nameInfo)
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
#
# makePlot = sjPlot::plot_likert(dataL1) +
#   labs(subtitle = plotSubTitle) +
#   theme(
#     text = element_text(size = 16)
#     , legend.position = "top"
#   )
#
# ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ****************************************************************************
# 4번
# ****************************************************************************
colnames(allData)

data = allData %>%
  dplyr::select("성격2", "위기특성5", "위기특성6", "적응특성1", "적응특성2") %>%
  dplyr::mutate(
    type = dplyr::case_when(
      성격2 < 35 ~ "낮음",
      성격2 >= 35 & 성격2 < 65 ~ "보통",
      성격2 >= 65 ~ "높음"
    )
  ) %>%
  tidyr::gather(-성격2, -type, key = "key", value = "val") %>%
  dplyr::select(-성격2) %>%
  dplyr::group_by(type, key) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  )

data$type = forcats::fct_relevel(data$type, c("낮음", "보통", "높음"))
data$key = forcats::fct_relevel(data$key, c("낮음", "보통", "높음"))

keyList = c("위기특성", "적응특성")
for (keyInfo in keyList) {

  dataL1 = data %>%
    dplyr::filter(stringr::str_detect(key, regex(keyInfo)))

  plotSubTitle = sprintf("성격2 요인 집단에 따른 %s의 점수 분포", keyInfo)
  saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

  makePlot = ggplot(dataL1, aes(x = key, y = meanVal, fill = key)) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_text(aes(label = round(meanVal, 1)), vjust = 1.6, color = "white", size = 5) +
    labs(x = NULL, y = "평균 점수", fill = NULL, subtitle = plotSubTitle) +
    theme(
      text = element_text(size = 16)
      , axis.text.x = element_text(angle = 45, hjust = 1)
      , legend.position = "top"
    ) +
    facet_wrap(~type, scale = "free_x")

  ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
}

# ****************************************************************************
# 5번
# ****************************************************************************
colnames(subData)

data = allData %>%
  dplyr::rename(
    "Q12" = "학업적,.심리적으로.어려운.일을.겪었을.때.아래.교내.기관.중.자발적으로.이용해.본.적이.있는.기관을.모두.체크해주세요."
  ) %>%
  dplyr::select("위기군여부", "생활적응", "심리적응", "Q12") %>%
  dplyr::mutate(
    type = dplyr::case_when(
      생활적응 <= bot30 ~ "부적응군",
      생활적응 > bot30 & 생활적응 < top30 ~ "적응군",
      생활적응 >= top30 ~ "우수군"
    )
    , type2 = ifelse(위기군여부 > 0, "위기군", NA)
  ) %>%
  dplyr::mutate(
    type3 = dplyr::case_when(
      type == "부적응군" & type2 == "위기군" ~ "부적응군-위기군",
      type == "부적응군" ~ "부적응군",
      type2 == "위기군" ~ "위기군"
    )
  ) %>%
  dplyr::filter(
    type == "부적응군" | type2 == "위기군"
  ) %>%
  tibble::rowid_to_column()

dataL1 = data %>%
  dplyr::mutate(
    부적응군 = ifelse(stringr::str_detect(type3, regex("부적응군")), 1, 0)
    , 위기군 = ifelse(stringr::str_detect(type3, regex("위기군")), 1, 0)
    , `부적응군-위기군` = ifelse(stringr::str_detect(type3, regex("부적응군-위기군")), 1, 0)
  ) %>%
  dplyr::select(rowid, 부적응군, 위기군, `부적응군-위기군`, 생활적응, 심리적응) %>%
  as.data.frame()

# UpSetR::upset(
#   dataL1
#   , sets.bar.color = "grey"
#   , attribute.plots = list(
#     gridrows = 60
#     , ncols = 2
#     , plots = list(
#       list(plot = scatter_plot, x = "rowid", y = "생활적응")
#       , list(plot = scatter_plot, x = "rowid", y = "심리적응")
#     )
#   )
#   , sets = c("부적응군", "부적응군-위기군", "위기군")
#   , queries = list(
#     list(query = intersects, params = list("위기군"), active = TRUE)
#     , list(query = intersects, params = list("부적응군"), active = TRUE)
#   )
# )

plotSubTitle = sprintf("%s", "Q12 부적응군 및 위기군 집단에 따른 생활적응 및 심리적응 분포")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

size = get_size_mode('exclusive_intersection')
makePlot = ComplexUpset::upset(
  dataL1
  , c("부적응군", "부적응군-위기군", "위기군")
  , name = "집단"
  , width_ratio = 0.3
  , annotations = list(
    '생활적응' = (
      ggplot(mapping = aes(y = 생활적응)) +
        geom_jitter(aes(color = 생활적응), na.rm = TRUE) +
        geom_violin(alpha = 0.5, na.rm = TRUE)
    )
    , '심리적응' = (
      ggplot(mapping = aes(y = 심리적응)) +
        geom_jitter(aes(color = 심리적응), na.rm = TRUE) +
        geom_violin(alpha = 0.5, na.rm = TRUE)
    )
  )
) +
  labs(subtitle = plotSubTitle) +
  theme(
    text = element_text(size = 12)
    , legend.position = "top"
  )

ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# print(10,)
# 정성적 데이터, 정량적 데이터의 조합과 산포도 등을 동시에 플롯하여 데이터 세트의 특징을 탐색적으로 파악하는 데 편리한 "UpSetR" 패키지입니다.
# 조금, 사용법에 버릇이 있습니다만 「사용 데이터 예를 최소 구성」에 출력예를 소개합니다.


#
# # *****************************************
# # 시각화
# # *****************************************
# plotSubTitle = sprintf("%s", "제품전후 전력량에 따른 비교추이")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
# makePlot = ggpubr::ggscatter(
#   data = dataL1, x = "id", y = "val", color = "key"
#   , add = "reg.line", alpha = 0.3, palette = c("#00AFBB", "#E7B800")
# ) +
#   labs(title = NULL, x = "인덱스", y = "전력 사용량", color = NULL, subtitle = plotSubTitle) +
#   theme_bw() +
#   ggpubr::stat_regline_equation(aes(color = key), label.x.npc = 0.0, label.y.npc = 0.95, size = 6) +
#   ggpubr::stat_cor(aes(color = key), label.x.npc = 0.5, label.y.npc = 0.95, p.accuracy = 0.01, r.accuracy = 0.01, size = 6) +
#   theme(
#     text = element_text(size = 18)
#     , legend.position = "top"
#   )
#
# ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# ggplot2::last_plot()
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#
# plotSubTitle = sprintf("%s", "제품전후 전력량에 따른 밀도함수")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
# makePlot = ggpubr::ggdensity(
#   data = dataL1, x = "val", add = "mean", rug = TRUE,
#   color = "key", fill = "key", palette = c("#00AFBB", "#E7B800")
#   ) +
#   labs(title = NULL, x = "전력 사용량", y = "밀도함수", color = NULL, subtitle = plotSubTitle) +
#   theme(
#     text = element_text(size = 18)
#     , legend.position = "top"
#   )
#
# ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# ggplot2::last_plot()
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#
#
# plotSubTitle = sprintf("%s", "제품전후 전력량에 따른 빈도분포")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
# makePlot = ggpubr::gghistogram(
#   data = dataL1, x = "val", add = "mean", rug = TRUE,
#   color = "key", fill = "key", palette = c("#00AFBB", "#E7B800")
# ) +
#   labs(title = NULL, x = "전력 사용량", y = "밀도함수", color = NULL, subtitle = plotSubTitle) +
#   theme(
#     text = element_text(size = 18)
#     , legend.position = "top"
#   )
#
# ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# ggplot2::last_plot()
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#
#
# plotSubTitle = sprintf("%s", "제품전후 전력량에 따른 상자그림")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
# makePlot = ggpubr::ggboxplot(
#   dataL1, x = "key", y = "val", color = "key", palette =c("#00AFBB", "#E7B800"),
#   add = "jitter", shape = "key", alpah = 0.1
# ) +
#   ggpubr::stat_compare_means(comparisons = list( c("before", "after"))) +
#   labs(title = NULL, x = "전력 사용량", y = "밀도함수", color = NULL, subtitle = plotSubTitle) +
#   theme(
#     text = element_text(size = 18)
#     , legend.position = "top"
#   )
#
# ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# ggplot2::last_plot()
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#
#
# # *****************************************
# # 통계 검정
# # *****************************************
# # F 검정
# fTest = var.test(val ~ key, data = dataL1, conf.level = 0.95)
# print(fTest)
#
# # F 검정에서 유의수준 p-value < 0.05 이하로서 귀무가설이 기각 (두 그룹은 분산 차이)
# mainTitle = sprintf("%s", "제품전후 간의 F 검정")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
# plot(fTest) +
#   ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)
#
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#
# # T 검정
# # 등분산 가정 O
# # tTest = t.test(val ~ key, data = dataL1, conf.level = 0.95, var.equal = TRUE, paired = FALSE)
#
# # 등분산 가정 X
# tTest = t.test(val ~ key, data = dataL1, conf.level = 0.95, var.equal = FALSE, paired = FALSE)
#
# # T 검정에서 유의수준 p-value는 0.01 이하로서 귀무가설 기각 (두 그룹은 평균 차이)
# print(tTest)
#
# mainTitle = sprintf("%s", "제품전후 간의 T 검정")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#
# plot(tTest) +
#   ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)
#
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")