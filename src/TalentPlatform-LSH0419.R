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
  na.omit() %>%
  tidyr::separate("Q7.1", c("Q7a", "Q7b", "Q7c", "Q7d"), sep = ", ") %>%
  gather(-type, key = "key", value = "Q7.1") %>%
  dplyr::filter(
    !is.na(Q7.1)
    , Q7.1 != "1학년"
  ) %>%
  dplyr::select(-key) %>%
  tibble::rowid_to_column()

# selData = data %>%
#   dplyr::filter(type == "우수군") %>%
#   dplyr::select(-c("type")) %>%
#   dplyr::rename(
#     "Q7.1(우수군)" = "Q7.1"
#   )
#
# dataL1 = data %>%
#   dplyr::left_join(selData, by = c("rowid" = "rowid")) %>%
#   dplyr::select(-c("type", "rowid")) %>%
#   dplyr::mutate_if(is.character, factor) %>%
#   as.data.frame()

dataL1 = data %>%
  dplyr::mutate(
    ANS7.1 = dplyr::case_when(
      stringr::str_detect(Q7.1, regex("거리가 가까워서")) ~ 1
      , stringr::str_detect(Q7.1, regex("학비가 저렴하거나 장학금 혜택 때문에")) ~ 2
      , stringr::str_detect(Q7.1, regex("전공에 비전이 있다고 생각해서")) ~ 3
      , stringr::str_detect(Q7.1, regex("동기 및 선후배들이 좋아서")) ~ 4
      , stringr::str_detect(Q7.1, regex("대안이 없어서")) ~ 5
      , stringr::str_detect(Q7.1, regex("기타")) ~ 6
    )
  ) %>%
  # dplyr::mutate_if(is.character, factor) %>%
  as.data.frame()

# Estimate the Partial Credit Model
mod <- mirt(dataL1$ANS7.1, 1, itemtype = "Rasch",
            technical = list(removeEmptyRows = TRUE),
            verbose = FALSE)

# We set the seed to reproduce the same results
library(RColorBrewer)
library(WrightMap)

set.seed(2020)
rasch.sim.thetas <- rnorm(1000)
rasch.sim.thresholds <- runif(10, -3, 3)
wrightMap( rasch.sim.thetas, rasch.sim.thresholds)

# Create the item-person map
itempersonmap(mod)


# 1. 거리가 가까워서
# 2. 학비가 저렴하거나 장학금 혜택 때문에
# 3. 전공에 비전이 있다고 생각해서
# 4. 동기 및 선후배들이 좋아서
# 5. 별대른 대안이 없어서
# 6. 기타

plotSubTitle = sprintf("%s (%s)", "Q7.1 전체 및 우수군 집단에 따른 항목별 점수 분포", nameInfo)
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

makePlot = sjPlot::plot_likert(dataL1) +
  labs(subtitle = plotSubTitle) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
  )

ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

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

plotSubTitle = sprintf("%s", "성격2 요인 집단에 따른 위기특성 및 적응특성의 점수 분포")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

makePlot = ggplot(data, aes(x = key, y = meanVal, fill = key)) +
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
    , type2 = ifel
  ) %>%
  dplyr::filter(
    !is.na(생활적응)
    , !is.na(심리적응)
    , !is.na(Q12)
  )

dataL1 = data %>%
  tidyr::gather(-type, -Q12, key = "key", value = "val") %>%
  dplyr::group_by(type, key, Q12) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  ) %>%
  dplyr::mutate_if(is.character, factor)

dataL1$type = forcats::fct_relevel(dataL1$type, c("부적응군", "우수군", "적응군"))
# data$key = forcats::fct_relevel(data$key)
# data$key = forcats::fct_relevel(data$key, c("낮음", "보통", "높음"))


# 1. 거리가 가까워서
# 2. 학비가 저렴하거나 장학금 혜택 때문에
# 3. 전공에 비전이 있다고 생각해서
# 4. 동기 및 선후배들이 좋아서
# 5. 별대른 대안이 없어서
# 6. 기타


dataL2 = data %>%
  dplyr::select(-생활적응, -심리적응) %>%
  tibble::rowid_to_column()

selData = dataL2 %>%
  dplyr::filter(type == "부적응군") %>%
  dplyr::select(-c("type")) %>%
  dplyr::rename(
    "Q12(부적응군)" = "Q12"
  )

selData2 = dataL2 %>%
  dplyr::filter(type == "위기군") %>%
  dplyr::select(-c("type")) %>%
  dplyr::rename(
    "Q12(위기군)" = "Q12"
  )

dataL3 = dataL2 %>%
  dplyr::left_join(selData, by = c("rowid" = "rowid")) %>%
  dplyr::left_join(selData2, by = c("rowid" = "rowid")) %>%
  dplyr::select(-c("type", "rowid")) %>%
  dplyr::mutate_if(is.character, factor) %>%
  as.data.frame()

plotSubTitle = sprintf("%s", "Q12 전체와 부적응군 및 위기군 집단에 따른 항목별 점수 분포")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

makePlot = sjPlot::plot_likert(dataL3) +
  labs(subtitle = plotSubTitle) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
  )

ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


plotSubTitle = sprintf("%s", "Q12 및 집단에 따른 생활적응 및 심리적응의 점수 분포")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)

makePlot = ggplot(dataL1, aes(x = key, y = meanVal, fill = key)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = round(meanVal, 1)), vjust = 1.6, color = "white", size = 4) +
  labs(x = NULL, y = "평균 점수", fill = NULL, subtitle = plotSubTitle) +
  theme(
    text = element_text(size = 12)
    , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  facet_grid(Q12 ~ type)

ggsave(makePlot, filename = saveImg, width = 8, height = 10, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# names(dataL1)
# rownames(dataL1)
# dataL1 = data %>%
#   dplyr::mutate_if(is.character, factor) %>%
#   as.data.frame()
# # dplyr::select("Q12")
#
# gg_miss_var(dataL1)
# gg_miss_upset(dataL1)
#
# gg_miss_var(airquality, facet = Month)
#
#
# library(ShinyItemAnalysis)


# summary(dataL1)
# # gg_miss_upset(dataL1, nsets = 10)
# naniar::gg_miss_upset(dataL1)
# naniar::gg_miss_upset(data)
# gg_miss_upset(airquality)
# gg_miss_var(airquality)

# gg_miss_upset(airquality, nsets = 10)
# gg_miss_upset(airquality, nsets = 10, nintersects = 10)
# gg_miss_upset(riskfactors)
# gg_miss_upset(riskfactors, nsets = 10)
# gg_miss_upset(riskfactors, nsets = 10, nintersects = 10)

items <- dd %>%
  # Change responses from 1-2-3-4 to 0-1-2-3 by subtracting 1
  apply(., 2, function(x) x - 1)

# Estimate the Partial Credit Model
mod <- mirt(items, 1, itemtype = "Rasch",
            technical = list(removeEmptyRows = TRUE),
            verbose = FALSE)


# Create the item-person map
itempersonmap(mod)

data <- sim_irt(500, 8, seed = 123)
mod <- mirt(data, 1, itemtype = "2PL", verbose = FALSE)
itempersonMap(mod)


# tidyr::gather(-성격2, -type, key = "key", value = "val") %>%
# dplyr::select(-성격2) %>%
# dplyr::group_by(type, key) %>%
# dplyr::summarise(
#   meanVal = mean(val, na.rm = TRUE)
# )

# data$type = forcats::fct_relevel(data$type, c("낮음", "보통", "높음"))
# data$key = forcats::fct_relevel(data$key, c("낮음", "보통", "높음"))
#
# ggplot(data, aes(x = key, y = meanVal, fill = key)) +
#   geom_bar(position = "dodge", stat = "identity") +
#   geom_text(aes(label = round(meanVal, 0)), vjust = 1.6, color = "white", size = 5) +
#   labs(x = "법정동", y = "연소득당 거래금액", fill = NULL, subtitle = "법정동에 따른 연소득당 거래금액 히스토그램") +
#   # scale_fill_gradientn(colours = cbMatlab, na.value = NA) +
#   theme(
#     text = element_text(size = 18)
#     , axis.text.x = element_text(angle = 45, hjust = 1)
#   ) +
#   facet_wrap(~type, scale = "free_x")
# # ggsave(filename = saveImg, width = 12, height = 8
#


#
# dplyr::select(-c("생활적응")) %>%
# na.omit() %>%
# tidyr::separate("Q7.1", c("Q7a", "Q7b", "Q7c", "Q7d"), sep = ", ") %>%
# gather(-type, key = "key", value = "Q7.1") %>%
# dplyr::filter(
#   !is.na(Q7.1)
#   , Q7.1 != "1학년"
# ) %>%
# dplyr::select(-key) %>%
# tibble::rowid_to_column()

dd
names(dd)
rownames(dd)

selData = data %>%
  dplyr::filter(type == "우수군") %>%
  dplyr::select(-c("type")) %>%
  dplyr::rename(
    "Q7.1(우수군)" = "Q7.1"
  )

names(dd)
rownames(dd)

dataL1 = data %>%
  dplyr::left_join(selData, by = c("rowid" = "rowid")) %>%
  dplyr::select(-c("type", "rowid")) %>%
  dplyr::mutate_if(is.character, factor) %>%
  as.data.frame()

# data = allData %>%
#   dplyr::select("생활적응", "특성11", "이전에.학교중단에.대한.생각을.했으나,.학교를.더.다니게.된.이유는.무엇입니까?") %>%
#   na.omit()
#
# dataL1 = data %>% dplyr::mutate(
#   type = dplyr::case_when(
#     생활적응 <= bot30 ~ "부적응군",
#     생활적응 > bot30 & 생활적응 < top30 ~ "적응군",
#     생활적응 >= top30 ~ "우수군"
#   )
# )
#
# # dataL1$type = forcats::fct_relevel(dataL1$type, c("부적응군", "우수군", "적응군"))
# # dataL1$type = forcats::fct_relevel(dataL1$type, c("부적응군", "우수군", "적응군"))
#
# dataL2 = dataL1 %>%
#   dplyr::mutate_if(is.character, factor) %>%
#   as.data.frame()
#
# likert(dataL2[, 3:3])
#
# plot(likert(dataL2[, 3:4]))
#
# # dataL1[,1:2]
# plot(likert(dataL1[, 2:3]), ordered = F, wrap = 60)
# plot(likert(dataL1[, 3:3]))
#
# likert(dataL1[, 3:3])
#
#
# dataL1$type = forcats::fct_relevel(dataL1$type, c("부적응군", "우수군", "적응군"))
# dataL1$key = forcats::fct_relevel(dataL1$key, c("낮음", "보통", "높음"))
# dataL1$key2 = forcats::fct_relevel(dataL1$key2, c("낮음", "보통", "높음"))


#
# dataL1 = data %>%
#   as.tibble() %>%
#   dplyr::filter(
#     ! is.na(before)
#     , ! is.na(after)
#   ) %>%
#   dplyr::mutate(
#     id = dplyr::row_number()
#   ) %>%
#   gather(-id, key = "key", value = "val")
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