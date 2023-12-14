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
library(openxlsx)
library(forcats)
library(extrafont)
library(magick)
library(webshot)
library(webshot2)

# 파일 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "LSH0529. R을 이용한 학위 논문 표 및 그림 시각화.xlsx"))

# ================================================
# 그림 생성
# ================================================
figData = openxlsx::read.xlsx(fileInfo, sheet = "그림") %>% 
  as.tibble()

# figData$name %>% unique()
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
  tidyr::pivot_longer(cols = c("Heating", "Cooling"), names_to = "key", values_to = "val") %>% 
  dplyr::mutate(
    name2 = dplyr::case_when(
      name2 == "바닥" ~ "FLOOR"
      , name2 == "외벽" ~ "EXTERIOR WALL"
      , name2 == "지붕" ~ "ROOF"
      , name2 == "창면적비" ~ "WINDOW AREA RATIO"
      , name2 == "창호" ~ "WINDOW"
      , TRUE ~ NA_character_
    )
  )

legList = figDataL1$leg %>% unique() %>% sort(decreasing = TRUE)

# 정렬
figDataL1$key = forcats::fct_relevel(figDataL1$key, c("Heating", "Cooling"))
figDataL1$leg = forcats::fct_relevel(figDataL1$leg, legList)

summary(figDataL1)

mainTitle = sprintf("%s", "열관류율의 냉난방에너지 영향정도")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(figDataL1, aes(x = leg, y = val, color = key, group = key, label = round(val, 2))) + 
  geom_line() +
  geom_point() +
  labs(x = NULL, y = "Building Performance", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  scale_y_continuous(minor_breaks = seq(0, 50, 5), breaks=seq(0, 50, 5), limits = c(20, 40)) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
    , axis.text.x = element_text(angle = 45, hjust = 1, size = 11)
  ) +
  facet_wrap(~ name2, scale = "free_x", ncol = 2) + 
  ggsave(filename = saveImg, width = 8, height = 10, dpi = 600)

# shell.exec(saveImg)
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

summary(figDataL1)

mainTitle = sprintf("%s", "난방 총 용량의 건물에너지성능 영향정도")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(figDataL1, aes(x = leg, y = val, color = key, group = key, label = round(val, 2))) + 
  geom_line() +
  geom_point() +
  labs(x = NULL, y = "Building Performance", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  scale_y_continuous(minor_breaks = seq(0, 50, 5), breaks=seq(0, 50, 5), limits = c(30, 50)) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
    , axis.text.x = element_text(angle = 45, hjust = 1, size = 14)
  ) +
  facet_wrap(~ name2, scale = "free_x", ncol = 2) + 
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# 냉난방설비 종류의 건물에너지성능 영향정도
figDataL1 = figData %>% 
  dplyr::filter(name == "냉난방설비 종류의 건물에너지성능 영향정도") %>% 
  dplyr::mutate(
    key2 = ifelse(is.na(key2), "", key2)
    # , leg = sprintf("%s\n%s", key2, key)
    # , leg = sprintf("%s (%s)", key2, key)
    , leg = sprintf("%s", key)
  ) %>% 
  dplyr::select(-c(name, key, key2)) %>%
  tidyr::pivot_longer(cols = c("Heating", "Cooling", "Hot.Water", "Lighting", "Ventilation", "PEP", "PEC", "IR"), names_to = "key", values_to = "val")

# 정렬
figDataL1$key = forcats::fct_relevel(figDataL1$key, c("Heating", "Cooling", "Hot.Water", "Lighting", "Ventilation", "PEP", "PEC", "IR"))
figDataL1$leg = forcats::fct_relevel(figDataL1$leg, c("ST.", "EHP", "GHP", "Geo"))

# summary(figDataL1)

mainTitle = sprintf("%s", "냉난방설비 종류의 건물에너지성능 영향정도")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(figDataL1, aes(x = key, y = val, fill = leg, group = leg, label = round(val, 2))) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
  labs(x = NULL, y = "Building Performance", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  scale_y_continuous(minor_breaks = seq(0, 300, 50), breaks=seq(0, 300, 50), limits = c(0, 250)) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
  ) +
  facet_wrap(~ name2, scale = "free_x", ncol = 2) + 
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 신재생에너지 종류의 건물에너지성능 영향정도
figDataL1 = figData %>% 
  dplyr::filter(name == "신재생에너지 종류의 건물에너지성능 영향정도") %>% 
  dplyr::mutate(
    key2 = ifelse(is.na(key2), "", key2)
    # , leg = sprintf("%s\n%s", key2, key)
    # , leg = sprintf("%s (%s)", key2, key)
    , leg = sprintf("%s", key)
  ) %>% 
  dplyr::select(-c(name, key, key2)) %>%
  tidyr::pivot_longer(cols = c("Heating", "Cooling", "Hot.Water", "Lighting", "Ventilation", "PEP", "PEC", "IR"), names_to = "key", values_to = "val")

# 정렬
figDataL1$key = forcats::fct_relevel(figDataL1$key, c("Heating", "Cooling", "Hot.Water", "Lighting", "Ventilation", "PEP", "PEC", "IR"))
figDataL1$leg = forcats::fct_relevel(figDataL1$leg, c("ST.", "PV", "Geo"))

# summary(figDataL1)

mainTitle = sprintf("%s", "신재생에너지 종류의 건물에너지성능 영향정도")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(figDataL1, aes(x = key, y = val, fill = leg, group = leg, label = round(val, 2))) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
  labs(x = NULL, y = "Building Performance", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  scale_y_continuous(minor_breaks = seq(0, 300, 50), breaks=seq(0, 300, 50), limits = c(0, 250)) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
  ) +
  facet_wrap(~ name2, scale = "free_x", ncol = 2) + 
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 태양광 효율의 건물에너지성능 영향정도
figDataL1 = figData %>% 
  dplyr::filter(name == "태양광 효율의 건물에너지성능 영향정도") %>% 
  dplyr::mutate(
    key2 = ifelse(is.na(key2), "", key2)
    # , leg = sprintf("%s\n%s", key2, key)
    , leg = sprintf("%s (%s)", key2, key)
    # , leg = sprintf("%s", key)
  ) %>% 
  dplyr::select(-c(name, key, key2)) %>%
  tidyr::pivot_longer(cols = c("Heating", "Cooling", "Hot.Water", "Lighting", "Ventilation", "PEP", "PEC", "IR"), names_to = "key", values_to = "val")

# 정렬
legList = figDataL1$leg %>% unique() %>% sort(decreasing = FALSE)

figDataL1$key = forcats::fct_relevel(figDataL1$key, c("Heating", "Cooling", "Hot.Water", "Lighting", "Ventilation", "PEP", "PEC", "IR"))
figDataL1$leg = forcats::fct_relevel(figDataL1$leg, legList)

# summary(figDataL1)

mainTitle = sprintf("%s", "태양광 효율의 건물에너지성능 영향정도")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(figDataL1, aes(x = key, y = val, fill = leg, group = leg, label = round(val, 2))) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
  labs(x = NULL, y = "Building Performance", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  scale_y_continuous(minor_breaks = seq(0, 300, 50), breaks=seq(0, 300, 50), limits = c(0, 250)) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
    , legend.key.size = unit(0.5, "cm")
    # , axis.text.x = element_text(angle = 45, hjust = 1, size = 14)
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  facet_wrap(~ name2, scale = "free_x", ncol = 2) + 
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# EHP의 난방 COP값의 건물에너지성능 영향정도
figDataL1 = figData %>% 
  dplyr::filter(name == "EHP의 난방 COP값의 건물에너지성능 영향정도") %>% 
  dplyr::mutate(
    key2 = ifelse(is.na(key2), "", key2)
    # , leg = sprintf("%s\n%s", key2, key)
    , leg = sprintf("%s (%s)", key2, key) %>% stringr::str_trim(side = "both")
    # , leg = sprintf("%s", key)
  ) %>% 
  dplyr::select(-c(name, key, key2)) %>%
  tidyr::pivot_longer(cols = c("Heating", "Cooling", "Hot.Water", "Lighting", "Ventilation", "PEP", "PEC", "IR"), names_to = "key", values_to = "val")

# 정렬
# legList = figDataL1$leg %>% unique() %>% sort(decreasing = TRUE)
legList = c("5.25 (H-COP 1)", "4.9 (H-COP 2)",   "4.55 (H-COP 3)", "4.2 (H-COP 4)", "3.85 (H-COP 5)", "(ST.)", "3.15 (H-COP 6)", "2.8 (H-COP 7)", "2.45 (H-COP 8)", "2.1 (H-COP 9)", "1.75 (H-COP 10)")

figDataL1$key = forcats::fct_relevel(figDataL1$key, c("Heating", "Cooling", "Hot.Water", "Lighting", "Ventilation", "PEP", "PEC", "IR"))
figDataL1$leg = forcats::fct_relevel(figDataL1$leg, legList)

# summary(figDataL1)

mainTitle = sprintf("%s", "EHP의 난방 COP값의 건물에너지성능 영향정도")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(figDataL1, aes(x = key, y = val, fill = leg, group = leg, label = round(val, 2))) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
  labs(x = NULL, y = "Building Performance", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  scale_y_continuous(minor_breaks = seq(0, 300, 50), breaks=seq(0, 300, 50), limits = c(0, 250)) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
    # , legend.key.size = unit(0.4, "cm")
    # , axis.text.x = element_text(angle = 45, hjust = 1, size = 14)
  ) +
  guides(fill = guide_legend(nrow = 3)) +
  facet_wrap(~ name2, scale = "free_x", ncol = 2) + 
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 전열교환기 난방효율의 건물에너지성능 영향정도
figDataL1 = figData %>% 
  dplyr::filter(name == "전열교환기 난방효율의 건물에너지성능 영향정도") %>% 
  dplyr::mutate(
    key2 = ifelse(is.na(key2), "", key2)
    # , leg = sprintf("%s\n%s", key2, key)
    , leg = sprintf("%s (%s)", key2, key) %>% stringr::str_trim(side = "both")
    # , leg = sprintf("%s", key)
  ) %>% 
  dplyr::select(-c(name, key, key2)) %>%
  tidyr::pivot_longer(cols = c("Heating", "Cooling", "Hot.Water", "Lighting", "Ventilation", "PEP", "PEC", "IR"), names_to = "key", values_to = "val")

# 정렬
# legList = figDataL1$leg %>% unique() %>% sort(decreasing = FALSE)
legList = c("60 (H-EX-1)", "(ST.)", "65 (H-EX-2)", "70 (H-EX-3)", "75 (H-EX-4)", "80 (H-EX-5)")

figDataL1$key = forcats::fct_relevel(figDataL1$key, c("Heating", "Cooling", "Hot.Water", "Lighting", "Ventilation", "PEP", "PEC", "IR"))
figDataL1$leg = forcats::fct_relevel(figDataL1$leg, legList)

# summary(figDataL1)

mainTitle = sprintf("%s", "전열교환기 난방효율의 건물에너지성능 영향정도")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(figDataL1, aes(x = key, y = val, fill = leg, group = leg, label = round(val, 2))) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
  labs(x = NULL, y = "Building Performance", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  scale_y_continuous(minor_breaks = seq(0, 300, 50), breaks=seq(0, 300, 50), limits = c(0, 250)) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
    , legend.key.size = unit(0.4, "cm")
    # , axis.text.x = element_text(angle = 45, hjust = 1, size = 14)
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  facet_wrap(~ name2, scale = "free_x", ncol = 2) + 
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ================================================
# 표 생성
# ================================================
tabData = openxlsx::read.xlsx(fileInfo, sheet = "표") %>% 
  as.tibble()

colnames(tabData)

nameList = tabData$name %>% unique() %>% sort(decreasing = TRUE)
nameInfo = "제로에너지건축물 인증 5등급을 만족하는 기술요소 조합"
for (nameInfo in nameList) {
  cat(sprintf("[CHECK] nameInfo : %s", nameInfo), "\n")
  
  # 제로에너지건축물 인증 1~5등급을 만족하는 기술요소 조합
  tabDataL1 = tabData %>% 
    dplyr::filter(name == nameInfo) %>% 
    dplyr::select(c(type, id, id2, key2, PV, IR)) %>%
    tidyr::pivot_longer(cols = c("PV", "IR"), names_to = "key", values_to = "val") %>% 
    dplyr::mutate(
      leg = sprintf("(%s) %s", type, key) %>% stringr::str_trim(side = "both")
    )
  
  legList = tabDataL1$leg %>% unique()
  id2List = tabDataL1$id2 %>% unique()
  idList = tabDataL1$id %>% unique()
  
  # 정렬
  tabDataL1$leg = forcats::fct_relevel(tabDataL1$leg, legList)
  tabDataL1$id2 = forcats::fct_relevel(tabDataL1$id2, id2List)
  tabDataL1$id = forcats::fct_relevel(tabDataL1$id, idList)
  
  summary(tabDataL1)
  
  mainTitle = sprintf("%s", nameInfo)
  saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
  dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)
  
  makePlot = ggplot(tabDataL1, aes(x = id2, y = val, color = id, label = round(val, 2))) + 
    geom_line() +
    geom_point() +
    labs(x = NULL, fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
    # scale_y_continuous(minor_breaks = seq(0, 50, 5), breaks=seq(0, 50, 5), limits = c(30, 50)) +
    theme(
      text = element_text(size = 16)
      , legend.position = "top"
      , legend.key.size = unit(0.45, "cm")
      , axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
    ) +
    labs(y = "     IR (%)                              PV Capa. (Kw)") +
    guides(color = guide_legend(nrow = 2)) +
    facet_wrap(~ leg, scale = "free_y", ncol = 1)
  
  ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
  
  # shell.exec(saveImg)
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
  
  # 제로에너지건축물 인증 1~5등급을 만족하는 기술비용 조합 세부
  tabDataL2 =  tabData %>% 
    dplyr::filter(
      name == nameInfo
      , ! is.na(key2)
    ) %>% 
    dplyr::mutate(
      leg = sprintf("(%s) %s", type, id) %>% stringr::str_trim(side = "both")
      , key2Fac = factor(key2, levels = c("패시브, 액티브, 신재생 최소", "패시브 최대, 액티브, 신재생 최소", "패시브, 액티브 최대, 신재생 최소"))
    ) %>% 
    dplyr::arrange(key2Fac) %>%
    tibble::column_to_rownames("leg") %>% 
    dplyr::select(id2, PV, IR)
    # dplyr::select(key2, id2, PV, IR)
  
  subTitle = sprintf("%s 세부", nameInfo)
  saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, subTitle)
  dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)
  tmpImg = tempfile(fileext = ".png")
  
  tabDataL3 = tabDataL2 %>% 
    dplyr::mutate(
      IR = formattable::color_tile("white", "orange")(IR)
      # , key2 = kableExtra::cell_spec(key2, "html", align = "c", color = factor(key2, c("패시브, 액티브, 신재생 최소", "패시브, 액티브 최대, 신재생 최소", "패시브 최대, 액티브, 신재생 최소"), c("red", "green", "blue")))
    ) %>% 
    kableExtra::kbl(escape = FALSE, row.names = TRUE, col.names = c("Desc.", "Comb.", "PV Capa. (kW)", "IR (%)")) %>%
    kableExtra::kable_paper(c("striped"), full_width = FALSE) %>% 
    kableExtra::column_spec(3, color = "white", background = spec_color(tabDataL2$PV, end = 0.7)) %>% 
    kableExtra::save_kable(file = tmpImg, density = 600, zoom = 10)
  
  # shell.exec(tmpImg)
  # file_move(tmpImg, saveImg)
  
  # 이미지 여백 제거
  tmpImg %>% 
    magick::image_read() %>% 
    magick::image_trim() %>%
    magick::image_write(path = saveImg)
  
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
}


nameList = tabData$name2 %>% unique() %>% sort(decreasing = TRUE)
nameInfo = "제로에너지건축물 인증 5등급 기술요소 조합에 대한 패시브 및 신재생 공사비"
for (nameInfo in nameList) {
  cat(sprintf("[CHECK] nameInfo : %s", nameInfo), "\n")
  
  # 제로에너지건축물 인증 1~5등급 기술요소 조합에 대한 패시브 및 신재생 공사비
  tabDataL1 = tabData %>% 
    dplyr::filter(name2 == nameInfo) %>% 
    dplyr::select(c(type, id, id2, key2, passiveCost, renewCost, totalCost)) %>%
    tidyr::pivot_longer(cols = c("passiveCost", "renewCost", "totalCost"), names_to = "key", values_to = "val") %>% 
    dplyr::mutate(
      label = dplyr::case_when(
        # key == "passiveCost" ~ "패시브"
        # , key == "renewCost" ~ "신재생"
        # , key == "totalCost" ~ "전체"
        key == "passiveCost" ~ "PASSIVE"
        , key == "renewCost" ~ "RENEWABLE"
        , key == "totalCost" ~ "TOTAL"
        , TRUE ~ NA_character_
      )
    )
  
  # legList = tabDataL1$leg %>% unique()
  id2List = tabDataL1$id2 %>% unique()
  labelList = tabDataL1$label %>% unique()
  
  # 정렬
  # tabDataL1$leg = forcats::fct_relevel(tabDataL1$leg, legList)
  tabDataL1$id2 = forcats::fct_relevel(tabDataL1$id2, id2List)
  tabDataL1$label = forcats::fct_relevel(tabDataL1$label, labelList)
  
  summary(tabDataL1)
  
  mainTitle = sprintf("%s", nameInfo)
  saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
  dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)
  
  makePlot = ggplot(tabDataL1, aes(x = id2, y = val, color = label, group = label, label = round(val, 2))) + 
    geom_line() +
    geom_point() +
    labs(x = NULL, y = "Cost", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
    # scale_y_continuous(minor_breaks = seq(0, 50, 5), breaks=seq(0, 50, 5), limits = c(30, 50)) +
    theme(
      text = element_text(size = 16)
      , legend.position = "top"
      # , legend.key.size = unit(0.45, "cm")
      , axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
    ) +
    guides(color = guide_legend(nrow = 1)) +
    # facet_wrap(~ label, scale = "free_y", ncol = 1)
    facet_wrap(~ type, scale = "free_y", ncol = 1)
  
  ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)

  # shell.exec(saveImg)
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
  
  
  # 제로에너지건축물 인증 1~5등급 기술요소 조합에 대한 패시브 및 신재생 공사비 세부
  tabDataL2 =  tabData %>% 
    dplyr::filter(
      name2 == nameInfo
      , ! is.na(key2)
    ) %>% 
    dplyr::mutate(
      leg = sprintf("(%s) %s", type, id) %>% stringr::str_trim(side = "both")
      , key2Fac = factor(key2, levels = c("패시브, 액티브, 신재생 최소", "패시브 최대, 액티브, 신재생 최소", "패시브, 액티브 최대, 신재생 최소"))
    ) %>% 
    dplyr::arrange(key2Fac) %>%
    tibble::column_to_rownames("leg") %>% 
    dplyr::select(totalCost)
  
  subTitle = sprintf("%s 세부", nameInfo)
  saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, subTitle)
  dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)
  tmpImg = tempfile(fileext = ".png")
  tmpHtml = tempfile(fileext = ".html")
  
  tabDataL3 = tabDataL2 %>% 
    # dplyr::mutate(
      # totalCost = formattable::color_tile("white", "orange")(totalCost)
        # , key2 = kableExtra::cell_spec(key2, "html", align = "c", color = factor(key2, c("패시브, 액티브, 신재생 최소", "패시브, 액티브 최대, 신재생 최소", "패시브 최대, 액티브, 신재생 최소"), c("red", "green", "blue")))
    # ) %>%
    kableExtra::kbl(escape = FALSE, row.names = TRUE, col.names = c("Cost of Passive + Renewable (won)")) %>%
    kableExtra::kable_paper(c("striped"), full_width = FALSE) %>% 
    kableExtra::column_spec(2, color = "white", background = spec_color(tabDataL2$totalCost, end = 0.7)) %>% 
    kableExtra::save_kable(file = tmpHtml, density = 600)

  # file_move(tmpImg, saveImg)
  
  webshot::webshot(tmpHtml, tmpImg, zoom = 10)
  # webshot2::webshot(tmpHtml, tmpImg)
  
  # 이미지 여백 제거
  tmpImg %>% 
    magick::image_read() %>% 
    magick::image_trim() %>%
    magick::image_write(path = saveImg)
  
  # shell.exec(saveImg)
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
}


# 제로에너지건축물 인증 1~5등급 기술요소 조합에 대한 패시브 및 신재생 공사비
tabDataL1 = tabData %>% 
  dplyr::select(c(type, id, id2, key2, passiveCost, renewCost, totalCost)) %>%
  tidyr::pivot_longer(cols = c("passiveCost", "renewCost", "totalCost"), names_to = "key", values_to = "val") %>% 
  dplyr::mutate(
    label = dplyr::case_when(
      # key == "passiveCost" ~ "패시브"
      # , key == "renewCost" ~ "신재생"
      # , key == "totalCost" ~ "전체"
      key == "passiveCost" ~ "PASSIVE"
      , key == "renewCost" ~ "RENEWABLE"
      , key == "totalCost" ~ "TOTAL"
      , TRUE ~ NA_character_
    )
    , type = dplyr::case_when(
      type == "1등급" ~ "CLASS 1"
      , type == "2등급" ~ "CLASS 2"
      , type == "3등급" ~ "CLASS 3"
      , type == "4등급" ~ "CLASS 4"
      , type == "5등급" ~ "CLASS 5"
      , TRUE ~ NA_character_
    )
  )

typeList = tabDataL1$type %>% unique() %>% sort()
id2List = tabDataL1$id2 %>% unique()
labelList = tabDataL1$label %>% unique()

# 정렬
tabDataL1$type = forcats::fct_relevel(tabDataL1$type, typeList)
tabDataL1$id2 = forcats::fct_relevel(tabDataL1$id2, id2List)
tabDataL1$label = forcats::fct_relevel(tabDataL1$label, labelList)

summary(tabDataL1)

mainTitle = sprintf("%s", "제로에너지건축물 인증 1~5등급 기술요소 조합에 대한 패시브 및 신재생 공사비")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)

makePlot = ggplot(tabDataL1, aes(x = id2, y = val, color = type, group = type, label = round(val, 2))) + 
  geom_line() +
  geom_point() +
  labs(x = NULL, y = "Cost", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  # scale_y_continuous(minor_breaks = seq(0, 50, 5), breaks=seq(0, 50, 5), limits = c(30, 50)) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
    # , legend.key.size = unit(0.45, "cm")
    , axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
  ) +
  guides(color = guide_legend(nrow = 1)) +
  # facet_wrap(~ label, scale = "free_y", ncol = 1)
  facet_wrap(~ label, scale = "free_y", ncol = 1)

ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 제로에너지건축물 인증 1~5등급 기술요소 조합에 대한 패시브 및 신재생 공사비 세부
tabDataL2 =  tabData %>% 
  dplyr::filter(
    ! is.na(key2)
  ) %>% 
  dplyr::mutate(
    leg = sprintf("(%s) %s", type, id) %>% stringr::str_trim(side = "both")
    , key2Fac = factor(key2, levels = c("패시브, 액티브, 신재생 최소", "패시브 최대, 액티브, 신재생 최소", "패시브, 액티브 최대, 신재생 최소"))
  ) %>% 
  # dplyr::arrange(key2Fac) %>%
  tibble::column_to_rownames("leg") %>% 
  dplyr::select(totalCost)

subTitle = sprintf("%s 세부", "제로에너지건축물 인증 1~5등급 기술요소 조합에 대한 패시브 및 신재생 공사비 세부")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, subTitle)
dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)
tmpImg = tempfile(fileext = ".png")
tmpHtml = tempfile(fileext = ".html")

tabDataL3 = tabDataL2 %>% 
  # dplyr::mutate(
  # totalCost = formattable::color_tile("white", "orange")(totalCost)
  # , key2 = kableExtra::cell_spec(key2, "html", align = "c", color = factor(key2, c("패시브, 액티브, 신재생 최소", "패시브, 액티브 최대, 신재생 최소", "패시브 최대, 액티브, 신재생 최소"), c("red", "green", "blue")))
  # ) %>%
  kableExtra::kbl(escape = FALSE, row.names = TRUE, col.names = c("Cost of Passive + Renewable (won)")) %>%
  kableExtra::kable_paper(c("striped"), full_width = FALSE) %>% 
  kableExtra::column_spec(2, color = "white", background = spec_color(tabDataL2$totalCost, end = 0.7)) %>% 
  kableExtra::save_kable(file = tmpHtml, density = 600)

# file_move(tmpImg, saveImg)

webshot::webshot(tmpHtml, tmpImg, zoom = 10)
# webshot2::webshot(tmpHtml, tmpImg)

# 이미지 여백 제거
tmpImg %>% 
  magick::image_read() %>% 
  magick::image_trim() %>%
  magick::image_write(path = saveImg)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")