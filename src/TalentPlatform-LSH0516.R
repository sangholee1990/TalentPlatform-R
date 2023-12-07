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
# R을 이용한 설문조사 전문가 및 시민 데이터 시각화 (전문가 데이터, 시민 PDF)

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0516"

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
library(ggradar)
library(sysfonts)

# 국문 폰트 설정
if (Sys.info()["sysname"] == "Windows") {
  sysfonts::font.add(family = "malgun", regular = "C:/Windows/Fonts/malgun.ttf")
  showtext::showtext_opts(dpi = 600)
  showtext::showtext.auto()
}

# 파일 읽기
# fileInfo = "C:/SYSTEMS/PROG/R/TalentPlatform-R/resources/input/test/LSH0516/20231205_★설문+결과_정리_0816.xlsx")
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "20231205_★설문+결과_정리_0816.xlsx"))

data = openxlsx::read.xlsx(fileInfo) %>% 
  as.tibble()

# 시민 및 전문가에 따른 응답률
dataL1 = data %>% 
  dplyr::filter(name == "비율")

mainTitle = sprintf("%s", "시민 및 전문가에 따른 응답률")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL1, aes(x = type, y = val, fill = type)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(val, 2)), nudge_y = -5.0, size = 5, color = "white") +
  facet_wrap(~name, scale = "free_x") +
  labs(x = NULL, y = "응답률", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  theme(text = element_text(size = 16)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# 시민 및 전문가에 따른 공간 하위요소 TOP3
dataL1 = data %>% 
  dplyr::filter(name == "공간 하위요소")

mainTitle = sprintf("%s", "시민 및 전문가에 따른 공간 하위요소 TOP3")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL1, aes(x = type, y = val, fill = key, label = round(val, 2))) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(nudge_y = -1.0, size = 5, color = "white") +
  facet_wrap(~order, scale = "free_x") +
  labs(x = NULL, y = "비율", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  theme(text = element_text(size = 16)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# 시민 및 전문가에 따른 공간 하위요소
dataL1 = data %>% 
  dplyr::filter(name == "공간 하위요소2") %>% 
  dplyr::select(type, key, val) %>%
  tidyr::spread(key = "key", value = "val") %>% 
  dplyr::rename(
    "Group" = "type"
  ) 

mainTitle = sprintf("%s", "시민 및 전문가에 따른 공간 하위요소")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggradar::ggradar(
  dataL1
  , values.radar = c(0, 12, 24)
  , legend.position = "bottom"
  , grid.min = 0, grid.mid = 12, grid.max = 24
  , font.radar = "malgun"
  ) +
  ggsave(filename = saveImg, width = 12, height = 10, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ggplot(dataL1, aes(x = type, y = val, fill = type, label = round(val, 2))) + 
#   geom_bar(position="dodge", stat="identity") +
#   geom_text(nudge_y = -1.0, size = 5, color = "white") +
#   facet_wrap(~key, scale = "free_x") +
#   labs(x = NULL, y = "비율", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
#   theme(text = element_text(size = 16)) +
#   ggsave(filename = saveImg, width = 10, height = 10, dpi = 600)

# 시민 및 전문가에 따른 행태 하위요소
dataL1 = data %>% 
  dplyr::filter(name == "행태 하위요소")

mainTitle = sprintf("%s", "시민 및 전문가에 따른 행태 하위요소")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL1, aes(x = type, y = val, fill = type, label = round(val, 2))) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(nudge_y = -2.0, size = 5, color = "white") +
  facet_wrap(~key, scale = "free_x") +
  labs(x = NULL, y = "비율", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  theme(text = element_text(size = 16)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 응답자에 따른 공간 하위요소
dataL1 = data %>% 
  dplyr::filter(name == "응답자에 따른 공간 하위요소") %>% 
  dplyr::select(type, key, val) %>%
  tidyr::spread(key = "key", value = "val") %>% 
  dplyr::rename(
    "Group" = "type"
  ) 

mainTitle = sprintf("%s", "응답자에 따른 공간 하위요소")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggradar::ggradar(
  dataL1
  , values.radar = c(0, 3.5, 7)
  , legend.position = "bottom"
  , grid.min = 0, grid.mid = 3.5, grid.max = 7
  , font.radar = "malgun"
) +
  ggsave(filename = saveImg, width = 12, height = 10, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 응답자에 따른 행태 하위요소
dataL1 = data %>% 
  dplyr::filter(name == "응답자에 따른 행태 하위요소") %>% 
  dplyr::select(type, key, val) %>%
  tidyr::spread(key = "key", value = "val") %>% 
  dplyr::rename(
    "Group" = "type"
  ) 

mainTitle = sprintf("%s", "응답자에 따른 행태 하위요소")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggradar::ggradar(
  dataL1
  , values.radar = c(0, 1.5, 3)
  , legend.position = "bottom"
  , grid.min = 0, grid.mid = 1.5, grid.max = 3
  , font.radar = "malgun"
) +
  ggsave(filename = saveImg, width = 12, height = 10, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# 응답자에 따른 시민의 성별
dataL1 = data %>% 
  dplyr::filter(name == "응답자에 따른 시민의 성별") %>% 
  dplyr::mutate(
    id = sprintf("%s-%s", key, order)
  ) %>% 
  dplyr::select(id, type, val) %>%
  tidyr::spread(key = "id", value = "val") %>% 
  dplyr::rename(
    "Group" = "type"
  ) 

mainTitle = sprintf("%s", "응답자에 따른 시민의 성별")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggradar::ggradar(
  dataL1
  , values.radar = c(0, 3.5, 3)
  , legend.position = "bottom"
  , grid.min = 0, grid.mid = 3.5, grid.max = 7
  , font.radar = "malgun"
) +
  ggsave(filename = saveImg, width = 12, height = 10, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 응답자에 따른 시민의 연령대
dataL1 = data %>% 
  dplyr::filter(name == "응답자에 따른 시민의 연령대") %>% 
  dplyr::mutate(
    id = sprintf("%s-%s", key, order)
  ) %>% 
  dplyr::select(id, type, val) %>%
  tidyr::spread(key = "id", value = "val") %>% 
  dplyr::rename(
    "Group" = "type"
  ) 

mainTitle = sprintf("%s", "응답자에 따른 시민의 연령대")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggradar::ggradar(
  dataL1
  , values.radar = c(0, 3.5, 3)
  , legend.position = "bottom"
  , grid.min = 0, grid.mid = 3.5, grid.max = 7
  , font.radar = "malgun"
) +
  ggsave(filename = saveImg, width = 12, height = 10, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 응답자에 따른 전문가의 직업
dataL1 = data %>% 
  dplyr::filter(name == "응답자에 따른 전문가의 직업") %>% 
  dplyr::mutate(
    id = sprintf("%s-%s", key, order)
  ) %>% 
  dplyr::select(id, type, val) %>%
  tidyr::spread(key = "id", value = "val") %>% 
  dplyr::rename(
    "Group" = "type"
  ) 

mainTitle = sprintf("%s", "응답자에 따른 전문가의 직업")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggradar::ggradar(
  dataL1
  , values.radar = c(0, 3.5, 3)
  , legend.position = "bottom"
  , grid.min = 0, grid.mid = 3.5, grid.max = 7
  , font.radar = "malgun"
) +
  ggsave(filename = saveImg, width = 12, height = 10, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 응답자에 따른 전문가의 경력
dataL1 = data %>% 
  dplyr::filter(name == "응답자에 따른 전문가의 경력") %>% 
  dplyr::mutate(
    id = sprintf("%s-%s", key, order)
  ) %>% 
  dplyr::select(id, type, val) %>%
  tidyr::spread(key = "id", value = "val") %>% 
  dplyr::rename(
    "Group" = "type"
  ) 

mainTitle = sprintf("%s", "응답자에 따른 전문가의 경력")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggradar::ggradar(
  dataL1
  , values.radar = c(0, 3.5, 3)
  , legend.position = "bottom"
  , grid.min = 0, grid.mid = 3.5, grid.max = 7
  , font.radar = "malgun"
) +
  ggsave(filename = saveImg, width = 12, height = 10, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# 시민 및 전문가에 따른 형태 하위요소 TOP3
dataL1 = data %>% 
  dplyr::filter(name == "시민 및 전문가에 따른 형태 하위요소 TOP3")

mainTitle = sprintf("%s", "시민 및 전문가에 따른 형태 하위요소 TOP3")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL1, aes(x = type, y = val, fill = key, label = round(val, 2))) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(nudge_y = -2.0, size = 5, color = "white") +
  facet_wrap(~order, scale = "free_x") +
  labs(x = NULL, y = "비율", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  theme(text = element_text(size = 16)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 시민에 따른 상위 요소 비율
dataL1 = data %>%
  dplyr::filter(name == "시민에 따른 상위 요소 비율")

mainTitle = sprintf("%s", "시민에 따른 상위 요소 비율")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL1, aes(x = '', y = val, fill = key, label = sprintf("%s\n%s %%", key, round(val, 2)))) +
  geom_bar(stat = 'identity', alpha = 0.6) +
  coord_polar('y', start = 0) +
  ggrepel::geom_label_repel(position = position_stack(vjust = 0.5), color = "white", size = 5, alpha = 0.8, show.legend = FALSE) +
  labs(x = NULL, y = NULL, fill = NULL, title = NULL, subtitle = mainTitle) +
  theme_minimal() +
  theme_classic() +
  theme(
    text = element_text(size = 16)
    , axis.line = element_blank()
    , axis.text = element_blank()
    , axis.ticks = element_blank()
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 시민에 따른 공간 하위요소 비율
dataL1 = data %>%
  dplyr::filter(name == "시민에 따른 공간 하위요소 비율")

mainTitle = sprintf("%s", "시민에 따른 공간 하위요소 비율")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL1, aes(x = '', y = val, fill = key, label = sprintf("%s\n%s %%", key, round(val, 2)) )) +
  geom_bar(stat = 'identity', alpha = 0.6) +
  coord_polar('y', start = 0) +
  ggrepel::geom_label_repel(position = position_stack(vjust = 0.25), color = "white", size = 5, alpha = 0.8, show.legend = FALSE) +
  labs(x = NULL, y = NULL, fill = NULL, title = NULL, subtitle = mainTitle) +
  theme_minimal() +
  theme_classic() +
  theme(
    text = element_text(size = 16)
    , axis.line = element_blank()
    , axis.text = element_blank()
    , axis.ticks = element_blank()
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 시민에 따른 행태 하위요소 비율
dataL1 = data %>%
  dplyr::filter(name == "시민에 따른 행태 하위요소 비율")

mainTitle = sprintf("%s", "시민에 따른 행태 하위요소 비율")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL1, aes(x = '', y = val, fill = key, label = sprintf("%s\n%s %%", key, round(val, 2)) )) +
  geom_bar(stat = 'identity', alpha = 0.6) +
  coord_polar('y', start = 0) +
  ggrepel::geom_label_repel(position = position_stack(vjust = 0.5), color = "white", size = 5, alpha = 0.8, show.legend = FALSE) +
  labs(x = NULL, y = NULL, fill = NULL, title = NULL, subtitle = mainTitle) +
  theme_minimal() +
  theme_classic() +
  theme(
    text = element_text(size = 16)
    , axis.line = element_blank()
    , axis.text = element_blank()
    , axis.ticks = element_blank()
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 전문가에 따른 상위 요소 비율
# dataL1 = data %>% 
#   dplyr::filter(name == "상위 요소 비율")
# 
# mainTitle = sprintf("%s", "전문가에 따른 상위 요소 비율")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# 
# ggplot(dataL1, aes(x = '', y = val, fill = key, label = sprintf("%s\n%s %%", key, round(val, 2)))) +
#   geom_bar(stat = 'identity', alpha = 0.6) +
#   coord_polar('y', start = 0) +
#   ggrepel::geom_label_repel(position = position_stack(vjust = 0.5), color = "white", size = 5, alpha = 0.8, show.legend = FALSE) +
#   labs(x = NULL, y = NULL, fill = NULL, title = NULL, subtitle = mainTitle) +
#   theme_minimal() +
#   theme_classic() +
#   theme(
#     text = element_text(size = 16)
#     , axis.line = element_blank()
#     , axis.text = element_blank()
#     , axis.ticks = element_blank()
#     , legend.position = "top"
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
# 
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 전문가에 따른 공간 하위요소 비율
# dataL1 = data %>% 
#   dplyr::filter(name == "공간 하위요소 비율")
# 
# mainTitle = sprintf("%s", "전문가에 따른 공간 하위요소 비율")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# 
# ggplot(dataL1, aes(x = '', y = val, fill = key, label = sprintf("%s\n%s %%", key, round(val, 2)) )) +
#   geom_bar(stat = 'identity', alpha = 0.6) +
#   coord_polar('y', start = 0) +
#   ggrepel::geom_label_repel(position = position_stack(vjust = 0.25), color = "white", size = 5, alpha = 0.8, show.legend = FALSE) +
#   labs(x = NULL, y = NULL, fill = NULL, title = NULL, subtitle = mainTitle) +
#   theme_minimal() +
#   theme_classic() +
#   theme(
#     text = element_text(size = 16)
#     , axis.line = element_blank()
#     , axis.text = element_blank()
#     , axis.ticks = element_blank()
#     , legend.position = "top"
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
# 
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 전문가에 따른 행태 하위요소 비율
# dataL1 = data %>% 
#   dplyr::filter(name == "행태 하위요소 비율")
# 
# mainTitle = sprintf("%s", "전문가에 따른 행태 하위요소 비율")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# 
# ggplot(dataL1, aes(x = '', y = val, fill = key, label = sprintf("%s\n%s %%", key, round(val, 2)) )) +
#   geom_bar(stat = 'identity', alpha = 0.6) +
#   coord_polar('y', start = 0) +
#   ggrepel::geom_label_repel(position = position_stack(vjust = 0.5), color = "white", size = 5, alpha = 0.8, show.legend = FALSE) +
#   labs(x = NULL, y = NULL, fill = NULL, title = NULL, subtitle = mainTitle) +
#   theme_minimal() +
#   theme_classic() +
#   theme(
#     text = element_text(size = 16)
#     , axis.line = element_blank()
#     , axis.text = element_blank()
#     , axis.ticks = element_blank()
#     , legend.position = "top"
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
# 
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
