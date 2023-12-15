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
# R을 이용한 성별에 따른 성관계 시작 연령과 학년에 따른 성관계 경험률 시각화

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0532"

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
library(forcats)

# ================================================
# 성관계 경험률 파일 읽기
# ================================================
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "성관계_경험률_2023.csv"))

sxlExrtData = readr::read_csv(fileInfo, locale = locale("ko", encoding = "EUC-KR"))

sxlExrtDataL1 = sxlExrtData %>% 
  dplyr::filter(응답자특성별 %in% c("학년별")) %>% 
  dplyr::select(응답자특성별2, 남학생경험률, 여학생경험률) %>% 
  tidyr::gather(-응답자특성별2, key = "key", value = "val") %>% 
  dplyr::mutate(
    leg = dplyr::case_when(
      stringr::str_detect(key, regex("남학생경험률")) ~ "남성"
      , stringr::str_detect(key, regex("여학생경험률")) ~ "여성"
    )
  )

# 정렬
sxlExrtDataL1$응답자특성별2= forcats::fct_relevel(sxlExrtDataL1$응답자특성별2, c("중1", "중2", "중3", "고1", "고2", "고3"))

# 학년 및 성별에 따른 성관계 경험률
mainTitle = sprintf("%s", "학년 및 성별에 따른 성관계 경험률")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(sxlExrtDataL1, aes(x = 응답자특성별2, y = val, fill = leg, group = leg, label = round(val, 2))) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(position = position_dodge(width = 0.9), vjust = 1.5, size = 5, color = "white") +
  labs(x = "학년", y = "성관계 비율 [%]", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
  

# ================================================
# 성관계 시작 연령 파일 읽기
# ================================================
fileInfo2 = Sys.glob(file.path(globalVar$inpPath, serviceName, "성관계_시작_연령_2023.csv"))

bgnAgeSxlIntData = readr::read_csv(fileInfo2, locale = locale("ko", encoding = "EUC-KR"))

bgnAgeSxlIntDataL1 = bgnAgeSxlIntData %>% 
  dplyr::filter(! 성별 %in% c("전체")) %>% 
  dplyr::select(성별, 평균연령) %>% 
  tidyr::gather(-성별, key = "key", value = "val") %>% 
  dplyr::mutate(
    leg = dplyr::case_when(
      stringr::str_detect(성별, regex("남학생")) ~ "남성"
      , stringr::str_detect(성별, regex("여학생")) ~ "여성"
    )
  )

# 성별에 따른 성관계 시작 연령
mainTitle = sprintf("%s", "성별에 따른 성관계 시작 연령")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(bgnAgeSxlIntDataL1, aes(x = 성별, y = val, fill = leg, group = leg, label = round(val, 2))) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(position = position_dodge(width = 0.9), vjust = 1.5, size = 5, color = "white") +
  labs(x = "성별", y = "성관계 시작 연령", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
