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
# R을 이용한 내국인 해외여행 자료 시각화 (월별 여행객, 연령대별 여행자)

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0527"

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


# ================================================
# 연령대별 여행자 비율
# ================================================
# 파일 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "국민 해외관광객 성연령별.csv"))
sexData = readr::read_csv(fileInfo, locale = locale("ko", encoding = "EUC-KR"))

sexDataL1 = sexData %>%
  dplyr::select(-c("남성 수", "여성 수")) %>% 
  tidyr::pivot_longer(cols = c(`남성 비율`, `여성 비율`), names_to = "key", values_to = "val")

# 연령대별 여행자 비율
mainTitle = sprintf("%s", "연령대별 여행자 비율")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(sexDataL1, aes(x = 연령대, y = val, fill = key, group = key, label = round(val, 2))) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(position = position_dodge(width = 0.9), vjust = 1.5, size = 5, color = "white") +
  labs(x = "연렁대", y = "비율 [%]", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
    ) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# ================================================
# 월별 여행객
# ================================================
# 파일 읽기
fileInfo2 = Sys.glob(file.path(globalVar$inpPath, serviceName, "국민 해외관광객 추이.csv"))
trendData = readr::read_csv(fileInfo2, locale = locale("ko", encoding = "EUC-KR"))

trendDataL1 = trendData %>% 
  dplyr::mutate(
    dtDate = readr::parse_date(as.character(기준연월), format = "%Y%m")
  ) %>% 
  dplyr::rename(
    val = `국민 해외관광객 수`
  )

# 월별 여행객
mainTitle = sprintf("%s", "월별 여행객")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(data = trendDataL1, aes(x = dtDate, y = val)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month", minor_breaks = "1 month") +
  labs(x = "날짜 [년-월]", y = "해외 여행객", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
    , axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
  ) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
