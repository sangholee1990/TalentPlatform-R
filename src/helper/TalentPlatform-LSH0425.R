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
# R을 이용한 대중교통 통행량 막대 그래프 시각화

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0425"

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
library(xlsx)

# 파일 읽기
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "*.xlsx"))

# fileInfo = fileList[1]
data = tibble::tibble()
for (fileInfo in fileList) {
  
  selData = xlsx::read.xlsx(fileInfo, sheetIndex = 1) %>%
    tibble::as.tibble() %>% 
    dplyr::filter(
      ! is.na(통행량)
    )
  
  data = dplyr::bind_rows(data, selData)
}


dataL1 = data %>%
  dplyr::rename(key = 시도.출발.) %>% 
  dplyr::select(c("일자", key, "통행량")) %>% 
  dplyr::arrange(desc(통행량)) 

statData = dataL1 %>%
  dplyr::group_by(일자) %>% 
  dplyr::summarise(
    sumVal = sum(통행량, na.rm = TRUE)
  )

statDataL1 = dataL1 %>% 
  dplyr::left_join(statData, by = c("일자" = "일자")) %>% 
  dplyr::mutate(
    perVal = (통행량 / sumVal) * 100.0
  ) %>% 
  dplyr::arrange(desc(통행량)) 


# 내림차순 정렬
dataL1$key = forcats::fct_relevel(dataL1$key, unique(dataL1$key))
statDataL1$key = forcats::fct_relevel(statDataL1$key, unique(statDataL1$key))


# 주요 시도에 따른 대중교통 통행량 막대 그래프
plotSubTitle = sprintf("%s", "주요 시도에 따른 대중교통 통행량 막대 그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(data = dataL1, aes(x = key, y = 통행량, fill = 일자, label = round(통행량, 0))) +
  geom_bar(stat = "identity", width = 0.5, position=position_dodge(width = 0.75))  +
  geom_text(position = position_dodge(width = 0.75), size = 4, vjust = -0.5, hjust = 0.5, color = "black") +
  labs(title = NULL, x = "주요 시도", y = "통행량", colour = NULL, fill = NULL, subtitle = plotSubTitle) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
  ) + 
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


# 주요 시도에 따른 대중교통 %통행량 막대 그래프
plotSubTitle = sprintf("%s", "주요 시도에 따른 대중교통 %통행량 막대 그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(data = statDataL1, aes(x = key, y = perVal, fill = 일자, label = round(perVal, 1))) +
  geom_bar(stat = "identity", width = 0.5, position=position_dodge(width = 0.75))  +
  geom_text(position = position_dodge(width = 0.75), size = 4, vjust = -0.5, hjust = 0.5, color = "black") +
  labs(title = NULL, x = "주요 시도", y = "%통행량", colour = NULL, fill = NULL, subtitle = plotSubTitle) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
  ) + 
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
