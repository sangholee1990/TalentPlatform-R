
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
# R을 이용한 막대 그래프 시각화

# 15년도 5712
# 17년도 25108
# 19년도 89918
# 20년도 134962
# 21년도 220000

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0322"
contextPath = ifelse(env == "local", ".", "E:/04. TalentPlatform/Github/TalentPlatform-R")

if (env == "local") {
  globalVar = list(
    "inpPath" = contextPath
    , "figPath" = contextPath
    , "outPath" = contextPath
    , "tmpPath" = contextPath
    , "logPath" = contextPath
  )
} else {
  source(here::here(file.path(contextPath, "src"), "InitConfig.R"), encoding = "UTF-8")
}


#================================================
# 비즈니스 로직 수행
#================================================
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

data = data.frame(
  key = c("2015", "2017", "2019", "2020", "2021")
  , val = c(5712, 25108, 89918, 134962, 220000)
)

plotSubTitle = sprintf("%s", "연도별 막대 그래프")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)

ggplot(data, aes(x = key, y = val, fill = val, label = scales::comma(val))) +
  geom_bar(stat = "identity") +
  geom_text(hjust = 0.5, vjust = 1.25, color = "white", size = 5) +
  labs(x = "연도", y = NULL, fill = NULL, title = plotSubTitle) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
