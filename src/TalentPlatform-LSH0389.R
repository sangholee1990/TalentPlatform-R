
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
# R을 이용한 적포도주 및 백포도주 간의 품질 차이

# (Diva 와인 회사의 경영진은 (표준 편차로 측정한) 스프레드에 차이가 있는지 알고 싶습니다
# 그들의 적포도주와 백포도주의 품질. 시뮬레이션을 사용하여 테스트할 SQL 및 R 코드를 작성합니다)

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0389"

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

#================================================
# 비즈니스 로직 수행
#================================================
# 라이브러리 읽기
library(tidyverse)
library(readr)
library(moonBook)
library(webr)
library(ggstatsplot)
library(useful)

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "WINE.csv"))
data = readr::read_csv(fileInfo)

# data$type %>% unique()

# 연도, 적포도주/백포도주의 표준편차
dataL1 = data %>%
  dplyr::select(year, colour, quality) %>%
  dplyr::group_by(year, colour) %>%
  dplyr::summarise(
    sdQuality = sd(quality, na.rm = TRUE)
  ) %>%
  tidyr::spread(key = "colour", value = "sdQuality") %>%
  na.omit()


# *******************************************************
# F 테스트
# *******************************************************
fTest = var.test(dataL1$red, dataL1$white, conf.level = 0.95)

print(fTest)
# F의 귀무 가설은 2 그룹의 분산이 차이가 없음 (등 분산)
# 따라서 P < 0.05로 작을 경우 귀무 가설이 기각되어 2 그룹은 상이한 분산이고, 그 이외의 경우 등 분산
# F-test에서 p-value 0.14 (p > 0.05)로서  등 분산으로 판단
# 따라서 2 그룹의 검증은 스튜던트 T 검정을 수행

mainTitle = sprintf("%s", "적포도주 및 백포도주 간의 F 테스트")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

plot(fTest) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# *******************************************************
# T 테스트
# *******************************************************
# 등분산 가정 O
tTest = t.test(dataL1$red, dataL1$white, conf.level = 0.95, var.equal = TRUE, paired = FALSE)

# 등분산 가정 X
# tTest = t.test(dataL1$red, dataL1$white, conf.level = 0.95, var.equal = FALSE, paired = FALSE)

#  p-value는 0.02로서 귀무가설 기각 (두 포도주의 품질 차이가 없다)
print(tTest)

mainTitle = sprintf("%s", "적포도주 및 백포도주 간의 T 테스트")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

plot(tTest) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")