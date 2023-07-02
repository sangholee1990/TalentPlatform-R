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
# R을 이용한 통계량 (평균, 중앙값, 표준편차, 샘플오차, 95% 신뢰구간, 정상성 검정, T검정) 계산

# 각 그룹의 평균,중앙값,SD, IQR, sampling error, 95%CI, Test normality, T.test를 구해야하는데요

#================================================
# 초기 환경변수 설정
# ================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0310"
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
# 라이브러리 읽기
library(tidyverse)
library(readr)

# 파일 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "ID_10.csv"))
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))
dataL1 = data %>% 
  dplyr::select(x, Group)

# 평균
mean(dataL1$x, na.rm = TRUE)

# 중앙값
median(dataL1$x, na.rm = TRUE)

# SD
sd(dataL1$x, na.rm = TRUE)

# IQR
IQR(dataL1$x, na.rm = TRUE)

# sampling error
sqrt(var(dataL1$x, na.rm = TRUE) / length(dataL1$x))

# 95% CI
# CI::CI_z(dataL1$x, ci = 0.95)
meanVal = mean(dataL1$x, na.rm = TRUE)
errorVal = qt(0.975, df = length(dataL1$x) - 1) * sd(dataL1$x) / sqrt(length(dataL1$x))
left95 = meanVal - errorVal
right95 = meanVal + errorVal

# Test normality
# 정규성 검정
# P값이 0.966909으로서 귀무가설 채택, 대립가설 기각 (정규 분포를 따름)
shapiro.test(dataL1$x)

# T.test
# P값이 0.6739593으로서 귀무가설 채택 (두 특성의 분산 차이가 없다)
# 따라서 동일한 분산 조건 (var.equal = TRUE)
fTest = var.test(x ~ Group, data = dataL1)
fTest

# P값이 8.546373e-05로서 귀무가설 기각하지 못함 (두 특성은 차이가 없다)
tTest = t.test(x ~ Group, data = dataL1, var.equal = TRUE)
tTest