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
# R을 이용한 특정 컬럼 조건 추출 및 CSV 저장

# 첨부해드릴 이미지처럼 ;로 나뉜 데이터중 7번째가 5 인 데이터들만 뽑아서 csv파일로 만드려고 합니다!

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0403"

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

# 파일 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "sbus202210.txt"))
data = readr::read_delim(fileInfo, delim = ";", col_names = FALSE, skip = 1, locale = locale("ko", encoding = "EUC-KR"))

summary(data)

# 노선 컬럼 (7번째)에서 5인 데이터들만 추출
dataL1 = data %>% 
  dplyr::filter(X7 == 5)

# csv 파일 생성
saveFile = sprintf("%s/%s/%s.csv", globalVar$outPath, serviceName, "sbus202210")
dir.create(path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(x = dataL1, file = saveFile, col_names = TRUE)
cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")
