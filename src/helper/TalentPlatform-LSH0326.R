
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
# R을 이용한 그룹별 평균 및 순위 산정 그리고 컬럼 검사

# 안녕하세요
# 혹시 제가 보내드리는 파일을
# 그룹별로 평균계산해서 100개의 그룹에 대한 랭킹을 정해주는 함수랑
# 그 2번째 열에 적힌 같은 숫자를 가진 애들의 첫번째 열 값이 하나라도 0 이 아니면 
# 그 그룹에 대해서 1을 주는 함수
# 
# 이렇게 두개 가능할까요?

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0326"
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

# 함수 선언
groupRankChk = function(data) {
  
  dataL1 = data %>% 
    dplyr::group_by(gr) %>% 
    dplyr::summarise(
      meanVal = mean(v1, na.rm = TRUE)
    ) %>% 
    dplyr::mutate(
      rankVal = rank(meanVal)
    ) %>% 
    dplyr::select(gr, rankVal)
  
  return(dataL1)
}

groupTypeChk = function(data) {
  
  dataL1 = data %>% 
    dplyr::group_by(gr) %>% 
    dplyr::summarise(
      sumVal = sum(v1 == 0, na.rm = TRUE)
      , cnt = n()
    ) %>% 
    dplyr::mutate(
      type = ifelse(sumVal == cnt, 0, 1)
    ) %>% 
    dplyr::select(gr, type)
  
  return(dataL1)
}


# 파일 찾기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "data.csv"))

data = readr::read_csv(file = fileInfo)
summary(data)

dataL1 = groupRankChk(data)
dataL2 = groupTypeChk(data)

