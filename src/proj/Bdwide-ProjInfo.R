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
# 

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "bdwide"
serviceName = "PRJ0003"
contextPath = ifelse(env == "local", ".", getwd())

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
library(fs)
library(openxlsx)


#==================================================================================================
# 재능플랫폼 목록 조회
#==================================================================================================
globalVar$inpPath = "G:/내 드라이브/shlee/04. TalentPlatform/[재능플랫폼] 최종납품"

data = dplyr::bind_rows(
  tibble::tibble(prjDir = fs::path_file(list.dirs(file.path(globalVar$inpPath), recursive = FALSE)))
  , tibble::tibble(prjDir = fs::path_file(list.dirs(file.path(globalVar$inpPath, "[완료]"), recursive = FALSE)))
  , tibble::tibble(prjDir = fs::path_file(list.dirs(file.path(globalVar$inpPath, "[완료-미입금]"), recursive = FALSE)))
  , tibble::tibble(prjDir = fs::path_file(list.dirs(file.path(globalVar$inpPath, "[완료-미응답]"), recursive = FALSE)))
)

dataL1 = data %>%
  dplyr::filter(
    stringr::str_detect(prjDir, regex("LSH"))
  ) %>% 
  dplyr::mutate(
    prjName = stringr::str_split(prjDir, pattern = "] ", n = 2) %>% map_chr(., 2)
  ) %>% 
  tidyr::separate(prjName, sep = "\\. ", into = c("serviceNum", "serviceName") ) %>% 
  dplyr::arrange(serviceNum)


saveXlsxFile = sprintf("%s/%s_%s.xlsx", globalVar$outPath, serviceName, "재능플랫폼 외주목록")
wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "외주목록")
openxlsx::writeData(wb, "외주목록", dataL1, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = FALSE)
