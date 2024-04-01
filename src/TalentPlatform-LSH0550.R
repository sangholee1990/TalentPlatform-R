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
# R을 이용한 구 주소를 신 주소체계 변환

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0550"

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
library(openxlsx)
library(tidyverse)
library(openxlsx)
library(RCurl)
library(stringr)
library(httr)
library(jsonlite)
library(tibble)

# API 인증키 정보
apiId = globalVar$naverApigwApiKeyId
apiPw = globalVar$naverApigwApiKey

# 파일 검색
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "주소변경.xlsx"))

# 파일 읽기
data = openxlsx::read.xlsx(fileInfo, sheet = 1, startRow = 1)

dataL1 = tibble::tibble()
# for (i in 1:nrow(data)) {
for (i in 1:50) { 
  addr = data$address[i]
  cat(sprintf("[CHECK] addr : %s", addr), "\n")
  
  # 네이버 API (지도) 요청
  apiUrl = "https://naveropenapi.apigw.ntruss.com/map-geocode/v2/geocode"
  apiQuery = stringr::str_c("?query=", RCurl::curlEscape(stringr::str_conv(addr, encoding = "UTF-8")))

  apiRes = httr::GET(
    stringr::str_c(apiUrl, apiQuery)
    , httr::add_headers("X-NCP-APIGW-API-KEY-ID" = apiId, "X-NCP-APIGW-API-KEY" = apiPw, "Accept" = "application/json")
  )
  
  resData = httr::content(apiRes, as = "text") %>%
    jsonlite::fromJSON()
  
  if (resData['error']['errorCode'] == "200") next
  if (resData['status'] != "OK") next
  
  resDataL1 = resData$addresses %>% 
    tibble::as.tibble() %>%
    dplyr::select(-tidyselect::any_of("addressElements"))
  
  resDataL1$i = i
  resDataL1$addr = addr
  if (nrow(resDataL1) < 1) next
  
  dataL1 = dplyr::bind_rows(dataL1, resDataL1)
}

saveXlsxFile = sprintf("%s/%s/%s.xlsx", globalVar$outPath, serviceName, "신주소변경")
dir.create(fs::path_dir(saveXlsxFile), showWarnings = FALSE, recursive = TRUE)

wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Sheet1")
openxlsx::writeData(wb, "Sheet1", dataL1, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)

cat(sprintf("[CHECK] saveXlsxFile : %s", saveXlsxFile), "\n")
