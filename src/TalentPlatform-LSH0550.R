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
# apiId = "아이디"
# apiPw = "비밀번호"
apiId = globalVar$naverApigwApiKeyId
apiPw = globalVar$naverApigwApiKey

# 파일 검색
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "주소변경.xlsx"))
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "주소목록.xlsx"))

# 파일 읽기
data = openxlsx::read.xlsx(fileInfo, sheet = 1, startRow = 1)

# i = 10
# i = 13
# i = 34
# i = 43
# i = 50
# addr = "충청북도 충주시 중원대로 3379"
# addr = "경기도 하남시 망월동 1125"
# addr = "망월동 1125"

# 주소와 좌표 검색 API 사용하기
# https://navermaps.github.io/maps.js.ncp/docs/tutorial-3-geocoder-geocoding.example.html
dataL1 = tibble::tibble()
# for (i in 1:nrow(data)) {
for (i in 1:50) { 
  
  # 엑셀 내 주소 가져오기
  addr = data$address[i]
  cat(sprintf("[CHECK] addr : %s", addr), "\n")
  
  # 네이버 지오코딩 API 파라미터
  apiUrl = "https://naveropenapi.apigw.ntruss.com/map-geocode/v2/geocode"
  apiQuery = stringr::str_c(
    "?query=", RCurl::curlEscape(stringr::str_conv(addr, encoding = "UTF-8"))
    , "&count=100"
    , "&page=1"
    # , "&language=eng"
    )

  # API 요청
  apiRes = httr::GET(
    stringr::str_c(apiUrl, apiQuery)
    , httr::add_headers("X-NCP-APIGW-API-KEY-ID" = apiId, "X-NCP-APIGW-API-KEY" = apiPw, "Accept" = "application/json")
  )
  
  # API 응답
  resData = httr::content(apiRes, as = "text") %>%
    jsonlite::fromJSON()
  
  # API 응답 성공 시 동작
  if (resData['error']['errorCode'] == "200") next
  if (resData['status'] != "OK") next
  
  # 데이터프레임으로 변환
  resDataL1 = resData$addresses %>% 
    tibble::as.tibble()
  
  # 특정 컬럼 (addressElements) 삭제
  resDataL2 = resDataL1 %>% 
    dplyr::select(-tidyselect::any_of("addressElements")) %>% 
    dplyr::arrange(desc(roadAddress)) %>% 
    dplyr::slice(1)
  
  # 컬럼 내 roadAddress, jibunAddress 있을 경우 "아파트" 키워드 포함 텍스트 삭제
  if("roadAddress" %in% names(resDataL2)) resDataL2$addr2 = stringr::str_replace(resDataL2$roadAddress, "\\s*[^\\d\\s]*아파트.*$", "")
  # if("roadAddress" %in% names(resDataL2)) resDataL2$roadAddress = stringr::str_replace(resDataL2$roadAddress, "\\s*[^\\d\\s]*아파트.*$", "")
  if("jibunAddress" %in% names(resDataL2)) resDataL2$jibunAddress = stringr::str_replace(resDataL2$jibunAddress, "\\s*[^\\d\\s]*아파트.*$", "")
  
  # 최종 파일 포맷으로 지정
  # 데이터 유효성 검사를 통해 정상/비정상 구분
  resDataL3 = tibble::tibble(
    addr = addr  
    , addr2 = ifelse(nrow(resDataL2) == 0 || nchar(resDataL2$addr2) < 1, NA, resDataL2$addr2)
    , jibunAddress = ifelse(nrow(resDataL2) == 0 || nchar(resDataL2$jibunAddress) < 1, NA, resDataL2$jibunAddress)
    , roadAddress = ifelse(nrow(resDataL2) == 0 || nchar(resDataL2$roadAddress) < 1, NA, resDataL2$roadAddress)
    , englishAddress = ifelse(nrow(resDataL2) == 0 || nchar(resDataL2$englishAddress) < 1, NA, resDataL2$englishAddress)
    , y = ifelse(nrow(resDataL2) == 0 || nchar(resDataL2$y) < 1, NA, resDataL2$y)
    , x = ifelse(nrow(resDataL2) == 0 || nchar(resDataL2$x) < 1, NA, resDataL2$x)
  )
  
  dataL1 = dplyr::bind_rows(dataL1, resDataL3)
}

# 엑셀 파일 지정
# saveXlsxFile = sprintf("%s/%s/%s.xlsx", globalVar$outPath, serviceName, "신주소변경")
saveXlsxFile = sprintf("%s/%s/%s.xlsx", globalVar$outPath, serviceName, "신주소목록")
dir.create(fs::path_dir(saveXlsxFile), showWarnings = FALSE, recursive = TRUE)

# 엑셀 파일 저장
wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Sheet1")
openxlsx::writeData(wb, "Sheet1", dataL1, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)

cat(sprintf("[CHECK] saveXlsxFile : %s", saveXlsxFile), "\n")
