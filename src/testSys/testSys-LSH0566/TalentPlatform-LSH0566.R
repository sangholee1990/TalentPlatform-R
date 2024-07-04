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
# R을 이용한 EUDAMED 사이트로부터 웹 스크래핑 및 장기간 수집

# 선생님 안녕하세요. 견적 요청드리고싶은 건 있어서 다시 연락드립니다. 이번에도 R에서 html 코딩이 필요한 상황입니다.

# 1. https://ec.europa.eu/tools/eudamed/#/screen/search-device?submitted=true 접속 (결과값: 471839 records found)
#   
#   2. 471839건 각각 View action 누르면 나오는 페이지에서 아래 내용을 스크래핑
# 1) Actor/Organisation name
# 2) Actor ID/SRN
# 3) Applicable legislation
# 4) Basic UDI-DU/EUDAMED DI/ Issuing entity
# 5) Risk class
# 6) Device name
# 7) UDI-DI code/Issuing entity
# 8) Status,Nomenclature code(s)
# 9) Name/Trade name(s)
# 10) Member state of the placing on the EU market of the device
# 
# 3. 위 스크래핑해온 10가지 항목을 dataframe으로 만든 후 엑셀로 추출
# 4. 결과: 10 X 471839 크기의 데이터셋

# 다만 혹시, 3번 요청항목 "Application legislation" 컬럼을 보지못했는데 추가해주실 수 있을까요?

# 그리고, 혹시 3개 컬럼을 추가로 요청드려도 될지도 여쭈어요 (1. Basic UDI-DI details 섹션 내 Version, 2. Last update date, 3. UDI-DI details 섹션 내 additional product description)

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0566"

if (Sys.info()[["sysname"]] == "Windows") {
  contextPath = ifelse(env == "local", getwd(), "C:/SYSTEMS/PROG/R/TalentPlatform-R")
} else {
  contextPath = ifelse(env == "local", getwd(), "/SYSTEMS/PROG/R/PyCharm")
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
library(httr)
library(jsonlite)
library(rvest)

# 함수 선언
errorHandler = function(x) {
  ifelse(is.na(x) || length(x) == 0 || x == "", NA, x)
}

allCnt = 464573
# pageSize = 300
pageSize = 25

pageMax = ceiling(allCnt / pageSize)
pageList = 0:pageMax

# ******************************************************************************
# 설정 API 요청
# ******************************************************************************
apiCodeA = "https://ec.europa.eu/tools/eudamed/api/translationTexts/target/?target=a&language=en&languageIso2Code=undefined"

resCodeA = httr::GET(apiCodeA) %>% 
  httr::content(as = "text", encoding = "UTF-8") %>% 
  jsonlite::fromJSON()

apiCodeP = "https://ec.europa.eu/tools/eudamed/api/translationTexts/target/?target=p&language=en&languageIso2Code=undefined"

resCodeP = httr::GET(apiCodeP) %>% 
  httr::content(as = "text", encoding = "UTF-8") %>% 
  jsonlite::fromJSON()


# ******************************************************************************
# 기본 API 요청
# ******************************************************************************
for (pageInfo in pageList) {
  
  # 테스트 조건
  # if (pageInfo > 0) next
  if (pageInfo > 1) next
  
  per = round(pageInfo / length(pageList) * 100, 2)
  cat(sprintf("[CHECK] per : %s %%", per), "\n")
  
  # 파일 검사
  saveFile = sprintf("%s/%s/%s-%s-%s.csv", globalVar$outPath, serviceName, "dataL1", pageSize, pageInfo)
  dir.create(fs::path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
  
  isFile = file.exists(path = saveFile)
  if (isFile == TRUE) next
  
  api = sprintf("https://ec.europa.eu/tools/eudamed/api/devices/udiDiData?page=%s&pageSize=%s&size=%s&iso2Code=en&deviceStatusCode=refdata.device-model-status.on-the-market&languageIso2Code=en", pageInfo, pageSize, pageSize)
  
  # API 요청
  resData = httr::GET(api) %>% 
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()
  
  if (length(resData) < 1) next
  
  # 데이터프레임으로 변환
  resDataL1 = resData$content %>% 
    tibble::as.tibble()
  
  if (nrow(resDataL1) < 1) next
  
  # ******************************************************************************
  # 세부 API 요청
  # ******************************************************************************
  dataL1 = tibble::tibble()
  uuidList = resDataL1$uuid
  for (uuidInfo in uuidList) {
    cat(sprintf("[CHECK] pageInfo : %s / uuidInfo : %s", pageInfo, uuidInfo), "\n")
    
    apiDtlDev = sprintf("https://ec.europa.eu/tools/eudamed/api/devices/basicUdiData/udiDiData/%s?languageIso2Code=en", uuidInfo)
    apiDtlTool = sprintf("https://ec.europa.eu/tools/eudamed/api/devices/udiDiData/%s?languageIso2Code=en", uuidInfo)
    urlDtl = sprintf("https://ec.europa.eu/tools/eudamed/#/screen/search-device/%s", uuidInfo)
    
    # API 요청/응답
    resDtlDevData = httr::GET(apiDtlDev) %>% 
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON()
    
    if (length(resDtlDevData) < 1) next
    
    resDtlToolData = httr::GET(apiDtlTool) %>% 
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON()
    
    if (length(resDtlToolData) < 1) next
    
    # ******************************************************************************
    # 데이터 가공
    # ******************************************************************************
    manDtlInfo = resDtlDevData$manufacturer
    
    # 1) Actor/Organisation name
    acrOrgName = tryCatch({
      sprintf("%s[%s]", manDtlInfo$name, manDtlInfo$names$texts$language$isoCode %>% toupper()) %>% errorHandler()
    }, error = function(e) {NA})
    
    # 2) Actor ID/SRN
    actIdSrn = tryCatch({
      sprintf("%s", manDtlInfo$srn) %>% errorHandler()
    }, error = function(e) {NA})
    
    # 3) Applicable legislation
    appLeg = tryCatch({
      sprintf("%s", resCodeA[resDtlDevData$legislation$code] %>% unlist()) %>% errorHandler()
    }, error = function(e) {NA})
    
    # 4) UDI-DU/EUDAMED DI/ Issuing entity
    udiDu = tryCatch({
      issEnt = resCodeA[resDtlDevData$basicUdi$issuingAgency$code] %>% unlist()
      sprintf("%s / %s", resDtlDevData$basicUdi$code, issEnt) %>% errorHandler()
    }, error = function(e) {NA})
    
    # 5) Risk class`refdata.risk
    riskCls = tryCatch({
      sprintf("%s", resCodeA[resDtlDevData$riskClass$code] %>% unlist()) %>% errorHandler()
    }, error = function(e) {NA})
    
    # 6) Device name
    devName = tryCatch({
      sprintf("%s", resDtlDevData$deviceName) %>% errorHandler()
    }, error = function(e) {NA})
    
    # 7) UDI-DI code/Issuing entity
    udiDi = tryCatch({
      sprintf("%s / %s", resDtlToolData$primaryDi$code, resCodeA[resDtlToolData$primaryDi$issuingAgency$code] %>% unlist()) %>% errorHandler()
    }, error = function(e) {NA})
    
    # 8) Status,Nomenclature code(s)
    status = tryCatch({
      tmpCode = (resDtlToolData$cndNomenclatures$description$texts %>% unlist())["text"] %>% tolower()
      sprintf("%s: %s", resDtlToolData$cndNomenclatures$code, paste0(toupper(substring(tmpCode, 1, 1)), substring(tmpCode, 2))) %>% errorHandler()
    }, error = function(e) {NA})
    
    # 9) Name/Trade name(s)
    tradeName = tryCatch({
      sprintf("%s [%s]", resDtlToolData$tradeName$texts$text, resDtlToolData$tradeName$texts$language$isoCode %>% toupper()) %>% errorHandler()
    }, error = function(e) {NA})
    
    # 10) Member state of the placing on the EU market of the device
    memberState = tryCatch({
      sprintf("%s", resDtlToolData$placedOnTheMarket$name) %>% errorHandler()
    }, error = function(e) {NA})
    
    # 11) Basic UDI-DI details 섹션 내 Version
    version = tryCatch({
      sprintf("Version %s", resDtlDevData$versionNumber) %>% errorHandler()
    }, error = function(e) {NA})
    
    # 12) Basic UDI-DI details 섹션 내 Last update date
    lastUpdData = tryCatch({
      sprintf("%s", resDtlToolData$deviceStatus$statusDate) %>% errorHandler()
    }, error = function(e) {NA})
    
    # 13) UDI-DI details 섹션 내 additional product description
    addProDesc = tryCatch({
      sprintf("%s [%s]", resDtlToolData$additionalDescription$text$text, resDtlToolData$additionalDescription$text$language$isoCode %>% toupper()) %>% errorHandler()
    }, error = function(e) {NA})
    
    # 데이터 변환
    data = data.frame(
      pageInfo, uuidInfo, api, urlDtl, apiDtlDev, apiDtlTool
      , acrOrgName, actIdSrn, appLeg, udiDu, riskCls, devName, udiDi, status, tradeName, memberState, version, lastUpdData, addProDesc
    )
    
    if (nrow(data) < 1) next
    
    dataL1 = dplyr::bind_rows(dataL1, data)
  }

  if (nrow(dataL1) > 0) {
    readr::write_csv(x = dataL1, file = saveFile)
    cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")  
  }
}

# 데이터 병합 
fileList = Sys.glob(file.path(globalVar$outPath, serviceName, "dataL1-*-*.csv"))

dataL1 = fileList %>%
  purrr::map(readr::read_csv) %>%
  purrr::reduce(dplyr::bind_rows)

print(dataL1)

saveXlsxFile = sprintf("%s/%s/%s.xlsx", globalVar$outPath, serviceName, "dataL1")
dir.create(fs::path_dir(saveXlsxFile), showWarnings = FALSE, recursive = TRUE)
wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Sheet1")
openxlsx::writeData(wb, "Sheet1", dataL1, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)
cat(sprintf("[CHECK] saveXlsxFile : %s", saveXlsxFile), "\n")
