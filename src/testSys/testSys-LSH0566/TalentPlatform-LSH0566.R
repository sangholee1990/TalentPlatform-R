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

# 혹시 지난번 짜주신 코드에서 2개 항목만 더 수집할 수 있도록 추가해주실 수 있을까요? Manufacturer details 내에서 Address 부분이랑 Mandate validity dates 에서 From과 To 에 있는 날짜에요!

# 계정 로그인 방법
# https://aurumguide.tistory.com/2
# sa /dms01user01

# dbo.TB_EUDAMED

# 테이블 생성
# CREATE TABLE TB_EUDAMED (
#   uuidInfo NVARCHAR(255) PRIMARY KEY,
#   urlDtl NVARCHAR(MAX),
#   acrOrgName NVARCHAR(255),
#   actIdSrn NVARCHAR(255),
#   address NVARCHAR(255),
#   manDate NVARCHAR(255),
#   appLeg NVARCHAR(255),
#   udiDu NVARCHAR(255),
#   riskCls NVARCHAR(255),
#   devName NVARCHAR(255),
#   udiDi NVARCHAR(255),
#   status NVARCHAR(255),
#   tradeName NVARCHAR(255),
#   memberState NVARCHAR(255),
#   version NVARCHAR(255),
#   lastUpdData NVARCHAR(255),
#   addProDesc NVARCHAR(255),
#   regDate NVARCHAR(255)
# );

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
library(DBI)
library(odbc)
library(readxl)
library(glue)

# MSSQL 연결 설정
con = odbc::dbConnect(
  odbc::odbc()
  , Driver = "SQL Server"
  # , Server = "SHLEE"
  , Server = "localhost"
  , Database = "master"
  , UID = "sa"
  , PWD = "dms01user01"
  , encoding = "UTF-8"
  , Port = 1433
  )

# # 특정 컬럼만 선택하여 쿼리 실행
query = "SELECT uuidInfo, acrOrgName, actIdSrn, memberState FROM dbo.TB_EUDAMED;"
data = DBI::dbGetQuery(con, query)
print(data)





# 함수 선언
errorHandler = function(x) {
  ifelse(is.na(x) || length(x) == 0 || x == "", NA, x)
}

allCnt = 464573
# pageSize = 300
pageSize = 10

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
pageInfo = 1
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
    
    apiDtlMan = sprintf("https://ec.europa.eu/tools/eudamed/api/actors/%s/publicInformation?languageIso2Code=en", manDtlInfo$uuid)
    
    resDtlManData = httr::GET(apiDtlMan) %>% 
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON()
    
    if (length(resDtlManData) < 1) next
    
    # 1) Actor/Organisation name
    acrOrgName = tryCatch({
      sprintf("%s[%s]", manDtlInfo$name, manDtlInfo$names$texts$language$isoCode %>% toupper()) %>% errorHandler()
    }, error = function(e) {NA})
    
    # 2) Actor ID/SRN
    actIdSrn = tryCatch({
      sprintf("%s", manDtlInfo$srn) %>% errorHandler()
    }, error = function(e) {NA})
    
    # 14) Address
    address = tryCatch({
      sprintf("%s", manDtlInfo$geographicalAddress) %>% errorHandler()
    }, error = function(e) {NA})
    
    # 15) Mandate validity dates
    manDate = tryCatch({
      autRepInfo = resDtlManData$actorDataPublicView$authorisedRepresentatives
      srtDate = ifelse(is.na(autRepInfo$startDate), "-", autRepInfo$startDate)
      endDate = ifelse(is.na(autRepInfo$endDate), "-", autRepInfo$endDate)
      sprintf("From %s To %s", srtDate, endDate) %>% errorHandler()
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
      # pageInfo, uuidInfo, api, urlDtl, apiDtlDev, apiDtlTool
      pageInfo, uuidInfo, urlDtl
      , acrOrgName, actIdSrn, address, manDate, appLeg, udiDu, riskCls, devName, udiDi, status, tradeName, memberState, version, lastUpdData, addProDesc
    ) %>% 
      dplyr::mutate(
        regDate = Sys.time()
      )
    
    if (nrow(data) < 1) next
    # dataL1 = dplyr::bind_rows(dataL1, data)
    
    
    
    
    
    
    
    # R의 NULL 값 및 특수 문자 처리 함수
    handle_na <- function(x) {
      if (is.na(x)) {
        return("NULL")
      } else {
        # 문자열 내부의 따옴표 처리 및 SQL 형식 맞추기
        return(paste0("'", gsub("'", "''", x), "'"))
      }
    }
    
    # 날짜 형식 올바르게 변환 함수
    format_datetime <- function(dt) {
      if (is.na(dt)) {
        return("NULL")
      } else {
        # 날짜 형식을 'YYYY-MM-DD HH:MM:SS'로 변환
        return(paste0("'", format(as.POSIXct(dt, format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d %H:%M:%S"), "'"))
      }
    }
    
    # 데이터프레임의 각 행에 대해 MERGE 쿼리 생성 및 실행
  
    
    data[] <- lapply(data, function(col) {
      if (is.character(col)) {
        return(iconv(col, from = "UTF-8", to = "CP949", sub = ""))
      } else {
        return(col)
      }
    })
    row_data <- data[1, ]
    
    # 동적 MERGE 쿼리 생성
    merge_query <- glue::glue("
    MERGE INTO dbo.TB_EUDAMED AS Target
    USING (SELECT
            {handle_na(row_data$uuidInfo)} AS uuidInfo,
            {handle_na(row_data$urlDtl)} AS urlDtl,
            {handle_na(row_data$acrOrgName)} AS acrOrgName,
            {handle_na(row_data$actIdSrn)} AS actIdSrn,
            {handle_na(row_data$address)} AS address,
            {format_datetime(row_data$manDate)} AS manDate,
            {handle_na(row_data$appLeg)} AS appLeg,
            {handle_na(row_data$udiDu)} AS udiDu,
            {handle_na(row_data$riskCls)} AS riskCls,
            {handle_na(row_data$devName)} AS devName,
            {handle_na(row_data$udiDi)} AS udiDi,
            {handle_na(row_data$status)} AS status,
            {handle_na(row_data$tradeName)} AS tradeName,
            {handle_na(row_data$memberState)} AS memberState,
            {handle_na(row_data$version)} AS version,
            {format_datetime(row_data$lastUpdData)} AS lastUpdData,
            {handle_na(row_data$addProDesc)} AS addProDesc,
            {format_datetime(row_data$regDate)} AS regDate
          ) AS Source
    ON Target.uuidInfo = Source.uuidInfo
    WHEN MATCHED THEN
      UPDATE SET
        Target.urlDtl = Source.urlDtl,
        Target.acrOrgName = Source.acrOrgName,
        Target.actIdSrn = Source.actIdSrn,
        Target.address = Source.address,
        Target.manDate = Source.manDate,
        Target.appLeg = Source.appLeg,
        Target.udiDu = Source.udiDu,
        Target.riskCls = Source.riskCls,
        Target.devName = Source.devName,
        Target.udiDi = Source.udiDi,
        Target.status = Source.status,
        Target.tradeName = Source.tradeName,
        Target.memberState = Source.memberState,
        Target.version = Source.version,
        Target.lastUpdData = Source.lastUpdData,
        Target.addProDesc = Source.addProDesc,
        Target.regDate = Source.regDate
    WHEN NOT MATCHED BY TARGET THEN
      INSERT (uuidInfo, urlDtl, acrOrgName, actIdSrn, address, manDate, appLeg, udiDu, riskCls, devName, udiDi, status, tradeName, memberState, version, lastUpdData, addProDesc, regDate)
      VALUES (Source.uuidInfo, Source.urlDtl, Source.acrOrgName, Source.actIdSrn, Source.address, Source.manDate, Source.appLeg, Source.udiDu, Source.riskCls, Source.devName, Source.udiDi, Source.status, Source.tradeName, Source.memberState, Source.version, Source.lastUpdData, Source.addProDesc, Source.regDate);
  ")
    
    # iconv(merge_query, from = "CP949", to = "UTF-8", sub = "")
    dbExecute(con, merge_query)
    
      
      # MERGE 쿼리 실행
      tryCatch({
        dbExecute(con, merge_query)
      }, error = function(e) {
        message(glue("Row {1}: Error occurred - {e$message}"))
      })
    }
    
    
    
    
    # 동적 MERGE 쿼리 생성
    merge_query <- glue_sql("
    MERGE INTO dbo.TB_EUDAMED AS Target
    USING (SELECT
            '{data$uuidInfo}' AS uuidInfo,
            '{data$urlDtl}' AS urlDtl,
            '{data$acrOrgName}' AS acrOrgName,
            '{data$actIdSrn}' AS actIdSrn,
            '{data$address}' AS address,
            '{data$manDate}' AS manDate,
            '{data$appLeg}' AS appLeg,
            '{data$udiDu}' AS udiDu,
            '{data$riskCls}' AS riskCls,
            '{data$devName}' AS devName,
            '{data$udiDi}' AS udiDi,
            '{data$status}' AS status,
            '{data$tradeName}' AS tradeName,
            '{data$memberState}' AS memberState,
            '{data$version}' AS version,
            '{data$lastUpdData}' AS lastUpdData,
            '{data$addProDesc}' AS addProDesc,
            '{data$regDate}' AS regDate
          ) AS Source
    ON Target.uuidInfo = Source.uuidInfo
    WHEN MATCHED THEN
      UPDATE SET
        Target.urlDtl = Source.urlDtl,
        Target.acrOrgName = Source.acrOrgName,
        Target.actIdSrn = Source.actIdSrn,
        Target.address = Source.address,
        Target.manDate = Source.manDate,
        Target.appLeg = Source.appLeg,
        Target.udiDu = Source.udiDu,
        Target.riskCls = Source.riskCls,
        Target.devName = Source.devName,
        Target.udiDi = Source.udiDi,
        Target.status = Source.status,
        Target.tradeName = Source.tradeName,
        Target.memberState = Source.memberState,
        Target.version = Source.version,
        Target.lastUpdData = Source.lastUpdData,
        Target.addProDesc = Source.addProDesc,
        Target.regDate = Source.regDate
    WHEN NOT MATCHED BY TARGET THEN
      INSERT (uuidInfo, urlDtl, acrOrgName, actIdSrn, address, manDate, appLeg, udiDu, riskCls, devName, udiDi, status, tradeName, memberState, version, lastUpdData, addProDesc, regDate)
      VALUES (Source.uuidInfo, Source.urlDtl, Source.acrOrgName, Source.actIdSrn, Source.address, Source.manDate, Source.appLeg, Source.udiDu, Source.riskCls, Source.devName, Source.udiDi, Source.status, Source.tradeName, Source.memberState, Source.version, Source.lastUpdData, Source.addProDesc, Source.regDate);
  ", .con = con)
    
    DBI::dbExecute(con, merge_query)
    
    # MERGE 쿼리 실행
    tryCatch({
      DBI::dbExecute(con, merge_query)
    }, error = function(e) {
      cat(sprintf("[ERROR] pageInfo : %s / uuidInfo : %s", pageInfo, uuidInfo), "\n")
      # message(glue("Row {i}: Error occurred - {e$message}"))
    })
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

# print(dataL1)

saveXlsxFile = sprintf("%s/%s/%s.xlsx", globalVar$outPath, serviceName, "dataL1")
dir.create(fs::path_dir(saveXlsxFile), showWarnings = FALSE, recursive = TRUE)
wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Sheet1")
openxlsx::writeData(wb, "Sheet1", dataL1, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)
cat(sprintf("[CHECK] saveXlsxFile : %s", saveXlsxFile), "\n")
