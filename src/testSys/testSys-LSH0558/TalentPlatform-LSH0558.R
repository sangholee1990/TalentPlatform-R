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
# R을 이용한 엑셀 파일 기반으로 웹 스크래핑 및 신규 컬럼 추가

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0558"

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
library(openxlsx)
library(rvest)
library(stringr)
library(tidyverse)
library(fs)

fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "USA_PMA_2024_04010430.xlsx"))
for (fileInfo in fileList) {

  fileName = tools::file_path_sans_ext(fs::path_file(fileInfo))
  data = openxlsx::read.xlsx(fileInfo, sheet = 1, startRow = 1) %>% 
    dplyr::mutate(Decision.Date = as.Date(Decision.Date, origin = "1899-12-30")) %>% 
    tibble::as.tibble()
  
  # i = 1
  dataL1  = tibble::tibble()
  for (i in 1:nrow(data)) {
    
    rowData = data[i, ]
    urlDtlInfo = rowData$Web.Address
    
    # 산출물코드 웹 스크래핑
    productCode = tryCatch(
      expr = {
        tableData = urlDtlInfo %>% 
          read_html() %>% 
          rvest::html_elements("#pma-details") %>%
          rvest::html_table()
        
        tableDataL1 = tibble::tibble()
        for (j in 1:length(tableData)) {
          tableDataL1 = dplyr::bind_rows(tableDataL1, tableData[[j]])
        }
        
        tableInfo = tableDataL1 %>% 
          dplyr::filter(stringr::str_detect(X1, stringr::regex("Product Code"))) %>% 
          dplyr::slice(1)
        
        productCode = stringr::str_replace_all(tableInfo$X2, "\\[.*?\\]", "") %>% 
          stringr::str_trim()
        
      }, error = function(error) {
        NA
      }
    )
    
    
    cat(sprintf("[CHECK] per : %s%% / productCode : %s / urlDtlInfo : %s", round(i / nrow(data) * 100, 2), productCode, urlDtlInfo), "\n")
    
    # 신규 컬럼 추가
    rowDataL1 = rowData %>% 
      dplyr::mutate(productCode = productCode)
    
    dataL1 = dplyr::bind_rows(dataL1, rowDataL1)
    
    # 시간 지연
    Sys.sleep(1)
  }
  
  # 엑셀 파일 지정
  saveXlsxFile = sprintf("%s/%s/OUT_%s.xlsx", globalVar$outPath, serviceName, fileName)
  dir.create(fs::path_dir(saveXlsxFile), showWarnings = FALSE, recursive = TRUE)
  
  wb = openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(wb, "Sheet1", dataL1, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
  openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)
  cat(sprintf("saveXlsxFile : %s", saveXlsxFile), "\n")
}
