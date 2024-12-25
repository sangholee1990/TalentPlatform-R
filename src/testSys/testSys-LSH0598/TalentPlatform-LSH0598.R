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
# R을 이용한 대용량 XML 파일로부터 CSV 변환

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0598"

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
library(ggplot2)
library(dplyr)
library(fs)
library(purrr)

# 함수 설정
getTagText = function(lines, tag) {
  tryCatch({
    pattern = paste0("<", tag, ">(.*?)</", tag, ">")
    matches = str_match(paste0(lines, collapse = ""), pattern)
    val = matches[, 2]
    ifelse(is.na(val) | length(val) < 1, NULL, val)
  }, error = function(e) {
    NULL
  })
}

# 태그 목록
tagList = c(
  "zxxsdycpbs", "cpbsbmtxmc", "cpbsfbrq", "zxxsdyzsydydsl", "sydycpbs",
  "bszt", "sfyzcbayz", "zcbacpbs", "sfybtzjbs", "btcpbsyzxxsdycpbssfyz",
  "btcpbs", "cpmctymc", "spmc", "ggxh", "sfwblztlcp", "cpms", "cphhhbh",
  "yflbm", "qxlb", "flbm", "tyshxydm", "zczbhhzbapzbh", "ylqxzcrbarmc",
  "ylqxzcrbarywmc", "ybbm", "cplb", "cgzmraqxgxx", "sfbjwycxsy", "zdcfsycs",
  "sfwwjbz", "syqsfxyjxmj", "mjfs", "qtxxdwzlj", "tsrq", "scbssfbhph",
  "scbssfbhxlh", "scbssfbhscrq", "scbssfbhsxrq", "tscchcztj", "tsccsm",
  "deviceRecordKey", "versionNumber", "versionTime", "versionStauts",
  "correctionNumber", "correctionRemark", "correctionTime", 
  "qylxrcz", "qylxryx", "qylxrdh"
)


# 파일 읽기
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "*/*.xml"))
for (fileInfo in fileList) {
  fileData = readLines(fileInfo, encoding = "UTF-8")
  fileNameNotExt = tools::file_path_sans_ext(fs::path_file(fileInfo))
  
  # 인덱스 추출
  srtIdx = grep("<device>", fileData)
  endIdx = grep("</device>", fileData)
  
  dataL1 = tibble::tibble()
  for (i in seq_along(srtIdx)) {
    if (is.na(srtIdx[i]) | is.na(endIdx[i])) next
    
    if (i %% ceiling(length(srtIdx) / 10) == 0 || i == length(srtIdx)) {
      prog = (i / length(srtIdx)) * 100
      cat(sprintf("[CHECK] prog : %.2f%% (%s / %s)", prog, i, length(srtIdx)), "\n")
    }
    
    devInfo = fileData[srtIdx[i]:endIdx[i]]
    
    data = purrr::map_dfc(tagList, function(tagInfo) {
      val = getTagText(devInfo, tagInfo)
      tibble::tibble(!!tagInfo := val)
    })
    data$fileNameNotExt = fileNameNotExt
    
    if (length(data) < 1) next
    
    if (nrow(dataL1) < 1) {
      dataL1 = data
    } else {
      dataL1 = dplyr::bind_rows(dataL1, data)
    }
  }
  
  saveFile = sprintf("%s/%s/CSV/%s.csv", globalVar$outPath, serviceName, fileNameNotExt)
  dir.create(fs::path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(dataL1, saveFile)
  cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")
}


# 데이터 병합
fileList = Sys.glob(file.path(globalVar$outPath, serviceName, "CSV/*.csv"))

dataL2 = fileList %>%
  purrr::map(read.csv) %>%
  purrr::reduce(dplyr::bind_rows)

saveFile = sprintf("%s/%s/%s.csv", globalVar$outPath, serviceName, "UDID_FULL")
readr::write_csv(x = dataL2, file = saveFile)
cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")
