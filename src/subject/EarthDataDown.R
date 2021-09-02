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
# 테스트 

#================================================
# 초기 환경변수 설정
#================================================
# env = "local"   # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"   # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "TEST0001"
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
library(rGEDI)
library(tidyverse)

srtDate = "2020-08-01"
endDate = "2021-06-01"

contextPath = "https://e4ftl01.cr.usgs.gov/VIIRS/VNP43C3.001"
dtDateList = seq(lubridate::ymd(srtDate), lubridate::ymd(endDate), by = "1 days")
saveDir = sprintf("%s/%s", globalVar$outPath, serviceName)

netrc = file.path(saveDir, ".netrc")
netrcConn = file(netrc)

Sys.setenv(
  "NASA_USER" = "Leesangho"
  , "NASA_PASSWORD" = "cjswo123!@Q"
)

writeLines(c("machine urs.earthdata.nasa.gov",
             sprintf("login %s", Sys.getenv("NASA_USER")),
             sprintf("password %s", Sys.getenv("NASA_PASSWORD"))
), netrcConn)

close(netrcConn)


data = tibble(dtDateList) %>% 
  dplyr::mutate(
    dtDateFmt = lubridate::parse_date_time(dtDateList, "%Y-%m-%d") %>% format("%Y.%m.%d")
    , dtMonth = lubridate::month(dtDateList)
    , urlList = file.path(contextPath, dtDateFmt)
  ) %>% 
  dplyr::filter(dtMonth %in% c(2, 5, 8, 11))


for (urlInfo in data$urlList) {
  
  urlDtlList = urlInfo %>%
    purrr::map(~getUrlTagHref(.x, 'a')) %>%
    unlist() %>%
    as.tibble() %>% 
    dplyr::filter(
      ! stringr::str_detect(value, regex(".xml"))
      , stringr::str_detect(value, regex(".h5"))
    )
  
  # urlDtlInfo = urlDtlList$value[1]
  for (urlDtlInfo in urlDtlList$value) {
    
    saveFile = sprintf("%s/%s/%s", globalVar$outPath, serviceName, urlDtlInfo)
    
    isFile = file.exists(path = saveFile)
    if (isFile == TRUE) next
    
    rGEDI::gediDownload(
      file.path(urlInfo, urlDtlInfo)
      , saveDir
    )
  }
}
