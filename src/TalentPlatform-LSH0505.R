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
# R을 이용한 대한민국 지상관측소으로부터 실시간 기상정보 데이터 수집 (시간, 온도, 습도 등)

# 기상청 API 허브 (https://apihub.kma.go.kr)에서 API 발급 필요

# ASOS 헤더 정보
#--------------------------------------------------------------------------------------------------
#  기상청 지상관측 시간자료 [입력인수형태][예] ?tm=201007151200&stn=0&help=1
#--------------------------------------------------------------------------------------------------
#  1. TM     : 관측시각 (KST)
#  2. STN    : 국내 지점번호
#  3. WD     : 풍향 (16방위)
#  4. WS     : 풍속 (m/s)
#  5. GST_WD : 돌풍향 (16방위)
#  6. GST_WS : 돌풍속 (m/s)
#  7. GST_TM : 돌풍속이 관측된 시각 (시분)
#  8. PA     : 현지기압 (hPa)
#  9. PS     : 해면기압 (hPa)
# 10. PT     : 기압변화경향 (Code 0200) 
# 11. PR     : 기압변화량 (hPa)
# 12. TA     : 기온 (C)
# 13. TD     : 이슬점온도 (C)
# 14. HM     : 상대습도 (%)
# 15. PV     : 수증기압 (hPa)
# 16. RN     : 강수량 (mm) : 여름철에는 1시간강수량, 겨울철에는 3시간강수량
# 17. RN_DAY : 일강수량 (mm) : 해당시간까지 관측된 양(통계표)
# 18. RN_JUN : 일강수량 (mm) : 해당시간까지 관측된 양을 전문으로 입력한 값(전문)
# 19. RN_INT : 강수강도 (mm/h) : 관측하는 곳이 별로 없음
# 20. SD_HR3 : 3시간 신적설 (cm) : 3시간 동안 내린 신적설의 높이
# 21. SD_DAY : 일 신적설 (cm) : 00시00분부터 위 관측시간까지 내린 신적설의 높이
# 22. SD_TOT : 적설 (cm) : 치우지 않고 그냥 계속 쌓이도록 놔눈 경우의 적설의 높이
# 23. WC     : GTS 현재일기 (Code 4677)
# 24. WP     : GTS 과거일기 (Code 4561) .. 3(황사),4(안개),5(가랑비),6(비),7(눈),8(소나기),9(뇌전)
# 25. WW     : 국내식 일기코드 (문자열 22개) : 2자리씩 11개까지 기록 가능 (코드는 기상자원과 문의)
# 26. CA_TOT : 전운량 (1/10)
# 27. CA_MID : 중하층운량 (1/10)
# 28. CH_MIN : 최저운고 (100m)
# 29. CT     : 운형 (문자열 8개) : 2자리 코드로 4개까지 기록 가능
# 30. CT_TOP : GTS 상층운형 (Code 0509)
# 31. CT_MID : GTS 중층운형 (Code 0515)
# 32. CT_LOW : GTS 하층운형 (Code 0513)
# 33. VS     : 시정 (10m)
# 34. SS     : 일조 (hr)
# 35. SI     : 일사 (MJ/m2)
# 36. ST_GD  : 지면상태 코드 (코드는 기상자원과 문의)
# 37. TS     : 지면온도 (C)
# 38. TE_005 : 5cm 지중온도 (C)
# 39. TE_01  : 10cm 지중온도 (C)
# 40. TE_02  : 20cm 지중온도 (C)
# 41. TE_03  : 30cm 지중온도 (C)
# 42. ST_SEA : 해면상태 코드 (코드는 기상자원과 문의)
# 43. WH     : 파고 (m) : 해안관측소에서 목측한 값
# 44. BF     : Beaufart 최대풍력(GTS코드)
# 45. IR     : 강수자료 유무 (Code 1819) .. 1(Sec1에 포함), 2(Sec3에 포함), 3(무강수), 4(결측)
# 46. IX     : 유인관측/무인관측 및 일기 포함여부 (code 1860) .. 1,2,3(유인) 4,5,6(무인) / 1,4(포함), 2,5(생략), 3,6(결측)
#--------------------------------------------------------------------------------------------------

# AWS 헤더 정보
#--------------------------------------------------------------------------------------------------
#  WD1    : 1분 평균 풍향 (degree) : 0-N, 90-E, 180-S, 270-W, 360-무풍 
#  WS1    : 1분 평균 풍속 (m/s) 
#  WDS    : 최대 순간 풍향 (degree) 
#  WSS    : 최대 순간 풍속 (m/s) 
#  WD10   : 10분 평균 풍향 (degree) 
#  WS10   : 10분 평균 풍속 (m/s) 
#  TA     : 1분 평균 기온 (C) 
#  RE     : 강수감지 (0-무강수, 0이 아니면-강수) 
#  RN-15m : 15분 누적 강수량 (mm) 
#  RN-60m : 60분 누적 강수량 (mm) 
#  RN-12H : 12시간 누적 강수량 (mm) 
#  RN-DAY : 일 누적 강수량 (mm) 
#  HM     : 1분 평균 상대습도 (%) 
#  PA     : 1분 평균 현지기압 (hPa) 
#  PS     : 1분 평균 해면기압 (hPa) 
#  TD     : 이슬점온도 (C) 
#  *) -50 이하면 관측이 없거나, 에러처리된 것을 표시 
#--------------------------------------------------------------------------------------------------

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0505"

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
library(tidyverse)
library(lubridate)
library(fs)
# API 웹 요청
library(httr)
library(curl)
# 엑셀 다운로드
library(openxlsx)

# 설정 정보
sysOpt = list(
  
  # 기상청 API 허브 정보
  apiInfo = list(
    asos = list(
      url = "https://apihub.kma.go.kr/api/typ01/url/kma_sfctm3.php"
      , stn = "0"
      , help = "0"
      , saveFilePattern = "/DATA/OBS/%Y%m/%d/ASOS_OBS_%Y%m%d%H%M.csv"
      , saveXlsxPattern = "/DATA/OBS/ASOS_OBS_%s-%s.xlsx"
    )
    # , aws = list(
    #   url = "https://apihub.kma.go.kr/api/typ01/cgi-bin/url/nph-aws2_min"
    #   , disp = "0"
    #   , stn = "0"
    #   , help = "0"
    #   , saveFilePattern = "/DATA/OBS/%Y%m/%d/AWS_OBS_%Y%m%d%H%M.csv"
    # )
  )
  
  # 시작일/종료일
  , srtDate = "2023-01-01 00:00"
  , endDate = "2023-01-02 00:00"
  
  # 10초 이내
  , timeOut = 10
  , authKey = "hQDU-t1aQHaA1PrdWvB2eA"
)

# ******************************************************************************
# 자료 수집
# ******************************************************************************
# i = 1
# key = "asos"
# 시작일-종료일, 3시간 목록
# key = "aws"
for (key in names(sysOpt$apiInfo)) {
  apiInfo = sysOpt$apiInfo[[key]]
  cat(sprintf("[CHECK] key : %s", key), "\n")
  
  # 날짜 목록
  dtDateList = seq(lubridate::ymd_hm(sysOpt$srtDate), lubridate::ymd_hm(sysOpt$endDate), by = "3 hours")
  
  for (i in 1:length(dtDateList)) {
    dtDateInfo = dtDateList[i]
    
    saveFile = format(dtDateInfo, apiInfo$saveFilePattern)
    if (file.exists(saveFile) == TRUE) {
      cat(sprintf("[ERROR] 파일 존재 : %s", saveFile), "\n")
      next
    }
      
    # API 요청을 위한 파라미터 설정
    if (key == "asos") {
      params =  list(
        tm1 = format(dtDateInfo, "%Y%m%d%H%M")
        , tm2 = format(dtDateInfo + lubridate::hours(3), "%Y%m%d%H%M")
        , stn = apiInfo$stn
        , help = apiInfo$help
        , authKey = sysOpt$authKey
      )
    } else {
      params =  list(
        tm1 = format(dtDateInfo, "%Y%m%d%H%M")
        , tm2 = format(dtDateInfo + lubridate::hours(3), "%Y%m%d%H%M")
        , stn = apiInfo$stn
        , disp = apiInfo$disp
        , help = apiInfo$help
        , authKey = sysOpt$authKey
      )
    }
    
    queryStr = paste0("?", paste0(names(params), "=", params, collapse = "&"))
    url = paste0(apiInfo$url, queryStr)
    # cat(sprintf("[CHECK] url : %s", url), "\n")
    
    # curl을 사용한 API 요청
    res = tryCatch({
      curl::curl_fetch_memory(url, handle = new_handle(timeout = sysOpt$timeOut))
      # curl::curl_fetch_memory(url)
    }, error = function(e) {
      NULL
    })
    
    if (is.null(res) || res$status_code != 200) {
      cat(sprintf("[ERROR] API 실패 : %s", dtDateInfo), "\n")
      next
    }
    
    # 응답 내용 확인
    tmpFileInfo = tempfile()
    resCont = rawToChar(res$content, multiple = FALSE)
    writeLines(resCont, tmpFileInfo)
    
    data = readr::read_table(tmpFileInfo, comment = "#", col_names = FALSE, progress = FALSE, show_col_types = FALSE)
    if (nrow(data) < 1) next
    
    dir.create(fs::path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
    readr::write_csv(data, saveFile, col_names = FALSE)
    cat(sprintf("[CHECK] API 성공 : %s : %s", dtDateInfo, saveFile), "\n")
  }
}


# ******************************************************************************
# 자료 처리
# ******************************************************************************
# i = 1
# key = "asos"
for (key in names(sysOpt$apiInfo)) {
  apiInfo = sysOpt$apiInfo[[key]]
  cat(sprintf("[CHECK] key : %s", key), "\n")
  
  # 날짜 목록
  dtDateList = seq(lubridate::ymd_hm(sysOpt$srtDate), lubridate::ymd_hm(sysOpt$endDate), by = "3 hours")
  
  # 파일 읽기
  dataL2 = tibble::tibble()
  for (i in 1:length(dtDateList)) {
    dtDateInfo = dtDateList[i]
    
    saveFile = format(dtDateInfo, apiInfo$saveFilePattern)
    if (file.exists(saveFile) == FALSE) next
    
    data = readr::read_csv(saveFile, col_names = FALSE, progress = FALSE, show_col_types = FALSE)

    #  1. TM     : 관측시각 (KST)
    #  2. STN    : 국내 지점번호
    # 12. TA     : 기온 (C)
    # 14. HM     : 상대습도 (%)
    
    # 컬럼 이름 변경
    # TM, STN 문자열 변환
    # 특정 STN 지점 (90, 92) 선택
    dataL1 = data %>% 
      dplyr::rename(
        TM = X1
        , STN = X2
        , TA = X12
        , HM = X14
      ) %>% 
      dplyr::select(TM, STN, TA, HM) %>% 
      dplyr::filter(STN %in% c(90, 92)) %>%
      dplyr::mutate(across(c(TM, STN), as.character)) 
    
    # 데이터 병합
    dataL2 = dplyr::bind_rows(dataL2, dataL1)
  }
  
  # 엑셀 저장
  saveXlsxFile = sprintf(apiInfo$saveXlsxPattern , min(dataL2$TM, na.rm = TRUE), max(dataL2$TM, na.rm = TRUE))
  dir.create(path_dir(saveXlsxFile), showWarnings = FALSE, recursive = TRUE)
  wb = openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, key)
  openxlsx::writeData(wb, key, dataL2, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
  openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)
  cat(sprintf("[CHECK] saveXlsxFile : %s", saveXlsxFile), "\n")
}
