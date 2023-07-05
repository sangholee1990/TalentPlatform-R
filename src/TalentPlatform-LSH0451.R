# ===============================================================================================
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
# R을 이용한 한전 파워플래너 로그인 및 일별요금 CSV 파일 적재

# 로그인 ID (고객번호: 0422206954) , 패스워드(yusan2f6931)는 개발 시작시 제공
# 년월을 지정하고 , 프로그램을 가동하면 , 
# 데이터가 .csv 파일에 저장될 수 있으면 좋겠습니다.
# 
# 3. 예)  
# 검색기간:  2022년 1월 ~  2022년 05월
# 
# 실행 엔터
# 
# 지정된 폴더에 자동으로 고객번호 끝 4자리와 조합해서
# 6954.2022.01.csv 
# 6954.2022.02.csv
# 6954.2022.03.csv
# 6954.2022.04.csv
# 6954.2022.05.csv 
# 
# 저장할 수 있으면 좋겠습니다.

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0451"

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
library(RSelenium)
library(wdman)
library(webdriver)
library(xml2)
library(rvest)
library(magrittr)
library(janitor)
library(stringr)
library(purrr)

sysOpt = list(
  # 사용자 아이디/비밀번호
  "userId" = "0438306931"
  , "userPw" = "yusan2f6931"
  
  # 시작일/종료일
  , "srtDate" = "2008-01-01"
  , "endDate" = "2023-12-31"
)

# 전역 설절
options(readr.num_columns = 0)

# 설치 방법
# webdriver::install_phantomjs()
pjs = webdriver::run_phantomjs()

# 세션 시작
ses = Session$new(port = pjs$port)

# 로그인 화면 이동
ses$go("https://pp.kepco.co.kr/intro.do")


# 사용자 이름과 비밀번호 입력 필드를 찾습니다.
ses$executeScript("$('#RSA_USER_ID').val(arguments[0]);", list(sysOpt$userId))
ses$executeScript("$('#RSA_USER_PWD').val(arguments[0]);", list(sysOpt$userPw))

# 자바스크립트를 사용하여 로그인 버튼을 클릭합니다.
ses$executeScript("$('#intro_form > form > fieldset > input.intro_btn').click();", list())

# 일별요금 화면 이동
ses$go("https://pp.kepco.co.kr/re/re0103N.do?menu_id=O010406")

# 날짜 목록을 기준으로 반복문 수행
dtDateList = seq(lubridate::ymd(sysOpt$srtDate), lubridate::ymd(sysOpt$endDate), by = "1 month")
# i = 1
for (i in 1:length(dtDateList)) {
  dtDateInfo = dtDateList[i]
  sYear = format(dtDateInfo, "%Y")
  sMonth = format(dtDateInfo, "%m")
  dataL2 = tibble::tibble()
  
  if (Sys.Date() < dtDateInfo) next

  cat(sprintf("nYear : %s / nMonth : %s", sYear, sMonth), "\n")

  ses$executeScript("$('#SEARCH_YEAR').val(arguments[0]);", list(sYear))
  ses$executeScript("$('#SEARCH_MONTH').val(arguments[0]);", list(sMonth))
  ses$executeScript("getData();")
  Sys.sleep(0.5)
  
  data = ses$getSource()[[1]] %>%
    xml2::read_html() %>%
    rvest::html_node("#tableList") %>%
    rvest::html_table()
  
  if (nrow(data) < 1) next
  
  dataL1 = data %>% 
    janitor::clean_names() %>% 
    dplyr::filter(
      ! ilja %in% c("일자", "계")
    ) %>% 
    readr::type_convert()
  
  # 모든 컬럼에서 0값 표시
  isCheck = dataL1 %>% 
    dplyr::select(-ilja) %>% 
    dplyr::summarise_all(~sum(. == 0)) %>% 
    summarise(across(everything(), ~all(. == nrow(dataL1)))) %>%
    unlist() %>%
    all()
   
  if (isCheck) next
  
  dataL2 = dataL1 %>% 
    magrittr::set_colnames(colnames(data))
  
  if (nrow(dataL2) < 1) next
  saveFile = sprintf("%s/%s/%s/%s.csv", globalVar$outPath, serviceName, stringr::str_sub(sysOpt$userId, -4), format(dtDateInfo, "%Y.%m"))
  dir.create(fs::path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(dataL2, saveFile)
  cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")
}

# 세션 종료
# ses$close()