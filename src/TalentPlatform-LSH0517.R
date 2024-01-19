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
# R을 이용한 선거 데이터 (서울특별시 용산구) 3단계 시각화 및 도표 삽입
# R을 이용한 선거 데이터 (경상남도 남해군) 3단계 시각화 및 도표 삽입
# R을 이용한 선거 데이터 (충청남도 아산시) 3단계 시각화 및 도표 삽입
# R을 이용한 선거 데이터 (서울특별시 도봉구) 3단계 시각화 및 도표 삽입

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
# serviceName = "LSH0214"
# serviceName = "LSH0287"
serviceName = "LSH0517"

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

#================================================
# 비즈니스 로직 수행
#================================================
# 라이브러리 읽기
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(openxlsx)
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggplot2)
library(lubridate)
library(raster)
# devtools::install_github("cran/rgeos")
library(rgeos)
library(maptools)
library(rgdal)
library(sf)
library(ggmap)
library(ggcharts)
library(scales)
library(raster)
library(cowplot)
library(patchwork)
library(scatterpie)
library(readxl)
library(ggplot2) 
library(grid)
library(gridExtra) 
library(cowplot) 
library(RSelenium)
library(rvest)
library(stringr)
library(XML)
library(tidyverse)
library(httr)
library(AER)
library(forcats)
library(zoo)
library(openxlsx)


# 읍면동 지도 읽기
mapInfo = Sys.glob(file.path(globalVar$mapPath, "LSMD/경기도/LSMD_ADM_SECT_UMD_41_202401.shp"))

# shp 파일 읽기 (2)
mapGlobal = sf::st_read(mapInfo, quiet = TRUE, options = "ENCODING=EUC-KR") %>% 
  sf::st_transform(sp::CRS("+proj=longlat"))

#   # 통합 데이터셋
#   dataL5 = mapGlobal %>%
#     dplyr::inner_join(codeDataL1, by = c("adm_dr_cd" = "읍면동코드")) %>%
#     dplyr::left_join(dataL4, by = c("adm_dr_nm" = "투표구2")) 
#   


#=================================================
# 시군구/읍면동 설정
#=================================================
# 선거 데이터 읽기
addrName = "서울특별시"
# addrDtlName = "용산구"
# addrDtlName = "동대문구"
# addrDtlName = "강서구"
# addrDtlName = "도봉구"
# addrDtlName = "도봉구을"
addrDtlName = "마포구"

# addrName = "경기도"
# addrDtlName = "안성시"
# addrDtlName = "동두천시"
# addrDtlName = "부천시"

# addrName = "경상남도"
# addrDtlName = "남해군"

# addrName = "충청남도"
# addrDtlName = "아산시"

# 세부 투표구
# addrDtlVoteName = c("쌍문2동", "쌍문4동", "방학1동", "방학2동", "방학3동", "도봉1동", "도봉2동")

# **********************************************
# 대선 데이터
# **********************************************
# fileName = "중앙선거관리위원회_제20대 대통령선거 투표구별 개표자료_20220309.xlsx"
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, fileName))
# 
# # data = xlsx::read.xlsx(fileInfo, sheetName = "Data", encoding="UTF-8") %>% 
# #   as.tibble()
# # data = xlsx::read.xlsx2(fileInfo, sheetName = "Data", encoding="UTF-8") %>%
#   # as.tibble()
# data = openxlsx::read.xlsx(fileInfo, sheet = 1) %>% 
#   as.tibble()
# 
# # colnames(data)
# 
# dataL1 = data %>% 
#   dplyr::filter(
#     stringr::str_detect(시도, regex(addrName))
#     , stringr::str_detect(구시군, regex(addrDtlName))
#   ) %>% 
#   dplyr::select(tidyselect::matches("시도|구시군|읍면동명|투표구명|선거인수|투표수|국민의힘.윤석열|더불어민주당.이재명")) %>%
#   dplyr::select(-무효투표수) %>% 
#   readr::type_convert()
# 
# dataL2 = dataL1 %>% 
#   dplyr::filter(
#     stringr::str_detect(투표구명, regex("NA|소계|관내사전투표"))
#   ) %>% 
#   dplyr::mutate(
#     읍면동명 = zoo::na.locf(읍면동명)
#     , addr = sprintf("%s-%s-%s", 시도, 구시군, 읍면동명)
#   ) %>% 
#   dplyr::select(-c("시도", "구시군", "읍면동명"))
# 
# dataL3 = dataL2 %>%
#   dplyr::group_by(addr) %>%
#   dplyr::summarise(across(everything(), list(소계 = ~.x[투표구명 == "소계"], 
#                                       관내 = ~.x[투표구명 == "관내사전투표"]))) %>% 
#   dplyr::mutate(선거인수_일반 = 선거인수_소계 - 선거인수_관내,
#          투표수_일반 = 투표수_소계 - 투표수_관내,
#          국민의힘.윤석열_일반  = 국민의힘.윤석열_소계  - 국민의힘.윤석열_관내,
#          더불어민주당.이재명_일반 = 더불어민주당.이재명_소계 - 더불어민주당.이재명_관내) %>%
#   dplyr::select(addr, 선거인수_일반, 투표수_일반, 국민의힘.윤석열_일반, 더불어민주당.이재명_일반) %>% 
#   rename_with(~str_replace(.x, "_일반", ""), ends_with("일반")) %>%
#   dplyr::mutate(투표구명 = "일반")
#   
# headData = dataL1 %>% 
#   dplyr::filter(
#     stringr::str_detect(읍면동명, regex("합계|거소·선상투표|관외사전투표|재외투표"))
#   ) 
# 
# bodyData = dplyr::bind_rows(dataL2, dataL3) %>%
#   tidyr::separate(col = addr, into = c("시도", "구시군", "읍면동명"), sep = "-") %>%
#   dplyr::filter(
#     stringr::str_detect(읍면동명, regex(paste(addrDtlVoteName, collapse = "|")))
#   ) %>% 
#   dplyr::select(c("시도", "구시군", "읍면동명", "투표구명", "선거인수", "투표수", "국민의힘.윤석열", "더불어민주당.이재명")) %>% 
#   dplyr::mutate(
#     투표구명 = factor(투표구명, levels = c("소계", "관내사전투표", "일반"))
#     , 읍면동명 = factor(읍면동명, levels = addrDtlVoteName)
#   ) %>% 
#   dplyr::arrange(시도, 구시군, 읍면동명, 투표구명)
# 
# tailData = dataL1 %>% 
#   dplyr::filter(
#     stringr::str_detect(읍면동명, regex("잘못 투입·구분된 투표지"))
#   ) 
# 
# dataL4 = dplyr::bind_rows(headData, bodyData, tailData)


# **********************************************
# 국회의원 지역구 데이터
# **********************************************
# fileName = "중앙선거관리위원회 국회의원선거 개표결과 정보_20200415/지역구/1서울/개표상황(투표구별)_도봉구을.xlsx"
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, fileName))
# 
# data = openxlsx::read.xlsx(fileInfo, sheet = 1, startRow = 4) %>% 
#   as.tibble()
# 
# # colnames(data)
# 
# dataL1 = data %>% 
#   dplyr::select(tidyselect::matches("읍면동명|투표구명|선거인수|투표수|국민의힘.윤석열|더불어민주당.이재명")) %>%
#   dplyr::select(-무효투표수) %>% 
#   readr::type_convert()
# 
# dataL2 = dataL1 %>% 
#   dplyr::filter(
#     stringr::str_detect(투표구명, regex("NA|소계|관내사전투표"))
#   ) %>% 
#   dplyr::mutate(
#     읍면동명 = zoo::na.locf(읍면동명)
#     , addr = sprintf("%s-%s-%s", 시도, 구시군, 읍면동명)
#   ) %>% 
#   dplyr::select(-c("시도", "구시군", "읍면동명"))
# 
# dataL3 = dataL2 %>%
#   dplyr::group_by(addr) %>%
#   dplyr::summarise(across(everything(), list(소계 = ~.x[투표구명 == "소계"], 
#                                              관내 = ~.x[투표구명 == "관내사전투표"]))) %>% 
#   dplyr::mutate(선거인수_일반 = 선거인수_소계 - 선거인수_관내,
#                 투표수_일반 = 투표수_소계 - 투표수_관내,
#                 국민의힘.윤석열_일반  = 국민의힘.윤석열_소계  - 국민의힘.윤석열_관내,
#                 더불어민주당.이재명_일반 = 더불어민주당.이재명_소계 - 더불어민주당.이재명_관내) %>%
#   dplyr::select(addr, 선거인수_일반, 투표수_일반, 국민의힘.윤석열_일반, 더불어민주당.이재명_일반) %>% 
#   rename_with(~str_replace(.x, "_일반", ""), ends_with("일반")) %>%
#   dplyr::mutate(투표구명 = "일반")
# 
# headData = dataL1 %>% 
#   dplyr::filter(
#     stringr::str_detect(읍면동명, regex("합계|거소·선상투표|관외사전투표|재외투표"))
#   ) 
# 
# bodyData = dplyr::bind_rows(dataL2, dataL3) %>%
#   tidyr::separate(col = addr, into = c("시도", "구시군", "읍면동명"), sep = "-") %>%
#   dplyr::filter(
#     stringr::str_detect(읍면동명, regex(paste(addrDtlVoteName, collapse = "|")))
#   ) %>% 
#   dplyr::select(c("시도", "구시군", "읍면동명", "투표구명", "선거인수", "투표수", "국민의힘.윤석열", "더불어민주당.이재명")) %>% 
#   dplyr::mutate(
#     투표구명 = factor(투표구명, levels = c("소계", "관내사전투표", "일반"))
#     , 읍면동명 = factor(읍면동명, levels = addrDtlVoteName)
#   ) %>% 
#   dplyr::arrange(시도, 구시군, 읍면동명, 투표구명)
# 
# tailData = dataL1 %>% 
#   dplyr::filter(
#     stringr::str_detect(읍면동명, regex("잘못 투입·구분된 투표지"))
#   ) 
# 
# dataL4 = dplyr::bind_rows(headData, bodyData, tailData)

# **********************************************
# 20대 대선 다운로드
# **********************************************
# 셀레늄 이용
# rD = RSelenium::rsDriver(port = 5002L, browser="chrome", chromever=binman::list_versions("chromedriver")$win32[2])
# remDr = rD[["client"]]
# remDr$navigate("http://info.nec.go.kr/main/showDocument.xhtml?electionId=0020220309&topMenuId=VC&secondMenuId=VCCP08")
# 
# Sys.setlocale("LC_ALL", "English")
# 
# data = remDr$getPageSource()[[1]] %>%
#   xml2::read_html() %>%
#   rvest::html_nodes(xpath = '/html/body/div[2]/div/div[4]/div/div/div[2]/div[2]/div[2]/table') %>%
#   rvest::html_table() %>%
#   as.data.frame()
# 
# Sys.setlocale("LC_ALL", "Korean")
# 
# saveFile = sprintf("%s/%s_%s_%s_%s.csv", globalVar$outPath, serviceName, "20대선", addrName, addrDtlName)
# # saveFile = sprintf("%s/%s_%s_%s_%s.csv", globalVar$outPath, serviceName, "39국회의원", addrName, addrDtlName)
# saveTmp = tempfile(fileext = "csv")
# readr::write_excel_csv(x = data, file = saveTmp, col_names = FALSE)
# fs::file_copy(saveTmp, saveFile, overwrite = TRUE)

#=================================================
# 선거 주제도
#=================================================
fileInfoPattern = sprintf("*%s %s* 선거분석.xlsx", addrName, addrDtlName)
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, fileInfoPattern))
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, fileInfoPattern))[1]

# data = openxlsx::read.xlsx(fileInfo, sheet = 1)
# data = readxl::read_excel(fileInfo, sheet = 1)
data = xlsx::read.xlsx(fileInfo, sheetIndex = 2, encoding="UTF-8") %>% 
  as.tibble()

colnames(data) = c("종류", "투표구", "세부투표구", "투표자수", "자유한국당", "더불어민주당",  "%자유한국당", "%더불어민주당",	 "중도층",	"%중도층")

# 세부 투표구에 대한 위/경도 반환
# dataGeoL1 = dataGeo %>% 
#   dplyr::mutate(
#     addr = stringr::str_c(주소, 건물명, sep = " ")
#   )

# addrList = dataGeoL1$addr%>% unique %>% sort %>%
#   as.tibble()

# saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "서울시 강서구 투표구 정보")

# 각 주소에 따라 위/경도 반환
# for (i in 1:nrow(addrList)) {
# 
#   # 구글 API 하루 제한
#   addrData = ggmap::mutate_geocode(addrList[i, 'value'], value, source = "google")
# 
#   if (nrow(addrData) < 1) { next }
# 
#   readr::write_csv(x = addrData, file = saveFile, append = TRUE)
# }

# addrData = readr::read_csv(file = saveFile, col_names = c("value", "lon", "lat"))

# dataGeoL2 = dataGeoL1 %>% 
#   dplyr::left_join(addrData, by = c("addr" = "value"))

# summary(dataGeoL2)

dataL1 = data %>%
  as.tibble() %>%
  readr::type_convert()

dataL2 = dataL1 %>%
  dplyr::filter(세부투표구 %in% c("소계")) %>% 
  rowwise() %>% 
  dplyr::mutate(
    중도층 = sum(dplyr::c_across(matches("중도층")), na.rm = TRUE)
  ) %>% 
  dplyr::select(-tidyselect::matches("중도층[0-9]")) %>% 
  dplyr::select(-c(종류)) %>% 
  tidyr::gather(-c(투표구, 세부투표구), key = "key", value = "val") %>% 
  dplyr::group_by(투표구, key) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  ) %>% 
  dplyr::ungroup() %>% 
  tidyr::spread(key = "key", value = "meanVal")

dataL3 = data %>% 
  dplyr::filter(세부투표구 %in% c("소계")) %>% 
  rowwise(투표구) %>% 
  dplyr::mutate(
    maxVal = max(더불어민주당, 자유한국당, 중도층, na.rm = TRUE)
    , val = dplyr::case_when(
      자유한국당 == maxVal ~ "자유한국당"
      , 더불어민주당 == maxVal ~ "더불어민주당"
      , 중도층 == maxVal ~ "중도층"
    )
    # , 투표구3 = gsub("*제[[:digit:]]+동", "", 투표구)# %>% str_replace(투표구, ., "")
    # , 투표구3 = ifelse(grepl("제[[:digit:]]+동", 투표구), str_replace(투표구, ., ""), 투표구)
    , 투표구2 = dplyr::case_when(
      # stringr::str_detect(투표구, regex("원효로제1동")) ~ "원효로1동"
      # , stringr::str_detect(투표구, regex("원효로제2동")) ~ "원효로2동"
      # , stringr::str_detect(투표구, regex("이촌제1동")) ~ "이촌1동"
      # , stringr::str_detect(투표구, regex("이촌제2동")) ~ "이촌2동"
      # , stringr::str_detect(투표구, regex("이태원제1동")) ~ "이태원1동"
      # , stringr::str_detect(투표구, regex("이태원제2동")) ~ "이태원2동"
      # , TRUE ~ 투표구
      
      grepl("원효로제1동", 투표구) ~ "원효로1동"
      , grepl("원효로제2동", 투표구) ~ "원효로2동"
      , grepl("이촌제1동", 투표구) ~ "이촌1동"
      , grepl("이촌제2동", 투표구) ~ "이촌2동"
      , grepl("이태원제1동", 투표구) ~ "이태원1동"
      , grepl("이태원제2동", 투표구) ~ "이태원2동"
      
      , grepl("휘경제1동", 투표구) ~ "휘경1동"
      , grepl("휘경제2동", 투표구) ~ "휘경2동"
      , grepl("전농제1동", 투표구) ~ "전농1동"
      , grepl("전농제2동", 투표구) ~ "전농2동"
      , grepl("답십리제1동", 투표구) ~ "답십리1동"
      , grepl("답십리제2동", 투표구) ~ "답십리2동"
      
      , grepl("장안제1동", 투표구) ~ "장안1동"
      , grepl("장안제2동", 투표구) ~ "장안2동"
      , grepl("이문제1동", 투표구) ~ "이문1동"
      , grepl("이문제2동", 투표구) ~ "이문2동"
      
      , grepl("가양제1동", 투표구) ~ "가양1동"
      , grepl("가양제2동", 투표구) ~ "가양2동"
      , grepl("가양제3동", 투표구) ~ "가양3동"
      , grepl("등촌제1동", 투표구) ~ "등촌1동"
      , grepl("등촌제2동", 투표구) ~ "등촌2동"
      , grepl("등촌제3동", 투표구) ~ "등촌3동"
      , grepl("발산제1동", 투표구) ~ "발산1동"
      , grepl("방화제1동", 투표구) ~ "방화1동"
      , grepl("방화제2동", 투표구) ~ "방화2동"
      , grepl("방화제3동", 투표구) ~ "방화3동"
      , grepl("화곡제1동", 투표구) ~ "화곡1동"
      , grepl("화곡제2동", 투표구) ~ "화곡2동"
      , grepl("화곡제3동", 투표구) ~ "화곡3동"
      , grepl("화곡제4동", 투표구) ~ "화곡4동"
      , grepl("화곡제6동", 투표구) ~ "화곡6동"
      , grepl("화곡제8동", 투표구) ~ "화곡8동"
      
      , TRUE ~ 투표구
    )
  )

# data$투표구 %>% unique() %>% sort()

# 읍면동 지도 읽기
mapInfo = Sys.glob(file.path(globalVar$mapPath, "koreaInfo/bnd_dong_00_2019_2019_2Q.shp"))

# LAWD_CD	법정동코드
# SIDO_NM	시도명
# SGG_NM	시군구명
# UMD_NM	읍면동명
# RI_NM	리명
# CRE_DT	생성 일자
# DEL_DT	말소 일자
# OLD_LAWDCD	구법정동코드
# FRST_REGIST_DT	최초 등록일시
# LAST_UPDT_DT	최종 수정일시
# mapInfo = Sys.glob(file.path(globalVar$mapPath, "LSMD/경기도/*.shp"))

# shp 파일 읽기 (2)
mapGlobal = sf::st_read(mapInfo, quiet = TRUE, options = "ENCODING=EUC-KR") %>% 
  sf::st_transform(sp::CRS("+proj=longlat"))

# 법정동 코드 읽기 (2)
codeInfo = Sys.glob(file.path(globalVar$mapPath, "admCode/admCode.xlsx"))
codeData = openxlsx::read.xlsx(codeInfo, sheet = 1, startRow = 2) %>% 
  as.tibble()

# 행정표준코드관리시스템: https://www.code.go.kr/stdcode/regCodeL.do
# codeInfo = Sys.glob(file.path(globalVar$mapPath, "admCode/20240119_법정동코드 전체자료.txt"))

# codeData = readr::read_delim(codeInfo, delim = "\t", locale = locale("ko", encoding = "EUC-KR"), col_types = "ccc") %>%
#   magrittr::set_colnames(c("EMD_CD", "addr", "isUse")) %>%
#     tidyr::separate(col = "addr", into = c("시도", "시군구", "읍면동", "리"), sep = " ") %>%
#     dplyr::mutate(
#       emdCd = stringr::str_sub(EMD_CD, 1, 8)
#     ) %>%
#     dplyr::filter(
#       ! is.na(읍면동)
#       , is.na(리)
#       , isUse == "존재"
    # )


# codeInfo = Sys.glob(file.path(globalVar$mapPath, "LSMD/LSCT_LAWDCD.csv"))
# codeData = readr::read_csv(codeInfo, locale = locale("ko", encoding = "EUC-KR"))

codeDataL1 = codeData %>%
  dplyr::filter(
    # stringr::str_detect(시도명칭, regex(addrName))
    # , stringr::str_detect(시군구명칭, regex(addrDtlName))
    grepl(addrName, 시도명칭)
    , grepl(addrDtlName, 시군구명칭)
    # grepl(addrName, 시도)
    # , grepl(addrDtlName, 시군구)
    # grepl(addrName, SIDO_NM)
    # , grepl(addrDtlName, SGG_NM)
  )

# 통합 데이터셋
dataL5 = mapGlobal %>%
  dplyr::inner_join(codeDataL1, by = c("adm_dr_cd" = "읍면동코드")) %>%
  dplyr::left_join(dataL3, by = c("adm_dr_nm" = "투표구2")) %>%
  # dplyr::inner_join(codeDataL1, by = c("EMD_CD" = "emdCd")) %>%
  # dplyr::left_join(dataL3, by = c("EMD_NM" = "투표구2")) %>% 
  dplyr::filter(
    ! is.na(투표자수)
  )

# dplyr::tbl_df(dataL5)

# ************************************************
# 선거 주제도
# ************************************************
plotSubTitle = sprintf("%s %s 선거 주제도",addrName, addrDtlName)
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
saveTmp = tempfile(fileext = ".png")

# ggplotDefaultColor = hue_pal()(3)
ggplotDefaultColor = c("red", "blue", "grey")

ggplot() +
  theme_bw() +
  coord_fixed(ratio = 1) +
  # geom_sf(data = dataL5, aes(fill = factor(val)), inherit.aes = FALSE, alpha = 0.3) +
  geom_sf(data = dataL5, aes(fill = val), inherit.aes = FALSE, alpha = 1.0, color = "white") +
  geom_sf_text(data = dataL5, aes(label = 읍면동명칭), color = "white") +
  # geom_point(data = dataDtlL3, aes(x = lon, y = lat, color = factor(val)), shape = 16, show.legend = FALSE) +
  # ggrepel::geom_label_repel(
  #   data = dataDtlL3
  #   , aes(x = lon, y = lat, fill = factor(val), label = label)
  #   , color = "white"
  #   , segment.color = "black"
  #   , show.legend = FALSE
  #   , segment.size = 0.2
  #   , size = 3
  # ) +
  scale_fill_manual(
    name = NULL
    , na.value = "transparent"
    , values = c("자유한국당" = ggplotDefaultColor[1], "더불어민주당" = ggplotDefaultColor[2], "중도층" = ggplotDefaultColor[3])
    , labels = c("자유한국당", "더불어민주당", "중도층")
    , breaks = c("자유한국당", "더불어민주당", "중도층")
  ) +
  scale_color_manual(
    name = NULL
    , na.value = "transparent"
    , values = c("자유한국당" = ggplotDefaultColor[1], "더불어민주당" = ggplotDefaultColor[2], "중도층" = ggplotDefaultColor[3])
  ) +
  # xlim(127.80, 128.08) +
  # ylim(34.69, 34.95) +
  labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = NULL, subtitle = NULL) +
  theme(
    text = element_text(size = 16)
    , panel.grid.major.x = element_blank()
    , panel.grid.major.y = element_blank()
    , panel.grid.minor.x = element_blank()
    , panel.grid.minor.y = element_blank()
    , axis.text.x = element_blank()
    , axis.ticks.x = element_blank()
    , axis.title.x = element_blank()
    , axis.text.y = element_blank()
    , axis.ticks.y = element_blank()
    , axis.title.y = element_blank()
    , plot.subtitle = element_text(hjust = 1)
    , legend.position = "top"
  ) +
  ggsave(filename = saveTmp, width = 8, height = 8, dpi = 600)

fs::file_move(saveTmp, saveImg)
# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ************************************************
# 선거 빈도분포
# ************************************************
dataDtlL4 = data %>% 
  dplyr::filter(! 세부투표구 %in% c("소계")) %>% 
  # rowwise(투표구) %>%
  dplyr::mutate(
    투표구2 = dplyr::case_when(
      # stringr::str_detect(투표구, regex("원효로제1동")) ~ "원효로1동"
      # , stringr::str_detect(투표구, regex("원효로제2동")) ~ "원효로2동"
      # , stringr::str_detect(투표구, regex("이촌제1동")) ~ "이촌1동"
      # , stringr::str_detect(투표구, regex("이촌제2동")) ~ "이촌2동"
      # , stringr::str_detect(투표구, regex("이태원제1동")) ~ "이태원1동"
      # , stringr::str_detect(투표구, regex("이태원제2동")) ~ "이태원2동"
      
      grepl("원효로제1동", 투표구) ~ "원효로1동"
      , grepl("원효로제2동", 투표구) ~ "원효로2동"
      , grepl("이촌제1동", 투표구) ~ "이촌1동"
      , grepl("이촌제2동", 투표구) ~ "이촌2동"
      , grepl("이태원제1동", 투표구) ~ "이태원1동"
      , grepl("이태원제2동", 투표구) ~ "이태원2동"
      
      , grepl("휘경제1동", 투표구) ~ "휘경1동"
      , grepl("휘경제2동", 투표구) ~ "휘경2동"
      , grepl("전농제1동", 투표구) ~ "전농1동"
      , grepl("전농제2동", 투표구) ~ "전농2동"
      , grepl("답십리제1동", 투표구) ~ "답십리1동"
      , grepl("답십리제2동", 투표구) ~ "답십리2동"
      
      , grepl("장안제1동", 투표구) ~ "장안1동"
      , grepl("장안제2동", 투표구) ~ "장안2동"
      , grepl("이문제1동", 투표구) ~ "이문1동"
      , grepl("이문제2동", 투표구) ~ "이문2동"
      
      , grepl("가양제1동", 투표구) ~ "가양1동"
      , grepl("가양제2동", 투표구) ~ "가양2동"
      , grepl("가양제3동", 투표구) ~ "가양3동"
      , grepl("등촌제1동", 투표구) ~ "등촌1동"
      , grepl("등촌제2동", 투표구) ~ "등촌2동"
      , grepl("등촌제3동", 투표구) ~ "등촌3동"
      , grepl("발산제1동", 투표구) ~ "발산1동"
      , grepl("방화제1동", 투표구) ~ "방화1동"
      , grepl("방화제2동", 투표구) ~ "방화2동"
      , grepl("방화제3동", 투표구) ~ "방화3동"
      , grepl("화곡제1동", 투표구) ~ "화곡1동"
      , grepl("화곡제2동", 투표구) ~ "화곡2동"
      , grepl("화곡제3동", 투표구) ~ "화곡3동"
      , grepl("화곡제4동", 투표구) ~ "화곡4동"
      , grepl("화곡제6동", 투표구) ~ "화곡6동"
      , grepl("화곡제8동", 투표구) ~ "화곡8동"
      
      , TRUE ~ 투표구
    )
    # , label = stringr::str_match_all(세부투표구, "제[[:digit:]]+투") %>% unlist() %>% stringr::str_conv("EUC-KR")
    , label = gsub("제[[:digit:]]+투", "", 세부투표구) %>% str_replace(세부투표구, ., "")
  ) %>% 
  # dplyr::na_if(0) %>% 
  dplyr::select(투표구2, 세부투표구, `%자유한국당`, `%더불어민주당`, `%중도층`, label) %>% 
  tidyr::gather(-c(투표구2, 세부투표구, label), key = "key", value = "val") %>% 
  dplyr::mutate(
    val = ifelse(is.na(val), 0, val)
  )

# dplyr::tbl_df(dataDtlL4)


# 정당에 따른 정렬
dataDtlL4$key = forcats::fct_relevel(dataDtlL4$key, rev(c("%자유한국당", "%더불어민주당", "%중도층")))
# dataDtlL4$key = forcats::fct_relevel(dataDtlL4$key, c("%자유한국당", "%더불어민주당", "%중도층"))

selLabel = paste0("제", c(1:99), "투")
dataDtlL4$label = forcats::fct_relevel(dataDtlL4$label, selLabel)

plotSubTitle = sprintf("%s %s 선거 빈도분포", addrName, addrDtlName)
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
saveTmp = tempfile(fileext = ".png")

ggplot(dataDtlL4, aes(x = label, y = val, fill = key, group = key, label = round(val, 0))) +
  geom_bar(position = position_stack(), stat = "identity") +
  # geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), size = 4, color = "white") +
  coord_flip() +
  labs(x = "세부 투표구", y = "비율", fill = NULL, subtitle = plotSubTitle) +
  theme(
    text = element_text(size = 14)
    , legend.position = "top"
    , axis.ticks.x = element_blank()
    , axis.text.x = element_blank()
  ) +
  scale_fill_manual(
    name = NULL
    , na.value = "transparent"
    # , values = c("%자유한국당" = ggplotDefaultColor[1], "%더불어민주당" = ggplotDefaultColor[3], "%중도층" = "gray")
    , values = c("%자유한국당" = ggplotDefaultColor[1], "%더불어민주당" = ggplotDefaultColor[2], "%중도층" = ggplotDefaultColor[3])
    , labels = c("자유한국당", "더불어민주당", "중도층")
    , breaks = c("%자유한국당", "%더불어민주당", "%중도층")
  ) +
  # facet_wrap(~투표구2, scale = "free", ncol = 3) +
  facet_wrap(~투표구2, scale = "free", ncol = 4) +
  ggsave(filename = saveTmp, width = 16, height = 12, dpi = 600)

fs::file_move(saveTmp, saveImg)
# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

#=================================================
# 인구현황
#=================================================
# [행정안전부] 주민등록 인구통계 : https://jumin.mois.go.kr
# [검색조건] 연령별 인구현황
#   행정구역 (경상북도-남해군, 서울특별시-도봉구)
#   등록구분 (거주자)
#   연간 (2021년, 2022년, 2023년)
#   구분 (남/여 구분)
#   연령 구분 단위 (1세)
#   만 연령구분 (0, 100이상)
# [검색]
# [다운로드] csv 파일 다운로드
# [바꾸기] 1행 컬럼 (2021년_ > 공백, 2022년_ > 공백, 2023년_ > 공백)
# [바꾸기] 1열 컬럼 (텍스트 나누기)
# [수정] 행정구역 수정 (쌍문제1동 -> 쌍문1동)


# addrName = "경상남도"
# addrDtlName = "남해군"

# 선거 데이터 읽기
# fileInfoPattern = sprintf("*%s %s* 선거분석.xlsx", addrName, addrDtlName)
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, fileInfoPattern))
# data = xlsx::read.xlsx(fileInfo, sheetIndex = 3, encoding = "UTF-8")
data = openxlsx::read.xlsx(fileInfo, sheet = "인구현황")

# fileInfoPattern = sprintf("선거분석 (%s %s).csv", addrName, addrDtlName)
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, fileInfoPattern))
# data = readr::read_csv(file = fileInfo)

dataL1 = data %>%
  as.tibble() %>%
  na.omit() %>%
  readr::type_convert() %>% 
  dplyr::rename(
    투표구 = 행정구역
  )

# sexListPattern = c("남", "여", "남|여")
sexListPattern = c("남", "여")
# sexInfoPattern = "남"
saveDataL1 = tibble::tibble()

for (sexInfoPattern in sexListPattern) {
  
  # 한글변환 문제
  # sexInfo = stringr::str_replace(sexInfoPattern, "\\|", "") %>% unlist()
  sexInfo = gsub("\\|", "", sexInfoPattern)
  
  dataL2 = dataL1 %>%
    dplyr::select(투표구, tidyselect::matches("[[:digit:]]+세")) %>% 
    tidyr::gather(-투표구, key = "key", value = "투표수") %>% 
    dplyr::mutate(
      age = stringr::str_match_all(key, "[[:digit:]]+") %>% unlist()
      # , sex = stringr::str_match_all(key, "^남|여") %>% unlist()
      , sex = gsub("[^남|여]", "", key) %>% stringr::str_trim(side = c("both"))
      # , isSex = dplyr::case_when(
      #   stringr::str_detect(sex, regex(sexInfoPattern)) ~ TRUE
      #   , TRUE ~ FALSE
      # )
      , isSex = grepl(sexInfoPattern, sex)
    ) %>% 
    dplyr::filter(isSex == TRUE) %>%
    dplyr::mutate(
      type = dplyr::case_when(
        18 <= age & age <= 20 ~ "18-20세"
        , 21 <= age & age <= 30 ~ "21-30세"
        , 31 <= age & age <= 40 ~ "31-40세"
        , 41 <= age & age <= 50 ~ "41-50세"
        , 51 <= age & age <= 60 ~ "51-60세"
        , 61 <= age & age <= 70 ~ "61-70세"
        , 71 <= age ~ "71세 이상"
      )
    ) %>% 
    dplyr::filter(
      ! is.na(age)
      , ! is.na(type)
    ) %>% 
    dplyr::select(-age)
  
  statData = dataL2 %>%
    dplyr::group_by(투표구, type) %>%
    dplyr::summarise(
      sumKeyVal = sum(투표수, na.rm = TRUE) 
    )
  
  statDataL2 = dataL2 %>% 
    dplyr::group_by(투표구) %>% 
    dplyr::summarise(
      sumVal = sum(투표수, na.rm = TRUE) 
    )
  
  dataL4 = statData %>% 
    dplyr::left_join(statDataL2, by = c("투표구" = "투표구")) %>% 
    tidyr::spread(key = "type", value = "sumKeyVal") %>% 
    dplyr::mutate(
      투표구2 = dplyr::case_when(
        # stringr::str_detect(투표구, regex("원효로제1동")) ~ "원효로1동"
        # , stringr::str_detect(투표구, regex("원효로제2동")) ~ "원효로2동"
        # , stringr::str_detect(투표구, regex("이촌제1동")) ~ "이촌1동"
        # , stringr::str_detect(투표구, regex("이촌제2동")) ~ "이촌2동"
        # , stringr::str_detect(투표구, regex("이태원제1동")) ~ "이태원1동"
        # , stringr::str_detect(투표구, regex("이태원제2동")) ~ "이태원2동"
        # , TRUE ~ 투표구
        
        grepl("원효로제1동", 투표구) ~ "원효로1동"
        , grepl("원효로제2동", 투표구) ~ "원효로2동"
        , grepl("이촌제1동", 투표구) ~ "이촌1동"
        , grepl("이촌제2동", 투표구) ~ "이촌2동"
        , grepl("이태원제1동", 투표구) ~ "이태원1동"
        , grepl("이태원제2동", 투표구) ~ "이태원2동"
        
        , grepl("휘경제1동", 투표구) ~ "휘경1동"
        , grepl("휘경제2동", 투표구) ~ "휘경2동"
        , grepl("전농제1동", 투표구) ~ "전농1동"
        , grepl("전농제2동", 투표구) ~ "전농2동"
        , grepl("답십리제1동", 투표구) ~ "답십리1동"
        , grepl("답십리제2동", 투표구) ~ "답십리2동"
        
        , grepl("장안제1동", 투표구) ~ "장안1동"
        , grepl("장안제2동", 투표구) ~ "장안2동"
        , grepl("이문제1동", 투표구) ~ "이문1동"
        , grepl("이문제2동", 투표구) ~ "이문2동"
        
        , grepl("가양제1동", 투표구) ~ "가양1동"
        , grepl("가양제2동", 투표구) ~ "가양2동"
        , grepl("가양제3동", 투표구) ~ "가양3동"
        , grepl("등촌제1동", 투표구) ~ "등촌1동"
        , grepl("등촌제2동", 투표구) ~ "등촌2동"
        , grepl("등촌제3동", 투표구) ~ "등촌3동"
        , grepl("발산제1동", 투표구) ~ "발산1동"
        , grepl("방화제1동", 투표구) ~ "방화1동"
        , grepl("방화제2동", 투표구) ~ "방화2동"
        , grepl("방화제3동", 투표구) ~ "방화3동"
        , grepl("화곡제1동", 투표구) ~ "화곡1동"
        , grepl("화곡제2동", 투표구) ~ "화곡2동"
        , grepl("화곡제3동", 투표구) ~ "화곡3동"
        , grepl("화곡제4동", 투표구) ~ "화곡4동"
        , grepl("화곡제6동", 투표구) ~ "화곡6동"
        , grepl("화곡제8동", 투표구) ~ "화곡8동"
        
        , TRUE ~ 투표구
      )
    )
  
  saveData = dataL4 %>%
    dplyr::rename(
      합계 = sumVal
    ) %>% 
    dplyr::mutate(
      성별 = sexInfo
    )
  
  # 데이터 병합
  saveDataL1 = dplyr::bind_rows(saveDataL1, saveData)
  
  
  # ****************************************************************************
  # 인구현황 배경지도
  # ****************************************************************************
  typeList = dataL4$투표구2 %>% unique() %>% sort()
  
  plotSubTitle = sprintf("%s %s 인구현황 막대 (%s)", addrName, addrDtlName, sexInfo)
  saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
  dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
  saveTmp = tempfile(fileext = ".png")
  
  makePlotBg = ggplot() +
    theme_bw() +
    coord_fixed(ratio = 1) +
    geom_sf(data = dataL5, fill = NA, inherit.aes = FALSE) +
    geom_sf_text(data = dataL5, aes(label = 읍면동명칭)) +
    labs(
      x = NULL
      , y = NULL
      , color = NULL
      , fill = NULL
      , subtitle = plotSubTitle
    ) +
    # xlim(127.80, 128.08) +
    # ylim(34.69, 34.95) +
    theme(
      text = element_text(size = 14)
      , panel.border = element_blank()
      , panel.grid = element_blank()
      , panel.grid.major.x = element_blank()
      , panel.grid.major.y = element_blank()
      , panel.grid.minor.x = element_blank()
      , panel.grid.minor.y = element_blank()
      , axis.text.x = element_blank()
      , axis.ticks.x = element_blank()
      , axis.title.x = element_blank()
      , axis.text.y = element_blank()
      , axis.ticks.y = element_blank()
      , axis.title.y = element_blank()
      , legend.position = "top"
      , legend.box = "horizontal"
      , plot.margin = unit(c(0.2, 0, 0, 0), "lines")
    )
  
  ggsave(makePlotBg, filename = saveTmp, width = 8, height = 8, dpi = 600)
  fs::file_move(saveTmp, saveImg)
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
  
  # ****************************************************************************
  # 인구현황 막대 그래프
  # ****************************************************************************
  # typeInfo = typeList[1]
  for (typeInfo in typeList) {
    
    dataL6 = dataL4 %>%
      dplyr::ungroup() %>%
      dplyr::filter(투표구2 == typeInfo) %>%
      dplyr::select(-c("투표구", "sumVal")) %>%
      tidyr::gather(-투표구2, key = "key", value = "val") %>% 
      dplyr::mutate(
        per = val / sum(val, na.rm = TRUE) * 100.0
      )
    
    cbSet1 = RColorBrewer::brewer.pal(7, "Set1")
    
    plotSubTitle = sprintf("%s %s 인구현황 막대차트 (%s, %s)", addrName, addrDtlName, sexInfo, typeInfo)
    saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
    dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
    saveTmp = tempfile(fileext = ".png")
    
    makePlot = ggplot(dataL6, aes(x=key, y=per, group=key, fill=key, label = round(per, 0))) +
      geom_bar(position="dodge", stat="identity", show.legend = FALSE, alpha = 1) +
      geom_text(nudge_y = -1.50, color = "black", size = 4) +
      labs(
        x = NULL
        , y = NULL
        , color = NULL
        , fill = NULL
        , subtitle = NULL
      ) +
      geom_label(data=dataL6, aes(x = "41-50세", y = 0, label = 투표구2)
                 , color = alpha("white", 1), vjust=0, nudge_x = 0, nudge_y = 0, label.size = NA
                 , fill = alpha("black", 0.25), size = 5) +
      scale_fill_manual(
        name = NULL
        , na.value = "transparent"
        , values = c("18-20세" = cbSet1[1], "21-30세" = cbSet1[2], "31-40세" = cbSet1[3], "41-50세" = cbSet1[4], "51-60세" = cbSet1[5], "61-70세" = cbSet1[6], "71세 이상" = cbSet1[7])
        , labels = c("18-20세", "21-30세", "31-40세", "41-50세", "51-60세", "61-70세", "71세 이상")
      ) +
      theme_bw() +
      theme(
        text = element_text(size = 16)
        , panel.border = element_blank()
        , panel.grid.major.x = element_blank()
        , panel.grid.major.y = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , axis.text.x = element_blank()
        , axis.ticks.x = element_blank()
        , axis.title.x = element_blank()
        , axis.text.y = element_blank()
        , axis.ticks.y = element_blank()
        , axis.title.y = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , panel.background = element_rect(fill = "transparent")
        , plot.background = element_rect(fill = "transparent", color = NA)
        , legend.background = element_rect(fill = "transparent")
        , legend.box.background = element_rect(fill = "transparent")
      )
    
    ggsave(makePlot, filename = saveTmp, width = 2, height = 2, dpi = 600)
    fs::file_move(saveTmp, saveImg)
    cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
  }
}

# shell.exec(saveImg)

# saveXlsxFile = sprintf("%s/%s_%s_%s_%s.xlsx", globalVar$outPath, serviceName, addrName, addrDtlName, "인구현황")
# 
# wb = openxlsx::createWorkbook()
# openxlsx::addWorksheet(wb, "(결과)인구현황")
# openxlsx::writeData(wb, "(결과)인구현황", saveDataL1, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
# openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)


# ****************************************************************************
# 범례 그리기
# ****************************************************************************
# legendData = tibble::tibble(
#   x = 1:7
#   , y = 1:7
#   , legend = c("18-20세", "21-30세", "31-40세", "41-50세", "51-60세", "61-70세", "71세 이상")
# )
# 
# plotSubTitle = "범례 정보"
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle)
# saveTmp = tempfile(fileext = ".png")
# 
# makePlotLegend = ggplot(legendData, aes(x = x, y = y, fill = legend)) +
#   geom_bar(position="dodge", stat="identity", rsize = 7) +
#   scale_fill_manual(
#     name = NULL
#     , na.value = "transparent"
#     , values = c("18-20세" = cbSet1[1], "21-30세" = cbSet1[2], "31-40세" = cbSet1[3], "41-50세" = cbSet1[4], "51-60세" = cbSet1[5], "61-70세" = cbSet1[6], "71세 이상" = cbSet1[7])
#     , labels = c("18-20세", "21-30세", "31-40세", "41-50세", "51-60세", "61-70세", "71세 이상")
#   )
# 
# getLegend = cowplot::get_legend(makePlotLegend)
# grid::grid.newpage()
# png(file = saveTmp, width = 2, height = 2, units = "in", res = 600, bg = "transparent")
# grid::grid.draw(getLegend)
# dev.off()
# fs::file_move(saveTmp, saveImg)



# ****************************************************************************
# 인구현황 파이 그래프
# ****************************************************************************
# for (sexInfoPattern in sexListPattern) {
#   
#   # 한글변환 문제
#   # sexInfo = stringr::str_replace(sexInfoPattern, "\\|", "") %>% unlist()
#   sexInfo = gsub("\\|", "", sexInfoPattern)
#   
#   dataL2 = dataL1 %>%
#     dplyr::select(투표구, tidyselect::matches("[[:digit:]]+세")) %>% 
#     tidyr::gather(-투표구, key = "key", value = "투표수") %>% 
#     dplyr::mutate(
#       age = stringr::str_match_all(key, "[[:digit:]]+") %>% unlist()
#       # , sex = stringr::str_match_all(key, "^남|여") %>% unlist()
#       , sex = gsub("[^남|여]", "",key) %>% stringr::str_trim(side = c("both"))
#       # , isSex = dplyr::case_when(
#       #   stringr::str_detect(sex, regex(sexInfoPattern)) ~ TRUE
#       #   , TRUE ~ FALSE
#       # )
#       , isSex = grepl(sexInfoPattern, sex)
#     ) %>% 
#     dplyr::filter(isSex == TRUE) %>%
#     dplyr::mutate(
#       type = dplyr::case_when(
#         16 <= age & age <= 20 ~ "16-20세"
#         , 21 <= age & age <= 30 ~ "21-30세"
#         , 31 <= age & age <= 40 ~ "31-40세"
#         , 41 <= age & age <= 50 ~ "41-50세"
#         , 51 <= age & age <= 60 ~ "51-60세"
#         , 61 <= age & age <= 70 ~ "61-70세"
#         , 71 <= age ~ "71세 이상"
#       )
#     ) %>% 
#     dplyr::filter(
#       ! is.na(age)
#       , ! is.na(type)
#     ) %>% 
#     dplyr::select(-age)
#   
#   statData = dataL2 %>%
#     dplyr::group_by(투표구, type) %>%
#     dplyr::summarise(
#       sumKeyVal = sum(투표수, na.rm = TRUE) 
#     )
#   
#   statDataL2 = dataL2 %>% 
#     dplyr::group_by(투표구) %>% 
#     dplyr::summarise(
#       sumVal = sum(투표수, na.rm = TRUE) 
#     )
#   
#   dataL4 = statData %>% 
#     dplyr::left_join(statDataL2, by = c("투표구" = "투표구")) %>% 
#     tidyr::spread(key = "type", value = "sumKeyVal") %>% 
#     dplyr::mutate(
#       투표구2 = dplyr::case_when(
#         stringr::str_detect(투표구, regex("원효로제1동")) ~ "원효로1동"
#         , stringr::str_detect(투표구, regex("원효로제2동")) ~ "원효로2동"
#         , stringr::str_detect(투표구, regex("이촌제1동")) ~ "이촌1동"
#         , stringr::str_detect(투표구, regex("이촌제2동")) ~ "이촌2동"
#         , stringr::str_detect(투표구, regex("이태원제1동")) ~ "이태원1동"
#         , stringr::str_detect(투표구, regex("이태원제2동")) ~ "이태원2동"
#         , TRUE ~ 투표구
#       )
#     )
#   
#   saveData = dataL4 %>%
#     dplyr::rename(
#       합계 = sumVal
#     ) %>% 
#     dplyr::mutate(
#       성별 = sexInfo
#     )
#   
#   codeDataL1 = codeData %>%
#     dplyr::filter(
#       # stringr::str_detect(시도명칭, regex("서울특별시")), stringr::str_detect(시군구명칭, regex("용산구"))
#       stringr::str_detect(시도명칭, regex(addrName)), stringr::str_detect(시군구명칭, regex(addrDtlName))
#     ) 
#   
#   # 통합 데이터셋
#   dataL5 = mapGlobal %>%
#     dplyr::inner_join(codeDataL1, by = c("adm_dr_cd" = "읍면동코드")) %>%
#     dplyr::left_join(dataL4, by = c("adm_dr_nm" = "투표구2")) 
#   
#   # 중심 위/경도 반환
#   posData = sf::st_centroid(dataL5$geometry) %>% 
#     sf::st_coordinates() %>% 
#     as.tibble() %>% 
#     dplyr::rename(
#       "lon" = "X"
#       , "lat" = "Y"
#     )
#   
#   dataL6 = dplyr::bind_cols(dataL5, posData) %>% 
#     dplyr::mutate(
#       xOffset = dplyr::case_when(
#         읍면동명칭 == "대덕면" ~ -0.02
#         , 읍면동명칭 == "금광면" ~ 0.02
#         , 읍면동명칭 == "안성1동" ~ 0.025
#         , TRUE ~ 0
#       )
#       , yOffset = dplyr::case_when(
#         읍면동명칭 == "안성3동" ~ 0.01
#         , TRUE ~ 0
#       )
#     )
#  
#   dataL7 = na.omit(dataL6)
#   
#   dataL8 = dataL7 %>% 
#     as.tibble() %>% 
#     dplyr::mutate(
#       geometry = NULL
#     )
#   
#   # ggplotDefaultColor = c("red", "blue", "grey")
#   
#   plotSubTitle2 = sprintf("%s %s 인구현황 파이차트 (%s)", addrName, addrDtlName, sexInfo)
#   saveImg2 = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, plotSubTitle2)
#   
#   makePiePlot = ggplot() +
#     theme_bw() +
#     coord_fixed(ratio = 1) +
#     geom_sf(data = dataL7, fill = NA, inherit.aes = FALSE) +
#     geom_sf_text(data = dataL7, aes(label = 읍면동명칭)) +
#     # 크기 비율 X
#     scatterpie::geom_scatterpie(
#       aes(x = lon + xOffset, y = lat + yOffset, group = factor(읍면동명칭), r = 0.025)
#       # aes(x = lon, y = lat, group = factor(읍면동명칭), r = 0.025)
#       , cols=c("16-20세", "21-30세", "31-40세", "41-50세", "51-60세", "61-70세", "71세 이상")
#       , data = dataL8, color = NA, alpha = 0.8
#       # , data = dataL8, color = NA, alpha = 0.75
#     ) +
#     # 크기 비율 O
#     # scatterpie::geom_scatterpie(
#     #   aes(x = lon, y = lat, group = factor(읍면동명칭), r = sumVal/5000000)
#     #   , cols=c("18-20세", "21-30세", "31-40세", "41-50세", "51-60세", "61-70세", "71세 이상")
#     #   , data = dataL8, color = NA, alpha = 0.75
#     # ) +
#     scatterpie::geom_scatterpie_legend(
#       dataL8$sumVal/2000000
#       # , x =  min(posData$lon, na.rm = TRUE)
#       # , y = min(posData$lat, na.rm = TRUE)
#       , x =  min(posData$lon, na.rm = TRUE) - 0.02
#       , y = min(posData$lat, na.rm = TRUE) - 0.02
#       # , x =  127.80 + 0.02
#       # , y = 34.69 + 0.02
#     ) +
#     labs(
#       x = NULL
#       , y = NULL
#       , color = NULL
#       , fill = NULL
#       , subtitle = plotSubTitle2
#     ) +
#     # scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99")) +
#     # xlim(127.80, 128.08) + 
#     # ylim(34.69, 34.95) + 
#     scale_fill_brewer(palette = "Set1") +
#     theme(
#       text = element_text(size = 14)
#       , panel.grid.major.x = element_blank()
#       , panel.grid.major.y = element_blank()
#       , panel.grid.minor.x = element_blank()
#       , panel.grid.minor.y = element_blank()
#       , axis.text.x = element_blank()
#       , axis.ticks.x = element_blank()
#       , axis.title.x = element_blank()
#       , axis.text.y = element_blank()
#       , axis.ticks.y = element_blank()
#       , axis.title.y = element_blank()
#       , legend.position = "top"
#       , legend.box = "horizontal"
#       , plot.margin = unit(c(0.2, 0, 0, 0), "lines")
#     )
#   
#     ggsave(makePiePlot, filename = saveImg2, width = 8, height = 8, dpi = 600)
#     
# }