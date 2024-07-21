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
# R을 이용한 네이버 뉴스 API 수집 및 텍스트 분석 (키워드 추출, 빈도분포, 단어구름)

# 안녕하세요, 처음에 큰 금액 결제가 부담스러워 일단 5000원 결제 드리고 문의 드립니다.
# 원하는 작업 말씀 드리겠습니다.
# r스튜디오 사용해 '2024년 7월 1일~2024년 7월 15일 보름간 NAVER에 올라온 뉴스(기사) 중 제목 맨 앞에 '[단독]'이 표시된 기사의 갯수와, 그 기사들이 특정 인물 또는 사건 사고 등을 다루는지, 그리고 같은 기간 올라온 다른 보통의 기사들과의 길이 차이를 파악하는 프로그래밍을 하기를 원합니다.

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0572"

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
library(RCurl)
library(httr)
library(tidyverse)
library(stringr)
library(jsonlite)
library(RColorBrewer)
library(wordcloud)
library(remotes)
library(RcppMeCab)
library(RmecabKo)

# *************************************************************
# 네이버 뉴스 API 수집
# *************************************************************
# 네이버 아이디/비밀번호
reqId = "BNDqTQqb0NECaQN56flk"
reqPw = "5t2GECIt1m"

# API 주소
reqUrl = "https://openapi.naver.com/v1/search/news.json"

dataL2 = tibble::tibble()
keywordList = c("[단독]")
for (keyword in keywordList) {
  
  # pageList = seq(1, 10)
  pageList = seq(1, 20)
  for (page in pageList) {
    
    cat(sprintf("[CHECK] keyword : %s / page : %s", keyword, page), "\n")
    
    reqQuery = list(
      # query = RCurl::curlEscape(stringr::str_conv(keyword, encoding = "UTF-8"))
      query = stringr::str_conv(keyword, encoding = "UTF-8")
      , display = 100
      , start = page
      , sort = "date"
      # , sort = "sim"
    )
    
    # API 요청
    resRes = httr::GET(
      reqUrl, query = reqQuery
      , httr::add_headers("X-Naver-Client-Id" = reqId, "X-Naver-Client-Secret" = reqPw)
    )
    
    # JSON 파일
    jsonData = httr::content(resRes, as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON()
    
    data = jsonData$items
    if (length(data) < 1) next
    
    dataL1 = data %>% 
      dplyr::mutate(
        keyword = keyword
        , page = page
        , url = resRes$url
      )
    
    dataL2 = dplyr::bind_rows(dataL2, dataL1)
  }
}

# 키워드 여부 Y/N
dataL3 = dataL2 %>% 
  dplyr::mutate(
    isKeyword = dplyr::case_when(
      stringr::str_detect(title, regex("[단독]")) ~ "Y"
      , TRUE ~ "N"
    )
  )

saveFile = sprintf("%s/%s/%s.csv", globalVar$outPath, serviceName, "원천 데이터")
dir.create(dirname(saveFile), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(dataL3, file = saveFile)
cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")

# *************************************************************
# RcppMeCab 라이브러리를 통해 명사 추출
# *************************************************************
# RmecabKo 라이브러리를 위한 메타 정보 (명사 사전 등)
RmecabKo::install_mecab("c:/mecab")

isKeywordList = dataL3$isKeyword %>% unique() %>% sort()
for (isKeywordInfo in isKeywordList) {
  
  cat(sprintf("[CHECK] isKeywordInfo : %s", isKeywordInfo), "\n")
  
  dataL4 = dataL3 %>% 
    dplyr::filter(stringr::str_detect(isKeyword, regex(isKeywordInfo)))
  
  # 문장 취합
  contAll = paste(dataL4$title, collapse = " ")
  contAll
  
  # 명사 추출
  # 특정 키워드 제외 & 국문어 포함 & 2글자 이상
  contData = RcppMeCab::pos(utf8::as_utf8(contAll), format = "data.frame") %>% 
    dplyr::filter(pos == "NNG") %>%
    dplyr::select(token) %>% 
    dplyr::filter(! stringr::str_detect(token, "단독")) %>% 
    dplyr::filter(stringr::str_detect(token, "^[가-힣]+$")) %>% 
    dplyr::filter(nchar(token) >= 2)
  
  # 명사/키워드에 대한 빈도개수 및 정렬
  tokenData = table(contData$token) %>% sort(decreasing = TRUE)
  tokenData
  
  # 상위 20위 키워드
  keywordData = tokenData[1:20]
  print(keywordData)
  
  saveFile = sprintf("%s/%s/%s-%s.csv", globalVar$outPath, serviceName, "TOP20 키워드", isKeywordInfo)
  dir.create(dirname(saveFile), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(data.frame(keywordData), file = saveFile)
  cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")
  
  # 상위 20위 빈도분포 시각화
  saveImg = sprintf("%s/%s/%s-%s.png", globalVar$figPath, serviceName, "바차트 시각화", isKeywordInfo)
  dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)
  png(file = saveImg, width = 12, height = 6, units = "in", res = 600)

  barplot(keywordData)
  
  dev.off()
  # shell.exec(saveImg)
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
  
  
  # 상위 20위 단어 구름 = 워드 클라우드 시각화
  # 컬러바
  pal = brewer.pal(8, "Dark2")
  wordcloud::wordcloud(names(keywordData), freq = keyword, colors = pal)
  
  # html 저장
  fig = wordcloud2::wordcloud2(data = keywordData)
  tmpHtml = tempfile(fileext = ".html")
  htmlwidgets::saveWidget(fig, tmpHtml, selfcontained = FALSE)
  
  # html에서 png로 저장
  saveImg = sprintf("%s/%s/%s-%s.png", globalVar$figPath, serviceName, "워드클라우드 시각화", isKeywordInfo)
  dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)
  webshot::webshot(tmpHtml, saveImg, vwidth = 1000, vheight = 800, delay = 5)
  
  # shell.exec(saveImg)
  cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
}
