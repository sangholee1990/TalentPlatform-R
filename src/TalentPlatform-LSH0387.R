
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
# R을 이용한 연도별 증권사 매출액 예측 및 시각화

# 네.. 저는 증권업종에 종사하고 있는데, 한두개 정도의 회귀분석은 엑셀등을 통해서 블로그 등을 보고 따라할 수 있지만
# 예를들어 1000여개 종목에 대해 매출액 추정을 하고 결정계수가 높은 종목 순으로 정렬하려 한다면 프로그램을 본격 배워보는 수 밖에 없을 것 같아 레슨을 찾아보게 되었어요
# 이런 목적에 적합한 강의가 가능한지요?
# 그리고 레슨은 오프라인으로 들으려 합니다. 제가 있는 (작은)사무실도 좋구요, 스터디카페 같은 곳도 좋구요. . .

# 각 종목별로 2017년 ~ 2021년 데이터(매출액)을 토대로 2022년 매출액을 추정한느 것입니다. 이때 결정계수가 높아 의미가 있는 종목이 있겠고 그렇지 않은 종목도 있을것으로 보입니다. 결정계수가 높은 종목 순으로 정렬할 수 있다면 더욱 좋구요..
# 중요한 것은 이같은 데이터 처리과정에 대해 저 스스로 회사 내부에서 할 수 있도록 배워보는데 있습니다. 한번 테스트 삼아 해보시고, 괜찮다면 차후 일정을 잡아서 제가 레슨을 받는 방향으로 진행해 보려 합니다.
# 이 외에도 미국 주식 시황과 한국 시장 간의 상관계수 등 통계 패키지 R 로 할 수 있는 여러가지 기능에 대해 배워보려 합니다.

# 그리고 R2 결정계수 높은 종목은 그래프도 함께 출력되면 좋습니다.

# 혹시 과거 5년 데이터를 돌린 후 그 추정액이 적절하게 나왔는지 검증해 보려는 의도라면
# 기본 자료의 연도를 하나씩당겨서 2016~ 2020년도 것을 드리고, 2021년도 매출액을 전달해 드릴 수도 있습니다.
# 그냥 어제 언급이 있으셔서 말씀드리니, 이와 같은 자료가 필요할 것 같으면 말씀주세요

# 아.. 그렇군요.. 그럼 제가 지금 당장은 어렵고 오후에 최근 10년 자료를 전달드리겠습니다.
# (다시 말씀드리지만 이 작업이 시간적으로 기한이 있는 것이 아니기 때문에 신정 연휴를 편히 보내시고, 나중에 해주셔도 됩니다.)

# 2022년 예상 매출액은 (통계적 방법에 의한 추정이 아닌) 증권사의 예상 매출액 입니다.
# 제가 하려는 것은 증권사 컨센서스(예상 매출액)이 없는 종목들을 대상으로 통계적 방법에 의해
# 향후 매출을 추정하는 작업을 해보는 것입니다.
#
# 물론 우리나라 경제 특성상 경기순환 종목이 많아서 대부분의 기업들 매출이 통게적으로 유의미한
# 숫자가 나오지는 않을 것 같습니다.
#
# 다만 한 200종목 정도라도 의미있는 결과가 나온다면 성공적 입니다.
#
# 예를들어 대한제분 같은 종목은 밀가루를 판매하는 회사라서 매출액 변화율이 크지 않습니다.
# 이 회사를 회귀분석 했을 때 10년 이란 장기간을 하는 것보다 5년이 유의미해서 제가 처음부터 다음 종목들도 5년으로 해보려 했던 것입니다.
#
# 전체적으로 한번 살펴봐 주시면 감사하겠습니다.

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0387"

if (Sys.info()["sysname"] == "Windows") {
  contextPath = ifelse(env == "local", ".", "E:/04. TalentPlatform/Github/TalentPlatform-R")
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
library(tidyverse)
library(readr)
library(httr)
library(rvest)
library(jsonlite)
library(RCurl)
library(dplyr)
library(data.table)
library(Rcpp)
library(h2o)

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "raw data1.xlsx"))
data = openxlsx::read.xlsx(fileInfo, sheet = 1) %>%
  as.tibble()

dataL1 = data %>%
  tidyr::gather(-c("종목코드", "Period", "결산월"), key = "key", value = "val") %>%
  dplyr::mutate(
    dtDate = readr::parse_date(key, "%Y")
    , dtYear = lubridate::year(dtDate)
    , dtXran = lubridate::decimal_date(dtDate)
  ) %>%
  dplyr::select(-결산월)
#
# trainData = dataL1 %>%
#   dplyr::filter(dtYear %in% 2022)
#
# testData = dataL1 %>%
#   dplyr::filter(dtYear == 2022) %>%
#   dplyr::filter(! is.na(val))

# testData = tibble(dtDate = seq(as.Date("2018-09-01"), as.Date("2023-02-01"), "1 month")) %>%
#   dplyr::mutate(
#     dtYear = lubridate::year(dtDate)
#     , dtMonth = lubridate::month(dtDate)
#     , dtXran = lubridate::decimal_date(dtDate)
#   ) %>%
#   dplyr::filter(
#     dtMonth %in% c(9, 10, 11)
#   )

dataL3 = tibble::tibble()

periodList = dataL1$Period %>% unique() %>% sort()
# periodInfo = periodList[1]
for (periodInfo in periodList) {

  dataL2 = dataL1 %>%
    dplyr::filter(
      Period == periodInfo
      , ! is.na(val)
    ) %>%
    tibble::rowid_to_column("idx")

  if (nrow(dataL2) < 8) next

  # trainIdx = as.integer(nrow(dataL2) * 0.70)
  # trainIdx = as.integer(nrow(dataL2) * 0.95)
  trainIdx = as.integer(nrow(dataL2) * 0.9)
  testIdx = trainIdx + 1

  trainData = dataL2 %>%
    dplyr::filter(idx <= trainIdx)

  testData = dataL2 %>%
    dplyr::filter(idx > trainIdx)


  tryCatch(
    expr = {

      # ******************************************************************************
      # 다중선형회귀모형
      # ******************************************************************************
      lmModel = lm(val ~ dtYear, data = trainData)
      summary(lmModel)

      dataL2$prdLM = predict(lmModel, newdata = dataL2)

      # ******************************************************************************
      # 머신러닝 및 딥러닝 모형
      # ******************************************************************************
      # 초기화
      h2o::h2o.init()
      h2o::h2o.no_progress()

      # 모델 학습
      amlModel = h2o::h2o.automl(
        x = c("dtYear")
        , y = c("val")
        , training_frame = h2o::as.h2o(trainData)
        , validation_frame = h2o::as.h2o(testData)
        , nfolds = 5
        , sort_metric = "RMSE"
        , stopping_metric = "RMSE"
        , seed = 123
        , max_runtime_secs = 60
        , stopping_rounds = 10
        , stopping_tolerance = 0.005
        # , max_models = 5
      )

      dataL2$prdAml = as.data.frame(h2o::h2o.predict(object = amlModel, newdata = as.h2o(dataL2)))$predict

      saveFile = sprintf("%s/%s/%s.csv", globalVar$outPath, serviceName, "증권사 매출액 예측 결과")
      dir.create(path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
      readr::write_csv(x = dataL2, path = saveFile, append = TRUE)
      cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")

      dataL3 = dplyr::bind_rows(dataL3, dataL2)
    }
  )



}

# ******************************************************************************
# 에측 결과 저장
# ******************************************************************************
saveXlsxFile = sprintf("%s/%s/%s.xlsx", globalVar$outPath, serviceName, "증권사 매출액 예측 결과")
dir.create(path_dir(saveXlsxFile), showWarnings = FALSE, recursive = TRUE)
wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "예측 데이터")
openxlsx::writeData(wb, "예측 데이터", dataL3, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)
openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)
cat(sprintf("[CHECK] saveXlsxFile : %s", saveXlsxFile), "\n")