
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
# R을 이용한 데이터 전처리 및 막대그래프 시각화

# 안녕하세요. 제가 R프로그램을 활용해서 대학교 과제를 진행하고 있는데 아래 부분에 대하여 이번주 일요일 정도까지 작업 가능한지 문의 드립니다.
# 우선 제가 원하는 것은 붙임으로 첨부한 2개의 통계 자료(RAW DATA) 에 대하여 행,열에 명시된 "코드+이름" 을 "이름"으로 바꾸고(rename 사용) 그 후
# 바뀐 자료를 가지고 간단한 히스토그램이나 막대그래프를 만들어서 보고서에 첨부하려고 하는데 어떻게 만드는지에 대한 R프로그램 코드 내역이 필요합니다.
# 대학생 과제라서 높은 퀄리티를 요하지는 않습니다.
# 가능하신지 여부와 생각하시는 금액까지 적어서 답장 주시면 감사하겠습니다.

# 수고 많으셨습니다. 왼쪽,오른쪽 사진 2개 다 업종은 건설업 제조업 기타의 사업 운수창고업종만 살려주시고 나머지는 삭제해도 될 것 같습니다.
# 왼쪽의 발생형태의 경우 업무상질병,떨어짐,끼임,부딪힘,깔림,절단 베임 찔림만 남기고 수정 뷰탁드립니다.
# 오른쪽 사진은 위에 기재한대로 말씀드린 업종만 남기고 다시 부탁드립니다

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0378"

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
  source(here::here(file.path(contextPath, "src"), "InitConfig.R"), encoding = "UTF-8")
}

#================================================
# 비즈니스 로직 수행
#================================================
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(magrittr)

# *****************************************************
# 산업중분류별 사고건수
# *****************************************************
# inpFile = Sys.glob(file.path(globalVar$inpPath, serviceName, "file1_occur.xlsx"))
# data = openxlsx::read.xlsx(inpFile, sheet = 1, startRow = 2)
#
# data2 = select(data, 1, 2, 4, 6, 9, 10, 14, 23)
# data2 = rename(data2, "산업중분류별" = "15118AI7.산업중분류별(1)")
# data2 = rename(data2, "산업중분류별2" = "15118AI7.산업중분류별(2)")
# data2 = rename(data2, "떨어짐" = "15118AJ401.떨어짐")
# data2 = rename(data2, "부딪힘" = "15118AJ403.부딪힘")
# data2 = rename(data2, "끼임" = "15118AJ406.끼임")
# data2 = rename(data2, "절단·베임·찔림" = "15118AJ407.절단·베임·찔림")
# data2 = rename(data2, "깔림·뒤집힘" = "15118AJ411.깔림·뒤집힘")
# data2 = rename(data2, "업무상질병" = "15118AJ423.업무상질병")
#
# data3 = filter(data2, 산업중분류별2 == "소계")
# data4 = gather(data3, -c("산업중분류별", "산업중분류별2"), key = "분류", value = "사고건수")
#
# # data4$`산업중분류별` %>% unique()
#
# data5 = filter(data4, 산업중분류별 == "15118AI7AD 건설업")
# ggplot() +
#   geom_bar(data = data5, aes(x = 분류, y = 사고건수), position="dodge", stat = "identity")
#
# data5 = filter(data4, 산업중분류별 == "15118AI7AB 제조업")
# ggplot() +
#   geom_bar(data = data5, aes(x = 분류, y = 사고건수), position="dodge", stat = "identity")
#
# data5 = filter(data4, 산업중분류별 == "15118AI7AJ 기타의 사업")
# ggplot() +
#   geom_bar(data = data5, aes(x = 분류, y = 사고건수), position="dodge", stat = "identity")
#
# data5 = filter(data4, 산업중분류별 == "15118AI7AE 운수·창고 및 통신업")
# ggplot() +
#   geom_bar(data = data5, aes(x = 분류, y = 사고건수), position="dodge", stat = "identity")
#
#
# # *****************************************************
# # 산업중분류별 재직건수
# # *****************************************************
# inpFile = Sys.glob(file.path(globalVar$inpPath, serviceName, "file2_year.xlsx"))
# data = openxlsx::read.xlsx(inpFile, sheet = 1, startRow = 2)
#
# data2 = select(data, -3)
# data2 = rename(data2, "산업중분류별" = "15118AI7.산업중분류별(1)")
# data2 = rename(data2, "산업중분류별2" = "15118AI7.산업중분류별(2)")
# data2 = rename(data2, "6개월.미만" = "15118AI9AA.6개월.미만")
# data2 = rename(data2, "6개월~1년.미만" = "15118AI9AB.6개월~1년.미만")
# data2 = rename(data2, "1~2년.미만" = "15118AI9AC.1~2년.미만")
# data2 = rename(data2, "2~3년.미만" = "15118AI9AD.2~3년.미만")
# data2 = rename(data2, "3~4년.미만" = "15118AI9AE.3~4년.미만")
# data2 = rename(data2, "4~5년.미만" = "15118AI9AF.4~5년.미만")
# data2 = rename(data2, "5~10년.미만" = "15118AI9AG.5~10년.미만")
# data2 = rename(data2, "10년.이상" = "15118AI9AH.10년.이상")
# data2 = rename(data2, "분류불능" = "15118AI9AI.분류불능")
#
# data3 = filter(data2, 산업중분류별2 == "소계")
# data4 = gather(data3, -c("산업중분류별", "산업중분류별2"), key = "분류", value = "재직건수")
#
# data5 = filter(data4, 산업중분류별 == "15118AI7AD 건설업")
# ggplot() +
#   geom_bar(data = data5, aes(x = 분류, y = 재직건수), position="dodge", stat = "identity")
#
# data5 = filter(data4, 산업중분류별 == "15118AI7AB 제조업")
# ggplot() +
#   geom_bar(data = data5, aes(x = 분류, y = 재직건수), position="dodge", stat = "identity")
#
# data5 = filter(data4, 산업중분류별 == "15118AI7AJ 기타의 사업")
# ggplot() +
#   geom_bar(data = data5, aes(x = 분류, y = 재직건수), position="dodge", stat = "identity")
#
# data5 = filter(data4, 산업중분류별 == "15118AI7AE 운수·창고 및 통신업")
# ggplot() +
#   geom_bar(data = data5, aes(x = 분류, y = 재직건수), position="dodge", stat = "identity")

# *****************************************************
# 산업중분류별 사고건수
# *****************************************************
inpFile = Sys.glob(file.path(globalVar$inpPath, serviceName, "file1_occur.xlsx"))
data = openxlsx::read.xlsx(inpFile, sheet = 1, startRow = 2)

colData = colnames(data) %>%
  as.tibble() %>%
  tidyr::separate(value, c("code", "name"), sep = "\\.")

dataL1 = data %>%
    magrittr::set_colnames(colData$name) %>%
    tidyr::separate("산업중분류별(1)", c("code", "name"), sep = " ") %>%
    tidyr::separate("산업중분류별(2)", c("code2", "name2"), sep = " ")

dataL2 = dataL1 %>%
  as.tibble() %>%
  dplyr::filter(! code2 == "소계") %>%
  # dplyr::rename(
  #   "산업중분류별(1)" = "name"
  #   , "산업중분류별(2)" = "name2"
  # ) %>%
  dplyr::select(-c(code, code2, 계)) %>%
  tidyr::gather(-c("name", "name2"), key = "key", value = "val")

dataL3 = data2 %>%
  dplyr::filter(
    name %in% c("건설업", "제조업", "기타의", "운수·창고")
    , key %in% c("업무상질병", "떨어짐", "끼임", "부딪힘", "깔림·뒤집힘", "절단·베임·찔림")
  ) %>%
  dplyr::group_by(name, key) %>%
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  ) %>%
  dplyr::arrange(desc(sumVal))

dataL3 = dataL2 %>%
  dplyr::filter(
    name %in% c("건설업", "제조업", "기타의", "운수·창고")
    , key %in% c("업무상질병", "떨어짐", "끼임", "부딪힘", "깔림·뒤집힘", "절단·베임·찔림")
  ) %>%
  dplyr::group_by(name, key) %>%
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  ) %>%
  dplyr::arrange(desc(sumVal))


# 건설업", "제조업", "기타의", "운수·창고"

colData = colnames(data) %>%
  as.tibble() %>%
  tidyr::separate(value, c("code", "name"), sep = "\\.")

dataL1 = data %>%
    magrittr::set_colnames(colData$name) %>%
    tidyr::separate("산업중분류별(1)", c("code", "name"), sep = " ") %>%
    tidyr::separate("산업중분류별(2)", c("code2", "name2"), sep = " ")

dataL2 = dataL1 %>%
  as.tibble() %>%
  dplyr::filter(! code2 == "소계") %>%
  # dplyr::rename(
  #   "산업중분류별(1)" = "name"
  #   , "산업중분류별(2)" = "name2"
  # ) %>%
  dplyr::select(-c(code, code2, 계)) %>%
  tidyr::gather(-c("name", "name2"), key = "key", value = "val")
  # tidyr::gather(-c("산업중분류별(1)", "산업중분류별(2)"), key = "key", value = "val")
# 건설업 제조업 기타의 사업 운수창고

# dataL2$name %>% unique()
# dataL2$key %>% unique()

dataL3 = dataL2 %>%
  dplyr::filter(
    name %in% c("건설업", "제조업", "기타의", "운수·창고")
    , key %in% c("업무상질병", "떨어짐", "끼임", "부딪힘", "깔림·뒤집힘", "절단·베임·찔림")
  ) %>%
  dplyr::group_by(name, key) %>%
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  ) %>%
  dplyr::arrange(desc(sumVal))

# 정렬
dataL3$key = forcats::fct_relevel(dataL3$key, unique(dataL3$key))

# 산업중분류별 사고건수 막대 그래프
plotSubTitle = sprintf("%s", "산업중분류별 사고건수 막대 그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

# ggplot(data = dataL3, aes(x = key, y = sumVal,  fill = sumVal)) +
ggplot(data = dataL3, aes(x = key, y = sumVal)) +
# ggplot(data = dataL2, aes(x = key, y = val)) +
  geom_bar(stat = "identity", width = 0.4, position=position_dodge(width = 0.5)) +
  scale_y_continuous(limits=c(0, 300)) +
  labs(title = NULL, x = NULL, y = "사고 건수", colour = NULL, fill = NULL, subtitle = plotSubTitle) +
  theme(
    text = element_text(size = 16)
    # , axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
    , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
     , legend.key.width = unit(2, "cm")
  ) +
  # facet_wrap(~name, scales = "free_y", ncol = 2) +
  facet_wrap(~name, ncol = 2) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

ggplot2::last_plot()


# *****************************************************
# 산업중분류별 재직건수
# *****************************************************
inpFile = Sys.glob(file.path(globalVar$inpPath, serviceName, "file2_year.xlsx"))
data = openxlsx::read.xlsx(inpFile, sheet = 1, startRow = 2)

colData = colnames(data) %>%
  as.tibble() %>%
  tidyr::separate(value, c("code", "name"), sep = "\\.")

dataL1 = data %>%
    magrittr::set_colnames(colData$name) %>%
    tidyr::separate("산업중분류별(1)", c("code", "name"), sep = " ") %>%
    tidyr::separate("산업중분류별(2)", c("code2", "name2"), sep = " ")

dataL2 = dataL1 %>%
  as.tibble() %>%
  dplyr::filter(! code2 == "소계") %>%
  dplyr::select(-c(code, code2, 계)) %>%
  tidyr::gather(-c("name", "name2"), key = "key", value = "val")
  # tidyr::gather(-c("산업중분류별(1)", "산업중분류별(2)"), key = "key", value = "val")

dataL3 = dataL2 %>%
  dplyr::filter(
    name %in% c("건설업", "제조업", "기타의", "운수·창고")
  ) %>%
  dplyr::group_by(name, key) %>%
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  ) %>%
  dplyr::arrange(desc(sumVal))


# 정렬
dataL3$key = forcats::fct_relevel(dataL3$key, unique(dataL2$key))

# 산업중분류별 재직건수 막대 그래프
plotSubTitle = sprintf("%s", "산업중분류별 재직건수 막대 그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

# ggplot(data = dataL3, aes(x = key, y = sumVal,  fill = sumVal)) +
ggplot(data = dataL3, aes(x = key, y = sumVal)) +
  geom_bar(stat = "identity", width = 0.4, position=position_dodge(width = 0.5)) +
  scale_y_continuous(limits=c(0, 450)) +
  labs(title = NULL, x = NULL, y = "사고 건수", colour = NULL, fill = NULL, subtitle = plotSubTitle) +
  theme(
    text = element_text(size = 16)
    , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
     , legend.key.width = unit(2, "cm")
  ) +
  # facet_wrap(~name, scales = "free_y", ncol = 2) +
  facet_wrap(~name, ncol = 2) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

ggplot2::last_plot()
