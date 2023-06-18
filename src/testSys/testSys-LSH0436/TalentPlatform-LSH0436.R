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
# R을 이용한 발전량 데이터에 대한 통계 검정, 교차표 및 시각화

# 1차적으로 필터를 걸어서 다음과 같은 데이터만 이용하려고 합니다!
# filter(dt, ENG_CLSF=='신재생에너지 총발전량'|ENG_CLSF=='태양광'|
#          ENG_CLSF=='풍력'|ENG_CLSF=='수력'|ENG_CLSF=='해양' )
# 1. 지역별 총 발전량 차이 분석: 지역별 총 발전량이 있는데, 사업용+자가용을 합쳐서 그래프를 그리고 싶습니다.
# 결측치는 0으로 설정하고 그려주시면 될 것 같습니다
# 이후 "지역별 총 발전량에는 유의미한 차이가 있을것이다(사업용+자가용)"검정, "지역별 태양광발전량에는 차이가 있을지" 검정을 진행하고싶습니다!
# 검정방법은 ,, 가장 적절할만한 것으로 부탁드리겠습니다.
# 그리고 지역별로(시 / 도) 종류&연도별 발전량에 대한 교차표와
# 지역별 태양광, 풍력에 대한 연도별 그래프(x축 연도, y축 태양광 발전량 & x축 연도, y축 풍력 발전량) 부탁드릴 수 있을까요?


# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0436"

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
library(forcats)

# 파일 조회
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "energy.csv"))

# 파일 읽기
data = readr::read_csv(fileList)

# 1차적으로 필터링
# 결측치는 0으로 설정하고 그려주시면 될 것 같습니다 (평균 시 문제 발생)
dataL1 = data %>% 
  dplyr::filter(ENG_CLSF %in% c('신재생에너지 총발전량', '태양광', '풍력', '수력', '해양')) %>% 
  tidyr::gather(-NO, -RGN_SORT, -ENG_CLSF, -USE_CLSF, -UNIT, key = "sKey", value = "sVal") %>% 
  dplyr::mutate(
    dtDate = readr::parse_datetime(sKey, format = "YR_%Y")
    , nYear = lubridate::year(dtDate)
    , val = readr::parse_number(sVal)
  )

# 1. 지역별 총 발전량 차이 분석: 지역별 총 발전량이 있는데, 사업용+자가용을 합쳐서 그래프를 그리고 싶습니다.
dataL2 = dataL1 %>% 
  dplyr::filter(RGN_SORT != "전국") %>% 
  dplyr::group_by(RGN_SORT) %>% 
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
    ) %>% 
  dplyr::arrange(sumVal)
  # dplyr::arrange(desc(sumVal))

# 정렬
dataL2$RGN_SORT = forcats::fct_relevel(dataL2$RGN_SORT, dataL2$RGN_SORT)

# 지역별 총 발전량 그래프
plotSubTitle = sprintf("%s", "지역별 총 발전량 그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(dataL2, aes(x = RGN_SORT, y = sumVal, fill = RGN_SORT)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(title = NULL, subtitle = plotSubTitle, x = "지역", y = "총 발전량", fill = NULL) +
  theme(
    text = element_text(size = 16)
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# 이후 "지역별 총 발전량에는 유의미한 차이가 있을것이다(사업용+자가용)"검정
# Kruskal-Wallis  비모수 검정 : 3개 이상의 그룹 간 중위수 차이 (정규분포 X , 등분산성 가정 위배)
# 귀무가설 : 모든 그룹의 중위수 동일
# 대립가설 : 적어도 한 그룹의 중위수 다름
# 그 결과 유의수준 (p-value)는 0.4529608로서 유의수준 0.05 조건 하에서 귀무가설 기각하지 못함
# 즉 지역별 총 발전량은 통계적으로 유사하다.
kruRes = kruskal.test(sumVal ~ RGN_SORT, data = dataL2)
print(kruRes)

# "지역별 태양광발전량에는 차이가 있을지" 검정을 진행하고싶습니다!
dataL3 = dataL1 %>% 
  dplyr::filter(
    RGN_SORT != "전국"
    , ENG_CLSF == "태양광"
    ) %>%
  dplyr::group_by(RGN_SORT) %>% 
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  ) %>% 
  dplyr::arrange(sumVal)

# 그 결과 유의수준 (p-value)는 0.4529608로서 유의수준 0.05 조건 하에서 귀무가설 기각하지 못함
# 즉 지역별 총 발전량은 통계적으로 유사하다.
kruRes = kruskal.test(sumVal ~ RGN_SORT, data = dataL3)
print(kruRes)

# 그리고 지역별로(시 / 도) 종류&연도별 발전량에 대한 교차표
dataL4 = dataL1 %>% 
  dplyr::filter(
    RGN_SORT != "전국"
  ) %>%
  dplyr::group_by(RGN_SORT, nYear) %>% 
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  ) %>% 
  dplyr::arrange(sumVal)

crossTab = xtabs(sumVal ~ RGN_SORT + nYear, data = dataL4)
print(crossTab)

# 지역별 태양광, 풍력에 대한 연도별 그래프(x축 연도, y축 태양광 발전량 & x축 연도, y축 풍력 발전량) 부탁드릴 수 있을까요?
dataL5 = dataL1 %>% 
  dplyr::filter(
    RGN_SORT != "전국"
    , ENG_CLSF %in% c('태양광', '풍력')
  ) %>%
  dplyr::group_by(RGN_SORT, ENG_CLSF, nYear) %>% 
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  ) %>% 
  dplyr::arrange(sumVal)

# 지역별 태양광 및 풍력에 대한 연도별 그래프
plotSubTitle = sprintf("%s", "지역별 태양광 및 풍력에 대한 연도별 그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(data = dataL5, aes(x=nYear, y=sumVal, color=RGN_SORT)) + 
  geom_line() +
  geom_point() +
  labs(title = NULL, subtitle = plotSubTitle, x = "연도", y = "발전량", fill = NULL, color = NULL) +
  theme(
    text = element_text(size = 16)
    # , legend.position = "top"
  ) +
  facet_wrap(~ENG_CLSF, scale = "free_y") +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
