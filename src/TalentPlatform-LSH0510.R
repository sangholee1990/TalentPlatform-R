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
# R을 이용한 mtcar 데이터 분석 및 시각화 (상자 그림, 상위 5개 막대 그래프, 빈도 분포)

# class(자동차 종류)가 "compact", "subcompact", "suv"인 자동차의 cty(도시 연비)가 어떻게 다른지 비교해보려고 합니다.세차종의cty를나타낸상자그림 을만들어보세요.

# mpg 데이터를 활용해 데이터 상의 자동차 중에서 어떤 회사에서 생산한＂suv＂차 종의 도시 연비가 좋은지 알아보려고 합니다.
# "suv" 차종을 대상으로 평균 city(도시 연비)가 가장 높은회사 다섯 곳을 막대그래프로 표현해보세요.
# 막대는 연비가 높은순으로 정렬 하세요

# mpg 데이터상 자동차 중에서 어떤 class(자동차 종류가 가장 많은지 알아보려고 합니다.
# 자동차 종류별 빈도를 표현한 막대그래프를 만들어보세요.

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0510"

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
library(ggplot2)
library(dplyr)

# 차종별 도시 연비에 대한 상자 그림
class_mpg = mpg %>% 
  dplyr::filter(class %in% c("compact", "subcompact", "suv"))

ggplot(class_mpg, aes(x = class, y = cty)) + 
  geom_boxplot() +
  labs(x = "차종", y = "평균 도시 연비", title = "차종별 도시 연비에 대한 상자 그림")

# SUV 제조사별 평균 도시 연비 상위 5위 막대 그래프
df = mpg %>% 
  dplyr::filter(class == "suv") %>% 
  dplyr::group_by(manufacturer) %>% 
  dplyr::summarise(mean_cty = mean(cty, na.rm = TRUE)) %>%
  dplyr::arrange(desc(mean_cty)) %>% 
  head(5)

ggplot(df, aes(x = reorder(manufacturer, -mean_cty), y = mean_cty)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(mean_cty, 2)), nudge_y = -0.75, size = 4, color = "white") +
  labs(x = "제조사", y = "평균 도시 연비", title = "SUV 제조사별 평균 도시 연비 상위 5위 막대 그래프")

# 자동차 종류별 빈도 분포
df = mpg %>% 
  dplyr::group_by(class) %>% 
  dplyr::summarise(cnt = n()) %>%
  dplyr::arrange(desc(cnt))

ggplot(data = df, aes(x = reorder(class, -cnt), y = cnt)) + 
  geom_bar(stat = "identity")  +
  geom_text(aes(label = cnt), nudge_y = -2.0, size = 4, color = "white") +
  labs(x = "자동차 종류", y = "개수", title = "자동차 종류별 빈도 분포")
