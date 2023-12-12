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
# R을 이용한 고정 및 확률 효과모형 적용 및 하우스만 검정

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0520"

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
library(openxlsx)
# install.packages("plm")
library(plm)
library(rstatix)


# me 파일 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "ME2.csv"))
meData = readr::read_csv(fileInfo)

# 컬럼 정보
colnames(meData)

# 요약 정보
summary(meData)

# se 파일 읽기
fileInfo2 = Sys.glob(file.path(globalVar$inpPath, serviceName, "SE2.csv"))
seData = readr::read_csv(fileInfo2)

# 컬럼 정보
colnames(seData)

# 요약 정보
summary(seData)

# ================================================
# 고정효과 모형
# ================================================
# 상관계수를 위한 데이터 추출
meDataL1 = meData %>%
  dplyr::select(-location, -years)

# 상관계수
corMat = rstatix::cor_mat(meDataL1)

# 상관계수의 유의성 검정
corPmat = rstatix::cor_pmat(meDataL1)

# 상관계수/유의성 검정 시각화
ggcorrplot::ggcorrplot(corMat, hc.order = TRUE, type = "lower", lab_col = "black", outline.color = "white", lab = TRUE, p.mat = corPmat) + 
  labs(title = '상관계수 행렬') +
  theme(
    panel.background = element_rect(fill = "white")
    , plot.background = element_rect(fill = "white", color = NA)
  ) 


# 고정효과 모형 학습
# 독립변수: number_of_plants, consumer_price_index, per_capita_personal_income, working_age_population, manufacturing_business_cycle_index, total_company_asset
# 종속변수: manufacturing_employment
meModel = plm::plm(
  manufacturing_employment ~ number_of_plants + consumer_price_index + per_capita_personal_income + working_age_population + manufacturing_business_cycle_index + total_company_asset
  , index = c('location', 'years')
  , data = meData
  , model = "within"
  )

# 모형 요약
# 종속 변수 (제조업 고용 manufacturing_employment)를 예측하기 위해서 독립변수 6종을 통해 고정효과 모형을 수행함
# 모형 결과 수정된 결정계수는 0.1951로서 유의수준 0.05 이하에서 통계적으로 유의미함 (P값 참조)
# 또한 회귀계수의 경우 consumer_price_index, per_capita_personal_income, working_age_population은 통계적으로 유의미한 영향을 보인 반면
# 그 외 (number_of_plants, manufacturing_business_cycle_index, total_company_asset)는 유의하지 못함
summary(meModel)

# 시각화
plot(meModel)

# ================================================
# 확률효과 모형
# ================================================
# 상관계수를 위한 데이터 추출
seDataL1 = seData %>% 
  dplyr::select(-years, -location)

# 상관계수
corMat = rstatix::cor_mat(seDataL1)

# 상관계수의 유의성 검정
corPmat = rstatix::cor_pmat(seDataL1)

# 상관계수/유의성 검정 시각화
ggcorrplot::ggcorrplot(corMat, hc.order = TRUE, type = "lower", lab_col = "black", outline.color = "white", lab = TRUE, p.mat = corPmat) + 
  labs(title = '상관계수 행렬') +
  theme(
    panel.background = element_rect(fill = "white")
    , plot.background = element_rect(fill = "white", color = NA)
  ) 

# 확률효과 모형 학습
# 독립변수: number_of_company, consumer_price_index, per_capita_personal_income, working_age_population, service_business_cycle_index, total_company_asset
# 종속변수: service_employment
seModel = plm::plm(
  service_employment ~ number_of_company + consumer_price_index + per_capita_personal_income + working_age_population + service_business_cycle_index + total_company_asset
  , index = c('location', 'years')
  , data = seData
  , model = "random"
  )

# 모형 요약
# 종속 변수 (서비스 취업 service_employment)를 예측하기 위해서 독립변수 6종을 통해 확률효과 모형을 수행함
# 모형 결과 수정된 결정계수는 0.9305로서 유의수준 0.05 이하에서 통계적으로 유의미함 (P값 참조)
# 또한 회귀계수의 경우 number_of_company, per_capita_personal_income, working_age_population, total_company_asset은 통계적으로 유의미한 영향을 보인 반면
# 그 외 (consumer_price_index, service_business_cycle_index)는 유의하지 못함
summary(seModel)

# 시각화
plot(seModel)

# ================================================
# 하우스만 검정
# ================================================
hausmanTest = plm::phtest(meModel, seModel)
hausmanTest

# 두 패널 (고정효과 모형, 확률효과 모형) 모형 간의 일관성 및 적합성을 결정하기 위해서 하우스만 검정을 수행함
# 그 결과 P값은 2.2204e-16로서 유의수준 0.05 이하에서 귀무가설 (고정효과 모형과 확률효과 모형이 일관된 결과이다)을 기각하여 두 모형 간에 추정된 계수은 통계적으로 유의미함
# 즉 제조업 고용인 고정효과 모형이 서비스 취업인 확률효과 모형보다 더 적합하다고 판단됨
