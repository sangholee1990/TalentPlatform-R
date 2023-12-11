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
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
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


# 파일 읽기
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "ME2.csv"))
meData = readr::read_csv(fileInfo)

colnames(meData)
summary(meData)


fileInfo2 = Sys.glob(file.path(globalVar$inpPath, serviceName, "SE2.csv"))
seData = readr::read_csv(fileInfo2)


colnames(seData)
summary(seData)
# ================================================
# 고정효과모형
# ================================================
meDataL1 = meData %>%
  dplyr::select(-location, -years)


corMat = rstatix::cor_mat(meDataL1)
corPmat = rstatix::cor_pmat(meDataL1)

# 상관계수
corMat %>%
  dplyr::select(-rowname) %>% 
  as.data.frame() %>% 
  round(3)

# 상관계수 유의성검정
corPmat %>%
  dplyr::select(-rowname) %>% 
  as.data.frame() %>% 
  round(4)

# 상관계수 행렬
ggcorrplot::ggcorrplot(corMat, hc.order = TRUE, type = "lower", lab_col = "black", outline.color = "white", lab = TRUE, p.mat = corPmat) + 
  labs(title = '상관계수 행렬') +
  theme(
    panel.background = element_rect(fill = "white")
    , plot.background = element_rect(fill = "white", color = NA)
  ) 

# 다중공선성 계산
lmFit = lm(manufacturing_employment ~ total_exports + number_of_plants + consumer_price_index + per_capita_personal_income + working_age_population + manufacturing_business_cycle_index + total_company_asset, data = meDataL1)

# 다중공선성 계산
# 10 이상 제거  number_of_plants O , consumer_price_index O per_capita_personal_income, total_exports                   
car::vif(lmFit)



# 패널 데이터로 변환 및 고정효과모형 적용
# total_exports 제거
meModel = plm::plm(manufacturing_employment ~ number_of_plants + consumer_price_index + per_capita_personal_income + working_age_population + manufacturing_business_cycle_index + total_company_asset, index = c('location', 'years'), data = meData, model = "within")

# 모델 요약 출력
summary(meModel)


s# ================================================
# 확률효과모형
# ================================================
seDataL1 = seData %>% 
  dplyr::select(-years, -location)


corMat = rstatix::cor_mat(seDataL1)
corPmat = rstatix::cor_pmat(seDataL1)

# 상관계수
corMat %>%
  dplyr::select(-rowname) %>% 
  as.data.frame() %>% 
  round(3)

# 상관계수 유의성검정
corPmat %>%
  dplyr::select(-rowname) %>% 
  as.data.frame() %>% 
  round(4)

# 상관계수 행렬
ggcorrplot::ggcorrplot(corMat, hc.order = TRUE, type = "lower", lab_col = "black", outline.color = "white", lab = TRUE, p.mat = corPmat) + 
  labs(title = '상관계수 행렬') +
  theme(
    panel.background = element_rect(fill = "white")
    , plot.background = element_rect(fill = "white", color = NA)
  ) 

# 다중공선성 계산
lmFit = lm(service_employment ~ total_exports + number_of_company + consumer_price_index + per_capita_personal_income + working_age_population + service_business_cycle_index + total_company_asset, data = seDataL1)

# 다중공선성 계산
# 10 이상 제거  number_of_plants O , consumer_price_index O per_capita_personal_income, total_exports                   
car::vif(lmFit)


seModel = plm::plm(service_employment ~ number_of_company + consumer_price_index + per_capita_personal_income + working_age_population + service_business_cycle_index + total_company_asset, index = c('location', 'years'), data = seData, model = "random")

summary(seModel)


# ================================================
# 하우스만 검정
# ================================================
hausman_test <- phtest(meModel, seModel)
hausman_test

# P값은 2.2204e-16로서 유의수준 0.05 이하에서 귀무가설 (고정효과모형과 무작위효과모형이 동일한 계수이다)을 기각하여 두 모형 간에 추정된 계수은 통계적으로 유의미함
# 또한 고정효과모형이 확률효과모형보다 데이터에 더 적합하다고 할 수 있습니다. 다시 말해, 개별 패널(여기서는 'location'과 'years')의 고유한 특성이 종속변수(여기서는 'manufacturing_employment')에 영향을 미침 
