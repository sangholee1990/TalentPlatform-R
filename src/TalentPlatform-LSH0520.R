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

fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "ME2.csv"))
data = readr::read_csv(fileInfo)

colnames(data)


fileInfo2 = Sys.glob(file.path(globalVar$inpPath, serviceName, "SE2.csv"))
data2 = readr::read_csv(fileInfo2)


summary(data)
summary(data2)

colnames(data)
colnames(data2)


# 패널 데이터로 변환
pdata <- pdata.frame(data, index = c("location", "years"))

# 고정효과모형 적용
fe_model <- plm(manufacturing_employment ~ total_exports + number_of_plants + consumer_price_index + per_capita_personal_income + working_age_population + manufacturing_business_cycle_index + total_company_asset, data = pdata, model = "within")

# 모델 요약 출력
summary(fe_model)


dataL1 = data %>%
  dplyr::select(-location, -year)

full_model <- lm(manufacturing_employment ~ ., data = dataL1)

# 단계적 선택을 사용하여 모델 최적화
stepwise_model <- step(full_model, direction = "both")

# 선택된 모델에서 사용된 변수 확인
# selected_vars <- names(coef(stepwise_model))

# 고정효과모형 적용을 위한 공식 생성
# fe_formula <- as.formula(paste("manufacturing_employment ~", paste(selected_vars[-1], collapse = " + ")))


# 고정효과모형
feModel = plm::plm(manufacturing_employment ~ years + total_exports + 
number_of_plants + per_capita_personal_income + working_age_population + total_company_asset, index = c('years'), data = dataL1, model = "within")

summary(feModel)


# feModel = plm::plm(manufacturing_employment ~ ., data = dataL1, model = "within")

# feModel
# summary(feModel)

# 확률효과모형
# reModel = plm::plm(service_employment ~ ., data = data2, model = "random")

reModel = plm::plm(manufacturing_employment ~ years + total_exports + 
number_of_plants + per_capita_personal_income + working_age_population + total_company_asset, index = c('years'), data = dataL1, model = "random")

reModel
summary(reModel)

# 하우스만 검정
hausman_test <- phtest(fe_model, re_model)

# 결과 출력
summary(fe_model)
summary(re_model)
print(hausman_test)



# 
# # 참조 데이터 읽기
# # fileInfo2 = "C:/SYSTEMS/PROG/R/TalentPlatform-R/resources/input/test/LSH0518/(2022년 17차 한국복지패널조사) 조사설계서-가구용(beta1).xlsx"
# fileInfo2 = Sys.glob(file.path(globalVar$inpPath, serviceName, "(2022년 17차 한국복지패널조사) 조사설계서-가구용(beta1).xlsx"))
# 
# refData = openxlsx::read.xlsx(fileInfo2, sheet = "직종코드(2019 신분류)") 
# 
# # 특정 컬럼 선택
# refDataL1 = refData[ , c("소분류", "X4")]
# refDataL1$소분류 = as.numeric(refDataL1$소분류)
# colnames(refDataL1) = c("nameCode", "name")
# head(refDataL1)
# 
# # 원본 데이터 읽기
# for (i in 15:17) {
#   
#   if (i == 15) year = 2020
#   if (i == 16) year = 2021
#   if (i == 17) year = 2022
#   
#   fileInfo = Sys.glob(sprintf("C:/SYSTEMS/PROG/R/TalentPlatform-R/resources/input/test/LSH0518/Koweps_h%s_*_beta*.sav", i))
#   
#   if (length(fileInfo) < 1) next
#   
#   data = read_spss(fileInfo)
#   
#   # 컬럼 정보
#   colnames(data)
#   
#   # 특정 컬럼 선택
#   # 성별 h1601_4, h1701_4 : 1.남, 2.여
#   # 직종 h1603_8, h1703_8 : 직종코드 참조
#   sexCode = sprintf("h%s01_4", i)
#   nameCode = sprintf("h%s03_8", i)
#   dataL1 = data[ , c(sexCode, nameCode)]
#   colnames(dataL1) = c("sexCode", "nameCode")
#   
#   # 데이터 병합 (좌측 조인)
#   dataL2 = merge(dataL1, refDataL1, by = "nameCode", all.x = TRUE)
#   
#   dataL2$sex = factor(dataL2$sexCode, levels = c("1", "2"), labels = c("남", "여"))
#   
#   # 남성에 따른 상위 10개 직업
#   dataL3 = dataL2[dataL2$sex == "남", ]
#   
#   jobFreq = table(dataL3$name)
#   jobFreqSort = sort(jobFreq, decreasing = TRUE)
#   jobTop = head(jobFreqSort, 10)
#   print(jobTop)
#   
#   # 이미지 저장
#   mainTitle = sprintf("%s년 남성에 따른 상위 10개 직업", year)
#   saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
#   dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)
#   
#   png(file = saveImg, width = 22, height = 6, units = "in", res = 600)
#   barplot(jobTop, las = 1, main = mainTitle, xlab = "직업", ylab = "빈도")
#   dev.off()
#   
#   # 여성에 따른 상위 10개 직업
#   dataL3 = dataL2[dataL2$sex == "여", ]
#   
#   jobFreq = table(dataL3$name)
#   jobFreqSort = sort(jobFreq, decreasing = TRUE)
#   jobTop = head(jobFreqSort, 10)
#   print(jobTop)
#   
#   # 이미지 저장
#   mainTitle = sprintf("%s년 여성에 따른 상위 10개 직업", year)
#   saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
#   dir.create(dirname(saveImg), showWarnings = FALSE, recursive = TRUE)
#   
#   png(file = saveImg, width = 22, height = 6, units = "in", res = 600)
#   barplot(jobTop, las = 1, main = mainTitle, xlab = "직업", ylab = "빈도") 
#   dev.off()
# }
# 
# 
# # 주요 결과
# # 남성의 경우 매년 상위 직업군 (경영 관련 사무원, 청소원 및 환경미화원, 매장 판매 종사자, 영업 종사자)에 속하며 특히 모든 연도에서 상위 2개 직업 (작물 재배 종사자, 자동차 운전원)으로 나타냄
# # 반면에 건설 및 광업 단순 종사자는 2020년 및 2022년에는 상위 직업으로 속하나 2021년 다소 순위가 하락함
# 
# # 여성의 경우 매년 상위 직업군 (돌봄 및 보건 서비스 종사자, 음식 관련 단순 종사자, 매장 판매 종사자)에 속하며 특히 모든 연도에서 상위 2개 직업 (청소원 및 환경미화원, 작물 재배 종사자)으로 나타냄
# # 반면에 일부 직업 (가사 및 육아 도우미, 조리사)은 여성의 상위 직업군에 속하나 남성에는 나타나지 않음. 이는 전통적으로 남성보다 여성에게 할당되는 역할과 관련되어 있기 때문으로 판단됨
# 
# # 종합 결과
# # 2020~2022년 성별에 따른 상위 10개 직업 분포 및 시각화를 수행함.
# # 그 결과 특정 직업은 성별에 관계없이 안정적인 상위 직업군 (남성: 작물 재배 종사자, 자동차 운전원; 여성:청소원 및 환경미화원, 작물 재배 종사자) 속함. 반면에 성별 차이에 따라 특화 직업군 (남성: 건설 및 광업 단순 종사자; 여성: 가사 및 육아 도우미, 조리사)의 특징을 보임