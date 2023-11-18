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
# R을 이용한 요양병원 장기입원 노인의 환자유형에 따른 퇴원의향 연구

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0496"

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
library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)
library(lubridate)
library(tidyverse)
library(tidyLPA)
library(dplyr)
library(mclust)
library(multcomp)
library(psych)
library(mclust)
library(abdiv)
library(moments)
library(rstatix)

# devtools::install_github("data-edu/tidyLPA", force = TRUE)
# installed.packages("devtools")
# install.packages("tidyverse")
# install.packages("multcomp")

# showtext::showtext_opts(dpi = 100)
# showtext::showtext_opts(dpi = 600)
# showtext::showtext.auto()

# ==============================================================================
# 파일 읽기
# ==============================================================================
# 전국 병의원 및 약국 현황 자료에 근거하여 환자 대면 조사가 가능한 요양병원의 65세 이상 노인 환자를 선정
# 본 연구의 모집단은 건강보험심사평가원에서 제공하는 전국 병의원 및 약국 현황자료(2023년 3월 기준)에 근거하여, 
# 환자 대면조사가 가능한 요양병원에 입원한 65세 이상 노인 환자를 편의표집한다. 
# 본 연구의 대상자는 다음 기준에 모두 해당하는 경우에 선정한다.

# 1) 임상적 안정 상태로 퇴원이 가능하다고 의료진이 판단하였으나 입원중인 환자
# 2) 암, 당뇨, 간질환, 심장질환, 폐질환 등 노인성 만성질환을 가진 환다
# 3) 치매 또는 정신질환 진단을 받지 않은 환자
# 4) 연구에 대한 충분한 이해를 표현하고, 스스로 참여를 결정하여 동의서에 서명한 환자

# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "dataset_practice_3_1106.csv"))
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "dataset_practice_3_1107.csv"))
data = readr::read_csv(fileInfo, locale = readr::locale(encoding = "UTF-8"))

# ==============================================================================
# 데이터 점수화
# ==============================================================================
data_1 = data %>%
  dplyr::mutate(
    fam_total = fam_1 + fam_2 + fam_3 + 5 - fam_4 + fam_5,
    carebur_total = carebur_1 + carebur_2 + carebur_3 + carebur_4,
    illper_total = illper_1 + illper_2 + 5 - illper_3 + 5 - illper_4 + illper_5 + 5 - illper_6 + illper_7 + illper_8,
    satisf_total = satisf_1 + satisf_2 + satisf_3 + satisf_4 + satisf_5 + satisf_6,
    continu_total = continu_1 + continu_2 + continu_3 + continu_4 + continu_5 + continu_6 + continu_7 + continu_8 + continu_9 + continu_10 + continu_11 + continu_12,
    service_total = service_1 + service_2 + service_3 + service_4 + service_5 + service_6 + service_7,
    function_total = function_1_1 + function_1_2 + function_1_3 + function_1_4 + function_1_5 + function_1_6 + function_1_7 + function_1_8 + function_1_9 + function_1_10,
    function2_total = function_2,
    function3_total = function_3,
    function23_total = function_2 + function3_total,
    function4_total = function_4
  ) %>%
  dplyr::mutate(across(c(gender, residen_home, live_alone, live_alone_t, marriage, ltc_insurance, ltc_insurance_t, med_aid, adm_way, chronic, classification), as.factor)) %>%
  dplyr::mutate(across(c(function_total, paybur_1, age, duration, function2_total, function3_total, function23_total, function4_total), as.numeric))

# ==============================================================================
# 1. 대상자의 일반적 특성과 장기입원 관련 요인: 기술통계 (평균/표준편차)
# ==============================================================================
# 장기입원 노인환자 유형(fam_total, carebur_total, illper_totle,continu_total, service_total, function_total)
# 일반적 특성을 파악하기 위해서 주요 변수에 대한 수치화 과정 (아래 수식 참조)을 통해 평균, 표준편차를 계산함
# 특히 전체 환자 퇴원의향의 경우 평균 1.63로서 대부분 65세 이상 환자군 퇴원를 희망함
data_1_summary = data_1 %>%
  summarise(
    across(
      c(illper_total, function_total, fam_total, carebur_total, satisf_total, continu_total, service_total, paybur_1, inten_1, age, duration, function2_total, function3_total, function23_total, function4_total),
      list(mean = ~mean(.x, na.rm = TRUE), 
           sd = ~sd(.x, na.rm = TRUE),
           skew = ~skewness(.x, na.rm = TRUE), 
           kurtosis = ~kurtosis(.x, na.rm = TRUE)
           )
    )
  ) %>%
  round(2) %>% 
  t()

data_1_summary


# colnames(data_1)
data_1_summary = data_1 %>%
  summarise(
    across(
      c(illper_1:illper_8, function_1_1:function_1_10, paybur_1, fam_1:fam_5, carebur_1:carebur_4, satisf_1:satisf_6, continu_1:continu_12, service_1:service_7),
      list(mean = ~mean(.x, na.rm = TRUE)
           , sd = ~sd(.x, na.rm = TRUE)
      )
    )
  ) %>%
  round(2) %>%
  t()

data_1_summary



# gender: 성별, 남성(1), 여성(2)
# residen_home: 거주지, 대도시(1), 중소도시(2), 농어촌(3)
# live_alone:동반거주유형, 노인부부(1), 자녀 동거(2), 기타(3), 혼자거주(4)
# marriage: 배우자 유무, 없음(1), 있음(2)
# ltc_insurance: 장기요양등급 (1등급, 2등급, 3등급, 4등급, 미해당)
# med_aid: 의료급여여부(1종, 2종, 미해당)
# adm_way: 입원경로(응급, 외래)
# chronic: 만성질환수(1개, 2개 이상)
# inten_1: 퇴원의향(의향없음, 의향있음)
# classification: 환자분류군(의료최고도, 의료고도, 의료중도, 의료경도, 선택입원군)
m2 = data_1 %>%
  # dplyr::select(patient, inten_1, gender, residen_home, live_alone, marriage, ltc_insurance, med_aid, adm_way, chronic, classification, duration, age) %>%
  dplyr::select(patient, inten_1, gender, residen_home, live_alone, marriage, ltc_insurance, med_aid, adm_way, chronic, classification, age, duration, function2_total, function3_total, function4_total) %>%
  dplyr::mutate(
    inten_1_fac = inten_1,
    # age_g = factor(ifelse(age < 75, '75세미만', '75세이상')),
    # duration_l = factor(ifelse(duration < 6, '120일미만', '120일이상'), levels = c('120일미만', '120일이상')),
    gender = factor(gender, levels = c(1, 2), labels = c('남성', '여성')),
    residen_home = factor(residen_home, levels = c(1, 2, 3), labels = c('대도시', '중소도시', '농어촌')),
    live_alone = factor(live_alone, levels = c(1, 2, 3, 4), labels = c('노인부부', '자녀 동거', '기타', '혼자거주')),
    marriage = factor(marriage, levels = c(1, 2), labels = c('없음', '있음')),
    ltc_insurance = factor(ltc_insurance, levels = c(1, 2, 3, 4, 5), labels = c('1등급', '2등급', '3등급', '4등급', '미해당')),
    med_aid = factor(med_aid, levels = c(1, 2, 3), labels = c('1종', '2종', '미해당')),
    adm_way = factor(adm_way, levels = c(1, 2), labels = c('응급', '외래')),
    chronic = factor(chronic, levels = c(1, 2), labels = c('1개', '2개이상')),
    inten_1 = factor(ifelse(inten_1 <= 1, 1, 2), levels = c(1, 2), labels = c('의향없음', '의향있음')),
    age_g = factor(ifelse(age < 75, 1, 2), levels = c(1, 2), labels = c('75세미만', '75세이상')),
    duration_l = factor(ifelse(duration < 6, 1, 2), levels = c(1, 2), labels = c('120일미만', '120일이상')),
    classification = factor(classification, levels = c(1, 2, 3, 4, 5), labels = c('의료최고도', '의료고도', '의료중도', '의료경도', '선택입원군')),
    function2_total = factor(function2_total, levels = c(1, 2, 3, 4), labels = c('조절할 수 있음', '가끔 실금함', '자주 실금함', '조절 못함')),
    function3_total = factor(function3_total, levels = c(1, 2, 3, 4), labels = c('조절할 수 있음', '가끔 실금함', '자주 실금함', '조절 못함')),
    function4_total = factor(function4_total, levels = c(1, 2), labels = c('예', '아니오'))
  )

# ==============================================================================
# 1. 대상자의 일반적 특성과 장기입원 관련 요인: 빈도/백분율
# ==============================================================================
# 기초통계량(인구특성)
cnt = nrow(m2)

# 성별
a1 <- table(m2$inten_1, m2$gender); addmargins(a1); (addmargins(a1) / cnt * 100.0) %>% round(2); prop.table(a1, 2) %>% round(4) * 100 

# 연령
a2 <- table(m2$inten_1, m2$age_g); addmargins(a2); (addmargins(a2) / cnt * 100.0) %>% round(2); prop.table(a2, 2) %>% round(4) * 100 %>% round(2) 

# 거주지
a4 <- table(m2$inten_1, m2$residen_home); addmargins(a4); (addmargins(a4) / cnt * 100.0) %>% round(2); prop.table(a4, 2) %>% round(4) * 100 

# 동반거주유형
a5 <- table(m2$inten_1, m2$live_alone); addmargins(a5); (addmargins(a5) / cnt * 100.0) %>% round(2); prop.table(a5, 2) %>% round(4) * 100 

# 배우자 유무
a6 <- table(m2$inten_1, m2$marriage); addmargins(a6); (addmargins(a6) / cnt * 100.0) %>% round(2); prop.table(a6, 2) %>% round(4) * 100 

# 장기요양등급
a8 <- table(m2$inten_1, m2$ltc_insurance); addmargins(a8); (addmargins(a8) / cnt * 100.0) %>% round(2); prop.table(a8, 2) %>% round(4) * 100 

# 의료급여여부
a9 <- table(m2$inten_1, m2$med_aid); addmargins(a9); (addmargins(a9) / cnt * 100.0) %>% round(2); prop.table(a9, 2) %>% round(4) * 100 

# 입원경로
a10 <- table(m2$inten_1, m2$adm_way); addmargins(a10); (addmargins(a10) / cnt * 100.0) %>% round(2); prop.table(a10, 2) %>% round(4) * 100 

# 환자분류군
a7 <- table(m2$inten_1, m2$classification); addmargins(a7); (addmargins(a7) / cnt * 100.0) %>% round(2); prop.table(a7, 2) %>% round(4) * 100 

# 입원기간
a11 <- table(m2$inten_1, m2$duration_l); addmargins(a11); (addmargins(a11) / cnt * 100.0) %>% round(2); prop.table(a11, 2) %>% round(4) * 100 

# 만성질환수(na 3개)
a12 <- table(m2$inten_1, m2$chronic); addmargins(a12); (addmargins(a12) / cnt * 100.0) %>% round(2); prop.table(a12, 2) %>% round(4) * 100 

# 와상여부
a13 <- table(m2$inten_1, m2$function4_total); addmargins(a13); (addmargins(a13) / cnt * 100.0) %>% round(2); prop.table(a13, 2) %>% round(4) * 100 

# 대변 조절
a14 <- table(m2$inten_1, m2$function2_total); addmargins(a14); (addmargins(a14) / cnt * 100.0) %>% round(2); prop.table(a14, 2) %>% round(4) * 100 

# 소변 조절
a15 <- table(m2$inten_1, m2$function3_total); addmargins(a15); (addmargins(a15) / cnt * 100.0) %>% round(2); prop.table(a15, 2) %>% round(4) * 100 


#범주형 변수간 시각화
# par(mfrow = c(2, 2))
par(mfrow = c(1, 1))
# par(family = "AppleGothic")

# 대상자의 일반적인 특성을 파악하기 위해서 입원 전 거주지, 연령, 입원기간에 대한 상세 분석을 수행함

# 입원 전 거주지의 경우 대도시, 중소도시, 농어촌에 따라 큰 차이를 보임
# 대도시에서는 퇴원을 희망하는 환자가 많은 반면에 농어촌 지역은 반대 경향을 보임
# 이는 대도시 거주자들은 인근에서 다양한 의료 자원 및 복지 서비스를 쉽게 접근할 수 있으나
# 농촌의 경우 병원/주민센터 등의 복지를 누르기에는 상당한 제약으로 판단되기 때문임
barplot(a4, main = "입원 전 거주지에 따른 퇴원의향", xlab = "입원전 거주지(residence)", ylab = "퇴원의향(discharge will)",
        col = c("lightgrey", "lightblue"), legend = rownames(a4), beside = TRUE)

# 연령의 경우 남성 환자는 퇴원을 희망하는 경우가 많은 반면에 여성은 반대 경향을 보임
# 특히 여성 환자는 남성보다 연령이 증가할수록 만성질환 유병률이 높고 정신건강 측면, 
# 일상샐황 수행 능력이 현저히 떨어지기 때문에 퇴원의향과 간접적 영향을 끼친다고 파악됨
barplot(a1, main = "성별에 따른 퇴원의향", xlab = "입원기간(Adm.duration)", ylab = "퇴원의향(discharge will)",
        col = c("lightgrey", "lightblue"), legend = rownames(a1), beside = TRUE)


mosaicplot(inten_1 ~ age_g, data = m2,
           main = "연령에 따른 퇴원의향 분포 비교",
           xlab = "연령대(age)", ylab = "퇴원의향(will)",
           col = rainbow(length(unique(m2$inten_1))))

mosaicplot(inten_1 ~ duration_l, data = m2,
           main = "입원기간에 따른 퇴원의향 분포 비교",
           xlab = "입원기간(duration)", ylab = "퇴원의향(will)",
           col = rainbow(length(unique(m2$inten_1))))

# ==============================================================================
# 1. 대상자의 일반적 특성과 장기입원 관련 요인: 상관계수
# ==============================================================================
data_1$patient
# 상관계수 계산
corData = data_1 %>% 
  # dplyr::select(inten_1, gender, residen_home, live_alone, marriage, ltc_insurance, med_aid, adm_way, chronic, classification, age, duration) %>%
  dplyr::select(illper_total, function_total, paybur_1, fam_total, carebur_total, satisf_total, continu_total, service_total) %>%
  na.omit() %>% 
  dplyr::mutate(across(everything(), as.numeric))


# corMat = cor(corData)
# corMat %>% round(2)
# corMat[ , "inten_1"] %>% sort() %>% round(2)

corMat = rstatix::cor_mat(corData)
corPmat = rstatix::cor_pmat(corData)

# 상관계수
corMat %>%
  dplyr::select(-rowname) %>% 
  as.data.frame() %>% 
  round(2)

# 상관계수 유의성검정
corPmat %>%
  dplyr::select(-rowname) %>% 
  as.data.frame() %>% 
  round(2)

# 상관계수 행렬
ggcorrplot::ggcorrplot(corMat, hc.order = TRUE, type = "lower", lab_col = "black", outline.color = "white", lab = TRUE, p.mat = corPmat) + 
  labs(title = 'Correlation Matrix')


data_1 %>%
  summarise(
    across(
      c(illper_total, function_total, paybur_1, fam_total, carebur_total, satisf_total, continu_total, service_total),
      list(mean = ~mean(.x, na.rm = TRUE)
           , sd = ~sd(.x, na.rm = TRUE)
      )
    )
  ) %>%
  round(2) %>% 
  t()


# 퇴원 여부와의 관계성을 파악하기 위해 상관분석을 수행함
# 그 결과 퇴원 여부를 기준으로 chronic, ltc_insurance, med_aid, marriage, adm_way으로 갈수록 양의 관계인 반면
# classification, residen_home, live_alone, gender으로 음의 관계가 높음
# 특히 상관성이 높은 성별 (gender)의 경우 여성이 남성보다 퇴원의향 낮은 경향을 보인
# 반면에 입원 경로 (adm_way)에서는 응급보다 외래를 통한 입원 환자가 퇴원 의향이 더 높음
# 이는 응급 환자의 경우 심각한 건강 상태나 즉각적인 의료적 필요성을 실감하여 퇴원에 신중하나
# 외래 환자에서는 상대적인 안정적인 건강상태를 지니고 있어 퇴원의향이 높을 수 있음
# ==============================================================================
# 1. 대상자의 일반적 특성과 장기입원 관련 요인: 내적 합치도
# ==============================================================================
# 내적 합치도 Cronbach's alpha 계산
cronAlpha = data_1 %>% 
  dplyr::select(inten_1, gender, residen_home, live_alone, marriage, ltc_insurance, med_aid, adm_way, chronic, classification, age, duration) %>%
  dplyr::mutate(across(everything(), as.numeric)) %>% 
  psych::alpha(check.keys = TRUE)

cronAlpha$alpha.drop

# 퇴원 여부와의 관계성을 파악하기 위해 내적합치도를 계산함
# 그 결과 대부분 0.7 이하로서 낮은 내적 합치도를 지님에도 불구하고 대부분 0.26~0.38로 분포함
# 이에 65세 이상의 고령 환자에 대한 데이터 신뢰도 향상이 요구되기 때문에 
# 이 연구에서는 다양한 통계적 검정 (카이제곱 검정, 일원배치 분산분석 등)을 통해 타당성을 확보함

# ==============================================================================
# 2. 장기입원 관련요인에 따른 잠재유형 결과 및 해석
# 장기입원 관련요인에 따른 잠재유형(Latent profile)을 도출하기 위해서 잠재프로파일 분석을 수행
# 가장 적합한 지수를 가진 잠재유형 수를 결정
# ==============================================================================
# 장기입원 관련요인을 도출하기 위해서 7종 주요 변수를 통해 잠재유형 (Latent profile)을 수행함
# 최적의 프로파일을 찾기 위해서 1~10 프로파일을 설정하여 시뮬레이션하였고 AIC 검증 지표에서 가장 낮은 오차를 선정함
# 그 결과 최종적인 잠재유형 수는 8로 결정함
set.seed(1)

modelCfg = data_1 %>%
  # dplyr::select(fam_total:function_total, age, duration) %>%
  # dplyr::select(illper_total, function_total, paybur_1, fam_total, carebur_total, satisf_total, continu_total, service_total) %>%
  # paybur_1 변수 제외 : 잠재 프로파일 분석과정에서 10개 미만 고유값일 경우 오류 발생
  dplyr::select(illper_total, function_total, fam_total, carebur_total, satisf_total, continu_total, service_total) %>%
  # na.omit() %>%
  single_imputation() %>%
  scale()

# 1~5 n_profiles 프로파일 시뮬레이션을 통해 최적의 프로파일 찾기
simData = tibble::tibble()
for (i in 1:4) {
  set.seed(1)
  
  model = tidyLPA::estimate_profiles(modelCfg, n_profiles = i, variances = "equal", covariances = "zero")
  # model = tidyLPA::estimate_profiles(modelCfg, n_profiles = i, variances = "equal", covariances = "varying")
  # model = tidyLPA::estimate_profiles(modelCfg, n_profiles = i, variances = "varying", covariances = "zero")
  # model = tidyLPA::estimate_profiles(modelCfg, n_profiles = i, variances = "varying", covariances = "varying")

  valData = model[[1]][[2]] %>%
    as.tibble()
  
  simData = dplyr::bind_rows(simData, valData)
}

# 표4 장기입원 노인환자 모형 적합도
simDataL1 = simData %>% 
  # dplyr::arrange(BIC) %>%
  as.data.frame() %>% 
  round(2)

for (cls in simDataL1$Classes) {
  modelProf = estimate_profiles(modelCfg, n_profiles = cls, variances = "equal", covariances = "zero")
  
  modelResData = get_data(modelProf) %>% 
    as.tibble()
  
  modelResDataL1 = modelResData %>% 
    dplyr::group_by(Class) %>% 
    dplyr::summarise(
      cnt = n()
      , rat = cnt / nrow(modelResData) * 100.0
    ) %>% 
    dplyr::mutate(
      label = sprintf("%s (%s)", round(rat, 2), cnt)
    )
}


# 가장 적합한 지수를 지닌 잠재유형 수 결정
# 현재 검증지표에 따라 최적의 결과 상이
# 즉 학위논문 주제에 따라 변경 필요
bestData = simData %>% 
  # dplyr::arrange(KIC) %>%
  # dplyr::arrange(AWE) %>%
  # dplyr::arrange(CLC) %>%
  # dplyr::arrange(AIC) %>%
  dplyr::arrange(BIC) %>%
  dplyr::slice(1)

bestData

m3 = tidyLPA::estimate_profiles(modelCfg, n_profiles = bestData$Classes, variances = "equal", covariances = "zero")

# 밀도함수 시각화
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "plot_density")
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

tidyLPA::plot_density(m3) +
  theme(text = element_text(size = 16)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n") 

# 산포도 시각화
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "plot_bivariate")
# dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# 
# tidyLPA::plot_bivariate(m3, rawdata = FALSE) +
#   theme(text = element_text(size = 16)) +
#   ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)
# 
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# 프로파일 시각화
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "plot_profiles")
dir.create(path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

tidyLPA::plot_profiles(m3) +
  theme(text = element_text(size = 16)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# class count
get_data(m3) %>%
  dplyr::group_by(Class) %>%
  count()

#주요 변수 combine
c3 = get_data(m3) %>% 
  as.tibble() %>% 
  dplyr::select(Class)

data_2 = data_1 %>% 
  dplyr::select(fam_total:function_total, paybur_1)

# colnames(m2)
# colnames(data_2)

data_3 = cbind(m2, data_2, c3) %>%
  dplyr::mutate(
    Class_fac = as.factor(Class),
    inten_1_num = as.numeric(inten_1)
  )



# 그림4 요양병원 장기입원 노인 환자에 따른 잠재유형2 잠재프로파일
# <표 5> 노인 환자 잠재유형의 응답 평균 및 표준편차
# statData$key %>% unique() %>% sort()
statData = data_3 %>%
  dplyr::select(c(illper_total, function_total, paybur_1, fam_total, carebur_total, satisf_total, continu_total, service_total, Class)) %>% 
  tidyr::gather(-Class, key = "key", value = "val") %>% 
  dplyr::mutate(
    label = dplyr::case_when(
      stringr::str_detect(key, regex("carebur_total")) ~ "돌봄 부담에 대한 인지"
      , stringr::str_detect(key, regex("continu_total")) ~ "돌봄 지속성"
      , stringr::str_detect(key, regex("fam_total")) ~ "가족 지지"
      , stringr::str_detect(key, regex("function_total")) ~ "기능적 독립"
      , stringr::str_detect(key, regex("illper_total")) ~ "질병 인식"
      , stringr::str_detect(key, regex("paybur_1")) ~ "환자 의료비 부담"
      , stringr::str_detect(key, regex("satisf_total")) ~ "거주 만족도"
      , stringr::str_detect(key, regex("service_total")) ~ "지역사회서비스 접근용이성"
    )
  ) %>% 
  dplyr::group_by(Class, label) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
    , sdVal = sd(val, na.rm = TRUE)
  )

# 정렬
statData$label = forcats::fct_relevel(statData$label, c("가족 지지", "돌봄 부담에 대한 인지", "질병 인식", "환자 의료비 부담", "거주 만족도", "돌봄 지속성", "지역사회서비스 접근용이성", "기능적 독립"))
statData$Class = as.factor(statData$Class)

mainTitle = sprintf("요양병원 장기입원 노인 환자에 따른 잠재유형2 잠재프로파일 %s", "막대그래프")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

ggplot(statData, aes(x = label, y = meanVal, color = Class, group = Class)) +
  geom_line() +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = scales::comma(meanVal)), vjust = -1.0, size = 4, show.legend = FALSE) +
  labs(x = NULL, y = "평균", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  theme(
    text = element_text(size = 16)
    , axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")


# ==============================================================================
# 3. 잠재유형에 따른 퇴원의향의 차이 비교: 
# ==============================================================================
# 2. 노인 환자 잠재유형에 따른 퇴원의향
# 가. 노인 환자 잠재유형간 퇴원 의향 차이
statData = data_3 %>% 
  dplyr::group_by(Class, inten_1_fac) %>% 
  dplyr::summarise(
    cnt = n()
    , rat = cnt / nrow(data_3) * 100.0
  ) %>% 
  dplyr::mutate(
    label = sprintf("%s (%s)", round(rat, 2), cnt)
  )
print(statData)

# 잠재유형에 따른 퇴원의향 평균/표준편차
data_3 %>% 
  dplyr::group_by(Class) %>% 
  dplyr::summarise(
    meanVal = mean(inten_1_num, na.rm = TRUE), 
    sd = sd(inten_1_num, na.rm = TRUE)
  )

# 잠재유형 (독립변수 범주형)과 퇴원의향 (종속변수 범주형)에 대한 독립성 검증 (카이제곱 검정)
tableRes = base::table(data_3$inten_1, data_3$Class_fac)
chiRes = chisq.test(tableRes)
chiRes

# 잠재유형과 퇴원의향에 대한 독립성 검증을 위해서 카이제곱 검정을 수행함
# 그 결과 P값는 0.13으로서 0.05 이하에서 통계적으로 유의미한 관계가 없기 때문에 서로 간의 관계성이 없음

# Pearson's Chi-squared test
# 
# data:  tableRes
# X-squared = 11.073039, df = 7, p-value = 0.1354647

# 일원배치 분산분석은 하나의 범주형 독립변수가 연속형 종속변수에 미치는 효과를 평가합니다. 
# 퇴원의향이 점수화 되어 있을 경우(예를 들어, 퇴원 의향의 강도)

# 잠재유형 (종속변수 범주형)에 따른 퇴원의향 (종속변수 연속형)의 평균 차이평가 (일원배치 분산분석)
# 잠재유형 (종속변수 범주형)에 따른 퇴원의향 (종속변수 연속형)의 평균 차이 평가를 위해서 일원배치 분산분석을 수행함
# 그 결과 P값는 0.13으로서 0.05 이하에서 통계적으로 유의미한 관계가 없기 때문에 서로 간의 평균 차이는 우연에 의해 발생했을 가능성이 높음 (연관성 없음)
aovRes = aov(inten_1_num ~ Class_fac, data = data_3)
summary(aovRes)

# plot(aovRes, 1)

# 사후 검정을 위해서 잠재유형 간의 평균이 통계적 유의성 검정
# thkRes = TukeyHSD(aovRes)
# summary(thkRes, which = "Class_fac")
# plot(thkRes)

# boxplot으로 비교하기
boxplot(data = data_3, inten_1 ~ Class, main = "환자유형에 따른 퇴원의향 비교", xlab = "환자유형", ylab = "퇴원의향", col = c("lightgreen"))

# 일반적 특성에 따른 퇴원의향 차이
# demog_aov = aov(inten_1 ~ gender + residen_home + live_alone + marriage + ltc_insurance + med_aid + chronic + adm_way + classification + age_g + duration_l, data = data_3)
demog_aov = aov(inten_1_num ~ gender + residen_home + live_alone + marriage + ltc_insurance + med_aid + chronic + adm_way + classification, data = data_3)
summary(demog_aov) #residen_home=.004**, adm_way=.019*, duration_1=.047* (adm_way는 n수 부족)

# 거주지 차이: 중소도시와 대도시간의 퇴원의향의 평균에 차이가 있다. p=.003**
summary(glht(demog_aov, linfct = mcp(residen_home = "Tukey"))) 

# 외래/응급 p=.099
summary(glht(demog_aov, linfct = mcp(adm_way = "Tukey")))

# 120일 이상/이하 p=.476*
# summary(glht(demog_aov, linfct = mcp(duration_l = "Tukey"))) 

# ==============================================================================
# 4. 잠재유형의 퇴원의향에 영향을 미치는 요인 
# ==============================================================================
# 로지스틱 회귀분석을 통해 잠재유형의 퇴원의향에 영향을 미치는 요인을 탐색
data_4 = data_3 %>% 
  dplyr::mutate(
    inten_1_num = ifelse(inten_1 == "의향없음", 0, 1)
  ) %>% 
  na.omit() %>% 
  dplyr::select(inten_1_num, Class, fam_total, carebur_total, illper_total, satisf_total, continu_total, service_total, function_total, paybur_1, age, duration)

testData = data_4

# 전체 변수에 대한 로지스틱 회귀모형 수행
# 독립변수 : 퇴원의향 제외한 전체 변수
# 종속변수 : 퇴원의향
glmFitVarAll = glm(inten_1_num ~ ., data = data_4, family = binomial)

# 기본값으로 변수 선택
# rsStep = step(glmFitVarAll)
# summary(rsStep)

# AIC 기준으로 변수 선택
rsStepAic = MASS::stepAIC(glmFitVarAll, direction = "both")

# 결과에 대한 요약
summary(rsStepAic)

# 한 눈에 분석 결과 확인 가능
rsStepAic$anova


# 테스트셋을 이용한 예측
obs = testData$inten_1_num
prd = predict.glm(rsStepAic, newdata = testData, type = "response")
prdBin = ifelse(prd > 0.5, 1, 0)

# 검증 측정을 위한 기초 설정
lmPred = ROCR::prediction(prd, obs)

# ROC 커브를 위한 설정
perform = ROCR::performance(lmPred, "tpr", "fpr")
plot(perform, main = 'ROC Curve')

# AUC 측정 : 1에 가까울수록 최고 성능 : 0.69
ROCR::performance(lmPred, "auc")@y.values[[1]]

# 이항편차 측정 : 낮을수록 좋음 : 56.67
# abdiv::binomial_deviance(obs, prd)


# 분할표에서 퇴원 O/X일때 실측 (obs), 예측 (prdBin)에서 서로간의 동일할 경우 높은 성능을 보임
# 즉 반면에 실측/예측에서 서로간의 퇴원여부가 서로 다를 다소 오차를 보임
# 이는 데이터 개수가 155개로 적기 때문에 다양한 조건에 대해 학습자료의 부재로 판단됨
# 따라서 다종다양한 학습 데이터를 수집할 뿐만 아니라 분석 변수를 포함한다면 퇴원여부의 예측 성능이 향상될 것으로 사료됨
#    prdBin
# obs  0  1
# 0 46 30
# 1 25 48
table(obs, prdBin)



# 장기입원 환자유형에 따른 퇴원의향의 차이를 확인하기 위해 정규성 검증 (일원배치 분산분석)
# 그 결과 P값는 0.02으로서 0.05 이하에서 통계적으로 유의미하기 때문에 정규분포를 따르지 않는다고 판단됨
tableRes = table(data_3$inten_1, data_3$Class_fac)
tableRes

shaRes = shapiro.test(tableRes)
shaRes
