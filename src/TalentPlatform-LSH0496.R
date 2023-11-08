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
# R을 이용한 일반음식점 데이터 기반으로 지도 및 막대그래프 시각화

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

# devtools::install_github("data-edu/tidyLPA")
# installed.packages("devtools")
# install.packages("tidyverse")
# install.packages("multcomp")

# showtext::showtext_opts(dpi = 100)
# showtext::showtext_opts(dpi = 600)
# showtext::showtext.auto()

# ==============================================================
# 
# ==============================================================
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "dataset_practice_3_1106.csv"))
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "dataset_practice_3_1107.csv"))
data = readr::read_csv(fileInfo, locale = readr::locale(encoding = "UTF-8"))

#data setting
# setwd('/Users/junlovesyoon/Desktop/dataset')
#data import_ older_LTC_will (practice 100 samples)
# data_1<-read.csv(file="dataset_practice_coding_1106.csv", header= TRUE,stringsAsFactors = F)


#데이터 점수 합산 및 변경
dataL1 = data %>%
  dplyr::mutate(
    fam_total = fam_1 + fam_2 + fam_3 + 5 - fam_4 + fam_5,
    carebur_total = carebur_1 + carebur_2 + carebur_3 + carebur_4,
    illper_total = illper_1 + illper_2 + 5 - illper_3 + 5 - illper_4 + illper_5 + 5 - illper_6 + illper_7 + illper_8,
    satisf_total = satisf_1 + satisf_2 + satisf_3 + satisf_4 + satisf_5 + satisf_6,
    continu_total = continu_1 + continu_2 + continu_3 + continu_4 + continu_5 + continu_6 + continu_7 + continu_8 + continu_9 + continu_10 + continu_11 + continu_12,
    service_total = service_1 + service_2 + service_3 + service_4 + service_5 + service_6 + service_7,
    function_total = function_1_1 + function_1_2 + function_1_3 + function_1_4 + function_1_5 + function_1_6 + function_1_7 + function_1_8 + function_1_9 + function_1_10
  ) %>%
  # Convert multiple columns to factors at once
  dplyr::mutate(across(c(gender, residen_home, live_alone, live_alone_t, marriage, ltc_insurance, ltc_insurance_t, med_aid, adm_way, chronic, classification), as.factor)) %>%
  # Convert columns to numeric
  # dplyr::mutate(across(c(age, function_total, paybur_1), as.numeric))
  dplyr::mutate(across(c(function_total, paybur_1), as.numeric))


# data_1 <- data_1 %>% mutate(fam_total=fam_1+fam_2+fam_3+5-fam_4+fam_5) %>% #역환산 4번 문항
#   mutate(carebur_total=carebur_1+carebur_2+carebur_3+carebur_4) %>% 
#   mutate(illper_total=illper_1+illper_2+5-illper_3+5-illper_4+illper_5+5-illper_6+illper_7+illper_8) %>% #역환산 3,4,6번
#   mutate(satisf_total=satisf_1+satisf_2+satisf_3+satisf_4+satisf_5+satisf_6) %>% 
#   mutate(continu_total=continu_1+continu_2+continu_3+continu_4+continu_5+continu_6+continu_7+continu_8+continu_9+continu_10+continu_11+continu_12) %>% 
#   mutate(service_total=service_1+service_2+service_3+service_4+service_5+service_6+service_7) %>% 
#   mutate(function_total=function_1_1+function_1_2+function_1_3+function_1_4+function_1_5+function_1_6+function_1_7+function_1_8+function_1_9+function_1_10) 
# 
# data_1$gender<-as.factor(data_1$gender)
# data_1$residen_home<-as.factor(data_1$residen_home)
# data_1$live_alone<-as.factor(data_1$live_alone)
# data_1$live_alone_t<-as.factor(data_1$live_alone_t)
# data_1$marriage<-as.factor(data_1$marriage)
# data_1$ltc_insurance<-as.factor(data_1$ltc_insurance)
# data_1$ltc_insurance_t<-as.factor(data_1$ltc_insurance_t)
# data_1$med_aid<-as.factor(data_1$med_aid)
# data_1$adm_way<-as.factor(data_1$adm_way)
# data_1$chronic<-as.factor(data_1$chronic)
# data_1$classification<-as.factor(data_1$classification)
# data_1$age<-as.numeric(data_1$age)
# data_1$function_total<-as.numeric(data_1$function_total)
# data_1$paybur_1<-as.numeric(data_1$paybur_1)

#빈도표준편차
#장기입원 노인환자 유형(fam_total, carebur_total, illper_totle,continu_total, service_total, function_total)

data_1_summary <- dataL1 %>%
  summarise(
    across(
      c(illper_total, function_total, fam_total, carebur_total, satisf_total, 
        continu_total, service_total, paybur_1, inten_1),
      list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE))
    )
  )

# data_1$illper_total %>% mean()
# data_1$illper_total %>% sd()
# data_1$function_total %>% mean()
# data_1$function_total %>% sd()
# data_1$fam_total %>% mean()
# data_1$fam_total %>% sd()
# data_1$carebur_total %>% mean()
# data_1$carebur_total%>% sd()
# data_1$satisf_total %>% mean()
# data_1$satisf_total %>% sd()
# data_1$continu_total %>% mean()
# data_1$continu_total %>% sd()
# data_1$service_total %>% mean()
# data_1$service_total %>% sd()
# data_1$paybur_1 %>% mean()
# data_1$paybur_1 %>% sd()

#전체 환자 퇴원의향 평균, 표준편차
# data_1$inten_1 %>% mean()
# data_1$inten_1 %>% sd()

#desctiptive analysis

# admission duration calculation
# colnames(dataL1)

# duration, age 컬럼 부재
m2 <- dataL1 %>%
  # dplyr::select(patient, inten_1, gender, residen_home, live_alone, marriage, ltc_insurance, med_aid, adm_way, chronic, classification, duration, age) %>%
  dplyr::select(patient, inten_1, gender, residen_home, live_alone, marriage, ltc_insurance, med_aid, adm_way, chronic, classification) %>%
  mutate(
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
    classification = factor(classification, levels = c(1, 2, 3, 4, 5), labels = c('의료최고도', '의료고도', '의료중도', '의료경도', '선택입원군'))
  )
# m2<-select(data_1,patient,inten_1,gender,residen_home,live_alone,marriage, ltc_insurance,med_aid,adm_way,chronic,classification,duration, age)
# 
# m2$age_g<-ifelse(m2$age<75,1,2) #75세 미만 1, 이상 2
# #m2$inten_1<-ifelse(m2$inten_1<=1,1,2) #퇴원의향 없음 1, 있음 2
# m2$duration_l<-ifelse(m2$duration<6,1,2) #6개월 미만 입원 1, 이상 2 
# str(m2)
# 
# #범주형: 빈도수, 최비치, 비율, 백분율 등 분포 특성 중심성, 변동성 파악 필요/연관성 파악
# #m2$inten_1<-factor(m2$inten_1,levels=c(1,2),labels=c('의향없음','의향있음'))
# m2$gender<-factor(m2$gender,levels=c(1,2),
#                   labels=c('남성','여성'))
# 
# m2$residen_home<-factor(m2$residen_home,level=c(1,2,3),
#                         labels=c('대도시','중소도시','농어촌'))
# m2$live_alone<-factor(m2$live_alone,levels=c(1,2,3,4),
#                       labels=c('노인부부','자녀 동거', '기타','혼자거주'))
# m2$marriage<-factor(m2$marriage,level=c(1,2),
#                     labels=c('없음','있음'))
# m2$ltc_insurance<-factor(m2$ltc_insurance,levels=c(1,2,3,4,5),
#                          labels=c('1등급','2등급','3등급','4등급','미해당'))
# m2$adm_way<-factor(m2$adm_way,levels=c(1,2),
#                    labels=c('응급','외래'))
# m2$chronic<-factor(m2$chronic,levels=c(1,2),
#                    labels=c('1개','2개이상'))
# 
# m2$classification<-factor(m2$classification,levels=c(1,2,3,4,5),
#                           labels=c('의료최고도','의료고도','의료중도','의료경도','선택입원군'))
# 
# #환자입원기간 분류(입원환자부담금 기준)
# m2$duration_l<-factor(m2$duration_l,levels=c(1,2),
#                       labels=c('120일미만', '120일이상'))
# #전기노인, 후기노인으로 구분 
# m2$age_g<-factor(m2$age_g,levels=c(1,2),
#                  labels=c('75세미만','75세이상'))
# m2$med_aid<-factor(m2$med_aid,levels=c(1,2,3),
#                    labels=c('1종','2종','미해당'))

#기초통계량(인구특성)
a1<-table(m2$inten_1,m2$gender);addmargins(a1);prop.table(a1,2)*100 #성별
a2<-table(m2$inten_1,m2$age_g);addmargins(a2); prop.table(a2,2)*100 #연령
a4<-table(m2$inten_1,m2$residen_home);addmargins(a4);prop.table(a4,2)*100 #거주지
a5<-table(m2$inten_1,m2$live_alone) ;addmargins(a5);prop.table(a5,2)*100#동반거주유형
a6<-table(m2$inten_1,m2$marriage);addmargins(a6);prop.table(a6,2)*100 #배우자 유무
a7<-table(m2$inten_1,m2$classification);addmargins(a7) ;prop.table(a7,2)*100 #환자분류군
a8<-table(m2$inten_1,m2$ltc_insurance) ;addmargins(a8);prop.table(a8,2)*100#장기요양등급
a9<-table(m2$inten_1,m2$med_aid) ;addmargins(a9);prop.table(a9,2)*100 #의료급여여부
a10<-table(m2$inten_1,m2$adm_way) ;addmargins(a10);prop.table(a10,2)*100#입원경로
a11<-table(m2$inten_1,m2$duration_l) ;addmargins(a11);prop.table(a11,2)*100#입원기간
a12<-table(m2$inten_1,m2$chronic) ;addmargins(a12);prop.table(a12,2)*100#만성질환수(na 3개)

#범주형 변수간 시각화
par(mfrow=c(2,2))
par(family="AppleGothic")
barplot(a4,main="입원 전 거주지에 따른 퇴원의향", xlab="입원전 거주지(residence)", ylab="퇴원의향(discharge will)",
        col=c("lightgrey","lightblue"),legend=rownames(a4),beside=TRUE)

barplot(a1,main="성별에 따른 퇴원의향", xlab="입원기간(Adm.duration)", ylab="퇴원의향(discharge will)",
        col=c("lightgrey","lightblue"),legend=rownames(a1),beside=TRUE)


mosaicplot(inten_1~age_g, data=m2,
           main="연령에 따른 퇴원의향 분포 비교",
           xlab="연령대(age)",ylab="퇴원의향(will)",
           col=rainbow(length(unique(m2$inten_1))))

mosaicplot(inten_1~duration_l, data=m2,
           main="입원기간에 따른 퇴원의향 분포 비교",
           xlab="입원기간(duration)",ylab="퇴원의향(will)",
           col=rainbow(length(unique(m2$inten_1))))

#집단간 차이 분석
a1
prop.test(a1, alternative = "two.sided", conf.level = 0.95)
prop.test(a2, alternative = "two.sided", conf.level = 0.95)
prop.test(a, alternative = "two.sided", conf.level = 0.95)


# LPA analysis in R 

c1<-data_1[1:100,]%>%
  select(fam_total:function_total, paybur_1) %>%
  single_imputation() %>%
  scale() %>%
  estimate_profiles(1:7,variances = "equal", covariances = "zero") #AIC, BIC the lowest penalty = 3 %
#paybur_1 violates the assumption...

plot_profiles(c1)
compare_solutions(c1,statistics=c("AIC","BIC"))
get_fit(c1)

c2<-data_1[1:100,]%>%
  select(fam_total:function_total,paybur_1) %>%
  single_imputation() %>%
  scale() %>%
  estimate_profiles(5, variances = "equal", covariances = "zero") #AIC, BIC the lowest penalty = 3 %

#plot of the profiles
plot_profiles(c1[[5]],rawdata = FALSE, add_line = T)

#Distribution plot for a specific(density) e.g. 3 class solution
plot_density(c1[[5]]) %>%
  plot_bivariate(c1[[5]],rawdata = FALSE)
#Estimates for the variables
get_fit(m3) #extra fit indextes
get_estimates(c1[[5]]) #means and standard error of data

m3<-data_1[1:100,]%>%
  select(fam_total:function_total) %>%
  single_imputation() %>%
  scale() %>%
  estimate_profiles(5, variances = "equal", covariances = "zero") #model1
get_data(m3) %>% group_by(Class) %>% count() #class count

#주요 변수 combine

c3<-get_data(c2) %>%data.frame() #prints the data out if you wanted to use it for subsequent research
c4<-c3 %>% select(Class)

data_2<-data_1 %>%select(fam_total:function_total,paybur_1)
data_3<-cbind(m2,data_2,c4) 
data_3$Class<-as.factor(data_3$Class)

#유형별 퇴원의향 평균 /표준편차
data_3$inten_1<-as.numeric(data_3$inten_1)
data_3 %>% aggregate(inten_1~Class,mean) 
data_3 %>% aggregate(inten_1~Class,sd)

#유형에 따른 퇴원의향에 차이가 있는지

#환자 유형에 따른 퇴원의향 평균, 표준편차 차이
tapply(inten_1,Class,summary) #평균, 최대값 최소값
tapply(inten_1,Class,sd) #표준편차

#ANOVA
class_aov<-aov(inten_1~Class,data=data_3)
summary(class_aov)
plot(class_aov,1) #class 5 개 간에 퇴원의향 평균차이 있음. p-value=.004**
bartlett.test(inten_1~Class, data=data_3)
TukeyHSD(class_aov, which="Class") #class 5 vs class 3 or class 4 에서 평균의 차이가 있음.

#boxplot으로 비교하기
attach(data_3)
boxplot(inten_1~Class,
        main="환자유형에 따른 퇴원의향 비교",
        xlab="환자유형",
        ylab="퇴원의향",
        col=c("lightgreen"))


#일반적 특성에 따른 퇴원의향 차이
demog_aov<-aov(inten_1~gender+residen_home+live_alone+marriage+ltc_insurance+med_aid+chronic+adm_way+classification+age_g+duration_l, data=data_3)
summary(demog_aov) #residen_home=.004**, adm_way=.019*, duration_1=.047* (adm_way는 n수 부족)

summary(glht(demog_aov,linfct=mcp(residen_home="Tukey"))) #거주지 차이: 중소도시와 대도시간의 퇴원의향의 평균에 차이가 있다. p=.003**
summary(glht(demog_aov,linfct=mcp(adm_way="Tukey"))) #외래/응급 p=.099
summary(glht(demog_aov,linfct=mcp(duration_l="Tukey"))) #120일 이상/이하 p=.476*

#그룹별데이터 추출 ->필요한가?
class_1<-subset.data.frame(data_3, Class==1)
class_2<-subset.data.frame(data_3, Class==2)
class_3<-subset.data.frame(data_3, Class==3)
class_4<-subset.data.frame(data_3, Class==4)
class_5<-subset.data.frame(data_3, Class==5)

#환자유형별 일반적 특성 분석, 관련 요인 파악









# # 대한민국 지리 데이터
# mapKor = sf::st_read(file.path(globalVar$mapPath, "gadm36_KOR_shp/gadm36_KOR_1.shp"))
# 
# # 파일 읽기
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "07_24_04_P.csv"))
# data = readr::read_csv(fileInfo, locale = readr::locale(encoding = "UTF-8"))
# 
# dataL1 = data %>%
#   tidyr::separate(col = "도로명전체주소", into = c("sido"), sep = " ") %>%
#   dplyr::mutate(
#     dtYear = format(인허가일자, "%Y")
#     , sidoAddr = dplyr::case_when(
#       stringr::str_detect(sido, regex("부산광역시")) ~ "부산"
#       , stringr::str_detect(sido, regex("충청북도")) ~ "충북"
#       , stringr::str_detect(sido, regex("충청남도")) ~ "충남"
#       , stringr::str_detect(sido, regex("대구광역시")) ~ "대구"
#       , stringr::str_detect(sido, regex("대전광역시")) ~ "대전"
#       , stringr::str_detect(sido, regex("강원특별자치도")) ~ "강원"
#       , stringr::str_detect(sido, regex("광주광역시")) ~ "광주"
#       , stringr::str_detect(sido, regex("경기도")) ~ "경기"
#       , stringr::str_detect(sido, regex("경상북도")) ~ "경북"
#       , stringr::str_detect(sido, regex("경상남도")) ~ "경남"
#       , stringr::str_detect(sido, regex("인천광역시")) ~ "인천"
#       , stringr::str_detect(sido, regex("제주특별자치도")) ~ "제주"
#       , stringr::str_detect(sido, regex("전라북도")) ~ "전북"
#       , stringr::str_detect(sido, regex("전라남도")) ~ "전남"
#       , stringr::str_detect(sido, regex("세종특별자치시")) ~ "세종"
#       , stringr::str_detect(sido, regex("서울특별시")) ~ "서울"
#       , stringr::str_detect(sido, regex("울산광역시")) ~ "울산"
#     )
#   )
# 
# # =======================================================================================================
# # 1. 전국 업종별 일반음식점 사업체 수(2023년 기준)
# # =======================================================================================================
# # 비중이 높은 순으로 10개 업종만 막대그래프로 시각화
# dataL2 = dataL1 %>%
#   dplyr::filter(
#     인허가일자 >= as.Date("2023-01-01")
#   ) %>%
#   dplyr::group_by(업태구분명) %>%
#   dplyr::summarise(
#     cnt = n()
#   ) %>%
#   na.omit() %>%
#   dplyr::arrange(desc(cnt)) %>%
#   dplyr::slice(1:10)
# 
# # 정렬
# dataL2$업태구분명 = forcats::fct_relevel(dataL2$업태구분명, dataL2$업태구분명)
# 
# mainTitle = sprintf("2023년 전국 업종별 일반음식점 사업체 %s", "막대그래프")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# 
# ggplot(dataL2, aes(x = 업태구분명, y = cnt, fill = 업태구분명)) +
#   geom_bar(stat = "identity", width = 1.0, position = position_dodge(width = 1.0), alpha = 0.6) +
#   labs(x = "10개 업종", y = "사업체 수", fill = NULL, title = NULL, subtitle = mainTitle) +
#   ggrepel::geom_text_repel(aes(label = scales::comma(cnt)), position = position_dodge(width = 1.0), color = "white", vjust = -0.5, size = 4) +
#   theme(
#     text = element_text(size = 16)
#     , axis.text.x = element_text(angle = 45, hjust = 1)
#     , legend.position = "none"
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
# 
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# 
# # 전국업종별 분포를 원형그래프로 시각화
# dataL3 = dataL2 %>%
#   dplyr::mutate(per = (cnt / sum(dataL2$cnt, na.rm = TRUE)) * 100) %>%
#   dplyr::mutate(name = sprintf("%s\n(%s %%)", 업태구분명, round(per, 1)))
# 
# mainTitle = sprintf("2023년 전국 업종별 일반음식점 사업체 %s", "원형그래프")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# 
# ggplot(dataL3, aes(x = '', y = per, fill = 업태구분명)) +
#   geom_bar(stat = 'identity', alpha = 0.6) +
#   coord_polar('y', start = 0) +
#   ggrepel::geom_label_repel(aes(label = name), position = position_stack(vjust = 0.5), color = "white", size = 4, alpha = 0.8) +
#   labs(x = NULL, y = NULL, fill = NULL, title = NULL, subtitle = mainTitle) +
#   theme_minimal() +
#   theme_classic() +
#   theme(
#     text = element_text(size = 16)
#     , axis.line = element_blank()
#     , axis.text = element_blank()
#     , axis.ticks = element_blank()
#     , legend.position = "none"
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
# 
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# 
# 
# # 총 사업체 수 기준으로 지도 시각화
# dataL4 = dataL1 %>%
#   dplyr::group_by(sido, sidoAddr) %>%
#   dplyr::summarise(
#     cnt = n()
#   ) %>%
#   na.omit() %>%
#   dplyr::arrange(desc(cnt))
# 
# dataL5 = dataL4 %>%
#   dplyr::mutate(
#     per = (cnt / sum(dataL4$cnt, na.rm = TRUE)) * 100
#   ) %>%
#   dplyr::mutate(
#     GID_1 = dplyr::case_when(
#       stringr::str_detect(sido, regex("부산광역시")) ~ "KOR.1_1"
#       , stringr::str_detect(sido, regex("충청북도")) ~ "KOR.2_1"
#       , stringr::str_detect(sido, regex("충청남도")) ~ "KOR.3_1"
#       , stringr::str_detect(sido, regex("대구광역시")) ~ "KOR.4_1"
#       , stringr::str_detect(sido, regex("대전광역시")) ~ "KOR.5_1"
#       , stringr::str_detect(sido, regex("강원특별자치도")) ~ "KOR.6_1"
#       , stringr::str_detect(sido, regex("광주광역시")) ~ "KOR.7_1"
#       , stringr::str_detect(sido, regex("경기도")) ~ "KOR.8_1"
#       , stringr::str_detect(sido, regex("경상북도")) ~ "KOR.9_1"
#       , stringr::str_detect(sido, regex("경상남도")) ~ "KOR.10_1"
#       , stringr::str_detect(sido, regex("인천광역시")) ~ "KOR.11_1"
#       , stringr::str_detect(sido, regex("제주특별자치도")) ~ "KOR.12_1"
#       , stringr::str_detect(sido, regex("전라북도")) ~ "KOR.13_1"
#       , stringr::str_detect(sido, regex("전라남도")) ~ "KOR.14_1"
#       , stringr::str_detect(sido, regex("세종특별자치시")) ~ "KOR.15_1"
#       , stringr::str_detect(sido, regex("서울특별시")) ~ "KOR.16_1"
#       , stringr::str_detect(sido, regex("울산광역시")) ~ "KOR.17_1"
#     )
#     # , name = sprintf("%s\n%s / %s %%", sidoAddr, scales::comma(cnt), round(per, 1))
#     , name = sprintf("%s\n%s\n%s%%", sidoAddr, scales::comma(cnt), round(per, 1))
#   )
# 
# dataL6 = mapKor %>%
#   dplyr::left_join(dataL5, by = c("GID_1" = "GID_1")) %>%
#   dplyr::mutate(
#     centroids = sf::st_centroid(geometry)
#   ) %>%
#   dplyr::mutate(
#     lon = sf::st_coordinates(centroids)[, 1],
#     lat = sf::st_coordinates(centroids)[, 2]
#   )
# 
# # 지도 시각화
# mainTitle = sprintf("2023년 전국 업종별 일반음식점 사업체 %s", "지도")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# 
# ggplot(data = dataL6) +
#   geom_sf(aes(geometry = geometry, fill = cnt, group = sido), alpha = 0.6) +
#   ggrepel::geom_label_repel(aes(x = lon, y = lat, label = name), size = 3.5, box.padding = 0.0, label.padding = 0.2, fill = scales::alpha("white", 0.7), arrow = grid::arrow(length = unit(0.0, "inches"))) +
#   scale_fill_continuous(trans = "reverse") +
#   metR::scale_x_longitude(breaks = seq(125, 131, 1), limits = c(125, 131), expand = c(0, 0)) +
#   metR::scale_y_latitude(breaks = seq(33, 39, 1), limits = c(33, 39), expand = c(0, 0)) +
#   labs(subtitle = mainTitle, x = NULL, y = NULL, fill = NULL, colour = NULL, title = NULL) +
#   theme(legend.position = "none") +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
# 
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# 
# # =======================================================================================================
# # 2. 10대 또는 5대 업종의 지역별 분포
# # 지역별 비교가 목적이기 때문에, 2023년 기준 17개 시도 지역별 업종의 비중 비교(원형그래프 시각화)
# # =======================================================================================================
# # sidoAddrInfo = sidoAddrList[1]
# # sidoAddrInfo = "대전"
# sidoAddrList = dataL1$sidoAddr %>% unique() %>% sort()
# for (sidoAddrInfo in sidoAddrList) {
#   
#   dataL2 = dataL1 %>%
#     dplyr::filter(
#       인허가일자 >= as.Date("2023-01-01")
#     ) %>%
#     dplyr::filter(sidoAddr == sidoAddrInfo) %>%
#     dplyr::group_by(업태구분명) %>%
#     dplyr::summarise(
#       cnt = n()
#     ) %>%
#     na.omit() %>%
#     dplyr::arrange(desc(cnt)) %>%
#     dplyr::slice(1:10)
#   
#   # 정렬
#   dataL2$업태구분명 = forcats::fct_relevel(dataL2$업태구분명, dataL2$업태구분명)
#   
#   dataL3 = dataL2 %>%
#     dplyr::mutate(per = (cnt / sum(dataL2$cnt, na.rm = TRUE)) * 100) %>%
#     dplyr::mutate(name = sprintf("%s\n(%s %%)", 업태구분명, round(per, 1)))
#   
#   mainTitle = sprintf("2023년 %s 업종별 일반음식점 사업체 %s", sidoAddrInfo, "원형그래프")
#   saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
#   dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#   
#   makePlot = ggplot(dataL3, aes(x = '', y = per, fill = 업태구분명)) +
#     geom_bar(stat = 'identity', alpha = 0.6) +
#     coord_polar('y', start = 0) +
#     ggrepel::geom_label_repel(aes(label = name), max.iter = 1000000, position = position_stack(vjust = 0.5), color = "white", size = 4, alpha = 0.8) +
#     labs(x = NULL, y = NULL, fill = NULL, title = NULL, subtitle = mainTitle) +
#     theme_minimal() +
#     theme_classic() +
#     theme(
#       text = element_text(size = 16)
#       , axis.line = element_blank()
#       , axis.text = element_blank()
#       , axis.ticks = element_blank()
#       , legend.position = "none"
#     )
#   
#   ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
#   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# }
# 
# 
# # =======================================================================================================
# # 3. 시계열자료로는 특정 세부업종으로, 치킨통닭과 김밥(도시락) 냉면집 이렇게 3개가 필요한데요,
# # 최근 10년간(2013년~2023년) 개폐업 현황을 막대그래프로 시각화
# # 단위는 전국, 서울, 대전, 세종 이렇게 가능할지 검토 부탁드립니다.
# # 3번 관련해서는, CSV자료에 영업기간도 도출할 수 있는데,
# # 통닭(치킨), 김밥(도시락), 냉면집
# # 서울, 대전, 세종, 전국
# # =======================================================================================================
# typeList = c("통닭(치킨)", "김밥(도시락)", "냉면집")
# sidoList = c("서울", "대전", "세종")
# 
# # typeInfo = typeList[1]
# # sidoInfo = sidoList[1]
# for (typeInfo in typeList) {
#   for (sidoInfo in sidoList) {
#     
#     dataL2 = dataL1 %>%
#       dplyr::filter(
#         업태구분명 == typeInfo
#         , sidoAddr == sidoInfo
#         , dplyr::between(인허가일자, as.Date("2013-01-01"), as.Date("2023-12-31"))
#       ) %>%
#       dplyr::mutate(
#         dtYear = format(인허가일자, "%Y") %>% as.numeric()
#       ) %>%
#       dplyr::group_by(dtYear, 영업상태명, 업태구분명) %>%
#       dplyr::summarise(
#         cnt = n()
#       ) %>%
#       na.omit() %>%
#       dplyr::arrange(dtYear)
#     
#     mainTitle = sprintf("2013~2023년 %s (%s) 개폐업 현황", typeInfo, sidoInfo)
#     saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
#     dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#     
#     makePlot = ggplot(dataL2, aes(x = dtYear, y = cnt, color = 영업상태명)) +
#       geom_line() +
#       geom_point() +
#       ggrepel::geom_text_repel(aes(label = scales::comma(cnt)), vjust = -1.0, size = 4, show.legend = FALSE) +
#       scale_x_continuous(minor_breaks = 2013:2023, breaks = 2013:2023) +
#       labs(x = "연도 ", y = "개폐업 수", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
#       theme(
#         text = element_text(size = 16)
#         , legend.position = "top"
#       )
#     
#     ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
#     cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#   }
# }
# 
# 
# for (typeInfo in typeList) {
#   dataL2 = dataL1 %>%
#     dplyr::filter(
#       업태구분명 == typeInfo
#       , dplyr::between(인허가일자, as.Date("2013-01-01"), as.Date("2023-12-31"))
#     ) %>%
#     dplyr::mutate(
#       dtYear = format(인허가일자, "%Y") %>% as.numeric()
#     ) %>%
#     dplyr::group_by(dtYear, 영업상태명, 업태구분명) %>%
#     dplyr::summarise(
#       cnt = n()
#     ) %>%
#     na.omit() %>%
#     dplyr::arrange(dtYear)
#   
#   mainTitle = sprintf("2013~2023년 %s (%s) 개폐업 현황", typeInfo, "전국")
#   saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
#   dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#   
#   makePlot = ggplot(dataL2, aes(x = dtYear, y = cnt, color = 영업상태명)) +
#     geom_line() +
#     geom_point() +
#     ggrepel::geom_text_repel(aes(label = scales::comma(cnt)), vjust = -1.0, size = 4, show.legend = FALSE) +
#     scale_x_continuous(minor_breaks = 2013:2023, breaks = 2013:2023) +
#     labs(x = "연도 ", y = "개폐업 수", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
#     theme(
#       text = element_text(size = 16)
#       , legend.position = "top"
#     )
#   
#   ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
#   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# }
# 
# # =======================================================================================================
# # 혹시 세부업종+특정 지역 결합해서 그 지역의 폐업업종 중 영업기간으로 분류하여 
# # 시각화할 수 있을지도 검토 부탁드립니다
# # =======================================================================================================
# for (typeInfo in typeList) {
#   for (sidoInfo in sidoList) {
#     
#     dataL2 = dataL1 %>%
#       dplyr::filter(
#         업태구분명 == typeInfo
#         # , sidoAddr == sidoInfo
#         , 영업상태명 == "폐업"
#         , dplyr::between(인허가일자, as.Date("2013-01-01"), as.Date("2023-12-31"))
#       ) %>%
#       dplyr::mutate(
#         dtYear = format(인허가일자, "%Y") %>% as.numeric()
#         , diffYear = lubridate::interval(인허가일자, 폐업일자) / lubridate::dyears(1)
#         , name = dplyr::case_when(
#           diffYear < 1 ~ "1년 미만"
#           , diffYear >= 1 & diffYear < 3 ~ "1-3년"
#           , diffYear >= 3 & diffYear < 5 ~ "3-5년"
#           , diffYear >= 5 & diffYear < 7 ~ "5-7년"
#           , diffYear >= 7 & diffYear < 9 ~ "7-9년"
#           , TRUE ~ "9년 이상"
#         )
#       ) %>%
#       dplyr::group_by(영업상태명, name) %>%
#       dplyr::summarise(
#         cnt = n()
#       ) %>%
#       na.omit() %>%
#       dplyr::arrange(name)
#     
#     mainTitle = sprintf("2013~2023년 %s (%s) 폐업기간 분류", typeInfo, sidoInfo)
#     saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
#     dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
#     
#     # 정렬
#     dataL2$name = forcats::fct_relevel(dataL2$name, c("1년 미만", "1-3년", "3-5년", "5-7년", "7-9년", "9년 이상"))
#     
#     makePlot = ggplot(dataL2, aes(x = name, y = cnt, fill = name)) +
#       geom_bar(stat = "identity", width = 0.75, position = position_dodge(width = 1.0), alpha = 0.6, show.legend = FALSE) +
#       ggrepel::geom_text_repel(aes(label = scales::comma(cnt)), position = position_dodge(width = 1.0), color = "white", vjust = 0.0, size = 5) +
#       labs(x = "폐업 기간 ", y = "개폐업 수", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
#       theme(
#         text = element_text(size = 16)
#         , legend.position = "top"
#       ) +
#       guides(color = guide_legend(nrow = 1))
#     
#     ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
#     cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#   }
# }
# 
# # ==============================================================
# # 2023.10.22
# # ==============================================================
# # # 파일 읽기
# # fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "07_24_04_P.csv"))
# # data = readr::read_csv(fileInfo, locale = readr::locale(encoding = "UTF-8"))
# # 
# # # 업종별 개폐업현황
# # dataL1 = data %>% 
# #   dplyr::rename(
# #     geoX = "좌표정보(X)"
# #     , geoY = "좌표정보(Y)"
# #   ) %>% 
# #   dplyr::select(영업상태명, 업태구분명, 인허가일자, geoX, geoY) %>% 
# #   na.omit()
# # 
# # # 좌표 변환
# # sfData = st_as_sf(dataL1, coords = c("geoX", "geoY"), crs = 2097)
# # sfDataL1 = st_transform(sfData, 4326)
# # 
# # dataL2 = sfDataL1 %>%
# #   dplyr::mutate(
# #     lon = st_coordinates(.)[,1]
# #     , lat = st_coordinates(.)[,2]
# #   ) %>%
# #   st_set_geometry(NULL) 
# # 
# # # summary(dataL2)
# # 
# # # 연도별 일반음식점 (전체) 개폐업 현황
# # dataL3 = dataL2 %>% 
# #   dplyr::mutate(
# #     dtYear = format(인허가일자, "%Y") %>% as.numeric()
# #   ) %>% 
# #   dplyr::group_by(dtYear, 영업상태명) %>%
# #   dplyr::summarise(
# #     cnt = n()
# #   )
# # 
# # mainTitle = sprintf("연도별 일반음식점 (%s) 개폐업 현황", "전체")
# # saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# # dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# # 
# # ggplot(dataL3, aes(x = dtYear, y = cnt, fill = 영업상태명)) +
# #   geom_bar(stat = "identity", width = 1.0, position = position_dodge(width = 1.0)) +
# #   labs(x = "연도 ", y = "개수", fill = NULL, title = NULL, subtitle = mainTitle) +
# #   theme(
# #     text = element_text(size = 16)
# #     , legend.position = "top"
# #   ) +
# #   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
# # 
# # cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# # 
# # 
# # # 연도별 일반음식점 (업태구분명) 개폐업 현황
# # typeList = dataL2$업태구분명 %>% unique() %>% sort()
# # for (typeInfo in typeList) {
# #   
# #   dataL4 = dataL2 %>% 
# #     dplyr::filter(
# #       업태구분명 == typeInfo
# #     ) %>% 
# #     dplyr::mutate(
# #       dtYear = format(인허가일자, "%Y") %>% as.numeric()
# #     ) %>% 
# #     dplyr::group_by(dtYear, 영업상태명) %>%
# #     dplyr::summarise(
# #       cnt = n()
# #     )
# #   
# #   mainTitle = sprintf("연도별 일반음식점 (%s) 개폐업 현황", typeInfo)
# #   saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# #   dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# #   
# #   makePlot = ggplot(dataL4, aes(x = dtYear, y = cnt, fill = 영업상태명)) +
# #     # geom_bar(stat = "identity", width = 0.5, position=position_dodge(width = 0.5)) +
# #     # geom_bar(stat = "identity", position=position_dodge(width = 0.5)) +
# #     geom_bar(stat = "identity", width = 1.0, position = position_dodge(width = 1.0)) +
# #     labs(x = "연도 ", y = "개수", fill = NULL, title = NULL, subtitle = mainTitle) +
# #     theme(
# #       text = element_text(size = 16)
# #       , legend.position = "top"
# #     )
# #   
# #   ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# #   
# #   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# # }
# # 
# # # 지도 시각화
# # # 대한민국 지리 데이터
# # mapKor = sf::st_read(file.path(globalVar$mapPath, "gadm36_KOR_shp/gadm36_KOR_1.shp"))
# # mapKor2 = sf::st_read(file.path(globalVar$mapPath, "gadm36_KOR_shp/gadm36_KOR_2.shp"))
# # 
# # typeList = dataL2$영업상태명 %>% unique() %>% sort()
# # for (typeInfo in typeList) {
# #   
# #   dataL5 = dataL2 %>%
# #     dplyr::filter(영업상태명 == typeInfo) %>%
# #     dplyr::select(lon, lat)
# #   
# #   mainTitle = sprintf("일반음식점 %s 지도", typeInfo)
# #   saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# #   dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# #   
# #   makePlot = ggplot() +
# #     geom_sf(data = mapKor2, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = "white") +
# #     geom_sf(data = mapKor, aes(x = NULL, y = NULL, fill = NULL, z = NULL), lwd = 0.5, color = "black", fill = NA) +
# #     # geom_point(data = dataL5, aes(x = lon, y = lat), alpha = 0.1) +
# #     stat_density_2d(data = dataL5, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = "polygon") +
# #     scale_fill_gradient(low = "green", high = "red") +
# #     scale_alpha(range = c(0, 1.0), guide = FALSE) +
# #     metR::scale_x_longitude(breaks = seq(125, 131, 1), limits = c(125, 131), expand = c(0, 0)) +
# #     metR::scale_y_latitude(breaks = seq(33, 39, 1), limits = c(33, 39), expand = c(0, 0)) +
# #     labs(subtitle = mainTitle, x = NULL, y = NULL, fill = NULL, colour = NULL, title = NULL)
# #   
# #   ggsave(makePlot, filename = saveImg, width = 10, height = 8, dpi = 600)
# #   
# #   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
# # }