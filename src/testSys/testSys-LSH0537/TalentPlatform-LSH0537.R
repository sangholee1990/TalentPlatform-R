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
# R을 이용한 mtcars 데이터 분석 (기초 통게량, 다양한 시각화, 상관계수)

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0537"

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
library(moments)

# (2점) cyl(실린더수) 변수로 막대그래프(bar plot)을 그려라
barplot(table(mtcars$cyl), xlab = "실린더", ylab = "빈도", main = "실린더에 대한 막대 그래프")


# (2점) mpg(연비) 변수로 히스토그램(histogram)을 그려라
hist(mtcars$mpg, xlab = "연비 (마일/갤런)", main = "연비에 대한 히스토그램")


# (2점) mpg(연비)변수의 상자그림(box plot)을 그려라
boxplot(mtcars$mpg, ylab = "연비 (마일/갤런)", main = "연비에 대한 상자 그림")


# (3점) mpg(연비)변수의 평균, 표준편차, 왜도를 구하여라
meanMpg = mean(mtcars$mpg, na.rm = TRUE)
cat(sprintf("[CHECK] 연비의 평균 : %s", meanMpg), "\n")

sdMpg = sd(mtcars$mpg, na.rm = TRUE)
cat(sprintf("[CHECK] 연비의 표준편차 : %s", sdMpg), "\n")

skewMpg = skewness(mtcars$mpg)
cat(sprintf("[CHECK] 연비의 왜도 : %s", skewMpg), "\n")


# (3점) hp(마력)변수의 평균, 표준편차, 첨도를 구하여라
meanHp = mean(mtcars$hp, na.rm = TRUE)
cat(sprintf("[CHECK] 마력의 평균 : %s", meanHp), "\n")

sdHp = sd(mtcars$hp, na.rm = TRUE)
cat(sprintf("[CHECK] 마력의 표준편차 : %s", sdHp), "\n")

kurtHp = kurtosis(mtcars$hp)
cat(sprintf("[CHECK] 마력의 첨도 : %s", kurtHp), "\n")


# (4점) mpg(연비)변수와 hp(마력)변수의 상관계수를 구하고 의미를 설명하여라
corVal = cor(mtcars$mpg, mtcars$hp)
cat(sprintf("[CHECK] 연비 및 마력의 상관계수 : %s", corVal), "\n")

# 연비 및 마력의 상관계수는 -0.78로서 높은 음의 관계를 지님
# 즉 연비가 증가할수록 마력이 감소함을 의미함
