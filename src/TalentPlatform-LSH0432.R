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
# R을 이용한 기업간의 이직의 영향 분석

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0432"

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
library(xlsx)
library(ggpubr)
library(ggrepel)
library(Hmisc)
library(corrplot)

# loading packages
library(DBI)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(sjPlot)
library(Hmisc)
library(pequod)
library(car)
library(R.matlab)
library(VGAM)
library(multilevel)
library(plm)
library(tidyr)
library(lmtest)


# Set working directory (adjust this so it points to the directory on your machine 
# where the files are)
# setwd("E:/R")



# creating a new dataset "finaldata"
# finaldata <- read.csv(file = "emba_dataset_team project.csv")
# summary(finaldata)



##### testing the models  ###################################################################
# 파일 읽기
fileList = Sys.glob(file.path(globalVar$inpPath, serviceName, "emba_dataset_team+project.csv"))

data = readr::read_csv(fileList) %>% 
  readr::type_convert()

summary(data)

# 전체 변수에 대한 다중선형 회귀모형 수행
# 독립변수 : 2019년 기준 직원 이직률 (turnover19) 제외한 전체 변수
# 종속변수 : 2019년 기준 직원 이직률 (turnover19)
lmFit = lm(turnover19 ~ ., data = data)

# 전체 변수에 대한 요약 결과
summary(lmFit)

# 전체 변수에서 변수 선택
stepFit = step(lmFit)

# 유의미한 변수에 대한 요약 결과
summary(stepFit)

stepFit$coefficients %>% sort() %>% round(4)
# 2019년 채용 인력 중 퇴사자 비율
# 2019년 기준 기업의 혁신 전략
# 2019 년 기준 노사관계 대립 정도

# 기업 id


# 2019년 기준 직원 인당 연평균 교육훈련 시간
# 2019년 기준 노조 유무 (유노조=1, 무노조=0)


lmFit = pequod::lmres(turnover19 ~ ., data = data)
summary(lmFit)

# 


















# 2021년 유럽 기준으로 언론자유지수 및 경제성장률의 추세 시각화
plotSubTitle = sprintf("%s", "2021년 유럽 기준으로 언론자유지수 및 경제성장률의 추세 시각화")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

makePlot = ggpubr::ggscatter(
  data = data, x = "rsf", y = "gdp", add = "reg.line"
) +
  labs(title = NULL, x = "언론 자유 지수 (RSF)", y = "경제 성장률 (GDP)", color = NULL, subtitle = plotSubTitle) +
  theme_bw() +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 0.95, size = 6) +
  ggpubr::stat_cor(label.x.npc = 0.5, label.y.npc = 0.95, p.accuracy = 0.01, r.accuracy = 0.01, size = 6) +
  ggrepel::geom_text_repel(aes(label = nation), colour = "grey50") +
  theme(
    text = element_text(size = 18)
    , legend.position = "top"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# 자료 전처리
dataL1 = data %>% 
  dplyr::select(gdp, rsf)

# 상관계수 테이블
corData = Hmisc::rcorr(as.matrix(dataL1))
corMat = corData$r
pMat = corData$P

# 상관계수 그림
plotSubTitle = sprintf("%s", "2021년 유럽 기준으로 언론자유지수 및 경제성장률의 상관계수")
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)

png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
corrplot(corMat, type = "upper", order = "hclust", method = "number", p.mat = pMat, sig.level = 0.05, insig = "blank")
dev.off()

cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")