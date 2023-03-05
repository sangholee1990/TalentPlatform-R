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
# R 관련 Rgeoprofile 라이브러리 수동 설치 및 예제 테스트
# https://evolve.sbcs.qmul.ac.uk/lecomber/sample-page/geographic-profiling/geographic-profiling-in-r/

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0404"

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
  # source(here::here(file.path(contextPath, "src"), "InitConfig.R"), encoding = "UTF-8")
  source(file.path(contextPath, "src", "InitConfig.R"))
}

#================================================
# 비즈니스 로직 수행
#================================================
# [작업사항] Rgeoprofile 라이브러리 설치
# rtools, r 최신 버전
# 이미 최신 r 및 rtools 다운로드 완료

# 파일 다운로드 -> 파일명 변경 (Rgeoprofile_1.2.tar.gz)
# https://www.dropbox.com/s/8wbbfjqi5779jz1/Rgeoprofile_1.2.tar%202.gz

# 설치 및 의존 라이브러리 설치
# install.packages("coda")
# install.packages("C:/Users/sin17/Desktop/Rgeoprofile_1.2.tar.gz", repos = NULL, type ="source", dependencies = TRUE)
# install.packages("C:/Users/sangh/OneDrive/바탕 화면/Rgeoprofile_1.2.tar.gz", repos = NULL, type ="source", dependencies = TRUE)

# 라이브러리 읽기
library(Rgeoprofile)

# 매뉴얼 참조하여 테스트 
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "dummy crime sites.txt"))
mydata = read.table(fileInfo, header=FALSE)
mysources = read.table(fileInfo, header=FALSE)

# Rgeoprofile::LoadData(Data=mydata, sources = "NULL")
Rgeoprofile::LoadData(Data=mydata, sources = mysources)

sigma = 1
tau = "DEFAULT"


Rgeoprofile::ModelParameters()
Rgeoprofile::ModelParameters(sigma = 1, tau = "DEFAULT", minburnin = 100, maxburnin = 1000, chains = 5, Samples = 10000)

Rgeoprofile::GraphicParameters(Guardrail = 0.05, nring = 20, transp = 0.4, gridsize = 640, gridsize2 = 300, MapType = "roadmap", Location = getwd(), pointcol = "black")

Rgeoprofile::CreateMaps(PlotPrior = T)

points(mysources , pch = 15 , col = "blue")

Rgeoprofile::RunMCMC()

Rgeoprofile::ThinandAnalyse(thinning = 100)

# Rgeoprofile::PlotGP(Window = "DEFAULT" , Legend = T)

# Rgeoprofile::Reporthitscores()