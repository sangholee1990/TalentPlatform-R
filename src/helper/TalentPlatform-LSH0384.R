
# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0384"

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
  source(here::here(file.path(contextPath, "src"), "InitConfig.R"), encoding = "UTF-8")
}

#================================================
# 비즈니스 로직 수행
#================================================
# R을 이용한 주사위 대상으로 부트스트랩의 평균 및 중앙값 및 90% 신뢰구간
# 1. 육면체 주사위를 연속으로 10번 던졌을 때의 결과를 기록했습니다. 결과는 다음과 같습니다.
# 3,5,1,6,2,3,4,4,2,1
#
# R 프로그래밍을 통해 부트스트래핑 방법을 사용하여 평균 및 중앙값에 대한 다이 결과의 90% 신뢰 구간을 결정합니다.
# 단순화를 위해 부트스트래핑 횟수 10을 사용합니다.
# 이 계산을 위해 R 코드를 작성하고 평균과 중앙값에 대한 신뢰 구간을 표시해야 합니다.

# 난수 초기값
set.seed(123)

# 표본 주사위
sampleInfo = c(1:6)
# 부트스트랩 횟수
bootDo = 10
# 부트스트랩 추출 개수
bootNum = 10

# 부스스트랩 주사위 목록
bostSample = lapply(1:bootDo, function(i) sample(sampleInfo, size=bootNum, replace = TRUE))

bostMean = mapply(mean, bostSample)
bostMeanConf = quantile(bostMean, p = c(0.10, 0.90))

cat(sprintf("[CHECK] 평균에 대한 평균 : %s", mean(bostMean, na.rm  =TRUE)), "\n")
cat(sprintf("[CHECK] 평균에 대한 90%% 신뢰구간 : %s ~ %s", bostMeanConf[1], bostMeanConf[2]), "\n")

bostMed= mapply(median, bostSample)
bostMedConf = quantile(bostMed, p = c(0.10, 0.90))

cat(sprintf("[CHECK] 중앙값에 대한 평균 : %s", mean(bostMed, na.rm  =TRUE)), "\n")
cat(sprintf("[CHECK] 중앙값에 대한 90%% 신뢰구간 : %s ~ %s", bostMedConf[1], bostMedConf[2]), "\n")
