
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
# R을 이용한 swmmr 패키지를 활용한 연계 작업 그리고 DEoptim를 통해 유출량 모의

# SWMM 프로그램 setup 파일 및 SWMM 프로그램 실행을 위한 inp, rpt, out 파일 및 R코드를 함께 업로드했습니다.
# 저의 요구사항은 다음과 같습니다.
# 1. swmmr 패키지를 활용한 inp 파일 실행
# 2. DEoptim 패키지를 활용하여 모델링 된 유역의 출구(out)에서의 유량 관측 값에 대해 nse 목적함수를 사용한 SWMM 프로그램 내 매개변수 calibration
# 크게 이 두가지 입니다.
# 
# SWMM 내 대표적인 매개변수로는 유역 변수 Subcatchment 내 'N-Imperv', 'N-Perv', 'Dstore-Imperv', 'Dstore-Perv'가 있고 
# 가능하다면 Conduit 변수 내 'Roughness' 변수를 함께 calibration 하고 싶습니다. 

# Calibration을 위한 유역 출구의 유량 관측 값은 'runoff201007.csv' 입니다.

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0337"
contextPath = ifelse(env == "local", ".", "E:/04. TalentPlatform/Github/TalentPlatform-R")

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

# ******************************************************************************
# 라이브러리 읽기
# ******************************************************************************
library(swmmr)
library(DEoptim)
library(xts)
library(hydroGOF)

# 난수 정의
set.seed(84)


# ******************************************************************************
# 함수 정의
# ******************************************************************************
# NSE 검증스코어
# nse <- function(x) {
#   1 - sum( ((x[, 1] - x[, 2]) ^ 2), na.rm = TRUE) / sum( ((x[, 1] - mean(x[, 1], na.rm = TRUE)) ^ 2), na.rm = TRUE)
# }


# 목적함수 설정
setObj = function(x, inp, obs) {
  
  # set new parameters and update inp object
  inp$subcatchments = transform(
    inp$subcatchments,
    Perc_Imperv = ifelse(Perc_Imperv, x, Perc_Imperv)
    # Perc_Imperv = ifelse(Perc_Imperv > 0.0001, x, Perc_Imperv)
  )
  
  # 테스트
  # inp$subcatchments = transform(
  #   inp$subcatchments,
  #   Width = ifelse(Width > 0.0001, x, Width)
  # )
  # 
  # inp$subareas = transform(
  #   inp$subareas,
  #   'N-Imperv' = ifelse('N-Imperv' > 0.0001, x, N-Imperv)
  # )

  # write new inp file to disk
  tmp_inp = tempfile()
  swmmr::write_inp(inp, tmp_inp)
  
  # run swmm with new parameter set
  swmmFileInfo = suppressMessages(run_swmm(tmp_inp, stdout = NULL))
  
  # remove files when function exits to avoid heavy disk usage
  on.exit(file.remove(unlist(swmm_files)))
  
  # read sim result
  sim = read_out(
    file = swmmFileInfo$out # path to out file
    , iType = 1 # type: node
    , object_name = "out" # name of node
    , vIndex = 4 # parameter at node: total inflow
  )$out$total_inflow # directly access to xts object
  # [["out"]]$flow_rate # directly access to xts object
  
  # calculate goodness-of-fit
  # note: multiply by minus one to have a real min problem (nse: +1 to -Inf)
  # nse(merge(obs, sim)) * -1
  hydroGOF::NSE(obs, sim)
}


# ******************************************************************************
# 설정 파일 설정
# ******************************************************************************
inpFile = Sys.glob(file.path(globalVar$inpPath, serviceName, "TESTcalibrationj07.inp"))
rptFile = Sys.glob(file.path(globalVar$inpPath, serviceName, "TESTcalibrationj07.rpt"))
outFile = Sys.glob(file.path(globalVar$inpPath, serviceName, "TESTcalibrationj07.out"))
exeFile = Sys.glob(file.path("C:/Program Files (x86)/EPA SWMM 5.1.015/swmm5.exe"))


# ******************************************************************************
# obs 파일 읽기
# ******************************************************************************
# df = read.csv("F:/대학원/박사/GCM SWMM par real/runoff201007.csv")
obsFile = Sys.glob(file.path(globalVar$inpPath, serviceName, "runoff201007.csv"))
df = read.csv(obsFile)
obsdata = df[2:nrow(df),2]
dates = seq(as.Date("2010-07-02"),length=30,by="days")

obsData = as.xts(x=obsdata,order.by=dates)
# class(obs)


# ******************************************************************************
# swmmr 실행
# ******************************************************************************
swmmFileInfo = swmmr::run_swmm(
  inp = inpFile
  , rpt = rptFile
  , out = outFile
  , exec = exeFile
)


# ******************************************************************************
# 유출량 테스트
# ******************************************************************************
# 입력 데이터 확인
inpData = swmmr::read_inp(swmmFileInfo$inp)

# 유출량 모의
simRes = read_out(
  file = swmmFileInfo$out # path to out file
  , iType = 1 # type: node
  , object_name = "out" # name of node
  , vIndex = 4 # parameter at node: total inflow
)$out$total_inflow # directly access to xts object



# ******************************************************************************
# 유출량 검보정
# ******************************************************************************
calibRes = DEoptim::DEoptim(
  fn = setObj
  , lower = c(0, 0)
  , upper = c(100, 100)
  , control = list(
    itermax = 10 # maximum iterations
    # , trace = 1 # print progress every 10th iteration
    , packages = c("swmmr") # export packages to optimization environment
    , parVar = c("hydroGOF::NSE")
    # , parVar = c("nse") # export function to optimization environment
    # , parallelType = 0 # set to 1 to use all available cores
  )
  , inp = inpData # 'inp' object
  , obs = obsData # xts object containing observation data
)

# iterations에 따른 검증스코어 (NSE) 결과
calibRes$member$bestvalit
