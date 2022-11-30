#================================================
# 요구사항
#================================================
# R을 이용한 서울시 아파트 실거래가 회귀분석 및 주택 가격 결정 요인

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0367"

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
# Main
#================================================
Sys.setenv(PROJ_LIB = "/usr/local/anaconda3/envs/r36/share/proj")

# library Load, 패키지 확인(check)하고 없으면 설치하고 load
# requiredPackages <- c("rgdal", "tidyr", "dplyr", "plot.matrix", "data.table", "foreign", "stringr", "plyr", "grid", "ggplot2", "colorspace", "magick", "stringr", "raster", 'reshape2', 'viridis', 'gstat', 'measurements', 'sf', 'sp', 'ggplot2', 'ggrepel', 'tidyverse','extrafont', 'plyr', 'rgdal', 'ggtext', 'plyr', 'gridtext', 'sysfonts', 'grDevices', 'colorRamps', 'gdata')

# for (p in requiredPackages) {
#   if (!require(p, character.only = TRUE)) install.packages(p)
#   require(p, character.only = TRUE)
# }

# 폴더 경로 설정
# setwd("E:/자료_test")

# 강우 이벤트 분석 기간
Period <- "20220906" 
start<- as.POSIXct(paste(as.Date(Period, '%Y%m%d'), "00:00",sep=" "))
end<- as.POSIXct(paste(as.Date(Period, '%Y%m%d'), "23:50",sep=" "))
time <-format(seq.POSIXt(start, end, by = "10 min"), format = "%Y%m%d%H%M")

# 격자 정보( No : 격자 번호)

# xy_Bsn <- fread("Test_Info_xy_1km.txt", header=TRUE)
xy_Bsn <- fread(file.path(globalVar$inpPath, serviceName, "Test_Info_xy_1km.txt"), header=TRUE)
# x       y             xy     No

# 레이더 10분 강우(행: xy_Bsn의 No, 열 : 해당 기간 10분 레이더 값, 총 144개)
# Rain <- fread("Test_RDR_1km_220906.txt", header=TRUE)
Rain <- fread(file.path(globalVar$inpPath, serviceName, "Test_RDR_1km_220906.txt"), header=TRUE)

# 위와 같은 포맷, 해당 강우 모형별 값 
# D_U1 <- fread("Test_D_U1_1km_220906.txt", header=TRUE)
# D_N2 <- fread("Test_D_N2_1km_220906.txt", header=TRUE)
D_U1 <- fread(file.path(globalVar$inpPath, serviceName, "Test_D_U1_1km_220906.txt"), header=TRUE)
D_N2 <- fread(file.path(globalVar$inpPath, serviceName, "Test_D_N2_1km_220906.txt"), header=TRUE)

# xy_Bsn의 격자내 특성값(행 : No*16개, No4 : 격자내 구분 , Adj~ : 주변격자, cnt~: 주변격자 유출특성, )
# Info <- read.csv("Test_DB_V5.csv", fill = TRUE, colClasses =c( 'numeric', 'numeric', 'character','character', 'character', 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'))
Info <- read.csv(file.path(globalVar$inpPath, serviceName, "Test_DB_V5.csv"), fill = TRUE, colClasses =c( 'numeric', 'numeric', 'character','character', 'character', 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'))

summary(xy_Bsn)
summary(Rain)
summary(D_U1)
summary(D_N2)
summary(Info)
# 시간별(10M) 특성에 따른 격자별 최대값 산출
# i = 36
dataL1 = tibble::tibble()
for (i in 36:length(time)){   # i<-36부터 test

    # 격자 번호와 레이더 값, 유출값 결합
    # 열을 선택 (29행)
    All.RD_Bsn <- data.table(No=xy_Bsn$No, Rain=Rain[[i]], D_U1=D_U1[[i]], D_N2=D_N2[[i]])
    colnames(All.RD_Bsn)[1]<- "No"

    # 격자내 특성값(No4)에 결합 
    Info3 <- merge(Info, All.RD_Bsn, by='No')
    # head(Info)
    # head(Info3)
    # tail(Info3)

    # dd = Info %>%
    #   dplyr::filter(No == 272219)
      # dplyr::arrange(desc(No4))

    #%>%
      # dplyr::arrange(No)
      # dplyr::arrange(desc(No4))
      # dplyr::filter(No == 272219)

    # dim(Info)
    # dim(All.RD_Bsn)
    # dim(Info3)

    # head(xy_Bsn)
    # head(Rain)
    # head(D_U1)
    # head(D_N2)
    # colnames(Info3)

    # merging Info3 with All.RD_Bsn, 주변격자의 모형별 유출량 값 결합
    # k = 4
    for (k in 1:4){
        colnames(All.RD_Bsn)[1]<- paste0("Adj_No_", k)
        temp<- merge(Info3[paste0("Adj_No_", k)], All.RD_Bsn, by=paste0("Adj_No_", k), all.x=T)
        # temp<- merge(Info3[paste0("Adj_No_", k)], All.RD_Bsn, by=paste0("Adj_No_", k))
        Info3[paste0("Adj_D_U1_", k)] <- temp[, 3]
    }
    # head(Info3)

    # colnames(Info3)
    # k = 4
    for (k in 1:4){
        colnames(All.RD_Bsn)[1]<- paste0("Adj_No_", k)
        temp<- merge(Info3[paste0("Adj_No_", k)], All.RD_Bsn, by=paste0("Adj_No_", k), all.x=T)
        Info3[paste0("Adj_D_N2_", k)] <- temp[, 4]
    }

    # head(Info3)
    #
    # # Info3$Adj_D_N2_4

    colnames(All.RD_Bsn)[1]<- "No"
    # colnames(Info3)
    #
    # dd = Info3 %>%
    #   dplyr::select(No, No4, Adj_D_U1_4, Adj_D_N2_4, cnt_N2_4, Adj_D_U1_4)

    # 모형별 유출량 산정, 주변격자의 유출량*특성값 연산
    # k = 4
    # k = 3
    for ( k in 1:4){
        Info3[paste0("No4.D_U1_", k)] <- (Info3[paste0("cnt_U1_", k)] * Info3[paste0("Adj_D_U1_", k)] * 0.7 + Info3[paste0("cnt_N2_", k)] * Info3[paste0("Adj_D_N2_", k)] * 0.8) / 15.0
    }

    # Info3 %>%
    #   dplyr::select(No4.D_U1_1, cnt_U1_1, Adj_D_U1_1, cnt_N2_1, Adj_D_N2_1)

    # Info3 %>%
    #   dplyr::select(No4.D_U1_3, cnt_U1_3, Adj_D_U1_3, cnt_N2_1, Adj_D_N2_3)

    # (0.8  * 2.82313 * 0.7 + 0.1 * 1.91992 * 0.8)/15.0

    # 7 19 11 23

    #  [1] "No"         "No4"        "Adj_No_1"   "Adj_No_2"   "Adj_No_3"   "Adj_No_4"   "cnt_U1_1"   "cnt_U1_2"   "cnt_U1_3"
    # [10] "cnt_U1_4"   "cnt_N2_1"   "cnt_N2_2"   "cnt_N2_3"   "cnt_N2_4"   "ratio"      "Rain"       "D_U1"       "D_N2"
    # [19] "Adj_D_U1_1" "Adj_D_U1_2" "Adj_D_U1_3" "Adj_D_U1_4" "Adj_D_N2_1" "Adj_D_N2_2" "Adj_D_N2_3" "Adj_D_N2_4"

    # 17 18  11 23
    # paste0("No4.D_U1_", k)

    for ( k in 1:4){
        Info3[paste0("No4.D_N2_", k)] <- (Info3$D_U1*0.3+ Info3$D_N2*0.4+Info3[paste0("cnt_N2_", k)] * Info3[paste0("Adj_D_N2_", k)] * 0.8) / 15.0
    }

    # Info3 %>%
    #   dplyr::select(No4.D_U1_1, No4.D_U1_2, No4.D_U1_3, No4.D_U1_4, No4.D_N2_1, No4.D_N2_2, No4.D_N2_3, No4.D_N2_4)
      # dplyr::select(No4.D_U1_1, No4.D_U1_2, No4.D_U1_3, No4.D_U1_4, No4.D_N2_1, D_U1, D_N2, cnt_N2_1, Adj_D_N2_1)
    # dplyr::mutate(rtn = (D_U1 * 0.3 + D_N2 * 0.4 + cnt_N2_1 * Adj_D_N2_1 * 0.8) / 15.0 )

    # dd = Info3 %>%
    #   dplyr::select(No4.D_U1_1, No4.D_U1_2, No4.D_U1_3, No4.D_U1_4, No4.D_N2_1, No4.D_N2_2, No4.D_N2_3, No4.D_N2_4)

    # head(Info3)
    # tail(Info3)
    # colnames(Info3)


    # selData = Info3[startsWith(names(Info3), "No4.D")]
    # colnames(selData)
    # head(selData)

    # 유출량 평가 Index 산정, 격자(No)별 최대값 산정 
    Info3$sum <- rowSums(Info3[startsWith(names(Info3), "No4.D")], na.rm=TRUE)
    Info3$Index <- Info3$sum * Info3$ratio
    Info3 <- tbl_df(Info3)
    # head(Info3)

    # selData = Info3 %>%
    #   dplyr::select(No, colnames(selData), sum, Index)


    # 중복 제거
    maxIndex <- Info3 %>%
      dplyr::group_by(No) %>%
      dplyr::slice(which.max(Index))
    # head(maxIndex)

    # 결과 파일 저장
    summary <- data.table(No=maxIndex$No, Rain=maxIndex$Rain,  Index=maxIndex$Index)
    # write.fwf(summary, file = paste("Result_", time[i], ".txt", sep=""), quote = FALSE, sep = "\t", colnames = TRUE)
    # head(summary)
    #        No Rain        Index
    # 1: 270206  1.2 0.5858943780
    # 2: 270207  1.0 0.9161427733
    # 3: 270208  0.7 0.4948165440
    # 4: 271199  0.3 0.3728350920
    # 5: 271200  0.5 0.3560031200
    # 6: 271205  0.0 0.3685312140

    sumData = tibble::tibble(time = time[i], No = maxIndex$No, Rain = maxIndex$Rain, Index = maxIndex$Index)

    dataL1 = dplyr::bind_rows(dataL1, sumData)

    # saveFile = file.path(globalVar$outPath, serviceName, paste0("Result_", time[i], ".txt"))
    # dir.create(path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
    # readr::write_csv(x = summary, file = saveFile)

    # write.fwf(summary, file = saveFile, quote = FALSE, sep = "\t", colnames = TRUE)
    # write.fwf(summary, file = saveFile, quote = FALSE, sep = "\t", colnames = TRUE)
}

saveFile = file.path(globalVar$outPath, serviceName, paste0("Result_R_", Period, ".txt"))
dir.create(path_dir(saveFile), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(x = dataL1, file = saveFile)
