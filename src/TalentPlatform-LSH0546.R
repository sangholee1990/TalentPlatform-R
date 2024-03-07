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
# R을 이용한 2차원 (X, Y, Z 평면) 내 산점도 및 신뢰구간 95% 시각화

# 안녕하세요. 문의사항이 있어 연락드립니다.
# 논문투고용 figure 가 필요한데, 해당 data 를 excel 로 가지고 있고 통계는 다 돌렸습니다.
# 다만 scatterplot 및 95% confidence boundary 를 나타낸 그림이 필요한데, 다른 분께서 진행하신 기존 작업물들이 다 R program 으로 그리셨더라구요.
# 그래서 R program 으로 제작의뢰드려고 하는데 가능한지 궁금합니다. figure 예시 첨부드립니다. 감사합니다.

# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0546"

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
library(rlang)
library(magrittr)

# ================================================
# 파일 읽기
# ================================================
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "20240307_엑셀-크몽.xlsx"))

data = openxlsx::read.xlsx(fileInfo, sheet = 1)

data$group = as.factor(data$group)

# colInfo = c("X", "Y")
# typeInfo = 21
typeList = data$type %>% unique() %>% sort()
colList = list(c("X", "Y"), c("X", "Z"), c("Z", "Y"))
for (typeInfo in typeList) {
  for (colInfo in colList) {
    
    dataL1 = data %>%
      dplyr::filter(
        type == typeInfo
      )
    
    statData = dataL1 %>% 
      dplyr::group_by(group) %>% 
      dplyr::summarise(
        meanX = mean(x, na.rm = TRUE)
        , meanY = mean(y, na.rm = TRUE)
        , meanZ = mean(z, na.rm = TRUE)
        , sdX = sd(x, na.rm = TRUE)
        , sdY = sd(y, na.rm = TRUE)
        , sdZ = sd(z, na.rm = TRUE)
      )
    

    # i = 1
    # groupInfo = 0
    statDataL1 = tibble::tibble()
    groupList = statData$group %>% unique() %>% sort()
    for (groupInfo in groupList) {
      
      statDataX = statData %>% 
          dplyr::filter(group == groupInfo) %>% 
          dplyr::select(dplyr::ends_with(colInfo[[1]])) %>%
          magrittr::set_colnames(c("mean", "sd"))
      
      statDataY = statData %>% 
        dplyr::filter(group == groupInfo) %>% 
        dplyr::select(dplyr::ends_with(colInfo[[2]])) %>%
        magrittr::set_colnames(c("mean", "sd"))
      
        x = cos(seq(0, 5 * pi, length.out = 1000)) * (1.96 * statDataX$sd) + statDataX$mean
        y = sin(seq(0, 5 * pi, length.out = 1000)) * (1.96 * statDataY$sd) + statDataY$mean
        
      tmpData = tibble(
        group = groupInfo
        , x = x
        , y = y
      )
      
      statDataL1 = dplyr::bind_rows(statDataL1, tmpData)
    }
    
        
    statData %>% 
      dplyr::select(dplyr::ends_with(colInfo[[1]]))
    
    
    ggplot() +
      geom_point(data = dataL1, aes(x, y, color = group)) +
      geom_point(data = statData, aes(x, y, color = group)) +
      geom_path(data = statDataL1, aes(x, y, color = group)) +
      labs(title = NULL, x = sprintf("%s-axis error (mm)", colInfo[[1]]), y =  sprintf("%s-axis error (mm)", colInfo[[2]]))
    
  }
}
    
  #   
  #     geom_bar(stat = "identity", position = "dodge") +
  #     geom_text(position = position_dodge(width = 0.9), vjust = 1.5, size = 5, color = "white") +
  #     labs(x = "학년", y = "성관계 비율 [%]", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
  #     theme(
  #       text = element_text(size = 16)
  #       , legend.position = "top"
  #     ) # +
  #     # ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)
  #   
  #   # shell.exec(saveImg)
  #   cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
  #   
  #   
  # }
# }
  
#   
#   dataL1 = data %>%
#     dplyr::filter(
#       type == typeInfo
#     ) 
# # }
# 
# sxlExrtData = readr::read_csv(fileInfo, locale = locale("ko", encoding = "EUC-KR"))
# 
# sxlExrtDataL1 = sxlExrtData %>% 
#   dplyr::filter(응답자특성별 %in% c("학년별")) %>% 
#   dplyr::select(응답자특성별2, 남학생경험률, 여학생경험률) %>% 
#   tidyr::gather(-응답자특성별2, key = "key", value = "val") %>% 
#   dplyr::mutate(
#     leg = dplyr::case_when(
#       stringr::str_detect(key, regex("남학생경험률")) ~ "남성"
#       , stringr::str_detect(key, regex("여학생경험률")) ~ "여성"
#     )
#   )
# 
# # 정렬
# sxlExrtDataL1$응답자특성별2= forcats::fct_relevel(sxlExrtDataL1$응답자특성별2, c("중1", "중2", "중3", "고1", "고2", "고3"))
# 
# # 학년 및 성별에 따른 성관계 경험률
# mainTitle = sprintf("%s", "학년 및 성별에 따른 성관계 경험률")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# 
# ggplot(sxlExrtDataL1, aes(x = 응답자특성별2, y = val, fill = leg, group = leg, label = round(val, 2))) + 
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_text(position = position_dodge(width = 0.9), vjust = 1.5, size = 5, color = "white") +
#   labs(x = "학년", y = "성관계 비율 [%]", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
#   theme(
#     text = element_text(size = 16)
#     , legend.position = "top"
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)
# 
# # shell.exec(saveImg)
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
#   
# 
# # ================================================
# # 성관계 시작 연령 파일 읽기
# # ================================================
# fileInfo2 = Sys.glob(file.path(globalVar$inpPath, serviceName, "성관계_시작_연령_2023.csv"))
# 
# bgnAgeSxlIntData = readr::read_csv(fileInfo2, locale = locale("ko", encoding = "EUC-KR"))
# 
# bgnAgeSxlIntDataL1 = bgnAgeSxlIntData %>% 
#   dplyr::filter(! 성별 %in% c("전체")) %>% 
#   dplyr::select(성별, 평균연령) %>% 
#   tidyr::gather(-성별, key = "key", value = "val") %>% 
#   dplyr::mutate(
#     leg = dplyr::case_when(
#       stringr::str_detect(성별, regex("남학생")) ~ "남성"
#       , stringr::str_detect(성별, regex("여학생")) ~ "여성"
#     )
#   )
# 
# # 성별에 따른 성관계 시작 연령
# mainTitle = sprintf("%s", "성별에 따른 성관계 시작 연령")
# saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, mainTitle)
# dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
# 
# ggplot(bgnAgeSxlIntDataL1, aes(x = 성별, y = val, fill = leg, group = leg, label = round(val, 2))) + 
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_text(position = position_dodge(width = 0.9), vjust = 1.5, size = 5, color = "white") +
#   labs(x = "성별", y = "성관계 시작 연령", fill = NULL, color = NULL, title = NULL, subtitle = mainTitle) +
#   theme(
#     text = element_text(size = 16)
#     , legend.position = "top"
#   ) +
#   ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)
# 
# # shell.exec(saveImg)
# cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
