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
# R을 이용한 2차원 (X, Y, Z 평면) 내 산점도 및 신뢰구간 95% 시각화 현행화

# 선생님 기다려주셔서 감사합니다. 엑셀파일 송부드립니다. 저번에 해주셨던 대로, scatterplot 과 95% confidence boundary, 그리고 평균점을 표시하는 figure 를 그려주시면 됩니다.
# 엑셀파일 보시면 아시겠지만 sample 수는 119 이며, 그룹 구분 기준이 두가지입니다. 
# 수술방법에 따라 0,1,2 로 세그룹, 비대칭유무에따라 0,1,2 로 세그룹입니다. 
# 우선 수술방법에 따라 0,1,2 그룹으로 구분한 것을 먼저 보면, 전치부의 XY plane, ZY plane, XZ plane 3개의 figure 에 각각 0,1,2 그룹이 다른 색으로 표시되게끔 부탁드립니다. 
# 마찬가지 과정을 우측구치부, 좌측구치부에 하면 총 9개의 figure 가 나옵니다. 마찬가지 방법으로 비대칭유무 그룹 0,1,2 도 해주시면 9개의 figure가 나와서 총 18개의 figure 가 됩니다. 우선은 예시로 하나만 보내주시면 확인하고 피드백드리겠습니다. 감사합니다.

# 1) 수술방법에 따라 세 그룹은 현재 잘되어있는 것 같습니다. 다만 비대칭유무에 따른 그룹은 0,1,2 세 그룹을 각각 Symmetry / Leftward asymmetry / Rightward asymmetry 로 표기해주세요. 그리고 빨강/파랑/초록 이 아니라, 다른 3가지 색깔(구분잘되는것으로) 부탁드립니다.
# 
# 2) 그리고 제가 깜박잊고 전치/우측구치/좌측구치 평균 data 를 안보내드렸네요. 평균데이터 추가한 엑셀파일 다시 송부드리겠습니다. 평균으로도 scatterplot 제작해주시면 감사하겠습니다. 수술그룹 3개평면, 비대칭그룹 3개평면이니까 그래프가 6개가 추가되겠네요.
# 
# 3) 마지막으로 scatterplot 자체 크기가 더 확대되서 보였으면 합니다. 현재 각 축 마다 0,2,4 mm 까지 축표시가 되어있고 그거보다 더 크게 그래프가(거의 5이상) 그려져있는 것으로 보입니다. 축마다 눈금을 0,1,2 로하고 실제로는 약 3이상 정도 표시되게끔 해주실 수 있으실까요?
#   
#   그리고 포인트는 여러개 보내주실 필요 없이 14p 하나만해도 충분할 것 같습니다.
# 항상 감사드립니다. 문의사항 있으면 알려주시면 감사하겠습니다.

# 선생님 확인했습니다. 감사합니다. 다만 이게 너무 확대를 해버리니까 confidence boundary 가 짤리는 현상(ex. 전치)이 발생하는군요ㅠ 혹시 그래프가 짤리지는 않게끔 4에 가깝게까지 보이게할 수 있나요? 4 라는 숫자를 굳이 쓰지 않더라도 3이후로 좀 더 넓게까지 포함되게요!
  
# 그리고 비대칭그룹 색깔을 변경해주신 것은 좋은데 주황색을 제외하면(파랑,초록은..) 기존 수술그룹과 크게 차이가 안나는 것 같습니다. 조금더 다르게 해주시면 감사하겠습니다.

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0616"

if (Sys.info()[["sysname"]] == "Windows") {
  contextPath = ifelse(env == "local", getwd(), "C:/SYSTEMS/PROG/R/TalentPlatform-R")
} else {
  contextPath = ifelse(env == "local", getwd(), "/SYSTEMS/PROG/R/PyCharm")
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
library(scales)
library(fs)

# ggplotDefaultColor = scales::hue_pal()(3)
# c("#F8766D", "#00BA38", "#619CFF")

# ggplotDefaultColorNew = scales::muted(ggplotDefaultColor, l = 100)
# "#82261D" "#005800" "#004696"
# "#B85B55" "#088A29" "#4B76C1"
# "#D47670" "#3BA44B" "#6A90DB"
# "#FFE5DF" "#B6FFC0" "#DEFFFF"

# ================================================
# 파일 읽기
# ================================================
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "20240307_엑셀-크몽.xlsx"))
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "20240820_엑셀+크몽5.xlsx"))
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "20250502_크몽+wafer+fitting+error+의뢰.xlsx"))

data = openxlsx::read.xlsx(fileInfo, sheet = 1)
data$group = as.factor(data$group)

# fontSize = 16
# colInfo = c("X", "Y")
# typeInfo = "우측구치"
# keyInfo = "수술"
# fontSizeList = seq(10, 20, 2)
fontSizeList = seq(14, 14, 2)
typeList = data$type %>% unique() %>% sort()
keyList = c("비대칭", "수술")
colList = list(c("X", "Y"), c("X", "Z"), c("Z", "Y"))

for (fontSize in fontSizeList) {
  for (typeInfo in typeList) {
    for (keyInfo in keyList) {
      for (colInfo in colList) {
        
        # summary(data)
        
        dataL1 = data %>%
          dplyr::filter(
            type == typeInfo,
            stringr::str_detect(key, regex(keyInfo))
          ) %>% 
          dplyr::rename(
            X = x
            , Y = y
            , Z = z
          )
        
        statData = dataL1 %>% 
          dplyr::group_by(group) %>% 
          dplyr::summarise(
            meanX = mean(X, na.rm = TRUE)
            , meanY = mean(Y, na.rm = TRUE)
            , meanZ = mean(Z, na.rm = TRUE)
            , sdX = sd(X, na.rm = TRUE)
            , sdY = sd(Y, na.rm = TRUE)
            , sdZ = sd(Z, na.rm = TRUE)
          )
        
        
        # groupInfo = 0
        statDataL1 = tibble::tibble()
        statDataL2 = tibble::tibble()
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
          
          # x = cos(seq(0, 5 * pi, length.out = 100)) * (1.96 * statDataX$sd) + statDataX$mean
          # y = sin(seq(0, 5 * pi, length.out = 100)) * (1.96 * statDataY$sd) + statDataY$mean
          x = cos(seq(0, 5 * pi, length.out = 10000)) * (1.96 * statDataX$sd) + statDataX$mean
          y = sin(seq(0, 5 * pi, length.out = 10000)) * (1.96 * statDataY$sd) + statDataY$mean
          # x = cos(seq(0, 5 * pi, length.out = 200)) * (1.96 * statDataX$sd) + statDataX$mean
          # y = sin(seq(0, 5 * pi, length.out = 200)) * (1.96 * statDataY$sd) + statDataY$mean
          
          tmpData = tibble::tibble(group = groupInfo, x = x, y = y) %>% 
            dplyr::mutate(angle = atan2(y, x)) %>%
            dplyr::arrange(angle)
          
          statDataL1 = dplyr::bind_rows(statDataL1, tibble::tibble(group = groupInfo, x = statDataX$mean, y = statDataY$mean) )
          statDataL2 = dplyr::bind_rows(statDataL2, tmpData)
        }
        
        # statDataL3 = statDataL2 %>%
        #   dplyr::group_by(group) %>%
        #   dplyr::mutate(
        #     xend = lead(x)
        #     , yend = lead(y)
        #   ) %>%
        #   na.omit()
        
        plotSubTitle = sprintf("%s_%s_%s%s-axis-error", typeInfo, keyInfo, colInfo[[1]], colInfo[[2]])
        # saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
        # saveImg = sprintf("%s/%s/%s.tiff", globalVar$figPath, serviceName, plotSubTitle)
        # saveImg = sprintf("%s/%s/FIG/%s.tiff", globalVar$figPath, serviceName, plotSubTitle)
        saveImg = sprintf("%s/%s/FIG/%sp/%s.tiff", globalVar$figPath, serviceName, fontSize, plotSubTitle)
        dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
        

        # 비동기의 경우 라벨, 색상 설정
        isAsync = stringr::str_detect("비대칭", regex(keyInfo, ignore_case = TRUE))
        labelList = ifelse(isAsync, list(c("Symmetry", "Leftward asymmetry", "Rightward asymmetry")), list(c("Maxilla-first", "Mandible-first", "Mandible-only"))) %>% 
          unlist()
        
        pointColList = ifelse(isAsync, list(c("#E69F00", "#6A0DAD", "black")), list(c("red", "blue", "green"))) %>% 
          unlist()
        
        pathColList = ifelse(isAsync, list(c("#E69F00", "#6A0DAD", "black")), list(c("red", "blue", "#008000"))) %>% 
          unlist()
        
        ggplotDefaultColor = ifelse(isAsync, list(c("#FFC573", "#B589D9", "#8E8E8E")), list(c("#F8766D", "#619CFF", "#00BA38"))) %>% 
          unlist()
        
        makePlot = ggplot() +
          geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") + 
          geom_vline(xintercept = 0, colour = "grey", linetype = "dashed") +
          
          # geom_point(data = dataL1, aes(colInfo[[1]], colInfo[[2]], colour = group)) +
          # geom_point(data = dataL1, aes(x = !!sym(colInfo[[1]]), y = !!sym(colInfo[[2]]), colour = factor(group)), size = 1, alpha = 0.4) +
          # geom_point(data = dataL1, aes(x = !!sym(colInfo[[1]]), y = !!sym(colInfo[[2]]), colour = factor(group)), size = 1.5, alpha = 0.5) +
          # geom_point(data = dataL1, aes(x = !!sym(colInfo[[1]]), y = !!sym(colInfo[[2]]), colour = factor(group)), size = 1.4, alpha = 0.5) +
          geom_point(data = dataL1, aes(x = !!sym(colInfo[[1]]), y = !!sym(colInfo[[2]]), colour = factor(group)), size = 2, alpha = 0.5) +
          
          # geom_point(data = statDataL1, aes(x, y, color = group), shape=17, size=3, show.legend = FALSE) +
          # geom_point(data = statDataL1 %>% dplyr::filter(group == 0), aes(x, y), color = "red", shape=17, size=3, show.legend = FALSE) +
          # geom_point(data = statDataL1 %>% dplyr::filter(group == 1), aes(x, y), color = "blue", shape=17, size=3, show.legend = FALSE) +
          # geom_point(data = statDataL1 %>% dplyr::filter(group == 2), aes(x, y), color = "green", shape=17, size=3, show.legend = FALSE) +
          geom_point(data = statDataL1 %>% dplyr::filter(group == 0), aes(x, y), color = pointColList[1], shape=17, size=3, show.legend = FALSE) +
          geom_point(data = statDataL1 %>% dplyr::filter(group == 1), aes(x, y), color = pointColList[2], shape=17, size=3, show.legend = FALSE) +
          geom_point(data = statDataL1 %>% dplyr::filter(group == 2), aes(x, y), color = pointColList[3], shape=17, size=3, show.legend = FALSE) +
          # geom_path(data = statDataL2, aes(x, y, colour = factor(group)), size = 0.5, linetype = 2, show.legend = FALSE) +
          # geom_path(data = statDataL2, aes(x, y, colour = factor(group)), size = 0.5, linetype = 2, show.legend = FALSE) +
          # geom_path(data = statDataL2, aes(x, y, colour = factor(group)), size = 0.5, linetype = 3, show.legend = FALSE) +
          # geom_path(data = statDataL2, aes(x, y, colour = factor(group)), size = 0.35, linetype = 2, show.legend = FALSE, alpha = 1.0) +
          geom_path(data = statDataL2 %>% dplyr::filter(group == 0), aes(x, y), color = pathColList[1], size = 0.4, linetype = 2, show.legend = FALSE, alpha = 1.0) +
          geom_path(data = statDataL2 %>% dplyr::filter(group == 1), aes(x, y), color = pathColList[2], size = 0.4, linetype = 2, show.legend = FALSE, alpha = 1.0) +
          geom_path(data = statDataL2 %>% dplyr::filter(group == 2), aes(x, y), color = pathColList[3], size = 0.4, linetype = 2, show.legend = FALSE, alpha = 1.0) +
          # geom_segment(data = statDataL3, aes(x = x, y = y, xend = xend, yend = yend, colour = factor(group)), size = 0.75, linetype = 4, show.legend = FALSE) +
          # labs(title = NULL, x = sprintf("%s-axis error (mm)", colInfo[[1]]), y =  sprintf("%s-axis error (mm)", colInfo[[2]]), color = "group") +
          labs(title = NULL, x = sprintf("%s-axis discrepancy (mm)", colInfo[[1]]), y =  sprintf("%s-axis discrepancy (mm)", colInfo[[2]]), color = "group") +
          # xlim(-5, 5) +
          # ylim(-5, 5) +
          # scale_x_continuous(minor_breaks = seq(-4, 4, 2), breaks=seq(-4, 4, 2), limits=c(-5, 5)) +
          # scale_y_continuous(minor_breaks = seq(-4, 4, 2), breaks=seq(-4, 4, 2), limits=c(-5, 5)) +
          scale_x_continuous(minor_breaks = seq(-3, 3, 1), breaks=seq(-3, 3, 1), limits=c(-3.5, 3.5)) +
          scale_y_continuous(minor_breaks = seq(-3, 3, 1), breaks=seq(-3, 3, 1), limits=c(-3.5, 3.5)) +
          theme_classic() +
          theme(
            panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
            , legend.background = element_rect(colour = "black", fill = NA, size = 0.5)
            , legend.position = c(0.98, 0.98)
            , legend.justification = c("right", "top")
            , text = element_text(size = fontSize)
          ) +
          scale_color_manual(
            name = NULL
            , na.value = "transparent"
            , values = c(
              "0" = ggplotDefaultColor[1]
              , "1" = ggplotDefaultColor[2]
              , "2" = ggplotDefaultColor[3]
            )
            # , labels = c("Maxilla-first", "Mandible-first", "Mandible-only")
            , labels = labelList
          )
        
        ggsave(makePlot, filename = saveImg, width = 5, height = 5, dpi = 600)
        
        # shell.exec(saveImg)
        cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
      }
    }
  }
}

