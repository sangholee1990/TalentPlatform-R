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

# 1. 점보다는 confidence boundary 인 점선이 더 눈에 띄고 싶습니다. 점의 크기를 지금보다 약 20-30% 정도 작게 할 수 있을까요.
# 2. 점선의 색깔을 점보다는 더 눈에 띄게 채도? 등을 조금 변경할 수 있을까요. 그리고 점선이 더 촘촘하게 찍혔으면 좋겠습니다.
# 3. Index 에 Mandible only 를 Mandible-only 로 수정해주시면 감사하겠습니다.


# 1. 각 점들의 크기가 줄고 연하게 표시된 것은 좋으나, 너무 작고 연한 색깔이 된 것 같습니다. 크기를 지금보다는 조금만 더 키워주시고 색깔은 조금만 덜 연하게 해주시면 감사하겠습니다.
# 2. Boundary 가 너무 연하게 표시되었습니다. Boundary 가 점보다 더 진하게(더 쨍하게? 더 눈에띄게?)강조해주시면 감사하겠습니다. 그리고 Boundary 가 dot dot dot 으로 디자인된 점선인데, - - - - 이런식으로 표시된 점선이면 좋겠습니다.


# 1. 제가 보내드린 예시와 유사하게 confidence boundary 를 조금 더 쨍한 색깔로 하여서 가시성을 높일 수 있을까요? 그리고 보내드린 예시와 같이 점선을 구성하는 (-) 가 더 여러개로 촘촘했으면 좋겠습니다.
# 2. 예시와 유사하게 각 sample 을 나타내는 dot 을 지금보다 아주 조금만 크기를 줄일 수 있을까요?
# 3. 예시와 유사하게 평균을 나타내는 삼각형의 크기를 조금만 줄일 수 있을까요?

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0546"

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

ggplotDefaultColor = scales::hue_pal()(3)
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
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "20240820_엑셀+크몽5.xlsx"))

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
    
    plotSubTitle = sprintf("%s_%s%s-axis-error", typeInfo, colInfo[[1]], colInfo[[2]])
    # saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
    # saveImg = sprintf("%s/%s/%s.tiff", globalVar$figPath, serviceName, plotSubTitle)
    saveImg = sprintf("%s/%s/FIG/%s.tiff", globalVar$figPath, serviceName, plotSubTitle)
    dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
  
    makePlot = ggplot() +
      geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") + 
      geom_vline(xintercept = 0, colour = "grey", linetype = "dashed") +
      
      # geom_point(data = dataL1, aes(colInfo[[1]], colInfo[[2]], colour = group)) +
      # geom_point(data = dataL1, aes(x = !!sym(colInfo[[1]]), y = !!sym(colInfo[[2]]), colour = factor(group)), size = 1, alpha = 0.4) +
      # geom_point(data = dataL1, aes(x = !!sym(colInfo[[1]]), y = !!sym(colInfo[[2]]), colour = factor(group)), size = 1.5, alpha = 0.5) +
      geom_point(data = dataL1, aes(x = !!sym(colInfo[[1]]), y = !!sym(colInfo[[2]]), colour = factor(group)), size = 1.4, alpha = 0.5) +
      
      # geom_point(data = statDataL1, aes(x, y, color = group), shape=17, size=3, show.legend = FALSE) +
      # geom_point(data = statDataL1 %>% dplyr::filter(group == 0), aes(x, y), color = "red", shape=17, size=3, show.legend = FALSE) +
      # geom_point(data = statDataL1 %>% dplyr::filter(group == 1), aes(x, y), color = "blue", shape=17, size=3, show.legend = FALSE) +
      # geom_point(data = statDataL1 %>% dplyr::filter(group == 2), aes(x, y), color = "green", shape=17, size=3, show.legend = FALSE) +
      geom_point(data = statDataL1 %>% dplyr::filter(group == 0), aes(x, y), color = "red", shape=17, size=2.0, show.legend = FALSE) +
      geom_point(data = statDataL1 %>% dplyr::filter(group == 1), aes(x, y), color = "blue", shape=17, size=2.0, show.legend = FALSE) +
      geom_point(data = statDataL1 %>% dplyr::filter(group == 2), aes(x, y), color = "green", shape=17, size=2.0, show.legend = FALSE) +
      # geom_path(data = statDataL2, aes(x, y, colour = factor(group)), size = 0.5, linetype = 2, show.legend = FALSE) +
      # geom_path(data = statDataL2, aes(x, y, colour = factor(group)), size = 0.5, linetype = 2, show.legend = FALSE) +
      # geom_path(data = statDataL2, aes(x, y, colour = factor(group)), size = 0.5, linetype = 3, show.legend = FALSE) +
      # geom_path(data = statDataL2, aes(x, y, colour = factor(group)), size = 0.35, linetype = 2, show.legend = FALSE, alpha = 1.0) +
      geom_path(data = statDataL2 %>% dplyr::filter(group == 0), aes(x, y), color = "red", size = 0.4, linetype = 2, show.legend = FALSE, alpha = 1.0) +
      geom_path(data = statDataL2 %>% dplyr::filter(group == 1), aes(x, y), color = "blue", size = 0.4, linetype = 2, show.legend = FALSE, alpha = 1.0) +
      geom_path(data = statDataL2 %>% dplyr::filter(group == 2), aes(x, y), color = "green", size = 0.4, linetype = 2, show.legend = FALSE, alpha = 1.0) +
      # geom_segment(data = statDataL3, aes(x = x, y = y, xend = xend, yend = yend, colour = factor(group)), size = 0.75, linetype = 4, show.legend = FALSE) +
      labs(title = NULL, x = sprintf("%s-axis error (mm)", colInfo[[1]]), y =  sprintf("%s-axis error (mm)", colInfo[[2]]), color = "group") +
      # xlim(-5, 5) +
      # ylim(-5, 5) +
      scale_x_continuous(minor_breaks = seq(-4, 4, 2), breaks=seq(-4, 4, 2), limits=c(-5, 5)) +
      scale_y_continuous(minor_breaks = seq(-4, 4, 2), breaks=seq(-4, 4, 2), limits=c(-5, 5)) +
      theme_classic() +
      theme(
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
        , legend.background = element_rect(colour = "black", fill = NA, size = 0.5)
        , legend.position = c(0.98, 0.98)
        , legend.justification = c("right", "top")
      ) +
      scale_color_manual(
        name = NULL
        , na.value = "transparent"
        , values = c(
          "0" = ggplotDefaultColor[1]
          , "1" = ggplotDefaultColor[3]
          , "2" = ggplotDefaultColor[2]
          )
        , labels = c("Maxilla-first", "Mandible-first", "Mandible-only")
      )
      
    ggsave(makePlot, filename = saveImg, width = 5, height = 5, dpi = 600)
    
    # shell.exec(saveImg)
    cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
  }
}
