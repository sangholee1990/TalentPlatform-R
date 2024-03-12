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

# 1. Group 명을 아래와 같이 변경해주세요.
# 
# 0 -> Maxilla-first
# 1 -> Mandible-first
# 2 -> Mandible only
# 
# 2. Group 색깔을 현재 보내주신 것에서 1번과 2번을 바꿔주세요.
# 
# 3. 95% confidence boundary 는 점선이 되게 해주세요. 그리고 예시처럼 점선이 조금만 더 촘촘하게 해주세요.
# 
# 
# 4. 현재 그래프가 좀 큰 것 같습니다 (제가 보내드린 예시 그래프와 비교해보면 축 눈금에서 4mm보다 큰 부위와 -4mm 보다 작은 부위 여유공간이 적어보입니다.). 예시 보내주신 것과 같이 해주시면 감사하겠습니다.

# 다만 이게 논문에 싣을거라 TIF or TIFF 형태로 300DPI 이상으로 해주실 수 있을까요?

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
library(scales)
library(fs)

ggplotDefaultColor = scales::hue_pal()(3)

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
    
    
    # i = 1
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
      
      x = cos(seq(0, 5 * pi, length.out = 100)) * (1.96 * statDataX$sd) + statDataX$mean
      y = sin(seq(0, 5 * pi, length.out = 100)) * (1.96 * statDataY$sd) + statDataY$mean
      
      tmpData = tibble::tibble(group = groupInfo, x = x, y = y) %>% 
        dplyr::mutate(angle = atan2(y, x)) %>%
        dplyr::arrange(angle)
      
      statDataL1 = dplyr::bind_rows(statDataL1, tibble::tibble(group = groupInfo, x = statDataX$mean, y = statDataY$mean) )
      statDataL2 = dplyr::bind_rows(statDataL2, tmpData)
    }
    
    plotSubTitle = sprintf("%s_%s%s-axis-error", typeInfo, colInfo[[1]], colInfo[[2]])
    # saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, plotSubTitle)
    saveImg = sprintf("%s/%s/%s.tiff", globalVar$figPath, serviceName, plotSubTitle)
    dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
    
    makePlot = ggplot() +
      geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") + 
      geom_vline(xintercept = 0, colour = "grey", linetype = "dashed") +
      
      # geom_point(data = dataL1, aes(colInfo[[1]], colInfo[[2]], colour = group)) +
      geom_point(data = dataL1, aes(x = !!sym(colInfo[[1]]), y = !!sym(colInfo[[2]]), colour = group)) +
      
      # geom_point(data = statDataL1, aes(x, y, color = group), shape=17, size=3, show.legend = FALSE) +
      geom_point(data = statDataL1 %>% dplyr::filter(group == 0), aes(x, y), color = "red", shape=17, size=3, show.legend = FALSE) +
      geom_point(data = statDataL1 %>% dplyr::filter(group == 1), aes(x, y), color = "blue", shape=17, size=3, show.legend = FALSE) +
      geom_point(data = statDataL1 %>% dplyr::filter(group == 2), aes(x, y), color = "green", shape=17, size=3, show.legend = FALSE) +

      # geom_path(data = statDataL2, aes(x, y, colour = group), size = 0.5, linetype = 2, show.legend = FALSE) +
      geom_path(data = statDataL2 %>% dplyr::filter(group == 0), aes(x, y), color = "red", size = 0.5, linetype = 2, show.legend = FALSE) +
      geom_path(data = statDataL2 %>% dplyr::filter(group == 1), aes(x, y), color = "blue", size = 0.5, linetype = 2, show.legend = FALSE) +
      geom_path(data = statDataL2 %>% dplyr::filter(group == 2), aes(x, y), color = "green", size = 0.5, linetype = 2, show.legend = FALSE) +
      
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
        , values = c("0" = ggplotDefaultColor[1], "1" = ggplotDefaultColor[3], "2" = ggplotDefaultColor[2])
        , labels = c("Maxilla-first", "Mandible-first", "Mandible only")
      )
    
    ggsave(makePlot, filename = saveImg, width = 5, height = 5, dpi = 600)
    
    # shell.exec(saveImg)
    cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
  }
}

