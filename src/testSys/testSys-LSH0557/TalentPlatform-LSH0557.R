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
# R을 이용한 한글 텍스트 분석 (키워드 추출) 및 시각화 (워드클라우드, 바차트, 파이차트)

# ================================================
# 초기 환경변수 설정
# ================================================
env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
# env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "test"
serviceName = "LSH0557"

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
library(stringr)
library(tidyverse)
library(RcppMeCab)
library(RmecabKo)
library(wordcloud2)
library(htmlwidgets)
library(webshot)

# 명사 추출을 위한 메타 정보
RmecabKo::install_mecab("c:/mecab")

# 파일 검색
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "new_mycar.txt"))

# 파일 읽기
data = readr::read_tsv(fileInfo, locale = locale("ko", encoding = "EUC-KR"), col_names = FALSE)

# 국문 처리
dataL1 = data %>%
  dplyr::mutate(cont = stringr::str_extract_all(X1, "[가-힣]+")) %>%
  dplyr::mutate(cont = sapply(cont, paste, collapse = " ")) %>%
  dplyr::mutate(cont = stringr::str_trim(cont)) %>%
  dplyr::filter(cont != "")

# ==================================================================
# 워드클라우드
# ==================================================================
contAll = paste(dataL1$cont, collapse = " ")

dataL2 = RcppMeCab::pos(utf8::as_utf8(contAll), format = "data.frame") %>%
  dplyr::filter(pos == "NNG") %>%
  dplyr::select(token)

# 100개 키워드
keywordData = dataL2 %>%
  dplyr::group_by(token) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::mutate(len = stringr::str_length(token)) %>% 
  dplyr::filter(
    freq >= 2
    , len >= 2
  ) %>%
  dplyr::arrange(desc(freq)) %>% 
  dplyr::slice(1:100)

# html 저장
fig = wordcloud2::wordcloud2(data = keywordData)
tmpHtml = tempfile(fileext = ".html")
htmlwidgets::saveWidget(fig, tmpHtml, selfcontained = FALSE)

# html에서 png로 저장
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "워드클라우드 시각화")
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
webshot::webshot(tmpHtml, saveImg, vwidth = 1000, vheight = 800, delay = 5)

# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

saveFile = sprintf("%s/%s/%s.csv", globalVar$outPath, serviceName, "TOP100 키워드")
dir.create(dirname(saveFile), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(keywordData, file = saveFile)
cat(sprintf("[CHECK] saveFile : %s", saveFile), "\n")

# ==================================================================
# 바차트
# ==================================================================
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "바차트 시각화")
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

keywordDataL1 = keywordData %>% 
  dplyr::slice(1:10) %>% 
  dplyr::mutate(
    color = ifelse(freq >= 10, "red", ifelse(freq >= 6, "yellow", "green"))
    , per = round(freq / sum(keywordData$freq, na.rm = TRUE) * 100, 1)
  )

barplotData = barplot(height = keywordDataL1$freq,
        names.arg = keywordDataL1$token,
        col = keywordDataL1$color,
        ylim = c(0, 20),
        main = "고객 불만 사항 상위 TOP 10",
        xlab = "항목",
        ylab = "불만건수",
        las = 2
        )

# 불만 건수와 비율 텍스트 추가
text(x = barplotData, 
     y = keywordDataL1$freq, 
     labels = paste("( ", keywordDataL1$per, " % )\n", keywordDataL1$freq, "건", sep = ""),
     col = "black",
     cex = 1.0)

# 가로 점선 추가
abline(h = 8, col = "red", lty = 2)
abline(h = 5, col = "black", lty = 2)

dev.off()
# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")

# ==================================================================
# 파이차트
# ==================================================================
saveImg = sprintf("%s/%s/%s.png", globalVar$figPath, serviceName, "파이차트 시각화")
dir.create(fs::path_dir(saveImg), showWarnings = FALSE, recursive = TRUE)
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

pie(keywordDataL1$freq, labels = paste(keywordDataL1$token, "\n", keywordDataL1$freq, "건"), col = keywordDataL1$color, main = "고객 불만 사항 상위 Top 10")

dev.off()
# shell.exec(saveImg)
cat(sprintf("[CHECK] saveImg : %s", saveImg), "\n")
