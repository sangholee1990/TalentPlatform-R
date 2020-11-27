
#=====================================
# Set Env
#=====================================
# Sys.setlocale("LC_ALL", "Korean")
# options(encoding = "UTF-8")
# Sys.setenv(LANG = "ko_KR.UTF-8")
# 
# Sys.setlocale("LC_ALL", "English")
# options(encoding = "UTF-8")
# Sys.setenv(LANG = "en_US.UTF-8")

globalVar = new.env()

globalVar$optDig = 2
globalVar$memLimit = 9999999999999

# config
globalVar$config = getwd()
# globalVar$config = "D:/04. 재능플랫폼/Github/TalentPlatform-R"
globalVar$inpConfig = paste(globalVar$config, 'INPUT', 'o2job', sep = '/')
globalVar$figConfig = paste(globalVar$config, 'FIG', 'o2job', sep = '/')
globalVar$outConfig = paste(globalVar$config, 'OUTPUT', 'o2job', sep = '/')
globalVar$logConfig = paste(globalVar$config, 'LOG', 'o2job', sep = '/')
globalVar$mapConfig = paste(globalVar$config, 'CONFIG', 'MAP_INFO', sep = '/')
globalVar$systemConfig = paste(globalVar$config, 'CONFIG', 'system.cfg', sep = '/')

# key
configInfo = yaml::yaml.load_file(globalVar$systemConfig)
globalVar$googleKey = configInfo$default$googleKey
globalVar$dataKey = configInfo$default$dataKey

utils::ls.str(globalVar)

# Rtools
# writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
# Sys.which("make")

#=====================================
# Set Fun
#=====================================

log = log4r::create.logger()
log4r::logfile(log) = paste0(globalVar$logConfig, "/log4r_", format(Sys.time(), "%Y%m%d"), ".log")
log4r::level(log) = "INFO"

tryCatch(
  expr = {
    # 주 소스 코드
    main = FALSE
    
    log4r::info(log, paste0("main : ", main))

  }
  , warning = function(warning) { log4r::warn(log, warning) }
  , error = function(error) { log4r::error(log, error) }
  , finally = { 
    
    main = TRUE
    
    log4r::info(log, paste0("main : ", main))
  }
)

#=====================================
# Set Data
#=====================================
options(digits = globalVar$optDig)
memory.limit(size = globalVar$memLimit)

library(ggmap)
ggmap::register_google(key = globalVar$googleKey)

# 패키지 업데이트
# update.packages(ask = FALSE)



#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

# 다음 R 스크립트를 완성하여 아래 왼쪽 그래프와 같은 출력이 나오도록 하자.
# n (= 10)명 학생들의 중간시험성적 mid와 기말시험성적 final의 변롸를 보여준다.
# mid로부터 final에 이르는 변화를 수평, 수직, 수평 선분을 이어 보여준다.
# mid의 수평축 위치는 -1이고 final의 수평축 위치는 1이다.
# mid의 순위가 i (=1, ..., n)인 학생의 수직선분의 수평축 위치는 -1 + ((2 * i) / (n + 1))이다.
# 그리고 완성된 R 스크립트를 n = 30인 모의생성자료에 적용하여 그래프 출력을 제시하라.

n = 30
x = rnorm(n, 50, 10)
y = 50 +
  0.5 * (x - 50) +
  sqrt(1 - 0.5^2) * rnorm(n, 0, 10)

par(mfrow = c(1, 1))

plot(c(-1.2, 1.2), c(20, 80), type = "n", xlab = "exams", ylab = "score", main = "score changes", axes = FALSE)

axis(side = 1, at = c(-1, 1), labels = c("mid", "final"))
axis(side = 2, at = seq(20, 80, 10))
color = rainbow(n)
order.1 = order(x)

x = x[order.1]
y = y[order.1]

xAxisMin = -1
xAxisMax = 1

for (i in 1:n) {
  xPos = -1 + ((2 * i) / (n + 1))


  data = data.frame(
    x = c(xAxisMin, xPos, xPos, xAxisMax)
    , y = c(x[i], x[i], y[i], y[i])
  )

  # 선 그리기
  lines(data$x, data$y, col = color[i])

  # 점 그리기
  points(data[c(1, 4),], col = color[i], pch = 19)
}
dev.off()

# openair::mydata에서 변수 wd는 바람이 불어오는 방향이다 (wd = 0은 북이고 wd = 90은 동, wd = 180은 남, wd = 270은 서이다).
# wd에 대한 히스토그램을 아래 왼쪽 그림을 바탕으로 아래 오른쪽 그림과 같은 부채꼴 모양이 되도록 R 스크립트를 완성하라.
# k (=9)개 구간으로 나눈 히스토그램이다.
# 최대 빈도 구간의 부채꼴에 대하여 반경이 1이 되도록 한다.
# 그리고 18 (=k)개 구간의 부채꼴 히스토그램을 제시하라.

library(openair)

data(mydata)

str(mydata)

k = 18

par(mfrow = c(1, 1))

histogram = hist(mydata$wd, breaks = seq(0, 360, length = k + 1))

radius = histogram$counts / max(histogram$counts, na.rm = TRUE)

s = seq(0, 2 * pi, length = 361)

par(mfrow = c(1, 2))

plot(cos(s), sin(s), type = "l", axes = FALSE, lty = "dotted", xlab = "", ylab = "", main = "wind direction")
text(0, 0, "*")
text(0, 1, "N"); text(1, 0, "E"); text(0, -1, "S"); text(-1, 0, "W")


plot(cos(s), sin(s), type = "l", axes = FALSE, lty = "dotted", xlab = "", ylab = "", main = "wind direction")
text(0, 0, "*")
text(0, 1, "N"); text(1, 0, "E"); text(0, -1, "S"); text(-1, 0, "W")

number = k

for (i in 1:number) {

  sector = s[histogram$breaks[i] + 1]
  sectorArea = seq(sector, sector + (2 * pi) / number, length = 1000)

  xx = c(0, radius[i] * cos(sectorArea))
  yy = c(0, radius[i] * sin(sectorArea))

  polygon(xx, yy)
}


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(ggpubr)

# 1) 국가통계정보 시스템을 이용하여 다음을 조사하여라
# 가) 최근 10년간 소비자물가지수의 변동사항을 조사하고 그래프를 이용하여 분석하여라.
data = read.csv("INPUT/o2job/소비자물가지수.csv", header = TRUE, fileEncoding = "euc-kr")

dataL1 = data %>%
  tidyr::gather(-시도별, key = "key", value = "val") %>%
  dplyr::mutate(
    dtYear = readr::parse_datetime(key, "X%Y")
    , year = lubridate::year(dtYear)
  )


ggscatter(dataL1, x = "year", y = "val", add = "reg.line") +
  stat_cor(label.x = 2009, label.y = 110) +
  stat_regline_equation(label.x = 2009, label.y = 108) +
  theme_bw()


# 나) 최근 10년간 국민소득의 변동사항을 조사하고 그래프를 이용하여 분석하여라.
data = read.csv("INPUT/o2job/연간지표.csv", header = TRUE, fileEncoding = "euc-kr")

dataL1 = data %>%
  tidyr::gather(-연간지표별, key = "key", value = "val") %>%
  dplyr::filter(연간지표별 == "국민총소득(명목, 원화표시) (십억원)") %>%
  dplyr::mutate(
    dtYear = readr::parse_datetime(key, "X%Y")
    , year = lubridate::year(dtYear)
  )

ggscatter(dataL1, x = "year", y = "val", add = "reg.line") +
  stat_cor(label.x = 2009, label.y = 2000000) +
  stat_regline_equation(label.x = 2009, label.y = 1900000) +
  theme_bw()

# 다) 최근 10년간 수출 및 수입의 변동사항을 조사하고 그래프를 이용하여 분석하여라.
data = read.csv("INPUT/o2job/수출·수입_G20.csv", header = TRUE, fileEncoding = "euc-kr")

dataL1 = data %>%
  tidyr::gather(-타입, key = "key", value = "val") %>%
  dplyr::mutate(
    dtYear = readr::parse_datetime(key, "X%Y")
    , year = lubridate::year(dtYear)
  )

# 수입
dataL1 %>%
  dplyr::filter(타입 == "수입") %>%
  ggscatter(x = "year", y = "val", add = "reg.line") +
  stat_cor(label.x = 2008, label.y = 600000) +
  stat_regline_equation(label.x = 2008, label.y = 550000) +
  theme_bw()

# 수출
dataL1 %>%
  dplyr::filter(타입 == "수출") %>%
  ggscatter(x = "year", y = "val", add = "reg.line") +
  stat_cor(label.x = 2008, label.y = 600000) +
  stat_regline_equation(label.x = 2008, label.y = 550000) +
  theme_bw()

# 1) 태능선수촌에서 88올림픽에 대비하여 연습하고 있는 육상선수 중에서 25명을 뽑아 체격과 50 m 달기의 기록을 수집해 보니 다음과 같다. 성별은 (1) 남자 (2) 여자이고, 신장과 다리길이는 cm로, 체중은 kg으로, 50 m 달리기는 초 단위로 측정한 것이다.

# dataN2 = data.frame(
#     num = c(10010, 10012, 10015, 10017, 10021, 10023, 10044, 10055, 10059, 10060, 10065, 10070, 10072, 10074, 10079, 10080, 10090, 10093, 10096, 10101, 10103, 10118, 10123, 10125, 10126)
#     , gender = c(1, 2, 1, 1, 2, 2, 1, 2, 1, 1, 1, 2, 1, 2, 2, 2, 1, 2, 2, 1, 1, 1, 2, 1, 1)
#     , kidney = c(184.0, 160.3, 179.3, 176.2, 166.4, 168.0, 177.0, 162.4, 170.9, 188.3, 174.3, 171.7, 185.3, 165.5, 172.2, 168.6, 176.0, 168.1, 165.9, 183.0, 163.2, 176.5, 165.3, 180.9, 176.5)
#     , weight = c(76.4, 57.2, 74.2, 68.2, 56.6, 64.8, 67.5, 51.2, 65.8, 77.5, 64.2, 62.6, 80.8, 64.5, 81.6, 68.0, 81.3, 72.3, 54.1, 84.0, 63.0, 68.3, 54.7, 96.0, 74.4)
#     , legLength = c(101.6, 90.2, 99.4, 97.1, 91.0, 92.9, 103.6, 95.0, 79.5, 103.1, 102.7, 99.6, 101.2, 93.5, 97.5, 94.0, 95.6, 95.4, 92.6, 98.4, 86.7, 102.6, 96.5, 103.5, 95.1)
#     , running = c(6.17, 6.87, 6.39, 6.77, 6.93, 7.15, 7.68, 7.50, 6.70, 6.58, 6.39, 6.92, 6.38, 6.91, 7.35, 7.12, 6.55, 7.26, 6.96, 6.48, 6.84, 6.00, 7.48, 6.71, 6.73)
# )

dataN2 = read.csv("INPUT/o2job/5번.txt", sep = "", header = TRUE, fileEncoding = "euc-kr")

# 가) 성별로 신장, 체중, 다리길이, 50m 달리기의 기술통계량을 구하여라.
dataN2 %>%
  dplyr::group_by(sex) %>%
  dplyr::summarise_all(funs(
    mean(., na.rm = TRUE) # 평균값
    , median(., na.rm = TRUE) # 중앙값
    , sd(., na.rm = TRUE) # 표준편차
    , max(., na.rm = TRUE) # 표준편차
    , min(., na.rm = TRUE) # 표준편차
  )) %>%
  dplyr::glimpse()

# 나) 성별로 신장, 체중, 다리길이, 50m 달리기의 줄기-잎 그림과 히스토그램, 상자 그림을 그리고 설명하여라.


# 줄기-잎 그림
ind = which(dataN2$sex == 1)

# 성별에 따른 신장
stem(dataN2[ind,]$height)
stem(dataN2[-ind,]$height)

# 성별에 따른 체중
stem(dataN2[ind,]$weight)
stem(dataN2[-ind,]$weight)

# 성별에 따른 다리길이
stem(dataN2[ind,]$leg)
stem(dataN2[-ind,]$leg)

# 성별에 따른 50m 달리기
stem(dataN2[ind,]$run50)
stem(dataN2[-ind,]$run50)

# 히스토그램
# 성별에 따른 신장도
par(mfrow = c(1, 2))
hist(dataN2[ind,]$height, main = "남자")
hist(dataN2[-ind,]$height, main = "여자")

# ggplot(dataN2, aes(x = height, fill = as.factor(sex))) +
#     geom_histogram(alpha = 0.6) +
#     scale_fill_discrete(labels=c("남자", "여자"), name = "성별")

# 성별에 따른 체중
par(mfrow = c(1, 2))
hist(dataN2[ind,]$weight, main = "남자")
hist(dataN2[-ind,]$weight, main = "여자")

# ggplot(dataN2, aes(x = weight, fill = as.factor(sex))) +
#     geom_histogram(alpha = 0.6) +
#     scale_fill_discrete(labels=c("남자", "여자"), name = "성별")

# 성별에 따른 다리길이
par(mfrow = c(1, 2))
hist(dataN2[ind,]$leg, main = "남자")
hist(dataN2[-ind,]$leg, main = "여자")

# ggplot(dataN2, aes(x = leg, fill = as.factor(sex))) +
#     geom_histogram(alpha = 0.6) +
#     scale_fill_discrete(labels=c("남자", "여자"), name = "성별")

# 성별에 따른 50m 달리기
par(mfrow = c(1, 2))
hist(dataN2[ind,]$run50, main = "남자")
hist(dataN2[-ind,]$run50, main = "여자")

# ggplot(dataN2, aes(x = run50, fill = as.factor(sex))) +
#     geom_histogram(alpha = 0.6) +
#     scale_fill_discrete(labels=c("남자", "여자"), name = "성별")


# 상자그림
# 성별에 따른 신장
ggplot(dataN2, aes(x = height, fill = as.factor(sex))) +
  geom_boxplot(alpha = 0.6) +
  scale_fill_discrete(labels = c("남자", "여자"), name = "성별")

# 성별에 따른 체중
ggplot(dataN2, aes(x = weight, fill = as.factor(sex))) +
  geom_boxplot(alpha = 0.6) +
  scale_fill_discrete(labels = c("남자", "여자"), name = "성별")

# 성별에 따른 다리길이
ggplot(dataN2, aes(x = leg, fill = as.factor(sex))) +
  geom_boxplot(alpha = 0.6) +
  scale_fill_discrete(labels = c("남자", "여자"), name = "성별")

# 성별에 따른 50m 달리기
ggplot(dataN2, aes(x = run50, fill = as.factor(sex))) +
  geom_boxplot(alpha = 0.6) +
  scale_fill_discrete(labels = c("남자", "여자"), name = "성별")

# 다) 신장과 50m 달리기의 산점도를 성별로 구분하여 그리고 설명하여라.
# 성별에 따른 신장
ggplot(dataN2, aes(x = height, y = run50, colour = as.factor(sex))) +
  geom_point(alpha = 0.6, size = 5) +
  scale_color_discrete(labels = c("남자", "여자"), name = "성별")

cor(dataN2[ind,])
lm(dataN2[ind,]$run50 ~ dataN2[ind,]$height)

lm(dataN2[-ind,]$run50 ~ dataN2[-ind,]$height)
cor(dataN2[-ind,])

# 다. 다음은 두 명의 볼링선수가 10회 게임을 하여 그 점수를 기록한 것이다.
# dataN3 = data.frame(
#     A = c(198, 119, 174, 235, 134, 192, 124, 241, 158, 176)
#     , B = c(196, 159, 162, 178, 188, 169, 173, 183, 177, 152)
# )

dataN3 = read.csv("INPUT/o2job/6번.txt", sep = "", header = TRUE, fileEncoding = "euc-kr")

# 1) 각 선수의 점수에 대한 평균값, 중앙값, 표준편차를 구하여라.
dataN3L1 = dataN3 %>%
  tidyr::gather(key = "key", value = "val")


dataN3L1 %>%
  dplyr::group_by(key) %>%
  dplyr::summarise_all(funs(
    mean(., na.rm = TRUE) # 평균값
    , median(., na.rm = TRUE) # 중앙값
    , sd(., na.rm = TRUE) # 표준편차
  )) %>%
  glimpse()

# 2) 각 선수의 점수에 히스토그램, 줄기-잎 그림, 상자그림을 그려라.

# 히스토그램
ggplot(dataN3L1, aes(x = val, fill = key)) +
  geom_histogram(alpha = 0.6)

# 줄기-잎 그림
stem(dataN3$A)
stem(dataN3$B)


# 상자그림
ggplot(dataN3L1, aes(x = val, fill = key)) +
  geom_boxplot(alpha = 0.6)

# 3) 누가 더 우수한 선수라고 판단되는가? 그 이유는?

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

#=========================================
# 문제 1
#=========================================
# 1. diabetes 데이터에서 나이가 많은 그룹이 나이가 작은 그룹에 비하여 logCpeptide의 평균이 크다는 가설을 임의화 방법으로 테스트하고자 한다.
# p-값을 산출하고 영가설 하에서 확률적으로 동등한 표본 평균 간 차이들의 히스토그램에 관측된 차이를 marking하라.

library(bootstrap)

data(diabetes)

# 모집단
ind = which(diabetes$age > 10)

groupA = diabetes[ind,]
groupB = diabetes[-ind,]

meanGroupA = mean(groupA$logCpeptide, na.rm = TRUE)
meanGroupB = mean(groupB$logCpeptide, na.rm = TRUE)

r = meanGroupA - meanGroupB
r

# 표본 평균
n.repeat = 1000
r.random = rep(0, n.repeat)
count = 0

for (k in 1:n.repeat) {
  sampleList = base::sample(1:nrow(diabetes), replace = TRUE)

  # sample(X, N, replace=F)
  r.data = diabetes[sampleList,]

  r.ind = which(r.data$age > 10)

  r.groupA = r.data[r.ind,]
  # r.groupB = diabetes[-r.ind, ]
  r.meanGroupA = mean(r.groupA$logCpeptide, na.rm = TRUE)

  r.star = r.meanGroupA - meanGroupB

  r.random[k] = r.star

  if (r.star <= r) count = count + 1

}

hist(r.random, nclass = 20)
text(r, 0, "|", col = "red", cex = 2.0)
p.value = count / n.repeat
p.value

#=========================================
# 문제 2
#=========================================

# 앞 문제의 계속. 나이가 많은구룹의 평균 logCpeptide와 나이가 작은 그룹의 평균 logCpeptide 간 차이에 대한 신뢰구간을 붓스트랩 방법으로 산출하고자 한다.
# 95% 신뢰구간을 산출하고 붓스트랩 관측 차이들의 히스토그램에 marking하라.

n.repeat = 1000
r.boot = rep(0, n.repeat)
count = 0

for (k in 1:n.repeat) {
  sampleList = base::sample(1:nrow(diabetes), replace = FALSE)

  r.ind = which(diabetes[sampleList,]$age > 10)

  r.groupA = diabetes[r.ind,]
  r.groupB = diabetes[-r.ind,]
  r.meanGroupA = mean(r.groupA$logCpeptide, na.rm = TRUE)
  r.meanGroupB = mean(r.groupB$logCpeptide, na.rm = TRUE)

  r.star = r.meanGroupA - r.meanGroupB

  r.boot[k] = r.star
}

hist(r.boot, nclass = 20)

conf.1 = quantile(r.boot, p = 0.05)
conf.2 = quantile(r.boot, p = 0.95)

text(conf.1, 0, "|", col = "red", cex = 2.0)
text(conf.2, 0, "|", col = "red", cex = 2.0)

round(c(conf.1, conf.2), 2)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(RSelenium)
library(rvest)
library(stringr)
library(tidyverse)
library(data.table)
library(foreach)
library(httr)
library(webdriver)
library(seleniumPipes)
library(readxl)
library(foreach)

# options(add.error.underscore=FALSE)

setWindowTab = function(remDr, windowId) {
  qpath = sprintf("%s/session/%s/window", remDr$serverURL, remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}

getXpathText = function(xpath) {
  remDr$getPageSource()[[1]] %>%
    read_html() %>%
    rvest::html_nodes(xpath = xpath) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

getCssText = function(css) {
  remDr$getPageSource()[[1]] %>%
    read_html() %>%
    rvest::html_nodes(css = css) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}


# 1안)
# https://github.com/rstudio/webdriver

# 설치 방법
# install_phantomjs()
pjs = run_phantomjs()
pjs

ses = Session$new(port = pjs$port)
ses$go("https://onland.kbstar.com/quics?page=C059652&%EB%B2%95%EC%A0%95%EB%8F%99%EC%BD%94%EB%93%9C=11650108&%EB%A9%94%EC%9D%B8%EA%B2%80%EC%83%89%EC%97%AC%EB%B6%80=1&%EB%A9%94%EC%9D%B8%EA%B2%80%EC%83%89%ED%83%80%EC%9E%85=2&QSL=F")
ses$getUrl()
ses$getTitle()
ses$takeScreenshot()


# 2안)
# cd /c/selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 5000

remDr = remoteDriver(
  remoteServerAddr = "localhost"
  , port = 5000L
  , browserName = "chrome"
)


data = read_excel("INPUT/o2job/크롤링_의뢰_엑셀시트.xlsx", sheet = "아파트 매매 실거래가")


# 크롬 열기
remDr$open()

# 매물 시세 접속
remDr$navigate("https://onland.kbstar.com/quics?page=C059652")


i = 3

dataL1 = data.frame()

# for (i in 1:nrow(data)) {
foreach::foreach(i = 1:nrow(data), .combine = c) %do% {

  Sys.sleep(2)

  getRowData = data %>%
    tibble::rowid_to_column() %>%
    dplyr::filter(rowid == i) %>%
    dplyr::select(시군구:도로명)

  searchVal = paste(stringr::str_split(getRowData$시군구, pattern = " ")[[1]][3], getRowData$단지명)

  Sys.sleep(2)

  remDr$executeScript("unifiedSearchShow();")

  Sys.sleep(2)

  remDr$executeScript(paste0("$('#unifiedSearchKeyword').val('", searchVal, "')"))

  Sys.sleep(2)

  remDr$executeScript("unifiedSearchKeyword();")

  Sys.sleep(5)

  remDr$findElement(using = "xpath", value = '//*[@id="unifiedSearchResult"]/div[2]/ul/li')$clickElement()

  Sys.sleep(2)

  getHandle = remDr$getWindowHandles()
  setWindowTab(remDr, getHandle[[2]])

  Sys.sleep(2)

  # 총 세대수
  totalNumberHouseholds = getXpathText('//*[@id="ccChangeArea"]/div[*]/table/tbody/tr[1]/td[2]')[1]
  # //*[@id="ccChangeArea"]/div[3]/table/tbody/tr[1]/td[2]
  # //*[@id="ccChangeArea"]/div[5]/table/tbody/tr[1]/td[2]

  # 총 주차대수
  totalNumberParkingSpaces = getXpathText('//*[@id="ccChangeArea"]/div[*]/table/tbody/tr[2]/td[1]')[1]
  # //*[@id="ccChangeArea"]/div[3]/table/tbody/tr[2]/td[1]
  # //*[@id="ccChangeArea"]/div[5]/table/tbody/tr[2]/td[1]


  # 난방방식
  heatingSystem = getXpathText('//*[@id="ccChangeArea"]/div[*]/table/tbody/tr[4]/td[1]')
  # //*[@id="ccChangeArea"]/div[3]/table/tbody/tr[4]/td[1]

  Sys.sleep(2)

  remDr$findElement(using = "xpath", value = '//*[@id="siseTabBtn"]/a')$clickElement()

  Sys.sleep(2)

  # 현관구조
  structureFrontDoor = getXpathText('//*[@id="b062071"]/div[9]/table/tbody/tr[2]/td')


  if (length(totalNumberHouseholds) == 0) totalNumberHouseholds = NA
  if (length(totalNumberParkingSpaces) == 0) totalNumberParkingSpaces = NA
  if (length(heatingSystem) == 0) heatingSystem = NA
  if (length(structureFrontDoor) == 0) structureFrontDoor = NA

  setRowDataL1 = getRowData %>%
    dplyr::mutate(
      "현관구조" = structureFrontDoor
      , "가구당주차대수" = totalNumberParkingSpaces
      , "총세대수" = totalNumberHouseholds
      , "난방연료" = heatingSystem
    )


  dataL1 = dplyr::bind_rows(dataL1, setRowDataL1)

  # }, error = function(error) {
  #     message("Caught an error : " + error)
  # }, warning = function(warning) {
  #     message("Caught an warning : " + warning)
  # }, finally = {
  #
  #     percentVal = paste0(round((i / nrow(data)) * 100, 2), " %")
  #
  #     cat(percentVal , "\n")
  #     }
  # )

  percentVal = paste0(round((i / nrow(data)) * 100, 2), " %")
  cat(percentVal, "\n")

  Sys.sleep(5)

  remDr$closeWindow()

  Sys.sleep(2)

  setWindowTab(remDr, getHandle[[1]])
}

# remDr$close()

# Write Using L3 Data Frame
# data.table::fwrite(
#     dfData
#     , sep = ","
#     , file = paste0("OUTPUT/dfData_", format(Sys.time(), "%Y%m%d%H%M%S")
#                     , ".csv")
#     , append = FALSE
#     , row.names = FALSE
#     , col.names = TRUE
#     # , dateTimeAs = "write.csv"
#     , na = NA
# )


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(Hmisc)
library(fitdistrplus)

data = read.csv("INPUT/o2job/공무원연금공단_지역별_연령별_연금대출_현황.csv", sep = ",", header = TRUE, fileEncoding = "euc-kr")

# 1) 수치 요약
summary(data)

# 1) 명목 요약
Hmisc::describe(data)


#
# 2) 평균금액 분포함수
# (1) normal
getVal = data$평균금액
getLogVal = log(getVal)

meanVal = mean(getLogVal, na.rm = TRUE)
sdVal = sd(getLogVal, na.rm = TRUE)
maxVal = max(getLogVal, na.rm = TRUE)

# normal
normalData = pnorm(getLogVal, mean = meanVal, sd = sdVal)
hist(normalData)

# gamma
gammaData = pgamma(getLogVal, shape = 10, rate = 1)
hist(gammaData)

# poisson
poissonData = ppois(getLogVal, lambda = 100)
hist(poissonData)

# negative binomial
nBinomData = pnbinom(getLogVal, size = 10, prob = 0.2)
hist(nBinomData)

# 3) R 패키지를 이용한 파라메타 추정
hist(getLogVal, probability = TRUE)

setFitVal = fitdist(getLogVal, "norm")
summary(setFitVal)

setFitMeanVal = setFitVal$estimate[1]
setFitSdVal = setFitVal$estimate[2]

x = seq(0, 20, 0.01)
curve(dnorm(x, setFitMeanVal, setFitSdVal), col = "red", lwd = 2, add = TRUE)

# 4) Chebyshev's Inequality
n = length(getLogVal)

k = seq(0.5, 4, 0.5)
l = length(k)

result = data.frame(
  p1 = rep(0, l)
  , p2 = rep(0, l)
)

for (i in 1:l) {
  x1 = getLogVal[which(abs(getLogVal - meanVal) <= (k[i] * sdVal))]
  result$p1[i] = length(x1) / n
}

result$p2 = 1 - 1 / k^2
result

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(tidyverse)
library(xlsx)
library(tidyverse)
library(readr)
library(data.table)

# 02.가장 인기있는 인터넷 브라우저 1996-2019
data = read.csv("INPUT/o2job/rank/browser-ww-quarterly-20093-20202.csv", sep = ",", header = TRUE, fileEncoding = "euc-kr")

dataL1 = data %>%
  tidyr::gather(-Date, key = "key", value = "val") %>%
  tidyr::spread(key = "Date", value = "val") %>%
  dplyr::arrange(desc(`2019-4`)) %>%
  dplyr::top_n(13)

xlsx::write.xlsx2(dataL1, file = "OUTPUT/o2job/Work.xlsx", sheetName = "02", append = TRUE, row.names = FALSE, col.names = TRUE)

#  03.가장 인기있는 검색 엔진 1994-2019
data = read.csv("INPUT/o2job/rank/search_engine-ww-quarterly-20091-20202.csv", sep = ",", header = TRUE, fileEncoding = "euc-kr")

dataL1 = data %>%
  tidyr::gather(-Date, key = "key", value = "val") %>%
  tidyr::spread(key = "Date", value = "val") %>%
  dplyr::arrange(desc(`2019-4`)) %>%
  dplyr::top_n(13)

xlsx::write.xlsx2(dataL1, file = "OUTPUT/o2job/Work.xlsx", sheetName = "03", append = TRUE, row.names = FALSE, col.names = TRUE)

# 04.가장 인기있는 소셜 네트워크 2003-2019
data = read.csv("INPUT/o2job/rank/social_media-ww-quarterly-20091-20202.csv", sep = ",", header = TRUE, fileEncoding = "euc-kr")

dataL1 = data %>%
  tidyr::gather(-Date, key = "key", value = "val") %>%
  tidyr::spread(key = "Date", value = "val") %>%
  dplyr::arrange(desc(`2019-4`)) %>%
  dplyr::top_n(13)

xlsx::write.xlsx2(dataL1, file = "OUTPUT/o2job/Work.xlsx", sheetName = "04", append = TRUE, row.names = FALSE, col.names = TRUE)

# 05.서울 아파트 구별 가격변동 추이
data = read.csv("INPUT/o2job/rank/아파트매매가격지수_시도_시_군_구__20200612003125.csv", sep = ",", header = TRUE, fileEncoding = "euc-kr")

dataL1 = data %>%
  tidyr::gather(-행정구역별, key = "key", value = "val") %>%
  dplyr::mutate(
    dtYear = readr::parse_datetime(key, "X%Y..%m")
    , year = lubridate::year(dtYear)
  ) %>%
  dplyr::group_by(행정구역별, year) %>%
  dplyr::summarise(meanVal = mean(val, na.rm = TRUE)) %>%
  tidyr::spread(key = "year", value = "meanVal") %>%
  dplyr::arrange(desc(`2019`)) %>%
  as.data.frame()

xlsx::write.xlsx2(dataL1, file = "OUTPUT/o2job/Work.xlsx", sheetName = "05", append = TRUE, row.names = FALSE, col.names = TRUE)

# 10. 가장 인기있는 스트리밍 게임 2015-2019
fileList = Sys.glob("INPUT/o2job/rank/Game/*")

i = 2

dataL1 = data.frame()
for (i in 1:length(fileList)) {


  sYearMonth = stringr::str_sub(fileList[i], 23, 29)
  dtDate = readr::parse_datetime(sYearMonth, "%Y_%m")
  year = lubridate::year(dtDate)
  dtDate = lubridate::month(dtDate)

  data = data.table::fread(file = fileList[i])

  dataL1 = dplyr::bind_rows(dataL1
    , data.frame(data, "date" = sYearMonth)
  )
}

dataL2 = dataL1 %>%
  tidyr::gather(-date, key = "key", value = "val") %>%
  tidyr::spread(key = "date", value = "val")


# 15. 남자 야구 선수 상위 10명
data = read.csv("INPUT/o2job/rank/야구선수연봉.csv", sep = ",", header = TRUE, fileEncoding = "euc-kr")

dataL1 = data %>%
  tidyr::gather(-year, -name, key = "key", value = "val") %>%
  tidyr::spread(key = "year", value = "val")

xlsx::write.xlsx2(dataL1, file = "OUTPUT/o2job/Work.xlsx", sheetName = "15", append = TRUE, row.names = FALSE, col.names = TRUE)

# 15. 남자 야구 선수 상위 10명
# 두산이대호 (롯데): 25
# 박병호 (키움): 20
# 양의지 (NC): 20
# 손아섭 (롯데): 20
# 최형우 (KIA): 15
# 이재원 (SK): 13
# 김현수 (LG): 13
# 강민호 (삼성): 12.5
# 민병헌 (롯데): 12.5
# 최정 (SK): 12
# 황재균 (KT): 12
# 양현종 (KIA): 23
# 오승환 (삼성): 12
# 차우찬 (LG): 10
# 정우람 (한화): 8
# 우규민 (삼성): 7
# 유희관 (두산): 4.7
# 이용찬 (두산): 4.2
# 이현승 (두산): 4
# 윤성환 (삼성): 4
# 최원태 (키움): 3.7


library(RSelenium)
library(rvest)
library(stringr)
library(tidyverse)
library(data.table)
library(foreach)
library(httr)
library(webdriver)
library(seleniumPipes)
library(readxl)
library(foreach)
library(XML)

Sys.setlocale("LC_ALL")
options(encoding = "UTF-8")
Sys.setenv(LANG = "en_US.UTF-8")


dtDate = seq.Date(as.Date("2019-01-01"), as.Date("2019-12-31"), "1 day")
sDate = format(dtDate, "%Y%m%d")


url = sprintf("https://sullygnome.com/games/2016february/streamed")

# readHTMLTable(url)

xml2::read_html(url) %>%
  rvest::html_nodes("#tblControl") %>%
  rvest::html_tag("td")

rvest::html_nodes("img") %>%

  rvest::html_nodes(xpath = '//*[@id="tblControl"]') %>%
  rvest::html_tag("src")

rvest::html_tag("img")
# rvest::html_table(fill = TRUE)


xml2::read_html(url) %>%
  rvest::html_nodes(css = '.display') %>%
  rvest::html_table()


rvest::html_text()


getUrlmg()

dd = xml2::read_html(url) %>%
  rvest::html_nodes(xpath = '//*[@id="tblControl"]') %>%
  rvest::html_table()


getUrlText = function(url, xpath) {
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath = paste0(xpath)) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

getUrlHref = function(url, xpath) {
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath = paste0(xpath)) %>%
    rvest::html_attr("href")
}

getUrlmg = function(url, xpath) {
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath = paste0(xpath)) %>%
    rvest::html_attr("src")
}


getUrlText('//*[@id="main_content"]/div[2]/ul[*]/li[*]/dl/dt[2]/a')

url = sprintf("https://news.naver.com/main/list.nhn?mode=LSD&mid=sec&sid1=001&date=%s&page=%d", d, seq(1:1500))


for (d in sDate) {

  print(d)
  url = sprintf("https://news.naver.com/main/list.nhn?mode=LSD&mid=sec&sid1=001&date=%s&page=%d", d, seq(1:1500))

  data = url %>%
    purrr::map(~getUrlText(.x, '//*[@id="main_content"]/div[2]/ul[*]/li[*]/dl/dt[2]/a')) %>%
    unlist() %>%
    as.data.frame()

  dataL1 <- data %>%
    magrittr::set_colnames("title") %>%
    dplyr::distinct(title) %>%
    dplyr::mutate(date = d)


  write.csv(dataL1, paste0("./TITLE/2019/title_", d, ".csv"), fileEncoding = "CP949")

}


# options(add.error.underscore=FALSE)

setWindowTab = function(remDr, windowId) {
  qpath = sprintf("%s/session/%s/window", remDr$serverURL, remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}

getXpathText = function(xpath) {
  remDr$getPageSource()[[1]] %>%
    read_html() %>%
    rvest::html_nodes(xpath = xpath) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

getCssText = function(css) {
  remDr$getPageSource()[[1]] %>%
    read_html() %>%
    rvest::html_nodes(css = css) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}


# 2안)
# cd /c/selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 5000

remDr = remoteDriver(
  remoteServerAddr = "localhost"
  , port = 5000L
  , browserName = "chrome"
)


data = read_excel("INPUT/o2job/크롤링_의뢰_엑셀시트.xlsx", sheet = "아파트 매매 실거래가")


# 크롬 열기
remDr$open()

# 매물 시세 접속
remDr$navigate("https://onland.kbstar.com/quics?page=C059652")


i = 3

dataL1 = data.frame()

# for (i in 1:nrow(data)) {
foreach::foreach(i = 1:nrow(data), .combine = c) %do% {

  Sys.sleep(2)

  getRowData = data %>%
    tibble::rowid_to_column() %>%
    dplyr::filter(rowid == i) %>%
    dplyr::select(시군구:도로명)

  searchVal = paste(stringr::str_split(getRowData$시군구, pattern = " ")[[1]][3], getRowData$단지명)

  Sys.sleep(2)

  remDr$executeScript("unifiedSearchShow();")

  Sys.sleep(2)

  remDr$executeScript(paste0("$('#unifiedSearchKeyword').val('", searchVal, "')"))

  Sys.sleep(2)

  remDr$executeScript("unifiedSearchKeyword();")

  Sys.sleep(5)

  remDr$findElement(using = "xpath", value = '//*[@id="unifiedSearchResult"]/div[2]/ul/li')$clickElement()

  Sys.sleep(2)

  getHandle = remDr$getWindowHandles()
  setWindowTab(remDr, getHandle[[2]])

  Sys.sleep(2)

  # 총 세대수
  totalNumberHouseholds = getXpathText('//*[@id="ccChangeArea"]/div[*]/table/tbody/tr[1]/td[2]')[1]
  # //*[@id="ccChangeArea"]/div[3]/table/tbody/tr[1]/td[2]
  # //*[@id="ccChangeArea"]/div[5]/table/tbody/tr[1]/td[2]

  # 총 주차대수
  totalNumberParkingSpaces = getXpathText('//*[@id="ccChangeArea"]/div[*]/table/tbody/tr[2]/td[1]')[1]
  # //*[@id="ccChangeArea"]/div[3]/table/tbody/tr[2]/td[1]
  # //*[@id="ccChangeArea"]/div[5]/table/tbody/tr[2]/td[1]


  # 난방방식
  heatingSystem = getXpathText('//*[@id="ccChangeArea"]/div[*]/table/tbody/tr[4]/td[1]')
  # //*[@id="ccChangeArea"]/div[3]/table/tbody/tr[4]/td[1]

  Sys.sleep(2)

  remDr$findElement(using = "xpath", value = '//*[@id="siseTabBtn"]/a')$clickElement()

  Sys.sleep(2)

  # 현관구조
  structureFrontDoor = getXpathText('//*[@id="b062071"]/div[9]/table/tbody/tr[2]/td')


  if (length(totalNumberHouseholds) == 0) totalNumberHouseholds = NA
  if (length(totalNumberParkingSpaces) == 0) totalNumberParkingSpaces = NA
  if (length(heatingSystem) == 0) heatingSystem = NA
  if (length(structureFrontDoor) == 0) structureFrontDoor = NA

  setRowDataL1 = getRowData %>%
    dplyr::mutate(
      "현관구조" = structureFrontDoor
      , "가구당주차대수" = totalNumberParkingSpaces
      , "총세대수" = totalNumberHouseholds
      , "난방연료" = heatingSystem
    )


  dataL1 = dplyr::bind_rows(dataL1, setRowDataL1)

  # }, error = function(error) {
  #     message("Caught an error : " + error)
  # }, warning = function(warning) {
  #     message("Caught an warning : " + warning)
  # }, finally = {
  #
  #     percentVal = paste0(round((i / nrow(data)) * 100, 2), " %")
  #
  #     cat(percentVal , "\n")
  #     }
  # )

  percentVal = paste0(round((i / nrow(data)) * 100, 2), " %")
  cat(percentVal, "\n")

  Sys.sleep(5)

  remDr$closeWindow()

  Sys.sleep(2)

  setWindowTab(remDr, getHandle[[1]])
}


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(tidyverse)

data1 = readr::read_table2("INPUT/o2job/report (3).txt")
# data2 = read_table2("INPUT/o2job/report (4).txt")
data2 = readr::read_table2("INPUT/o2job/report (6).txt")
# data3 = readr::read_table2("INPUT/o2job/report (7).txt")
data4 = readr::read_table2("INPUT/o2job/report (8).txt")

tmpData4 = data4 %>%
  dplyr::mutate(
    dtYear = readr::parse_datetime(stringr::str_sub(기간_4분기, 1, 4), "%Y")
    , 기간 = lubridate::year(dtYear)
  ) %>%
  dplyr::select(-기간_4분기, -dtYear) %>%
  dplyr::group_by(자치구, 기간) %>%
  dplyr::summarise_all(funs(
    sum(., na.rm = TRUE) # 합계
  ))

data5 = readr::read_table2("INPUT/o2job/report (9).txt")
data6 = readr::read_table2("INPUT/o2job/report (10).txt")

data = data1 %>%
  dplyr::full_join(data2, by = c("기간" = "기간", "자치구" = "자치구")) %>%
  # dplyr::full_join(data3, by = c("기간" = "기간", "자치구" = "자치구")) %>%
  dplyr::full_join(tmpData4, by = c("기간" = "기간", "자치구" = "자치구")) %>%
  dplyr::full_join(data5, by = c("기간" = "기간", "자치구" = "자치구")) %>%
  dplyr::full_join(data6, by = c("기간" = "기간", "자치구" = "자치구"))


tmpData = data %>%
  dplyr::filter(자치구 != 합계)

tmpDataL1 = tmpData %>%
  dplyr::select(-기간, -자치구, -경찰청, -소방본부, -대기업_종사자수, -소방서) %>%
  dplyr::mutate_all(funs(as.numeric))

dataL1 = data.frame(기간 = tmpData$기간, 자치구 = tmpData$자치구, tmpDataL1) %>%
  dplyr::group_by(자치구, 기간) %>%
  dplyr::summarise_all(funs(
    sum(., na.rm = TRUE) # 합계
  )) # %>%
# tidyr::gather(-자치구, key = "key", value = "val")

# ggplot


dataL2 = na.omit(dataL1)

# tidyr::spread(key = "기간", value = "합계_발생")


unique(dataL2$자치구)
unique(dataL2$기간)

dd = cor(dataL2[, -1])

# 기간 평균
dataL3 = dataL2 %>%
  dplyr::group_by(자치구) %>%
  dplyr::summarise_all(funs(
    mean(., na.rm = TRUE) # 합계
  ))

d4 = cor(dataL3[, -1])


# dplyr::filter(자치구=="강남구")

require(Hmisc)
require(corrplot)

corData = Hmisc::rcorr(as.matrix(dataL2))

corMat = corData$r
pMat = corData$P

corrplot(corMat, type = "upper", order = "hclust", method = "number",
         p.mat = pMat, sig.level = 0.05, insig = "blank")


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(DescTools)
library(e1071)

data = data.frame(
  temp = c(38.8, 34.0, 39.0, 38.8, 36.2, 30.4, 32.2, 34.1, 35.0, 42.2, 37.3, 32.6, 31.6, 34.1, 34.1, 33.8, 35.8, 32.3, 36.3, 31.3)
  , sale = c(423000, 207900, 464600, 460000, 264500, 107500, 161600, 131200, 206000, 910400, 338600, 138300, 157400, 172100, 153000, 127200, 200600, 116100, 265200, 132500)
)

# 1. R을 활용한 시각화 문제입니다.
# (1)
hist(data$sale)

# (2)
stem(data$sale)

# (3)
table(data$sale)

# (4) 이상치 (outlier)가 존재합니다.
boxplot(data$sale)

# (5)
plot(data$temp, data$sale)

# 2. R을 활용한 통계량 측정 문제입니다.
# (1) 평균, 중위수 모드
mean(data$temp, na.rm = TRUE)
median(data$temp, na.rm = TRUE)
DescTools::Mode(data$temp, na.rm = TRUE)

# (2) 평균, 중위수 모드
mean(data$sale, na.rm = TRUE)
median(data$sale, na.rm = TRUE)
DescTools::Mode(data$sale, na.rm = TRUE)

# (3) 범위 분산, 표준편차, 변동계수
range(data$temp)
var(data$temp)
sd(data$temp)
range(data$temp)
sd(data$temp, na.rm = TRUE) / mean(data$temp, na.rm = TRUE)

# (4) 범위 분산, 표준편차, 변동계수
range(data$sale)
var(data$sale)
sd(data$sale)
range(data$sale)
sd(data$sale, na.rm = TRUE) / mean(data$sale, na.rm = TRUE)

# (5) 왜도, 첨도
e1071::skewness(data$temp)
e1071::kurtosis(data$temp)

# (6) 왜도, 첨도
e1071::skewness(data$sale)
e1071::kurtosis(data$sale)

# 3. R을 회귀분석 문제입니다.
# (1) 56233.92x -1711021.14
lmFit = lm(sale ~ temp, data = data)
paste0(round(coef(lmFit)[2], 2), "x + ", round(coef(lmFit)[1], 2))

# (2) 0.8093442
cor(data$temp, data$sale)^2

# (3) p-value는 6.784e-08로서 0.001 이하이기 때문에 통계적으로 유의하다다
summary(lmFit)

# (4)
predict(lmFit, newdata = data.frame(temp = 30:40))

# 4. R을 활용한 ANOVA 문제입니다.
data2 = data.frame(
  key = factor(c("LGU", "LGU", "LGU", "SKT", "SKT", "SKT", "SKT", "KT", "KT", "KT"))
  , val = c(5.3, 6.0, 6.7, 5.5, 6.2, 6.4, 5.7, 7.5, 7.2, 7.9)
)

# (1) aovResult에서 P-value는 0.00839로서 0.05보다 작기 떄문에 통신 3사에 따른 만족도 차이가 있다.
aovReult = aov(val ~ key, data = data2)
summary(aovReult)


# (2) 타 그룹 (LGU-KT, SKT-KT)에 비해 SKT-LGU의 p-value는 0.99로서 통계적으로 유의하지 않다
TukeyHSD(aovReult)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(RSelenium)
library(rvest)
library(stringr)
library(tidyverse)
library(data.table)
library(foreach)
library(httr)
library(webdriver)
library(seleniumPipes)
library(readxl)
library(foreach)
library(ggwordcloud)
library(wordcloud2)
library(htmlwidgets)
library(webshot)
library(xlsx)

# options(add.error.underscore=FALSE)

setWindowTab = function(remDr, windowId) {
  qpath = sprintf("%s/session/%s/window", remDr$serverURL, remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}

getXpathText = function(xpath) {
  remDr$getPageSource()[[1]] %>%
    read_html() %>%
    rvest::html_nodes(xpath = xpath) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

getCssText = function(css) {
  remDr$getPageSource()[[1]] %>%
    read_html() %>%
    rvest::html_nodes(css = css) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}


remDr = remoteDriver(
  remoteServerAddr = "localhost"
  , port = 5002L
  , browserName = "chrome"
)


# 크롬 열기
remDr$open()

# 학술연구정보서비스 접속
remDr$navigate("http://www.riss.kr/index.do")

getHandle = remDr$getWindowHandles()
setWindowTab(remDr, getHandle[[1]])

searchVal = "SW 검색"

remDr$executeScript(paste0("$('#query').val('", searchVal, "')"))

# 검색 버튼
remDr$findElement(using = "xpath", value = '//*[@id="fullpage"]/div[1]/div/div/div[1]/div/div[2]/form/fieldset/div[1]/div/button')$clickElement()


# 학위논문 메뉴 이동
remDr$findElement(using = "xpath", value = '//*[@id="tabMenu"]/div/ul/li[2]/a/span')$clickElement()


# 목록 1000개씩 출력
remDr$executeScript(paste0("$('#sortSelect2_top option:checked').val(1000);"))

# 재조회
remDr$executeScript("orderSearch('re_a_kor');")

# 학위논문에 대한 논문 목록
getPaper = getXpathText('//*[@id="divContent"]/div[2]/div/div[2]/div[2]/ul/li[*]/div[2]/p[1]/a')

# 학위논문에 대한 논문 Url 목록
getHref = remDr$getPageSource()[[1]] %>%
  read_html() %>%
  rvest::html_nodes(xpath = '//*[@id="divContent"]/div[2]/div/div[2]/div[2]/ul/li[*]/div[2]/p[1]/a') %>%
  rvest::html_attr("href")

dataL1 = data.frame()

foreach::foreach(i = 1:length(getPaper), .combine = c) %do% {
  getDetailPage = paste0("http://www.riss.kr/", getHref[i])
  remDr$navigate(getDetailPage)

  Sys.sleep(2)

  getWriter = getXpathText('//*[@id="soptionview"]/div/div[1]/div[2]/div[1]/ul/li[1]/div/p/a')[1]
  # 키워드 검색
  getTagInst = getCssText('div p .instituteInfo')

  getKeyword = data.frame(주제어 = getTagInst) %>%
    dplyr::filter(주제어 != getWriter)

  if (nrow(getKeyword) == 1) {
    getKeyword = stringr::str_split(getKeyword, ",") %>%
      unlist()
  }

  data = data.frame(
    "번호" = i
    , "논문" = getPaper[i]
    , "url" = getHref[i]
    , "저자" = getWriter
    , "주제어" = getKeyword
  )

  dataL1 = dplyr::bind_rows(dataL1, data)
}


dataL2 = dataL1 %>%
  dplyr::group_by(주제어) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame()

ggplot(dataL2, aes(label = 주제어, color = 주제어)) +
  geom_text_wordcloud() +
  theme_minimal()

fig = wordcloud2(data = dataL2)

# html로 내보내기
saveWidget(fig, "fig.html", selfcontained = FALSE)

# 내보낸 html 페이지로부터 png 형태로 불러와서 저장
webshot::webshot("fig.html", "Keyword.png", vwidth = 775, vheight = 550, delay = 10)

# 출력
xlsx::write.xlsx2(dataL1, file = "OUTPUT/o2job/Keyword.xlsx", sheetName = "키워드", append = TRUE, row.names = FALSE, col.names = TRUE)

xlsx::write.xlsx2(dataL2, file = "OUTPUT/o2job/Keyword.xlsx", sheetName = "빈도분석", append = TRUE, row.names = FALSE, col.names = TRUE)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(forecastML)
library(dplyr)
library(DT)
library(ggplot2)
library(glmnet)
library(randomForest)

data("data_seatbelts", package = "forecastML")
data <- data_seatbelts
date_frequency <- "1 month" # Time step frequency.
# The date indices, which don't come with the stock dataset, should not be included in the modeling
dates <- seq(as.Date("1969-01-01"), as.Date("1984-12-01"), by = date_frequency)
data$PetrolPrice <- round(data$PetrolPrice, 3)
data <- data[, c("DriversKilled", "kms", "PetrolPrice", "law")]
DT::datatable(head(data, 5))

data_train <- data[1:(nrow(data) - 12),]
data_test <- data[(nrow(data) - 12 + 1):nrow(data),]

outcome_col <- 1  # The column index of our DriversKilled outcome.

horizons <- c(1, 3, 6, 12)  # 4 models that forecast 1, 1:3, 1:6, and 1:12 time steps ahead.

# A lookback across select time steps in the past. Feature lags 1 through 9, for instance, will be
# silently dropped from the 12-step-ahead model.
lookback <- c(1:6, 9, 12, 15)

# A non-lagged feature that changes through time whose value we either know (e.g., month) or whose
# value we would like to forecast.
dynamic_features <- "law"

data_list <- forecastML::create_lagged_df(data_train,
                                          outcome_col = outcome_col,
                                          type = "train",
                                          horizons = horizons,
                                          lookback = lookback,
                                          date = dates[1:nrow(data_train)],
                                          frequency = date_frequency,
                                          dynamic_features = dynamic_features
)

windows <- forecastML::create_windows(lagged_df = data_list, window_length = 24, skip = 0,
                                      window_start = NULL, window_stop = NULL,
                                      include_partial_window = TRUE)
windows


# Example 1 - LASSO
# Alternatively, we could define an outcome column identifier argument, say, 'outcome_col = 1' in
# this function or just 'outcome_col' and then set the argument as 'outcome_col = 1' in train_model().

model_function <- function(data) {

  # The 'law' feature is constant during some of our outer-loop validation datasets so we'll
  # simply drop it so that glmnet converges.
  constant_features <- which(unlist(lapply(data[, -1], function(x) { !(length(unique(x)) > 1) })))

  if (length(constant_features) > 1) {
    data <- data[, -c(constant_features + 1)]  # +1 because we're skipping over the outcome column.
  }

  x <- data[, -(1), drop = FALSE]
  y <- data[, 1, drop = FALSE]
  x <- as.matrix(x, ncol = ncol(x))
  y <- as.matrix(y, ncol = ncol(y))

  model <- glmnet::cv.glmnet(x, y, nfolds = 3)
  return(list("model" = model, "constant_features" = constant_features))
}

# Example 2 - Random Forest
# Alternatively, we could define an outcome column identifier argument, say, 'outcome_col = 1' in
# this function or just 'outcome_col' and then set the argument as 'outcome_col = 1' in train_model().
model_function_2 <- function(data) {

  outcome_names <- names(data)[1]
  model_formula <- formula(paste0(outcome_names, "~ ."))

  model <- randomForest::randomForest(formula = model_formula, data = data, ntree = 200)
  return(model)
}

#=================================================================
library(e1071)
library(adabag)
library(rpart)
library(ipred)

# SVM
model_function_3 <- function(data) {

  outcome_names <- names(data)[1]
  model_formula <- formula(paste0(outcome_names, "~ ."))

  model <- e1071::svm(formula = model_formula, data = data, method = "C-classification", kernal = "radial", gamma = 0.1, cost = 10)


  return(model)
}

# Bagging
model_function_4 <- function(data) {

  outcome_names <- names(data)[1]
  model_formula <- formula(paste0(outcome_names, "~ ."))


  # model <- e1071::svm(formula = model_formula, data = data, method="C-classification", kernal="radial", gamma=0.1, cost=10)
  model = ipred::bagging(formula = model_formula, data = data, coob = TRUE)


  return(model)
}

# Boosting
model_function_5 <- function(data) {

  library(gbm)

  outcome_names <- names(data)[1]
  model_formula <- formula(paste0(outcome_names, "~ ."))

  model <- gbm::gbm(formula = model_formula, data = data, distribution = "multinomial")
  # model <- adabag::boosting(formula = model_formula, data = data, boos = FALSE, mfinal = 100, coeflearn = 'Zhu')

  return(model)
}

#=================================================================

# windows <- forecastML::create_windows(lagged_df = data_list, window_length = 24, skip = 48,
#                                       window_start = NULL, window_stop = NULL,
#                                       include_partial_window = TRUE)
# windows

#future::plan(future::multiprocess)

model_results <- forecastML::train_model(data_list, windows, model_name = "LASSO",
                                         model_function, use_future = FALSE)

model_results_2 <- forecastML::train_model(data_list, windows, model_name = "RF",
                                           model_function_2, use_future = FALSE)

# model_results_3 <- forecastML::train_model(data_list, windows, model_name = "SVM",
#                                            model_function_3, use_future = FALSE)

model_results_4 <- forecastML::train_model(data_list, windows, model_name = "BAGGING",
                                           model_function_4, use_future = FALSE)

# model_results_5 <- forecastML::train_model(data_list, windows, model_name = "BOOSTING",
#                                            model_function_5, use_future = FALSE)


# Example 1 - LASSO.
prediction_function <- function(model, data_features) {

  if (length(model$constant_features) > 1) {  # 'model' was passed as a list.
    data_features <- data_features[, -c(model$constant_features)]
  }

  x <- as.matrix(data_features, ncol = ncol(data_features))

  data_pred <- data.frame("y_pred" = predict(model$model, x, s = "lambda.min"))
  return(data_pred)
}


prediction_function_2 <- function(model, data_features) {

  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}

#=================================================================
# SVM
prediction_function_3 <- function(model, data_features) {

  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}

# Bagging
prediction_function_4 <- function(model, data_features) {

  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}

# Boosting
prediction_function_5 <- function(model, data_features) {

  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}

#=================================================================

data_results <- predict(model_results, model_results_2, model_results_4, prediction_function = list(prediction_function, prediction_function_2, prediction_function_4), data = data_list)


data_results$DriversKilled_pred <- round(data_results$DriversKilled_pred, 0)
DT::datatable(head(data_results, 30), options = list(scrollX = TRUE))


plot(data_results, type = "prediction", horizons = c(1, 6, 12))


# horizons 별 결과 시각화 ( 예측값, 잔차, 안정성 )
plot(data_results, type = "prediction", horizons = c(1, 6, 12))
plot(data_results, type = "residual", horizons = c(1, 6, 12), windows = 1:4)
plot(data_results, type = "forecast_stability", windows = max(data_results$window_number))

# 예측결과 지표들을 보여줌 (mae, mape, smape, 등등..)
data_error <- forecastML::return_error(data_results, metrics = c("mae", "mape", "smape"))
data_error$error_global[, c("mae", "mape", "smape")] <- lapply(data_error$error_global[, c("mae", "mape", "smape")], round, 1)

DT::datatable(data_error$error_global, options = list(scrollX = TRUE))

# 예측오차의 다양한 시각화
# plot(data_error, data_results, type = "time", facet = ~ horizon, horizons = c(1, 6, 12))
# plot(data_error, data_results, type = "horizon", facet = ~ horizon, horizons = c(1, 6, 12))
# plot(data_error, data_results, type = "global")
plot(data_error, type = "window", facet = ~horizon, horizons = c(1, 6, 12))
plot(data_error, type = "horizon", facet = ~horizon, horizons = c(1, 6, 12))
plot(data_error, type = "global", facet = ~horizon)

# hyperparameters를 살펴보기 위해 model의 hyperparameter를 return 하는 함수 정의
hyper_function <- function(model) {

  lambda_min <- model$model$lambda.min
  lambda_1se <- model$model$lambda.1se

  data_hyper <- data.frame("lambda_min" = lambda_min, "lambda_1se" = lambda_1se)
  return(data_hyper)
}

# return_hyper를 통해 hyperparameter 살펴보기
data_hyper <- forecastML::return_hyper(model_results, hyper_function)

# hyperparameter별 결과 시각화
plot(data_hyper, data_results, data_error, type = "stability", horizons = c(1, 6, 12))
plot(data_hyper, data_results, data_error, type = "error", c(1, 6, 12))


# test data에 대한 예측을 위한 forecast dataset 생성
# type = 'forecast'
data_forecast_list <- forecastML::create_lagged_df(data_train,
                                                   outcome_col = outcome_col,
                                                   type = "forecast",
                                                   horizons = horizons,
                                                   lookback = lookback,
                                                   date = dates[1:nrow(data_train)],
                                                   frequency = date_frequency,
                                                   dynamic_features = dynamic_features
)

# forecast dataset 예시
DT::datatable(head(data_forecast_list$horizon_6), options = list(scrollX = TRUE))


# 안전벨트 법 도입시기 이므로 law = 1 대입
for (i in seq_along(data_forecast_list)) {
  data_forecast_list[[i]]$law <- 1
}

# forecast dataset에 대해 훈련된 모델 적용
data_forecast <- predict(model_results, model_results_2,
                         prediction_function = list(prediction_function, prediction_function_2),
                         data = data_forecast_list)


# horizons 마다의 forecast 결과
data_forecast$DriversKilled_pred <- round(data_forecast$DriversKilled_pred, 0)
DT::datatable(head(data_forecast, 10), options = list(scrollX = TRUE))


# forecast dataset의 예측결과 시각화
# 각 windows별 훈련된 모델로 예측하기 때문에 plot마다 7개의 예측결과를 나타냄
plot(data_forecast,
     data_actual = data[-(1:150),],  # Actuals from the training and test data sets.
     actual_indices = dates[-(1:150)],
     horizons = c(1, 6, 12))

# forecast error 보기
data_error <- forecastML::return_error(data_forecast,
                                       data_test = data_test,
                                       test_indices = dates[(nrow(data_train) + 1):length(dates)],
                                       metrics = c("mae", "mape", "smape", "mdape"))

data_error$error_by_horizon[, c("mae", "mape", "smape", "mdape")] <- lapply(data_error$error_by_horizon[, c("mae", "mape", "smape", "mdape")], round, 1)
DT::datatable(head(data_error$error_by_horizon, 10), options = list(scrollX = TRUE))  # LASSO로 결정


# training dataset을 전부 훈련하기 대한 data list 생성
data_list <- forecastML::create_lagged_df(data_train,
                                          outcome_col = outcome_col,
                                          type = "train",
                                          horizons = horizons,
                                          lookback = lookback,
                                          date = dates[1:nrow(data_train)],
                                          frequency = date_frequency,
                                          dynamic_features = dynamic_features
)

# training dataset 전부 훈련해야하므로 window_length=0 으로 지정
windows <- forecastML::create_windows(data_list, window_length = 0)
plot(windows, data_list, show_labels = TRUE) # 시각화

# training datasest 학습
# LASSO로 결정
model_results <- forecastML::train_model(data_list, windows, model_name = "LASSO", model_function)

# historical data 예측 (training dataset을 예측)
data_results <- predict(model_results, prediction_function = list(prediction_function), data = data_list)
DT::datatable(head(data_results, 10), options = list(scrollX = TRUE))

plot(data_results, type = "prediction", horizons = c(1, 6, 12)) # horizons 에 따른 시각화

# 예측 결과 지표 return
data_error <- forecastML::return_error(data_results, metrics = c("mae", "mape", "mdape", "smape"),
                                       models = NULL)

data_error$error_global[, c("mae", "mape", "mdape", "smape")] <- lapply(data_error$error_global[, c("mae", "mape", "mdape", "smape")], round, 1)
DT::datatable(head(data_error$error_global), options = list(scrollX = TRUE))


######################################################################################

# 데이터 불러오기
data("data_seatbelts", package = "forecastML")
data <- data_seatbelts

date_frequency <- "1 month"  # Time step frequency.

# 날짜 sequence 생성
dates <- seq(as.Date("1969-01-01"), as.Date("1984-12-01"), by = date_frequency)


data$PetrolPrice <- round(data$PetrolPrice, 3) # 소수점 3자리까지 표현
data <- data[, c("DriversKilled", "kms", "PetrolPrice", "law")]  # 사용 할 변수 선택
DT::datatable(head(data, 5)) # 데이터 예시


# 마지막 12달 : test data
# 12달 제외 : train data
data_train <- data[1:(nrow(data) - 12),]
data_test <- data[(nrow(data) - 12 + 1):nrow(data),]


# 날짜별 Driverskilled 시각화
p <- ggplot(data, aes(x = dates, y = DriversKilled)) # plot
p <- p + geom_line() # 선으로 표현
p <- p + geom_vline(xintercept = dates[nrow(data_train)], color = "red", size = 1.1) # train test 분리선
p <- p + theme_bw() + xlab("Dataset index") # 배경 밑 x축 label 변경
p

outcome_col <- 1  # 반응변수 열의 번호

# 예측 기간 종류( 1-step-ahead, 3-step-ahead , ...)
horizons <- c(1, 3, 6, 12)

# 변수로 사용할 과거 시점들
lookback <- c(1:6, 9, 12, 15)

dynamic_features <- "law" # 현재 시점에만 사용되는 변수


# horizons와 lookback의 조합으로 data_list 생성
# horizons의 개수만큼 list 생성
data_list <- forecastML::create_lagged_df(data_train,
                                          outcome_col = outcome_col,
                                          type = "train",
                                          horizons = horizons,
                                          lookback = lookback,
                                          date = dates[1:nrow(data_train)],
                                          frequency = date_frequency,
                                          dynamic_features = dynamic_features
)

# horizons = 6 일 때의 예시
DT::datatable(head(data_list$horizon_6, 10), options = list(scrollX = TRUE))

plot(data_list) # 각 horizons 별 사용할 시점과 예측할 미래 시점 시각화


# cross vaidation을 위한 기간 분할
# window_length = 24  ==> 한 block 당 데이터 수 = 24
windows <- forecastML::create_windows(lagged_df = data_list, window_length = 24, skip = 0,
                                      window_start = NULL, window_stop = NULL,
                                      include_partial_window = TRUE)

# 시각화
plot(windows, data_list, show_labels = TRUE)


# Example 1 - LASSO

# 데이터를 LASSO에 적합하여 model로 return하는 함수 정의
# 이후에 train_model() 에 쓰기 위함
model_function <- function(data) {

  # The 'law' feature is constant during some of our outer-loop validation datasets so we'll
  # simply drop it so that glmnet converges.
  constant_features <- which(unlist(lapply(data[, -1], function(x) { !(length(unique(x)) > 1) })))

  if (length(constant_features) > 1) {
    data <- data[, -c(constant_features + 1)]  # +1 because we're skipping over the outcome column.
  }

  x <- data[, -(1), drop = FALSE]
  y <- data[, 1, drop = FALSE]
  x <- as.matrix(x, ncol = ncol(x))
  y <- as.matrix(y, ncol = ncol(y))

  model <- glmnet::cv.glmnet(x, y, nfolds = 3)
  return(list("model" = model, "constant_features" = constant_features))
}

# Example 2 - Random Forest
# 데이터를 RandomForest에 적합하여 model로 return하는 함수 정의
# 이후에 train_model() 에 쓰기 위함
model_function_2 <- function(data) {

  outcome_names <- names(data)[1]
  model_formula <- formula(paste0(outcome_names, "~ ."))

  model <- randomForest::randomForest(formula = model_formula, data = data, ntree = 200)
  return(model)
}

# Example 3 - SVM
model_function_3 <- function(data) {

  outcome_names <- names(data)[1]
  model_formula <- formula(paste0(outcome_names, "~ ."))

  model <- e1071::svm(formula = model_formula, data = data, method = "C-classification", kernal = "radial")
  return(model)
}

# Example 4 - Bagging
model_function_4 <- function(data) {

  outcome_names <- names(data)[1]
  model_formula <- formula(paste0(outcome_names, "~ ."))

  model = ipred::bagging(formula = model_formula, data = data, coob = TRUE)
  return(model)
}

# Example 5 - Ridge
model_function_5 <- function(data) {
  constant_features <- which(unlist(lapply(data[, -1], function(x) { !(length(unique(x)) > 1) })))

  if (length(constant_features) > 1) {
    data <- data[, -c(constant_features + 1)]  # +1 because we're skipping over the outcome column.
  }

  x <- data[, -(1), drop = FALSE]
  y <- data[, 1, drop = FALSE]
  x <- as.matrix(x, ncol = ncol(x))
  y <- as.matrix(y, ncol = ncol(y))

  model <- glmnet::cv.glmnet(x, y, nfolds = 3, alpha = 0)
  return(list("model" = model, "constant_features" = constant_features))
}

############################################################################################################################################################

# cross_validation

model_results <- forecastML::train_model(data_list, windows, model_name = "LASSO",
                                         model_function, use_future = FALSE)

model_results_2 <- forecastML::train_model(data_list, windows, model_name = "RF",
                                           model_function_2, use_future = FALSE)
model_results_3 <- forecastML::train_model(data_list, windows, model_name = "SVM",
                                           model_function_3, use_future = FALSE)

model_results_4 <- forecastML::train_model(data_list, windows, model_name = "BAGGING",
                                           model_function_4, use_future = FALSE)

model_results_5 <- forecastML::train_model(data_list, windows, model_name = "Ridge",
                                           model_function_5, use_future = FALSE)

############################################################################################################################################################

# 훈련된 LASSO 모델을 이용해 predict하여 data.frame으로 return 하는 함수 정의
prediction_function <- function(model, data_features) {

  if (length(model$constant_features) > 1) {  # 'model' was passed as a list.
    data <- data[, -c(model$constant_features)]
  }

  x <- as.matrix(data_features, ncol = ncol(data_features))

  data_pred <- data.frame("y_pred" = predict(model$model, x, s = "lambda.min"))
  return(data_pred)
}

# Example 2 - Random Forest
# 훈련된 RF 모델을 이용해 predict하여 data.frame으로 return 하는 함수 정의
prediction_function_2 <- function(model, data_features) {

  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}

# Example 3 - SVM
prediction_function_3 <- function(model, data_features) {

  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}

# Example 4 - Bagging
prediction_function_4 <- function(model, data_features) {

  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}

# Example 5 - Ridge
prediction_function_5 <- function(model, data_features) {

  if (length(model$constant_features) > 1) {  # 'model' was passed as a list.
    data <- data[, -c(model$constant_features)]
  }

  x <- as.matrix(data_features, ncol = ncol(data_features))

  data_pred <- data.frame("y_pred" = predict(model$model, x, s = "lambda.min"))
  return(data_pred)
}

# 각 모델과 horizons별 예측 결과
# windows 별 결과도 보여줌
data_results <- predict(model_results, model_results_2, model_results_3, model_results_4, model_results_5,
                        prediction_function = list(prediction_function, prediction_function_2, prediction_function_3, prediction_function_4, prediction_function_5),
                        data = data_list)

# 예측값 정수로 만듦
data_results$DriversKilled_pred <- round(data_results$DriversKilled_pred, 0)

# 예측값 보기
DT::datatable(head(data_results, 30), options = list(scrollX = TRUE))


# horizons 별 결과 시각화 ( 예측값, 잔차, 안정성 )
plot(data_results, type = "prediction", horizons = c(1, 6, 12))
plot(data_results, type = "residual", horizons = c(1, 6, 12), windows = 5:7)
plot(data_results, type = "forecast_stability", windows = max(data_results$window_number))

# 예측결과 지표들을 보여줌 (mae, mape, smape, 등등..)
data_error <- forecastML::return_error(data_results, metrics = c("mae", "mape", "smape"))
data_error$error_global[, c("mae", "mape", "smape")] <- lapply(data_error$error_global[, c("mae", "mape", "smape")], round, 1)

DT::datatable(data_error$error_global, options = list(scrollX = TRUE))

# 예측오차의 다양한 시각화
# plot(data_error, data_results, type = "windows", facet = ~ horizon, horizons = c(1, 6, 12), windows = 5:7)
# plot(data_error, data_results, type = "horizon", facet = ~ horizon, horizons = c(1, 6, 12))
# plot(data_error, data_results, type = "global")

plot(data_error, type = "window", facet = ~horizon, horizons = c(1, 6, 12))
plot(data_error, type = "horizon", facet = ~horizon, horizons = c(1, 6, 12))
plot(data_error, type = "global", facet = ~horizon)

# 각 모델과 horizons별 예측 결과
# windows 별 결과도 보여줌
data_results <- predict(model_results, model_results_2, model_results_4,
                        prediction_function = list(prediction_function, prediction_function_2, prediction_function_4),
                        data = data_list)

# 예측값 정수로 만듦
data_results$DriversKilled_pred <- round(data_results$DriversKilled_pred, 0)

# 예측값 보기
DT::datatable(head(data_results, 30), options = list(scrollX = TRUE))


# horizons 별 결과 시각화 ( 예측값, 잔차, 안정성 )
plot(data_results, type = "prediction", horizons = c(1, 6, 12))
plot(data_results, type = "residual", horizons = c(1, 6, 12), windows = 5:7)
plot(data_results, type = "forecast_stability", windows = max(data_results$window_number))

# 예측결과 지표들을 보여줌 (mae, mape, smape, 등등..)
data_error <- forecastML::return_error(data_results, metrics = c("mae", "mape", "smape"))
data_error$error_global[, c("mae", "mape", "smape")] <- lapply(data_error$error_global[, c("mae", "mape", "smape")], round, 1)

DT::datatable(data_error$error_global, options = list(scrollX = TRUE))

# 예측오차의 다양한 시각화
# plot(data_error, data_results, type = "windows", facet = ~ horizon, horizons = c(1, 6, 12))
# plot(data_error, data_results, type = "horizon", facet = ~ horizon, horizons = c(1, 6, 12))
# plot(data_error, data_results, type = "global")

plot(data_error, type = "window", facet = ~horizon, horizons = c(1, 6, 12))
plot(data_error, type = "horizon", facet = ~horizon, horizons = c(1, 6, 12))
plot(data_error, type = "global", facet = ~horizon)


# forecast data list 생성
data_forecast_list <- forecastML::create_lagged_df(data_train,
                                                   outcome_col = outcome_col,
                                                   type = "forecast",
                                                   horizons = horizons,
                                                   lookback = lookback,
                                                   date = dates[1:nrow(data_train)],
                                                   frequency = date_frequency,
                                                   dynamic_features = dynamic_features
)

# 안전벨트 법 도입 (law = 1)
for (i in seq_along(data_forecast_list)) {
  data_forecast_list[[i]]$law <- 1
}

# training dataset 전체로 훈련된 모델을 forecast data에 적용
data_forecast <- predict(model_results, prediction_function = list(prediction_function), data = data_forecast_list)

# 예측 결과 시각화
plot(data_forecast,
     data_actual = data[-(1:150),],
     actual_indices = dates[-(1:150)])


# 예측 결과 지표 return
data_error <- forecastML::return_error(data_forecast, data_test = data_test,
                                       test_indices = dates[(nrow(data_train) + 1):nrow(data)],
                                       metrics = c("mae", "mape", "mdape", "smape"))

data_error$error_by_horizon[, c("mae", "mape", "mdape", "smape")] <- lapply(data_error$error_by_horizon[, c("mae", "mape", "mdape", "smape")], round, 1)
data_error$error_global[, c("mae", "mape", "mdape", "smape")] <- lapply(data_error$error_global[, c("mae", "mape", "mdape", "smape")], round, 1)

DT::datatable(data_error$error_global, options = list(scrollX = TRUE))


# 각 h-step-ahead forecasting을 결합
data_combined <- forecastML::combine_forecasts(data_forecast)


data_actual <- data[dates >= as.Date("1980-01-01"),]
actual_indices <- dates[dates >= as.Date("1980-01-01")]

# 모델 결합 결과 시작화
plot(data_combined, data_actual = data_actual, actual_indices = actual_indices)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================
library(RSelenium)
library(rvest)
library(stringr)
library(tidyverse)
library(data.table)
library(foreach)
library(httr)
library(webdriver)
library(seleniumPipes)
library(readxl)
library(foreach)
library(ggwordcloud)
library(wordcloud2)
library(htmlwidgets)
library(webshot)
library(xlsx)
library(log4r)
library(readxl)
library(noncompliance)

Sys.setlocale("LC_ALL", "English")
options(encoding = "UTF-8")
Sys.setenv(LANG = "en_US.UTF-8")

log = log4r::create.logger()
log4r::logfile(log) = paste0("OUTPUT/o2job/log4r_", format(Sys.time(), "%Y%m%d"), ".log")
log4r::level(log) = "INFO"

setWindowTab = function(remDr, windowId) {
  qpath = sprintf("%s/session/%s/window", remDr$serverURL, remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}

getXpathText = function(xpath) {
  remDr$getPageSource()[[1]] %>%
    read_html() %>%
    rvest::html_nodes(xpath = xpath) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

getCssText = function(css) {
  remDr$getPageSource()[[1]] %>%
    read_html() %>%
    rvest::html_nodes(css = css) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}


remDr = remoteDriver(
  remoteServerAddr = "localhost"
  , port = 5001
  , browserName = "chrome"
)


# system("ls")
#
# shell.exec("selenium")
# shell.exec("cmd.exe dir")
# shell.exec("java -Dwebdriver.gecko.driver=geckodriver.exe -jar selenium-server-standalone-3.141.59.jar -port 5000")


# 크롬 열기
remDr$open()

# while(TRUE) {

# getRootUrl = 'https://www.qoo10.jp/s/G9SKIN?keyword=+G9skin&furusato_gdlc_cd=&keyword_auto_change='
getRootUrl = 'https://www.qoo10.jp/s/BERRISOM?keyword=berrisom&furusato_gdlc_cd=&keyword_auto_change='

remDr$navigate(getRootUrl)

Sys.setlocale("LC_ALL", "English")
options(encoding = "UTF-8")
Sys.setenv(LANG = "en_US.UTF-8")

getRootGoodCode = remDr$getPageSource()[[1]] %>%
  read_html() %>%
  rvest::html_nodes("#search_result_item_list tr") %>%
  rvest::html_attr("goodscode")

getRootTableData = remDr$getPageSource()[[1]] %>%
  read_html() %>%
  rvest::html_node(xpath = '//*[@id="search_rst_gform"]/div[4]/table') %>%
  rvest::html_table()

tmpRootUrl = remDr$getPageSource()[[1]] %>%
  read_html() %>%
  rvest::html_nodes("#search_result_item_list .sbj a") %>%
  rvest::html_attr("href")

getRootUrl = data.frame(url = tmpRootUrl) %>%
  dplyr::filter(stringr::str_detect(url, "banner_no"))

setRootData = data.frame(
  "getRootGoodCode" = getRootGoodCode
  , "getRootUrl" = getRootUrl
  , getRootTableData
)

dataL2 = data.frame()
i = 1
for (i in 1:nrow(setRootData)) {

  tryCatch(
    expr = {
      getGoodCode = setRootData$getRootGoodCode[i]
      getUrl = setRootData$url[i]
      remDr$navigate(getUrl)

      Sys.setlocale("LC_ALL", "English")
      options(encoding = "UTF-8")
      Sys.setenv(LANG = "en_US.UTF-8")

      getTot = getXpathText('//*[@id="total_feedback_cnt"]')

      dataL1 = data.frame()

      if (length(getTot) > 0) {

        getIndex = ceiling(as.integer(stringr::str_remove_all(getTot, ",")) / 10)


        for (j in 1:getIndex) {

          log4r::info(log, paste0("진행률 : ", round((i / nrow(setRootData)) * 100, 2), " %"))

          remDr$executeScript(paste0('javascript:opinionList(', j, ');'))

          getTableData = remDr$getPageSource()[[1]] %>%
            read_html() %>%
            rvest::html_node("#tb_opinion") %>%
            rvest::html_table()

          dataL1 = dplyr::bind_rows(dataL1, getTableData) %>%
            dplyr::mutate(goodCode = setRootData[i, 1])
        }
      } else {
        dataL1 = data.frame(goodCode = setRootData[i, 1])
      }

      dataL2 = dplyr::bind_rows(dataL2, dataL1)
    }
    , warning = function(warning) { log4r::warn(log, warning) }
    , error = function(error) { log4r::error(log, error) }
    , finally = { getText = NULL }
  )
}


dataL3 = dataL2 %>%
  dplyr::left_join(setRootData, by = c("goodCode" = "getRootGoodCode"))

colnames(dataL3) = c("evaluation ", "feedback", "date", "writer", "goodCode", "url", "NA", "productName", "price", "type", "rank")

dataL4 = dataL3 %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    review = stringr::str_trim(unlist(stringr::str_split(feedback, " ", n = 2))[1], "both")
    , reviewProduct = stringr::str_trim(unlist(stringr::str_split(feedback, " ", n = 2))[2], "both")
  ) %>%
  as.data.frame()


# dataL3 = xlsx::read.xlsx2(file = "OUTPUT/o2job/Qoo10.xlsx", sheetName = "G9SKIN", append = TRUE, row.names = FALSE, col.names = TRUE)

# 출력
# xlsx::write.xlsx2(dataL3, file = "OUTPUT/o2job/Qoo10.xlsx", sheetName = "G9SKIN", append = TRUE, row.names = FALSE, col.names = TRUE)

xlsx::write.xlsx2(dataL4, file = "OUTPUT/o2job/Qoo10.xlsx", sheetName = "berrisom", append = TRUE, row.names = FALSE, col.names = TRUE)

# }


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

# 1번 문제
# 월       Jan Feb Mar Apr May Jun Jul  Aug Sep Oct Nov Dec
# 최저기온 –10 –9   2    3   10  12  22  26   20  13   5   -8
#
# 1) weather라는 벡터를 생성하여 월별 최저온도 값을 저장하시오.
weather = data.frame(
  key = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  , val = c(-10, -9, 2, 3, 10, 12, 22, 26, 20, 13, 5, -8)
)

# 2) 6월 최저기온을 출력하시오
ind = which(weather$key == "Jun")
weather[ind,]

# 3) 1월과 12월의 최저기온을 동시에 출력하시오
ind = which(weather$key == "Jan" | weather$key == "Dec")
weather[ind,]

# 4) 1~6월 최저기온의 평균을 출력하시오
ind = which(weather$key == "Jan" |
              weather$key == "Feb" |
              weather$key == "Mar" |
              weather$key == "Apr" |
              weather$key == "May" |
              weather$key == "Jun")
mean(weather[ind,]$val, na.rm = TRUE)

# 5) 7~12월 최저기온의 합을 출력하시오
ind = which(!(weather$key == "Jan" |
  weather$key == "Feb" |
  weather$key == "Mar" |
  weather$key == "Apr" |
  weather$key == "May" |
  weather$key == "Jun"))

sum(weather[ind,]$val, na.rm = TRUE)


# 2번문제
# 1) cat() 함수를 사용하여 10,000초를 몇시간 몇분 몇초인지 출력하시오

# library(lubridate)
# dtDate = lubridate::seconds_to_period(10000)
# cat(paste0(dtDate))

hour = 10000 / (60 * 60)
hourInt = as.integer(hour)
minute = (hour - hourInt) * 60
minuteInt = as.integer(minute)
sec = (minute - minuteInt) * 60
secInt = as.integer(sec)

cat(paste(hourInt, minuteInt, secInt))

# 3번문제
#  이름 : Kim Lee Park Choi Jung Kang Cho Yoon Jang Yim
#  점수 : 100  84  93    99  87    83    76  89    99  78
#
# 1) names() 함수를 사용하여 score_student 벡터를 생성하여 점수를 저장하고 원소 값의 이름을 지정하시오

score_student = c(100, 84, 93, 99, 87, 83, 76, 89, 99, 78)
names(score_student) = c("Kim", "Lee", "Park", "Choi", "Jung", "Kang", "Cho", "Yoon", "Jang", "Yim")

#
# 2) score_student 에서 95점 이상은 A+ , 90점 ~ 94점은 A- , 85점 ~ 89점은 B+, 80점 ~ 84점은 B-, 79점 이하는 C+ 로 변경하여 score_grade 벡터에 저장하시오

score_grade = c()
for (i in 1:length(score_student)) {

  score = score_student[i]

  if (score >= 95) {
    result = "A+"
  } else if (score >= 90) {
    result = "A-"
  } else if (score >= 85) {
    result = "B+"
  } else if (score >= 80) {
    result = "B-"
  } else {
    result = "C+"
  }

  score_grade = append(score_grade, result)
}

score_grade

#
# 3) score_grade 에서 홀수 번째에 저장된 값을 출력하시오

# 문제 4번
ques_3 <- 50:100

# 1)ques_3 에서 65보다 크고 85보다 작은 수를 출력하시오
ind = which(ques_3 > 65 & ques_3 < 85)
print(min(ques_3[ind], na.rm = TRUE))

# 2)ques_3 에서 9로 나누었을 때 나머지가 2인 수를 모두 출력하시오.
ind = which(ques_3 %% 9 == 2)
print(ques_3[ind])

# 3) ques_3 에서 3의 배수들의 값을 0으로 변경하여 출력하시오
ind = which(ques_3 %% 3 == 0)
print(ques_3[ind])

# 4) ques_3 에서 짝수를 모두 합한 값을 출력하시오.
ind = which(ques_3 %% 2 == 0)
print(sum(ques_3[ind], na.rm = TRUE))

# 5) ques_3 에서 2와 5의 공배수를 모두 출력하시오.
ind = which(ques_3 %% 2 == 0 & ques_3 %% 5 == 0)
print(ques_3[ind])


# 5번문제
#      T S
#  [1,] 35 23
#  [2,] 41 21
#  [3,] 44 24
#  [4,] 32 22
#  [5,] 50 25
# 1) 위와 같은 내용의 매트릭스(2행 5열) age를 생성하시오

data = data.frame(
  "T" = c(35, 41, 44, 32, 50)
  , "S" = c(23, 21, 24, 22, 25)
)

age = as.matrix(data)


# 2)  age의 열 이름을 각각 Teacher, Student로  행의 이름은 위에서 부터 Class1, Class2, Class3, Class4, Class5로 변경하시오.
rownames(age) = c("Class1", "Class2", "Class3", "Class4", "Class5")
colnames(age) = c("Teacher", "Student")

age


# 6번 문제
# 헬리코박터균이 1시간 마다 4배씩 증가한다고 가정한다.
# 헬리코박터균의 증식을 계산하는 함수를 작성하고, 함수를 호출하여 답을 출력하라
#
#  헬리코박터균 5마리를 배양하면 5시간 후에 헬리코박터균의 수는 얼마인가?
getVal(5, 0)

getVal = function(val, hour) {

  result = val * (4^hour)

  return(result)
}


# 7번 문제
# 구구단 2단 부터 9단 까지 아래와 같이 출력하는 프로그램을 작성하시오

for (i in 2:9) {
  for (j in 1:9) {
    cat(i, " * ", j, " = ", i * j, "\n")
  }
}

# 8번 문제 피라미드 별 모양을 출력하시오
# Upper triangle

max = 10
space <- max - 1
for (i in 0:(max - 1)) {
  for (j in 0:space) cat(" ")
  for (j in 0:i) cat("* ")
  cat("\n")
  space <- space - 1
}


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(dplyr)
library(MASS)
library(xlsx)
library(data.table)
library(ggplot2)
library(psych)
library(stargazer)
library(QuantPsyc)
library(BBmisc)
library(lm.beta)

data = read.xlsx("INPUT/o2job/survey.xlsx", sheetName = "Sheet3", encoding = "UTF-8")

dataL1 = data %>%
  dplyr::select(도시규모, 성별, 연령, 반공세대, 신세대, 학력, 소득, 자신대북정책, 영남거주, 호남거주)
# dplyr::select(도시규모, 성별, 출생연도, 반공세대, 신세대, 학력, 소득, 자신대북정책)

dataL2 = na.omit(dataL1)

summary(dataL2)

# NA 행 찾기
ind = which(is.na(dataL1), arr.ind = TRUE)
naRowInd = sort(unique(ind[, 1]))
dataL1[naRowInd,]

# 도시규모 + 성별 + 연령 + 출생연도 + 반공세대 + 신세대 + 학력 + 소득
lmFit = lm(자신대북정책 ~ ., data = dataL2)
summary(lmFit)

lmBetaFit = lm.beta::lm.beta(lmFit)

# Beta 회귀계수
round(lmBetaFit$standardized.coefficients, 3)

lm.beta::summary.lm.beta(lmBetaFit, standardized = TRUE)

# 표준화
# dataL3 = BBmisc::normalize(dataL2, method = "range", range = c(0, 1))
#
# lmFitNor = lm(자신대북정책 ~ ., data = dataL3)
#
# summary(lmFitNor)


# 변수 간 공간 없이 출력창에 결과 출력
# stargazer(dataL2, type="text", title="이념 회귀분석", no.space = TRUE)

# 변수 간 공간 넣어 출력창에 결과 출력
# stargazer(dataL1, type="text", title="이념 회귀분석", no.space = FALSE)

# ideology 란 이름의 html 파일로 저장
# stargazer(dataL1, type = "html", title = "이념 회귀분석", out = "ideology.html")

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggplot2)

# 1) 각각의 파일을 다운로드한 뒤 고유번호 컬럼을 연도로 바꾼다
seoul = read.csv("INPUT/o2job/서울시 연평균기온 2005-2008년 위치정보.csv")

dplyr::tbl_df(seoul)

# 2) 각 관측소별로 2005~2008의 평균 기온을 구한다
tmp1 = seoul %>%
  dplyr::group_by(o.name) %>%
  dplyr::summarise(meanVal = mean(temp, na.rm = TRUE))

dplyr::tbl_df(tmp1)

# 3) 관측소별 평균기온을 막대그래프로 표시한다
barplot(tmp1$meanVal, names.arg = tmp1$o.name, col = "blue", main = "서울지역 연평균 기온")

# 4) 평균 기온이 높은 상위 3개 관측소와 하위3개 관측소의 이름, 평균기온을 보이시오
seoul %>%
  dplyr::group_by(o.name) %>%
  dplyr::summarise(meanVal = mean(temp, na.rm = TRUE)) %>%
  dplyr::mutate(
    rank = rank(desc(meanVal))
    , rankDesc = rank(meanVal)
  ) %>%
  dplyr::filter(rankDesc <= 3 | rank <= 3) %>%
  dplyr::arrange(rank) %>%
  dplyr::seect(o.name, meanVal, rank)

# 5) 4년간 서울지역 연평균 기온의 추이를 다음과 같이 보이시오
tmp2 = seoul %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(meanVal = mean(temp, na.rm = TRUE))

barplot(tmp2$meanVal, names.arg = tmp2$year, col = "red", main = "연평균 기온 변화")


# 6) 관측소별 위치를 지도에 표시한다
dataL1 = seoul %>%
  dplyr::group_by(long, lat, o.name) %>%
  dplyr::summarise(meanVal = mean(temp, na.rm = TRUE)) %>%
  dplyr::mutate(meanSize = (meanVal - 11)^3)

gc = data.frame(
  lon = dataL1$long
  , lat = dataL1$lat
)

cenVal = c(mean(dataL1$lon, na.rm = TRUE), mean(dataL1$lat, na.rm = TRUE))
mapMarker = get_googlemap(center = cenVal, maptype = "roadmap", zoom = 11, markers = gc)

ggmap(mapMarker)

# 7) 관측소별 평균기온및 관측소명을 지도에 표시한다
map = get_googlemap(center = cenVal, maptype = "roadmap", zoom = 11)

ggmap(map) +
  geom_point(data = dataL1, aes(x = long, y = lat, size = meanSize), alpha = 0.5, colour = c("red")) +
  geom_text(data = dataL1, aes(x = long, y = lat), size = 5, label = dataL1$o.name)

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(ggplot2)
library(tidyverse)
library(dplyr)
library(spatstat)

# 만기에서의 콜옵션 소유자의 수입 Y를 기준자산, 여기서는 주식 S_T의 가격의 그래프로 나타내라.  S_T의 범위는 0부터 200까지이며 K=100으로 고정한다.

# K값
K = 100.0

# S_T 주식의 가격
s_t = seq(0, 200, 1)


# 만기의 수입 = max⁡(S_T-K,0)계산
Y = c()
S_T = c()

for (s in s_t) {
  Y_part = max(s - K, 0)
  Y = append(Y, Y_part) # Y 축 (수입) 계산
  S_T = append(S_T, s) # X 축 생성 (주식의 가격 모의)
}

plot(S_T, Y)


# 노벨상을 수상한 블랙-숄즈-머턴은 이와 같은 콜옵션의 프리미엄 C 가 특정가정들을 만족한 경우 다음과 같은 식에 의해서 결정되어야 한다고 증명하였다.

fnBSM = function(S = S, K = K, T1 = T1, sigma = sigma, rf = rf) {

  d1 <- (log(S / K) + ((rf + (0.5 * (sigma**2))) * T1)) / (sigma * sqrt(T1))
  d2 <- d1 - (sigma * sqrt(T1))

  C = (S * pnorm(d1)) - ((K * exp(-rf * T1)) * pnorm(d2))

  return(C)
}

S = 100.0
K = 100.0
T1 = 0.5
sigma = 0.1
rf = 0.04

result = fnBSM(S = S, K = K, T1 = T1, sigma = sigma, rf = rf)

round(result, 2)

# 재무 이론의 발전은 블랙숄즈머턴의 방법론을 이용하여 처음 다섯개의 주가 경로에 대해 오늘부터 만기까지(0->T) 꺽은선 형태의 그래프로 표기한다.

N = 10000
dt = 1.0 / 1000.0
S = 100.0
K = 100.0
T1 = 0.5
sigma = 0.1
rf = 0.04

data = data.frame()

# 만기시 옵션의 가치 및 최종옵션가치 초기값 지정
Vi = c()
Ci = c()

for (i in 1:N) {

  # 초기 가격
  S_index = c(S)

  # 이전 가격
  S_c = S_index[length(S_index)]

  for (t in seq(dt, T1, by = dt)) {
    S_part = S_c * exp((rf - (sigma**2 / 2.0)) * dt + (sigma * sqrt(dt) * rnorm(1, 0, 1)))
    S_index = append(S_index, S_part)
  }

  if (i <= 5) {
    result_part = data.frame(s_index = S_index, del = seq(0, T1, by = dt), N = i)
    data = rbind(data, result_part)
  }

  # 만기 시점의 옵션의 가치 계산
  Vi = append(Vi, max(S_index - K, 0))

  # 최종옵션 가치 산출을 위한 계산 수행
  Ci = append(Ci, max(S_index - K, 0) / N)
}

C = exp(-rf * T1) * sum(Ci)

# 최종 옵션의 가치 (C)
round(C, 2)

ggplot(data, aes(x = del, y = s_index, colour = N)) +
  geom_line()


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(RSelenium)
library(rvest)
library(stringr)
library(tidyverse)
library(data.table)
library(foreach)
library(httr)
library(webdriver)
library(seleniumPipes)
library(readxl)
library(foreach)
library(ggwordcloud)
library(wordcloud2)
library(htmlwidgets)
library(webshot)
library(xlsx)
library(log4r)
library(readxl)
library(tcltk)
library(beepr)

Sys.setlocale("LC_ALL")
options(encoding = "UTF-8")
Sys.setenv(LANG = "en_US.UTF-8")


log = log4r::create.logger()
log4r::logfile(log) = paste0("OUTPUT/o2job/log4r_", format(Sys.time(), "%Y%m%d"), ".log")
log4r::level(log) = "INFO"

data = readxl::read_excel("INPUT/o2job/웹사이트_정보.xlsx", sheet = "입력자료")

setWindowTab = function(remDr, windowId) {
  qpath = sprintf("%s/session/%s/window", remDr$serverURL, remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}

getXpathText = function(xpath) {
  remDr$getPageSource()[[1]] %>%
    read_html() %>%
    rvest::html_nodes(xpath = xpath) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

getCssText = function(css) {
  remDr$getPageSource()[[1]] %>%
    read_html() %>%
    rvest::html_nodes(css = css) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

remDr = remoteDriver(
  remoteServerAddr = "localhost"
  , port = 5001L
  , browserName = "chrome"
)

# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 5000

# 크롬 열기
remDr$open()

getRootHandle = remDr$getWindowHandles()

dataL1 = data %>%
  dplyr::filter(
    !is.na(targetTag)
    , isYn == "y"
  )

while (TRUE) {
  foreach::foreach(i = 1:nrow(dataL1), .combine = c) %do% {

    Sys.setlocale("LC_ALL")
    options(encoding = "UTF-8")
    Sys.setenv(LANG = "en_US.UTF-8")

    tryCatch(
      expr = {
        getRowData = dataL1[i,]

        # Url 정보 가져오기
        getUrl = getRowData$url
        getName = getRowData$name
        getTag = getRowData$targetTag

        remDr$navigate(getUrl)

        # 키워드 가져오기
        getText = getCssText(getTag)

        getTextReplace = stringr::str_replace_all(getText, " ", "")

        # 품절 키워드 여부
        isKeywordList = stringr::str_detect(getTextReplace, "|품절|입고|재입고|종료|불가능|OUT")

        isKeyword = FALSE

        if (length(isKeywordList) < 1) { isKeyword = TRUE }

        for (j in 1:length(isKeywordList)) {
          if (isKeywordList[j] == TRUE) { isKeyword = TRUE }
        }

        # 테스트용
        # if (i == 15)  { isKeyword = FALSE }

        log4r::info(log, paste0("[", i, "] ", getName, " | ", "품절 키워드 여부  : ", isKeyword, " | ", "getText : ", getText))

        # 품절 키워드가 없는 경우
        if (isKeyword == FALSE) {

          remDr$executeScript('alert("구매해주세요.");')

          # 마리오 알람 소리
          beepr::beep(sound = 8)

          break
        }
      }
      , warning = function(warning) { log4r::warn(log, warning) }
      , error = function(error) { log4r::error(log, error) }
      , finally = {
        getText = NULL
        getHandle = remDr$getWindowHandles()

        if (length(getHandle) > 1) {
          for (k in 1:length(getHandle)) {
            if (getRootHandle != getHandle[[k]]) {
              setWindowTab(remDr, getHandle[[k]])
              remDr$closeWindow()
            }
          }

          setWindowTab(remDr, getRootHandle[[1]])
        }
      }
    )
  }
}


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(faraway)
library(tidyverse)
library(grid)
library(readxl)
library(ggmap)

# 그림2] worldcup 데이터셋을 가지고 [그림 2]와 같이 worldcup_summary 데이터프레임을 추출하였다.
# 아래 내용을 잘 관찰하고 이와 동일한 결과물을 출력할 수 있도록 R 프로그램을 작성하시오.

data(worldcup)

worldcup_summary = worldcup %>%
  dplyr::group_by(Team) %>%
  dplyr::summarise(
    Mean_Time = mean(Time, na.rm = TRUE)
    , Mean_Shots = mean(Shots, na.rm = TRUE)
    , Mean_Passes = mean(Passes, na.rm = TRUE)
    , Mean_Tackles = mean(Tackles, na.rm = TRUE)
    , Mean_Saves = mean(Saves, na.rm = TRUE)
    , n_Player = n()
  ) %>%
  dplyr::arrange(desc(Mean_Time))

dplyr::tbl_df(worldcup_summary)

# worldcup 데이터셋을 사용하여 [그림 4]와 같이 동일하게 표출되는 R 프로그램을 작성하시오.

ind = which(worldcup$Shots == max(worldcup$Shots, na.rm = TRUE))
maxDf = worldcup[ind,]

getName = paste0(rownames(maxDf), " ( ", maxDf$Team, ", ", maxDf$Position, " )")

ggplot(worldcup, aes(x = Time, y = Shots)) +
  geom_point() +
  geom_segment(data = d, mapping = aes(x = 400, y = 25, xend = maxDf$Time, yend = maxDf$Shots), arrow = arrow(), size = 1, color = "red") +
  annotate("text", x = 300, y = 25, label = getName, color = "red", fontface = 2)


# 3.과제 5
# [그림 5]와 같이 <에어코리아> 사이트(https://www.airkorea.or.kr )에 접속하여 서울지역의 측정소 정보를 조회해 보고 해당 문제에 대해 R 프로그래밍을 작성하시오.
#
# 1.아래와 같은 서울지역의 측정소 정보를 가지는 데이터셋을 생성하시오. (5점)
# 측정소명 (ex. 강남구)
# 주소 (ex. 서울 강남구 학동로 426 강남구청 별관 1동)
dataQ5 = readxl::read_excel("INPUT/o2job/Q5.xls", sheet = "Sheet1")

dataQ5L1 = data %>%
  dplyr::select(측정소명, 측정소주소)

dplyr::tbl_df(dataQ5L1)

# 2.위 1번에서 생성한 서울지역의 측정소 주소의 경도 및 위도를 구하고 경도 및 위도 필드를 데이터셋에 추가하시오.

dataQ5L2 = ggmap::mutate_geocode(dataL1, 측정소주소, source = "google")

dplyr::tbl_df(dataQ5L2)

# 3.위 2번에서 생성한 데이터셋을 활용하여 각 측정소에 해당하는 주소를 구글 지도에 모두 마킹하시오.  (단, 각 마커에 각 측정소 이름이 표시될 수 있도록 함)


markerDf = data.frame(
  lon = dataQ5L2$lon
  , lat = dataQ5L2$lat
)

cenVal = c(mean(dataQ5L2$lon, na.rm = TRUE), mean(dataQ5L2$lat, na.rm = TRUE))
mapMarker = get_googlemap(center = cenVal, maptype = "roadmap", zoom = 11, markers = markerDf)

ggmap(mapMarker) +
  geom_text(data = dataQ5L2, aes(x = lon, y = lat), size = 5, label = dataL2$측정소명)

# 4.과제 6
# <에어코리아> 사이트(https://www.airkorea.or.kr)에 접속하여 2020년 4월 일평균 서울지역의 초미세먼지 정보를 조회해 보고 해당 문제에 대해 R 프로그래밍을 작성하시오.

# 1.서울지역 일평균 초미세먼지 정보를 가지는 데이터셋을 수집 및 생성하시오.
dataQ6 = readxl::read_excel("INPUT/o2job/Q6.xls", sheet = "Sheet1")

dplyr::tbl_df(dataQ6)

# 2.위 1번에서 생성한 2020년 4월 서울지역의 일평균 초미세먼지 정보와 시군구 데이터 프레임을 병합한 통합 데이터를 생성하시오.
#

endInd = stringr::str_locate(dataQ6$측정소명, "\\]") - 1
dataQ6$name = stringr::str_sub(dataQ6$측정소명, 2, endInd[, 1])

dataQ6L2 = dataQ6 %>%
  tidyr::gather(-측정망, -측정소명, -name, key = "key", value = "val") %>%
  dplyr::ungroup()

dplyr::tbl_df(dataQ6L2)


# 3.위 2번에서 생성한 통합 데이터을 활용하여 서울시 각 구마다 초미세먼지 현황을 단계 구분도 지도로 표출하시오.
#
dd = data.frame(name = dataQ6L2$name, val = dataQ6L2$val)

dataQ6L2$name = as.factor(dataQ6L2$name)

dataQ6L3 = dataQ6L2 %>%
  dplyr::group_by(name) %>%
  dplyr::mutate(
    meanVal = mean(val, na.rm = TRUE)
  )


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(ggplot2)
library(graphics)
library(broom)

# 1. ggplot2에 diamonds에서 임의로 선택한 500개의 행만 골라서 데이터 프레임을 만들고 그 데이터 프레임의 처음 10개의 행의 5열과 7열의 자료 추출

n = 500
ind = sample(1:nrow(diamonds), n)

dimData = diamonds[ind,]
dimDataL1 = dimData[1:10, 1:7]

dplyr::tbl_df(dimDataL1)


# 2. R에 내장된 UCBAmission에서 Dept별로 남자의 합격률과 여자의 합격률을 구하는 문제
ucbData = UCBAdmissions %>%
  broom::tidy() %>%
  group_by(Gender, Dept) %>%
  mutate(prop = n / sum(n)) %>%
  filter(Admit == "Admitted")

dplyr::tbl_df(ucbData)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(scales)
library(tidyverse)
library(ranger)
library(ggpubr)

car.df = read.csv("INPUT/o2job/ToyotaCorolla.csv")

car.df <- car.df[1:1000,] # select variables for regression
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18) # partition data
set.seed(1) # set seed for reproducing the partition
train.index <- sample(c(1:1000), 600)

train.df <- car.df[train.index, selected.var]

valid.df <- car.df[-train.index, selected.var] # use lm() to run a linear regression of Price on all 11 predictors in the # training set. # use . after ~ to include all the remaining columns in train.df as predictors.
car.lm <- lm(Price ~ ., data = train.df)
# use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)

summary(car.lm)

#======================================================
# 정규분포 + 밀도 함수
#======================================================
car.df %>%
  ggplot(aes(x = Price)) +
  geom_histogram(aes(y = ..density..), binwidth = 1000) +
  stat_function(fun = dnorm, args = list(mean = mean(car.df$Price, na.rm = TRUE), sd = sd(car.df$Price, na.rm = TRUE)), lwd = 2, col = 'red') +
  labs(title = "도요타 중고차 가격 분포", x = "중고차 가격", y = "확률밀도(density)", subtitle = "단위: 유로") +
  scale_x_continuous(labels = scales::comma) +
  ggsave(filename = "FIG/o2job/Img_001.png", dpi = 600)

#======================================================
# 빈도 분포
#======================================================
car.df %>%
  ggplot(aes(x = Price)) +
  geom_histogram(aes(y = ..count..)) +
  labs(title = "도요타 중고차 가격 분포", x = "중고차 가격", y = "빈도분포", subtitle = "단위: 유로") +
  scale_x_continuous(labels = scales::comma) +
  ggsave(filename = "FIG/o2job/Img_002.png", dpi = 600)

#========================================================
# Random Forest
#========================================================
tuc_rf = ranger::ranger(Price ~ ., train.df, num.trees = 500, respect.unordered.factors = "order")

valid.df$pred_rf = predict(tuc_rf, valid.df)$predictions


validData = data.frame(
  xAxis = valid.df$pred_rf
  , yAxis = valid.df$Price
  , type = "RF"
)

plot(xAxis, yAxis)

corVal = cor(xAxis, yAxis)
biasVal = Metrics::bias(xAxis, yAxis)
rmseVal = Metrics::rmse(xAxis, yAxis)

ggscatter(validData, x = "xAxis", y = "yAxis", color = "black", add = "reg.line", conf.int = TRUE, add.params = list(color = "blue", fill = "lightblue")) +
  stat_regline_equation(label.x = 5000, label.y = 25000) +
  annotate("text", x = 5000, y = 23000, label = sprintf("R = %s", round(corVal, 2)), hjust = 0) +
  annotate("text", x = 5000, y = 21000, label = sprintf("Bias = %s", round(biasVal, 2)), hjust = 0) +
  annotate("text", x = 5000, y = 19000, label = sprintf("RMSE = %s", round(rmseVal, 2)), hjust = 0) +
  theme_bw() +
  labs(title = "랜덤 포레스트를 이용한 도요타 중고차 가격 예측", x = "중고차 가격", y = "예측 중고차 가격", subtitle = "단위: 유로") +
  coord_equal() +
  ggsave(filename = "FIG/o2job/Img_003.png", width = 6, height = 6, dpi = 600)

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================


######################################## 모집단 및 표본집단의 평균/분산/표준편차 비교 ##############################
##  - 중심극한정리 : 동일한 확률분포를 가진 독립확률 변수 n개의 평균값은 n이 클수록 정규분포에 가까워짐
##  - 표본집단을 통하여 모집단의 정보를 알고싶음
##  - 즉 표본평균집단의 평균/분산/표준편차를 이용하여 모집단의 평균/분산/표준편차를 유추할 수 있음
##    -- 모집단의 평균                  = 표본평균집단의 평균
##    -- 모집단의 분산/자료수           = 표본평균집단의 분산
##    -- 모집단의 표준편차/루트(자료수) = 표본평균집단의 표준편차
##  -> 표본 개수가 많을수록 모집단을 더 잘 유추할 수 있음  -> 정규분포에 가까워짐
####################################################################################################################

set.seed(1)                   # 난수로 생성된 수열을 고정시킴
roulette = c(800, 8000, 80000) # 원판 과녁
prob = c(4 / 8, 3 / 8, 1 / 8)

hist(X)  # 모집단의 빈도분포
cat(mean(X), var(X), sd(X), "\n")  # 모집단의 평균/분산/표준편차


################################################### 표본평균집단에 대해서 ###########################################################

for (i in c(10, 100, 1000, 10000)) {
  DO = 100000    # Number of repetition
  N = i          # Number of sample

  # FALSE : 비복원 추출(무작위 정렬) : 한번 뽑은 것을 다시 뽑을 수 없는 추출
  sampleList = lapply(1:DO, function(i) sample(roulette, N, replace = TRUE, prob = prob)) # 복원 추출
  meanSampleList = mapply(mean, sampleList)

  meanVal = mean(meanSampleList, na.rm = TRUE)
  sdVal = sd(meanSampleList, na.rm = TRUE)
  maxVal = max(meanSampleList, na.rm = TRUE)
  minVal = min(meanSampleList, na.rm = TRUE)

  cat(N, meanVal, "\n")

  # FIG
  png(file = paste0("FIG/o2job/Img_004_", N, "_.png"), 1000, 600)
  hist(meanSampleList, breaks = 50, xlab = "Sample Distribution Mean", xlim = c(minVal - sdVal, maxVal + sdVal), col = "light grey", border = "grey", xaxs = "i", yaxs = "i",
       main = paste0("Central Limit Theorem : number of sample = ", N, ", number of repetition = ", sprintf("%d", DO)))

  XX1 = meanVal + sdVal
  XX2 = meanVal - sdVal
  XX3 = meanVal + 2 * sdVal
  XX4 = meanVal - 2 * sdVal
  XX5 = meanVal + 3 * sdVal
  XX6 = meanVal - 3 * sdVal

  YY = max(hist(meanSampleList, breaks = 50, plot = F)$counts)
  lines(c(meanVal, meanVal), c(0, YY), lty = 1, col = 4); text(meanVal, YY / 2, "Mean")
  lines(c(XX1, XX1), c(0, YY), lty = 1, col = 2); text(XX1, YY / 2, "+1σ")
  lines(c(XX2, XX2), c(0, YY), lty = 1, col = 2); text(XX2, YY / 2, "-1σ")
  lines(c(XX3, XX3), c(0, YY), lty = 1, col = 2); text(XX3, YY / 2, "+2σ")
  lines(c(XX4, XX4), c(0, YY), lty = 1, col = 2); text(XX4, YY / 2, "-2σ")
  lines(c(XX5, XX5), c(0, YY), lty = 1, col = 2); text(XX5, YY / 2, "+3σ")
  lines(c(XX6, XX6), c(0, YY), lty = 1, col = 2); text(XX6, YY / 2, "-3σ")
  lines(c(maxVal, maxVal), c(0, YY), lty = 1, col = 3); text(maxVal, YY / 2, "Max")
  lines(c(minVal, minVal), c(0, YY), lty = 1, col = 3); text(minVal, YY / 2, "Min")
  dev.off()
}

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(tidyverse)

tmpData1 = read.csv("INPUT/o2job/gene_annotation.csv")
tmpData2 = read.csv("INPUT/o2job/phylostratigraphy_nr.csv")

# Phylostratigraphy는 유전체 상에 존재하는 유전자들의 age를, phylogenetics와 통계적인 방법을 이용하여 추정하는 것을 의미합니다. 통계적 방법론을 제외하더라도, phylogenetics를 통해서 유전자들이 생겨난 시기를 종 분화와 관련하여 추정하는 것이 가능합니다.
#
# 첨부된 파일 중에서 'phylostratigraphy_nr.csv'가 그러한 분석의 결과를 정리한 테이블 입니다. 첨부된 그림과 이 데이터를 통해서 벼 도열병균들의 1만2천8백여개의 유전자들의 진화 과정 동안에 어느 시점에서 생겨났는지를 추정한 것입니다.
#
# Strata는 층위를 뜻하는 용어로, 각 유전자들이 생겨난 시기를 지칭하는 것으로 이해하여도 무방합니다. 따라서 Strata가 1이라는 것은 모든 세포들에 공통적인 유전자라는 뜻이고, 반대로 7이면 벼도열병균에만 존재하는 유전자라고 이해하시면 됩니다. 2-6까지는 서로 다른 시기에 등장한 유전자들이 되겠습니다.
#
# 위에서 언급된 파일들 외에, 유전자들의 서열을 저장한 'gene.fasta' , 'gene_annotation.csv' 파일들을 이용하여, 아래에서 지시하는 plot들을 그리고, 그 결과를 나름대로 해석을 해 주시면 됩니다.


# 1. 벼도열병균의 chromosome (총 7개의 chromosome 및 어셈블리가 되지 않은 1개의 조각 (8번이라고 되어 있음))들에 대해서 각 chromosome별로 어떤 strata의 유전자들이 얼마씩 있는지를 나타내는 plot을 작성.

data = tmpData1 %>%
  dplyr::left_join(tmpData2, by = c("Locus" = "Locus"))

dataL1 = data %>%
  dplyr::filter(Strata %in% c(7, 8)) %>%
  dplyr::group_by(Supercontig, Strata) %>%
  dplyr::summarise(number = n())

summary(dataL1)

ggplot(dataL1, aes(x = as.factor(Supercontig), y = number, fill = as.factor(Supercontig))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = number), vjust = 1.6, color = "white", size = 3.5) +
  labs(x = "Supercontig", fill = "Supercontig") +
  ggsave(filename = "FIG/o2job/Img_005.png", width = 10, height = 6, dpi = 600)

# 2. 각 strata에 속하는 유전자들의 길이 분포(y축은 log scale로 설정)를 나타내는 boxplot을 작성하고 그 결과에 대한 해석
ggplot(data, aes(x = Strata, y = log(Length), colour = as.factor(Strata))) +
  geom_boxplot() +
  labs(colour = "Strata") +
  ggsave(filename = "FIG/o2job/Img_006.png", width = 10, height = 6, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(tidyverse)
library(ggplot2)
library(lubridate)
library(readr)
library(data.table)
library(stringr)

data = data.table::fread("INPUT/o2job/서울교통공사_1_8호선일별역별시간대별승하차인원_2015년.csv", header = TRUE)

dplyr::tbl_df(data)


dataL1 = data %>%
  dplyr::rename("00~" = "24~") %>%
  dplyr::mutate(
    역이름 = stringr::str_replace_all(역명, stringr::str_c("\\(", 역번호, "\\)"), "")
  ) %>%
  tidyr::gather(-날짜, -역번호, -역명, -구분, -역이름, key = "key", value = "val") %>%
  dplyr::mutate(
    sYmdH = stringr::str_c(날짜, " ", stringr::str_sub(key, 1, 2))
    , dtYmdH = readr::parse_datetime(sYmdH, "%Y-%m-%d %H")
    , dtYear = lubridate::year(dtYmdH)
    , dtMonth = lubridate::month(dtYmdH)
    , dtHour = lubridate::hour(dtYmdH)
    , nVal = stringr::str_replace_all(val, "\\,", "")
  ) %>%
  dplyr::mutate_at(vars(nVal), funs(as.numeric))

dplyr::tbl_df(dataL1)

dataL2 = dataL1 %>%
  dplyr::group_by(역이름, 구분, dtYear, dtMonth, dtHour) %>%
  dplyr::summarise(
    sumVal = sum(nVal, na.rm = TRUE)
  )

dplyr::tbl_df(dataL2)

# 1. 1년치 데이터 중 월별 시간대별 승하차 인원을 파악한다.(ex )역이름 : 청담)
# ex): 15년 1월 청담 승차인원을 시간대별,
# 15년 1월 청담 하차인원을 시간대별,
# 15년 12월 그래프 까지

# 0시간을 24시간 변경
ind = which(dataL2$dtHour == 0)
dataL2[ind,]$dtHour = 24

dtYearList = unique(dataL2$dtYear)
dtMonthList = unique(dataL2$dtMonth)
dtTypeList = unique(dataL2$구분)
dtStationList = unique(dataL2$역이름)

i = 1
j = 1
k = 1
l = 1

# for (i in 1:length(dtYearList)) {
# for (j in 1:length(dtMonthList)) {
# for (k in 1:length(dtTypeList)) {
# for (l in 1:length(dtStationList)) {

titleLabel = paste0("[", dtYearList[i], "-", dtMonthList[j], "] ", dtTypeList[k], " : ", dtStationList[l])
saveName = paste0("FIG/o2job/TMP/Img_", dtYearList[i], "_", dtMonthList[j], "_", dtTypeList[k], "_", dtStationList[l], ".png")

# cat(titleLabel, "\n")

dataL2 %>%
  dplyr::filter(
    dtYear == dtYearList[i]
    , dtMonth == dtMonthList[j]
    , 구분 == dtTypeList[k]
    , 역이름 == dtStationList[l]
  ) %>%
  ggplot(aes(x = as.factor(dtHour), y = sumVal, fill = as.factor(dtHour))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sumVal), vjust = 1.6, color = "white", size = 3.5) +
  labs(
    x = "시간"
    , y = "승하차 인원 [명]"
    , fill = "시간"
    , title = titleLabel
  ) +
  ggsave(filename = saveName, width = 12, height = 8, dpi = 600)
# }
# }
# }
# }

# 2. R마크다운 보고서를 작성한다.
#
# 사용자에 따라 특정 연도, 월, 구분, 역이름에 따른 구현 기능
dataL3 = dataL2 %>%
  dplyr::filter(
    dtYear == 2015
    , dtMonth == 1
    , 구분 == "승차"
    , 역이름 == "가락시장"
  )

titleLabel = paste0("[", dataL3$dtYear[1], "-", dataL3$dtMonth[1], "] ", dataL3$구분[1], " : ", dataL3$역이름[1])
saveName = paste0("FIG/o2job/TMP/Img_", dataL3$dtYear[1], "_", ddataL3$dtMonth[1], "_", dataL3$구분[1], "_", dataL3$역이름[1], ".png")

ggplot(dataL3, aes(x = as.factor(dtHour), y = sumVal, fill = as.factor(dtHour))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sumVal), vjust = 1.6, color = "white", size = 3.5) +
  labs(
    x = "시간"
    , y = "승하차 인원 [명]"
    , fill = "시간"
    , title = titleLabel
  ) +
  ggsave(filename = saveName, width = 12, height = 8, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================
library(RSelenium)
library(rvest)
library(stringr)
library(tidyverse)
library(data.table)
library(foreach)
library(httr)
library(webdriver)
library(seleniumPipes)
library(readxl)
library(foreach)
library(ggwordcloud)
library(wordcloud2)
library(htmlwidgets)
library(webshot)
library(xlsx)
library(log4r)
library(readxl)
library(tcltk)
library(beepr)
#install.packages("noncompliance")
library(noncompliance)

Sys.setlocale("LC_ALL")
options(encoding = "UTF-8")
Sys.setenv(LANG = "en_US.UTF-8")


log = log4r::create.logger()
log4r::logfile(log) = paste0("OUTPUT/o2job/log4r_", format(Sys.time(), "%Y%m%d"), ".log")
log4r::level(log) = "INFO"

# data = readxl::read_excel("INPUT/o2job/웹사이트_정보.xlsx", sheet = "입력자료")

setWindowTab = function(remDr, windowId) {
  qpath = sprintf("%s/session/%s/window", remDr$serverURL, remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}

getXpathText = function(xpath) {
  remDr$getPageSource()[[1]] %>%
    read_html() %>%
    rvest::html_nodes(xpath = xpath) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

getCssText = function(css) {
  remDr$getPageSource()[[1]] %>%
    read_html() %>%
    rvest::html_nodes(css = css) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

remDr = remoteDriver(
  remoteServerAddr = "localhost"
  , port = 5000L
  , browserName = "chrome"
)

# cd selenium
# cd selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 5000

# 크롬 열기
remDr$open()

getRootHandle = remDr$getWindowHandles()

searchKeyword = "a"
searchKeyword = "atmosphere"

getUrl = sprintf("https://en.dict.naver.com/#/search?range=word&page=1&query=%s&shouldSearchOpen=false", searchKeyword)

remDr$navigate(getUrl)

number = as.numeric(stringr::str_replace_all(getXpathText('//*[@id="searchPage_entry"]/h3/span[2]'), ",", ""))
if (length(number) == 0) number = 0


if (number > 0) {
  data = data.frame()

  for (i in 1:number) {

    if (i > 15) break

    # i = 9
    # i = 1

    contextUrl = sprintf('//*[@id="searchPage_entry"]/div[1]/div[%s]', i)

    word = getXpathText(paste0(contextUrl, '/div[*]/a'))

    typeList = getCssText(".row .word_class")
    infoList = getXpathText(paste0(contextUrl, '/ul/li[*]/p'))

    # j = 1
    for (j in 1:length(infoList)) {

      isFlag = FALSE

      for (k in 1:length(typeList)) {

        if (stringr::str_detect(infoList[j], typeList[k]) == TRUE) {
          isFlag = TRUE

          break
        }
      }

      if (isFlag) {
        infoSplit = stringr::str_split_fixed(infoList[j], " ", n = 2)

        type = infoSplit[, 1]
        if (length(type) == 0) type = ""

        intrp = infoSplit[, 2]
      } else {
        intrp = infoList[j]
        type = ""
      }

      tmpData = data.frame(word, type, intrp)
      data = dplyr::bind_rows(data, tmpData)
    }


  }
}


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(tidyverse)
library(readxl)
library(readr)
library(lubridate)
library(scales)
library(ggpubr)

data = readxl::read_excel("INPUT/o2job/6월통계.xlsx", sheet = "Sheet4")

dataL1 = data %>%
  dplyr::filter(val > 0, date < 7)


dataL1 %>%
  dplyr::filter(type == "case01") %>%
  ggscatter(x = "date", y = "val", color = "label", add = "reg.line", fullrange = TRUE, conf.int = FALSE) +
  # stat_regline_equation(aes(color = label), label.x = 1) +
  # stat_cor(aes(color = label), label.x = 4) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  scale_y_continuous(breaks = seq(0, 100000, 10000)) +
  labs(title = "서비스 이용가구", x = "해당 월", y = "총 건수", subtitle = "단위: 건수") +
  ggsave(filename = "FIG/o2job/Img_Case01.png", width = 8, height = 6, dpi = 600)


dataL1 %>%
  dplyr::filter(type == "case03") %>%
  ggscatter(x = "date", y = "val", color = "label", add = "reg.line", fullrange = TRUE, conf.int = FALSE) +
  # stat_regline_equation(aes(color = label), label.x = 1) +
  # stat_cor(aes(color = label), label.x = 4) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  labs(title = "월 평균 이용시간(가구별)", x = "해당 월", y = "건수", subtitle = "단위: 시간") +
  ggsave(filename = "FIG/o2job/Img_Case03.png", width = 8, height = 6, dpi = 600)

dataL1 %>%
  dplyr::filter(type == "case04") %>%
  ggscatter(x = "date", y = "val", color = "label", add = "reg.line", fullrange = TRUE, conf.int = FALSE) +
  # stat_regline_equation(aes(color = label), label.x = 1) +
  # stat_cor(aes(color = label), label.x = 4) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  labs(title = "월 평균 이용시간(이용자별)", x = "해당 월", y = "건수", subtitle = "단위: 시간") +
  ggsave(filename = "FIG/o2job/Img_Case04.png", width = 8, height = 6, dpi = 600)


dataL1 %>%
  dplyr::filter(type == "case05") %>%
  ggscatter(x = "date", y = "val", color = "label", add = "reg.line", fullrange = TRUE, conf.int = FALSE) +
  # stat_regline_equation(aes(color = label), label.x = 1) +
  # stat_cor(aes(color = label), label.x = 4) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  scale_y_continuous(breaks = seq(18000, 24000, 1000)) +
  labs(title = "근로자 현황", x = "해당 월", y = "사람 수", subtitle = "단위: 명") +
  ggsave(filename = "FIG/o2job/Img_Case05.png", width = 8, height = 6, dpi = 600)


dataL1 %>%
  dplyr::filter(type == "case06") %>%
  ggscatter(x = "date", y = "val", color = "label", add = "reg.line", fullrange = TRUE, conf.int = FALSE) +
  # stat_regline_equation(aes(color = label), label.x = 1) +
  # stat_cor(aes(color = label), label.x = 4) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  scale_y_continuous(breaks = seq(0, 50000, 10000)) +
  labs(title = "중위소득", x = "해당 월", y = "소득", subtitle = "단위: 가구") +
  ggsave(filename = "FIG/o2job/Img_Case06.png", width = 8, height = 6, dpi = 600)


dataL1 %>%
  dplyr::filter(type == "case07") %>%
  ggscatter(x = "date", y = "val", color = "label", add = "reg.line", fullrange = TRUE, conf.int = FALSE) +
  # stat_regline_equation(aes(color = label), label.x = 1) +
  # stat_cor(aes(color = label), label.x = 4) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  # scale_y_continuous(breaks=seq(18000, 24000, 1000)) +
  labs(title = "이용가구(소득유형) / 실이용 누적", x = "해당 월", y = "소득", subtitle = "단위: 가구") +
  ggsave(filename = "FIG/o2job/Img_Case07.png", width = 8, height = 6, dpi = 600)


data2 = readxl::read_excel("INPUT/o2job/6월통계.xlsx", sheet = "Sheet2")

ggplot(data2, aes(x = key, y = val, fill = label)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "서비스 이용가구(소득유형별)_6월 기준", x = "", y = "중위소득", subtitle = "단위: 가구", fill = "구분") +
  ggsave(filename = "FIG/o2job/Img_Case02.png", width = 8, height = 6, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(igraph)

net <- graph_from_data_frame(d = links, vertices = nodes, directed = T)

degree(net, mode = "in")
centr_degree(net, mode = "in", normalized = T)


counts <- read.table("http://bowtie-bio.sourceforge.net/recount/countTables/wang_count_table.txt", row.names = 1, header = TRUE)
counts <- counts[rank(-rowMeans(counts)) <= 100,]
counts.log <- log10(counts + 1)
dim(counts.log)

cc <- cor(t(counts.log))

library(reshape2)
cc[upper.tri(cc)] <- NA
diag(cc) <- NA
cc.df <- melt(cc)
cc.df <- cc.df[!is.na(cc.df[, 3]),]

head(cc.df)

cc.df.sig <- cc.df[abs(cc.df[, 3]) > 0.75,]

g <- graph.data.frame(cc.df.sig[, 1:2], directed = F)


graph <- simplify(g)

V(graph)$indegree <- centr_degree(graph, mode = "in")$res


Nodelist <- data.frame(
  Names = c("Jim", "Carole", "Joe", "Michelle", "Jen", "Pete", "Paul", "Tim",
            "Jess", "Mark", "Jill", "Cam", "Kate"),
  YearsFarming = c(8.5, 6.5, 4, 1, 3, 10, 5, 5, 5, 1, 1, 6, 6),
  Age = c(22, 31, 25, 21, 22, 35, 42, 27, 26, 33, 26, 28, 22),
  Gender = c("Male", "Female", "Male", "Female", "Female", "Male", "Male", "Male", "Female", "Male", "Female", "Male", "Female"))

Nodelist

Edgelist <- data.frame(
  From = c("Jim", "Jim", "Jim", "Jill", "Kate", "Pete", "Pete", "Jess", "Jim", "Jim", "Pete"),
  To = c("Carole", "Jen", "Pete", "Carole", "Joe", "Carole", "Paul", "Mark", "Cam", "Mark", "Tim")
)

Edgelist


# 상록교목 >

FarmNetwork <- graph_from_data_frame(d = Edgelist, vertices = Nodelist, directed = T)
FarmNetwork

V(FarmNetwork)
colrs <- c("gray70", "blue")


V(FarmNetwork)$indegree <- centr_degree(FarmNetwork, mode = "in")$res
V(FarmNetwork)$color <- ifelse(V(FarmNetwork)$Gender == "Male", "orange", "dodgerblue") ## if male, make orange, if not, blue. Go gators!!!!
V(FarmNetwork)$size <- V(FarmNetwork)$YearsFarming * 2 ## scale by multiplying by 2
plot(FarmNetwork, edge.arrow.size = .5, vertex.label.dist = 2.5)


# data <- matrix(sample(0:1, 100, replace=TRUE, prob=c(0.8,0.2)), nc=10)
# network <- graph_from_adjacency_matrix(data , mode='undirected', diag=F )

plot.igraph(FarmNetwork, asp = 0,
            main = "Customizing Legends",
            sub = "Legend of the Cats!",
            vertex.label = NA,
            edge.color = "lightgray",
            edge.width = 0.5,
            edge.arrow.size = 0.5,
            edge.arrow.width = 2,
            edge.lty = "solid",
            edge.curved = 0.05)


legend(x = "topleft",
       legend = unique(V(FarmNetwork)$Gender),
       pch = 19,
       col = unique(V(FarmNetwork)$color),
       bty = "n",
       title = "Faculty Groups")


# legend_cats <- data.frame(attr = unique(vertex_attr(V(FarmNetwork), "Group")),
#                           color = unique(V(FarmNetwork)$color))
# legend_cats <- legend_cats[order(legend_cats$attr), c(1, 2)]
#
# legend(x = "bottomleft",
#        legend = legend_cats$attr,
#        pch = 19,
#        col = legend_cats$color,
#        bty = "n",
#        title = "Faculty Groups")


library(igraph)
plot(FarmNetwork)


num.of.v <- length(V(g))
V(g)$size <- rep(5, num.of.v)
V(g)$color <- rep("#E41A1C", num.of.v)
V(g)$shape <- rep("circle", num.of.v)
V(g)$label <- names(as.list(V(g)))
V(g)$label.cex <- rep(0.5, num.of.v)
V(g)$label.color <- rep("black", num.of.v)

plot(g)


plot(g, vertex.label = NA)


# A BA graph is quite centralized
g <- sample_pa(1000, m = 4)
centr_degree(g)$centralization
# centr_clo(g, mode = "all")$centralization
# centr_betw(g, directed = FALSE)$centralization
# centr_eigen(g, directed = FALSE)$centralization


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================
library(RSelenium)
library(rvest)
library(stringr)
library(tidyverse)
library(data.table)
library(foreach)
library(httr)
library(webdriver)
library(seleniumPipes)
library(readxl)
library(foreach)
library(ggwordcloud)
library(wordcloud2)
library(htmlwidgets)
library(webshot)
library(xlsx)
library(log4r)
library(readxl)
library(tcltk)
library(beepr)
#install.packages("noncompliance")
library(noncompliance)
library(tidyverse)
library(readr)
library(data.table)
library(RcppMeCab)
library(utf8)
library(RmecabKo)
# RmecabKo::install_mecab("c:/mecab")

Sys.setlocale("LC_ALL")
options(encoding = "UTF-8")
Sys.setenv(LANG = "en_US.UTF-8")

log = log4r::create.logger()
log4r::logfile(log) = paste0("OUTPUT/o2job/log4r_", format(Sys.time(), "%Y%m%d"), ".log")
log4r::level(log) = "INFO"

# data = readxl::read_excel("INPUT/o2job/웹사이트_정보.xlsx", sheet = "입력자료")

setWindowTab = function(remDr, windowId) {
  qpath = sprintf("%s/session/%s/window", remDr$serverURL, remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}

getXpathText = function(xpath) {
  remDr$getPageSource()[[1]] %>%
    read_html() %>%
    rvest::html_nodes(xpath = xpath) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

getCssText = function(css) {
  remDr$getPageSource()[[1]] %>%
    read_html() %>%
    rvest::html_nodes(css = css) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

remDr = remoteDriver(
  remoteServerAddr = "localhost"
  , port = 5000L
  , browserName = "chrome"
)

# cd selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 5000

# 크롬 열기
remDr$open()

getRootHandle = remDr$getWindowHandles()

# 설정 URL로 이동
remDr$navigate("https://play.google.com/store/apps/details?id=com.truefriend.neosmarta&showAllReviews=true")
remDr$navigate("https://play.google.com/store/apps/details?id=com.wooriwm.txsmart&showAllReviews=true")
remDr$navigate("https://play.google.com/store/apps/details?id=com.linkzen.app&showAllReviews=true")
remDr$navigate("https://play.google.com/store/apps/details?id=com.samsungpop.android.mpop&showAllReviews=true")
remDr$navigate("https://play.google.com/store/apps/details?id=com.miraeasset.trade&showAllReviews=true")

# css의 body를 element로 찾아 지정
webElem = remDr$findElement("css", "body")

while (TRUE) {
  webElem$sendKeysToElement(list(key = "end"))

  webElemButton = remDr$findElements(using = 'css selector', value = '.ZFr60d.CeoRYc')

  if (length(webElemButton) == 1) {
    # webElem$sendKeysToElement(list(key = "home"))
    webElemButton = remDr$findElements(using = 'css selector', value = '.ZFr60d.CeoRYc')
    remDr$mouseMoveToLocation(webElement = webElemButton[[1]])
    remDr$click()

    # webElemButton = remDr$findElements('xpath', '//*[@id="fcxH9b"]/div[4]/c-wiz/div/div[2]/div/div/main/div/div[1]/div[2]/div[2]/div/span/span')
    # remDr$mouseMoveToLocation(webElement = webElemButton[[1]])
    # remDr$click()

    webElem$sendKeysToElement(list(key = "end"))
  }
}


# 페이지 전체 소스 가져오기
frontPage = remDr$getPageSource()

# 페이지 전체 소스에서 리뷰 게시자 부분 추출하기
reviewNames = read_html(frontPage[[1]]) %>%
  html_nodes('.bAhLNe.kx8XBd') %>%
  html_nodes('.X43Kjb') %>%
  html_text()

# 페이지 전체 소스에서 리뷰 게시 일자 및 시간 부분 추출하기
reviewDates = read_html(frontPage[[1]]) %>%
  html_nodes('.bAhLNe.kx8XBd') %>%
  html_nodes('.p2TkOb') %>%
  html_text()

# 페이지 전체 소스에서 리뷰 내용 부분 추출하기
reviewComments = read_html(frontPage[[1]]) %>%
  html_nodes('.UD7Dzf') %>%
  html_text()

# 수집한 데이터 통합
reviewData = data.frame(
  name = reviewNames
  , date = reviewDates
  , comment = reviewComments
  , type = "키움증권영웅문S"
)

# 수집 리뷰 데이터 CSV 파일로 저장
write.csv(reviewData, paste0("OUTPUT/o2job/GooglePlayReview03.csv"))

remDr$close()

#==================================================
# 데이터 전처리
#==================================================

fileList = Sys.glob("OUTPUT/o2job/GooglePlayReview*.csv")

# fileList 한번에 읽기
data = fileList %>%
  purrr::map(read.csv) %>%
  purrr::reduce(dplyr::bind_rows)

data %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(n = n())

dplyr::tbl_df(data)

# comJoin = paste(data$comment[1:2], collapse = " ")
comJoin = paste(data$comment, collapse = " ")
stringr::str_length(comJoin)

dataL2 = data.frame()
i = 1
for (i in 1:length(data$comment)) {
  cat(i, "\n")

  dataL1 = RcppMeCab::pos(as_utf8(data$comment[i]), format = "data.frame") %>%
    dplyr::filter(pos == "NNG") %>%
    dplyr::select(token)

  dataL2 = dplyr::bind_rows(dataL2, dataL1)
}


dataL3 = dataL2 %>%
  dplyr::group_by(token) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame()

fig = wordcloud2(data = dataL3)

# html로 내보내기
saveWidget(fig, "fig.html", selfcontained = FALSE)

# 내보낸 html 페이지로부터 png 형태로 불러와서 저장
webshot::webshot("fig.html", "FIG/o2job/GooglePlay_Keyword.png", vwidth = 775, vheight = 550, delay = 10)

# 출력
xlsx::write.xlsx2(dataL3, file = "OUTPUT/o2job/GooglePlay_Keyword.xlsx", sheetName = "keyword", append = FALSE, row.names = FALSE, col.names = TRUE)

library(xlsx)

# [1단계] 엑셀 파일에서 1행 추가
# 사과 4000

# [2단계] 엑셀 파일 읽기
data = read.xlsx("OUTPUT/o2job/GooglePlay_Keyword.xlsx", sheetName = "keyword", encoding = "UTF-8")

head(data)
# token freq
# 1    사과  4000
# 2       앱 3375
# 3 업데이트 2749
# 4     사용 2284
# 5   로그인 1917
# 6     종목 1589

# [3단계]
fig = wordcloud2(data = data)

# [4단계] html로 내보내기
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# [5단계] html 페이지로부터 png 형태로 불러와서 저장
webshot::webshot("fig.html", "FIG/o2job/GooglePlay_Keyword.png", vwidth = 775, vheight = 550, delay = 10)

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

# 안녕하세요. 인사데이터로 퇴직과의 관계성을 보려 합니다.
#
# 퇴직 -> 0 (퇴직), 1(재직)
# 나이 -> 23~64
# 교육 -> 1(고등학교) 2(전문대) 3(4년제) 4(대학원)
# 교육국내외 -> 1(국내대학교) 2(해외대학교)
# 성별 -> 1(남자) 2(여자)
# 일 레벨 -> 2~11
# 직급 -> 사원(1) 대리(2) 과장(3) 4(차장) 5(부장) 6(이사) 7(상무) 8(전무) 9(부사장)
# 회사에서 일한 기간 -> 0~35.07
# 마지막 승진으로 부터 지난 기간-> 0~14.1
#
# 이런식으로 퇴직에 영향을 미치는 요인들과의 상관관계를 보려고 합니다. 처음에는 바로 powerbi 에서 corelation 을 보고 그에따라 clustering 을 하려 했는데 이 요인들로 바로 corelation 을 찾는건 불가능 한듯 하더군요 그래서 찾아본 결과 logistics regression을 돌려야 한다는데, 퇴직(0,1)에 영향을 끼치는 요인들을 찾아내고 이 요인들이 얼마나 큰 관계가 있는지를 보려고 한다면 어떻게 해아할까요? Powerbi 에서 R을 통해 작업하려 합니다.
#
# 아니면 다른 좋은 분석 방법리 있을까요?
#     코드를 짜주시는 견적서 함께 부탁드립니다.
#
# 데이터 양은 행 100개정도로 데이터 양이 많지는 않습니다


# 다음과 같은 과정을 수행하면 되나요?
#     - 입력 정보(종속변수 : 퇴직 / 독립변수 : 퇴직 외 변수) 를 통해 로지스틱 회귀모형으로 학습
# - 학습 결과에서 P-value 및 상관계수 분석 > 주요 변수 추출

# 라이브러리 선언
library(xlsx)
library(MASS)
library(ROCR)
library(abdiv)

# 컴퓨터 내부에서 돌아가는 특정한 난수 생성 공식에서 처음 시작값을 주어 매번 같은 값이 나오게 만드는 것
# 즉 이 코드는 훈련 및 데이터 셋 분할과정에서 사용
set.seed(3)

# 엑셀 파일 읽기
data = xlsx::read.xlsx("INPUT/o2job/LogisticsRegression.xlsx", sheetName = "in", encoding = "UTF-8")

# 엑셀 파일에서 NA값을 제거
dataL1 = na.omit(data)

# 자료형 변환 (number > factor)
dataL1$퇴직여부 = factor(dataL1$퇴직여부)

#=====================================================================
# 유의미 변수 선택
#=====================================================================
# Initial Model:
#     퇴직여부 ~ 나이 + 학력 + 국내외.학력 + 성별 + 난이도 + 직급 +
#     근속년수 + 승진후.지난.시간 + 상사
#
# Final Model:
#     퇴직여부 ~ 나이 + 난이도 + 직급 + 근속년수 + 승진후.지난.시간

# 전체 변수에 대한 로지스틱 회귀모형 수행
# 독립변수 : 퇴직여부 제외한 전체 변수
# 종속변수 : 퇴적여부
glmFitVarAll = glm(퇴직여부 ~ ., data = dataL1, family = binomial)

# 1) 기본값으로 변수 선택
# rsStep = step(glmFitVarAll)
# summary(rsStep)

# 1) AIC 기준으로 변수 선택
rsStepAic = MASS::stepAIC(glmFitVarAll, direction = "both")

# 결과에 대한 요약
summary(rsStepAic)

# 한 눈에 분석 결과 확인 가능
rsStepAic$anova

#=====================================================================
# 훈련 및 테스트 셋 설정 (60 : 40)
#=====================================================================
# 훈련 및 데이터 셋을 60:40으로 나누기 위한 인덱스 설정
ind = sample(1:nrow(dataL1), nrow(dataL1) * 0.6)

# 해당 인덱스에 따라 자료 할당
trainData = dataL1[ind,]
testData = dataL1[-ind,]

# 훈련 데이터셋 확인
dplyr::tbl_df(trainData)

# 테스트 데이터셋 확인
dplyr::tbl_df(testData)

#=====================================================================
# 전체 변수에 대해서서
# 훈련 데이터를 이용한 회귀모형 학습
# 테스트 데이터를 이용한 검증 수행
#=====================================================================
# 전체 변수에 대한 로지스틱 회귀모형 수행
# 독립변수 : 퇴직여부 제외한 전체 변수
# 종속변수 : 퇴적여부
library(RmecabKo)
glmFit = glm(퇴직여부 ~ ., data = trainData, family = binomial)

# 회귀모형에 대한 요약 결과
summary(glmFit)

# 실제 퇴직여부
yObs = as.numeric(as.character(testData$퇴직여부))

# 테스트셋을 이용한 예측 퇴직여부
yHat = predict.glm(glmFit, newdata = testData, type = "response")
# yHat =ifelse(yHatPred > 0.5, 1, 0)

# 검증 측정을 위한 기초 설정
lmPred = ROCR::prediction(yHat, yObs)

# ROC 커브를 위한 설정
perform = ROCR::performance(lmPred, "tpr", "fpr")
plot(perform, main = 'ROC Curve')

# AUC 측정 : 1에 가까울수록 최고 성능 : 0.81074
ROCR::performance(lmPred, "auc")@y.values[[1]]

# 이항편차 측정 : 낮을수록 좋음 : 19.61
abdiv::binomial_deviance(yObs, yHat)

#=====================================================================
# 유의미 변수에 대해서
# 훈련 데이터를 이용한 회귀모형 학습
# 테스트 데이터를 이용한 검증 수행
#=====================================================================
## 전체 변수에 대한 로지스틱 회귀모형 수행
# 독립변수 : 나이, 직급, 근속년수, 승진후.지난.시간
# 종속변수 : 퇴적여부
glmFitSel = glm(퇴직여부 ~ 나이 + 직급 + 근속년수 + 승진후.지난.시간, data = trainData, family = binomial)

# 실제 퇴직여부
yObs = as.numeric(as.character(testData$퇴직여부))

# 테스트셋을 이용한 예측 퇴직여부
yHat = predict.glm(glmFitSel, newdata = testData, type = "response")
# yHat =ifelse(yHatPred > 0.5, 1, 0)

# 검증 측정을 위한 기초 설정
lmPred = ROCR::prediction(yHat, yObs)

# ROC 커브를 위한 설정
perform = ROCR::performance(lmPred, "tpr", "fpr")
plot(perform, main = 'ROC Curve')

# AUC 측정 : 1에 가까울수록 최고 성능 : 0.82967
ROCR::performance(lmPred, "auc")@y.values[[1]]

# 이항편차 측정 : 낮을수록 좋음 : 19.338
abdiv::binomial_deviance(yObs, yHat)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(RSelenium)
library(rvest)
library(stringr)
library(tidyverse)
library(data.table)
library(foreach)
library(httr)
library(webdriver)
library(seleniumPipes)
library(readxl)
library(foreach)
library(XML)
library(tidyverse)
library(rvest)
library(utf8)
library(utils)
library(data.table)
library(foreach)
library(xlsx)
library(wordcloud2)
library(htmlwidgets)
library(readr)

Sys.setlocale("LC_ALL")
options(encoding = "UTF-8")
Sys.setenv(LANG = "en_US.UTF-8")

log = log4r::create.logger()
log4r::logfile(log) = paste0("OUTPUT/o2job/log4r_", format(Sys.time(), "%Y%m%d"), ".log")
log4r::level(log) = "INFO"

getXpathText = function(xpath) {
  remDr$getPageSource()[[1]] %>%
    read_html() %>%
    rvest::html_nodes(xpath = xpath) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

getTagText = function(tag) {
  remDr$getPageSource()[[1]] %>%
    read_html() %>%
    rvest::html_nodes(tag) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

getUrlText = function(url, xpath) {
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath = paste0(xpath)) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

getUrlTagText = function(url, tag) {
  xml2::read_html(url) %>%
    rvest::html_nodes(tag) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

getUrlHref = function(url, xpath) {
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath = paste0(xpath)) %>%
    rvest::html_attr("href")
}

getUrlTagHref = function(url, tag) {
  xml2::read_html(url) %>%
    rvest::html_nodes(tag) %>%
    rvest::html_attr("href")
}


getUrlmg = function(url, xpath) {
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath = paste0(xpath)) %>%
    rvest::html_attr("src")
}

getUrlAlt = function(url, xpath) {
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath = paste0(xpath)) %>%
    rvest::html_attr("alt")
}


#=============================================
# 만개의 레시피
#=============================================
pageCnt = (1535 %/% 10) + 1

# 기간 1년, 언론사 선정
contextPath = paste0("https://www.10000recipe.com/recipe/list.html?q=", utils::URLencode(iconv("여름", to = "UTF-8")))
urlInfo = paste0(contextPath, sprintf("&order=reco&page=%d", seq(1, pageCnt, 1)))

data = urlInfo %>%
  purrr::map(~getUrlText(.x, xpath = '//*[@id="contents_area_full"]/ul/ul/li[*]/div[2]/div[1]')) %>%
  unlist() %>%
  as.data.frame()

colnames(data) = c("title")


dataL2 = data.frame()
foreach::foreach(i = 1:nrow(data), .combine = c) %do% {

  dataL1 = RcppMeCab::pos(as_utf8(data$title[i]), format = "data.frame") %>%
    dplyr::filter(pos == "NNG") %>%
    dplyr::select(token)

  dataL2 = dplyr::bind_rows(dataL2, dataL1)
  log4r::info(log, paste0("진행률 : ", round((i / nrow(data)) * 100, 2), " %"))
}


dataL3 = dataL2 %>%
  dplyr::filter(!token %in% c("여름")) %>%
  dplyr::group_by(token) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame()

dplyr::tbl_df(dataL3)

# 출력
xlsx::write.xlsx2(dataL3, file = "OUTPUT/o2job/10000_Recipe.xlsx", append = FALSE, row.names = FALSE, col.names = TRUE)

fig = wordcloud2::wordcloud2(data = dataL3)

# html로 내보내기
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# 내보낸 html 페이지로부터 png 형태로 불러와서 저장
webshot::webshot("fig.html", "FIG/o2job/10000_Recipe.png", vwidth = 775, vheight = 550, delay = 10)


#=============================================
# 네이버 포스트
#=============================================
remDr = remoteDriver(
  remoteServerAddr = "localhost"
  , port = 5000L
  , browserName = "chrome"
)

# cd selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 5000

# 크롬 열기
remDr$open()

# NAVER 접속
remDr$navigate("https://nid.naver.com/nidlogin.login?mode=form&url=https%3A%2F%2Fwww.naver.com")

# 아이디 입력
remDr$findElement(using = "id", value = "id")$setElementAttribute("value", "backjoi")

# 비밀번호 입력
remDr$findElement(using = "id", value = "pw")$setElementAttribute("value", "cjswo123!Q")

# 로그인 버튼
remDr$findElement(using = "id", value = "log.login")$clickElement()


iCount = 1
isFlag = TRUE

dtEndDate = rev(format(seq(lubridate::ymd("2004-08-13"), lubridate::ymd("2020-08-13"), by = "1 years"), "%Y%m%d"))

urlInfo = paste0("https://post.naver.com/search/post.nhn?keyword=%EC%97%AC%EB%A6%84%20%EC%9A%94%EB%A6%AC&sortType=createDate.dsc&range=20000409000000:", dtEndDate[iCount], "235959")

remDr$navigate(urlInfo)

# css의 body를 element로 찾아 지정
webElem = remDr$findElement("css", "body")

dataL2 = data.frame()
jCount = 0

while (isFlag) {

  preCnt = length(unique(getTagText('.tit_feed')))

  webElem$sendKeysToElement(list(key = "end"))

  Sys.sleep(0.5)

  webElemButton = remDr$findElements(using = 'xpath', value = '//*[@id="more_btn"]/button')

  if (length(webElemButton) == 1) {
    remDr$mouseMoveToLocation(webElement = webElemButton[[1]])
    remDr$click()
    webElem$sendKeysToElement(list(key = "end"))
  }

  jCount = jCount + 1

  if (jCount > 120) {
    Sys.sleep(0.5)
    dateListCnt = length(unique(getTagText('.tit_feed')))

    if (preCnt == dateListCnt) {
      title = unique(getTagText('.tit_feed'))

      data.table::fwrite(
        data.frame(title)
        , sep = ","
        , file = paste0("OUTPUT/o2job/Naver_Post.csv")
        , append = FALSE
        , row.names = FALSE
        , col.names = TRUE
        , dateTimeAs = "write.csv"
        , na = NA
      )

      jCount = 0
      iCount = iCount + 1

      urlInfo = paste0("https://post.naver.com/search/post.nhn?keyword=%EC%97%AC%EB%A6%84%20%EC%9A%94%EB%A6%AC&sortType=createDate.dsc&range=20000409000000:", dtEndDate[iCount], "235959")

      remDr$navigate(urlInfo)
      webElem = remDr$findElement("css", "body")

      Sys.sleep(2)
    }
  }
}

data = readr::read_csv(file = paste0("OUTPUT/o2job/Naver_Post.csv"))


dataL2 = data.frame()
foreach::foreach(i = 1:nrow(data), .combine = c) %do% {

  dataL1 = RcppMeCab::pos(as_utf8(data$title[i]), format = "data.frame") %>%
    dplyr::filter(pos == "NNG") %>%
    dplyr::select(token)

  dataL2 = dplyr::bind_rows(dataL2, dataL1)
  log4r::info(log, paste0("진행률 : ", round((i / nrow(data)) * 100, 2), " %"))
}


dataL3 = dataL2 %>%
  dplyr::filter(!token %in% c("여름", "레시피", "요리")) %>%
  dplyr::group_by(token) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame()

dplyr::tbl_df(dataL3)

# 출력
xlsx::write.xlsx2(dataL3, file = "OUTPUT/o2job/Naver_Post.xlsx", append = FALSE, row.names = FALSE, col.names = TRUE)

fig = wordcloud2::wordcloud2(data = dataL3)

# html로 내보내기
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# 내보낸 html 페이지로부터 png 형태로 불러와서 저장
webshot::webshot("fig.html", "FIG/o2job/Naver_Post.png", vwidth = 775, vheight = 550, delay = 10)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

# 선생님 연합, 중앙, 조선 동아 일보 신문뉴스 기사를 분석하여 외상 후 스트레스 장애 의미분석 저번에 보내드린 논문 틀대로 하는게 가능할까요? (제목에 소방관은 빼구요)

# 다음과 같은 과정을 수행하면 되나요?

# 1) 네이버 키워드 검색 > 자료 수집
#    - 6개 키워드 선정 : PTSD, 외상, 스트레스, 장애, 외상후, 외상 후 스트레스 장애
#    - 자료 기간 : 최근 1년
#    - 수집 종류 : 연합, 조선, 중앙, 동아
# 2) 수집 결과 데이터셋 정보 : 둥록일, 신문 종류, 신문 제목, 신문 내용
# 3) 자료 전처리 : 중복 제거, 내용에 대해서 불용어 제거 (즉 명사만 추출)
# 4) 빈도분석 : 신문 내용 키워드
# 5) 워드 클라우드 시각화 : 신문 내용 키워드
# 6) 네트워크 분석 : 신문 내용 키워드

library(RSelenium)
library(rvest)
library(stringr)
library(tidyverse)
library(data.table)
library(foreach)
library(httr)
library(webdriver)
library(seleniumPipes)
library(readxl)
library(foreach)
library(XML)
library(tidyverse)
library(rvest)
library(utf8)
library(utils)
library(data.table)
library(foreach)
library(xlsx)
library(wordcloud2)
library(htmlwidgets)
library(concoR)
library(igraph)
library(widyr)
library(ggraph)
library(xlsx)
library(tidygraph)
library(tidytext)
library(igraph)
library(ggraph)
library(tidyverse)
library(tidytext)
library(janeaustenr)
library(tidygraph)

# install.packages("multilinguer")
# install.packages("rJava")
# library(multilinguer)
# install_jdk()
# install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
# install.packages("remotes")
# remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
# library(KoNLP) #최종적으로 "KoNLP" 패키지를 불러옵니다

# devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)
# Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_261')  # 설치한 JAVA version에 따라 달라집니다
# buildDictionary(ext_dic = "woorimalsam")  # "woorimalsam" dic을 불러옵니다
# useNIADic()  # "NIADic" dic을 불러옵니다


Sys.setlocale("LC_ALL")
options(encoding = "UTF-8")
Sys.setenv(LANG = "en_US.UTF-8")

log = log4r::create.logger()
log4r::logfile(log) = paste0("OUTPUT/o2job/log4r_", format(Sys.time(), "%Y%m%d"), ".log")
log4r::level(log) = "INFO"

getUrlText = function(url, xpath) {
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath = paste0(xpath)) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

getUrlTagText = function(url, tag) {
  xml2::read_html(url) %>%
    rvest::html_nodes(tag) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

getUrlHref = function(url, xpath) {
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath = paste0(xpath)) %>%
    rvest::html_attr("href")
}

getUrlTagHref = function(url, tag) {
  xml2::read_html(url) %>%
    rvest::html_nodes(tag) %>%
    rvest::html_attr("href")
}


getUrlmg = function(url, xpath) {
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath = paste0(xpath)) %>%
    rvest::html_attr("src")
}

getUrlAlt = function(url, xpath) {
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath = paste0(xpath)) %>%
    rvest::html_attr("alt")
}


keywordList = c("PTSD", "외상", "스트레스", "장애", "외상후", "외상 후 스트레스 장애")
cntList = c(2978, 33675, 159814, 512403, 1545, 5089)

# seq(1, 121, 10)

for (i in 1:length(keywordList)) {

  # 기간 1년, 언론사 선정
  contextPath = paste0("https://search.naver.com/search.naver?where=news&query=", utils::URLencode(iconv(keywordList[i], to = "UTF-8")), "&sm=tab_opt&sort=0&photo=0&field=0&reporter_article=&pd=5&ds=2019.08.09&de=2020.08.08&docid=&nso=so%3Ar%2Cp%3A1y%2Ca%3Aall&mynews=1&")

  urlInfo = paste0(contextPath, sprintf("start=%d&refresh_start=0", seq(1, cntList[i], 10)))

  log4r::info(log, paste0("키워드 : ", keywordList[i]))

  urlDtlInfo = urlInfo %>%
    purrr::map(~getUrlTagHref(.x, 'ul > li > dl > dd > a')) %>%
    unlist()

  log4r::info(log, paste0("urlDtlInfo : ", length(urlDtlInfo)))

  data = data.frame()
  for (urlInfo in urlDtlInfo) {

    html = read_html(urlInfo)

    #=================================================
    # 제목
    #=================================================
    title = html %>%
      html_nodes('#articleTitle') %>%
      html_text()

    if (length(title) == 0) {
      title = html %>%
        html_nodes('.end_tit') %>%
        html_text()
    }

    if (length(title) == 0) {
      title = html %>%
        html_nodes(xpath = '//*[@id="content"]/div/div[1]/div/div[1]/h4') %>%
        html_text()
    }

    titleInfo = title %>%
      str_replace_all(pattern = "\n", replacement = " ") %>%
      str_replace_all(pattern = "[\\^]", replacement = " ") %>%
      str_replace_all(pattern = "\"", replacement = " ") %>%
      str_replace_all(pattern = "\\s+", replacement = " ") %>%
      str_trim(side = "both")

    #======================================================
    # 본문
    #=====================================================
    content = html %>%
      html_nodes('._article_body_contents') %>%
      html_text()

    if (length(content) == 0) {
      content = html %>%
        html_nodes(xpath = '//*[@id="articeBody"]') %>%
        html_text()
    }

    if (length(content) == 0) {
      content = html %>%
        html_nodes('#newsEndContents') %>%
        html_text()
    }

    contentInfo = content %>%
      str_replace_all(pattern = "\n", replacement = " ") %>%
      str_replace_all(pattern = "[\\^]", replacement = " ") %>%
      str_replace_all(pattern = "\"", replacement = " ") %>%
      str_replace_all(pattern = "\\s+", replacement = " ") %>%
      str_trim(side = "both")

    #=================================================
    # 종류
    #=================================================
    type = html %>%
      rvest::html_nodes(xpath = '//*[@id="main_content"]/div[1]/div[1]/a/img') %>%
      rvest::html_attr("alt")

    if (length(type) == 0) {
      type = html %>%
        rvest::html_nodes(xpath = '//*[@id="content"]/div[1]/div/div[1]/a/img') %>%
        rvest::html_attr("alt")
    }

    if (length(type) == 0) {
      type = html %>%
        rvest::html_nodes(xpath = '//*[@id="pressLogo"]/a/img') %>%
        rvest::html_attr("alt")
    }

    if (length(titleInfo) == 0 ||
      length(contentInfo) == 0 ||
      length(type) == 0) {
      log4r::info(log, paste0("속성 정보가 없습니다. : ", url))
    } else {
      data = dplyr::bind_rows(data, data.frame(urlInfo, type, titleInfo, contentInfo))
    }
  }

  data.table::fwrite(
    data
    , sep = ","
    , file = paste0("OUTPUT/o2job/Naver_News_Keyword_", i, ".csv")
    , append = FALSE
    , row.names = FALSE
    , col.names = TRUE
    , dateTimeAs = "write.csv"
    , na = NA
  )

  log4r::info(log, paste0("엑셀 저장 : ", nrow(data)))

}


#==================================================
# 데이터 전처리
#==================================================

fileList = Sys.glob("OUTPUT/o2job/Naver_News_Keyword_*.csv")

# fileList 한번에 읽기
# rawData = fileList %>%
#     purrr::map(~ readr::read_csv(.x)) %>%
#     purrr::reduce(dplyr::bind_rows)

rawData = data.frame()
for (i in 1:length(fileList)) {
  tmpData = readr::read_csv(file = fileList[i])
  tmpDataL1 = data.frame(keyword = keywordList[i], tmpData)

  rawData = dplyr::bind_rows(rawData, tmpDataL1)
}

data = rawData %>%
  dplyr::filter(type %in% c("연합뉴스", "조선일보", "중앙일보", "동아일보"))

dplyr::glimpse(data)

dataL2 = data.frame()
foreach::foreach(i = 1:nrow(data), .combine = c) %do% {

  dataL1 = RcppMeCab::pos(as_utf8(data$contentInfo[i]), format = "data.frame") %>%
    dplyr::filter(pos == "NNG") %>%
    dplyr::select(token)

  tmpData = data.frame(did = i, keyword = data$keyword[i], dataL1)

  # dataL2 = dplyr::bind_rows(dataL2, data.frame(type = data$type[i], dataL1))
  # log4r::info(log, paste0("진행률 : ", round((i / nrow(data)) * 100, 2), " %"))

  readr::write_csv(x = tmpData, path = paste0("OUTPUT/o2job/Naver_News_Keyword4.csv"), append = TRUE)
}


#==================================================
# 데이터 처리
#==================================================

# dataL2 = readr::read_csv(file = paste0("OUTPUT/o2job/Naver_News_Keyword.csv"), col_names = c("token"))
# dataL2 = read.csv(file = paste0("OUTPUT/o2job/Naver_News_Keyword2.csv"), sep = ";", header = FALSE, col.names = c("type", "token"))
# dataL2 = readr::read_csv(file = paste0("OUTPUT/o2job/Naver_News_Keyword3.csv"), col_names = c("type", "token"))
dataL2 = readr::read_csv(file = paste0("OUTPUT/o2job/Naver_News_Keyword4.csv"), col_names = c("type", "keyword", "token"))

#==================================================
# 키워드 빈도에 따른 시각화
#==================================================
keywordData = dataL2 %>%
  dplyr::filter(!token %in% c("書", "請暇")) %>%
  dplyr::group_by(token) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame() %>%
  dplyr::top_n(n = 50)

fig = wordcloud2::wordcloud2(data = keywordData)

# html로 내보내기
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# 내보낸 html 페이지로부터 png 형태로 불러와서 저장
webshot::webshot("fig.html", "FIG/o2job/Naver_News_Keyword.png", vwidth = 775, vheight = 550, delay = 10)


dataL3 = dataL2 %>%
  dplyr::filter(!token %in% c("書", "請暇")) %>%
  # dplyr::group_by(type, token) %>%
  dplyr::group_by(keyword, token) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame()

tbl_df(dataL3)

#==================================================
# TF 및 TF-IDF 분석
#==================================================

# book_words <- austen_books() %>%
#     unnest_tokens(word, text) %>%
#     count(book, word, sort = TRUE) %>%
#     ungroup()
# book_words$chapter <- sample(1:10, nrow(book_words), T)
# book_words %>%
#     unite("book_chapter", book, chapter) %>%
#     bind_tf_idf(word, book_chapter, n) %>% print %>%
#     separate(book_chapter, c("book", "chapter"), sep="_") %>%
#     arrange(desc(tf_idf))


dataL4 = dataL3 %>%
  # dplyr::mutate(class = as.factor("A")) %>%
  tidytext::bind_tf_idf(term = token, document = keyword, n = freq)
# tidytext::bind_tf_idf(term = token, document = type, n = freq)

dataL5 = dataL4 %>%
  dplyr::arrange(dplyr::desc(x = tf)) %>%
  dplyr::top_n(n = 50)

xlsx::write.xlsx2(dataL5, file = "OUTPUT/o2job/Naver_News_Keyword_Result.xlsx", sheetName = "TF", append = TRUE, row.names = FALSE, col.names = TRUE)

dataL5 = dataL4 %>%
  dplyr::arrange(dplyr::desc(x = tf_idf)) %>%
  dplyr::top_n(n = 50)

xlsx::write.xlsx2(dataL5, file = "OUTPUT/o2job/Naver_News_Keyword_Result.xlsx", sheetName = "TF_IDF", append = TRUE, row.names = FALSE, col.names = TRUE)

fig = wordcloud2::wordcloud2(data = dataL5 %>%
  dplyr::select(token, freq))

# html로 내보내기
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# 내보낸 html 페이지로부터 png 형태로 불러와서 저장
webshot::webshot("fig.html", "FIG/o2job/Naver_News_Keyword_TF_IDF.png", vwidth = 775, vheight = 550, delay = 10)

#==================================================
# 연결중심성, 위세중심성
#==================================================
# 전체 네트워크를 시각화
dataL6 = dataL5 %>%
  widyr::pairwise_dist(item = token, feature = keyword, value = tf_idf, method = "euclidean", upper = FALSE) %>%
  dplyr::select(item1, item2) %>%
  # dplyr::select(keyword, token) %>%
  igraph::graph_from_data_frame()

# dataL6_2 = as_tbl_graph(dataL6)


# Cset.seed(2017)
#plot(bigram_graph)
# ggraph(dataL6_2, layout = "fr") +
#     geom_edge_link() +
#     geom_node_point() +
#     geom_node_text(aes(label = name), vjust = 1, hjust = 1)

ggraph(dataL6, layout = "fr") +
  geom_edge_link(width = 0.1, colour = "lightgray") +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, show.legend = FALSE) +
  theme_void() +
  ggsave(filename = "FIG/o2job/Naver_News_Keyword_All_Network.png", dpi = 600)


dataL7 = dataL5 %>%
  # widyr::pairwise_cor(item = token, feature = keyword, value = tf_idf, method = "pearson", sort = TRUE)  %>%
  # dplyr::filter(correlation >= 0.8) %>%
  # widyr::pairwise_dist(item = token, feature = keyword, value = tf_idf, method = "euclidean", upper = FALSE) %>%
  # dplyr::select(item1, item2) %>%
  tidygraph::as_tbl_graph() %>%
  dplyr::mutate(degreeCent = tidygraph::centrality_degree()) %>% # 고유 중심성
  dplyr::mutate(eigenVectorCent = tidygraph::centrality_eigen()) %>% #위세 중심성
  as_tibble() %>%
  arrange(desc(degreeCent)) %>%
  as.data.frame()

xlsx::write.xlsx2(dataL7, file = "OUTPUT/o2job/Naver_News_Keyword_Result.xlsx", sheetName = "CENT", append = TRUE, row.names = FALSE, col.names = TRUE)

#==================================================
# CONCOR 분석
#==================================================
dataL8 = dataL5 %>%
  widyr::pairwise_cor(item = token, feature = keyword, value = tf_idf, method = "pearson", sort = TRUE) %>%
  dplyr::filter(correlation >= 0.8)

dataL9 = dataL8 %>%
  igraph::graph_from_data_frame() %>%
  as.undirected()

png(file = "FIG/o2job/Naver_News_Keyword_Concor1.png", 1200, 1000, pointsize = 25)
comm = cluster_fast_greedy(dataL9)
igraph::plot_dendrogram(comm, mode = "hclust", main = "Pairwise word clusters dendogram")
dev.off()

ggraph(dataL9, layout = "fr") +
  geom_edge_link(width = 0.1, colour = "lightgray") +
  geom_node_point(colour = "#00AFBB") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  ggsave(filename = "FIG/o2job/Naver_News_Keyword_Concor2.png", dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================
# ================KoNLP============
# install.packages("multilinguer")
# library(multilinguer)
# install_jdk()
# install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
# install.packages("remotes")
# remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"), force=TRUE)
# library(KoNLP)

library(data.table)
library(RcppMeCab)
library(tidyverse)
library(foreach)
library(utf8)
library(utils)
# RmecabKo::install_mecab("c:/mecab")

data_namuh <- read.csv("INPUT/o2job/모바일증권 나무(계좌개설 겸용).csv")
data_bankis <- read.csv("INPUT/o2job/한국투자증권 (계좌개설 포함).csv")

# data_sap_na <- sapply(data_namuh,extractNoun,USE.NAMES = F)
# data_sap_ba <- sapply(data_bankis,extractNoun,USE.NAMES = F)
#
# data_unlist_na <-unlist(data_sap_na)
# data_unlist_ba <-unlist(data_sap_ba)
#
# wordcount_na <- table(data_unlist_na)
# wordcount_ba <- table(data_unlist_ba)

dplyr::tbl_df(data_namuh)

data = data_namuh
data = data_bankis

#==================================================
# 자료 처리
#==================================================
dataL2 = data.frame()
foreach::foreach(i = 1:nrow(data_namuh), .combine = c) %do% {

  dataL1 = RcppMeCab::pos(as_utf8(data$내용[i]), format = "data.frame") %>%
    dplyr::filter(pos == "NNG") %>%
    dplyr::select(token)

  dataL2 = dplyr::bind_rows(dataL2, dataL1)

  # log4r::info(log, paste0("진행률 : ", round((i / nrow(data)) * 100, 2), " %"))
}


#==================================================
# 키워드 빈도에 따른 시각화
#==================================================
keywordData = dataL2 %>%
  dplyr::group_by(token) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame()

xlsx::write.xlsx2(keywordData, file = "OUTPUT/o2job/App.xlsx", sheetName = "namuh", append = TRUE, row.names = FALSE, col.names = TRUE)
xlsx::write.xlsx2(keywordData, file = "OUTPUT/o2job/App.xlsx", sheetName = "bankis", append = TRUE, row.names = FALSE, col.names = TRUE)

# 상위 50개
fig = wordcloud2::wordcloud2(data = keywordData %>%
  dplyr::top_n(n = 50)
)

# html로 내보내기
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# # 내보낸 html 페이지로부터 png 형태로 불러와서 저장
# webshot::webshot("fig.html", "FIG/o2job/App_Namuh.png", vwidth = 775, vheight = 550, delay = 10)
webshot::webshot("fig.html", "FIG/o2job/App_Bankis.png", vwidth = 775, vheight = 550, delay = 10)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(readxl)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggplot2)
library(lubridate)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)

korData = raster::shapefile("INPUT/o2job/EMD_202005/EMD.shp")
korSpData = spTransform(korData, CRS("+proj=longlat"))
korMap = fortify(korSpData)

sheetList = c("급성 심근경색(I21) 외래")

data = readxl::read_excel("INPUT/o2job/2015-2019_홍성의료원_기초조사-3.xlsx", sheet = sheetList)

dplyr::glimpse(data)

dataL1 = na.omit(data)

dataL2 = dataL1 %>%
  dplyr::rename("dtDateTime" = "진료날짜") %>%
  dplyr::mutate(
    year = lubridate::year(dtDateTime)
    , month = lubridate::month(dtDateTime)
    , day = lubridate::day(dtDateTime)
    , korAddr = stringr::str_c(`주소지(시,도)`, 시군구, 읍면동, sep = " ")
  )

# dataL3 = ggmap::mutate_geocode(dataL2, korAddr, source="google") %>%
# as.data.frame()
dataL3 = readxl::read_excel("OUTPUT/o2job/홍성의료원_Result.xlsx", sheet = sheetList)

# 나이, 발생횟수에 대한 통계 분석
dataL4 = dataL3 %>%
  dplyr::group_by(korAddr, lon, lat) %>%
  dplyr::summarise(
    나이 = mean(나이, na.rm = TRUE)
    , 발생횟수 = n()
  )

cenVal = c(mean(dataL4$lon, na.rm = TRUE), mean(dataL4$lat, na.rm = TRUE))
mapMarker = get_googlemap(center = cenVal, maptype = "hybrid", zoom = 9)

ggmap(mapMarker, extent = "device") +
  geom_point(data = dataL4, aes(x = lon, y = lat, size = 나이, colour = 발생횟수), alpha = 0.75) +
  scale_color_gradientn(colours = rainbow(10)) +
  scale_radius() +
  labs(title = sheetList) +
  ggsave(filename = "FIG/o2job/Image_100.png", width = 8, height = 7, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================
library(tm)
library(tidyverse)
library(wordcloud2)
library(igraph)
library(qgraph)
# devtools::install_github("cpsievert/LDAvisData")
library(LDAvisData)
library(lda)
library(LDAvis)
library(servr)
library(rJava)
# Sys.setenv(JAVA_HOME='C:\\dev\\java\\jdk-14.0.2')
# Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_261')
library(KoNLP)
devtools::install_github('haven-jeon/KoNLP', force = TRUE)

#### 키워드 데이터 불러오기

Keyword = readLines("INPUT/o2job/report/Keyword.txt")
Keyword = gsub("\\.", "", Keyword) # 키워드에서 특수문자 '.' 제거

#### 삭제 대상 키워드 불러오기

Remove = read.csv("INPUT/o2job/report/Remove.csv",
                  header = TRUE, stringsAsFactors = FALSE)

#### 단어 대체 키워드 불러오기

Replace = read.csv("INPUT/o2job/report/0401.csv")

#### 특수문자, 숫자 제거

Keyword2 = Keyword %>%
  str_replace_all("[0-9]+", " ") %>%
  str_replace_all("\\s+", " ") %>%
  str_replace_all("http[a-zA-Z0-9]+", "") %>%
  str_replace_all('\n', ' ') %>%
  str_replace_all('\t', ' ')


##### gsub을 통해 해당 키워드들 삭제

Keyword2 = gsub("\\,", " ", Keyword2)
Keyword2 = gsub("[^가-힣]", " ", Keyword2)
Keyword2 = gsub("입시", "", Keyword2)
Keyword2 = gsub("평가", "", Keyword2)
Keyword2 = gsub("교과", "", Keyword2)
Keyword2 = gsub("학년도", "", Keyword2)
Keyword2 = gsub("단계", "", Keyword2)
Keyword2 = gsub("학년", "", Keyword2)
Keyword2 = gsub("자신", "", Keyword2)
Keyword2 = gsub("교수", "", Keyword2)
Keyword2 = gsub("분야", "", Keyword2)
Keyword2 = gsub("중심", "", Keyword2)
Keyword2 = gsub("과정", "", Keyword2)
Keyword2 = gsub("아이들", "", Keyword2)
Keyword2 = gsub("과목", "", Keyword2)
Keyword2 = gsub("시작", "", Keyword2)

Keyword2_2 = Keyword2

#### 비슷한 키워드 통일 작업 진행 (예시, 4차 -> 4차 산업)

R = c("4차", "입시", "주도", "전문가", "개별성", "평가", "교과", "상상력", "주도성", "산업", "인공")
R2 = c("4차산업", "입시", "주도성", "전문가", "창의성", "평가", "교과", "상상력", "주도성", "4차산업", "인공지능")

RR = c(Replace$word, R)
RR2 = c(Replace$최종, R2)

Replace2 = data.frame(
  Before = RR,
  After = RR2
)

#### 위 정보를 기반으로 키워드 정리 작업 진행

Keyword_V = c()

for (i in 1:length(Keyword2_2)) {

  if (i %% 100 == 0) {

    print(i)

  }

  STR = ""

  for (k in unlist(strsplit(Keyword2_2[i], " "))) {  # 키워드 띄어쓰기를 기준으로 분리

    #### 분리된 키워드를 다시 이어붙이는 작업 진행

    if (k != "") { # 분리된 키워드가 ""(공백)이 아닐때

      if (nchar(k) > 1) { # 분리된 키워드의 글자수가 2개 이상일 때

        for (s in 1:length(R)) {

          Index = grep(RR[s], k)

          if (length(Index) > 0) {

            k = RR2[s]

          }

        }

      }

    }

    STR = paste(STR, k) ## 분리된 텍스트 다시 이어 붙이기

  }

  Keyword_V[i] = STR

}

Keyword2_2[1] # Before
Keyword_V[1] # After

##### 텍스트 마이닝 작업 진행

text = Corpus(VectorSource(Keyword_V)) # 텍스트 분석을 위한 말뭉치(Corpus) 생성

#### TermDocumentMatrix(용어-문서 행렬) 생성

tdm = TermDocumentMatrix(text,
                         control = list(tokenize = extractNoun))

#### 키워드별 등장 빈도 Counting

ft = data.frame(word = rownames(as.matrix(tdm)),
                freq = rowSums(as.matrix(tdm)))

##### 키워드 분석 결과 정리
Word_Freq = ft %>%
  mutate(word = as.character(word)) %>%
  filter(nchar(word) > 1) %>% # 2글자 이상 단어만 남기기
  filter(!word %in% Remove$word) %>% # 제외 대상에 있는 키워드 제외
  filter(freq > 10) %>% # 최소 11번 이상 등장한 키워드들만 남기기
  group_by(word) %>%
  summarise(freq = sum(freq)) %>% # 동일 키워드가 있을 수가 있으니, 동일 키워드별 재집계(합)
  arrange(-freq) # 등장빈도 순으로 정렬


## 입시, 평가, 교과 데이터 제외

Word_Freq2 = Word_Freq %>%
  filter(!word %in% c("입시", "평가", "교과")) %>%
  filter(freq > 10)

## 인공능력 -> 인공지능으로 변경

Word_Freq2$word[grep("인공능력", Word_Freq2$word)] = "인공지능"


## 위 정리 방식과 같은 방식의 정리

Word_Freq2 = Word_Freq2 %>%
  mutate(word = as.character(word)) %>%
  filter(nchar(word) > 1) %>%
  filter(!word %in% Remove$word) %>%
  filter(freq > 70) %>%
  group_by(word) %>%
  summarise(freq = sum(freq)) %>%
  arrange(-freq)

### 워드클라우드 그리기

wordcloud2(Word_Freq2, size = 0.6)

### 메인 키워드 40개 추출

Main_Keywords = Word_Freq2$word[1:40]


### 동시출현 행렬 만들기
tdm1 = as.matrix(tdm)
tdm1 = tdm1[Main_Keywords,]
tam = tdm1 %*% t(tdm1)

qgraph(tam, labels = rownames(t(tam)), diag = F,
       layout = 'spring', edge.color = 'darkblue', vsize = 6)

#### 단어 빈도 csv 파일 저장

write.csv(Word_Freq2,
          "OUTPUT/o2job/report/Word_Freq3.csv",
          row.names = FALSE)

####################

#### 토픽 모델링

doc.list <- strsplit(Keyword_V, "[[:space:]]+") # 키워드 띄어쓰기를 기준으로 분리

## TermDocument Matrix 생성

TDM_M = as.matrix(tdm)
TF = rowMeans(TDM_M) # 평균등장빈도 계산
TF = sort(TF, decreasing = TRUE) # 평균적으로 많이 나온 키워드 순으로 정렬
TF = TF[1:300] # 상위 300개 키워드 선택

term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)


term.table[1:50]

#####


TF2 = TF[nchar(names(TF)) > 1] # 글자수 2개 이상 키워드만 남기기
TF2

Selected = names(term.table) %in% names(TF2) # 제 2사전에 포함된 단어만 추출
sum(Selected)
term.table = term.table[Selected]
vocab = names(term.table)

### 키워드 index 함수 생성
### index란 특정키워드는 특정 index에 저장하는 것을 의미
### 키워드 인덱스로 변환 (저장공간때문에 인덱스로 저장함)

get.terms = function(x) {
  index = match(x, vocab)
  index = index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}

documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set:
D = length(documents)  # number of documents (2,000)
W = length(vocab)  # number of terms in the vocab (14,568)
doc.length = sapply(documents, function(x) sum(x[2,]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N = sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency = as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]


### Topic갯수별 TopicModeling 진행

### Topic의 갯수를 3 ~ 10개까지 각각 설정하면서 Topic Modeling 진행

for (k in c(3, 4, 5, 6, 7, 8, 9, 10)) {

  print(k)

  K = k  # Topic 갯수
  G = 5000 # 연산 횟수
  alpha = 0.02 # Topic Modeling Parameter
  eta = 0.02 # Topic Modeling Parameter

  ### Topic Modeling
  fit = lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab,
                                    num.iterations = G, alpha = alpha,
                                    eta = eta, initial = NULL, burnin = 0,
                                    compute.log.likelihood = TRUE)

  theta = t(apply(fit$document_sums + alpha, 2, function(x) x / sum(x)))
  phi = t(apply(t(fit$topics) + eta, 2, function(x) x / sum(x)))

  Parameters = list(phi = phi,
                    theta = theta,
                    doc.length = doc.length,
                    vocab = vocab,
                    term.frequency = term.frequency)

  options(encoding = 'UTF-8')


  # create the JSON object to feed the visualization:

  json <- createJSON(phi = Parameters$phi,
                     theta = Parameters$theta,
                     doc.length = Parameters$doc.length,
                     vocab = Parameters$vocab,
                     term.frequency = Parameters$term.frequency,
                     encoding = 'UTF-8')

  serVis(json, out.dir = 'vis2',
         open.browser = TRUE)


  Topic_Words = as.data.frame(top.topic.words(fit$topics))

  ### 토픽 결과 저장

  write.csv(Topic_Words,
            paste0("OUTPUT/o2job/report/Topic_Words_", k, "topis.csv"),
            row.names = FALSE, fileEncoding = "CP949")

}


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(tidyverse)
#set working directory
# setwd("")
load("INPUT/o2job/tvtropes_analysis_stereotypes.RData")


#function for tracking tropes over time, one value for each decade
tropesearch = function(x) {
  #movies per year by decade
  mpyw = data.frame(year = rep(movies$oscarsYear, lengths(tropes)), winner = rep(movies$winner, lengths(tropes)), trope = unlist(tropes)) %>%
    mutate(era = floor(year / 10) * 10) %>%
    group_by(era) %>%
    summarise(ntropes = n())
  data.frame(year = rep(movies$oscarsYear, lengths(tropes)), trope = unlist(tropes)) %>%
    filter(trope %in% x) %>%
    #fill up values with n=0
    mutate(era = floor(year / 10) * 10) %>%
    group_by(era, trope) %>%
    summarise(n = n()) %>%
    ungroup %>%
    complete(era, nesting(trope), fill = list(n = 0)) %>%
    full_join(mpyw, by = c("era")) %>%
    mutate(n = ifelse(is.na(n), 0, n), perc = n / ntropes)
}

tmp = tropesearch(x) %>%
  mutate(trope = trope %>% as.character %>% factor(., levels = x)) %>%
  filter(era > 1920)
ggplot(tmp, aes(x = era, y = perc)) +
  facet_wrap(~trope, ncol = 3) +
  geom_line(color = "#00a5ff") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent,)


#plot frequency over time as line
freqplot = function(x, limits = NULL, file_end) {
  tmp = tropesearch(x) %>%
    mutate(trope = trope %>% as.character %>% factor(., levels = x)) %>%
    filter(era > 1920)
  ggplot(tmp, aes(x = era, y = perc)) +
    facet_wrap(~trope, ncol = 3) +
    geom_line(color = "#00a5ff") +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent)
  ggsave(paste0("INPUT/o2job/original/movies_line_", file_end, ".png"), device = "png", scale = 2, width = 80, height = 70, units = "mm")
  ggsave(paste0("INPUT/o2job/original/movies_line_", file_end, ".svg"), device = "svg", scale = 2, width = 80, height = 70, units = "mm")
}

#find movies that contain a trope within a timeframe
moviesearch = function(x, startyear = 1925, endyear = 2020) {
  data.frame(year = rep(movies$oscarsYear, lengths(tropes)), winner = rep(movies$winner, lengths(tropes)),
             movie = rep(movies$eligibleTitle, lengths(tropes)), trope = unlist(tropes)) %>%
    filter(trope %in% x, year >= startyear, year <= endyear)
}

##Look up tropes over time

## Asians/Asian-Americans
#make list
x = c("Yellow Peril", "Yellowface", "Asian Speekee Engrish", "Interchangeable Asian Cultures", "Mighty Whitey and Mellow Yellow", "Asian and Nerdy", "All Asians Know Martial Arts", "Asian Store-Owner", "Identical-Looking Asians")
#plot
freqplot(x, file_end = "asia")
#find movies
moviesearch(x) %>% View

#Black people
x = c("Scary Black Man", "Black Dude Dies First", "Black Best Friend", "Sassy Black Woman", "Blackface", "But Not Too Black")
freqplot(x, file_end = "black")
moviesearch(x) %>% View

rm(x)
save("tvtropes_analysis_stereotypes.RData")


#======================================================
# 주 소스 코드
#======================================================
xList = c("Yellow Peril", "Yellowface", "Asian Speekee Engrish", "Interchangeable Asian Cultures", "Mighty Whitey and Mellow Yellow", "Asian and Nerdy", "All Asians Know Martial Arts", "Asian Store-Owner", "Identical-Looking Asians")

for (x in xList) {
  tmp = tropesearch(x) %>%
    mutate(trope = trope %>% as.character %>% factor(., levels = x)) %>%
    filter(era > 1920)
  data = na.omit(tmp)

  ggplot(data, aes(x = era, y = perc)) +
    facet_wrap(~trope, ncol = 1) +
    geom_line(color = "#00a5ff") +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.002)) +
    ggsave(filename = paste0("OUTPUT/o2job/original/limit_", x, ".png"), dpi = 600)
}


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(noncompliance)
library(tidyverse)
library(readr)
library(data.table)
library(RcppMeCab)
library(utf8)
library(RmecabKo)
library(foreach)
# RmecabKo::install_mecab("c:/mecab")


fileList = Sys.glob("INPUT/o2job/OUTPUT/*.csv")


file = fileList[1]

# fileList 한번에 읽기
# rawData = fileList %>%
#     purrr::map(~ readr::read_csv(.x)) %>%
#     purrr::reduce(dplyr::bind_rows)
#
# rawData = data.frame()
# for (i in 1:length(fileList)) {
#     tmpData = readr::read_csv(file = fileList[i])
#     tmpDataL1 = data.frame(keyword = keywordList[i], tmpData)
#
#     rawData = dplyr::bind_rows(rawData, tmpDataL1)
# }

data = readr::read_csv(file)
i = 1


dataL2 = data.frame()
foreach::foreach(i = 1:nrow(data), .combine = c) %do% {

  dataL1 = RcppMeCab::pos(as_utf8(data$X1[i]), format = "data.frame")

  dataL2 = dplyr::bind_rows(dataL2, dataL1)
}

dataL3 = dplyr::bind_cols(data, dataL2)


library(rJava)
Sys.setenv(JAVA_HOME = 'C:\\Program Files\\Java\\jdk1.8.0_261')
library(KoNLP)
useSejongDic()


aa = paste(data$X1, collapse = " ")

a = MorphAnalyzer(data$X1) %>%
  reshape2::melt() %>%
  tibble


a = KoNLP::SimplePos09(data$X1) %>%
  reshape2::melt() %>%
  tibble

dataL1 = RcppMeCab::pos(as_utf8(aa), format = "data.frame")

dataL3 = SimplePos09(aa) %>%
  melt %>%
  as.data.frame()


library(politeness)

df_politeness <- politeness(data$X1, metric = "binary", drop_blank = TRUE)

bb = apply(df_politeness, 1, sum) %>%
  as.data.frame()

dataL3 = dplyr::bind_cols(data, df_politeness)

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

# install.packages("raster")
library(raster)
library(ggplot2)
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggplot2)
library(lubridate)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(openxlsx)
library(dplyr)
library(sf)
library(rgeos)
library(ggrepel)
library(geosphere)

# 한국행정구역 분류 지도 다운로드
# http://kssc.kostat.go.kr/ksscNew_web/kssc/common/CommonBoardList.do?gubun=1&strCategoryNameCode=019&strBbsId=kascrr&categoryMenu=014


data = openxlsx::read.xlsx("INPUT/o2job/2015-2019_홍성의료원_기초조사-3/2015-2019홍성의료원기초조사2.xlsx", sheet = 1)

la = shapefile('INPUT/o2job/LSMD_ADM_SECT_UMD/LSMD_ADM_SECT_UMD_44.shp')
laData = ggplot2::fortify(la, region = 'EMD_CD')

# # rgeos::gIsValid(la)
# la <- rgeos::gBuffer(la, byid = TRUE, width = 0)
geo = spTransform(la, CRS("+proj=longlat"))
geoData = ggplot2::fortify(geo)

emdNmDB = la@data

# 동대동(대천 3동) > 동대동
# 홍북면 > 홍북읍
dataL1 = data %>%
  dplyr::filter(
    !is.na(읍면동)
    , `주소지(시,도)` == "충청남도"
  ) %>%
  dplyr::left_join(emdNmDB, by = c("읍면동" = "EMD_NM")) %>%
  dplyr::group_by(EMD_CD, 읍면동) %>%
  dplyr::summarise(n = n())

merge(data)

M <- merge(seoul_map, seoul_sum, by = "id")


dataL2 = merge(seoul_map, seoul_sum, by = "id")


geosphere::centroid()

pol <- rbind(c(-180, -20), c(-160, 5), c(-60, 0), c(-160, -60), c(-180, -20))
centroid(pol)
# }

dataL2 = laData %>%
  dplyr::left_join(dataL1, by = c("id" = "EMD_CD")) %>%
  dplyr::left_join(emdNmDB, by = c("id" = "EMD_CD"))
#  %>%
# dplyr::left_join(geoData, by = c("order" = "order"),  suffix = c(".x", ".y"))
#
dataL3 = dataL2 %>%
  dplyr::filter(!is.na(n)) %>%
  dplyr::group_by(EMD_NM) %>%
  dplyr::summarise(
    meanLon = mean(long, na.rm = TRUE)
    , meanLat = mean(lat, na.rm = TRUE)
  )

ggplot() +
  geom_polygon(data = dataL2, aes(x = long, y = lat, group = group, fill = n), color = 'white') +
  geom_text_repel(data = dataL3, aes(x = meanLon, y = meanLat, label = EMD_NM), color = "red")


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

# F-분포는 F-검정 (F-test)과 두 집단 이상의 분산이 같은지 여부를 비교하는 분산분석(ANOVA, Analysis of Variance)에 사용되며, (카이제곱 분포처럼) 분산을 제곱한 값만을 사용하므로 양(+)의 값만을 가지고 되고, 왼쪽으로 치우치고 오른쪽으로 꼬리가 긴 비대칭형 형태를 띠고 있습니다.  (카이제곱 분포와 모양이 유사함)

library(moonBook)
library(webr)
library(tidyverse)
library(ggstatsplot)
# install.packages("officer", dep = TRUE)

# y1=c(45, 87, 123, 120, 70)
# y2=c(51, 71, 42, 37, 51, 78, 51, 49, 56, 47, 58)

# https://data-science.gr.jp/implementation/ist_r_student_t_test.html
# https://rpubs.com/Jigme505/TT_CI
reference = c(-0.007, -0.007, -0.007, -0.008, -0.008)
sample = c(-0.005, -0.006, -0.006, -0.006, -0.007)

# F 테스트
fTest = var.test(reference, sample, conf.level = 0.95)

# F의 귀무 가설은 2 그룹의 분산이 차이가 없다 (등 분산). 따라서 P < 0.05로 작을 경우 귀무 가설이 기각되어 2 그룹은 상이한 분산이고, 그 이외의 경우 등 분산이다.
# F-test에서 p-value 0.6328 (p > 0.05)로서 두 구간은 등 분산이라고 제한됨. 따라서 2 그룹의 검증은 스튜던트 t 검정을 수행한다.

fTest
plot(fTest) + xlim(-5, 15)

# 귀무 가설은 2 그룹의 평균은 차이가 없다.

# T 테스트
# 등분산 가정 O
tTest = t.test(reference, sample, conf.level = 0.95, var.equal = TRUE, paired = FALSE)

# 등분산 가정 X
# tTest = t.test(reference, sample, conf.level = 0.95, var.equal = FALSE, paired = FALSE)

# Welch Two Sample t-test
#
# data:  y1 and y2
# t = -3.5, df = 7.5294, p-value = 0.008893
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#     -0.0023325334 -0.0004674666
# sample estimates:
#     mean of x mean of y
# -0.0074   -0.0060

# t = -3.5 | t값으로서 기각 영역에 해당
# df = 7.5294 | t의 자유도
# p-value = 0.008893 | 0.05보다 작으면 귀무가설이 기각된다. 즉 2 그룹의 평균은 차이가 있다.
# alternative hypothesis: true difference in means is not equal to 0 | 대립 가설 (평균이 0이 아니다.)
# -0.0023325334 -0.0004674666 | 95% 신뢰구간
# sample estimates | 표본 평균의 추정치

tTest
plot(tTest)


# t = -3.5, df = 8, p-value = 0.008079
pt(q = -3.5, df = 8) * 2       # 양측 검정이라서 2배 해준다.
## [1] 0.008079082

qt(p = (0.008079082 / 2), df = 8)    # 확률값 p-vale의 반을 한 t 값 조회
## [1] -3.5

# t 값 구하기
(mean(child) - mean(parent)) / (s / sqrt(n))
## [1] -2.878929

#=================================================
# 품질 차이에 대한 비교
#=================================================
diff <- y2 - y1
mn <- mean(diff)
s <- sd(diff)
n <- length(y1)
# 평균 +- 95%일때의 통계량 * 표준오차(s / sqrt(n))
mn + c(-1, 1) * qt(.975, df = (n - 1)) * s / sqrt(n)
# mn + c(-1, 1) * qt(.950, df = (n-1)) * s / sqrt(n)

t.test(diff)


t.test(y2, y1)
# t = 3.5, df = 7.5294, p-value = 0.008893
pt(3.5, df = 7.5294) * 2


alpha <- 0.05
nx <- length(y1)
ny <- length(y2)
t.alpha.2.n <- qt(alpha / 2, df = nx + ny - 2)
# -2.144787
t.alpha.2.p <- qt(1 - alpha / 2, df = nx + ny - 2)
# 2.144787


#


#

df = rsT$parameter


qt(0.025, df)
qt(0.975, df)

qt(0.05, df)
qt(0.95, df)

#
pt(-1.859548, df)
#
pt(1.859548, df)


dt(1.859548, df)

#


t.test(x, mu = 10)

t.test(x, mu = 10, alternative = "less")


x = c(9.0, 9.5, 9.6, 10.2, 11.6)
y = c(9.9, 8.7, 9.8, 10.5, 8.9, 8.3, 9.8, 9.0)
t.test(x, y)

qt(c(.025, .975), df = 4)

pt(2.50, 25)
2 * (1 - pt(2.50, 25))

qt(.05, 25)
qt(1 - .05 / 2, 25)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(RSelenium)
library(rvest)
library(stringr)
library(tidyverse)
library(data.table)
library(foreach)
library(httr)
library(webdriver)
library(seleniumPipes)
library(readxl)
library(foreach)
library(XML)
library(tidyverse)
library(rvest)
library(utf8)
library(utils)
library(data.table)
library(foreach)
library(xlsx)
library(wordcloud2)
library(htmlwidgets)
library(concoR)
library(igraph)
library(widyr)
library(ggraph)
library(xlsx)
library(tidygraph)
library(tidytext)
library(igraph)
library(ggraph)
library(tidyverse)
library(tidytext)
library(janeaustenr)
library(tidygraph)
library(rjson)
library(openxlsx)
library(webshot)
library(htmlwidgets)

# install.packages("multilinguer")
install.packages("rJava")
library(rJava)
# library(multilinguer)
# install_jdk()
# install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
# install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts = c("--no-multiarch"))
library(KoNLP) #최종적으로 "KoNLP" 패키지를 불러옵니다

devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_261")
Sys.setenv(JAVA_HOME = 'C:\\Program Files\\Java\\jdk1.8.0_261')  # 설치한 JAVA version에 따라 달라집니다
# buildDictionary(ext_dic = "woorimalsam")  # "woorimalsam" dic을 불러옵니다
# useNIADic()  # "NIADic" dic을 불러옵니다

Sys.setlocale("LC_ALL")
options(encoding = "UTF-8")
Sys.setenv(LANG = "en_US.UTF-8")

ls.str(globalVar)

log = log4r::create.logger()
log4r::logfile(log) = paste0(globalVar$logConfig, "/", "log4r_", format(Sys.time(), "%Y%m%d"), ".log")
log4r::level(log) = "INFO"

getUrlText = function(url, xpath) {
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath = paste0(xpath)) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

getUrlTagText = function(url, tag) {
  xml2::read_html(url) %>%
    rvest::html_nodes(tag) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

getUrlHref = function(url, xpath) {
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath = paste0(xpath)) %>%
    rvest::html_attr("href")
}

getUrlTagHref = function(url, tag) {
  xml2::read_html(url) %>%
    rvest::html_nodes(tag) %>%
    rvest::html_attr("href")
}


getUrlmg = function(url, xpath) {
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath = paste0(xpath)) %>%
    rvest::html_attr("src")
}

getUrlAlt = function(url, xpath) {
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath = paste0(xpath)) %>%
    rvest::html_attr("alt")
}

getXpathText = function(xpath) {
  remDr$getPageSource()[[1]] %>%
    read_html() %>%
    rvest::html_nodes(xpath = xpath) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

getXpathAttr = function(xpath, attr) {
  remDr$getPageSource()[[1]] %>%
    read_html() %>%
    rvest::html_nodes(xpath = xpath) %>%
    rvest::html_attr(attr) %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

remDr = remoteDriver(
  remoteServerAddr = "localhost"
  , port = 5000L
  , browserName = "chrome"
)

# cd selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 5000

# 크롬 열기
remDr$open()

getRootHandle = remDr$getWindowHandles()

# GLOWPICK 주소
contextPath = paste0("https://www.glowpick.com/beauty/ranking?")
urlInfo = paste0(contextPath, "id=1&level=2&brand_category_id=")

# 이동
remDr$navigate(urlInfo)

# 상품 Url 정보 가져오기
urlDtlInfoList = getXpathAttr('//*[@id="gp-list"]/div/section[2]/ul/li[*]/meta[2]', 'content')

i = 2
dataL1 = data.frame()

# foreach::foreach(i = 15:length(urlDtlInfoList), .combine=c) %do% {
for (i in 1:length(urlDtlInfoList)) {

  Sys.sleep(2)
  urlDtlInfo = urlDtlInfoList[i]
  remDr$navigate(urlDtlInfo)

  Sys.sleep(2)

  # 상품
  title = getXpathText('//*[@id="gp-default-main"]/section/div/ul[1]/li[2]/section[1]/h1/span')

  # 브랜드
  brand = getXpathText('//*[@id="gp-default-main"]/section/div/ul[1]/li[2]/section[1]/div[3]/span')

  # 세트 및 가격
  tmpInfo = getXpathText('//*[@id="gp-default-main"]/section/div/ul[1]/li[2]/section[1]/div[2]/div[1]') %>%
    stringr::str_split("/") %>%
    unlist() %>%
    stringr::str_trim(side = "both")

  volume = tmpInfo[1]
  price = tmpInfo[2]

  # 설명
  des = getXpathText('//*[@id="gp-default-main"]/section/div/ul[1]/li[2]/section[3]/table/tbody/tr[2]/td/div')

  if (length(des) == 0) {
    des = getXpathText('//*[@id="gp-default-main"]/section/div/ul[1]/li[2]/section[4]/table/tbody/tr[3]/td/div')
  }

  if (length(des) == 0) {
    des = getXpathText('//*[@id="gp-default-main"]/section/div/ul[1]/li[2]/section[4]/table/tbody/tr[2]/td/div')
  }

  if (length(des) == 0) {
    des = getXpathText('//*[@id="gp-default-main"]/section/div/ul[1]/li[2]/section[3]/table/tbody/tr[3]/td/div')
  }


  #=====================
  # 성분정보
  #=====================
  isItem = FALSE
  # 팝업 열기
  for (k in 1:10) {
    xpathTag = paste0('//*[@id="gp-default-main"]/section/div/ul[1]/li[2]/section[', k, ']/div/span[2]/button/span[1]')

    popText = getXpathText(xpathTag)

    if (length(popText) > 0) {
      if (popText == "성분정보") {
        isItem = TRUE
        selXpathTag = xpathTag
      }
    }
  }

  itemEwgLevel = NA
  itemKor = NA
  itemEng = NA
  itemPurpose = NA

  if (isItem == TRUE) {
    remDr$findElement(using = "xpath", value = selXpathTag)$clickElement()

    # EWG 안정도 등급
    itemEwgLevel = getXpathText('//*[@id="gp-popup"]/div/section[2]/div[2]/ul/li[*]/div[1]/span[2]')

    # 국문명
    itemKor = getXpathText('//*[@id="gp-popup"]/div/section[2]/div[2]/ul/li[*]/div[2]/p[1]')

    # 영문명
    itemEng = getXpathText('//*[@id="gp-popup"]/div/section[2]/div[2]/ul/li[*]/div[2]/p[2]')

    # 목적
    itemPurpose = getXpathText('//*[@id="gp-popup"]/div/section[2]/div[2]/ul/li[*]/div[2]/p[3]')

    # 팝업 닫기
    remDr$findElement(using = "xpath", value = '//*[@id="gp-popup-bg"]/div/button')$clickElement()
  }

  #========================
  # 평점
  #========================
  # 총 평점
  totalScore = getXpathText('//*[@id="gp-default-main"]/section/div/ul[2]/li[2]/section/div/div/div[1]/div[1]')

  # 총 개수
  totalCnt = getXpathText('//*[@id="gp-default-main"]/section/div/ul[2]/li[2]/section/div/div/div[1]/div[3]')

  # 5단계 세부 점수
  stepScore = getXpathText('//*[@id="gp-default-main"]/section/div/ul[2]/li[2]/section/div/div/div[2]/ul/li[*]/div/p')

  #===============================
  # 리뷰
  #===============================
  Sys.sleep(1)

  # "좋아요 많은순"으로 선택
  remDr$findElement(using = "xpath", value = '//*[@id="gp-default-main"]/section/div/ul[2]/li[3]/section/div/div/button')$clickElement()
  remDr$findElement(using = "xpath", value = '//*[@id="gp-default-main"]/section/div/ul[2]/li[3]/section/div/div/div[2]/ul/li[3]')$clickElement()

  Sys.sleep(1)

  webElem = remDr$findElement("css", "body")
  remDr$findElement(using = "xpath", value = '//*[@id="gp-footer"]/div/div[1]/div[3]/figure/img')$clickElement()

  Sys.sleep(1)

  remDr$findElement(using = "xpath", value = '//*[@id="gp-footer"]/div/div[1]/div[3]/figure/img')$clickElement()


  # 10회 스크롤
  # foreach::foreach(j = 1:10, .combine=c) %do% {
  for (j in 1:10) {
    Sys.sleep(0.2)

    # webElem$sendKeysToElement(list(key = "down_arrow"))
    webElem$sendKeysToElement(list(key = "home"))
    webElem$sendKeysToElement(list(key = "end"))
  }


  review = getXpathText('//*[@id="gp-default-main"]/section/div/ul[2]/li[5]/section/ul/li[*]/div/p')


  itemData = data.frame(itemEwgLevel, itemKor, itemEng, itemPurpose)

  # JSON 변환
  itemJson = rjson::toJSON(itemData)

  # JSON에서 DF 변환
  # itemJsonToDf = rjson::fromJSON(data$itemJson) %>%
  #     as.data.frame()

  data = data.frame(
    cnt = i
    , title, brand, volume, price, des
    , itemJson
    , totalScore, totalCnt, stepScore[1], stepScore[2], stepScore[3], stepScore[4], stepScore[5], review
  )

  dataL1 = dplyr::bind_rows(dataL1, data)
}


xlsx::write.xlsx2(
  dataL1
  , file = paste0(globalVar$outConfig, "/", "Glowpick_CrawlingL2.xlsx")
  , sheetName = "자료수집2"
  , append = TRUE
  , row.names = FALSE
  , col.names = TRUE
)


dataL2 = dataL1 %>%
  dplyr::distinct(cnt, title, itemJson)


dataL3 = data.frame()
for (i in 1:nrow(dataL2)) {
  cnt = dataL2$cnt[i]
  title = dataL2$title[i]
  itemJsonToDf = rjson::fromJSON(dataL2$itemJson[i])

  tmpData = data.frame(cnt, title, itemJsonToDf)

  dataL3 = dplyr::bind_rows(dataL3, tmpData)
}


xlsx::write.xlsx2(
  dataL3
  , file = paste0(globalVar$outConfig, "/", "Glowpick_CrawlingL2.xlsx")
  , sheetName = "성분조사2"
  , append = TRUE
  , row.names = FALSE
  , col.names = TRUE
)


# 총 평점
dplyr::tbl_df(dataL1)

dataL3 = dataL1 %>%
  dplyr::distinct(cnt, title, itemJson)


#==============================================
# 자료 처리
#==============================================

dataInfo = xlsx::read.xlsx2(file = paste0(globalVar$outConfig, "/", "Glowpick_CrawlingL2.xlsx"), sheetIndex = 3)

dataDtlInfo = xlsx::read.xlsx2(file = paste0(globalVar$outConfig, "/", "Glowpick_CrawlingL2.xlsx"), sheetIndex = 4)


dataL4 = dataInfo %>%
  # dplyr::group_by(cnt, title) %>%
  dplyr::distinct(cnt, totalScore, itemJson)

head(dataDtlInfo)


library(wordcloud2)
library(htmlwidgets)

#
# 1. 평점데이터와 전성분 데이터 결합 가공을 통해 평점이 좋은 화장품에서 많이 사용하는 전성분 도출

# 키워드 빈도에 따른 시각화
dataL5 = dataDtlInfo %>%
  dplyr::group_by(itemKor) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame()

fig = wordcloud2::wordcloud2(data = dataL5)

# html로 내보내기
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# 내보낸 html 페이지로부터 png 형태로 불러와서 저장
webshot::webshot("fig.html", paste0(globalVar$figConfig, "/", "Glowpick_Crawling_Keyword.png"), vwidth = 775, vheight = 550)

xlsx::write.xlsx2(
  dataL5
  , file = paste0(globalVar$outConfig, "/", "Glowpick_CrawlingL2.xlsx")
  , sheetName = "키워드"
  , append = TRUE
  , row.names = FALSE
  , col.names = TRUE
)

# 2. 고객 리뷰 중 긍정적인 키워드 도출 후 긍정적 리뷰가 많은 전성분 도출
library(SentimentAnalysis)
library(tidyverse)

# head(dataInfo$review)

library(stringr)
doc <- dataInfo$review
docs <- str_replace_all(doc, "[^0-9a-zA-Zㄱ-ㅎㅏ-ㅣ가-힣[:space:]]", " ")
docs <- str_replace_all(docs, "[\n\t]", " ")
docs <- str_trim(docs)
docs <- str_replace_all(docs, "\\s+", " ")


library(tm)
corp <- VCorpus(VectorSource(docs))
tdm <- TermDocumentMatrix(corp,
                          control = list(wordLengths = c(1, Inf),
                                         tokenize = function(x) {
                                           ngram_tokenize(x, char = F)
                                         }))

tail(Terms(tdm))


senti_words_kr = readr::read_delim(paste0(globalVar$inpConfig, "/", "SentiWord_Dict.txt"), delim = '\t', col_names = c("term", "score"))
head(senti_words_kr)


x <- duplicated(senti_words_kr$term)
senti_words_kr2 <- senti_words_kr[!x,]
senti_dic_kr <- SentimentDictionaryWeighted(words = senti_words_kr2$term, scores = senti_words_kr2$score)
senti_dic_kr <- SentimentDictionary(senti_words_kr2$term[senti_words_kr2$score > 0],
                                    senti_words_kr2$term[senti_words_kr2$score < 0])

summary(senti_dic_kr)

senti_words_kr$term[duplicated(senti_words_kr$term)]


res_sentiment <- analyzeSentiment(corp, #대신에 corpus,
                                  language = "korean",
                                  rules = list("KoreanSentiment" = list(ruleSentiment, senti_dic_kr)),
                                  removeStopwords = F, stemming = F)

df2 <- data.frame(round(res_sentiment, 3), dataInfo)

theme_set(theme_minimal(base_family = "AppleGothic"))
df3 <- df2 %>%
  dplyr::mutate(pos_neg = dplyr::if_else(KoreanSentiment >= 0, "PositiveTweet", "NegativeTweet")) %>%
  dplyr::select(pos_neg, everything())

dataL6 = df3 %>%
  dplyr::group_by(cnt, pos_neg) %>%
  dplyr::summarise(
    positCnt = n()
    , sumScore = sum(KoreanSentiment, na.rm = TRUE)
  ) %>%
  dplyr::filter(!is.na(pos_neg))

dplyr::tbl_df(dataL6)

dataL6Posit = dataL6 %>%
  dplyr::filter(pos_neg == "PositiveTweet")

dataL6Negat = dataL6 %>%
  dplyr::filter(pos_neg == "NegativeTweet")

dataL7 = dataL6Posit %>%
  dplyr::left_join(dataL6Negat, by = c("cnt" = "cnt")) %>%
  dplyr::filter(!is.na(pos_neg.y)) %>%
  dplyr::mutate(
    positRatio = (positCnt.x / (positCnt.x + positCnt.y)) * 100.0
    , total = sum(sumScore.x, sumScore.y, na.rm = TRUE)
  ) %>%
  dplyr::arrange(desc(total, positRatio)) %>%
  as.data.frame()


xlsx::write.xlsx2(
  dataL7
  , file = paste0(globalVar$outConfig, "/", "Glowpick_CrawlingL2.xlsx")
  , sheetName = "긍부정키워드4"
  , append = TRUE
  , row.names = FALSE
  , col.names = TRUE
)


# 통합 (상위 10%)
dataL8 = dataL7 %>%
  head(10) %>%
  dplyr::left_join(dataDtlInfo, by = c("cnt" = "cnt"))

# 키워드 빈도에 따른 시각화
dataL9 = dataL8 %>%
  dplyr::group_by(itemKor) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame()


xlsx::write.xlsx2(
  dataL9
  , file = paste0(globalVar$outConfig, "/", "Glowpick_CrawlingL2.xlsx")
  , sheetName = "긍부정키워드3"
  , append = TRUE
  , row.names = FALSE
  , col.names = TRUE
)

fig = wordcloud2::wordcloud2(data = dataL9)

# html로 내보내기
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# 내보낸 html 페이지로부터 png 형태로 불러와서 저장
webshot::webshot("fig.html", paste0(globalVar$figConfig, "/", "Glowpick_Crawling_Keyword2.png"), vwidth = 775, vheight = 550, delay = 10)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(ggplot2)
library(tidyverse)

data = readr::read_csv(file = paste0(globalVar$inpConfig, "/", "mort_table_2_2014_2018.csv"))

filterCol = c("Major Cities of Australia"
  , "Inner Regional Australia"
  , "Outer Regional Australia"
  , "Remote Australia"
  , "Very Remote Australia"
  , "Unknown/missing"
  , "Australia (total)"
)

dataL1 = data %>%
  dplyr::filter(geography %in% filterCol) %>%
  dplyr::select(geography, rate_ratio)

ggplot(dataL1, aes(x = rate_ratio, fill = as.factor(geography))) +
  geom_histogram(alpha = 0.5, position = "identity") +
  ggsave(filename = "FIG/o2job/Img_007.png", dpi = 600)

dataL2 = dataL1 %>%
  dplyr::group_by(geography) %>%
  dplyr::summarise(sumRateRatio = sum(rate_ratio, na.rm = TRUE))

ggplot(dataL2, aes(x = reorder(as.factor(geography), sumRateRatio, sum), y = sumRateRatio, fill = as.factor(geography))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sumRateRatio), vjust = 1.6, color = "white", size = 3.5) +
  labs(x = "geography", fill = "Sum RateRatio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = "FIG/o2job/Img_008.png", width = 10, height = 7, dpi = 600)

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

# library(ggplot2)
# library(tidyverse)
# library(vcfR)
# library(data.table)
# library(splitstackshape)
# library(stringr)
#
#
# data = read.table(file = paste0(globalVar$inpConfig, "/", "100T_merged_somatic_onlyFilterMutectCalls_PASS_step2.cosmic90.ann.hg19_multianno.vcf"), header = TRUE)
#
# tmpData1 = splitstackshape::concat.split(data = data, split.col = "INFO", sep = ";", drop = FALSE)
#
# colnames(tmpData1)
#
# # INFO_001_2
tmpData2 = splitstackshape::concat.split(data = tmpData1, split.col = "INFO_001", sep = "=", drop = FALSE)
#
#
# # INFO_003_2
tmpData3 = splitstackshape::concat.split(data = tmpData2, split.col = "INFO_003", sep = "=", drop = TRUE)
#
# dplyr::glimpse(tmpData3)
#

tmpData4 = splitstackshape::concat.split(data = tmpData3, split.col = "INFO_167", sep = "=", drop = TRUE)

tmpData3$INFO_167
#
# tmpData3$INFO_168
#
#
dataL1 = tmpData4

dataL1$INFO_167_1
#
# # COSMIC90_coding=. 아닌것
dataL2 = dataL1 %>%
  dplyr::filter(!stringr::str_detect(INFO_167_1, "COSMIC90_coding"))

stringr::str_detect(dataL1$INFO, "COSMIC90_coding=.")
#
# dataL1$INFO
#
#
# dataL2$INFO_167

# 요파일에서 헤더 빼고 8번째 column에서:
#     DP=>10
# AF <0.001 또는 AF =0, AF=. 인것 파일하나와
#
#
# COSMIC90_coding=. 아닌것
# COSMIC90_noncoding=. 아닌것
# 파일 하나 필터링 가능할가요?
#

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(ggplot2)
library(tidyverse)
library(caret)
library(readr)
library(Metrics)

data = readr::read_csv(file = paste0(globalVar$inpConfig, "/", "insurance.csv"))

# 1) 변수를 요약하시오.
# (Hint : Summary 함수 이용)

summary(data)

# 범주형 변수 처리
data$sex = as.factor(data$sex)
data$smoker = as.factor(data$smoker)
data$region = as.factor(data$region)

# 2) Train set과 Test set으로 데이터를 구분 하시오
# (Hint : CreateDataPartition함수 이용)

indexTrain = createDataPartition(data$charges, p = 0.7, list = FALSE)
trainData = data[indexTrain,]
testData = data[-indexTrain,]


# 3) lm()함수를 이용해 선형회귀모형을 수립하시오.
lmFit = lm(charges ~ ., data = trainData)

summary(lmFit)

# 4) Test set을 예측하고, MAE와 RMSE 값을 구하시오.
testData %>%
  dplyr::summarise(
    rmse = Metrics::rmse(charges, predict(lmFit, newdata = testData))
    , mae = Metrics::mae(charges, predict(lmFit, newdata = testData))
  )

# 5) BMI 지수와 흡연여부를 상호작용항으로 반영하여 모형을 다시 수립하시오.
lmFit = lm(charges ~ . + bmi * smoker, data = trainData)

# 6) 5)의 모형의 MAE와 RMSE 값을 구해 성능 개선 여부를 판단하시오.

# 성능 개선
testData %>%
  dplyr::summarise(
    rmse = Metrics::rmse(charges, predict(lmFit, newdata = testData))
    , mae = Metrics::mae(charges, predict(lmFit, newdata = testData))
  )

# 7) 각 변수에 대해 계수값을 해석하시오.
# Ex) A 변수가 한 단위 증가하면 보험료는 얼마나 오른다/내린다.

lmFit = lm(charges ~ . + (bmi * sex), data = trainData)
summary(lmFit)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(tidyverse)
library(tools)
library(fs)

# 3번사진의 문제2번을 풀어야하는데
fileList = Sys.glob(paste0(globalVar$inpConfig, "/", "literacy_*.csv"))

data = data.frame()
for (fileInfo in fileList) {
  fileName = tools::file_path_sans_ext(fs::path_file(fileInfo))

  tmpData = readr::read_csv(file = fileInfo) %>%
    tidyr::gather(-country, key = "year", value = "val") %>%
    dplyr::mutate(fileName = fileName) %>%
    na.omit()

  data = dplyr::bind_rows(data, tmpData)
}

dataL1 = data %>%
  tidyr::spread(key = "fileName", value = "val")

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

# 1. R Stduido 작업환경 설정
# - 바탕화면에서 Test 작업환경 구축
#
# 2. Java 설치 확인
# - cmd 창에서 java 버전 확인 (java -version)
#
# 3. Java 환경변수 확인
# - 현재 미 설정
# - 일반적으로 KoNLP 라이브러리 경우 java 1.8에서 수행 가능
# - 따라서 Java 다운로드 수행
#
# 4. Java 1.8 다운로드
# - https://www.oracle.com/java/technologies/javase/javase-jdk8-downloads.html
# - jdk-8u261-windows-x64.exe
# - Open JRE가 아니라 Oracle Java 사용 권장
# - 관리자 권한으로 실행
# - JRE 및 JDK를 순차적으로 설치
# - cmd 창에서 java 버전 확인 (java -version)
# C:\Users\inu>java -version
# java version "1.8.0_261"
# Java(TM) SE Runtime Environment (build 1.8.0_261-b12)
# Java HotSpot(TM) 64-Bit Server VM (build 25.261-b12, mixed mode)

# 5. Java 환경변수 설정
# - JAVA_HOME : C:\Program Files\Java\jdk1.8.0_261
# - PATH : C:\Program Files\Java\jdk1.8.0_261\bin
# - cmd창에서 환경변수 확인 (echo %JAVA_HOME%)

# 6. Github에서 설치할 수 있는 라이브러리 설치 및 읽기
# - # install.packages("remotes")
# - library(remotes)

# 7. 의존성 라이브러리 설치
# - install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"), type = "binary")
# - # install.packages("remotes")
# - library(remotes)
# - library(devtools)
# - # install.packages("rmarkdown")
# - library(rmarkdown)
# - # install.packages("RSQLite")
# - library(RSQLite)

# 8. rJava 읽기
# - library(rJava)

# 9. KoNLP 설치 및 읽기
# - # remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
# - library(KoNLP)

# 10. useNIADic() 읽기
# - # devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE, force = TRUE)
# - useNIADic()

# 11. 설치 완료


#=====================================================
# KoNLP 라이브러리 (국문) 설치
#=====================================================
# 13행 에러 발생 시 조치 방법
# 1) 11행에서 install_github를 통해 재 설치
# 2) Rstudio 종료 후 Test.Rproj 재 실행
# 3) 13행 정상적으로 수행

# install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"), type = "binary")

remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts = c("--no-multiarch"), force = TRUE)
library(KoNLP)

# devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE, force = TRUE)
useNIADic()

# install.packages("remotes")
library(remotes)
library(devtools)

# install.packages("rmarkdown")
library(rmarkdown)

# install.packages("RSQLite")
library(RSQLite)

library(rJava)


#=====================================================
# RcppMeCab 라이브러리 (국/영문) 수행
#=====================================================
# 라이브러리 읽기
library(RcppMeCab)
library(utf8)
library(RmecabKo)
library(ggplot2)
library(tidyverse)
library(xlsx)
library(wordcloud2)
library(htmlwidgets)
library(webshot)
library(readr)
library(foreach)

# RmecabKo::install_mecab("c:/mecab")

# fileList = Sys.glob("review.txt")
fileList = Sys.glob("review_utf8.txt")

data = readr::read_lines(file = fileList)

head(data)

i = 3
dataL2 = data.frame()
foreach::foreach(i = 1:length(data), .combine = c) %do% {

  if (data[i] != "") {
    dataL1 = RcppMeCab::pos(as_utf8(data[i]), format = "data.frame") %>%
      dplyr::filter(pos == "NNG") %>%
      dplyr::select(token)

    dataL2 = dplyr::bind_rows(dataL2, dataL1)
  }
}

dplyr::tbl_df(dataL2)

dataL3 = dataL2 %>%
  # dplyr::filter(! token %in% c("영화", "연기", "배우")) %>%
  dplyr::group_by(token) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame()

dplyr::tbl_df(dataL3)


# 출력
xlsx::write.xlsx2(dataL3, file = paste0("XLSX/review_utf8_", format(Sys.time(), "%Y%m%d%H%M%S"), ".xlsx"), append = FALSE, row.names = FALSE, col.names = TRUE)

fig = wordcloud2::wordcloud2(data = dataL3)
fig

# html로 내보내기
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# 내보낸 html 페이지로부터 png 형태로 불러와서 저장
webshot::webshot("fig.html", paste0("FIG/review_utf8_", format(Sys.time(), "%Y%m%d%H%M%S"), ".png"), vwidth = 775, vheight = 550, delay = 10)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================
# 1. (25점)   에 포함된 소인수(prime factor) 2의 개수를 산출하는 R 사용자 함수를 만들어
# 제시하라 (  에는 2가 8개 있다).   를 소인수 분해하면 2가 몇 개 있는가

library(schoolmath)

primeFactor = function(n) {

  data(primlist)
  dummy <- 2
  end <- 0
  faclist <- 0
  test <- is.prim(n)
  if (test == TRUE) {
    # msg <- cat(n, "is a prime!\n")
    return(NULL)
  }
  while (end == 0) {
    prim <- primlist[dummy]
    if (prim > n) {
      end <- 1
    }
    else {
      test <- n / prim
      test2 <- is.whole(test)
      if (test2 == TRUE) {
        faclist <- c(faclist, prim)
        test3 <- is.prim(test)
        if (test3 == TRUE) {
          end <- 1
          faclist <- c(faclist, test)
        }
        else {
          n <- test
          dummy <- 1
        }
      }
      dummy <- dummy + 1
    }
  }
  faclist <- faclist[-1]
  return(faclist)
}

findPrimeFactorCnt = function(num) {
  total = 1

  for (i in 1:num) {
    total = total * i
  }

  ind = which(primeFactor(total) == 2)

  return(length(ind))
}

findPrimeFactorCnt(num = 10)
findPrimeFactorCnt(num = 100)

# 2. (25점) 대한민국 국회는 총 300석으로, A, B, C, D, E 당의 지역구⋅비례대표 수가 각각
# (161, 13), (84, 19), (1, 5), (0, 3), (7, 7) 명이다. 30명을 비복원 임의추출하여 특별위원
# 회를 구성하는 경우 위원회의 정당 및 지역구⋅비례대표 구성표를 5*2 표로 제시하라.
# * sample() 함수를 사용. 출력은 임의적이다.

data = data.frame()
data = dplyr::bind_rows(
  data
  , data.frame(rowType = "A", colType = "지역구", val = seq(1, 161))
  , data.frame(rowType = "B", colType = "지역구", val = seq(1, 84))
  , data.frame(rowType = "C", colType = "지역구", val = seq(1, 19))
  , data.frame(rowType = "E", colType = "지역구", val = seq(1, 7))
  , data.frame(rowType = "A", colType = "비례대표", val = seq(1, 13))
  , data.frame(rowType = "B", colType = "비례대표", val = seq(1, 19))
  , data.frame(rowType = "C", colType = "비례대표", val = seq(1, 5))
  , data.frame(rowType = "D", colType = "비례대표", val = seq(1, 3))
  , data.frame(rowType = "E", colType = "비례대표", val = seq(1, 7))
)

table(data$rowType, data$colType)

ind = sample(nrow(data), size = 30, replace = FALSE)
dataL1 = data[ind,]

table(dataL1$rowType, dataL1$colType)


# 3. (25점)  개 요소의 벡터     ⋯   에서  개 요소를 복원 임의추출하는 기능의 R
# 사용자 함수를 만들어라. 그 함수를 써서   에서 5개를 뽑아 출력을 제시하라 (2회 반복).
# * 물론 sample() 함수를 사용할 수 없다.

printSample = function(n, m) {
  data = runif(n)

  # cat("data : ", data, "\n")

  for (i in 1:2) {
    ind = ceiling(runif(5, min = 1, max = 10))
    dataL1 = data[ind]
    cat("dataL1 : ", dataL1, "\n")
  }
}


n = 1:10
m = 5

printSample(n, m)


# 4. (25점) 다음은 윌콕슨 검정과 관련된 R 스크립트와 출력이다.
# > x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
# > y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.28, 1.07, 3.14, 1.29)
# > wilcox.test(x, y, paired = FALSE, alternative = "greater")
# Wilcoxon rank sum test
# data: x and y
# W = 58, p-value = 0.06796
# alternative hypothesis: true location shift is greater than 0
# > wilcox.test(x, y, paired = TRUE, alternative = "greater")
# Wilcoxon signed rank test
# data: x and y
# V = 40, p-value = 0.01953
# alternative hypothesis: true location shift is greater than 0
# paired가 F인 경우의 Wilcoxon rank sum 통계량  와 T인 경우의 Wilcoxon signed
# rank 통계량  를 산출하는 R 스크립트를 제시하라 (물론, 같은 값이 출력되어야 옳다).

x = c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y = c(0.878, 0.647, 0.598, 2.05, 1.06, 1.28, 1.07, 3.14, 1.29)

wilcoxTestPairFalse = wilcox.test(x, y, paired = FALSE, alternative = "greater")
cat("W : ", wilcoxTestPairFalse$statistic, "\n")

wilcoxTestPairTrue = wilcox.test(x, y, paired = TRUE, alternative = "greater")
cat("V : ", wilcoxTestPairTrue$statistic, "\n")

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

# [요구사항]
# 집에서밥을먹는다.
# 집에서 밥을 먹는다.

library(tidyverse)
library(reticulate)
library(stringr)
library(officer)
library(magrittr)
library(fs)
library(flextable)

# 한글 (HWP)를 텍스트 (txt)로 변환
# https://cloudconvert.com/hwp-to-txt

#===============================
# R에서 Anaconda3 불러오기
#===============================
# 환경변수 설정
if (.Platform$OS.type == "windows") {
  Sys.setenv(RETICULATE_PYTHON = 'C:/ProgramData/Anaconda3/python.exe')
  Sys.setenv(PATH = paste("C:/ProgramData/Anaconda3/Library/bin", Sys.getenv()["PATH"], sep = ";"))
}

reticulate::py_discover_config()

reticulate::conda_list()
# name                                 python
# 1 Anaconda3 C:\\ProgramData\\Anaconda3\\python.exe

# 임시 conda 삭제
# reticulate::conda_remove("PyCharm")

reticulate::py_config()
# python:         C:/ProgramData/Anaconda3/python.exe
# libpython:      C:/ProgramData/Anaconda3/python37.dll
# pythonhome:     C:/ProgramData/Anaconda3
# version:        3.7.8 | packaged by conda-forge | (default, Jul 31 2020, 01:53:57) [MSC v.1916 64 bit (AMD64)]
# Architecture:   64bit
# numpy:          C:/ProgramData/Anaconda3/Lib/site-packages/numpy
# numpy_version:  1.18.5

# reticulate::use_python("C:\\ProgramData\\Anaconda3\\python.exe", required = TRUE)

# 라이브러리 읽기
# from pykospacing import spacing
pykospacing = reticulate::import("pykospacing")

# pykospacing$spacing("김형호영화시장분석가는'1987'의네이버영화정보네티즌10점평에서언급된단어들을지난해12월27일부터올해1월10일까지통계프로그램R과KoNLP패키지로텍스트마이닝하여분석했다.")
pykospacing$spacing(stringr::str_remove_all("친애하는 지도자동지께서 주체의 사회주의경제관리리론 전반을  관통하고있는 기본원리를 새롭 게 정식 화 히 심 으 로 써 주체 의 사회주의경제 관리 리론이 의거하고있는 사상리론적 , 방법론적  기초가 뚜렷이 밝혀지게 되였으며 이 기본원리에 의거하여 사회주의경제관리리론을  더욱 과학적으로 체계 화할 수 있 게 되 였 다", " "))

#===============================
# 주 프로그램 수행
#===============================
# inpConfig :  chr "E:/02. 블로그/지식iN/INPUT/o2job"
# fileList = Sys.glob(paste0(globalVar$inpConfig, "/DOC/*.txt"))
# fileList = Sys.glob(paste0(globalVar$inpConfig, "/DOC/1986년-1992년.txt"))
fileList = Sys.glob(paste0(globalVar$inpConfig, "/DOC/1993-2001.txt"))

for (fileInfo in fileList) {
  data = readr::read_lines(file = fileInfo) %>%
    as.tibble() %>%
    dplyr::filter(!stringr::str_length(value) == 0) %>%
    as.tibble()

  dataL1 = tibble()
  for (i in 1:nrow(data)) {
    result = pykospacing$spacing(stringr::str_remove_all(data[i,]$value, " ")) %>%
      as.tibble()

    dataL1 = dplyr::bind_rows(dataL1, result)
  }

  #===============================
  # 워드 생산
  #===============================
  # "D:/02. 블로그/지식iN/OUTPUT/o2job/1986년.docx"
  outFile = paste(globalVar$outConfig, stringr::str_replace_all(fs::path_file(fileInfo), ".txt", "_1.docx"), sep = "/")

  doc = officer::read_docx() %>%
    officer::body_add_par(value = paste(dataL1[1:20000,]$value, "(LineBreak)(LineBreak)", collapse = ""))
  print(doc, target = outFile)

  outFile = paste(globalVar$outConfig, stringr::str_replace_all(fs::path_file(fileInfo), ".txt", "_2.docx"), sep = "/")

  doc = officer::read_docx() %>%
    officer::body_add_par(value = paste(dataL1[20000:nrow(dataL1),]$value, "(LineBreak)(LineBreak)", collapse = ""))

  print(doc, target = outFile)


  # 워드에서 변경
  # Ctrl + H
  # "(LineBreak)(LineBreak)" to "^p" 치환
}


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(RcppMeCab)
library(utf8)
library(RmecabKo)
library(ggplot2)
library(tidyverse)
library(xlsx)
library(wordcloud2)
library(htmlwidgets)
library(webshot)
library(readr)
library(foreach)
library(ggwordcloud)
library(reticulate)

# RmecabKo::install_mecab("c:/mecab")

#===============================
# R에서 Anaconda3 불러오기
#===============================
# 환경변수 설정
if (.Platform$OS.type == "windows") {
  Sys.setenv(RETICULATE_PYTHON = 'C:/ProgramData/Anaconda3/python.exe')
  Sys.setenv(PATH = paste("C:/ProgramData/Anaconda3/Library/bin", Sys.getenv()["PATH"], sep = ";"))
}


fileList = Sys.glob(paste0(globalVar$inpConfig, "/DOC/파일_*.txt"))

dataL2 = data.frame()

for (fileInfo in fileList) {
  data = readLines(file(fileInfo, encoding = "EUC-KR")) %>%
    as.tibble() %>%
    dplyr::filter(!stringr::str_length(value) == 0) %>%
    as.tibble()

  foreach::foreach(i = 1:nrow(data), .combine = c) %do% {
    dataL1 = RcppMeCab::pos(as_utf8(data[i,]$value), format = "data.frame") %>%
      dplyr::filter(pos == "NNG") %>%
      dplyr::select(token)

    dataL2 = dplyr::bind_rows(dataL2, dataL1)
  }
}

dplyr::tbl_df(dataL2)

dataL3 = dataL2 %>%
  # dplyr::filter(! token %in% c("영화", "연기", "배우")) %>%
  dplyr::group_by(token) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame()

dplyr::tbl_df(dataL3)

dataL3 = read.xlsx(file = paste(globalVar$outConfig, "DOC_Keyword.xlsx", sep = "/"), sheetName = "Sheet1", encoding = "UTF-8")

# 엑셀 출력
# xlsx::write.xlsx2(dataL3, file = paste(globalVar$outConfig, "DOC_Keyword.xlsx", sep = "/"), append = FALSE, row.names = FALSE, col.names = TRUE)

fig = wordcloud2::wordcloud2(data = dataL3, size = 2)

# html로 내보내기
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# 내보낸 html 페이지로부터 png 형태로 불러와서 저장
webshot::webshot("fig.html", paste(globalVar$figConfig, "DOC_Keyword.png", sep = "/"), delay = 10)

dataL4 = dataL3 %>%
  dplyr::slice(1:100)

# ggsave를 통해 이미지 저장
ggwordcloud::ggwordcloud2(dataL4[, c("token", "freq")], size = 2) +
  # theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  ggsave(filename = paste(globalVar$figConfig, "DOC_Keyword2.png", sep = "/"), width = 10, height = 8, dpi = 600)


wordcloud = reticulate::import("wordcloud")
plt = reticulate::import("matplotlib.pyplot")
np = reticulate::import("numpy")

r_to_py

# (dict_a <- reticulate::r_to_py(x = list(a = 1, b = 2)))
(dict_c <- reticulate::py_dict(key = c("a", "b"), values = c(1, 2)))
(dict2 <- reticulate::py_dict(kk = dict(zip(dataL4$token, dataL4$freq))))

df_py <- r_to_py(dataL4)

wc = wordcloud$WordCloud(width = 1000, height = 800, background_color = "white")
wc = wc$generate_from_frequencies(dict_c)


plt$imshow(wc, interpolation = "bilinear")
plt.axis("off")
savefigName = contextPath + '/../resources/image/Image_01.png'
plt.savefig(savefigName, width = 1000, heiht = 1000, dpi = 600, bbox_inches = 'tight')
plt.show()


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(MASS)
library(moonBook)
library(webr)
library(ggplot2)

# 모기 살충제의 50% 치사량을 구하기 위해 용량에 따른 사망자를 측정하여 다음 자료를 얻었다.
# 이 자료로 R을 이용하여 로짓 분석을 하여 모형을 구하고 모기 50 %를 죽일 수 있는 치사량 LD50을 구하라

data = data.frame(
  dose = c(0.5, 1.0, 2.0, 5.0)
  , total = c(81, 84, 82, 80)
  , dead = c(11, 40, 66, 80)
  , deadRatio = c(13.6, 47.6, 80.5, 100.0)
)

glmModel = glm(log(dose) ~ cind(total - dead, dead), family = binomial(link = logit), data = data)
summary(glmModel)  # p-value almost agree for the b parameter

# 50% 치사량 사망률 설정 (p)
xp = MASS::dose.p(glmModel, p = 0.50)

# 예측 모기약 농도 (estVal)
estVal = exp(cbind(xp))
estVal

# 즉 사망률 0.50의 경우 예측 모기약 농도 1.0527입니다.

# A와 B 두 약의 심장 박동에 대한 효과를 비교하기 위하여 시험대상자를 4명씩 랜덤하게 두 군으로 나누고
# 투약 후 5분 경과 시점부터 시작하여 10분 간격으로 5회에 걸쳐 심장박동수를 계측한 결과이다.

aData = data.frame(
  type1 = c(72, 72, 73, 76, 78)
  , type2 = c(75, 74, 78, 79, 81)
  , type3 = c(73, 74, 77, 80, 82)
  , type4 = c(70, 72, 74, 74, 77)
)

bData = data.frame(
  type1 = c(74, 76, 77, 80, 79)
  , type2 = c(78, 81, 81, 83, 80)
  , type3 = c(69, 71, 71, 73, 75)
  , type4 = c(80, 81, 84, 87, 83)
)

dataL1 = dplyr::bind_rows(
  data.frame(aData, type = "A")
  , data.frame(bData, type = "B")
)

dataL2 = dataL1 %>%
  tidyr::gather(-type, key = "key", value = "val")

boxplot(val ~ type, data = dataL2)

# P값이 0.47883으로서 귀무가설 기각하지 못함 (두 약의 분산 차이가 없다)
# 따라서 등분산 조건 (var.equal = TRUE)
fTest = var.test(val ~ type, data = dataL2)
fTest

plot(fTest) +
  xlim(0, 5) +
  ggsave(filename = paste(globalVar$figConfig, "fTest.png", sep = "/"), width = 10, height = 6, dpi = 600)

# P값이 0.054로서 귀무가설 기각 (두 약의 심장 박동은 차이가 있다)
tTest = t.test(val ~ type, data = dataL2, var.equal = TRUE)
tTest

plot(tTest) +
  xlim(-5, 5) +
  ggsave(filename = paste(globalVar$figConfig, "tTest.png", sep = "/"), width = 10, height = 6, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(readr)
library(tidyverse)
library(lubridate)
library(xlsx)

fileList = Sys.glob(paste(globalVar$inpConfig, "origin.csv", sep = "/"))

data = readr::read_csv(file = fileList, locale = locale("ko", encoding = "EUC-KR"))

# 날짜, 서비스,  유저, `방문 시간(분)`, `방문 일수(일)`
colnames(data) = c("date", "service", "user", "visitMin", "visitDay")

dataL1 = na.omit(data)

dataL2 = dataL1 %>%
    dplyr::mutate(
        sDate = as.character(date)
        , dtDate = readr::parse_date(sDate, "%Y%m")
        , dtMonth = lubridate::month(dtDate)
    )

# 11년 전체 서비스에 대한 월별 유저 수 / 월별 방문일수 / 월별 방문 시간
dataL3 = dataL2 %>%
    dplyr::group_by(dtMonth) %>%
    dplyr::summarise(
        cnt = n()
        , sumVisitHour = sum(visitMin, na.rm = TRUE) / 60.0
        , sumVisitDay = sum(visitDay, na.rm = TRUE)
    ) %>%
    as.data.frame()

dplyr::tbl_df(dataL3)

xlsx::write.xlsx2(dataL3, file = paste(globalVar$outConfig, "Web_Visitor2.xlsx", sep = "/"), sheetName = "s01", append = TRUE, row.names = FALSE, col.names = TRUE)



# 11년 서비스별 : 월별 유저 수 / 월별 방문일수 / 월별 방문 시간
dataL3 = dataL2 %>%
    dplyr::group_by(service, dtMonth) %>%
    dplyr::summarise(
        cnt = n()
        , sumVisitHour = sum(visitMin, na.rm = TRUE) / 60.0
        , sumVisitDay = sum(visitDay, na.rm = TRUE)
    ) %>%
    dplyr::arrange(service, dtMonth) %>%
    as.data.frame()

dplyr::tbl_df(dataL3)
xlsx::write.xlsx2(dataL3, file = paste(globalVar$outConfig, "Web_Visitor2.xlsx", sep = "/"), sheetName = "s02", append = TRUE, row.names = FALSE, col.names = TRUE)

# 서비스 중 어떤 것이 가장 방문 일수가 작은가
dataL3 = dataL2 %>%
    dplyr::group_by(service) %>%
    dplyr::summarise(
        cnt = n()
        , sumVisitHour = sum(visitMin, na.rm = TRUE) / 60.0
        , sumVisitDay = sum(visitDay, na.rm = TRUE)
    ) %>%
    dplyr::arrange(sumVisitDay) %>%
    as.data.frame()

dplyr::tbl_df(dataL3)
xlsx::write.xlsx2(dataL3, file = paste(globalVar$outConfig, "Web_Visitor2.xlsx", sep = "/"), sheetName = "s03", append = TRUE, row.names = FALSE, col.names = TRUE)

# 서비스 중 특정 시즌에 유저가 많다든가 적은 (시즌을 타는) 서비스가 있는가
dataL3 = dataL2 %>%
    dplyr::mutate(type = dplyr::case_when(
        3 <= dtMonth & dtMonth <= 5 ~ "spring"
        , 6 <= dtMonth & dtMonth <= 8 ~ "summer"
        , 9 <= dtMonth & dtMonth <= 11 ~ "fall"
        , 12 <= dtMonth | dtMonth <= 2 ~ "winte"
        , TRUE ~ "NULL"
    )) %>%
    dplyr::group_by(type, service) %>%
    dplyr::summarise(
        cnt = n()
        , sumVisitHour = sum(visitMin, na.rm = TRUE) / 60.0
        , sumVisitDay = sum(visitDay, na.rm = TRUE)
    ) %>%
    dplyr::arrange(sumVisitDay) %>%
    as.data.frame()
xlsx::write.xlsx2(dataL3, file = paste(globalVar$outConfig, "Web_Visitor2.xlsx", sep = "/"), sheetName = "s04", append = TRUE, row.names = FALSE, col.names = TRUE)

dplyr::tbl_df(dataL3)


# 충성도 고객이 높은 서비스가 어떤 것인가? 
#	* 충성도 높은 고객 : (방문 일수가 30 이상 AND 방문 시간 500분 이상으로 가정)
dataL3 = dataL2 %>%
    dplyr::filter(
        visitDay >= 30
        , (visitMin / 60) >= 500
    ) %>%
    dplyr::group_by(service) %>%
    dplyr::summarise(
        cnt = n()
        , meanVisitHour = mean(visitMin, na.rm = TRUE) / 60.0
        , meanVisitDay = mean(visitDay, na.rm = TRUE)
    ) %>%
    dplyr::arrange(desc(cnt)) %>%
    as.data.frame()
xlsx::write.xlsx2(dataL3, file = paste(globalVar$outConfig, "Web_Visitor2.xlsx", sep = "/"), sheetName = "s05", append = TRUE, row.names = FALSE, col.names = TRUE)

dplyr::tbl_df(dataL3)

# 충성도가 낮은 고객이 많은 서비스가 어떤 것인가 ?
#	* 충성도가 낮은 고객 : (방문일수 5이하 OR 방문 시간 10 이하로 가정)
dataL3 = dataL2 %>%
    dplyr::filter(
        visitDay <= 5
        , (visitMin / 60) <= 10
    ) %>%
    dplyr::group_by(service) %>%
    dplyr::summarise(
        cnt = n()
        , meanVisitHour = mean(visitMin, na.rm = TRUE) / 60.0
        , meanVisitDay = mean(visitDay, na.rm = TRUE)
    ) %>%
    dplyr::arrange(desc(cnt)) %>%
    as.data.frame()
xlsx::write.xlsx2(dataL3, file = paste(globalVar$outConfig, "Web_Visitor2.xlsx", sep = "/"), sheetName = "s06", append = TRUE, row.names = FALSE, col.names = TRUE)

# 유저가 중복되는 것이 많은 서비스가 어떤 것인가?
#	ex ) 서비스 1의 유저 1111
#	      서비스 3의 유저 1111    > 1111이라는 동일 유저가 1, 3 서비스를 중복으로 이용하고 있다.

# 서비스에 따라 중복 개수
dataL3 = dataL2 %>%
    dplyr::group_by(service) %>%
    dplyr::summarise(
        cnt = n()
        , distServiceCnt = n_distinct(service)
        , distUserCnt = n_distinct(user)
        , meanVisitMin = mean(visitMin, na.rm = TRUE) / 60.0
        , meanVisitHour = mean(visitDay, na.rm = TRUE)
    ) %>%
    dplyr::arrange(desc(distUserCnt)) %>%
    as.data.frame()
xlsx::write.xlsx2(dataL3, file = paste(globalVar$outConfig, "Web_Visitor2.xlsx", sep = "/"), sheetName = "s07", append = TRUE, row.names = FALSE, col.names = TRUE)

dataL3

# 유저에 따라 중복 개수
dataL3 = dataL2 %>%
    dplyr::group_by(user) %>%
    dplyr::summarise(
        cnt = n()
        , distServiceCnt = n_distinct(service)
        , distUserCnt = n_distinct(user)
        , meanVisitMin = mean(visitMin, na.rm = TRUE) / 60.0
        , meanVisitHour = mean(visitDay, na.rm = TRUE)
    ) %>%
    dplyr::arrange(desc(distServiceCnt)) %>%
    as.data.frame()
xlsx::write.xlsx2(dataL3, file = paste(globalVar$outConfig, "Web_Visitor2.xlsx", sep = "/"), sheetName = "s08", append = TRUE, row.names = FALSE, col.names = TRUE)

dataL3


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(dplyr)

data = data.frame(
    height = c(185, 166, 172, 180, 163, 170, 177)
    , weight =  c(80, 73, 72, 100, 72, 67, 75)
)

dataL1 = data %>%
    dplyr::mutate(bmi = (weight / height^2) * 10000.0) %>%
    dplyr::filter(bmi > 26) %>%
    dplyr::select(weight)

dataL1

data2 = data.frame(
    radius = c(1, 2.5, 3, 4.5, 5, 6.7)
    , height =  c(2, 4, 6, 8, 10, 12)
)

data2L1 = data2 %>%
    dplyr::mutate(
        area = pi * (radius^2)
        , volume = pi * (radius^2) * height
    )

data2L1

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(ggplot2)
library(tidyverse)

data = ggplot2::midwest

# 1.(10점) popadults는 해당 지역의 성인 인구(number of adults), poptotal은 전체 인구(total population)를 나타냅니다. midwest 데이터에 '전체 인구 대비 미성년 인구 백분율' 변수(child_ratio)를 추가하시오.

dataL1 = data %>% 
    dplyr::mutate(child_ratio = (popadults / poptotal) * 100)

dataL1

# 2.(10점) 미성년 인구 백분율이 가장 높은 상위 10개 county(지역)의 미성년 인구 백분율을 출력하시오.
dataL1 %>%
    dplyr::arrange(desc(child_ratio)) %>%
    dplyr::top_n(10) %>%
    dplyr::select(county, child_ratio)


# 3.(10점) 다음 분류표의 기준에 따라 미성년비율 등급 변수(grade)를 추가하고, 각 등급에 몇 개의 지역이 있는지 출력하시오.
dataL1 %>%
    dplyr::mutate(grade = dplyr::case_when(
        child_ratio >= 45.0 ~ "large"
        , 30.0 <= child_ratio & child_ratio < 45.0 ~ "middle"
        , child_ratio < 30.0 ~ "small"
        , TRUE ~ "NA"
    )) %>%
    dplyr::group_by(grade) %>%
    dplyr::summarise(n = n())

# 4.(10점) popasian은 해당 지역의 아시아인 인구를 나타냅니다. '전체 인구 대비 아시아인 인구 백분율' 변수(asian_ratio)를 추가하고, 하위 5개 지역의 state(주), county(지역명), 아시아인 인구 백분율(asian_ratio)을 출력하시오.

data %>% 
    dplyr::mutate(asian_ratio = (popasian / poptotal) * 100) %>%
    dplyr::arrange(asian_ratio) %>%
    dplyr::top_n(-5) %>%
    dplyr::select(state, county, asian_ratio)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(readr)
library(tidyverse)

# "E:/04. 재능플랫폼/Github/TalentPlatform-R/INPUT/o2job 서울시 코로나19 확진자 현황.csv"
fileList = Sys.glob(paste(globalVar$inpConfig, "서울시 코로나19 확진자 현황.csv", sep = "/"))
data = readr::read_csv(file = fileList, locale = locale("ko", encoding = "EUC-KR"))


# 파이차트 작성
dataL1 = data %>%
  dplyr::mutate(type = dplyr::case_when(
    stringr::str_detect(접촉력, "해외") & stringr::str_detect(접촉력, "해외") ~ "해외"
    , stringr::str_detect(접촉력, "확인중") ~ "확인중"
    , TRUE ~ "국내"
  )) %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(cnt = n())

label = paste0(dataL1$type, " ", round((dataL1$cnt / sum(dataL1$cnt, na.rm = TRUE) * 100)), "%")
pie(dataL1$cnt, labels = label, col=rainbow(5), main="국내외 접촉력 비율")

# 바차트 작성
dataL2 = data %>%
  dplyr::group_by(지역) %>%
  dplyr::summarise(cnt = n()) %>%
  dplyr::arrange(desc(cnt)) %>%
  dplyr::top_n(5)

barplot(dataL2$cnt, names.arg = dataL2$지역, col = rainbow(5), main = "많이 발생한 구")


# 그룹화된 바차트 작성
dataL3 = data %>%
  dplyr::filter(지역 %in% c("서대문구", "동작구", "마포구")) %>%
  dplyr::mutate(type = dplyr::case_when(
    상태 == "사망" ~ "사망"
    , 상태 == "퇴원" ~ "퇴원"
    , TRUE ~ "투병"
  ))

count = table(dataL3$type, dataL3$지역)
barplot(count, main="3개 구의 환자상태", col=rainbow(3), legend = rownames(count))

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(xlsx)
library(MASS)
library(ROCR)
library(abdiv)
library(xlsx)
library(tidyverse)
library(stringr)
library(hablar)

fileList = Sys.glob(paste(globalVar$inpConfig, "20201018(자료)통영_취합완성본_전1018.xlsx", sep = "/"))


data = xlsx::read.xlsx2(file = fileList, sheetIndex = 1)


# 결측값 확인 
dataNA = data %>%
  readr::type_convert() %>%
  dplyr::select(everything()) %>%
  dplyr::summarise_all(funs(sum(is.na(.)))) %>%
  tidyr::gather(key = "key", value = "val") %>%
  dplyr::filter(val > 0)

paste(dataNA$key, collapse = ", ")

# 엑셀 파일에서 NA값을 제거
dataL1 = data %>%
    dplyr::select(- c( "ID", "취미_어떤", "행태_술빈도", "행태_술량", "대화_총개수", "소통_총개수", "행태_담배년", "행태_담배갑", "행태_담배갑년", "행태_담배갑년gr", "행태_술빈도", "행태_술량", "행태_운동종류", "행태_교육종류", "병력_고혈압나이", "병력_고혈압기간", "병력_고혈압20년yn", "병력_당뇨나이", "병력_골다공증", "병력_골다공증나이", "병력_치주나이", "병력_머리사고나이", "병력_마취나이", "병력_총개수", "관리_총개수", "관리_치매어렵이유", "관리_치매어렵이유123", "변화_총개수")) %>%
    readr::type_convert() %>%
    na.omit()

# 자료형 변환
dataL2 = dataL1 %>% 
  # dplyr::select(진단_yn, dplyr::contains("yn")) %>%
  dplyr::select(진단_yn, dplyr::contains("인지")) %>%
  hablar::convert(fct(contains("yn")))

# "0=대조군
# 1=치매이행군"

#=====================================================================
# 유의미 변수 선택
#=====================================================================
# Initial Model:
#     진단_yn ~ 나머지 변수
#
# Final Model:
#     진단_yn ~ 나이 + 난이도 + 직급 + 근속년수 + 승진후.지난.시간

# 전체 변수에 대한 로지스틱 회귀모형 수행
# 독립변수 : 진단_yn 제외한 전체 변수
# 종속변수 : 진단_yn
glmFitVarAll = glm(진단_yn ~ ., data = dataL2, family = binomial)
summary(glmFitVarAll)

# 1) 기본값으로 변수 선택
# stepRes = step(glmFitVarAll)
summary(stepRes)

# 1) AIC 기준으로 변수 선택
stepAic = MASS::stepAIC(glmFitVarAll, direction = "both")

# 결과에 대한 요약
summary(stepAic)

# predict.glm(glmFitVarAll, type='response')

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(ggplot2)
library(tidyverse)
library(forcats)

data = data.frame(
  group = c("정상군", "정상군", "치매이행군", "치매이행군", "정상군", "정상군", "치매이행군", "치매이행군", "정상군", "정상군", "정상군", "치매이행군", "치매이행군", "치매이행군", "정상군", "정상군", "치매이행군", "치매이행군", "정상군", "정상군", "치매이행군", "치매이행군", "정상군", "정상군", "치매이행군", "치매이행군")
  , variable = c("남자", "여자", "남자", "여자", "70대 이하", "80대 이상", "70대 이하", "80대 이상", "무학/초등", "중고등", "대학이상", "무학/초등", "중고등", "대학이상", "없음", "있음", "없음", "있음", "의료급여", "건강보험", "의료급여", "건강보험", "민간병원", "보건소", "민간병원", "보건소")
  , value = c(30, 70, 24, 76, 49, 51, 44, 56, 78, 20, 2, 82, 16, 2, 42, 58, 52, 48, 19, 81, 24, 76, 92, 8, 90, 10)
  , type = c("성별", "성별", "성별", "성별", "나이", "나이", "나이", "나이", "교육", "교육", "교육", "교육", "교육", "교육", "직업(과거)", "직업(과거)", "직업(과거)", "직업(과거)", "의료보장", "의료보장", "의료보장", "의료보장", "주 관리기관", "주 관리기관", "주 관리기관", "주 관리기관")
)

# type 정렬
data$type = forcats::fct_relevel(data$type, c("성별", "나이", "교육", "직업(과거)", "의료보장", "주 관리기관"))

# variable 정렬
data$variable = forcats::fct_relevel(data$variable,  c("남자", "여자","70대 이하", "80대 이상", "무학/초등", "중고등", "대학이상", "없음", "있음", "의료급여", "건강보험", "민간병원", "보건소"))

ggplot(data, aes(x = variable, y = value, fill = group, label=round(value,1))) +
  geom_bar(position = "dodge", stat="identity") +
  theme(legend.position = "top") +
  geom_text(aes(group=group),position=position_dodge(width=0.9), size=5,vjust=-0.5,hjust=0.5) +
  ylim(0, 100) +
  facet_wrap( ~ type, scale="free") +
  labs(x = "구분", y = "특성", fill="", subtitle = "대상자 특성") + 
  scale_fill_manual(values=c("#00bfc4", "#f8766d")) +
  theme(text = element_text(size=18)) + 
  ggsave(filename = paste(globalVar$figConfig, "Img_020.png", sep="/"), width = 12, height = 8, dpi = 600)


# 건강관리 및 최근변화
dataL1 = data.frame(
  minCi  = c(0.37, 0.21, 0.47, 0.31, 0.52, 0.72, 0.52, 0.94, 1.14, 0.91, 0.86, 0.62, 0.41, 0.76,  0.30, 0.29, 0.23, 2.31, 1.18,  0.87, 1.78)
  , rr = c(0.54, 0.45, 0.71, 0.52, 0.77, 0.86, 0.73, 1.05, 2.00, 1.22, 1.19, 0.83, 0.64, 0.87, 0.54, 0.49, 1.33, 6.00, 1.91, 1.48, 2.81)
  , maxCi = c(0.79, 0.95, 1.07, 0.86, 1.13, 1.03, 1.04, 1.17, 3.50, 1.62, 1.64, 1.11, 1.00, 1.00, 0.98, 0.82, 7.72, 15.57, 3.07, 2.51, 4.43)
  , p = c(0.001, 0.025, 0.083, 0.005, 0.167, 0.060, 0.425, 0.015, 0.204, 0.3, 0.194, 0.037, 0.021, 0.064, 0.064, 0.064, 0.749, 0.001, 0.008, 0.003, 0.003)
  , sampleNum = seq(1, 21)
  , type = c("혈압혈당인지", "조기증상인지", "행복지수", "주관적건강인지", "구강건강인지", "규칙 식사", "규칙 운동", "과거질병력", "머리사고 병력", "전신마취 병력", "치매서비스받기", "혈압관리", "혈당관리", "예방관리", "관리 수 1-2개", "관리 수 3개 이상", "배우자 상실", "중병 걸림", "변화(3년이내)", "변화 수 1개", "변화 수 2개 이상")
  , group = c("인지", "인지", "인지", "인지", "인지", "행태", "행태", "병력", "병력", "병력", "관리", "관리", "관리", "관리", "관리", "관리", "변화", "변화", "변화", "변화", "변화")
) %>%
  dplyr::mutate(pVal = dplyr::case_when(
    p <= 0.01 ~ "0.01"
    , p <= 0.05 ~ "0.05"
    , p <= 0.10 ~ "0.10"
    , TRUE ~ ""
  ))

# pVal 정렬
dataL1$pVal = forcats::fct_relevel(dataL1$pVal,  c("", "0.10", "0.05", "0.01"))


ggplot(data=dataL1, aes(x=rr, y=sampleNum)) +
  geom_errorbarh(aes(xmin=minCi, xmax=maxCi, color  = pVal), size = 1) +
  geom_point(aes(color=pVal), size = 3)  + 
  geom_vline(xintercept = mean(dataL1$rr, na.rm = TRUE), size =1, color="black") +
  scale_y_reverse() +
  geom_label(aes(x=-7.8, y=sampleNum, label=paste0(type), fill = group), fontface = "bold", colour = "white", size = 4.5, hjust="inward", vjust="center") +  
  geom_text(aes(x=-5, y=sampleNum, label=paste("RR :", round(rr, 2))), size = 4.5, hjust="inward") +
  geom_text(aes(x=-2.5, y=sampleNum, label=paste("P : ", p, sep="")), size = 4.5, hjust="inward") +
  theme(legend.position = "top") +
  scale_x_continuous(expand = c(0, 0), limits = c(-8, 16)) +
  labs(x = "RR (95% CI)", y = "특성", color="P-Value", fill="", subtitle = "건강관리 및 최근변화") +
  theme(text = element_text(size=18)) +
  ggsave(filename = paste(globalVar$figConfig, "Img_021.png", sep="/"), width = 12, height = 8, dpi = 600)



# 소통, 어울림 및 형태
dataL3 = data.frame(
  rr = c(0.85, 0.84, 0.63, 0.39, 0.12, 0.55, 0.40, 0.16)
  , minCi = c(0.54, 0.73, 0.50, 0.27, 0.05, 0.40, 0.25, 0.02)
  , maxCi = c(1.34, 0.97, 0.79, 0.56, 0.32, 0.76, 0.62, 1.03)
  , p = c(0.477, 0.004, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001)
  , sampleNum = seq(1, 8)
  , type = c("배우자 동거", "긴급시 동행", "주위와 대화", "대화상대 수 1,2개", "대화상대 수 3개 이상", "소통", "소통 수 1,2개", "소통 수 3개 이상")
  , group = c("거주", "거주", "대화", "대화", "대화", "소통", "소통", "소통")
) %>%
  dplyr::mutate(pVal = dplyr::case_when(
    p <= 0.01 ~ "0.01"
    , p <= 0.05 ~ "0.05"
    , p <= 0.10 ~ "0.10"
    , TRUE ~ ""
  ))


ggplot(data=dataL3, aes(x=rr, y=sampleNum)) +
  geom_errorbarh(aes(xmin=minCi, xmax=maxCi, color  = pVal), size = 1) +
  geom_point(aes(color=pVal), size = 3)  + 
  geom_vline(xintercept = mean(dataL3$rr, na.rm = TRUE), size =1, color="black") +
  scale_y_reverse() +
  geom_label(aes(x=-1.9, y=sampleNum, label=paste0(type), fill = group), fontface = "bold", colour = "white", size = 4.5, hjust="inward", vjust="center") +  
  geom_text(aes(x=-1.0, y=sampleNum, label=paste("RR :", round(rr, 2))), size = 4.5, hjust="inward") +
  geom_text(aes(x=-0.5, y=sampleNum, label=paste("P : ", p, sep="")), size = 4.5, hjust="inward") +
  theme(legend.position = "top") +
  scale_x_continuous(expand = c(0, 0), limits = c(-2, 2)) +
  labs(x = "RR (95% CI)", y = "특성", color="P-Value", fill="", subtitle = "소통, 어울림 및 형태") +
  theme(text = element_text(size=18)) +
  ggsave(filename = paste(globalVar$figConfig, "Img_022.png", sep="/"), width = 12, height = 8, dpi = 600)

# 긴급 시 동행, 주위와 대화, 소통
dataL4 = data.frame(
  group = c("정상군", "정상군", "치매이행군", "치매이행군", "정상군", "정상군", "치매이행군", "치매이행군", "정상군", "정상군", "치매이행군", "치매이행군")
  , variable = c("있다", "없다", "있다", "없다", "있다", "없다", "있다", "없다", "있다", "없다", "있다", "없다")
  , value = c(95, 5, 80, 20, 95, 5, 60, 40, 80, 20, 44, 56)
  , type = c("긴급 시 동행", "긴급 시 동행", "긴급 시 동행", "긴급 시 동행", "주위와 대화", "주위와 대화", "주위와 대화", "주위와 대화", "소통", "소통", "소통", "소통")
)

# type 정렬
dataL4$type = forcats::fct_relevel(dataL4$type, c("긴급 시 동행", "주위와 대화", "소통"))

# variable 정렬
dataL4$variable = forcats::fct_relevel(dataL4$variable,  c("있다", "없다"))

ggplot(dataL4, aes(x = variable, y = value, fill = group, label=round(value,1))) +
  geom_bar(position = "dodge", stat="identity") +
  theme(legend.position = "top") +
  geom_text(aes(group=group),position=position_dodge(width=0.9), size=5,vjust=-0.5,hjust=0.5) +
  ylim(0, 100) +
  facet_wrap( ~ type, scale="free") +
  labs(x = "구분", y = "특성", fill="", subtitle = "대상자 특성") + 
  scale_fill_manual(values=c("#00bfc4", "#f8766d")) +
  theme(text = element_text(size=18)) + 
  ggsave(filename = paste(globalVar$figConfig, "Img_023.png", sep="/"), width = 12, height = 8, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(readr)
library(xlsx)
library(readxl)
library(openxlsx)
library(tidyverse)
library(forcats)

fileList = Sys.glob(paste0(globalVar$inpConfig, "/KWCS5th(190924).xlsx"))

# data = openxlsx::read.xlsx(fileList, sheet = 1)
data = readxl::read_excel(path = fileList, sheet = 1)

# 성별, 연령, 학력, 소득, 건강상태, 근로환경만족도
# HH02_01_A, HH02_01_B, EF1, EF11, Q60, Q69

# 성별, 연령, 학력, 소득, 건강상태, 근로환경만족도
# Q34 (근무형태)
dataQ34 = data %>%
  dplyr::select(! c("Q34_1_1", "Q34_1_4_ETC")) %>%
  dplyr::select(ID, dplyr::contains("Q34"))# %>%
  # tidyr::gather(-ID, key = "key", value = "val") %>%
  # dplyr::mutate(
  #   q34 = dplyr::case_when(
  #     val == 1 ~ 1
  #     , val == 2 ~ 0
  #     , TRUE ~ NA_real_
  #   )
  # ) %>%
  # dplyr::group_by(ID) %>%
  # dplyr::summarise(sumQ34 = sum(q34, na.rm = TRUE))
  
# Q57번 (차별)
tmpDataQ57 = data %>%
    dplyr::select(ID, dplyr::contains("Q57")) %>%
    tidyr::gather(-ID, key = "key", value = "val") 

tmpDataQ57$val = replace(tmpDataQ57$val, tmpDataQ57$val == 2, 0)

dataQ57 = tmpDataQ57 %>%
    dplyr::mutate(
        q57 = dplyr::case_when(
            val == 1 ~ 1
            , val == 2 ~ 0
            , TRUE ~ NA_real_
        )
    ) %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(sumQ57 = sum(q57, na.rm = TRUE))
dataL1$Q34_5 = replace(dataL1$Q34_5, dataL1$Q34_5 == 2, 0)

# Q64번 (폭력경험)
tmpDataQ64 = data %>%
    dplyr::select(! dplyr::contains("KQ64")) %>%
    dplyr::select(ID, dplyr::contains("Q64")) %>%
    tidyr::gather(-ID, key = "key", value = "val") 

tmpDataQ64$val = replace(tmpDataQ64$val, tmpDataQ64$val == 2, 0)

dataQ64 = tmpDataQ64 %>%
    dplyr::mutate(
        q64 = dplyr::case_when(
            val == 1 ~ 1
            , val == 2 ~ 0
            , TRUE ~ NA_real_
        )
    ) %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(sumQ64 = sum(q64, na.rm = TRUE))


dataL1 = data %>%
  dplyr::select(! c("Q34_1_1", "Q34_1_4_ETC")) %>%
  dplyr::select(ID, HH02_01_A, HH02_01_B, EF1, EF11, Q60, Q69, dplyr::contains("Q34")) %>%
  dplyr::mutate(
    typeHH02_01_A = dplyr::case_when(
      HH02_01_A == 1 ~ "남"
      , HH02_01_A == 2 ~ "여"
      , TRUE ~ "NULL"
    )
    , age = (2020 - HH02_01_B) + 1
    , typeAge = dplyr::case_when(
      age < 30 ~ "30세 미만"
      , 30 <= age & age < 40 ~ "30-40세 미만"
      , 40 <= age & age < 50 ~ "40-50세 미만"
      , 50 <= age & age < 60 ~ "50-60세 미만"
      , 60 >= age ~ "60세 이상"
      , TRUE ~ "NULL"
    )
    , typeEF1 = dplyr::case_when(
      # EF1 == 1 ~ "무학 또는 초등학교 졸업 미만"
      # , EF1 == 2 ~ "초등학교 졸업"
      EF1 == 1 ~ "NULL"
      , EF1 == 2 ~ "NULL"
      , EF1 == 3 ~ "중학교 졸업"
      , EF1 == 4 ~ "고등학교 졸업"
      , EF1 == 5 ~ "전문 대학교 졸업"
      , EF1 == 6 ~ "대학교 졸업"
      , EF1 == 7 ~ "대학원 재학 이상"
      # , EF1 == 9 ~ "거절"
      , EF1 == 9 ~ "NULL"
      , TRUE ~ "NULL"
    )
    , typeEF11 = dplyr::case_when(
      EF11 < 100 ~ "100만 미만"
      , 100 <= EF11 & EF11 < 200 ~ "100-200만 미만"
      , 200 <= EF11 & EF11 < 300 ~ "200-300만 미만"
      , 300 <= EF11 & EF11 < 400 ~ "300-400만 미만"
      , 400 <= EF11 & EF11 < 500 ~ "400-500만 미만"
      , EF11 >= 500 ~ "500만 이상"
      , TRUE ~ "NULL"
    )
    , typeQ60 = dplyr::case_when(
      Q60 == 1 ~ "매우 좋다"
      , Q60 == 2 ~ "좋은 편이다"
      , Q60 == 3 ~ "보통이다"
      , Q60 == 4 ~ "나쁜 편이다"
      # , Q60 == 5 ~ "매우 나쁘다"
      , Q60 == 5 ~ "NULL"
      # , Q60 == 8 ~ "모름/무응답"
      # , Q60 == 9 ~ "거절"
      , Q60 == 8 ~ "NULL"
      , Q60 == 9 ~ "NULL"
      , TRUE ~ "NULL"
    )
    , typeQ69 = dplyr::case_when(
      Q69 == 1 ~ "매우 만족한다"
      , Q60 == 2 ~ "만족한다"
      , Q60 == 3 ~ "별로 만족하지 않는다"
      , Q60 == 4 ~ "전혀 만족하지 않는다"
      # , Q60 == 8 ~ "모름/무응답"
      # , Q60 == 9 ~ "거절"
      , Q60 == 8 ~ "NULL"
      , Q60 == 9 ~ "NULL"
      , TRUE ~ "NULL"
    )
    , typeQ34_1 = dplyr::case_when(
        Q34_1 == 1 ~ "예"
        , Q34_1 == 2 ~ "아니다"
        , TRUE ~ "NULL"
    )
      , typeQ34_2 = dplyr::case_when(
          Q34_2 == 1 ~ "예"
          , Q34_2 == 2 ~ "아니다"
          , TRUE ~ "NULL"
      )
      , typeQ34_3 = dplyr::case_when(
          Q34_3 == 1 ~ "예"
          , Q34_3 == 2 ~ "아니다"
          , TRUE ~ "NULL"
      )
      , typeQ34_4 = dplyr::case_when(
          Q34_4 == 1 ~ "예"
          , Q34_4 == 2 ~ "아니다"
          , TRUE ~ "NULL"
      )
      , typeQ34_5 = dplyr::case_when(
          Q34_5 == 1 ~ "예"
          , Q34_5 == 2 ~ "아니다"
          , TRUE ~ "NULL"
      )
  ) %>%
  # dplyr::left_join(dataQ34, by = c("ID" = "ID")) %>%
  dplyr::left_join(dataQ57, by = c("ID" = "ID")) %>%
  dplyr::left_join(dataQ64, by = c("ID" = "ID")) %>%
  dplyr::na_if("NULL") %>%
  na.omit()

dataL1$Q34_1 = replace(dataL1$Q34_1, dataL1$Q34_1 == 2, 0)
dataL1$Q34_2 = replace(dataL1$Q34_2, dataL1$Q34_2 == 2, 0)
dataL1$Q34_3 = replace(dataL1$Q34_3, dataL1$Q34_3 == 2, 0)
dataL1$Q34_4 = replace(dataL1$Q34_4, dataL1$Q34_4 == 2, 0)
dataL1$Q34_5 = replace(dataL1$Q34_5, dataL1$Q34_5 == 2, 0)

#=====================================================================
# 훈련 테스트 셋 설정 (1000개)
#=====================================================================

set.seed(1)

# 훈련 및 데이터 셋을 60:40으로 나누기 위한 인덱스 설정
ind = sample(1:nrow(dataL1), 1000)

# 해당 인덱스에 따라 자료 할당
dataL2 = dataL1[ind, ]

dplyr::tbl_df(dataL2)

#==================================
# Table 1
#==================================
# paste(unique(dataL2$typeQ34_1), collapse = ", ")

dataL2$typeHH02_01_A =forcats::fct_relevel(dataL2$typeHH02_01_A, c("남", "여"))
dataL2$typeAge = forcats::fct_relevel(dataL2$typeAge, c("30세 미만", "30-40세 미만", "40-50세 미만", "50-60세 미만", "60세 이상"))
dataL2$typeEF1 =forcats::fct_relevel(dataL2$typeEF1, c("중학교 졸업", "고등학교 졸업", "전문 대학교 졸업", "대학교 졸업", "대학원 재학 이상"))
dataL2$typeEF11 =forcats::fct_relevel(dataL2$typeEF11, c("100만 미만", "100-200만 미만", "200-300만 미만", "300-400만 미만", "400-500만 미만", "500만 이상"))
dataL2$typeQ60 =forcats::fct_relevel(dataL2$typeQ60, c("매우 좋다", "좋은 편이다", "보통이다", "나쁜 편이다"))
dataL2$typeQ69 =forcats::fct_relevel(dataL2$typeQ69, c("매우 만족한다", "만족한다", "별로 만족하지 않는다", "전혀 만족하지 않는다"))
dataL2$typeQ34_1 =forcats::fct_relevel(dataL2$typeQ34_1, c("예", "아니오"))
dataL2$typeQ34_2 = forcats::fct_relevel(dataL2$typeQ34_2, c("예", "아니오"))
dataL2$typeQ34_3 =forcats::fct_relevel(dataL2$typeQ34_3, c("예", "아니오"))
dataL2$typeQ34_4 =forcats::fct_relevel(dataL2$typeQ34_4, c("예", "아니오"))
dataL2$typeQ34_5 =forcats::fct_relevel(dataL2$typeQ34_5, c("예", "아니오"))


dataL2 %>%
  dplyr::group_by(typeHH02_01_A) %>%
  dplyr::summarise(cnt = n()) %>%
  dplyr::mutate(
    ratio = (cnt / sum(cnt, na.rm = TRUE)) * 100
    , label = paste0(cnt, " (", ratio, ")")
  )

dataL2 %>%
  dplyr::group_by(typeAge) %>%
  dplyr::summarise(cnt = n()) %>%
  dplyr::mutate(
    ratio = (cnt / sum(cnt, na.rm = TRUE)) * 100
    , label = paste0(cnt, " (", ratio, ")")
    )

dataL2 %>%
  dplyr::group_by(typeEF1) %>%
  dplyr::summarise(cnt = n()) %>%
  dplyr::mutate(
    ratio = (cnt / sum(cnt, na.rm = TRUE)) * 100
    , label = paste0(cnt, " (", ratio, ")")
  )

dataL2 %>%
  dplyr::group_by(typeEF11) %>%
  dplyr::summarise(cnt = n()) %>%
  dplyr::mutate(
    ratio = (cnt / sum(cnt, na.rm = TRUE)) * 100
    , label = paste0(cnt, " (", ratio, ")")
  )

dataL2 %>%
  dplyr::group_by(typeQ60) %>%
  dplyr::summarise(cnt = n()) %>%
  dplyr::mutate(
    ratio = (cnt / sum(cnt, na.rm = TRUE)) * 100
    , label = paste0(cnt, " (", ratio, ")")
  )

dataL2 %>%
  dplyr::group_by(typeQ69) %>%
  dplyr::summarise(cnt = n()) %>%
  dplyr::mutate(
    ratio = (cnt / sum(cnt, na.rm = TRUE)) * 100
    , label = paste0(cnt, " (", ratio, ")")
  )


colList = c("typeQ34_1", "typeQ34_2", "typeQ34_3", "typeQ34_4", "typeQ34_5")

for (i in 1:length(colList)) {
    dataL2 %>%
        dplyr::group_by(get(colList[i], dataL2)) %>%
        dplyr::summarise(cnt = n()) %>%
        dplyr::mutate(
            ratio = (cnt / sum(cnt, na.rm = TRUE)) * 100
            , label = paste0(cnt, " (", ratio, ")")
        ) %>%
        dplyr::glimpse()
}


#==================================
# Table 2
#==================================
dataL2 %>%
  dplyr::select(! dplyr::contains("type")) %>%
  dplyr::select(dplyr::contains(c("Q34", "Q57", "Q64"))) %>%
  dplyr::summarise_all(funs(
      mean(., na.rm = TRUE) # 평균
      , sd(., na.rm = TRUE) # 표준편차
      , min(., na.rm = TRUE)
      , max(., na.rm = TRUE)
    )) %>%
    round(2) %>%
  dplyr::glimpse()


#==================================
# Table 3
#==================================
# 모든 변수
typeList = c("typeHH02_01_A", "typeAge", "typeEF1", "typeEF11", "typeQ60", "typeQ69")
# typeNameList = c("성별", "연령", "학력", "소득", "건강상태", "근로만족도")
colList = dataL2 %>%
    dplyr::select(! dplyr::contains("type")) %>%
    # dplyr::select(dplyr::contains(c("Q34", "Q57", "Q64"))) %>%
    dplyr::select(dplyr::contains(c("Q34"))) %>%
    colnames()

# colNameList = c("매일 근무시간의 길이가 같다", "매주 근무 일수가 같다", "매주 근무시간대가 같다", "출퇴근 시각이 정해져 있다", "교대 근무를 한다", "연령 차별", "인종, 출신 민족, 피부색에 따른 차별", "국적에 따른 차별", "성차별", "종교에 따른 차별", "장애에 따른 차별", "동성애와 같은 성적 지향에 따른 차별", "학벌에 따른 차별", "출신 지역에 따른 차별", "고용 형태(비정규직 등)에 따른 차별", "언어폭력", "원하지 않는 성적 관심", "위협", "모욕적 행동")

dataL3 = data.frame()
dataL4 = data.frame()

for (i in 1:length(typeList)) {
    for (j in 1:length(colList)) {
        
        rsAov = aov(get(colList[j], dataL2) ~ get(typeList[i], dataL2), data = dataL2) %>%
            summary %>%
            unlist()
        
        tmpData = tibble(
            # typeName = typeNameList[i]
            type = typeList[i]
            , col = colList[j]
            # , colName = colNameList[j]
            , fVal = rsAov[7] %>% round(2)
            , pVal = rsAov[9] %>% round(2)
        ) %>%
        dplyr::mutate(label = paste0(fVal, " (", pVal, ")"))
        
        dataL3 = dplyr::bind_rows(dataL3, tmpData)
    }
    
    tmpData2 = dataL2 %>%
        dplyr::group_by(type = get(typeList[i], dataL2)) %>%
        dplyr::select(! dplyr::contains("type")) %>%
        # dplyr::select(dplyr::contains(c("Q34", "Q57", "Q64"))) %>%
        dplyr::select(dplyr::contains(c("Q34"))) %>%
        dplyr::summarise_all(funs(
            mean(., na.rm = TRUE) %>% round(2) # 평균
            , sd(., na.rm = TRUE) %>% round(2) # 표준편차
            # , n() # 자료 개수
        ))
    
    dataL4 = dplyr::bind_rows(dataL4, tmpData2)
}

xlsx::write.xlsx2(dataL3, file = paste0(globalVar$outConfig, "/Survery.xlsx"), sheetName = "dataL3_3", append = TRUE, row.names = FALSE, col.names = TRUE)

xlsx::write.xlsx2(dataL4, file = paste0(globalVar$outConfig, "/Survery.xlsx"), sheetName = "dataL4_3", append = TRUE, row.names = FALSE, col.names = TRUE)

# 성별
# dataL2 %>%
#   dplyr::group_by(typeHH02_01_A) %>%
#     dplyr::select(! dplyr::contains("type")) %>%
#     dplyr::select(dplyr::contains(c("Q34", "Q57", "Q64"))) %>%
#   dplyr::summarise_all(funs(
#     mean(., na.rm = TRUE) # 평균
#     , sd(., na.rm = TRUE) # 표준편차
#   )) %>%
#   dplyr::glimpse()

# aov(Q34_1 ~ typeHH02_01_A, data = dataL2) %>% summary
# aov(Q34_2 ~ typeHH02_01_A, data = dataL2) %>% summary
# aov(sumQ64 ~ typeHH02_01_A, data = dataL2) %>% summary
#
# # 연령
# dataL2 %>%
#   dplyr::group_by(typeAge) %>%
#   dplyr::summarise_all(funs(
#     mean(., na.rm = TRUE) # 평균
#     , sd(., na.rm = TRUE) # 표준편차
#   )) %>%
#   dplyr::glimpse()
# 
# aov(sumQ34 ~ typeAge, data = dataL2) %>% summary
# aov(sumQ57 ~ typeAge, data = dataL2) %>% summary
# aov(sumQ64 ~ typeAge, data = dataL2) %>% summary

# # 학력
# dataL2 %>%
#   dplyr::group_by(typeEF1) %>%
#   dplyr::summarise_all(funs(
#     mean(., na.rm = TRUE) # 평균
#     , sd(., na.rm = TRUE) # 표준편차
#   )) %>%
#   dplyr::glimpse()
# 
# aov(sumQ34 ~ typeEF1, data = dataL2) %>% summary
# aov(sumQ57 ~ typeEF1, data = dataL2) %>% summary
# aov(sumQ64 ~ typeEF1, data = dataL2) %>% summary

# 소득
# dataL2 %>%
#   dplyr::group_by(typeEF11) %>%
#   dplyr::summarise_all(funs(
#     mean(., na.rm = TRUE) # 평균
#     , sd(., na.rm = TRUE) # 표준편차
#   )) %>%
#   dplyr::glimpse()
# 
# aov(sumQ34 ~ typeEF11, data = dataL2) %>% summary
# aov(sumQ57 ~ typeEF11, data = dataL2) %>% summary
# aov(sumQ64 ~ typeEF11, data = dataL2) %>% summary

# 건강 상태
# dataL2 %>%
#   dplyr::group_by(typeQ60) %>%
#   dplyr::summarise_all(funs(
#     mean(., na.rm = TRUE) # 평균
#     , sd(., na.rm = TRUE) # 표준편차
#   )) %>%
#   dplyr::glimpse()
# 
# aov(sumQ34 ~ typeQ60, data = dataL2) %>% summary
# aov(sumQ57 ~ typeQ60, data = dataL2) %>% summary
# aov(sumQ64 ~ typeQ60, data = dataL2) %>% summary

# 근로 환경 만족도
# dataL2 %>%
#   dplyr::group_by(typeQ69) %>%
#   dplyr::summarise_all(funs(
#     mean(., na.rm = TRUE) # 평균
#     , sd(., na.rm = TRUE) # 표준편차
#   )) %>%
#   dplyr::glimpse()
# 
# aov(sumQ34 ~ typeQ69, data = dataL2) %>% summary
# aov(sumQ57 ~ typeQ69, data = dataL2) %>% summary
# aov(sumQ64 ~ typeQ69, data = dataL2) %>% summary


#==================================
# Table 4
#==================================
# 근무형태와 차별
# cor.test(dataL2$sumQ34, dataL2$sumQ57)

# 근무형태와 폭력경험
# cor.test(dataL2$sumQ34, dataL2$sumQ64)

colList = c("Q34_1", "Q34_2", "Q34_3", "Q34_4", "Q34_5")
colList2 = c("sumQ57", "sumQ64")

# 모든 변수
dataL5 = data.frame()

for (j in 1:length(colList2)) {
    for (i in 1:length(colList)) {
        corTest = cor.test(get(colList[i], dataL2), get(colList2[j], dataL2))
        
        tmpData = tibble(
            colI = colList[i]
            , colJ = colList2[j]
            # , colNameI = colNameList[i]
            # , colNameJ = colNameList[j]
            , corVal = corTest$estimate %>% round(2)
            , pVal = corTest$p.value %>% round(2)
        ) %>%
        dplyr::mutate(
            label = paste0(corVal, " (", pVal, ")")
        )
        
        dataL5 = dplyr::bind_rows(dataL5, tmpData)
    }
}

xlsx::write.xlsx2(dataL5, file = paste0(globalVar$outConfig, "/Survery.xlsx"), sheetName = "dataL5", append = TRUE, row.names = FALSE, col.names = TRUE)

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(readr)
library(xlsx)
library(readxl)
library(openxlsx)
library(tidyverse)
library(scales)

# 논문수, 기업화, 민영화, 자유화, 국내총생상, 경제성장률
data = data.frame(
  year = seq(1986, 2019)
  , paperCnt = c(18, 8, 36, 49, 65, 59, 64, 59, 63, 61, 59, 57, 57, 63, 66, 69, 71, 85, 74, 79, 83, 84, 85, 94, 99, 113, 129, 135, 137, 143, 173, 182, 173, 169)
  , v1 = c(433,	836,	880,	882,	1090,	1117,	1016,	990,	1011,	1056,	1019,	1139,	988,	772,	1103,	1182,	1179,	1190,	1136,	1295,	1169,	1177,	1605,	1249,	1249,	1643,	1406,	1853,	1578,	1599,	2639,	1793,	1548,	1638)
  , v2 = c(647,	586,	1315,	937,	1805,	1076,	1108,	1047,	1317,	1898,	2148,	1658,	1729,	1700,	1118,	1656,	1214,	1212,	1319,	1324,	1585,	1426,	1429,	1466,	1466,	1290,	2078,	1918,	1025,	1244,	2983,	2102,	1720,	1808)
  , v3 = c(280,	177,	484,	229,	407,	543,	1081,	450,	677,	749,	752,	1423,	1108,	951,	1033,	732,	669,	1003,	988,	506,	926,	742,	610,	1010,	1010,	1476,	1787,	1722,	1176,	1135,	2452,	1870,	1268,	1107)
  # , gdpNor = c(rep(NA, 14), 18928, 20223, 21277, 21887, 23707, 24735, 24358, 24758, 27241, 28484, 29880, 32228, 33212, 33614, 33949, 34137, 36103, 36382, 35671, NA)
  , gdpNor = c(rep(NA, 4), 16354.1,	16737.0,	16415.9,	16406.3,	16997.7,	17132.0,	17222.0,	16784.4,	17538.6,	18688.1, 18927.5,	20222.8,	21276.7,	21887.4,	23707.0,	24734.8, 24357.9, 24757.6,	27241.2,	28483.5,	29879.9,	32227.8,	33211.9,	33614.2,	33949.4,	34136.7,	36103.3,	36381.8,	35670.5,	35278.6)
  , gdpSou = c(rep(NA, 14), 651634, 707021, 784741, 837365, 908439, 957448, 1005602, 1089660, 1154217, 1205348, 1322611, 1388937, 1440111, 1500819, 1562929, 1658020, 1740780, 1835698, 1893497, NA)
  # , gdpRatNor = c(rep(NA, 14), 0.4, 3.8, 1.2, 1.8, 2.1, 3.8, -1.0, -1.2, 3.1, -0.9, -0.5, 0.8, 1.3, 1.1, 1.0, -1.1, 3.9, -3.5, -4.1, NA)
  , gdpRatNor = c(rep(NA, 4), -4.3,	-4.4,	-7.1,	-4.5,	-2.1,	-4.4,	-3.4,	-6.5,	-0.9,	6.1,	0.4,	3.8,	1.2,	1.8,	2.1,	3.8,	-1.0,	-1.2,	3.1,	-0.9,	-0.5,	0.8,	1.3,	1.1,	1.0,	-1.1,	3.9,	-3.5,	-4.1,	0.4)
  , gdpRatSou = c(rep(NA, 14), 0.0, 4.9, 7.7, 3.1, 5.2, 4.3, 5.3, 5.8, 3.0, 0.8, 6.8, 3.7, 2.4, 3.2, 3.2, 2.8, 2.9, 3.2, 2.7, NA)
)


dataL1 = data %>%
  dplyr::mutate(
    dtDate = readr::parse_date(as.character(year), "%Y")
    , var1 = v1 / paperCnt
    , var2 = v2 / paperCnt
    , var3 = v3 / paperCnt
  )


ggData = dataL1 %>%
  dplyr::select(dtDate, var1, var2, var3) %>%
  dplyr::rename(
    "토픽1 (기업화)" = var1
    , "토픽2 (민영화)" = var2
    , "토픽3 (자유화)" = var3
    ) %>%
  tidyr::gather(-dtDate, key = "key", value = "val")

ggplot(ggData, aes(x = dtDate, y = val, color = key, label=round(val, 0))) +
  geom_line(size = 2) +
  scale_x_date(labels = date_format("%y"), date_breaks = "1 years", limits = c(as.Date("1985-01-01"), as.Date("2019-01-01")), date_minor_breaks = "1 years") + 
  ylim(0, 105) +
  labs(x = "년도", y = "토픽 지수", color="", subtitle = "시계열 LDA-토픽모델링 분석 결과") + 
  theme(text = element_text(size=18))  + 
  ggsave(filename = paste(globalVar$figConfig, "Img_031.png", sep="/"), width = 12, height = 8, dpi = 600)



ggplot(ggData, aes(x = dtDate, y = val, fill = key, label=round(val, 0))) +
  geom_bar(position = "dodge", stat="identity") +
  theme(legend.position = "top") +
  geom_text(aes(group=key),position=position_dodge(width=0.9), size=5,vjust=-0.5,hjust=0.5) +
  ylim(0, 105) +
  facet_wrap( ~ key, scale="free") +
  labs(x = "", y = "", fill="", subtitle = "") + 
  theme(text = element_text(size=18)) + 
  ggsave(filename = paste(globalVar$figConfig, "Img_030.png", sep="/"), width = 12, height = 8, dpi = 600)



# Table 3.4
dataL1 %>%
  dplyr::select(dplyr::contains("var")) %>%
  dplyr::summarise_all(funs(
    n() # 개수
    , mean(., na.rm = TRUE) # 평균
    , sd(., na.rm = TRUE) # 표준편차
    , sum(., na.rm = TRUE) # 합계
    , min(., na.rm = TRUE) # 최소값
    , max(., na.rm = TRUE) # 최대값
  )) %>%
  round(2)

# Table 3.5
dataL1 %>%
  dplyr::select(dplyr::contains("var"), gdpNor, gdpRatNor) %>%
  na.omit() %>%
  cor() %>%
  round(2)

dataL1 %>%
    dplyr::select(v1, v2, v3, gdpNor, gdpRatNor) %>%
    na.omit() %>%
    cor() %>%
    round(2)


xlsx::write.xlsx2(dataL1, file = paste0(globalVar$outConfig, "/Gdp_Rat.xlsx"), sheetName = "RAW", append = TRUE, row.names = FALSE, col.names = TRUE)

#==================================
# GDP
#==================================
dataL2 = dataL1 %>%
  dplyr::select(-gdpSou, -gdpRatSou) %>%
  na.omit() %>%
  dplyr::mutate(
    scaleVar1 = scale(var1)
    , scaleVar2 = scale(var2)
    , scaleVar3 = scale(var3)
  )

# 북한
lm(gdpNor ~ scaleVar1, data = dataL2) %>% summary()
lm(gdpNor ~ scaleVar2, data = dataL2) %>% summary()
lm(gdpNor ~ scaleVar3, data = dataL2) %>% summary()


lmFit = lm(gdpNor ~ scaleVar1*scaleVar2 + scaleVar1*scaleVar3 + scaleVar2*scaleVar3, data = dataL2)
# lmFit = lm(gdpNor ~ scaleVar1 + scaleVar2 + scaleVar3 + scaleVar1*scaleVar2 + scaleVar1*scaleVar3 + scaleVar2*scaleVar3, data = dataL2)
summary(lmFit)
MASS::stepAIC(lmFit, direction = "both") %>% summary()

stepFitGdp = MASS::stepAIC(lmFit, direction = "both")

# 남한
lm(gdpSou ~ scaleVar1, data = dataL2) %>% summary()
lm(gdpSou ~ scaleVar2, data = dataL2) %>% summary()
lm(gdpSou ~ scaleVar3, data = dataL2) %>% summary()

lmFit = lm(gdpSou ~ scaleVar1 + scaleVar2 + scaleVar3, data = dataL2)
summary(lmFit)
MASS::stepAIC(lmFit, direction = "both") %>% summary()

#==================================
# 경제성장률
#==================================
# 북한
lm(gdpRatNor ~ scaleVar1, data = dataL2) %>% summary()
lm(gdpRatNor ~ scaleVar2, data = dataL2) %>% summary()
lm(gdpRatNor ~ scaleVar3, data = dataL2) %>% summary()

# lmFit = lm(gdpRatNor ~ scaleVar1 + scaleVar2 + scaleVar3 + scaleVar1*scaleVar2 + scaleVar1*scaleVar3 + scaleVar2*scaleVar3, data = dataL2)
lmFit = lm(gdpRatNor ~ scaleVar1*scaleVar2 + scaleVar1*scaleVar3 + scaleVar2*scaleVar3, data = dataL2)
summary(lmFit)
MASS::stepAIC(lmFit, direction = "both") %>% summary()
stepFitGdpRat = MASS::stepAIC(lmFit, direction = "both")

# 남한
lm(gdpRatSou ~ scaleVar1, data = dataL2) %>% summary()
lm(gdpRatSou ~ scaleVar2, data = dataL2) %>% summary()
lm(gdpRatSou ~ scaleVar3, data = dataL2) %>% summary()

lmFit = lm(gdpRatSou ~ scaleVar1 + scaleVar2 + scaleVar3, data = dataL2)
summary(lmFit)
MASS::stepAIC(lmFit, direction = "both") %>% summary()

#==================================
# 시각화
#==================================
ggData = dataL2 %>%
  dplyr::mutate(
    stepFitGdp = predict(stepFitGdp)
    , stepFitGdpRat = predict(stepFitGdpRat)
    ) %>%
  dplyr::select(dtDate, gdpNor, stepFitGdp) %>%
  dplyr::rename(
    "GDP (북한)" = gdpNor
    , "GDP (북한) 예측" = stepFitGdp
  ) %>%
  tidyr::gather(-dtDate, key = "key", value = "val")

ggplot(ggData, aes(x = dtDate, y = val, color = key, label=round(val, 0))) +
  geom_line(size = 2) +
  scale_x_date(labels = date_format("%y"), date_breaks = "1 years", limits = c(as.Date("1990-01-01"), as.Date("2018-01-01")), date_minor_breaks = "1 years") + 
  labs(x = "년도", y = "GDP", color="", subtitle = "북한 GDP 그래프") + 
  theme(text = element_text(size=18))  + 
  ggsave(filename = paste(globalVar$figConfig, "Img_033.png", sep="/"), width = 12, height = 8, dpi = 600)


ggData = dataL2 %>%
  dplyr::mutate(
    stepFitGdp = predict(stepFitGdp)
    , stepFitGdpRat = predict(stepFitGdpRat)
  ) %>%
  dplyr::select(dtDate, gdpRatNor, stepFitGdpRat) %>%
  dplyr::rename(
    "경제 성장률 (북한)" = gdpRatNor
    , "경제 성장률 (북한) 예측" = stepFitGdpRat
  ) %>%
  tidyr::gather(-dtDate, key = "key", value = "val")

ggplot(ggData, aes(x = dtDate, y = val, color = key, label=round(val, 0))) +
  geom_line(size = 2) +
  scale_x_date(labels = date_format("%y"), date_breaks = "1 years", limits = c(as.Date("1990-01-01"), as.Date("2018-01-01")), date_minor_breaks = "1 years") + 
  labs(x = "년도", y = "경제 성장률", color="", subtitle = "북한 경제 성장률 그래프") + 
  theme(text = element_text(size=18))  + 
  ggsave(filename = paste(globalVar$figConfig, "Img_034.png", sep="/"), width = 12, height = 8, dpi = 600)

xlsx::write.xlsx2(ggData, file = paste0(globalVar$outConfig, "/Gdp_Rat.xlsx"), sheetName = "L5", append = TRUE, row.names = FALSE, col.names = TRUE)

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(ggplot2)
library(forcats)
library(tidyverse)
library(ggpubr)
library(Metrics)

data = mpg
# manufacturer : 제조업체
# model : 차량 모델
# displ : 배기량
# cyl :실린더 개수
# trans : 자동기어 여부
# cty : 도시 연비
# hwy : 고속도로 연비
# fl : 연료 형태
# class : 차량 타입

# mpg 데이터는 ggplot2 패키지에 포함되어 있는 예제 데이터이다. 아래의 질문에 답하라.(워드 프로그램으로  제출할 것)
# 질문) 자동차 제조업체별 도시 연비를 막대 그래프로 나타내시오.
# 1) 막대 그래프를 멋지게 작성하라.

ggData = data %>%
  dplyr::select(cty, manufacturer) %>%
  dplyr::group_by(manufacturer) %>%
  dplyr::summarise(meanCty = mean(cty, na.rm = TRUE)) %>%
  dplyr::arrange(meanCty)

ggData$manufacturer = forcats::fct_relevel(ggData$manufacturer, ggData$manufacturer)

ggplot(ggData, aes(x = manufacturer, y = meanCty, fill = manufacturer)) +
  geom_bar(position = "dodge", stat="identity") +
  geom_text(aes(label = round(meanCty, 2)), vjust = 1.6, color = "white", size = 4) +
  labs(x = "제조업체", y = "도시 연비", fill="", subtitle = "자동차 제조업체에 따른 도시 연비 그래프") + 
  theme(text = element_text(size=18)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figConfig, "Img_035.png", sep="/"), width = 12, height = 8, dpi = 600)

summary(ggData)

# 2)제조업체별 도시 연비를 설명하라.
# 15종 제조업체에 따라 도시 연비를 막대그래프로 시각화하여 통계 분석하였다.
# 즉 평균 및 최소/최대값은 각각 16.4 및 11.3-24.4로 나타났으며
# 특히 높은 연비 TOP5의 경우 honda, volkswagen, subaru, hyundai, toyota인 반면
# jeep, mercury, dodge, land rover, lincoln 순으로 낮았다.

# 3) 제조업체별 도시 연비와 고속도로 연비는 차이가 있는지 설명하라.
# 전체 제조업체에 대한 평균 (도시 연비: 16.4, 고속도로 연비: 23.0) 및 최대/최소값 (도시 연비: 11.3-24.4, 고속도로 연비: 16.5-32.6)은 큰 차이를 보였다. 
# 특히 큰 차이 TOP5의 경우 pontiac, audi, volkswagen, hyundai, honda인 반면
# ford, land rover, dodge, mercury, jeep 순으로 낮았다.

dataL3 = data %>%
  dplyr::select(cty, manufacturer, hwy) %>%
  dplyr::group_by(manufacturer) %>%
  dplyr::summarise(
    meanCty = mean(cty, na.rm = TRUE)
    , meanHwy = mean(hwy, na.rm = TRUE)
    , diff = meanCty - meanHwy
  ) %>%
  dplyr::arrange(diff)

# 보너스 점수: 개별적으로 추가적인 분석을 수행하고 그래프와 이를 설명하는 글을 입력하시오.
# 도시 연비 및 고속도로 연비를 이용하여 산점도로 시각화하였다.
# 두 자료의 편이 (Bias) 및 평균제곱근오차 (RMSE)는 각각 -6.58 및 6.77로서 다소 오차를 보였음에도  불구하고 상관성은 0.97로서 0.000 이하의 유의성을 나타내었다.

corTest = cor.test(dataL3$meanCty, dataL3$meanHwy)
corVal = round(corTest$estimate, 2)
pVal = round(corTest$p.value, 2)
biasVal = round(Metrics::bias(dataL3$meanCty, dataL3$meanHwy), 2)
rmseVal = round(Metrics::rmse(dataL3$meanCty, dataL3$meanHwy), 2)

ggscatter(dataL3, x = "meanCty", y = "meanHwy", color = "black", add = "reg.line", conf.int = TRUE, add.params = list(color = "blue", fill = "lightblue")) +
  stat_regline_equation(label.x = 10, label.y = 35, size = 5) +
  annotate("text", x = 10, y = 33, size = 5, label = sprintf("R = %s (P < %.3f)", corVal, pVal), hjust = 0) +
  annotate("text", x = 10, y = 31, size = 5, label = sprintf("Bias = %s", biasVal), hjust = 0) +
  annotate("text", x = 10, y = 29, size = 5, label = sprintf("RMSE = %s", rmseVal), hjust = 0) +
  xlim(10, 35) + 
  ylim(10, 35) +
  theme_bw() +
  labs(title = "", x = "도시 연비", y = "고속도로 연비", subtitle = "도시 연비와 고속도로 연비의 산점도") +
  coord_equal() +
  theme(text = element_text(size=18)) + 
  ggsave(filename = paste(globalVar$figConfig, "Img_036.png", sep="/"), width = 6, height = 6, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(ggplot2)
library(forcats)
library(tidyverse)
library(ggpubr)
library(Metrics)
library(scales)
library(viridis)

data = data.frame(
  key1 = c("홍길동", "임꺽정", "전우치", "변강쇠", "판매원1", "판매원2", "판매원3", "판매원4", "판매원5", "판매원6")
  , key2 = c(12, 7, 3, 12, 3, 4, 2, 5, 4, 9)
  , key3 = c(5000000, 7000000, 3500000, 11500000, 4000000, 5000000, 2500000, 5500000, 4500000, 12000000)
)

ggData = data %>%
  dplyr::mutate(
    key4 = key3 / 1000000
  ) 

dataL1 = data %>%
  dplyr::arrange(key4)
 
ggData$key1 = forcats::fct_relevel(ggData$key1, dataL1$key1)

ggplot(ggData, aes(x = key1, y = key4, fill = key2)) +
  geom_bar(position = "dodge", stat="identity") +
  geom_text(aes(label = round(key2, 2)), vjust = 1.6, color = "white", size = 6) +
  labs(x = "", y = "수수료 [단위 : 백만원]", fill="판매수", subtitle = "이름에 따른 수수료 그래프") + 
  theme(text = element_text(size=18)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(direction = -1) + 
  ggsave(filename = paste(globalVar$figConfig, "Img_036.png", sep="/"), width = 12, height = 8, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(ggplot2)
library(tidyverse)
library(forcats)
library(readr)
library(ggrepel)

fileList = Sys.glob(paste(globalVar$inpConfig, "TC_TOUR_PURPSBY_CNSMP_SCALE_STLE_INFO_2019.csv", sep = "/"))

data = readr::read_csv(file = fileList, locale = locale("ko", encoding = "UTF-8"))

dplyr::tbl_df(data)

#==========================================
# 성별
#==========================================
dataL1 = dplyr::bind_rows(
    data.frame(table(data$SEX_NM, data$INTRST_FASHN_AT), type =  "패션주목")
    , data.frame(table(data$SEX_NM, data$INTRST_PC_AT), type = "가격주목")
    , data.frame(table(data$SEX_NM, data$INTRST_HEALTH_AT), type = "건강주목")
    , data.frame(table(data$SEX_NM, data$INTRST_WDOMOUTH_AT), type = "입소문주목")
    , data.frame(table(data$SEX_NM, data$INTRST_QLITY_AT), type = "품질주목")
    , data.frame(table(data$SEX_NM, data$INTRST_SAFE_AT), type = "안전주목")
    , data.frame(table(data$SEX_NM, data$INTRST_EXPRN_AT), type = "체험주목")
    , data.frame(table(data$SEX_NM, data$INTRST_BRAND_AT), type = "브랜드주목")
)
    
dataL2 = dataL1 %>%
    dplyr::group_by(type, Var1) %>%
    dplyr::summarise(cnt = sum(Freq, na.rm = TRUE))

dataL3 = dataL1 %>%
    dplyr::left_join(dataL2, by = c("type" = "type", "Var1" = "Var1")) %>%
    dplyr::mutate(
        val = (Freq / cnt) *  100.0
        , isYn = dplyr::case_when(
            Var2 == "Y" ~ "있다"
            , Var2 == "N" ~ "없다"
            , TRUE ~ "NULL"
        ) 
    )%>%
    dplyr::rename(
        "key" = Var1
    )

ggplot(dataL3, aes(x = key, y = val, fill = isYn, label=round(val, 1))) +
    geom_bar(position = "dodge", stat="identity") +
    theme(legend.position = "top") +
    geom_text(aes(group=isYn), position=position_dodge(width=0.9), size=5, vjust=1.6, hjust=0.5, color = "white") +
    ylim(0, 100) +
    facet_wrap( ~ type, scale="free") +
    labs(x = "성별", y = "비율", fill="", subtitle = "") + 
    theme(text = element_text(size=18)) + 
    ggsave(filename = paste(globalVar$figConfig, "Img_040.png", sep="/"), width = 15, height = 10, dpi = 600)

#==========================================
# 나이
#==========================================
dataL1 = dplyr::bind_rows(
    data.frame(table(data$YEAR_NM, data$INTRST_FASHN_AT), type =  "패션주목")
    , data.frame(table(data$YEAR_NM, data$INTRST_PC_AT), type = "가격주목")
    , data.frame(table(data$YEAR_NM, data$INTRST_HEALTH_AT), type = "건강주목")
    , data.frame(table(data$YEAR_NM, data$INTRST_WDOMOUTH_AT), type = "입소문주목")
    , data.frame(table(data$YEAR_NM, data$INTRST_QLITY_AT), type = "품질주목")
    , data.frame(table(data$YEAR_NM, data$INTRST_SAFE_AT), type = "안전주목")
    , data.frame(table(data$YEAR_NM, data$INTRST_EXPRN_AT), type = "체험주목")
    , data.frame(table(data$YEAR_NM, data$INTRST_BRAND_AT), type = "브랜드주목")
)

dataL2 = dataL1 %>%
    dplyr::group_by(type, Var1) %>%
    dplyr::summarise(cnt = sum(Freq, na.rm = TRUE))

dataL3 = dataL1 %>%
    dplyr::left_join(dataL2, by = c("type" = "type", "Var1" = "Var1")) %>%
    dplyr::mutate(
        val = (Freq / cnt) *  100.0
        , isYn = dplyr::case_when(
            Var2 == "Y" ~ "있다"
            , Var2 == "N" ~ "없다"
            , TRUE ~ "NULL"
        ) 
    )%>%
    dplyr::rename(
        "key" = Var1
    )

ggplot(dataL3, aes(x = key, y = val, fill = isYn, label=round(val, 0))) +
    geom_bar(position = "dodge", stat="identity") +
    theme(legend.position = "top") +
    geom_text(aes(group=isYn), position=position_dodge(width=0.9), size=5, vjust=1.6, hjust=0.5, color = "white") +
    ylim(0, 100) +
    facet_wrap( ~ type, scale="free") +
    labs(x = "나이", y = "비율", fill="", subtitle = "") + 
    theme(text = element_text(size=18)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggsave(filename = paste(globalVar$figConfig, "Img_041.png", sep="/"), width = 15, height = 10, dpi = 600)


#==========================================
# 혼인상태
#==========================================
dataL1 = dplyr::bind_rows(
    data.frame(table(data$MRRG_NM, data$INTRST_FASHN_AT), type =  "패션주목")
    , data.frame(table(data$MRRG_NM, data$INTRST_PC_AT), type = "가격주목")
    , data.frame(table(data$MRRG_NM, data$INTRST_HEALTH_AT), type = "건강주목")
    , data.frame(table(data$MRRG_NM, data$INTRST_WDOMOUTH_AT), type = "입소문주목")
    , data.frame(table(data$MRRG_NM, data$INTRST_QLITY_AT), type = "품질주목")
    , data.frame(table(data$MRRG_NM, data$INTRST_SAFE_AT), type = "안전주목")
    , data.frame(table(data$MRRG_NM, data$INTRST_EXPRN_AT), type = "체험주목")
    , data.frame(table(data$MRRG_NM, data$INTRST_BRAND_AT), type = "브랜드주목")
)

dataL2 = dataL1 %>%
    dplyr::group_by(type, Var1) %>%
    dplyr::summarise(cnt = sum(Freq, na.rm = TRUE))

dataL3 = dataL1 %>%
    dplyr::left_join(dataL2, by = c("type" = "type", "Var1" = "Var1")) %>%
    dplyr::mutate(
        val = (Freq / cnt) *  100.0
        , isYn = dplyr::case_when(
            Var2 == "Y" ~ "있다"
            , Var2 == "N" ~ "없다"
            , TRUE ~ "NULL"
        ) 
    )%>%
    dplyr::rename(
        "key" = Var1
    )

ggplot(dataL3, aes(x = type, y = val, fill = isYn, group=isYn, label=round(val, 0))) +
    geom_bar(position = "dodge", stat="identity") +
    theme(legend.position = "top") +
    # geom_text_repel(position=position_dodge(width=0.5), size=5, color = "white") +
    ylim(0, 100) +
    labs(x = "혼인상태", y = "비율", fill="", subtitle = "") + 
    facet_wrap( ~ key) +
    coord_polar() +
    theme_bw() +
    theme(
        axis.text.x = element_text(face = "bold")
        , text = element_text(size=18)
    ) +
    ggsave(filename = paste(globalVar$figConfig, "Img_042.png", sep="/"), width = 15, height = 8, dpi = 600)

# ggplot(dataL3, aes(x = key, y = val, fill = isYn, label=round(val, 0))) +
#     geom_bar(position = "dodge", stat="identity") +
#     theme(legend.position = "top") +
#     geom_text(aes(group=isYn), position=position_dodge(width=0.9), size=5, vjust=1.6, hjust=0.5, color = "white") +
#     ylim(0, 100) +
#     facet_wrap( ~ type, scale="free") +
#     labs(x = "혼인상태", y = "비율", fill="", subtitle = "") + 
#     theme(text = element_text(size=18)) + 
#     # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#     ggsave(filename = paste(globalVar$figConfig, "Img_042.png", sep="/"), width = 15, height = 10, dpi = 600)




#==========================================
# 가정상태
#==========================================
dataL1 = dplyr::bind_rows(
    data.frame(table(data$CHLDRN_NM, data$INTRST_FASHN_AT), type =  "패션주목")
    , data.frame(table(data$CHLDRN_NM, data$INTRST_PC_AT), type = "가격주목")
    , data.frame(table(data$CHLDRN_NM, data$INTRST_HEALTH_AT), type = "건강주목")
    , data.frame(table(data$CHLDRN_NM, data$INTRST_WDOMOUTH_AT), type = "입소문주목")
    , data.frame(table(data$CHLDRN_NM, data$INTRST_QLITY_AT), type = "품질주목")
    , data.frame(table(data$CHLDRN_NM, data$INTRST_SAFE_AT), type = "안전주목")
    , data.frame(table(data$CHLDRN_NM, data$INTRST_EXPRN_AT), type = "체험주목")
    , data.frame(table(data$CHLDRN_NM, data$INTRST_BRAND_AT), type = "브랜드주목")
)

dataL2 = dataL1 %>%
    dplyr::group_by(type, Var1) %>%
    dplyr::summarise(cnt = sum(Freq, na.rm = TRUE))

dataL3 = dataL1 %>%
    dplyr::left_join(dataL2, by = c("type" = "type", "Var1" = "Var1")) %>%
    dplyr::mutate(
        val = (Freq / cnt) *  100.0
        , isYn = dplyr::case_when(
            Var2 == "Y" ~ "있다"
            , Var2 == "N" ~ "없다"
            , TRUE ~ "NULL"
        ) 
    )%>%
    dplyr::rename(
        "key" = Var1
    )

ggplot(dataL3, aes(x = key, y = val, fill = isYn, label=round(val, 0))) +
    geom_bar(position = "dodge", stat="identity") +
    theme(legend.position = "top") +
    geom_text(aes(group=isYn), position=position_dodge(width=0.9), size=5, vjust=1.6, hjust=0.5, color = "white") +
    ylim(0, 100) +
    facet_wrap( ~ type, scale="free") +
    labs(x = "가정상태", y = "비율", fill="", subtitle = "") + 
    theme(text = element_text(size=18)) + 
    # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggsave(filename = paste(globalVar$figConfig, "Img_043.png", sep="/"), width = 15, height = 10, dpi = 600)



#==========================================
# 자산지수
#==========================================
dataL1 = dplyr::bind_rows(
    data.frame(table(data$ASSETS_IDEX_NM, data$INTRST_FASHN_AT), type =  "패션주목")
    , data.frame(table(data$ASSETS_IDEX_NM, data$INTRST_PC_AT), type = "가격주목")
    , data.frame(table(data$ASSETS_IDEX_NM, data$INTRST_HEALTH_AT), type = "건강주목")
    , data.frame(table(data$ASSETS_IDEX_NM, data$INTRST_WDOMOUTH_AT), type = "입소문주목")
    , data.frame(table(data$ASSETS_IDEX_NM, data$INTRST_QLITY_AT), type = "품질주목")
    , data.frame(table(data$ASSETS_IDEX_NM, data$INTRST_SAFE_AT), type = "안전주목")
    , data.frame(table(data$ASSETS_IDEX_NM, data$INTRST_EXPRN_AT), type = "체험주목")
    , data.frame(table(data$ASSETS_IDEX_NM, data$INTRST_BRAND_AT), type = "브랜드주목")
)

dataL2 = dataL1 %>%
    dplyr::group_by(type, Var1) %>%
    dplyr::summarise(cnt = sum(Freq, na.rm = TRUE))

dataL3 = dataL1 %>%
    dplyr::left_join(dataL2, by = c("type" = "type", "Var1" = "Var1")) %>%
    dplyr::mutate(
        val = (Freq / cnt) *  100.0
        , isYn = dplyr::case_when(
            Var2 == "Y" ~ "있다"
            , Var2 == "N" ~ "없다"
            , TRUE ~ "NULL"
        ) 
    )%>%
    dplyr::rename(
        "key" = Var1
    )

ggplot(dataL3, aes(x = type, y = val, fill = isYn, group=isYn, label=round(val, 0))) +
    geom_bar(position = "dodge", stat="identity") +
    theme(legend.position = "top") +
    # geom_text_repel(position=position_dodge(width=0.5), size=5, color = "white") +
    ylim(0, 100) +
    labs(x = "자산지수", y = "비율", fill="", subtitle = "") + 
    facet_wrap( ~ key) +
    coord_polar() +
    theme_bw() +
    theme(
        axis.text.x = element_text(face = "bold")
        , text = element_text(size=18)
    ) +
    ggsave(filename = paste(globalVar$figConfig, "Img_044.png", sep="/"), width = 15, height = 8, dpi = 600)

    # ggplot(dataL3, aes(x = key, y = val, fill = isYn, label=round(val, 0))) +
    #     geom_bar(position = "dodge", stat="identity") +
    #     theme(legend.position = "top") +
    #     geom_text(aes(group=isYn), position=position_dodge(width=0.9), size=5, vjust=1.6, hjust=0.5, color = "white") +
    #     ylim(0, 100) +
    #     facet_wrap( ~ type, scale ="free") +
    #     labs(x = "가정상태", y = "비율", fill="", subtitle = "") + 
    #     theme(text = element_text(size=18)) +
    #     ggsave(filename = paste(globalVar$figConfig, "Img_044.png", sep="/"), width = 15, height = 8, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

rm(list = ls())    #delete objects
cat("\014")        #clear console
library(keras)  #https://keras.rstudio.com/
library(MESS) # calculate auc
#Keras is a high-level neural networks API developed with a focus on enabling fast experimentation. Being able to go from idea to result with the least possible delay is key to doing good research.

#Dataset of 25,000 movies reviews from IMDB, labeled by sentiment (positive/negative). 
#Reviews have been preprocessed, and each review is encoded as a sequence of word indexes (integers). 
#For convenience, words are indexed by overall frequency in the dataset, so that for instance 
#the integer "3" encodes the 3rd most frequent word in the data. This allows for quick filtering 
#operations such as: "only consider the top 10,000 most common words, but eliminate the 
#top 20 most common words".
# https://blogs.rstudio.com/ai/posts/2017-12-07-text-classification-with-keras/
#Lists are the R objects which contain elements of different types like − numbers, strings, vectors and another list inside it. A list can also contain a matrix or a function as its elements. 
p                   =     2500
imdb                =     dataset_imdb(num_words = p, skip_top = 00) #, skip_top = 10
train_data          =     imdb$train$x
train_labels        =     imdb$train$y
test_data           =     imdb$test$x
test_labels         =     imdb$test$y

##아래 아홉줄은 제가 작성한 코드입니다... 많이 허접하지만, 대충 뭘 하려는지 느낌이 오실거라 생각됩니다.문제는 제대로 작동을 안한다는거죠..

numberWords.train   =   max(sapply(train_data, max))
numberWords.test    =   max(sapply(test_data, max))

positive_train_data=subset(imdb$train$x, imdb$train$y==1)
tenpro_positive_train_data=head(positive_train_data, 1250)

positive_train_labels = subset(imdb$train$y, imdb$train$y==1)
tenpro_positive_labels_data=head(positive_train_labels, 1250)

negative_train_data=subset(imdb$train$x, imdb$train$y==0)
negative_train_labels=subset(imdb$train$y, imdb$train$y==0)

new_train_data=rbind(negative_train_data, tenpro_positive_train_data)
new_train_labels=rbind(tenpro_positive_) ##does not work. It should include 13250 elements not 25000.



##sub_imdb=subset(imdb$train$x, imdb$train$y==0)

#c(c(train_data, train_labels), c(test_data, test_labels)) %<-% imdb

#The variables train_data and test_data are lists of reviews; each review is a list of
#word indices (encoding a sequence of words). train_labels and test_labels are
#lists of 0s and 1s, where 0 stands for negative and 1 stands for positive:

str(train_data[[1]])

word_index                   =     dataset_imdb_word_index() #word_index is a named list mapping words to an integer index
reverse_word_index           =     names(word_index) # Reverses it, mapping integer indices to words
names(reverse_word_index)    =     word_index




index.to.review <- function(index) {
    decoded_review <- sapply(train_data[[index]], function(index) {
        word <- if (index >= 3) reverse_word_index[[as.character(index - 3)]]
        if (!is.null(word)) word else "?"
    }) 
    return(decoded_review)
}

cat(index.to.review(10))
# Decodes the review. 
# Note that the indices are offset by 3 because 0, 1, and 2 are reserved indices 
# for “padding,” “start of sequence,” and “unknown.”


vectorize_sequences <- function(sequences, dimension = p) {
    results <- matrix(0, nrow = length(sequences), ncol = dimension)
    for (i in 1:length(sequences))
        results[i, sequences[[i]]] <- 1
    results
}


X.train          =        vectorize_sequences(train_data)
X.test           =        vectorize_sequences(test_data)

#str(X.train[1,])
y.train          =        as.numeric(train_labels)
n.train          =        length(y.train)
y.test           =        as.numeric(test_labels)
n.test           =        length(y.test)


#fit                     =        glm(y.train ~ X.train, family = "binomial")
library(glmnet) 
fit                     =        glmnet(X.train, y.train, family = "binomial", lambda=0.0)

beta0.hat               =        fit$a0
beta.hat                =        as.vector(fit$beta)

# (a) What are the top 10 words associated with positive reviews?
print(paste("word most associated with positive reviews = ", reverse_word_index[[as.character((which.max(beta.hat)-3))]]))

# (b) What are the top 10 words associated with negative reviews?
print(paste("word most associated with negative reviews = ", reverse_word_index[[as.character((which.min(beta.hat)-3))]]))

# (c) How can we identify the review in the training set that is hardest to classify? Find it and present the review. (1 point)
prob.train              =        exp(X.train %*% beta.hat +  beta0.hat  )/(1 + exp(X.train %*% beta.hat +  beta0.hat  ))


hardest.to.classify     =        which.min(abs(prob.train-0.5))
most.positive           =        which.max(prob.train)
most.negative           =        which.min(prob.train)
print(paste("-------------------------------------------------------------"))
print(paste("The hardest review to classify for this logisitc regression model is:  "))
cat(index.to.review(hardest.to.classify))
print(paste("-------------------------------------------------------------"))
print(paste("The most positive review based on this logisitc regression model is:  "))
cat(index.to.review(most.positive))
print(paste("-------------------------------------------------------------"))
print(paste("The most negative review based on this logisitc regression model is: "))
cat(index.to.review(most.negative))
print(paste("-------------------------------------------------------------"))


distance.P     =    (X.train[y.train==1, ] %*% beta.hat + beta0.hat) 
distance.N     =    X.train[y.train==0, ] %*% beta.hat + beta0.hat 

breakpoints = pretty( (min(c(distance.P,distance.N))-0.001):max(c(distance.P,distance.N)),n=200)
# n=200 above refers to the number of bins used in the histogram
# the large the number of bins, the higher the level of detail we see
hg.pos = hist(distance.P, breaks=breakpoints,plot=FALSE) # Save first
hg.neg = hist(distance.N, breaks=breakpoints,plot=FALSE) # Save second
color1 = rgb(0,0,230,max = 255, alpha = 80, names = "lt.blue")
color2 = rgb(255,0,0, max = 255, alpha = 80, names = "lt.pink")

library(latex2exp)

plot(hg.pos,col=color1,xlab=TeX('$x^T  \\beta +  \\beta_0$'),main =   paste("train: histogram  ")) # Plot 1st histogram using a transparent color
plot(hg.neg,col=color2,add=TRUE) # Add 2nd histogram using different color


prob.test               =        exp(X.test %*% beta.hat +  beta0.hat  )/(1 + exp(X.test %*% beta.hat +  beta0.hat  ))
dt                      =        0.01
thta                    =        1-seq(0,1, by=dt)
thta.length             =        length(thta)
FPR.train               =        matrix(0, thta.length)
TPR.train               =        matrix(0, thta.length)
FPR.test                =        matrix(0, thta.length)
TPR.test                =        matrix(0, thta.length)

# The ROC curve is a popular graphic for simultaneously displaying the ROC curve two types of errors for all possible thresholds. 
# The name “ROC” is historic, and comes from communications theory. It is an acronym for receiver operating characteristics.
# ROC curves are useful for comparing different classifiers, since they take into account all possible thresholds.
# varying the classifier threshold changes its true positive and false positive rate. 

for (i in c(1:thta.length)){
    # calculate the FPR and TPR for train data 
    y.hat.train             =        ifelse(prob.train > thta[i], 1, 0) #table(y.hat.train, y.train)
    FP.train                =        sum(y.train[y.hat.train==1] == 0) # false positives = negatives in the data that were predicted as positive
    TP.train                =        sum(y.hat.train[y.train==1] == 1) # true positives = positives in the data that were predicted as positive
    P.train                 =        sum(y.train==1) # total positives in the data
    N.train                 =        sum(y.train==0) # total negatives in the data
    FPR.train[i]            =        FP.train/N.train # false positive rate = type 1 error = 1 - specificity
    TPR.train[i]            =        TP.train/P.train # true positive rate = 1 - type 2 error = sensitivity
    
    # calculate the FPR and TPR for test data 
    y.hat.test              =        ifelse(prob.test > thta[i], 1, 0)
    FP.test                 =        sum(y.test[y.hat.test==1] == 0) # false positives = negatives in the data that were predicted as positive
    TP.test                 =        sum(y.hat.test[y.test==1] == 1) # true positives = positives in the data that were predicted as positive
    P.test                  =        sum(y.test==1) # total positives in the data
    N.test                  =        sum(y.test==0) # total negatives in the data
    FPR.test[i]             =        FP.test/N.test # false positive rate = type 1 error = 1 - specificity
    TPR.test[i]             =        TP.test/P.test # true positive rate = 1 - type 2 error = sensitivity    
    # print(paste("K=", K, " ki=",ki, ", K-fold CV=", Kfold.CV.err[i]))
}
#auc.train = auc(FPR.train, TPR.train)
auc.train     =       sum((TPR.train[1:(thta.length-1)] + 0.5 * diff(TPR.train)) * diff(FPR.train))
auc.test      =       sum((TPR.test[1:(thta.length-1)] + 0.5 * diff(TPR.test)) * diff(FPR.test))

print(paste("train AUC =",sprintf("%.4f", auc.train)))
print(paste("test AUC  =",sprintf("%.4f", auc.test)))


library(ggplot2)

errs.train      =   as.data.frame(cbind(FPR.train, TPR.train))
errs.train      =   data.frame(x=errs.train$V1,y=errs.train$V2,type="Train")
errs.test       =   as.data.frame(cbind(FPR.test, TPR.test))
errs.test       =   data.frame(x=errs.test$V1,y=errs.test$V2,type="Test")
errs            =   rbind(errs.train, errs.test)

ggplot(errs) + geom_line(aes(x,y,color=type)) + labs(x="False positive rate", y="True positive rate") +
    ggtitle("ROC curve",(sprintf("train AUC=%.4f,test AUC =%0.4f",auc.train,auc.test)))


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(ggplot2)
library(tidyverse)
library(forcats)
library(readr)
library(ggrepel)
library(lubridate)
library(scales)

# hue_pal()(2)

fileList = Sys.glob(paste(globalVar$inpConfig, "Brands_All.csv", sep = "/"))

data = readr::read_csv(file = fileList, locale = locale("ko", encoding = "EUC-KR"))

dplyr::tbl_df(data)

# 2개의 그래프를 도출하고 싶은데요.
# 1. 구매력이 큰 사람이 많다 (벨커브)
# 2. 초기에는 구매력이 적었지만 점점 구매력이 상승중인 사람들에게만 노출시켜서 구매를 하게끔 할 것 이다.


summary(data)


# TRANS DATE : 구매날짜
# BILL NO. transaction 번호
# SEQ
# P. TYPE 상태
# STORE 판매된 곳
# BRAND 브랜드명
# PORDUCT GENDER 구매한 상품에 관한 성별(남성, 여성, 아동)
# STYLE 스타일번호(의미없음)
# COLOR 색상(의미없음)
# COLOR DESC. 색상 설명(의미없음)
# SIZE 사이즈
# CATEGORY 카테고리
# STYLE DESC. 상세 카테고리
# Q'TY 구매 수량
# DC
# GROSS AMOUNT 판매가격
# NET AMOUNT 실제 구매 가격
# NA. 국가
# UID 고객 구분 key(UID가 없는 일부 고객은 개별고객으로 분류 하시면 될 것 같습니다)
# CUST GENDER 구매자 성별
# AGE 구매자 나이대
# BRI YEAR 구매자 태어난 년도
# SEASON 판매 시즌
# COST 원가


dataL1 = data %>%
  dplyr::select(`Q'TY`, `NET AMOUNT`, `UID`, `PORDUCT GENDER`, `TRANS DATE`, `UID`) %>%
  dplyr::filter(
    `Q'TY` > 0 
    , `NET AMOUNT` > 0 
    , ! is.na(UID)
  ) %>%
  dplyr::mutate(
    dtDate = readr::parse_date(as.character(`TRANS DATE`), "%Y%m%d")
    , dtYear = lubridate::year(dtDate)
    , val = `Q'TY` * `NET AMOUNT` / 100000
    )
  
summary(dataL2)

topPer70 = mean(dataL2$meanVal, na.rm = TRUE) + (sd(dataL2$meanVal, na.rm = TRUE))

dataL2 = dataL1 %>%
  dplyr::group_by(dtDate, UID) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  ) %>%
  dplyr::mutate(type = dplyr::case_when(
    meanVal > topPer70 ~ "TOP 30%"
    , TRUE ~ "BOT 70%"
  ))

# 1. 구매력이 큰 사람이 많다 (벨커브)
ggplot(dataL2, aes(x = meanVal, colour = type, fill = type)) +
  geom_histogram(aes(y=..density..), binwidth = 0.2,  colour="black", fill="white") +
  geom_density(alpha = 0.2) +
  stat_function(fun = dnorm, args = list(mean = mean(dataL2$meanVal, na.rm = TRUE), sd = sd(dataL2$meanVal, na.rm = TRUE)), col = 'blue') +
  geom_rug(aes(x = meanVal, y = 0), position = position_jitter(height = 0)) +
  labs(x = "구매력 [단위 : 1,000,000]", y = "밀도 함수", colour = "특성", fill="특성", subtitle = "") +
  xlim(0, 20) +
  ylim(0, 0.35) +
  theme(text = element_text(size=18)) +
  ggsave(filename = "FIG/o2job/Img_050.png", width = 12, height = 6, dpi = 600)
  


# 2. 초기에는 구매력이 적었지만 점점 구매력이 상승중인 사람들에게만 노출시켜서 구매를 하게끔 할 것 이다.

topUid10 = dataL1 %>%
  dplyr::group_by(UID) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
    , cnt = n()
  ) %>%
  dplyr::filter(cnt > 5) %>%
  dplyr::arrange(desc(cnt)) %>%
  dplyr::top_n(10)

botUid10 = dataL1 %>%
  dplyr::group_by(UID) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
    , cnt = n()
  ) %>%
  dplyr::filter(cnt > 5) %>%
  dplyr::arrange(cnt) %>%
  dplyr::slice(1:10)


topBotUid10 = dplyr::bind_rows(topUid10, botUid10) %>%
  dplyr::distinct(UID)


topPer70 = mean(dataL2$cnt, na.rm = TRUE) + (sd(dataL2$cnt, na.rm = TRUE))

dataL2 = dataL1 %>%
  dplyr::group_by(UID, dtYear) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
    , cnt = n()
  ) %>%
  dplyr::mutate(type = dplyr::case_when(
    cnt > topPer70 ~ "TOP 30%"
    , TRUE ~ "BOT 70%"
  ))

dataL3 = dataL2 %>%
  dplyr::filter(UID %in% topBotUid10$UID)


dataL4 = dataL3 %>%
  dplyr::filter(type == "BOT 70%")

lmFitBot = loess(meanVal ~ dtYear, data = dataL4)
summary(lmFitBot)

dataL4 = dataL3 %>%
  dplyr::filter(type == "TOP 30%")

lmFitTop = loess(meanVal ~ dtYear, data = dataL4)
summary(lmFitTop)


ggplot(dataL3, aes(x = dtYear, y = meanVal, colour = type, shape = type)) +
  geom_point(size = 3) +
  geom_smooth(method='loess', formula = y ~ x, se = FALSE) +
  labs(x = "년도", y = "구매력 [단위 : 1,000,000]", shape="특성", colour="특성", fill="", subtitle = "") +
  # annotate("text", x = 2011, y = 7.75, size = 5, label = sprintf("Y = %.2f X - %.2f", 0.0034, 3.2693), hjust = 0, color = hue_pal()(1)) +
  # annotate("text", x = 2011, y = 7.25, size = 5, label = sprintf("Y = %.2f X - %.2f", 0.2101, 419.6287), hjust = 0, color = hue_pal()(2)) +
  annotate("text", x = 2011, y = 7.75, size = 5, label = sprintf("Span is %.2f (DF : %.2f)", 0.75, 2), hjust = 0, color = hue_pal()(1)) +
  annotate("text", x = 2011, y = 7.25, size = 5, label = sprintf("Span is %.2f (DF : %.2f)", 0.75, 2), hjust = 0, color = hue_pal()(2)) +
  xlim(2010, 2020) +
  ylim(0, 8) +
  theme(text = element_text(size=18)) +
  ggsave(filename = "FIG/o2job/Img_051.png", width = 10, height = 8, dpi = 600)

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(ggplot2)
library(tidyverse)
library(forcats)
library(readr)
library(ggrepel)
library(lubridate)
library(scales)
library(neural)
library(scales)
library(neuralnet)
library(ggpubr)
library(h2o)

# https://www.jstage.jst.go.jp/article/pjsai/JSAI2010/0/JSAI2010_2D14/_pdf/-char/ja

# hue_pal()(2)

fileInfo = Sys.glob(paste(globalVar$inpConfig, "out.csv", sep = "/"))
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))


fileInfo = Sys.glob(paste(globalVar$inpConfig, "out2.csv", sep = "/"))
data2 = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))

dplyr::tbl_df(data)
dplyr::tbl_df(data2)

# parse_date_time("5.14", "%m.%d")
# paste(2012, 12, sep = "-")
# data2L1$dtReqDate

data2L1 = data2 %>%
    na.omit() %>%
    dplyr::filter(계절현상 %in% c("개화", "단풍시작")) %>%
    tidyr::gather(-c(X1, 계절관측, 계절현상, 연도), key = "key", value = "val") %>%
    dplyr::mutate(
        dtDate = lubridate::parse_date_time2(val, "%m.%d")
        , dtMonth = lubridate::month(dtDate)
        , dtDay = lubridate::day(dtDate)
        , sDate = paste(연도, dtMonth, dtDay, sep = "-")
        , dtReqDate = readr::parse_date(sDate, "%Y-%m-%d")
        , dtYear = lubridate::year(dtReqDate)
        , dtReqJul = lubridate::yday(dtReqDate)
    )

dataL1 = data %>%
    # dplyr::select(date, 강릉_AOT40, 강릉_temp, 강릉_RH) %>%
    dplyr::mutate(
        dtYear = lubridate::year(date)
        , dtJul = lubridate::yday(date)
    ) %>%
    # dplyr::left_join(data2L1, by = c("dtYear" = "dtYear")) %>%
    dplyr::left_join(data2L1, by = c("date" = "dtReqDate")) %>%
    dplyr::filter(
        계절관측 == "벚나무"
        , 계절현상 == "개화"
        # , key == "강릉"
    ) %>%
    dplyr::mutate(
        dtDiff = dtReqJul - dtJul 
    ) %>%
    dplyr::arrange(date) %>%
    na.omit() %>%
    dplyr::select(강릉_AOT40, 강릉_temp, 강릉_RH, dtReqJul) %>%
    dplyr::rename(
        aot = "강릉_AOT40"
        , temp = "강릉_temp"
        , rh = "강릉_RH"
    )


# 표준화 방법 (0 - 1 변환)
dataL1$aot = scales::rescale(dataL1$aot)
dataL1$temp = scales::rescale(dataL1$temp)
dataL1$rh = scales::rescale(dataL1$rh)


dataL1 %>%
    dplyr::select(강릉_AOT40, 강d릉_temp, 강릉_RH, dtJul) %>%
    ggpairs(.) + 
    theme(text = element_text(size=18)) + 
    ggsave(filename = paste(globalVar$figConfig, "Img_060_ggpairs.png", sep="/"), width = 12, height = 8, dpi = 600)

dataL1 %>%
    dplyr::select(강릉_AOT40, 강릉_temp, 강릉_RH, dtJul) %>%
    cor()
    

trainData = dataL1
testData = dataL1

#====================================
# 데이터 분할
#====================================
ind = nrow(dataL1)

# 훈련 및 데이터 셋을 60:40으로 나누기 위한 인덱스 설정
# ind = sample(1:nrow(dataL1), nrow(dataL1) * 0.6)

# 해당 인덱스에 따라 자료 할당
trainData = dataL1[-ind, ]
testData = dataL1[ind, ]

# 훈련 데이터셋 확인
dplyr::tbl_df(trainData)

# 테스트 데이터셋 확인
dplyr::tbl_df(testData)


xVar = "aot + temp + rh"
yVar = "dtReqJul"

form = paste0(yVar, " ~ ", xVar)
# form = "dtDiff ~ 강릉_AOT40 + 강릉_temp + 강릉_RH"


resultData = data.frame()

#===============================================
# 다중선형회귀 방법
#===============================================
lmFit = lm(form, data = trainData)
summary(lmFit)
# MASS::stepAIC(lmFit, direction = "both") %>% summary()

predictions = predict(lmFit, newdata = testData)

resultData = dplyr::bind_rows(resultData
    , data.frame(
        type = "MLR"
        , pred = predictions
        , real = testData$dtReqJul
        )
    )

#===============================================
# neuralnet 방법
#===============================================
neuralModel = neuralnet::neuralnet(
    formula = form 
    , data = trainData
    , hidden = c(10)
    # , hidden = c(64)
    , learningrate = 0.01
    , threshold = 0.01   
    , stepmax = 50000
    , rep = 2
)


png(file = paste(globalVar$figConfig, "Img_060_HiddenLayer.png", sep='/'), width = 10, height = 5, units="in", res = 1200)
plot(neuralModel, rep = 'best')
dev.off()

rsPredictions = neuralnet::compute(neuralModel, testData)
predictions = rsPredictions$net.result

resultData = dplyr::bind_rows(resultData
    , data.frame(
        type = "ANN"
        , pred = predictions
        , real = testData$dtReqJul
    )
)


#===============================================
# Deep Learning 방법
#===============================================

# initiallization
h2o.init()

x = c("aot", "temp", "rh")



# trainData$dtReqJul
# a

# model = h2o::h2o.deeplearning(x=x, y=yVar, training_frame = as.h2o(trainData))

model = h2o::h2o.deeplearning(
    x = x
    , y = yVar
    , training_frame = as.h2o(trainData)
    # , nfolds = 1
    # , stopping_metric = 'RMSE'
    # , epochs = 400
    # , overwrite_with_best_model = TRUE
    # , activation = 'Tanh'
    # , input_dropout_ratio = 0.1
    , hidden = c(64, 64)
    # , l1 = 0
    , loss = 'Automatic'
    , distribution = 'AUTO'
    )

summary(model)

rsPredictions = as.data.frame(predict(model, as.h2o(testData)))
predictions = rsPredictions$predict

resultData = dplyr::bind_rows(resultData
    , data.frame(
        type = "DL"
        , pred = predictions
        , real = testData$dtReqJul
    )
)

#=====================================
# Plot
#=====================================
actualValues = testData$dtReqJul

result = data.frame(
    predictions
    , actualValues
) 

xAxis = result$predictions
yAxis = result$actualValues

# plot(yAxis, col = "red")
# points(xAxis)

plot(xAxis, yAxis, col='blue', main='Recl vs Predicted', pch=1, type="p", xlab="Actual", ylab="Predicted")

cor(xAxis, yAxis)
Metrics::bias(xAxis, yAxis)
Metrics::rmse(xAxis, yAxis)

abline(0, 1, col="black")




# 다중선형회귀모형
resultDataL1 = resultData %>%
    dplyr::filter(type == "MLR")

xAxis = resultDataL1$pred
yAxis = resultDataL1$real

corTest = cor.test(xAxis, yAxis)
corVal = round(corTest$estimate, 2)
pVal = round(corTest$p.value, 2)
biasVal = round(Metrics::bias(xAxis, yAxis), 2)
rmseVal = round(Metrics::rmse(xAxis, yAxis), 2)

ggscatter(resultDataL1, x = "pred", y = "real", color = "black", add = "reg.line", conf.int = TRUE, add.params = list(color = "blue", fill = "lightblue")) +
    stat_regline_equation(label.x = 80, label.y = 100, size = 5) +
    annotate("text", x = 80, y = 97.5, size = 5, label = sprintf("R = %s (P < %.3f)", corVal, pVal), hjust = 0) +
    annotate("text", x = 80, y = 95, size = 5, label = sprintf("Bias = %s", biasVal), hjust = 0) +
    annotate("text", x = 80, y = 92.5, size = 5, label = sprintf("RMSE = %s", rmseVal), hjust = 0) +
    xlim(80, 100) + 
    ylim(80, 100) + 
    theme_bw() +
    labs(title = "", x = "예측", y = "실제", subtitle = "다중선형회귀모형") +
    coord_equal() +
    theme(text = element_text(size=18)) + 
    ggsave(filename = paste(globalVar$figConfig, "Img_060_MLR.png", sep="/"), width = 6, height = 6, dpi = 600)


# ANN
resultDataL1 = resultData %>%
    dplyr::filter(type == "ANN")

xAxis = resultDataL1$pred
yAxis = resultDataL1$real

corTest = cor.test(xAxis, yAxis)
corVal = round(corTest$estimate, 2)
pVal = round(corTest$p.value, 2)
biasVal = round(Metrics::bias(xAxis, yAxis), 2)
rmseVal = round(Metrics::rmse(xAxis, yAxis), 2)

ggscatter(resultDataL1, x = "pred", y = "real", color = "black", add = "reg.line", conf.int = TRUE, add.params = list(color = "blue", fill = "lightblue")) +
    stat_regline_equation(label.x = 80, label.y = 100, size = 5) +
    annotate("text", x = 80, y = 97.5, size = 5, label = sprintf("R = %s (P < %.3f)", corVal, pVal), hjust = 0) +
    annotate("text", x = 80, y = 95, size = 5, label = sprintf("Bias = %s", biasVal), hjust = 0) +
    annotate("text", x = 80, y = 92.5, size = 5, label = sprintf("RMSE = %s", rmseVal), hjust = 0) +
    xlim(80, 100) + 
    ylim(80, 100) + 
    theme_bw() +
    labs(title = "", x = "예측", y = "실제", subtitle = "인공신경망") +
    coord_equal() +
    theme(text = element_text(size=18)) + 
    ggsave(filename = paste(globalVar$figConfig, "Img_060_ANN.png", sep="/"), width = 6, height = 6, dpi = 600)


# DL
resultDataL1 = resultData %>%
    dplyr::filter(type == "DL")

xAxis = resultDataL1$pred
yAxis = resultDataL1$real

corTest = cor.test(xAxis, yAxis)
corVal = round(corTest$estimate, 2)
pVal = round(corTest$p.value, 2)
biasVal = round(Metrics::bias(xAxis, yAxis), 2)
rmseVal = round(Metrics::rmse(xAxis, yAxis), 2)

ggscatter(resultDataL1, x = "pred", y = "real", color = "black", add = "reg.line", conf.int = TRUE, add.params = list(color = "blue", fill = "lightblue")) +
    stat_regline_equation(label.x = 80, label.y = 100, size = 5) +
    annotate("text", x = 80, y = 97.5, size = 5, label = sprintf("R = %s (P < %.3f)", corVal, pVal), hjust = 0) +
    annotate("text", x = 80, y = 95, size = 5, label = sprintf("Bias = %s", biasVal), hjust = 0) +
    annotate("text", x = 80, y = 92.5, size = 5, label = sprintf("RMSE = %s", rmseVal), hjust = 0) +
    xlim(80, 100) + 
    ylim(80, 100) + 
    theme_bw() +
    labs(title = "", x = "예측", y = "실측", subtitle = "딥러닝") +
    coord_equal() +
    theme(text = element_text(size=18)) + 
    ggsave(filename = paste(globalVar$figConfig, "Img_060_DL.png", sep="/"), width = 6, height = 6, dpi = 600)




#===============================================
# mlptrain, mlp 방법
#===============================================

inDataL2 = dataL1 %>%
    dplyr::select(강릉_AOT40, 강릉_temp, 강릉_RH) %>%
    # scale() %>%
    as.matrix()

outDataL2 = dataL1 %>%
    dplyr::select(dtJul) %>%
    # scale() %>%
    as.matrix()

neurons = 4

data<-mlptrain(inDataL2, neurons = 64, outDataL2, it=10000);
mlp(x,data$weight,data$dist,data$neurons,data$actfns)


# mlptrain
# 
# mlp
# # 
# dataL2 
# 
# 
# x<-matrix(c(1,1,0,0,1,0,1,0),4,2)
# y<-matrix(c(0,1,1,0),4,1)
# neurons<-4
# data<-mlptrain(x,neurons,y,it=4000);
# mlp(x,data$weight,data$dist,data$neurons,data$actfns)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

#==========================================
# 선호도 점수
#==========================================

library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
library(plotly)
library(arules)
library(arulesViz)
library(visNetwork)
library(rlang)
library(igraph)
library(kableExtra)
library(tibble)
library(png)
library(xlsx)
library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
library(plotly)
library(arules)
library(arulesViz)
library(visNetwork)
library(igraph)
library(kableExtra)
library(tibble)
library(png)

# hue_pal()(2)

BWS_SCORE <- function(data = data) {
  
  result_data <- data %>%
    dplyr::mutate(
      B_W01 = b1 - w1
      , B_W02 = b2 - w2
      , B_W03 = b3 - w3
      , B_W04 = b4 - w4
      , B_W05 = b5 - w5
      , B_W06 = b6 - w6
      , B_W07 = b7 - w7
      , B_W08 = b8 - w8
      , B_W09 = b9 - w9
      , B_W10 = b10 - w10
      , B_W11 = b11 - w11
      , B_W12 = b12 - w12
      , B_W13 = b13 - w13
      , BWMEAN01 = (b1 - w1) / ((b1 + w1) * questnum) 
      , BWMEAN02 = (b2 - w2) / ((b2 + w2) * questnum)
      , BWMEAN03 = (b3 - w3) / ((b3 + w3) * questnum)
      , BWMEAN04 = (b4 - w4) / ((b4 + w4) * questnum)
      , BWMEAN05 = (b5 - w5) / ((b5 + w5) * questnum)
      , BWMEAN06 = (b6 - w6) / ((b6 + w6) * questnum)
      , BWMEAN07 = (b7 - w7) / ((b7 + w7) * questnum)
      , BWMEAN08 = (b8 - w8) / ((b8 + w8) * questnum)
      , BWMEAN09 = (b9 - w9) / ((b9 + w9) * questnum)
      , BWMEAN10 = (b10 - w10) / ((b10 + w10) * questnum)
      , BWMEAN11 = (b11 - w11) / ((b11 + w11) * questnum)
      , BWMEAN12 = (b12 - w12) / ((b12 + w12) * questnum)
      , BWMEAN13 = (b13 - w13) / ((b13 + w13) * questnum)
      , BWSQRT01 = sqrt(b1/w1)
      , BWSQRT02 = sqrt(b2/w2)
      , BWSQRT03 = sqrt(b3/w3)
      , BWSQRT04 = sqrt(b4/w4)
      , BWSQRT05 = sqrt(b5/w5)
      , BWSQRT06 = sqrt(b6/w6)
      , BWSQRT07 = sqrt(b7/w7)
      , BWSQRT08 = sqrt(b8/w8)
      , BWSQRT09 = sqrt(b9/w9)
      , BWSQRT10 = sqrt(b10/w10)
      , BWSQRT11 = sqrt(b11/w11)
      , BWSQRT12 = sqrt(b12/w12)
      , BWSQRT13 = sqrt(b13/w13)
      )
  
  return(result_data)
  
}


# fileInfo = Sys.glob(paste(globalVar$inpConfig, "wine.csv", sep = "/"))
fileInfo = Sys.glob(paste(globalVar$inpConfig, "wine2.csv", sep = "/"))
data_wind <- read.csv(fileInfo)

fileInfo = Sys.glob(paste(globalVar$inpConfig, "인구통계별.xlsx", sep = "/"))
data_human <- xlsx::read.xlsx2(fileInfo, sheetIndex = 1)

data_human = data_human %>%
  dplyr::distinct()

data_human$id <- as.numeric(data_human$id)
full_data <- dplyr::inner_join(data_wind,data_human,by=c("id"))

## BWS 분석 데이터셋 만들기 ##
questnum = 4.0
full_data_L1 <- BWS_SCORE(full_data)


## SEQ1 탐색적 데이터 분석 (B-W를 이용한 BAR 그래프) ##

################# 전체 데이터 테이블화 #####################

# 요소 추출 #
sex_full <- unique(full_data_L1$sex)
age_full <- unique(full_data_L1$age)
nfam_full <- unique(full_data_L1$nfam)
edu_full <- unique(full_data_L1$edu)
income_full <- unique(full_data_L1$income)
job_full <- unique(full_data_L1$job)
marr_full <- unique(full_data_L1$marr)


xindex <- c("B-W", "Mean(B-W)", "SQRT(B-W)")
yindex <- c("색", "향", "맛", "생산지역", "생산국가", "가격", "브랜드", "빈티지", "품종", "추천", "프로모션", "레이블", "알콜도수")

## 전체 데이터 탐색 ##
full_data_PARTR_ALL <- full_data_L1 %>%
  dplyr::mutate(all = 1) %>%
  dplyr::group_by(all) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(B_W01:BWSQRT13)

out_data <- matrix(data = full_data_PARTR_ALL, nrow = 13, ncol = 3)
out_data_L1 <- as.data.frame(out_data,row.names = yindex)
colnames(out_data_L1) <- xindex



fwrite(out_data_L1, paste(globalVar$outConfig, "BWS/all.csv", sep="/"), row.names = TRUE)

for (i in sex_full) {
  
  ## 전체 데이터 탐색 ##
  full_data_PARTR_ALL <- full_data_L1 %>%
    dplyr::filter(sex == i) %>%
    dplyr::group_by(sex) %>%
    dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
    dplyr::select(B_W01:BWSQRT13)
  
  Sys.sleep(3)
  
  out_data <- matrix(data = full_data_PARTR_ALL, nrow = 13, ncol = 3)
  out_data_L1 <- as.data.frame(out_data,row.names = yindex)
  colnames(out_data_L1) <- xindex
  
  fwrite(out_data_L1, paste(globalVar$outConfig, paste0("BWS/sex_", i, ".csv"), sep="/"), row.names = TRUE)
}


for (i in age_full) {
  
  ## 전체 데이터 탐색 ##
  full_data_PARTR_ALL <- full_data_L1 %>%
    dplyr::filter(age == i) %>%
    dplyr::group_by(age) %>%
    dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
    dplyr::select(B_W01:BWSQRT13)
  
  Sys.sleep(2)
  
  out_data <- matrix(data = full_data_PARTR_ALL, nrow = 13, ncol = 3)
  out_data_L1 <- as.data.frame(out_data,row.names = yindex)
  colnames(out_data_L1) <- xindex
  
  fwrite(out_data_L1, paste(globalVar$outConfig, paste0("BWS/age_", i, ".csv"), sep="/"), row.names = TRUE)
  
  
}


for (i in nfam_full) {
  
  ## 전체 데이터 탐색 ##
  full_data_PARTR_ALL <- full_data_L1 %>%
    dplyr::filter(nfam == i) %>%
    dplyr::group_by(nfam) %>%
    dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
    dplyr::select(B_W01:BWSQRT13)
  
  Sys.sleep(2)
  
  out_data <- matrix(data = full_data_PARTR_ALL, nrow = 13, ncol = 3)
  out_data_L1 <- as.data.frame(out_data,row.names = yindex)
  colnames(out_data_L1) <- xindex
  
  fwrite(out_data_L1, paste(globalVar$outConfig, paste0("BWS/nfam_", i, ".csv"), sep="/"), row.names = TRUE)
  
}




for (i in edu_full) {
  
  ## 전체 데이터 탐색 ##
  full_data_PARTR_ALL <- full_data_L1 %>%
    dplyr::filter(edu == i) %>%
    dplyr::group_by(edu) %>%
    dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
    dplyr::select(B_W01:BWSQRT13)
  
  Sys.sleep(2)
  
  out_data <- matrix(data = full_data_PARTR_ALL, nrow = 13, ncol = 3)
  out_data_L1 <- as.data.frame(out_data,row.names = yindex)
  colnames(out_data_L1) <- xindex
  
  fwrite(out_data_L1, paste(globalVar$outConfig, paste0("BWS/edu_", i, ".csv"), sep="/"), row.names = TRUE)
}

for (i in income_full) {
  
  ## 전체 데이터 탐색 ##
  full_data_PARTR_ALL <- full_data_L1 %>%
    dplyr::filter(income == i) %>%
    dplyr::group_by(income) %>%
    dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
    dplyr::select(B_W01:BWSQRT13)
  
  Sys.sleep(2)
  
  out_data <- matrix(data = full_data_PARTR_ALL, nrow = 13, ncol = 3)
  out_data_L1 <- as.data.frame(out_data,row.names = yindex)
  colnames(out_data_L1) <- xindex
  
  fwrite(out_data_L1, paste(globalVar$outConfig, paste0("BWS/income_", i, ".csv"), sep="/"), row.names = TRUE)
  
  
}


for (i in job_full) {
  
  ## 전체 데이터 탐색 ##
  full_data_PARTR_ALL <- full_data_L1 %>%
    dplyr::filter(job == i) %>%
    dplyr::group_by(job) %>%
    dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
    dplyr::select(B_W01:BWSQRT13)
  
  Sys.sleep(2)
  
  out_data <- matrix(data = full_data_PARTR_ALL, nrow = 13, ncol = 3)
  out_data_L1 <- as.data.frame(out_data,row.names = yindex)
  colnames(out_data_L1) <- xindex
  
  fwrite(out_data_L1, paste(globalVar$outConfig, paste0("BWS/job_", i, ".csv"), sep="/"), row.names = TRUE)
}



for (i in marr_full) {
  
  ## 전체 데이터 탐색 ##
  full_data_PARTR_ALL <- full_data_L1 %>%
    dplyr::filter(marr == i) %>%
    dplyr::group_by(marr) %>%
    dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
    dplyr::select(B_W01:BWSQRT13)
  
  Sys.sleep(2)
  
  out_data <- matrix(data = full_data_PARTR_ALL, nrow = 13, ncol = 3)
  out_data_L1 <- as.data.frame(out_data,row.names = yindex)
  colnames(out_data_L1) <- xindex
  
  fwrite(out_data_L1, paste(globalVar$outConfig, paste0("BWS/marr_", i, ".csv"), sep="/"), row.names = TRUE)
}



################# 전체 데이터 테이블화 #####################



## 전체 데이터 탐색 ##
full_data_PARTR_ALL <- full_data_L1 %>%
  dplyr::mutate(all = 1) %>%
  dplyr::group_by(all) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(all, BWSQRT01:BWSQRT13) %>%
  dplyr::rename(
    "색" = "BWSQRT01"
    , "향" = "BWSQRT02"
    , "맛" = "BWSQRT03"
    , "생산지역" = "BWSQRT04"
    , "생산국가" = "BWSQRT05"
    , "가격" = "BWSQRT06"
    , "브랜드" = "BWSQRT07"
    , "빈티지" = "BWSQRT08"
    , "품종" = "BWSQRT09"
    , "추천" = "BWSQRT10"
    , "프로모션" = "BWSQRT11"
    , "레이블" = "BWSQRT12"
    , "알콜도수" = "BWSQRT13"
  )


dataL1_all = full_data_PARTR_ALL %>%
  tidyr::gather(-all, key="key", val="val")

dataL1_all$key = forcats::fct_relevel(dataL1_all$key, yindex)

ggplot(dataL1_all, aes(x = key, y = val, fill = key)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  geom_text(aes(x=key, y=val, label = round(val, 2)), size = 5, color="white", vjust=2, position = position_dodge(width=0.9)) +
  geom_hline(yintercept=1) +
  labs(x = "", y = "선호도 점수", fill="", subtitle = "[전체] 항목에 따른 선호도 점수") + 
  theme(text = element_text(size=18)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figConfig, "BWS/full.png", sep="/"), width = 12, height = 8, dpi = 600)


## 성별에 따른 항목별 스코어 차이 ##
full_data_PARTR_SEX <- full_data_L1 %>%
  dplyr::group_by(sex) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(sex,BWSQRT01:BWSQRT13) %>%
  dplyr::rename(
    "색" = "BWSQRT01"
    , "향" = "BWSQRT02"
    , "맛" = "BWSQRT03"
    , "생산지역" = "BWSQRT04"
    , "생산국가" = "BWSQRT05"
    , "가격" = "BWSQRT06"
    , "브랜드" = "BWSQRT07"
    , "빈티지" = "BWSQRT08"
    , "품종" = "BWSQRT09"
    , "추천" = "BWSQRT10"
    , "프로모션" = "BWSQRT11"
    , "레이블" = "BWSQRT12"
    , "알콜도수" = "BWSQRT13"
  )

dataL1_sex = full_data_PARTR_SEX %>%
  tidyr::gather(-sex, key="key", val="val")

dataL1_sex$key = forcats::fct_relevel(dataL1_sex$key, yindex)

ggplot(dataL1_sex, aes(x = key, y = val, fill = sex)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  geom_text(aes(x=key, y=val, label = round(val, 1)), size = 5, color="white", vjust=2, position = position_dodge(width=0.9)) +
  geom_hline(yintercept=1) +
  labs(x = "", y = "선호도 점수", fill="", subtitle = "[성별] 항목에 따른 선호도 점수") + 
  theme(text = element_text(size=18)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figConfig, "BWS/sex.png", sep="/"), width = 12, height = 8, dpi = 600)


## 나이에 따른 항목별 스코어 차이 ##
full_data_PARTR_AGE <- full_data_L1 %>%
  dplyr::group_by(age) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(age,BWSQRT01:BWSQRT13) %>%
  dplyr::rename(
    "색" = "BWSQRT01"
    , "향" = "BWSQRT02"
    , "맛" = "BWSQRT03"
    , "생산지역" = "BWSQRT04"
    , "생산국가" = "BWSQRT05"
    , "가격" = "BWSQRT06"
    , "브랜드" = "BWSQRT07"
    , "빈티지" = "BWSQRT08"
    , "품종" = "BWSQRT09"
    , "추천" = "BWSQRT10"
    , "프로모션" = "BWSQRT11"
    , "레이블" = "BWSQRT12"
    , "알콜도수" = "BWSQRT13"
  )


dataL1_age = full_data_PARTR_AGE %>%
  tidyr::gather(-age, key="key", val="val")

dataL1_age$key = forcats::fct_relevel(dataL1_age$key, yindex)

ggplot(dataL1_age, aes(x = key, y = val, fill = age)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  # geom_text(aes(x=key, y=val, label = round(val, 1)), size = 5, color="white", vjust=2, position = position_dodge(width=0.9),size = 2) +
  geom_hline(yintercept=1) +
  labs(x = "", y = "선호도 점수", fill="", subtitle = "[나이] 항목에 따른 선호도 점수") + 
  theme(text = element_text(size=18)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figConfig, "BWS/age.png", sep="/"), width = 12, height = 8, dpi = 600)


## nfam에 따른 항목별 스코어 차이 ##
full_data_PARTR_nfam <- full_data_L1 %>%
  dplyr::group_by(nfam) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(nfam,BWSQRT01:BWSQRT13) %>%
  dplyr::rename(
    "색" = "BWSQRT01"
    , "향" = "BWSQRT02"
    , "맛" = "BWSQRT03"
    , "생산지역" = "BWSQRT04"
    , "생산국가" = "BWSQRT05"
    , "가격" = "BWSQRT06"
    , "브랜드" = "BWSQRT07"
    , "빈티지" = "BWSQRT08"
    , "품종" = "BWSQRT09"
    , "추천" = "BWSQRT10"
    , "프로모션" = "BWSQRT11"
    , "레이블" = "BWSQRT12"
    , "알콜도수" = "BWSQRT13"
  )



dataL1_nfam = full_data_PARTR_nfam %>%
  tidyr::gather(-nfam, key="key", val="val")

dataL1_nfam$key = forcats::fct_relevel(dataL1_nfam$key, yindex)

ggplot(dataL1_nfam, aes(x = key, y = val, fill = nfam)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  geom_text(aes(x=key, y=val, label = round(val, 0)), size = 5, color="white", vjust=2, position = position_dodge(width=0.9),size = 2) +
  geom_hline(yintercept=1) +
  labs(x = "", y = "선호도 점수", fill="", subtitle = "항목에 따른 선호도 점수") + 
  theme(text = element_text(size=18)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figConfig, "BWS/nfam.png", sep="/"), width = 12, height = 8, dpi = 600)


## edu에 따른 항목별 스코어 차이 ##
full_data_PARTR_edu <- full_data_L1 %>%
  dplyr::group_by(edu) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(edu,BWSQRT01:BWSQRT13) %>%
  dplyr::rename(
    "색" = "BWSQRT01"
    , "향" = "BWSQRT02"
    , "맛" = "BWSQRT03"
    , "생산지역" = "BWSQRT04"
    , "생산국가" = "BWSQRT05"
    , "가격" = "BWSQRT06"
    , "브랜드" = "BWSQRT07"
    , "빈티지" = "BWSQRT08"
    , "품종" = "BWSQRT09"
    , "추천" = "BWSQRT10"
    , "프로모션" = "BWSQRT11"
    , "레이블" = "BWSQRT12"
    , "알콜도수" = "BWSQRT13"
  )

dataL1_edu = full_data_PARTR_edu %>%
  tidyr::gather(-edu, key="key", val="val")

dataL1_edu$key = forcats::fct_relevel(dataL1_edu$key, yindex)

ggplot(dataL1_edu, aes(x = key, y = val, fill = edu)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  geom_text(aes(x=key, y=val, label = round(val, 0)), size = 5, color="white", vjust=2, position = position_dodge(width=0.9),size = 2) +
  geom_hline(yintercept=1) +
  labs(x = "", y = "선호도 점수", fill="", subtitle = "[교육] 항목에 따른 선호도 점수") + 
  theme(text = element_text(size=18)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figConfig, "BWS/edu.png", sep="/"), width = 12, height = 8, dpi = 600)



## income에 따른 항목별 스코어 차이 ##
full_data_PARTR_income <- full_data_L1 %>%
  dplyr::group_by(income) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(income,BWSQRT01:BWSQRT13) %>%
  dplyr::rename(
    "색" = "BWSQRT01"
    , "향" = "BWSQRT02"
    , "맛" = "BWSQRT03"
    , "생산지역" = "BWSQRT04"
    , "생산국가" = "BWSQRT05"
    , "가격" = "BWSQRT06"
    , "브랜드" = "BWSQRT07"
    , "빈티지" = "BWSQRT08"
    , "품종" = "BWSQRT09"
    , "추천" = "BWSQRT10"
    , "프로모션" = "BWSQRT11"
    , "레이블" = "BWSQRT12"
    , "알콜도수" = "BWSQRT13"
  )



dataL1_income = full_data_PARTR_income %>%
  tidyr::gather(-income, key="key", val="val")

dataL1_income$key = forcats::fct_relevel(dataL1_income$key, yindex)

ggplot(dataL1_income, aes(x = key, y = val, fill = income)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  geom_text(aes(x=key, y=val, label = round(val, 0)), size = 5, color="white", vjust=2, position = position_dodge(width=0.9),size = 2) +
  geom_hline(yintercept=1) +
  labs(x = "", y = "선호도 점수", fill="", subtitle = "[수입] 항목에 따른 선호도 점수") + 
  theme(text = element_text(size=18)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figConfig, "BWS/income.png", sep="/"), width = 12, height = 8, dpi = 600)
  


## job에 따른 항목별 스코어 차이 ##
full_data_PARTR_job <- full_data_L1 %>%
  dplyr::group_by(job) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(job,BWSQRT01:BWSQRT13) %>%
  dplyr::rename(
    "색" = "BWSQRT01"
    , "향" = "BWSQRT02"
    , "맛" = "BWSQRT03"
    , "생산지역" = "BWSQRT04"
    , "생산국가" = "BWSQRT05"
    , "가격" = "BWSQRT06"
    , "브랜드" = "BWSQRT07"
    , "빈티지" = "BWSQRT08"
    , "품종" = "BWSQRT09"
    , "추천" = "BWSQRT10"
    , "프로모션" = "BWSQRT11"
    , "레이블" = "BWSQRT12"
    , "알콜도수" = "BWSQRT13"
  )


dataL1_job = full_data_PARTR_job %>%
  tidyr::gather(-job, key="key", val="val")

dataL1_job$key = forcats::fct_relevel(dataL1_job$key, yindex)

ggplot(dataL1_job, aes(x = key, y = val, fill = job)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  geom_text(aes(x=key, y=val, label = round(val, 0)), size = 5, color="white", vjust=2, position = position_dodge(width=0.9),size = 2) +
  geom_hline(yintercept=1) +
  labs(x = "", y = "선호도 점수", fill="", subtitle = "[직업] 항목에 따른 선호도 점수") + 
  theme(text = element_text(size=18)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figConfig, "BWS/job.png", sep="/"), width = 12, height = 8, dpi = 600)



## marr에 따른 항목별 스코어 차이 ##
full_data_PARTR_marr <- full_data_L1 %>%
  dplyr::group_by(marr) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(marr,BWSQRT01:BWSQRT13) %>%
  dplyr::rename(
    "색" = "BWSQRT01"
    , "향" = "BWSQRT02"
    , "맛" = "BWSQRT03"
    , "생산지역" = "BWSQRT04"
    , "생산국가" = "BWSQRT05"
    , "가격" = "BWSQRT06"
    , "브랜드" = "BWSQRT07"
    , "빈티지" = "BWSQRT08"
    , "품종" = "BWSQRT09"
    , "추천" = "BWSQRT10"
    , "프로모션" = "BWSQRT11"
    , "레이블" = "BWSQRT12"
    , "알콜도수" = "BWSQRT13"
  )

dataL1_marr = full_data_PARTR_marr %>%
  tidyr::gather(-marr, key="key", val="val")

dataL1_marr$key = forcats::fct_relevel(dataL1_marr$key, yindex)

ggplot(dataL1_marr, aes(x = key, y = val, fill = marr)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  geom_text(aes(x=key, y=val, label = round(val, 0)), size = 5, color="white", vjust=2, position = position_dodge(width=0.9),size = 2) +
  geom_hline(yintercept=1) +
  labs(x = "", y = "선호도 점수", fill="", subtitle = "항목에 따른 선호도 점수") + 
  theme(text = element_text(size=18)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figConfig, "BWS/marr.png", sep="/"), width = 12, height = 8, dpi = 600)



#======================================================
# 연관 규칙
#======================================================
library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
library(plotly)
library(arules)
library(arulesViz)
library(visNetwork)
library(igraph)
library(kableExtra)
library(tibble)
library(png)
library(xlsx)
library(fs)


set.seed(1)

# fileInfo = Sys.glob(paste(globalVar$inpConfig, "kyung123.csv", sep = "/"))
# fileInfo = Sys.glob(paste(globalVar$inpConfig, "kyung1_20201110.csv", sep = "/"))
fileList = Sys.glob(paste(globalVar$inpConfig, "KYUNG/*.csv", sep = "/"))

# fileInfo = "E:/04. 재능플랫폼/Github/TalentPlatform-R/INPUT/o2job/KYUNG/kyung1234.csv"
    
for (fileInfo in fileList) {
    
    fileName = tools::file_path_sans_ext(fs::path_file(fileInfo))
    data = read.csv(fileInfo, header=TRUE, sep=",", stringsAsFactors = FALSE)

    log4r::info(log, paste0("fileInfo : ", fileName))
    
    fileInfo2 = Sys.glob(paste(globalVar$inpConfig, "와인음용별.xlsx", sep = "/"))
    data2 = xlsx::read.xlsx2(file = fileInfo2, sheetName = "Sheet1") 
    
    dataL1 = data2 %>%
      readr::type_convert() %>%
      # dplyr::left_join(data, by = c("id" = "id")) %>%
      dplyr::left_join(data, by = c("id" = "X")) %>%
      na.omit()
    
    levelList = sort(unique(dataL1$level))
    freList = sort(unique(dataL1$fre))
    dayList = sort(unique(dataL1$day))
    drinkpList = sort(unique(dataL1$drinkp))
    
    
    # levelInfo = 1
    
    #=======================================
    # 지식수준 (levelList)
    #=======================================
    for (levelInfo in levelList) {
      
      log4r::info(log, paste0("levelInfo : ", levelInfo))
      
      saveFile = paste0("WineType/WindType_", fileName, "_levelInfo_", levelInfo, ".csv")
      saveImg = paste0("WineType/WindType_", fileName, "_levelInfo_", levelInfo, ".png")
    
      dataL2 = dataL1 %>%
        dplyr::filter(level == levelInfo) %>%
        dplyr::select(- c("level", "fre", "day", "drinkp"))
      
      
      # 연관 규칙
      foodlist = c()
      for (i in unique(dataL2$id)) {
        
        data_part = dataL2 %>%
          dplyr::filter(id == i) %>%
          dplyr::select(-id) %>%
          t() %>%
          dplyr::as_data_frame(rownames = "foodname") %>%
          dplyr::filter(V1 == 1)
        
        foodlist_part = as.vector(list(data_part$foodname))
        foodlist_part2 = as.factor(data_part$foodname)
        
        
        if (length(foodlist_part2) == 1) {
          next
        }
        
        foodlist = append(foodlist, foodlist_part)
      }
      
      transactions = as(foodlist, "transactions")
      
      # Implementing Apriori Algorithm
      # rules = apriori(transactions, parameter = list(supp=0.01, conf=0.5, minlen=2, maxlen=3))
      rules = apriori(transactions, parameter = list(supp=0.1, conf=0.5))
      
      if (length(rules) == 0) { next }
      
      rules_dt = data.table( 
        lhs = labels( lhs(rules) )
        , rhs = labels( rhs(rules) )
        , quality(rules) 
      )[ order(-lift), ]
      
      
      write.csv(rules_dt, paste(globalVar$outConfig, saveFile, sep="/"))
      
      subrules2 = head(sort(rules, by="confidence"),length(rules))
      ig = plot(subrules2, method="graph", control=list(type="items") )
      ig_df = get.data.frame( ig, what = "both" )
      
      png(file = paste(globalVar$figConfig, saveImg, sep='/'), width = 8, height = 5, units="in", res = 600)
      plot(subrules2, method="graph", control=list(type="items") )
      dev.off()
    }
    
    
    #=======================================
    # 음용횟수 (freList)
    #=======================================
    for (freInfo in freList) {
      
      log4r::info(log, paste0("freInfo : ", freInfo))
      
      saveFile = paste0("WineType/WindType_", fileName, "_freInfo_", freInfo, ".csv")
      saveImg = paste0("WineType/WindType_", fileName, "_freInfo_", freInfo, ".png")
      
      dataL2 = dataL1 %>%
        dplyr::filter(fre == freInfo) %>%
        dplyr::select(- c("level", "fre", "day", "drinkp"))
      
      # 연관 규칙
      foodlist = c()
      for (i in unique(dataL2$id)) {
        
        data_part = dataL2 %>%
          dplyr::filter(id == i) %>%
          dplyr::select(-id) %>%
          t() %>%
          dplyr::as_data_frame(rownames = "foodname") %>%
          dplyr::filter(V1 == 1)
        
        foodlist_part = as.vector(list(data_part$foodname))
        foodlist_part2 = as.factor(data_part$foodname)
        
        
        if (length(foodlist_part2) == 1) {
          next
        }
        
        foodlist = append(foodlist, foodlist_part)
      }
      
      transactions = as(foodlist, "transactions")
      
      # Implementing Apriori Algorithm
      # rules = apriori(transactions, parameter = list(supp=0.01, conf=0.5, minlen=2, maxlen=3))
      rules = apriori(transactions, parameter = list(supp=0.1, conf=0.5))
      
      if (length(rules) == 0) { next }
      
      rules_dt = data.table( 
        lhs = labels( lhs(rules) )
        , rhs = labels( rhs(rules) )
        , quality(rules) 
      )[ order(-lift), ]
      
      
      write.csv(rules_dt, paste(globalVar$outConfig, saveFile, sep="/"))
      
      subrules2 = head(sort(rules, by="confidence"),length(rules))
      ig = plot(subrules2, method="graph", control=list(type="items") )
      ig_df = get.data.frame( ig, what = "both" )
      
      png(file = paste(globalVar$figConfig, saveImg, sep='/'), width = 8, height = 5, units="in", res = 600)
      plot(subrules2, method="graph", control=list(type="items") )
      dev.off()
      
    }
    
    
    #=======================================
    # 음용기간 (dayList)
    #=======================================
    for (dayInfo in dayList) {
      
      log4r::info(log, paste0("dayInfo : ", dayInfo))

      saveFile = paste0("WineType/WindType_", fileName, "_dayInfo_", dayInfo, ".csv")
      saveImg = paste0("WineType/WindType_", fileName, "_dayInfo_", dayInfo, ".png")
      
      dataL2 = dataL1 %>%
        dplyr::filter(day == dayInfo) %>%
        dplyr::select(- c("level", "fre", "day", "drinkp"))
      
      
      # 연관 규칙
      foodlist = c()
      for (i in unique(dataL2$id)) {
        
        data_part = dataL2 %>%
          dplyr::filter(id == i) %>%
          dplyr::select(-id) %>%
          t() %>%
          dplyr::as_data_frame(rownames = "foodname") %>%
          dplyr::filter(V1 == 1)
        
        foodlist_part = as.vector(list(data_part$foodname))
        foodlist_part2 = as.factor(data_part$foodname)
        
        
        if (length(foodlist_part2) == 1) {
          next
        }
        
        foodlist = append(foodlist, foodlist_part)
      }
      
      transactions = as(foodlist, "transactions")
      
      # Implementing Apriori Algorithm
      # rules = apriori(transactions, parameter = list(supp=0.01, conf=0.5, minlen=2, maxlen=3))
      rules = apriori(transactions, parameter = list(supp=0.1, conf=0.5))
      
      if (length(rules) == 0) { next }
      
      rules_dt = data.table( 
        lhs = labels( lhs(rules) )
        , rhs = labels( rhs(rules) )
        , quality(rules) 
      )[ order(-lift), ]
      
      
      write.csv(rules_dt, paste(globalVar$outConfig, saveFile, sep="/"))
      
      subrules2 = head(sort(rules, by="confidence"),length(rules))
      ig = plot(subrules2, method="graph", control=list(type="items") )
      ig_df = get.data.frame( ig, what = "both" )
      
      png(file = paste(globalVar$figConfig, saveImg, sep='/'), width = 8, height = 5, units="in", res = 600)
      plot(subrules2, method="graph", control=list(type="items") )
      dev.off()
      
    }
    
    #=======================================
    # 음용장소 (drinkpList)
    #=======================================
    for (drinkpInfo in drinkpList) {
      
      log4r::info(log, paste0("drinkpInfo : ", drinkpInfo))
        
      saveFile = paste0("WineType/WindType_", fileName, "_drinkpInfo_", drinkpInfo, ".csv")
      saveImg = paste0("WineType/WindType_", fileName, "_drinkpInfo_", drinkpInfo, ".png")
      
      dataL2 = dataL1 %>%
          dplyr::filter(drinkp == drinkpInfo) %>%
          dplyr::select(- c("level", "fre", "day", "drinkp"))
      
      
      # 연관 규칙
      foodlist = c()
      for (i in unique(dataL2$id)) {
          
          data_part = dataL2 %>%
              dplyr::filter(id == i) %>%
              dplyr::select(-id) %>%
              t() %>%
              dplyr::as_data_frame(rownames = "foodname") %>%
              dplyr::filter(V1 == 1)
          
          foodlist_part = as.vector(list(data_part$foodname))
          foodlist_part2 = as.factor(data_part$foodname)
          
          
          if (length(foodlist_part2) == 1) {
              next
          }
          
          foodlist = append(foodlist, foodlist_part)
      }
      
      transactions = as(foodlist, "transactions")
      
      # Implementing Apriori Algorithm
      # rules = apriori(transactions, parameter = list(supp=0.01, conf=0.5, minlen=2, maxlen=3))
      rules = apriori(transactions, parameter = list(supp=0.1, conf=0.5))
      
      if (length(rules) == 0) { next }
      
      rules_dt = data.table( 
          lhs = labels( lhs(rules) )
          , rhs = labels( rhs(rules) )
          , quality(rules) 
      )[ order(-lift), ]
      
      
      write.csv(rules_dt, paste(globalVar$outConfig, saveFile, sep="/"))
      
      subrules2 = head(sort(rules, by="confidence"),length(rules))
      ig = plot(subrules2, method="graph", control=list(type="items") )
      ig_df = get.data.frame( ig, what = "both" )
      
      png(file = paste(globalVar$figConfig, saveImg, sep='/'), width = 8, height = 5, units="in", res = 600)
      plot(subrules2, method="graph", control=list(type="items") )
      dev.off()
      
    }
    
}


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(ggplot2)
library(tidyverse)
library(xlsx)
library(readr)
library(magrittr)
library(ggpol)
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggplot2)
library(lubridate)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(openxlsx)
library(dplyr)
library(geosphere)
library(ggrepel)
library(gdata)
library(sf)
library(ggpubr)
library(ggh4x)
library(ggh4x)
library(remotes)
library(devtools)


# remotes::install_github("teunbrand/ggh4x")

fileInfo = Sys.glob(paste(globalVar$inpConfig, "2015-2019 홍성의료원 기초조사_L1.xlsx", sep = "/"))

#==========================================
# 그래프 1
#==========================================
mapInfo = Sys.glob(paste(globalVar$mapConfig, "/KOREA_INFO/TL_SCCO_CTPRVN.shp", sep = "/"))
la = shapefile(mapInfo, encoding = "UTF-8")

geo = sp::spTransform(la, CRS("+proj=longlat"))
geoData = ggplot2::fortify(geo,region = 'CTPRVN_CD',region2 = "CTP_KOR_NM")
# head(geoData)

# 위치 정보 읽기 (법정동 - 시도)
codeInfo = Sys.glob(paste(globalVar$mapConfig, "/ADM_CODE/법정동코드_전체자료.txt", sep = "/"))
code = read.table(codeInfo, sep="\t", header = TRUE, colClasses = "character")
colnames(code) = c("EMD_CD", "full_addr", "useflag")

code_L1 = code %>%
  tidyr::separate(col = "full_addr", into = c("d1", "d2", "d3", "d4", "d5"), sep=" ") %>%
  dplyr::filter(is.na(d3), is.na(d2)) %>%
  dplyr::mutate(code = str_sub(EMD_CD,1,2)) %>%
  dplyr::filter(useflag == "존재") %>%
  dplyr::select(-c(EMD_CD, d2, d3, d4, d5, useflag)) %>%
  dplyr::rename(
    "si_do" = "d1"
    , "sido_code" = "code"
  )

unique(code_L1$si_do)

# 대전 및 충남만 선택
code_L2_deaseon = code_L1 %>%
  dplyr::filter(si_do %in%c("충청남도", "충청북도", "대전광역시", "세종특별자치시")) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(sido_code)) %>%
  dplyr::mutate(code = as.character(round(as.numeric(sido_code, 0))))

geoData_L1 = geoData %>%
  dplyr::inner_join(code_L2_deaseon, by = c("id" = "code")) 


# 환자 데이터와 지도와 매칭 (시도)
data = openxlsx::read.xlsx(xlsxFile = fileInfo, sheet = "G1") %>%
# data = xlsx::read.xlsx2(file = fileInfo, sheetName = "G1") %>%
  readr::type_convert() %>%
  dplyr::mutate(
    dtDate = readr::parse_date(as.character(date), "%Y-%m-%d")
    , dtYear = lubridate::year(dtDate)
  )

yearList = sort(unique(data$dtYear))
key5List = unique(data$key5)
key6List = unique(data$key6)

# key5Info = "급성심근경색"
# yearInfo = "2015"
# key5Info = "뇌졸중"
# key6Info = "외래"


for (key5Info in key5List) {
  for (key6Info in key6List) {
    
    # 전체 수행
    tmpData = data %>%
      dplyr::group_by(key5, key6) %>%
      dplyr::summarise(sumVal = n()) %>%
      dplyr::filter(
        key5 == key5Info
        , key6 == key6Info
      )
    
    if (nrow(tmpData) <= 0) {
      next
    }
    
    dataL1 = data %>%
      dplyr::group_by(addr1, key5, key6) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::filter(
        key5 == key5Info
        , key6 == key6Info
      ) %>%
      dplyr::mutate(cnt = (n / tmpData$sumVal) * 100)
    
    if (nrow(dataL1) <= 0) {
      next
    }
    
    dataL2 = geoData_L1 %>%
      dplyr::left_join(dataL1, by = c("si_do" = "addr1")) %>%
      dplyr::mutate(
        plotLabel = dplyr::case_when(
          si_do == "충청남도" ~ dplyr::if_else(! is.na(n) & n > 0, paste0(round(n, 0), "\n충남"), "충남")
          , si_do == "세종특별자치시" ~ dplyr::if_else(! is.na(n) & n > 0, paste0(round(n, 0), "\n세종"), "세종")
          , si_do == "충청북도" ~ dplyr::if_else(! is.na(n) & n > 0, paste0(round(n, 0), "\n충북"), "충북")
          , si_do == "대전광역시" ~ dplyr::if_else(! is.na(n) & n > 0, paste0(round(n, 0), "\n대전"), "대전")
          , TRUE ~ ""
        )
        , xOffset = dplyr::case_when(
          si_do == "충청남도" ~ 0.25
          , si_do == "충청북도" ~ -0.35
          , si_do == "세종특별자치시" ~ -0.015
          , TRUE ~ 0
        )
        , yOffset = dplyr::case_when(
          TRUE ~ 0
        )
      )
    
    # 시각화
    saveImg = paste0("TMP2/Img_076_", "2015-2019", "_", key6Info, "_", key5Info, ".png")
    plotSubTitle = paste0("[", key6Info, "] ", key5Info, " 의료이용률 시도별 현황(", "2015-2019", ")")
    
    
    ggplot() +
      theme_bw() +
      coord_fixed(ratio = 1) +
      geom_polygon(data=dataL2, aes(x=long, y=lat, group=group, fill = n)) +
      scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
      geom_path(data=dataL2, aes(x=long,y=lat,group=group), colour='black',size = 0.5) +
      ggh4x::stat_midpoint(data=dataL2, aes(x=long + xOffset, y=lat + yOffset, group=sido_code, label = plotLabel), geom = "text", size = 4) +  
      #ggh4x::stat_midpoint(data=geoData_L1, aes(x=long, y=lat, group=sigungu_code, label = sigungu_name), geom = "text", size = 3) + # 시군구
      labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "단위 : 명", subtitle = NULL) +
      theme_minimal() +
      theme(
        text = element_text(size=18)
        , panel.grid.major.x = element_blank()
        , panel.grid.major.y = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , axis.text.x=element_blank()
        , axis.ticks.x=element_blank()
        , axis.title.x=element_blank()
        , axis.text.y=element_blank()
        , axis.ticks.y=element_blank()
        , axis.title.y=element_blank()
        , plot.subtitle = element_text(hjust = 1)
        # , legend.position = "none"
      ) + 
      ggsave(filename = paste(globalVar$figConfig, saveImg, sep="/"), width = 10, height = 5, dpi = 600)
    
    # 각 연도별 수행
    for (yearInfo in yearList) {
    
        dataL1 = data %>%
          dplyr::group_by(dtYear, addr1, key5, key6) %>%
          dplyr::summarise(n = n()) %>%
          dplyr::filter(
            dtYear == yearInfo
            , key5 == key5Info
            , key6 == key6Info
          ) %>%
          dplyr::mutate(cnt = (n / tmpData$sumVal) * 100)
    
        if (nrow(dataL1) <= 0) {
          next
        }
        
        dataL2 = geoData_L1 %>%
          dplyr::left_join(dataL1, by = c("si_do" = "addr1")) %>%
          dplyr::mutate(
            plotLabel = dplyr::case_when(
              si_do == "충청남도" ~ dplyr::if_else(! is.na(n) & n > 0, paste0(round(n, 0), "\n충남"), "충남")
              , si_do == "세종특별자치시" ~ dplyr::if_else(! is.na(n) & n > 0, paste0(round(n, 0), "\n세종"), "세종")
              , si_do == "충청북도" ~ dplyr::if_else(! is.na(n) & n > 0, paste0(round(n, 0), "\n충북"), "충북")
              , si_do == "대전광역시" ~ dplyr::if_else(! is.na(n) & n > 0, paste0(round(n, 0), "\n대전"), "대전")
              , TRUE ~ ""
              )
            , xOffset = dplyr::case_when(
              si_do == "충청남도" ~ 0.25
              , si_do == "충청북도" ~ -0.35
              , si_do == "세종특별자치시" ~ -0.015
              , TRUE ~ 0
              )
            , yOffset = dplyr::case_when(
              TRUE ~ 0
              )
            )
        
        # 시각화
        saveImg = paste0("TMP2/Img_076_", yearInfo, "_", key6Info, "_", key5Info, ".png")
        plotSubTitle = paste0("[", key6Info, "] ", key5Info, " 의료이용률 시도별 현황(", yearInfo, ")")
        
        ggplot() +
          theme_bw() +
          coord_fixed(ratio = 1) +
          geom_polygon(data=dataL2, aes(x=long, y=lat, group=group, fill = n)) +
          scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
          geom_path(data=dataL2, aes(x=long, y=lat, group=group), colour='black',size = 0.5) +
          ggh4x::stat_midpoint(data=dataL2, aes(x=long + xOffset, y=lat + yOffset, group=sido_code, label = plotLabel), geom = "text", size = 4) +  
          #ggh4x::stat_midpoint(data=geoData_L1, aes(x=long, y=lat, group=sigungu_code, label = sigungu_name), geom = "text", size = 3) + # 시군구
          labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "단위 : 명", subtitle = NULL) +
          theme_minimal() +
          theme(
            text = element_text(size=18)
            , panel.grid.major.x = element_blank()
            , panel.grid.major.y = element_blank()
            , panel.grid.minor.x = element_blank()
            , panel.grid.minor.y = element_blank()
            , axis.text.x=element_blank()
            , axis.ticks.x=element_blank()
            , axis.title.x=element_blank()
            , axis.text.y=element_blank()
            , axis.ticks.y=element_blank()
            , axis.title.y=element_blank()
            , plot.subtitle = element_text(hjust = 1)
            # , legend.position = "none"
          ) + 
          ggsave(filename = paste(globalVar$figConfig, saveImg, sep="/"), width = 10, height = 5, dpi = 600)
    }
  }
}


#==========================================
# 그래프 2
#==========================================
refKey = c("전체", "홍성", "예산", "청양", "보령", "기타")

data = xlsx::read.xlsx2(file = fileInfo, sheetName = "G2") %>%
  readr::type_convert()

tmpData = data %>%
  dplyr::group_by(year, key2, key3) %>%
  dplyr::summarise(
    val = sum(val, na.rm = TRUE)
  ) %>%
  dplyr::bind_cols(key = "전체")

dataL1 = data %>%
  bind_rows(tmpData) 

key2List = unique(dataL1$key2)
key3List = unique(dataL1$key3)

# key2Info = "외래"
# key3Info = "급성심근경색"

for (key2Info in key2List) {
  for (key3Info in key3List) {
    plotSubTitle = paste0("[", key2Info, "] ", key3Info, " 의료이용률 시도별 연도별 추이")
    saveImg = paste0("TMP2/Img_070_", key2Info, "_", key3Info, ".png")
    
    dataL2 = dataL1 %>%
      dplyr::filter(
        key2 == key2Info
        , key3 == key3Info
      ) 
    
    ind = which(dataL2$key == "전체")
    
    dataL3 = dataL2 %>%
      dplyr::mutate(
        perVal = (val / sum(dataL2[ind, ]$val, na.rm = TRUE)) * 100
      )
    
    dataL3$key = forcats::fct_relevel(dataL3$key, refKey)
    
    ggplot(dataL3, aes(x = year, y = perVal, colour = key)) +
      geom_line(size = 1.5) +
      # xlim(80, 100) + 
      scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = NULL) +
      ylim(0, 33) +
      theme_bw() +
      labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, subtitle = "단위 : %") +
      theme(
        text = element_text(size=18)
        # , panel.grid.major.x = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , plot.subtitle = element_text(hjust = 1)
      ) + 
      ggsave(filename = paste(globalVar$figConfig, saveImg, sep="/"), width = 10, height = 6, dpi = 600)
  }
}


#==========================================
# 그래프 2.2
#==========================================
refKey = c("전체", "남성", "여성")

data = xlsx::read.xlsx2(file = fileInfo, sheetName = "G2-2") %>%
  readr::type_convert()

tmpData = data %>%
  dplyr::group_by(year, key2, key3) %>%
  dplyr::summarise(
    val = sum(val, na.rm = TRUE)
  ) %>%
  dplyr::bind_cols(key = "전체")

dataL1 = data %>%
  bind_rows(tmpData) 

key2List = unique(dataL1$key2)
key3List = unique(dataL1$key3)

for (key2Info in key2List) {
  for (key3Info in key3List) {
    plotSubTitle = paste0("[", key2Info, "] ", key3Info, " 의료이용률 성별 연도별 추이")
    saveImg = paste0("TMP2/Img_071_", key2Info, "_", key3Info, ".png")
    
    dataL2 = dataL1 %>%
      dplyr::filter(
        key2 == key2Info
        , key3 == key3Info
      ) 
    
    ind = which(dataL2$key == "전체")
    
    dataL3 = dataL2 %>%
      dplyr::mutate(
        perVal = (val / sum(dataL2[ind, ]$val, na.rm = TRUE)) * 100
      )
    
    dataL3$key = forcats::fct_relevel(dataL3$key, refKey)
    
    ggplot(dataL3, aes(x = year, y = perVal, colour = key)) +
      geom_line(size = 1.5) +
      # xlim(80, 100) + 
      scale_y_continuous(breaks = seq(0, 100, 10), minor_breaks = NULL) +
      ylim(0, 33) +
      theme_bw() +
      labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, subtitle = "단위 : %") +
      theme(
        text = element_text(size=18)
        # , panel.grid.major.x = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , plot.subtitle = element_text(hjust = 1)
      ) + 
      ggsave(filename = paste(globalVar$figConfig, saveImg, sep="/"), width = 10, height = 6, dpi = 600)
  }
}


#==========================================
# 그래프 3
#==========================================
refKey = c("전체", "남성", "여성")

data = xlsx::read.xlsx2(file = fileInfo, sheetName = "G3") %>%
  readr::type_convert()

tmpData = data %>%
  dplyr::group_by(year, key2, key3) %>%
  # dplyr::group_by(year) %>%
  dplyr::summarise(
    val = sum(val, na.rm = TRUE)
  ) %>%
  dplyr::bind_cols(key = "전체")

dataL1 = data

yearList = unique(dataL1$year)
key2List = unique(dataL1$key2)
key3List = unique(dataL1$key3)

yearInfo = 2015
key2Info = "외래"
key3Info = "뇌졸중"

for (yearInfo in yearList) {
    for (key2Info in key2List) {
      for (key3Info in key3List) {
          
        plotSubTitle = paste0("[", key2Info, "] ", key3Info, " 의료이용률 연령별∙성별 현황(", yearInfo, ")")
        saveImg = paste0("TMP2/Img_072_", yearInfo, "_", key2Info, "_", key3Info, ".png")
        
        dataL2 = dataL1 %>%
          dplyr::filter(
            year == yearInfo
            , key2 == key2Info
            , key3 == key3Info
          ) 
        
        sumData = tmpData %>%
          dplyr::filter(
            year == yearInfo
            , key2 == key2Info
            , key3 == key3Info
          )
        
        dataL3 = dataL2 %>%
          dplyr::mutate(
            perVal = dplyr::case_when(
              key == "남성" ~  -(val / sumData$val) * 100
              , key == "여성" ~  (val / sumData$val) * 100
              , TRUE ~ NA_real_
            )
            # , perValLabel =  sprintf("%.2f", abs(perVal))
            , perValLabel =  sprintf("%.1f", abs(perVal))
            , nudgePerVal = dplyr::case_when(
                 perVal > 0 ~  (perVal + 1.25)
                 , perVal < 0 ~  (perVal - 1.25)
                 , perVal == 0 & key == "남성" ~ -1.1
                 , perVal == 0 & key == "여성" ~ 1.1
                 , TRUE ~  NA_real_
              )
          ) 
        
        
        ggplot(dataL3, aes_string(x = "key4", y = "perVal", fill = "key")) +
          geom_bar(stat = "identity") + 
          theme_bw() +
          labs(title = plotSubTitle, x = NULL, y = NULL, fill = NULL, colour = NULL, subtitle = "단위 : %") +
          theme(
            text = element_text(size=18)
            # , panel.grid.major.x = element_blank()
            , panel.grid.minor.x = element_blank()
            , panel.grid.minor.y = element_blank()
            , plot.subtitle = element_text(hjust = 1)
            , legend.position = "none"
          ) + 
            geom_text(aes_string(x = "key4", y = "nudgePerVal", label = "perValLabel"), size=5, hjust=0.5, vjust=0.5) +
            coord_flip() +
            ggpol::facet_share(~ key, dir = "h", scales = "free_x", reverse_num = TRUE) +
            scale_y_continuous(breaks = seq(-100, 100, 10), minor_breaks = NULL) +
            ggsave(filename = paste(globalVar$figConfig, saveImg, sep="/"), width = 10, height = 6, dpi = 600)
        
        }
    }
}


#==========================================
# 그래프 4 : 시군구별 현황
#==========================================
mapInfo = Sys.glob(paste(globalVar$mapConfig, "/KOREA_INFO/TL_SCCO_SIG.shp", sep = "/"))
la = shapefile(mapInfo, encoding = "UTF-8")

geo = spTransform(la, CRS("+proj=longlat"))
head(geo)
geoData = ggplot2::fortify(geo,region = 'SIG_CD',region2 = "SIG_KOR_NM")
head(geoData)

codeInfo = Sys.glob(paste(globalVar$mapConfig, "/ADM_CODE/법정동코드_전체자료.txt", sep = "/"))
code = read.table(codeInfo, sep="\t", header = TRUE, colClasses = "character") %>%
  dplyr::rename(
    "EMD_CD" = "법정동코드"
    , "full_addr" = "법정동명"
    , "useflag" = "폐지여부"
  )

code_L1 = code %>%
  tidyr::separate(col = "full_addr",into = c("d1","d2","d3","d4","d5"), sep=" ") %>%
  dplyr::filter(is.na(d3), !is.na(d2)) %>%
  dplyr::mutate(code = str_sub(EMD_CD,1,5)) %>%
  dplyr::filter(useflag == "존재") %>%
  dplyr::select(- c(EMD_CD, d3, d4, d5, useflag)) %>%
  dplyr::rename(
    "si_do" = "d1"
    , "sigungu_name" = "d2"
    , "sigungu_code" = "code"
  )


unique(code_L1$si_do)

# 대전 및 충남만 선택
geoData_L1 = code_L1 %>%
  dplyr::filter(si_do %in% c("충청남도", "대전광역시", "세종특별자치시")) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(sigungu_code)) %>%
  dplyr::mutate(code = as.character(round(as.numeric(sigungu_code,0)))) %>%
  dplyr::inner_join(geoData, by = c("code" = "id"))

# 환자 데이터와 지도와 매칭 (시도)
data = openxlsx::read.xlsx(xlsxFile = fileInfo, sheet = "G1") %>%
  readr::type_convert() %>%
  dplyr::mutate(
    dtDate = readr::parse_date(as.character(date), "%Y-%m-%d")
    , dtYear = lubridate::year(dtDate)
  )

yearList = sort(unique(data$dtYear))
key5List = unique(data$key5)
key6List = unique(data$key6)

# addr1Info = "충청남도"
# key5Info = "급성심근경색"
# key6Info = "외래"


for (key5Info in key5List) {
  for (key6Info in key6List) {
    
    # 전체
    tmpData = data %>%
      dplyr::group_by(key5, key6, addr1) %>%
      dplyr::summarise(sumVal = n()) %>%
      dplyr::filter(
        key5 == key5Info
        , key6 == key6Info
        , addr1 == addr1Info
      )
    
    if (nrow(tmpData) <= 0) {
      next
    }
    
    dataL1 = data %>%
      dplyr::group_by(addr1, addr2, key5, key6) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::filter(
        key5 == key5Info
        , key6 == key6Info
      ) %>%
      dplyr::mutate(cnt = (n / tmpData$sumVal) * 100)
    
    if (nrow(dataL1) <= 0) {
      next
    }
    
    
    # unique(geoData_L1$si_do)
    # unique(geoData_L1$sigungu_name)
    
    dataL2 = geoData_L1 %>%
      dplyr::left_join(dataL1, by = c("si_do" = "addr1", "sigungu_name" = "addr2")) %>%
      dplyr::mutate(
        plotLabel = dplyr::case_when(
          n > 0 ~ paste0(round(n, 0), "\n", sigungu_name)
          , TRUE ~ sigungu_name
        )
        , xOffset = dplyr::case_when(
          sigungu_name == "태안군" ~ 0.275
          , sigungu_name == "보령시" ~ 0.275
          , sigungu_name == "서산시" ~ 0
          , sigungu_name == "예산군" ~ 0.05
          , sigungu_name == "홍성군" ~ 0.05
          , sigungu_name == "서천군" ~ 0.05
          , TRUE ~ 0
        )
        , yOffset = dplyr::case_when(
          sigungu_name == "서산시" ~ -0.05
          , sigungu_name == "태안군" ~ 0.1
          , sigungu_name == "서천군" ~ 0
          , sigungu_name == "반곡동" ~ -0.1
          , TRUE ~ 0
        )
      )
    
    # 시각화
    # saveImg = paste0("TMP2/Img_077.png")
    saveImg = paste0("TMP2/Img_077_", "2015-2019", "_", key6Info, "_", key5Info, ".png")
    plotSubTitle = paste0("[", key6Info, "] ", key5Info, " 의료이용률 시군구별 현황(", yearInfo, ")")
    
    ggplot() +
      theme_bw() +
      coord_fixed(ratio = 1) +
      geom_polygon(data=dataL2, aes(x=long, y=lat, group=group, fill = n)) +
      scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
      geom_path(data=dataL2, aes(x=long, y=lat, group=group), colour='black',size = 0.5) +
      ggh4x::stat_midpoint(data=dataL2, aes(x=long + xOffset, y=lat + yOffset, group=sigungu_code, label = plotLabel), geom = "text", size = 4) +
      labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "단위 : 명", subtitle = NULL) +
      theme_minimal() +
      theme(
        text = element_text(size=18)
        , panel.grid.major.x = element_blank()
        , panel.grid.major.y = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , axis.text.x=element_blank()
        , axis.ticks.x=element_blank()
        , axis.title.x=element_blank()
        , axis.text.y=element_blank()
        , axis.ticks.y=element_blank()
        , axis.title.y=element_blank()
        , plot.subtitle = element_text(hjust = 1)
        # , legend.position = "none"
      ) + 
      ggsave(filename = paste(globalVar$figConfig, saveImg, sep="/"), width = 10, height = 5, dpi = 600)
    
    
    
    # 연도별
    for (yearInfo in yearList) {
  
      dataL1 = data %>%
        dplyr::group_by(dtYear, addr1, addr2, key5, key6) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::filter(
          dtYear == yearInfo
          , key5 == key5Info
          , key6 == key6Info
        ) %>%
        dplyr::mutate(cnt = (n / tmpData$sumVal) * 100)
      
      if (nrow(dataL1) <= 0) {
        next
      }
      
      
      # unique(geoData_L1$si_do)
      # unique(geoData_L1$sigungu_name)
      
      dataL2 = geoData_L1 %>%
        dplyr::left_join(dataL1, by = c("si_do" = "addr1", "sigungu_name" = "addr2")) %>%
        dplyr::mutate(
          plotLabel = dplyr::case_when(
            n > 0 ~ paste0(round(n, 0), "\n", sigungu_name)
            , TRUE ~ sigungu_name
          )
          , xOffset = dplyr::case_when(
            sigungu_name == "태안군" ~ 0.275
            , sigungu_name == "보령시" ~ 0.275
            , sigungu_name == "서산시" ~ 0
            , sigungu_name == "예산군" ~ 0.05
            , sigungu_name == "홍성군" ~ 0.05
            , sigungu_name == "서천군" ~ 0.05
            , TRUE ~ 0
          )
          , yOffset = dplyr::case_when(
            sigungu_name == "서산시" ~ -0.05
            , sigungu_name == "태안군" ~ 0.1
            , sigungu_name == "서천군" ~ 0
            , sigungu_name == "반곡동" ~ -0.1
            , TRUE ~ 0
            )
        )
      
      # 시각화
      # saveImg = paste0("TMP2/Img_077.png")
      saveImg = paste0("TMP2/Img_077_", yearInfo, "_", key6Info, "_", key5Info, ".png")
      plotSubTitle = paste0("[", key6Info, "] ", key5Info, " 의료이용률 시군구별 현황(", yearInfo, ")")
      
      ggplot() +
        theme_bw() +
        coord_fixed(ratio = 1) +
        geom_polygon(data=dataL2, aes(x=long, y=lat, group=group, fill = n)) +
        scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
        geom_path(data=dataL2, aes(x=long, y=lat, group=group), colour='black',size = 0.5) +
        ggh4x::stat_midpoint(data=dataL2, aes(x=long + xOffset, y=lat + yOffset, group=sigungu_code, label = plotLabel), geom = "text", size = 4) +
        labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "단위 : 명", subtitle = NULL) +
        theme_minimal() +
        theme(
          text = element_text(size=18)
          , panel.grid.major.x = element_blank()
          , panel.grid.major.y = element_blank()
          , panel.grid.minor.x = element_blank()
          , panel.grid.minor.y = element_blank()
          , axis.text.x=element_blank()
          , axis.ticks.x=element_blank()
          , axis.title.x=element_blank()
          , axis.text.y=element_blank()
          , axis.ticks.y=element_blank()
          , axis.title.y=element_blank()
          , plot.subtitle = element_text(hjust = 1)
          # , legend.position = "none"
        ) + 
        ggsave(filename = paste(globalVar$figConfig, saveImg, sep="/"), width = 10, height = 5, dpi = 600)
      
    }
  }
}


#==========================================
# 그래프 5 : 시도별 추세
#==========================================
data = xlsx::read.xlsx2(file = fileInfo, sheetName = "G1") %>%
    readr::type_convert()

dataL1 = data %>%
    dplyr::mutate(
        dtDate = readr::parse_date(as.character(date), "%Y-%m-%d")
        , dtYear = lubridate::year(dtDate)
    )

key5List = unique(dataL1$key5)
key6List = unique(dataL1$key6)

addr1List = unique(dataL1$addr1)

# key5Info = "급성심근경색"
# key6Info = "외래"
# addr1Info = "충청남도"

# 전국
for (key6Info in key6List) {
    for (key5Info in key5List) {
      
      plotSubTitle = paste0("[", key6Info, "] ", key5Info, " 의료이용률 시도별 추세(2015~2019)")
      saveImg = paste0("TMP2/Img_073_", key6Info, "_", key5Info, "_", "전국", ".png")
     
      tmpData = dataL1 %>%
        dplyr::filter(
          key5 == key5Info
          , key6 == key6Info
        ) %>%
        dplyr::group_by(dtYear, key5, key6) %>%
        dplyr::summarise(cnt = n()) %>%
        dplyr::mutate(facetLabel = "전국")
      
      if (nrow(tmpData) < 4) {
        next
      }
      
      sumVal = sum(tmpData$cnt, na.rm = TRUE)
      
      tmpDataL1 = tmpData %>%
        dplyr::mutate(val = (cnt / sumVal) * 100) 
      
      ggpubr::ggscatter(tmpDataL1, x = "dtYear", y = "val", color = "black", add = "reg.line", conf.int = TRUE, cor.coef = FALSE, add.params = list(color = "blue", fill = "lightblue")) +
        ggpubr::stat_cor(method = "pearson", label.x = 2015, label.y = 50, size = 5) +
        ggpubr::stat_regline_equation(label.x = 2015, label.y = 45, size = 5) +
        ylim(0, 50) +
        theme_bw() +
        labs(title = plotSubTitle, x = NULL, y = NULL, fill = NULL, colour = NULL, subtitle = "단위 : %") +
        theme(
          text = element_text(size=18)
          # , panel.grid.major.x = element_blank()
          , panel.grid.minor.x = element_blank()
          , panel.grid.minor.y = element_blank()
          , plot.subtitle = element_text(hjust = 1)
          , legend.position = "none"
        ) + 
        facet_wrap( ~ facetLabel) +
        ggsave(filename = paste(globalVar$figConfig, saveImg, sep="/"), width = 10, height = 6, dpi = 600)
    }
}
      

# 시도별
for (key6Info in key6List) {
  for (key5Info in key5List) {
      for (addr1Info in addr1List) {
              
              plotSubTitle = paste0("[", key6Info, "] ", key5Info, " 의료이용률 시도별 추세(2015~2019)")
              saveImg = paste0("TMP2/Img_073_", key6Info, "_", key5Info, "_", addr1Info, ".png")
              
              cat(key5Info, " ", key6Info, " ", addr1Info)
              
              dataL2 = dataL1 %>%
                  dplyr::filter(
                    addr1 == addr1Info
                    , key5 == key5Info
                    , key6 == key6Info
                  ) %>%
                  dplyr::group_by(dtYear, key5, key6, addr1) %>%
                  dplyr::summarise(
                      cnt = n()
                  ) %>%
                  dplyr::mutate(
                      facetLabel = stringr::str_c(addr1)
                  )
              
              if (nrow(dataL2) < 4) {
                  next
              }
              
              sumVal = sum(dataL2$cnt, na.rm = TRUE)
              
              dataL3 = dataL2 %>%
                  dplyr::mutate(val = (cnt / sumVal) * 100) 
              
              ggpubr::ggscatter(dataL3, x = "dtYear", y = "val", color = "black", add = "reg.line", conf.int = TRUE, cor.coef = FALSE, add.params = list(color = "blue", fill = "lightblue")) +
                  ggpubr::stat_cor(method = "pearson", label.x = 2015, label.y = 50, size = 5) +
                  ggpubr::stat_regline_equation(label.x = 2015, label.y = 45, size = 5) +
                  ylim(0, 50) +
                  theme_bw() +
                  labs(title = plotSubTitle, x = NULL, y = NULL, fill = NULL, colour = NULL, subtitle = "단위 : %") +
                  theme(
                      text = element_text(size=18)
                      # , panel.grid.major.x = element_blank()
                      , panel.grid.minor.x = element_blank()
                      , panel.grid.minor.y = element_blank()
                      , plot.subtitle = element_text(hjust = 1)
                      , legend.position = "none"
                  ) + 
                  facet_wrap( ~ facetLabel) +
                  ggsave(filename = paste(globalVar$figConfig, saveImg, sep="/"), width = 10, height = 6, dpi = 600)
          }
      }
}

#==========================================
# 그래프 5 : 시군구별 추세
#==========================================
key5List = unique(dataL1$key5)
key6List = unique(dataL1$key6)

addr1List = unique(dataL1$addr1)
addr2List = unique(dataL1$addr2)
addr3List = unique(dataL1$addr3)

# key5Info = "급성심근경색"
# key6Info = "외래"
# addr1Info = "충청남도"
# addr2Info = "홍성군"

for (key6Info in key6List) {
  for (key5Info in key5List) {
    for (addr1Info in addr1List) {
        for (addr2Info in addr2List) {
                
                plotSubTitle = paste0("[", key6Info, "] ", key5Info, " 의료이용률 시군구별 추세(2015~2019)")
                saveImg = paste0("TMP2/Img_074_", key6Info, "_", key5Info, "_", addr1Info, "_", addr2Info, ".png")
                
                cat(key5Info, " ", key6Info, " ", addr1Info, " ", addr2Info)
                
                dataL2 = dataL1 %>%
                  dplyr::filter(
                    addr1 == addr1Info
                    , addr2 == addr2Info
                    , key5 == key5Info
                    , key6 == key6Info
                  ) %>%
                  dplyr::group_by(dtYear, key5, key6, addr1, addr2) %>%
                  dplyr::summarise(
                    cnt = n()
                  ) %>%
                  dplyr::mutate(
                    facetLabel = stringr::str_c(addr1, " ", addr2)
                  )
          
                
                if (nrow(dataL2) < 0) {
                    next
                }
                
                
                sumVal = sum(dataL2$cnt, na.rm = TRUE)
                
                dataL3 = dataL2 %>%
                  dplyr::mutate(val = (cnt / sumVal) * 100) 
                
                if (nrow(dataL3) < 3) {
                    next
                }
                
                
                ggpubr::ggscatter(dataL3, x = "dtYear", y = "val", color = "black", add = "reg.line", conf.int = TRUE, cor.coef = FALSE, add.params = list(color = "blue", fill = "lightblue")) +
                    ggpubr::stat_cor(method = "pearson", label.x = 2015, label.y = 50, size = 5) +
                    ggpubr::stat_regline_equation(label.x = 2015, label.y = 45, size = 5) +
                    # annotate("text", x = 2015, y = 90, size = 5, label = sprintf("R = %s (P < %.3f)", corVal, pVal), hjust = 0) +
                    # annotate("text", x = 10, y = 31, size = 5, label = sprintf("Bias = %s", biasVal), hjust = 0) +
                    # annotate("text", x = 10, y = 29, size = 5, label = sprintf("RMSE = %s", rmseVal), hjust = 0) +
                    # xlim(10, 35) + 
                    ylim(0, 50) +
                    theme_bw() +
                    labs(title = plotSubTitle, x = NULL, y = NULL, fill = NULL, colour = NULL, subtitle = "단위 : %") +
                    theme(
                        text = element_text(size=18)
                        # , panel.grid.major.x = element_blank()
                        , panel.grid.minor.x = element_blank()
                        , panel.grid.minor.y = element_blank()
                        , plot.subtitle = element_text(hjust = 1)
                        , legend.position = "none"
                    ) + 
                    facet_wrap( ~ facetLabel) +
                    ggsave(filename = paste(globalVar$figConfig, saveImg, sep="/"), width = 10, height = 6, dpi = 600)
            }
        }
    }
}


#==========================================
# 그래프 6 : 읍면동별 현황
#==========================================
# 읍면동 한반도 지도 읽기
mapInfo = Sys.glob(paste(globalVar$mapConfig, "/KOREA_INFO/bnd_dong_00_2019_2019_2Q.shp", sep = "/"))
la = shapefile(mapInfo, encoding = "UTF-8")
geo = sp::spTransform(la, CRS("+proj=longlat"))

geoData = ggplot2::fortify(geo, region = 'adm_dr_cd',region2 = "adm_dr_nm")
head(geoData)


# 행정 코드 (행정동) 읽기
codeInfo = Sys.glob(paste(globalVar$mapConfig, "/ADM_CODE/adm_code.xlsx", sep = "/"))
code = openxlsx::read.xlsx(codeInfo, sheet = 1, startRow = 2)

code_L1 = code %>%
    dplyr::select("시도코드", "시도명칭", "시군구코드", "시군구명칭", "읍면동코드", "읍면동명칭") %>%
    dplyr::rename(
        "si_do" = "시도코드"
        , "si_do_name" = "시도명칭"
        , "sigungu_code" = "시군구코드"
        , "sigungu_name" = "시군구명칭"
        , "emd_code" = "읍면동코드"
        , "emd_name" = "읍면동명칭"
    )

dplyr::tbl_df(code_L1)

unique(code_L1$si_do_name)

# 위치 정보와 한반도 지도 병합
geoData_L1 = code_L1 %>%
    # dplyr::filter(si_do_name %in% c("대전광역시", "충청남도")) %>%
    dplyr::filter(si_do_name %in% c("충청남도")) %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(sigungu_code)) %>%
    # dplyr::mutate(code = as.character(round(as.numeric(sigungu_code,0)))) %>%
    dplyr::mutate(code = as.character(round(as.numeric(emd_code, 0)))) %>%
    dplyr::inner_join(geoData, by = c("code" = "id"))

dplyr::tbl_df(geoData_L1)

options(java.parameters = "-Xmx8192m")


# 환자 데이터 읽기
data = openxlsx::read.xlsx(xlsxFile = fileInfo, sheet = "G1") %>%
  readr::type_convert() %>%
  dplyr::mutate(
    dtDate = readr::parse_date(as.character(date), "%Y-%m-%d")
    , dtYear = lubridate::year(dtDate)
  )

yearList = sort(unique(data$dtYear))
key5List = unique(data$key5)
key6List = unique(data$key6)

# addr1Info = "충청남도"
# addr2Info = "홍성군"
# yearInfo = 2018
# key5Info = "급성심근경색"
# key6Info = "외래"


for (key5Info in key5List) {
  for (key6Info in key6List) {
    
    # 전체 
    tmpData = data %>%
      dplyr::group_by(key5, key6, addr1) %>%
      dplyr::summarise(sumVal = n()) %>%
      dplyr::filter(
        key5 == key5Info
        , key6 == key6Info
        , addr1 == "충청남도"
      )
    
    if (nrow(tmpData) <= 0) {
      next
    }
    
    dataL1 = data %>%
      dplyr::group_by(addr1, addr2, addr3, key5, key6) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::filter(
        key5 == key5Info
        , key6 == key6Info
      ) %>%
      dplyr::mutate(cnt = (n / tmpData$sumVal) * 100)
    
    if (nrow(dataL1) <= 0) {
      next
    }
    
    dataL2 = geoData_L1 %>%
      dplyr::left_join(dataL1, by = c("si_do_name" = "addr1", "sigungu_name" = "addr2", "emd_name" = "addr3")) %>%
      dplyr::mutate(
        plotLabel = dplyr::case_when(
          cnt > 0 ~ emd_name
          , stringr::str_detect(emd_name, regex("원성.동")) ~ ""
          , stringr::str_detect(emd_name, regex("당진.동")) ~ ""
          , stringr::str_detect(emd_name, regex("대천.동")) ~ ""
          , stringr::str_detect(emd_name, regex("동문.동")) ~ ""
          , stringr::str_detect(emd_name, regex("부성.동")) ~ ""
          , stringr::str_detect(emd_name, regex("성정.동")) ~ ""
          , stringr::str_detect(emd_name, regex("쌍용.동")) ~ ""
          , stringr::str_detect(emd_name, regex("온양.동")) ~ ""
          , TRUE ~ emd_name
        )
        , xOffset = dplyr::case_when(
          emd_name == "근흥면" ~ 1.0
          , emd_name == "안면읍" ~ 0.5
          , emd_name == "오천면" ~ 1.0
          , emd_name == "장항읍" ~ 0.06
          , emd_name == "한산면" ~ 0
          , emd_name == "지곡면" ~ 0.1
          , emd_name == "이원면" ~ -0.01
          , emd_name == "원북면" ~ -0.01
          , TRUE ~ 0
        )
        , yOffset = dplyr::case_when(
          emd_name == "마서면" ~ 0.02
          , emd_name == "진산면" ~ -0.02
          , emd_name == "복수면" ~ -0.02
          , emd_name == "군북면" ~ 0.02
          , emd_name == "원북면" ~ -0.02
          , emd_name == "팔봉면" ~ -0.02
          , emd_name == "신평면" ~ -0.02
          , emd_name == "해미면" ~ 0.01
          , emd_name == "이원면" ~ -0.03
          , emd_name == "원북면" ~ -0.045
          , emd_name == "이원면" ~ 0.01
          , emd_name == "장압면" ~ 0.05
          , emd_name == "남면" ~ -0.04
          , emd_name == "우성면" ~ -0.03
          , emd_name == "신창면" ~ 0.02
          , emd_name == "선장면" ~ -0.02
          , emd_name == "장항읍" ~ 0.005
          , TRUE ~ 0
        )
      )
    
    # 시각화
    # saveImg = paste0("TMP2/Img_078.png")
    saveImg = paste0("TMP2/Img_078_", "2015-2019", "_", key6Info, "_", key5Info, ".png")
    plotSubTitle = paste0("[", key6Info, "] ", key5Info, " 의료이용률 읍면동별 현황(", "2015-2019", ")")
    
    
    ggplot() +
      theme_bw() +
      coord_fixed(ratio = 1) +
      geom_polygon(data=dataL2, aes(x=long, y=lat, group=group, fill = n)) +
      scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
      geom_path(data=dataL2, aes(x=long, y=lat, group=group), colour='black',size = 0.5) +
      ggh4x::stat_midpoint(data=dataL2, aes(x=long + xOffset, y=lat + yOffset, group= emd_name, label = plotLabel), geom = "text", size = 2) +
      labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "단위 : 명", subtitle = NULL) +
      theme_minimal() +
      theme(
        text = element_text(size=18)
        , panel.grid.major.x = element_blank()
        , panel.grid.major.y = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , axis.text.x=element_blank()
        , axis.ticks.x=element_blank()
        , axis.title.x=element_blank()
        , axis.text.y=element_blank()
        , axis.ticks.y=element_blank()
        , axis.title.y=element_blank()
        , plot.subtitle = element_text(hjust = 1)
        # , legend.position = "none"
      ) + 
      ggsave(filename = paste(globalVar$figConfig, saveImg, sep="/"), width = 10, height = 5, dpi = 600)
    
    
    # 년마다 수행행
    for (yearInfo in yearList) {
      
      tmpData = data %>%
        dplyr::group_by(key5, key6, addr1) %>%
        dplyr::summarise(sumVal = n()) %>%
        dplyr::filter(
          key5 == key5Info
          , key6 == key6Info
          , addr1 == "충청남도"
        )
      
      if (nrow(tmpData) <= 0) {
        next
      }
      
      dataL1 = data %>%
        dplyr::group_by(dtYear, addr1, addr2, addr3, key5, key6) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::filter(
          dtYear == yearInfo
          , key5 == key5Info
          , key6 == key6Info
        ) %>%
        dplyr::mutate(cnt = (n / tmpData$sumVal) * 100)
      
      if (nrow(dataL1) <= 0) {
        next
      }
      
      
      dataL2 = geoData_L1 %>%
        dplyr::left_join(dataL1, by = c("si_do_name" = "addr1", "sigungu_name" = "addr2", "emd_name" = "addr3")) %>%
        dplyr::mutate(
          plotLabel = dplyr::case_when(
            cnt > 0 ~ emd_name
            , stringr::str_detect(emd_name, regex("원성.동")) ~ ""
            , stringr::str_detect(emd_name, regex("당진.동")) ~ ""
            , stringr::str_detect(emd_name, regex("대천.동")) ~ ""
            , stringr::str_detect(emd_name, regex("동문.동")) ~ ""
            , stringr::str_detect(emd_name, regex("부성.동")) ~ ""
            , stringr::str_detect(emd_name, regex("성정.동")) ~ ""
            , stringr::str_detect(emd_name, regex("쌍용.동")) ~ ""
            , stringr::str_detect(emd_name, regex("온양.동")) ~ ""
            , TRUE ~ emd_name
          )
          , xOffset = dplyr::case_when(
            emd_name == "근흥면" ~ 1.0
            , emd_name == "안면읍" ~ 0.5
            , emd_name == "오천면" ~ 1.0
            , emd_name == "장항읍" ~ 0.06
            , emd_name == "한산면" ~ 0
            , emd_name == "지곡면" ~ 0.1
            , emd_name == "이원면" ~ -0.01
            , emd_name == "원북면" ~ -0.01
            , TRUE ~ 0
          )
          , yOffset = dplyr::case_when(
            emd_name == "마서면" ~ 0.02
            , emd_name == "진산면" ~ -0.02
            , emd_name == "복수면" ~ -0.02
            , emd_name == "군북면" ~ 0.02
            , emd_name == "원북면" ~ -0.02
            , emd_name == "팔봉면" ~ -0.02
            , emd_name == "신평면" ~ -0.02
            , emd_name == "해미면" ~ 0.01
            , emd_name == "이원면" ~ -0.03
            , emd_name == "원북면" ~ -0.045
            , emd_name == "이원면" ~ 0.01
            , emd_name == "장압면" ~ 0.05
            , emd_name == "남면" ~ -0.04
            , emd_name == "우성면" ~ -0.03
            , emd_name == "신창면" ~ 0.02
            , emd_name == "선장면" ~ -0.02
            , emd_name == "장항읍" ~ 0.005
            , TRUE ~ 0
          )
        )
      
      # 시각화
      saveImg = paste0("TMP2/Img_079_", yearInfo, "_", key6Info, "_", key5Info, ".png")
      plotSubTitle = paste0("[", key6Info, "] ", key5Info, " 의료이용률 읍면동별 현황(", yearInfo, ")")
      
      
      ggplot() +
        theme_bw() +
        coord_fixed(ratio = 1) +
        geom_polygon(data=dataL2, aes(x=long, y=lat, group=group, fill = n)) +
        scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
        geom_path(data=dataL2, aes(x=long, y=lat, group=group), colour='black',size = 0.5) +
        ggh4x::stat_midpoint(data=dataL2, aes(x=long + xOffset, y=lat + yOffset, group= emd_name, label = plotLabel), geom = "text", size = 2) +
        labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "단위 : 명", subtitle = NULL) +
        theme_minimal() +
        theme(
          text = element_text(size=18)
          , panel.grid.major.x = element_blank()
          , panel.grid.major.y = element_blank()
          , panel.grid.minor.x = element_blank()
          , panel.grid.minor.y = element_blank()
          , axis.text.x=element_blank()
          , axis.ticks.x=element_blank()
          , axis.title.x=element_blank()
          , axis.text.y=element_blank()
          , axis.ticks.y=element_blank()
          , axis.title.y=element_blank()
          , plot.subtitle = element_text(hjust = 1)
          # , legend.position = "none"
        ) + 
        ggsave(filename = paste(globalVar$figConfig, saveImg, sep="/"), width = 10, height = 5, dpi = 600)
      
    }
  }
}


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================
# 실습 과제 <11주차 과제 - 100점>
#    (1) 아래의 조건에 맞는 학번_이름.sqlite 파일을 생성한 후, 한림SmartLEAD에 제출할 것(ex.D20514_이진우.sqlite)
# (2) 제출 데드라인 : 2020.11.9(월요일) ~ 2020.11.15(일요일) 23:59 까지
# 1. SQLite를 이용하여 학번_이름.sqlite 데이터베이스를 생성
# 2. dbplyr를 이용하여 학번_이름(영어)인 테이블 생성 (ex. D20514_jinoo)
# <테이블의 구조>
#     
# ID : 자신의 학번
# GRADE : 자신의 학년
# NAME : 자신의 이름
# RESIDENCE : 자신의 거주 지역
# 3. dbplyr를 이용하여 각 열에 맞는 데이터 삽입(1행만)
# 예시 코드

# install.packages(c("DBI", "RSQLite", "tidyverse", "dbplyr"))
library(DBI)
library(RSQLite)
library(tidyverse)
library(dbplyr)

getwd()
dir.create("test")
list.dirs()

# 자신의 이름과 학번으로 설정할 것
con = RSQLite::dbConnect(drv = RSQLite::SQLite(), dbname = "test/D20165255_주현성.sqlite") 

DF = tibble(ID = c("D20165255"), GRADE = c("3"), NAME = c("주현성"), RESIDENCE = c(" 서울시 송파구"))

DF %>% show()

# connection, 생성한 dataframe, 테이블 명, 일시적인 테이블 해제
dplyr::copy_to(con, DF, "D20165255_JooHyuSung", temporary = FALSE) 

# 생성한 테이블 확인
dbListTables(con)

dplyr::tbl(con, "D20165255_JooHyuSung") %>% 
    collect() %>% 
    show()

dbDisconnect(con)

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

#==========================================
# 문제사진1
#==========================================
sDate = "1945-08-15"

format(as.Date(sDate), "%A")

#==========================================
# 문제사진5
#==========================================
nN = as.numeric(readline("n을 입력하세요"))
nP = as.numeric(readline("p을 입력하세요"))

liSeq = seq(1, nN)
nSum = sum(liSeq ** nP, na.rm = TRUE)


#==========================================
# 문제사진7
#==========================================
library(readr)
library(tidyverse)
library(rapport)
library(ggplot2)
library(scales)
library(pastecs)
library(psych)

# 2번 문제
dfData = read.csv(file = "./rpy/nutrient2.csv")

dfData2 = na_if(dfData, 0)

# > summary(data2)
# id         calcium          iron       protein       vitaminA       viataminC
# Min.   :  1   Min.   :   7   Min.   : 0   Min.   :  2   Min.   :    2   Min.   :  0
# 1st Qu.:226   1st Qu.: 326   1st Qu.: 7   1st Qu.: 46   1st Qu.:  277   1st Qu.: 26
# Median :457   Median : 547   Median :10   Median : 61   Median :  523   Median : 54
# Mean   :464   Mean   : 621   Mean   :11   Mean   : 66   Mean   :  836   Mean   : 79
# 3rd Qu.:704   3rd Qu.: 823   3rd Qu.:14   3rd Qu.: 81   3rd Qu.:  939   3rd Qu.:109
# Max.   :942   Max.   :2866   Max.   :59   Max.   :251   Max.   :34434   Max.   :415
#               NA's   :4      NA's   :2    NA's   :1     NA's   :9       NA's   :7

# 표준편차
stat.desc(dfData2)[13, ]

# 상자그림
boxplot(dfData2)

# 히스토그램
hist(dfData2)

#==========================================
# 문제사진8
#==========================================
dfData = read.csv(file = "./rpy/pima2.csv")
dfData2 = table(dfData$diabetes)

describe.by(dfData[ , c(3:9)], dfData$diabetes)

# 막대그림
barplot(dfData2)

# 원그림
pie(dfData2)

# (3)
dfData3 = cut(dfData$age, breaks = c(20, 30, 40, 50, 99), labels = c("20-30", "31-40", "41-50", "50+"), include.lowest = FALSE, right = TRUE)

dfData4 = table(dfData3, dfData$diabetes)
barplot(t(dfData4), legend=c("neg", "pos"))

# (4)
dfData5 = cut(dfData$pregnant, breaks = c(0, 5, 10, 99), labels = c("0-5", "6-10", "10+"), include.lowest = TRUE, right = FALSE)

dfData6 = table(dfData5, dfData$diabetes)
barplot(t(dfData6), legend=c("neg", "pos"))

# (5)
dfData6 = data.frame(dfData, type = dfData5)

# 평균
aggregate(. ~ diabetes + type, data=dfData6, mean, na.rm=TRUE)

# 표준편차
aggregate(. ~ diabetes + type, data=dfData6, sd, na.rm=TRUE)


#==========================================
# 문제사진9
#==========================================
library(MASS)
library(moonBook)
library(webr)
library(ggplot2)
library(tidyverse)

dfData = data.frame(
  Placebo = c(105, 119, 100, 97, 96, 101, 94, 95, 98)
  , Caffeine = c(96, 99, 94, 89, 96, 93, 88, 105, 88)
)

dfData2 = tidyr::gather(dfData)

# P값이 0.53으로서 귀무가설 기각하지 못함 (두 캡슐의 분산 차이가 없다)
# 따라서 등분산 조건 (var.equal = TRUE)
fTest = var.test(value ~ key, data = dfData2)
fTest

# P값이 0.063로서 귀무가설 기각 (두 캡슐의 차이가 있다)
tTest = t.test(value ~ key, data = dfData2, var.equal = TRUE)
tTest

#==========================================
# 문제사진10
#==========================================
dfData = readr::read_csv(file = "./rpy/mtcars.csv")
dfData2 = tidyr::gather(dfData[ , c(2, 10)])

# P값이 0.01으로서 귀무가설 기각 (두 변수간의 분산 차이가 있다)
# 따라서 상이한 분산 조건 (var.equal = FALSE)
fTest = var.test(value ~ key, data = dfData2)
fTest

# P값이 0.01 이하로서 귀무가설 기각 (두 변수간의 차이가 있다)
tTest = t.test(value ~ key, data = dfData2, var.equal = FALSE)
tTest


#==========================================
# 문제사진11
#==========================================
dfData = readr::read_csv(file = "./rpy/computer.csv")

dfData2 = dfData %>%
  dplyr::select(erp, myct, mmax, cach, chmin, chmax, prpe)

# 산점도
pairs(dfData2)

# 상관계수 행렬
cor(dfData2)

# 다중 선형 회귀모형
modelFit = lm(erp ~ myct + mmax + cach + chmin + chmax, data = dfData2)
summary(modelFit)

#==========================================
# 문제사진12
#==========================================
dfData = readr::read_csv(file = "./rpy/mtcars.csv")

modelFit = lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, data = dfData)

modelStepAic = MASS::stepAIC(modelFit, direction = "both")

summary(modelStepAic)


#==========================================
# 문제사진13
#==========================================
dfData = readr::read_csv(file = "./rpy/bateriasoap.csv")

modelFit = aov(BacterialCounts ~ Method, data = dfData)

modelTest = TukeyHSD(modelFit)
modelTest

#==========================================
# 문제사진14
#==========================================
dfData = readr::read_csv(file = "./rpy/downloading.csv")

modelFit = aov(`Time(Sec)` ~ TimeofDay, data = dfData)

modelTest = TukeyHSD(modelFit)
modelTest

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(MASS)
library(moonBook)
library(webr)
library(ggplot2)
library(tidyverse)

data = data.frame(
  typeA = c(0.4, 0.4, 0.4, 0.4, 0.5, 0.5, 0.5, 0.5, 0.5, 0.6, 0.7, 0.7, 0.8, 0.9, 0.9, 1.0, 1.0, 2.0)
  , typeB = c(0.4, 0.5, 0.5, 0.9, 0.5, 0.5, 0.5, 0.5, 0.5, 0.6, 1.1, 1.2, 0.8, 1.2, 1.9, 0.9, 2.0, 3.7)
)

# 4주 전/후의 차이
dataL1 = data %>%
  dplyr::mutate(diff = typeA - typeB)

# 4주 전/후 평균
mean(dataL1$diff, na.rm = TRUE)

# 각각의 평균과 분포에 대하여 정규성 파악
hist(dataL1$diff, xlab = "Diff")

# 정규성 검정
# P값이 0.0002541으로서 귀무가설 기각 (정규 분포를 따르지 않음)
shapiro.test(dataL1$diff)

qqnorm(dataL1$diff)
qqline(dataL1$diff)

# F 테스트 및 T 테스트
dataL2 = data %>%
  tidyr::gather()

# P값이 0.003173으로서 귀무가설 기각 (두 특성의 분산 차이가 있다)
# 따라서 상이한 분산 조건 (var.equal = FALSE)
fTest = var.test(value ~ key, data = dataL2)
fTest

plot(fTest) +
  xlim(0, 5) #+
  # ggsave(filename = paste(globalVar$figConfig, "fTest.png", sep = "/"), width = 10, height = 6, dpi = 600)

# P값이 0.1666로서 귀무가설 기각하지 못함 (두 특성은 차이가 없다)
tTest = t.test(value ~ key, data = dataL2, var.equal = FALSE)
tTest

plot(tTest) +
  xlim(-5, 5) # +
  # ggsave(filename = paste(globalVar$figConfig, "tTest.png", sep = "/"), width = 10, height = 6, dpi = 600)
