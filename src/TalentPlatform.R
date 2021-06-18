#=====================================
# Init Confiure
#=====================================
rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

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


Sys.setlocale("LC_CTYPE", "ko_KR.UTF-8")


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

Sys.setlocale("LC_ALL", locale = "us")
readr::guess_encoding("INPUT/o2job/연간지표.csv")

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

# excel -------------------------------------------------------------------



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
# 최저기온 ?10 ?9   2    3   10  12  22  26   20  13   5   -8
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


# 만기의 수입 = max?(S_T-K,0)계산
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
#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."
# globalVar$mapPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

# serviceName = "LSH0163"

#================================================
# Main
#================================================
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
# [그림 5]와 같이 <에어코리아> 사이트(https://www.airkorea.or.kr)에 접속하여 서울지역의 측정소 정보를 조회해 보고 해당 문제에 대해 R 프로그래밍을 작성하시오.
#
# 1.아래와 같은 서울지역의 측정소 정보를 가지는 데이터셋을 생성하시오. (5점)
# 측정소명 (ex. 강남구)
# 주소 (ex. 서울 강남구 학동로 426 강남구청 별관 1동)
fileInfo = Sys.glob(paste(globalVar$inpPath, "Q5.xls", sep = "/"))
dataQ5 = readxl::read_excel(fileInfo, sheet = "Sheet1")

dataQ5L1 = dataQ5 %>%
  dplyr::select(측정소명, 측정소주소)

dplyr::tbl_df(dataQ5L1)

# 2.위 1번에서 생성한 서울지역의 측정소 주소의 경도 및 위도를 구하고 경도 및 위도 필드를 데이터셋에 추가하시오.
dataQ5L2 = ggmap::mutate_geocode(dataQ5L1, 측정소주소, source = "google")

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

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."
# globalVar$mapPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

# serviceName = "LSH0163"

#================================================
# Main
#================================================
library(scales)
library(tidyverse)
library(ranger)
library(ggpubr)

fileInfo = Sys.glob(paste(globalVar$inpPath, "ToyotaCorolla.csv", sep = "/"))
car.df = read.csv(fileInfo)

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
saveImg = sprintf("%s/%s.png", globalVar$figPath, "Img_001")

car.df %>%
  ggplot(aes(x = Price)) +
  geom_histogram(aes(y = ..density..), binwidth = 1000) +
  stat_function(fun = dnorm, args = list(mean = mean(car.df$Price, na.rm = TRUE), sd = sd(car.df$Price, na.rm = TRUE)), lwd = 2, col = 'red') +
  labs(title = "도요타 중고차 가격 분포", x = "중고차 가격", y = "확률밀도(density)", subtitle = "단위: 유로") +
  scale_x_continuous(labels = scales::comma) +
  ggsave(filename = saveImg, dpi = 600)

#======================================================
# 빈도 분포
#======================================================
saveImg = sprintf("%s/%s.png", globalVar$figPath, "Img_002")

car.df %>%
  ggplot(aes(x = Price)) +
  geom_histogram(aes(y = ..count..)) +
  labs(title = "도요타 중고차 가격 분포", x = "중고차 가격", y = "빈도분포", subtitle = "단위: 유로") +
  scale_x_continuous(labels = scales::comma) +
  ggsave(filename = saveImg, dpi = 600)

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
library(useful)

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
log4r::logfile(log) = paste0(globalVar$logPath, "/", "log4r_", format(Sys.time(), "%Y%m%d"), ".log")
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
  , file = paste0(globalVar$outPath, "/", "Glowpick_CrawlingL2.xlsx")
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
  , file = paste0(globalVar$outPath, "/", "Glowpick_CrawlingL2.xlsx")
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

dataInfo = xlsx::read.xlsx2(file = paste0(globalVar$outPath, "/", "Glowpick_CrawlingL2.xlsx"), sheetIndex = 3)

dataDtlInfo = xlsx::read.xlsx2(file = paste0(globalVar$outPath, "/", "Glowpick_CrawlingL2.xlsx"), sheetIndex = 4)


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
webshot::webshot("fig.html", paste0(globalVar$figPath, "/", "Glowpick_Crawling_Keyword.png"), vwidth = 775, vheight = 550)

xlsx::write.xlsx2(
  dataL5
  , file = paste0(globalVar$outPath, "/", "Glowpick_CrawlingL2.xlsx")
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


senti_words_kr = readr::read_delim(paste0(globalVar$inpPath, "/", "SentiWord_Dict.txt"), delim = '\t', col_names = c("term", "score"))
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
  , file = paste0(globalVar$outPath, "/", "Glowpick_CrawlingL2.xlsx")
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
  , file = paste0(globalVar$outPath, "/", "Glowpick_CrawlingL2.xlsx")
  , sheetName = "긍부정키워드3"
  , append = TRUE
  , row.names = FALSE
  , col.names = TRUE
)

fig = wordcloud2::wordcloud2(data = dataL9)

# html로 내보내기
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# 내보낸 html 페이지로부터 png 형태로 불러와서 저장
webshot::webshot("fig.html", paste0(globalVar$figPath, "/", "Glowpick_Crawling_Keyword2.png"), vwidth = 775, vheight = 550, delay = 10)


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

data = readr::read_csv(file = paste0(globalVar$inpPath, "/", "mort_table_2_2014_2018.csv"))

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
# data = read.table(file = paste0(globalVar$inpPath, "/", "100T_merged_somatic_onlyFilterMutectCalls_PASS_step2.cosmic90.ann.hg19_multianno.vcf"), header = TRUE)
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

data = readr::read_csv(file = paste0(globalVar$inpPath, "/", "insurance.csv"))

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
fileList = Sys.glob(paste0(globalVar$inpPath, "/", "literacy_*.csv"))

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
# 1. (25점) ?  에 포함된 소인수(prime factor) 2의 개수를 산출하는 R 사용자 함수를 만들어
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

# 2. (25점) 대한민국 국회는 총 300석으로, A, B, C, D, E 당의 지역구?비례대표 수가 각각
# (161, 13), (84, 19), (1, 5), (0, 3), (7, 7) 명이다. 30명을 비복원 임의추출하여 특별위원
# 회를 구성하는 경우 위원회의 정당 및 지역구?비례대표 구성표를 5*2 표로 제시하라.
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


# 3. (25점) ? 개 요소의 벡터 ?   ? ?  ?? 에서 ? 개 요소를 복원 임의추출하는 기능의 R
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
# reticulate::conda_remove("r-reticulate")

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
# inpPath :  chr "E:/02. 블로그/지식iN/INPUT/o2job"
# fileList = Sys.glob(paste0(globalVar$inpPath, "/DOC/*.txt"))
# fileList = Sys.glob(paste0(globalVar$inpPath, "/DOC/1986년-1992년.txt"))
fileList = Sys.glob(paste0(globalVar$inpPath, "/DOC/1993-2001.txt"))

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
  outFile = paste(globalVar$outPath, stringr::str_replace_all(fs::path_file(fileInfo), ".txt", "_1.docx"), sep = "/")

  doc = officer::read_docx() %>%
    officer::body_add_par(value = paste(dataL1[1:20000,]$value, "(LineBreak)(LineBreak)", collapse = ""))
  print(doc, target = outFile)

  outFile = paste(globalVar$outPath, stringr::str_replace_all(fs::path_file(fileInfo), ".txt", "_2.docx"), sep = "/")

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


fileList = Sys.glob(paste0(globalVar$inpPath, "/DOC/파일_*.txt"))

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

dataL3 = read.xlsx(file = paste(globalVar$outPath, "DOC_Keyword.xlsx", sep = "/"), sheetName = "Sheet1", encoding = "UTF-8")

# 엑셀 출력
# xlsx::write.xlsx2(dataL3, file = paste(globalVar$outPath, "DOC_Keyword.xlsx", sep = "/"), append = FALSE, row.names = FALSE, col.names = TRUE)

fig = wordcloud2::wordcloud2(data = dataL3, size = 2)

# html로 내보내기
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# 내보낸 html 페이지로부터 png 형태로 불러와서 저장
webshot::webshot("fig.html", paste(globalVar$figPath, "DOC_Keyword.png", sep = "/"), delay = 10)

dataL4 = dataL3 %>%
  dplyr::slice(1:100)

# ggsave를 통해 이미지 저장
ggwordcloud::ggwordcloud2(dataL4[, c("token", "freq")], size = 2) +
  # theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  ggsave(filename = paste(globalVar$figPath, "DOC_Keyword2.png", sep = "/"), width = 10, height = 8, dpi = 600)


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
  ggsave(filename = paste(globalVar$figPath, "fTest.png", sep = "/"), width = 10, height = 6, dpi = 600)

# P값이 0.054로서 귀무가설 기각 (두 약의 심장 박동은 차이가 있다)
tTest = t.test(val ~ type, data = dataL2, var.equal = TRUE)
tTest

plot(tTest) +
  xlim(-5, 5) +
  ggsave(filename = paste(globalVar$figPath, "tTest.png", sep = "/"), width = 10, height = 6, dpi = 600)


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

fileList = Sys.glob(paste(globalVar$inpPath, "origin.csv", sep = "/"))

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

xlsx::write.xlsx2(dataL3, file = paste(globalVar$outPath, "Web_Visitor2.xlsx", sep = "/"), sheetName = "s01", append = TRUE, row.names = FALSE, col.names = TRUE)


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
xlsx::write.xlsx2(dataL3, file = paste(globalVar$outPath, "Web_Visitor2.xlsx", sep = "/"), sheetName = "s02", append = TRUE, row.names = FALSE, col.names = TRUE)

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
xlsx::write.xlsx2(dataL3, file = paste(globalVar$outPath, "Web_Visitor2.xlsx", sep = "/"), sheetName = "s03", append = TRUE, row.names = FALSE, col.names = TRUE)

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
xlsx::write.xlsx2(dataL3, file = paste(globalVar$outPath, "Web_Visitor2.xlsx", sep = "/"), sheetName = "s04", append = TRUE, row.names = FALSE, col.names = TRUE)

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
xlsx::write.xlsx2(dataL3, file = paste(globalVar$outPath, "Web_Visitor2.xlsx", sep = "/"), sheetName = "s05", append = TRUE, row.names = FALSE, col.names = TRUE)

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
xlsx::write.xlsx2(dataL3, file = paste(globalVar$outPath, "Web_Visitor2.xlsx", sep = "/"), sheetName = "s06", append = TRUE, row.names = FALSE, col.names = TRUE)

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
xlsx::write.xlsx2(dataL3, file = paste(globalVar$outPath, "Web_Visitor2.xlsx", sep = "/"), sheetName = "s07", append = TRUE, row.names = FALSE, col.names = TRUE)

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
xlsx::write.xlsx2(dataL3, file = paste(globalVar$outPath, "Web_Visitor2.xlsx", sep = "/"), sheetName = "s08", append = TRUE, row.names = FALSE, col.names = TRUE)

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
  , weight = c(80, 73, 72, 100, 72, 67, 75)
)

dataL1 = data %>%
  dplyr::mutate(bmi = (weight / height^2) * 10000.0) %>%
  dplyr::filter(bmi > 26) %>%
  dplyr::select(weight)

dataL1

data2 = data.frame(
  radius = c(1, 2.5, 3, 4.5, 5, 6.7)
  , height = c(2, 4, 6, 8, 10, 12)
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

# "E:/04. TalentPlatform/Github/TalentPlatform-R/INPUT/o2job 서울시 코로나19 확진자 현황.csv"
fileList = Sys.glob(paste(globalVar$inpPath, "서울시 코로나19 확진자 현황.csv", sep = "/"))
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
pie(dataL1$cnt, labels = label, col = rainbow(5), main = "국내외 접촉력 비율")

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
barplot(count, main = "3개 구의 환자상태", col = rainbow(3), legend = rownames(count))

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

fileList = Sys.glob(paste(globalVar$inpPath, "20201018(자료)통영_취합완성본_전1018.xlsx", sep = "/"))


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
  dplyr::select(-c("ID", "취미_어떤", "행태_술빈도", "행태_술량", "대화_총개수", "소통_총개수", "행태_담배년", "행태_담배갑", "행태_담배갑년", "행태_담배갑년gr", "행태_술빈도", "행태_술량", "행태_운동종류", "행태_교육종류", "병력_고혈압나이", "병력_고혈압기간", "병력_고혈압20년yn", "병력_당뇨나이", "병력_골다공증", "병력_골다공증나이", "병력_치주나이", "병력_머리사고나이", "병력_마취나이", "병력_총개수", "관리_총개수", "관리_치매어렵이유", "관리_치매어렵이유123", "변화_총개수")) %>%
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
data$variable = forcats::fct_relevel(data$variable, c("남자", "여자", "70대 이하", "80대 이상", "무학/초등", "중고등", "대학이상", "없음", "있음", "의료급여", "건강보험", "민간병원", "보건소"))

ggplot(data, aes(x = variable, y = value, fill = group, label = round(value, 1))) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.position = "top") +
  geom_text(aes(group = group), position = position_dodge(width = 0.9), size = 5, vjust = -0.5, hjust = 0.5) +
  ylim(0, 100) +
  facet_wrap(~type, scale = "free") +
  labs(x = "구분", y = "특성", fill = "", subtitle = "대상자 특성") +
  scale_fill_manual(values = c("#00bfc4", "#f8766d")) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = paste(globalVar$figPath, "Img_020.png", sep = "/"), width = 12, height = 8, dpi = 600)


# 건강관리 및 최근변화
dataL1 = data.frame(
  minCi = c(0.37, 0.21, 0.47, 0.31, 0.52, 0.72, 0.52, 0.94, 1.14, 0.91, 0.86, 0.62, 0.41, 0.76, 0.30, 0.29, 0.23, 2.31, 1.18, 0.87, 1.78)
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
dataL1$pVal = forcats::fct_relevel(dataL1$pVal, c("", "0.10", "0.05", "0.01"))


ggplot(data = dataL1, aes(x = rr, y = sampleNum)) +
  geom_errorbarh(aes(xmin = minCi, xmax = maxCi, color = pVal), size = 1) +
  geom_point(aes(color = pVal), size = 3) +
  geom_vline(xintercept = mean(dataL1$rr, na.rm = TRUE), size = 1, color = "black") +
  scale_y_reverse() +
  geom_label(aes(x = -7.8, y = sampleNum, label = paste0(type), fill = group), fontface = "bold", colour = "white", size = 4.5, hjust = "inward", vjust = "center") +
  geom_text(aes(x = -5, y = sampleNum, label = paste("RR :", round(rr, 2))), size = 4.5, hjust = "inward") +
  geom_text(aes(x = -2.5, y = sampleNum, label = paste("P : ", p, sep = "")), size = 4.5, hjust = "inward") +
  theme(legend.position = "top") +
  scale_x_continuous(expand = c(0, 0), limits = c(-8, 16)) +
  labs(x = "RR (95% CI)", y = "특성", color = "P-Value", fill = "", subtitle = "건강관리 및 최근변화") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = paste(globalVar$figPath, "Img_021.png", sep = "/"), width = 12, height = 8, dpi = 600)


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


ggplot(data = dataL3, aes(x = rr, y = sampleNum)) +
  geom_errorbarh(aes(xmin = minCi, xmax = maxCi, color = pVal), size = 1) +
  geom_point(aes(color = pVal), size = 3) +
  geom_vline(xintercept = mean(dataL3$rr, na.rm = TRUE), size = 1, color = "black") +
  scale_y_reverse() +
  geom_label(aes(x = -1.9, y = sampleNum, label = paste0(type), fill = group), fontface = "bold", colour = "white", size = 4.5, hjust = "inward", vjust = "center") +
  geom_text(aes(x = -1.0, y = sampleNum, label = paste("RR :", round(rr, 2))), size = 4.5, hjust = "inward") +
  geom_text(aes(x = -0.5, y = sampleNum, label = paste("P : ", p, sep = "")), size = 4.5, hjust = "inward") +
  theme(legend.position = "top") +
  scale_x_continuous(expand = c(0, 0), limits = c(-2, 2)) +
  labs(x = "RR (95% CI)", y = "특성", color = "P-Value", fill = "", subtitle = "소통, 어울림 및 형태") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = paste(globalVar$figPath, "Img_022.png", sep = "/"), width = 12, height = 8, dpi = 600)

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
dataL4$variable = forcats::fct_relevel(dataL4$variable, c("있다", "없다"))

ggplot(dataL4, aes(x = variable, y = value, fill = group, label = round(value, 1))) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.position = "top") +
  geom_text(aes(group = group), position = position_dodge(width = 0.9), size = 5, vjust = -0.5, hjust = 0.5) +
  ylim(0, 100) +
  facet_wrap(~type, scale = "free") +
  labs(x = "구분", y = "특성", fill = "", subtitle = "대상자 특성") +
  scale_fill_manual(values = c("#00bfc4", "#f8766d")) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = paste(globalVar$figPath, "Img_023.png", sep = "/"), width = 12, height = 8, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

rm(list = ls())

prjName = "o2job"

source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")


serviceName = "LSH0057"

library(readr)
library(xlsx)
library(readxl)
library(openxlsx)
library(tidyverse)
library(forcats)

fileInfo = Sys.glob(paste0(globalVar$inpPath, "/KWCS5th(190924).xlsx"))

# data = openxlsx::read.xlsx(fileInfo, sheet = 1)
data = readxl::read_excel(path = fileInfo, sheet = 1)

# 성별, 연령, 학력, 소득, 건강상태, 근로환경만족도
# HH02_01_A, HH02_01_B, EF1, EF11, Q60, Q69


# Q34 (근무형태)
dataQ34 = data %>%
  dplyr::select(!c("Q34_1_1", "Q34_1_4_ETC")) %>%
  dplyr::select(ID, dplyr::contains("Q34"))

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
  dplyr::summarise(sumQ57 = sum(q57, na.rm = TRUE)) %>%
  dplyr::mutate(
    isQ57 = ifelse(sumQ57 > 0, 1, 0)
  )

# Q64번 (폭력경험)
tmpDataQ64 = data %>%
  dplyr::select(!dplyr::contains("KQ64")) %>%
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
  dplyr::summarise(sumQ64 = sum(q64, na.rm = TRUE)) %>%
  dplyr::mutate(
    isQ64 = ifelse(sumQ64 > 0, 1, 0)
  )

dataL1 = data %>%
  dplyr::select(!c("Q34_1_1", "Q34_1_4_ETC")) %>%
  dplyr::select(ID, Q05A, HH02_01_A, HH02_01_B, EF1, EF11, Q60, Q69, dplyr::contains("Q34")) %>%
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
  na.omit() %>%
  dplyr::filter(Q05A != 4) # Q05A에서 무급가족종사자 샘플 제거

dataL1$Q34_1 = replace(dataL1$Q34_1, dataL1$Q34_1 == 2, 0)
dataL1$Q34_2 = replace(dataL1$Q34_2, dataL1$Q34_2 == 2, 0)
dataL1$Q34_3 = replace(dataL1$Q34_3, dataL1$Q34_3 == 2, 0)
dataL1$Q34_4 = replace(dataL1$Q34_4, dataL1$Q34_4 == 2, 0)
dataL1$Q34_5 = replace(dataL1$Q34_5, dataL1$Q34_5 == 2, 0)


dataQ34 = dataL1 %>%
  dplyr::select(ID, Q34_1, Q34_2, Q34_3, Q34_4, Q34_5) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    sumQ34 = sum(Q34_1, Q34_2, Q34_3, Q34_4, Q34_5, na.rm = TRUE)
  )

#=====================================================================
# 훈련 테스트 셋 설정 (1000개)
#=====================================================================
set.seed(1)

# 훈련 및 데이터 셋을 60:40으로 나누기 위한 인덱스 설정
ind = sample(1:nrow(dataL1), 1000)

# 해당 인덱스에 따라 자료 할당
dataL2 = dataL1[ind,] %>%
  dplyr::left_join(dataQ34, by = c("ID" = "ID"))

dplyr::tbl_df(dataL2)

#==================================
# Table 1
#==================================
# paste(unique(dataL2$typeQ34_1), collapse = ", ")

dataL2$typeHH02_01_A = forcats::fct_relevel(dataL2$typeHH02_01_A, c("남", "여"))
dataL2$typeAge = forcats::fct_relevel(dataL2$typeAge, c("30세 미만", "30-40세 미만", "40-50세 미만", "50-60세 미만", "60세 이상"))
dataL2$typeEF1 = forcats::fct_relevel(dataL2$typeEF1, c("중학교 졸업", "고등학교 졸업", "전문 대학교 졸업", "대학교 졸업", "대학원 재학 이상"))
dataL2$typeEF11 = forcats::fct_relevel(dataL2$typeEF11, c("100만 미만", "100-200만 미만", "200-300만 미만", "300-400만 미만", "400-500만 미만", "500만 이상"))
dataL2$typeQ60 = forcats::fct_relevel(dataL2$typeQ60, c("매우 좋다", "좋은 편이다", "보통이다", "나쁜 편이다"))
dataL2$typeQ69 = forcats::fct_relevel(dataL2$typeQ69, c("매우 만족한다", "만족한다", "별로 만족하지 않는다", "전혀 만족하지 않는다"))

dataL2$typeQ34_1 = forcats::fct_relevel(dataL2$typeQ34_1, c("예", "아니오"))
dataL2$typeQ34_2 = forcats::fct_relevel(dataL2$typeQ34_2, c("예", "아니오"))
dataL2$typeQ34_3 = forcats::fct_relevel(dataL2$typeQ34_3, c("예", "아니오"))
dataL2$typeQ34_4 = forcats::fct_relevel(dataL2$typeQ34_4, c("예", "아니오"))
dataL2$typeQ34_5 = forcats::fct_relevel(dataL2$typeQ34_5, c("예", "아니오"))
dataL2$isQ57 = forcats::fct_relevel(dataL2$isQ57, c("예", "아니오"))
dataL2$isQ64 = forcats::fct_relevel(dataL2$isQ64, c("예", "아니오"))

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
  dplyr::select(!dplyr::contains("type")) %>%
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
  dplyr::select(!dplyr::contains("type")) %>%
  # dplyr::select(dplyr::contains(c("Q34", "Q57", "Q64"))) %>%
  dplyr::select(dplyr::contains(c("Q34")), "Q60", "Q69") %>%
  colnames()


dataL3 = data.frame()
dataL4 = data.frame()

# i = 1
# j = 1

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
    dplyr::select(!dplyr::contains("type")) %>%
    # dplyr::select(dplyr::contains(c("Q34", "Q57", "Q64"))) %>%
    dplyr::select(dplyr::contains(c("Q34")), "Q60", "Q69") %>%
    dplyr::summarise_all(funs(
      mean(., na.rm = TRUE) %>% round(2) # 평균
      , sd(., na.rm = TRUE) %>% round(2) # 표준편차
      # , n() # 자료 개수
    ))

  dataL4 = dplyr::bind_rows(dataL4, tmpData2)
}

xlsx::write.xlsx2(dataL3, file = paste0(globalVar$outPath, "/Survery.xlsx"), sheetName = "dataL3_3", append = TRUE, row.names = FALSE, col.names = TRUE)

xlsx::write.xlsx2(dataL4, file = paste0(globalVar$outPath, "/Survery.xlsx"), sheetName = "dataL4_3", append = TRUE, row.names = FALSE, col.names = TRUE)

#==================================
# Table 4
#==================================
# 근무형태와 차별
# cor.test(dataL2$sumQ34, dataL2$sumQ57)

# 근무형태와 폭력경험
# cor.test(dataL2$sumQ34, dataL2$sumQ64)

# colList = c("Q34_1", "Q34_2", "Q34_3", "Q34_4", "Q34_5")
colList = c("sumQ34", "sumQ57", "sumQ64")
colList2 = c("sumQ34", "sumQ57", "sumQ64")

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

xlsx::write.xlsx2(dataL5, file = paste0(globalVar$outPath, "/Survery.xlsx"), sheetName = "dataL5", append = TRUE, row.names = FALSE, col.names = TRUE)


#==================================
# Table 5
#==================================
# 모든 변수 (성별, 연령, 학력, 소득, 건강상태, 근로만족도)
typeList = c("typeHH02_01_A", "typeAge", "typeEF1", "typeEF11", "typeQ60", "typeQ69")

colList = dataL2 %>%
  dplyr::select(!dplyr::contains("type")) %>%
  dplyr::select(dplyr::contains(c("Q34", "isQ57", "isQ64"))) %>%
  colnames()

dataL3 = data.frame()
dataL4 = data.frame()
resData = data.frame()

i = 1
j = 1
#
# summary(dataL2)
#
# dataL2 %>%
#   dplyr::select(Q34_1,typeHH02_01_A) %>%
#   dplyr::group_by(Q34_1, typeHH02_01_A) %>%
#   dplyr::summarise(cnt = n()) %>%
#   dplyr::mutate(
#     ratio = (cnt / sum(cnt, na.rm = TRUE)) * 100
#     , label = paste0(cnt, "(", ratio, ")")
#   )


for (j in 1:length(colList)) {
  for (i in 1:length(typeList)) {


    tmpData = dataL2 %>%
      dplyr::select(typeList[i], colList[j]) %>%
      dplyr::group_by(get(typeList[i]), get(colList[j])) %>%
      dplyr::summarise(cnt = n()) %>%
      dplyr::mutate(
        ratio = round((cnt / sum(cnt, na.rm = TRUE)) * 100, 2)
        , label = paste0(cnt, "(", ratio, ")")
      )

    resData = data.frame(
      type = typeList[i]
      , col = colList[j]
      , tmpData
    )

    dataL3 = dplyr::bind_rows(dataL3, resData)


    tableData = table(get(typeList[i], dataL2), get(colList[j], dataL2))

    # Perform the Chi-Square test
    chisqTest = chisq.test(tableData)

    resData2 = data.frame(
      type = typeList[i]
      , col = colList[j]
      , xSqu = chisqTest$statistic %>% round(2)
      , pVal = chisqTest$p.value %>% round(2)
    ) %>%
      dplyr::mutate(
        label = paste0(xSqu, "(", pVal, ")")
      )

    dataL4 = dplyr::bind_rows(dataL4, resData2)
  }
}

saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "Survery.xlsx")

isDir = dir.exists(path = fs::path_dir(saveFile))
if (isDir == FALSE) { file.create(fs::path_dir(saveFile)) }
isFile = file.exists(path = saveFile)
if (isFile == TRUE) { file.remove(path = saveFile) }

xlsx::write.xlsx2(as.data.frame(dataL3), file = saveFile, sheetName = "dataL3", append = TRUE, row.names = FALSE, col.names = TRUE)
xlsx::write.xlsx2(as.data.frame(dataL4), file = saveFile, sheetName = "dataL4", append = TRUE, row.names = FALSE, col.names = TRUE)

dataL3 %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(
    ss = sum(cnt, na.rm = TRUE)
  )

#==================================
# Table 6
#==================================

library(nnet)
library(MASS)

colList = dataL2 %>%
  dplyr::select(!dplyr::contains("type")) %>%
  dplyr::select(dplyr::contains(c("Q34", "isQ57", "isQ64"))) %>%
  colnames()

dataL5 = data.frame()

for (i in 1:length(colList)) {

  glmFitVarAll = glm(as.factor(get(colList[i], dataL2)) ~ typeHH02_01_A +
    typeAge +
    typeEF1 +
    typeEF11 +
    typeQ60 +
    typeQ69 +
    0, data = dataL2, family = binomial)

  # glmFitVarAll = nnet::multinom(Q34_1 ~ typeHH02_01_A + typeAge + typeEF1 + typeEF11 + typeQ60 + typeQ69 + 0, data = dataL2)
  # summary(glmFitVarAll)

  # glmFitVarAll = MASS::polr(Q34_1 ~ typeHH02_01_A + typeAge + typeEF1 + typeEF11 + typeQ60 + typeQ69 + 0, data = dataL2)
  # summary(glmFitVarAll)


  # 1) AIC 기준으로 변수 선택
  stepAic = MASS::stepAIC(glmFitVarAll, direction = "both")

  # 결과에 대한 요약
  summary(stepAic)

  # 신뢰구간
  confData = confint(stepAic, level = 0.95) %>%
    as.data.frame() %>%
    dplyr::filter(!is.na(`97.5 %`))

  # 회귀계수, 신뢰구간, 오즈비, p-value
  resData = data.frame(
    col = colList[i]
    , col = colList[i]
    , coef = summary(stepAic)$coefficients[, 1] %>% round(2)
    , odds = exp(summary(stepAic)$coefficients[, 1]) %>% round(2)
    , conf = confData %>% round(2)
    , pVal = summary(stepAic)$coefficients[, 4] %>% round(2)
  )

  dataL5 = dplyr::bind_rows(dataL5, resData)
}

saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "Survery.xlsx")

xlsx::write.xlsx2(dataL5, file = saveFile, sheetName = "dataL5", append = TRUE, row.names = FALSE, col.names = TRUE)

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."
# globalVar$mapPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

# serviceName = "LSH0163"

#================================================
# Main
#================================================
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
  , v1 = c(433, 836, 880, 882, 1090, 1117, 1016, 990, 1011, 1056, 1019, 1139, 988, 772, 1103, 1182, 1179, 1190, 1136, 1295, 1169, 1177, 1605, 1249, 1249, 1643, 1406, 1853, 1578, 1599, 2639, 1793, 1548, 1638)
  , v2 = c(647, 586, 1315, 937, 1805, 1076, 1108, 1047, 1317, 1898, 2148, 1658, 1729, 1700, 1118, 1656, 1214, 1212, 1319, 1324, 1585, 1426, 1429, 1466, 1466, 1290, 2078, 1918, 1025, 1244, 2983, 2102, 1720, 1808)
  , v3 = c(280, 177, 484, 229, 407, 543, 1081, 450, 677, 749, 752, 1423, 1108, 951, 1033, 732, 669, 1003, 988, 506, 926, 742, 610, 1010, 1010, 1476, 1787, 1722, 1176, 1135, 2452, 1870, 1268, 1107)
  # , gdpNor = c(rep(NA, 14), 18928, 20223, 21277, 21887, 23707, 24735, 24358, 24758, 27241, 28484, 29880, 32228, 33212, 33614, 33949, 34137, 36103, 36382, 35671, NA)
  , gdpNor = c(rep(NA, 4), 16354.1, 16737.0, 16415.9, 16406.3, 16997.7, 17132.0, 17222.0, 16784.4, 17538.6, 18688.1, 18927.5, 20222.8, 21276.7, 21887.4, 23707.0, 24734.8, 24357.9, 24757.6, 27241.2, 28483.5, 29879.9, 32227.8, 33211.9, 33614.2, 33949.4, 34136.7, 36103.3, 36381.8, 35670.5, 35278.6)
  , gdpSou = c(rep(NA, 14), 651634, 707021, 784741, 837365, 908439, 957448, 1005602, 1089660, 1154217, 1205348, 1322611, 1388937, 1440111, 1500819, 1562929, 1658020, 1740780, 1835698, 1893497, NA)
  # , gdpRatNor = c(rep(NA, 14), 0.4, 3.8, 1.2, 1.8, 2.1, 3.8, -1.0, -1.2, 3.1, -0.9, -0.5, 0.8, 1.3, 1.1, 1.0, -1.1, 3.9, -3.5, -4.1, NA)
  , gdpRatNor = c(rep(NA, 4), -4.3, -4.4, -7.1, -4.5, -2.1, -4.4, -3.4, -6.5, -0.9, 6.1, 0.4, 3.8, 1.2, 1.8, 2.1, 3.8, -1.0, -1.2, 3.1, -0.9, -0.5, 0.8, 1.3, 1.1, 1.0, -1.1, 3.9, -3.5, -4.1, 0.4)
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

ggplot(ggData, aes(x = dtDate, y = val, color = key, label = round(val, 0))) +
  geom_line(size = 2) +
  scale_x_date(labels = date_format("%y"), date_breaks = "1 years", limits = c(as.Date("1985-01-01"), as.Date("2019-01-01")), date_minor_breaks = "1 years") +
  ylim(0, 105) +
  labs(x = "년도", y = "토픽 지수", color = "", subtitle = "시계열 LDA-토픽모델링 분석 결과") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = paste(globalVar$figPath, "Img_031.png", sep = "/"), width = 12, height = 8, dpi = 600)


ggplot(ggData, aes(x = dtDate, y = val, fill = key, label = round(val, 0))) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.position = "top") +
  geom_text(aes(group = key), position = position_dodge(width = 0.9), size = 5, vjust = -0.5, hjust = 0.5) +
  ylim(0, 105) +
  facet_wrap(~key, scale = "free") +
  labs(x = "", y = "", fill = "", subtitle = "") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = paste(globalVar$figPath, "Img_030.png", sep = "/"), width = 12, height = 8, dpi = 600)


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


xlsx::write.xlsx2(dataL1, file = paste0(globalVar$outPath, "/Gdp_Rat.xlsx"), sheetName = "RAW", append = TRUE, row.names = FALSE, col.names = TRUE)

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


lmFit = lm(gdpNor ~ scaleVar1 * scaleVar2 +
  scaleVar1 * scaleVar3 +
  scaleVar2 * scaleVar3, data = dataL2)
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
lmFit = lm(gdpRatNor ~ scaleVar1 * scaleVar2 +
  scaleVar1 * scaleVar3 +
  scaleVar2 * scaleVar3, data = dataL2)
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

ggplot(ggData, aes(x = dtDate, y = val, color = key, label = round(val, 0))) +
  geom_line(size = 2) +
  scale_x_date(labels = date_format("%y"), date_breaks = "1 years", limits = c(as.Date("1990-01-01"), as.Date("2018-01-01")), date_minor_breaks = "1 years") +
  labs(x = "년도", y = "GDP", color = "", subtitle = "북한 GDP 그래프") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = paste(globalVar$figPath, "Img_033.png", sep = "/"), width = 12, height = 8, dpi = 600)


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

ggplot(ggData, aes(x = dtDate, y = val, color = key, label = round(val, 0))) +
  geom_line(size = 2) +
  scale_x_date(labels = date_format("%y"), date_breaks = "1 years", limits = c(as.Date("1990-01-01"), as.Date("2018-01-01")), date_minor_breaks = "1 years") +
  labs(x = "년도", y = "경제 성장률", color = "", subtitle = "북한 경제 성장률 그래프") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = paste(globalVar$figPath, "Img_034.png", sep = "/"), width = 12, height = 8, dpi = 600)

xlsx::write.xlsx2(ggData, file = paste0(globalVar$outPath, "/Gdp_Rat.xlsx"), sheetName = "L5", append = TRUE, row.names = FALSE, col.names = TRUE)

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."
# globalVar$mapPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

# serviceName = "LSH0163"

#================================================
# Main
#================================================
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
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = round(meanCty, 2)), vjust = 1.6, color = "white", size = 4) +
  labs(x = "제조업체", y = "도시 연비", fill = "", subtitle = "자동차 제조업체에 따른 도시 연비 그래프") +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figPath, "Img_035.png", sep = "/"), width = 12, height = 8, dpi = 600)

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
  theme(text = element_text(size = 18)) +
  ggsave(filename = paste(globalVar$figPath, "Img_036.png", sep = "/"), width = 6, height = 6, dpi = 600)


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
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = round(key2, 2)), vjust = 1.6, color = "white", size = 6) +
  labs(x = "", y = "수수료 [단위 : 백만원]", fill = "판매수", subtitle = "이름에 따른 수수료 그래프") +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(direction = -1) +
  ggsave(filename = paste(globalVar$figPath, "Img_036.png", sep = "/"), width = 12, height = 8, dpi = 600)

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

fileList = Sys.glob(paste(globalVar$inpPath, "TC_TOUR_PURPSBY_CNSMP_SCALE_STLE_INFO_2019.csv", sep = "/"))

data = readr::read_csv(file = fileList, locale = locale("ko", encoding = "UTF-8"))

dplyr::tbl_df(data)

#==========================================
# 성별
#==========================================
dataL1 = dplyr::bind_rows(
  data.frame(table(data$SEX_NM, data$INTRST_FASHN_AT), type = "패션주목")
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
    val = (Freq / cnt) * 100.0
    , isYn = dplyr::case_when(
      Var2 == "Y" ~ "있다"
      , Var2 == "N" ~ "없다"
      , TRUE ~ "NULL"
    )
  ) %>%
  dplyr::rename(
    "key" = Var1
  )

ggplot(dataL3, aes(x = key, y = val, fill = isYn, label = round(val, 1))) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.position = "top") +
  geom_text(aes(group = isYn), position = position_dodge(width = 0.9), size = 5, vjust = 1.6, hjust = 0.5, color = "white") +
  ylim(0, 100) +
  facet_wrap(~type, scale = "free") +
  labs(x = "성별", y = "비율", fill = "", subtitle = "") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = paste(globalVar$figPath, "Img_040.png", sep = "/"), width = 15, height = 10, dpi = 600)

#==========================================
# 나이
#==========================================
dataL1 = dplyr::bind_rows(
  data.frame(table(data$YEAR_NM, data$INTRST_FASHN_AT), type = "패션주목")
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
    val = (Freq / cnt) * 100.0
    , isYn = dplyr::case_when(
      Var2 == "Y" ~ "있다"
      , Var2 == "N" ~ "없다"
      , TRUE ~ "NULL"
    )
  ) %>%
  dplyr::rename(
    "key" = Var1
  )

ggplot(dataL3, aes(x = key, y = val, fill = isYn, label = round(val, 0))) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.position = "top") +
  geom_text(aes(group = isYn), position = position_dodge(width = 0.9), size = 5, vjust = 1.6, hjust = 0.5, color = "white") +
  ylim(0, 100) +
  facet_wrap(~type, scale = "free") +
  labs(x = "나이", y = "비율", fill = "", subtitle = "") +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figPath, "Img_041.png", sep = "/"), width = 15, height = 10, dpi = 600)


#==========================================
# 혼인상태
#==========================================
dataL1 = dplyr::bind_rows(
  data.frame(table(data$MRRG_NM, data$INTRST_FASHN_AT), type = "패션주목")
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
    val = (Freq / cnt) * 100.0
    , isYn = dplyr::case_when(
      Var2 == "Y" ~ "있다"
      , Var2 == "N" ~ "없다"
      , TRUE ~ "NULL"
    )
  ) %>%
  dplyr::rename(
    "key" = Var1
  )

ggplot(dataL3, aes(x = type, y = val, fill = isYn, group = isYn, label = round(val, 0))) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.position = "top") +
  # geom_text_repel(position=position_dodge(width=0.5), size=5, color = "white") +
  ylim(0, 100) +
  labs(x = "혼인상태", y = "비율", fill = "", subtitle = "") +
  facet_wrap(~key) +
  coord_polar() +
  theme_bw() +
  theme(
    axis.text.x = element_text(face = "bold")
    , text = element_text(size = 18)
  ) +
  ggsave(filename = paste(globalVar$figPath, "Img_042.png", sep = "/"), width = 15, height = 8, dpi = 600)

# ggplot(dataL3, aes(x = key, y = val, fill = isYn, label=round(val, 0))) +
#     geom_bar(position = "dodge", stat="identity") +
#     theme(legend.position = "top") +
#     geom_text(aes(group=isYn), position=position_dodge(width=0.9), size=5, vjust=1.6, hjust=0.5, color = "white") +
#     ylim(0, 100) +
#     facet_wrap( ~ type, scale="free") +
#     labs(x = "혼인상태", y = "비율", fill="", subtitle = "") +
#     theme(text = element_text(size=18)) +
#     # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#     ggsave(filename = paste(globalVar$figPath, "Img_042.png", sep="/"), width = 15, height = 10, dpi = 600)


#==========================================
# 가정상태
#==========================================
dataL1 = dplyr::bind_rows(
  data.frame(table(data$CHLDRN_NM, data$INTRST_FASHN_AT), type = "패션주목")
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
    val = (Freq / cnt) * 100.0
    , isYn = dplyr::case_when(
      Var2 == "Y" ~ "있다"
      , Var2 == "N" ~ "없다"
      , TRUE ~ "NULL"
    )
  ) %>%
  dplyr::rename(
    "key" = Var1
  )

ggplot(dataL3, aes(x = key, y = val, fill = isYn, label = round(val, 0))) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.position = "top") +
  geom_text(aes(group = isYn), position = position_dodge(width = 0.9), size = 5, vjust = 1.6, hjust = 0.5, color = "white") +
  ylim(0, 100) +
  facet_wrap(~type, scale = "free") +
  labs(x = "가정상태", y = "비율", fill = "", subtitle = "") +
  theme(text = element_text(size = 18)) +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figPath, "Img_043.png", sep = "/"), width = 15, height = 10, dpi = 600)


#==========================================
# 자산지수
#==========================================
dataL1 = dplyr::bind_rows(
  data.frame(table(data$ASSETS_IDEX_NM, data$INTRST_FASHN_AT), type = "패션주목")
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
    val = (Freq / cnt) * 100.0
    , isYn = dplyr::case_when(
      Var2 == "Y" ~ "있다"
      , Var2 == "N" ~ "없다"
      , TRUE ~ "NULL"
    )
  ) %>%
  dplyr::rename(
    "key" = Var1
  )

ggplot(dataL3, aes(x = type, y = val, fill = isYn, group = isYn, label = round(val, 0))) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.position = "top") +
  # geom_text_repel(position=position_dodge(width=0.5), size=5, color = "white") +
  ylim(0, 100) +
  labs(x = "자산지수", y = "비율", fill = "", subtitle = "") +
  facet_wrap(~key) +
  coord_polar() +
  theme_bw() +
  theme(
    axis.text.x = element_text(face = "bold")
    , text = element_text(size = 18)
  ) +
  ggsave(filename = paste(globalVar$figPath, "Img_044.png", sep = "/"), width = 15, height = 8, dpi = 600)

# ggplot(dataL3, aes(x = key, y = val, fill = isYn, label=round(val, 0))) +
#     geom_bar(position = "dodge", stat="identity") +
#     theme(legend.position = "top") +
#     geom_text(aes(group=isYn), position=position_dodge(width=0.9), size=5, vjust=1.6, hjust=0.5, color = "white") +
#     ylim(0, 100) +
#     facet_wrap( ~ type, scale ="free") +
#     labs(x = "가정상태", y = "비율", fill="", subtitle = "") +
#     theme(text = element_text(size=18)) +
#     ggsave(filename = paste(globalVar$figPath, "Img_044.png", sep="/"), width = 15, height = 8, dpi = 600)


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
#Lists are the R objects which contain elements of different types like ? numbers, strings, vectors and another list inside it. A list can also contain a matrix or a function as its elements.
p = 2500
imdb = dataset_imdb(num_words = p, skip_top = 00) #, skip_top = 10
train_data = imdb$train$x
train_labels = imdb$train$y
test_data = imdb$test$x
test_labels = imdb$test$y

##아래 아홉줄은 제가 작성한 코드입니다... 많이 허접하지만, 대충 뭘 하려는지 느낌이 오실거라 생각됩니다.문제는 제대로 작동을 안한다는거죠..

numberWords.train = max(sapply(train_data, max))
numberWords.test = max(sapply(test_data, max))

positive_train_data = subset(imdb$train$x, imdb$train$y == 1)
tenpro_positive_train_data = head(positive_train_data, 1250)

positive_train_labels = subset(imdb$train$y, imdb$train$y == 1)
tenpro_positive_labels_data = head(positive_train_labels, 1250)

negative_train_data = subset(imdb$train$x, imdb$train$y == 0)
negative_train_labels = subset(imdb$train$y, imdb$train$y == 0)

new_train_data = rbind(negative_train_data, tenpro_positive_train_data)
new_train_labels = rbind(tenpro_positive_) ##does not work. It should include 13250 elements not 25000.


##sub_imdb=subset(imdb$train$x, imdb$train$y==0)

#c(c(train_data, train_labels), c(test_data, test_labels)) %<-% imdb

#The variables train_data and test_data are lists of reviews; each review is a list of
#word indices (encoding a sequence of words). train_labels and test_labels are
#lists of 0s and 1s, where 0 stands for negative and 1 stands for positive:

str(train_data[[1]])

word_index = dataset_imdb_word_index() #word_index is a named list mapping words to an integer index
reverse_word_index = names(word_index) # Reverses it, mapping integer indices to words
names(reverse_word_index) = word_index


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


X.train = vectorize_sequences(train_data)
X.test = vectorize_sequences(test_data)

#str(X.train[1,])
y.train = as.numeric(train_labels)
n.train = length(y.train)
y.test = as.numeric(test_labels)
n.test = length(y.test)


#fit                     =        glm(y.train ~ X.train, family = "binomial")
library(glmnet)
fit = glmnet(X.train, y.train, family = "binomial", lambda = 0.0)

beta0.hat = fit$a0
beta.hat = as.vector(fit$beta)

# (a) What are the top 10 words associated with positive reviews?
print(paste("word most associated with positive reviews = ", reverse_word_index[[as.character((which.max(beta.hat) - 3))]]))

# (b) What are the top 10 words associated with negative reviews?
print(paste("word most associated with negative reviews = ", reverse_word_index[[as.character((which.min(beta.hat) - 3))]]))

# (c) How can we identify the review in the training set that is hardest to classify? Find it and present the review. (1 point)
prob.train = exp(X.train %*% beta.hat + beta0.hat) / (1 + exp(X.train %*% beta.hat + beta0.hat))


hardest.to.classify = which.min(abs(prob.train - 0.5))
most.positive = which.max(prob.train)
most.negative = which.min(prob.train)
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


distance.P = (X.train[y.train == 1,] %*% beta.hat + beta0.hat)
distance.N = X.train[y.train == 0,] %*% beta.hat + beta0.hat

breakpoints = pretty((min(c(distance.P, distance.N)) - 0.001):max(c(distance.P, distance.N)), n = 200)
# n=200 above refers to the number of bins used in the histogram
# the large the number of bins, the higher the level of detail we see
hg.pos = hist(distance.P, breaks = breakpoints, plot = FALSE) # Save first
hg.neg = hist(distance.N, breaks = breakpoints, plot = FALSE) # Save second
color1 = rgb(0, 0, 230, max = 255, alpha = 80, names = "lt.blue")
color2 = rgb(255, 0, 0, max = 255, alpha = 80, names = "lt.pink")

library(latex2exp)

plot(hg.pos, col = color1, xlab = TeX('$x^T  \\beta +  \\beta_0$'), main = paste("train: histogram  ")) # Plot 1st histogram using a transparent color
plot(hg.neg, col = color2, add = TRUE) # Add 2nd histogram using different color


prob.test = exp(X.test %*% beta.hat + beta0.hat) / (1 + exp(X.test %*% beta.hat + beta0.hat))
dt = 0.01
thta = 1 - seq(0, 1, by = dt)
thta.length = length(thta)
FPR.train = matrix(0, thta.length)
TPR.train = matrix(0, thta.length)
FPR.test = matrix(0, thta.length)
TPR.test = matrix(0, thta.length)

# The ROC curve is a popular graphic for simultaneously displaying the ROC curve two types of errors for all possible thresholds.
# The name “ROC” is historic, and comes from communications theory. It is an acronym for receiver operating characteristics.
# ROC curves are useful for comparing different classifiers, since they take into account all possible thresholds.
# varying the classifier threshold changes its true positive and false positive rate.

for (i in c(1:thta.length)) {
  # calculate the FPR and TPR for train data
  y.hat.train = ifelse(prob.train > thta[i], 1, 0) #table(y.hat.train, y.train)
  FP.train = sum(y.train[y.hat.train == 1] == 0) # false positives = negatives in the data that were predicted as positive
  TP.train = sum(y.hat.train[y.train == 1] == 1) # true positives = positives in the data that were predicted as positive
  P.train = sum(y.train == 1) # total positives in the data
  N.train = sum(y.train == 0) # total negatives in the data
  FPR.train[i] = FP.train / N.train # false positive rate = type 1 error = 1 - specificity
  TPR.train[i] = TP.train / P.train # true positive rate = 1 - type 2 error = sensitivity

  # calculate the FPR and TPR for test data
  y.hat.test = ifelse(prob.test > thta[i], 1, 0)
  FP.test = sum(y.test[y.hat.test == 1] == 0) # false positives = negatives in the data that were predicted as positive
  TP.test = sum(y.hat.test[y.test == 1] == 1) # true positives = positives in the data that were predicted as positive
  P.test = sum(y.test == 1) # total positives in the data
  N.test = sum(y.test == 0) # total negatives in the data
  FPR.test[i] = FP.test / N.test # false positive rate = type 1 error = 1 - specificity
  TPR.test[i] = TP.test / P.test # true positive rate = 1 - type 2 error = sensitivity
  # print(paste("K=", K, " ki=",ki, ", K-fold CV=", Kfold.CV.err[i]))
}
#auc.train = auc(FPR.train, TPR.train)
auc.train = sum((TPR.train[1:(thta.length - 1)] + 0.5 * diff(TPR.train)) * diff(FPR.train))
auc.test = sum((TPR.test[1:(thta.length - 1)] + 0.5 * diff(TPR.test)) * diff(FPR.test))

print(paste("train AUC =", sprintf("%.4f", auc.train)))
print(paste("test AUC  =", sprintf("%.4f", auc.test)))


library(ggplot2)

errs.train = as.data.frame(cbind(FPR.train, TPR.train))
errs.train = data.frame(x = errs.train$V1, y = errs.train$V2, type = "Train")
errs.test = as.data.frame(cbind(FPR.test, TPR.test))
errs.test = data.frame(x = errs.test$V1, y = errs.test$V2, type = "Test")
errs = rbind(errs.train, errs.test)

ggplot(errs) +
  geom_line(aes(x, y, color = type)) +
  labs(x = "False positive rate", y = "True positive rate") +
  ggtitle("ROC curve", (sprintf("train AUC=%.4f,test AUC =%0.4f", auc.train, auc.test)))


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

fileList = Sys.glob(paste(globalVar$inpPath, "Brands_All.csv", sep = "/"))

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
    , !is.na(UID)
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
  geom_histogram(aes(y = ..density..), binwidth = 0.2, colour = "black", fill = "white") +
  geom_density(alpha = 0.2) +
  stat_function(fun = dnorm, args = list(mean = mean(dataL2$meanVal, na.rm = TRUE), sd = sd(dataL2$meanVal, na.rm = TRUE)), col = 'blue') +
  geom_rug(aes(x = meanVal, y = 0), position = position_jitter(height = 0)) +
  labs(x = "구매력 [단위 : 1,000,000]", y = "밀도 함수", colour = "특성", fill = "특성", subtitle = "") +
  xlim(0, 20) +
  ylim(0, 0.35) +
  theme(text = element_text(size = 18)) +
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
  geom_smooth(method = 'loess', formula = y ~ x, se = FALSE) +
  labs(x = "년도", y = "구매력 [단위 : 1,000,000]", shape = "특성", colour = "특성", fill = "", subtitle = "") +
  # annotate("text", x = 2011, y = 7.75, size = 5, label = sprintf("Y = %.2f X - %.2f", 0.0034, 3.2693), hjust = 0, color = hue_pal()(1)) +
  # annotate("text", x = 2011, y = 7.25, size = 5, label = sprintf("Y = %.2f X - %.2f", 0.2101, 419.6287), hjust = 0, color = hue_pal()(2)) +
  annotate("text", x = 2011, y = 7.75, size = 5, label = sprintf("Span is %.2f (DF : %.2f)", 0.75, 2), hjust = 0, color = hue_pal()(1)) +
  annotate("text", x = 2011, y = 7.25, size = 5, label = sprintf("Span is %.2f (DF : %.2f)", 0.75, 2), hjust = 0, color = hue_pal()(2)) +
  xlim(2010, 2020) +
  ylim(0, 8) +
  theme(text = element_text(size = 18)) +
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

fileInfo = Sys.glob(paste(globalVar$inpPath, "out.csv", sep = "/"))
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))


fileInfo = Sys.glob(paste(globalVar$inpPath, "out2.csv", sep = "/"))
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
  theme(text = element_text(size = 18)) +
  ggsave(filename = paste(globalVar$figPath, "Img_060_ggpairs.png", sep = "/"), width = 12, height = 8, dpi = 600)

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
trainData = dataL1[-ind,]
testData = dataL1[ind,]

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


png(file = paste(globalVar$figPath, "Img_060_HiddenLayer.png", sep = '/'), width = 10, height = 5, units = "in", res = 1200)
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

plot(xAxis, yAxis, col = 'blue', main = 'Recl vs Predicted', pch = 1, type = "p", xlab = "Actual", ylab = "Predicted")

cor(xAxis, yAxis)
Metrics::bias(xAxis, yAxis)
Metrics::rmse(xAxis, yAxis)

abline(0, 1, col = "black")


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
  theme(text = element_text(size = 18)) +
  ggsave(filename = paste(globalVar$figPath, "Img_060_MLR.png", sep = "/"), width = 6, height = 6, dpi = 600)


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
  theme(text = element_text(size = 18)) +
  ggsave(filename = paste(globalVar$figPath, "Img_060_ANN.png", sep = "/"), width = 6, height = 6, dpi = 600)


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
  theme(text = element_text(size = 18)) +
  ggsave(filename = paste(globalVar$figPath, "Img_060_DL.png", sep = "/"), width = 6, height = 6, dpi = 600)


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

data <- mlptrain(inDataL2, neurons = 64, outDataL2, it = 10000);
mlp(x, data$weight, data$dist, data$neurons, data$actfns)


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
      , BWSQRT01 = sqrt(b1 / w1)
      , BWSQRT02 = sqrt(b2 / w2)
      , BWSQRT03 = sqrt(b3 / w3)
      , BWSQRT04 = sqrt(b4 / w4)
      , BWSQRT05 = sqrt(b5 / w5)
      , BWSQRT06 = sqrt(b6 / w6)
      , BWSQRT07 = sqrt(b7 / w7)
      , BWSQRT08 = sqrt(b8 / w8)
      , BWSQRT09 = sqrt(b9 / w9)
      , BWSQRT10 = sqrt(b10 / w10)
      , BWSQRT11 = sqrt(b11 / w11)
      , BWSQRT12 = sqrt(b12 / w12)
      , BWSQRT13 = sqrt(b13 / w13)
    )

  return(result_data)

}


# fileInfo = Sys.glob(paste(globalVar$inpPath, "wine.csv", sep = "/"))
fileInfo = Sys.glob(paste(globalVar$inpPath, "wine2.csv", sep = "/"))
data_wind <- read.csv(fileInfo)

fileInfo = Sys.glob(paste(globalVar$inpPath, "인구통계별.xlsx", sep = "/"))
data_human <- xlsx::read.xlsx2(fileInfo, sheetIndex = 1)

data_human = data_human %>%
  dplyr::distinct()

data_human$id <- as.numeric(data_human$id)
full_data <- dplyr::inner_join(data_wind, data_human, by = c("id"))

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
out_data_L1 <- as.data.frame(out_data, row.names = yindex)
colnames(out_data_L1) <- xindex


fwrite(out_data_L1, paste(globalVar$outPath, "BWS/all.csv", sep = "/"), row.names = TRUE)

for (i in sex_full) {

  ## 전체 데이터 탐색 ##
  full_data_PARTR_ALL <- full_data_L1 %>%
    dplyr::filter(sex == i) %>%
    dplyr::group_by(sex) %>%
    dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
    dplyr::select(B_W01:BWSQRT13)

  Sys.sleep(3)

  out_data <- matrix(data = full_data_PARTR_ALL, nrow = 13, ncol = 3)
  out_data_L1 <- as.data.frame(out_data, row.names = yindex)
  colnames(out_data_L1) <- xindex

  fwrite(out_data_L1, paste(globalVar$outPath, paste0("BWS/sex_", i, ".csv"), sep = "/"), row.names = TRUE)
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
  out_data_L1 <- as.data.frame(out_data, row.names = yindex)
  colnames(out_data_L1) <- xindex

  fwrite(out_data_L1, paste(globalVar$outPath, paste0("BWS/age_", i, ".csv"), sep = "/"), row.names = TRUE)


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
  out_data_L1 <- as.data.frame(out_data, row.names = yindex)
  colnames(out_data_L1) <- xindex

  fwrite(out_data_L1, paste(globalVar$outPath, paste0("BWS/nfam_", i, ".csv"), sep = "/"), row.names = TRUE)

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
  out_data_L1 <- as.data.frame(out_data, row.names = yindex)
  colnames(out_data_L1) <- xindex

  fwrite(out_data_L1, paste(globalVar$outPath, paste0("BWS/edu_", i, ".csv"), sep = "/"), row.names = TRUE)
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
  out_data_L1 <- as.data.frame(out_data, row.names = yindex)
  colnames(out_data_L1) <- xindex

  fwrite(out_data_L1, paste(globalVar$outPath, paste0("BWS/income_", i, ".csv"), sep = "/"), row.names = TRUE)


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
  out_data_L1 <- as.data.frame(out_data, row.names = yindex)
  colnames(out_data_L1) <- xindex

  fwrite(out_data_L1, paste(globalVar$outPath, paste0("BWS/job_", i, ".csv"), sep = "/"), row.names = TRUE)
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
  out_data_L1 <- as.data.frame(out_data, row.names = yindex)
  colnames(out_data_L1) <- xindex

  fwrite(out_data_L1, paste(globalVar$outPath, paste0("BWS/marr_", i, ".csv"), sep = "/"), row.names = TRUE)
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
  tidyr::gather(-all, key = "key", val = "val")

dataL1_all$key = forcats::fct_relevel(dataL1_all$key, yindex)

ggplot(dataL1_all, aes(x = key, y = val, fill = key)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_text(aes(x = key, y = val, label = round(val, 2)), size = 5, color = "white", vjust = 2, position = position_dodge(width = 0.9)) +
  geom_hline(yintercept = 1) +
  labs(x = "", y = "선호도 점수", fill = "", subtitle = "[전체] 항목에 따른 선호도 점수") +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figPath, "BWS/full.png", sep = "/"), width = 12, height = 8, dpi = 600)


## 성별에 따른 항목별 스코어 차이 ##
full_data_PARTR_SEX <- full_data_L1 %>%
  dplyr::group_by(sex) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(sex, BWSQRT01:BWSQRT13) %>%
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
  tidyr::gather(-sex, key = "key", val = "val")

dataL1_sex$key = forcats::fct_relevel(dataL1_sex$key, yindex)

ggplot(dataL1_sex, aes(x = key, y = val, fill = sex)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_text(aes(x = key, y = val, label = round(val, 1)), size = 5, color = "white", vjust = 2, position = position_dodge(width = 0.9)) +
  geom_hline(yintercept = 1) +
  labs(x = "", y = "선호도 점수", fill = "", subtitle = "[성별] 항목에 따른 선호도 점수") +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figPath, "BWS/sex.png", sep = "/"), width = 12, height = 8, dpi = 600)


## 나이에 따른 항목별 스코어 차이 ##
full_data_PARTR_AGE <- full_data_L1 %>%
  dplyr::group_by(age) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(age, BWSQRT01:BWSQRT13) %>%
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
  tidyr::gather(-age, key = "key", val = "val")

dataL1_age$key = forcats::fct_relevel(dataL1_age$key, yindex)

ggplot(dataL1_age, aes(x = key, y = val, fill = age)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  # geom_text(aes(x=key, y=val, label = round(val, 1)), size = 5, color="white", vjust=2, position = position_dodge(width=0.9),size = 2) +
  geom_hline(yintercept = 1) +
  labs(x = "", y = "선호도 점수", fill = "", subtitle = "[나이] 항목에 따른 선호도 점수") +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figPath, "BWS/age.png", sep = "/"), width = 12, height = 8, dpi = 600)


## nfam에 따른 항목별 스코어 차이 ##
full_data_PARTR_nfam <- full_data_L1 %>%
  dplyr::group_by(nfam) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(nfam, BWSQRT01:BWSQRT13) %>%
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
  tidyr::gather(-nfam, key = "key", val = "val")

dataL1_nfam$key = forcats::fct_relevel(dataL1_nfam$key, yindex)

ggplot(dataL1_nfam, aes(x = key, y = val, fill = nfam)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_text(aes(x = key, y = val, label = round(val, 0)), size = 5, color = "white", vjust = 2, position = position_dodge(width = 0.9), size = 2) +
  geom_hline(yintercept = 1) +
  labs(x = "", y = "선호도 점수", fill = "", subtitle = "항목에 따른 선호도 점수") +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figPath, "BWS/nfam.png", sep = "/"), width = 12, height = 8, dpi = 600)


## edu에 따른 항목별 스코어 차이 ##
full_data_PARTR_edu <- full_data_L1 %>%
  dplyr::group_by(edu) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(edu, BWSQRT01:BWSQRT13) %>%
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
  tidyr::gather(-edu, key = "key", val = "val")

dataL1_edu$key = forcats::fct_relevel(dataL1_edu$key, yindex)

ggplot(dataL1_edu, aes(x = key, y = val, fill = edu)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_text(aes(x = key, y = val, label = round(val, 0)), size = 5, color = "white", vjust = 2, position = position_dodge(width = 0.9), size = 2) +
  geom_hline(yintercept = 1) +
  labs(x = "", y = "선호도 점수", fill = "", subtitle = "[교육] 항목에 따른 선호도 점수") +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figPath, "BWS/edu.png", sep = "/"), width = 12, height = 8, dpi = 600)


## income에 따른 항목별 스코어 차이 ##
full_data_PARTR_income <- full_data_L1 %>%
  dplyr::group_by(income) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(income, BWSQRT01:BWSQRT13) %>%
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
  tidyr::gather(-income, key = "key", val = "val")

dataL1_income$key = forcats::fct_relevel(dataL1_income$key, yindex)

ggplot(dataL1_income, aes(x = key, y = val, fill = income)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_text(aes(x = key, y = val, label = round(val, 0)), size = 5, color = "white", vjust = 2, position = position_dodge(width = 0.9), size = 2) +
  geom_hline(yintercept = 1) +
  labs(x = "", y = "선호도 점수", fill = "", subtitle = "[수입] 항목에 따른 선호도 점수") +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figPath, "BWS/income.png", sep = "/"), width = 12, height = 8, dpi = 600)


## job에 따른 항목별 스코어 차이 ##
full_data_PARTR_job <- full_data_L1 %>%
  dplyr::group_by(job) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(job, BWSQRT01:BWSQRT13) %>%
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
  tidyr::gather(-job, key = "key", val = "val")

dataL1_job$key = forcats::fct_relevel(dataL1_job$key, yindex)

ggplot(dataL1_job, aes(x = key, y = val, fill = job)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_text(aes(x = key, y = val, label = round(val, 0)), size = 5, color = "white", vjust = 2, position = position_dodge(width = 0.9), size = 2) +
  geom_hline(yintercept = 1) +
  labs(x = "", y = "선호도 점수", fill = "", subtitle = "[직업] 항목에 따른 선호도 점수") +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figPath, "BWS/job.png", sep = "/"), width = 12, height = 8, dpi = 600)


## marr에 따른 항목별 스코어 차이 ##
full_data_PARTR_marr <- full_data_L1 %>%
  dplyr::group_by(marr) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(marr, BWSQRT01:BWSQRT13) %>%
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
  tidyr::gather(-marr, key = "key", val = "val")

dataL1_marr$key = forcats::fct_relevel(dataL1_marr$key, yindex)

ggplot(dataL1_marr, aes(x = key, y = val, fill = marr)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  geom_text(aes(x = key, y = val, label = round(val, 0)), size = 5, color = "white", vjust = 2, position = position_dodge(width = 0.9), size = 2) +
  geom_hline(yintercept = 1) +
  labs(x = "", y = "선호도 점수", fill = "", subtitle = "항목에 따른 선호도 점수") +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figPath, "BWS/marr.png", sep = "/"), width = 12, height = 8, dpi = 600)


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

serviceName = "LSH0064"

# fileInfo = Sys.glob(paste(globalVar$inpPath, "kyung123.csv", sep = "/"))
# fileInfo = Sys.glob(paste(globalVar$inpPath, "kyung1_20201110.csv", sep = "/"))
fileList = Sys.glob(paste(globalVar$inpPath, "KYUNG/*.csv", sep = "/"))

# fileInfo = "D:/04. TalentPlatform/Github/TalentPlatform-R/INPUT/o2job/KYUNG/kyung1.csv"

#********************************************************
# 연관규칙 생성
#********************************************************
for (fileInfo in fileList) {

  fileName = tools::file_path_sans_ext(fs::path_file(fileInfo))

  data = readr::read_csv(fileInfo) %>%
    dplyr::select(-salad, -sandwich) %>%
    dplyr::rename(
      "id" = "X1"
      , "스테이크" = "steak"
      , "파스타" = "pasta"
      , "치즈" = "cheese"
      , "과일" = "fruit"
      , "피자" = "pizza"
      , "리조또" = "risotto"
      , "생선" = "fish"
      , "치킨" = "chiken"
      , "초콜릿" = "chocolate"
      , "한식" = "koreanfood"
      , "중식" = "chinesefood"
      , "일식" = "japanesefood"
      , "기타" = "theothers"
    )

  if (nrow(data) < 0) { next }

  # 연관 규칙
  foodlist = c()
  # i = 1
  for (i in unique(data$id)) {

    data_part = data %>%
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

  if (length(foodlist) < 0) { next }

  # 바이너리 코드를 transactions으로 변환하세요.
  trans = as(foodlist, "transactions")

  # 최소규칙 지지도 = 0.01, 최소규칙 신뢰도 = 0.01로 규칙
  rules = arules::apriori(trans, parameter = list(supp = 0.001, conf = 0.5))
  # rulesPlot = head(sort(rules, by = "lift"), 30)
  rulesPlot = head(sort(rules, by = "lift"), 10)

  # 시각화
  saveImg = sprintf("%s/Img_%s_%s.png", globalVar$figPath, serviceName, fileName)

  png(file = saveImg, width = 8, height = 5, units = "in", res = 600)
  plot(rulesPlot, method = "graph", control = list(type = "items"))
  dev.off()
}


#*********************************************************************************************
# 와인음용 병합하여 연관규칙 생성
#*********************************************************************************************
fileList = Sys.glob(paste(globalVar$inpPath, "KYUNG/*.csv", sep = "/"))

# fileInfo = "E:/04. TalentPlatform/Github/TalentPlatform-R/INPUT/o2job/KYUNG/kyung1234.csv"

for (fileInfo in fileList) {

  fileName = tools::file_path_sans_ext(fs::path_file(fileInfo))
  data = read.csv(fileInfo, header = TRUE, sep = ",", stringsAsFactors = FALSE)

  log4r::info(log, paste0("fileInfo : ", fileName))

  fileInfo2 = Sys.glob(paste(globalVar$inpPath, "와인음용별.xlsx", sep = "/"))
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
      dplyr::select(-c("level", "fre", "day", "drinkp"))


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


    # rules = apriori(transactions, parameter = list(supp=0.01, conf=0.5, minlen=2, maxlen=3))

    # Implementing Apriori Algorithm
    # rules = apriori(transactions, parameter = list(supp=0.01, conf=0.5, minlen=2, maxlen=3))
    rules = apriori(transactions, parameter = list(supp = 0.1, conf = 0.5))

    if (length(rules) == 0) { next }

    rules_dt = data.table(
      lhs = labels(lhs(rules))
      , rhs = labels(rhs(rules))
      , quality(rules)
    )[order(-lift),]


    write.csv(rules_dt, paste(globalVar$outPath, saveFile, sep = "/"))

    subrules2 = head(sort(rules, by = "confidence"), length(rules))
    ig = plot(subrules2, method = "graph", control = list(type = "items"))
    ig_df = get.data.frame(ig, what = "both")

    png(file = paste(globalVar$figPath, saveImg, sep = '/'), width = 8, height = 5, units = "in", res = 600)
    plot(subrules2, method = "graph", control = list(type = "items"))
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
      dplyr::select(-c("level", "fre", "day", "drinkp"))

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
    rules = apriori(transactions, parameter = list(supp = 0.1, conf = 0.5))

    if (length(rules) == 0) { next }

    rules_dt = data.table(
      lhs = labels(lhs(rules))
      , rhs = labels(rhs(rules))
      , quality(rules)
    )[order(-lift),]


    write.csv(rules_dt, paste(globalVar$outPath, saveFile, sep = "/"))

    subrules2 = head(sort(rules, by = "confidence"), length(rules))
    ig = plot(subrules2, method = "graph", control = list(type = "items"))
    ig_df = get.data.frame(ig, what = "both")

    png(file = paste(globalVar$figPath, saveImg, sep = '/'), width = 8, height = 5, units = "in", res = 600)
    plot(subrules2, method = "graph", control = list(type = "items"))
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
      dplyr::select(-c("level", "fre", "day", "drinkp"))


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
    rules = apriori(transactions, parameter = list(supp = 0.1, conf = 0.5))

    if (length(rules) == 0) { next }

    rules_dt = data.table(
      lhs = labels(lhs(rules))
      , rhs = labels(rhs(rules))
      , quality(rules)
    )[order(-lift),]


    write.csv(rules_dt, paste(globalVar$outPath, saveFile, sep = "/"))

    subrules2 = head(sort(rules, by = "confidence"), length(rules))
    ig = plot(subrules2, method = "graph", control = list(type = "items"))
    ig_df = get.data.frame(ig, what = "both")

    png(file = paste(globalVar$figPath, saveImg, sep = '/'), width = 8, height = 5, units = "in", res = 600)
    plot(subrules2, method = "graph", control = list(type = "items"))
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
      dplyr::select(-c("level", "fre", "day", "drinkp"))


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
    rules = apriori(transactions, parameter = list(supp = 0.1, conf = 0.5))

    if (length(rules) == 0) { next }

    rules_dt = data.table(
      lhs = labels(lhs(rules))
      , rhs = labels(rhs(rules))
      , quality(rules)
    )[order(-lift),]


    write.csv(rules_dt, paste(globalVar$outPath, saveFile, sep = "/"))

    subrules2 = head(sort(rules, by = "confidence"), length(rules))
    ig = plot(subrules2, method = "graph", control = list(type = "items"))
    ig_df = get.data.frame(ig, what = "both")

    png(file = paste(globalVar$figPath, saveImg, sep = '/'), width = 8, height = 5, units = "in", res = 600)
    plot(subrules2, method = "graph", control = list(type = "items"))
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

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0065"

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

fileInfo = Sys.glob(paste(globalVar$inpPath, "2015-2019 홍성의료원 기초조사_L1.xlsx", sep = "/"))

#==========================================
# 그래프 1
#==========================================
mapInfo = Sys.glob(paste(globalVar$mapPath, "/koreaInfo/TL_SCCO_CTPRVN.shp", sep = "/"))
la = shapefile(mapInfo, encoding = "UTF-8")

geo = sp::spTransform(la, CRS("+proj=longlat"))
geoData = ggplot2::fortify(geo, region = 'CTPRVN_CD', region2 = "CTP_KOR_NM")
# head(geoData)

# 위치 정보 읽기 (법정동 - 시도)
codeInfo = Sys.glob(paste(globalVar$mapPath, "/admCode/법정동코드_전체자료.txt", sep = "/"))
code = read.table(codeInfo, sep = "\t", header = TRUE, colClasses = "character")
colnames(code) = c("EMD_CD", "full_addr", "useflag")

code_L1 = code %>%
  tidyr::separate(col = "full_addr", into = c("d1", "d2", "d3", "d4", "d5"), sep = " ") %>%
  dplyr::filter(is.na(d3), is.na(d2)) %>%
  dplyr::mutate(code = str_sub(EMD_CD, 1, 2)) %>%
  dplyr::filter(useflag == "존재") %>%
  dplyr::select(-c(EMD_CD, d2, d3, d4, d5, useflag)) %>%
  dplyr::rename(
    "si_do" = "d1"
    , "sido_code" = "code"
  )

unique(code_L1$si_do)

# 대전 및 충남만 선택
code_L2_deaseon = code_L1 %>%
  dplyr::filter(si_do %in% c("충청남도", "충청북도", "대전광역시", "세종특별자치시")) %>%
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
          si_do == "충청남도" ~ dplyr::if_else(!is.na(n) & n > 0, paste0(round(n, 0), "\n충남"), "충남")
          , si_do == "세종특별자치시" ~ dplyr::if_else(!is.na(n) & n > 0, paste0(round(n, 0), "\n세종"), "세종")
          , si_do == "충청북도" ~ dplyr::if_else(!is.na(n) & n > 0, paste0(round(n, 0), "\n충북"), "충북")
          , si_do == "대전광역시" ~ dplyr::if_else(!is.na(n) & n > 0, paste0(round(n, 0), "\n대전"), "대전")
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
      geom_polygon(data = dataL2, aes(x = long, y = lat, group = group, fill = n)) +
      scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
      geom_path(data = dataL2, aes(x = long, y = lat, group = group), colour = 'black', size = 0.5) +
      ggh4x::stat_midpoint(data = dataL2, aes(x = long + xOffset, y = lat + yOffset, group = sido_code, label = plotLabel), geom = "text", size = 4) +
      #ggh4x::stat_midpoint(data=geoData_L1, aes(x=long, y=lat, group=sigungu_code, label = sigungu_name), geom = "text", size = 3) + # 시군구
      labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "단위 : 명", subtitle = NULL) +
      theme_minimal() +
      theme(
        text = element_text(size = 18)
        , panel.grid.major.x = element_blank()
        , panel.grid.major.y = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , axis.text.x = element_blank()
        , axis.ticks.x = element_blank()
        , axis.title.x = element_blank()
        , axis.text.y = element_blank()
        , axis.ticks.y = element_blank()
        , axis.title.y = element_blank()
        , plot.subtitle = element_text(hjust = 1)
        # , legend.position = "none"
      ) +
      ggsave(filename = paste(globalVar$figPath, saveImg, sep = "/"), width = 10, height = 5, dpi = 600)

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
            si_do == "충청남도" ~ dplyr::if_else(!is.na(n) & n > 0, paste0(round(n, 0), "\n충남"), "충남")
            , si_do == "세종특별자치시" ~ dplyr::if_else(!is.na(n) & n > 0, paste0(round(n, 0), "\n세종"), "세종")
            , si_do == "충청북도" ~ dplyr::if_else(!is.na(n) & n > 0, paste0(round(n, 0), "\n충북"), "충북")
            , si_do == "대전광역시" ~ dplyr::if_else(!is.na(n) & n > 0, paste0(round(n, 0), "\n대전"), "대전")
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
        geom_polygon(data = dataL2, aes(x = long, y = lat, group = group, fill = n)) +
        scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
        geom_path(data = dataL2, aes(x = long, y = lat, group = group), colour = 'black', size = 0.5) +
        ggh4x::stat_midpoint(data = dataL2, aes(x = long + xOffset, y = lat + yOffset, group = sido_code, label = plotLabel), geom = "text", size = 4) +
        #ggh4x::stat_midpoint(data=geoData_L1, aes(x=long, y=lat, group=sigungu_code, label = sigungu_name), geom = "text", size = 3) + # 시군구
        labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "단위 : 명", subtitle = NULL) +
        theme_minimal() +
        theme(
          text = element_text(size = 18)
          , panel.grid.major.x = element_blank()
          , panel.grid.major.y = element_blank()
          , panel.grid.minor.x = element_blank()
          , panel.grid.minor.y = element_blank()
          , axis.text.x = element_blank()
          , axis.ticks.x = element_blank()
          , axis.title.x = element_blank()
          , axis.text.y = element_blank()
          , axis.ticks.y = element_blank()
          , axis.title.y = element_blank()
          , plot.subtitle = element_text(hjust = 1)
          # , legend.position = "none"
        ) +
        ggsave(filename = paste(globalVar$figPath, saveImg, sep = "/"), width = 10, height = 5, dpi = 600)
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
        perVal = (val / sum(dataL2[ind,]$val, na.rm = TRUE)) * 100
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
        text = element_text(size = 18)
        # , panel.grid.major.x = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , plot.subtitle = element_text(hjust = 1)
      ) +
      ggsave(filename = paste(globalVar$figPath, saveImg, sep = "/"), width = 10, height = 6, dpi = 600)
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
        perVal = (val / sum(dataL2[ind,]$val, na.rm = TRUE)) * 100
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
        text = element_text(size = 18)
        # , panel.grid.major.x = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , plot.subtitle = element_text(hjust = 1)
      ) +
      ggsave(filename = paste(globalVar$figPath, saveImg, sep = "/"), width = 10, height = 6, dpi = 600)
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

      plotSubTitle = paste0("[", key2Info, "] ", key3Info, " 의료이용률 연령별?성별 현황(", yearInfo, ")")
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
            key == "남성" ~ -(val / sumData$val) * 100
            , key == "여성" ~ (val / sumData$val) * 100
            , TRUE ~ NA_real_
          )
          # , perValLabel =  sprintf("%.2f", abs(perVal))
          , perValLabel = sprintf("%.1f", abs(perVal))
          , nudgePerVal = dplyr::case_when(
            perVal > 0 ~ (perVal + 1.25)
            , perVal < 0 ~ (perVal - 1.25)
            , perVal == 0 & key == "남성" ~ -1.1
            , perVal == 0 & key == "여성" ~ 1.1
            , TRUE ~ NA_real_
          )
        )


      ggplot(dataL3, aes_string(x = "key4", y = "perVal", fill = "key")) +
        geom_bar(stat = "identity") +
        theme_bw() +
        labs(title = plotSubTitle, x = NULL, y = NULL, fill = NULL, colour = NULL, subtitle = "단위 : %") +
        theme(
          text = element_text(size = 18)
          # , panel.grid.major.x = element_blank()
          , panel.grid.minor.x = element_blank()
          , panel.grid.minor.y = element_blank()
          , plot.subtitle = element_text(hjust = 1)
          , legend.position = "none"
        ) +
        geom_text(aes_string(x = "key4", y = "nudgePerVal", label = "perValLabel"), size = 5, hjust = 0.5, vjust = 0.5) +
        coord_flip() +
        ggpol::facet_share(~key, dir = "h", scales = "free_x", reverse_num = TRUE) +
        scale_y_continuous(breaks = seq(-100, 100, 10), minor_breaks = NULL) +
        ggsave(filename = paste(globalVar$figPath, saveImg, sep = "/"), width = 10, height = 6, dpi = 600)

    }
  }
}


#==========================================
# 그래프 4 : 시군구별 현황
#==========================================
mapInfo = Sys.glob(paste(globalVar$mapPath, "/koreaInfo/TL_SCCO_SIG.shp", sep = "/"))
la = shapefile(mapInfo, encoding = "UTF-8")

geo = spTransform(la, CRS("+proj=longlat"))
head(geo)
geoData = ggplot2::fortify(geo, region = 'SIG_CD', region2 = "SIG_KOR_NM")
head(geoData)

codeInfo = Sys.glob(paste(globalVar$mapPath, "/admCode/법정동코드_전체자료.txt", sep = "/"))
code = read.table(codeInfo, sep = "\t", header = TRUE, colClasses = "character") %>%
  dplyr::rename(
    "EMD_CD" = "법정동코드"
    , "full_addr" = "법정동명"
    , "useflag" = "폐지여부"
  )

code_L1 = code %>%
  tidyr::separate(col = "full_addr", into = c("d1", "d2", "d3", "d4", "d5"), sep = " ") %>%
  dplyr::filter(is.na(d3), !is.na(d2)) %>%
  dplyr::mutate(code = str_sub(EMD_CD, 1, 5)) %>%
  dplyr::filter(useflag == "존재") %>%
  dplyr::select(-c(EMD_CD, d3, d4, d5, useflag)) %>%
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
  dplyr::mutate(code = as.character(round(as.numeric(sigungu_code, 0)))) %>%
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
      geom_polygon(data = dataL2, aes(x = long, y = lat, group = group, fill = n)) +
      scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
      geom_path(data = dataL2, aes(x = long, y = lat, group = group), colour = 'black', size = 0.5) +
      ggh4x::stat_midpoint(data = dataL2, aes(x = long + xOffset, y = lat + yOffset, group = sigungu_code, label = plotLabel), geom = "text", size = 4) +
      labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "단위 : 명", subtitle = NULL) +
      theme_minimal() +
      theme(
        text = element_text(size = 18)
        , panel.grid.major.x = element_blank()
        , panel.grid.major.y = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , axis.text.x = element_blank()
        , axis.ticks.x = element_blank()
        , axis.title.x = element_blank()
        , axis.text.y = element_blank()
        , axis.ticks.y = element_blank()
        , axis.title.y = element_blank()
        , plot.subtitle = element_text(hjust = 1)
        # , legend.position = "none"
      ) +
      ggsave(filename = paste(globalVar$figPath, saveImg, sep = "/"), width = 10, height = 5, dpi = 600)


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
        geom_polygon(data = dataL2, aes(x = long, y = lat, group = group, fill = n)) +
        scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
        geom_path(data = dataL2, aes(x = long, y = lat, group = group), colour = 'black', size = 0.5) +
        ggh4x::stat_midpoint(data = dataL2, aes(x = long + xOffset, y = lat + yOffset, group = sigungu_code, label = plotLabel), geom = "text", size = 4) +
        labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "단위 : 명", subtitle = NULL) +
        theme_minimal() +
        theme(
          text = element_text(size = 18)
          , panel.grid.major.x = element_blank()
          , panel.grid.major.y = element_blank()
          , panel.grid.minor.x = element_blank()
          , panel.grid.minor.y = element_blank()
          , axis.text.x = element_blank()
          , axis.ticks.x = element_blank()
          , axis.title.x = element_blank()
          , axis.text.y = element_blank()
          , axis.ticks.y = element_blank()
          , axis.title.y = element_blank()
          , plot.subtitle = element_text(hjust = 1)
          # , legend.position = "none"
        ) +
        ggsave(filename = paste(globalVar$figPath, saveImg, sep = "/"), width = 10, height = 5, dpi = 600)

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
        text = element_text(size = 18)
        # , panel.grid.major.x = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , plot.subtitle = element_text(hjust = 1)
        , legend.position = "none"
      ) +
      facet_wrap(~facetLabel) +
      ggsave(filename = paste(globalVar$figPath, saveImg, sep = "/"), width = 10, height = 6, dpi = 600)
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
          text = element_text(size = 18)
          # , panel.grid.major.x = element_blank()
          , panel.grid.minor.x = element_blank()
          , panel.grid.minor.y = element_blank()
          , plot.subtitle = element_text(hjust = 1)
          , legend.position = "none"
        ) +
        facet_wrap(~facetLabel) +
        ggsave(filename = paste(globalVar$figPath, saveImg, sep = "/"), width = 10, height = 6, dpi = 600)
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
            text = element_text(size = 18)
            # , panel.grid.major.x = element_blank()
            , panel.grid.minor.x = element_blank()
            , panel.grid.minor.y = element_blank()
            , plot.subtitle = element_text(hjust = 1)
            , legend.position = "none"
          ) +
          facet_wrap(~facetLabel) +
          ggsave(filename = paste(globalVar$figPath, saveImg, sep = "/"), width = 10, height = 6, dpi = 600)
      }
    }
  }
}


#==========================================
# 그래프 6 : 읍면동별 현황
#==========================================
# 읍면동 한반도 지도 읽기
mapInfo = Sys.glob(paste(globalVar$mapPath, "/koreaInfo/bnd_dong_00_2019_2019_2Q.shp", sep = "/"))
la = shapefile(mapInfo, encoding = "UTF-8")
geo = sp::spTransform(la, CRS("+proj=longlat"))

geoData = ggplot2::fortify(geo, region = 'adm_dr_cd', region2 = "adm_dr_nm")
head(geoData)


# 행정 코드 (행정동) 읽기
codeInfo = Sys.glob(paste(globalVar$mapPath, "/admCode/admCode.xlsx", sep = "/"))
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
    # saveImg = paste0("TMP2/Img_078_", "2015-2019", "_", key6Info, "_", key5Info, ".png")

    saveImg = sprintf("%s/%s_%s_%s_%s_%s.png", globalVar$figPath, serviceName, "078", "2015-2019", key6Info, key5Info)
    plotSubTitle = paste0("[", key6Info, "] ", key5Info, " 의료이용률 읍면동별 현황(", "2015-2019", ")")

    ggplot() +
      theme_bw() +
      coord_fixed(ratio = 1) +
      geom_polygon(data = dataL2, aes(x = long, y = lat, group = group, fill = n)) +
      scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
      geom_path(data = dataL2, aes(x = long, y = lat, group = group), colour = 'black', size = 0.1) +
      ggh4x::stat_midpoint(data = dataL2, aes(x = long + xOffset, y = lat + yOffset, group = emd_name, label = plotLabel), geom = "text", size = 2) +
      labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "단위 : 명", subtitle = NULL) +
      theme_minimal() +
      theme(
        text = element_text(size = 18)
        , panel.grid.major.x = element_blank()
        , panel.grid.major.y = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , axis.text.x = element_blank()
        , axis.ticks.x = element_blank()
        , axis.title.x = element_blank()
        , axis.text.y = element_blank()
        , axis.ticks.y = element_blank()
        , axis.title.y = element_blank()
        , plot.subtitle = element_text(hjust = 1)
        # , legend.position = "none"
      ) +
      ggsave(filename = saveImg, width = 10, height = 5, dpi = 600)
  }
}


for (key5Info in key5List) {
  for (key6Info in key6List) {
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
        geom_polygon(data = dataL2, aes(x = long, y = lat, group = group, fill = n)) +
        scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
        geom_path(data = dataL2, aes(x = long, y = lat, group = group), colour = 'black', size = 0.1) +
        ggh4x::stat_midpoint(data = dataL2, aes(x = long + xOffset, y = lat + yOffset, group = emd_name, label = plotLabel), geom = "text", size = 2) +
        labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "단위 : 명", subtitle = NULL) +
        theme_minimal() +
        theme(
          text = element_text(size = 18)
          , panel.grid.major.x = element_blank()
          , panel.grid.major.y = element_blank()
          , panel.grid.minor.x = element_blank()
          , panel.grid.minor.y = element_blank()
          , axis.text.x = element_blank()
          , axis.ticks.x = element_blank()
          , axis.title.x = element_blank()
          , axis.text.y = element_blank()
          , axis.ticks.y = element_blank()
          , axis.title.y = element_blank()
          , plot.subtitle = element_text(hjust = 1)
          # , legend.position = "none"
        ) +
        ggsave(filename = paste(globalVar$figPath, saveImg, sep = "/"), width = 10, height = 5, dpi = 600)

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
nSum = sum(liSeq**nP, na.rm = TRUE)


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
stat.desc(dfData2)[13,]

# 상자그림
boxplot(dfData2)

# 히스토그램
hist(dfData2)

#==========================================
# 문제사진8
#==========================================
dfData = read.csv(file = "./rpy/pima2.csv")
dfData2 = table(dfData$diabetes)

describe.by(dfData[, c(3:9)], dfData$diabetes)

# 막대그림
barplot(dfData2)

# 원그림
pie(dfData2)

# (3)
dfData3 = cut(dfData$age, breaks = c(20, 30, 40, 50, 99), labels = c("20-30", "31-40", "41-50", "50+"), include.lowest = FALSE, right = TRUE)

dfData4 = table(dfData3, dfData$diabetes)
barplot(t(dfData4), legend = c("neg", "pos"))

# (4)
dfData5 = cut(dfData$pregnant, breaks = c(0, 5, 10, 99), labels = c("0-5", "6-10", "10+"), include.lowest = TRUE, right = FALSE)

dfData6 = table(dfData5, dfData$diabetes)
barplot(t(dfData6), legend = c("neg", "pos"))

# (5)
dfData6 = data.frame(dfData, type = dfData5)

# 평균
aggregate(. ~ diabetes + type, data = dfData6, mean, na.rm = TRUE)

# 표준편차
aggregate(. ~ diabetes + type, data = dfData6, sd, na.rm = TRUE)


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
dfData2 = tidyr::gather(dfData[, c(2, 10)])

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

modelFit = lm(mpg ~ cyl +
  disp +
  hp +
  drat +
  wt +
  qsec +
  vs +
  am +
  gear +
  carb, data = dfData)

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
# ggsave(filename = paste(globalVar$figPath, "fTest.png", sep = "/"), width = 10, height = 6, dpi = 600)

# P값이 0.1666로서 귀무가설 기각하지 못함 (두 특성은 차이가 없다)
tTest = t.test(value ~ key, data = dataL2, var.equal = FALSE)
tTest

plot(tTest) +
  xlim(-5, 5) # +
# ggsave(filename = paste(globalVar$figPath, "tTest.png", sep = "/"), width = 10, height = 6, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(RSelenium)
library(rvest)
library(stringr)
library(XML)
library(tidyverse)
library(httr)
library(AER)
library(forcats)

#===============================================
# Question 1 (20점)
#===============================================
# 다음과 같은 두 문장을 네이버 파파고를 통해 번역하는 구문을 짜시오.
# string = “부장님 싫다. 직장생활 어렵다."

#=========================================
# 셀레늄 이용
#=========================================

# cd CONFIG/selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 5000

remDr = remoteDriver(
  remoteServerAddr = "localhost"
  , port = 500L
  , browserName = "chrome"
)

# 크롬 열기
remDr$open()


#=========================================
# 네이버 번역기
#=========================================
# remDr$navigate("https://www.naver.com") #통제하고있는 크롬사이트를 네이버로 이동
#
# webElem1 <- remDr$findElement(using = 'class',
#                               value = 'input_text') #ele1은 class중에서 inputtext찿ㅈ아서 가는거야
#
#
# #확인절차
#
# webElem1$highlightElement() #이렇게하면 번쩍이면서 옐로우 색으로 바뀜
# webElem1$clearElement()
#
# #확인햇으니 뭐를 넣어주자 =키
#
# webElem1$sendKeysToElement(list("네이버 맞춤법 검사기",
#                                 key = "enter"))
#
# string = "부장님 싫다. 직장생활 어렵다."
#
# sentence = list(string)
#
# webElem2 = remDr$findElement(using = 'class',
#                               value = 'txt_gray') #이 렇게 하면 textgray가 정의가 된것임(ㅋㄹ래스 옆에tx그레이있음)
#
#
# webElem2$highlightElement() #여기가 맞는지확인
# webElem2$clearElement()
#
#
# webElem2$sendKeysToElement(sentence)
#
# #한가지팁 검사하기 클릭을 해주기 위해서 2초정도 재워
# Sys.sleep(2)
#
#
# webElem3 <- remDr$findElement(using = 'class',
#                               value = 'btn_check')
#
# webElem3$highlightElement()
# webElem3$clickElement()
#
# Sys.sleep(2)
#
# webElem4 <- remDr$findElement(using = 'class',
#                               value = '_result_text')
# webElem4$highlightElement()
#
# webElem4$getElementText() -> result
#
# #("\\.") - 여러문장일경우 이건돈표시임
# result_clean <- stringr::str_split(result, "\\.")
#
# result_clean

#=========================================
# 파파고 번역기
#=========================================
remDr$navigate("https://papago.naver.com")

string = "부장님 싫다. 직장생활 어렵다."
sentence = list(string)

webElem2 = remDr$findElement(using = 'id', value = 'txtSource')
webElem2$sendKeysToElement(sentence)
# webElem2$highlightElement()

webElem3 = remDr$findElement(using = 'id', value = 'txtTarget')

# "I don't like the manager. Work is hard."
webElem3$getElementText()

#=========================================
# API 이용
#=========================================
# # API 아이디/비밀번호 신청
# # https://developers.naver.com/main/
#
# # 번역하려는 문장을 설정합니다.
# string = "부장님 싫다. 직장생활 어렵다."
#
# # 위 문장을 text 요청 변수에 추가하여 영문으로 번역합니다.
# result = httr::POST(
#   url = 'https://openapi.naver.com/v1/papago/n2mt'
#   , encode = 'json'
#   , body = list(
#     source = 'ko'
#     , target = 'en'
#     , text = string)
#   , config = add_headers('X-Naver-Client-Id' = globalVar$naverKeyId, 'X-Naver-Client-Secret' = globalVar$naverKeyPw)
# )
#
# # 응답 결과를 확인합니다.
# result
#
# # JSON 타입의 데이터를 추출합니다.
# convText2Json = httr::content(x = result)
#
# # json 객체의 구조를 파악합니다.
# convText2Json
#
# # 영문으로 번역된 결과만 출력
# # "I don't like the manager. Work is hard."
# convText2Json$message$result$translatedText

#===============================================
# Question 2 (30점)
#===============================================
# 다음 홈페이지에서 가서 IMF에서 보고한 1인당 GDP를 스크레이핑하고 1등에서 30분까지 순서대로 바 그래프를 그리시오.
# https://en.wikipedia.org/wiki/List_of_countries_by_past_and_projected_GDP_(nominal)_per_capita

data = data.frame()

# for (i in 1:4) {
#   tmpData = xml2::read_html('https://en.wikipedia.org/wiki/List_of_countries_by_past_and_projected_GDP_(PPP)') %>%
#     rvest::html_nodes(xpath = paste0('//*[@id="mw-content-text"]/div/table[', i, ']')) %>%
#     rvest::html_table() %>%
#     as.data.frame()
#
#   colnames(tmpData)[1] = c("name")
#
#   if (i == 1) {
#     data = tmpData
#   } else {
#      data = data %>% dplyr::left_join(tmpData, by = c("name" = "name"))
#   }
# }

Sys.setlocale("LC_ALL", "English")
#
#  tmpData = xml2::read_html('https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)') %>%
#   rvest::html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[3]/tbody/tr[2]/td[1]/table') %>%
#     rvest::html_table() %>%
#     as.data.frame()

tmpData = xml2::read_html('https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)_per_capita') %>%
  rvest::html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table/tbody/tr[2]/td[1]/table') %>%
  rvest::html_table() %>%
  as.data.frame()

data = tmpData %>%
  dplyr::filter(stringr::str_detect(Rank, "\\d")) %>%
  dplyr::rename(
    "name" = "Country.Territory"
  ) %>%
  dplyr::select(-Rank)

dataL1 = data %>%
  dplyr::mutate(
    name = stringr::str_remove_all(name, "\\[n 2\\]\\[n 3\\]")
    , name = stringr::str_remove_all(name, "\\[n 4\\]")
    , name2 = unlist(stringr::str_split(name, " ", simplify = TRUE))[, 1]
    , name3 = stringr::str_replace_all(name, " ", "-")
  ) %>%
  dplyr::na_if("") %>%
  na.omit() %>%
  tidyr::gather(-name, -name2, -name3, key = "key", value = "val") %>%
  dplyr::mutate(
    val2 = readr::parse_number(val)
  ) %>%
  dplyr::arrange(desc(val2)) %>%
  dplyr::top_n(30)

dplyr::tbl_df(dataL1)


dataL2 = dataL1
dataL2$name3 = as.factor(dataL2$name3)
dataL2$name3 = forcats::fct_relevel(dataL2$name3, rev(dataL1$name3))

ggplot(dataL2, aes(x = name3, y = val2, fill = name3)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip() +
  labs(x = "Countries", y = "GDP", fill = "", subtitle = "List of countries by past and projected GDP (PPP)") +
  theme(text = element_text(size = 10)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "none"
  ) +
  ggsave(filename = paste(globalVar$figPath, paste0("Img_LSH0072.png"), sep = "/"), width = 6, height = 15, dpi = 600)


Sys.setlocale("LC_ALL", "English")

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)_per_capita"

GDP <- read_html(url) %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

df <- GDP[[3]]

names(df) <- c(
  "Rank",
  "Country",
  "dollar"
)
df
names(df)


df$Rank <- as.numeric(df$Rank)
df
is.na(df)
na.rm = TRUE
df1 <- na.omit(df)
df1[c(1:30),]

tmpData = df1 %>%
  dplyr::rename(
    "Country.Territory" = "Country"
  )


# Question 3 (50점)
# AER" 패키지를 설치하고 로딩하시오. 그리고 “CPS1988" 데이터를 불러들이시오.
# (25점) 교육수익률을 추정하시오.
# (25점) 파트타임 결정에 미치는 모형을 구성하고, 추정하시오.
# (보너스: 20점) 위 파트타임 결정모형 추정에서 샘플의 50%(임의추출)를 추정에 사용하고,
# 추정된 모형과 나머지 50%의 샘플을 이용하여 파트타임 모형이 얼마나 잘 작동하는지 확인해보시오.
# 이의 추출시 sample()을 사용하면 됩니다.

data("CPS1988", package = "AER")

#================================================================
# (25점) 교육수익률을 추정하시오.
#================================================================
# lm(formula = log(CPS1988$wage) ~ CPS1988$experience + I(CPS1988$experience^2) + education + ethnicity, data = CPS1988)

dataL1 = CPS1988 %>%
  dplyr::mutate(
    lagWage = lag(wage)
    , per = (wage / lagWage) - 1.0
  ) %>%
  na.omit() %>%
  dplyr::select(-lagWage) %>%
  dplyr::select(-ethnicity, -smsa, -region, -parttime)

lmFit = lm(per ~ ., data = dataL1)

# 기본값으로 변수 선택
# summary(lmFit)

# AIC 기준으로 변수 선택
stepAic = MASS::stepAIC(lmFit, direction = "both")

# 결과에 대한 요약
summary(stepAic)


plot(dataL1$per, predict(stepAic))
cor(dataL1$per, predict(stepAic))

#================================================================
# (25점) 파트타임 결정에 미치는 모형을 구성하고, 추정하시오.
#================================================================
dataL1 = CPS1988 %>%
  na.omit()

glmFit = glm(parttime ~ ., data = dataL1, family = binomial)

# 회귀모형에 대한 요약 결과
summary(glmFit)

# 실제 파트타임
yObs = as.numeric(dataL1$parttime)

# 테스트셋을 이용한 예측 파트타임
yHat = predict.glm(glmFit, newdata = dataL1, type = "response")
# yHat =ifelse(yHatPred > 0.5, 1, 2)

# 검증 측정을 위한 기초 설정
lmPred = ROCR::prediction(yHat, yObs)

# ROC 커브를 위한 설정
perform = ROCR::performance(lmPred, "tpr", "fpr")
plot(perform, main = 'ROC Curve')

# AUC 측정 : 1에 가까울수록 최고 성능 : 0.9
ROCR::performance(lmPred, "auc")@y.values[[1]]

#==================================================================================
# (보너스: 20점) 위 파트타임 결정모형 추정에서 샘플의 50%(임의추출)를 추정에 사용하고,
# 추정된 모형과 나머지 50%의 샘플을 이용하여 파트타임 모형이 얼마나 잘 작동하는지 확인해보시오.
# 이의 추출시 sample()을 사용하면 됩니다.
#=================================================================================
dataL1 = CPS1988 %>%
  na.omit()

#=====================================================================
# 훈련 및 테스트 셋 설정 (50 : 50)
#=====================================================================
# 훈련 및 데이터 셋을 50:50으로 나누기 위한 인덱스 설정
ind = sample(1:nrow(dataL1), nrow(dataL1) * 0.5)

# 해당 인덱스에 따라 자료 할당
trainData = dataL1[ind,]
testData = dataL1[-ind,]

# 훈련 데이터셋 확인
dplyr::tbl_df(trainData)

# 테스트 데이터셋 확인
dplyr::tbl_df(testData)

#==========================================
# 훈련 데이터셋으로 학습 모형
#==========================================
glmFit = glm(parttime ~ ., data = trainData, family = binomial)

# 회귀모형에 대한 요약 결과
summary(glmFit)

#==========================================
# 테스트 데이터셋으로 검증
#==========================================
# 실제 퇴직여부
yObs = as.numeric(testData$parttime)

# 테스트셋을 이용한 예측 퇴직여부
yHat = predict.glm(glmFit, newdata = testData, type = "response")
# yHat =ifelse(yHatPred > 0.5, 1, 2)

# 검증 측정을 위한 기초 설정
lmPred = ROCR::prediction(yHat, yObs)

# ROC 커브를 위한 설정
perform = ROCR::performance(lmPred, "tpr", "fpr")
plot(perform, main = 'ROC Curve')

# AUC 측정 : 1에 가까울수록 최고 성능 : 0.89
ROCR::performance(lmPred, "auc")@y.values[[1]]


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(tidyverse)

# 1. covid19_exp.csv 파일을 이용할 때, t-test를 통해 covid positive와 negative group에서 통계적으로 가장 유의하게 차이가 나는(statistically significant) 유전자가 무엇인지 찾아보시오

# R에서 covid19_exp.csv 파일 불러오기위한 코드
data1 = read.csv('https://www.dropbox.com/s/e1l7xyvm54x9fbb/covid19_exp.csv?dl=1', stringsAsFactors = FALSE)

# R에서 covid19_clinical.csv 파일 불러오기위한 코드
data2 = read.csv('https://www.dropbox.com/s/z9te6mbuvpc0ysd/covid19_clinical.csv?dl=1', stringsAsFactors = FALSE)


data = data1 %>%
  dplyr::left_join(data2, by = c("sample" = "sample"))


colList = colnames(data1 %>% select(-sample))

dataL2 = data.frame()
for (colInfo in colList) {

  dataL1 = data %>%
    dplyr::select(val = colInfo, key = covid_status)

  tTest = t.test(dataL1[dataL1$key == "positive",]$val, dataL1[dataL1$key == "negative",]$val)

  tmpData = tibble(
    "colInfo" = colInfo
    , "tVal" = tTest$statistic
    , "pVal" = tTest$p.value
  )

  dataL2 = dplyr::bind_rows(dataL2, tmpData)
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

library(RSelenium)
library(rvest)
library(stringr)
library(XML)
library(tidyverse)
library(httr)
library(AER)
library(forcats)
library(openxlsx)
library(xml2)
library(stringi)
library(RCurl)

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

getUrlXpathText = function(url, xpath) {
  xml2::read_html(url) %>%
    rvest::html_nodes(xpath = xpath) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

# 고혈압(I10-15) 외래
fileInfo = Sys.glob(paste(globalVar$inpPath, "/홍성의료원/2015-2019 충남대학병원 기초조사.xlsx", sep = "/"))

data = openxlsx::read.xlsx(xlsxFile = fileInfo, sheet = "Sheet1")

# 10000개 선택
dataL1 = data %>%
  tibble::rowid_to_column("rowId") %>%
  readr::type_convert() %>%
  dplyr::filter(addr != 1) %>%
  head(10000)

dataL2 = dataL1 %>%
  dplyr::mutate(
    addrEnc = RCurl::curlEscape(stringr::str_conv(addr, encoding = "UTF-8"))
    , urlInfo = paste0("https://www.juso.go.kr/support/AddressMainSearch.do?searchKeyword=", addrEnc, "&dsgubuntext=&dscity1text=&dscounty1text=&dsemd1text=&dsri1text=&dssan1text=&dsrd_nm1text=")
  )

log4r::info(log, paste0("[START] Main : ", "주소 변환"))

urlList = dataL2 %>%
  dplyr::distinct(urlInfo) %>%
  as.tibble()

dataRes = tibble()

for (i in 1:nrow(urlList)) {
  urlInfo2 = urlList[i, "urlInfo"]

  val = urlInfo2 %>%
    purrr::map(~getUrlXpathText(.x, '//*[@id="container"]/div/span')) %>%
    unlist()

  if (length(val) < 1) {
    val = "NA"
  }

  log4r::info(log, paste0("진행률 : ", round((i / nrow(urlList)) * 100, 2), " %"))

  tmpData = data.frame(
    "urlInfo" = urlInfo2
    , "val" = val
  )

  dataRes = dplyr::bind_rows(dataRes, tmpData)
}

log4r::info(log, paste0("[END] Main : ", "주소 변환"))


dataL3 = dataL2 %>%
  dplyr::left_join(dataRes, by = c("urlInfo" = "urlInfo"))

dataL4 = dataL3 %>%
  dplyr::filter(
    # ! val %in% c("0", "NA")
    val %in% c("0", "NA")
  )

openxlsx::write.xlsx(dataL3, file = paste(globalVar$outPath, "ConvAddr.xlsx", sep = "/"))


# INFO  [2020-12-05 23:21:41] [START] Main : 주소 변환
# INFO  [2020-12-06 00:09:50] [END] Main : 주소 변환
# 50분
round((nrow(dataL3) / 10000) * 100, 2)

# port = 5000
# runCommand = paste0("java -Dwebdriver.chrome.driver='", globalVar$seleniumConfig, "/chromedriver.exe' -jar '", globalVar$seleniumConfig, "/selenium-server-standalone-3.141.59.jar' ", "-port ", port)
# rstudioapi::terminalExecute(runCommand)

# remDr = remoteDriver(
#   remoteServerAddr = "localhost"
#   , port = port
#   , browserName = "chrome"
# )

# 크롬 열기
# remDr$open()

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
library(ggpubr)
library(ggstatsplot)

#=============================================================================
# 에디터 초이스가 true이면 인스톨이 많을것이다
#=============================================================================
fileInfo = Sys.glob(paste(globalVar$inpPath, "Google-Playstore.csv", sep = "/"))
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "UTF-8"))

dplyr::glimpse(data)

dataL1 = data %>%
  dplyr::mutate(value = readr::parse_number(Installs)) %>%
  dplyr::rename(
    "key" = "Editors Choice"
  ) %>%
  na.omit()

dplyr::glimpse(dataL1)

# F 테스트 및 T 테스트
dataL2 = dataL1 %>%
  dplyr::select(key, value) %>%
  dplyr::mutate_at(vars(key), funs(as.factor))

# P값이 2.2204e-16으로서 귀무가설 기각 (두 특성의 분산 차이가 있다)
# 따라서 상이한 분산 조건 (var.equal = FALSE)
fTest = var.test(value ~ key, data = dataL2, conf.level = 0.95)
fTest

# P값이 4.232928e-05로서 에디터 선택이 TRUE일 경우 평균적으로 41,756,039건으로 설치가 많다.
tTest = t.test(value ~ key, data = dataL2, var.equal = FALSE)
tTest


#=============================================================================
# 게임 카테고리가 true이면 인스톨이 많을것이다
#=============================================================================
fileInfo = Sys.glob(paste(globalVar$inpPath, "play.csv", sep = "/"))
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "UTF-8"))

dataL1 = data %>%
  dplyr::mutate(value = readr::parse_number(Installs)) %>%
  na.omit()

dataL2 = dataL1 %>%
  dplyr::select(Category, value) %>%
  dplyr::mutate(
    key = dplyr::case_when(
      stringr::str_detect(Category, "Strategy|Adventure|Puzzle|Casual|Trivia|Word|Racing|Board|Arcade|Role Playing|Action|Simulation|Video Players & Editors|Casino") ~ "게임"
      , stringr::str_detect(Category, "Education") ~ "교육"
      , stringr::str_detect(Category, "Communication|Dating|Social") ~ "소셜미디어"
      , stringr::str_detect(Category, "Books & Reference|Health & Fitness|Sports") ~ "자기계발"
      , stringr::str_detect(Category, "Maps & Navigation|Tools|Productivity|Food & Drink|Medical|Finance|Business|Personalization|Lifestyle|Shopping|Weather|News & Magazines") ~ "유틸리티"
      , stringr::str_detect(Category, "Music|Music & Audio|Art & Design|House & Home|Photography|Comics|Entertainment|Travel & Local") ~ "취미"
      , TRUE ~ "기타"
    )
  ) %>%
  dplyr::mutate_at(vars(key), funs(as.factor))

dplyr::glimpse(dataL2)

# aovRes에서 P-value는 2.22e-16로서 0.05보다 작기 떄문에 게임 카테고리에 따른 설치건수가 차이가 있다.
aovRes = aov(value ~ key, data = dataL2)
summary(aovRes)

# 타 특성에 비해 게임은 p-value가 0.001 이하로서 통계적으로 유의하다.
tukeyRes = TukeyHSD(aovRes)

tukeyResL1 = tukeyRes$key %>%
  as.data.frame() %>%
  dplyr::arrange("diff", "p adj")

# 예시 그림
plot(tukeyRes)

# 예시 그림
tky = tukeyResL1
tky$pair = rownames(tky)

# Plot pairwise TukeyHSD comparisons and color by significance level
ggplot(tky, aes(colour = cut(`p adj`, c(0, 0.01, 0.05, 1),
                             label = c("p < 0.01", "p < 0.05", "Non-Sig")))) +
  geom_hline(yintercept = 0, lty = "11", colour = "grey30") +
  geom_errorbar(aes(pair, ymin = lwr, ymax = upr), width = 0.2) +
  geom_point(aes(pair, diff)) +
  labs(colour = "") +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

library(ggpubr)
library(ggstatsplot)
library(exactRankTests)
library(BSDA)

# 1. 다음 검증들의 귀무분포를 구하는 코드를 작성하여라.
#===============================================
# Singed-rank test (n=16)
#===============================================
set.seed(1)
sampeList = sample(125:150, 16, replace = TRUE)

set.seed(2)
sampeList2 = sample(140:150, 16, replace = TRUE)

data = data.frame(
  key = c(rep("A", 16), rep("B", 16))
  , val = c(sampeList, sampeList2)
)

boxplot(val ~ key, data = data)

# V = 13.5, p-value = 0.0155513
# 유의수준 0.0155513로서 귀무가설 기각 (즉 두 그룹의 특성은 다르다)
wilcoxTest = wilcox.test(sampeList, sampeList2, paired = TRUE)
wilcoxTest

#===============================================
# Wilcoxon Rank Sum Test (m=12, n=8)
#===============================================
set.seed(1)
sampeList = sample(rnorm(8, 12, 1), 8, replace = TRUE)

set.seed(2)
sampeList2 = sample(rnorm(8, 12, 1), 8, replace = TRUE)

data = data.frame(
  key = c(rep("A", 8), rep("B", 8))
  , val = c(sampeList, sampeList2)
)

boxplot(val ~ key, data = data)

# W = 24, p-value = 0.4281671
# 유의수준 0.4281671로서 귀무가설 기각 못함 (즉 두 그룹의 특성은 같다)
wilcoxTest = wilcox.test(val ~ key, data = data)
wilcoxTest

#===============================================
# Kruskal - Wallis Test (n1=3, n2=3, n3=6)
#===============================================
set.seed(1)
sampeList = sample(125:140, 3, replace = TRUE)

set.seed(2)
sampeList2 = sample(135:145, 3, replace = TRUE)

set.seed(3)
sampeList3 = sample(135:150, 6, replace = TRUE)

data = data.frame(
  key = c(rep("A", 3), rep("B", 3), rep("C", 6))
  , val = c(sampeList, sampeList2, sampeList3)
)

boxplot(val ~ key, data = data)

# Kruskal-Wallis chi-squared = 6.7729682, df = 2, p-value = 0.0338274
# 유의수준 0.0338274로서 귀무가설 기각 못함 (즉 세 그룹의 특성은 같다)
kruskalTest = kruskal.test(val ~ key, data = data)
kruskalTest


# 2. 문제풀이에 대한 코드 (wilcox.test 등이 아닌 코드 구현)를 작성한 후 각 데이터에 실시하라.
# 코드의 output으로 검정통계량과 p-value 두 값만 나오도록 합니다 (3번 제외)
# 코드에 동점처리 포함해야 합니다.
# 가설, 검정통계량, p-value, 결과 해석 명시

#===================================================================================================
# 부호순위검정을 이용하여 알파=0.05에서 평균의 값이 160보다 큰가에 대하여 검정하여라 (단측 검정).
#===================================================================================================
data = c(176.9, 158.3, 152.1, 158.8, 172.4, 169.8, 159.7, 162.7, 156.6, 174.5, 184.4, 165.2, 147.6, 177.8, 160.0, 160.5)

# V = 87.5, p-value = 0.06253867
# R 내장 함수를 통해 구현 결과
# tie가 있어 정확한 p값을 계산할 수 없습니다
wilcox.test(data, mu = 160, alternative = "greater", conf.int = TRUE)

# 정확한 p값 계산을 위한 타 라이브러리 사용
# s = 9, p-value = 0.3036194
# BSDA::SIGN.test(data, md = 160, alternative = "greater", conf.level = 0.95)

mu = 160
n = 16
dataL1 = (mu - data)

obsRank = rank(dataL1, ties.method = "average")
ind = which(dataL1 > 0)
obsSum = sum(obsRank[ind], na.rm = TRUE)

obsN = (n * (n + 1)) / 4
obsSd = sqrt(((n * (n + 1)) * (2 * n + 1)) / 24)
obsStat = (obsSum - obsN) / obsSd

pVal = pnorm(obsStat, lower.tail = FALSE)

output = data.frame(
  "V" = obsSum
  , "p-value" = pVal
)

# 유의수준 0.25로서 신뢰구간 95%에 대해 귀무가설 기각 못함 (즉 두 그룹의 특성은 같다)
output

#===================================================================================================
# 다음은 A 집단과 B 집단의 점수를 나타내는 데이터다. 월콬슨 순위합검정을 이용하여 알파=0.05에서 두 집단 사이에 점수의 차이가 있는지 검정하여라.
#===================================================================================================
data = data.frame(
  key = c(rep("A", 12), rep("B", 8))
  , val = c(70, 80, 72, 76, 76, 76, 72, 78, 82, 92, 68, 84, 68, 72, 62, 70, 66, 68, 52, 64)
)

boxplot(val ~ key, data = data)

# # W = 90.5, p-value = 0.00112865
# wilcoxTest = wilcox.test(val ~ key, data = data)
# wilcoxTest

n1 = 12
n2 = 8
n = n1 + n2

obsRank = rank(data$val, ties.method = "average")

W1 = sum(obsRank[1:n1], na.rm = TRUE) - (n1 * (n1 + 1) / 2)
W2 = sum(obsRank[n1 + 1:n2], na.rm = TRUE) - (n2 * (n2 + 1) / 2)
W = max(W1, W2)

obsTable = table(obsRank)
sum(obsTable^3 - obsTable)

VW = n1 * n2 / 12 / (n^2 - n) * (n^3 -
  n -
  sum(obsTable^3 - obsTable, na.rm = TRUE))
EW = n1 * n2 / 2
Z = (W - EW) / sqrt(VW)
Pval = 2 * pnorm(Z, lower.tail = FALSE)

output = data.frame(
  "W" = W
  , "p-value" = Pval
)

# P값이 0.001 이하로서 귀무가설 기각 (두 그룹은 차이가 있다)
output


#===================================================================================================
# 크러스칼-윌리스 검정을 이용하여 알파=0.05에서 세 그룹 사이에 차이가 있다고 할 수 있는지 검정하여라 (코드 output으로는 검정통계량만 보이고 p-value는 안 보여도 됩니다).
#===================================================================================================
data = data.frame(
  key = c(rep("A", 3), rep("B", 3), rep("C", 6))
  , val = c(56, 60, 60, 48, 57, 53, 56, 60, 63, 54, 58, 52)
)

boxplot(val ~ key, data = data)

# Kruskal-Wallis chi-squared = 2.9065836, df = 2, p-value = 0.2337994
# kruskalTest = kruskal.test(val ~ key, data = data)
# kruskalTest

obsRank = rank(data$val, ties.method = "average")
anovaRes = anova(lm(obsRank ~ key, data = data))

chiSquared = anovaRes$`Sum Sq`[1] / var(obsRank, na.rm = TRUE)
dfVal = anovaRes$Df[1]
pVal = pchisq(chiSquared, df = dfVal, lower.tail = FALSE)

output = data.frame(
  "chi-squared" = chiSquared
  , "dfVal" = dfVal
  , "pVal" = pVal
)


# P값이 0.233로서 귀무가설 기각하지 못함 (세 그룹은 차이가 없다)
output

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
library(ggcorrplot)

# 타임스탬프
# 1. 귀하의 성별( Gender)을 알려 주세요.
# 2. 귀하의 나이(age) 는 어디에 속합니까?
# 3. 귀하의 학력은 어디에 속합니까?
# 4. 귀하의 직업은 어디에 속합니까?

# 이 논문에서 표 2와 같이 그리면 될까요?
# 7페이지의 4개 경우 별로 종속변수로 선정
# 독립변수 : 기본정보 (4개), 모임(예술) (6개), 취향 (2개), 삶의 의미 (6개)18
# 종속변수 : 1개
#
# 단, 취향관련 질문의 경우 예술적 취향 선호도 (음악, 독서 등)을 점수화 (1-6점)로 변경할 수 있을까요?

#====================
# 컬럼 정보
#====================
# 타임스탬프
#
# [기본정보]
# 1. 귀하의 성별( Gender)을 알려 주세요.
# 2. 귀하의 나이(age) 는 어디에 속합니까?
# 3. 귀하의 학력은 어디에 속합니까?
# 4. 귀하의 직업은 어디에 속합니까?

# 모임과 취향(인생의 행복과 의미)에 관한 설문
# [모임] 예술
# 귀하는 예술을 좋아하십니까?	만약 모임을 즐기는 분이시라면 한달에 몇 번 정도 모임을 하시나요 (기억나는 횟수에 표시)
# 귀하께서 참여하시는 특정 예술(음악, 독서, 미술, 댄스 등등) 모임이 있다면 한달에 몇번인지 숫자를 클릭 해 주세요. (없는 분은 1번을 선택해 주세요)
# 귀하는 예술과 관련된 소모임이 귀하의 인생에 긍정적인 영향을 끼친다고 생각 하시나요?
# 귀하께서 믿고 좋아하는 지인이 특정 모임을 권유한다면 그 모임에 나갈 의향이 있으신가요?
# 귀하께서는 특정 소모임에서 불쾌했던 경험을 하신적이 있었나요?

# [취향]
# 귀하께서는 어떤 예술적 취향을 선호 하시나요?
# 귀하께서는 같은 취향을 가지신 분들과 특정 공간에서 만나는 것을 어떻게 생각 하시는가요?
# 귀하께서는 예술이 자신의 삶에 어느 정도 영향을 끼친다고 생각 하시나요?
#
# [삶의 의미]
# 1. 내 인생에는 충분한 목적이 없다. (*목적이 없을경우 1번 부터 시작함)
# 2. 나에게 있어, 내가 하는 일은 가치 있는 것이다.
# 3. 내가 하는 일의 대부분은 나에게 하찮은 것 같고 중요하지 않은 것 같다.
# 4. 나는 나의 활동을 매우 소중히 여긴다.
# 5. 나는 내가 하는 일에 별로 신경 쓰지 않는다.
# 6. 나는 살아가는 많은 이유가 있다.

# [종속 변수]
# 1. 살롱문화란 말을 접해 보거나 경험해 본 적이 있나요? (살롱이란 말을 들어 본적 있나요?)
# 2. 귀하께서 살롱문화가 무엇인지 알고 경험하신적이 있다면 그것이 긍정적인 기분이었는지 부정적 기분이었는지 궁금합니다.
# 3. 내 인생에 나쁜 영향을 줄 수 있는 문제(예:코로나)가 생기더라도 좋아하는 소모임을 계속 할 의향이 있으신가요? (2019 년을 중심으로, 코로나 이전 상황에 입각하여 대답해주세요)
# 4. 내 인생에 나쁜 영향을 줄 수 있는 문제(예:코로나)가 생기더라도 좋아하는 소모임을 계속 할 의향이 있으신가요? (2020년 현재를 중심으로, 코로나 상황에 입각하여 대답해주세요)

fileInfo = Sys.glob(paste0(globalVar$inpPath, "/모임과_취향(인생의_행복과_의미)에_관한_설문의_설문지_응답_시트2.csv"))

data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "UTF-8"))

dataL1 = data %>%
  dplyr::mutate(
    key3Conv = dplyr::case_when(
      stringr::str_detect(key3, "석사, 박사 이상") ~ "박사 이상"
      , TRUE ~ key3
    )
    , key4Conv = dplyr::case_when(
      stringr::str_detect(key4, "예술관련 종사자") ~ "예술가"
      , stringr::str_detect(key4, "전문직") ~ "전문직"
      , stringr::str_detect(key4, "회사원 ") ~ "회사원"
      , stringr::str_detect(key4, "언론계 종사자") ~ "언론계"
      , stringr::str_detect(key4, "건축업자") ~ "건축업자"
      , stringr::str_detect(key4, "자영업자") ~ "자영업자"
      , stringr::str_detect(key4, "전업 주부") ~ "전업주부"
      , TRUE ~ "기타"
    )
  )


# 성별, 연령, 학력, 직업
ggData1 = dataL1 %>%
  dplyr::group_by(key = key1) %>%
  dplyr::summarise(cnt = n()) %>%
  dplyr::mutate(
    ratio = round((cnt / sum(cnt, na.rm = TRUE)) * 100, 2)
    , label = paste0(cnt, " (", ratio, ")")
    , type = "성별"
  )

ggData2 = dataL1 %>%
  dplyr::group_by(key = key2) %>%
  dplyr::summarise(cnt = n()) %>%
  dplyr::mutate(
    ratio = round((cnt / sum(cnt, na.rm = TRUE)) * 100, 2)
    , label = paste0(cnt, " (", ratio, ")")
    , type = "연령"
  )

ggData3 = dataL1 %>%
  dplyr::group_by(key = key3Conv) %>%
  dplyr::summarise(cnt = n()) %>%
  dplyr::mutate(
    ratio = round((cnt / sum(cnt, na.rm = TRUE)) * 100, 2)
    , label = paste0(cnt, " (", ratio, ")")
    , type = "학력"
  )

ggData4 = dataL1 %>%
  dplyr::group_by(key = key4Conv) %>%
  dplyr::summarise(cnt = n()) %>%
  dplyr::mutate(
    ratio = round((cnt / sum(cnt, na.rm = TRUE)) * 100, 2)
    , label = paste0(cnt, " (", ratio, ")")
    , type = "직업"
  )

ggData = dplyr::bind_rows(ggData1, ggData2, ggData3, ggData4)

# type 정렬
ggData$type = forcats::fct_relevel(ggData$type, c("성별", "나이", "학력", "직업"))

# key 정렬
ggData$key = forcats::fct_relevel(ggData$key, c("남자", "여자", "20대 (10대포함)", "30대", "40대", "50대", "60대 이상", "고졸(중졸포함)", "전문대 졸", "대졸", "석사", "박사 이상", "건축업자", "언론계", "예술가", "자영업자", "전문직", "전업주부", "회사원", "기타"))

ggplot(ggData, aes(x = key, y = ratio, fill = type, label = round(ratio, 1))) +
  # ggplot(ggData, aes(x = key, y = ratio, label = round(ratio, 1))) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.position = "top") +
  geom_text(aes(group = type), position = position_dodge(width = 0.9), size = 5, vjust = -0.5, hjust = 0.5) +
  ylim(0, 100) +
  facet_wrap(~type, scale = "free") +
  labs(x = "구분", y = "특성 [%]", fill = "", subtitle = "대상자 특성") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = paste(globalVar$figPath, "Img_LSH0078_1.png", sep = "/"), width = 13, height = 8, dpi = 600)


dataL2 = dataL1 %>%
  dplyr::select(dplyr::contains(c("key", "ref"))) %>%
  # dplyr::select(-key3, -key4, -key5)

  # key 정렬
  dataL2$key1 = forcats::fct_relevel(dataL2$key1, c("남자", "여자"))
dataL2$key2 = forcats::fct_relevel(dataL2$key2, c("20대 (10대포함)", "30대", "40대", "50대", "60대 이상"))
dataL2$key3Conv = forcats::fct_relevel(dataL2$key3Conv, c("고졸(중졸포함)", "전문대 졸", "대졸", "석사", "박사 이상"))
dataL2$key4Conv = forcats::fct_relevel(dataL2$key4Conv, c("건축업자", "언론계", "예술가", "자영업자", "전문직", "전업주부", "회사원", "기타"))

keyList = c("key1", "key2", "key3Conv", "key4Conv")
colList = dataL2 %>%
  dplyr::select(dplyr::contains(c("ref"))) %>%
  colnames()

dataL3 = data.frame()
dataL4 = data.frame()

for (i in 1:length(keyList)) {
  for (j in 1:length(colList)) {

    rsAov = aov(get(colList[j], dataL2) ~ get(keyList[i], dataL2), data = dataL2) %>%
      summary %>%
      unlist()

    tmpData = tibble(
      # typeName = typeNameList[i]
      type = keyList[i]
      , col = colList[j]
      # , colName = colNameList[j]
      , fVal = rsAov[7] %>% round(2)
      , pVal = rsAov[9] %>% round(2)
    ) %>%
      dplyr::mutate(label = paste0(fVal, " (", pVal, ")"))

    dataL3 = dplyr::bind_rows(dataL3, tmpData)
  }

  tmpData2 = dataL2 %>%
    dplyr::group_by(type = get(keyList[i], dataL2)) %>%
    # dplyr::select(!dplyr::contains("type")) %>%
    # dplyr::select(dplyr::contains(c("Q34", "Q57", "Q64"))) %>%
    dplyr::select(dplyr::contains(c("ref"))) %>%
    dplyr::summarise_all(funs(
      mean(., na.rm = TRUE) %>% round(2) # 평균
      , sd(., na.rm = TRUE) %>% round(2) # 표준편차
      # , n() # 자료 개수
    ))

  dataL4 = dplyr::bind_rows(dataL4, tmpData2)
}


wb = openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "ggData")
openxlsx::writeData(wb, "ggData", ggData, startRow = 1, startCol = 1)

openxlsx::addWorksheet(wb, "dataL3")
openxlsx::writeData(wb, "dataL3", dataL3, startRow = 1, startCol = 1)

openxlsx::addWorksheet(wb, "dataL4")
openxlsx::writeData(wb, "dataL4", dataL4, startRow = 1, startCol = 1)

openxlsx::saveWorkbook(wb, file = paste0(globalVar$outPath, "/Survery_LSH0078.xlsx"), overwrite = TRUE)


#==================================
# 상관계수 행렬
#==================================
dataL5 = dataL1 %>%
  dplyr::select(dplyr::contains(c("ref"))) %>%
  dplyr::rename(
    "살롱문화 경험" = "ref1"
    , "긍정 수치" = "ref2"
    , "코로나 이전 영향" = "ref3"
    , "코로나 이후 영향" = "ref4"
  )

corMat = cor(dataL5)
corPmat = cor_pmat(dataL5)

ggcorrplot(corMat, hc.order = TRUE, type = "lower", lab_col = "black", outline.color = "white", lab = TRUE, p.mat = corPmat) +
  ggsave(filename = paste(globalVar$figPath, "Img_LSH0078_2.png", sep = "/"), width = 6, height = 6, dpi = 600)


#==================================
# 조절 변수
#==================================
dataL2 = dataL1 %>%
  dplyr::select(val1, val3, val4, val5)

summary(dataL2)
pairs(dataL2)

cor(dataL2)

plot(dataL2$val1, dataL2$val3)
plot(dataL2$val4, dataL2$val3)
plot(dataL2$val5, dataL2$val3)

# 예술을 좋아하면 (독립변수)-예술모임(살롱)도 좋아한다. (종속변수)
# 조절변수: 삶의 의미에 관련된질문, 소모임의 불쾌한 기억
lmFit = lm(val3 ~ val1, data = dataL2)
summary(lmFit)

lmFit = lm(val3 ~ val1 * val4, data = dataL2)
summary(lmFit)

lmFit = lm(val3 ~ val1 * val5, data = dataL2)
summary(lmFit)

lmFit = lm(val3 ~ val1 * val4 * val5, data = dataL2)
summary(lmFit)

ggiraphExtra::ggPredict(lmFit, show.point = FALSE)

lmFit = lm(val3 ~ val1 + val1 + val1 * val5, data = dataL2)
summary(lmFit)

ggiraphExtra::ggPredict(lmFit, mode = 3, colorn = 50, show.point = FALSE)


# 살롱문화를 선호하는 것이 (독립변수)-인간에게 행복을 줄 수 있다. (종속변수)
# -조절변수: 모임에서의 안좋은 기억, 인생에 충분한 목적이 없다. 천재지변(코로나)
dataL2 = dataL1 %>%
  dplyr::select(ref1, ref2, ref3, ref4)

summary(dataL2)
# pairs(dataL2)

cor(dataL2)

plot(dataL2$val1, dataL2$val3)
plot(dataL2$val4, dataL2$val3)
plot(dataL2$val5, dataL2$val3)

lmFit = lm(ref2 ~ ref1, data = dataL2)
summary(lmFit)

lmFit = lm(ref2 ~ ref1 * ref3, data = dataL2)
summary(lmFit)

lmFit = lm(ref2 ~ ref1 * ref4, data = dataL2)
summary(lmFit)

lmFit = lm(ref2 ~ ref1 * ref3 * ref4, data = dataL2)
summary(lmFit)


#==================================
# 가설 검정
#==================================
dataL2 = dataL1 %>%
  dplyr::select(ref2, ref3, ref4) %>%
  dplyr::rename(
    "긍정 수치" = "ref2"
    , "코로나 이전 영향" = "ref3"
    , "코로나 이후 영향" = "ref4"
  ) %>%
  tidyr::gather()

# aovRes에서 P-value는 7.0602e-08로서 0.05보다 작기 떄문에 코로나 이전/이후에 따른 행복의 차이가 있다.
aovRes = aov(value ~ key, data = dataL2)
summary(aovRes)

# 타 특성에 비해 코로나 이전-행복지수는 p-value가 0.01 이하로서 통계적으로 유의하다.
tukeyRes = TukeyHSD(aovRes)

# 예시 그림
plot(tukeyRes)

tukeyResL1 = tukeyRes$key %>%
  as.data.frame()

# 예시 그림
tky = tukeyResL1
tky$pair = rownames(tky)

# Plot pairwise TukeyHSD comparisons and color by significance level
ggplot(tky, aes(colour = cut(`p adj`, c(0, 0.01, 0.05, 0.1, 1),
                             label = c("p < 0.01", "p < 0.05", "p < 0.1", "Non-Sig")))) +
  geom_hline(yintercept = 0, lty = "11", colour = "grey30") +
  geom_errorbar(aes(pair, ymin = lwr, ymax = upr), width = 0.2) +
  geom_point(aes(pair, diff)) +
  labs(colour = "") +
  theme(text = element_text(size = 18)) +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figPath, "Img_LSH0078_3.png", sep = "/"), width = 10, height = 8, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

serviceName = "LSH0074"

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
library(tidyr)
library(geosphere)
library(ggrepel)
library(gdata)
library(data.table)
library(ggh4x)
library(remotes)
library(devtools)
library(pracma)
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
library(readr)


#=====================================================================================
# 구글 API를 통해 주소를 위경도로 변환
# register_google(key = 'AIzaSyD0d6wMKB4UHwtVkHIPhWISq4MkzDFqdu4')
#
# geo_locate_data_L1 <- geo_locate_data %>%
#   dplyr::select(addr) %>%
#   dplyr::mutate(addr = gsub(x = addr,pattern = "\n",replacement = " ")) %>%
#   dplyr::distinct()
#
#
# # point_data_TEST <- point_data_L1 %>%
# #   dplyr::filter(addr_new == "대전 정림동 591")
#
# point_data_TEST <- geo_locate_data_L1 %>%
#   dplyr::slice(1:10)
#
# # go
# locate_res <- mutate_geocode(geo_locate_data_L1,addr)
# write.csv(locate_res,"./imsi충남.csv")
#
# locate_res_L1 <- locate_res %>%
#   dplyr::select(-addr_new) %>%
#   dplyr::mutate(lon_google = lon, lat_google = lat) %>%
#   dplyr::select(-lon,-lat)
#
# locate_res_L1 = locate_res %>%
#   dplyr::filter(is.na(lon))
#=====================================================================================

# 데이터와 지도와 매칭 (시도)
fileInfo = Sys.glob(paste(globalVar$mapPath, "/admCode/충남기관.xlsx", sep = "/"))
dataInfo = openxlsx::read.xlsx(fileInfo, sheet = "DATA")

fileInfo = Sys.glob(paste(globalVar$mapPath, "/admCode/충남_inGeoData.csv", sep = "/"))
stationData = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR")) %>%
  dplyr::select(-X1) %>%
  dplyr::left_join(dataInfo, by = c("addr" = "addr")) # %>%
# dplyr::filter(! stringr::str_detect(title, regex("진료소")))

# unique(geoData_L1$sigungu_name)
#
#************************************************
# [openxlsx] Write
#************************************************
# wb = openxlsx::createWorkbook()
#
# openxlsx::addWorksheet(wb, "stationData")
# openxlsx::writeData(wb, "stationData", stationData, startRow = 1, startCol = 1)
#
# saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "충남기관_L1.xlsx")
# openxlsx::saveWorkbook(wb, file = saveFile, overwrite = TRUE)

#==========================================
# 시군구별 현황
#==========================================
mapInfo = Sys.glob(paste(globalVar$mapPath, "/koreaInfo/TL_SCCO_SIG.shp", sep = "/"))
la = shapefile(mapInfo, encoding = "UTF-8")

geo = spTransform(la, CRS("+proj=longlat"))
dplyr::tbl_df(geo)

geoData = ggplot2::fortify(geo, region = 'SIG_CD', region2 = "SIG_KOR_NM")
dplyr::tbl_df(geoData)

codeInfo = Sys.glob(paste(globalVar$mapPath, "/admCode/법정동코드_전체자료.txt", sep = "/"))
code = read.table(codeInfo, sep = "\t", header = TRUE, colClasses = "character", fileEncoding = "EUC-KR") %>%
  dplyr::rename(
    "EMD_CD" = "법정동코드"
    , "full_addr" = "법정동명"
    , "useflag" = "폐지여부"
  )

code_L1 = code %>%
  tidyr::separate(col = "full_addr", into = c("d1", "d2", "d3", "d4", "d5"), sep = " ") %>%
  dplyr::filter(is.na(d3), !is.na(d2)) %>%
  dplyr::mutate(code = str_sub(EMD_CD, 1, 5)) %>%
  dplyr::filter(useflag == "존재") %>%
  dplyr::select(-c(EMD_CD, d3, d4, d5, useflag)) %>%
  dplyr::rename(
    "si_do" = "d1"
    , "sigungu_name" = "d2"
    , "sigungu_code" = "code"
  )

unique(code_L1$si_do)

# 충청남도 읽기
geoData_L1 = code_L1 %>%
  dplyr::filter(si_do %in% c("충청남도")) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(sigungu_code)) %>%
  dplyr::mutate(code = as.character(round(as.numeric(sigungu_code, 0)))) %>%
  dplyr::add_row(si_do = "충청남도", sigungu_name = "동남구", sigungu_code = "44131", code = "44131") %>%
  dplyr::add_row(si_do = "충청남도", sigungu_name = "서북구", sigungu_code = "44133", code = "44133") %>%
  dplyr::inner_join(geoData, by = c("code" = "id"))


dataL2 = geoData_L1 %>%
  # dplyr::left_join(dataL1, by = c("si_do" = "addr1", "sigungu_name" = "addr2")) %>%
  dplyr::mutate(
    plotLabel = sigungu_name
    , xOffset = dplyr::case_when(
      sigungu_name == "태안군" ~ 0.275
      , sigungu_name == "보령시" ~ 0.275
      , sigungu_name == "서산시" ~ 0
      , sigungu_name == "예산군" ~ 0.05
      , sigungu_name == "홍성군" ~ 0.05
      , sigungu_name == "서천군" ~ 0.05
      , sigungu_name == "동남구" ~ 0.02
      , TRUE ~ 0
    )
    , yOffset = dplyr::case_when(
      sigungu_name == "서산시" ~ -0.05
      , sigungu_name == "태안군" ~ 0.1
      , sigungu_name == "서천군" ~ 0
      , sigungu_name == "반곡동" ~ -0.1
      , sigungu_name == "동남구" ~ 0.025
      , sigungu_name == "서북구" ~ 0.025
      , sigungu_name == "보령시" ~ -0.05
      , TRUE ~ 0
    )
    , backColor = dplyr::case_when(
      stringr::str_detect(sigungu_name, regex("태안군|서산시|당진시")) ~ "1"
      , stringr::str_detect(sigungu_name, regex("아산시|서북구|동남구")) ~ "2"
      , stringr::str_detect(sigungu_name, regex("홍성군|예산군|청양군|보령시")) ~ "3"
      , stringr::str_detect(sigungu_name, regex("공주시|계룡시")) ~ "4"
      , stringr::str_detect(sigungu_name, regex("부여군|서천군|논산시|금산군")) ~ "5"
      , TRUE ~ "NA"
    )
  )

#*******************************************************************
# 1. 충남 전체지도에 1-9번 서비스 전체 지도 - 1개
# > 충청남도 전체 자원 분포 현황 [1-9]
#*******************************************************************

# 시각화
saveImg = sprintf("%s/TMP3/Img_%s_%05d_%s.png", globalVar$figPath, serviceName, 1, "충청남도 전체 자원 분포 현황")
# saveImg = sprintf("%s/TMP4/Img_%s_%05d_%s.png", globalVar$figPath, serviceName, 1, "충청남도 전체 자원 분포 현황 (진료소 X)")
plotSubTitle = sprintf("%s", "충청남도 전체 자원 분포 현황")

# ggplot(data = dataL2, aes(x = long, y = lat, group = group, fill = as.factor(sigungu_name))) +
ggplot(data = dataL2, aes(x = long, y = lat, group = group, fill = factor(backColor))) +
  theme_bw() +
  coord_fixed(ratio = 1) +
  geom_polygon() +
  # geom_polygon(fill = "white", alpha = 0.6) +
  # scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
  geom_path(colour = 'black', size = 0.5) +
  geom_point(data = stationData, aes(x = lon, y = lat, group = NULL, fill = NULL), colour = "red", pch = 16, size = 0.35) +
  ggh4x::stat_midpoint(data = dataL2, aes(x = long + xOffset, y = lat + yOffset, group = sigungu_code, label = plotLabel), geom = "text", size = 5.5) +
  scale_fill_manual(values = c("1" = "#d2edf4", "2" = "#aeb3d3", "3" = "#f8d5af", "4" = "#f0f0b0", "5" = "#cde2a1"), name = NULL, na.value = NA) +
  labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = NULL, subtitle = NULL) +
  theme_minimal() +
  theme(
    text = element_text(size = 18)
    , panel.grid.major.x = element_blank()
    , panel.grid.major.y = element_blank()
    , panel.grid.minor.x = element_blank()
    , panel.grid.minor.y = element_blank()
    , axis.text.x = element_blank()
    , axis.ticks.x = element_blank()
    , axis.title.x = element_blank()
    , axis.text.y = element_blank()
    , axis.ticks.y = element_blank()
    , axis.title.y = element_blank()
    , plot.subtitle = element_text(hjust = 1)
    , legend.position = "none"
  ) +
  ggsave(filename = saveImg, width = 10, height = 5, dpi = 600)


#*******************************************************************
# 2. 충남 전체지도에 각 서비스 별 지도 - 9개
# > 충청남도 전체 자원 분포 현황 [1, 2, ..., 9]
#*******************************************************************

valList = sort(unique(stationData$val))

# valInfo = "문화 및 여가"

for (valInfo in valList) {

  stationDataL1 = stationData %>%
    dplyr::filter(val == valInfo)

  # if (nrow(stationDataL1) < 1) { next }

  # 시각화
  saveImg = sprintf("%s/TMP3/Img_%s_%05d_%s_%s.png", globalVar$figPath, serviceName, 2, "충청남도 전체 자원 분포 현황", valInfo)
  plotSubTitle = sprintf("[%s] %s", valInfo, "충청남도 전체 자원 분포 현황")

  # ggplot(data = dataL2, aes(x = long, y = lat, group = group, fill = as.factor(sigungu_name))) +
  ggplot(data = dataL2, aes(x = long, y = lat, group = group, fill = factor(backColor))) +
    theme_bw() +
    coord_fixed(ratio = 1) +
    geom_polygon() +
    # geom_polygon(fill = "white", alpha = 0.6) +
    # scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
    geom_path(colour = 'black', size = 0.5) +
    geom_point(data = stationDataL1, aes(x = lon, y = lat, group = NULL, fill = NULL), colour = "red", pch = 16, size = 0.35) +
    ggh4x::stat_midpoint(data = dataL2, aes(x = long + xOffset, y = lat + yOffset, group = sigungu_code, label = plotLabel), geom = "text", size = 5.5) +
    labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = NULL, subtitle = NULL) +
    scale_fill_manual(values = c("1" = "#d2edf4", "2" = "#aeb3d3", "3" = "#f8d5af", "4" = "#f0f0b0", "5" = "#cde2a1"), name = NULL, na.value = NA) +
    theme_minimal() +
    theme(
      text = element_text(size = 18)
      , panel.grid.major.x = element_blank()
      , panel.grid.major.y = element_blank()
      , panel.grid.minor.x = element_blank()
      , panel.grid.minor.y = element_blank()
      , axis.text.x = element_blank()
      , axis.ticks.x = element_blank()
      , axis.title.x = element_blank()
      , axis.text.y = element_blank()
      , axis.ticks.y = element_blank()
      , axis.title.y = element_blank()
      , plot.subtitle = element_text(hjust = 1)
      , legend.position = "none"
    ) +
    ggsave(filename = saveImg, width = 10, height = 5, dpi = 600)
}


#==========================================
# 그래프 6 : 읍면동별 현황
#==========================================
# 읍면동 한반도 지도 읽기
mapInfo = Sys.glob(paste(globalVar$mapPath, "/koreaInfo/bnd_dong_00_2019_2019_2Q.shp", sep = "/"))
la = shapefile(mapInfo, encoding = "UTF-8")
geo = sp::spTransform(la, CRS("+proj=longlat"))

geoData = ggplot2::fortify(geo, region = 'adm_dr_cd', region2 = "adm_dr_nm")

# 행정 코드 (행정동) 읽기
codeInfo = Sys.glob(paste(globalVar$mapPath, "/admCode/admCode.xlsx", sep = "/"))
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

unique(geoData_L1$sigungu_name)

# 위치 정보와 한반도 지도 병합
geoData_L1 = code_L1 %>%
  dplyr::filter(si_do_name %in% c("충청남도")) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(sigungu_code)) %>%
  # dplyr::mutate(code = as.character(round(as.numeric(sigungu_code,0)))) %>%
  dplyr::mutate(
    code = as.character(round(as.numeric(emd_code, 0)))
    , sigungu_name = dplyr::case_when(
      stringr::str_detect(sigungu_name, regex("동남구")) ~ "동남구"
      , stringr::str_detect(sigungu_name, regex("서북구")) ~ "서북구"
      , TRUE ~ sigungu_name
    )
  ) %>%
  dplyr::inner_join(geoData, by = c("code" = "id"))


#*******************************************************************
# 3. 충남 시군구별 전체서비스 지도 - 16개
# > 충청남도 시군구별 자원 분포도 [계룡, 공주, ..., 태안] [1-9]
#*******************************************************************
nameList = sort(unique(geoData_L1$sigungu_name))

# nameInfo = "홍성군"
nameInfo = "서북구"
# nameList = c("태안군", "홍성군")

# "계룡시" "공주시" "금산군" "논산시" "당진시" "동남구" "보령시" "부여군" "서북구" "서산시"
# "서천군" "아산시" "예산군" "청양군" "태안군" "홍성군"

for (nameInfo in nameList) {

  dataL2 = geoData_L1 %>%
    dplyr::filter(sigungu_name == nameInfo) %>%
    dplyr::mutate(
      # plotLabel = dplyr::case_when(
      #   stringr::str_detect(emd_name, regex("원성.동")) ~ "원성동"
      #   , stringr::str_detect(emd_name, regex("당진.동")) ~ "당진동"
      #   , stringr::str_detect(emd_name, regex("대천.동")) ~ "대천동"
      #   , stringr::str_detect(emd_name, regex("동문.동")) ~ "동문동"
      #   , stringr::str_detect(emd_name, regex("부성.동")) ~ "부성동"
      #   , stringr::str_detect(emd_name, regex("성정.동")) ~ "성정동"
      #   , stringr::str_detect(emd_name, regex("쌍용.동")) ~ "쌍용동"
      #   , stringr::str_detect(emd_name, regex("온양.동")) ~ "온양동"
      #   TRUE ~ emd_name
      # )
      plotLabel = emd_name
      , xOffset = dplyr::case_when(
        emd_name == "근흥면" ~ 0.275
        , emd_name == "안면읍" ~ 0.07
        , nameInfo == "태안군" & emd_name == "남면" ~ 0.06
        , nameInfo == "부여군" & emd_name == "남면" ~ -0.01
        , emd_name == "오천면" ~ 0.27
        , emd_name == "장항읍" ~ 0.06
        , emd_name == "한산면" ~ -0.01
        , emd_name == "지곡면" ~ 0.02
        , emd_name == "이원면" ~ -0.01
        , emd_name == "원북면" ~ -0.01
        , emd_name == "서부면" ~ 0.02
        , emd_name == "정안면" ~ 0.02
        , emd_name == "연산면" ~ 0.01
        , emd_name == "면천면" ~ -0.01
        , emd_name == "천북면" ~ -0.01
        , emd_name == "옥산면" ~ -0.01
        , emd_name == "장암면" ~ 0.04
        , emd_name == "대산읍" ~ 0.02
        , emd_name == "팔봉면" ~ 0.005
        , emd_name == "서면" ~ 0.015
        , emd_name == "시초면" ~ 0.005
        , emd_name == "충화면" ~ -0.01
        , emd_name == "초촌면" ~ 0.02
        , TRUE ~ 0
      )
      , yOffset = dplyr::case_when(
        emd_name == "마서면" ~ 0.01
        , emd_name == "마산면" ~ -0.01
        , emd_name == "진산면" ~ -0.02
        , emd_name == "복수면" ~ -0.02
        , emd_name == "군북면" ~ 0.02
        , emd_name == "원북면" ~ -0.03
        , emd_name == "팔봉면" ~ -0.02
        , emd_name == "신평면" ~ -0.02
        , emd_name == "해미면" ~ 0.01
        , emd_name == "이원면" ~ -0.03
        , emd_name == "원북면" ~ -0.045
        , emd_name == "이원면" ~ 0.01
        , emd_name == "장압면" ~ 0.05
        , nameInfo == "태안군" & emd_name == "남면" ~ -0.01
        , emd_name == "우성면" ~ -0.03
        , emd_name == "신창면" ~ 0.02
        , emd_name == "선장면" ~ -0.02
        , emd_name == "장항읍" ~ 0.005
        , emd_name == "남일면" ~ -0.01
        , emd_name == "부리면" ~ 0.01
        , emd_name == "남이면" ~ 0.01
        , emd_name == "주산면" ~ -0.01
        , emd_name == "천북면" ~ 0.01
        , emd_name == "대천동" ~ 0.01
        , emd_name == "오천면" ~ 0.09
        , emd_name == "초춘면" ~ 0.01
        , emd_name == "양화면" ~ 0.01
        , emd_name == "구룡면" ~ -0.01
        , emd_name == "옥산면" ~ -0.01
        , emd_name == "임천면" ~ -0.01
        , emd_name == "장암면" ~ 0.005
        , emd_name == "성거읍" ~ -0.01
        , emd_name == "직산읍" ~ -0.005
        , emd_name == "지곡면" ~ -0.02
        , emd_name == "성연면" ~ -0.01
        , emd_name == "서면" ~ 0.005
        , emd_name == "비인면" ~ 0.01
        , emd_name == "문산면" ~ 0.01
        , emd_name == "종천면" ~ 0.005
        , emd_name == "안면읍" ~ -0.03
        , emd_name == "태안읍" ~ 0.04
        , emd_name == "근흥면" ~ 0.01
        , TRUE ~ 0
      )
      , backColor = dplyr::case_when(
        stringr::str_detect(sigungu_name, regex("태안군|서산시|당진시")) ~ "1"
        , stringr::str_detect(sigungu_name, regex("아산시|서북구|동남구")) ~ "2"
        , stringr::str_detect(sigungu_name, regex("홍성군|예산군|청양군|보령시")) ~ "3"
        , stringr::str_detect(sigungu_name, regex("공주시|계룡시")) ~ "4"
        , stringr::str_detect(sigungu_name, regex("부여군|서천군|논산시|금산군")) ~ "5"
        , TRUE ~ "NA"
      )
      # , fontSize = dplyr::case_when(
      # stringr::str_detect(emd_name, regex("원성.동|당진.동|대천.동|동문.동|부성.동|성정.동|쌍용.동|온양.동")) ~ 4
      # stringr::str_detect(emd_name, regex("원성.동")) ~ "원성동"
      # , stringr::str_detect(emd_name, regex("당진.동")) ~ "당진동"
      # , stringr::str_detect(emd_name, regex("대천.동")) ~ "대천동"
      # , stringr::str_detect(emd_name, regex("동문.동")) ~ "동문동"
      # stringr::str_detect(emd_name, regex("부성.동")) ~ 3.5
      # , stringr::str_detect(emd_name, regex("성정.동")) ~ "성정동"
      # , stringr::str_detect(emd_name, regex("쌍용.동")) ~ "쌍용동"
      # , stringr::str_detect(emd_name, regex("온양.동")) ~ "온양동"
      # TRUE ~ 3.5
      # )
    )

  if (nrow(dataL2) < 1) { next }

  stationDataL1 = stationData %>%
    dplyr::filter(stringr::str_detect(addr, nameInfo))

  # if (nrow(stationDataL1) < 1) { next }

  # 시각화
  saveImg = sprintf("%s/TMP3/Img_%s_%05d_%s_%s.png", globalVar$figPath, serviceName, 3, "충청남도 시군구별 자원 분포도", nameInfo)

  # saveImg = sprintf("%s/TMP4/Img_%s_%05d_%s_%s.png", globalVar$figPath, serviceName, 3, "충청남도 시군구별 자원 분포도 (진료소 O)", nameInfo)
  plotSubTitle = sprintf("%s (%s)", "충청남도 시군구별 자원 분포도", nameInfo)

  ggplot(data = dataL2, aes(x = long, y = lat, group = group, fill = factor(backColor))) +
    theme_bw() +
    coord_fixed(ratio = 1) +
    geom_polygon() +
    # geom_polygon(fill = "white", alpha = 0.6) +
    # scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
    geom_path(colour = 'black', size = 0.5) +
    geom_point(data = stationDataL1, aes(x = lon, y = lat, group = NULL, fill = NULL), colour = "red", pch = 16, size = 1) +
    ggh4x::stat_midpoint(data = dataL2, aes(x = long + xOffset, y = lat + yOffset, group = emd_code, label = plotLabel), geom = "text", size = 2.5) +
    labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = NULL, subtitle = NULL) +
    scale_fill_manual(values = c("1" = "#d2edf4", "2" = "#aeb3d3", "3" = "#f8d5af", "4" = "#f0f0b0", "5" = "#cde2a1"), name = NULL, na.value = NA) +
    theme_minimal() +
    theme(
      text = element_text(size = 18)
      , panel.grid.major.x = element_blank()
      , panel.grid.major.y = element_blank()
      , panel.grid.minor.x = element_blank()
      , panel.grid.minor.y = element_blank()
      , axis.text.x = element_blank()
      , axis.ticks.x = element_blank()
      , axis.title.x = element_blank()
      , axis.text.y = element_blank()
      , axis.ticks.y = element_blank()
      , axis.title.y = element_blank()
      , plot.subtitle = element_text(hjust = 1)
      , legend.position = "none"
    ) +
    ggsave(filename = saveImg, width = 10, height = 5, dpi = 600)
}

#****************************************************************************
# 4. 충남 서비스별 시군별 지도 - 9*16= 144개
# > 충청남도 시군구별 자원 분포도 [계룡, 공주, ..., 태안] [1, 2, ..., 9]
#****************************************************************************

valList = sort(unique(stationData$val))
nameList = sort(unique(geoData_L1$sigungu_name))
# nameList = c("태안군", "홍성군")
# valInfo = "문화 및 여가"
# nameInfo = "금산군"

for (nameInfo in nameList) {
  for (valInfo in valList) {

    # 시각화
    saveImg = sprintf("%s/TMP3/Img_%s_%05d_%s_%s_%s.png", globalVar$figPath, serviceName, 4, "충청남도 시군구별 자원 분포도", nameInfo, valInfo)
    plotSubTitle = sprintf("[%s] %s (%s)", valInfo, "충청남도 시군구별 자원 분포도", nameInfo)

    # if (fs::file_exists(saveImg) == TRUE) { next }

    dataL2 = geoData_L1 %>%
      dplyr::filter(sigungu_name == nameInfo) %>%
      dplyr::mutate(
        # plotLabel = dplyr::case_when(
        #   stringr::str_detect(emd_name, regex("원성.동")) ~ "원성동"
        #   , stringr::str_detect(emd_name, regex("당진.동")) ~ "당진동"
        #   , stringr::str_detect(emd_name, regex("대천.동")) ~ "대천동"
        #   , stringr::str_detect(emd_name, regex("동문.동")) ~ "동문동"
        #   , stringr::str_detect(emd_name, regex("부성.동")) ~ "부성동"
        #   , stringr::str_detect(emd_name, regex("성정.동")) ~ "성정동"
        #   , stringr::str_detect(emd_name, regex("쌍용.동")) ~ "쌍용동"
        #   , stringr::str_detect(emd_name, regex("온양.동")) ~ "온양동"
        #   TRUE ~ emd_name
        # )
        plotLabel = emd_name
        , xOffset = dplyr::case_when(
          emd_name == "근흥면" ~ 0.275
          , emd_name == "안면읍" ~ 0.07
          , nameInfo == "태안군" & emd_name == "남면" ~ 0.06
          , nameInfo == "부여군" & emd_name == "남면" ~ -0.01
          , emd_name == "오천면" ~ 0.27
          , emd_name == "장항읍" ~ 0.06
          , emd_name == "한산면" ~ -0.01
          , emd_name == "지곡면" ~ 0.02
          , emd_name == "이원면" ~ -0.01
          , emd_name == "원북면" ~ -0.01
          , emd_name == "서부면" ~ 0.02
          , emd_name == "정안면" ~ 0.02
          , emd_name == "연산면" ~ 0.01
          , emd_name == "면천면" ~ -0.01
          , emd_name == "천북면" ~ -0.01
          , emd_name == "옥산면" ~ -0.01
          , emd_name == "장암면" ~ 0.04
          , emd_name == "대산읍" ~ 0.02
          , emd_name == "팔봉면" ~ 0.005
          , emd_name == "서면" ~ 0.015
          , emd_name == "시초면" ~ 0.005
          , emd_name == "충화면" ~ -0.01
          , emd_name == "초촌면" ~ 0.02
          , TRUE ~ 0
        )
        , yOffset = dplyr::case_when(
          emd_name == "마서면" ~ 0.01
          , emd_name == "마산면" ~ -0.01
          , emd_name == "진산면" ~ -0.02
          , emd_name == "복수면" ~ -0.02
          , emd_name == "군북면" ~ 0.02
          , emd_name == "원북면" ~ -0.03
          , emd_name == "팔봉면" ~ -0.02
          , emd_name == "신평면" ~ -0.02
          , emd_name == "해미면" ~ 0.01
          , emd_name == "이원면" ~ -0.03
          , emd_name == "원북면" ~ -0.045
          , emd_name == "이원면" ~ 0.01
          , emd_name == "장압면" ~ 0.05
          , nameInfo == "태안군" & emd_name == "남면" ~ -0.01
          , emd_name == "우성면" ~ -0.03
          , emd_name == "신창면" ~ 0.02
          , emd_name == "선장면" ~ -0.02
          , emd_name == "장항읍" ~ 0.005
          , emd_name == "남일면" ~ -0.01
          , emd_name == "부리면" ~ 0.01
          , emd_name == "남이면" ~ 0.01
          , emd_name == "주산면" ~ -0.01
          , emd_name == "천북면" ~ 0.01
          , emd_name == "대천동" ~ 0.01
          , emd_name == "오천면" ~ 0.09
          , emd_name == "초춘면" ~ 0.01
          , emd_name == "양화면" ~ 0.01
          , emd_name == "구룡면" ~ -0.01
          , emd_name == "옥산면" ~ -0.01
          , emd_name == "임천면" ~ -0.01
          , emd_name == "장암면" ~ 0.005
          , emd_name == "성거읍" ~ -0.01
          , emd_name == "직산읍" ~ -0.005
          , emd_name == "지곡면" ~ -0.02
          , emd_name == "성연면" ~ -0.01
          , emd_name == "서면" ~ 0.005
          , emd_name == "비인면" ~ 0.01
          , emd_name == "문산면" ~ 0.01
          , emd_name == "종천면" ~ 0.005
          , emd_name == "안면읍" ~ -0.03
          , emd_name == "태안읍" ~ 0.04
          , emd_name == "근흥면" ~ 0.01
          , TRUE ~ 0
        )
        , backColor = dplyr::case_when(
          stringr::str_detect(sigungu_name, regex("태안군|서산시|당진시")) ~ "1"
          , stringr::str_detect(sigungu_name, regex("아산시|서북구|동남구")) ~ "2"
          , stringr::str_detect(sigungu_name, regex("홍성군|예산군|청양군|보령시")) ~ "3"
          , stringr::str_detect(sigungu_name, regex("공주시|계룡시")) ~ "4"
          , stringr::str_detect(sigungu_name, regex("부여군|서천군|논산시|금산군")) ~ "5"
          , TRUE ~ "NA"
        )
        # , fontSize = dplyr::case_when(
        # stringr::str_detect(emd_name, regex("원성.동|당진.동|대천.동|동문.동|부성.동|성정.동|쌍용.동|온양.동")) ~ 4
        # stringr::str_detect(emd_name, regex("원성.동")) ~ "원성동"
        # , stringr::str_detect(emd_name, regex("당진.동")) ~ "당진동"
        # , stringr::str_detect(emd_name, regex("대천.동")) ~ "대천동"
        # , stringr::str_detect(emd_name, regex("동문.동")) ~ "동문동"
        # stringr::str_detect(emd_name, regex("부성.동")) ~ 3.5
        # , stringr::str_detect(emd_name, regex("성정.동")) ~ "성정동"
        # , stringr::str_detect(emd_name, regex("쌍용.동")) ~ "쌍용동"
        # , stringr::str_detect(emd_name, regex("온양.동")) ~ "온양동"
        # TRUE ~ 3.5
        # )
      )

    if (nrow(dataL2) < 1) { next }

    stationDataL1 = stationData %>%
      dplyr::filter(
        stringr::str_detect(addr, nameInfo)
        , val == valInfo
      )

    # if (nrow(stationDataL1) < 1) { next }


    ggplot(data = dataL2, aes(x = long, y = lat, group = group, fill = factor(backColor))) +
      theme_bw() +
      coord_fixed(ratio = 1) +
      geom_polygon() +
      # scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
      geom_path(colour = 'black', size = 0.5) +
      geom_point(data = stationDataL1, aes(x = lon, y = lat, group = NULL, fill = NULL), colour = "red", pch = 16, size = 1) +
      ggh4x::stat_midpoint(data = dataL2, aes(x = long + xOffset, y = lat + yOffset, group = emd_code, label = plotLabel), geom = "text", size = 2.5) +
      scale_fill_manual(values = c("1" = "#d2edf4", "2" = "#aeb3d3", "3" = "#f8d5af", "4" = "#f0f0b0", "5" = "#cde2a1"), name = NULL, na.value = NA) +
      labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = NULL, subtitle = NULL) +
      theme_minimal() +
      theme(
        text = element_text(size = 18)
        , panel.grid.major.x = element_blank()
        , panel.grid.major.y = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , axis.text.x = element_blank()
        , axis.ticks.x = element_blank()
        , axis.title.x = element_blank()
        , axis.text.y = element_blank()
        , axis.ticks.y = element_blank()
        , axis.title.y = element_blank()
        , plot.subtitle = element_text(hjust = 1)
        , legend.position = "none"
      ) +
      ggsave(filename = saveImg, width = 10, height = 5, dpi = 600)
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
library(arules)
library(ggfortify)
library(arulesViz)
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

#==================================================================================================
# 문제 1.r
#==================================================================================================
# 01. 구글 클래스룸에서 [basket.csv] 파일을 다운받은 후에 파일을 불러와서 basket_df로 저장하세요. 관련된 모든 변수명을 basket_000으로 변환하세요.
basket_df = read.csv("basket.csv", fileEncoding = "EUC-KR", encoding = "EUC-KR")

head(basket_df)

# 02. 1이상인 데이터를 1로 변환하고, dataframe을 matrix 형식으로 변환하세요.
basket_df2 = basket_df %>%
  dplyr::mutate_all(funs(replace(., . > 1, 1)))

basket_count = basket_df2 %>%
  as.matrix()

# 03. 바이너리 코드를 transactions으로 변환하세요.
basket_trans = as(basket_count, "transactions")

head(basket_count)

head(arules::inspect(basket_trans))

# 04. support = 0.3인 상위 5 변수를 출력해서 아래와 같이 그래프를 작성하세요.
itemFrequencyPlot(basket_trans, support = 0.3, topN = 5)

# 05. 최소규칙 지지도 = 0.1, 최소규칙 신뢰도 = 0.7로 규칙을 생성하세요.
basket_rules = arules::apriori(
  basket_trans
  , parameter = list(supp = 0.1, conf = 0.7)
)

# 06. rules을 테이블 형태로 저장해주세요.
basket_rules_tbl = tibble(
  lhs = labels(lhs(basket_rules))
  , rhs = labels(rhs(basket_rules))
  , quality(basket_rules)
)

str(basket_rules_tbl)


# 07. 최소규칙 지지도=0.1, 최소규칙 신뢰도=0.7, 리프트=1.5 이상인 규칙을 찾고, lift가 높은 순서로 sorting 해주세요.
# basket_rules_lift = sort(basket_rules, by="lift", decreasing=TRUE)
basket_rules_tbl2 = basket_rules_tbl %>%
  dplyr::filter(
    support >= 0.1
    , confidence >= 0.7
    , lift > 1.5
  ) %>%
  dplyr::arrange(desc(lift))

basket_rules_tbl2


# 08. {반제품}을 선택하는 규칙만 찾아주세요.
basket_rules_tbl3 = basket_rules_tbl %>%
  dplyr::filter(stringr::str_detect(rhs, "반제품"))

basket_rules_tbl3


# 09. 5개의 규칙만 선택해서 그래프로 그려주세요.
set.seed(123)

basket_rules_plot = head(sort(basket_rules, by = "lift"), 5)
plot(basket_rules_plot, method = "graph", engine = "htmlwidget")


#==================================================================================================
# 문제 2.r
#==================================================================================================
# 01. 구글 클래스룸에서 [iris.csv] 파일을 다운받은 후에 파일을 불러와서 iris_df로 저장하세요.
# 관련된 모든 변수명을 iris_000으로 변환하세요.
iris_df = readr::read_csv(file = "iris.csv")

head(iris_df)

# 02. 변수중에서 [Species]는 제거합니다. 결과를 아래와 같이 보여주세요.
iris_tb = iris_df %>%
  dplyr::select(-Species)

head(iris_tb)

# 03. Sepal.Length, Sepal.Width를 이용하여 데이터 분포를 확인하세요.
ggplot(iris_tb, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(alpha = 0.3)

# 04. 거리를 계산하기 위해 숫자형 변수를 정규화 하세요.
iris_tb2 = iris_tb %>%
  dplyr::mutate_all(scale)

iris_tb2

# 05. 최적군집수를 찾기 위해 엘보우 차트를 계산하세요. 군집수는 2~6개로 하세요.
iris_res = tibble()

# i = 1
for (i in 2:6) {
  iris_cluster = kmeans(iris_tb2, center = i)

  iris_tmpData = tibble(
    "k" = i
    , "tot.withinss" = iris_cluster$tot.withinss
  )

  iris_res = dplyr::bind_rows(iris_res, iris_tmpData)
}


ggplot(iris_res, aes(x = k, y = tot.withinss)) +
  geom_point() +
  geom_line()

# 06. Sepal.Length, Sepal.Width를 이용하여 군집별 그래프를 그려주세요.
iris_res = tibble()
iris_resMean = tibble()

i = 2
for (i in 2:6) {
  iris_rsKmeans = kmeans(iris_tb2[, c("Sepal.Length", "Sepal.Width")], i)

  iris_meanKmeans = iris_rsKmeans$centers %>%
    as.tibble()

  iris_tmpData = tibble(
    iris_tb2[, c("Sepal.Length", "Sepal.Width")]
    , "cluster" = iris_rsKmeans$cluster
    , "i" = i
  )

  iris_res = dplyr::bind_rows(iris_res, iris_tmpData)

  iris_tmpData2 = tibble(
    iris_meanKmeans
    , "i" = i
  )

  iris_resMean = dplyr::bind_rows(iris_resMean, iris_tmpData2)
}

ggplot(data = iris_res, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(colour = factor(cluster))) +
  geom_point(data = iris_resMean, aes(x = Sepal.Length, y = Sepal.Width), stroke = 2, shape = 4) +
  facet_wrap(~i) +
  labs(colour = "cluster")

# 07. 최적군집수를 3개로 설정해서 군집을 분류해 주세요. ‘seed=123’로 고정해주세요.
set.seed(123)

iris_rsKmeans = kmeans(iris_tb2, 3)
iris_meanKmeans = iris_rsKmeans$centers %>%
  as.tibble()

iris_klust_best = tibble(
  iris_meanKmeans
  , "size" = iris_rsKmeans$size
  , "withinss" = iris_rsKmeans$withinss
  , "cluster" = as.factor(unique(iris_rsKmeans$cluster))
) %>%
  dplyr::arrange(cluster)

iris_klust_best

# 08. 군집별 특성을 그래프로 그려주세요.
iris_dataL1 = iris_klust_best %>%
  tidyr::gather(-c("size", "withinss", "cluster"), key = "type", value = "cases")

ggplot(data = iris_dataL1, aes(x = type, y = cases, group = cluster, colour = cluster)) +
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
serviceName = "LSH0080"

library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(rvset)
library(httr)
library(XML)
library(RCurl)
library(ggplot2)
library(ggmap)
library(tm)
library(wordcloud2)
library(htmlwidgets)
library(webshot)
library(arulesViz)
library(jpeg)
library(png)
library(grid)

#=====================================================================
# 경영데이터분석
#=====================================================================
# 이 과제는 기말시험을 대체하기 위한 목적에서 진행되는 것으로 각각의 문항에 대해 채점이
# 이루어지며, 이 점수를 기말시험 성적으로 반영하게 됩니다. 만점은 100점이며, 일부 문항에
# 대해 추가점수를 부여합니다.
# 다른 과제와 마찬가지로 자신이 활용할 수 있는 여러 자료들을 이용하여 문제를 해결하면 됩
# 니다. 다만 이 과정에서 다른 사람의 도움을 받으면 안 됩니다.
# 과제 결과물은 자유 형식으로 제출하도록 하는데, 각각의 문항 번호를 붙여 결과물을 구분할
# 수 있도록 해 주세요.
# 과제 결과물의 맨 마지막에는 자신이 사용한 프로그램(스크립트)을 모두 붙여 넣도록 합니다.
# 만약 프로그램이 제시되지 않은 경우에는 제출한 결과물과 상관없이 0점 처리됩니다. 가능한
# 한 프로그램의 각 문항별로 주석을 추가하도록 합니다
# 문제가 갖고 있을지도 모를 오류에 대한 질문만 '강의Q&A'게시판을 통해 받도록 하겠습니다.


#*********************************************************************
# 과제 1
#*********************************************************************
# 네이버에서 제공하는 검색 API 가운데 지역 검색을 위한 API를 활용하려 합니다. 검색어는 "광교대학로 맛집"을 사용합니다. 이와 같은 API에 대한 https://developers.naver.com/docs/search/local/ 의 문서를 참고하여 다음의 질문에 대한 결과를 제시하세요.

# (1) 모든 검색결과를 수집하여 하나의 데이터프레임으로 만듭니다. 검색결과 업체에 대한 정보만을 포함하면 됩니다. 그리고 이렇게 만들어진 데이터프레임에 대한 str함수를 적용한 결과를 제시하세요. (10점)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 네이버 API (지역)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
keyword = "광교대학로 맛집"

# reqUrl = "https://openapi.naver.com/v1/search/local.xml"
reqUrl = "https://openapi.naver.com/v1/search/local.json"
reqQuery = stringr::str_c("?query=", RCurl::curlEscape(stringr::str_conv(keyword, encoding = "EUC-KR")))
reqDisplay = "&display=50"
reqStart = "&start=50"
reqSort = "&sort=date"

reqId = globalVar$naverKeyId
reqPw = globalVar$naverKeyPw

resRes = httr::GET(
  stringr::str_c(reqUrl, reqQuery, reqDisplay, reqStart, reqSort)
  , add_headers("X-Naver-Client-Id" = reqId, "X-Naver-Client-Secret" = reqPw)
)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# JSON 파일 처리
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
resData = httr::content(resRes, as = 'text') %>%
  jsonlite::fromJSON()

dataNaver = tibble(resData$items) %>%
  dplyr::mutate(
    title = str_replace_all(title, pattern = "<b>|</b>", replacement = "")
    , title = str_replace_all(title, pattern = "&amp;", replacement = "&")
    , title = str_trim(title, side = "both")
  ) %>%
  dplyr::select(title, category, roadAddress) %>%
  dplyr::bind_cols(type = "naver")

str(dataNaver)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  XML 파일 처리
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# xmlRes = XML::xmlParse(resRes)
#
# title = XML::xpathSApply(xmlRes, "/rss/channel/item/title", xmlValue) %>%
#   str_replace_all(pattern = "<b>|</b>", replacement = "") %>%
#   str_replace_all(pattern = "&amp;", replacement = "&") %>%
#   str_trim(side = "both")
#
# category = XML::xpathSApply(xmlRes, "/rss/channel/item/category", xmlValue)
#
# data = tibble::tibble(title, category)


# (2) 수집한 검색 결과에 나타난 업체를 네이버 지도 API를 이용해 나타내세요. (10점)

dataGeo = tibble::tibble()

for (i in 1:nrow(dataNaver)) {
  keyword = dataNaver$roadAddress[i]

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 네이버 API (지도)
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  reqUrl = "https://naveropenapi.apigw.ntruss.com/map-geocode/v2/geocode"
  reqQuery = stringr::str_c("?query=", RCurl::curlEscape(stringr::str_conv(keyword, encoding = "UTF-8")))

  reqId = globalVar$naverApigwApiKeyId
  reqPw = globalVar$naverApigwApiKey

  reqRes = httr::GET(
    stringr::str_c(reqUrl, reqQuery)
    , add_headers("X-NCP-APIGW-API-KEY-ID" = reqId, "X-NCP-APIGW-API-KEY" = reqPw, "Accept" = "application/json")
  )

  resData = httr::content(reqRes, as = 'text') %>%
    jsonlite::fromJSON()

  data = resData$addresses

  makerLabel = stringr::str_c("markers=type:n|size:mid|pos:", data$x, "%20", data$y, "|label:", i)
  tmpData = tibble::tibble(lon = data$x, lat = data$y, maker = makerLabel)
  dataGeo = dplyr::bind_rows(dataGeo, tmpData)
}

dataGeoL1 = dataGeo %>%
  readr::type_convert() %>%
  dplyr::summarise(
    meanLon = mean(lon, na.rm = TRUE)
    , meanLat = mean(lat, na.rm = TRUE)
  )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 네이버 API (지도 이미지)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
reqUrl = "https://naveropenapi.apigw.ntruss.com/map-static/v2/raster"
reqQuery = stringr::str_c(
  "?center="
  , RCurl::curlEscape(stringr::str_conv(dataGeoL1$meanLon, encoding = "UTF-8"))
  , ","
  , RCurl::curlEscape(stringr::str_conv(dataGeoL1$meanLat, encoding = "UTF-8"))
)
reqSize = "&w=300&h=300"
reqLevel = "&level=16"
reqMaker = stringr::str_c("&", paste(dataGeo$maker, collapse = "&"))

reqId = globalVar$naverApigwApiKeyId
reqPw = globalVar$naverApigwApiKey

reqRes = httr::GET(
  stringr::str_c(reqUrl, reqQuery, reqSize, reqLevel, reqMaker)
  , add_headers("X-NCP-APIGW-API-KEY-ID" = reqId, "X-NCP-APIGW-API-KEY" = reqPw)
)

jsonRes = httr::content(reqRes, as = "raw")
img = jpeg::readJPEG(jsonRes)
jpeg::writeJPEG(img, target = "KGU_map.jpg")

mapImg = imager::load.image("KGU_map.jpg")
plot(mapImg)

# (3) 카카오 역시 키워드로 장소 검색 기능을 API를 통해 제공하고 있습니다.(카카오 개발자 사이트 (developer.kakao.com)의 내용 참고) 앞서 문제와 같은 작업을 카카오에서 제공하는 API를 이용해 수행합니다. 즉 검색어를 "광교대학로 맛집"을 사용한 모든 검색 결과를 데이터프레임으로 만들고 이 데이터 프레임에 대한 str 함수를 적용한 결과를 제시하세요. (20점)
# https://developers.kakao.com/tool/rest-api/open/get/v2-local-search-keyword.%7Bformat%7D

# curl -X GET "https://dapi.kakao.com/v2/local/search/keyword.xml?page=1&size=15&sort=accuracy&query=%EA%B4%91%EA%B5%90%EB%8C%80%ED%95%99%EB%A1%9C+%EB%A7%9B%EC%A7%91" \
# -H "Authorization: KakaoAK {REST_API_KEY}"

# curl -X GET "https://dapi.kakao.com/v2/local/search/address.xml?page=45&size=30&query=%EC%95%88%EB%93%9C%EB%A1%9C%EC%9D%B4%EB%93%9C" \
# -H "Authorization: KakaoAK {REST_API_KEY}"

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 카카오 API (검색)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
keyword = "광교대학로 맛집"

reqUrl = "https://dapi.kakao.com/v2/local/search/keyword.json"
# reqUrl = "https://openapi.naver.com/v1/search/local.json"
reqQuery = stringr::str_c("?query=", RCurl::curlEscape(stringr::str_conv(keyword, encoding = "EUC-KR")))
reqPage = "&page=45"
reqSize = "&size=15"
reqSort = "&sort=accuracy"

reqKakaoRestApiKey = globalVar$kakaoRestApiKey

resRes = httr::GET(
  stringr::str_c(reqUrl, reqQuery, reqDisplay, reqStart, reqSort)
  , add_headers("Authorization" = str_c("KakaoAK ", reqKakaoRestApiKey))
)

resData = httr::content(resRes, as = 'text') %>%
  jsonlite::fromJSON()

dataKakao = tibble(resData$documents) %>%
  dplyr::select(place_name, category_name) %>%
  dplyr::rename(
    "title" = "place_name"
    , "category" = "category_name"
  ) %>%
  dplyr::bind_cols(type = "kakao")

str(dataKakao)

# (4) 앞서 만든 두 개의 데이터프레임을 이용하여 카테고리에 따른 빈도를 각각 제시하세요. (10점)
data = dplyr::bind_rows(dataNaver, dataKakao) %>%
  dplyr::mutate(
    cate = dplyr::case_when(
      stringr::str_detect(category, regex("한식")) ~ "한식"
      , stringr::str_detect(category, regex("일식")) ~ "일식"
      , stringr::str_detect(category, regex("뷔페")) ~ "뷔페"
      , stringr::str_detect(category, regex("카페|커피전문점")) ~ "카페"
      , stringr::str_detect(category, regex("패스트푸드|샌드위치|도시락|다이어트")) ~ "패스트푸드 "
      , TRUE ~ "NA"
    )
  )

dataL1 = data %>%
  dplyr::group_by(cate) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq))

dataL1

#*********************************************************************
# 과제 2
#*********************************************************************
# 경기데이터드림에서 제공하는 Open API 가운데 '자동 기상관측 장비(AWS) 시간별 관측정보 현황' 데이터를 이용하여 다음의 질문에 답하시오.
# (1) Open API를 이용하여 경기도 수원시 팔달구에 있는 모든 관측정보로 부터 11월 한 달 동안의 오전 9시 기준 평균 기온과 습도를 구하시오. 이때 -99로 시작되는 데이터는 오류값이므로 분석에서 제외해야 합니다. (10점)

# https://data.gg.go.kr/portal/data/village/selectServicePage.do?page=1&rows=10&sortColumn=&sortDirection=&infId=458YRRY04VI3BBMI6Q8326869752&infSeq=3&searchWord=%EA%B8%B0%EC%83%81&sigunNm=%EC%88%98%EC%9B%90%EC%8B%9C&sigunFlag=41110
# https://data.gg.go.kr/portal/myPage/actKeyPage.do?tabIdx=1#actKey
# gyeonggiDataKey

# https://openapi.gg.go.kr/AWS1hourObser?MESURE_DE=20190101

dtDateList = seq(as.Date("2019-11-01"), as.Date("2019-11-30"), "1 day")

reqUrl = "https://openapi.gg.go.kr/AWS1hourObser"
reqIndex = "&pIndex=1"
reqSize = "&pSize=100"
reqType = "&type=json"
reqMesureTm = stringr::str_c("&MESURE_TM=", RCurl::curlEscape(stringr::str_conv("09", encoding = "EUC-KR")))
reqSigunNm = stringr::str_c("&SIGUN_NM=", RCurl::curlEscape(stringr::str_conv("수원시", encoding = "EUC-KR")))


dataL1 = tibble::tibble()
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 경기데이터드림 API (자료 요청)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
for (i in 1:length(dtDateList)) {

  sDate = format(dtDateList[i], "%Y%m%d")

  reqMesureDt = stringr::str_c("?MESURE_DE=", RCurl::curlEscape(stringr::str_conv(sDate, encoding = "EUC-KR")))

  resRes = httr::GET(
    stringr::str_c(reqUrl, reqMesureDt, reqMesureTm, reqSigunNm, reqIndex, reqSize, reqType)
  )

  resData = httr::content(resRes, as = 'text') %>%
    jsonlite::fromJSON()

  data = resData$AWS1hourObser$row[[2]]

  if (nrow(data) < 1) { next }

  dataL1 = dplyr::bind_rows(dataL1, data)
}


dataL2 = dataL1 %>%
  readr::type_convert() %>%
  dplyr::filter(
    stringr::str_detect(LEGALDONG_NM, regex("팔달구"))
  ) %>%
  dplyr::mutate(
    dtDate = readr::parse_date(as.character(MESURE_DE), "%Y%m%d")
    , dtMonth = lubridate::month(dtDate)
  ) %>%
  dplyr::select(LEGALDONG_NM, dtMonth, TP_INFO, HD_INFO) %>%
  mutate_all(~ifelse(. < -99, NA, .))

dataL3 = dataL2 %>%
  dplyr::group_by(dtMonth) %>%
  dplyr::summarise(
    meanTpInfo = mean(TP_INFO, na.rm = TRUE)
    , meanHdInfo = mean(HD_INFO, na.rm = TRUE)
  )

dataL3

# (2) 수원시에 있는 자동 기상관측 장비의 위치를 네이버 지도 API를 이용해 나타내세요. (10점)

# https://data.gg.go.kr/portal/data/service/selectServicePage.do?page=1&rows=10&sortColumn=&sortDirection=&infId=IW2T5YWJTM7NUZ2QPVU025865983&infSeq=1&order=&searchWord=%EC%9E%90%EB%8F%99%EA%B8%B0%EC%83%81

reqUrl = "https://openapi.gg.go.kr/Ggaotowetherequpinstl"
reqIndex = "&pIndex=1"
reqSize = "&pSize=100"
reqType = "&type=json"
reqSigunNm = stringr::str_c("?SIGUN_NM=", RCurl::curlEscape(stringr::str_conv("수원시", encoding = "EUC-KR")))

resRes = httr::GET(
  stringr::str_c(reqUrl, reqSigunNm, reqIndex, reqSize, reqType)
)

resData = httr::content(resRes, as = 'text') %>%
  jsonlite::fromJSON()

data = resData$Ggaotowetherequpinstl$row[[2]]

dataL1 = data %>%
  dplyr::filter(
    stringr::str_detect(REFINE_LOTNO_ADDR, regex("팔달구"))
  )


# i = 1
dataGeo = tibble::tibble()

for (i in 1:nrow(dataL1)) {
  keyword = dataL1$REFINE_ROADNM_ADDR[i]

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 네이버 API (지도)
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  reqUrl = "https://naveropenapi.apigw.ntruss.com/map-geocode/v2/geocode"
  reqQuery = stringr::str_c("?query=", RCurl::curlEscape(stringr::str_conv(keyword, encoding = "UTF-8")))

  reqId = globalVar$naverApigwApiKeyId
  reqPw = globalVar$naverApigwApiKey

  reqRes = httr::GET(
    stringr::str_c(reqUrl, reqQuery)
    , add_headers("X-NCP-APIGW-API-KEY-ID" = reqId, "X-NCP-APIGW-API-KEY" = reqPw, "Accept" = "application/json")
  )

  resData = httr::content(reqRes, as = 'text') %>%
    jsonlite::fromJSON()

  data = resData$addresses
  makerLabel = stringr::str_c("markers=type:n|size:mid|pos:", data$x, "%20", data$y, "|label:", i)
  tmpData = tibble::tibble(lon = data$x, lat = data$y, maker = makerLabel)
  dataGeo = dplyr::bind_rows(dataGeo, tmpData)
}

dataGeoL1 = dataGeo %>%
  readr::type_convert() %>%
  dplyr::summarise(
    meanLon = mean(lon, na.rm = TRUE)
    , meanLat = mean(lat, na.rm = TRUE)
  )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 네이버 API (지도 이미지)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
reqUrl = "https://naveropenapi.apigw.ntruss.com/map-static/v2/raster"
reqQuery = stringr::str_c(
  "?center="
  , RCurl::curlEscape(stringr::str_conv(dataGeoL1$meanLon, encoding = "UTF-8"))
  , ","
  , RCurl::curlEscape(stringr::str_conv(dataGeoL1$meanLat, encoding = "UTF-8"))
)
reqSize = "&w=300&h=300"
reqLevel = "&level=14"
reqMaker = stringr::str_c("&", paste(dataGeo$maker, collapse = "&"))

reqId = globalVar$naverApigwApiKeyId
reqPw = globalVar$naverApigwApiKey

reqRes = httr::GET(
  stringr::str_c(reqUrl, reqQuery, reqSize, reqLevel, reqMaker)
  , add_headers("X-NCP-APIGW-API-KEY-ID" = reqId, "X-NCP-APIGW-API-KEY" = reqPw)
)

jsonRes = httr::content(reqRes, as = "raw")
img = jpeg::readJPEG(jsonRes)
jpeg::writeJPEG(img, target = "KGU_map2.jpg")

mapImg = imager::load.image("KGU_map2.jpg")
plot(mapImg)

#*********************************************************************
# 과제 3
#*********************************************************************
# 네이버에서 제공하는 검색 API 가운데 카페 검색을 위한 API를 활용하려 합니다. 검색어는 "경기대학교"를 사용합니다. 이와 같은 API에 대한 https://developers.naver.com/docs/search/cafearticle/ 의문서서를 참고하여 다음의 질문에 대한 결과를 제시하세요

# (1) 100개의 검색결과에 포함된 본문 링크를 이용하여, 카페 게시글의 본문들을 크기가 100인 문자열벡터에 넣기 위한 프로그램을 작성하세요. 즉 10장 실습과정에서 대통령 취임사의 한 행이 하나의 문자열에 입력된 것과 같이 카페 게시글이 취임사에서의 한 행이라고 생각하면 됩니다. 물론 카페 게시글에 포함된 특수문자나 본문 내용과 상관 없는 부분(링크 주소 등)들은 사전에 제거해야 합니다. (10점)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 네이버 API (카페)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
keyword = "경기대학교"

addrEnc = RCurl::curlEscape(stringr::str_conv(keyword, encoding = "EUC-KR"))

reqUrl = "https://openapi.naver.com/v1/search/cafearticle.json"
reqQuery = stringr::str_c("?query=", RCurl::curlEscape(stringr::str_conv(keyword, encoding = "EUC-KR")))
reqDisplay = "&display=100"
reqStart = "&start=1000"
reqSort = "&sort=date"

reqId = globalVar$naverKeyId
reqPw = globalVar$naverKeyPw

resRes = httr::GET(
  stringr::str_c(reqUrl, reqQuery, reqDisplay, reqStart, reqSort)
  , add_headers("X-Naver-Client-Id" = reqId, "X-Naver-Client-Secret" = reqPw)
)

# JSON 파일
resData = httr::content(resRes, as = 'text') %>%
  jsonlite::fromJSON()

dataCafe = tibble(resData$items)

dplyr::tbl_df(dataCafe)

# (2) 100개의 카페 게시글을 이용해 단어-문서 행렬을 생성하여 가장 높은 출현 빈도를 갖는 단어 100개를 제시하세요. 이 과정에서 한글의 형태소 분석을 할 필요는 없습니다. 단 형태소 분석을 통해 단어-문서 행렬에 대해 정리하면 추가점수 10점을 얻을 수 있습니다. (10점과 함께 추가점수를 받으면 최대 20점)

# 단일 문장으로 변환
# sent = paste(dataCafe$description, collapse = " ")
sent = dataCafe$description

sentL1 = sent %>%
  str_replace_all("[0-9]+", " ") %>%
  str_replace_all("\\s+", " ") %>%
  str_replace_all("http[a-zA-Z0-9]+", "") %>%
  str_replace_all('\n', ' ') %>%
  str_replace_all('\t', ' ') %>%
  str_replace_all("[[:punct:]]", '') %>%
  str_replace_all("[\\$,]", '') %>%
  str_replace_all('<b>', '') %>%
  str_replace_all('</b>', '') %>%
  str_replace_all('\\(', '') %>%
  str_replace_all('\\)', '') %>%
  str_replace_all('\\)', '') %>%
  str_replace_all('shuttlekakaomobilitycomshuttles', '') %>%
  str_replace_all('zip', '')

# 텍스트 분석을 위한 말뭉치(Corpus) 생성
text = Corpus(VectorSource(sentL1)) #

# TermDocumentMatrix(용어-문서 행렬) 생성
tdm = TermDocumentMatrix(text)

# 키워드별 등장 빈도
data = data.frame(
  word = rownames(as.matrix(tdm))
  , freq = rowSums(as.matrix(tdm))
)

# (3) 앞에서 만든 단어-문서 행렬로부터 가장 출현 빈도가 높은 단어 100개를 이용한 워드 클라우드를 작성하세요. (10점)

# 출현빈도 상위 100개
dataL1 = data %>%
  dplyr::arrange(desc(freq)) %>%
  dplyr::top_n(100)

# 워드 클라우드
wordcloud2::wordcloud2(data = dataL1)

# (4) 연관규칙발견을 위한 분석방법을 통해 나타난 결과를 살펴보고 이 가운데 상위 10개의 규칙을 이용해 우리학교 이름으로 검색된 카페글의 특징을 제시하세요. (이 문제는 안 풀어도 되지만 만약 풀면 추가 점수 10점)

# 경기대학교와 상관성이 0.3 이상의 경우
tm::findAssocs(tdm, "경기대학교", 0.2)

tdmMat = as.matrix(tdm)
tdmMat = ifelse(tdmMat > 0, 1, 0)
tdmMatConv = t(tdmMat)

# 바이너리 코드를 transactions으로 변환하세요.
tdmTrans = as(tdmMatConv, "transactions")

# 최소규칙 지지도 = 0.01, 최소규칙 신뢰도 = 0.01로 규칙을 생성하세요.
# 오랜 시간 걸림 (3분 소요)
tdmRules = arules::apriori(
  tdmTrans
  , parameter = list(supp = 0.01, conf = 0.01)
)

# 10개의 규칙만 선택해서 그래프로 그려주세요.
set.seed(123)

tdmRulesPlot = head(sort(tdmRules, by = "lift"), 10)
plot(tdmRulesPlot, method = "graph", engine = "htmlwidget")

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

serviceName = "LSH0081"

library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(modelr)
library(magrittr)
library(gam)
library(spData)
library(sf)
library(cowplot)
library(utils)

# 파일 읽기
fileInfo = Sys.glob(paste(globalVar$inpPath, "covid_19_india.csv", sep = "/"))
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "UTF-8"))

summary(data)
colnames(data)

# 인도 전체 지점에 대한 사망률 및 누적 사망률 처리
dataL1 = data %>%
  readr::type_convert() %>%
  dplyr::mutate(
    dtDate = readr::parse_date(as.character(Date), "%d/%m/%y")
  ) %>%
  dplyr::group_by(dtDate) %>%
  dplyr::summarise(sumDeath = sum(Deaths, na.rm = TRUE)) %>%
  dplyr::arrange(dtDate) %>%
  dplyr::mutate(
    cumDeath = cumsum(sumDeath)
  )

summary(dataL1)

# 누적 사망률 시계열 시각화
minDate = as.Date(min(dataL1$dtDate, na.rm = TRUE) - 1)
maxDate = as.Date(max(dataL1$dtDate, na.rm = TRUE) + 3)

saveImg = sprintf("%s/Img_%s_%02d.png", globalVar$figPath, serviceName, 1)

Sys.setlocale("LC_TIME", "english")

# Visualization Using ggplot2
ggplot(data = dataL1, aes(x = dtDate, y = cumDeath / 1000000)) +
  # cowplot::theme_minimal_grid() +
  theme_bw() +
  geom_point() +
  geom_line() +
  scale_x_date(expand = c(0, 0), date_minor_breaks = "15 days", date_breaks = "15 days", date_labels = "%d %b", limits = as.Date(c(minDate, maxDate))) +
  scale_y_continuous(expand = c(0, 0), minor_breaks = seq(0, 15, 3), breaks = seq(0, 15, 3), limits = c(0, 15)) +
  labs(
    x = "Date [Day Month]"
    , y = "Cumulative Death [1,000,000 명]"
    , fill = NULL
    , colour = NULL
    , title = NULL
    , subtitle = NULL
    , caption = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "black")
    , axis.title.x = element_text(face = "bold", size = 18, colour = "black")
    , axis.title.y = element_text(face = "bold", size = 18, colour = "black", angle = 90)
    , axis.text.x = element_text(angle = 45, hjust = 1, size = 18, colour = "black")
    , axis.text.y = element_text(size = 18, colour = "black")
    , legend.title = element_text(face = "bold", size = 14, colour = "white")
    , legend.position = "none"
    , legend.justification = c(0, 0.96)
    , legend.key = element_blank()
    , legend.text = element_text(size = 14, face = "bold", colour = "white")
    , legend.background = element_blank()
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

# 회귀모형 학습
dataL2 = dataL1 %>%
  dplyr::filter(cumDeath > 0)

dplyr::tbl_df(dataL2)

Linear = lm(cumDeath ~ dtDate, data = dataL2)
Quadratic = lm(cumDeath ~ poly(dtDate, 2), data = dataL2)
Cubic = lm(cumDeath ~ poly(dtDate, 3), data = dataL2)
Gam = gam(cumDeath ~ s(dtDate), data = dataL2)


dataModel = dataL2 %>%
  modelr::data_grid(dtDate = modelr::seq_range(dtDate, nrow(dataL2))) %>%
  modelr::gather_predictions(Linear, Quadratic, Cubic, Gam)

dplyr::tbl_df(dataL2)

dataModelL2 = dataModel %>%
  dplyr::left_join(dataL2, c("dtDate" = "dtDate")) %>%
  dplyr::group_by(model) %>%
  dplyr::summarise(
    r = cor(pred, cumDeath)
    , rmse = Metrics::rmse(pred, cumDeath)
  ) %>%
  dplyr::arrange(rmse, r)

dplyr::tbl_df(dataModelL2)

# 회귀모형 학습 시계열 시각화
minDate = as.Date(min(dataL2$dtDate, na.rm = TRUE) - 1)
maxDate = as.Date(max(dataL2$dtDate, na.rm = TRUE) + 2)

Sys.setlocale("LC_TIME", "english")
saveImg = sprintf("%s/Img_%s_%02d.png", globalVar$figPath, serviceName, 2)

ggplot(data = dataL2, aes(x = dtDate, y = cumDeath / 1000000)) +
  theme_bw() +
  geom_point(shape = 1, size = 2.2) +
  geom_line(data = dataModel, aes(x = dtDate, y = pred / 1000000, col = model), size = 0.95) +
  scale_x_date(expand = c(0, 0), date_minor_breaks = "15 days", date_breaks = "15 days", date_labels = "%d %b", limits = as.Date(c(minDate, maxDate))) +
  scale_y_continuous(expand = c(0, 0), minor_breaks = seq(0, 15, 3), breaks = seq(0, 15, 3), limits = c(0, 15)) +
  labs(
    x = "Date [Day Month]"
    , y = "Cumulative Death"
    , fill = NULL
    , colour = NULL
    , title = NULL
    , subtitle = NULL
    , caption = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "black")
    , axis.title.x = element_text(face = "bold", size = 18, colour = "black")
    , axis.title.y = element_text(face = "bold", size = 18, colour = "black")
    , axis.text.x = element_text(angle = 45, hjust = 1, size = 16, colour = "black")
    , axis.text.y = element_text(size = 16, colour = "black")
    , legend.title = element_text(face = "bold", size = 14, colour = "white")
    , legend.position = "none"
    , legend.justification = c(0, 0.96)
    , legend.key = element_blank()
    , legend.text = element_text(size = 14, face = "bold", colour = "white")
    , legend.background = element_blank()
    # , text = element_text(family = font)
    # , plot.margin = unit(c(0, 12, 0, 0), "mm")
  ) +
  facet_wrap(~model) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


# 회귀모형 예측 시계열 시각화
dtStartDate = lubridate::ymd(dataL2$dtDate[nrow(dataL2)]) + lubridate::days(1)
dtEndDate = dtStartDate + lubridate::days(100)

dataPredData = data.frame(dtDate = seq.Date(dtStartDate, dtEndDate, "1 days"))

dplyr::tbl_df(dataPredData)

dataPredDataL1 = dataPredData %>%
  modelr::gather_predictions(Cubic, Linear, Gam, Quadratic)

dplyr::tbl_df(dataPredDataL1)
summary(dataPredDataL1)

minDate = as.Date(min(dataL2$dtDate, na.rm = TRUE) - 1)
maxDate = as.Date(max(dataPredDataL1$dtDate, na.rm = TRUE) + 2)

Sys.setlocale("LC_TIME", "english")
saveImg = sprintf("%s/Img_%s_%02d.png", globalVar$figPath, serviceName, 3)

ggplot(data = dataL2, aes(x = dtDate, y = cumDeath / 1000000)) +
  theme_bw() +
  geom_point(shape = 1, size = 2.2) +
  geom_line() +
  geom_line(data = dataPredDataL1, aes(x = dtDate, y = pred / 1000000, col = model), size = .6) +
  scale_x_date(expand = c(0, 0), date_minor_breaks = "15 days", date_breaks = "15 days", date_labels = "%d %b", limits = as.Date(c(minDate, maxDate))) +
  scale_y_continuous(expand = c(0, 0), minor_breaks = seq(0, 40, 5), breaks = seq(0, 40, 5), limits = c(0, 40)) +
  labs(
    x = "Date [Day Month]"
    , y = "Cumulative Death"
    , fill = NULL
    , colour = NULL
    , title = NULL
    , subtitle = NULL
    , caption = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "black")
    , axis.title.x = element_text(face = "bold", size = 18, colour = "black")
    , axis.title.y = element_text(face = "bold", size = 18, colour = "black", angle = 90)
    , axis.text.x = element_text(angle = 45, hjust = 1, size = 18, colour = "black")
    , axis.text.y = element_text(size = 18, colour = "black")
    , legend.title = element_text(face = "bold", size = 14, colour = "black")
    , legend.position = c(0, 1.0)
    , legend.justification = c(0, 0.96)
    , legend.key = element_blank()
    , legend.text = element_text(size = 14, colour = "black")
    , legend.background = element_blank()
    # , text = element_text(family = font)
    # , plot.margin = unit(c(0, 12, 0, 0), "mm")
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

serviceName = "LSH0081"

# log = log4r::create.logger()
# log4r::logfile(log) = paste0(globalVar$logPath, "/log4r_", format(Sys.time(), "%Y%m%d"), ".log")
# log4r::level(log) = "INFO"

# tryCatch(
#   expr = {
#     # 주 소스 코드
#     log4r::info(log, sprintf("%s", "[START] Main R"))
#
#   }
#   , warning = function(warning) { log4r::warn(log, warning) }
#   , error = function(error) { log4r::error(log, error) }
#   , finally = {
#     log4r::info(log, sprintf("%s", "[END] Main R"))
#   }
# )

library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(modelr)
library(magrittr)
library(gam)
library(spData)
library(sf)
library(cowplot)
library(utils)
library(Metrics)
library(ggpubr)
library(scales)
library(neuralnet)
library(plyr)

#************************************************
# [openxlsx] Read
#************************************************
# data = openxlsx::read.xlsx(fileList, sheet = 1)

#************************************************
# [openxlsx] Write
#************************************************
# wb = openxlsx::createWorkbook()
#
# openxlsx::addWorksheet(wb, "ggData")
# openxlsx::writeData(wb, "ggData", ggData, startRow = 1, startCol = 1)

# openxlsx::saveWorkbook(wb, file = paste0(globalVar$outPath, "/Survery_LSH0078.xlsx"), overwrite = TRUE)

#************************************************
# File Info
#************************************************
# fileInfo = Sys.glob(paste(globalVar$inpPath, "play.csv", sep = "/"))
# data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "UTF-8"))

# nameList = sort(unique(geoData_L1$sigungu_name))
# fileName = tools::file_path_sans_ext(fs::path_file(fileInfo))
# if (nrow(dataL2) < 1) { next }
# saveImg = sprintf("%s/Img_%s_%02d.png", globalVar$figPath, serviceName, 1)

#************************************************
# Data Info
#************************************************
# dplyr::mutate(
#   backColor = dplyr::case_when(
#     stringr::str_detect(sigungu_name, regex("태안군|서산시|당진시")) ~ "1"
#     , TRUE ~ "NA"
#   )
# )


#================================================================================
# 데이터는 Rda 파일 형태로 있어서 그냥 load해서 사용하시면 됩니다.
# load("csvdata.no.na.Rda")
# 변수가 6개 있는데 그 중 첫번째인 self.esteem(자아존중감)이 종속변수이고, 나머지 변수들이 독립변수입니다.
# 강의내용과 샘플코드를 활용해서 학생의 자아존중감을 예측해보세요. 영향력있는 변수는 무엇인가요?
#================================================================================

set.seed(1)

fileInfo = Sys.glob(paste(globalVar$inpPath, "csvdata.no.na.Rda", sep = "/"))
load(fileInfo)

data = csvdata.no.na

# colnames(data)

#====================================
# 데이터 분할  (0 - 1 변환)
#====================================
# 훈련 및 데이터 셋을 60:40으로 나누기 위한 인덱스 설정
ind = sample(1:nrow(data), nrow(data) * 0.6)
# ind = nrow(data)

# 해당 인덱스에 따라 자료 할당
# 표준화/정규화 수행 (평균-표준편차, 0-1 변환)
trainData = data[-ind,] %>%
  dplyr::mutate_each_(funs(scale), vars = c("sexw1.re", "self.confidence", "attachment", "monitor", "negative.parenting"))
# dplyr::mutate_each_(funs(scales::rescale) ,vars=c("sexw1.re", "self.confidence", "attachment", "monitor", "negative.parenting"))


testData = data[ind,] %>%
  dplyr::mutate_each_(funs(scale), vars = c("sexw1.re", "self.confidence", "attachment", "monitor", "negative.parenting"))
# dplyr::mutate_each_(funs(scales::rescale) ,vars=c("sexw1.re", "self.confidence", "attachment", "monitor", "negative.parenting"))

# trainData = data[-ind,]
# testData = data[ind,]


# 훈련 데이터셋 확인
dplyr::tbl_df(trainData)

# 테스트 데이터셋 확인
dplyr::tbl_df(testData)

allVar = colnames(trainData)
predictorVarList = allVar[!allVar %in% "self.esteem"]
predictorVar = paste(predictorVarList, collapse = " + ")
form = as.formula(paste("self.esteem ~", predictorVar, collapse = "+"))

resultData = data.frame()

#===============================================
# 다중선형회귀 방법
#===============================================
lmFit = lm(form, data = trainData)
# summary(lmFit)

# 영향력이 있는 변수 (self.confidence, attachment, negative.parenting)
stepAic = MASS::stepAIC(lmFit, direction = "both")
summary(stepAic)

predictions = predict(stepAic, newdata = testData)

resultData = dplyr::bind_rows(resultData
  , data.frame(
    type = "MLR"
    , pred = predictions
    , real = testData$self.esteem
  )
)

# 시각화
xAxis = resultData$pred
yAxis = resultData$real

corTest = cor.test(xAxis, yAxis)
corVal = round(corTest$estimate, 2)
pVal = round(corTest$p.value, 2)
biasVal = round(Metrics::bias(xAxis, yAxis), 2)
rmseVal = round(Metrics::rmse(xAxis, yAxis), 2)

summary(resultData)

saveImg = sprintf("%s/Img_%s_%02d.png", globalVar$figPath, serviceName, 1)

ggpubr::ggscatter(resultData, x = "pred", y = "real", color = "black", add = "reg.line", conf.int = TRUE, add.params = list(color = "blue", fill = "lightblue")) +
  stat_regline_equation(label.x = 0, label.y = 30, size = 5) +
  ggplot2::annotate("text", x = 0, y = 28, size = 5, label = sprintf("R = %s (P < %.3f)", corVal, pVal), hjust = 0) +
  ggplot2::annotate("text", x = 0, y = 26, size = 5, label = sprintf("Bias = %s", biasVal), hjust = 0) +
  ggplot2::annotate("text", x = 0, y = 24, size = 5, label = sprintf("RMSE = %s", rmseVal), hjust = 0) +
  xlim(0, 30) +
  ylim(0, 30) +
  theme_bw() +
  labs(title = NULL, x = "예측", y = "실제", subtitle = "다중선형회귀모형") +
  coord_equal() +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 6, height = 6, dpi = 600)


#================================================================================
# 통신회사 고객이탈 예측(분류문제에 인공신경망 활용)
# 인공신경망 사용(neuralnet::neuralnet)
# 종속변수: Churn (지난 달 이탈 여부)
# 독립변수:
#   ?고객이 이용중인 서비스: phone, multiple lines, internet, online security, online backup, device protection, tech support, and streaming TV and movies
# ?고객계정 관련 정보: how long they’ve been a customer(tenure), contract, payment method, paperless billing, monthly charges, and total charges
# ?고객의 인구통계학 정보 ? gender, age range, and if they have partners and dependents
# 참고1: https://www.kaggle.com/blastchar/telco-customer-churn/data#
# 참고2: 강의자료에서는 인공신경망을 수치예측용으로만 다뤘으니 분류용으로 다룬 예제 링크를 추가해 드립니다. 참 고하세요. https://www.datatechnotes.com/2017/10/neural-networks-example-in-r.html
#================================================================================

set.seed(1)

fileInfo = Sys.glob(paste(globalVar$inpPath, "WA_Fn-UseC_-Telco-Customer-Churn.csv", sep = "/"))

data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "UTF-8")) %>%
  readr::type_convert() %>%
  dplyr::mutate_if(is.character, as.factor) %>%
  dplyr::select(-customerID) %>%
  na.omit() %>%
  # dplyr::mutate_at(vars(Churn), funs(as.numeric)) %>%
  dplyr::mutate(
    gender = as.integer(gender) - 1
    , Partner = as.integer(Partner) - 1
    , Dependents = as.integer(Dependents) - 1
    , MultipleLines = as.integer(MultipleLines) - 1
    , InternetService = as.integer(InternetService) - 1
    , OnlineSecurity = as.integer(OnlineSecurity) - 1
    , TechSupport = as.integer(TechSupport) - 1
    , StreamingTV = as.integer(StreamingTV) - 1
    , StreamingMovies = as.integer(StreamingMovies) - 1
    , Contract = as.integer(Contract) - 1
    , PaperlessBilling = as.integer(PaperlessBilling) - 1
    , PaymentMethod = as.integer(PaymentMethod) - 1
    , Contract = as.integer(Contract) - 1
    , OnlineBackup = as.integer(OnlineBackup) - 1
    , DeviceProtection = as.integer(DeviceProtection) - 1
    , PhoneService = as.integer(PhoneService) - 1
    , Churn = as.integer(Churn) - 1
  )

#====================================
# 데이터 분할
#====================================
# 훈련 및 데이터 셋을 60:40으로 나누기 위한 인덱스 설정
ind = sample(1:nrow(data), nrow(data) * 0.6)
# ind = nrow(data)

# 해당 인덱스에 따라 자료 할당
# 표준화 수행
trainData = data[-ind,] %>%
  dplyr::mutate_each_(funs(scale), vars = c("SeniorCitizen", "tenure", "PhoneService", "MultipleLines", "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "Contract", "PaperlessBilling", "PaymentMethod", "MonthlyCharges", "TotalCharges"))
# dplyr::mutate_each_(funs(scales::rescale), vars=c("SeniorCitizen", "Dependents", "tenure", "MultipleLines", "InternetService", "OnlineSecurity", "TechSupport", "StreamingTV", "StreamingMovies", "Contract", "PaperlessBilling", "PaymentMethod", "MonthlyCharges", "TotalCharges"))

testData = data[ind,] %>%
  dplyr::mutate_each_(funs(scale), vars = c("SeniorCitizen", "tenure", "PhoneService", "MultipleLines", "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "Contract", "PaperlessBilling", "PaymentMethod", "MonthlyCharges", "TotalCharges"))
# dplyr::mutate_each_(funs(scales::rescale), vars=c("SeniorCitizen", "Dependents", "tenure", "MultipleLines", "InternetService", "OnlineSecurity", "TechSupport", "StreamingTV", "StreamingMovies", "Contract", "PaperlessBilling", "PaymentMethod", "MonthlyCharges", "TotalCharges"))

trainData = data[ind,]
testData = data[-ind,]

# 훈련 데이터셋 확인
dplyr::tbl_df(trainData)

# 테스트 데이터셋 확인
dplyr::tbl_df(testData)

allVar = colnames(trainData)
predictorVarList = allVar[!allVar %in% "Churn"]
predictorVar = paste(predictorVarList, collapse = "+")
form = as.formula(paste("Churn ~", predictorVar, collapse = "+"))

resultData = data.frame()


# lmFit = lm(form, data = trainData)
glmFitVarAll = glm(form, data = trainData, family = binomial)
# summary(lmFit)

# 영향력이 있는 변수 (self.confidence, attachment, negative.parenting)
stepAic = MASS::stepAIC(glmFitVarAll, direction = "both")
summary(stepAic)


bestForm = as.formula("Churn ~ SeniorCitizen + tenure + PhoneService +
    MultipleLines + InternetService + OnlineSecurity + OnlineBackup +
    DeviceProtection + TechSupport + Contract + PaperlessBilling +
    PaymentMethod + MonthlyCharges + TotalCharges")

#===============================================
# neuralnet 방법
#===============================================
neuralModel = neuralnet::neuralnet(
  formula = bestForm
  , data = trainData
  # , hidden = c(3)
  # , hidden = c(64)
  # , learningrate = 0.01
  # , threshold = 0.01
  # , stepmax = 50000
  # , rep = 2
  , act.fct = "logistic"
  # , algorithm = "rprop+"
  # , err.fct = "sse"
  # , threshold = 0.01
  # , linear.output=TRUE
  # , lifesign = "full"
)

saveImg = sprintf("%s/Img_%s_%02d.png", globalVar$figPath, serviceName, 2)

png(file = saveImg, width = 10, height = 5, units = "in", res = 1200)
plot(neuralModel, rep = 'best')
dev.off()

rsPredictions = neuralnet::compute(neuralModel, testData)
predictions = ifelse(rsPredictions$net.result > 0.5, 1, 0)

resultData = data.frame(
  pred = predictions
  , real = testData$Churn
)

# 시각화
xAxis = resultData$pred
yAxis = resultData$real

library(caret)
caret::confusionMatrix(as.factor(xAxis), as.factor(yAxis))

# 검증 측정을 위한 기초 설정
lmPred = ROCR::prediction(xAxis, yAxis)

# ROC 커브를 위한 설정
perform = ROCR::performance(lmPred, "tpr", "fpr")
plot(perform, main = 'ROC Curve')

# AUC 측정 : 1에 가까울수록 최고 성능 : 0.7146214332
ROCR::performance(lmPred, "auc")@y.values[[1]]

# 이항편차 측정 : 낮을수록 좋음 : 19.61
abdiv::binomial_deviance(xAxis, yAxis)


rsPredictions = neuralnet::compute(neuralModel, testData)
predictions = rsPredictions$net.result

resultData = data.frame(
  pred = predictions
  , real = testData$Churn
)

xAxis = resultData$pred
yAxis = resultData$real

corTest = cor.test(xAxis, yAxis)
corVal = round(corTest$estimate, 2)
pVal = round(corTest$p.value, 2)
biasVal = round(Metrics::bias(xAxis, yAxis), 2)
rmseVal = round(Metrics::rmse(xAxis, yAxis), 2)

summary(resultData)

saveImg = sprintf("%s/Img_%s_%02d.png", globalVar$figPath, serviceName, 3)

ggpubr::ggscatter(resultData, x = "pred", y = "real", color = "black", add = "reg.line", conf.int = TRUE, add.params = list(color = "blue", fill = "lightblue")) +
  stat_regline_equation(label.x = 0, label.y = 0.90, size = 5) +
  ggplot2::annotate("text", x = 0, y = 0.80, size = 5, label = sprintf("R = %s (P < %.3f)", corVal, pVal), hjust = 0) +
  ggplot2::annotate("text", x = 0, y = 0.70, size = 5, label = sprintf("Bias = %s", biasVal), hjust = 0) +
  ggplot2::annotate("text", x = 0, y = 0.60, size = 5, label = sprintf("RMSE = %s", rmseVal), hjust = 0) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_bw() +
  labs(title = NULL, x = "예측", y = "실제", subtitle = "인공신경망") +
  coord_equal() +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 6, height = 6, dpi = 600)

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH00000"


# fileInfo = Sys.glob(paste(globalVar$inpPath, "out.csv", sep = "/"))
# data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))
# saveImg = sprintf("%s/Img_%s_%s.png", globalVar$figPath, serviceName, fileName)


library(tidyverse)
library(openxlsx)
library(lubridate)
library(region5air)
library(openair)
library(TTR)

fileInfo = Sys.glob(paste(globalVar$inpPath, "오존오염도최종.xlsx", sep = "/"))
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))

data = openxlsx::read.xlsx(xlsxFile = fileInfo, sheet = 1)


dataL1 = data %>%
  tidyr::gather(-시도별, key = "key", value = "value") %>%
  dplyr::filter(시도별 != "총계") %>%
  dplyr::mutate(
    dtDate = readr::parse_datetime(key, "%Y..%m")
    # , month = lubridate::month(dtDate)
    # , weekday = lubridate::wday(dtDate, label = FALSE, abbr = FALSE)
  ) %>%
  dplyr::filter(
    시도별 == "서울"
  )


tsData = ts(dataL1$value, start = c(2019, 1), frequency = 12)

autoplot(decompose(dataL1$value))


# 자기상관함수
acf(tsData)

# 추세선 시각
plot(tsData, type = "l", col = "red")


# 3개월 단위 이동평균법으로 평활
smaData = TTR::SMA(tsData, n = 3)
plot(smaData)

# 예측 모형 생성
model = arima(tsData, order = c(1, 1, 10))

# 모형이 통계적으로 적절
# X-squared = 0.0010167828, df = 1, p-value = 0.9745621
Box.test(model$residuals, lag = 1, type = "Ljung")

fsData = forecast::forecast(model, h = 12)
fsTestData = forecast::forecast(model, h = 6)

# 향후 24개월 예측치 시각화
plot(fsData)

# 향후 6개월 예측치 시각화
plot(fsTestData)

# 시계열 요소 분해 시각화
stl(tsData, s.window = 10)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

serviceName = "LSH0080"

#======================================================================================
#nycflights13 데이터셋은 2013.1.1. ~ 2013.12.31. 동안의 미국 뉴욕 인근 3개의 국제공항인
#케네디(J.F.Kennedy)공항(JFK), 뉴어크(Newark)공항(EWR), 라과디아 공항(LGA)에서
#출발이 계획되어 있던 모든 비행편에 대한 비행 정보를 기록한 데이터이다.
#데이터셋에 있는 5개의 테이블 중 flights 테이블은 비행편에 대한 정보를 담고 있다.
#nycflights13::flights 데이터(336,776개 관측, 19개 변수)로부터 다음 질문에 답하라.
#(빈칸에 적절한 R코드를 작성하시오)
#======================================================================================

#1) 항공사 carrier별 빈도를 아래와 같은 막대그림으로 나타내라. 밑줄친 곳에 적당한 R표현은?
library(nycflights13)
data(flights)
str(flights)
tab.0 <- table(flights$carrier)
# _________________________________________________________
barplot(sort(tab.0), main = "NYC Flights 2013", horiz = TRUE, las = 1)
# _________________________________________________________


#2)그 중 상위 10개 항공사만으로 부 데이터 flights.1을 구성하여 이후 풀이에 적용하라.
#flights.1이 총 데이터를 커버하는 비율은 몇 퍼센트인가? 밑줄친 곳에 적당한 R표현은?

# 답 (출력). 98.3%
rk <- rank(-tab.0); rk
carrier.top10 <- names(tab.0)[rk <= 10]
flights.1 <- subset(flights, carrier %in% carrier.top10)
str(flights.1)
round(nrow(flights.1) / nrow(flights) * 100, 1)


#3)나쁜 항공편 badflight를 출발지연 dep_delay가 NA이거나 60분 이상인 경우로 정의하자.
#항공사 별 나쁜 항공편의 비율을 다음과 같이 순서 정렬된 표로 제시하라.
#밑줄친 곳에 적당한 R표현은?
flights.1$badflight <- as.factor(flights.1$dep_delay >= 60 | is.na(flights.1$dep_delay) == TRUE)
levels(flights.1$badflight) <- c("good", "bad")
tab.2 <- with(flights.1, table(carrier, badflight)); tab.2m <- addmargins(tab.2)
flights.1$carrier = forcats::fct_relevel(flights.1$carrier, c("EV", "9E", "MQ", "WN", "B6", "AA", "UA", "VX", "US", "DL"))
tab.2 <- with(flights.1, table(carrier, badflight)); tab.2m <- addmargins(tab.2)
round(2 * prop.table(tab.2m, 1), 3)

#4) 나쁜 항공편이 월(month)과 관련이 있는가를 아래와 같이 2원표와 모자이크 플롯으로 나타내라.
#나쁜 항공편 비율이 높은 3개 달은 6월, 7월, 12월이다. 밑줄친 곳에 적당한 R표현은?

tab.3 <- with(flights.1, table(month, badflight))
tab.3m <- addmargins(tab.3)
tab.3m = 2 * prop.table(tab.3m, 1)
round(tab.3m, 3)
mosaicplot(tab.3, color = c("gray", "red"), off = 0, shade = FALSE, main = "NYC Flights 2013")

## Part 2
# install.packages("Lahman")
library(Lahman)

#======================================================================================
#메이저리그(Lahman 팩키지의 Salaries, Batting, Pitching)의 2015년 데이터에 대하여 다음
#질문들에 답하라.
#======================================================================================
#1. Lahman 팩키지의 Teams 데이터로부터 1975년 이후 리그(American League, National League) 별
# 연 관중 수(attendance)의 시도표를 제시하고 주요 특징을 기술하라.
#도움말: aggregate()와 ts() 활용

data = Teams %>%
  dplyr::filter(lgID %in% c("AL", "NL"))

bb = aggregate(attendance ~ lgID + yearID, data = data, sum)
ts(bb)


#2. [앞 문제의 계속] 1975년 이후 Boston Redsox 팀에 대하여 관중 수 (= y축) 대 팀 승률(= x축)의 산점도를 만들어라
#(이때 좌표에 점 대신 연도의 마지막 두 숫자를 넣어라). LADodgers 팀에 대하여 같은 질문에 답하라. 두 그래프는 어떻게 다른가?
#도움말: 유사 코딩의 반복을 피하기 위해서 사용자 함수를 만들어 사용할 필요가 있다.

makePlot = function(inTeamId) {
  library(tidyverse)

  data = Teams %>%
    dplyr::filter(
      yearID > 1975
      , teamID == inTeamId
    ) %>%
    dplyr::mutate(
      ratio = W / (W + L) * 100
      , plotLabel = substr(yearID, 3, 4)
    )

  if (nrow(data) < 1) { next }

  plot(attendance ~ ratio, col = "white", data = data)
  text(attendance ~ ratio, labels = plotLabel, data = data)

}

# Boston Redsox
makePlot("BOS")

# LADodgers
makePlot("LAA")

#3. Lahman 팩키지의 Batting 데이터로부터 모든 선수의 활동년 수를 산출하여 히스토그램으로 제시하라.
#활동년 수가 가장 큰 선수를 찾아 그의 연도별 활동(G, AB, H)을 살펴보라.
# 위키피디어에서 그를 찾아보라) *G:게임수, AB:타석, H:안타
# 도움말: plyr::ddply() 함수와 which.max() 함수를 활용

library(plyr)

# 모든 선수이 활동년 수
hist(Batting$yearID)

#
data = plyr::ddply(Batting
  , .(playerID)
  , function(df) {
    data.frame(cnt = nrow(df))
  }
)

ind = which.max(data$cnt)
dataL1 = subset(Batting, playerID == data[ind,]$playerID)[, c("playerID", "yearID", "G", "AB", "H")]
dataL1

#4. [앞 문제의 계속] 메이저리그 역사에서 Career 타율(BA, batting average)이 가장 좋은선수는 누구인가?
#단, 총 타석 수를 5,000 이상으로 조건화한다. 그의 Career 안타수, Caree 타석수는 얼마였는가?
#(위키피디어에서 그를 찾아보라)
data = Batting %>%
  dplyr::group_by(playerID) %>%
  dplyr::summarise(
    sumH = sum(H, na.rm = TRUE)
    , sumAB = sum(AB, na.rm = TRUE)
  ) %>%
  dplyr::filter(sumAB >= 5000) %>%
  dplyr::mutate(BA = sumH / sumAB) %>%
  dplyr::arrange(desc(BA)) %>%
  dplyr::top_n(1)

data

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 (크몽, 오투잡)
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

serviceName = "LSH0080"

library(dplyr)
library(stringr)
library(httr)
library(rvest)
library(tidyverse)

trim = function(x) gsub("^\\s+|\\s+$", "", x)

# 테스트 1
keywordList = c("harry potter")

# 테스트 2
# keywordList = c("deep learning", "machine learning", "natural language processing")
url = "https://www.goodreads.com"

# start = proc.time()
multiBookData = tibble::tibble()
cnt = 0

# [Step 2] 페이지 중앙의 Search and browse books에 “harry potter”를 검색하세요. 이후 맨 아래에 페이
# 지를 변경해가며 URL의 특징을 파악하세요.
for (keywordInfo in keywordList) {

  # [Step 3] 수업시간에 배운 내용을 바탕으로 필요한 library를 호출하고, 제목, 저자, 평점, 평점의 수, 리뷰, 개수, 요약문에 관한 변수를 선언하세요.
  tmp_prefixUrl = paste0(url, "/search?utf8=%E2%9C%93")
  tmp_urlInit = modify_url(tmp_prefixUrl, query = list(query = keywordInfo))
  tmp_contentInit <- read_html(tmp_urlInit)

  # [Step 4] 이번 과제에서는 검색 키워드가 변화하더라도 마지막 페이지까지 Scraping을 수행할 수 있도록 코드를 구현하고자 합니다. 첫 번째 페이지를 기준으로 문제 1의 결과를 이용하여 아래 그림의 마지막 페이 지인 100을 정수 타입으로 변수에 할당할 수 있는 코드를 작성하세요. (변수에 100을 직접 할당하는 방식은 허용되지 않습니다.)
  tmp_lastPageList = tmp_contentInit %>%
    html_nodes(css = ".leftContainer > div > div > a") %>%
    html_text()
  tmp_lastPage = as.numeric(tmp_lastPageList[length(tmp_lastPageList) - 1])

  # [Step 5] Step 4의 결과를 바탕으로 처음 페이지부터 마지막 페이지까지 URL을 통해 접근할 수 있는 코드를 작성하세요.
  pageList = 1:tmp_lastPage
  # pageList = 1:2
  bookData = tibble::tibble()
  # pageInfo = 2

  for (pageInfo in pageList) {
    # https://www.goodreads.com/search?page=5&query=harry+potter&tab=books&utf8=%E2%9C%93

    # [Step 7] Step 6에서 수집된 URL의 개수를 이용하여 반복문을 작성하고, 이후 Step 6에서 수집된URL과 문 제 2의 결과를 이용하여 서적 제목의 링크로 이동할 수 있는 URL을 변수에 할당하세요. 결과적으로 반복문을 수행하며 페이지 내의 모든 서적 제목에 대해 지정된 링크로 이동하는 URL을 변수에 할당할 수 있는 코드를 작성하세요.
    tmp_url = modify_url(tmp_prefixUrl, query = list(query = keywordInfo, page = pageInfo)) %>% gsub('%2B', '+', .)

    tmp_content = read_html(tmp_url)
    tmp_titleLink = tmp_content %>%
      html_nodes(css = '.bookTitle') %>%
      html_attr("href")
    tmp_urlDtlList = paste0(url, tmp_titleLink)

    # 조회 결과 링크 목록
    for (tmp_urlDtlInfo in tmp_urlDtlList) {

      # tmp_title = NA
      # tmp_author = NA
      # tmp_score = NA
      # tmp_ratingNum = NA
      # tmp_reviewNum = NA
      # tmp_summary = NA

      tryCatch(
        expr = {
          tmp_contentDtl = read_html(tmp_urlDtlInfo)

          # [Step 8] 서적의 제목을 수집하는 코드를 작성하세요. 이 때, 서적의 제목에 두 글자 이상의 공백이 있다면 일괄적으로 한 칸의 공백으로 변경하세요. 이후 제목의 앞 뒤에 여백이 있다면 여백을 제거하세요.
          tmp_title = tmp_contentDtl %>%
            html_nodes(xpath = '//*[@id="bookTitle"]') %>%
            html_text() %>%
            trim()

          # [Step 9] 서적의 저자를 수집하는 코드를 작성하세요. 이 때, 저자의 이름 앞 뒤에 여백 또는 쉼표가 있다면 제거하고 저자의 이름이 ‘, ’로 구분되도록 수집하세요. (아래 예시에서는 J.K. Rowling, Mary GrandPre (Illustrator)와 같이 수집)
          tmp_author = tmp_contentDtl %>%
            html_nodes(xpath = '//*[@id="bookAuthors"]') %>%
            html_text() %>%
            gsub("\\s+", " ", .) %>%
            gsub("\"", "", .) %>%
            str_trim

          # [Step 10] 서적의 평점을 수집하는 코드를 작성하세요. 평점에 공백이 존재할 경우 제거하여 수집하도록 코드를 작성하세요.
          tmp_score = tmp_contentDtl %>%
            html_nodes(xpath = '//*[@id="bookMeta"]/span[2]') %>%
            html_text() %>%
            gsub("\\s+", " ", .) %>%
            gsub("\"", "", .) %>%
            str_trim %>%
            readr::parse_number()

          # [Step 11] 서적의 총 평점 개수를 수집하는 코드를 작성하세요. 평점의 개수는 정수의 형태로 수집되도록 코드를 작성하세요. (아래 예시에서는 7071903)
          tmp_ratingNum = tmp_contentDtl %>%
            html_nodes(xpath = '//*[@id="bookMeta"]/a[2]/meta') %>%
            html_attr("content") %>%
            gsub("\\s+", " ", .) %>%
            gsub("\"", "", .) %>%
            str_trim %>%
            readr::parse_number()

          # [Step 12] 서적의 총 리뷰 개수를 수집하는 코드를 작성하세요. 리뷰의 개수는 정수의 형태로 수집되도록 코드를 작성하세요. (아래의 예시에서는 112684)
          tmp_reviewNum = tmp_contentDtl %>%
            html_nodes(xpath = '//*[@id="bookMeta"]/a[3]/meta') %>%
            html_attr("content") %>%
            gsub("\\s+", " ", .) %>%
            gsub("\"", "", .) %>%
            str_trim %>%
            readr::parse_number()

          # [Step 13] 서적의 요약문을 수집하는 코드를 작성하세요. 페이지에 보이는 부분만을 수집하는 코드를 작성하면 됩니다. (아래 예시에서는 “…more”까지) 두 칸 이상의 공백이 존재할 경우 일괄적으로 한 칸 공백으로 변경하고, 앞 뒤에 여백이 있을 경우 제거하세요. 만일 요약문이 존재하지 않으면 한 칸 공백 (“ ”)를 수집된 요약문으로 간주하도록 코드를 작성하세요.
          tmp_summary = tmp_contentDtl %>%
            html_nodes(xpath = '//*[@id="description"]') %>%
            html_text() %>%
            gsub("\\s+", " ", .) %>%
            gsub("\"", "", .) %>%
            str_trim

          if (str_length(tmp_summary) < 1) { tmp_summary = "" }

          tmp_bookData = tibble::tibble(
            cnt = cnt
            , title = tmp_title
            , author = tmp_author
            , score = tmp_score
            , raingNumber = tmp_ratingNum
            , reviewNumber = tmp_reviewNum
            , summary = tmp_summary
          )

          bookData = dplyr::bind_rows(bookData, tmp_bookData)
        }
        , error = function(e) {

          print("An error occurs, skip the revie")

          # cat(sprintf(
          #   "%s : %s"
          #   , "An error occurs, skip the revie"
          #   , e
          # ), "\n")
        }
        , finally = {
          cnt = cnt + 1

          cat(sprintf(
            "keywordInfo : %10s | pageInfo : %5s | cnt : %5s"
            , keywordInfo
            , pageInfo
            , cnt
          ), "\n")
        }
      )
    }

  }
  tmp_multiBookData = tibble::tibble(keyword = keywordInfo, bookData)
  multiBookData = dplyr::bind_rows(multiBookData, tmp_multiBookData)
}


# [Step 14] Step 8 ~ Step 13의 결과를 이용하여 수집된 Data를 DataFrame 형태로 변환한 뒤 csv파일로 저장하세요. 이 때, 파일 이름은 “books.csv”로 설정하세요. 해당 파일은 본 과제의 제출물 중 하나입니다
saveFile = sprintf("%s/%s", globalVar$outPath, "books2.csv")
readr::write_csv(bookData, file = saveFile)

# [Step 15] Step 14의 결과는 정해진 1개의 Keyword인 “harry potter”로 검색했을 때 등장하는 모든 서적에 대한 제목, 저자, 평점, 평점의 수, 리뷰의 수, 요약문입니다. 이후의 Step에서는 복수 개의 Keyword에 대해 등장하는 모든 서적을 수집하고자 합니다. Good Reads 웹 사이트(https://www.goodreads.com/)로 돌아가서 중앙에 검색 키워드를 “the chronicles of narnia”, “the lord of the rings”로 변화시켜 가며 URL의 패턴을 파악하세요.
saveFile = sprintf("%s/%s", globalVar$outPath, "multi_books2.csv")
readr::write_csv(multiBookData, file = saveFile)

# [Step 16] 문제 3의 결과를 참고하여, modify_url 함수를 사용해 검색 키워드에 따라 페이지에 접근할 수 있는 URL을 만들어 보세요.
# Hint modify_url(url, query = list(q = search_keyword))

# [Step 17] 검색 키워드 “deep learning”, “machine learning”, “natural language processing”에 대해 수행되는 반복문을 작성하고, Step 16의 결과와 Step 14까지 작성한 코드를 바탕으로 자동적으로 3개 키워드에 대해 검색된 서적의 제목, 저자, 평점, 평점의 수, 리뷰의 수, 요약문을 수집하는 코드를 작성하세요. 수집된 결과가 검색 Keyword로 구분될 수 있도록 별도의 Column을 만들어 DataFrame을 만들고, 결과를 “multi_books.csv”로 저장하세요. 해당 파일은 본 과제의 제출물 중 하나입니다.
# Hint: 단순 반복문으로도 Step 17을 수행할 수 있지만, 필요하다면 함수를 사용하세요.
# Hint: books.csv와 multi_books.csv의 예시는 각각 다음과 같습니다. 임의로 몇 개의 Row를
# 제거했으므로 수집 내용은 고려하지 마세요.

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

serviceName = "LSH0084"

library(ISwR)
library(parameters)
library(tidyverse)
library(lme4)
library(glmmTMB)
library(Metrics)

#=========================================================================================
# 3. red.cell.folate 데이터는 ventilation 방법에 따른 folate 수치를 비교한 데이터이다.
#=========================================================================================
data(red.cell.folate)

#*************************************************************************************
# (a) ventilation 종류에 따른 folate 값을 비교하는 크루스칼-윌리스 검정을 시행하여라.
#*************************************************************************************
# 등분산 가정
pairwiseTest = pairwise.t.test(red.cell.folate$folate, red.cell.folate$ventilation, pool.sd = TRUE, conf.level = 0.95)

#*************************************************************************************
# (b) 등분산인지 검정하고 등산분이 아닐 때의 검정을 시행해보아라
#*************************************************************************************
# P값이 0.3508로서 귀무가설 기각하지 못함 (분산은 유사하다 == 등분산)
bartlett.test(red.cell.folate$folate ~ red.cell.folate$ventilation)

#*************************************************************************************
# (c) 앞서 분석한 것을 살펴보고 ventilation 종류별로 차이가 있는지 없는지 95% 신뢰도에서 결론을 말하여라
#*************************************************************************************
# P값이 0.0417로서 귀무가설 기각 (N2O+O2,24h과 N2O+O2,op 그룹의 평균은 다르다)
# P값이 0.3095로서 귀무가설 기각하지 못함 (N2O+O2,24h과 O2,24h 그룹의 평균은 같다)


#=========================================================================================
# 17. 다음 물음에 답하여라
#=========================================================================================

#*************************************************************************************
# (a) log(bwt)를 log(bpd)와 log(ad)로 다항회귀를 해보아라. 계수를 살펴보고 계수의 근삿값을 통해
# bwt를 bpd와 ad로 구하는 공식을 만들어보아라.
#*************************************************************************************
lmFit = lm(log(bwt) ~ log(bpd) + log(ad), data = secher)
summary(lmFit)

# log 기반으로 회귀계수 : log(bpd), log(ad)
lmFit$coefficients

# 회귀계수 : bpd, ad
exp(lmFit$coefficients)

#*************************************************************************************
# (b) cystfibr 데이터의 height에 관한 이차 함수로 pemax를 근사해보아라
#*************************************************************************************
lmFit = lm(pemax ~ height + I(height^2) + I(height^3), data = cystfibr)

summary(lmFit)

#*************************************************************************************
# (c) newdata와 predict 함수를 사용하여 pemax를 추정해보아라.
#*************************************************************************************
# 훈련 및 데이터 셋을 60:40으로 나누기 위한 인덱스 설정
ind = sample(1:nrow(cystfibr), nrow(cystfibr) * 0.6)

# 해당 인덱스에 따라 자료 할당
testData = cystfibr[-ind,]

# 실제 pemax
yObs = testData$pemax

# 예측 pemax
yHatPred = predict(lmFit, newdata = testData)

plot(yHatPred, yObs)

#+++++++++++++++++++++++++++++
# 검증 지수
#+++++++++++++++++++++++++++++
# 상관계수
cor(yHatPred, yObs)

# Bias
Metrics::bias(yHatPred, yObs)

# RMSE
Metrics::rmse(yHatPred, yObs)


#=========================================================================================
# 19. malaria 데이터는 여러 요인에 따른 말리리아 감염에 관한 데이터이다.
#=========================================================================================
#*************************************************************************************
# (a) malaria 질병 데이터에 대하여 말라리아 발생을 log (ab)와 age로 설명하는 logistic 모델을 세우고 회귀식을 구하여라.
#*************************************************************************************

# 회귀식 = (-0.6823 * log(ab)) + (-0.065 * age) + 2.572
glmFit = glm(mal ~ log(ab) + age, data = malaria, family = binomial)
summary(glmFit)

#*************************************************************************************
# (b) newdata 용법과 predict 함수를 사용하여 예측해 보아라
#*************************************************************************************
#+++++++++++++++++++++++++++++++++++++++
# 테스트 데이터로 newdata 설정
#+++++++++++++++++++++++++++++++++++++++
# 훈련 및 데이터 셋을 60:40으로 나누기 위한 인덱스 설정
ind = sample(1:nrow(malaria), nrow(malaria) * 0.6)

# 해당 인덱스에 따라 자료 할당
testData = malaria[-ind,]

# 실제 말라리아 발생률
yObs = as.numeric(as.character(testData$mal))

# 테스트셋을 이용한 예측 퇴직여부
yHatPred = predict.glm(glmFit, newdata = testData, type = "response")
yHatCate = ifelse(yHatPred > 0.5, 1, 0)

#+++++++++++++++++++++++++++++
# 검증 지수
#+++++++++++++++++++++++++++++
# 검증 측정을 위한 기초 설정
lmPred = ROCR::prediction(yHat, yObs)

# ROC 커브를 위한 설정
perform = ROCR::performance(lmPred, "tpr", "fpr")
plot(perform, main = 'ROC Curve')

# AUC 측정 : 1에 가까울수록 최고 성능 : 0.6927
ROCR::performance(lmPred, "auc")@y.values[[1]]

# 이항편차 측정 : 낮을수록 좋음 : 5.54
abdiv::binomial_deviance(yObs, yHat)

#*************************************************************************************
# (c) confint 함수를 사용하여 95% 신뢰구간을 구해보아라. 이를 그래프로 표현해 보아라.
#*************************************************************************************
# confint(glmFit, level = 0.95)
glmFitParam = model_parameters(glmFit)

ggplot(data = glmFitParam, aes(x = reorder(Parameter, desc(Parameter)))) +
  geom_pointrange(aes(y = Coefficient, ymin = CI_low, ymax = CI_high)) +
  geom_hline(yintercept = 0, lty = 2) +
  coord_flip() +
  xlab("Parameter") +
  ylab("Estimate")


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

serviceName = "LSH0085"

library(tidyverse)
library(lubridate)
library(openxlsx)
library(fs)
library(astsa)
library(tseries)
library(ggrepel)

# globalVar = new.env()
# globalVar$inpPath  = "."
# globalVar$outPath  = "."
# globalVar$figPath  = "."

#=================================================
# 1. Arima 모형을 이용한 데이터의 계절조정
#=================================================
fileInfo = Sys.glob(paste(globalVar$inpPath, "빈일자리(계절조정필요).xlsx", sep = "/"))
data = openxlsx::read.xlsx(fileInfo, sheet = 2)
# 시점	빈일자리_전체 (명)
# 2010 1/4	218,500
# 2010 2/4	213,181

# 사분기 자료 생성
tsData = ts(data$val, start = c(2010, 1), frequency = 4)

autoplot(tsData)


# STL은 다양한 상황에서 사용할 수 있는 강력한 시계열 분해 기법입니다. STL은 “Seasonal and Trend decomposition using Loess(Loess를 사용한 계절성과 추세 분해)”의 약자입니다. 여기에서 Loess는 비선형 관계를 추정하기 위한 기법입니다. STL기법은 R. B. Cleveland, Cleveland, McRae, & Terpenning (1990) 가 개발하였습니다.
# STL은 고전적인 분해, SEATS, X11을 뛰어넘는 몇 가지 장점을 가지고 있습니다:
# SEAT와 X11과는 다르게, STL은 월별이나 분기별 데이터를 포함하여 어떤 종류의 계절성도 다룰 수 있습니다.
# 계절적인 성분이 시간에 따라 변해도 괜찮습니다. 계절성분의 변화율을 사용자가 조절할 수 있습니다.
# 추세-주기의 매끄러운 정도를 사용자가 조절할 수 있습니다.
# 가끔 있는 이상값이 추세-주기와 계절성분에 영향을 주지 않게 만들 수 있습니다(즉, 사용자가 강력한 분해법을 명시할 수 있습니다). 하지만, 이상값은 나머지 성분(remainder)에 영향을 줄 것입니다.

# 옵션 정보
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/stl

# 계절 조정
decomp = stl(log(tsData), s.window = "periodic")
seasonData = exp(forecast::seasadj(decomp))

saveImg = sprintf("%s/Img_%s_%02d_%s.png", globalVar$figPath, serviceName, 1, "빈 일자리에 대한 계절조정 시계열")

autoplot(cbind("원본" = tsData, "계절 조정" = seasonData)) +
  labs(
    x = "년도"
    , y = "빈 일자리 [명]"
  ) +
  theme(
    text = element_text(size = 18)
    , legend.position = "top"
    , plot.margin = unit(c(0, 4, 0, 0), "mm")
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

# # 시계열 분해 그래프 생성
# autoplot(decompose(tsData))
#
#
# # 시계열 안정성 진단 및 검정
# # P값이 0.4548로서 귀무가설 기각하지 못함 (비정상 시계열)
# adf.test(tsData, alternative = c("stationary"), k = 0)
# # Dickey-Fuller = -2.3005676, Lag order = 0, p-value = 0.454803
#
# Box.test(tsData, type = c("Ljung-Box"))
# # X-squared = 29.011223, df = 1, p-value = 7.19602e-08
#
# # 로그 차분
# tsDataL1 = diff(log(tsData))
#
# plot(tsDataL1)
#
# adf.test(tsDataL1, alternative = c("stationary"), k = 0)
# # Dickey-Fuller = -5.3588359, Lag order = 0, p-value = 0.01
#
# Box.test(tsDataL1, type = c("Ljung-Box"))
# # X-squared = 0.24728101, df = 1, p-value = 0.6189961
#
# #***********************************
# #  자기 상관성 진단
# #***********************************
# fit = auto.arima(tsData)
#
# summary(fit)
# Box.test(fit$residuals, type = c("Ljung-Box"))
#
# predData = forecast::stlf(tsData)
# summary(predData)
#
# fit <- stl(tsData, t.window=13, s.window="periodic",
#            robust=TRUE)
#
# seasadj(fit)
# fit %>% seasadj() %>% naive() %>%
#   autoplot() + ylab("신규 구매 지수") +
#   ggtitle("계절성으로 조정된 데이터의 단순 예측값")
#
# autoplot(predData)
#
#
# predData = stl(tsData)
# summary(predData)
#
# autoplot(predData)
#
#
# tsData %>%
#   stl(t.window=13, s.window="periodic", robust=TRUE) %>%
#   autoplot()
#
#
#
# library(seasonal)
#
# predData = tsData %>%
#   seas(x11 = "")
#
# seasadj(predData)
#
# autoplot(tsData) +
#   autolayer(trendcycle(predData), series="추세") +
#   autolayer(seasadj(predData), series="계절성으로 조정된 것") +
#   ggtitle("전자 장비 지수의 X11 분해")
#
# # 5년 후까지 예측
# # pred = predict(fit, n.ahead = 5 * 4)
# #
# # ts.plot(tsData, exp(pred$pred), log = "y")
#
#
#
# plot.ts(tsData, type='o', xlab="시간", ylab="주당 수익", main="J&J의 주당 수익")
#
#
# logz = log(tsData)                 #로그수익
# dlogz = diff(logz)             #로그 수익의 1차 차분(즉 로그 수익률)
# Ddlogz = diff(dlogz, 4)        #주기 4 계절 차분
# plot.ts( cbind(tsData, logz, dlogz, Ddlogz), main="", type='o') #시도표
#
#
#
# # 자기 상관계수
# acf2(Ddlogz, 24)
#
# sarima()
#
# autoplot(decompose(dataL1$value))

#=================================================
# 2. 베버리지 곡선 그래프 나타내는 법
#=================================================
fileInfo = Sys.glob(paste(globalVar$inpPath, "데이터.csv", sep = "/"))
emptyData = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))

# 계절 조정된 데이터를 이용한 결원률
dataL1$yAxis
# 실업률
dataL1$xAxis

dataL1$time

# 2010 1/4 > Q1 > 2010.25
# 2010 2/4 > Q2 > 2010.50

dataL1 = emptyData %>%
  dplyr::bind_cols(tibble(seasonData)) %>%
  dplyr::mutate(
    yAxis = (seasonData / `경제활동인구 (명)`) * 100.0
  ) %>%
  dplyr::rename(
    "xAxis" = `실업률 (%)`
  ) %>%
  dplyr::mutate(
    sDate = stringr::str_sub(time, 1, 4)
    , dtDate = readr::parse_date(sDate, "%Y")
    , xran = readr::parse_number(sDate) + (readr::parse_number(stringr::str_sub(time, 6, 6)) / 4)
    , dtYear = lubridate::year(dtDate)
    , type = stringr::str_c("Q", stringr::str_sub(time, 6, 6))
    , label = stringr::str_c(stringr::str_sub(time, 3, 4), type)
    # , isDum = ifelse(xran >= 2014, 1, 0)
    , isDum = ifelse(xran >= 2015, 1, 0)
  )

dataL1$xran
dataL1$dtYear
dataL1$dtYear


saveImg = sprintf("%s/Img_%s_%02d_%s.png", globalVar$figPath, serviceName, 2, "베버리지 곡선")

ggplot(dataL1, aes(x = xAxis, y = yAxis, colour = factor(dtYear))) +
  geom_path() +
  geom_point(data = dataL1, aes(shape = type), size = 3) +
  # ggrepel::geom_text_repel(aes(label = label)) +
  ggrepel::geom_label_repel(aes(label = label), show.legend = FALSE) +
  labs(
    x = "실업률 [%]"
    , y = "결원율 [%]"
    , fill = NULL
    , colour = NULL
    , title = NULL
    , subtitle = NULL
    , caption = NULL
  ) +
  theme(
    text = element_text(size = 18)
    # , legend.position = "none"
    # , plot.margin=unit(c(4, 4, 4, 4), "mm")
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

#=================================================
# 3. 데이터 분석한 R 스크립트(회귀분석+더미변수)
#=================================================

# 종속변수 : 계절 조정된 데이터를 이용한 결원률
# 독립변수 : xran
lmFit = lm(yAxis ~ xran * isDum, data = dataL1)
# lmFit = lm(yAxis ~ xran + isDum + xran * isDum, data = dataL1)
summary(lmFit)

# 통계 계수
# round(coef(lmFit), 2)

#*************************************************
# 2014년 이후 : 통계적 유의 X
#*************************************************
# Call:
#   lm(formula = yAxis ~ xran * isDum, data = dataL1)
#
# Residuals:
#   Min       1Q   Median       3Q      Max
# -0.18525 -0.08612  0.00471  0.06709  0.15785
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) 69.408090  47.271926    1.47     0.15
# xran        -0.034175   0.023495   -1.45     0.15
# isDum       -0.084247  50.783268    0.00     1.00
# xran:isDum   0.000165   0.025231    0.01     0.99
#
# Residual standard error: 0.0983 on 39 degrees of freedom
# Multiple R-squared:  0.339,	Adjusted R-squared:  0.288
# F-statistic: 6.65 on 3 and 39 DF,  p-value: 0.000976


#*************************************************
# 2015년 이후 : 통계적 유의 O
#*************************************************
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)   2.378114  32.146424    0.07    0.941
# xran         -0.000852   0.015973   -0.05    0.958
# isDum       102.713170  39.348896    2.61    0.013 *
#   xran:isDum   -0.050876   0.019535   -2.60    0.013 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.0953 on 39 degrees of freedom
# Multiple R-squared:  0.378,	Adjusted R-squared:  0.33
# F-statistic: 7.89 on 3 and 39 DF,  p-value: 0.000312

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

serviceName = "LSH0086"

library(tidyverse)
library(lubridate)
library(openxlsx)

#*****************************************************
# 예시 파일 읽기
#*****************************************************
fileInfo = Sys.glob(paste(globalVar$inpPath, "예시.xlsx", sep = "/"))
data = openxlsx::read.xlsx(fileInfo, sheet = 1)

# model 값이 obs 값과 유사한 값으로 재배열되어야 합니다.
# 엑셀에서는 다음과 같이 필터를 사용하여 계산되었습니다.
# 1. Month를 1부터 432개로 변경한다.
# 2. obs값을 오름차순으로 변경한다.(month의 변동이 생김)-ABC행만을 필터로 포함
# 3. model값을 오름차순으로 변경한다.
# 4. 마지막 ABCD를 모두 필터하여 month를 오름차순한다.
# 이 순서로하였습니다.
# R로 구현가능할까요?

tmpData = data %>%
  dplyr::select(model) %>%
  dplyr::arrange(model)

dataL1 = data %>%
  dplyr::select(-model) %>%
  tibble::rowid_to_column() %>%
  dplyr::arrange(obs) %>%
  dplyr::bind_cols(tmpData) %>%
  dplyr::arrange(rowid)


#*****************************************************
# 동적 파일 읽기
#*****************************************************
fileInfo = Sys.glob(paste(globalVar$inpPath, "ACCESS1-3+1970-2005BG.csv", sep = "/"))
modelData = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "UTF-8")) %>%
  dplyr::select(-X1)

fileInfo = Sys.glob(paste(globalVar$inpPath, "real1970-2005.csv", sep = "/"))
obsData = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "UTF-8"))

seqList = seq(1, 22)
dataL2 = tibble::tibble()
dataL3 = tibble::tibble()

seqInfo = 2

for (seqInfo in seqList) {
  colModelName = colnames(modelData)[seqInfo]
  colObsName = colnames(obsData)[seqInfo]

  modelDataL1 = modelData %>%
    dplyr::select(colModelName) %>%
    dplyr::rename(
      "model" = colModelName
    ) %>%
    dplyr::arrange(model)


  obsDataL1 = obsData %>%
    dplyr::select(colObsName) %>%
    dplyr::rename(
      "obs" = colObsName
    ) %>%
    tibble::rowid_to_column() %>%
    dplyr::arrange(obs) %>%
    dplyr::bind_cols(modelDataL1) %>%
    dplyr::arrange(rowid) %>%
    dplyr::bind_cols(colModelName = colModelName, colObsName = colObsName)

  dataL2 = dplyr::bind_rows(dataL2, obsDataL1)

  obsDataL2 = obsDataL1 %>%
    dplyr::select(obs, model)

  if (seqInfo == 1) {
    dataL3 = obsDataL2
  } else {
    dataL3 = dplyr::bind_cols(dataL3, obsDataL2)
  }
}

saveFile1 = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "Row_Data_From_Model_Obs.csv")
readr::write_csv(dataL2, file = saveFile1)

saveFile2 = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "Col_Data_From_Model_Obs.csv")
readr::write_csv(dataL3, file = saveFile2)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

serviceName = "LSH0087"

# 제출내용 : R 파일 (1번~8번, 10번 문제를 수행한 코드) 1개, 9번 문제 는 #을 여러개 이용하여 설명하시오.
# 학번 :                                , 이름 :

if (!require("gapminder")) { install.packages("gapminder"); library(gapminder) }
if (!require("dplyr")) { install.packages("dplyr"); library(dplyr) }
if (!require(ggplot2)) { install.packages("ggplot2"); library(ggplot2) }
library(ggrepel)
library(tidyverse)

options("scipen" = 100, "digits" = 4)

data(gapminder)

# 변수 설명
# country : 국가이름
# continent : 국가가 속한 대륙의 이름
# year : 연도
# lifeExp : Life Expectancy, 기대 수명
# pop : Population, 인구수
# gdpPercap : GDP per capita, 1인당 국내 총생산액

# 1. 기본 폴더를 C:\data\ 로 정하시오.

# 2. 파일 구조보기
str(gapminder)

# 3. 데이터에 대한 요약 함수 사용

# 4. 변수별 결측값 계산

# 5. 대륙별 평균 gdpPercap을 평균 gdpPercap가 높은 대륙부터 정렬하여 출력
gapminder %>%
  dplyr::group_by(continent) %>%
  dplyr::summarise(meanGdp = mean(gdpPercap, na.rm = TRUE)) %>%
  dplyr::arrange(desc(meanGdp))

# 6. 국가별 gdp라는 변수를 생성하여 국내 총생산액을 계산하고 gdp.csv 라는 파일명으로 저장하시오.
# gdp는 pop와 gdpPercap의 곱으로 계산하시오.
gdpData = gapminder %>%
  dplyr::mutate(gdp = pop * gdpPercap)

# globalVar = new.env()
# globalVar$outPath = "."
saveFile = sprintf("%s/%s", globalVar$outPath, "gdp.csv")
readr::write_csv(gdpData, file = saveFile)

# gdpData %>%
#   dplyr::group_by(country) %>%
#   dplyr::summarise(
#     sumGdp = sum(gdp, na.rm = TRUE)
#   ) %>%
#   dplyr::arrange(desc(sumGdp))

# 7. 대륙별 gdp 합을 계산하여 gdp_sum이라는 변수에 저장하고 continent_gdp_sum.csv 라는 이름으로 저장하세요.
continentGdpData = gapminder %>%
  dplyr::mutate(gdp = pop * gdpPercap) %>%
  dplyr::group_by(continent) %>%
  dplyr::summarise(
    gdp_sum = sum(gdp, na.rm = TRUE)
  )

# globalVar = new.env()
# globalVar$outPath = "."
saveFile = sprintf("%s/%s", globalVar$outPath, "continent_gdp_sum.csv")
readr::write_csv(continentGdpData, file = saveFile)

# 8. gdp.csv에 저장된 데이터를 gdp라는 변수에 할당하시오.
# gdp를 이용하여 year가 2007인 데이터만 추출하여 gdp_2007 변수에 할당하시오.
# gdp_2007 데이터를 이용하여 그래프를 그리되 x축은 pop, y축은 lifeExp, size는 gdp, color는 continent로 지정하시오)
gdp = gdpData

gdp_2007 = gdp %>%
  dplyr::filter(year == 2007)

ggplot(data = gdp_2007, aes(x = pop, y = lifeExp, colour = continent, size = gdp)) +
  geom_point()

# gdp_2007 %>%
#   dplyr::group_by(continent) %>%
#   dplyr::summarise_all(funs(
#     mean(., na.rm = TRUE) # 평균
#   )) %>%
#   dplyr::arrange(desc(gdp))

# 9. 8번문제에서 나온 그림을 이용하여 다음에 대한 설명을 서술하시오.
# 대륙별로 인구, 기대수명, gdp의 특징은 무엇인가?

# 인구는 아시아, 미국, 유럽, 아프리카, 오세아니아 순으로 낮았고
# 기대수명의 경우 오세아니아, 유럽, 미국, 아시아, 아프리카 순으로 낮았다
# 그리고 GDP는 미국, 아시아, 유럽, 오세아니아, 아프리카 순으로 낮았다.

# 여기에 답을 추가하세요.

# 10. gdp_2007 데이터를 이용하여 대륙별, 국가별로 x축은 인구,
# y축은 기대수명,점의 크기는 gdp가 나오게 그림을 그리시오.

# 대륙별
ggplot(data = gdp_2007, aes(x = pop, y = lifeExp, size = gdp)) +
  geom_point() +
  facet_wrap(~continent, ncol = 3)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

serviceName = "LSH0088"

library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(shiny)
library(ggpubr)
library(GGally)
library(rnaturalearth)
library(sf)
library(RColorBrewer)

#************************************************
# Set Env
#************************************************
# globalVar = new.env()

# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

# globalVar$inpPath = "D:/04. TalentPlatform/Github/TalentPlatform-R/INPUT/o2job"
# globalVar$figPath = "D:/04. TalentPlatform/Github/TalentPlatform-R/FIG/o2job"
# globalVar$outPath = "D:/04. TalentPlatform/Github/TalentPlatform-R/OUTPUT/o2job"

#===========================================================================
# 1. Data Process
#===========================================================================
fileInfo1 = Sys.glob(paste(globalVar$inpPath, "Corporate-Tax-Rates-Data-1980-2019.csv", sep = "/"))
corporateTaxData = readr::read_csv(file = fileInfo1, locale = locale("ko", encoding = "UTF-8")) %>%
  dplyr::select(-iso_2, -iso_3) %>%
  dplyr::mutate(
    country = dplyr::case_when(
      stringr::str_detect(country, regex("United States of America")) ~ "United States"
      , TRUE ~ country
    )
  ) %>%
  readr::type_convert()

fileInfo2 = Sys.glob(paste(globalVar$inpPath, "HappyPlanetIndex.csv", sep = "/"))
happyPlanetIndexData = readr::read_csv(file = fileInfo2, locale = locale("ko", encoding = "UTF-8")) %>%
  dplyr::mutate(
    Country = dplyr::case_when(
      stringr::str_detect(Country, regex("United States of America")) ~ "United States"
      , TRUE ~ Country
    )
  ) %>%
  readr::type_convert() %>%
  dplyr::rename(
    "gdpPerCapita" = "GDPperCapita"
  )

fileInfo3 = Sys.glob(paste(globalVar$inpPath, "tax revenue GDP.xlsx", sep = "/"))
taxRevenueGdpData = openxlsx::read.xlsx(fileInfo3, sheet = 2) %>%
  dplyr::na_if("..") %>%
  tidyr::gather(-isOecd, -Country, key = "year", value = "perGdp") %>%
  readr::type_convert()

fileInfo4 = Sys.glob(paste(globalVar$inpPath, "Gini.xlsx", sep = "/"))
giniIndexData = openxlsx::read.xlsx(fileInfo4, sheet = 2) %>%
  dplyr::na_if("..") %>%
  tidyr::gather(-Country, -Methodology, -Unit, key = "year", value = "gini") %>%
  dplyr::filter(Methodology == "New income definition since 2012") %>%
  readr::type_convert()

data = corporateTaxData %>%
  dplyr::left_join(happyPlanetIndexData, by = c("country" = "Country")) %>%
  dplyr::left_join(taxRevenueGdpData, by = c("country" = "Country", "year" = "year")) %>%
  dplyr::left_join(giniIndexData, by = c("country" = "Country", "year" = "year")) %>%
  dplyr::left_join(gapminder::country_codes, by = c("country" = "country"))

dataL1 = data %>%
  dplyr::select(country, year, gdp, perGdp, gini, gdpPerCapita, isOecd, iso_alpha, rate) %>%
  rename("iso_a3" = "iso_alpha") %>%
  dplyr::mutate(
    dtDate = readr::parse_date(as.character(year), "%Y")
  )

#===========================================================================
# 2. Shiny App
#===========================================================================

#**********************************************
# a. 국가 선택 기능
# b. 세금이나 Gini  Index 등 선택 가능
# c. 연도 설정 가능
#**********************************************
dataL2 = dataL1 %>%
  tidyr::gather(-c(country, year, isOecd, iso_a3, dtDate, rate), key = "type", value = "val")

# ui = fluidPage(
#   titlePanel("Table From country, type, year")
#   , sidebarLayout(
#     sidebarPanel(
#       selectInput(inputId = "country", label = "country : ", choices = unique(dataL2$country))
#       , selectInput(inputId = "type", label = "type : ", choices = unique(dataL2$type))
#       , selectInput(inputId = "year", label = "year : ", choices = unique(dataL2$year))
#     )
#
#     , mainPanel(
#       tabPanel("table", DT::dataTableOutput("table"))
#     )
#   )
# )
#
# server = function(input, output) {
#
#   output$table <- DT::renderDataTable({
#     dataL2 %>%
#       filter(
#         country == input$country
#         , year == input$year
#         , type == input$type
#       ) %>%
#     DT::datatable()
#   })
# }
#
# shinyApp(ui, server)

shiny::runApp("ShinyApp")

#===========================================================================
# 3. Rmd
#===========================================================================

#**********************************************
# a. 세계지도에 corporate tax, tax revenue GDP %, GDP 각각 시각화 (모든 국가, %별로 그라데이션 구분, 2019 자료)
#**********************************************
world = ne_countries(type = "countries", returnclass = 'sf')

dataL3 = world %>%
  dplyr::left_join(dataL1, by = c("iso_a3" = "iso_a3")) %>%
  dplyr::filter(
    year == 2019
    , !is.na(iso_a3)
  )

valList = c("gdp", "perGdp", "gdpPerCapita", "rate")
# valInfo = "rate"

for (valInfo in valList) {
  saveImg = sprintf("%s/Img_%s_%02d_%s.png", globalVar$figPath, serviceName, 1, valInfo)
  plotSubtitle = sprintf("2019년 %s 시각화", valInfo)

  ggplot(dataL3, aes_string(fill = valInfo)) +
    geom_sf(data = world, fill = "#f0f0f0", colour = "white") +
    geom_sf(alpha = 0.8, colour = "white", size = 0.1) +
    scale_fill_gradientn(colours = brewer.pal(5, "Oranges"), na.value = NA) +
    labs(title = NULL, subtitle = plotSubtitle, caption = NULL, x = NULL, y = NULL) +
    theme_minimal() +
    theme(text = element_text(size = 18)) +
    ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)
}


#**********************************************
# b. 세계지도에  Gini Index, GDP per capita 각각 시각화(OECD국가)
#**********************************************
world = ne_countries(type = "countries", returnclass = 'sf')

dataL3 = world %>%
  dplyr::left_join(dataL1, by = c("iso_a3" = "iso_a3")) %>%
  dplyr::filter(
    !is.na(iso_a3)
    , !is.na(gini)
    , !is.na(gdpPerCapita)
    , isOecd == "Yes"
  )

yearList = sort(unique(dataL3$year))
valList = c("gini", "gdpPerCapita")
# yearInfo = "1990"
# valInfo  = "gini"

for (valInfo in valList) {
  for (yearInfo in yearList) {

    saveImg = sprintf("%s/Img_%s_%02d_%s_%s.png", globalVar$figPath, serviceName, 2, yearInfo, valInfo)
    plotSubtitle = sprintf("%s년 %s 시각화", yearInfo, valInfo)

    cat(saveImg, "\n")

    dataL4 = dataL3 %>%
      dplyr::filter(year == yearInfo)

    if (nrow(dataL4) < 1) { next }

    ggplot(dataL4, aes_string(fill = valInfo)) +
      geom_sf(data = world, fill = "#f0f0f0", colour = "white") +
      geom_sf(alpha = 0.8, colour = "white", size = 0.1) +
      scale_fill_gradientn(colours = brewer.pal(5, "Oranges"), na.value = NA) +
      labs(title = NULL, subtitle = plotSubtitle, caption = NULL, x = NULL, y = NULL) +
      theme_minimal() +
      theme(text = element_text(size = 18)) +
      ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)
  }
}


#**********************************************
# c. Corporate tax 와 GDP/Corporate tax 와 GDP per capita 관계 시각화(분포도 등)
#**********************************************
saveImg = sprintf("%s/Img_%s_%02d_%s.png", globalVar$figPath, serviceName, 3, "관계 시각화")

ggData = dataL1 %>%
  dplyr::select(gdp, perGdp, gini, gdpPerCapita) %>%
  GGally::ggpairs(.) %>%
  print(progress = FALSE)

ggData +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

#**********************************************
# d. Tax Revenue GDP%와 GINI 분포도
#**********************************************
saveImg = sprintf("%s/Img_%s_%02d_%s.png", globalVar$figPath, serviceName, 4, "Tax Revenue GDP와 GINI 분포도")

ggpubr::ggscatter(dataL1, x = "perGdp", y = "gini", color = "black", add = "reg.line", conf.int = TRUE, cor.coef = FALSE, add.params = list(color = "blue", fill = "lightblue")) +
  ggpubr::stat_cor(method = "pearson", label.x = 0, label.y = 0.8, size = 5) +
  ggpubr::stat_regline_equation(label.x = 0, label.y = 0.7, size = 5) +
  ylim(0, 0.8) +
  theme_bw() +
  labs(title = NULL, x = "Tax Revenue GDP%", y = "GINI", fill = NULL, colour = NULL, subtitle = "Tax Revenue GDP%와 GINI 분포도") +
  theme(
    text = element_text(size = 18)
    , legend.position = "none"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


#**********************************************
# e. 미국 Tax Revenue GDP% 와 GINI, GDP per capita 의 historical trend 시각화(시계열)
#**********************************************
dataL3 = dataL2 %>%
  dplyr::filter(
    type %in% c("perGdp", "gini", "gdpPerCapita")
    , country == "United States"
  )

saveImg = sprintf("%s/Img_%s_%02d_%s.png", globalVar$figPath, serviceName, 5, "Tax Revenue GDP와 GINI 분포도")

ggpubr::ggscatter(dataL3, x = "dtDate", y = "val", color = "type", add = "reg.line", conf.int = TRUE, cor.coef = FALSE, add.params = list(color = "blue", fill = "lightblue")) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 1.0, method = "pearson", size = 5) +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 0.90, size = 5) +
  theme_bw() +
  labs(title = NULL, x = "Date [Year]", y = "Value", fill = NULL, colour = NULL, subtitle = "미국 Tax Revenue GDP% 와 GINI, GDP per capita 의 historical trend") +
  theme(
    text = element_text(size = 18)
    , legend.position = "none"
  ) +
  facet_wrap(~type, scales = "free", ncol = 2) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================
serviceName = "LSH0089"

library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(scales)

#************************************************
# Set Env
#************************************************
globalVar = new.env()
globalVar$inpPath = "."
globalVar$figPath = "."
globalVar$outPath = "."

#==========================================================================
# 취업률 : 2019.11월과 2020.11월 까지의 내용을 기간별 하나의 곡선그래프로 표기해서 2019와 2020 한눈에 비교 가능하게 정리
#==========================================================================

fileInfo1 = Sys.glob(paste(globalVar$inpPath, "성_연령별_실업자_2019.xlsx", sep = "/"))
tmpData1 = openxlsx::read.xlsx(fileInfo1, sheet = 1)

fileInfo2 = Sys.glob(paste(globalVar$inpPath, "성_연령별_실업자_2020.xlsx", sep = "/"))
tmpData2 = openxlsx::read.xlsx(fileInfo2, sheet = 1)

fileInfo3 = Sys.glob(paste(globalVar$inpPath, "성_연령별_취업자_2019.xlsx", sep = "/"))
tmpData3 = openxlsx::read.xlsx(fileInfo3, sheet = 1)

fileInfo4 = Sys.glob(paste(globalVar$inpPath, "성_연령별_취업자_2020.xlsx", sep = "/"))
tmpData4 = openxlsx::read.xlsx(fileInfo4, sheet = 1)

data = dplyr::bind_rows(
  data.frame(type = "실업자", tmpData1, tmpData2)
  , data.frame(type = "취업자", tmpData3, tmpData4)
) %>%
  dplyr::select(-성별.1, -연령계층별.1)

dataL1 = data %>%
  tidyr::gather(-type, -성별, -연령계층별, key = "sDate", value = "value") %>%
  tidyr::spread(key = "성별", value = "value") %>%
  dplyr::mutate(
    dtDate = readr::parse_date(sDate, "X%Y..%m")
    , dtYear = lubridate::year(dtDate)
    , dtMonth = lubridate::month(dtDate)
  ) %>%
  dplyr::filter(
    연령계층별 == "계"
  )

dataL2 = dataL1 %>%
  dplyr::select(-남자, -여자) %>%
  tidyr::spread(key = "type", value = "계") %>%
  dplyr::mutate(
    총계 = (실업자 + 취업자)
    , 실업률 = (실업자 / 총계) * 100.0
    , 취업률 = (취업자 / 총계) * 100.0
  ) %>%
  tidyr::gather(-연령계층별, -sDate, -dtDate, -dtYear, -dtMonth, -실업자, -취업자, -총계, key = "key", value = "value") %>%
  dplyr::filter(
    dplyr::between(dtMonth, 1, 11)
  )

#==========================================================================
# 실업률 : 취업률과 동일
#==========================================================================
saveImg = sprintf("%s/Img_%s_%02d_%s.png", globalVar$figPath, serviceName, 1, "2019-2020년 1-11월 실업률-취업률")

ggplot(dataL2, aes(x = dtMonth, y = value, colour = factor(dtYear))) +
  geom_line(size = 2) +
  labs(x = "월", y = "비율", color = NULL, subtitle = "2019-2020년 1-11월 실업률/취업률") +
  scale_x_continuous(breaks = seq(1, 11, 1)) +
  # scale_x_date(date_minor_breaks = "1 months", date_breaks = "1 months", date_labels = "%Y-%m") +
  theme(
    text = element_text(size = 18)
    , legend.position = "top"
  ) +
  facet_wrap(~key, scale = "free", ncol = 1) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)


# saveImg = sprintf("%s/Img_%s_%02d_%s.png", globalVar$figPath, serviceName, 2, "2019-2020년 1-11월 취업률")
#
# ggplot(dataL2, aes(x = dtMonth, y = 취업률, colour = factor(dtYear))) +
#   geom_line(size = 2) +
#   labs(x = "월", y = "취업률 총계", color = NULL, subtitle = "2019-2020년 1-11월 취업률") +
#   scale_x_continuous(breaks = seq(1, 11, 1)) +
#   theme(text = element_text(size = 18)) +
#   ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)

#==========================================================================
# 취업률과 실업률 연령, 학력, 성별로 나눠서 막대그래프로 표기
#==========================================================================

#************************
# 성별
#************************
dataL1 = data %>%
  tidyr::gather(-type, -성별, -연령계층별, key = "sDate", value = "value") %>%
  tidyr::spread(key = "성별", value = "value") %>%
  dplyr::mutate(
    dtDate = readr::parse_date(sDate, "X%Y..%m")
    , dtYear = lubridate::year(dtDate)
    , dtMonth = lubridate::month(dtDate)
  ) %>%
  dplyr::filter(
    연령계층별 == "계"
  )

tmpData1 = dataL1 %>%
  dplyr::select(-계, -여자) %>%
  tidyr::spread(key = "type", value = "남자") %>%
  dplyr::mutate(type = "남자")

tmpData2 = dataL1 %>%
  dplyr::select(-계, -남자) %>%
  tidyr::spread(key = "type", value = "여자") %>%
  dplyr::mutate(type = "여자")

tmpData4 = dplyr::bind_rows(tmpData1, tmpData2)

tmpData5 = tmpData4 %>%
  dplyr::group_by(dtDate) %>%
  dplyr::summarise(
    총계 = sum(실업자, 취업자, na.rm = TRUE)
  )

dataL3 = tmpData4 %>%
  dplyr::left_join(tmpData5, by = c("dtDate" = "dtDate")) %>%
  dplyr::mutate(
    총계 = (실업자 + 취업자)
    , 실업률 = (실업자 / 총계) * 100.0
    , 취업률 = (취업자 / 총계) * 100.0
  ) %>%
  tidyr::gather(-c(연령계층별, sDate, dtDate, dtYear, dtMonth, 실업자, 취업자, type, 총계), key = "key", value = "value") %>%
  dplyr::mutate(
    xLabel = format(dtDate, "%Y-%m")
  )

saveImg = sprintf("%s/Img_%s_%02d_%s.png", globalVar$figPath, serviceName, 3, "성별에 따른 실업률-취업률")

ggplot(dataL3, aes(x = xLabel, y = value, fill = type, group = type, label = round(value, 0))) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.position = "top") +
  # geom_text(size = 5, vjust = 1.6, hjust = 0.5, color = "white") +
  labs(x = "월", y = "비율", fill = NULL, subtitle = "성별에 따른 실업률/취업률") +
  # scale_x_continuous(breaks = seq(1, 11, 1)) +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~key, scale = "free", ncol = 1) +
  ggsave(filename = saveImg, width = 8, height = 10, dpi = 600)


#************************
# 연령
#************************
dataL1 = data %>%
  tidyr::gather(-type, -성별, -연령계층별, key = "sDate", value = "value") %>%
  tidyr::spread(key = "연령계층별", value = "value") %>%
  dplyr::mutate(
    dtDate = readr::parse_date(sDate, "X%Y..%m")
    , dtYear = lubridate::year(dtDate)
    , dtMonth = lubridate::month(dtDate)
  ) %>%
  dplyr::filter(
    성별 == "계"
  )

tmpData1 = dataL1 %>%
  dplyr::group_by(dtDate) %>%
  dplyr::summarise(
    총계 = sum(c(`15 - 19세`, `15 - 24세`, `15 - 29세`, `15 - 64세`, `20 - 29세`, `30 - 39세`, `40 - 49세`, `50 - 59세`, `60세이상`), na.rm = TRUE)
  )


colnames(dataL3)
dataL3 = dataL1 %>%
  dplyr::left_join(tmpData1, by = c("dtDate" = "dtDate")) %>%
  dplyr::mutate(
    `15-19세` = (`15 - 19세` / 총계) * 100.0
    , `15-24세` = (`15 - 24세` / 총계) * 100.0
    , `15-29세` = (`15 - 29세` / 총계) * 100.0
    , `20-29세` = (`20 - 29세` / 총계) * 100.0
    , `30-39세` = (`30 - 39세` / 총계) * 100.0
    , `40-49세` = (`40 - 49세` / 총계) * 100.0
    , `50-59세` = (`50 - 59세` / 총계) * 100.0
    , `60세이상` = (`60세이상` / 총계) * 100.0
  ) %>%
  dplyr::select(-c(`15 - 19세`, `15 - 24세`, `15 - 29세`, `15 - 64세`, `20 - 29세`, `30 - 39세`, `40 - 49세`, `50 - 59세`)) %>%
  tidyr::gather(-type, -성별, -sDate, -계, -dtDate, -dtYear, -dtMonth, -총계, key = "key", value = "value") %>%
  dplyr::mutate(
    xLabel = format(dtDate, "%Y-%m")
  )

saveImg = sprintf("%s/Img_%s_%02d_%s.png", globalVar$figPath, serviceName, 4, "연령에 따른 실업률-취업률")

ggplot(dataL3, aes(x = xLabel, y = value, fill = key, group = key, label = round(value, 0))) +
  geom_bar(position = "dodge", stat = "identity") +
  # geom_text(size = 5, vjust = 1.6, hjust = 0.5, color = "white") +
  labs(x = "월", y = "비율", fill = NULL, subtitle = "연령에 따른 실업률/취업률") +
  # scale_x_continuous(breaks = seq(1, 11, 1)) +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  facet_wrap(~type, scale = "free", ncol = 1) +
  ggsave(filename = saveImg, width = 8, height = 10, dpi = 600)


#************************
# 학력
#************************
fileInfo1 = Sys.glob(paste(globalVar$inpPath, "연령_교육정도별_실업자_2019 분기.xlsx", sep = "/"))
tmpData1 = openxlsx::read.xlsx(fileInfo1, sheet = 1)

fileInfo2 = Sys.glob(paste(globalVar$inpPath, "연령_교육정도별_실업자_2020 분기.xlsx", sep = "/"))
tmpData2 = openxlsx::read.xlsx(fileInfo2, sheet = 1)

data = dplyr::bind_rows(
  data.frame(tmpData1, tmpData2)
) %>%
  dplyr::select(-연령계층별.1, -교육정도별.1) %>%
  dplyr::filter(
    연령계층별 == "계"
    , 교육정도별 != "계"
  ) %>%
  readr::type_convert()

dataL3 = data %>%
  tidyr::gather(-type, -교육정도별, -연령계층별, key = "sDate", value = "value") %>%
  dplyr::mutate(
    dtDate = readr::parse_date(sDate, "X%Y.%m.4")
    , dtYear = lubridate::year(dtDate)
    , dtMonth = lubridate::month(dtDate)
  ) %>%
  dplyr::mutate(
    type = "실업자"
    , xLabel = dplyr::case_when(
      stringr::str_detect(sDate, regex(".1.4")) ~ "1분기"
      , stringr::str_detect(sDate, regex(".2.4")) ~ "2분기"
      , stringr::str_detect(sDate, regex(".3.4")) ~ "3분기"
      , stringr::str_detect(sDate, regex(".4.4")) ~ "4분기"
      , TRUE ~ "NA"
    )
    , key2 = stringr::str_trim(교육정도별)
  )


saveImg = sprintf("%s/Img_%s_%02d_%s.png", globalVar$figPath, serviceName, 5, "학력에 따른 실업자")

ggplot(dataL3, aes(x = xLabel, y = value, fill = key2, group = key2, label = round(value, 0))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(position = position_dodge(width = 0.9), size = 4, vjust = 1.6, hjust = 0.5, color = "white") +
  labs(x = "분기", y = "실업자", color = NULL, fill = NULL, subtitle = "학력에 따른 실업자") +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  facet_wrap(~dtYear, scale = "free", ncol = 1) +
  ggsave(filename = saveImg, width = 12, height = 10, dpi = 600)


#==========================================================================
# 실업자 대비 구직급여 신청 동향 변동 각 년도별로 표기
# (실업자는 막대그래프로 / 구직급여 신청 동행은 곡선그래프)
#==========================================================================

#*******************************************
# 실업자는 막대그래프
#*******************************************
fileInfo1 = Sys.glob(paste(globalVar$inpPath, "성_연령별_실업자_2019.xlsx", sep = "/"))
tmpData1 = openxlsx::read.xlsx(fileInfo1, sheet = 1)

fileInfo2 = Sys.glob(paste(globalVar$inpPath, "성_연령별_실업자_2020.xlsx", sep = "/"))
tmpData2 = openxlsx::read.xlsx(fileInfo2, sheet = 1)

data = dplyr::bind_rows(
  data.frame(type = "실업자", tmpData1, tmpData2)
) %>%
  dplyr::select(-성별.1, -연령계층별.1) %>%
  dplyr::filter(
    성별 == "계"
    , 연령계층별 == "계"
  )

dataL3 = data %>%
  tidyr::gather(-type, -성별, -연령계층별, key = "sDate", value = "value") %>%
  # tidyr::spread(key = "성별", value = "value") %>%
  dplyr::mutate(
    dtDate = readr::parse_date(sDate, "X%Y..%m")
    , dtYear = lubridate::year(dtDate)
    , dtMonth = lubridate::month(dtDate)
  )

saveImg = sprintf("%s/Img_%s_%02d_%s.png", globalVar$figPath, serviceName, 1, "2019-2020년 실업자 막대 그래프")

ggplot(dataL3, aes(x = dtDate, y = value, fill = value, group = value, label = round(value, 0))) +
  geom_bar(position = "dodge", stat = "identity", alpha = 0.6) +
  geom_text(position = position_dodge(width = 0.9), size = 4, vjust = 1.6, hjust = 0.5, color = "white") +
  labs(x = "월", y = "실업자", color = NULL, fill = NULL, subtitle = "2019-2020년 실업자 막대 그래프") +
  scale_x_date(date_minor_breaks = "1 months", date_breaks = "1 months", date_labels = "%Y-%m") +
  guides(fill = guide_colourbar(barwidth = 20.0, barheight = 1.0)) +
  scale_fill_gradientn(colours = rainbow(10)) +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  facet_wrap(~type, scale = "free") +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)

#*******************************************
# 구직급여 신청 동행은 곡선그래프
#*******************************************
fileInfo1 = Sys.glob(paste(globalVar$inpPath, "구직급여_신청_동향_월__2019.xlsx", sep = "/"))
tmpData1 = openxlsx::read.xlsx(fileInfo1, sheet = 1)

fileInfo2 = Sys.glob(paste(globalVar$inpPath, "구직급여_신청_동향_월__2020.xlsx", sep = "/"))
tmpData2 = openxlsx::read.xlsx(fileInfo2, sheet = 1)

data = dplyr::bind_rows(
  data.frame(type = "구직자", tmpData1, tmpData2)
) %>%
  dplyr::select(-항목.1)

dataL1 = data %>%
  tidyr::gather(-type, -항목, key = "sDate", value = "value") %>%
  # tidyr::spread(key = "성별", value = "value") %>%
  dplyr::mutate(
    dtDate = readr::parse_date(sDate, "X%Y..%m")
    , dtYear = lubridate::year(dtDate)
    , dtMonth = lubridate::month(dtDate)
  )

saveImg = sprintf("%s/Img_%s_%02d_%s.png", globalVar$figPath, serviceName, 2, "2019-2020년 실업자 대비 구직급여 신청 동향 변동")

ggplot(dataL1, aes(x = dtDate, y = value, colour = factor(항목))) +
  geom_line(size = 2) +
  labs(x = "월", y = "신청 동향", color = NULL, subtitle = "2019-2020년 실업자 대비 구직급여 신청 동향 변동") +
  scale_x_date(date_minor_breaks = "1 months", date_breaks = "1 months", date_labels = "%Y-%m") +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "top"
  ) +
  facet_wrap(~항목, scale = "free", ncol = 1) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

#************************************************
# 요구사항
#************************************************
# 전에 드린 mod값을 obs에 일치시키는 랜덤포레스트와 svm을 구축하는것입니다!
# 구축하는데 있어서 랜덤포레스트는 매개변수 최적값을 산정해주실수 있는지 궁금합니다!
# 앙상블 형태로 7 3으로 분할부탁드립니다!
# 검증 지수 : 상관계수 (R), 유의수준 (p-Value), 편이 (Bias), 평균제곱근오차 (RMSE), %Bias, %RMSE로 하면 될까요?

serviceName = "LSH0091"

library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(randomForest)
library(mlbench)
library(caret)
library(doParallel)
library(tictoc)
library(mlbench)
library(e1071)
library(modelr)
library(parallel)

#************************************************
# Set Env
#************************************************
# globalVar = new.env()
globalVar$inpPath = "."
globalVar$figPath = "."
globalVar$outPath = "."

#************************************************
# Set Fun
#************************************************
perfEval = function(x, y) {

  if (length(x) < 1) { return(sprintf("%s", "x 값 없음")) }
  if (length(y) < 1) { return(sprintf("%s", "y 값 없음")) }

  slope = coef(lm(y ~ x))[2]
  interp = coef(lm(y ~ x))[1]
  xMean = mean(x, na.rm = TRUE)
  yMean = mean(y, na.rm = TRUE)
  xSd = sd(x, na.rm = TRUE)
  ySd = sd(y, na.rm = TRUE)
  cnt = length(x)
  bias = mean(x - y, na.rm = TRUE)
  rBias = (bias / yMean) * 100.0
  rmse = sqrt(mean((x - y)^2, na.rm = TRUE))
  rRmse = (rmse / yMean) * 100.0
  r = cor(x, y)
  r2 = cor(x, y)^2
  diffMean = mean(x - y, na.rm = TRUE)
  diffSd = sd(x - y, na.rm = TRUE)
  # perDiffMean = mean((x - y) / y, na.rm = TRUE) * 100.0

  return(c(slope, interp, xMean, yMean, xSd, ySd, cnt, bias, rBias, rmse, rRmse, r, r2, diffMean, diffSd))
}

# 검증 지수 테이블 생성
perfTable = data.frame(matrix(0, nrow = 2 * 22, ncol = 15))
rownames(perfTable) = c(paste0("RF-", 1:22), paste0("SVM-", 1:22))
colnames(perfTable) = c("slope", "interp", "xMean", "yMean", "xSd", "ySd", "cnt", "bias", "rBias", "rmse", "rRmse", "r", "r2", "diffMean", "diffSd")

# fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0086_Row_Data_From_Model_Obs.csv", sep = "/"))
# data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "UTF-8"))

fileInfo1 = Sys.glob(paste(globalVar$inpPath, "LSH0091_real1970-2005.csv", sep = "/"))
tmpData1 = readr::read_csv(file = fileInfo1, locale = locale("ko", encoding = "UTF-8"))

fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0091_Amon ACCESS1-3 1970-2005.csv", sep = "/"))
tmpData2 = readr::read_csv(file = fileInfo2, locale = locale("ko", encoding = "UTF-8"))


colList = colnames(tmpData1)
rfPredData = data.frame()
svmPredData = data.frame()
# i = 1

for (i in 1:length(colList)) {
  data = data.frame(tmpData1[, i], tmpData2[, i]) %>%
    magrittr::set_colnames(c("obs", "model"))

  dataL1 = data %>%
    dplyr::filter(
      !is.na(obs)
      , !is.na(model)
    )

  #=====================================================================
  # 훈련 및 테스트 셋 설정 (70 : 30)
  #=====================================================================
  set.seed(1)

  # 훈련 및 데이터 셋을 60:40으로 나누기 위한 인덱스 설정
  # ind = sample(1:nrow(dataL1), nrow(dataL1) * 0.7)
  ind = 1:302

  # 해당 인덱스에 따라 자료 할당
  trainData = dataL1[ind,]
  testData = dataL1[-ind,]

  # 훈련 데이터셋 확인
  dplyr::tbl_df(trainData)

  # 테스트 데이터셋 확인
  dplyr::tbl_df(testData)

  #=====================================================================
  # 랜덤포레스트 (RF)
  #=====================================================================

  #***********************************
  # 학습
  #***********************************
  tictoc::tic()

  # [시작] 병렬 처리
  # oSocClu = parallel::makePSOCKcluster(parallel::detectCores())
  # doParallel::registerDoParallel(oSocClu)

  rfModel = caret::train(
    obs ~ model
    , data = trainData
    , method = "rf"
    , tuneGrid = expand.grid(.mtry = 1:2)
    , trControl = caret::trainControl(method = "repeatedcv", number = 10, repeats = 1, search = "grid") # 10분할 교차 검증 및 1번 반복
  )

  # [종료] 병렬 처리
  # stopCluster(oSocClu)

  tictoc::toc()

  ggplot(rfModel)

  #***********************************
  # 검증
  #***********************************
  yObs = testData$obs
  yHat = predict(rfModel, newdata = testData)

  tmpData = data.frame(yHat) %>%
    magrittr::set_colnames(paste0("RF-", colList[i]))

  if (ncol(rfPredData) == 0) {
    rfPredData = tmpData
  } else {
    rfPredData = dplyr::bind_cols(rfPredData, tmpData)
  }

  perfTable[i,] = round(perfEval(yHat, yObs), 2)
  perfTable

  #=====================================================================
  # 서포트벡터 (SVM)
  #=====================================================================

  #***********************************
  # 학습
  #***********************************
  tictoc::tic()

  # [시작] 병렬 처리
  # oSocClu = parallel::makePSOCKcluster(parallel::detectCores())
  # doParallel::registerDoParallel(oSocClu)

  svmModel = caret::train(
    obs ~ model
    , data = trainData
    , method = "svmLinear"
    , tuneGrid = expand.grid(.C = 2^(seq(-5, 5, 2)))
    # , preProcess = c("center", "scale")
    , trControl = caret::trainControl(method = "repeatedcv", number = 10, repeats = 1, search = "grid") # 10분할 교차 검증 및 1번 반복
  )

  # [종료] 병렬 처리
  # stopCluster(oSocClu)

  tictoc::toc()

  # ggplot(svmModel)

  #***********************************
  # 검증
  #***********************************
  yObs = testData$obs
  yHat = predict(svmModel, newdata = testData)

  tmpData = data.frame(yHat) %>%
    magrittr::set_colnames(paste0("SVM-", colList[i]))

  if (ncol(svmPredData) == 0) {
    svmPredData = tmpData
  } else {
    svmPredData = dplyr::bind_cols(svmPredData, tmpData)

  }

  perfTable[i + 22,] = round(perfEval(yHat, yObs), 2)
  perfTable
}


#=====================================================================
# 검증 결과 출력
#=====================================================================
saveXlsxFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "Pred_Data_From_Model_Obs.xlsx")

wb = openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "perfTable")
openxlsx::writeData(wb, "perfTable", perfTable, startRow = 1, startCol = 1, colNames = TRUE, rowNames = TRUE)

openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)

saveCsvFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "Pred_Data_From_Model_Obs.csv")
utils::write.csv(perfTable, file = saveCsvFile)

saveRfCsvFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "RF_Pred_Data_From_Model_Obs.csv")
utils::write.csv(rfPredData, file = saveRfCsvFile)

saveSvmCsvFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "SVM_Pred_Data_From_Model_Obs.csv")
utils::write.csv(svmPredData, file = saveSvmCsvFile)

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

#************************************************
# 요구사항
#************************************************
# 다음에 딥러닝 부탁드릴때 다시연락드리겠습니다!
# 딥러닝 인풋자료는 오늘 과 같은데 시간은 얼마나 걸릴까요?
# 활성함수 relu이고, 최종값과 최종값이 도출되기 전 마지막 레이어 값을 도출하는 겁니다!

#************************************************
# 문의사항
#************************************************
# 1. 딥러닝된 수치 결과값이 csv 파일로 필요해서 추가부탁드리겠습니다. 월단위 강수량으로 부탁드리겠습니다 (마지막 레이어 및 최종 결과물).
# A. 마지막 레이어 (LSH0092_DL_Last_Layer_Data_From_Model_Obs_3-1000_TRUE.csv) 및 수치 결과값 (LSH0092_DL_Pred_Data_From_Model_Obs_3-1000_TRUE.csv)은 다음과 같이 첨부하였습니다.

# 월 단위 강수량의 경우 트레이닝 및 테스트를 어떻게 수행하여 도출해야 되나요?
# 한 예로서 전체 460개 데이터셋에서 365개 트레이닝 셋으로 30개씩 테스트 셋으로 학습해주세요?

# 2. 최적의 모형을 선정하여 계산된 결과인지 여쭤보고 싶습니다!
# A. 10번의 교차 검증 (nfolds = 10)을 수행하여 최적의 결과를 도출합니다.

# 3. learning ratio는 몇으로 설정하셨는지 궁금합니다!
# A. 트레이닝 및 테스트 비율은 각각 302:132로 설정하였고 레이어 개수 (layerInfo) = 3, 학습 횟수 (epochsInfo) = 1000 파라미터로 구동하였습니다. 이 과정에서 10번의 교차 검증 (nfolds = 10)을 수행하여 최적의 결과를 도출합니다.

# 4. 레이어 결과물이 현재 한자리수 소수점으로 나오는데 혹시 표준화 된결과인지 궁금합니다!
# A. "LSH0092_DL_Last_Layer_Data_From_Model_Obs_3-1000_TRUE.csv" MS사의 vscode 편집기 열어보면 소수점 10 자리 이상으로 보입니다. 그리고 표준화된 결과는 아닙니다.

# 저 그리고 마지막으로 하나만 더 부탁드리겠습니다. x값이 epochs이고 y값이 loss의 플럿을 보고싶습니다!
# A. 연속 변수에 대한 예측이기 때문에 loss 함수를 평균제곱근오차 (RMSE)로 대체하였습니다.

serviceName = "LSH0092"

library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(h2o)
library(log4r)

#================================================
# Set Env
#================================================
globalVar = new.env()
globalVar$inpPath = "."
globalVar$figPath = "."
globalVar$outPath = "."
globalVar$logPath = "."

#================================================
# Set Fun
#================================================
log = log4r::create.logger()
log4r::logfile(log) = paste0(globalVar$logPath, "/log4r_", format(Sys.time(), "%Y%m%d"), ".log")
log4r::level(log) = "INFO"

perfEval = function(x, y) {

  if (length(x) < 1) { return(sprintf("%s", "x 값 없음")) }
  if (length(y) < 1) { return(sprintf("%s", "y 값 없음")) }

  slope = coef(lm(y ~ x))[2]
  interp = coef(lm(y ~ x))[1]
  xMean = mean(x, na.rm = TRUE)
  yMean = mean(y, na.rm = TRUE)
  xSd = sd(x, na.rm = TRUE)
  ySd = sd(y, na.rm = TRUE)
  cnt = length(x)
  bias = mean(x - y, na.rm = TRUE)
  rBias = (bias / yMean) * 100.0
  rmse = sqrt(mean((x - y)^2, na.rm = TRUE))
  rRmse = (rmse / yMean) * 100.0
  r = cor.test(x, y)$estimate
  p = cor.test(x, y)$p.value
  diffMean = mean(x - y, na.rm = TRUE)
  diffSd = sd(x - y, na.rm = TRUE)
  # perDiffMean = mean((x - y) / y, na.rm = TRUE) * 100.0

  return(c(slope, interp, xMean, yMean, xSd, ySd, cnt, bias, rBias, rmse, rRmse, r, p, diffMean, diffSd))
}

biasCorr = function(actu, pred, minVal, maxVal, interVal, isPlot = FALSE) {

  factorVal = seq(minVal, maxVal, by = interVal)

  # RMSE Fitting
  liResult = lapply(1:length(factorVal), function(i) Metrics::rmse(actu, pred * factorVal[i])) %>%
    unlist()

  ind = which(liResult == min(liResult, na.rm = TRUE))

  if (isPlot == TRUE) {
    plot(liResult)
  }

  # Best Factor Index
  ind = which(liResult == min(liResult, na.rm = TRUE))

  calibFactor = factorVal[[ind]]
  calPred = calibFactor * pred

  meanDiff = mean(actu, na.rm = TRUE) - mean(calPred, na.rm = TRUE)
  newPred = (calPred) + meanDiff

  cat(
    sprintf("%s : %.2f", "[보정 X] RMSE", Metrics::rmse(actu, pred))
    , sprintf("%s : %.2f", "[보정 O] RMSE", Metrics::rmse(actu, newPred))
    , "\n"
  )

  return(c(newPred))
}

#================================================
# Main
#================================================
# 검증 지수 테이블 생성
perfTable = data.frame(matrix(0, nrow = 2 * 22, ncol = 15))
rownames(perfTable) = c(paste0("ORI-", 1:22), paste0("DL-", 1:22))
colnames(perfTable) = c("slope", "interp", "xMean", "yMean", "xSd", "ySd", "cnt", "bias", "rBias", "rmse", "rRmse", "r", "pVal", "diffMean", "diffSd")

# fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0086_Row_Data_From_Model_Obs.csv", sep = "/"))
# data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "UTF-8"))

fileInfo1 = Sys.glob(paste(globalVar$inpPath, "LSH0091_real1970-2005.csv", sep = "/"))
tmpData1 = readr::read_csv(file = fileInfo1, locale = locale("ko", encoding = "UTF-8"))

fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0091_Amon ACCESS1-3 1970-2005.csv", sep = "/"))
tmpData2 = readr::read_csv(file = fileInfo2, locale = locale("ko", encoding = "UTF-8"))

isCorr = TRUE
layerInfo = 3
epochsInfo = 1000
setParamInfo = sprintf("%s-%s", layerInfo, epochsInfo)

colList = colnames(tmpData1)
dlPredData = data.frame()
dlLastLayerData = data.frame()
oriData = data.frame()

# i = 1
for (i in 1:length(colList)) {

  tryCatch(
    expr = {
      log4r::info(log, sprintf("%s", "[START] h2o.deeplearning"))

      data = data.frame(tmpData1[, i], tmpData2[, i]) %>%
        magrittr::set_colnames(c("obs", "model"))

      dataL1 = data %>%
        dplyr::filter(
          !is.na(obs)
          , !is.na(model)
        )

      #*****************************************
      # 훈련 및 테스트 셋 설정 (70 : 30)
      #*****************************************
      set.seed(1)

      # 훈련 및 데이터 셋을 60:40으로 나누기 위한 인덱스 설정
      # ind = sample(1:nrow(dataL1), nrow(dataL1) * 0.7)
      ind = 1:302

      #*****************************************
      # 해당 인덱스에 따라 자료 할당
      #*****************************************
      trainData = dataL1[ind,]
      testData = dataL1[-ind,]

      #*****************************************
      # 해당 인덱스에 따라 자료 할당 (표준화)
      #*****************************************
      # trainData = dataL1[ind,] %>%
      #   dplyr::mutate_each_(funs(scale), vars = c("obs"))
      #
      # testData = dataL1[-ind,] %>%
      #   dplyr::mutate_each_(funs(scale), vars = c("obs"))

      #*****************************************
      # 해당 인덱스에 따라 자료 할당 (정규화)
      #*****************************************
      # trainData = dataL1[ind,] %>%
      #   dplyr::mutate_each_(funs(scales::rescale), vars = c("obs"))
      #
      # testData = dataL1[-ind,] %>%
      #   dplyr::mutate_each_(funs(scales::rescale), vars = c("obs"))

      # 훈련 데이터셋 확인
      dplyr::tbl_df(trainData)

      # 테스트 데이터셋 확인
      dplyr::tbl_df(testData)

      yObs = testData$obs
      yModel = testData$model

      if (isCorr == TRUE) {
        yModel = biasCorr(yObs, yModel, -100, 100, 0.001, FALSE)
      }

      # 원시 데이터 (ORI)
      perfTable[i,] = round(perfEval(yModel, yObs), 2)

      #===============================================
      # Deep Learning (DL)
      #===============================================

      #***********************************
      # 학습
      #***********************************
      tictoc::tic()

      # 초기화
      h2o.init()

      # activation : 활성화 함수로서 Rectifier 정규화 선형 함수 (즉 Keras의 ReLU 동일)
      # hidden : 숨겨진 레이어의 수와 뉴런 수 (일반적으로 입력 차원의 1/10 or 1/100 단위)
      # epochs : 반복 횟수 (기본 10-40)
      # nfolds : 훈련 반복 수

      layerNum = as.integer(nrow(trainData) / 10)
      # layerNum = as.integer(nrow(trainData) / 100)

      dlModel = h2o::h2o.deeplearning(
        x = c("obs")
        , y = c("model")
        , training_frame = as.h2o(trainData)
        , activation = "Rectifier"
        , hidden = rep(layerNum, layerInfo)
        , nfolds = 10
        , epochs = epochsInfo
        , seed = 1
      )

      tictoc::toc()

      tryCatch(

        expr = {
          log4r::info(log, sprintf("%s", "[START] Make Image"))

          saveImg = sprintf("%s/%s_%s_%s.png", globalVar$figPath, serviceName, colList[i], "Training-Scoring-History")
          png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

          plot(dlModel, timestep = "epochs", metric = "rmse")
        }

        , warning = function(warning) {
          log4r::warn(log, warning)
        }

        , error = function(error) {
          log4r::error(log, error)
        }

        , finally = {
          dev.off()

          log4r::info(log, sprintf("%s", "[END] Make Image"))
        }
      )

      tmpLastLayer = h2o::h2o.deepfeatures(dlModel, as.h2o(trainData), layer = layerInfo) %>%
        as.tibble() %>%
        dplyr::mutate(colInfo = colList[i])

      dlLastLayerData = dplyr::bind_rows(dlLastLayerData, tmpLastLayer)

      #***********************************
      # 검증
      #***********************************
      yObs = testData$obs
      yHat = as.data.frame(h2o::h2o.predict(object = dlModel, newdata = as.h2o(testData)))$predict

      if (isCorr == TRUE) {
        yHat = biasCorr(yObs, yHat, -100, 100, 0.001, FALSE)
      }

      tmpData = data.frame(yHat) %>%
        magrittr::set_colnames(paste0("DL-", colList[i]))

      if (ncol(dlPredData) == 0) {
        dlPredData = tmpData
      } else {
        dlPredData = dplyr::bind_cols(dlPredData, tmpData)
      }

      perfTable[i + 22,] = round(perfEval(yHat, yObs), 2)

    }

    , warning = function(warning) {
      perfTable[i + 22,] = 0

      log4r::warn(log, warning)
    }

    , error = function(error) {
      perfTable[i + 22,] = 0

      log4r::error(log, error)
    }

    , finally = {

      log4r::info(log, sprintf("%s", "[END] h2o.deeplearning"))
    }
  )
}


#=====================================================================
# 검증 결과 출력
#=====================================================================
# XLSX 파일 생성
saveXlsxFile = sprintf("%s/%s_%s_%s_%s.xlsx", globalVar$outPath, serviceName, "Pred_Data_From_Model_Obs", setParamInfo, isCorr)

wb = openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "perfTable")
openxlsx::writeData(wb, "perfTable", perfTable, startRow = 1, startCol = 1, colNames = TRUE, rowNames = TRUE)

openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)

# CSV 파일 생성
saveCsvFile = sprintf("%s/%s_%s_%s_%s.csv", globalVar$outPath, serviceName, "Pred_Data_From_Model_Obs", setParamInfo, isCorr)
utils::write.csv(perfTable, file = saveCsvFile)

# CSV 파일 생성
saveDlCsvFile = sprintf("%s/%s_%s_%s_%s.csv", globalVar$outPath, serviceName, "DL_Pred_Data_From_Model_Obs", setParamInfo, isCorr)
utils::write.csv(dlPredData, file = saveDlCsvFile)


# CSV 파일 생성
saveDlLastLayerCsvFile = sprintf("%s/%s_%s_%s_%s.csv", globalVar$outPath, serviceName, "DL_Last_Layer_Data_From_Model_Obs", setParamInfo, isCorr)
utils::write.csv(dlLastLayerData, file = saveDlLastLayerCsvFile)

# file_show(globalVar$outPath)

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

rm(list = ls())

prjName = "o2job"

source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

#************************************************
# 요구사항
#************************************************
# RNN CNN에 대한것도 조금 알려주셨으면좋겠습니다!

serviceName = "LSH0094"

library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(tensorflow)
library(keras)
library(log4r)
library(reticulate)
library(h2o)

#================================================
# Set Env
#================================================
# globalVar = new.env()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."
# globalVar$logPath = "."

#================================================
# Main
#================================================
# 검증 지수 테이블 생성
perfTable = data.frame(matrix(0, nrow = 3 * 22, ncol = 15))
rownames(perfTable) = c(paste0("ORI-", 1:22), paste0("DNN-", 1:22), paste0("RNN-", 1:22))
colnames(perfTable) = c("slope", "interp", "xMean", "yMean", "xSd", "ySd", "cnt", "bias", "rBias", "rmse", "rRmse", "r", "pVal", "diffMean", "diffSd")

# fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0086_Row_Data_From_Model_Obs.csv", sep = "/"))
# data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "UTF-8"))

fileInfo1 = Sys.glob(paste(globalVar$inpPath, "LSH0091_real1970-2005.csv", sep = "/"))
tmpData1 = readr::read_csv(file = fileInfo1, locale = locale("ko", encoding = "UTF-8"))

fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0091_Amon ACCESS1-3 1970-2005.csv", sep = "/"))
tmpData2 = readr::read_csv(file = fileInfo2, locale = locale("ko", encoding = "UTF-8"))

isCorr = TRUE
layerInfo = 3
epochsInfo = 1000
setParamInfo = sprintf("%s-%s", layerInfo, epochsInfo)

colList = colnames(tmpData1)
dlPredData = data.frame()
dlLastLayerData = data.frame()
oriData = data.frame()

# i = 1
for (i in 1:length(colList)) {

  tryCatch(
    expr = {
      log4r::info(log, sprintf("%s", "[START] h2o.deeplearning"))

      data = data.frame(tmpData1[, i], tmpData2[, i]) %>%
        magrittr::set_colnames(c("obs", "model"))

      dataL1 = data %>%
        dplyr::filter(
          !is.na(obs)
          , !is.na(model)
        )

      #*****************************************
      # 훈련 및 테스트 셋 설정 (70 : 30)
      #*****************************************
      set.seed(1)

      # 훈련 및 데이터 셋을 60:40으로 나누기 위한 인덱스 설정
      # ind = sample(1:nrow(dataL1), nrow(dataL1) * 0.7)
      ind = 1:302

      #*****************************************
      # 해당 인덱스에 따라 자료 할당
      #*****************************************
      trainData = dataL1[ind,]
      testData = dataL1[-ind,]

      #*****************************************
      # 해당 인덱스에 따라 자료 할당 (표준화)
      #*****************************************
      # trainData = dataL1[ind,] %>%
      #   dplyr::mutate_each_(funs(scale), vars = c("obs"))
      #
      # testData = dataL1[-ind,] %>%
      #   dplyr::mutate_each_(funs(scale), vars = c("obs"))

      #*****************************************
      # 해당 인덱스에 따라 자료 할당 (정규화)
      #*****************************************
      # trainData = dataL1[ind,] %>%
      #   dplyr::mutate_each_(funs(scales::rescale), vars = c("obs"))
      #
      # testData = dataL1[-ind,] %>%
      #   dplyr::mutate_each_(funs(scales::rescale), vars = c("obs"))

      # 훈련 데이터셋 확인
      dplyr::tbl_df(trainData)

      # 테스트 데이터셋 확인
      dplyr::tbl_df(testData)

      yObs = testData$obs
      yModel = testData$model

      if (isCorr == TRUE) {
        # 훈련 데이터셋 보정
        obsTrainData = trainData$obs
        modelTrainData = trainData$model
        corTrainData = biasCorr(obsTrainData, modelTrainData, -100, 100, 0.001, FALSE)

        trainData$model = corTrainData
      }

      # 원시 데이터 (ORI)
      perfTable[i,] = round(perfEval(yModel, yObs), 2)

      #===============================================
      # Deep Neural Network (DNN)
      #===============================================

      # FLAGS = flags(
      #   flag_boolean("stateful", FALSE),
      #   flag_boolean("stack_layers", FALSE),
      #   flag_integer("batch_size", 10),
      #   flag_integer("n_timesteps", 12),
      #   flag_integer("n_epochs", 100),
      #   flag_numeric("dropout", 0.2),
      #   flag_numeric("recurrent_dropout", 0.2),
      #   flag_string("loss", "logcosh"),
      #   flag_string("optimizer_type", "sgd"),
      #   flag_integer("n_units", 128),
      #   flag_numeric("lr", 0.003),
      #   flag_numeric("momentum", 0.9),
      #   flag_integer("patience", 10)
      # )
      #
      # flag_string("optimizer_type", "sgd")
      # optimizer <- switch(FLAGS$optimizer_type,
      #                     sgd = optimizer_sgd(lr = FLAGS$lr,
      #                                         momentum = FLAGS$momentum)
      # )

      #***********************************
      # 학습
      #***********************************
      tictoc::tic()

      # keras::install_keras()
      dnnModel = keras::keras_model_sequential()

      # layerNum = as.integer(nrow(trainData) / 10)

      dnnModel %>%
        layer_dense(units = 256, activation = 'relu', input_shape = c(1)) %>%
        layer_dropout(rate = 0.4) %>%
        layer_dense(units = 128, activation = 'relu') %>%
        layer_dropout(rate = 0.3) %>%
        layer_dense(units = 10, activation = 'softmax')

      # dnnModel %>%
      #   layer_dense(units = layerNum, activation = 'relu', input_shape = c(1)) %>%
      #   layer_dense(units = layerNum, activation = 'relu') %>%
      #   layer_dense(units = layerNum, activation = 'softmax')

      #
      # dnnModel %>% compile(
      #   loss = 'mean_squared_error',
      #   optimizer = 'adam',
      #   metrics = c(rmse_metric, r2_metric, 'mae')
      # )
      #

      # , rmse = Metrics::rmse(pred, cumDeath)

      dnnModel %>% compile(
        loss = 'mean_squared_error'
        , optimizer = 'sgd'
        # , metrics = c(cor, Metrics::rmse)
      )

      batchInfo = 5
      epochsInfo = 100

      # 모형 적합
      dnnModel %>% fit(
        trainData$obs
        , trainData$model
        , epochs = epochsInfo
        , batch_size = batchInfo
        , verbose = FALSE
        , validation_split = 0.3
      )

      plot(dnnModel)

      dnnModel %>%
        evaluate(testData$model, testData$obs)


      # 모형 테스트
      # dnn_model %>%
      #   evaluate(x_test, y_test)


      library(keras)

      model = keras_model_sequential()
      model %>%
        layer_dense(units = 32, input_shape = c(784)) %>%
        layer_activation('relu') %>%
        layer_dense(units = 10) %>%
        layer_activation('softmax')

      model %>% compile(
        optimizer = 'rmsprop',
        loss = 'categorical_crossentropy',
        metrics = c('accuracy')
      )


      summary(dnnModel)

      # 초기화
      h2o.init()

      # activation : 활성화 함수로서 Rectifier 정규화 선형 함수 (즉 Keras의 ReLU 동일)
      # hidden : 숨겨진 레이어의 수와 뉴런 수 (일반적으로 입력 차원의 1/10 or 1/100 단위)
      # epochs : 반복 횟수 (기본 10-40)
      # nfolds : 훈련 반복 수

      layerNum = as.integer(nrow(trainData) / 10)
      # layerNum = as.integer(nrow(trainData) / 100)

      dlModel = h2o::h2o.deeplearning(
        x = c("obs")
        , y = c("model")
        , training_frame = as.h2o(trainData)
        , activation = "Rectifier"
        , hidden = rep(layerNum, layerInfo)
        , nfolds = 10
        , epochs = epochsInfo
        , seed = 1
      )

      tictoc::toc()

      tryCatch(

        expr = {
          log4r::info(log, sprintf("%s", "[START] Make Image"))

          saveImg = sprintf("%s/%s_%s_%s.png", globalVar$figPath, serviceName, colList[i], "Training-Scoring-History")
          png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

          plot(dlModel, timestep = "epochs", metric = "rmse")
        }

        , warning = function(warning) {
          log4r::warn(log, warning)
        }

        , error = function(error) {
          log4r::error(log, error)
        }

        , finally = {
          dev.off()

          log4r::info(log, sprintf("%s", "[END] Make Image"))
        }
      )

      tmpLastLayer = h2o::h2o.deepfeatures(dlModel, as.h2o(trainData), layer = layerInfo) %>%
        as.tibble() %>%
        dplyr::mutate(colInfo = colList[i])

      dlLastLayerData = dplyr::bind_rows(dlLastLayerData, tmpLastLayer)

      #***********************************
      # 검증
      #***********************************
      yObs = testData$obs
      yHat = as.data.frame(h2o::h2o.predict(object = dlModel, newdata = as.h2o(testData)))$predict

      if (isCorr == TRUE) {
        yHat = biasCorr(yObs, yHat, -100, 100, 0.001, FALSE)
      }

      tmpData = data.frame(yHat) %>%
        magrittr::set_colnames(paste0("DL-", colList[i]))

      if (ncol(dlPredData) == 0) {
        dlPredData = tmpData
      } else {
        dlPredData = dplyr::bind_cols(dlPredData, tmpData)
      }

      perfTable[i + 22,] = round(perfEval(yHat, yObs), 2)

    }

    , warning = function(warning) {
      perfTable[i + 22,] = 0

      log4r::warn(log, warning)
    }

    , error = function(error) {
      perfTable[i + 22,] = 0

      log4r::error(log, error)
    }

    , finally = {

      log4r::info(log, sprintf("%s", "[END] h2o.deeplearning"))
    }
  )
}


#=====================================================================
# 검증 결과 출력
#=====================================================================
# XLSX 파일 생성
saveXlsxFile = sprintf("%s/%s_%s_%s_%s.xlsx", globalVar$outPath, serviceName, "Pred_Data_From_Model_Obs", setParamInfo, isCorr)

wb = openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "perfTable")
openxlsx::writeData(wb, "perfTable", perfTable, startRow = 1, startCol = 1, colNames = TRUE, rowNames = TRUE)

openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)

# CSV 파일 생성
saveCsvFile = sprintf("%s/%s_%s_%s_%s.csv", globalVar$outPath, serviceName, "Pred_Data_From_Model_Obs", setParamInfo, isCorr)
utils::write.csv(perfTable, file = saveCsvFile)

# CSV 파일 생성
saveDlCsvFile = sprintf("%s/%s_%s_%s_%s.csv", globalVar$outPath, serviceName, "DL_Pred_Data_From_Model_Obs", setParamInfo, isCorr)
utils::write.csv(dlPredData, file = saveDlCsvFile)


# CSV 파일 생성
saveDlLastLayerCsvFile = sprintf("%s/%s_%s_%s_%s.csv", globalVar$outPath, serviceName, "DL_Last_Layer_Data_From_Model_Obs", setParamInfo, isCorr)
utils::write.csv(dlLastLayerData, file = saveDlLastLayerCsvFile)

# file_show(globalVar$outPath)


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

serviceName = "LSH0094"

library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(akima)

#================================================
# Set Env
#================================================
globalVar = new.env()
globalVar$inpPath = "."
globalVar$figPath = "."
globalVar$outPath = "."
globalVar$logPath = "."

#================================================
# Main
#================================================
# fileInfo = Sys.glob(paste(globalVar$inpPath, "샘플1-1.xlsx", sep = "/"))
# c1Data = openxlsx::read.xlsx(fileInfo, sheet = 1)
# c2Data = openxlsx::read.xlsx(fileInfo, sheet = 2)
# c3Data = openxlsx::read.xlsx(fileInfo, sheet = 3)

# fileInfo1 = Sys.glob(paste(globalVar$inpPath, "새폴더-1/C1/201209.csv", sep = "/"))
# c1Data = readr::read_csv(file = fileInfo1, locale = locale("ko", encoding = "UTF-8"))
#
# fileInfo2 = Sys.glob(paste(globalVar$inpPath, "새폴더-1/C2/201209.csv", sep = "/"))
# c2Data = readr::read_csv(file = fileInfo2, locale = locale("ko", encoding = "UTF-8"))
#
# fileInfo3 = Sys.glob(paste(globalVar$inpPath, "새폴더-1/C3/201209.csv", sep = "/"))
# c3Data = readr::read_csv(file = fileInfo3, locale = locale("ko", encoding = "UTF-8"))

fileList1 = Sys.glob(paste(globalVar$inpPath, "새폴더-1/C1/*.csv", sep = "/"))
# fileInfo1 = "E:/04. TalentPlatform/Github/TalentPlatform-R/INPUT/o2job/새폴더-1/C1/201209.csv"

for (fileInfo1 in fileList1) {

  fileName = tools::file_path_sans_ext(fs::path_file(fileInfo1))

  fileInfo2 = fileInfo1 %>% str_replace_all(pattern = "/C1/", replacement = "/C2/")
  fileInfo3 = fileInfo1 %>% str_replace_all(pattern = "/C1/", replacement = "/C3/")

  c1Data = readr::read_csv(file = fileInfo1, locale = locale("ko", encoding = "UTF-8"))
  c2Data = readr::read_csv(file = fileInfo2, locale = locale("ko", encoding = "UTF-8"))
  c3Data = readr::read_csv(file = fileInfo3, locale = locale("ko", encoding = "UTF-8"))

  idList = sort(unique(c(c1Data$CellID, c2Data$CellID, c3Data$CellID)))

  isC1Flag = FALSE
  isC2Flag = FALSE
  isC3Flag = FALSE
  isTotalFlag = FALSE

  minC1Cnt = 9999999
  minC2Cnt = 9999999
  minC3Cnt = 9999999
  minTotalCnt = 9999999

  dataC1L1 = tibble()
  dataC2L1 = tibble()
  dataC3L1 = tibble()
  dataTotalL1 = tibble()
  # idInfo = "FTL7110042"
  idList = idList[1:200]

  for (idInfo in idList) {

    c1DataL1 = c1Data %>%
      as.tibble() %>%
      dplyr::filter(
        t == as.integer(t)
        , CellID == idInfo
      )

    c2DataL1 = c2Data %>%
      as.tibble() %>%
      dplyr::filter(
        t == as.integer(t)
        , CellID == idInfo
      )

    c3DataL1 = c3Data %>%
      as.tibble() %>%
      dplyr::filter(
        t == as.integer(t)
        , CellID == idInfo
      )

    if (nrow(c1DataL1) < 1) { next }
    if (nrow(c2DataL1) < 1) { next }
    if (nrow(c3DataL1) < 1) { next }

    seqList = seq(min(c1DataL1$t, na.rm = TRUE), max(c1DataL1$t, na.rm = TRUE))
    c1DataL2 = approx(x = c1DataL1$t, y = c1DataL1$V, xout = seqList) %>%
      as.tibble() %>%
      dplyr::mutate(id = idInfo, key = sprintf("%s-%04d", "c1", x))

    seqList = seq(min(c2DataL1$t, na.rm = TRUE), max(c2DataL1$t, na.rm = TRUE))
    c2DataL2 = approx(x = c2DataL1$t, y = c2DataL1$V, xout = seqList) %>%
      as.tibble() %>%
      dplyr::mutate(id = idInfo, key = sprintf("%s-%04d", "c2", x))

    seqList = seq(min(c3DataL1$t, na.rm = TRUE), max(c3DataL1$t, na.rm = TRUE))
    c3DataL2 = approx(x = c3DataL1$t, y = c3DataL1$V, xout = seqList) %>%
      as.tibble() %>%
      dplyr::mutate(id = idInfo, key = sprintf("%s-%04d", "c3", x))

    dataC1 = dplyr::bind_rows(c1DataL2) %>%
      dplyr::select(-x) %>%
      tidyr::spread(key = key, value = y)

    dataC2 = dplyr::bind_rows(c2DataL2) %>%
      dplyr::select(-x) %>%
      tidyr::spread(key = key, value = y)

    dataC3 = dplyr::bind_rows(c3DataL2) %>%
      dplyr::select(-x) %>%
      tidyr::spread(key = key, value = y)

    dataTotal = dplyr::bind_rows(c1DataL2, c2DataL2, c3DataL2) %>%
      dplyr::select(-x) %>%
      tidyr::spread(key = key, value = y)

    if (ncol(dataC1) < minC1Cnt) {
      colNameC1 = colnames(dataC1)
      minC1Cnt = ncol(dataC1)
    }

    if (ncol(dataC2) < minC2Cnt) {
      colNameC2 = colnames(dataC2)
      minC2Cnt = ncol(dataC2)
    }

    if (ncol(dataC3) < minC3Cnt) {
      colNameC3 = colnames(dataC3)
      minC3Cnt = ncol(dataC3)
    }

    if (ncol(dataTotal) < minTotalCnt) {
      colNameTotal = colnames(dataTotal)
      minTotalCnt = ncol(dataTotal)
    }

    dataC1L1 = dplyr::bind_rows(dataC1L1, dataC1)
    dataC2L1 = dplyr::bind_rows(dataC2L1, dataC2)
    dataC3L1 = dplyr::bind_rows(dataC3L1, dataC3)
    dataTotalL1 = dplyr::bind_rows(dataTotalL1, dataTotal)

    if (ncol(dataC1L1) != length(colNameC1)) {
      isC1Flag = TRUE
    }

    if (ncol(dataC2) != length(colNameC2)) {
      isC2Flag = TRUE
    }

    if (ncol(dataC3) != length(colNameC3)) {
      isC3Flag = TRUE
    }

    if (ncol(dataTotal) != length(colNameTotal)) {
      isTotalFlag = TRUE
    }

    cat(
      sprintf("%s %s : %04d %04d %04d %04d", fileName, idInfo, ncol(dataC1), ncol(dataC2), ncol(dataC3), sum(ncol(dataC1), ncol(dataC2), ncol(dataC3), na.rm = TRUE))
      , sprintf(": %s %s %s %s", isC1Flag, isC2Flag, isC3Flag, isTotalFlag)
      , "\n"
    )
  }


  # 컬럼 순서대로 변경
  dataC1L2 = dataC1L1 %>%
    dplyr::select(colNameC1) %>%
    magrittr::set_colnames(c(id, seq(0, length(colNameC1)))) %>%
    as.data.frame()

  dataC2L2 = dataC2L1 %>%
    dplyr::select(colNameC2) %>%
    magrittr::set_colnames(c(id, seq(0, length(colNameC2)))) %>%
    as.data.frame()

  dataC3L2 = dataC3L1 %>%
    dplyr::select(colNameC3) %>%
    magrittr::set_colnames(c(id, seq(0, length(dataC3L1)))) %>%
    as.data.frame()

  dataTotalL2 = dataTotalL1 %>%
    dplyr::select(colNameTotal) %>%
    magrittr::set_colnames(c(id, seq(0, length(dataTotalL1)))) %>%
    as.data.frame()

  # 컬럼 순서대로 변경
  dataC1L3 = dataC1L1 %>%
    magrittr::set_colnames(c(id, seq(0, ncol(dataC1L1)))) %>%
    as.data.frame()

  dataC2L3 = dataC2L1 %>%
    magrittr::set_colnames(c(id, seq(0, ncol(dataC2L1)))) %>%
    as.data.frame()

  dataC3L3 = dataC3L1 %>%
    magrittr::set_colnames(c(id, seq(0, ncol(dataC3L1)))) %>%
    as.data.frame()

  dataTotalL3 = dataTotalL1 %>%
    magrittr::set_colnames(c(id, seq(0, ncol(dataTotalL1)))) %>%
    as.data.frame()


  #=======================================
  # XLSX 출력
  #=======================================
  wb = openxlsx::createWorkbook()

  # 최소 컬럼
  openxlsx::addWorksheet(wb, paste("Min-C1", isC1Flag, sep = "-"))
  openxlsx::writeData(wb, paste("Min-C1", isC1Flag, sep = "-"), dataC1L2, startRow = 1, startCol = 1)

  openxlsx::addWorksheet(wb, paste("Min-C2", isC2Flag, sep = "-"))
  openxlsx::writeData(wb, paste("Min-C2", isC2Flag, sep = "-"), dataC2L2, startRow = 1, startCol = 1)

  openxlsx::addWorksheet(wb, paste("Min-C3", isC3Flag, sep = "-"))
  openxlsx::writeData(wb, paste("Min-C3", isC3Flag, sep = "-"), dataC3L2, startRow = 1, startCol = 1)

  openxlsx::addWorksheet(wb, paste("Min-Total", isTotalFlag, sep = "-"))
  openxlsx::writeData(wb, paste("Min-Total", isTotalFlag, sep = "-"), dataTotalL2, startRow = 1, startCol = 1)


  # 모든 컬럼
  openxlsx::addWorksheet(wb, paste("ALL-C1", isC1Flag, sep = "-"))
  openxlsx::writeData(wb, paste("ALL-C1", isC1Flag, sep = "-"), dataC1L3, startRow = 1, startCol = 1)

  openxlsx::addWorksheet(wb, paste("ALL-C2", isC2Flag, sep = "-"))
  openxlsx::writeData(wb, paste("ALL-C2", isC2Flag, sep = "-"), dataC2L3, startRow = 1, startCol = 1)

  openxlsx::addWorksheet(wb, paste("ALL-C3", isC3Flag, sep = "-"))
  openxlsx::writeData(wb, paste("ALL-C3", isC3Flag, sep = "-"), dataC3L3, startRow = 1, startCol = 1)

  openxlsx::addWorksheet(wb, paste("ALL-Total", isTotalFlag, sep = "-"))
  openxlsx::writeData(wb, paste("ALL-Total", isTotalFlag, sep = "-"), dataTotalL3, startRow = 1, startCol = 1)

  saveFile = sprintf("%s/%s_%s_%s.xlsx", globalVar$outPath, serviceName, "샘플 전처리", fileName)
  openxlsx::saveWorkbook(wb, file = saveFile, overwrite = TRUE)

  #=======================================
  # CSV 출력
  #=======================================
  # 최소 컬럼
  saveFile = sprintf("%s/%s_%s_%s.csv", globalVar$outPath, serviceName, "Min-C1", fileName)
  utils::write.csv(dataC1L2, file = saveFile)

  saveFile = sprintf("%s/%s_%s_%s.csv", globalVar$outPath, serviceName, "Min-C2", fileName)
  utils::write.csv(dataC2L2, file = saveFile)

  saveFile = sprintf("%s/%s_%s_%s.csv", globalVar$outPath, serviceName, "Min-C3", fileName)
  utils::write.csv(dataC3L2, file = saveFile)

  saveFile = sprintf("%s/%s_%s_%s.csv", globalVar$outPath, serviceName, "Min-Total", fileName)
  utils::write.csv(dataTotalL2, file = saveFile)

  saveFile = sprintf("%s/%s_%s_%s.csv", globalVar$outPath, serviceName, "ALL-C1", fileName)
  utils::write.csv(dataC1L3, file = saveFile)

  saveFile = sprintf("%s/%s_%s_%s.csv", globalVar$outPath, serviceName, "ALL-C2", fileName)
  utils::write.csv(dataC2L3, file = saveFile)

  saveFile = sprintf("%s/%s_%s_%s.csv", globalVar$outPath, serviceName, "ALL-C3", fileName)
  utils::write.csv(dataC3L3, file = saveFile)

  saveFile = sprintf("%s/%s_%s_%s.csv", globalVar$outPath, serviceName, "ALL-Total", fileName)
  utils::write.csv(dataTotalL3, file = saveFile)

}

# file.show(saveFile)


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
# 안녕하세요. 핸들링, 시각화 데이터 및 설명입니다.
# 사용할 데이터는 다음과 같습니다.
# 1. 서울_인구_읍면동(2020_1~3분기) : 서울시 읍면동/시군구 단위 2020_1~3분기 인구 데이터
# 2. 서울 병원 : 서울특별시 병원 데이터
# 3. 행정구역 좌표의 shp 파일 (파일명: 행정구역.shp)
# 위 데이터를 이용하여 첨부한 사진처럼,
# (1) 읍면동 별 전체 인구에 대해 표현하고, 그 위에 1차 의료기관을 포인트로 표시 해 주시면 됩니다.
# 데이터 핸들링 과정 작업하실 때, R의 dplyr 패키지 사용 부탁드립니다.
# 전문가님이 전달해주시는 내용으로 제가 다시 공부할 예정이어서,
# 초보자 시선에서 이해 가능하도록 어려운 표현보다는 쉽고 구체적으로 함께 설명해주시면 정말 감사하겠습니다.
# 잘 부탁드립니다.
# 강소민 드림

serviceName = "LSH0095"

library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(openxlsx)
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
library(sf)

#================================================
# Set Env
#================================================
globalVar = new.env()
globalVar$inpPath = "."
globalVar$figPath = "."
globalVar$outPath = "."
globalVar$logPath = "."
globalVar$mapPath = "."

#================================================
# Main
#================================================
# 서울 병원 데이터 읽기
fileInfo1 = Sys.glob(paste(globalVar$inpPath, "서울 병원.xlsx", sep = "/"))
stationData = openxlsx::read.xlsx(fileInfo1, sheet = 1) %>%
  as.tibble() %>%
  na.omit() %>%
  readr::type_convert() %>%
  rename(
    lon = x좌표
    , lat = y좌표
  )

# 인구 데이터 읽기
fileInfo2 = Sys.glob(paste(globalVar$inpPath, "서울_인구_읍면동(2020_1~3분기).txt", sep = "/"))
data = readr::read_tsv(file = fileInfo2, locale = locale("ko", encoding = "UTF-8"))

dataL1 = data %>%
  readr::type_convert() %>%
  dplyr::filter(
    구분 == "계"
    , 자치구 != "합계"
    , 동 != "소계"
  ) %>%
  dplyr::select(기간, 자치구, 동, 계) %>%
  dplyr::group_by(자치구, 동) %>%
  dplyr::summarise(
    sumVal = sum(계, na.rm = TRUE)
  ) %>%
  dplyr::rename(
    addr2 = 자치구
    , addr3 = 동
  )


# 법정동 파일 읽기
fileInfo3 = Sys.glob(paste(globalVar$inpPath, "EMD.shp", sep = "/"))

mapData = shapefile(fileInfo3)
geoData = spTransform(mapData, CRS("+proj=longlat"))
mapGeoData = ggplot2::fortify(geoData, region = 'EMD_CD', region2 = "EMD_KOR_NM")


# 법정동 코드 읽기
codeInfo = Sys.glob(paste(globalVar$mapPath, "/admCode/법정동코드_전체자료.txt", sep = "/"))

code = utils::read.table(codeInfo, sep = "\t", header = TRUE, colClasses = "character", fileEncoding = "EUC-KR") %>%
  as.tibble() %>%
  magrittr::set_colnames(c("EMD_CD", "addr", "isUse"))


# 서울특별시
codeL1 = code %>%
  tidyr::separate(col = "addr", into = c("addr1", "addr2", "addr3", "addr4", "addr5"), sep = " ") %>%
  dplyr::select(-addr4, -addr5) %>%
  dplyr::filter(
    stringr::str_detect(addr1, regex("서울특별시"))
    , isUse == "존재"
  ) %>%
  dplyr::filter(
    !is.na(addr1)
    , !is.na(addr2)
    , !is.na(addr3)
  ) %>%
  dplyr::mutate(
    id = stringr::str_sub(EMD_CD, 1, 8)
  )

# 도 : addr1
# 시군구 : addr2
# 읍면동 : addr3
dataL2 = mapGeoData %>%
  dplyr::inner_join(codeL1, by = c("id" = "id")) %>%
  dplyr::left_join(dataL1, by = c("addr2" = "자치구", "addr3" = "동"))


# 시각화
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "서울특별시_읍면동_인구_분포도")
plotSubTitle = sprintf("[2020년 1/3 분기] %s", "서울특별시 읍면동 인구 분포도")

ggplot() +
  theme_bw() +
  coord_fixed(ratio = 1) +
  geom_polygon(data = dataL2, aes(x = long, y = lat, group = group, fill = sumVal)) +
  scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$sumVal, na.rm = TRUE)), na.value = "white") +
  geom_path(data = dataL2, aes(x = long, y = lat, group = group), colour = 'black', size = 0.5) +
  ggh4x::stat_midpoint(data = dataL2, aes(x = long, y = lat, group = group, label = addr3), geom = "text", size = 3) +
  geom_point(data = stationData, aes(x = lon, y = lat, group = NULL, fill = NULL), colour = "red", pch = 16, size = 0.35) +
  labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "단위 : 명", subtitle = NULL) +
  theme_minimal() +
  theme(
    text = element_text(size = 18)
    , panel.grid.major.x = element_blank()
    , panel.grid.major.y = element_blank()
    , panel.grid.minor.x = element_blank()
    , panel.grid.minor.y = element_blank()
    , axis.text.x = element_blank()
    , axis.ticks.x = element_blank()
    , axis.title.x = element_blank()
    , axis.text.y = element_blank()
    , axis.ticks.y = element_blank()
    , axis.title.y = element_blank()
    , plot.subtitle = element_text(hjust = 1)
    # , legend.position = "none"
  ) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)

file_show(saveImg)


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
# 데이터 시각화 및 이상치 제거
# 단변수 boxplot > 이변수 scatter plot
# 표준편차 분포상 3sd 이상 떨어진 값들 (1sd, 2sd, 3sd)

serviceName = "LSH0096"

library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(GGally)

#================================================
# Set Env
#================================================
globalVar = new.env()
globalVar$inpPath = "."
globalVar$figPath = "."
globalVar$outPath = "."
globalVar$logPath = "."
globalVar$mapPath = "."

#================================================
# Main
#================================================
fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0096_1.csv", sep = "/"))
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "UTF-8"))

dataL1 = data %>%
  dplyr::mutate(
    dtDateTime = readr::parse_datetime(time, "%Y.%m.%d %H:%M")
  )

# 요약 통계량
summary(dataL1)

dataL2 = dataL1 %>%
  dplyr::select(-time, -dtDateTime, -y, -x)


dataL3 = dataL2 %>%
  tidyr::gather(key = "key", value = "value")


# 시각화 : 이변수 scatter plot
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "산점도")

GGally::ggpairs(dataL2) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

# 시각화 : 단변수 boxplot
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "상자그림")

ggplot(dataL3, aes(x = key, y = value, colour = factor(key))) +
  geom_boxplot() +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

# 표준편차 분포상 3sd 이상 떨어진 값들 (1sd, 2sd, 3sd)

# "TD"  "SFV" "FA"  "N"
colList = colnames(dataL2)

for (colInfo in colList) {

  dataL4 = dataL2 %>%
    dplyr::select(colInfo) %>%
    magrittr::set_colnames(c("val"))


  #**************************************
  # 68.27% / 95.45% / 99.73% 신뢰구간
  #**************************************
  meanVal = mean(dataL4$val, na.rm = TRUE)
  sdVal = sd(dataL4$val, na.rm = TRUE)

  left68Conf = meanVal - (1 * sdVal)
  right68Conf = meanVal + (1 * sdVal)
  left95Conf = meanVal - (2 * sdVal)
  right95Conf = meanVal + (2 * sdVal)
  left99Conf = meanVal - (3 * sdVal)
  right99Conf = meanVal + (3 * sdVal)

  labelData = data.frame(
    label = c("-1σ", "+1σ", "-2σ", "+2σ", "-3σ", "+3σ")
    , value = c(left68Conf, right68Conf, left95Conf, right95Conf, left99Conf, right99Conf)
  )

  saveImg = sprintf("%s/%s_%s_%s.png", globalVar$figPath, serviceName, colInfo, "빈도분포")
  plotSubTitle = sprintf("[%s] 빈도분포", colInfo)

  ggplot(data = dataL4, aes(x = val)) +
    geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
    stat_function(fun = dnorm, args = list(mean = meanVal, sd = sdVal), colour = "blue", size = 1) +
    geom_vline(data = labelData, aes(xintercept = value, colour = label), show.legend = FALSE) +
    geom_text(data = labelData, aes(x = value, y = 0.05, label = label), show.legend = FALSE, hjust = -0.5, size = 5) +
    labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "단위 : 명", subtitle = NULL) +
    theme(text = element_text(size = 18)) +
    ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

  file_show(saveImg)

  # 95% 신뢰구간에서 이상치 확인
  dataL4 %>%
    dplyr::filter(
      !dplyr::between(val, left95Conf, right95Conf)
    )

  # 95% 신뢰구간에서 이상치 제거
  dataL5 = dataL4 %>%
    dplyr::filter(
      dplyr::between(val, left95Conf, right95Conf)
    )

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

rm(list = ls())

prjName = "o2job"

source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

#================================================
# 요구사항
#================================================
# 이 두개의 데이터를 돌리는데 while (any(R_mat[i, ] > 0) | i == 1) {
# D_mat <- rbind(D_mat, abs(deltaT_mat[1, 1:model_num] - R1_avg[i]))
# R_mat <- rbind(R_mat, (((mov_avg_diff/abs(R_mat[1, ]))^1)*((mov_avg_diff/abs(D_mat[i, ]))^1))^1)
# R1_avg <- c(R1_avg, abs(sum(R_mat[i+1, ]*D_mat[i, ])/sum(R_mat[i+1, ]))) 에서 에러가 발생해서요!

serviceName = "LSH0099"

#================================================
# Set Env
#================================================
# globalVar = new.env()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

########################################################################
# rm(list = ls())
gc()

# setting path where the data is
# path <- "F:/R/tu/tttt/tut/GCM/Comparesion dough index/index/dataset/SPEI/CMIP5"
# setwd(path)

# input file name
# input_file_name1 <- "SPEI busan historical"
# input_file_name2 <- "SPEI3 busan RCP45"


# reading csv
# 1 --> left side input
# 2 --> right side input
# inputt1 <- read.csv(paste0(input_file_name1, ".csv"))
# inputt2 <- read.csv(paste0(input_file_name2, ".csv"))

fileInfo1 = Sys.glob(paste(globalVar$inpPath, "SPEI_busan_historical.csv", sep = "/"))
fileInfo2 = Sys.glob(paste(globalVar$inpPath, "SPEI3_busan_RCP45.csv", sep = "/"))

inputt1 <- read.csv(fileInfo1)
inputt2 <- read.csv(fileInfo2)
input1 <- inputt1[-1:-2, -1]
input2 <- inputt2[-1:-2, -1]
# how many models using at a time
model_num <- ncol(input1) - 1

# setting specific indices to select
indices <- 1:430

# deltaT matrix calculation
deltaT_mat <- data.frame(matrix(NA, 1, ncol(input1)))
colnames(deltaT_mat) <- c(colnames(input1)[1:model_num], "Ensemble")

for (i in 1:model_num) {
  deltaT_mat[1, i] <- -abs(mean(input2[indices, i]) - mean(input1[indices, i]))
}
deltaT_mat$Ensemble <- mean(as.numeric(deltaT_mat[1, 1:model_num]))

# Bi + Ri together
R_mat <- data.frame(matrix(NA, 1, model_num))
colnames(R_mat) <- paste0("B", 1:model_num)

# B1, 2, 3, 4 calculation
for (i in 1:model_num) {
  R_mat[1, i] <- mean(input1[indices, i]) - mean(input1[indices, 11])
}

# moving average calculation
mov_avg_list <- c()
for (i in 1:311) { # (nrow(input1)-119)
  mov_avg_list <- c(mov_avg_list, mean(input1$c11[i:(i + 119)]))
}
mov_avg_diff <- max(mov_avg_list) - min(mov_avg_list)

# to make calculation easy setting 1st element of R1_avg as deltaT_Ensemble
R1_avg <- deltaT_mat[1, "Ensemble"]

D_mat <- c()

i <- 1

#==================================
# 기존 로직
#==================================
# iteration
# while (any(R_mat[i, ] > 0) | i == 1) {
# while (TRUE) {
#
#   D_mat <- rbind(D_mat, abs(deltaT_mat[1, 1:model_num] - R1_avg[i]))
#   R_mat <- rbind(R_mat, (((mov_avg_diff/abs(R_mat[1, ]))^1)*((mov_avg_diff/abs(D_mat[i, ]))^1))^1)
#   R1_avg <- c(R1_avg, abs(sum(R_mat[i+1, ]*D_mat[i, ])/sum(R_mat[i+1, ])))
#
#   chkVal = sum(R_mat[i, ], na.rm = TRUE)
#
#   if (chkVal < 0.001) {
#     cat(i, chkVal, "\n")
#     break
#   }
#
#   i <- i+1
# }
#

#==================================
# Fortran을 이용한 반복 수행
#==================================
dimD = tibble(deltaT_mat) %>%
  tidyr::gather(key = "key", value = "val") %>%
  dplyr::select(val)

dimR = tibble(R_mat) %>%
  tidyr::gather(key = "key", value = "val") %>%
  dplyr::select(val)

dimAvg = tibble(R1_avg) %>%
  tidyr::gather(key = "key", value = "val") %>%
  dplyr::select(val)

dimMovAvg = tibble(mov_avg_diff) %>%
  tidyr::gather(key = "key", value = "val") %>%
  dplyr::select(val)

utils::write.table(dimD, file = "./input-dimD.dat", col.names = FALSE, row.names = FALSE)
utils::write.table(dimR, file = "./input-dimR.dat", col.names = FALSE, row.names = FALSE)
utils::write.table(dimAvg, file = "./input-dimAvg.dat", col.names = FALSE, row.names = FALSE)
utils::write.table(dimMovAvg, file = "./input-dimMovAvg.dat", col.names = FALSE, row.names = FALSE)

system(paste(
  "gfortran"
  , "./CallFortranInR.f90"
))

system(paste(
  "./a.exe"
))

fileName = tools::file_path_sans_ext(fs::path_file(fileInfo1))

saveFile1 = sprintf("./%s_%s", fileName, "result-dimD.dat")
file.copy("./result-dimD.dat", saveFile1)

saveFile2 = sprintf("./%s_%s", fileName, "result-dimR.dat")
file.copy("./result-dimR.dat", saveFile2)

saveFile3 = sprintf("./%s_%s", fileName, "result-dimAvg.dat")
file.copy("./result-dimAvg.dat", saveFile3)


#==================================
# Fortran을 이용한 반복 수행
#==================================
dimD = tibble(deltaT_mat) %>%
  tidyr::gather(key = "key", value = "val") %>%
  dplyr::select(val)

dimR = tibble(R_mat) %>%
  tidyr::gather(key = "key", value = "val") %>%
  dplyr::select(val)

dimAvg = tibble(R1_avg) %>%
  tidyr::gather(key = "key", value = "val") %>%
  dplyr::select(val)

dimMovAvg = tibble(mov_avg_diff) %>%
  tidyr::gather(key = "key", value = "val") %>%
  dplyr::select(val)

utils::write.table(dimD, file = "./FORTRAN/input-dimD.dat", col.names = FALSE, row.names = FALSE)
utils::write.table(dimR, file = "./FORTRAN/input-dimR.dat", col.names = FALSE, row.names = FALSE)
utils::write.table(dimAvg, file = "./FORTRAN/input-dimAvg.dat", col.names = FALSE, row.names = FALSE)
utils::write.table(dimMovAvg, file = "./FORTRAN/input-dimMovAvg.dat", col.names = FALSE, row.names = FALSE)

system(paste(
  "gfortran"
  , "./FORTRAN/CallFortranInR.f90"
))

system(paste(
  "./a.exe"
))

fileName = tools::file_path_sans_ext(fs::path_file(fileInfo1))

saveFile1 = sprintf("./FORTRAN/%s_%s", fileName, "result-dimD.dat")
file.copy("./FORTRAN/result-dimD.dat", saveFile1)

saveFile2 = sprintf("./FORTRAN/%s_%s", fileName, "result-dimR.dat")
file.copy("./FORTRAN/result-dimR.dat", saveFile2)

saveFile3 = sprintf("./FORTRAN/%s_%s", fileName, "result-dimAvg.dat")
file.copy("./FORTRAN/result-dimAvg.dat", saveFile3)


# testRun = function(a, b, c) {
#   .Fortran ("looptest"
#     , arg1 = a
#     , arg2 = b
#     , arg3 = c
#   )
# }

# gfortran -shared -o CallFortranInR.dll CallFortranInR.f90
# system(paste(
#   "gfortran"
#   , "-shared"
#   , "-o ./FORTRAN/CallFortranInR.dll"
#   , "./FORTRAN/CallFortranInR.f90"
# ))

# dyn.load("./FORTRAN/CallFortranInR.dll")
# is.loaded("TestSub")
# is.loaded("TestLoop")

# testRun = function(a, b) {
#   .Fortran ("TestLoop"
#             , arg1 = as.double(a)
#             , arg2 = as.double(b)
#             , arg3 = double(1)
#   )
# }


#==================================
# R을 이용한 반복 수행
#==================================
# TestLoop = function(i) {
#
#   # <<- 전역 변수 수행
#   D_mat <<- rbind(D_mat, abs(deltaT_mat[1, 1:model_num] - R1_avg[i]))
#   R_mat <<- rbind(R_mat, (((mov_avg_diff/abs(R_mat[1, ]))^1)*((mov_avg_diff/abs(D_mat[i, ]))^1))^1)
#   R1_avg <<- c(R1_avg, abs(sum(R_mat[i+1, ]*D_mat[i, ])/sum(R_mat[i+1, ])))
#   i <<- i + 1
#
#   chkVal = sum(R_mat[i, ], na.rm = TRUE)
#
#   if (chkVal < 0.01) {
#     result = list("D_mat" = D_mat, "R_mat" = R_mat, "R1_avg" = R1_avg, "chkVal" = chkVal)
#     cat(i, chkVal, "\n")
#
#     return( result )
#   }
# }
#
#
# result = sapply(295361:500000, TestLoop)


# D_mat <- rbind(D_mat, abs(deltaT_mat[1, 1:model_num] - R1_avg[i]))
# R_mat <- rbind(R_mat, (((mov_avg_diff/abs(R_mat[1, ]))^1)*((mov_avg_diff/abs(D_mat[i, ]))^1))^1)
# R1_avg <- c(R1_avg, abs(sum(R_mat[i+1, ]*D_mat[i, ])/sum(R_mat[i+1, ])))


# saving the result
# D matrix
# write.csv(D_mat,"F:/R/tu/tttt/tut/GCM/Projection/REA/results/SSP585/Ensemble/far Di yeosu results.csv")

# Bi Ri matrix
# write.csv(cbind(R_mat, R1_avg),"F:/R/tu/tttt/tut/GCM/Projection/REA/results/SSP585/Ensemble/far Bi Ri yeosu results.csv")

# Rb matrix

# result formatting: last lines of each matrix
# D_final <- D_mat[nrow(D_mat)-1, ]
# colnames(D_final) <- paste0(colnames(D_final), "_D")
# R_final <- R_mat[nrow(R_mat), ]
# colnames(R_final) <- paste0("R_", 1:ncol(R_final))
# colnames(deltaT_mat) <- paste0(colnames(deltaT_mat), "_delta")
# result <- cbind(D_final, R1_avg[length(R1_avg)], R_mat[nrow(R_mat), ], deltaT_mat[, ])
# colnames(result)[model_num+1] <- "R1_avg"
#
# # result
# write.csv(result,"F:/R/tu/tttt/tut/GCM/Projection/REA/results/Ensemble/SSP585/far SSP585 yeosu results.csv")
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

rm(list = ls())

prjName = "o2job"

source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

#================================================
# 요구사항
#================================================
# 이 두개의 데이터를 돌리는데 while (any(R_mat[i, ] > 0) | i == 1) {
# D_mat <- rbind(D_mat, abs(deltaT_mat[1, 1:model_num] - R1_avg[i]))
# R_mat <- rbind(R_mat, (((mov_avg_diff/abs(R_mat[1, ]))^1)*((mov_avg_diff/abs(D_mat[i, ]))^1))^1)
# R1_avg <- c(R1_avg, abs(sum(R_mat[i+1, ]*D_mat[i, ])/sum(R_mat[i+1, ]))) 에서 에러가 발생해서요!

serviceName = "LSH0100"

library(SPEI)

#================================================
# Set Env
#================================================
# globalVar = new.env()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

########################################################################
# rm(list = ls())
gc()

# setting path where the data is
# path <- "F:/R/tu/tttt/tut/GCM/Comparesion dough index/index/dataset/SPEI/CMIP5"
# setwd(path)

# input file name
# input_file_name1 <- "SPEI busan historical"
# input_file_name2 <- "SPEI3 busan RCP45"


# reading csv
# 1 --> left side input
# 2 --> right side input
# inputt1 <- read.csv(paste0(input_file_name1, ".csv"))
# inputt2 <- read.csv(paste0(input_file_name2, ".csv"))

fileInfo1 = Sys.glob(paste(globalVar$inpPath, "SPEI_busan_historical.csv", sep = "/"))
fileInfo2 = Sys.glob(paste(globalVar$inpPath, "SPEI3_busan_RCP45.csv", sep = "/"))

inputt1 <- read.csv(fileInfo1)
inputt2 <- read.csv(fileInfo2)


fileInfo1 = Sys.glob(paste(globalVar$inpPath, "INFUT/TEM/Period.csv", sep = "/"))
fileInfo2 = Sys.glob(paste(globalVar$inpPath, "INFUT/TEM/pr+GFDL-ESM2G+RCP45+2021-2100.csv", sep = "/"))
fileInfo3 = Sys.glob(paste(globalVar$inpPath, "INFUT/TEM/ta+GFDL-ESM2G+rcp45+2021-2100.csv", sep = "/"))


# setwd("F:/R/tu/tttt/tut/GCM/Comparesion dough index/index/SPEI")
input1 <- read.csv(fileInfo1, header = T)

# setwd("F:/R/tu/tttt/tut/GCM/Comparesion dough index/CMIP5/pr/projection")
input3 <- read.csv(fileInfo2, header = T)

# setwd("F:/R/tu/tttt/tut/GCM/Comparesion dough index/CMIP5/tem/avg/projection")
input4 <- read.csv(fileInfo3, header = T)

year <- input1[, 1]
month <- input1[, 2]
PRCP <- input3[13:972, -1]
TMED <- input4[13:972, -1]

tail(TMED)
wichita.1 <- data.frame(cbind(year, month, PRCP, TMED))
wichita <- as.data.frame(wichita.1)
pr <- wichita[, 3:24]
tem <- wichita[, 25:46]

#=========================================
# 코드 수정
#=========================================

wichita$PET <- SPEI::thornthwaite(tem$s1.1, 37.57, na.rm = TRUE)
spei3 <- SPEI::spei(pr$s1 - wichita$PET, 3, na.rm = TRUE)
spei6 <- SPEI::spei(pr$s1 - wichita$PET, 6, na.rm = TRUE)
spei9 <- SPEI::spei(pr$s1 - wichita$PET, 9, na.rm = TRUE)
spei12 <- SPEI::spei(pr$s1 - wichita$PET, 12, na.rm = TRUE)

seoul3 <- spei3$fitted
seoul3 <- as.numeric(seoul3)

seoul6 <- spei6$fitted
seoul6 <- as.numeric(seoul6)

seoul9 <- spei9$fitted
seoul9 <- as.numeric(seoul9)


seoul12 <- spei12$fitted
seoul12 <- as.numeric(seoul12)

#======================================================================
# 데이터 전처리
#======================================================================
data = tibble(year = year, month = month, seoul3 = seoul3, seoul6 = seoul6, seoul9 = seoul9, seoul12 = seoul12) %>%
  # data = tibble(year = year, month = month, seoul3 = seoul3) %>%
  dplyr::mutate(
    dtDate = lubridate::make_datetime(year, month)
  ) %>%
  tibble::rowid_to_column("rowNum")

# NA 갯수
data %>%
  dplyr::select(everything()) %>%
  dplyr::summarise_all(funs(sum(is.na(.))))

# -INF의 경우 이전 달 및 다음 달 평균 수행
colList = c("seoul3", "seoul6", "seoul9", "seoul12")
# colInfo = "seoul3"

for (colInfo in colList) {

  naData = data %>%
    dplyr::rename(
      val = colInfo
    ) %>%
    dplyr::filter(
      is.na(val)
        | is.infinite(val)
    )

  # i = 1
  for (i in 1:nrow(naData)) {

    selRowNum = naData$rowNum[i]
    selDate = naData$dtDate[i]

    dtPrevDate = selDate %m-% months(1)
    dtNextDate = selDate %m+% months(1)

    dataL1 = data %>%
      dplyr::rename(
        val = colInfo
      ) %>%
      dplyr::filter(dtDate %in% c(dtPrevDate, dtNextDate)) %>%
      dplyr::filter(!(is.na(val) | is.infinite(val)))

    if (nrow(dataL1) < 1) { next }

    # meanVal = mean(dataL1$val, na.rm = TRUE)
    minVal = min(dataL1$val, na.rm = TRUE)

    # data[selRowNum, colInfo] = meanVal
    data[selRowNum, colInfo] = minVal
  }
}

# NA 갯수
data %>%
  dplyr::select(everything()) %>%
  dplyr::summarise_all(funs(sum(is.na(.))))


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

#================================================
# Set Env
#================================================
# globalVar = new.env()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())

prjName = "o2job"

source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

#================================================
# Main
#================================================
library(GGally)

serviceName = "LSH0102"

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0102_t_20210216.csv", sep = "/"))
# data = read.csv(file = fileInfo, sep = ",", header = TRUE)
data = data.table::fread(file = fileInfo)
# data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "UTF-8"))

dataL1 = data %>%
  readr::type_convert() %>%
  as.tibble() %>%
  dplyr::mutate(
    dtDateTime = readr::parse_datetime(as.character(index), "%Y%m%d%H%M")
  ) %>%
  dplyr::filter(
    !is.na(type)
    , type != "NON"
  ) %>%
  dplyr::filter(
    as.Date("2019-12-15") <= dtDateTime & dtDateTime <= as.Date("2020-01-14")
      | as.Date("2020-03-09") <= dtDateTime & dtDateTime <= as.Date("2020-05-11")
  ) %>%
  dplyr::select(-dtDateTime, -season, -wind_direction, -index)

colnames(dataL2)

dataL2 = dataL1 %>%
  dplyr::mutate(type = dplyr::if_else(type == "typical NPF", "typical", "weak"))

summary(dataL2)

dataL2 %>%
  dplyr::select(everything()) %>%
  dplyr::summarise_all(funs(sum(is.na(.))))


# sort(unique(dataL1$type))
typeList = sort(unique(dataL2$type))

#*****************************
# ALL 산점도 행렬
#*****************************
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "ALL_산점도_행렬")

GGally::ggpairs(dataL2, mapping = ggplot2::aes(colour = type)) +
  theme(
    text = element_text(size = 14)
    , legend.position = "bottom"
  ) +
  ggsave(filename = saveImg, width = 14, height = 10, dpi = 600)

fs::file_show(saveImg)

#*****************************
# NON 산점도 행렬
#*****************************
dataL3 = dataL2 %>%
  dplyr::filter(type == "NON") %>%
  dplyr::select(-type)

summary(dataL3)
cor(dataL3)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "NON_산점도_행렬")

GGally::ggpairs(dataL3) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 14, height = 10, dpi = 600)

#*****************************
# NPF 산점도 행렬
#*****************************
# colnames(dataL3)

dataL3 = dataL2 %>%
  dplyr::filter(type != "NON") %>%
  dplyr::select(-type)

summary(dataL3)
cor(dataL3)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "NPF_산점도 행렬")

GGally::ggpairs(dataL3) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 14, height = 10, dpi = 600)

file.show(saveImg)


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
# 파일과 코드인데요
# 저는 208번 변수까지의 값을 보고 싶은데 1번 변수에 대한 값만 출력이 되어서요 ㅠㅠ
# 그리고 변수명 까지 같이 출력되게 하고 싶은데

#================================================
# Set Env
#================================================
# globalVar = new.env()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())

prjName = "o2job"

source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

#================================================
# Main
#================================================
library(psych)
library(readr)

serviceName = "LSH0103"

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0103_valll.csv", sep = "/"))

# fileInfo = c("E:/04. TalentPlatform/Github/TalentPlatform-R/RESOURCES/INPUT/o2job/LSH0103_valll.csv")
fileInfo = c("C:/Users/Dayeon/Desktop/20210216_통계결과/valll.csv")
val = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "UTF-8")) %>%
  na.omit() %>%
  as.data.frame()

val$change <- as.factor(val$change)

# summary(val)

j = 1

for (j in c(1:208)) {
  m <- psych::describeBy(val[j], val$change, mat = TRUE)

  if (nrow(m) < 1) { next }

  cat("colnames : ", colnames(val)[j], "\n")

  print(m)
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

#================================================
# 요구사항
#================================================
# [시안]
# 시안 40개 (샘플 1) : 평균
# 시안 40개 (샘플 2) : 평균
# ...
# 시안 40개 (샘플 100,000번) : 평균
# 
# [서울]
# 서울 40개 (샘플 1) : 평균
# 서울 40개 (샘플 2) : 평균
# ...
# 서울 40개 (샘플 100,000) : 평균
# 
# 이렇게 해서 (서울 샘플 100,000개 - 시안 샘플 100,000개) > 0.02
# 이게 몇개 있는 지 알면 되나요?

# 전체 모집단 80개의 행중에 표본행 40개를 추출하여 지역별로 평균했을때, 
# 서울의 것이 시안보다 0.02 이상 클 확률	

#================================================
# Set Env
#================================================
# globalVar = new.env()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())

prjName = "o2job"

source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

#================================================
# Main
#================================================
serviceName = "LSH0104"

set.seed(1)

# fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0104_첫번째+확률+의뢰(수정).xlsx", sep = "/"))
# fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0104_두번째+확률+의뢰(수정).xlsx", sep = "/"))
fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0104_세번째+확률+의뢰.xlsx", sep = "/"))

data = openxlsx::read.xlsx(fileInfo, sheet = 2)

# Number of repetition
repetition = 100000
repetition = 1000000

# Number of sample
# sampleNum = 40
sampleNum = 39

# FALSE : 비복원 추출(무작위 정렬) : 한번 뽑은 것을 다시 뽑을 수 없는 추출
indList = lapply(1:repetition, function(i) sample(1:nrow(data), sampleNum, replace = FALSE))

sampleList = mapply(function(i) mean(data[i, "sample"], na.rm = TRUE), indList)
seoulList = mapply(function(i) mean(data[i, "seoul"], na.rm = TRUE), indList)

# 전체 모집단 80개의 행중에 표본행 40개를 추출하여 지역별로 평균했을때, 
# 서울의 것이 시안보다 0.02 이상 클 확률

# isFlag = ((seoulList - sampleList) > 0.02)
isFlag = ((seoulList - sampleList) > 0.05)
precent = sum(isFlag, na.rm = TRUE) / length(isFlag)

cat(sprintf(
  "반복횟수 : %10s | 샘플추출 개수 : %2s | 조건 개수 : %10s | 전체 개수 : %10s | 확률 : %10s"
  , repetition
  , sampleNum
  , sum(isFlag, na.rm = TRUE)
  , length(isFlag)
  , precent
), "\n")

#******************************************
# 첫번째 (조건 : ((서울 - 시안) > 0.02))
#******************************************
# 반복횟수 : 100,000 | 샘플추출 개수 : 40 | 조건 개수 :        600 | 전체 개수 :     100000 | 확률 :      0.006 
# 반복횟수 : 1,000,000 | 샘플추출 개수 : 40 | 조건 개수 :       6298 | 전체 개수 :    1000000 | 확률 :   0.006298 

#******************************************
# 두번째 (조건 : ((서울 - 시안) > 0.02))
#******************************************
# 반복횟수 : 100,000 | 샘플추출 개수 : 40 | 조건 개수 :          4 | 전체 개수 :     100000 | 확률 : 0.00004
# 반복횟수 : 1,000,000 | 샘플추출 개수 : 40 | 조건 개수 :         27 | 전체 개수 :    1000000 | 확률 : 0.000027

#******************************************
# 세번째 (조건 : ((서울 - 시안) > 0.05))
#******************************************
# 반복횟수 : 100,000 | 샘플추출 개수 : 39 | 조건 개수 :      93676 | 전체 개수 :     100000 | 확률 :    0.93676 
# 반복횟수 : 1,000,000 | 샘플추출 개수 : 39 | 조건 개수 :     938432 | 전체 개수 :    1000000 | 확률 :   0.938432


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

rm(list = ls())

prjName = "o2job"

source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

#================================================
# 요구사항
#================================================
# 제가 첨부해드렸던 신라의 사례로 해보시는것이 좋을듯 한데 어떻게 생각하시는지요? 총 16개의 일식입니다.
# BC 54.05.09 / BC 28.06.19 / BC 26.10.23 / BC 15.03.29 / BC 02.02.05/ AD 02.11.22
# AD 16.08.21 / AD 124.10.25 / AD 127.08.25 / AD 141.11.16 / AD 166.02.18 / AD 186.07.04
# AD 193.02.19 / AD 194.08.04 / AD 200.09.26 / AD 201.03.22

# serviceName = "LSH0093"
# serviceName = "LSH0097"
# serviceName = "LSH0098"
# serviceName = "LSH0101"
serviceName = "LSH0105"

library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(metR)
library(openxlsx)
library(MBA)
library(noncompliance)
library(colorRamps)
library(sf)

#================================================
# Set Env
#================================================
# globalVar = new.env()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

#================================================
# Main Source Code
#================================================
cbMatlab = colorRamps::matlab.like(11)
mapGlobal = sf::st_read(paste(globalVar$mapPath, "gshhg-shp-2.3.6/GSHHS_shp/i/GSHHS_i_L1.shp", sep = "/"))

xRange = as.numeric(c(90, 150))
yRange = as.numeric(c(10, 60))
# yRange = as.numeric(c(5, 65))

newLon = seq(from = xRange[1], to = xRange[2], by = 0.1)
newLat = seq(from = yRange[1], to = yRange[2], by = 0.1)

gridData = noncompliance::expand.grid.DT(
  newLon
  , newLat
  , col.names = c("lon", "lat")
)

fileInfo = Sys.glob(paste(globalVar$inpPath, "mapImageToData.xlsx", sep = "/"))

#**************************************************
# 샘플 내삽
#**************************************************
sampleData = openxlsx::read.xlsx(fileInfo, sheet = 1) %>%
  as.tibble()

# saveImg = sprintf("%s/%s_%s_%s.png", globalVar$figPath, serviceName, "Image", "Sample")
#
# sampleData %>%
#   ggplot(aes(x = lon, y = lat, colour = factor(val))) +
#   geom_point() +
#   theme(text = element_text(size = 18)) +
#   ggsave(filename = saveImg, width = 12, height = 10, dpi = 600)

sampleDataL1 = MBA::mba.points(sampleData, gridData)
# dataL1 = MBA::mba.points(tmpData, gridData, extend = FALSE)

sampleDataL2 = sampleDataL1 %>%
  as.data.frame() %>%
  as.tibble() %>%
  dplyr::rename(
    xAxis = xyz.est.x
    , yAxis = xyz.est.y
    , zAxis = xyz.est.z
  )

saveImg = sprintf("%s/%s_%s_%s.png", globalVar$figPath, serviceName, "MakeImageToMapData", "Sample")

ggplot(data = sampleDataL2, aes(x = xAxis, y = yAxis, fill = zAxis, z = zAxis)) +
  geom_tile() +
  scale_fill_gradientn(colours = cbMatlab, limits = c(0, 1.0), breaks = seq(0.3, 0.9, 0.2), na.value = NA) +
  # metR::geom_contour2(color = "black", alpha = 1.0, breaks = seq(0.3, 0.9, 0.2), show.legend = FALSE) +
  metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.3, show.legend = FALSE, size = 0.5) +
  metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.5, show.legend = FALSE, size = 1) +
  metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.7, show.legend = FALSE, size = 2) +
  metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.9, show.legend = FALSE, size = 4) +
  metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = seq(0.3, 0.9, 0.2), rotate = TRUE, na.rm = TRUE) +
  geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = NA) +
  metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(90, 150), expand = c(0, 0)) +
  metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(10, 60), expand = c(0, 0)) +
  labs(
    subtitle = NULL
    , x = NULL
    , y = NULL
    , fill = NULL
    , colour = NULL
    , title = NULL
  ) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 10, dpi = 600)


#**************************************************
# 이미지 별로 내삽
#**************************************************
# data = openxlsx::read.xlsx(fileInfo, sheet = 2) %>%
#   as.tibble()

fileInfo = Sys.glob(paste(globalVar$inpPath, "mapImageToData.xlsx", sep = "/"))
data = openxlsx::read.xlsx(fileInfo, sheet = 6) %>%
  as.tibble()

# data = openxlsx::read.xlsx(fileInfo, sheet = 2) %>%
#   as.tibble()

# data = openxlsx::read.xlsx(fileInfo, sheet = 4) %>%
#   as.tibble()

# data = openxlsx::read.xlsx(fileInfo, sheet = 4) %>%
#   as.tibble() %>%
#   dplyr::filter(!type %in% c(42, 43))

typeList = sort(unique(data$type))
# typeList = max(data$type, na.rm = TRUE)

dataL3 = tibble()

for (typeInfo in typeList) {

  tmpData = data %>%
    dplyr::filter(
      type == typeInfo
      , !is.na(val)
    ) %>%
    dplyr::select(-type)

  # saveImg = sprintf("%s/%s_%s_%s.png", globalVar$figPath, serviceName, "Image", typeInfo)

  # tmpData %>%
  #   ggplot() +
  #   geom_point(aes(x = lon, y = lat, colour = factor(val))) +
  #   theme(text = element_text(size = 18)) # +
  # ggsave(filename = saveImg, width = 12, height = 10, dpi = 600)

  dataL1 = MBA::mba.points(tmpData, gridData)

  dataL2 = dataL1 %>%
    as.data.frame() %>%
    as.tibble() %>%
    dplyr::rename(
      xAxis = xyz.est.x
      , yAxis = xyz.est.y
      , zAxis = xyz.est.z
    ) %>%
    dplyr::mutate(
      type = typeInfo
    )

  # summary(dataL2)

  dataL3 = dplyr::bind_rows(dataL3, dataL2)

  # saveImg = sprintf("%s/%s_%s_%s.png", globalVar$figPath, serviceName, "MakeImageToMapData", typeInfo)
  # 
  # ggplot(data = dataL2, aes(x = xAxis, y = yAxis, fill = zAxis, z = zAxis)) +
  #   # geom_tile() +
  #   geom_raster(interpolate = TRUE, na.rm = TRUE) +
  #   scale_fill_gradientn(colours = cbMatlab, limits = c(0.0, 1.0), breaks = c(0, 0.3, 0.5, 0.7, 0.9), na.value = NA) +
  #   # metR::geom_contour2(color = "black", alpha = 1.0, breaks = seq(0.3, 0.9, 0.2), show.legend = FALSE) +
  #   metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0, show.legend = FALSE, size = 0.1) +
  #   metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.3, show.legend = FALSE, size = 0.5) +
  #   metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.5, show.legend = FALSE, size = 1) +
  #   metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.7, show.legend = FALSE, size = 2) +
  #   metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.9, show.legend = FALSE, size = 4) +
  #   geom_point(data = tmpData, aes(x = lon, y = lat, colour = factor(val), fill = NULL, z = NULL)) +
  #   metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = c(0, 0.3, 0.5, 0.7, 0.9), rotate = TRUE, na.rm = TRUE, size = 5) +
  # 
  #   geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = NA) +
  #   metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(90, 150), expand = c(0, 0)) +
  #   metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(10, 60), expand = c(0, 0)) +
  #   labs(
  #     subtitle = NULL
  #     , x = NULL
  #     , y = NULL
  #     , fill = NULL
  #     , colour = NULL
  #     , title = NULL
  #   ) +
  #   theme(text = element_text(size = 18)) +
  #   ggsave(filename = saveImg, width = 10, height = 10, dpi = 600)

  # file.show(saveImg)
}


#**************************************************
# 공간 평균
#**************************************************
# dataL4 %>%
#   dplyr::filter(
#     xAxis == 125.0
#     , yAxis == 30.0
#   )
#
# dataL3 %>%
#   dplyr::filter(
#     zAxis > 0
#   ) %>%
#   dplyr::filter(
#     xAxis == 130.0
#     , yAxis == 10.0
#   )
#
# dataL4 %>%
#   dplyr::filter(
#     xAxis == 130.0
#     , yAxis == 10.0
#   )

dataL4 = dataL3 %>%
  dplyr::group_by(xAxis, yAxis) %>%
  dplyr::summarise(
    meanVal = mean(zAxis, na.rm = TRUE)
  )

summary(dataL4)

ind = which(dataL4$meanVal == max(dataL4$meanVal, na.rm = TRUE))
maxData = dataL4[ind,]

saveImg = sprintf("%s/%s_%s_%s.png", globalVar$figPath, serviceName, "MakeImageToMapData", "1-16_Mean_Color")
saveImg = sprintf("%s/%s_%s_%s.png", globalVar$figPath, serviceName, "MakeImageToMapData", "1-16_Mean")

ggplot(data = dataL4, aes(x = xAxis, y = yAxis, fill = meanVal, z = meanVal)) +
  # metR::geom_contour_fill(na.fill = TRUE, kriging = TRUE)
  # geom_tile() +
  # geom_raster(interpolate = TRUE, na.rm = TRUE) +
  # scale_fill_gradientn(colours = cbMatlab, limits = c(0, 1.0), breaks = seq(0, 1.0, 0.2), na.value = NA) +
  metR::geom_contour2(color = "black", alpha = 1.0, breaks = c(seq(0.69, 0, -0.04), 0.7), show.legend = FALSE, size = 0.5) +
  metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = c(seq(0.69, 0, -0.04), 0.7), rotate = TRUE, na.rm = TRUE, size = 5) +
  # metR::geom_contour2(color = "black", alpha = 1.0, breaks = c(seq(0, 1.0, 0.04), 0.62), show.legend = FALSE, size = 0.5) +
  # metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = c(seq(0, 1.0, 0.04), 0.62), rotate = TRUE, na.rm = TRUE, size = 5) +
  # metR::geom_contour2(color = "black", alpha = 1.0, breaks = c(seq(0, 0.5, 0.05), 0.502, 0.504), show.legend = FALSE, size = 0.5) +
  # metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = c(seq(0, 0.5, 0.05), 0.502, 0.504), rotate = TRUE, na.rm = TRUE, size = 5) +
  # metR::geom_contour2(color = "black", alpha = 1.0, breaks = c(seq(0, 0.62, 0.02), 0.625, 0.629), show.legend = FALSE, size = 0.5) +
  # metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 1, breaks =  c(seq(0, 0.62, 0.02), 0.625, 0.629), rotate = TRUE, na.rm = TRUE, size = 5) +
  # metR::geom_contour2(color = "black", alpha = 1.0, breaks = c(0.40, 0.48, 0.53, 0.55), show.legend = FALSE, size = 0.5) +
  # metR::geom_contour2(color = "black", alpha = 1.0, breaks = c(0.4, 0.5, 0.6, 0.7), show.legend = FALSE, size = 0.5) +
  # metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.4, show.legend = FALSE, size = 0.5) +
  # metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.5, show.legend = FALSE, size = 1) +
  # metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.6, show.legend = FALSE, size = 2) +
  # metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.7, show.legend = FALSE, size = 4) +
  #
  # metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.40, show.legend = FALSE, size = 0.5) +
  # metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.48, show.legend = FALSE, size = 1) +
  # metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.53, show.legend = FALSE, size = 2) +
  # metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.55, show.legend = FALSE, size = 4) +
  # metR::geom_contour2(color = "black", alpha = 1.0, breaks = seq(0, 1.0, 0.02), show.legend = FALSE, size = 0.5) +
  # metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.3, show.legend = FALSE, size = 0.5) +
  # metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.5, show.legend = FALSE, size = 1) +
  # metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.7, show.legend = FALSE, size = 2) +
  # metR::geom_contour2(color = "black", alpha = 1.0, breaks = 0.9, show.legend = FALSE, size = 4) +
  # metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = seq(0, 1.0, 0.02), rotate = TRUE, na.rm = TRUE, size = 5) +
  # metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = seq(0, 1.0, 0.05), rotate = TRUE, na.rm = TRUE, size = 5) +
  # metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = c(0.40, 0.48, 0.53, 0.55), rotate = TRUE, na.rm = TRUE, size = 5) +
  # metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, skip = 0, breaks = c(0.4, 0.5, 0.6, 0.7), rotate = TRUE, na.rm = TRUE, size = 5) +
  # geom_point(data = sampleData, aes(x = lon, y = lat, colour = factor(val), fill = NULL, z = NULL)) +
  geom_sf(data = mapGlobal, aes(x = NULL, y = NULL, fill = NULL, z = NULL), color = "black", fill = NA) +
  geom_point(data = maxData, aes(x = xAxis, y = yAxis, colour = meanVal, fill = NULL, z = NULL), color = "red") +
  metR::scale_x_longitude(breaks = seq(90, 150, 10), limits = c(90, 150), expand = c(0, 0)) +
  metR::scale_y_latitude(breaks = seq(10, 60, 10), limits = c(10, 60), expand = c(0, 0)) +
  labs(
    subtitle = NULL
    , x = NULL
    , y = NULL
    , fill = NULL
    , colour = NULL
    , title = NULL
  ) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 10, dpi = 600)

file.show(saveImg)


#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

rm(list = ls())

prjName = "o2job"

source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

#================================================
# 요구사항
#================================================

serviceName = "LSH0106"

library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(openxlsx)
library(foreign)
library(maptools)


fileInfo = Sys.glob(paste(globalVar$inpPath, "google_web.dbf", sep = "/"))

data = foreign::read.dbf(fileInfo, as.is = TRUE)

colnames(data)

dataL1 = data %>%
  as.tibble() %>%
  dplyr::select(2:11)


dataL2 = dataL1 %>%
  dplyr::slice(1:1000) %>%
  dplyr::group_by(DEPART, PUBLIC) %>%
  dplyr::summarise(cnt = n())


wb = openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "dataL1")
openxlsx::writeData(wb, "dataL1", dataL1, startRow = 1, startCol = 1)
saveFile = sprintf("%s/%s_%s.xlsx", globalVar$outPath, serviceName, "google_web")
openxlsx::saveWorkbook(wb, file = saveFile)

# saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "google_web")
# readr::write_csv(x = dataL1, path = saveFile)
# utils::write.csv(dataL1, file = saveFile)


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
# R,python 2개로 개발할시, 예를들어서 csv를 인풋하면 학습데이터,테스터데이터 2가지 파일로 랜덤으로 분리되서  저장이되면 좋겠네요. 코드 돌리면서 분리하는 과정에서 학습과 테스터의 퍼센테이지를 그때그때 적용하면서 코드를 돌리면 좋겠네요.
# 그런데요 퍼센테이지도 입력을 할 수 있는데, 나중에 데이터 항목들이 늘어나거나 줄어들거나, 변동이 생길때도 생각해서 이부분도 참고해주셔
# 입력자료 1 : input1.csv
# 학습 및 테스트 비율 : 80:20
# 출력자료 : 
# input1_trainData_80:20.csv
# input1_testData_80:20.excel

#================================================
# Set Env
#================================================
# globalVar = new.env()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0107"

library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(openxlsx)
library(foreign)
library(maptools)

set.seed(1)
trainPerRat = 80

fileList = Sys.glob(paste(globalVar$inpPath, "LSH0107_input*.csv", sep = "/"))

for (fileInfo in fileList) {

  data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "UTF-8"))

  #=====================================================================
  # 훈련 및 테스트 셋 설정 (80 : 20)
  #=====================================================================
  # 훈련 및 데이터 셋을 80:20으로 나누기 위한 인덱스 설정
  ind = sample(1:nrow(data), nrow(data) * trainPerRat / 100)

  # 해당 인덱스에 따라 자료 할당
  trainData = data[ind,]
  testData = data[-ind,]

  # 훈련 데이터셋 확인
  # dplyr::tbl_df(trainData)

  # 테스트 데이터셋 확인
  # dplyr::tbl_df(testData)

  fileName = tools::file_path_sans_ext(fs::path_file(fileInfo))

  # 훈련 데이터셋 CSV 저장
  saveTrainCsvFile = sprintf("%s/%s_%s_%s-%s.csv", globalVar$outPath, fileName, "trainData", trainPerRat, 100 - trainPerRat)
  readr::write_csv(x = trainData, file = saveTrainCsvFile)

  # 테스트 데이터셋 CSV 저장
  saveTestCsvFile = sprintf("%s/%s_%s_%s-%s.csv", globalVar$outPath, fileName, "testData", trainPerRat, 100 - trainPerRat)
  readr::write_csv(x = testData, file = saveTestCsvFile)

  # 훈련/테스트 데이터셋 EXCEL 저장
  saveExcelFile = sprintf("%s/%s_%s-%s.xlsx", globalVar$outPath, fileName, trainPerRat, 100 - trainPerRat)

  wb = openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, "trainData")
  openxlsx::writeData(wb, "trainData", trainData, startRow = 1, startCol = 1)

  openxlsx::addWorksheet(wb, "testData")
  openxlsx::writeData(wb, "testData", testData, startRow = 1, startCol = 1)

  openxlsx::saveWorkbook(wb, file = saveExcelFile, overwrite = TRUE)
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

#================================================
# 요구사항
#================================================
# 2. 데이터에서 다음의 4개의 변수들 SP, DAX, FTSE, NIKKEI 간의 상관행렬 (correlation matrix) 를 구하시오. 가장 상관관계가 높은 변수들과 상관관계가 가장 낮은 변수들을 고르시오.

# 6. 숫자 1로만 이루어진 변수를 하나 생성하여 stock.data 의 첫번째 열로 추가하여 합쳐서 이를 stock.data2로 저장하시오. 그 후 새롭게 생성된 열의 변수명을 ONE 이라고 지정하시 오. (주의: 숫자 1의 개수는 데이터 행의 수와 같아야 함.) 마지막으로 처음 세 행의 데이터만 보이도록 head 명령어를 이용하여 출력하시오. 

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0108"
showtext::showtext_opts(dpi = 100)
showtext::showtext.auto()

library(tidyverse)
library(psych)
library(corrr)

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0108_Stock.csv", sep = "/"))

stock.data = read.table(file = fileInfo, sep = ",", header = TRUE)

#====================================================================================
# 2번
#====================================================================================
# 상관 행렬 그림
stock.data %>%
  dplyr::select(SP, DAX, FTSE, NIKKEI) %>% # 4개 변수 선택
  psych::cor.plot() # 상관계수 행렬 그림

# 가장 상관관계가 높은 변수 : FTSE-DAX
# 가장 상관관계가 낮은 변수 : SP-NIKKEI
stock.data %>%
  dplyr::select(SP, DAX, FTSE, NIKKEI) %>% # 4개 변수 선택
  corrr::correlate() %>% # 상관계수 계산
  tidyr::gather(-term, key = "colname", value = "cor") %>% # 데이터 전처리
  dplyr::filter(!is.na(cor)) %>%
  dplyr::arrange(desc(cor)) # 상관계수에 따른 내림차순 정렬

#====================================================================================
# 6번
#====================================================================================
stock.data2 = cbind(1, stock.data) %>% # 최 상단 컬럼에 1 추가
  dplyr::rename("ONE" = "1") # 칼럼명 변경 (1 > ONE)

# 데이터 행의 수
nrow(stock.data2)

# 숫자 1의 개수
length(stock.data2[, "ONE"])

# 처음 3행 출력
head(stock.data2, 3)


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
# 실습과제 (1, 5, 6번)

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0109"
showtext::showtext_opts(dpi = 100)
showtext::showtext.auto()

# Q1. sample(1:100, 50) 함수를 이용하여 벡터 random_num에 50개의 임의의 정수를 저장 하고 반복문 for를 이용하여 벡터 random_num에 있는 홀수 정수를 출력하는 문제이며 홀수 정수의 출력은 벡터 random_num에서 첫 번째 짝수가 발견될 때까지의 홀수 정수를 출력한다.
# 그래서 첫 번째 짝수가 발견되면 더 이상 출력하지 않고 for 반복문을 빠져나온다. 
# for 반복문을 빠져나오기 위해서 반복문 안에서 if 조건문과 break를 이용하면 for 반복문을 빠져나올 수 있으며 만약 여러 중첩된(nested) for문에서 break를 이용하는 경우에는 가장 가까운 for문만 빠져나올 수 있음. 아래와 같이 한 줄에 하나의 정수를 출력하는 코드를 작성하시오.

random_num = sample(1:100, 50)

for (i in random_num) {

  # 짝수인 경우
  if (random_num[1] %% 2 == 0) {
    break
  }

  # 홀수인 경우
  if (i %% 2 == 1) {
    cat(i, "\n")
  }
}

# Q5. “정수 입력: 이라는 입력 메시지를 출력하고, scan 함수를 사용하여 하나의 정수 입력 받아서, 변수 x 에 저장한다( 1 < x <= 1000 ).
# 홀수의 곱: 1 * 3 * 5 * * * n >= x
# 위의 식을 만족하는 가장 작은 홀수 n을 출력하는 코드를 작성하시오.

# 반복 실행
repeat {

  # 정수 입력 및 정수형 변경
  x = as.numeric(scan(what = "정수 입력", n = 1, nlines = 1))

  # 변수 범위 설정 및 출력
  if (x > 1 && x <= 1000) {
    cat("정수 입력: ", x, "\n")
    break
  }
}

# 홀수값 초기화
oddMat = 1
for (n in seq(1, 999, 2)) {
  oddMat = oddMat * n

  # 홀수의 곱을 만족하는 조건식
  if (oddMat >= x) {
    cat("n: ", n, "\n")

    break
  }
}


# Q6. "10개 문자 입력:" 이라는 입력 메시지를 출력하고, scan(what=", n=10) 함수를 사용하 여 10개 문자 입력 받아서, 벡터 in_chs 에 저장한다.
# 벡터 in_chs 에 저장된 10개 문자에서 반복되는 패턴을 찾는 문제이며 반복되는 패턴이 2개 존재하거나 반복되는 패턴이 없을 수 있음. 
# 2개 문자로 구성된 반복되는 패턴을 찾는 문제이며 1개의 문자가 반복되는 것은 반복되는 패턴이 아님.
# 벡터 in_chs 에 저장된 10개 문자에서 반복되는 패턴을 찾으면 아래와 같이 출력하는 코드를 작성하시오.
# <입력 메시지> 10개의 문자 입력:
# <출력 (입력 예: S => a => d => 4 => r => s => a => t => r => f) found pattern: sa

# 문자열 10개 입력
in_chs = scan(what = "", n = 10)

data = c()
for (i in 1:(length(in_chs) - 1)) {

  # 문자열 인덱스 1-2번째 그리고 소문자 변환
  getStr = tolower(paste0(in_chs[i], in_chs[i + 1]))

  # 데이터셋 append
  data = append(data, getStr)

  cat(i, getStr, "\n")
}

# 중복값 출력
cat("found pattern:", data[duplicated(data)], "\n")

#===============================================================================================
# Routine : Main R program
#
# Purpose : 재능상품 오투잡
#
# Author : 해솔
#
# Revisions: V1.0 May 28, 2020 First release (MS. 해솔)
#===============================================================================================

#************************************************
# 요구사항
#************************************************
# 과제물 제출

serviceName = "LSH0110"

# 벡터 인덱싱(5)의 강수량 관련 예제의 출력이 한글로 되도록 수정
# month.kname[which(rainfall > 100)] 명령 수행시 결과값이 5월, 6월, 7월, 8월, 9월로 나와야 함
# 힌트: month.kname 벡터를 한글 월 이름으로 정의
rainfall <- c(21.6, 23.6, 45.8, 77.0, 102.2, 133.3, 327.9, 348.0, 137.6, 49.3, 53.0, 24.9)
month.kname <- sprintf("%s월", 1:12)

month.kname[which(rainfall > 100)]

# 1) 1부터 12까지의 숫자 벡터로 3x4 행렬을 생성하여 변수 mtx에 할당하고
# 상수벡터 letters를 이용하여 행 이름과 열 이름을 지정할 것
mtx = matrix(1:12, nrow = 3, ncol = 4, dimnames = list(letters[1:3], letters[1:4]))
mtx

# 2) 1)번에서 만들어진 mtx 행렬에 (7,7,7,7)로 이루어진 행 추가
mtx = rbind(mtx, c(7, 7, 7, 7))
mtx

# 3) matrix(1:10000, 1000)인 행렬 mtx2를 생성하고 777번째 행과 3번째 열의 값을 추출할 것
mtx2 = matrix(1:10000, 1000)
mtx2[777, 3]


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
# 실습과제 (2, 3, 4번)

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0111"
showtext::showtext_opts(dpi = 100)
showtext::showtext.auto()

library(plotROC)

data = data.frame(
  OBS = 1:10
  , ACTUAL = c("P", "P", "P", "P", "P", "N", "N", "N", "N", "N")
  , MODEL1 = c(0.7, 0.8, 0.65, 0.9, 0.45, 0.5, 0.55, 0.35, 0.4, 0.6)
  , MODEL2 = c(0.75, 0.8, 0.65, 0.85, 0.3, 0.45, 0.55, 0.35, 0.4, 0.25)
)

# 하나는 모델1에 대해, 다른 하나는 모델 2에 대해 두 개의 ROC 곡선을 그립니다. 다른 색 (예: 모델 1의 빨간색과 모델 2의 파란색)을 사용하면 더 좋습니다.

pred1 = roc(data$ACTUAL, data$MODEL1)
pred2 = roc(data$ACTUAL, data$MODEL2)

pROC::plot.roc(pred1, col = "red", print.auc = TRUE, print.auc.adj = c(1, 0.5), max.auc.polygon = TRUE, print.thres = TRUE, print.thres.pch = 19, print.thres.col = "red", auc.polygon = TRUE, auc.polygon.col = "#D1F2EB", print.thres.adj = c(-0.1, 2))

pROC::plot.roc(pred2, add = TRUE, col = "blue", print.auc = TRUE, print.auc.adj = c(-0.2, 0.5), print.thres = TRUE, print.thres.pch = 19, print.thres.col = "blue", print.thres.adj = c(-0.1, 4))

legend("bottomright", legend = c("MODEL1", "MODEL2"), col = c("red", "blue"), lwd = 2)

# 각 ROC 곡선에 대한 AUC (곡선 아래의 영역)를 계산하고 한 모형이 다른 모형보다 우수한지 여부를 결론을 내립니다.

# MODEL1의 경우 0.88 (Good)로서 MODEL2보다 높은 정확도를 보인다.

# 새 모형을 개발했으며 다른 두 모형보다 훨씬 우수하다는 것을 보여주고자 했다고 가정합니다.
# 모형의 ROC 곡선에서 원하는 특성은 무엇입니까?
# 모델이 생성 할 수있는 확률의 샘플 세트를 제공하십시오.

# ACTUAL에 따른 P, N의 값의 분포에 따라 정확성이 달라집니다.
# 즉 P의 경우 1에 가까울수록 그리고 N의 경우 0에 가까울수록 우수한 모델이 됩니다.
data = data.frame(
  OBS = 1:10
  , ACTUAL = c("P", "P", "P", "P", "P", "N", "N", "N", "N", "N")
  , MODEL3 = c(1.0, 1.0, 1.0, 1.0, 1.0, 0, 0, 0, 0, 0)
)

pred3 = roc(data$ACTUAL, data$MODEL3)

pROC::plot.roc(pred3, col = "red", print.auc = TRUE, print.auc.adj = c(1, 0.5), max.auc.polygon = TRUE, print.thres = TRUE, print.thres.pch = 19, print.thres.col = "red", auc.polygon = TRUE, auc.polygon.col = "#D1F2EB", print.thres.adj = c(-0.1, 2))


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
# Y= NPS (1: 만족, 0: 불만족)
# X= region, product, sex, age, family
# 
# 타이타닉 생존율 시각화 및 생존 예측모델 유사하게 해주시면 될것같아요.
# 데이터 전처리는 필요없이 100% 되어 있고, 변수도 몇개 없긴합니다.
# 
# 기본적인 데이터 시각화
# - 주요 변수 대비 NPS 비율 및 변수간 모자익플롯 등 데이터 시각화 부탁드립니다.
# 
# 통계적 분석
# - NPS 1: 만족, 0: 불만족에 영향을 주는 주요변수 도출 및 검증
# - logistic regression, ramdom forest 등으로 NPS 예측모형
# - 모델 정확도 비교 (confusion matrix, ROC 커브 등)

#================================================
# 추가사항
#================================================
# 데이터 비율
# - NPS 0 : 3312개
# - NPS 1 : 25897개
# 로 학습 모형시 오직 1로만 예측되는 모형이 됩니다.
# 
# 따라서 NPS 0, 1로 적절한 분포하여 자료 개수를 수행
# - NPS 0 : 3312개
# - NPS 1 : 3312개
# 훈련 및 테스트 셋 설정 (70 : 30)
# 훈련/테스트 데이터셋으로 로지스틱 수행
# 훈련/테스트 데이터셋으로  수행

#================================================
# Set Env
# ================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0112"
showtext::showtext_opts(dpi = 100)
showtext::showtext.auto()

library(tidyverse)
library(lubridate)
library(openxlsx)
library(fs)
library(ggmosaic)
library(ggplot2)
library(caret)
library(randomForest)
library(ROCR)
library(abdiv)
library(scales)

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0112_npsrawdata.csv", sep = "/"))
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "UTF-8"))

# Y= NPS (1: 만족, 0: 불만족)
# X= region, product, sex, age, family
# 
# 타이타닉 생존율 시각화 및 생존 예측모델 유사하게 해주시면 될것같아요.
# 데이터 전처리는 필요없이 100% 되어 있고, 변수도 몇개 없긴합니다.
# 
# 주요 변수 대비 NPS 비율
data %>%
  dplyr::group_by(NPS) %>%
  dplyr::summarise(
    cnt = n()
    , ratio = (n() / nrow(data)) * 100.0
  )

# 변수간 모자익플롯 등 데이터 시각화
mosaicplot(~region + product + NPS + sex + family, data = data, color = TRUE)

ggplot(data, aes(x = region, fill = as.factor(NPS))) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  scale_y_continuous(labels = scales::percent) +
  ylab("percent [%]")

ggplot(data, aes(x = product, fill = as.factor(NPS))) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  scale_y_continuous(labels = scales::percent) +
  ylab("percent [%]")

ggplot(data, aes(x = sex, fill = as.factor(NPS))) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  scale_y_continuous(labels = scales::percent) +
  ylab("percent [%]")

ggplot(data, aes(x = family, fill = as.factor(NPS))) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  scale_y_continuous(labels = scales::percent) +
  ylab("percent [%]")


# 통계적 분석
# - NPS 1: 만족, 0: 불만족에 영향을 주는 주요변수 도출 및 검증
# - logistic regression, ramdom forest 등으로 NPS 예측모형
# - 모델 정확도 비교 (confusion matrix, ROC 커브 등)

tmpData0 = data %>%
  dplyr::filter(NPS == 0)

tmpData1 = data %>%
  dplyr::filter(NPS == 1)

ind = sample(1:nrow(tmpData1), nrow(tmpData0))
tmpData2 = tmpData1[ind,]

dataL1 = dplyr::bind_rows(tmpData0, tmpData2)
dataL1$NPS = as.factor(dataL1$NPS)

# 전체 변수에 대한 최적의 변수 선정
# 1) AIC 기준으로 변수 선택
glmFitVarAll = glm(NPS ~ ., data = dataL1, family = binomial)
rsStepAic = MASS::stepAIC(glmFitVarAll, direction = "both")

# 결과에 대한 요약
summary(rsStepAic)

# 한 눈에 분석 결과 확인 가능
# 초기 : NPS ~ region + product + sex + age + family
# 최적 : NPS ~ region + product + age + family
rsStepAic$anova

#=====================================================================
# 훈련 및 테스트 셋 설정 (70 : 30)
#=====================================================================
# 훈련 및 데이터 셋을 70:30으로 나누기 위한 인덱스 설정
ind = sample(1:nrow(dataL1), nrow(dataL1) * 0.7)

# 해당 인덱스에 따라 자료 할당
trainData = dataL1[ind,]
testData = dataL1[-ind,]

# 훈련 데이터셋 확인
dplyr::tbl_df(trainData)

# 테스트 데이터셋 확인
dplyr::tbl_df(testData)

#=====================================================================
# 로지스틱 회귀모형 수행
#=====================================================================
# 독립변수 : NPS 제외한 전체 변수
# 종속변수 : NPS
glmFit = glm(NPS ~ product + age + family, data = trainData, family = binomial)

# 회귀모형에 대한 요약 결과
summary(glmFit)

# 실제 NPS
yObs = as.numeric(as.character(testData$NPS))
yObsFac = factor(yObs, levels = c(0, 1), labels = c('Yes', 'No'))

# 테스트셋을 이용한 예측 NPS
yHatPred = as.numeric(predict.glm(glmFit, newdata = testData, type = "response"))
yHat = ifelse(yHatPred > 0.5, 1, 0)
yHatFac = factor(yHat, levels = c(0, 1), labels = c('Yes', 'No'))

#================================================
# 모델 정확도 비교
#================================================
# 혼동 매트릭스
confusionMatrix(yHatFac, yObsFac)

# 검증 측정을 위한 기초 설정
lmPred = ROCR::prediction(yHat, yObs)

# ROC 커브를 위한 설정
perform = ROCR::performance(lmPred, "tpr", "fpr")
plot(perform, main = 'ROC Curve')

# AUC 측정 : 1에 가까울수록 최고 성능 : 0.6011943117
ROCR::performance(lmPred, "auc")@y.values[[1]]

# 이항편차 측정 : 낮을수록 좋음 : 550.3588614
abdiv::binomial_deviance(yObs, yHat)


#=====================================================================
# RandomForest 회귀모형 수행
#=====================================================================
# 독립변수 : NPS 제외한 전체 변수
# 종속변수 : NPS
rfFit = randomForest::randomForest(NPS ~ product + age + family, data = trainData)
yHatRfPred = predict(rfFit, newdata = testData)

yHatRf = ifelse(as.numeric(yHatRfPred) > 1, 1, 0)
yHatRfFac = factor(yHatRf, levels = c(0, 1), labels = c('Yes', 'No'))

# 혼동 매트릭스
confusionMatrix(yHatRfFac, yObsFac)

# 검증 측정을 위한 기초 설정
lmRfPred = ROCR::prediction(yHatRf, yObs)

# ROC 커브를 위한 설정
performRf = ROCR::performance(lmRfPred, "tpr", "fpr")
plot(performRf, main = 'ROC Curve')

# AUC 측정 : 1에 가까울수록 최고 성능 : 0.6009015088
ROCR::performance(lmRfPred, "auc")@y.values[[1]]

# 이항편차 측정 : 낮을수록 좋음 : 551.0520085
abdiv::binomial_deviance(yObs, yHatRf)


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
# 데이터 세트는 "indiv_level_data"및 "team_level_data"라는 Excel 파일이고 R 구문 파일은 "method_syntax"라고합니다. 데이터 세트에는 다음 변수가 포함됩니다.
# 
# 개별 수준 데이터 세트
# 1) 팀 코드 = 팀 ID 번호
# 2) coop_1 ~ coop_5 = 팀 협력에 대한 개별 수준의 직원 인식 (5 개 항목)
# 3) commun_1 ~ commun_3 = 팀 커뮤니케이션에 대한 개별 수준의 직원 인식 (3 개 항목)
# 
# 팀 수준 데이터 세트
# 1) 팀 코드 = 팀 ID 번호
# 2) tper = 팀 리더의 팀 성과 등급
# 3) 서번트 = 팀 리더의 서번트 리더십 (1 개 항목, 1 = 서번트 팀 리더, 0 = 서번트 팀 리더가 아님과 같은 더미 변수) ? 주로 팔로워의 성장과 복지에 초점을 맞추고 그들을 지원하는 리더십 행동
# 4) 리드 스트레스 = 팀장의인지 스트레스 수준 (1 개 항목)
# 5) 팀 규모 = 팀원 수 (분석을위한 제어 변수로 고려하십시오)

# 가정 된 모델은 팀 수준의 관계를 기반으로하며 종속 변수는 팀 성과, 독립 변수는 서번트 리더십, 매개 변수는 팀 협력, 팀 커뮤니케이션, 리더의 스트레스 수준입니다. 팀 협력 및 커뮤니케이션 변수는 개인별로 측정됩니다. 팀 수준으로 집계되어야합니다. 기본 연구 모델 및 방법 관련 질문은 다음과 같습니다.

# 질문보고
# 연구 모델은 서번트 리더십이 세 가지 매개 메커니즘 (즉, 리더의 스트레스 수준, 팀 협력 및 팀 커뮤니케이션)을 통해 팀 성과와 긍정적으로 관련되어 있다고 가정합니다.
# 
# 가설 1 : 서번트 리더십은 팀 성과와 긍정적 인 관련이 있습니다.
# 가설 2 : 서번트 리더십은 팀 협력과 긍정적 인 관련이 있습니다.
# 가설 3 : 서번트 리더십은 팀 커뮤니케이션과 긍정적 인 관련이 있습니다.
# 가설 4 : 서번트 리더십은 팀 리더의 스트레스 수준과 긍정적 인 관련이 있습니다.
# 가설 5a / 5b / 5c : 팀 협력 (H5a), 팀 커뮤니케이션 (H5b) 및 팀 리더의 스트레스 수준 (H5c)은 서번트 리더십과 팀 성과 간의 관계를 중재합니다.
# 
# 1. 이러한 가설을 검증하기 위해서는 먼저인지 된 팀 협력 및 의사 소통과 같은 개별 수준의 변수를 팀 수준으로 집계해야합니다. 변수의 ICC [1], ICC [2], rwg를 확인하고보고하십시오. 두 변수가 팀 수준으로 집계 될 수 있는지 판단합니다.
# 
# 
# 2. OLS 회귀를 실행하여 가설 관계 (가설 1에서 가설 4로)를 테스트하고 R 구문 및 결과의 결과를 첨부 한 다음 결과를 해석하십시오 (미리 결정된 가설이 지원되는지 여부 판단).
# 
# 
# 3. 부트 스트랩 방법을 실행하여 가설 5a, 5b 및 5c가 지원되는지 여부를 확인한 다음 결과를 해석하십시오 (R 구문 및 결과의 결과를 첨부하십시오).

#================================================
# 추가사항
#================================================

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0113"
showtext::showtext_opts(dpi = 100)
showtext::showtext.auto()

# loading packages
library(DBI)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(sjPlot)
library(Hmisc)
library(pequod)
library(car)
library(R.matlab)
library(VGAM)
library(multilevel)
library(lattice)
library(lavaan)
library(mediation)
library(psych)

# Set working directory (adjust this so it points to the directory on your machine 
# where the files are)
# setwd("D:/R")

# creating a new dataset "data_team" and "data_indiv"
fileInfo1 = Sys.glob(paste(globalVar$inpPath, "LSH0113_team_level_data.csv", sep = "/"))
data_team = read.csv(file = fileInfo1)

fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0113_indiv_level_data.csv", sep = "/"))
data_indiv = read.csv(file = fileInfo2)

summary(data_team)
summary(data_indiv)

data_indivL1 = data_indiv %>%
  tibble::rowid_to_column() %>%
  dplyr::group_by(rowid) %>%
  dplyr::mutate(
    meanCoop = mean(c(coop1, coop2, coop3, coop4, coop5), na.rm = TRUE)
    , meanCommun = mean(c(commun1, commun2, commun3), na.rm = TRUE)
  )

data_indivL2 = data_indivL1 %>%
  dplyr::group_by(teamcode) %>%
  dplyr::summarise(
    coop = mean(meanCoop, na.rm = TRUE)
    , commun = mean(meanCommun, na.rm = TRUE)
  ) %>%
  dplyr::select(teamcode, coop, commun)

dataL1 = data_team %>%
  dplyr::left_join(data_indivL2, by = c("teamcode" = "teamcode"))

# 1. 이러한 가설을 검증하기 위해서는 먼저인지 된 팀 협력 및 의사 소통과 같은 개별 수준의 변수를 팀 수준으로 집계해야합니다.
# 변수의 ICC [1], ICC [2], rwg를 확인하고 보고하십시오.
# 두 변수가 팀 수준으로 집계 될 수 있는지 판단합니다.
rsAov = aov(teamcode ~ meanCoop + meanCommun, data = data_indivL1)

# 0.9821258417
multilevel::ICC1(rsAov)

# 0.9880124721
multilevel::ICC2(rsAov)

rsRwg = multilevel::rwg(data_indivL1$meanCoop + data_indivL1$meanCommun, data_indivL1$teamcode)

# 0.3411650263
mean(rsRwg$rwg, na.rm = TRUE)

# 2. OLS 회귀를 실행하여 가설 관계 (가설 1에서 가설 4로)를 테스트하고 R 구문 및 결과의 결과를 첨부 한 다음 결과를 해석하십시오 (미리 결정된 가설이 지원되는지 여부 판단).

# 가설 1 : 서번트 리더십은 팀 성과와 긍정적 인 관련이 있습니다.

# 결정계수 (상관계수^2)의 경우 0.1101699로서 양의 관계를 지니고 있음
# 이러한 결과는 유의수준 (p-value) 0.001 이하로서 통계적으로 유의하다.
lmFit = lm(servant ~ tper, data = dataL1)
summary(lmFit)

# 가설 2 : 서번트 리더십은 팀 협력과 긍정적 인 관련이 있습니다.

# 결정계수 (상관계수^2)의 경우 0.02136523로서 양의 관계를 지니고 있음
# 이러한 결과는 유의수준 (p-value) 0.001 이하로서 통계적으로 유의하다.
lmFit = lm(servant ~ coop, data = dataL1)
summary(lmFit)

# 가설 3 : 서번트 리더십은 팀 커뮤니케이션과 긍정적 인 관련이 있습니다.

# 결정계수 (상관계수^2)의 경우 0.05518577로서 양의 관계를 지니고 있음
# 이러한 결과는 유의수준 (p-value) 0.001 이하로서 통계적으로 유의하다.
lmFit = lm(servant ~ commun, data = dataL1)
summary(lmFit)

# 가설 4 : 서번트 리더십은 팀 리더의 스트레스 수준과 긍정적인 관련이 있습니다.

# 결정계수 (상관계수^2)의 경우 0.01655538로서 양의 관계를 지니고 있음
# 이러한 결과는 유의수준 (p-value) 0.01 이하로서 통계적으로 유의하다.
lmFit = lm(servant ~ leadstress, data = dataL1)
summary(lmFit)

# 3. 부트 스트랩 방법을 실행하여 가설 5a, 5b 및 5c가 지원되는지 여부를 확인한 다음 결과를 해석하십시오 (R 구문 및 결과의 결과를 첨부하십시오).

# 가설 5a / 5b / 5c : 팀 협력 (H5a), 팀 커뮤니케이션 (H5b) 및 팀 리더의 스트레스 수준 (H5c)은 서번트 리더십과 팀 성과 간의 관계를 중재합니다.

# Number of repetition
repNum = 1000

# Number of sample
sampleNum = nrow(dataL1)

# 가설 5a / 5b / 5c : 팀 협력 (H5a)
xVar = dataL1$coop
yVar = dataL1$servant

# FALSE : 비복원 추출(무작위 정렬) : 한번 뽑은 것을 다시 뽑을 수 없는 추출
xSampleList = lapply(1:repNum, function(i) sample(xVar, sampleNum, replace = FALSE))
ySampleList = lapply(1:repNum, function(i) yVar)

corSampleList = mapply(cor, xSampleList, ySampleList)

# 상관계수 : 0.1530967162
cor(xVar, yVar)

# 유의수준 :
# 95%         97.5%         99.8% 
# 0.07444762572 0.09048028248 0.13815082357 
quantile(corSampleList, probs = c(0.95, 0.975, 0.998))


# 팀 커뮤니케이션 (H5b)
# 가설 5a / 5b / 5c : 팀 협력 (H5a)
xVar = dataL1$commun
yVar = dataL1$servant

# FALSE : 비복원 추출(무작위 정렬) : 한번 뽑은 것을 다시 뽑을 수 없는 추출
xSampleList = lapply(1:repNum, function(i) sample(xVar, sampleNum, replace = FALSE))
ySampleList = lapply(1:repNum, function(i) yVar)

corSampleList = mapply(cor, xSampleList, ySampleList)

# 상관계수 : 0.2391390691
cor(xVar, yVar)

# 유의수준 : 0.001 이하
#           95%         97.5%         99.8% 
# 0.07665891282 0.09066556515 0.13027331685
quantile(corSampleList, probs = c(0.95, 0.975, 0.998))

# 팀 리더의 스트레스 수준 (H5c)
xVar = dataL1$leadstress
yVar = dataL1$servant

# FALSE : 비복원 추출(무작위 정렬) : 한번 뽑은 것을 다시 뽑을 수 없는 추출
xSampleList = lapply(1:repNum, function(i) sample(xVar, sampleNum, replace = FALSE))
ySampleList = lapply(1:repNum, function(i) yVar)

corSampleList = mapply(cor, xSampleList, ySampleList)

# 상관계수 : 0.1365245309
cor(xVar, yVar)

# 유의수준 : 0.001 이하
# 95% (0.01 이하)         97.5% (0.05 이하)         99.8% (0.001 이하)
# 0.06999343718 0.08796962693 0.12375567868 
quantile(corSampleList, probs = c(0.95, 0.975, 0.998))

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
# 과제 #03. 리스트 생성하기
# 과제 04. 데이터 프레임 다루기

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0114"
showtext::showtext_opts(dpi = 100)
showtext::showtext.auto()

#================================================
# 과제 #03. 리스트 생성하기
#================================================
# 1)	요소로 이름, 학교, 학년을 가지는 리스트 student를 생성하고("이수진", "동고", 2)과 ("김우혁, "서고", 3)를 입력하세요. (4점)
student = list(
  "이름" = c("이수진", "김우혁")
  , "학교" = c("동고", "서고")
  , "학년" = c(2, 3)
)

# 2)	student 리스트에 ("박찬호", "남고", 3)을 추가하세요. (3점)
student$이름[3] = c("박찬호")
student$학교[3] = c("남고")
student$학년[3] = c(3)

# 3)	student 리스트에 “성별” 요소를 추가하고 이수진은 “여”, 김우혁과 박선호는 “남”을 성별 요소로 입력하세요. (3점)
student["성별"] = list(
  c("여", "남", "남")
)

# 4)	최종 student 리스트를 출력하세요(1점)
print(student)

#================================================
# 과제 04. 데이터 프레임 다루기
#================================================
# mtcars 데이터 사용
library(dplyr)
library(sqldf)

# 1)	mtcars와 동일한 mtcars2 데이터프레임을 생성하고, (5점)
mtcars2 = mtcars

# 2)	mtcars2에 carname 컬럼을 추가 후 해당 컬럼에 mtcars2의 열 이름을 데이터로 추가, (5점)
mtcars2$carname = row.names(mtcars2)

# 3)	mtcars2의 열 이름 삭제 할 것(5점)
# Mtcars2 데이터 사용
row.names(mtcars2) = NULL

# 4)	Mazda 차량의 carname, mpg, hp, wt 를 R 함수를 사용하여 표시, (10점)
sqldf::sqldf("SELECT carname, mpg, hp, wt FROM mtcars2")

# 5)	Mazda와 Merc 차량의 carname, mpg, hp, wt 를 표시하는 코드 작성, 추가문제로 가산점 5점
sqldf::sqldf("SELECT carname, mpg, hp, wt FROM mtcars2 WHERE carname LIKE ('%Mazda%') OR carname LIKE ('%Merc%')")


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
# R 소스 코드를 이용한 마크다운, HTML 변환

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "E:/04. TalentPlatform/Github/TalentPlatform-R/resources/input/o2job"

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0115"
showtext::showtext_opts(dpi = 100)
showtext::showtext.auto()


# 각각 데이터 분석 셋에 대하여 전처리 및 분석 후 3데이터 풀링하여 분석 진행
# rm(list=ls())
library(tidyverse)

#================Dataset 1 :외국인인구수  ================#
fileInfo = Sys.glob(paste(globalVar$inpPath, "foreign_population.csv", sep = "/"))

# foreign<-read.csv(file = fileInfo, colClasses="character")
foreign = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))


colnames(foreign) <- c("기간", "자치구", "동", "합계",
                       "한국국적가지지않은자",
                       "외국인근로자", "결혼이민자", "유학생", "외국국적동포", "기타외국인",
                       "국적취득", "외국인주민자녀출생")

# dt<-foreign[-1,]  #첫행 제거
dt <- foreign

#change to numeric values : 특수문자 있는 경우 Numeric으로 변경하기 위함
dt$한국국적가지지않은자 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", dt$한국국적가지지않은자))
dt$합계 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", dt$합계))
dt$외국인근로자 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", dt$외국인근로자))
dt$결혼이민자 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", dt$결혼이민자))
dt$유학생 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", dt$유학생))
dt$외국국적동포 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", dt$외국국적동포))
dt$기타외국인 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", dt$기타외국인))
dt$국적취득 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", dt$국적취득))
dt$외국인주민자녀출생 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", dt$외국인주민자녀출생))

#합계라고 적힌 행은 삭제
dt2 <- subset(dt, 동 != "계" & 자치구 != "합계")


#지역별로 단순 인구수를 비교하는것보다 유의미한 결과를 위하여 외국인 국적 취득의 비율이라는 변수를 생성 
dt_f <- dplyr::group_by(dt2, 자치구) %>%
  dplyr::summarise("국적취득" = sum(국적취득, na.rm = T),
                   "결혼이민자" = sum(결혼이민자, na.rm = T),
                   "유학생" = sum(유학생, na.rm = T),
                   "외국국적동포" = sum(외국국적동포, na.rm = T),
                   "기타외국인" = sum(외국인근로자, na.rm = T),
                   "외국인주민자녀출생" = sum(외국인주민자녀출생, na.rm = T),
                   "합계" = sum(합계)) %>%
  mutate(국적취득비율 = 국적취득 / (합계 + 국적취득) * 100)

#Table 1. 구별로 Descriptive Table을 산출, 총 외국인 수중 국적을 취득한 비율은 약 8%정도.
#install.packages("pastecs")
library(pastecs)
Table <- stat.desc(dt_f)
output1 <- Table[c("nbr.val", "mean", "min", "max", "median"), -1]
output1


#output2 : 외국인 수가  많은 자치구 : 영등포구, 구로구, 금천구 
output2 <- ggplot(dt_f, aes(x = 자치구, y = 합계)) +
  geom_bar(position = "dodge", width = 0.5, stat = "identity") +
  ggtitle("서울 구별 외국인 인구 수 ") +
  theme(plot.title = element_text(size = 14, face = "bold.italic"))
output2

#output3 : 국적취득 비율이 높은 자치구 : 양천구 , 강서구, 은평구
output3 <- ggplot(dt_f, aes(x = 자치구, y = 국적취득비율)) +
  geom_bar(position = "dodge", width = 0.5, stat = "identity") +
  ggtitle("서울 구별 국적취득 비율") +
  theme(plot.title = element_text(size = 14, face = "bold.italic"))
output3


#새롭게 생성했던 "국적 취득 비율"과 다른 변수들과의 관계를 살핌.
#국적을 취득한 사람이 많은 지역은 어떤 특징을 가지고 있을지?


#install.packages("ggcorrplot")
library(ggcorrplot)
corr <- cor(dt_f[, -c(1, 2, 8)])   # 연관성을 볼때 필요없는 자치구/국적취득자/합계계 열은 삭제
output4 <- ggcorrplot(corr)
output4

#Result : 국적 취득 비율과 연관이 큰 변수는 외국인 주민자녀출생이며 연관성이 없는 변수는 유학생임.
#유학생은 거주 목적이 아니며 자녀출생은 거주의 목적이 있을 것으로 판단되어 상식과 일치하는 결과임.

#================Dataset 2 : 외국인등록자 수  ================#
fileInfo = Sys.glob(paste(globalVar$inpPath, "registered.csv", sep = "/"))
# reg<-read.csv(fileInfo)
reg = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))
head(reg)
colnames(reg) <- c("기간", "자치구", "동", "성별",
                   "계",
                   "A_0_4", "A_5_9", "A_10_14", "A_15_19", "A_20_24",
                   "A_25_29", "A_30_34", "A_35_39", "A_40_44", "A_45_49", "A_50_54", "A_55_59", "A_60_64"
  , "A_65_69", "A_70_74", "A_75_79", "A_80_84", "A_85_89", "A_90_94", "A_95_99", "A_100")

#change to numeric values
reg$A_0_4 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_0_4))
reg$A_5_9 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_5_9))
reg$A_10_14 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_10_14))
reg$A_15_19 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_15_19))
reg$A_20_24 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_20_24))
reg$A_25_29 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_25_29))
reg$A_30_34 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_30_34))
reg$A_35_39 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_35_39))
reg$A_40_44 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_40_44))
reg$A_45_49 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_45_49))
reg$A_50_54 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_50_54))
reg$A_55_59 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_55_59))
reg$A_60_64 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_60_64))
reg$A_65_69 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_65_69))
reg$A_70_74 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_70_74))
reg$A_75_79 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg333333333333333$A_75_79))
reg$A_80_84 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_80_84))
reg$A_85_89 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_85_89))
reg$A_90_94 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_90_94))
reg$A_95_99 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_95_99))
reg$A_100 <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", reg$A_100))


reg2 <- reg %>%
  dplyr::select(기간, 자치구, 동, 성별, 계, contains("A")) %>%
  spread(key = 성별, value = 계)

reg3 <- dplyr::group_by(reg2, 자치구) %>%
  dplyr::summarise("남자" = sum(남자, na.rm = T),
                   "여자" = sum(여자, na.rm = T),
                   "계" = sum(계, na.rm = T),
                   "A_65_69" = sum(A_65_69, na.rm = T),
                   "A_70_74" = sum(A_70_74, na.rm = T),
                   "A_75_79" = sum(A_75_79, na.rm = T),
                   "A_80_84" = sum(A_80_84, na.rm = T),
                   "A_85_89" = sum(A_85_89, na.rm = T),
                   "A_90_94" = sum(A_90_94, na.rm = T),
                   "A_95_99" = sum(A_95_99, na.rm = T),
                   "A_100" = sum(A_100, na.rm = T)
  )
reg3$upper65 = reg3$A_65_69 +
  reg3$A_70_74 +
  reg3$A_75_79 +
  reg3$A_80_84 +
  reg3$A_85_89 +
  reg3$A_90_94 +
  reg3$A_95_99 +
  reg3$A_100

reg3$proper = reg3$upper65 / reg3$계 * 100

#Output1 : Descriptive Table ; 인구별 분포를 파악하기 위해 65세 이상과 이하로 인구 카테고리 진행, 노인인구 비율은 약 4%로 보임
r_Table <- stat.desc(reg3)
r_output1 <- r_Table[c("nbr.val", "mean", "min", "max", "median"), -c(1, 5, 6, 7, 8, 9, 10, 11, 12)]
r_output1


#Output2 : 성별분포 파악을 위해 Boxplot을 그렸을때 평균은 여자 인구수가 더 많아보임..
boxplot(reg3$남자, reg3$여자, main = "성별에 따른 등록 외국인 수 분포포", names = c("남자", "여자"))

#Output3 : 통계적으로 차이가 있는지 확인 하기 위하여 외국인 성별  ttest결과 유의한 차이가 없음.
r_output3 <- t.test(reg3$남자, reg3$여자)
r_output3

#Output4 : 구별로 노인인구수 비율을 그림으로 그려봄
r_output4 <- ggplot(reg3, aes(x = 자치구, y = proper)) +
  geom_bar(position = "dodge", width = 0.5, stat = "identity") +
  ggtitle("서울 구별 등록 노인인구수 비율") +
  theme(plot.title = element_text(size = 14, face = "bold.italic"))
r_output4

#Output5 : 노인인구수 비율에 큰 아웃 라이어는 없음. 
boxplot(reg3$proper, main = "구별 등록 노인인구수 비율")

#================Dataset 3 : 인구밀도   ================#
fileInfo = Sys.glob(paste(globalVar$inpPath, "population_density.csv", sep = "/"))
# den<-read.csv(fileInfo)
den = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))
den2 <- subset(den, 동 != "소계" & 자치구 != "합계")

#평균 인구 밀도 계산
den3 <- dplyr::group_by(den2, 자치구) %>%
  dplyr::summarise("밀도" = mean(인구밀도, na.rm = T),
                   "인구" = mean(인구, na.rm = T)
  )

#추가 분석 아이디어 : 외국인 인구 등록 수는 서울 구별 인구 특징 중 하나인 인구밀도와 관련이 있을지?
final <- cbind(den3, reg3, dt_f)
colnames(final)
final <- final[, -c(4, 18)]   #자치구 중복 되어 열을 제거해줌

ggplot(data = final) +
  geom_point(mapping = aes(x = 밀도, y = 계), color = "red") +
  ggtitle("서울시 인구 밀도 별등록외국인의 분포") +
  theme(plot.title = element_text(color = "darkblue", size = 16))

#회귀분석 진행
fit = lm(계 ~ 밀도, data = final)
summary(fit)

#통계적으로 유의하지는 않으나 밀도의 계수가 음수 값으로 인구밀도가 높은곳에는 외국인 인구 등록 수와
#음의 상관관계가 있음 , 경제 지표 혹은 다른 covariate들을 보정하여 파악할 필요 있음.

#추가 분석2 : 외국인 인구 중 국적 취득한 사람들은 인구밀도와 관련이 있을지?
ggplot(data = final) +
  geom_point(mapping = aes(x = 국적취득, y = 밀도), color = "red") +
  ggtitle("서울시 인구 밀도 별등록외국인의 분포") +
  theme(plot.title = element_text(color = "darkblue", size = 16))
fit = lm(국적취득 ~ 밀도, data = final)
summary(fit)
#국적을 취득한 외국인들의 경우 인구밀도와 관련이 있는지 파악한 결과, 밀도 변수는 유의하지 않아 보임.

boxplot(final$국적취득) #그러나 분포를 보니  취득한 사람들의 비율에 아웃라이어 값이 있는것으로 파악됨
subset(final, 국적취득 > 2000)  #국적취득 외국인은 주로 강서구, 관악구, 구로구, 금천구, 영등포구에 많이 분포

#해당 지역 제거하고 분석
final2 <- subset(final, 국적취득 < 2000)

ggplot(data = final2) +
  geom_point(mapping = aes(x = 국적취득, y = 밀도), color = "red") +
  ggtitle("서울시 인구 밀도 별 등록외국인의 분포") +
  theme(plot.title = element_text(color = "darkblue", size = 16))

fit = lm(국적취득 ~ 밀도, data = final2)
summary(fit)


#아웃라이어를 제거하고 분석한 결과, 밀도가 낮은 지역과 국적취득 외국인 거주 인구수와 유의한 관계가 있음. 
#약 20%의 R스퀘어 값을 가지며, 밀도가 높을수록 국적취득 외국인 #거주 인구수는 줄어드는 것을 확인할수있음.
#고려할수있는 COVARIATE들로 함께 보정하여 분석해볼 필요가 있음.

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
# parallel 패키지를 통해 병렬 처리
# 기존 : 1개 20초 소요
# 신규 : 8개 40초 소요

#================================================
# Set Env
#================================================
globalVar = list()
globalVar$inpPath = "."
globalVar$figPath = "."
globalVar$outPath = "."

# rm(list = ls())
# prjName = "o2job"
# source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

#================================================
# Main
#================================================
library(raster)
library(tictoc)
library(raster)
library(sf)
library(doParallel)
library(parallel)
library(noncompliance)
library(tibble)

dataAll <- NULL

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0115_pr_day_CanESM5_historical_r1i1p1f1_gn_18500101-20141231.nc", sep = "/"))
r = raster::brick(fileInfo, hurrname = "t", level = 1, stopIfNotEqualSpaced = FALSE)

latList = seq(from = 31.125, to = 34, by = 0.25)
lonList = seq(from = 125.125, to = 128, by = 0.25)
saveFile = NA
serviceName = "LSH0117"

# 입력 데이터 
data = tibble::tibble(
  noncompliance::expand.grid.DT(
    latList
    , lonList
    , col.names = c("lon", "lat"))
)

# [시작] 병렬 처리
oSocClu = parallel::makePSOCKcluster(parallel::detectCores())
doParallel::registerDoParallel(oSocClu)

# 외부 변수 등록
parallel::clusterExport(oSocClu, "r")
parallel::clusterExport(oSocClu, "hurls")
parallel::clusterExport(oSocClu, "data")
parallel::clusterExport(oSocClu, "saveFile")
parallel::clusterExport(oSocClu, "globalVar")
parallel::clusterExport(oSocClu, "serviceName")

# 외부 라이브러리 등록
parallel::clusterEvalQ(oSocClu, library(raster))
parallel::clusterEvalQ(oSocClu, library(readr))

tictoc::tic()
# parallel::parSapply(oSocClu, X = 1:nrow(data), function(x) {
parallel::parSapply(oSocClu, X = 1:8, function(x) {
  hurls = data.frame(t(raster::extract(r, cbind(data$lat[x], data$lon[x]))))
  colnames(hurls) = c(paste(data$lat[x], data$lon[x], sep = "p"))

  saveFile = sprintf("%s/%s_%s_%s.csv", globalVar$outPath, serviceName, data$lat[x], data$lon[x])
  readr::write_csv(x = hurls, file = saveFile)
})
tictoc::toc()

# [종료] 병렬 처리
parallel::stopCluster(oSocClu)


############################################################################################################
# 기존 소스 코드
############################################################################################################
# rm(list=ls())
# gc()
# gc()
# memory.size()
# 
# setwd("H:/mon_gcm")
# library(raster)
# 
# dataAll<-NULL
# 
# 
# r <- brick("hur_Amon_ACCESS-ESM1-5_ssp245_r1i1p1f1_gn_201501-210012.nc", hurrname = "t", level=1, stopIfNotEqualSpaced = FALSE)
# 
# data1<-NULL
# k=1
# 
# 
# for(i in seq(from=31.125, to=40, by=0.25)) {
#   for(j in seq(from=125.125, to=135.125, by=0.25)) {
#     hurls <- t(extract(r, matrix(c(j,i), ncol = 2)))
#     colnames(hurls)<-c(paste(i,j, sep="p"))
#     if (ncol(hurls)>0) {
#       if (k==1) data1<-hurls
#       if (k>1) data1<-cbind(data1,hurls)
#     }
#     k=k+1
#   }
# }
# 
# 
# write.csv(data1,"hur_Amon_ACCESS-ESM1-5_ssp245_r1i1p1f1_gn_201501-210012.csv")


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
# mpg 데이터의 변수명은 긴 단어를 짧게 줄인 축약어로 되어있습니다. cty 변수는 도시 연비, hwy 변수는 고속도로 연비를 의미합니다. 변수명을 이해하기 쉬운 단어로 바꾸려고 합니다. mpg 데이터를 이용해서 아래 문제를 해결해 보세요.

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0115"
showtext::showtext_opts(dpi = 100)
showtext::showtext.auto()

#================================================
# Main
#================================================
library(ggplot2)
library(dplyr)

# Q1. ggplot2 패키지의 mpg 데이터를 사용할 수 있도록 불러온 뒤 복사본을 만드세요.
mpgData = mpg

# Q2. 복사본 데이터를 이용해서 cty는 city로, hwy는 highway로 변수명을 수정하세요.
mpgDataL1 = mpgData %>%
  dplyr::rename(
    city = cty
    , highway = hwy
  )

# Q3. 데이터 일부를 출력해서 변수명이 바뀌었는지 확인해 보세요. 아래와 같은 결과물이 출력되어야 합니다.
mpgDataL1


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
# 공개용 에듀 데이터 (EDSS)의 홈페이지 설명과 데이터를 이용하여 데이터를 파악하는데
# 의미 있는 다양한 기술 통계 (표, 그림, 평균, 분산)들을 만들어보고
# 결과 및 해석을 붙여 한 파일로 정리하여 제출하시오.
# EDSS는 회원가입만 하면 공객용 데이터는 바로 다운로드 받을 수 있게 되어 있습니다.
# R, Rex, Excel 상관없이 최소 10가지 이상의 결과를 만들어봅니다.

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0116"
showtext::showtext_opts(dpi = 600)
showtext::showtext.auto()

#================================================
# Main
#================================================
library(showtext)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(forcats)
library(DT)

# 의미 있는 다양한 기술 통계 (표, 그림, 평균, 분산)들을 만들어보고
fileInfo1 = Sys.glob(paste(globalVar$inpPath, "LSH0116_공개데이터_초18.xlsx", sep = "/"))
data = openxlsx::read.xlsx(fileInfo1, sheet = 1)
dataL1 = data %>%
  tibble::as.tibble() %>%
  dplyr::select(year:numcls_f6)

dataL1

fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0116_코드북및자료설명서_초18.xlsx", sep = "/"))
metaData = openxlsx::read.xlsx(fileInfo2, sheet = 1)
metaDataL1 = metaData %>%
  tibble::as.tibble() %>%
  dplyr::select(변수명, 주요제공항목) %>%
  dplyr::filter(!is.na(변수명), !is.na(주요제공항목))

metaDataL1

# 기술통계량
summary(dataL1)

#**********************************************************
# 시도명 (regname)에 따른 1-6학년 남학생/여학생 수
#**********************************************************
# 데이터 전처리 (문자열을 숫자형으로 변환, 학년별 학습별 학생수 합계)
dataL2 = dataL1 %>%
  dplyr::select(1:28) %>%
  readr::type_convert() %>%
  dplyr::mutate_at(vars(numcls_m1), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_f1), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_m2), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_f2), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_m3), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_f3), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_m4), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_f4), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_m5), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_f5), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_m6), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_f6), funs(as.numeric)) %>%
  dplyr::group_by(regname) %>%
  dplyr::summarise(
    sumM1 = sum(numcls_m1, na.rm = TRUE)
    , sumF1 = sum(numcls_f1, na.rm = TRUE)
    , sum1 = sum(numcls_m1, numcls_f1, na.rm = TRUE)
    , sumM2 = sum(numcls_m2, na.rm = TRUE)
    , sumF2 = sum(numcls_f2, na.rm = TRUE)
    , sum2 = sum(numcls_m2, numcls_f2, na.rm = TRUE)
    , sumM3 = sum(numcls_m3, na.rm = TRUE)
    , sumF3 = sum(numcls_f3, na.rm = TRUE)
    , sum3 = sum(numcls_m3, numcls_f3, na.rm = TRUE)
    , sumM4 = sum(numcls_m4, na.rm = TRUE)
    , sumF4 = sum(numcls_f4, na.rm = TRUE)
    , sum4 = sum(numcls_m4, numcls_f4, na.rm = TRUE)
    , sumM5 = sum(numcls_m5, na.rm = TRUE)
    , sumF5 = sum(numcls_f5, na.rm = TRUE)
    , sum5 = sum(numcls_m5, numcls_f5, na.rm = TRUE)
    , sumM6 = sum(numcls_m6, na.rm = TRUE)
    , sumF6 = sum(numcls_f6, na.rm = TRUE)
    , sum6 = sum(numcls_m6, numcls_f6, na.rm = TRUE)
  )

# 테이블 확인
DT::datatable(dataL2)

# 1-6학년 비율로 변환
dataL3 = dataL2 %>%
  dplyr::mutate(
    numclsM1Rat = (sumM1 / sum1) * 100
    , numclsF1Rat = (sumF1 / sum1) * 100
    , numclsM2Rat = (sumM2 / sum2) * 100
    , numclsF2Rat = (sumF2 / sum2) * 100
    , numclsM3Rat = (sumM3 / sum3) * 100
    , numclsF3Rat = (sumF3 / sum3) * 100
    , numclsM4Rat = (sumM4 / sum4) * 100
    , numclsF4Rat = (sumF4 / sum4) * 100
    , numclsM5Rat = (sumM5 / sum5) * 100
    , numclsF5Rat = (sumF5 / sum5) * 100
    , numclsM6Rat = (sumM6 / sum6) * 100
    , numclsF6Rat = (sumF6 / sum6) * 100
  ) %>%
  tidyr::gather(-c(1:19), key = "key", value = "val") %>%
  dplyr::mutate(
    type = dplyr::case_when(
      stringr::str_detect(key, regex("numclsM1Rat|numclsF1Rat")) ~ "1학년"
      , stringr::str_detect(key, regex("numclsM2Rat|numclsF2Rat")) ~ "2학년"
      , stringr::str_detect(key, regex("numclsM3Rat|numclsF3Rat")) ~ "3학년"
      , stringr::str_detect(key, regex("numclsM4Rat|numclsF4Rat")) ~ "4학년"
      , stringr::str_detect(key, regex("numclsM5Rat|numclsF5Rat")) ~ "5학년"
      , stringr::str_detect(key, regex("numclsM6Rat|numclsF6Rat")) ~ "6학년"
      , TRUE ~ "NA"
    )
    , keyName = dplyr::case_when(
      stringr::str_detect(key, regex("numclsM")) ~ "남자"
      , stringr::str_detect(key, regex("numclsF")) ~ "여자"
      , TRUE ~ "NA"
    )
  )

# 테이블 확인
DT::datatable(dataL3)

# keyName 정렬
dataL3$keyName = forcats::fct_relevel(dataL3$keyName, c("남자", "여자"))
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "시도명에 따른 1-6학년 남여 비율")

# 데이터 시각화
ggplot(dataL3, aes(x = regname, y = val, fill = keyName, label = round(val, 0))) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.position = "top") +
  geom_text(aes(group = keyName), position = position_dodge(width = 0.9), size = 5, vjust = -0.5, hjust = 0.5) +
  ylim(0, 60) +
  facet_wrap(~type, scale = "free", ncol = 2) +
  labs(x = "시도명", y = "비율", fill = NULL, subtitle = "시도명에 따른 1-6학년 남/여 비율") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 18, height = 12, dpi = 600)

# file_show(saveImg)


#**********************************************************
# 지역행정구분명 (regtypename)에 따른 1-6학년 남학생/여학생 수
#**********************************************************
dataL2 = dataL1 %>%
  dplyr::select(1:28) %>%
  readr::type_convert() %>%
  dplyr::mutate_at(vars(numcls_m1), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_f1), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_m2), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_f2), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_m3), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_f3), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_m4), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_f4), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_m5), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_f5), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_m6), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_f6), funs(as.numeric)) %>%
  dplyr::group_by(regtypename) %>%
  dplyr::summarise(
    sumM1 = sum(numcls_m1, na.rm = TRUE)
    , sumF1 = sum(numcls_f1, na.rm = TRUE)
    , sum1 = sum(numcls_m1, numcls_f1, na.rm = TRUE)
    , sumM2 = sum(numcls_m2, na.rm = TRUE)
    , sumF2 = sum(numcls_f2, na.rm = TRUE)
    , sum2 = sum(numcls_m2, numcls_f2, na.rm = TRUE)
    , sumM3 = sum(numcls_m3, na.rm = TRUE)
    , sumF3 = sum(numcls_f3, na.rm = TRUE)
    , sum3 = sum(numcls_m3, numcls_f3, na.rm = TRUE)
    , sumM4 = sum(numcls_m4, na.rm = TRUE)
    , sumF4 = sum(numcls_f4, na.rm = TRUE)
    , sum4 = sum(numcls_m4, numcls_f4, na.rm = TRUE)
    , sumM5 = sum(numcls_m5, na.rm = TRUE)
    , sumF5 = sum(numcls_f5, na.rm = TRUE)
    , sum5 = sum(numcls_m5, numcls_f5, na.rm = TRUE)
    , sumM6 = sum(numcls_m6, na.rm = TRUE)
    , sumF6 = sum(numcls_f6, na.rm = TRUE)
    , sum6 = sum(numcls_m6, numcls_f6, na.rm = TRUE)
  )

# 테이블 확인
DT::datatable(dataL2)

dataL3 = dataL2 %>%
  dplyr::mutate(
    numclsM1Rat = (sumM1 / sum1) * 100
    , numclsF1Rat = (sumF1 / sum1) * 100
    , numclsM2Rat = (sumM2 / sum2) * 100
    , numclsF2Rat = (sumF2 / sum2) * 100
    , numclsM3Rat = (sumM3 / sum3) * 100
    , numclsF3Rat = (sumF3 / sum3) * 100
    , numclsM4Rat = (sumM4 / sum4) * 100
    , numclsF4Rat = (sumF4 / sum4) * 100
    , numclsM5Rat = (sumM5 / sum5) * 100
    , numclsF5Rat = (sumF5 / sum5) * 100
    , numclsM6Rat = (sumM6 / sum6) * 100
    , numclsF6Rat = (sumF6 / sum6) * 100
  ) %>%
  tidyr::gather(-c(1:19), key = "key", value = "val") %>%
  dplyr::mutate(
    type = dplyr::case_when(
      stringr::str_detect(key, regex("numclsM1Rat|numclsF1Rat")) ~ "1학년"
      , stringr::str_detect(key, regex("numclsM2Rat|numclsF2Rat")) ~ "2학년"
      , stringr::str_detect(key, regex("numclsM3Rat|numclsF3Rat")) ~ "3학년"
      , stringr::str_detect(key, regex("numclsM4Rat|numclsF4Rat")) ~ "4학년"
      , stringr::str_detect(key, regex("numclsM5Rat|numclsF5Rat")) ~ "5학년"
      , stringr::str_detect(key, regex("numclsM6Rat|numclsF6Rat")) ~ "6학년"
      , TRUE ~ "NA"
    )
    , keyName = dplyr::case_when(
      stringr::str_detect(key, regex("numclsM")) ~ "남자"
      , stringr::str_detect(key, regex("numclsF")) ~ "여자"
      , TRUE ~ "NA"
    )
  )

# 테이블 확인
DT::datatable(dataL3)

# keyName 정렬
dataL3$keyName = forcats::fct_relevel(dataL3$keyName, c("남자", "여자"))
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "지역행정명에 따른 1-6학년 남여 비율")

ggplot(dataL3, aes(x = regtypename, y = val, fill = keyName, label = round(val, 0))) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.position = "top") +
  geom_text(aes(group = keyName), position = position_dodge(width = 0.9), size = 5, vjust = -0.5, hjust = 0.5) +
  ylim(0, 60) +
  facet_wrap(~type, scale = "free", ncol = 2) +
  labs(x = "지역행정명", y = "비율", fill = NULL, subtitle = "지역행정명에 따른 1-6학년 남/여 비율") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 18, height = 12, dpi = 600)

# file.show(saveImg)

# 테이블 확인
DT::datatable(dataL3)


#**********************************************************
# 설립유형(schtypename)에 따른 1-6학년 남학생/여학생 수
#**********************************************************
dataL2 = dataL1 %>%
  dplyr::select(1:28) %>%
  readr::type_convert() %>%
  dplyr::mutate_at(vars(numcls_m1), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_f1), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_m2), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_f2), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_m3), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_f3), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_m4), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_f4), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_m5), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_f5), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_m6), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(numcls_f6), funs(as.numeric)) %>%
  dplyr::group_by(schtypename) %>%
  dplyr::summarise(
    sumM1 = sum(numcls_m1, na.rm = TRUE)
    , sumF1 = sum(numcls_f1, na.rm = TRUE)
    , sum1 = sum(numcls_m1, numcls_f1, na.rm = TRUE)
    , sumM2 = sum(numcls_m2, na.rm = TRUE)
    , sumF2 = sum(numcls_f2, na.rm = TRUE)
    , sum2 = sum(numcls_m2, numcls_f2, na.rm = TRUE)
    , sumM3 = sum(numcls_m3, na.rm = TRUE)
    , sumF3 = sum(numcls_f3, na.rm = TRUE)
    , sum3 = sum(numcls_m3, numcls_f3, na.rm = TRUE)
    , sumM4 = sum(numcls_m4, na.rm = TRUE)
    , sumF4 = sum(numcls_f4, na.rm = TRUE)
    , sum4 = sum(numcls_m4, numcls_f4, na.rm = TRUE)
    , sumM5 = sum(numcls_m5, na.rm = TRUE)
    , sumF5 = sum(numcls_f5, na.rm = TRUE)
    , sum5 = sum(numcls_m5, numcls_f5, na.rm = TRUE)
    , sumM6 = sum(numcls_m6, na.rm = TRUE)
    , sumF6 = sum(numcls_f6, na.rm = TRUE)
    , sum6 = sum(numcls_m6, numcls_f6, na.rm = TRUE)
  )

# 테이블 확인
DT::datatable(dataL2)

dataL3 = dataL2 %>%
  dplyr::mutate(
    numclsM1Rat = (sumM1 / sum1) * 100
    , numclsF1Rat = (sumF1 / sum1) * 100
    , numclsM2Rat = (sumM2 / sum2) * 100
    , numclsF2Rat = (sumF2 / sum2) * 100
    , numclsM3Rat = (sumM3 / sum3) * 100
    , numclsF3Rat = (sumF3 / sum3) * 100
    , numclsM4Rat = (sumM4 / sum4) * 100
    , numclsF4Rat = (sumF4 / sum4) * 100
    , numclsM5Rat = (sumM5 / sum5) * 100
    , numclsF5Rat = (sumF5 / sum5) * 100
    , numclsM6Rat = (sumM6 / sum6) * 100
    , numclsF6Rat = (sumF6 / sum6) * 100
  ) %>%
  tidyr::gather(-c(1:19), key = "key", value = "val") %>%
  dplyr::mutate(
    type = dplyr::case_when(
      stringr::str_detect(key, regex("numclsM1Rat|numclsF1Rat")) ~ "1학년"
      , stringr::str_detect(key, regex("numclsM2Rat|numclsF2Rat")) ~ "2학년"
      , stringr::str_detect(key, regex("numclsM3Rat|numclsF3Rat")) ~ "3학년"
      , stringr::str_detect(key, regex("numclsM4Rat|numclsF4Rat")) ~ "4학년"
      , stringr::str_detect(key, regex("numclsM5Rat|numclsF5Rat")) ~ "5학년"
      , stringr::str_detect(key, regex("numclsM6Rat|numclsF6Rat")) ~ "6학년"
      , TRUE ~ "NA"
    )
    , keyName = dplyr::case_when(
      stringr::str_detect(key, regex("numclsM")) ~ "남자"
      , stringr::str_detect(key, regex("numclsF")) ~ "여자"
      , TRUE ~ "NA"
    )
  )

# 테이블 확인
DT::datatable(dataL3)

# keyName 정렬
dataL3$keyName = forcats::fct_relevel(dataL3$keyName, c("남자", "여자"))
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "설립유형에 따른 1-6학년 남여 비율")

ggplot(dataL3, aes(x = schtypename, y = val, fill = keyName, label = round(val, 0))) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(legend.position = "top") +
  geom_text(aes(group = keyName), position = position_dodge(width = 0.9), size = 5, vjust = -0.5, hjust = 0.5) +
  ylim(0, 60) +
  facet_wrap(~type, scale = "free", ncol = 2) +
  labs(x = "설립유형", y = "비율", fill = NULL, subtitle = "설립유형에 따른 1-6학년 남/여 비율") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 18, height = 12, dpi = 600)

# file.show(saveImg)

# 테이블 확인
DT::datatable(dataL3)

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
# 과제 5 평과 제곱미터 변환함수 작성하기

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0114"
showtext::showtext_opts(dpi = 100)
showtext::showtext.auto()

#================================================
# 과제 5 평과 제곱미터 변환함수 작성하기
#================================================
# 1) 평과 제곱미터를 서로 변환해주는 transPyungMeter() 함수 작성
transPyungMeter <- function(x, inUnit) {

  if (inUnit == "PY") {
    sprintf("%s평은 %s제곱미터입니다.", x, round(x * 3.30579, 2))
  } else if (inUnit == "M2") {
    sprintf("%s 제곱미터는 %s평입니다.", x, round(x / 3.30579, 2))
  } else {
    sprintf("%s을/를 확인해주시기 바랍니다.", inUnit)
  }

}

# 2) transPyungMeter(x, unit="PY") 형태로 선언할 것
# 3) unit은 문자열로 평을 제곱미터로 변환할 때는 "PY"를 입력하고
# 제곱미터를 평으로 입력할 때는 "M2"를 입력

# 4) 33평을 제곱미터로 변환할 때는 아래와 같이 함수 사용. 제곱미터를 평으로도 변환해야함
# transPyungMeter(33, "PY"), transPyungMeter(10, "M2") 
# 실행결과: "33평은 109.09 제곱미터입니다.", "10 제곱미터는 3.03평입니다."
transPyungMeter(33, "PY")
transPyungMeter(10, "M2")

# 5) 변환 값은 소숫점 2자리에서 반올림하여 표시할 것(round함수 참고)

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
# R을 이용한 온도, 풍속, 강수량의 통계 처리 및 관측자료 검증

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0120"

#================================================
# Main
#================================================
library(tidyverse)
library(readr)
library(lubridate)
library(hydroGOF)
library(spacetime)

#*****************************************
# 강수량 계산
#*****************************************
fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0120_pr+day+CanESM5_historical.csv", sep = "/"))
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))

dataL1 = data %>%
  tidyr::gather(-c(1:2), key = "key", value = "val") %>%
  dplyr::group_by(PERIOD, Month, key) %>%
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  ) %>%
  tidyr::spread(key = "key", value = "sumVal") %>%
  dplyr::arrange(PERIOD, Month)

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "pr_%Y%m")
utils::write.csv(dataL1, file = saveFile, row.names = FALSE, fileEncoding = "EUC-KR")

dataL2 = data %>%
  tidyr::gather(-c(1:2), key = "key", value = "val") %>%
  dplyr::group_by(PERIOD, key) %>%
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  ) %>%
  tidyr::spread(key = "key", value = "sumVal") %>%
  dplyr::arrange(PERIOD)

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "pr_%Y")
utils::write.csv(dataL2, file = saveFile, row.names = FALSE, fileEncoding = "EUC-KR")

# 검증 데이터셋
fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0120_Pr+obs+data.csv", sep = "/"))
valData = readr::read_csv(file = fileInfo2, locale = locale("ko", encoding = "EUC-KR"))

valDataL1 = valData %>%
  dplyr::mutate(
    dtDate = readr::parse_date(as.character(Period), "%Y-%m-%d")
    , PERIOD = lubridate::year(dtDate)
    , Month = lubridate::month(dtDate)
  ) %>%
  dplyr::select(-Period, -dtDate) %>%
  tidyr::gather(-c(PERIOD, Month), key = "key", value = "val") %>%
  dplyr::group_by(PERIOD, Month, key) %>%
  dplyr::summarise(
    sumVal = sum(val, na.rm = TRUE)
  ) %>%
  tidyr::spread(key = "key", value = "sumVal") %>%
  dplyr::arrange(PERIOD, Month)

valDataL2 = tibble()

# i = 3
# j = 3
for (i in 3:ncol(dataL1)) {
  for (j in 3:ncol(valDataL1)) {
    if (colnames(dataL1)[i] != colnames(valDataL1)[j]) next

    rtnData = hydroGOF::gof(sim = dataL1[, i], obs = valDataL1[, j], digits = 5, na.rm = TRUE) %>%
      as.data.frame()

    if (nrow(valDataL2) == 0) {
      valDataL2 = rtnData
    } else {
      valDataL2 = dplyr::bind_cols(valDataL2, rtnData)
    }
  }
}

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "pr_valid_%Y%m")
utils::write.csv(valDataL2, file = saveFile, row.names = TRUE, fileEncoding = "EUC-KR")


#*****************************************
# 온도  계산
#*****************************************
fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0120_ta_day_CanESM5_historical.csv", sep = "/"))
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))

dataL1 = data %>%
  tidyr::gather(-c(1:2), key = "key", value = "val") %>%
  dplyr::group_by(PERIOD, Month, key) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  ) %>%
  tidyr::spread(key = "key", value = "meanVal") %>%
  dplyr::arrange(PERIOD, Month)

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "tm_%Y%m")
utils::write.csv(dataL1, file = saveFile, row.names = FALSE, fileEncoding = "EUC-KR")

dataL2 = data %>%
  tidyr::gather(-c(1:2), key = "key", value = "val") %>%
  dplyr::group_by(PERIOD, key) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  ) %>%
  tidyr::spread(key = "key", value = "meanVal") %>%
  dplyr::arrange(PERIOD)

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "tm_%Y")
utils::write.csv(dataL2, file = saveFile, row.names = FALSE, fileEncoding = "EUC-KR")

# 검증 데이터셋
fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0120_Ta+obs+data.csv", sep = "/"))
valData = readr::read_csv(file = fileInfo2, locale = locale("ko", encoding = "EUC-KR"))

valDataL1 = valData %>%
  dplyr::mutate(
    dtDate = readr::parse_date(as.character(Period), "%Y-%m-%d")
    , PERIOD = lubridate::year(dtDate)
    , Month = lubridate::month(dtDate)
  ) %>%
  dplyr::select(-Period, -dtDate) %>%
  tidyr::gather(-c(PERIOD, Month), key = "key", value = "val") %>%
  dplyr::group_by(PERIOD, Month, key) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  ) %>%
  tidyr::spread(key = "key", value = "meanVal") %>%
  dplyr::arrange(PERIOD, Month)

valDataL2 = tibble()

for (i in 3:ncol(dataL1)) {
  for (j in 3:ncol(valDataL1)) {
    if (colnames(dataL1)[i] != colnames(valDataL1)[j]) next

    rtnData = hydroGOF::gof(sim = dataL1[, i], obs = valDataL1[, j], na.rm = TRUE) %>%
      round(3) %>%
      as.data.frame()

    if (nrow(valDataL2) == 0) {
      valDataL2 = rtnData
    } else {
      valDataL2 = dplyr::bind_cols(valDataL2, rtnData)
    }
  }
}

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "tm_valid_%Y%m")
utils::write.csv(valDataL2, file = saveFile, row.names = TRUE, fileEncoding = "EUC-KR")

#*****************************************
# 풍속 계산
#*****************************************
fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0120_sfcwind+day+CanESM5_historical.csv", sep = "/"))
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))

dataL1 = data %>%
  tidyr::gather(-c(1:2), key = "key", value = "val") %>%
  dplyr::group_by(PERIOD, Month, key) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  ) %>%
  tidyr::spread(key = "key", value = "meanVal") %>%
  dplyr::arrange(PERIOD, Month)

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "sw_%Y%m")
utils::write.csv(dataL1, file = saveFile, row.names = FALSE, fileEncoding = "EUC-KR")

dataL2 = data %>%
  tidyr::gather(-c(1:2), key = "key", value = "val") %>%
  dplyr::group_by(PERIOD, key) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  ) %>%
  tidyr::spread(key = "key", value = "meanVal") %>%
  dplyr::arrange(PERIOD)

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "sw_%Y")
utils::write.csv(dataL2, file = saveFile, row.names = FALSE, fileEncoding = "EUC-KR")

# 검증 데이터셋
fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0120_obs+sw+obs.csv", sep = "/"))
valData = readr::read_csv(file = fileInfo2, locale = locale("ko", encoding = "EUC-KR"))

valDataL1 = valData %>%
  dplyr::mutate(
    dtDate = readr::parse_date(as.character(Period), "%Y-%m-%d")
    , PERIOD = lubridate::year(dtDate)
    , Month = lubridate::month(dtDate)
  ) %>%
  dplyr::select(-Period, -dtDate) %>%
  tidyr::gather(-c(PERIOD, Month), key = "key", value = "val") %>%
  dplyr::group_by(PERIOD, Month, key) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  ) %>%
  tidyr::spread(key = "key", value = "meanVal") %>%
  dplyr::arrange(PERIOD, Month)

valDataL2 = tibble()

for (i in 3:ncol(dataL1)) {
  for (j in 3:ncol(valDataL1)) {
    if (colnames(dataL1)[i] != colnames(valDataL1)[j]) next

    rtnData = hydroGOF::gof(sim = dataL1[, i], obs = valDataL1[, j], na.rm = TRUE) %>%
      round(3) %>%
      as.data.frame()

    if (nrow(valDataL2) == 0) {
      valDataL2 = rtnData
    } else {
      valDataL2 = dplyr::bind_cols(valDataL2, rtnData)
    }
  }
}

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "sw_valid_%Y%m")
utils::write.csv(valDataL2, file = saveFile, row.names = TRUE, fileEncoding = "EUC-KR")

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
# 1. 삼성주식에 종가에 대한 자료를 평활하라고 하는데 어떤 평활법을 사용해야하는지 궁금합니다.
# 2. 평활된 자료와 원자료를 오버레이 시켜 그림을 그리라고 합니다. (어떤 평활법을 사용해야하는지 부터 막혀서 진행이 되질 않습니다ㅜㅜ)

serviceName = "LSH0122"

#================================================
# Main
#================================================
data = c(56400, 54200, 47750, 50000, 50700, 52800, 57900, 54000, 58200, 56600, 66700, 81000)

tsData = ts(data, start = 2020, frequency = 12)

# 1. 작년(2020년) 월별 평균 자료에 대한 평활을 하시오. (평균자료를 탐색하여 적절한 평활기법을 이용하세요) 

# 단순지수평활법(Simple Exponential Smoothing with 1 parameter)
simpleModel = HoltWinters(tsData, alpha = 0.2, beta = FALSE, gamma = FALSE)

# 비계절적 Holt-Winters (with 2 parameter)
multiModel = HoltWinters(tsData, gamma = FALSE)


# 2. 평활된 자료와 원 자료를 오버레이(overlay)시켜 그림을 그리시오.
# 단순지수평활법 : 흑색 (원 자료) vs 적색 (평활화)
# plot(simpleModel, main="Simple Exponential Smoothing(alpha=0.2)")
# simpleModel$SSE

# 비계절적 Holt-Winters : 흑색 (원 자료) vs 적색 (평활화)
plot(multiModel, main = "No Seasonal Holt-Winters")

# 두 모형의 SSE 비교하여 작은값 선정 (비계절적 Holt-Winters)
multiModel$SSE

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
# 설명을 드리자면 최저온도와 최고 온도를 이용해서 평균온도를 추정하는 것으로 바꾸고싶습니다!
# INPUT은 최고온도 최저온도이고 OUPUT은 평균온도입니다!

#================================================
# 문의사항
#================================================

#================================================
# Set Env
#================================================
globalVar = new.env()
globalVar$inpPath = "."
globalVar$figPath = "."
globalVar$outPath = "."
globalVar$logPath = "."

# rm(list = ls())
# prjName = "o2job"
# source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0123"

library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(h2o)
library(log4r)
library(caret)

#================================================
# Set Fun
#================================================
saveLogFile = sprintf("%s/%s_%s_%s.log", globalVar$logPath, Sys.info()["sysname"], Sys.info()["nodename"], format(Sys.time(), "%Y%m%d"))

log = log4r::create.logger()
log4r::logfile(log) = saveLogFile
log4r::level(log) = "INFO"

perfEval = function(x, y) {

  if (length(x) < 1) { return(sprintf("%s", "x 값 없음")) }
  if (length(y) < 1) { return(sprintf("%s", "y 값 없음")) }

  slope = coef(lm(y ~ x))[2]
  interp = coef(lm(y ~ x))[1]
  xMean = mean(x, na.rm = TRUE)
  yMean = mean(y, na.rm = TRUE)
  xSd = sd(x, na.rm = TRUE)
  ySd = sd(y, na.rm = TRUE)
  cnt = length(x)
  bias = mean(x - y, na.rm = TRUE)
  rBias = (bias / yMean) * 100.0
  rmse = sqrt(mean((x - y)^2, na.rm = TRUE))
  rRmse = (rmse / yMean) * 100.0
  r = cor.test(x, y)$estimate
  p = cor.test(x, y)$p.value
  diffMean = mean(x - y, na.rm = TRUE)
  diffSd = sd(x - y, na.rm = TRUE)
  # perDiffMean = mean((x - y) / y, na.rm = TRUE) * 100.0

  return(c(slope, interp, xMean, yMean, xSd, ySd, cnt, bias, rBias, rmse, rRmse, r, p, diffMean, diffSd))
}

biasCorr = function(actu, pred, minVal, maxVal, interVal, isPlot = FALSE) {

  factorVal = seq(minVal, maxVal, by = interVal)

  # RMSE Fitting
  liResult = lapply(1:length(factorVal), function(i) Metrics::rmse(actu, pred * factorVal[i])) %>%
    unlist()

  ind = which(liResult == min(liResult, na.rm = TRUE))

  if (isPlot == TRUE) {
    plot(liResult)
  }

  # Best Factor Index
  ind = which(liResult == min(liResult, na.rm = TRUE))

  calibFactor = factorVal[[ind]]
  calPred = calibFactor * pred

  meanDiff = mean(actu, na.rm = TRUE) - mean(calPred, na.rm = TRUE)
  newPred = (calPred) + meanDiff

  cat(
    sprintf("%s : %.2f", "[보정 X] RMSE", Metrics::rmse(actu, pred))
    , sprintf("%s : %.2f", "[보정 O] RMSE", Metrics::rmse(actu, newPred))
    , "\n"
  )

  return(c(newPred))
}

#================================================
# Main
#================================================
fileInfo1 = Sys.glob(paste(globalVar$inpPath, "LSH0123_real+TAMIN.csv", sep = "/"))
tmpData1 = readr::read_csv(file = fileInfo1, locale = locale("ko", encoding = "EUC-KR")) %>% dplyr::select(-Period)

fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0123_real+TAMAX.csv", sep = "/"))
tmpData2 = readr::read_csv(file = fileInfo2, locale = locale("ko", encoding = "EUC-KR")) %>% dplyr::select(-Period)

fileInfo3 = Sys.glob(paste(globalVar$inpPath, "LSH0123_Ta+obs+data.csv", sep = "/"))
tmpData3 = readr::read_csv(file = fileInfo3, locale = locale("ko", encoding = "EUC-KR")) %>% dplyr::select(-Period)

fileInfo4 = Sys.glob(paste(globalVar$inpPath, "LSH0123_idw_tasmax_day_ACCESS-ESM1-5_ssp245_r1i1p1f1_gn_20150101-21001231.csv", sep = "/"))
tmpData4 = readr::read_csv(file = fileInfo4, locale = locale("ko", encoding = "EUC-KR")) %>%
  dplyr::select(-X1) %>%
  magrittr::set_colnames(colnames(tmpData3))


fileInfo5 = Sys.glob(paste(globalVar$inpPath, "LSH0123_idw_tasmin_day_ACCESS-ESM1-5_ssp245_r1i1p1f1_gn_20150101-21001231.csv", sep = "/"))
tmpData5 = readr::read_csv(file = fileInfo5, locale = locale("ko", encoding = "EUC-KR")) %>%
  dplyr::select(-X1) %>%
  magrittr::set_colnames(colnames(tmpData3))


# 검증 지수 테이블 생성
rowNum = ncol(tmpData3)
perfTable = data.frame(matrix(0, nrow = 2 * rowNum, ncol = 15))
rownames(perfTable) = c(paste0("SVM-", 1:rowNum), paste0("DL-", 1:rowNum))
colnames(perfTable) = c("slope", "interp", "xMean", "yMean", "xSd", "ySd", "cnt", "bias", "rBias", "rmse", "rRmse", "r", "pVal", "diffMean", "diffSd")


isCorr = FALSE
layerInfo = 1
epochsInfo = 100
nfoldsInfo = 2
# layerInfo = 3
# epochsInfo = 1000
# nfoldsInfo = 10
setParamInfo = sprintf("%s-%s", layerInfo, epochsInfo)

# 초기화
h2o::h2o.init()

colList = colnames(tmpData1)
dlPredData = data.frame()
dlLastLayerData = data.frame()
svmPredData = data.frame()

# i =1
# for (i in 2:length(colList)) {
for (i in 1:1) {

  data = data.frame(tmpData1[, i], tmpData2[, i], tmpData3[, i]) %>%
    magrittr::set_colnames(c("taMin", "taMax", "obs"))

  dataL1 = data %>%
    dplyr::filter(
      !is.na(obs)
      , !is.na(taMin)
      , !is.na(taMax)
    )

  if (nrow(dataL1) < 1) next

  #*****************************************
  # 훈련 및 테스트 셋 설정 (70 : 30)
  #*****************************************
  set.seed(1)

  # 훈련 및 데이터 셋을 60:40으로 나누기 위한 인덱스 설정
  # ind = sample(1:nrow(dataL1), nrow(dataL1) * 0.7)
  # ind = 1:9131

  #*****************************************
  # 해당 인덱스에 따라 자료 할당
  #*****************************************
  # trainData = dataL1[ind,]
  # testData = dataL1[-ind,]

  trainData = dataL1
  testData = data.frame(tmpData4[, i], tmpData4[, i]) %>%
    magrittr::set_colnames(c("taMax", "taMin")) %>%
    dplyr::filter(
      !is.na(taMax)
      , !is.na(taMin)
    )

  #*****************************************
  # 해당 인덱스에 따라 자료 할당 (표준화)
  #*****************************************
  # trainData = dataL1[ind,] %>%
  #   dplyr::mutate_each_(funs(scale), vars = c("obs"))
  #
  # testData = dataL1[-ind,] %>%
  #   dplyr::mutate_each_(funs(scale), vars = c("obs"))

  #*****************************************
  # 해당 인덱스에 따라 자료 할당 (정규화)
  #*****************************************
  # trainData = dataL1[ind,] %>%
  #   dplyr::mutate_each_(funs(scales::rescale), vars = c("obs"))
  #
  # testData = dataL1[-ind,] %>%
  #   dplyr::mutate_each_(funs(scales::rescale), vars = c("obs"))

  # 훈련 데이터셋 확인
  dplyr::tbl_df(trainData)

  # 테스트 데이터셋 확인
  dplyr::tbl_df(testData)

  # yObs = testData$obs
  # yModel = testData$model

  # if (isCorr == TRUE) {
  #   yModel = biasCorr(yObs, yModel, -100, 100, 0.001, FALSE)
  # }

  # 원시 데이터 (ORI)
  # perfTable[i, ] = round(perfEval(yModel, yObs), 2)

  # =====================================================================
  # 서포트벡터 (SVM)
  #=====================================================================
  #***********************************
  # 학습
  #***********************************
  tryCatch(
    expr = {

      log4r::info(log, sprintf("%s", "[START] SVM"))

      tictoc::tic()

      svmModel = caret::train(
        obs ~ taMin + taMax
        , data = trainData
        , method = "svmLinear"
        , tuneGrid = expand.grid(.C = 2^(seq(-5, 5, 2)))
        # , preProcess = c("center", "scale")
        , trControl = caret::trainControl(method = "repeatedcv", number = nfoldsInfo, repeats = 1, search = "grid") # 10분할 교차 검증 및 1번 반복
      )

      tictoc::toc()

      #***********************************
      # 검증
      #***********************************
      # yObs = testData$obs
      yHat = predict(svmModel, newdata = testData)

      tmpData = data.frame(yHat) %>%
        magrittr::set_colnames(paste0("SVM-", colList[i]))

      if (ncol(svmPredData) == 0) {
        svmPredData = tmpData
      } else {
        svmPredData = dplyr::bind_cols(svmPredData, tmpData)

      }

      # perfTable[i,] = round(perfEval(yHat, yObs), 2)
      # perfTable
    }

    , warning = function(warning) {
      perfTable[i,] = "WARN"

      log4r::warn(log, warning)
    }

    , error = function(error) {
      perfTable[i,] = "ERROR"

      log4r::error(log, error)
    }

    , finally = {

      log4r::info(log, sprintf("%s", "[END] SVM"))
    }
  )

  #===============================================
  # Deep Learning (DL)
  #===============================================
  tryCatch(
    expr = {
      log4r::info(log, sprintf("%s", "[START] Deep Learning"))

      #***********************************
      # 학습
      #***********************************
      tictoc::tic()

      # activation : 활성화 함수로서 Rectifier 정규화 선형 함수 (즉 Keras의 ReLU 동일)
      # hidden : 숨겨진 레이어의 수와 뉴런 수 (일반적으로 입력 차원의 1/10 or 1/100 단위)
      # epochs : 반복 횟수 (기본 10-40)
      # nfolds : 훈련 반복 수

      layerNum = as.integer(nrow(trainData) / 10)
      # layerNum = as.integer(nrow(trainData) / 100)

      dlModel = h2o::h2o.deeplearning(
        x = c("taMin", "taMax")
        , y = c("obs")
        , training_frame = as.h2o(trainData)
        , activation = "Rectifier"
        , hidden = rep(layerNum, layerInfo)
        , nfolds = nfoldsInfo
        , epochs = epochsInfo
        , seed = 1
      )

      tictoc::toc()

      tryCatch(

        expr = {
          log4r::info(log, sprintf("%s", "[START] Make Image"))

          saveImg = sprintf("%s/%s_%s_%s.png", globalVar$figPath, serviceName, colList[i], "Training-Scoring-History")
          png(file = saveImg, width = 10, height = 8, units = "in", res = 600)

          plot(dlModel, timestep = "epochs", metric = "rmse")
        }

        , warning = function(warning) {
          log4r::warn(log, warning)
        }

        , error = function(error) {
          log4r::error(log, error)
        }

        , finally = {
          dev.off()

          log4r::info(log, sprintf("%s", "[END] Make Image"))
        }
      )

      tmpLastLayer = h2o::h2o.deepfeatures(dlModel, as.h2o(trainData), layer = layerInfo) %>%
        as.tibble() %>%
        dplyr::mutate(colInfo = colList[i])

      dlLastLayerData = dplyr::bind_rows(dlLastLayerData, tmpLastLayer)

      #***********************************
      # 검증
      #***********************************
      # yObs = testData$obs
      yHat = as.data.frame(h2o::h2o.predict(object = dlModel, newdata = as.h2o(testData)))$predict

      if (isCorr == TRUE) {
        yHat = biasCorr(yObs, yHat, -100, 100, 0.001, FALSE)
      }

      tmpData = data.frame(yHat) %>%
        magrittr::set_colnames(paste0("DL-", colList[i]))

      if (ncol(dlPredData) == 0) {
        dlPredData = tmpData
      } else {
        dlPredData = dplyr::bind_cols(dlPredData, tmpData)
      }

      # perfTable[i + rowNum,] = round(perfEval(yHat, yObs), 2)

    }

    , warning = function(warning) {
      perfTable[i + rowNum,] = "WARN"

      log4r::warn(log, warning)
    }

    , error = function(error) {
      perfTable[i + rowNum,] = "ERROR"

      log4r::error(log, error)
    }

    , finally = {

      log4r::info(log, sprintf("%s", "[END] Deep Learning"))
    }
  )
}


#=====================================================================
# 검증 결과 출력
#=====================================================================
# XLSX 파일 생성
saveXlsxFile = sprintf("%s/%s_%s_%s_%s.xlsx", globalVar$outPath, serviceName, "Pred_Data_From_Model_Obs", setParamInfo, isCorr)

wb = openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "perfTable")
openxlsx::writeData(wb, "perfTable", perfTable, startRow = 1, startCol = 1, colNames = TRUE, rowNames = TRUE)

openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)

# CSV 파일 생성
saveCsvFile = sprintf("%s/%s_%s_%s_%s.csv", globalVar$outPath, serviceName, "SVM_Pred_Data_From_Model_Obs", setParamInfo, isCorr)
utils::write.csv(svmPredData, file = saveCsvFile)

# CSV 파일 생성
saveDlCsvFile = sprintf("%s/%s_%s_%s_%s.csv", globalVar$outPath, serviceName, "DL_Pred_Data_From_Model_Obs", setParamInfo, isCorr)
utils::write.csv(dlPredData, file = saveDlCsvFile)

# CSV 파일 생성
saveDlLastLayerCsvFile = sprintf("%s/%s_%s_%s_%s.csv", globalVar$outPath, serviceName, "DL_Last_Layer_Data_From_Model_Obs", setParamInfo, isCorr)
utils::write.csv(dlLastLayerData, file = saveDlLastLayerCsvFile)

# file_show(globalVar$outPath)


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
# 최종 결과물 (Data1, Data2)과 동일한 구조로  데이터 전처리 수행

#================================================
# 문의사항
#================================================

#================================================
# Set Env
#================================================
# globalVar = new.env()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."
# globalVar$logPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0125"

library(tidyverse)
library(ggplot2)
library(lubridate)
library(phonenumber)

#================================================
# Main
#================================================
# Data1.csv를 위한 기초 입력자료 읽기
fileInfo1 = Sys.glob(paste(globalVar$inpPath, "LSH0125_Track1_데이터3 samp_cst_feat.csv", sep = "/"))
tmpData1 = readr::read_csv(file = fileInfo1, locale = locale("ko", encoding = "EUC-KR"))

fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0125_Track1_데이터2 samp_train.csv", sep = "/"))
tmpData2 = readr::read_csv(file = fileInfo2, locale = locale("ko", encoding = "EUC-KR"))

# Data2.csv를 위한 기초 입력자료 읽기
fileInfo3 = Sys.glob(paste(globalVar$inpPath, "LSH0125_Track2_데이터1 trend_w_demo.csv", sep = "/"))
tmpData3 = readr::read_csv(file = fileInfo3, locale = locale("ko", encoding = "EUC-KR"))

fileInfo4 = Sys.glob(paste(globalVar$inpPath, "LSH0125_Track1_데이터1 mrc_info.csv", sep = "/"))
tmpData4 = readr::read_csv(file = fileInfo4, locale = locale("ko", encoding = "EUC-KR"))

#**************************************
# Data1.csv 전처리 및 파일 저장
#**************************************
# 단계별 과정을 통해 수행
# - 유일한 id 부여
# - as.numeric를 통해 MRC_ID_DI를 자료형 변환
# - spread를 통해 MRC_ID_DI의 행 구조를 열 구조로 변환 및 데이터 치완 (NA > 0)
# - left_join를 통해 cst_id_di 좌측 조인
# - select를 통해 원하는 컬럼명만 선택
data1 = tmpData2 %>%
  tibble::rowid_to_column("id") %>%
  dplyr::mutate(
    MRC_ID_DI = as.numeric(MRC_ID_DI)
    , key = paste0("dependent", MRC_ID_DI)
    , val = 1
  ) %>%
  tidyr::spread(key = "key", value = "val") %>%
  dplyr::mutate(across(everything(), ~replace_na(.x, 0))) %>%
  dplyr::left_join(tmpData1, by = c("cst_id_di" = "cst_id_di")) %>%
  dplyr::select(cst_id_di, id, num_range("dependent", 0:10), MRC_ID_DI, starts_with("VAR"))

# 파일 저장
saveFile1 = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "Data1.csv")
readr::write_csv(x = data1, file = saveFile1)

#**************************************
# Data2.csv 전처리 및 파일 저장
#**************************************
# 스코어 레벨에 따른 값 변환 함수
caseScore = function(scrore) {
  dplyr::case_when(
    scrore == "high" ~ 3
    , scrore == "mid" ~ 2
    , scrore == "low" ~ 1
    , TRUE ~ NA_real_
  )
}

# 단계별 과정을 통해 수행
# - 유일한 id 부여
# - left_join를 통해 Category 좌측 조인
# - letterToNumber를 통해 알파벳 숫자 변환 (A > 1)
# - 앞서 스코어 레벨에 따른 값 변환 함수를 컬럼별 (*스코어)로 적용
# - select를 통해 필요없는 컬럼 제거
# - rename를 통해 컬럼명 수정
# - select를 통해 원하는 컬럼명만 선택
data2 = tmpData3 %>%
  tibble::rowid_to_column("id") %>%
  dplyr::left_join(tmpData4, by = c("Category" = "Category")) %>%
  dplyr::mutate(
    Age = phonenumber::letterToNumber(`연령대`)
    , Marriage = caseScore(`기혼스코어`)
    , Baby = caseScore(`유아자녀스코어`)
    , Elementary = caseScore(`초등학생자녀스코어`)
    , Middle = caseScore(`중고생자녀스코어`)
    , College = caseScore(`대학생자녀스코어`)
    , Housewife = caseScore(`전업주부스코어`)
  ) %>%
  dplyr::select(-Category, -연령대, -ends_with("스코어")) %>%
  dplyr::rename(
    Gender = 성별구분
    , Category = MRC_ID_DI
  ) %>%
  dplyr::select(id, YM, Category, Gender, Age, Marriage, Baby, Elementary, Middle, College, Housewife)

# 파일 저장
saveFile2 = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "Data2.csv")
readr::write_csv(x = data2, file = saveFile2)


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
# parallel 패키지를 통해 병렬 처리
# NetCDF 모든 변수명 처리

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0126"

#================================================
# Main
#================================================
library(raster)
library(tictoc)
library(raster)
library(sf)
library(doParallel)
library(parallel)
library(noncompliance)
library(tibble)
library(RNetCDF)

# dataAll<-NULL

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0126_pr_day_ACCESS-ESM1-5_ssp585_r1i1p1f1_gn_20150101-20641231.nc", sep = "/"))

ncData = raster::brick(fileInfo)

ncData2 = raster::brick(fileInfo, hurrname = "t", level = 1, stopIfNotEqualSpaced = FALSE)

# variables <- raster::stack(fileInfo)
# 

names(ncData)

ncData@file

latList = seq(from = 31.125, to = 34, by = 0.25)
lonList = seq(from = 125.125, to = 128, by = 0.25)
saveFile = NA

# 입력 데이터 
data = tibble::tibble(
  noncompliance::expand.grid.DT(
    latList
    , lonList
    , col.names = c("lon", "lat"))
)

# [시작] 병렬 처리
oSocClu = parallel::makePSOCKcluster(parallel::detectCores())
doParallel::registerDoParallel(oSocClu)

# 외부 변수 등록
parallel::clusterExport(oSocClu, "r")
parallel::clusterExport(oSocClu, "hurls")
parallel::clusterExport(oSocClu, "data")
parallel::clusterExport(oSocClu, "saveFile")
parallel::clusterExport(oSocClu, "globalVar")
parallel::clusterExport(oSocClu, "serviceName")

# 외부 라이브러리 등록
parallel::clusterEvalQ(oSocClu, library(raster))
parallel::clusterEvalQ(oSocClu, library(readr))

tictoc::tic()
# parallel::parSapply(oSocClu, X = 1:nrow(data), function(x) {
parallel::parSapply(oSocClu, X = 1:8, function(x) {
  hurls = data.frame(t(raster::extract(r, cbind(data$lat[x], data$lon[x]))))
  colnames(hurls) = c(paste(data$lat[x], data$lon[x], sep = "p"))

  saveFile = sprintf("%s/%s_%s_%s.csv", globalVar$outPath, serviceName, data$lat[x], data$lon[x])
  readr::write_csv(x = hurls, file = saveFile)
})
tictoc::toc()

# [종료] 병렬 처리
parallel::stopCluster(oSocClu)


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
# 과제 06. delChar()함수 만들기
# 중간고사 과제 설명 동영상을 시청 후 아래의 분석 결과를 작성하세요.

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0127"
showtext::showtext_opts(dpi = 100)
showtext::showtext.auto()

#================================================
# 과제 06. delChar()함수 만들기
#================================================
# 인수로 넘겨진 문자를 역시 인수로 넘겨진 문자열에서 제거하는 함수 delChar()를 작성하세요(20점).
# delChar() 함수는 인수를 2개를 전달 받습니다.
# delChar(x,y)의 형태로 구성되고 y는 삭제할 문자, x는 대상 문자열의 형태입니다.
# 샘플 문자열: "I saw her standing there; Misery; Anna (Go to him); Chains; Boys Ask me why"
# 샘플 삭제 문자: ";“
# delChar("I saw her standing there; Misery; Anna (Go to him); Chains; Boys Ask me why“, “;”)를 실행하면 "I saw her standing there Misery Anna (Go to him) Chains Boys Ask me why“ 문자열을 반환 해야함
# substr(), nChar(), 반복문 등을 활용하세요.

delChar = function(x, y) {

  # 입력 파라미터
  txt <- x
  delText <- y

  newTxt <- ""

  for (i in 1:nchar(txt)) {
    char = substr(txt, i, i)
    if (char == delText) {
      char <- ""
    }

    newTxt <- paste0(newTxt, char)
  }

  return(newTxt)
}

delChar("I saw her standing there; Misery; Anna (Go to him); Chains; Boys Ask me why", ";")

#======================================================================
# 중간고사 과제 설명 동영상을 시청 후 아래의 분석 결과를 작성하세요.
#======================================================================
library(ggplot2)

# 문제 1) 다이아몬드 가격과 다른 특성과의 관계를 아래의 산포도를 그려서 확인 후 대략적으로 추측해보세요.
# 1-1 price와 color의 관계 산포도와 관계에 대한 설명 20점

# 캐럿 무게 (carat)에 따른 가격 (price)을 다이아몬드 색깔 (color) 별로 시각화하였다.
# 그 결과 캐럿 무게가 낮을 경우 다이아몬드 좋은 색깔일수록 높은 가격으로 책정되었다.
# 그러나 2.5 무게 이상일 경우 캐럿 무게가 높을수록 가격은 떨어짐을 볼 수 있다.
ggplot(diamonds, aes(x = carat, y = price, color = color)) +
  geom_point() +
  geom_smooth()


# 1-2 price와 cut 의 관계 산포도와 관계에 대한 설명 20점
# 캐럿 무게 (carat)에 따른 가격 (price)을 다이아몬드 등급 (cut) 별로 시각화하였다.
# 그 결과 캐럿 2 무게 이하인 경우 등급에 따라 높은 가격으로 책정되었다.
# 그러나 무게 2 이상인 경우 대부분 등급에서 감소하나 Fair의 경우 상승함을 보인다.
ggplot(diamonds, aes(x = carat, y = price, color = cut)) +
  geom_point() +
  geom_smooth()

# 문제 2) 다이아몬드 가격을 Y축, 가격에 미치는 영향 요인 하나를 X축, color와 clarity를 포인트의 색으로 설정한 산포도를 그리고 가격에 각 요인들이 미치는 영향을 종합적으로 설명하세요(40점)
# 캐럿 무게 (carat)에 따른 가격 (price)을 다이아몬드 등급 (cut) 별로 시각화하였다.
# 그 결과 캐럿 2 무게 이하인 경우 등급에 따라 높은 가격으로 책정되었다.
# 그러나 무게 2 이상인 경우 대부분 등급에서 감소하나 Fair의 경우 상승함을 보인다.
ggplot(diamonds, aes(x = carat, y = price, color = cut)) +
  geom_point() +
  geom_smooth()

# 캐럿 무게 (carat)에 따른 가격 (price)을 다이아몬드 투명도 (clarity) 별로 시각화하였다.
# 그 결과 캐럿 무게가 증가할수록 좋은 투명도에 따라 높은 가격으로 책정된다.
# 특히 캐럿 무게가 0.75-1.0의 경우 폭발적 가격 증가 폭 (2차 곡선)을 보이는 반면 낮은 투명도 (I1)에서는 비교적 낮은 상승 폭 (1차 곡선)을 보인다.
ggplot(diamonds, aes(x = carat, y = price, color = clarity)) +
  geom_point() +
  geom_smooth()


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
# 안녕하세요. R로 도시별 인구비율 현황을 지도로 시각화하고 싶습니다. 가능할까요?
# 필요기간은 내일이고, 데이터 자료는 현재 가지고 있는데요. 전국에 사업소가 있고, 각 도시별 입사 5년 이하 직원수를 시각화해시 표현하고 싶습니다.

#================================================
# Set Env
#================================================
globalVar = list()
globalVar$inpPath = "."
globalVar$figPath = "."
globalVar$outPath = "."

# rm(list = ls())
# prjName = "o2job"
# source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0128"

#================================================
# Main
#================================================
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
library(tidyr)
library(geosphere)
library(ggrepel)
library(gdata)
library(data.table)
library(ggh4x)
library(remotes)
library(devtools)
library(pracma)
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
library(readr)

# 데이터 읽기
fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0128_지도+표시.xlsx", sep = "/"))
data = openxlsx::read.xlsx(fileInfo, sheet = 1) %>%
  dplyr::rename(
    addr2 = "행.레이블"
    , val = "6년이하비율"
    , val2 = "3년이하비율"
  ) %>%
  dplyr::select(addr2, val, val2)

# 법정동 파일 읽기
fileInfo3 = Sys.glob(paste(globalVar$inpPath, "EMD.shp", sep = "/"))
mapData = shapefile(fileInfo3)
geoData = spTransform(mapData, CRS("+proj=longlat"))
mapGeoData = ggplot2::fortify(geoData, region = 'EMD_CD', region2 = "EMD_KOR_NM")

# 법정동 코드 읽기
codeInfo = Sys.glob(paste(globalVar$mapPath, "/admCode/법정동코드_전체자료.txt", sep = "/"))
code = utils::read.table(codeInfo, sep = "\t", header = TRUE, colClasses = "character", fileEncoding = "EUC-KR") %>%
  as.tibble() %>%
  magrittr::set_colnames(c("EMD_CD", "addr", "isUse"))

# 전국 도시
codeL1 = code %>%
  tidyr::separate(col = "addr", into = c("addr1", "addr2", "addr3", "addr4", "addr5"), sep = " ") %>%
  dplyr::select(-addr3, -addr4, -addr5) %>%
  dplyr::filter(
    # stringr::str_detect(addr1, regex("서울특별시"))
    isUse == "존재"
  ) %>%
  dplyr::filter(
    !is.na(addr1)
    , !is.na(addr2)
    # , ! is.na(addr3)
  ) %>%
  dplyr::mutate(
    id = stringr::str_sub(EMD_CD, 1, 8)
  )

# 도 : addr1
# 시군구 : addr2
# 읍면동 : addr3
dataL2 = mapGeoData %>%
  dplyr::inner_join(codeL1, by = c("id" = "id")) %>%
  dplyr::left_join(data, by = c("addr2" = "addr2"))


dataL3 = dataL2

# 시각화
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, " 6년이하 입사자의 비율")
# plotSubTitle = sprintf("[전국] %s", "6년이하 입사자의 비율")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, " 3년이하 입사자의 비율")
plotSubTitle = sprintf("[전국] %s", "3년이하 입사자의 비율")

ggplot() +
  theme_bw() +
  coord_fixed(ratio = 1) +
  # geom_polygon(data = dataL3, aes(x = long, y = lat, group = group, fill = val)) +
  geom_polygon(data = dataL3, aes(x = long, y = lat, group = group, fill = val2)) +
  scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, 100), na.value = "white") +
  geom_path(data = dataL3, aes(x = long, y = lat, group = group), colour = 'black', size = 0.01) +
  labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "단위 : %", subtitle = NULL) +
  theme_minimal() +
  theme(
    text = element_text(size = 18)
    , panel.grid.major.x = element_blank()
    , panel.grid.major.y = element_blank()
    , panel.grid.minor.x = element_blank()
    , panel.grid.minor.y = element_blank()
    , axis.text.x = element_blank()
    , axis.ticks.x = element_blank()
    , axis.title.x = element_blank()
    , axis.text.y = element_blank()
    , axis.ticks.y = element_blank()
    , axis.title.y = element_blank()
    , plot.subtitle = element_text(hjust = 1)
    # , legend.position = "none"
  ) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)

# file_show(saveImg)


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
# 아래는 예시 그래프에요. 변수가 CS(x축), H2SO4(y축) 이렇게 두개 있고,
# B(빨강)/G(파랑)/A(초록)/J(노랑),
# TNPF(동그라미)/NON(삼각형)
# 이렇게 가능할까요?

# 그리고 median(중앙값)으로 표현하고, error bar 범위는 25%~75%로 부탁드릴게요:)

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0129"

#================================================
# Main
#================================================
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggplot2)
library(lubridate)
library(MASS)
library(scales)

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0129_ref_median_min_max.xlsx", sep = "/"))
refData = openxlsx::read.xlsx(xlsxFile = fileInfo, sheet = "Sheet1")

fileList = Sys.glob(paste(globalVar$inpPath, "LSH0129_*.csv", sep = "/"))
dataL1 = tibble::tibble()
# fileInfo = "E:/04. TalentPlatform/Github/TalentPlatform-R/resources/input/o2job/LSH0129_A_2020_NON.csv"

for (fileInfo in fileList) {
  data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))

  x = data$CS
  y = data$H2SO4
  fileName = tools::file_path_sans_ext(fs::path_file(fileInfo))

  dataL1 = dplyr::bind_rows(
    dataL1
    , data.frame(
      fileName = fileName
      , x = median(x, na.rm = TRUE)
      , y = median(y, na.rm = TRUE)
      , xmin = quantile(x, 0.25, na.rm = TRUE)
      , xmax = quantile(x, 0.75, na.rm = TRUE)
      , ymin = quantile(y, 0.25, na.rm = TRUE)
      , ymax = quantile(y, 0.75, na.rm = TRUE)
    )
  )
}

dataL2 = dataL1 %>%
  dplyr::bind_rows(refData) %>%
  dplyr::mutate(
    makeColor = dplyr::case_when(
      stringr::str_detect(fileName, regex("_A_")) ~ 1
      , stringr::str_detect(fileName, regex("_B_")) ~ 2
      , stringr::str_detect(fileName, regex("_G_")) ~ 3
      , stringr::str_detect(fileName, regex("_J_")) ~ 4
      , TRUE ~ 5
    )
    , makeShape = dplyr::case_when(
      stringr::str_detect(fileName, regex("TNPF")) ~ 17 # 동그라미
      , stringr::str_detect(fileName, regex("NON")) ~ 18 # 삼각형
      , TRUE ~ 17
    )
  )

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "중간값에 따른 에러바 시각화_7크기")

ggplot(dataL2, aes(x = x, y = y, color = factor(makeColor), shape = factor(makeShape))) +
  geom_point(size = 7, show.legend = FALSE) +
  geom_errorbarh(aes(xmin = xmin, xmax = xmax), height = 0.10) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.05) +
  scale_color_manual(values = c("1" = "green", "2" = "red", "3" = "blue", "4" = "yellow", "5" = "Black"), name = NULL, na.value = NA
    , labels = c("A", "B", "G", "J", "REF")
  ) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks() +
  labs(title = NULL, x = bquote('CS (' * s^-1 * ')'), y = bquote('' * H[2] * SO[4] * ' concentration (' * cm^-3 * ')'), colour = NULL, fill = NULL, subtitle = NULL) +
  theme_bw() +
  theme(
    text = element_text(size = 18)
    , legend.position = "bottom"
    , plot.margin = unit(c(4, 4, 0, 0), "mm")
    , panel.grid.major.x = element_blank()
    , panel.grid.major.y = element_blank()
    , panel.grid.minor.x = element_blank()
    , panel.grid.minor.y = element_blank()
    , panel.border = element_rect(size = 1.5)
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


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
# 메일로 첨부하였습니다! 요구사항은 다음과 같습니다!
# 1. 관측소 제원을 인접격자의 데이터를 사용하여 idw를 산정하는 것입니다.
# 2. 관측소 idw 결과를 컬럼 형태로 csv파일로 받고싶습니다!

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())

prjName = "o2job"

source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")
serviceName = "LSH0130"

#================================================
# Main
#================================================
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(colorRamps)
library(lubridate)
library(extrafont)
library(ggrepel)
library(scales)
library(sf)
unloadNamespace('raster')
library(gstat)
library(sp)
library(metR)
library(akima)
library(stringr)

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0130_관측소+제원.xlsx", sep = "/"))
stationData = openxlsx::read.xlsx(xlsxFile = fileInfo, sheet = "Sheet1") %>%
  dplyr::select(Observation, X, Y) %>%
  dplyr::rename(
    name = Observation
    , lon = X
    , lat = Y
  )

# 관측소에 대한 공간 격자화
spNewData = stationData
coordinates(spNewData) = ~lon + lat
# plot(spNewData)

fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0130_tasmax_day_INM-CM4-8_historical_r1i1p1f1_gr1_19850101-20141231.csv", sep = "/"))
data = readr::read_csv(file = fileInfo2, locale = locale("ko", encoding = "EUC-KR"))

dtDateList = sort(unique(data$X1))
dataL3 = tibble::tibble()

# dtDateInfo = "1987-07-05"
for (dtDateInfo in dtDateList) {

  dataL1 = data %>%
    dplyr::filter(X1 == dtDateInfo) %>%
    tidyr::gather(-X1, key = "key", value = "val") %>%
    tidyr::separate(col = "key", into = c("tmpLon", "lat"), sep = "p") %>%
    tidyr::separate(col = "tmpLon", into = c(NA, "lon"), sep = "X") %>%
    readr::type_convert()

  spData = dataL1
  coordinates(spData) = ~lon + lat
  gridded(spData) = TRUE
  # plot(spData)

  if (nrow(dataL1) < 1) { next }

  i = 1
  for (i in 1:nrow(stationData)) {

    # IDW 학습 및 전처리수행
    spDataL1 = gstat::idw(
      formula = val ~ 1
      , locations = spData
      , newdata = spNewData
      , nmax = 4
    ) %>%
      as.data.frame() %>%
      dplyr::rename(
        val = var1.pred
      ) %>%
      dplyr::select(-var1.var)

    # 데이터 병합
    dataL3 = dplyr::bind_rows(
      dataL3
      , data.frame(
        date = dtDateInfo
        , name = stationData$name
        , spDataL1
      )
    )

  }
}

saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "obs-to-idw.csv")
readr::write_csv(dataL3, file = saveFile)

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
# 안녕하세요. 박스플롯을 그려야할게 있는데 혹시 오전중으로 가능할까요?

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0131"

#================================================
# Main
#================================================
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggplot2)
library(lubridate)
library(MASS)
library(scales)
library(forcats)

# fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0129_ref_median_min_max.xlsx", sep = "/"))
# refData = openxlsx::read.xlsx(xlsxFile = fileInfo, sheet = "Sheet1")

fileList = Sys.glob(paste(globalVar$inpPath, "LSH0131_*.csv", sep = "/"))
dataL1 = tibble::tibble()
# fileInfo = "E:/04. TalentPlatform/Github/TalentPlatform-R/resources/input/o2job/LSH0131_J_B_TNPF.csv"

for (fileInfo in fileList) {
  fileName = tools::file_path_sans_ext(fs::path_file(fileInfo))

  data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR")) %>%
    dplyr::select(season, J)

  dataL1 = dplyr::bind_rows(
    dataL1
    , data.frame(
      fileName = fileName
      , data
    )
  )
}

dataL2 = dataL1 %>%
  dplyr::mutate(
    makeLabel = dplyr::case_when(
      stringr::str_detect(fileName, regex("_B_")) ~ "Beijing"
      , stringr::str_detect(fileName, regex("_G_")) ~ "Gwangju"
      , TRUE ~ "NA"
    )
  )

dataL3 = dataL2 %>%
  dplyr::group_by(makeLabel, season) %>%
  dplyr::summarise(
    meanVal = mean(J, na.rm = TRUE)
  )

# season 정렬
dataL2$season = forcats::fct_relevel(dataL2$season, c("winter", "spring"))

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "베이징 및 광주에 대한 계절별 박스플롯 시각화_6x6")

ggplot(dataL2, aes(x = season, y = J, fill = season)) +
  geom_boxplot(alpha = 1.0, outlier.shape = NA, width = 0.5) +
  stat_boxplot(geom = 'errorbar', width = 0.5) +
  geom_boxplot(alpha = 1.0, outlier.shape = NA, width = 0.5) +
  stat_summary(fun.y = mean, geom = "point", shape = 4, size = 4, color = "red") +
  labs(title = NULL, x = "season", y = bquote('' * J[10] * ' (' * cm^-1 * '·' * s^-1 * ')'), colour = NULL, fill = NULL, subtitle = NULL) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)), limits = c(1e-3, 1e2)) +
  annotation_logticks(sides = "l") +
  scale_fill_manual(values = c("spring" = "green", "winter" = "blue"), name = NULL, na.value = NA
    , labels = c("spring", "winter")
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 18)
    , panel.grid.major.x = element_blank()
    , panel.grid.major.y = element_blank()
    , panel.grid.minor.x = element_blank()
    , panel.grid.minor.y = element_blank()
    , panel.border = element_rect(size = 1.5)
    , legend.position = "none"
  ) +
  # facet_wrap(~makeLabel, scale = "free") +
  facet_wrap(~makeLabel) +
  ggsave(filename = saveImg, width = 6, height = 4, dpi = 600)


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
# 메일로 첨부하였습니다! 요구사항은 다음과 같습니다!
# 1. 관측소 제원을 인접격자의 데이터를 사용하여 idw를 산정하는 것입니다.
# 2. 관측소 idw 결과를 컬럼 형태로 csv파일로 받고싶습니다!
# 3. 병렬 처리를 통해 수행

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0132"

#================================================
# Main
#================================================
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(colorRamps)
library(lubridate)
library(extrafont)
library(ggrepel)
library(scales)
library(sf)
unloadNamespace('raster')
library(gstat)
library(sp)
library(metR)
library(akima)
library(stringr)

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0130_관측소+제원.xlsx", sep = "/"))
stationData = openxlsx::read.xlsx(xlsxFile = fileInfo, sheet = "Sheet1") %>%
  dplyr::select(Observation, X, Y) %>%
  dplyr::rename(
    name = Observation
    , lon = X
    , lat = Y
  )

# 관측소에 대한 공간 격자화
spNewData = stationData
coordinates(spNewData) = ~lon + lat
# plot(spNewData)

fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0130_tasmax_day_INM-CM4-8_historical_r1i1p1f1_gr1_19850101-20141231.csv", sep = "/"))
data = readr::read_csv(file = fileInfo2, locale = locale("ko", encoding = "EUC-KR"))

dtDateList = sort(unique(data$X1))

# [시작] 병렬 처리
oSocClu = parallel::makePSOCKcluster(parallel::detectCores())
doParallel::registerDoParallel(oSocClu)

# 외부 변수 등록
parallel::clusterExport(oSocClu, "dtDateList")
parallel::clusterExport(oSocClu, "data")
parallel::clusterExport(oSocClu, "dataL1")
parallel::clusterExport(oSocClu, "saveFile")
parallel::clusterExport(oSocClu, "globalVar")
parallel::clusterExport(oSocClu, "serviceName")
parallel::clusterExport(oSocClu, "spData")
parallel::clusterExport(oSocClu, "spNewData")
parallel::clusterExport(oSocClu, "stationData")
parallel::clusterExport(oSocClu, "dataL3")


# 외부 라이브러리 등록
parallel::clusterEvalQ(oSocClu, library(gstat))
parallel::clusterEvalQ(oSocClu, library(readr))
parallel::clusterEvalQ(oSocClu, library(tidyverse))
parallel::clusterEvalQ(oSocClu, library(sp))

tictoc::tic()
# parallel::parSapply(oSocClu, X = 1:length(dtDateList), function(x) {
parallel::parSapply(oSocClu, X = 1:12, function(x) {

  dataL1 = data %>%
    dplyr::filter(X1 == dtDateList[x]) %>%
    tidyr::gather(-X1, key = "key", value = "val") %>%
    tidyr::separate(col = "key", into = c("tmpLon", "lat"), sep = "p") %>%
    tidyr::separate(col = "tmpLon", into = c(NA, "lon"), sep = "X") %>%
    readr::type_convert()

  spData = dataL1
  sp::coordinates(spData) = ~lon + lat
  gridded(spData) = TRUE

  if (nrow(dataL1) < 1) { next }


  # IDW 학습 및 전처리수행
  spDataL1 = gstat::idw(
    formula = val ~ 1
    , locations = spData
    , newdata = spNewData
    , nmax = 4
  ) %>%
    as.data.frame() %>%
    dplyr::rename(
      val = var1.pred
    ) %>%
    dplyr::select(-var1.var)

  # 데이터 병합
  dataL3 = data.frame(
    date = dtDateList[x]
    , name = stationData$name
    , spDataL1
  )

  saveFile = sprintf("%s/%s_%s_%s.csv", globalVar$outPath, serviceName, "obs-to-idw", dtDateList[x])
  readr::write_csv(x = dataL3, file = saveFile)

})

tictoc::toc()

# [종료] 병렬 처리
parallel::stopCluster(oSocClu)

# 데이터 병합
fileList = Sys.glob(paste(globalVar$outPath, "LSH0132_obs-to-idw_*.csv", sep = "/"))

dataL4 = fileList %>%
  purrr::map(read.csv) %>%
  purrr::reduce(dplyr::bind_rows)

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "obs-to-idw")
readr::write_csv(x = dataL4, file = saveFile)


#.28675 < 2.22e-16 ***
#   Size                         714.095503      0.927668 769.77489 < 2.22e-16 ***
#   floor                        564.777648      4.502576 125.43435 < 2.22e-16 ***
#   age                         -148.336805      3.191817 -46.47410 < 2.22e-16 ***
#   month..서울특별시강남구    64829.526498    166.050030 390.42165 < 2.22e-16 ***
#   month..서울특별시강북구     -805.734690    218.561292  -3.68654 0.00022734 ***
#   month..서울특별시강서구     8053.255943    166.176981  48.46192 < 2.22e-16 ***
#   month..서울특별시관악구     3729.689058    189.166040  19.71648 < 2.22e-16 ***
#   month..서울특별시광진구    20243.945770    216.924873  93.32238 < 2.22e-16 ***
#   month..서울특별시구로구     2175.713896    170.686670  12.74683 < 2.22e-16 ***
#   month..서울특별시금천구    -2234.774512    225.847891  -9.89504 < 2.22e-16 ***
#   month..서울특별시노원구     4886.230846    153.575292  31.81652 < 2.22e-16 ***
#   month..서울특별시도봉구     -592.799775    173.696904  -3.41284 0.00064292 ***
#   month..서울특별시동대문구   3779.582525    186.466948  20.26945 < 2.22e-16 ***
#   month..서울특별시동작구    15775.363927    185.045366  85.25133 < 2.22e-16 ***
#   month..서울특별시마포구    20595.798723    185.385332 111.09724 < 2.22e-16 ***
#   month..서울특별시서대문구   7303.349393    195.439850  37.36878 < 2.22e-16 ***
#   month..서울특별시서초구    52595.139619    175.827265 299.12960 < 2.22e-16 ***
#   month..서울특별시성동구    22299.669538    185.716819 120.07351 < 2.22e-16 ***
#   month..서울특별시성북구     2946.320627    172.463117  17.08377 < 2.22e-16 ***
#   month..서울특별시송파구    27216.025880    280.283682  97.10171 < 2.22e-16 ***
#   month..서울특별시양천구    17899.695766    172.554986 103.73329 < 2.22e-16 ***
#   month..서울특별시영등포구  14385.956311    178.174602  80.74078 < 2.22e-16 ***
#   month..서울특별시용산구    43667.253458    223.656637 195.24238 < 2.22e-16 ***
#   month..서울특별시은평구     2020.139947    190.529807  10.60275 < 2.22e-16 ***
#   month..서울특별시종로구    14289.511716    304.658584  46.90336 < 2.22e-16 ***
#   month..서울특별시중구      15813.197846    263.366606  60.04253 < 2.22e-16 ***
#   month..1                   -6317.346257    115.773516 -54.56642 < 2.22e-16 ***
#   month..2                   -6732.291546    112.637680 -59.76944 < 2.22e-16 ***
#   month..3                   -8311.958184    105.020519 -79.14604 < 2.22e-16 ***
#   month..4                   -8151.903832    110.417059 -73.82830 < 2.22e-16 ***
#   month..5                   -5360.166660    110.174679 -48.65153 < 2.22e-16 ***
#   month..6                    -556.038739    106.241399  -5.23373 1.6616e-07 ***
#   month..8                   -1819.941258    109.352983 -16.64281 < 2.22e-16 ***
#   month..9                   -4275.959629    108.604913 -39.37170 < 2.22e-16 ***
#   month..10                  -3982.975714    105.797017 -37.64733 < 2.22e-16 ***
#   month..11                  -1404.807631    112.116698 -12.52987 < 2.22e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 26266.21 on 1048538 degrees of freedom
# Multiple R-squared:  0.5880779,	Adjusted R-squared:  0.5880637 
# F-statistic: 41581.62 on 36 and 1048538 DF,  p-value: < 2.2204e-16

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
# R을 이용한 통계 검정 및 회귀분석 그리고 ANOVA

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0134"

#================================================
# Main
#================================================
library(readxl)
library(tidyverse)
library(lubridate)
library(readr)
library(dummies)
library(ggpubr)
library(Metrics)
library(ggpubr)
library(DDPNA)
library(margins)

#=============================================================================
# 1. A 농가는 해외이주노동자 10명에 대하여 다음과 같이 월 임금을 주고 있다.
# 유의수준 5%에서 월평균 임금이 1(백만원)이라는 주장에 대하여 검정하시오.
#=============================================================================

data = data.frame(
  id = 1:10
  , wage = c(0.95, 1.2, 0.9, 1.05, 1.1, 1.5, 2.5, 1.2, 1.65, 0.85)
)

# 유의수준 (p-value)이 0.097로서 귀무가설 기각 (임금이 1이 아니다)
tTest = t.test(data$wage, mu = 1.0, conf.level = 0.95)
tTest

#====================================================================================
# 2. 모든 조건이 동일한 30개 시험포를  무작위로 3개 집단으로 나누고 
# 3종류의 비료를 사용하여 작물재배실험을 하였다.
# 아래의 표는 비료종류별 생산량(kg)을 보여준다.
# 생산량은 정규성과 등분산을 만족한다고 가정한다.
# 유의수준 5%에서 비료종류별로 생산량에 차이가 있는지를 검정하시오.
#====================================================================================

data = data.frame(
  a = c(120, 97, 100, 130, 120, 95, 132, 94, 92, 103)
  , b = c(95, 99, 80, 93, 99, 97, 90, 88, 96, 92)
  , c = c(43, 56, 67, 68, 79, 60, 82, 53, 58, 75)
) %>%
  tidyr::gather()

aovData = aov(value ~ key, data = data)

# b-a, c-a, c-b의 경우 유의수준 5% 기준에서 유의수준 (p-value)이 0.002 이하로서 
# 귀무가설 기각 (비료 종류별로 생산량의 차이가 있다)
TukeyHSD(aovData)

#====================================================================================
# 3. A, B, C, D 행정구역을 구성하는 도시의 시민 650명을 대상으로
# 직업을 조사하였다. 직업은 “White Collar”, “Blue Collar”, 
# “No Collar”로 구분하였다. 거주지역과 직업과 서로 독립적인지를 
# 검정하시오.
#====================================================================================

matData = matrix(c(90, 60, 104, 95, 30, 50, 51, 20, 30, 40, 45, 35), byrow = TRUE, nrow = 3)

dimnames(matData) = list(
  "row" = c("White Collar", "Blue Collar", "No Collar")
  , "col" = c("A", "B", "C", "D")
)

# 유의수준 (p-value)이 0.001 이하로서 귀무가설 채택 (거주지역과 직업 간에는 서로 관련성이 있다. 즉 독립적이지 않다.)
chisq.test(matData)

#====================================================================================
# 4. 아래의 데이터를 이용하여 경력이 소득에 미치는 영향을 분석하시오.
# 데이터“incomemid”는 다음과 같은 2개 변수를 포함한다. 
# income: 연 소득(천만원)
# carrer: 경력
#====================================================================================

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0134_incomemid.csv", sep = "/"))
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "UTF-8"))

lmFit = lm(income ~ career, data = data)
lmSumm = summary(lmFit)

# (1) 모회귀모형, 표본회귀모형, 표본회귀선을 구하시오
# 모회귀모형 : data$income
# 표본회귀모형 : predict(lmFit) + residuals(lmFit)
# 표본회귀선 : predict(lmFit)  
#              income = (1.0191701 * career) + 23.6509864

# (2) 5% 유의수준에서 회귀계수의 유의성에 대하여 검정하시오.

# 회귀계수의 유의수준 (p-value)가 0.001 이하로서 통계적으로 유의하다.
#               Estimate Std. Error t value   Pr(>|t|)    
# (Intercept) 23.6509864  2.6678754 8.86510 6.5701e-14 ***
#   career     1.0191701  0.1327603 7.67677 1.8808e-11 ***

# (3) 한계효과를 대상으로 추정결과에 대하여 해석하시오.    

# 일반적인 최소 제곱 회귀에서는 한계 효과 (즉, 결과 변수의 척도)를 조사하는 유일한 방법이 있습니다.
# 그러나 일반화 선형 모델에서는 진정한 한계 효과 (즉 선형 예측 자의 척도에서 각 변수의 한계 기여도) 또는 부분 효과 (즉 기여도)를 조사 할 수 있습니다. 

# 그 결과 평균 1.019 기여도
margins::margins(lmFit)


# (4) 5% 유의수준에서 회귀계수의 신뢰구간을 구하라(직접 풀어볼것).
# confint(lmFit)

# 2.5 %       97.5 %
# (Intercept) 18.3507863874 28.951186466
# career       0.7554184673  1.282921634
for (i in 1:nrow(lmSumm$coef)) {
  cat(
    lmSumm$coef[i, 1] - qt(0.975, df = lmSumm$df[2]) * lmSumm$coef[i, 2]
    , lmSumm$coef[i, 1] + qt(0.975, df = lmSumm$df[2]) * lmSumm$coef[i, 2]
    , "\n"
  )
}

# (5) 경력이 35년인 사람의 소득은 얼마로 예측되는가?
# 59.3219382
predict(lmFit, newdata = data.frame(career = 35))

#====================================================================================
# 5. 회귀분석시 필요한 가정(Assumption)을 Y(종속변수), X(독립변수), ε(오차항)에 대하여 설명하시오.
#====================================================================================
# 일반적으로 최소자승법에 의거 추론할 경우 다음과 같은 기본적인 가정이  요하다.

# 가정 1) 회귀모형은 다음과같이 모수에 대해 선형(linear)인 모형이다: Yi = α + βXi + εi
# 선형모형(linear model): 모수 α 와 β 에 대하여 1 차 미분이 모수 α 와 β의 함수가 아니라 일정한 상수가 된다

# 가정 2) 전체표본에 있어서 독립변수 X 는 적어도 서로 다른 두 값을 가져야 한다.
# 
# 가정 3) 오차항의 평균은 0 이다: E(εi ) = 0 오차항들이 회귀선을 중심으로 대칭적으로 분포되어있어야 한다.
# 이 가정은 최소자승법에 의한 추정과정상의 계산을 간편하게 하기 위해 만들어진 편의상의 조건식
# 
# 가정 4) 독립변수 X 는 비확률(nonstochastic) 변수이다: E[εiXi ] = Xi E[εi] = 0
# 독립변수는 비확률적 변수로 일반적인 상수(constant)로 취급한다.
# 
# 가정 5) 오차항은 모든 관찰치에 대해 σ2 의 일정한 분산을 갖는다: var(εi) = 2, E(εi 2)=σ2
# 관측시점에 관계없이(i 값에 관계없이) 오차항들의 분포는 동일한 분산을 갖는다.
# 각 독립변수 X 의 값에 있어서 종속변수 Y 가 그 평균을 중심으로 분포되어 있는 정도가 같다. “동분산(homoskedasticity)” 가정이라 한다
# 
# 반면에 이분산(heteroskedasticity): 시계열 데이터(time series data)나 횡단면 데이터 (cross-section data)경우 분석대상의 기간이나 크기따라 분산의 크기가 달라지는 경우.
# 가정 6) 서로 다른 관찰치간의 오차항은 상관이 없다: Cov(εi, εj ) = E[εi, εj ] = 0 (i ≠j)
# 오차항끼리는 서로 1 차독립적인임을 의미한다.
# 
# 가정 7) 오차항이 정규분포를 따르며, εi ~ N(0,σ2)

#====================================================================================
# 6. 다음은 회귀분석의 결과표와 ANOVA표를 보여준다.
#====================================================================================
# (1) 빈 칸(ⓐ와 ⓑ)을 채우시오. 
# 표준오차 (a) = 계수 / t-통계량
# = 30.014 / 72.70
# = 0.4128473177

# 계수 (b) = 표준오차 * t-통계량
# = 0.0016 * -28.48
# = -0.045568

# (2) R2을 구하시오.
# 회귀 제곱평균 / 변동의 총합
# 266.682 / sum(266.682, 29.257)
# 결정계수 = 0.9011384103


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
# 설문조사 데이터 분석 및 ANOVA 검정

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0135"

#================================================
# Main
#================================================
library(readxl)
library(tidyverse)
library(lubridate)
library(readr)
library(dummies)
library(ggpubr)
library(Metrics)
library(ggpubr)
library(DDPNA)
library(margins)

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0135_데이터.xlsx", sep = "/"))
data = openxlsx::read.xlsx(xlsxFile = fileInfo, sheet = "Sheet1") %>%
  tibble::as.tibble()

# data$CLASS1 %>% unique() %>% sort()

dataL1 = data %>%
  dplyr::mutate(
    typeGENDER = dplyr::case_when(
      GENDER == 1 ~ "남자"
      , GENDER == 2 ~ "여자"
    )
    , typeCOEDU = dplyr::case_when(
      COEDU == 1 ~ "남녀공학"
      , COEDU == 2 ~ "남학교"
      , COEDU == 3 ~ "여학교"
    )
    , typeREGION = dplyr::case_when(
      REGION == 1 ~ "특별시"
      , REGION == 2 ~ "광역시"
      , REGION == 3 ~ "중소도시"
      , REGION == 4 ~ "읍면지역"
    )
    , typeY1S15 = dplyr::case_when(
      Y1S15 == 1 ~ "없다"
      , Y1S15 == 2 ~ "있다"
      , Y1S15 == -1 ~ "무응답"
    )
    , typeY1H7_2_1 = dplyr::case_when(
      Y1H7_2_1 == 0 ~ "안함"
      , Y1H7_2_1 == 1 ~ "함"
    )
    , typeSECTOR = dplyr::case_when(
      SECTOR == 1 ~ "국공립"
      , SECTOR == 2 ~ "사립"
    ) %>%
      dplyr::na_if(-1)
  )

#=====================================================================================
# 1. 1학년과 2학년 때 학생들의 독서에 대한 즐거움 정도가 차이가 있는지,
# 또한 1학년과 2학년 때의 국어성적에도 차이가 있는지 봐주세요. 
#=====================================================================================
# 독서에 대한 즐거움 정도
dataL1 %>%
  dplyr::select(sid, dplyr::contains("Y1S19"), dplyr::contains("Y2S14")) %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::summarise(
    sumY1 = sum(c(Y1S19_2, Y1S19_3, Y1S19_5, Y1S19_7), na.rm = TRUE)
    , sumY2 = sum(c(Y2S14_2, Y2S14_3, Y2S14_5, Y2S14_7), na.rm = TRUE)
    , meanY1 = mean(c(Y1S19_2, Y1S19_3, Y1S19_5, Y1S19_7), na.rm = TRUE)
    , meanY2 = mean(c(Y2S14_2, Y2S14_3, Y2S14_5, Y2S14_7), na.rm = TRUE)
  )

# A tibble: 1 x 4
# 합계/평균 국어성적 (1학년, 2학년)
# sumY1 sumY2 meanY1 meanY2
# <dbl> <dbl>  <dbl>  <dbl>
# 2571  2491   3.00   2.91

# 국어성적에도 차이
dataL1 %>%
  dplyr::select(sid, dplyr::contains("Y1KOR_S"), dplyr::contains("Y2KOR_S")) %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::summarise(
    meanY1 = mean(c(Y1KOR_S), na.rm = TRUE)
    , meanY2 = mean(c(Y2KOR_S), na.rm = TRUE)
  )

# A tibble: 1 x 2
# 평균 국어성적 (1학년, 2학년)
# meanY1 meanY2
# <dbl>  <dbl>
# 66.6   61.0

#=====================================================================================
# 2. 학생들이 다니고 있는 학교가 소규모 학교인지 여부에 
# 따라서 학생들의 2학년 때 학교교육 만족도와 학업자아개념, 국어 성적의 차이가 있는지를 봐주세요. 
# 또한 남녀 학생의 성별에 따라서 학생들의 2학년 때 학교교육 만족도와 학업자아개념, 국어 성적의 차이가 있는지 봐주세요. 
#=====================================================================================
# 소규모에 따른 분석
dataL1 %>%
  dplyr::select(sid, CLASS1, dplyr::contains("Y2S5"), dplyr::contains("Y2S2"), dplyr::contains("Y2KOR_S")) %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::mutate(classType = ifelse(CLASS1 < 3, "소규모", "대규모")) %>%
  dplyr::group_by(classType) %>%
  dplyr::summarise(
    sumS5Y2 = sum(c(Y2S5_15, Y2S5_16, Y2S5_17, Y2S5_18, Y2S5_19, Y2S5_20, Y2S5_21, Y2S5_22, Y2S5_23), na.rm = TRUE)
    , meanS5Y2 = mean(c(Y2S5_15, Y2S5_16, Y2S5_17, Y2S5_18, Y2S5_19, Y2S5_20, Y2S5_21, Y2S5_22, Y2S5_23), na.rm = TRUE)

    , sumS2Y2 = sum(c(Y2S2_24, Y2S2_25, Y2S2_26, Y2S2_28, Y2S2_29, Y2S2_30), na.rm = TRUE)
    , meanS2Y2 = mean(c(Y2S2_24, Y2S2_25, Y2S2_26, Y2S2_28, Y2S2_29, Y2S2_30), na.rm = TRUE)

    , meanKorY2 = mean(c(Y2KOR_S), na.rm = TRUE)
  )

# A tibble: 2 x 6
# 학교여부  만족도 (합계, 평균), 학업자아개념 (합계, 평균), 국어성적 (평균)
# classType sumS14Y2 meanS14Y2 sumS2Y2 meanS2Y2 meanKorY2
# <chr>        <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
# 대규모       7301     2.92    5340     3.20      61.9
# 소규모       1475     3.21     960     3.14      58.0


# 성별에 따른 분석
dataL1 %>%
  dplyr::select(sid, typeGENDER, dplyr::contains("Y2S5"), dplyr::contains("Y2S2"), dplyr::contains("Y2KOR_S")) %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::group_by(typeGENDER) %>%
  dplyr::summarise(
    sumS5Y2 = sum(c(Y2S5_15, Y2S5_16, Y2S5_17, Y2S5_18, Y2S5_19, Y2S5_20, Y2S5_21, Y2S5_22, Y2S5_23), na.rm = TRUE)
    , meanS5Y2 = mean(c(Y2S5_15, Y2S5_16, Y2S5_17, Y2S5_18, Y2S5_19, Y2S5_20, Y2S5_21, Y2S5_22, Y2S5_23), na.rm = TRUE)

    , sumS2Y2 = sum(c(Y2S2_24, Y2S2_25, Y2S2_26, Y2S2_28, Y2S2_29, Y2S2_30), na.rm = TRUE)
    , meanS2Y2 = mean(c(Y2S2_24, Y2S2_25, Y2S2_26, Y2S2_28, Y2S2_29, Y2S2_30), na.rm = TRUE)

    , meanKorY2 = mean(c(Y2KOR_S), na.rm = TRUE)
  )

# A tibble: 2 x 6
# 성별  만족도 (합계, 평균), 학업자아개념 (합계, 평균), 국어성적 (평균)
# GENDER sumS14Y2 meanS14Y2 sumS2Y2 meanS2Y2 meanKorY2
# <dbl>    <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
# 남자   4466     3.10    3139     3.27      56.9
# 여자   4310     2.83    3161     3.12      65.4

#=====================================================================================
# 3. 학교가 남녀공학인지 아니면 남학교 혹은 여학교인지에 따라서 학생들의 학교교육 
# 만족도에 차이가 있는지 봐주세요. 또한 지역규모에 따라서 학교교육 만족도에 차이가 
# 있는지 봐주세요. 
#=====================================================================================
# 남녀공학
dataL1 %>%
  dplyr::select(sid, typeCOEDU, dplyr::contains("Y2S5")) %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::group_by(typeCOEDU) %>%
  dplyr::summarise(
    sumS5Y2 = sum(c(Y2S5_15, Y2S5_16, Y2S5_17, Y2S5_18, Y2S5_19, Y2S5_20, Y2S5_21, Y2S5_22, Y2S5_23), na.rm = TRUE)
    , meanS5Y2 = mean(c(Y2S5_15, Y2S5_16, Y2S5_17, Y2S5_18, Y2S5_19, Y2S5_20, Y2S5_21, Y2S5_22, Y2S5_23), na.rm = TRUE)
  )

# A tibble: 3 x 3
# 남녀공학  만족도 (합계, 평균)
# typeCOEDU sumS5Y2 meanS5Y2
# <chr>       <dbl>    <dbl>
# 남녀공학     5945     3.00
# 남학교       1846     3.06
# 여학교       1264     2.65

# 지역규모
dataL1 %>%
  dplyr::select(sid, typeREGION, dplyr::contains("Y2S5")) %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::group_by(typeREGION) %>%
  dplyr::summarise(
    sumS5Y2 = sum(c(Y2S5_15, Y2S5_16, Y2S5_17, Y2S5_18, Y2S5_19, Y2S5_20, Y2S5_21, Y2S5_22, Y2S5_23), na.rm = TRUE)
    , meanS5Y2 = mean(c(Y2S5_15, Y2S5_16, Y2S5_17, Y2S5_18, Y2S5_19, Y2S5_20, Y2S5_21, Y2S5_22, Y2S5_23), na.rm = TRUE)
  )

# A tibble: 4 x 3
# 지역규모  만족도 (합계, 평균)
# typeREGION sumS5Y2 meanS5Y2
# <chr>        <dbl>    <dbl>
# 광역시        2346     2.93
# 읍면지역      1202     3.18
# 중소도시      3793     2.95
# 특별시        1714     2.89

#=====================================================================================
# 4. 2학년 때의 학교교육 만족도가 지역규모와 학생 자신의 학생회 활동 경험 여부에 
# 따라서 차이가 있는지를 살펴보되, 학생회 활동 경험에 따라서 지역규모에 따른 학교교육 
# 만족도 차이가 다르게 나타나는지에 초점을 두어 봐주세요. 
#=====================================================================================
# 지역규모 및 학생회활동경험
dataL1 %>%
  dplyr::select(sid, typeREGION, typeY1S15, dplyr::contains("Y2S5")) %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::group_by(typeREGION, typeY1S15) %>%
  dplyr::summarise(
    sumS5Y2 = sum(c(Y2S5_15, Y2S5_16, Y2S5_17, Y2S5_18, Y2S5_19, Y2S5_20, Y2S5_21, Y2S5_22, Y2S5_23), na.rm = TRUE)
    , meanS5Y2 = mean(c(Y2S5_15, Y2S5_16, Y2S5_17, Y2S5_18, Y2S5_19, Y2S5_20, Y2S5_21, Y2S5_22, Y2S5_23), na.rm = TRUE)
  )

# A tibble: 12 x 4
# Groups:   typeREGION [4]
# 지역규모 학생회활동경험 만족도 (합계, 평균)
# typeREGION typeY1S15 sumS5Y2 meanS5Y2
# <chr>      <chr>       <dbl>    <dbl>
# 2 광역시     없다         1786    2.83 
# 3 광역시     있다          506    2.68 
# 5 읍면지역   없다          793    3.04 
# 6 읍면지역   있다          370    2.94 
# 8 중소도시   없다         2805    2.64 
# 9 중소도시   있다          857    2.72 
# 11 특별시     없다         1224    2.52 
# 12 특별시     있다          386    2.38 

# 1 광역시     무응답          9    0.333
# 4 읍면지역   무응답         21    2.33 
# 7 중소도시   무응답         32    3.56 
# 10 특별시     무응답         35    0.972

# 중소도시를 제외한 학생회 활동경험에 따라 학교교육 만족도는 낮았다.
# 특히 읍면지역, 광역시, 특별시으로 갈수록 낮았다.

#*************************************
# 유의성 검정
#*************************************
# dataL2 = dataL1 %>%
#   dplyr::select(sid, typeREGION, typeY1S15, dplyr::contains("Y2S5")) %>% 
#   dplyr::filter(complete.cases(.)) %>%
#   dplyr::group_by(sid) %>%
#   dplyr::mutate(
#     sumS5Y2 = sum(c(Y2S5_15, Y2S5_16, Y2S5_17, Y2S5_18, Y2S5_19, Y2S5_20, Y2S5_21, Y2S5_22, Y2S5_23), na.rm = TRUE)
#     , meanS5Y2 = mean(c(Y2S5_15, Y2S5_16, Y2S5_17, Y2S5_18, Y2S5_19, Y2S5_20, Y2S5_21, Y2S5_22, Y2S5_23), na.rm = TRUE)
#   )

# dataL2 %>% 
#   dplyr::group_by(typeREGION, typeY1S15) %>% 
#   dplyr::summarise(
#     sumVal = sum(sumS5Y2, na.rm = TRUE)
#     , meanVal = mean(meanS5Y2, na.rm = TRUE)
#   )

# typeList = c("typeREGION", "typeY1S15")
# colList = c("meanS5Y2")
# 
# dataL3 = tibble::tibble()
# 
# for (i in 1:length(typeList)) {
#   for (j in 1:length(colList)) {
#     
#     rsAov = aov(get(colList[j], dataL2) ~ get(typeList[i], dataL2), data = dataL2) %>%
#       summary %>%
#       unlist()
#     
#     tmpData = tibble::tibble(
#       type = typeList[i]
#       , col = colList[j]
#       , fVal = rsAov[7] %>% round(2)
#       , pVal = rsAov[9] %>% round(2)
#     ) %>%
#       dplyr::mutate(label = paste0(fVal, " (", pVal, ")"))
#     
#     dataL3 = dplyr::bind_rows(dataL3, tmpData)
#   }
# }

#=====================================================================================
# 5. 1학년 때 수학교과에 있어서 수준별 이동수업을 하는지 여부가 학교의 
# 설립구분과 지역규모에 따라서 차이가 있는지 봐주세요.
#=====================================================================================
# 지역규모
dataL1 %>%
  dplyr::select(sid, typeSECTOR, typeREGION, typeY1H7_2_1, dplyr::contains("Y1MAT_S")) %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::group_by(typeSECTOR, typeREGION, typeY1H7_2_1) %>%
  dplyr::summarise(
    meanKorY2 = mean(c(Y1MAT_S), na.rm = TRUE)
  )

# A tibble: 15 x 4
# Groups:   typeSECTOR, typeREGION [8]
# 설립구분 지역 수준별 이동수업 평균 수학점수
# typeSECTOR typeREGION typeY1H7_2_1 meanKorY2
# <chr>      <chr>      <chr>            <dbl>
# 1 국공립     광역시     안함              62.8
# 2 국공립     광역시     함                61.1
# 3 국공립     읍면지역   안함              47.9
# 4 국공립     읍면지역   함                50.6
# 5 국공립     중소도시   안함              64.5
# 6 국공립     중소도시   함                56.1
# 7 국공립     특별시     안함              51.2
# 8 국공립     특별시     함                53.1
# 9 사립       광역시     안함              62.9
# 10 사립       광역시     함                65.7
# 11 사립       읍면지역   함                48.7
# 12 사립       중소도시   안함              49.6
# 13 사립       중소도시   함                68.3
# 14 사립       특별시     안함              51  
# 15 사립       특별시     함                37.5

#=====================================================================================
# 6. 선생님이 학업성적을 얼마나 중요하게 생각하는지, 그리고 선생님이 인성교육을 
# 얼마나 중요하게 생각하는지에 따라서 선생님의 교육태도가 학업성적을 얼마나 중요하게 
# 생각하는지, 그리고 인성교육을 얼마나 중요하게 생각하는지에 따라서 학생들의 
# 수학성적의 평균이 다르게 나타나는지 이원분산분석을 사용하여 봐주세요.
# (필요한 경우 하위 범주를 통합하여 범주의 수를 조정 가능). 
#=====================================================================================
rsAov = aov(dataL1$Y1MAT_S ~ dataL1$Y1S11_5 +
  dataL1$Y1S11_6 +
  dataL1$Y1S11_5:dataL1$Y1S11_6)
summary(rsAov)

# 교육태도 1 (선생님학업성적중요시)
# 교육태도 1의 경우 한 유의수준 (p-value)가 0.001로서
# 신뢰구간 95% (유의수준 0.05 이하)보다 작기 때문에 귀무가설 기각
# (즉 교육태도 1에 따른 수학 성적 차이는 있다.)

# 교육태도 2 (선생님인성교육중요시)
# 교육태도 2의 경우 유의수준 (p-value)가 0.07로서
# 신뢰구간 95% (유의수준 0.05 이하)보다 크기 때문에 귀무가설 채택
# (즉 교육태도 2에 따른 수학 성적 차이는 없다.)

# 교호작용: 교육태도 1 (선생님학업성적중요시) + 교육태도 2 (선생님인성교육중요시)
# 교육태도 1과 교육태도 2에 따른 교호작용 효과에 대한 유의수준 (p-value)가 0.01로서
# 신뢰구간 95% (유의수준 0.05 이하)보다 작기 때문에 귀무가설 기각
# (즉 교육태도 1과 교육태도 2의 교호작용 효과가 있다.)

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
# 1. correlations using cor.test
# 2. A t-Test
# 4. A Shapiro-Wilk test of normalit

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0136"

#================================================
# Main
#================================================
library(readxl)
library(tidyverse)
library(lubridate)
library(readr)
library(ggplot2)
library(MASS)
library(moonBook)
library(webr)
library(ggplot2)
library(tidyverse)
library(lsr)
library(ltm)

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0136_Provisional_COVID-19_Death_Counts_by_Sex__Age__and_State.csv", sep = "/"))
data = read.csv(fileInfo) %>%
  tibble::as.tibble()

# 데이터 전처리
dataL1 = data %>%
  dplyr::filter(State %in% c("Ohio", "Texas ", "Oregon", "Montana", "Wisconsin", "New York", "Florida", "Washington")) %>%
  dplyr::filter(Sex == "All Sexes") %>%
  dplyr::select("Age.Group", "State", "COVID.19.Deaths") %>%
  dplyr::group_by(State, Age.Group) %>%
  dplyr::summarise(
    COVID.19.Deaths.mean = sum(COVID.19.Deaths, na.rm = TRUE)
  )

dataL2 = data %>%
  dplyr::filter(State %in% c("Ohio", "Texas ", "Oregon", "Montana", "Wisconsin", "New York", "Florida", "Washington")) %>%
  dplyr::filter(Sex == "All Sexes") %>%
  dplyr::select("Age.Group", "COVID.19.Deaths") %>%
  dplyr::group_by(Age.Group) %>%
  dplyr::summarise(
    COVID.19.Deaths.mean = sum(COVID.19.Deaths, na.rm = TRUE)
  )

#=========================================
# 1. correlations using cor.test
#=========================================
dataL3 = dataL2 %>%
  dplyr::filter(!Age.Group %in% c("0-17 years", "1-4 years", "1-4 years", "All Ages", "Under 1 year")) %>%
  dplyr::mutate(
    age = stringr::str_replace_all(Age.Group, "years", "")
    , age = stringr::str_replace_all(age, "and over", "")
    , age = stringr::str_trim(age)
  ) %>%
  tidyr::separate(col = "age", into = c("srtAge", "endAge"), sep = "-") %>%
  readr::type_convert() %>%
  dplyr::group_by(Age.Group) %>%
  dplyr::mutate(
    age = mean(c(srtAge, endAge), na.rm = TRUE)
  )

# 상관계수는 0.91로서 0.01 이하의 유의수준을 지닌다.
# 즉 나이에 따른 사망수의 예측 설명력 (결정계수 = 상관계수^2)는 82% 설명 가능하다.
cor.test(dataL3$age, dataL3$COVID.19.Deaths.mean)

#====================================================
# 2. A t-Test (T 테스트)
#====================================================
# rsAov에서 P-value는 6.9377e-11로서 0.05보다 작기 때문에 나이에 따른 사망자 수의 차이가 있다.
rsAov = aov(COVID.19.Deaths.mean ~ Age.Group, data = dataL1)
summary(rsAov)

# 각 나이에 따른 분류 결과 및 유의성 검정 (msg) 포함 
resHsd = TukeyHSD(rsAov)

resHsdL1 = resHsd$Age.Group %>%
  as.data.frame() %>%
  dplyr::mutate(
    msg = ifelse(`p adj` < 0.05
      , sprintf("유의수준 (p.value)이 %s로서 귀무가설 기각 (사망자 수의 차이가 있다)", round(`p adj`, 2))
      , sprintf("유의수준 (p.value)이 %s로서 귀무가설 채택 (사망자 수의 차이가 없다)", round(`p adj`, 2))
    )
  )


#====================================================
# 4. A Shapiro-Wilk test of normalit (정규성 검정)
#====================================================
keyList = dataL1$Age.Group %>% unique %>% sort()
dataL4 = tibble::tibble()

for (key in keyList) {
  dataL3 = dataL1 %>%
    dplyr::filter(Age.Group == key)

  if (nrow(dataL3) < 1) { next }
  if (sum(dataL3$COVID.19.Deaths.mean, na.rm = TRUE) < 1) { next }

  res = shapiro.test(dataL3$COVID.19.Deaths.mean)
  pVal = res$p.value

  if (pVal < 0.05) {
    msg = sprintf("유의수준 (p.value)이 %s로서 귀무가설 기각 (정규 분포를 따르지 않음)", round(pVal, 2))
  } else {
    msg = sprintf("유의수준 (p.value)이 %s로서 귀무가설 채택 (정규 분포를 따름)", round(pVal, 2))
  }

  dataL4 = dplyr::bind_rows(dataL4, data.frame(
    key = key
    , statistic = res$statistic
    , p.value = pVal
    , msg = msg
  ))
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

#================================================
# 요구사항
#================================================
# 이 자료에 대한 산점도, 잔차분석을 통한 로버스트한 중회귀 직선을 찾아주시면 감사하겠습니다!!

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0137"

#================================================
# Main
#================================================
library(stringr)
library(openxlsx)
library(MASS)
library(showtext)
library(quantreg)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(Metrics)

# showtext::showtext_opts(dpi = 600)
# showtext::showtext.auto()

# 이 자료에 대한 산점도, 잔차분석을 통한 로버스트한 중회귀 직선을 찾아주시면 감사하겠습니다!!
fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0137_자료.xlsx", sep = "/"))
data = openxlsx::read.xlsx(fileInfo, sheet = 1)

# 로버스트 선형회귀 회귀모형
rlmFit = rlm(y ~ x1 + x2, data)
summary(rlmFit)

# 잔차 분석
tryCatch(

  expr = {
    log4r::info(log, sprintf("%s", "[START] Make Image"))

    saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "잔차 분석")

    png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
    plot(rlmFit, 1)
  }

  , warning = function(warning) {
    log4r::warn(log, warning)
  }

  , error = function(error) {
    log4r::error(log, error)
  }

  , finally = {
    dev.off()

    log4r::info(log, sprintf("%s", "[END] Make Image"))
  }
)

# 산점도
dataL1 = data.frame(
  y = data$y
  , yHat = predict(rlmFit, newdata = data)
)

corTest = cor.test(dataL1$y, dataL1$yHat)
corVal = round(corTest$estimate, 2)
pVal = round(corTest$p.value, 2)
biasVal = round(Metrics::bias(dataL1$y, dataL1$yHat), 2)
rmseVal = round(Metrics::rmse(dataL1$y, dataL1$yHat), 2)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "산점도")

ggpubr::ggscatter(dataL1, x = "y", y = "yHat", color = "black", add = "reg.line", conf.int = TRUE, add.params = list(color = "blue", fill = "lightblue")) +
  stat_regline_equation(label.x = 30, label.y = 90, size = 5) +
  annotate("text", x = 30, y = 85, size = 5, label = sprintf("R = %s (P < %.3f)", corVal, pVal), hjust = 0) +
  annotate("text", x = 30, y = 80, size = 5, label = sprintf("Bias = %s", biasVal), hjust = 0) +
  annotate("text", x = 30, y = 75, size = 5, label = sprintf("RMSE = %s", rmseVal), hjust = 0) +
  xlim(30, 90) +
  ylim(30, 90) +
  theme_bw() +
  labs(title = NULL, x = "실제값", y = "예측값", subtitle = "로버스트 선형회귀 회귀모형 산점도") +
  coord_equal() +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 6, height = 6, dpi = 600)


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
# 아래의 문제는 과제 6번으로 제출하셍요. R 코드만 제출하면 됩니다.
# 과제 7) 예제 txt 벡터에서 이메일 주소만 삭제하기

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "o2job"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0140"

#================================================
# Main
#================================================
library(stringr)

txt <- "A씨는 미국 월스트리트 저널(WSJ)에서 다음과 같이... 말했습니다. ㅋㅋ ㅎㅎ ㅠㅠ
한길수 특파원(abc@gmail.com)"

stringr::str_replace_all(txt, "[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "")


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
# 코로나 확진자 데이터 처리 및 시각화

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0142"

#================================================
# Main
#================================================
library(stringr)
library(openxlsx)
library(MASS)
library(showtext)
library(quantreg)
library(ggplot2)
library(tidyverse)
library(Metrics)
library(vcd)
library(coronavirus)
library(plotly)
library(ggpubr)
library(processx)
library(webshot)
library(htmlwidgets)

# showtext::showtext_opts(dpi = 100)
# showtext::showtext.auto()

# 1. Anscombe’s quartet은 네 개의 데이터셋으로 구성되어있고
# 각 데이터셋은 x와 y 두 개의 변수로 이루어져있다. 
# 각 데이터셋을 산점도로 표현하고, y를 결과변수, x를 설명변수로 하는 회귀직선을 산점도 위에 그려보시오
# *참고: 교과서 82쪽의 그림 3-8
# *데이터셋: R에 내장되어있는 anscombe (anscombe를 콘솔에 입력하거나 스크립트에 적어서 실행시키면 불러올 수 있음)

ansData = anscombe

# type 1 교과서 수행
xAxis = ansData$x1
yAxis = ansData$y1

lmFit = lm(yAxis ~ xAxis, data = data)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "산점도")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
plot(xAxis, yAxis, xlim = c(0, 15), ylim = c(0, 15), main = "산점도")
abline(lmFit, col = 'red')
dev.off()

# type 1-4 수행
ansDataL1 = dplyr::bind_rows(
  data.frame(x = ansData$x1, y = ansData$y1, type = "type 1")
  , data.frame(x = ansData$x2, y = ansData$y2, type = "type 2")
  , data.frame(x = ansData$x3, y = ansData$y3, type = "type 3")
  , data.frame(x = ansData$x4, y = ansData$y4, type = "type 4")
)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "산점도2")

ggpubr::ggscatter(ansDataL1, x = "x", y = "y", color = "type", add = "reg.line") +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 0.95, color = "black") +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.85) +
  facet_wrap(~type, scale = "free") +
  # theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 6, height = 6, dpi = 600)


# 2. COVID-19 관련 한국, 중국, 미국, 프랑스의 COVID-19 신규 확진자 수의 추이를 
# 데이터 시각화로 비교하고 향후 추이에 대해 의견 기술하라
# (데이터는 과제 작성일까지 올라와 있는 것을 이용하면 됨).
# *데이터 소스 : EU Open data
# https://data.europa.eu/euodp/en/data/dataset/covid-19-coronavirus-data-daily-up-to-14-december-2020
# Resources 아래의 COVID-19 cases worldwide ? daily 파일을 이용할 것
# *국가 코드 (geoId): 한국=KR, 중국=CN, 미국=US, 프랑스=FR

covidData = read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")

covidDataL1 = covidData %>%
  dplyr::mutate(
    dtDate = readr::parse_date(as.character(dateRep), "%d/%m/%Y")
  ) %>%
  dplyr::filter(
    geoId %in% c("KR", "CN", "US", "FR")
  )

# 2019년 12월 31일에서 2020년 12월 14일 동안 국가별 (한국, 중국, 미국, 프랑스)에 대한 확진자수를 산점도 및 선형회귀직선으로 시각화하였다.
# 그 결과 중국을 제외한 국가 (한국, 미국, 프랑스)는 2-3차 대유행를 거쳐 대부분 양의 상관관계 (0.42-0.80)으로서 0.001 이하의 유의수준을 보였다.
# 반면에 중국의 경우 1차 대유행 이후로 급격한 음의 상관계수 (-0.29)을 보였다.
# 이러한 결과를 토대로 향후 중국의 경우 코로나 종식으로 판단되나 타 국가에서는 향후 3-4차 이후의 코로나 대유행이 발생할 것으로 사료된다.

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "코로나 산점도")

ggpubr::ggscatter(covidDataL1, x = "dtDate", y = "cases", color = "geoId", add = "reg.line") +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 0.95, color = "black") +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.85) +
  facet_wrap(~geoId, scale = "free") +
  ggsave(filename = saveImg, width = 8, height = 6, dpi = 600)


# 3. 버클리대학 입학 자료(R에 내장된 데이터 UCBAdmissions, 교재 p.94 연습문제 3번 참조)를 바탕으로 버클리대학 입시에 
# 성차별이 있었는지를 시각화를 통해 탐구하고 설명하시오.

ucbaData = UCBAdmissions

# 그 결과 A/B 부서의 경우 남성이 더 많고 반면에 C/D/E/F 부서에서는 여성이 더 많다.
# 또한 부서 A/B는 성별에 관계없이 지원자 3명 중 약 2명을 인정하는 반면, 부서 C/D/E/F는 3 명 중 1명 또는 그 이하를 인정한다.
ucbaDataL1 = vcd::structable(Gender ~ Dept + Admit, data = ucbaData)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "버클리대학 성차별 시각화")
png(file = saveImg, width = 8, height = 6, units = "in", res = 600)
vcd::cotabplot(ucbaDataL1)
dev.off()

apply(ucbaData, c(2, 3, 1), sum)

# 4. R 패키지 “coronavirus”에 내장된 coronavirus 데이터셋을 이용하여, 
# 2020년 1월 22일부터 2021년 1월 21일까지 우리나라의 일일 코로나19 확진자 수의 변화를 시각화하시오.
# 시각화된 자료를 이용하여 1, 2, 3차 대유행 시점에 관하여 논하고 4차 대유행 가능성에 대해 논하시오.
# (힌트: country = “Korea, South”, type = “confirmed” 인 관측치만 사용할 것.)

coronaData = coronavirus

coronaDataL1 = coronaData %>%
  dplyr::filter(
    dplyr::between(date, as.Date("2020-01-22"), as.Date("2021-01-21"))
    , type == "confirmed"
    , country %in% c("Korea, South")
  )

lmFit = lm(cases ~ date, coronaDataL1)
summary(lmFit)

# 2020년 01월 22일에서 2021년 01월 21일 동안 대한민국 남한에 대한 확진자를 인터랙티브 그래프로 시각화하였다.
# 그 결과 1차 유행의 정점이었던 2020년 03월 93일부터 2차 유행 정점인 2020년 08월 26일까지는 176일이 걸렸습니다. 
# 또한 2차 유행의 정점에서 2차 유행의 정점까지는 120일이 소요되었습니다.
# 현재 3차 유행 (2020년 12월 24일)도 70-120일 이내로 짧은 주기로 오고 있습니다.

# 4차 대유행을 시뮬레이션 하기 위해서 선형회귀곡선 ((확진자) = 1.50 x (날짜) -27625.34)을 통해 확인한 결과
# 그 결과 수정된 결정계수는 0.33 (양의 상관관계)으로서 0.01 이하의 유의수준을 나타내고 있다.
# 이러한 결과를 토대로 4차 유행 또한 더 짧아질 수 있다고 판단됩니다.

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "대한민국 남한 코로나 시각화")

poltlyData = coronaDataL1 %>%
  plotly::plot_ly(x = ~date,
                  y = ~cases,
                  name = 'Active',
                  fillcolor = '#1f77b4',
                  type = 'scatter',
                  mode = 'none',
                  stackgroup = 'one') %>%
  plotly::layout(title = "Distribution of Covid19 Cases Korea, South",
                 legend = list(x = 0.1, y = 0.9),
                 yaxis = list(title = "Number of Confirmed"),
                 xaxis = list(title = "Date"))

# html로 내보내기
saveWidget(as.widget(poltlyData), "fig.html", selfcontained = FALSE)

# 내보낸 html 페이지로부터 png 형태로 불러와서 저장
webshot::webshot("fig.html", saveImg, vwidth = 1000, vheight = 600, cliprect = "viewport")


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
# 1) 스페인 바르셀로나 시내 우편번호(Zip code) 별 택배배송량(Order)을 히트맵으로 
# 시각화하고 싶습니다. 
# 첨부 엑셀 Sheet1의 데이타를 시각화는 것인데 예를 들어 첫행의 8025는 
# 실제로 Zip code 08025 이고 우편 구글맵 캡쳐한 곳에 표시된 지역입니다. 
# 해당지역이 가장 Order가 많은 지역이니 가장 진한 색 (불투명남색)이 되게 해주시면 됩니다.

# 엑셀로 해보려니까 zip code를 제대로 못 잡아서 방법이 없더라구요.
# 바르셀로나 소재 택배회사의 운영현황을 보면서 도심 내 미니창고를 지어야 하는지 
# 여부를 판단해보려고 합니다.
# 첫번째는 그 중에서 구역별(zip code 기준) 으로 어느 구역에 배송이 제일 많은지
# 히트맵으로 표시하려고 합니다. 첫번째 엑셀에 캡쳐해서 넣어놓은 
# 그림처럼 실제구역을 구분해서 색깔을 넣으면 (빨간색으로 표시된 테두리를 살려서 
# 안에 색깔 삽입) 너무 좋을 것 같은데 시간이 오래 걸리면 그냥 원크기와 색깔로 
# 표시해도 될 것 같습니다.

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0143"

#================================================
# Main
#================================================
library(stringr)
library(openxlsx)
library(MASS)
library(showtext)
library(quantreg)
library(ggplot2)
library(tidyverse)
library(Metrics)
library(vcd)
library(coronavirus)
library(plotly)
library(ggpubr)
library(processx)
library(webshot)
library(htmlwidgets)
library(zipcode)

# showtext::showtext_opts(dpi = 100)
# showtext::showtext.auto()

cbMatlab = colorRamps::matlab.like(11)

# fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0143_통합+문서2.xlsx", sep = "/"))
fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0143_LSH0143_통합+문서2.xlsx", sep = "/"))
data = openxlsx::read.xlsx(fileInfo, sheet = 1) %>%
  dplyr::rename(
    sumOrder = Sum.of.order
  )

dataL1 = na.omit(data) %>%
  dplyr::select(longitude, latitude, sumOrder)

# mapGlobal = sf::st_read(paste(globalVar$mapPath, "gshhg-shp-2.3.6/GSHHS_shp/i/GSHHS_i_L1.shp", sep = "/"), quiet = TRUE)

# ggplot(mapGlobal) +
#   geom_sf()


# shore = maptools::getRgshhsMap(
#   paste(globalVar$mapPath, "gshhg-bin-2.3.0/gshhs_f.b", sep = "/")
#   , xlim = c(min(dataL2$xAxis, na.rm = TRUE), max(dataL2$xAxis, na.rm = TRUE))
#   , ylim = c(min(dataL2$yAxis, na.rm = TRUE), max(dataL2$yAxis, na.rm = TRUE))
#   ) %>% 
#   fortify()

# xRange = as.numeric(c(90, 150))
# yRange = as.numeric(c(10, 60))
# newLon = seq(from = xRange[1], to = xRange[2], by = 0.1)
# newLat = seq(from = yRange[1], to = yRange[2], by = 0.1)

# newLon = seq(from = min(dataL1$longitude, na.rm = TRUE), to = max(dataL1$longitude, na.rm = TRUE), by = 0.005)
# newLat = seq(from = min(dataL1$latitude, na.rm = TRUE), to = max(dataL1$latitude, na.rm = TRUE), by = 0.005)
# 
# gridData = noncompliance::expand.grid.DT(
#   newLon
#   , newLat
#   , col.names = c("lon", "lat")
# )
# 
# dataL2 = MBA::mba.points(dataL1, gridData) %>%
#   as.data.frame() %>%
#   as.tibble() %>%
#   dplyr::rename(
#     xAxis = xyz.est.x
#     , yAxis = xyz.est.y
#     , zAxis = xyz.est.z
#   )


map = ggmap::get_map(
  location = c(lon = mean(dataL1$longitude, na.rm = TRUE), lat = mean(dataL1$latitude, na.rm = TRUE))
  , zoom = 12
)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "PostalCode")

ggmap(map, extent = "device") +
  # geom_point(data = dataL1, aes(x = longitude, y = latitude, colours = sumOrder)) +
  # geom_tile(data = dataL2, aes(x = xAxis, y = yAxis, fill = zAxis, alpha = 0.3), show.legend = FALSE) +
  geom_point(data = dataL1, aes(x = longitude, y = latitude, color = sumOrder, size = sumOrder, alpha = 0.3)) +
  scale_fill_gradientn(colours = cbMatlab, na.value = NA) +
  scale_color_gradientn(colours = cbMatlab, na.value = NA) +

  labs(
    subtitle = NULL
    , x = NULL
    , y = NULL
    , fill = NULL
    , colour = NULL
    , title = NULL
    , size = NULL
  ) +
  scale_alpha(guide = 'none') +
  scale_size(range = c(5, 20)) +
  # scale_size_discrete(
  #   "sumOrder"
  #   , range = c(0, 10)
  #   # , labels = c("0", "B", "G", "J", "REF")
  # ) +
  theme(
    text = element_text(size = 18)
    # , legend.position = c(1, 1)
    # , legend.justification = c(1, 1)
    # , legend.background = element_rect(fill = "transparent")
    # , legend.box.background = element_rect(fill = "transparent")
  ) +
  ggsave(filename = saveImg, width = 10, height = 10, dpi = 600)


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
# 하노이의 탑 문제를 R 코드로 작성하는 문제이고 3개의 원판 및 4개의 기둥 문제인데요.
# 혹시 이것도 가능할까요?

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0144"

#================================================
# Main
#================================================

#*****************************************
# 가정
#*****************************************
# 네개의 기둥과 서로 다른 크기인 n개의 원반
# 원반들은 세개의 기둥중 하나에 꽂혀 있어야 함
# 자신보다 작은 원반 위에 그 원반을 놓을 수 없다.

#*****************************************
# 구현
#*****************************************
# 기둥 : A, D, B, C (from, to, by, by)
# 원반 개수 : N
# 기둥 1에서 N개의 원반을 기둥 2를 이용하여 기둥 3으로 옮기는 알고리즘
hanoiPos4 = function(N, A, D, B, C) {
  if (N == 0) {
    cat("\n")
  } else if (N == 1) {
    # 실제로 옮김
    cat("move disk ", N, " from", A, "to", D, "\n")
  } else {

    # 기둥 A에서 N-2개의 원반을 기둥 B, C를 이용하여 기둥 D로 옮김
    hanoiPos4(N - 2, "A", "B", "C", "D")

    # 기둥 A에서 N-2개의 원반을 기둥 C로 옮김
    cat("move disk ", (N - 1), " from", A, "to", C, "\n")

    # 기둥 A에서 N개의 원반을 기둥 D로 옮김
    cat("move disk ", (N), " from", A, "to", D, "\n")

    # 기둥 C에서 N-1개의 원반을 기둥 D로 옮김
    cat("move disk ", (N - 1), " from", C, "to", D, "\n")

    # 기둥 B에서 N-2개의 원반을 기둥 A, C를 이용하여 기둥 D로 옮김
    hanoiPos4(N - 2, "B", "D", "A", "C")
  }
}

# 원반 3개를 통해 기둥 A에서 D로 이동
hanoiPos4(3, "A", "D", "B", "C")

# 원반 5개를 통해 기둥 A에서 D로 이동
# hanoiPos4(5, "A", "D", "B", "C")


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
# 안녕하세요~
# 지난번에 요청드린 그래프와 비슷한 작업인데요
# 결과 값 그대로 그래프 그려주시면 되어요~

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0145"

#================================================
# Main
#================================================
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggplot2)
library(lubridate)
library(MASS)
library(scales)

fileList = Sys.glob(paste(globalVar$inpPath, "LSH0145_*.xlsx", sep = "/"))

# fileInfo = "E:/04. TalentPlatform/Github/TalentPlatform-R/resources/input/test/LSH0145_CS_GR.xlsx"
# fileInfo = "E:/04. TalentPlatform/Github/TalentPlatform-R/resources/input/test/LSH0145_FR.xlsx"

for (fileInfo in fileList) {
  data = openxlsx::read.xlsx(xlsxFile = fileInfo, sheet = 1)
  # data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))

  fileName = tools::file_path_sans_ext(fs::path_file(fileInfo))

  dataL1 = data %>%
    dplyr::rename(
      x = 'X축'
      , y = 'Y축'
      , shapeType = 'Shape_Site.type'
      , colorClass = 'Color_Classification'
    ) %>%
    dplyr::select(x, y, shapeType, colorClass) %>%
    na.omit()

  dataL2 = dataL1 %>%
    # dplyr::bind_rows(refData) %>% 
    dplyr::mutate(
      makeColor = dplyr::case_when(
        stringr::str_detect(colorClass, regex("China")) ~ 1
        , stringr::str_detect(colorClass, regex("Korea")) ~ 2
        , stringr::str_detect(colorClass, regex("South")) ~ 3
        , stringr::str_detect(colorClass, regex("Europe")) ~ 4
        , stringr::str_detect(colorClass, regex("North")) ~ 5
        , stringr::str_detect(colorClass, regex("Antarctica")) ~ 6
        , TRUE ~ 1
      )
      , makeShape = dplyr::case_when(
        stringr::str_detect(shapeType, regex("Urban")) ~ 1 # 동그라미
        , stringr::str_detect(shapeType, regex("Rural")) ~ 2 # 사각형
        , stringr::str_detect(shapeType, regex("Mountain")) ~ 3 # 마름모
        , stringr::str_detect(shapeType, regex("Coast")) ~ 4 # 삼각형
        , stringr::str_detect(shapeType, regex("Remote")) ~ 5 # 뒤집힌 삼각형
        , TRUE ~ 21

      )
    )

  saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, fileName, "모양 및 색깔에 따른 산점도")

  ggplot(dataL2, aes(x = x, y = y, color = factor(makeColor), shape = factor(makeShape))) +
    # geom_point(size = 7, color = "transparent", show.legend = FALSE) + 
    geom_point(size = 7, fill = "transparent", show.legend = FALSE, stroke = 3) +
    # geom_errorbarh(aes(xmin = xmin, xmax = xmax), height = 0.10) +
    # geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.05) +
    scale_color_manual(values = c("1" = "orange", "2" = "red", "3" = "violet", "4" = "blue", "5" = "darkgreen", "6" = "grey"), name = NULL, na.value = NA
      , labels = c("China", "Korea", "South Asia", "Europe", "North America", "Antarctica / Arctic")
    ) +
    # scale_fill_manual(values = c("1" = "orange", "2" = "magenta", "3" = "violet", "4" = "blue", "5" = "green", "6" = "grey"), name = NULL, na.value = NA
    #                   , labels = c("China", "Korea", "South Asia", "Europe", "North America", "Antarctica / Arctic")
    # ) +
    scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
    # annotation_logticks(outside = TRUE) +
    annotation_logticks(size = 1.2, tick.length = unit(20 / 2, "pt"), minor.length = unit(20 / 3, "pt")) +
    # annotation_logticks() +
    labs(title = NULL, x = bquote('CS (' * s^-1 * ')'), y = bquote('' * H[2] * SO[4] * ' concentration (' * cm^-3 * ')'), colour = NULL, fill = NULL, subtitle = NULL) +
    theme_bw() +
    theme(
      text = element_text(size = 18)
      , legend.position = "bottom"
      , plot.margin = unit(c(4, 4, 0, 0), "mm")
      , panel.grid.major.x = element_blank()
      , panel.grid.major.y = element_blank()
      , panel.grid.minor.x = element_blank()
      , panel.grid.minor.y = element_blank()
      , panel.border = element_rect(size = 2)
      , axis.ticks.length = unit(20 / 2, "pt")
      , axis.ticks = element_line(size = 1.2)
      # , axis.ticks.x = element_blank()
      # , axis.ticks.y = element_blank()
      # , panel.background = element_blank()
      # , axis.line.x=element_line(colour="black")
      # , axis.line.y=element_line(colour="black")
      # , axis.ticks.margin = unit(1, "cm")
      # , axis.ticks = element_line(size = 1)
      # , axis.ticks.x = element_line(size = 1)
      # , axis.ticks.y = element_line(size = 1)
      # , axis.text.x = element_text(margin = unit(c(0.4, 0, 0, 0), "cm"))
      # , axis.text.y = element_text(margin = unit(c(0, 0.4, 0, 0), "cm"))
    ) +
    ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

  saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, fileName, "모양 및 색깔에 따른 산점도2")

  ggplot(dataL2, aes(x = x, y = y, color = factor(makeColor), fill = factor(makeColor), shape = factor(makeShape))) +
    geom_point(size = 7, show.legend = FALSE) +
    # geom_point(size = 7, fill = "transparent", show.legend = FALSE, stroke = 3) + 
    # geom_errorbarh(aes(xmin = xmin, xmax = xmax), height = 0.10) +
    # geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.05) +
    scale_color_manual(values = c("1" = "orange", "2" = "red", "3" = "violet", "4" = "blue", "5" = "darkgreen", "6" = "grey"), name = NULL, na.value = NA
      , labels = c("China", "Korea", "South Asia", "Europe", "North America", "Antarctica / Arctic")
    ) +
    scale_fill_manual(values = c("1" = "orange", "2" = "red", "3" = "violet", "4" = "blue", "5" = "darkgreen", "6" = "grey"), name = NULL, na.value = NA
      , labels = c("China", "Korea", "South Asia", "Europe", "North America", "Antarctica / Arctic")
    ) +
    scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
    # annotation_logticks(outside = TRUE) +
    annotation_logticks(size = 1.2, tick.length = unit(20 / 2, "pt"), minor.length = unit(20 / 3, "pt")) +
    # annotation_logticks() +
    labs(title = NULL, x = bquote('CS (' * s^-1 * ')'), y = bquote('' * H[2] * SO[4] * ' concentration (' * cm^-3 * ')'), colour = NULL, fill = NULL, subtitle = NULL) +
    theme_bw() +
    theme(
      text = element_text(size = 18)
      , legend.position = "bottom"
      , plot.margin = unit(c(4, 4, 0, 0), "mm")
      , panel.grid.major.x = element_blank()
      , panel.grid.major.y = element_blank()
      , panel.grid.minor.x = element_blank()
      , panel.grid.minor.y = element_blank()
      , panel.border = element_rect(size = 2)
      , axis.ticks.length = unit(20 / 2, "pt")
      , axis.ticks = element_line(size = 1.2)
      # , axis.ticks.x = element_blank()
      # , axis.ticks.y = element_blank()
      # , panel.background = element_blank()
      # , axis.line.x=element_line(colour="black")
      # , axis.line.y=element_line(colour="black")
      # , axis.ticks.margin = unit(1, "cm")
      # , axis.ticks = element_line(size = 1)
      # , axis.ticks.x = element_line(size = 1)
      # , axis.ticks.y = element_line(size = 1)
      # , axis.text.x = element_text(margin = unit(c(0.4, 0, 0, 0), "cm"))
      # , axis.text.y = element_text(margin = unit(c(0, 0.4, 0, 0), "cm"))
    ) +
    ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)
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

#================================================
# 요구사항
#================================================

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0146"

#================================================
# Main
#================================================
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggplot2)
library(lubridate)
library(MASS)
library(scales)
library(lubridate)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(data.table)
library(ggpubr)
library(forcats)

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0146_PAhourlyCHW.csv", sep = "/"))
# PAHourlyCHW <- read.csv(file = fileInfo, stringsAsFactors = TRUE)
PAHourlyCHW = data.table::fread(file = fileInfo)


ind = which(PAHourlyCHW$type2 == "office혻")
PAHourlyCHW[ind, "type2"] = "office"

PAHourlyCHW$weekday <- as.factor(PAHourlyCHW$weekday)
PAHourlyCHW$type1 <- as.factor(PAHourlyCHW$type1)
PAHourlyCHW$type2 <- as.factor(PAHourlyCHW$type2)

# 변수 다름 PAhourlyCHW > PAHourlyCHW
trainCHW_index <- sample(1:nrow(PAHourlyCHW), 0.8 * nrow(PAHourlyCHW))
testCHW_index <- setdiff(1:nrow(PAHourlyCHW), trainCHW_index)
# Build X_train, y_train, X_test, y_test
trainCHW <- PAHourlyCHW[trainCHW_index,]
testCHW <- PAHourlyCHW[testCHW_index,]

rm(trainCHW_index)
rm(testCHW_index)


# #밑에 lmCHW82type2가 polyline 을 그리려는 식입니다.
# lmCHW82type2<-lm(CHWEUI~factor(type2, exclude=c('Food','Health'))+poly(Height,2)+poly(Temp,2),data=trainCHW)
# summary(lmCHW82type2)#0.3568 

#*********************************************
# 3번 답변 
#*********************************************
# Levels: Education Food Health  Lab Lodge office? public
# trainCHW$type2 %>% unique() %>% sort()

# Food, Health를 제외한 자료 필터
trainCHWL1 = trainCHW %>%
  dplyr::filter(
    !as.numeric(type2) %in% c(2, 3)
  ) %>%
  dplyr::mutate(
    makeLegend = dplyr::case_when(
      stringr::str_detect(type2, regex("Education")) ~ "Education"
      , stringr::str_detect(type2, regex("Lab")) ~ "Lab"
      , stringr::str_detect(type2, regex("Lodge")) ~ "Lodging"
      , stringr::str_detect(type2, regex("office")) ~ "Office"
      , stringr::str_detect(type2, regex("public")) ~ "Public Assembly"
      , TRUE ~ "NA"
    )
  )

# type2에 대한 factor로 재 갱신
publictrainCHWL1$type2 = factor(trainCHWL1$type2)

# Levels: Education Lab Lodge office? 
# trainCHWL1$type2 %>% unique() %>% sort()

lmCHW82type2 = lm(CHWEUI ~ type2 + poly(Height, 2) + poly(Temp, 2), data = trainCHWL1)
summary(lmCHW82type2)

#*********************************************
# 4번 답변 
#*********************************************
meanHeight = mean(trainCHW$Height, na.rm = TRUE)
maenTemp = mean(trainCHW$Temp, na.rm = TRUE)

inData = data.frame(
  type2 = "Lab"
  , Temp = maenTemp
  , Height = meanHeight
)

predict(lmCHW82type2, newdata = inData)

inData = data.frame(
  type2 = "office"
  , Temp = maenTemp
  , Height = meanHeight
)

predict(lmCHW82type2, newdata = inData)



# 예측 결과 확인
y = trainCHWL1$CHWEUI
yHat = predict(lmCHW82type2)

cor(y, yHat)

# #이 부분은  워드파일 4번에 해당합니다. 
# Height = mean(trainCHW$Height) 
# Temp=mean(trainCHW$Temp) 
# coeffCHW = coefficients(lmCHW82type2); coeffCHW #http://www.r-tutor.com/elementary-statistics/simple-linear-regression/estimated-simple-regression-equation
# #Health일 경우 가정(워드 파일에 있는 summary 토대로 식 만든것임)
# chilled = coeffCHW[1] + coeffCHW[2] + coeffCHW[6]*Height+coeffCHW[7]*Height^2+ coeffCHW[8]*Temp+ coeffCHW[9]*Temp^2
# chilled#20536633   
# mean(trainCHW$CHWEUI)#12.9688

#*********************************************
# 1, 2번 답변 
#*********************************************
# #type 2 가 building type 입니다. Type2에 해당하는 항목 의미는 다음과 같습니다. 
# health:Medical, Lab:Lab, Education:Education, Food:Food Service, public:Public Assembly, Lodge:Lodging, officeA:Office
# #X-축 Temeprature(°C) Y:Chilled water consumption (kEUI) 
# 범례에 Lab, Lodging, Office, Public Assembly

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "산점도")

trainCHWL1$makeLegend = forcats::fct_relevel(trainCHWL1$makeLegend, c("Education", "Lab", "Lodging", "Office", "Public Assembly"))

lmFormula = y ~ poly(x, 2, raw = TRUE)

ggplot(trainCHWL1, aes(x = Temp, y = CHWEUI, color = makeLegend)) +
  geom_point(size = 3, alpha = 0.1) +
  geom_smooth(method = 'lm', formula = lmFormula, se = TRUE) +
  ggpubr::stat_regline_equation(aes(label = ..eq.label..), formula = lmFormula, parse = TRUE, label.x.npc = 0.0, label.y.npc = 1.0) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.80) +
  labs(
    x = "Temeprature(°C)"
    , y = "Chilled water consumption (kEUI)"
    , color = NULL
    , fill = NULL
    , subtitle = NULL
  ) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "산점도2")
# 
# ggpubr::ggscatter(
#   trainCHWL1, x = "Temp", y = "CHWEUI", color = "type2"
#   , add = "reg.line", conf.int = TRUE, scales = "free_x"
#   , palette = "jco", facet.by = "type2"
#   , add.params = list(color = "black", fill = "lightgray")
#   ) +
#   ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 0.95) +
#   ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.85) +
#   theme(text = element_text(size = 14)) +
#   ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)

# #이 부분은  워드파일 1,2번에 해당합니다. 
# plot(trainCHW$CHWEUI~trainCHW$Temp, col=as.integer(trainCHW$type2))#x축을 Temperature가 아니라 index (poly(Height,2)+poly(Temp,2))로 할 수 있는 방법이 있는지 궁금합니다 (다음번 미팅에 말씀해주셔도 되고 여기 코딩에 적어주셔도 됩니다.)
# #여기서부터는 제가 시도해봤지만 안된 PLOTTING 부분입니다. 
# 굳이 수정하실 필요없고 그냥 원하시는 ggplot으로 하시면 됩니다. 
# #type 2 가 building type 입니다. Type2에 해당하는 항목 의미는 다음과 같습니다. health:Medical, Lab:Lab, Education:Education, Food:Food Service, public:Public Assembly, Lodge:Lodging, officeA:Office
# #X-축 Temeprature(°C) Y:Chilled water consumption (kEUI) 범례에 Lab, Lodging, Office, Public Assembly
# myPredict <- predict( lmCHW82type2 ) 
# ix <- sort(Temp,index.return=T)$ix
# lines(Temp[ix], myPredict[ix], col=2, lwd=2 )  
# 
# chwplot<-
#   ggplot(trainCHW, aes(CHWEUI,Temp))+ geom_abline(intercept = 37, slope = -5)
# ggplot(trainCHW, aes(CHWEUI, Temp, colour = type2)) +
#   geom_point() +
#   geom_hline(aes(yintercept = type2, colour = type2), mean_type2)
# 
# plot(trainCHW$CHWEUI, trainCHW$CDD)
# plot(trainCHW$CHWEUI, trainCHW$Temp)
# abline(Lab, col='green')
# names(trainCHW)

# Basic scatter plot.
# p1 <- ggplot(trainCHW, aes(x=CDD, y=CHWEUI)) + 
#   geom_point( color="#69b3a2")
# 
# Plot.HandPick<-ggplot(data=trainCHW, aes(x=factor(type2, exclude=c('Food','Health'))+poly(Height,2)+poly(CDD,2), y=CHWEUI, group=type2))+
#   geom_line(size=2, aes(color=IQ))+
#   ylim(0,4)+
#   ylab("GPA")+
#   xlab("Work Ethic")+
#   ggtitle("Hand Picked Plot")

#이 부분은  워드파일 3번에 해당합니다. 
# lmCHW82type2<-lm(CHWEUI~factor(type2, exclude=c('Food','Health'))+poly(Height,2)+poly(Temp,2),data=trainCHW)#'Lab', 
# summary(lmCHW82type2)


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
# R을 이용한 관측소 정보에 따른 크리킹 (KRIGE) 계산 및 병렬 처리

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0147"

#================================================
# Main
#================================================
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(colorRamps)
library(lubridate)
library(extrafont)
library(ggrepel)
library(scales)
library(sf)
unloadNamespace('raster')
library(gstat)
library(sp)
library(metR)
library(akima)
library(stringr)
library(automap)

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0147_관측소+제원.xlsx", sep = "/"))
stationData = openxlsx::read.xlsx(xlsxFile = fileInfo, sheet = "Sheet1") %>%
  dplyr::select(Observation, X, Y) %>%
  dplyr::rename(
    name = Observation
    , lon = X
    , lat = Y
  )

# 관측소에 대한 공간 격자화
spNewData = stationData
coordinates(spNewData) = ~ lon + lat
# plot(spNewData)

fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0147_Pr ACCESS-ESM1-5_1985-2014.csv", sep = "/"))
data = readr::read_csv(file = fileInfo2, locale = locale("ko", encoding = "EUC-KR"))

dtDateList = data$X1 %>% unique() %>% sort()

# [시작] 병렬 처리
oSocClu = parallel::makePSOCKcluster(parallel::detectCores())
# oSocClu = parallel::makePSOCKcluster(100)
doParallel::registerDoParallel(oSocClu)

# 외부 변수 등록
parallel::clusterExport(oSocClu, "dtDateList")
parallel::clusterExport(oSocClu, "data")
parallel::clusterExport(oSocClu, "dataL1")
parallel::clusterExport(oSocClu, "saveFile")
parallel::clusterExport(oSocClu, "globalVar")
parallel::clusterExport(oSocClu, "serviceName")
parallel::clusterExport(oSocClu, "spData")
parallel::clusterExport(oSocClu, "spNewData")
parallel::clusterExport(oSocClu, "stationData")
parallel::clusterExport(oSocClu, "dataL3")


# 외부 라이브러리 등록
parallel::clusterEvalQ(oSocClu, library(gstat))
parallel::clusterEvalQ(oSocClu, library(readr))
parallel::clusterEvalQ(oSocClu, library(tidyverse))
parallel::clusterEvalQ(oSocClu, library(sp))
parallel::clusterEvalQ(oSocClu, library(automap))

# x = 1
tictoc::tic()
# parallel::parSapply(oSocClu, X = 1:length(dtDateList), function(x) {
parallel::parSapply(oSocClu, X = 1:12, function(x) {
  
  dataL1 = data %>%
    dplyr::filter(X1 == dtDateList[x]) %>%
    tidyr::gather(-X1, key = "key", value = "val") %>%
    tidyr::separate(col = "key", into = c("lat", "lon"), sep = "p") %>%
    # tidyr::separate(col = "tmpLon", into = c(NA, "lon"), sep = "X") %>%
    readr::type_convert()
  
  spData = dataL1
  sp::coordinates(spData) = ~ lon + lat
  gridded(spData) = TRUE
  
  if (nrow(dataL1) < 1) { next }
  
  # variogram model
  variogram = automap::autofitVariogram(val ~ 1, spData)
  # plot(variogram)
  
  # 정규 크리킹 및 전처리 수행
  spDataL1 = gstat::krige(
    formula = val ~ 1
    , locations = spData
    , newdata = spNewData
    , model = variogram$var_model
    , nmax = 4
  ) %>%
    as.data.frame() %>%
    dplyr::rename(
      val = var1.pred
    ) %>%
    dplyr::select(-var1.var, -lon, -lat)
    # dplyr::select(-var1.var)
  
  # 데이터 병합
  dataL3 = data.frame(
    date = dtDateList[x]
    , name = stationData$name
    , spDataL1
  ) %>% 
  tidyr::spread(key = "name", value = "val")
  
  saveFile = sprintf("%s/%s_%s_%s.csv", globalVar$outPath, serviceName, "obs-to-krige", dtDateList[x])
  readr::write_csv(x = dataL3, file = saveFile)
})

tictoc::toc()

# [종료] 병렬 처리
parallel::stopCluster(oSocClu)

# 데이터 병합
fileList = Sys.glob(paste(globalVar$outPath, "LSH0147_obs-to-krige_*.csv", sep = "/"))

dataL4 = fileList %>%
  # purrr::map(read.csv) %>%
  purrr::map(~ read.csv(.x, fileEncoding = "UTF-8")) %>% 
  purrr::reduce(dplyr::bind_rows)

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "obs-to-krige")
readr::write_csv(x = dataL4, file = saveFile)


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
# R을 이용한 관측소 정보에 따른 역거리 가중치 (IDW) 계산 및 병렬 처리

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0148"

#================================================
# Main
#================================================
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)
library(colorRamps)
library(lubridate)
library(extrafont)
library(ggrepel)
library(scales)
library(sf)
unloadNamespace('raster')
library(gstat)
library(sp)
library(metR)
library(akima)
library(stringr)

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0148_관측소+제원.xlsx", sep = "/"))
stationData = openxlsx::read.xlsx(xlsxFile = fileInfo, sheet = "Sheet1") %>%
  dplyr::select(Observation, X, Y) %>%
  dplyr::rename(
    name = Observation
    , lon = X
    , lat = Y
  )

# 관측소에 대한 공간 격자화
spNewData = stationData
coordinates(spNewData) = ~ lon + lat
# plot(spNewData)

fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0148_Pr ACCESS-ESM1-5_1985-2014.csv", sep = "/"))
data = readr::read_csv(file = fileInfo2, locale = locale("ko", encoding = "EUC-KR"))

dtDateList = data$X1 %>% unique() %>% sort()

# [시작] 병렬 처리
oSocClu = parallel::makePSOCKcluster(parallel::detectCores())
# oSocClu = parallel::makePSOCKcluster(100)
doParallel::registerDoParallel(oSocClu)

# 외부 변수 등록
parallel::clusterExport(oSocClu, "dtDateList")
parallel::clusterExport(oSocClu, "data")
parallel::clusterExport(oSocClu, "dataL1")
parallel::clusterExport(oSocClu, "saveFile")
parallel::clusterExport(oSocClu, "globalVar")
parallel::clusterExport(oSocClu, "serviceName")
parallel::clusterExport(oSocClu, "spData")
parallel::clusterExport(oSocClu, "spNewData")
parallel::clusterExport(oSocClu, "stationData")
parallel::clusterExport(oSocClu, "dataL3")


# 외부 라이브러리 등록
parallel::clusterEvalQ(oSocClu, library(gstat))
parallel::clusterEvalQ(oSocClu, library(readr))
parallel::clusterEvalQ(oSocClu, library(tidyverse))
parallel::clusterEvalQ(oSocClu, library(sp))

# x = 1
tictoc::tic()
# parallel::parSapply(oSocClu, X = 1:length(dtDateList), function(x) {
parallel::parSapply(oSocClu, X = 1:12, function(x) {
  
  dataL1 = data %>%
    dplyr::filter(X1 == dtDateList[x]) %>%
    tidyr::gather(-X1, key = "key", value = "val") %>%
    tidyr::separate(col = "key", into = c("lat", "lon"), sep = "p") %>%
    # tidyr::separate(col = "tmpLon", into = c(NA, "lon"), sep = "X") %>%
    readr::type_convert()
  
  spData = dataL1
  sp::coordinates(spData) = ~ lon + lat
  gridded(spData) = TRUE
  
  if (nrow(dataL1) < 1) { next }
  
  # IDW 학습 및 전처리수행
  spDataL1 = gstat::idw(
    formula = val ~ 1
    , locations = spData
    , newdata = spNewData
    , nmax = 4
  ) %>%
    as.data.frame() %>%
    dplyr::rename(
      val = var1.pred
    ) %>%
    dplyr::select(-var1.var, -lon, -lat)
  
  # 데이터 병합
  dataL3 = data.frame(
    date = dtDateList[x]
    , name = stationData$name
    , spDataL1
  ) %>% 
    tidyr::spread(key = "name", value = "val")
  
  saveFile = sprintf("%s/%s_%s_%s.csv", globalVar$outPath, serviceName, "obs-to-idw", dtDateList[x])
  readr::write_csv(x = dataL3, file = saveFile)
})

tictoc::toc()

# [종료] 병렬 처리
parallel::stopCluster(oSocClu)

# 데이터 병합
fileList = Sys.glob(paste(globalVar$outPath, "LSH0148_obs-to-idw_*.csv", sep = "/"))

dataL4 = fileList %>%
  purrr::map(~ read.csv(.x, fileEncoding = "UTF-8")) %>% 
  purrr::reduce(dplyr::bind_rows)

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "obs-to-idw")
readr::write_csv(x = dataL4, file = saveFile)


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
# R을 이용한 다중선형회귀모형 예측 및 분석

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0159"

#================================================
# Main
#================================================
library(ggplot2)
library(tidyverse)
library(caret)
library(readr)
library(Metrics)
library(car)
library(olsrr)
library(leaps)

# showtext::showtext_opts(dpi = 100)
# showtext::showtext.auto()
# 
fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0159_insurance.csv", sep = "/"))
data = readr::read_csv(file = fileInfo)

#**********************************************
# 데이터셋 확인
#**********************************************

# 요약 정보 보기
dplyr::tbl_df(data)

# 기초 통계량 보기
summary(data)

# 건강 보험료에 대한 상자 그림
boxplot(data$charges)

# 나이에 대한 히스토그램
hist(data$age)

# 건강 보험료가 최대인 행 정보
data[which.max(data$charges), ]

# 범주형 변수 처리
data$sex = as.factor(data$sex)
data$smoker = as.factor(data$smoker)
data$region = as.factor(data$region)


#**********************************************
# 회귀모형
#**********************************************
# 초기 모형 설정
# 독립변수 : 건강 보험료를 제외한 모든 변수
# 종속 변수 : 건강 보험료
fit = lm(charges ~ ., data = data)

#+++++++++++++++++++++++++++++++++++++
# 회귀 분석 결과
#+++++++++++++++++++++++++++++++++++++
# 수혜 나이가 많을수록 보험료는 254.8로 증가한다.
# 남성의 경우 보험료는 -131.3로 감소한다.
# BMI 지수가 높을수록 보험료는 383.5로 증가한다.
# 수혜 자녀수가 많을수록 보험료는 475.5로 증가한다.
# 흡연자의 경우 보험료는 23848.5로 증가한다.
# 북서부 지역의 경우 -352.9로 감소한다.
# 남동부 지역의 경우 -1035.0로 감소한다.
# 남서부 지역의 경우 -960.0로 감소한다.
summary(fit)


par(mfrow = c(2, 2))
# 좌측 상단에서 Q-Q 그림에서 정규분포를 띠지 않음
plot(fit)

# 귀무가설 기각 > 정규분포를 띠지 않음
shapiro.test(rstandard(fit))


par(mfrow = c(1, 1))
car::qqPlot(fit, simulate=TRUE, main="Q-Q_plot")

olsrr::ols_plot_resid_lev(fit)

olsrr::ols_plot_cooksd_bar(fit)

apply(data, 2, median)

#**********************************************
# 수정된 회귀모형
#****************************** ****************
# 1차 수정모형 : AIC 가장 낮은 모델
# 독립변수 : 나이, BMI, 자녀, 흡연 Yes, 지역 (북서부, 남동부, 남서부)
# 종속 변수 : 건강 보험료
stepFit = step(fit, direction = 'both')

#+++++++++++++++++++++++++++++++++++++
# 회귀 분석 결과
#+++++++++++++++++++++++++++++++++++++
# 수혜 나이가 많을수록 보험료는 256.9로 증가한다.
# BMI 지수가 높을수록 보험료는 338.6로 증가한다.
# 수혜 자녀수가 많을수록 보험료는 474.5로 증가한다.
# 흡연자의 경우 보험료는 23836.3로 증가한다.
# 북서부 지역의 경우 -352.1로 감소한다.
# 남동부 지역의 경우 -1034.3로 감소한다.
# 남서부 지역의 경우 -959.3로 감소한다.

# 이러한 결과를 토대로 고령화 사회에 접어들면서 보험료 관련 사업이 활성화 가능
summary(stepFit)

# 좌측 상단에서 Q-Q 그림에서 정규분포를 띠지 않음
par(mfrow = c(2, 2))
plot(stepFit)

# 귀무가설 기각 > 정규분포를 띠지 않음
shapiro.test(rstandard(stepFit))

car::qqPlot(stepFit, simulate=TRUE, main="Q-Q_plot")

olsrr::ols_plot_resid_lev(stepFit)

olsrr::ols_plot_cooksd_bar(stepFit)


modelset = regsubsets(charges ~ .,data = data)
par(mfrow = c(1, 2))
plot(modelset, scale = "r2")
plot(modelset, scale = "adjr2")

plot(modelset, scale = "bic")
plot(modelset, scale = "Cp")

# 최종적인 모델 (stepFit)
fit1 = lm(charges ~ ., data = data) # 최초
fit2 = lm(charges ~ age + bmi + children + smoker + region, data = data) # step
summary(fit1)
summary(fit2)

anova(fit1, fit2)

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
# 1. 종속변수 선정
# 1.1 상권 자료의 범주 4개 설명 -> 광원님
# 1.2 LH인 지역의 특성을 분석해볼 지표 설명 -> 지영님
# 2. 독립변수 선정
# 2.1 서비스업 자료(business_type) -> 광원님
# 2.1.1 서비스업의 유형 분류(종사자수 1단위 증가 시 해당 업종의 종사자수의 변화 정도)
# 2.1.2 변수 설명: 소비자서비스는 시장에 신규진입하기 용이한 업종이기 때문에 많을 것.
# 2.2 사업체수 자료(business_number) -> 지영님
# 2.2.1 사업체수, 평균종사자(종사자수/사업체수), 종사자밀도비 설명: 종사자밀도비는 종사자밀도/인구밀도로, 해당 수치가
# 높으면 주간인구가 상주인구보다 높은 업무지구. -> 위에서 밝히지 못한 서비스업을 부차적으로 설명할 수 있을 것.
# 2.3 사업체 영업기간 자료(business_age) -> 지영님
# 2.3.1 변수 설명: 신규진입하는 사업자가 많으면 그동안 사업체가 영업한 기간이 짧을 것, 상대적으로 연령이 낮을 것.
# 2.4 사업체 창업률 자료(business_founding) -> 종호님
# 2.4.1 변수 설명: 신규진입하는 사업자가 많으면 창업률은 당연히 높을 것.
# 2.5 자영업 종사자수 자료(business_private) -> 종호님
# 2.5.1 변수 설명: 신규진입하는 사업자는 대부분 개인사업일 것이기 때문에 이들이 많으면 자영업 종사자수도 많을 것.
# 3. 회귀분석 실시
# 3.1 산점도를 그려 변수 조망하기 -> 광원님
# 3.1.1 유의해보이는 변수 파악
# 3.2 서비스업 자료(business_type) -> 지영님
# 3.2.1 유형별 종사자수 합산 후 종속변수와의 관계 관찰 -> 유의미 X
# 3.2.2 유형별 종사자수 비율 계산 후 종속변수와의 관계 관찰 -> 유의미 X(보정지수1로 하면 유의미)
# 3.3 사업체수 자료(business_number) -> 지영님
# 3.3.1 로그로 변환한 세 변수와 종속변수와의 관계 관찰 -> 종사자밀도비만 유의미
# 3.4 사업체 영업기간 자료(business_age) -> 종호님
# 3.4.1 변수와 종속변수와의 관계 관찰 -> 유의미
# 3.5 사업체 창업률 자료(business_founding) -> 종호님
# 3.5.1 변수와 종속변수와의 관계 관찰 -> 유의미
# 3.6 자영업 종사자수 자료(business_private) -> 종호님
# 3.6.1 로그로 변환하거나 하지 않은 변수 모두에 대해 종속변수와의 관계 관찰 -> 모두 유의미 X
# 4. 교호관계 분석 -> 광원님
# 4.1 소비자서비스업 종사자수와 종사자밀도비의 관계 -> 각 변수가 모두 유의미해짐.
# 4.2 사업체 영업기간, 창업률, 자영업 종사자수와의 관계 -> 자영업 종사자수 단독, 이 변수와 엮인 관계에서만 유의미.
# 4.3 종사자밀도비와 사업체 영업기간 간의 교호관계가 가장 유의미(설명력 14.42%),
# 종사자밀도비와 사업체 창업률 간의 교호관계는 종사자밀도비와 교호관계만 유의미(설명력 17.34%).
# 사업체 영업기간과 창업률 간의 교호관계는 창업률만 유의미(설명력 11.67%).
# 4.4 커피전문점업 종사자와 사업체 창업률
# 5. 모델 선정 -> 광원님
# 5.1 4.4의 모델을 가져와서 강남북 범주형 자료와 통합.
# 5.2 잔차 분석. okay -> 나은 모델! nokay -> 다시 보는 걸루!

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0161"

#================================================
# Main
#================================================
library(GGally)
library(gridExtra)
library(mosaic)
library(mosaicData)
library(reactable)
library(rmarkdown)
library(tidyverse)
library(ggpubr)

showtext::showtext_opts(dpi = 100)
showtext::showtext.auto()

# 종속변수 선정
## 상권 자료(market) 범주 설명_지영님

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0161_market.csv", sep = "/"))
market <- readr::read_csv(fileInfo, locale=locale(encoding="EUC-KR"))
paged_table(market)

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0161_code.csv", sep = "/"))
code <- readr::read_csv(fileInfo, locale=locale(encoding="EUC-KR")) %>%
  select(-코드_2) %>% rename("코드"="코드_1")

marche <- market %>% unite("연도_분기", 기준_년_코드, 기준_분기_코드) %>%
  filter(연도_분기 %in% "2019_4") %>%
  mutate(운영개월 = 서울_운영_영업_개월_평균 - 운영_영업_개월_평균,
             폐업개월 = -(서울_폐업_영업_개월_평균 - 폐업_영업_개월_평균),
             영업지수 = 운영개월 + 폐업개월) %>%
  select(-상권_변화_지표_명, -운영_영업_개월_평균, -폐업_영업_개월_평균,
         -서울_운영_영업_개월_평균, -서울_폐업_영업_개월_평균,
         -보정영업지수2, -보정영업지수3, -보정영업지수4) %>%
  rename("행정동"="행정동_코드_명", "코드"="행정동_코드", "보정영업지수"="보정영업지수1")

markt <- left_join(marche, code, by="코드") %>%
  select(-행정동.x) %>%
  unite("자치구_행정동", 구, 행정동.y) %>%
  relocate(연도_분기, 코드, 자치구_행정동, 상권_변화_지표, 운영개월, 폐업개월, 영업지수)
paged_table(markt)

  ## 영업지수 지표 설명_광원님
ggplot(markt, aes(x=운영개월, y=폐업개월))+
  geom_point(mapping=aes(color=상권_변화_지표))+
  scale_color_manual(values=c("palevioletred2", "lightgoldenrod", "seagreen3", "slateblue"))+
  theme_minimal()+theme(legend.position=c(0.9, 0.5))

einfach1 <- ggplot(data=markt, mapping=aes(x=reorder(자치구_행정동, 영업지수), y=영업지수, fill=상권_변화_지표))+
  scale_fill_manual(values=c("palevioletred2", "lightgoldenrod", "seagreen3", "slateblue"))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_blank())+
  theme(legend.position=c(0.85, 0.2))

einfach2 <- ggplot(data=markt, mapping=aes(x=reorder(자치구_행정동, 보정영업지수), y=보정영업지수, fill=상권_변화_지표))+
  scale_fill_manual(values=c("palevioletred2", "lightgoldenrod", "seagreen3", "slateblue"))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_blank())+
  theme(legend.position=c(0.85, 0.2))

  # 독립변수 설정
  ## 서비스업 유형 자료(business_type)_광원님
# type <- readr::read_csv("business_type.csv", locale=locale(encoding="EUC-KR"))
fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0161_business_type.csv", sep = "/"))
type <- readr::read_csv(fileInfo, locale=locale(encoding="EUC-KR"))
paged_table(type)

sorte <- type %>%
  filter(!(행정동 %in% c("행정동", "합계", "소계"))) %>%
  select(-사업체수_1, -종사자수_1, -종사자수_2) %>%
  unite("자치구_행정동", 구, 행정동) %>%
  rename("D.전기가스공급업_사업체수"="D. 전기 가스 증기 및 공기조절 공급업",
         "D.전기가스공급업_종사자수"="D. 전기 가스 증기 및 공기조절 공급업_1",
         "E.수도하수재생업_사업체수"="E. 수도 하수 및 폐기물 처리 원료 재생업",
         "E.수도하수재생업_종사자수"="E. 수도 하수 및 폐기물 처리 원료 재생업_1",
         "F.건설업_사업체수"="F. 건설업", "F.건설업_종사자수"="F. 건설업_1",
         "G.도소매업_사업체수"="G. 도매 및 소매업", "G.도소매업_종사자수"="G. 도매 및 소매업_1",
         "H.운수창고업_사업체수"="H. 운수 및 창고업", "H.운수창고업_종사자수"="H. 운수 및 창고업_1",
         "I.숙박요식업_사업체수"="I. 숙박 및 음식점업", "I.숙박요식업_종사자수"="I. 숙박 및 음식점업_1",
         "J.정보통신업_사업체수"="J. 정보통신업", "J.정보통신업_종사자수"="J. 정보통신업_1",
         "K.금융보험업_사업체수"="K. 금융 및 보험업", "K.금융보험업_종사자수"="K. 금융 및 보험업_1",
         "L.부동산업_사업체수"="L. 부동산업", "L.부동산업_종사자수"="L. 부동산업_1",
         "M.전문과학기술업_사업체수"="M. 전문 과학 및 기술 서비스업",
         "M.전문과학기술업_종사자수"="M. 전문 과학 및 기술 서비스업_1",
         "N.사업시설관리임대업_사업체수"="N. 사업시설 관리 사업 지원 및 임대 서비스업",
         "N.사업시설관리임대업_종사자수"="N. 사업시설 관리 사업 지원 및 임대 서비스업_1",
         "O.공공행정국방업_사업체수"="O. 공공행정 국방 및 사회보장 행정",
         "O.공공행정국방업_종사자수"="O. 공공행정 국방 및 사회보장 행정_1",
         "P.교육서비스업_사업체수"="P. 교육 서비스업", "P.교육서비스업_종사자수"="P. 교육 서비스업_1",
         "Q.보건사회복지서비스업_사업체수"="Q. 보건업 및 사회복지 서비스업",
         "Q.보건사회복지서비스업_종사자수"="Q. 보건업 및 사회복지 서비스업_1",
         "R.예술체육여가서비스업_사업체수"="R. 예술 스포츠 및 여가관련 서비스업",
         "R.예술체육여가서비스업_종사자수"="R. 예술 스포츠 및 여가관련 서비스업_1",
         "S.기타개인서비스업_사업체수"="S. 협회 및 단체 수리 및 기타 개인 서비스업",
         "S.기타개인서비스업_종사자수"="S. 협회 및 단체 수리 및 기타 개인 서비스업_1") %>%
  mutate(사업체수=str_replace(사업체수, ",", ""), 종사자수=str_replace(종사자수, ",", ""),
             D.전기가스공급업_사업체수=str_replace(D.전기가스공급업_사업체수, ",", ""),
             D.전기가스공급업_종사자수=str_replace(D.전기가스공급업_종사자수, ",", ""),
             E.수도하수재생업_사업체수=str_replace(E.수도하수재생업_사업체수, ",", ""),
             E.수도하수재생업_종사자수=str_replace(E.수도하수재생업_사업체수, ",", ""),
             F.건설업_사업체수=str_replace(F.건설업_사업체수, ",", ""),
             F.건설업_종사자수=str_replace(F.건설업_종사자수, ",", ""),
             G.도소매업_사업체수=str_replace(G.도소매업_사업체수, ",", ""),
             G.도소매업_종사자수=str_replace(G.도소매업_종사자수, ",", ""),
             H.운수창고업_사업체수=str_replace(H.운수창고업_사업체수, ",", ""),
             H.운수창고업_종사자수=str_replace(H.운수창고업_종사자수, ",", ""),
             I.숙박요식업_사업체수=str_replace(I.숙박요식업_사업체수, ",", ""),
             I.숙박요식업_종사자수=str_replace(I.숙박요식업_종사자수, ",", ""),
             J.정보통신업_사업체수=str_replace(J.정보통신업_사업체수, ",", ""),
             J.정보통신업_종사자수=str_replace(J.정보통신업_종사자수, ",", ""),
             K.금융보험업_사업체수=str_replace(K.금융보험업_사업체수, ",", ""),
             K.금융보험업_종사자수=str_replace(K.금융보험업_종사자수, ",", ""),
             L.부동산업_사업체수=str_replace(L.부동산업_사업체수, ",", ""),
             L.부동산업_종사자수=str_replace(L.부동산업_종사자수, ",", ""),
             M.전문과학기술업_사업체수=str_replace(M.전문과학기술업_사업체수, ",", ""),
             M.전문과학기술업_종사자수=str_replace(M.전문과학기술업_종사자수, ",", ""),
             N.사업시설관리임대업_사업체수=str_replace(N.사업시설관리임대업_사업체수, ",", ""),
             N.사업시설관리임대업_종사자수=str_replace(N.사업시설관리임대업_종사자수, ",", ""),
             O.공공행정국방업_사업체수=str_replace(O.공공행정국방업_사업체수, ",", ""),
             O.공공행정국방업_종사자수=str_replace(O.공공행정국방업_종사자수, ",", ""),
             P.교육서비스업_사업체수=str_replace(P.교육서비스업_사업체수, ",", ""),
             P.교육서비스업_종사자수=str_replace(P.교육서비스업_종사자수, ",", ""),
             Q.보건사회복지서비스업_사업체수=str_replace(Q.보건사회복지서비스업_사업체수, ",", ""),
             Q.보건사회복지서비스업_종사자수=str_replace(Q.보건사회복지서비스업_종사자수, ",", ""),
             R.예술체육여가서비스업_사업체수=str_replace(R.예술체육여가서비스업_사업체수, ",", ""),
             R.예술체육여가서비스업_종사자수=str_replace(R.예술체육여가서비스업_종사자수, ",", ""),
             S.기타개인서비스업_사업체수=str_replace(S.기타개인서비스업_사업체수, ",", ""),
             S.기타개인서비스업_종사자수=str_replace(S.기타개인서비스업_종사자수, ",", ""))

sorte$사업체수 <- as.numeric(sorte$사업체수)
sorte$종사자수 <- as.numeric(sorte$종사자수)
sorte$D.전기가스공급업_사업체수 <- as.numeric(sorte$D.전기가스공급업_사업체수)
sorte$D.전기가스공급업_종사자수 <- as.numeric(sorte$D.전기가스공급업_종사자수)
sorte$E.수도하수재생업_사업체수 <- as.numeric(sorte$E.수도하수재생업_사업체수)
sorte$E.수도하수재생업_종사자수 <- as.numeric(sorte$E.수도하수재생업_종사자수)
sorte$F.건설업_사업체수 <- as.numeric(sorte$F.건설업_사업체수)
sorte$F.건설업_종사자수 <- as.numeric(sorte$F.건설업_종사자수)
sorte$G.도소매업_사업체수 <- as.numeric(sorte$G.도소매업_사업체수)
sorte$G.도소매업_종사자수 <- as.numeric(sorte$G.도소매업_종사자수)
sorte$H.운수창고업_사업체수 <- as.numeric(sorte$H.운수창고업_사업체수)
sorte$H.운수창고업_종사자수 <- as.numeric(sorte$H.운수창고업_종사자수)
sorte$I.숙박요식업_사업체수 <- as.numeric(sorte$I.숙박요식업_사업체수)
sorte$I.숙박요식업_종사자수 <- as.numeric(sorte$I.숙박요식업_종사자수)
sorte$J.정보통신업_사업체수 <- as.numeric(sorte$J.정보통신업_사업체수)
sorte$J.정보통신업_종사자수 <- as.numeric(sorte$J.정보통신업_종사자수)
sorte$K.금융보험업_사업체수 <- as.numeric(sorte$K.금융보험업_사업체수)
sorte$K.금융보험업_종사자수 <- as.numeric(sorte$K.금융보험업_종사자수)
sorte$L.부동산업_사업체수 <- as.numeric(sorte$L.부동산업_사업체수)
sorte$L.부동산업_종사자수 <- as.numeric(sorte$L.부동산업_종사자수)
sorte$M.전문과학기술업_사업체수 <- as.numeric(sorte$M.전문과학기술업_사업체수)
sorte$M.전문과학기술업_종사자수 <- as.numeric(sorte$M.전문과학기술업_종사자수)
sorte$N.사업시설관리임대업_사업체수 <- as.numeric(sorte$N.사업시설관리임대업_사업체수)
sorte$N.사업시설관리임대업_종사자수 <- as.numeric(sorte$N.사업시설관리임대업_종사자수)
sorte$O.공공행정국방업_사업체수 <- as.numeric(sorte$O.공공행정국방업_사업체수)
sorte$O.공공행정국방업_종사자수 <- as.numeric(sorte$O.공공행정국방업_종사자수)
sorte$P.교육서비스업_사업체수 <- as.numeric(sorte$P.교육서비스업_사업체수)
sorte$P.교육서비스업_종사자수 <- as.numeric(sorte$P.교육서비스업_종사자수)
sorte$Q.보건사회복지서비스업_사업체수 <- as.numeric(sorte$Q.보건사회복지서비스업_사업체수)
sorte$Q.보건사회복지서비스업_종사자수 <- as.numeric(sorte$Q.보건사회복지서비스업_종사자수)
sorte$R.예술체육여가서비스업_사업체수 <- as.numeric(sorte$R.예술체육여가서비스업_사업체수)
sorte$R.예술체육여가서비스업_종사자수 <- as.numeric(sorte$R.예술체육여가서비스업_종사자수)
sorte$S.기타개인서비스업_사업체수 <- as.numeric(sorte$S.기타개인서비스업_사업체수)
sorte$S.기타개인서비스업_종사자수 <- as.numeric(sorte$S.기타개인서비스업_종사자수)

par(mfrow=c(1,3), cex=.5)

plot(log(sorte$종사자수), log(sorte$G.도소매업_종사자수+1), col="white", ylim=c(0,10.5),
     xlab="전체 종사자수", ylab="해당 업종 종사자수")
abline(lm(log(sorte$G.도소매업_종사자수+1)~log(sorte$종사자수)), col="orange")
abline(lm(log(sorte$I.숙박요식업_종사자수+1)~log(sorte$종사자수)), col="palegoldenrod")
abline(lm(log(sorte$L.부동산업_종사자수+1)~log(sorte$종사자수)), col="peachpuff2")
abline(lm(log(sorte$R.예술체육여가서비스업_종사자수+1)~log(sorte$종사자수)), col="yellow2")
abline(lm(log(sorte$S.기타개인서비스업_종사자수+1)~log(sorte$종사자수)), col="yellow4")
legend("topleft", legend=c("G.도소매업", "I.숙박요식업", "L.부동산업",
                           "R.예술체육여가서비스업", "S.기타개인서비스업"), 
       col=c("orange", "palegoldenrod", "peachpuff2", "yellow2", "yellow4"), lty=1)

plot(log(sorte$종사자수), log(sorte$M.전문과학기술업_종사자수+1), col="white", ylim=c(0,10.5),
     xlab="전체 종사자수", ylab="해당 업종 종사자수")
abline(lm(log(sorte$F.건설업_종사자수+1)~log(sorte$종사자수)), col="yellowgreen")
abline(lm(log(sorte$J.정보통신업_종사자수+1)~log(sorte$종사자수)), col="seagreen")
abline(lm(log(sorte$K.금융보험업_종사자수+1)~log(sorte$종사자수)), col="springgreen4")
abline(lm(log(sorte$M.전문과학기술업_종사자수+1)~log(sorte$종사자수)), col="turquoise")
abline(lm(log(sorte$N.사업시설관리임대업_종사자수+1)~log(sorte$종사자수)), col="skyblue2")
legend("topleft", legend=c("F.건설업", "J.정보통신업", "K.금융보험업",
                           "M.전문과학기술업", "N.사업시설관리임대업"), 
       col=c("yellowgreen", "seagreen", "springgreen4", "turquoise", "skyblue2"), lty=1)

plot(log(sorte$종사자수), log(sorte$P.교육서비스업_종사자수+1), col="white", ylim=c(0,10.5),
     xlab="전체 종사자수", ylab="해당 업종 종사자수")
abline(lm(log(sorte$D.전기가스공급업_종사자수+1)~log(sorte$종사자수)), col="lightpink")
abline(lm(log(sorte$E.수도하수재생업_종사자수+1)~log(sorte$종사자수)), col="lightcoral")
abline(lm(log(sorte$H.운수창고업_종사자수+1)~log(sorte$종사자수)), col="salmon2")
abline(lm(log(sorte$O.공공행정국방업_종사자수+1)~log(sorte$종사자수)), col="royalblue2")
abline(lm(log(sorte$P.교육서비스업_종사자수+1)~log(sorte$종사자수)), col="slateblue")
abline(lm(log(sorte$Q.보건사회복지서비스업_종사자수+1)~log(sorte$종사자수)), col="mediumorchid4")
legend("topleft", legend=c("D.전기가스공급업", "E.수도하수재생업", "H.운수창고업",
                           "O.공공행정국방업", "P.교육서비스업", "Q.보건사회복지서비스업"), 
       col=c("lightpink", "lightcoral", "salmon2", "royalblue2", "slateblue", "mediumorchid4"), lty=1)
  
  ## 사업체수와 종사자밀도비 자료(business_number)_지영님
  
  # <br>
  # <br>
  
  ## 사업체 영업기간 자료(business_age)_지영님
  
  # <br>
  # <br>
  
  ## 사업체 창업률 자료(business_founding)_종호님
# 2.4.1 변수 설명: 신규진입하는 사업자가 많으면 창업률은 당연히 높을 것.
# 서비스 증가 > 시장에 신규진입하기 용이한 업종 증가 > 신규진입하는 사용자 증가 > 창업률 증가

# 즉 생산자/소비자 서비스업 및 사업체 창업률는 각각 0.198 및 0.241의 양의 상관성을 지님
# 특히 생산자보다 소비자에 대한 관계성이 있음
delta %>% 
  dplyr::select(생산자서비스업, 생산자서비스업비율, 소비자서비스업, 소비자서비스업비율, 사업체_창업률) %>% 
  GGally::ggpairs(.) +
  theme(text = element_text(size = 18))

  # <br>
  # <br>
  
  ## 자영업 종사자수 자료(business_private)_종호님
# 2.5.1 변수 설명: 신규진입하는 사업자는 대부분 개인사업일 것이기 때문에 이들이 많으면 자영업 종사자수도 많을 것.
epsilon %>% 
  dplyr::select(생산자서비스업, 생산자서비스업비율, 소비자서비스업, 소비자서비스업비율, 자영업_종사자수) %>% 
  GGally::ggpairs(.) +
  theme(text = element_text(size = 18))


  # <br>
  # <br>
  ## 생계형사업 종사자수 자료(business_living)_지영님
  
  # <br>
  # <br>
  
  # 단순회귀분석 실시
  ## 산점도로 변수 조망_광원님
  # ![](./scatterplot.png)
  
  ## 서비스업 유형 자료_광원님
sorte[is.na(sorte)] <- 0

gattung <- sorte %>%
  mutate(소비자서비스업 = G.도소매업_종사자수 + I.숙박요식업_종사자수 +
                  L.부동산업_종사자수 + R.예술체육여가서비스업_종사자수 + S.기타개인서비스업_종사자수,
                생산자서비스업 = F.건설업_종사자수 + J.정보통신업_종사자수 +
                  K.금융보험업_종사자수 + M.전문과학기술업_종사자수 + N.사업시설관리임대업_종사자수,
                전체서비스업 = 소비자서비스업 + 생산자서비스업,
                소비자서비스업비율 = 소비자서비스업 / 전체서비스업,
                생산자서비스업비율 = 생산자서비스업 / 전체서비스업) %>%
  select(자치구_행정동, 종사자수, 소비자서비스업, 생산자서비스업, 소비자서비스업비율, 생산자서비스업비율)

alpha <- left_join(markt, gattung, by="자치구_행정동") %>% 
  mutate(보정영업지수1 = 보정영업지수 + 5) %>%
  select(-종사자수, -보정영업지수) %>%
  rename("보정영업지수"="보정영업지수1")
paged_table(alpha)

par(mfrow=c(1,2), cex=.6)

plot(alpha$소비자서비스업비율, log(alpha$보정영업지수),
     xlab="소비자서비스업 종사자비율", ylab="보정영업지수")
abline(lm(log(alpha$보정영업지수)~alpha$소비자서비스업비율), col="slateblue")

plot(alpha$생산자서비스업비율, log(alpha$보정영업지수),
     xlab="생산자서비스업 종사자비율", ylab="보정영업지수")
abline(lm(log(alpha$보정영업지수)~alpha$생산자서비스업비율), col="deeppink")

summary(lm(log(보정영업지수)~소비자서비스업비율, alpha))
summary(lm(log(보정영업지수)~생산자서비스업비율, alpha))
# ```
# <br>
#   <br>
  
  ## 사업체수 자료 정리_지영님
  # ```{r message=FALSE, warning=FALSE, paged.print=TRUE}
# number <- readr::read_csv("business_number.csv", locale=locale(encoding="EUC-KR"))
fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0161_business_number.csv", sep = "/"))
number <- readr::read_csv(fileInfo, locale=locale(encoding="EUC-KR")) 
paged_table(number)

nombre <- number %>%
  select(구, 행정동, 사업체수, 평균종사자, 종사자밀도비) %>%
  unite("자치구_행정동", 구, 행정동)

beta <- left_join(alpha, nombre, by="자치구_행정동")
paged_table(beta)
# ```

# ```{r message=FALSE, warning=FALSE, paged.print=TRUE}
par(mfrow=c(1,3), cex=.5)

plot(log(beta$사업체수), log(beta$보정영업지수),
     xlab="log(사업체수)", ylab="보정영업지수")
abline(lm(log(beta$보정영업지수)~log(beta$사업체수)), col="slateblue")

plot(log(beta$평균종사자), log(beta$보정영업지수),
     xlab="log(평균종사자)", ylab="보정영업지수")
abline(lm(log(beta$보정영업지수)~log(beta$평균종사자)), col="deeppink")

plot(log(beta$종사자밀도비), log(beta$보정영업지수),
     xlab="log(종사자밀도비)", ylab="보정영업지수")
abline(lm(log(beta$보정영업지수)~log(beta$종사자밀도비)), col="lightseagreen")

summary(lm(log(보정영업지수)~log(사업체수), beta))
summary(lm(log(보정영업지수)~log(평균종사자), beta))
summary(lm(log(보정영업지수)~log(종사자밀도비), beta))
  
  ## 사업체 영업기간 자료 정리_지영님

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0161_business_age.csv", sep = "/"))  
age <- readr::read_csv(fileInfo, locale=locale(encoding="EUC-KR"))
paged_table(age)

annee <- age %>%
  separate(사업체_평균연령, into=c("연", "개월"), sep="년", convert=T) %>%
  separate(개월, into=c("숫자", "문자"), sep="개월", convert=T) %>%
  select(-문자) %>% mutate(월 = 숫자/12) %>%
  select(-숫자) %>% mutate(사업체_영업기간 = 연 + 월) %>%
  select(-연, -월) %>% filter(!(행정동 %in% c("합계", "소계"))) %>%
  unite("자치구_행정동", 구, 행정동)

gamma <- left_join(beta, annee, by="자치구_행정동") %>%
  select(-사업체수.y, -평균종사자.y, -종사자수) %>%
  rename("사업체수"="사업체수.x", "평균종사자"="평균종사자.x") %>%
  relocate(연도_분기, 코드, 자치구_행정동, 상권_변화_지표, 운영개월, 폐업개월, 영업지수, 보정영업지수,
                소비자서비스업비율, 생산자서비스업비율, 사업체수, 평균종사자, 종사자밀도비, 사업체_영업기간)
paged_table(gamma)

plot(gamma$사업체_영업기간, log(gamma$보정영업지수),
     xlab="사업체 영업기간", ylab="보정영업지수")
abline(lm(log(gamma$보정영업지수)~gamma$사업체_영업기간), col="slateblue")


# 3.4 사업체 영업기간 자료(business_age) -> 종호님
# 3.4.1 변수와 종속변수와의 관계 관찰 -> 유의미
ggData = gamma %>% 
  dplyr::mutate(
    x = 사업체_영업기간
    , y = log(보정영업지수)
  )

lmFit = lm(y ~ x, data = ggData)
summary(lmFit)


# 연도 분기에 따른 사업체 영업기간 및 보정영업지수의 상관 분석
# 상관계수는 -0.24 (음의 상관계수)로서 0.001 이하의 유의수준을 보임
ggpubr::ggscatter(
  ggData, x = "x", y = "y", color = "연도_분기"
  , add = "reg.line", conf.int = TRUE, scales = "free_x"
  , facet.by = "연도_분기"
  , add.params = list(color = "black", fill = "lightgray")
) +
  labs(
    title = NULL
    , x = "사업체 영업기간"
    , y = "보정영업지수"
    , subtitle = "연도 분기에 따른 사업체 영업기간 및 보정영업지수의 상관 분석"
  ) +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 0.95) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.85) +
  theme(text = element_text(size = 14))


# 상권 변화 지표에 따른 사업체 영업기간 및 보정영업지수의 상관 분석
# 상권 변화 지표에 따라 상관 분석 결과 HL의 경우 타 지표에 비해 양의 상관계수를 
# 보일 뿐만 아니라 0.87로서 유의하지 않는 결과를 보였다.
# 반면에 LH,LL의 경우 음의 상관계수 및 90% 신뢰구간 하에서 통계적으로 유의한 결과를 보였다.
ggpubr::ggscatter(
  ggData, x = "x", y = "y", color = "상권_변화_지표"
  , add = "reg.line", conf.int = TRUE, scales = "free_x"
  , facet.by = "상권_변화_지표"
  , add.params = list(color = "black", fill = "lightgray")
) +
  labs(
    title = NULL
    , x = "사업체 영업기간"
    , y = "보정영업지수"
    , subtitle = "상권 변화 지표에 따른 사업체 영업기간 및 보정영업지수의 상관 분석"
  ) +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 0.95) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.85) +
  theme(text = element_text(size = 14))


# 3.5 사업체 창업률 자료(business_founding) -> 종호님
# 3.5.1 변수와 종속변수와의 관계 관찰 -> 유의미
# founding <- readr::read_csv("business_founding.csv", locale=locale(encoding="EUC-KR"))
fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0161_business_founding.csv", sep = "/"))  
founding <- readr::read_csv(fileInfo, locale=locale(encoding="EUC-KR"))
paged_table(founding)

fondation <- founding %>%
  filter(!(행정동 %in% c("합계", "소계"))) %>%
  unite("자치구_행정동", 구, 행정동)

delta <- left_join(gamma, fondation, by="자치구_행정동") %>%
  select(-사업체수.y, -평균종사자.y, -종사자수) %>%
  rename("사업체수"="사업체수.x", "평균종사자"="평균종사자.x")
paged_table(delta)
# ```

# 3.5 사업체 창업률 자료(business_founding) -> 종호님
# 3.5.1 변수와 종속변수와의 관계 관찰 -> 유의미
# plot(delta$사업체수, log(delta$보정영업지수),
#      xlab="사업체수", ylab="보정영업지수")
# abline(lm(log(delta$보정영업지수)~delta$사업체수), col="slateblue")
# 
# summary(lm(log(delta$보정영업지수)~delta$사업체수))

ggData = delta %>% 
  dplyr::mutate(
    x = log(사업체_창업률)
    , y = log(보정영업지수)
    )

ggData = delta %>% 
  dplyr::mutate(
    x = log(사업체_창업률)
    , y = log(보정영업지수)
  )


lmFit = lm(y ~ x, data = ggData)
summary(lmFit)

# cor(ggData$x, ggData$y)

# 연도 분기에 따른 사업체 창업률 및 보정영업지수의 상관 분석
# 상관계수는 0.33 (양의 상관계수)로서 0.001 이하의 유의수준을 보임
ggpubr::ggscatter(
  ggData, x = "x", y = "y", color = "연도_분기"
  , add = "reg.line", conf.int = TRUE, scales = "free_x"
  , facet.by = "연도_분기"
  , add.params = list(color = "black", fill = "lightgray")
) +
  labs(
    title = NULL
    , x = "사업체 창업률"
    , y = "보정영업지수"
    , subtitle = "연도 분기에 따른 사업체 창업률 및 보정영업지수의 상관 분석"
  ) +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 0.95) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.85) +
  theme(text = element_text(size = 14))


# 상권 변화 지표에 따른 사업체 창업률 및 보정영업지수의 상관 분석
# 상권 변화 지표에 따라 상관 분석 결과 모든 지표에 비해 양의 상관계수를 보임
# 특히 HL의 경우 P값 0.54로서 통계적으로 유의하지 않은 반면 타 지표는 90% 신뢰구간 하에서 통계적으로 유의한 결과를 보였다.
ggpubr::ggscatter(
  ggData, x = "x", y = "y", color = "상권_변화_지표"
  , add = "reg.line", conf.int = TRUE, scales = "free_x"
  , facet.by = "상권_변화_지표"
  , add.params = list(color = "black", fill = "lightgray")
) +
  labs(
    title = NULL
    , x = "사업체 창업률"
    , y = "보정영업지수"
    , subtitle = "상권 변화 지표에 따른 사업체 창업률 및 보정영업지수의 상관 분석"
  ) +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 0.95) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.85) +
  theme(text = element_text(size = 14))


# ```{r message=FALSE, warning=FALSE, paged.print=TRUE}
# plot(delta$사업체_창업률, log(delta$보정영업지수),
#      xlab="사업체 창업률", ylab="보정영업지수")
# abline(lm(log(delta$보정영업지수)~delta$사업체_창업률), col="slateblue")
# 
# summary(lm(log(delta$보정영업지수)~delta$사업체_창업률))
# ```
# <br>
  # <br>
  
# 3.6 자영업 종사자수 자료(business_private) -> 종호님
# 3.6.1 로그로 변환하거나 하지 않은 변수 모두에 대해 종속변수와의 관계 관찰 -> 모두 유의미 X

  ## 자영업 종사자수 자료 정리_종호님
# private <- readr::read_csv("business_private.csv", locale=locale(encoding="EUC-KR"))
fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0161_business_private.csv", sep = "/"))  
private <- readr::read_csv(fileInfo, locale=locale(encoding="EUC-KR"))
paged_table(private)

prive <- private %>%
  filter(!(행정동 %in% c("합계", "소계"))) %>%
  select(구, 행정동, 자영업주_및_무급가족_소계) %>%
  unite("자치구_행정동", 구, 행정동) %>%
  rename("자영업_종사자수"="자영업주_및_무급가족_소계")

epsilon <- left_join(delta, prive, by="자치구_행정동")
paged_table(epsilon)
# ```

par(mfrow=c(1,2), cex=.6)

plot(epsilon$자영업_종사자수, log(epsilon$보정영업지수),
     xlab="자영업 종사자수", ylab="보정영업지수")
abline(lm(log(epsilon$보정영업지수)~epsilon$자영업_종사자수), col="slateblue")

plot(log(epsilon$자영업_종사자수), log(epsilon$보정영업지수),
     xlab="log(자영업 종사자수)", ylab="보정영업지수")
abline(lm(log(epsilon$보정영업지수)~log(epsilon$자영업_종사자수)), col="deeppink")

summary(lm(log(epsilon$보정영업지수)~epsilon$자영업_종사자수))
summary(lm(log(epsilon$보정영업지수)~(log(epsilon$자영업_종사자수))))
  

ggData = epsilon %>% 
  dplyr::mutate(
    # x = 자영업_종사자수
    x = log(자영업_종사자수)
    , y = log(보정영업지수)
  )

# lmFit = lm(y ~ x, data = ggData)
# summary(lmFit)
# cor(ggData$x, ggData$y)

# 연도 분기에 따른 자영업 종사자수 및 보정영업지수의 상관 분석
#******************************************
# 독립변수 : 자영업_종사자수
# 종속변수 : log(보정영업지수)
#******************************************
# 상관계수는 -0.12 (음의 상관계수)로서 0.005 이하의 유의수준을 보임
#******************************************
# 독립변수 : log(자영업_종사자수)
# 종속변수 : log(보정영업지수)
#******************************************
# 상관계수는 -0.055 (음의 상관계수)로서 0.26의 P값으로 통계적으로 유의하지 못함
ggpubr::ggscatter(
  ggData, x = "x", y = "y", color = "연도_분기"
  , add = "reg.line", conf.int = TRUE, scales = "free_x"
  , facet.by = "연도_분기"
  , add.params = list(color = "black", fill = "lightgray")
) +
  labs(
    title = NULL
    , x = "자영업 종사자수"
    , y = "보정영업지수"
    , subtitle = "연도 분기에 따른 자영업 종사자수 및 보정영업지수의 상관 분석"
  ) +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 0.95) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.85) +
  theme(text = element_text(size = 14))


# 상권 변화 지표에 따른 자영업 종사자수 및 보정영업지수의 상관 분석
#******************************************
# 독립변수 : 자영업_종사자수
# 종속변수 : log(보정영업지수)
#******************************************
# 상권 변화 지표에 따라 상관 분석 결과 HH를 제외한 모든 지표에서 통계적으로 유의하지 않은 결과를 보임
# 반면에 HH의 경우 상관계수는 -0.23으로서 95% 신뢰구간 하에서 통계적으로 유의한 결과를 보였다.
#******************************************
# 독립변수 : log(자영업_종사자수)
# 종속변수 : log(보정영업지수)
#******************************************
# 모든 지표에서 통계적으로 유의하지 않은 결과를 보임
ggpubr::ggscatter(
  ggData, x = "x", y = "y", color = "상권_변화_지표"
  , add = "reg.line", conf.int = TRUE, scales = "free_x"
  , facet.by = "상권_변화_지표"
  , add.params = list(color = "black", fill = "lightgray")
) +
  labs(
    title = NULL
    , x = "자영업 종사자수"
    , y = "보정영업지수"
    , subtitle = "상권 변화 지표에 따른 자영업 종사자수률 및 보정영업지수의 상관 분석"
  ) +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 0.95) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.85) +
  theme(text = element_text(size = 14))

  ## 생계형사업 종사자수 자료 정리_지영님
# living <- readr::read_csv("business_living.csv", locale=locale(encoding="EUC-KR"))
fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0161_business_living.csv", sep = "/"))  
living <- readr::read_csv(fileInfo, locale=locale(encoding="EUC-KR"))
paged_table(living)

# 커피 전문점 및 기타 비알콜 음료점업, 체인화편의점업 종사자수 구하기
vie <- living %>%
  rename("커피전문점업"="커피 전문점 및 기타 비알콜 음료점업", "체인화편의점업"="체인화 편의점") %>%
  select(구, 행정동, 커피전문점업, 체인화편의점업) %>%
  mutate(커피전문점업=str_replace(커피전문점업, ",", "")) %>%
  unite("자치구_행정동", 구, 행정동)
vie$커피전문점업 <- as.numeric(vie$커피전문점업)
vie$체인화편의점업 <- as.numeric(vie$체인화편의점업)
vie[is.na(vie)] <- 0

zeta <- left_join(epsilon, vie, by="자치구_행정동")
paged_table(zeta)

par(mfrow=c(1,2), cex=.6)

plot(log(zeta$커피전문점업+1), log(zeta$보정영업지수),
     xlab="log(커피전문점업 종사자수)", ylab="보정영업지수")
abline(lm(log(zeta$보정영업지수)~log(zeta$커피전문점업+1)), col="slateblue")

plot(log(zeta$체인화편의점업), log(zeta$보정영업지수),
     xlab="log(체인화편의점업 종사자수)", ylab="보정영업지수")
abline(lm(log(zeta$보정영업지수)~log(zeta$체인화편의점업)), col="deeppink")

summary(lm(log(zeta$보정영업지수)~log(zeta$커피전문점업+1)))
summary(lm(log(zeta$보정영업지수)~log(zeta$체인화편의점업)))
  
# 교호관계 분석
eta <- mutate(zeta, log보정영업지수=log(보정영업지수), log종사자밀도비=log(종사자밀도비),
              log커피전문점업=log(커피전문점업+1), log체인화편의점업=log(체인화편의점업))

ggpairs(eta, columns=c("log보정영업지수", "소비자서비스업비율", "log종사자밀도비",
                       "사업체_영업기간", "사업체_창업률", "log커피전문점업",
                       "log체인화편의점업"))+theme_bw()


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
# R을 이용한 서울시 아파트 실거래가 분석 및 매매 동향 예측

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."
# globalVar$mapPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0163"

#================================================
# Main
#================================================
library(ggplot2)
library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)
library(RCurl)
library(readr)
library(magrittr)
library(ggrepel)
library(colorRamps)
library(ggpubr)
library(sp)
library(gstat)

# showtext::showtext_opts(dpi = 100)
# showtext::showtext.auto()

cbMatlab = colorRamps::matlab.like(11)

# 공공데이터포털 API키
# reqDataKey = globalVar$dataKey
reqDataKey = "Ftj0WhfmnXN86rrVCPTGvlQJoJs9l+ZQjJzPgtc37yVPWuXs8UOP3kD2lTyy9DFInQZj2VvYFH1+Uh7gNgTLLA=="

# 요청 URL
reqUrl = "http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade"
# 요청 키
reqKey = stringr::str_c("?serviceKey=", RCurl::curlEscape(stringr::str_conv(reqDataKey, encoding = "UTF-8")))

# 서울에서 서울특별시 법정동 코드 읽기
codeInfo = Sys.glob(paste(globalVar$mapPath, "/admCode/법정동코드_전체자료.txt", sep = "/"))

codeList = readr::read_delim(codeInfo, delim = "\t", locale = locale("ko", encoding = "EUC-KR"), col_types = "ccc") %>%
  magrittr::set_colnames(c("EMD_CD", "addr", "isUse")) %>% 
  tidyr::separate(col = "addr", into = c("d1", "d2", "d3", "d4"), sep = " ") %>%
  dplyr::mutate(
    emdCd = stringr::str_sub(EMD_CD, 1, 5)
  ) %>% 
  dplyr::filter(
    stringr::str_detect(d1, regex("서울특별시"))
    , stringr::str_detect(isUse, regex("존재"))
    , is.na(d3)
    , is.na(d4)
    )

codeDistList = codeList %>%
  dplyr::distinct(emdCd)

# 날짜 기간
# dtDateList = seq(as.Date("2017-01-01"), as.Date(format(Sys.time(), "%Y-%m-%d")), "1 month")
dtDateList = seq(as.Date("2018-12-01"), as.Date(format(Sys.time(), "%Y-%m-%d")), "1 month")


#***********************************************
# 공공데이터포털 API (자료 수집)
#***********************************************
# i = 1
# i = 53
# j = 2

dataL1 = tibble::tibble()

for (i in 1:length(dtDateList)) {
  for (j in 1:nrow(codeDistList)) {

    sDate = format(dtDateList[i], "%Y%m")

    # 요청 법정동
    reqLawdCd = stringr::str_c("&LAWD_CD=", codeDistList[j, 'emdCd'])

    # 요청 날짜
    reqYmd = stringr::str_c("&DEAL_YMD=", sDate)

    resData = httr::GET(
      stringr::str_c(reqUrl, reqKey, reqLawdCd, reqYmd)
      ) %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON()

    resCode = resData$response$header$resultCode
    if (resCode != "00") { next }

    resItems = resData$response$body$items
    if (resItems == "") { next }

    cat(sprintf(
      "dtDate : %10s | code : %5s"
      , sDate
      , codeList[j, 'emdCd']
    ), "\n")

    resItem = resItems$item %>%
      as.data.frame()
      # readr::type_convert()

    dataL1 = dplyr::bind_rows(
      dataL1
      , data.frame(
        'dtYm' = sDate
        , 'emdCd' = codeDistList[j, 'emdCd']
        , resItem
        )
      )
  }
}

#***********************************************
# 자료 저장
#***********************************************
saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "seoul apartment transaction")
# readr::write_csv(x = dataL1, file = saveFile)

#***********************************************
# 데이터 전처리
#***********************************************
fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0163_seoul apartment transaction.csv", sep = "/"))
dataL2 = readr::read_csv(file = fileInfo) %>% 
  readr::type_convert() %>% 
  dplyr::mutate(
    지번2 = readr::parse_number(지번)
    , emdCd2 = as.character(emdCd)
  ) %>% 
  dplyr::left_join(codeList, by = c("emdCd2" = "emdCd")) %>%
  dplyr::mutate(
    addr = stringr::str_trim(paste(d1, d2, 아파트, 지번, seq = ' '))
    , val = 거래금액 / 전용면적 # 면적당 거래금액
  )

dataL3 = dataL2 %>% 
  dplyr::group_by(d2) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  )

#***********************************************
# 통계 분석
#***********************************************
# 면적당 거래금액 따른 기초 통계량
dataL2 %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE) # 평균값
    , medianVal = median(val, na.rm = TRUE) # 중앙값
    , sdianVal = sd(val, na.rm = TRUE) # 표준편차
    , maxVal = max(val, na.rm = TRUE) # 최대값
    , minVal = min(val, na.rm = TRUE) # 최소값
    , cnt = n() # 개수
  ) %>%
  dplyr::arrange(desc(meanVal))

# 법정동에 따른 면적당 거래금액 따른 기초 통계량
dataL2 %>%
  dplyr::group_by(d2) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE) # 평균값
    , medianVal = median(val, na.rm = TRUE) # 중앙값
    , sdianVal = sd(val, na.rm = TRUE) # 표준편차
    , maxVal = max(val, na.rm = TRUE) # 최대값
    , minVal = min(val, na.rm = TRUE) # 최소값
    , cnt = n() # 개수
  ) %>%
  dplyr::arrange(desc(meanVal))

  
#***********************************************
# 그래프 그리기(히스토그램, 상자 수염그림, 산점도 등)
#***********************************************
# 면적당 거래금액 따른 히스토그램
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "면적당 거래금액 따른 히스토그램")

ggplot(dataL2, aes(x = val)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = 0.2) +
  geom_rug(aes(x = val, y = 0), position = position_jitter(height = 0)) +
  labs(x = "면적당 거래금액", y = "밀도 함수", colour = NULL, fill = NULL, subtitle = "면적당 거래금액 따른 히스토그램") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 12, height = 6, dpi = 600)

# 법정동에 따른 면적당 거래금액 히스토그램
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "법정동에 따른 면적당 거래금액 히스토그램")

ggplot(dataL3, aes(x = d2, y = meanVal, fill = meanVal)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = round(meanVal, 0)), vjust = 1.6, color = "white", size = 4) +
  labs(x = "법정동", y = "면적당 거래금액", fill = NULL, subtitle = "법정동에 따른 면적당 거래금액 히스토그램") +
  scale_fill_gradientn(colours = cbMatlab, na.value = NA) +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)


# 면적당 거래금액 따른 상자 그림
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "면적당 거래금액 따른 상자 그림")

ggplot(dataL2, aes(y = val)) +
  geom_boxplot() +
  labs(x = NULL, y = "면적당 거래금액", colour = NULL, fill = NULL, subtitle = "면적당 거래금액 따른 상자 그림") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 12, height = 6, dpi = 600)

# 법정동에 따른 면적당 거래금액 상자 그림
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "법정동에 따른 면적당 거래금액 상자 그림")

ggplot(dataL2, aes(x = d2, y = val, color = d2)) +
  geom_boxplot() +
  labs(x = "법정동", y = "면적당 거래금액", fill = NULL, subtitle = "법정동에 따른 면적당 거래금액 상자 그림") +
  # scale_colour_gradientn(colours = cbMatlab, na.value = NA) +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)

# 면적당 거래금액 산점도
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "면적당 거래금액 산점도")

ggpubr::ggscatter(
  dataL2, x = "전용면적", y = "거래금액"
  , add = "reg.line", conf.int = TRUE, scales = "free_x"
  # , facet.by = "전체 법정동"
  , add.params = list(color = "blue", fill = "lightblue")
  ) +
  labs(
    title = NULL
    , x = "전용면적"
    , y = "거래금액"
    , color = NULL
    , subtitle = "면적당 거래금액 산점도"
  ) +
  theme_bw() +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 1.0, size = 5) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.90, p.accuracy  =  0.01,  r.accuracy  =  0.01, size = 5) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 8, height = 8, dpi = 600)

# 법정동에 따른 면적당 거래금액 산점도
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "법정동에 따른 면적당 거래금액 산점도")

ggpubr::ggscatter(
  dataL2, x = "전용면적", y = "거래금액", color = "d2"
  , add = "reg.line", conf.int = TRUE, scales = "free_x"
  , facet.by = "d2"
  , add.params = list(color = "black", fill = "lightgray")
  ) +
  labs(
    title = NULL
    , x = "전용면적"
    , y = "거래금액"
    , color = NULL
    , subtitle = "법정동에 따른 면적당 거래금액 산점도"
  ) +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 0.95) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.85, p.accuracy  =  0.01,  r.accuracy  =  0.01) +
  theme(text = element_text(size = 14)) +
  ggsave(filename = saveImg, width = 12, height = 15, dpi = 600)

#***********************************************
# 지도 그리기
#***********************************************
addrList = dataL2$addr %>% unique() %>% sort() %>%
  as.tibble()

# 구글 API 하루 제한
# addrData = ggmap::mutate_geocode(addrList, value, source = "google")

# 각 주소에 따라 위/경도 반환
# for (i in 1:nrow(addrList)) {
#   addrData = ggmap::mutate_geocode(addrList[i, 'value'], value, source = "google")
# 
#   if (nrow(addrData) < 1) { next }
# 
#   readr::write_csv(x = addrData, file = saveFile, append = TRUE)
# }

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "seoul apartment transaction-addrData")
addrData =  readr::read_csv(file = saveFile, col_names = c("value", "lon", "lat"))

dataL4 = dataL2 %>% 
  dplyr::left_join(addrData, by = c("addr" = "value")) %>% 
  dplyr::filter(
    ! is.na(lon)
    , ! is.na(lat)
    , dplyr::between(lon, 120, 130)
    , dplyr::between(lat, 30, 40)
  ) %>% 
  dplyr::group_by(lon, lat, addr) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  )

map = ggmap::get_map(
  location = c(lon = mean(dataL4$lon, na.rm = TRUE), lat = mean(dataL4$lat, na.rm = TRUE))
  , zoom = 12
)

# 면적당 거래금액 지도 맵핑
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "면적당 거래금액 지도 맵핑")

ggmap(map, extent = "device") +
  geom_point(data = dataL4, aes(x = lon, y = lat, fill = meanVal, color = meanVal, size = meanVal, alpha = 0.3)) +
  scale_color_gradientn(colours = cbMatlab, na.value = NA) +
  scale_fill_gradientn(colours = cbMatlab, na.value = NA) +
  labs(
    subtitle = NULL
    , x = NULL
    , y = NULL
    , fill = NULL
    , colour = NULL
    , title = NULL
    , size = NULL
  ) +
  scale_alpha(guide = 'none') +
  theme(
    text = element_text(size = 18)
  ) +
  ggsave(filename = saveImg, width = 10, height = 10, dpi = 600)


# 면적당 거래금액 지도 집중도
spNewData = expand.grid(
  x = seq(from = min(dataL4$lon, na.rm = TRUE), to = max(dataL4$lon, na.rm = TRUE), by = 0.003)
  , y = seq(from = min(dataL4$lat, na.rm = TRUE), to = max(dataL4$lat, na.rm = TRUE), by = 0.003)
)
sp::coordinates(spNewData) = ~ x + y
sp::gridded(spNewData) = TRUE

spData = dataL4
sp::coordinates(spData) = ~ lon + lat
# sp::gridded(spData) = TRUE

# IDW 학습 및 전처리수행
spDataL1 = gstat::idw(
  formula = meanVal ~ 1
  , locations = spData
  , newdata = spNewData
  , nmax = 4
) %>%
  as.data.frame() %>%
  dplyr::rename(
    lon = x
    , lat = y
    , val = var1.pred
  ) %>%
  dplyr::select(-var1.var) %>% 
  as.tibble()

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "면적당 거래금액 지도 집중도")

ggmap(map, extent = "device") +
  geom_tile(data = spDataL1, aes(x = lon, y = lat, fill = val, alpha = 0.2)) +
  # geom_raster(data = spDataL1, aes(x = lon, y = lat, fill = val, alpha = 0.2)) +
  # scale_color_gradientn(colours = cbMatlab, na.value = NA) +
  scale_fill_gradientn(colours = cbMatlab, na.value = NA) +
  labs(
    subtitle = NULL
    , x = NULL
    , y = NULL
    , fill = NULL
    , colour = NULL
    , title = NULL
    , size = NULL
  ) +
  scale_alpha(guide = 'none') +
  theme(
    text = element_text(size = 18)
  ) +
  ggsave(filename = saveImg, width = 10, height = 10, dpi = 600)


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
# R을 이용한 교통사고 다발지역 시각화

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."
# globalVar$mapPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0171"

#================================================
# Main
#================================================
library(ggplot2)
library(tidyverse)
library(readr)
library(magrittr)

# showtext::showtext_opts(dpi = 100)
# showtext::showtext.auto()

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0171_도로교통공단_교통사고다발지역_20191010.xlsx", sep = "/"))
data = openxlsx::read.xlsx(fileInfo, sheet = 1) %>%
  tibble::as.tibble() %>% 
  dplyr::rename(
    year = 사고년도
    , area = 시도시군구명
    , type = 사고유형구분
    , val = 발생
    , val2 = 사상
    , val3 = 사망
  ) %>% 
  dplyr::select(year, area, type, val, val2, val3)

# 0값을 NA로 변환
# dataL1 = data %>%
  # dplyr::na_if(0)

dataL1 = data

#========================================================
# 다음 3개의 그래프를 작성후 2.script로 저장
#========================================================
dataL2 = dataL1 %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(
    YearBS_CAR = mean(val, na.rm = TRUE)
    , YearSS_CAR = mean(val2, na.rm = TRUE)
    , YearSM_CAR = mean(val3, na.rm = TRUE)
  )

#*************************************
# 사고년도별 평균 발생건수
#*************************************
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "사고년도별 평균 발생건수")

ggplot(dataL2, aes(x = year, y = YearBS_CAR, fill = YearBS_CAR)) +
  geom_col() +
  labs(x = "사고년도", y = "평균 발생건수", colour = NULL, fill = NULL, subtitle = "사고년도별 평균 발생건수") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)


#*************************************
# 사고년도별 평균 사상자수
#*************************************
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "사고년도별 평균 사상자수")

ggplot(dataL2, aes(x = year, y = YearSS_CAR, fill = YearSS_CAR)) +
  geom_line() +
  labs(x = "사고년도", y = "평균 사상자수", colour = NULL, fill = NULL, subtitle = "사고년도별 평균 사상자수") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)


#*************************************
# 사고년도별 평균 사망자수
#*************************************
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "사고년도별 평균 사망자수")

ggplot(dataL2, aes(x = year, y = YearSM_CAR, fill = YearSM_CAR)) +
  geom_col() +
  labs(x = "사고년도", y = "평균 사망자수", colour = NULL, fill = NULL, subtitle = "사고년도별 평균 사망자수") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)


#========================================================
# 다음 2개의 그래프를 작성후 3.script 로 저장 
#========================================================
dataL2 = dataL1 %>% 
  dplyr::group_by(area) %>% 
  dplyr::summarise(
    Top10ss_CAR = mean(val2, na.rm = TRUE)
    , Top10SM_CAR = mean(val3, na.rm = TRUE)
  )

#*************************************
# 시도시군구명별 평균 사상자수 
#*************************************
dataL3 = dataL2 %>% 
  dplyr::arrange(desc(Top10ss_CAR)) %>%
  dplyr::slice(1:10)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "시도시군구명별 평균 사상자수")

ggplot(dataL3, aes(x = Top10ss_CAR, y = reorder(area, Top10ss_CAR), fill = Top10ss_CAR)) +
  geom_col() +
  labs(x = "평균 사상자수", y = "시도시군구명", colour = NULL, fill = NULL, subtitle = "시도시군구명별 평균 사상자수") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)


#*************************************
# 시도시군구명별 평균 사망자수 
#*************************************
dataL3 = dataL2 %>%
  dplyr::arrange(desc(Top10SM_CAR)) %>%
  dplyr::slice(1:10)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "시도시군구명별 평균 사망자수")

ggplot(dataL3, aes(x = Top10SM_CAR, y = reorder(area, Top10SM_CAR), fill = Top10ss_CAR)) +
  geom_col() +
  labs(x = "평균 사망자수", y = "시도시군구명", colour = NULL, fill = NULL, subtitle = "시도시군구명별 평균 사망자수") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)


#========================================================
# 4.Script 로 저장 
#========================================================

#*************************************
# 사고년도 및 사고유형별 평균 발생건수
#*************************************
dataL2 = dataL1 %>% 
  dplyr::group_by(year, type) %>% 
  dplyr::summarise(
    Year_TypeB_CAR = mean(val, na.rm = TRUE)
    , Year_TypeSS_CAR = mean(val2, na.rm = TRUE)
    , Year_TypeSM_CAR = mean(val3, na.rm = TRUE)
  ) 

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "사고년도 및 사고유형별 평균 발생건수 막대 그래프")

# 막대
ggplot(dataL2, aes(x = year, y = Year_TypeB_CAR, fill = type)) +
  geom_col(position = "dodge") +
  labs(x = "사고년도", y = "평균 발생건수", colour = NULL, fill = "사고유형 구분", subtitle = "사고년도 및 사고유형별 평균 발생건수") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

# 시계열
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "사고년도 및 사고유형별 평균 발생건수 시계열 그래프")

ggplot(dataL2, aes(x = year, y = Year_TypeB_CAR, color = type)) +
  geom_line() +
  labs(x = "사고년도", y = "평균 발생건수", colour = "사고유형 구분", fill = NULL, subtitle = "사고년도 및 사고유형별 평균 발생건수") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)


#*************************************
# 사고년도 및 사고유형별 평균 사상자수
#*************************************
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "사고년도 및 사고유형별 평균 사상자수 막대 그래프")

# 막대
ggplot(dataL2, aes(x = year, y = Year_TypeSS_CAR, fill = type)) +
  geom_col(position = "dodge") +
  labs(x = "사고년도", y = "평균 사상자수", colour = NULL, fill = "사고유형 구분", subtitle = "사고년도 및 사고유형별 평균 사상자수") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

# 시계열
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "사고년도 및 사고유형별 평균 사상자수 시계열 그래프")

ggplot(dataL2, aes(x = year, y = Year_TypeSS_CAR, colour = type)) +
  geom_line() +
  labs(x = "사고년도", y = "평균 사상자수", colour = "사고유형 구분", fill = NULL, subtitle = "사고년도 및 사고유형별 평균 사상자수") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

#*************************************
# 사고년도 및 사고유형별 평균 사망자수
#*************************************
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "사고년도 및 사고유형별 평균 사망자수 막대 그래프")

# 막대
ggplot(dataL2, aes(x = year, y = Year_TypeSM_CAR, fill = type)) +
  geom_col(position = "dodge") +
  labs(x = "사고년도", y = "평균 사망자수", colour = NULL, fill = "사고유형 구분", subtitle = "사고년도 및 사고유형별 평균 사망자수") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

# 시계열
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "사고년도 및 사고유형별 평균 사망자수 시계열 그래프")

ggplot(dataL2, aes(x = year, y = Year_TypeSM_CAR, colour = type)) +
  geom_line() +
  labs(x = "사고년도", y = "평균 사망자수", colour = "사고유형 구분", fill = NULL, subtitle = "사고년도 및 사고유형별 평균 사망자수") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)


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
# R을 이용한 마크다운, 다양한 그래프 시각화

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."
# globalVar$mapPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0166"

#================================================
# Main
#================================================

#====================================================================
# 2번 문제
#====================================================================
library(ggplot2)
library(tidyverse)
library(readr)

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0166_exam.csv", sep = "/"))
data = readr::read_csv(file = fileInfo)

#*********************************************************************
# exam 데이터셋에서 class가 1인 경우만 추출해서 데이터를 출력하고
# summary(), cor(), hist(), qplot(), boxplot(), pairs()를 적용하시오.
#*********************************************************************
dataL1 = data %>% 
  dplyr::filter(class == 1)

dplyr::tbl_df(dataL1)
summary(dataL1)
cor(dataL1)
hist(dataL1)
qplot(dataL1$english, dataL1$science)
pairs(dataL1)

#*********************************************************************
# exam 데이터셋에서 class가 2인 경우만 추출해서 데이터를 출력하고
# summary(), cor(), hist(), qplot(), boxplot(), pairs()를 적용하시오.
#*********************************************************************
dataL1 = data %>% 
  dplyr::filter(class == 2)

dplyr::tbl_df(dataL1)
summary(dataL1)
cor(dataL1)
hist(dataL1)
qplot(dataL1$english, dataL1$science)
pairs(dataL1)

#*********************************************************************
# exam 데이터셋에서 english가 80이상인 경우만 추출해서 데이터를 출력하고
# summary(), cor(), hist(), qplot(), boxplot(), pairs()를 적용하시오.
#*********************************************************************
dataL1 = data %>% 
  dplyr::filter(english >= 80)

dplyr::tbl_df(dataL1)
summary(dataL1)
cor(dataL1)
hist(dataL1)
qplot(dataL1$english, dataL1$science)
pairs(dataL1)


#*********************************************************************
# exam 데이터셋에서 class가 1이고 english가 80이상인 경우만 추출해서 데이터를 출력하고
# summary(), cor(), hist(), qplot(), boxplot(), pairs()를 적용하시오.
#*********************************************************************
dataL1 = data %>% 
  dplyr::filter(
    class == 1
    , english >= 80
    )

dplyr::tbl_df(dataL1)
summary(dataL1)
cor(dataL1)
hist(dataL1)
qplot(dataL1$english, dataL1$science)
pairs(dataL1)

#*********************************************************************
# class별로 english와 science의 평균을 계산해서 다음과 같이 출력하시오.
#*********************************************************************
dataL1 = data %>% 
  dplyr::group_by(class) %>% 
  dplyr::summarise(
    english_mean = mean(english, na.rm = TRUE)
    , science_mean = mean(science, na.rm = TRUE)
  )

dplyr::tbl_df(dataL1)

#====================================================================
# 3번 문제
#====================================================================
library(faraway)

data(nepali)

nepali <- nepali %>% 
  dplyr::select(id, sex, wt, ht, age) %>% 
  dplyr::mutate(
    id = factor(id)
    , sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female"))
  ) %>% 
  dplyr::distinct(id, .keep_all = TRUE)

# 실습 1
# 체중을 기준으로 히스토그램을 시각화
ggplot(nepali, aes(x = ht)) +
  geom_histogram()

# 실습 2
# 체중을 기준으로 히스토그램을 시각화
# fill (채우기) 및 color (테두리)를 통해 설정
# 그림 제목, x축 이름, x축의 범위를 추가
ggplot(nepali, aes(x = ht)) +
  geom_histogram(fill = "lightblue", color = "black")+
  ggtitle("Height of children") +
  xlab("Height (cm)") +
  xlim(c(0, 120))

# 실습 3
# 키 (x축) 및 체중 (y축)을 기준으로 산점도를 시각화
ggplot(nepali, aes(x = ht, y = wt)) +
  geom_point()

# 실습 4
# 키 (x축) 및 체중 (y축)을 기준으로 산점도를 시각화
# size (크기) 및 color (테두리)를 통해 설정
# 그림 제목, x축 이름, y축 이름을 추가
ggplot(nepali, aes(x = ht, y = wt)) +
  geom_point(color = "blue", size = 0.5) +
  ggtitle("Weight vsrsus Height") +
  xlab("Height (cm)") +
  ylab("Weight (kg)")

# 실습 5
# 키 (x축) 및 체중 (y축) 및 컬러 (성별)를 기준으로 산점도를 시각화
# size (크기)를 통해 설정
# 그림 제목, x축 이름, y축 이름을 추가
ggplot(nepali, aes(x = ht, y = wt, color = sex)) +
  geom_point(size = 0.5) +
  ggtitle("Weight vsrsus Height") +
  xlab("Height (cm)") +
  ylab("Weight (kg)")

# 실습 6
# 임의의 상수 (x축) 및 키 (y축)를 기준으로 상자 그림을 시각화
# x축 이름, y축 이름을 추가
ggplot(nepali, aes(x = 1, y = ht)) +
  geom_boxplot() +
  xlab("") +
  ylab("Height (kg)")
  
# 실습 7
# 성별 (x축) 및 키 (y축)를 기준으로 상자 그림을 시각화
# x축 이름, y축 이름을 추가
ggplot(nepali, aes(x = sex, y = ht)) +
  geom_boxplot() +
  xlab("Sex") +
  ylab("Height (kg)")


#====================================================================
# 4번 문제
#====================================================================
getwd()

x = data.frame(
  id = 1:4
  , name = c("Kim", "Lee", "Park", "Choi")
)

library(tidyr)
library(dplyr)

#====================================================================
# 5번 문제
#====================================================================

library(GGally)

# GGally 패키지에 포함되어있는 ggpairs은 다변량 시각화 전문 라이브러리
# 즉 여러 변수와 변수 간의 관계를 통해 하나의 그림으로 시각화
# base의 graphics에 수록되어있는 panel.pairs()의 ggplot 버전
# ggplot의 지식을 살려 원하는대로 사용자 정의 가능 (자유도가 매우 높은)

# 그에 따른 upper, lower, diag로 구성되어 있음
# upper
# 연속량 × 연속량 : 상관계수 (상관관계)
# 연속량 × 이산량 (factor) : 상자 그림 (분포)
# 이산량 × 이산량 : factor 다른 막대

# lower
# 연속량 × 연속량 : 산점도 (상관관계)
# 연속량 × 이산량 : factor 별 히스토그램 (분포)
# 이산량 × 이산량 : factor 다른 막대

# diag
# 연속량 : 밀도 (분포)
# 이산량 : 막대 그래프 (분포)


# 이 그림에서는 상관관계 (상관계수, 산점도), 분포 (밀도, 막대그래프, 상자 그림, 히스토그램)로 시각화
# 그 결과 체중의 경우 나이보다 키에 높은 상관계수 (0.957)뿐만 아니라 0.01 이하의 통계적 유의수준을 보임
# 일반적으로 키는 정규분포를 띠나 체중 및 나이의 경우 다봉분포를 확인

nepali %>% 
  dplyr::select(sex, wt, ht, age) %>% 
  GGally::ggpairs()


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
# R을 이용한 2004년 4월-2019년 12월까지 환율 데이터 변동률 계산 및 시계열 시각화

#================================================
# Set Env
#================================================
# R 프로그래밍을 위한 기초 환경변수 (입력자료 경로, 이미지저장 경로, 출력자료 경로, 지도 맵 경로) 설정
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."
# globalVar$mapPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0173"

#================================================
# Main
#================================================
# 필요한 라이브러리 읽기
library(ggplot2)
library(tidyverse)
library(xlsx)
library(rJava)
library(xlsxjars)
library(dplyr)
library(dint)
library(zoo)

# 기존 데이터
# fileInfo1 = Sys.glob(paste(globalVar$inpPath, "LSH0164_NIKKEI225.csv", sep = "/"))
# NKI <-read.csv(fileInfo1) %>% 
#   filter(NIKKEI225 != ".") %>%
#   tibble::as.tibble()
# 
# fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0164_NASDAQCOM.csv", sep = "/"))
# GDI <- read.csv(fileInfo2) %>% 
#   filter(NASDAQCOM != ".") %>%
#   tibble::as.tibble()
# 
# fileInfo3 = Sys.glob(paste(globalVar$inpPath, "LSH0164_DGS10.csv", sep = "/"))
# LIBOR <- read.csv(fileInfo3) %>%
#   dplyr::filter(DGS10 != ".") %>%
#   tibble::as.tibble()

# 신규 데이터
fileInfo1 = Sys.glob(paste(globalVar$inpPath, "LSH0173_NIKKEI225+from+FRED(2004).csv", sep = "/"))
NKI <-read.csv(fileInfo1) %>% 
  filter(NIKKEI225 != ".") %>%
  dplyr::mutate(DATE = readr::parse_date(Date, "%m/%d/%y")) %>% 
  tibble::as.tibble()

# fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0173_NASDAQCOM.csv", sep = "/"))
# fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0173_sp500 index.csv", sep = "/"))
fileList = Sys.glob(paste(globalVar$inpPath, "LSH0173_*.csv", sep = "/"))

for (fileInfo in fileList) {
  
  if (stringr::str_detect(fileInfo, regex("NIKKEI225"))) next
  
  fileName = tools::file_path_sans_ext(fs::path_file(fileInfo))
  
  GDI <- read.csv(fileInfo) %>% 
    filter(
      val != "."
      , val != "null"
    ) %>%
    dplyr::mutate(DATE = readr::parse_date(as.character(date), "%m/%d/%y")) %>% 
    tibble::as.tibble()
  
  dtDateKst = seq(
    lubridate::ymd(min(NKI$DATE, GDI$DATE, na.rm = TRUE), tz = "Asia/Seoul")
    , lubridate::ymd(max(NKI$DATE, GDI$DATE, na.rm = TRUE), tz = "Asia/Seoul")
    , by = "1 day"
  )
  
  dtDateList = tibble::tibble(dtDateKst) %>%
    dplyr::mutate(
      sQuart = stringr::str_c(format(dtDateKst, "%y"), "-Q", lubridate::quarter(dtDateKst))
      # sQuart = dint::date_yq(lubridate::year(dtDateKst), lubridate::month(dtDateKst))
      , sDateKst = format(dtDateKst, "%Y-%m-%d")
      # , dtDateUtc = lubridate::with_tz(dtDateKst, "UTC")
    )
  
  data = dtDateList %>% 
    dplyr::left_join(NKI, by = c("dtDateKst" = "DATE")) %>% 
    dplyr::left_join(GDI, by = c("dtDateKst" = "DATE")) %>% 
    # dplyr::left_join(LIBOR, by = c("sDateKst" = "DATE")) %>% 
    readr::type_convert() # %>% 
  # tidyr::fill(NIKKEI225, NASDAQCOM)
  
  dataL1 = data %>% 
    dplyr::mutate(
      perNKI = (NIKKEI225 - lag(NIKKEI225)) / lag(NIKKEI225) * 100.0
      , perVal = (val - lag(val)) / lag(val) * 100.0
      # , perDGS = (DGS10 - lag(DGS10)) / lag(DGS10) * 100.0
    ) %>% 
    dplyr::filter(
      dplyr::between(sDateKst, as.Date("2004-04-01"), as.Date("2019-12-31"))
    ) %>% 
    na.omit()
  
  quartList = dataL1$sQuart %>% unique() %>% sort()
  
  dataL3 = tibble::tibble()
  
  # quartInfo = "2004-Q2"
  for (quartInfo in quartList) {
    
    dataL2 = dataL1 %>% 
      dplyr::filter(sQuart == quartInfo)
    
    if (nrow(dataL2) < 1) next
    
    lmFit = lm(perVal ~ perNKI, data = dataL2)
    
    # 데이터 병합
    dataL3 = dplyr::bind_rows(
      dataL3
      , data.frame(
        sQuart = quartInfo
        , slope = coef(lmFit)[2]
      )
    )
  }
  
  saveFile = sprintf("%s/%s_%s_%s.csv", globalVar$outPath, serviceName, fileName, "dataL1")
  readr::write_csv(x = dataL1, file = saveFile)
  
  saveFile = sprintf("%s/%s_%s_%s.csv", globalVar$outPath, serviceName, fileName, "dataL3")
  readr::write_csv(x = dataL3, file = saveFile)
  
  saveImg = sprintf("%s/%s_%s_%s.png", globalVar$figPath, serviceName, fileName, "연도 4분기에 따른 기울기 시계열")
  
  ggplot(aes(x = sQuart, y = slope, group=1), data = dataL3) +
    geom_line() +
    labs(
      title = NULL
      , x = "연도 4분기"
      , y = "기울기"
      , color = NULL
      , subtitle = "연도 4분기에 따른 기울기 시계열"
    ) +
    theme(
      text = element_text(size = 14)
      , axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
    ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)
  
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

#================================================
# 요구사항
#================================================
# R을 이용한 서울시 아파트 실거래가 회귀분석 및 주택 가격 결정 요인

# 먼저 데이터를 수집합니다 (예 : 거주 국가의 10 년 도시 수준 데이터).
# 데이터 세트에는 주택 가격, 소득, 도시 크기 및 기타 변수가 있어야합니다.
# 그런 다음 회귀를 실행하고 
# 소득을 주택 가격으로 나눈 값으로 측정 한 주택 가격 결정 요인을 테스트 할 수 있습니다.

#================================================
# Set Env
#================================================
globalVar = list()
globalVar$inpPath = "."
globalVar$figPath = "."
globalVar$outPath = "."
globalVar$mapPath = "."

# rm(list = ls())
# prjName = "test"
# source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0169"

#================================================
# Main
#================================================
library(ggplot2)
library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)
library(RCurl)
library(readr)
library(magrittr)
library(ggrepel)
library(colorRamps)
library(ggpubr)
library(lm.beta)
library(ggpmisc)

cbMatlab = colorRamps::matlab.like(11)

# 공공데이터포털 API키
# reqDataKey = globalVar$dataKey
reqDataKey = "Ftj0WhfmnXN86rrVCPTGvlQJ%oJs9l+ZQjJzPgtc37yVPWuXs8UOP3kD2lTyy9DFInQZj2VvYFH1+Uh7gNgTLLA=="

# 요청 URL
reqUrl = "http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade"
# 요청 키
reqKey = stringr::str_c("?serviceKey=", RCurl::curlEscape(stringr::str_conv(reqDataKey, encoding = "UTF-8")))

# 서울에서 서울특별시 법정동 코드 읽기
codeInfo = Sys.glob(paste(globalVar$mapPath, "/admCode/법정동코드_전체자료.txt", sep = "/"))

codeList = readr::read_delim(codeInfo, delim = "\t", locale = locale("ko", encoding = "EUC-KR"), col_types = "ccc") %>%
  magrittr::set_colnames(c("EMD_CD", "addr", "isUse")) %>% 
  tidyr::separate(col = "addr", into = c("d1", "d2", "d3", "d4"), sep = " ") %>%
  dplyr::mutate(
    emdCd = stringr::str_sub(EMD_CD, 1, 5)
  ) %>% 
  dplyr::filter(
    stringr::str_detect(d1, regex("서울특별시"))
    , stringr::str_detect(isUse, regex("존재"))
    , is.na(d3)
    , is.na(d4)
  )

codeDistList = codeList %>%
  dplyr::distinct(emdCd)

# 날짜 기간
# dtDateList = seq(as.Date("2017-01-01"), as.Date(format(Sys.time(), "%Y-%m-%d")), "1 month")
dtDateList = seq(as.Date("2018-12-01"), as.Date(format(Sys.time(), "%Y-%m-%d")), "1 month")

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0169_가구_특성정보_(+소득정보)_201211.csv", sep = "/"))
costData = readr::read_csv(file = fileInfo) %>%
  dplyr::mutate(
    emdCd = stringr::str_sub(as.character(raw_dn_cd), 1, 5)
  ) %>% 
  dplyr::group_by(emdCd) %>% 
  dplyr::summarise(
    meanCost = mean(avrg_income_amount_am, na.rm = TRUE)
  )

#***********************************************
# 공공데이터포털 API (자료 수집)
#***********************************************
# i = 1
# i = 53
# j = 2

dataL1 = tibble::tibble()

for (i in 1:length(dtDateList)) {
  for (j in 1:nrow(codeDistList)) {
    
    sDate = format(dtDateList[i], "%Y%m")
    
    # 요청 법정동
    reqLawdCd = stringr::str_c("&LAWD_CD=", codeDistList[j, 'emdCd'])
    
    # 요청 날짜
    reqYmd = stringr::str_c("&DEAL_YMD=", sDate)
    
    resData = httr::GET(
      stringr::str_c(reqUrl, reqKey, reqLawdCd, reqYmd)
    ) %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON()
    
    resCode = resData$response$header$resultCode
    if (resCode != "00") { next }
    
    resItems = resData$response$body$items
    if (resItems == "") { next }
    
    cat(sprintf(
      "dtDate : %10s | code : %5s"
      , sDate
      , codeList[j, 'emdCd']
    ), "\n")
    
    resItem = resItems$item %>%
      as.data.frame()
    # readr::type_convert()
    
    dataL1 = dplyr::bind_rows(
      dataL1
      , data.frame(
        'dtYm' = sDate
        , 'emdCd' = codeDistList[j, 'emdCd']
        , resItem
      )
    )
  }
}

#***********************************************
# 자료 저장
#***********************************************
# saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "seoul apartment transaction.csv")
# readr::write_csv(x = dataL1, file = saveFile)

#***********************************************
# 데이터 전처리
#***********************************************
fileInfo = Sys.glob(paste(globalVar$outPath, "LSH0169_seoul apartment transaction.csv", sep = "/"))

dataL2 = readr::read_csv(file = fileInfo) %>% 
  readr::type_convert() %>% 
  dplyr::mutate(
    지번2 = readr::parse_number(지번)
    , emdCd = as.character(emdCd)
  ) %>% 
  dplyr::left_join(codeList, by = c("emdCd" = "emdCd")) %>%
  dplyr::left_join(costData, by = c("emdCd" = "emdCd")) %>% 
  dplyr::mutate(
    addr = stringr::str_trim(paste(d1, d2, 아파트, 지번, seq = ' '))
    , val = 거래금액 / meanCost # 연소득당 거래금액
    , val2 = 거래금액 / 전용면적 # 면적당 거래금액
  )

dataL3 = dataL2 %>% 
  dplyr::group_by(d2) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  )

#***********************************************
# 통계 분석
#***********************************************
# 연소득당 거래금액 따른 기초 통계량
dataL2 %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE) # 평균값
    , medianVal = median(val, na.rm = TRUE) # 중앙값
    , sdVal = sd(val, na.rm = TRUE) # 표준편차
    , maxVal = max(val, na.rm = TRUE) # 최대값
    , minVal = min(val, na.rm = TRUE) # 최소값
    , cnt = n() # 개수
  ) %>%
  dplyr::arrange(desc(meanVal))

# 법정동에 따른 연소득당 거래금액 따른 기초 통계량
dataL2 %>%
  dplyr::group_by(d2) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE) # 평균값
    , medianVal = median(val, na.rm = TRUE) # 중앙값
    , sdVal = sd(val, na.rm = TRUE) # 표준편차
    , maxVal = max(val, na.rm = TRUE) # 최대값
    , minVal = min(val, na.rm = TRUE) # 최소값
    , cnt = n() # 개수
  ) %>%
  dplyr::arrange(desc(meanVal))


#***********************************************
# 그래프 그리기(히스토그램, 상자 수염그림, 산점도 등)
#***********************************************
# 연소득당 거래금액 따른 히스토그램
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "연소득당 거래금액 따른 히스토그램")

ggplot(dataL2, aes(x = val)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = 0.2) +
  geom_rug(aes(x = val, y = 0), position = position_jitter(height = 0)) +
  labs(x = "연소득당 거래금액", y = "밀도 함수", colour = NULL, fill = NULL, subtitle = "연소득당 거래금액 따른 히스토그램") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 12, height = 6, dpi = 600)

# 법정동에 따른 연소득당 거래금액 히스토그램
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "법정동에 따른 연소득당 거래금액 히스토그램")

ggplot(dataL3, aes(x = d2, y = meanVal, fill = meanVal)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = round(meanVal, 0)), vjust = 1.6, color = "white", size = 4) +
  labs(x = "법정동", y = "연소득당 거래금액", fill = NULL, subtitle = "법정동에 따른 연소득당 거래금액 히스토그램") +
  scale_fill_gradientn(colours = cbMatlab, na.value = NA) +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)


# 연소득당 거래금액 따른 상자 그림
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "연소득당 거래금액 따른 상자 그림")

ggplot(dataL2, aes(y = val)) +
  geom_boxplot() +
  labs(x = NULL, y = "연소득당 거래금액", colour = NULL, fill = NULL, subtitle = "연소득당 거래금액 따른 상자 그림") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 12, height = 6, dpi = 600)

# 법정동에 따른 연소득당 거래금액 상자 그림
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "법정동에 따른 연소득당 거래금액 상자 그림")

ggplot(dataL2, aes(x = d2, y = val, color = d2)) +
  geom_boxplot() +
  labs(x = "법정동", y = "연소득당 거래금액", fill = NULL, subtitle = "법정동에 따른 연소득당 거래금액 상자 그림") +
  # scale_colour_gradientn(colours = cbMatlab, na.value = NA) +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)

# 연소득당 거래금액 산점도
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "연소득당 거래금액 산점도")

ggpubr::ggscatter(
  dataL2, x = "meanCost", y = "거래금액"
  , add = "reg.line", conf.int = TRUE, scales = "free_x"
  # , facet.by = "전체 법정동"
  , add.params = list(color = "blue", fill = "lightblue")
) +
  labs(
    title = NULL
    , x = "연소득"
    , y = "거래금액"
    , color = NULL
    , subtitle = "연소득당 거래금액 산점도"
  ) +
  theme_bw() +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 1.0, size = 5) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.90, p.accuracy  =  0.01,  r.accuracy  =  0.01, size = 5) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 8, height = 8, dpi = 600)

# 법정동에 따른 연소득당 거래금액 산점도
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "법정동에 따른 연소득당 거래금액 산점도")

ggpubr::ggscatter(
  dataL2, x = "meanCost", y = "거래금액", color = "d2"
  , add = "reg.line", conf.int = TRUE, scales = "free_x"
  , facet.by = "d2"
  , add.params = list(color = "black", fill = "lightgray")
) +
  labs(
    title = NULL
    , x = "연소득"
    , y = "거래금액"
    , color = NULL
    , subtitle = "법정동에 따른 연소득당 거래금액 산점도"
  ) +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 0.95) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.85, p.accuracy  =  0.01,  r.accuracy  =  0.01) +
  theme(text = element_text(size = 14)) +
  ggsave(filename = saveImg, width = 12, height = 15, dpi = 600)

#***********************************************
# 지도 그리기
#***********************************************
addrList = dataL2$addr %>% unique() %>% sort() %>%
  as.tibble()

# 구글 API 하루 제한
# addrData = ggmap::mutate_geocode(addrList, value, source = "google")

# 각 주소에 따라 위/경도 반환
# for (i in 1:nrow(addrList)) {
#   addrData = ggmap::mutate_geocode(addrList[i, 'value'], value, source = "google")
# 
#   if (nrow(addrData) < 1) { next }
# 
#   readr::write_csv(x = addrData, file = saveFile, append = TRUE)
# }

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "seoul apartment transaction-addrData")
addrData =  readr::read_csv(file = saveFile, col_names = c("value", "lon", "lat"))

dataL4 = dataL2 %>% 
  dplyr::left_join(addrData, by = c("addr" = "value")) %>% 
  dplyr::filter(
    ! is.na(lon)
    , ! is.na(lat)
    , dplyr::between(lon, 120, 130)
    , dplyr::between(lat, 30, 40)
  ) %>% 
  dplyr::group_by(lon, lat, addr) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  )


map = ggmap::get_map(
  location = c(lon = mean(dataL4$lon, na.rm = TRUE), lat = mean(dataL4$lat, na.rm = TRUE))
  , zoom = 12
)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "연소득당 거래금액 지도 매핑")

ggmap(map, extent = "device") +
  geom_point(data = dataL4, aes(x = lon, y = lat, color = meanVal, size = meanVal, alpha = 0.3)) +
  scale_color_gradientn(colours = cbMatlab, na.value = NA) +
  labs(
    subtitle = NULL
    , x = NULL
    , y = NULL
    , fill = NULL
    , colour = NULL
    , title = NULL
    , size = NULL
  ) +
  scale_alpha(guide = 'none') +
  theme(
    text = element_text(size = 18)
  ) +
  ggsave(filename = saveImg, width = 10, height = 10, dpi = 600)


#***********************************************
# 주택 가격 결정 요인을 위한 회귀분석
#***********************************************
dataL4 = dataL2 %>%
  dplyr::select(건축년도, 전용면적, 층, val2, d2, val)

#+++++++++++++++++++++++++++++++++++++++++++++++
# 전체 아파트
dataL5 = dataL4

# 중형 이상 아파트 (66 m2 이상)
dataL5 = dataL4 %>% 
  dplyr::filter(전용면적 >= 66) %>% 
  dplyr::select(-전용면적)

# 소형 아파트 (66 m2 미만)
dataL5 = dataL4 %>% 
  dplyr::filter(전용면적 < 66) %>% 
  dplyr::select(-전용면적)
#+++++++++++++++++++++++++++++++++++++++++++++++

# 선형회귀분석
lmFit = lm(val ~ ., data = dataL5)
summary(lmFit)

# 단계별 소거법
lmFitStep = MASS::stepAIC(lmFit, direction = "both")
summary(lmFitStep)

# Beta 회귀계수
lmBetaFit = lm.beta::lm.beta(lmFitStep)
lmBetaFit$standardized.coefficients %>% round(2) %>% sort() %>% rev()

# 산점도 그림
validData = data.frame(
  xAxis = predict(lmFitStep)
  , yAxis = dataL5$val
  # , type = "전체 아파트"
  # , type = "중형 아파트"
  , type = "소형 아파트"
  
)

# corVal = cor(validData$xAxis, validData$yAxis)
biasVal = Metrics::bias(validData$xAxis, validData$yAxis)
rmseVal = Metrics::rmse(validData$xAxis, validData$yAxis)

# 전체 아파트에 대한 주택가격 결정요인 (연소득당 거래금액) 예측 산점도
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "전체 아파트에 대한 주택가격 결정요인 예측 산점도")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "중형 아파트에 대한 주택가격 결정요인 예측 산점도")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "소형 아파트에 대한 주택가격 결정요인 예측 산점도")

ggscatter(
  validData, x = "xAxis", y = "yAxis", color = "black"
  , add = "reg.line", conf.int = TRUE
  , facet.by = "type"
  , add.params = list(color = "blue", fill = "lightblue")
) +
  theme_bw() +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 1.0, size = 4) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.9, size = 4) +
  ggpp::annotate("text_npc", npcx = 0.05, npcy = 0.8, label = sprintf("Bias = %s", round(biasVal, 2)), hjust = 0, size = 4) +
  ggpp::annotate("text_npc", npcx = 0.05, npcy = 0.7, label = sprintf("RMSE = %s", round(rmseVal, 2)), hjust = 0, size = 4) +
  # ggpp::annotate("text_npc", npcx = 0.05, npcy = 0.60, label = sprintf("Bias = %s", round(biasVal, 2)), hjust = 0, size = 4) +
  # ggpp::annotate("text_npc", npcx = 0.05, npcy = 0.55, label = sprintf("RMSE = %s", round(rmseVal, 2)), hjust = 0, size = 4) +
  labs(
    title = NULL
    , x = "예측"
    , y = "실측"
    # , subtitle = "전체 아파트에 대한 주택가격 결정요인 예측 산점도"
    # , subtitle = "중형 아파트에 대한 주택가격 결정요인 예측 산점도"
    , subtitle = "소형 아파트에 대한 주택가격 결정요인 예측 산점도"
  ) +
  theme(text = element_text(size = 16)) +
  ggsave(filename = saveImg, width = 6, height = 6, dpi = 600)


# 주택 가격 결정 요인을 위한 관계성
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "주택 가격 결정 요인을 위한 관계성")

dataL2 %>%
  dplyr::select(건축년도, 전용면적, 층, val2, val) %>% 
  dplyr::rename(
    "면적당거래금액" = val2
    , "연소득당거래금액" = val
  ) %>% 
  GGally::ggpairs(.) +
  theme(text = element_text(size = 18))

ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)

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
# R을 이용한 서울시 아파트 실거래가 회귀분석 및 주택 가격 결정 요인

# 데이터 및 데이터베이스 구조 설명 (데이터, 테이블 등)
# 데이터 처리과정 설명 (데이터 import/export 과정, SQL 문장 등)
# 데이터 요약 (요약통계량, 표/그래프 활용)
# 데이터 분석 (데이터 분석 기법 활용)
# 결론

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."
# globalVar$mapPath = "."
# globalVar$dbPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0167"

#================================================
# Main
#================================================
library(ggplot2)
library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)
library(RCurl)
library(readr)
library(magrittr)
library(ggrepel)
library(colorRamps)
library(ggpubr)
library(lm.beta)
library(ggpmisc)
library(DBI)
library(RSQLite)

cbMatlab = colorRamps::matlab.like(11)

dbInfo = sprintf("%s/%s_%s.sqlite", globalVar$dbPath, serviceName, "db4")

# DB에 연결하기
con = DBI::dbConnect(SQLite(), dbname = dbInfo)

#***********************************************
# 공공데이터포털 API (자료 수집)
# - 서울특별시 아파트 실거래 데이터
#***********************************************
# 공공데이터포털 API키
# reqDataKey = globalVar$dataKey
# reqDataKey = "Ftj0WhfmnXN86rrVCPTGvlQJ%oJs9l+ZQjJzPgtc37yVPWuXs8UOP3kD2lTyy9DFInQZj2VvYFH1+Uh7gNgTLLA=="
# 요청 URL
# reqUrl = "http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade"
# 요청 키
# reqKey = stringr::str_c("?serviceKey=", RCurl::curlEscape(stringr::str_conv(reqDataKey, encoding = "UTF-8")))
# 날짜 기간
# dtDateList = seq(as.Date("2017-01-01"), as.Date(format(Sys.time(), "%Y-%m-%d")), "1 month")
# codeDistList = data.frame(
#   emdCd = c(11000, 11110, 11140, 11170)
# )
# dataL1 = tibble::tibble()
# 
# for (i in 1:length(dtDateList)) {
#   for (j in 1:nrow(codeDistList)) {
#     
#     sDate = format(dtDateList[i], "%Y%m")
#     
#     # 요청 법정동
#     reqLawdCd = stringr::str_c("&LAWD_CD=", codeDistList[j, 'emdCd'])
#     
#     # 요청 날짜
#     reqYmd = stringr::str_c("&DEAL_YMD=", sDate)
#     
#     resData = httr::GET(
#       stringr::str_c(reqUrl, reqKey, reqLawdCd, reqYmd)
#     ) %>%
#       httr::content(as = "text", encoding = "UTF-8") %>%
#       jsonlite::fromJSON()
#     
#     resCode = resData$response$header$resultCode
#     if (resCode != "00") { next }
#     
#     resItems = resData$response$body$items
#     if (resItems == "") { next }
#     
#     cat(sprintf(
#       "dtDate : %10s | code : %5s"
#       , sDate
#       , codeList[j, 'emdCd']
#     ), "\n")
#     
#     resItem = resItems$item %>%
#       as.data.frame()
#     # readr::type_convert()
#     
#     dataL1 = dplyr::bind_rows(
#       dataL1
#       , data.frame(
#         'dtYm' = sDate
#         , 'emdCd' = codeDistList[j, 'emdCd']
#         , resItem
#       )
#     )
#   }
# }

#***********************************************
# 자료 저장
#***********************************************
# saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "seoul apartment transaction.csv")
# readr::write_csv(x = dataL1, file = saveFile)

#*************************************************
# 데이터 처리과정 설명 (데이터 import/export 과정, SQL 문장 등)
#*************************************************

#++++++++++++++++++++++++++++++++++++++
# 법정동 코드
#++++++++++++++++++++++++++++++++++++++
# 데이터 읽기
codeInfo = Sys.glob(paste(globalVar$mapPath, "/admCode/법정동코드_전체자료.txt", sep = "/"))
tbCode = readr::read_delim(codeInfo, delim = "\t", locale = locale("ko", encoding = "EUC-KR"), col_types = "ccc")

# 테이블 생성 
rs = DBI::dbSendQuery(
  con
  , "CREATE TABLE IF NOT EXISTS TB_CODE 
    (
    CODE_ID INTEGER PRIMARY KEY AUTOINCREMENT
    , 법정동코드 TEXT 
    , 법정동명 TEXT
    , 폐지여부 TEXT
  )"
)

DBI::dbClearResult(rs)

# 데이터 삽입
DBI::dbWriteTable(con, "TB_CODE", tbCode, row.names = FALSE, append = TRUE)

# 데이터 확인
sqlCodeList = DBI::dbGetQuery(con, "SELECT * FROM TB_CODE")

#++++++++++++++++++++++++++++++++++++++
# 아파트 실거래 데이터
#++++++++++++++++++++++++++++++++++++++
# 데이터 읽기
fileInfo = Sys.glob(paste(globalVar$outPath, "LSH0169_seoul apartment transaction.csv", sep = "/"))
tbApa = readr::read_csv(file = fileInfo) %>% 
  readr::type_convert() %>% 
  dplyr::rename(
    날짜 = dtYm
    , 법정동코드 = emdCd
  )

# 테이블 생성
rs = DBI::dbSendQuery(
  con
  , "CREATE TABLE IF NOT EXISTS TB_APA 
    (
    APA_ID INTEGER PRIMARY KEY AUTOINCREMENT
    , 날짜 NUMERIC 
    , 법정동코드 NUMERIC
    , 거래금액 NUMERIC
    , 건축년도 NUMERIC
    , 년 NUMERIC
    , 법정동 TEXT
    , 아파트 TEXT
    , 월 NUMERIC
    , 일 NUMERIC
    , 전용면적 NUMERIC
    , 지번 TEXT
    , 지역코드 NUMERIC
    , 층 NUMERIC
    , 해제사유발생일 TEXT
    , 해제여부 TEXT
  )"
)

DBI::dbClearResult(rs)

# 데이터 삽입
DBI::dbWriteTable(con, "TB_APA", tbApa, row.names = FALSE, append = TRUE)

# 테이블 확인
sqlApa = DBI::dbGetQuery(con, "SELECT * FROM TB_APA")

#++++++++++++++++++++++++++++++++++++++
# 법정동 소득 데이터
#++++++++++++++++++++++++++++++++++++++
# 데이터 읽기
fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0169_가구_특성정보_(+소득정보)_201211.csv", sep = "/"))
tbCost = readr::read_csv(file = fileInfo) %>% 
  magrittr::set_colnames(c("행정동", "행정동코드", "법정동", "법정동코드", "총인구수", "총가구수", "가구당인구수", "평균소득금액"))

# 테이블 생성
rs = DBI::dbSendQuery(
  con
  , "CREATE TABLE IF NOT EXISTS TB_COST 
    (
    COST_ID INTEGER PRIMARY KEY AUTOINCREMENT
    , 행정동 TEXT 
    , 행정동코드 NUMERIC
    , 법정동 TEXT
    , 법정동코드 NUMERIC
    , 총인구수 NUMERIC
    , 총가구수 NUMERIC
    , 가구당인구수 NUMERIC
    , 평균소득금액 NUMERIC
  )"
)

DBI::dbClearResult(rs)

# 데이터 삽입
DBI::dbWriteTable(con, "TB_COST", tbCost, row.names = FALSE, append = TRUE)

# 테이블 확인
sqlCost = DBI::dbGetQuery(con, "SELECT * FROM TB_COST")

#***********************************************
# 개별 데이터 전처리
#***********************************************

codeList = sqlCodeList %>% 
  magrittr::set_colnames(c("ID", "EMD_CD", "addr", "isUse")) %>% 
  tidyr::separate(col = "addr", into = c("d1", "d2", "d3", "d4"), sep = " ") %>%
  dplyr::mutate(
    emdCd = stringr::str_sub(EMD_CD, 1, 5)
  ) %>%
  dplyr::filter(
    stringr::str_detect(d1, regex("서울특별시"))
    , stringr::str_detect(isUse, regex("존재"))
    , is.na(d3)
    , is.na(d4)
  ) %>% 
  as.tibble()

codeDistList = codeList %>%
  dplyr::distinct(emdCd)

apaData = sqlApa %>% 
  dplyr::rename(
    dtYm = 날짜
    , emdCd = 법정동코드
  ) %>% 
  dplyr::mutate(
    지번2 = readr::parse_number(지번)
    , emdCd = as.character(emdCd)
  ) %>% 
  as.tibble()

costData = sqlCost %>%
  dplyr::rename(
    raw_dn_cd = 법정동코드
    , avrg_income_amount_am = 평균소득금액
  ) %>% 
  dplyr::mutate(
    emdCd = stringr::str_sub(as.character(raw_dn_cd), 1, 5)
  )  %>% 
  dplyr::group_by(emdCd) %>%
  dplyr::summarise(
    meanCost = mean(avrg_income_amount_am, na.rm = TRUE)
  ) %>% 
  as.tibble()


#***********************************************
# 통합 데이터 전처리
#***********************************************
dataL2 = apaData %>% 
  dplyr::left_join(codeList, by = c("emdCd" = "emdCd")) %>%
  dplyr::left_join(costData, by = c("emdCd" = "emdCd")) %>% 
  dplyr::mutate(
    addr = stringr::str_trim(paste(d1, d2, 아파트, 지번, seq = ' '))
    , val = 거래금액 / meanCost # 연소득당 거래금액
    , val2 = 거래금액 / 전용면적 # 면적당 거래금액
  )

dataL3 = dataL2 %>% 
  dplyr::group_by(d2) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  )

saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "dataL2.csv")
readr::write_csv(x = dataL2, file = saveFile)

saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "dataL3.csv")
readr::write_csv(x = dataL3, file = saveFile)


#***********************************************
# 데이터 요약 (요약통계량)
#***********************************************
# 연소득당 거래금액 따른 기초 통계량
dataL2 %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE) # 평균값
    , medianVal = median(val, na.rm = TRUE) # 중앙값
    , sdVal = sd(val, na.rm = TRUE) # 표준편차
    , maxVal = max(val, na.rm = TRUE) # 최대값
    , minVal = min(val, na.rm = TRUE) # 최소값
    , cnt = n() # 개수
  ) %>%
  dplyr::arrange(desc(meanVal))

# 법정동에 따른 연소득당 거래금액 따른 기초 통계량
dataL2 %>%
  dplyr::group_by(d2) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE) # 평균값
    , medianVal = median(val, na.rm = TRUE) # 중앙값
    , sdVal = sd(val, na.rm = TRUE) # 표준편차
    , maxVal = max(val, na.rm = TRUE) # 최대값
    , minVal = min(val, na.rm = TRUE) # 최소값
    , cnt = n() # 개수
  ) %>%
  dplyr::arrange(desc(meanVal))

#*******************************************************
# 데이터 요약 (표/그래프 활용)
#*******************************************************
# 연소득당 거래금액 따른 히스토그램
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "연소득당 거래금액 따른 히스토그램")

ggplot(dataL2, aes(x = val)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = 0.2) +
  geom_rug(aes(x = val, y = 0), position = position_jitter(height = 0)) +
  labs(x = "연소득당 거래금액", y = "밀도 함수", colour = NULL, fill = NULL, subtitle = "연소득당 거래금액 따른 히스토그램") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 12, height = 6, dpi = 600)

# 법정동에 따른 연소득당 거래금액 히스토그램
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "법정동에 따른 연소득당 거래금액 히스토그램")

ggplot(dataL3, aes(x = d2, y = meanVal, fill = meanVal)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = round(meanVal, 0)), vjust = 1.6, color = "white", size = 4) +
  labs(x = "법정동", y = "연소득당 거래금액", fill = NULL, subtitle = "법정동에 따른 연소득당 거래금액 히스토그램") +
  scale_fill_gradientn(colours = cbMatlab, na.value = NA) +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)


# 연소득당 거래금액 따른 상자 그림
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "연소득당 거래금액 따른 상자 그림")

ggplot(dataL2, aes(y = val)) +
  geom_boxplot() +
  labs(x = NULL, y = "연소득당 거래금액", colour = NULL, fill = NULL, subtitle = "연소득당 거래금액 따른 상자 그림") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 12, height = 6, dpi = 600)

# 법정동에 따른 연소득당 거래금액 상자 그림
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "법정동에 따른 연소득당 거래금액 상자 그림")

ggplot(dataL2, aes(x = d2, y = val, color = d2)) +
  geom_boxplot() +
  labs(x = "법정동", y = "연소득당 거래금액", fill = NULL, subtitle = "법정동에 따른 연소득당 거래금액 상자 그림") +
  # scale_colour_gradientn(colours = cbMatlab, na.value = NA) +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)

# 연소득당 거래금액 산점도
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "연소득당 거래금액 산점도")

ggpubr::ggscatter(
  dataL2, x = "meanCost", y = "거래금액"
  , add = "reg.line", conf.int = TRUE, scales = "free_x"
  # , facet.by = "전체 법정동"
  , add.params = list(color = "blue", fill = "lightblue")
) +
  labs(
    title = NULL
    , x = "연소득"
    , y = "거래금액"
    , color = NULL
    , subtitle = "연소득당 거래금액 산점도"
  ) +
  theme_bw() +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 1.0, size = 5) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.90, p.accuracy  =  0.01,  r.accuracy  =  0.01, size = 5) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 8, height = 8, dpi = 600)

#*******************************************************
# 데이터 분석 (데이터 분석 기법 활용)
#*******************************************************
# 주택 가격 결정 요인을 위한 회귀분석
dataL4 = dataL2 %>%
  dplyr::select(건축년도, 전용면적, 층, val2, d2, val)


# 주택 가격 결정 요인을 위한 관계성
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "주택 가격 결정 요인을 위한 관계성")

dataL4 %>% 
  dplyr::rename(
    "면적당거래금액" = val2
    , "연소득당거래금액" = val
  ) %>% 
  GGally::ggpairs(.) +
  theme(text = element_text(size = 18))

ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)


#+++++++++++++++++++++++++++++++++++++++++++++++
# 전체 아파트
dataL5 = dataL4

# # 중형 이상 아파트 (66 m2 이상)
# dataL5 = dataL4 %>% 
#   dplyr::filter(전용면적 >= 66) %>% 
#   dplyr::select(-전용면적)
# 
# # 소형 아파트 (66 m2 미만)
# dataL5 = dataL4 %>% 
#   dplyr::filter(전용면적 < 66) %>% 
#   dplyr::select(-전용면적)
#+++++++++++++++++++++++++++++++++++++++++++++++

# 선형회귀분석
lmFit = lm(val ~ ., data = dataL5)
summary(lmFit)

# 단계별 소거법
lmFitStep = MASS::stepAIC(lmFit, direction = "both")
summary(lmFitStep)

# Beta 회귀계수
lmBetaFit = lm.beta::lm.beta(lmFitStep)
lmBetaFit$standardized.coefficients %>% round(2) %>% sort() %>% rev()

# 산점도 그림
validData = data.frame(
  xAxis = predict(lmFitStep)
  , yAxis = dataL5$val
  , type = "전체 아파트"
)

# corVal = cor(validData$xAxis, validData$yAxis)
biasVal = Metrics::bias(validData$xAxis, validData$yAxis)
rmseVal = Metrics::rmse(validData$xAxis, validData$yAxis)

# 전체 아파트에 대한 주택가격 결정요인 (연소득당 거래금액) 예측 산점도
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "전체 아파트에 대한 주택가격 결정요인 예측 산점도")

ggscatter(
  validData, x = "xAxis", y = "yAxis", color = "black"
  , add = "reg.line", conf.int = TRUE
  , facet.by = "type"
  , add.params = list(color = "blue", fill = "lightblue")
) +
  theme_bw() +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 1.0, size = 4) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.9, size = 4) +
  ggpp::annotate("text_npc", npcx = 0.05, npcy = 0.8, label = sprintf("Bias = %s", round(biasVal, 2)), hjust = 0, size = 4) +
  ggpp::annotate("text_npc", npcx = 0.05, npcy = 0.7, label = sprintf("RMSE = %s", round(rmseVal, 2)), hjust = 0, size = 4) +
  labs(
    title = NULL
    , x = "예측"
    , y = "실측"
    , subtitle = "전체 아파트에 대한 주택가격 결정요인 예측 산점도"
  ) +
  theme(text = element_text(size = 16)) +
  ggsave(filename = saveImg, width = 6, height = 6, dpi = 600)


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
# R을 이용한 기후 지표 (ClimInd) 계산

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."
# globalVar$mapPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0168"

#================================================
# Main
#================================================
library(ggplot2)
library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)
library(RCurl)
library(readr)
library(magrittr)
library(ggrepel)
library(colorRamps)
library(ggpubr)
library(ClimInd)

# showtext::showtext_opts(dpi = 100)
# showtext::showtext.auto()

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0168_LSH0168_Fix+LSTM+pr+CanESM5+historical+FIX.csv", sep = "/"))
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))

dataL1 = data %>% 
  dplyr::mutate(
    dtDate = readr::parse_date(as.character(PERIOD), "%Y-%m-%d")
    , sDate = format(dtDate, "%m/%d/%y")
  )


dataL2 = tibble()

# i = "강릉"
for (i in names(dataL1)) {
  if (i == "PERIOD" | i == "dtDate" | i == "sDate") next
  
  val = dataL1[i] %>% unlist()
  
  if (length(val) < 1) next
  
  names(val) = dataL1["sDate"] %>% unlist()
  
  resData = tibble(
    stationName = i
    , year = names(ClimInd::cdd(data = val))
    , d50mm = ClimInd::d50mm(data = val)
    , d95p = ClimInd::d95p(data = val)
    , dr1mm = ClimInd::dr1mm(data = val)
    , dr3mm = ClimInd::dr3mm(data = val)
    , prcptot = ClimInd::prcptot(data = val)
    , r10mm = ClimInd::r10mm(data = val)
    , r20mm = ClimInd::r20mm(data = val)
    , rti = ClimInd::rti(data = val)
    , rx1day = ClimInd::rx1day(data = val)
    , rx5d = ClimInd::rx5d(data = val)
    , sdii = ClimInd::sdii(data = val)
    , cdd = ClimInd::cdd(data = val)
    , cwd = ClimInd::cwd(data = val)
  )
  
  dataL2 = dplyr::bind_rows(dataL2, resData)
}

saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "climate-index.csv")
readr::write_csv(x = dataL2, file = saveFile)

dataL3 = dataL2 %>% 
  tidyr::gather(-stationName, -year, key = "key", value = "val") %>% 
  tidyr::spread(key = "stationName", value = "val")

saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "climate-index-format.csv")
readr::write_csv(x = dataL3, file = saveFile)


#================================================
# 요구사항
#================================================
# R을 이용한 튼살 화장품 마케팅 분석

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."
# globalVar$mapPath = "."

serviceName = "LSH0172"

#================================================
# Main
#================================================
library(ggplot2)
library(tidyverse)
library(tm)
library(wordcloud)
library(wordcloud2)
library(data.table)
library(stringr)
library(qgraph)
library(ggplot2)
library(SnowballC)
library(KoNLP)
library(parallel)
library(topicmodels)
library(lda)
library(qgraph)


useNIADic()

# showtext::showtext_opts(dpi = 100)
# showtext::showtext.auto()

#
#튼살화장품 분석용 단어 사전에 추가
#
mergeUserDic(data.frame(c("비오템", "클라란스", "프라젠트라"), "ncn"))
mergeUserDic(data.frame(c("플라젠트라", "몽디에스", "아토팜"), "ncn"))
mergeUserDic(data.frame(c("출산용품", "세타필", "임부"), "ncn"))
mergeUserDic(data.frame(c("임산부", "튼살", "바디워시"), "ncn"))
mergeUserDic(data.frame(c("수딩젤", "튼살크림", "베이비로션"), "ncn"))
mergeUserDic(data.frame(c("바스", "베이비크림", "biotherm"), "ncn"))
mergeUserDic(data.frame(c("clarins", "atopalm", "mongdies"), "ncn"))
mergeUserDic(data.frame(c("팔머스", "팔머즈","farmers"), "ncn"))
mergeUserDic(data.frame(c("plagentra","베이비크림", "파머스"), "ncn"))
mergeUserDic(data.frame(c( "아르간", "유해성분", "유발성분"), "ncn"))
mergeUserDic(data.frame(c("아르간오일", "리에락","lierac"), "ncn"))
mergeUserDic(data.frame(c("nuxe", "눅스", "세일"), "ncn"))
mergeUserDic(data.frame(c("쎄타필", "세타필", "cetaphil"), "ncn"))
mergeUserDic(data.frame(c("대용량", "소용량", "위험성분"), "ncn"))
mergeUserDic(data.frame(c("위해성분", "무해성분", "보습성분"), "ncn"))
mergeUserDic(data.frame(c("천연성분", "스테로이드", "폴리아크릴아마이드"), "ncn"))
mergeUserDic(data.frame(c("유투버", "뷰티유투버", "프리미엄"), "ncn"))
mergeUserDic(data.frame(c("premium", "보습", "보습력"), "ncn"))
mergeUserDic(data.frame(c("콜라겐", "부신피질", "호르몬"), "ncn"))

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0172_13주차_튼살화장품분석실습.RData", sep = "/"))
load(file=fileInfo)

# v_data <- read.csv("cosmetic_data.csv")
v_data <- v_data[[1]]
v_data_bak <- v_data

# 속도 향상을 위한 병렬처리
# options(mc.cores=12)
#
# 원하는 단어 포함된 벡터 만들기
v_base <- v_data[grepl("*튼살*|*튼 살*", v_data)]
v_tr <- v_data[grepl("*튼살치료*|*튼살 치료*|*튼살완화*|*튼살 완화*", v_data)]
v_biotem <- v_data[grepl("*비오템*|*biotherm*", v_data)]
v_cla <- v_data[grepl("*클라란스*|*clarins*", v_data)]
v_pla <- v_data[grepl("*프라젠트라*|*plagentra*|*플라젠트라*", v_data)]
v_ato <- v_data[grepl("*아토팜*|*atopalm*", v_data)]
v_mon <- v_data[grepl("*몽디에스*|*mongdies*", v_data)]

# # 분석 데이터셋 설정
# vec_blog <- v_tr
# base_name <- "튼살"
vec_blog <- v_pla
base_name <- "프라젠트라"


# 코퍼스 만들기
docs<- Corpus(VectorSource(vec_blog))

# 전처리 수행
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removePunctuation)

# URL 제거 함수 작성 후 말뭉치(Corpus)에서 URL 제거
removeURL<-function(x) gsub("http[^[:space:]]*", "", x)
docs <- tm_map(docs, content_transformer(removeURL))

# 명사 추출
docs <- tm_map(docs, extractNoun)
docs_tr <- docs

# 텍스트 전처리
docs <- docs_biotem

# 한글, 영문, 숫자, 공백문자를 제외한 나머지 문자 삭제
docs <- tm_map(docs, content_transformer(function(x) {
  gsub("[^0-9a-zA-Z가-? ]", "", x)}))

# 코퍼스 content 문자열의 첫번째 영문자 c 삭제(왜 생기는지 모르겠음)
docs <- tm_map(docs, content_transformer(function(x) {gsub("^c", "", x)}))


# rm.word에 삭제할 단어 추가 후 for 문으로 단어 삭제.
# 여기서 반복하여 전처리를 꼼꼼히 해야함
rm.word <- c("언니", "기타", "해서")
rm.word <- c(rm.word, "이거", "정도", "보기")
rm.word <- c(rm.word, "번역", "본문", "복사")
rm.word <- c(rm.word, "하지", "번역", "도착")
rm.word <- c(rm.word, "들이", "때문", "하기")
rm.word <- c(rm.word, "진짜", "하면", "2018")
rm.word <- c(rm.word, "우리", "사실", "시작")
rm.word <- c(rm.word, "출발", "오늘", "추가")
rm.word <- c(rm.word, "바울", "처음", "생각")
rm.word <- c(rm.word, "요트", "사진", "가지")
rm.word <- c(rm.word, "이번", "제품", "ampgtamplt")
rm.word <- c(rm.word, "여기", "추천", "너무")
rm.word <- c(rm.word, "하루", "하나", "사람")
rm.word <- c(rm.word, "하게", "하다", "사용")

for (word in rm.word) {
  docs <- tm_map(docs, 
                 content_transformer(function(x) {gsub(word, "", x)}))
}


#단어문서 행렬 작성
tdm <- TermDocumentMatrix(docs, 
                          control = list(wordLengths = c(4, 12)))

#tdm #term-document 매트릭스 확인


doc.matrix <- as.matrix(tdm) #tdm을 매트릭스로 변환
doc.row <- rowSums(doc.matrix) #단어 빈도 구하기

#단어 빈도 높은 순서로 정렬
word.order <- order(doc.row, decreasing=TRUE)
#빈출 순서로 단어 30개 매트릭스 구하기
freq.words <- doc.matrix[word.order[1:50], ]

#전치 행렬 곱셈을 통해 관계 매트릭스 생성
co.matrix <- freq.words %*% t(freq.words)


#빈출단어 그래프 작성


#각 단어의 사용횟수 기록
term.freq <- rowSums(as.matrix(tdm))

# 원하는 횟수 이상 사용된 단어만 다시 저장
term.freq1 <- subset(term.freq, term.freq >= 100)
df_term_freq1 <- data.frame(term = names(term.freq1), freq = term.freq1)

# 빈도 막대그래프 그리기
ggplot(df_term_freq1, aes(x=reorder(term, freq), y=freq)) + 
  geom_bar(stat="identity") + 
  xlab("빈출단어") + 
  ylab("빈도") +
  coord_flip() + 
  theme(axis.text=element_text(size=12))

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "프라젠트라_빈도막대그래프")

# 빈도 막대그래프 저장
ggsave(
  # paste0("비오템_빈도막대그래프.png"),
  saveImg, 
  plot = last_plot(),
  device = NULL,
  #path = "d:/work",
  scale = 1,
  width = NA,
  height = NA,
  #units = c("in", "cm", "mm"),
  dpi = 600,
  limitsize = TRUE
)

# 워드클라우드2 그리기
term.freq3 <- subset(term.freq, term.freq >= 30)
df_term_freq3 <- data.frame(term = names(term.freq3), freq = term.freq3)

wordcloud2(df_term_freq3,
           size = 0.7,
           color = "random-light",
           rotateRatio = 10,
           backgroundColor = "grey")

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "프라젠트라_워드클라우드2")

ggsave(
  # paste0("비오템_워드클라우드2.png"),
  saveImg,
  plot = last_plot(),
  device = NULL,
  #path = "d:/work",
  scale = 1,
  width = NA,
  height = NA,
  #units = c("in", "cm", "mm"),
  dpi = 600,
  limitsize = TRUE
)




base_name <- "프라젠트라"

# tdm을 dtm으로 변경
dtm <- as.DocumentTermMatrix(tdm)

# 토픽 갯수 설정
topic_num <- 6
# 개별 토픽에서 출현빈도 기준 상위 n개 단어 지정. 여기서는 20개로 지정함
w_num <- 20

# seed를 특정 숫자로 설정하여 반복 실행시 동일한 값이 나오도록 함
set.seed(123)
ldaform<-dtm2ldaformat(dtm, omit_empty = F)

start = Sys.time()
# Gibbs sampling은 간단히 말하면 변수를 하나씩 바꿔가면서 표본을 수집하여 모형을 근사하는 방식
# 이때, burnin은 처음부터 burnin에 지정한 값까지는 제외시키고 나머지 값만을 모형 근사에 사용하겠다는 선언
# keep은 정한 값마다 log likelihood 값을 구해서 저장하도록 설정하는 변수
# iterations는 사후확률의 업데이트 횟수
# burnin은 확률 추정시 제외되는 초반부 이터레이션 값
# alpha는 문서내에서 토픽들의 확률분포(1을 주면 유니폼 분포)
# eta: 한 토픽 내에 단어들의 확률분포
result.lda <- lda.collapsed.gibbs.sampler(ldaform$documents,
                                          K = topic_num,
                                          vocab = ldaform$vocab,
                                          num.iterations = 10000,
                                          burnin = 100,
                                          alpha = 0.01,
                                          eta = 0.01)

end <- Sys.time()
end - start

# 토픽 상위 20개 가져오기
top_topic <- top.topic.words(result.lda$topics,20,by.score=T)

## 비율로 변환 및 추가
theta = rowSums(result.lda$document_sums)
topic.proportion = theta/sum(theta)

#
# 토픽내에 단어별 출현 확률 계산
#

top.words = top.topic.words(result.lda$topics, w_num)

# 빈도 상위단어 데이터 타입을 문자형으로 변환
c_top.words <- as.character(top.words)

# 추출된 상위단어의 토픽 선정하여 new.topics에 저장
new.topics <- subset(result.lda$topics,select=c_top.words)
count_by_words<-new.topics

#count 합계 함수
a=1
k=0
for(j in 1:ncol(new.topics))
{
  if(a * w_num + 1 == j)
  {
    k <- w_num * a
    a <- a + 1
  }
  count_by_words[a,j-k] <- count_by_words[a,j]
}

# 비율구하기
proportion_by_words <- t(count_by_words[,1:w_num]/
                           as.integer(result.lda$topic_sums))

#단어와 비율을 연결하여 행렬 생성
result <- matrix(paste(top.topic.words(result.lda$topics, w_num),
                       "(",proportion_by_words,")"),byrow = F, nrow = w_num)
output <- rbind(result,topic.proportion,t(result.lda$topic_sums))

# 토픽 빈도와 출현확률 출력
output


# LDA 분석 결과 파일로 저장
saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "top_topic")
write.csv(top_topic, file = saveFile, row.names = FALSE)

saveFile = sprintf("%s/%s_%s.csv", globalVar$outPath, serviceName, "topic_output")
write.csv(output, file = saveFile, row.names = FALSE)

#단어문서 행렬 작성
tdm <- TermDocumentMatrix(docs, 
                          control = list(wordLengths = c(4, 12)))

doc.matrix <- as.matrix(tdm) #tdm을 매트릭스로 변환
doc.row <- rowSums(doc.matrix) #단어 빈도 구하기

#단어 빈도 높은 순서로 정렬
word.order <- order(doc.row, decreasing=TRUE)
#빈출 순서로 단어 30개 매트릭스 구하기
freq.words <- doc.matrix[word.order[1:50], ]

#전치 행렬 곱셈을 통해 관계 매트릭스 생성
co.matrix <- freq.words %*% t(freq.words)

# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, paste0("전체 기능성튼살화장품: ", base_name))

# 네트웍 그래프 1: 전체 확인
qgraph(co.matrix, 
       normalize = T,
       labels=rownames(co.matrix),
       label.scale = TRUE,
       label.scale.equal = T,
       label.prop = 2,
       label.cex = 1,
       #edge.labels = TRUE,
       minimum = 150,
       #threshold = 200, # 지정 값 이하의 에지는 생략됨
       #cut = 100,
       #edge.label.bg = ifelse(edge >= 0.5, "red", "blue"),
       loopRotation = T,
       curveAll = T,
       fade = T,
       details = T,
       theme = "TeamFortress", # "classic", "colorblind", "gray", "Hollywood", "Borkulo", "gimme", "TeamFortress", "Reddit", "Leuven" or "Fried"
       borders = T,
       border.color = "#606060",
       shape = "square", # "circle", "square", "triangle", "diamond"
       vTrans =220, # 노드의 투명도 설정 0-255
       palette = 'pastel',
       rainbowStart = 0,
       #color = "green",
       title = paste0("기능성튼살화장품: ", base_name), 
       title.cex = 2,
       diag=F, 
       layout='spring', 
       #단어 빈도에 따라 원 크기 설정
       vsize=log(diag(co.matrix)) * 0.7 
) 


# 네트웍 그래프 2: 연결빈도가 높은 관계만 연결(threahold 값 조절)
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, paste0("높은빈도 기능성튼살화장품: ", base_name))

qgraph(co.matrix, 
       normalize = T,
       labels=rownames(co.matrix),
       label.scale = TRUE,
       label.scale.equal = T,
       label.prop = 1.8,
       label.cex = 1,
       #edge.labels = TRUE,
       minimum = 20,
       threshold = 300,
       #edge.label.bg = ifelse(edge >= 0.5, "red", "blue"),
       loopRotation = T,
       curveAll = T,
       fade = T,
       details = T,
       theme = "TeamFortress", # "classic", "colorblind", "gray", "Hollywood", "Borkulo", "gimme", "TeamFortress", "Reddit", "Leuven" or "Fried"
       borders = T,
       border.color = "#606060",
       shape = "circle", # "circle", "square", "triangle", "diamond"
       vTrans =220, # 노드의 투명도 설정 0-255
       palette = 'pastel',
       rainbowStart = 0,
       #color = "green",
       title = paste0("기능성튼살화장품: ", base_name), 
       title.cex = 2,
       diag=F, 
       layout='spring', 
       vsize=log(diag(co.matrix)) * 0.8 #단어 빈도에 따라 원 크기 설정
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

#================================================
# 요구사항
#================================================
# R을 이용한 Fortran 전처리 모듈 수정

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."
# globalVar$mapPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0176"

#================================================
# Main
#================================================
library(ggplot2)
library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)
library(RCurl)
library(readr)
library(magrittr)
library(ggrepel)
library(colorRamps)
library(ggpubr)
library(ClimInd)

# showtext::showtext_opts(dpi = 100)
# showtext::showtext.auto()

# data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR"))
# 
# dataL1 = data %>% 
#   dplyr::mutate(
#     dtDate = readr::parse_date(as.character(PERIOD), "%Y-%m-%d")
#     , sDate = format(dtDate, "%m/%d/%y")
#   )




########################################################################
# rm(list = ls()); gc()

# setting path where the data is
# path <- "F:/R/tu/tttt/tut/GCM/Interpolation act/dataset/LSTM/dataset/REA SSP245"
# setwd(path)

# input file name
# input_file_name1 <- "1 _historical"
# input_file_name2 <- "1 _Nearfut SSP245"
# 
# fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0168_LSH0168_Fix+LSTM+pr+CanESM5+historical+FIX.csv", sep = "/"))

# reading csv
# 1 --> left side input
# 2 --> right side input
# inputt1 <- read.csv(paste0(input_file_name1, ".csv"))
# inputt2 <- read.csv(paste0(input_file_name2, ".csv"))

fileInfo1 = Sys.glob(paste(globalVar$inpPath, "LSH0176_historical.csv", sep = "/"))
fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0176_Nearfut+SSP245.csv", sep = "/"))

inputt1 <- read.csv(fileInfo1)
inputt2 <- read.csv(fileInfo2)
inpul1<-inputt1[,]
input1<-inpul1[]
inpul2<-inputt2[,]
input2<-inpul2[,]

# how many models using at a time
model_num <- ncol(input1)-1

# setting specific indices to select
indices <- 1:10957

# deltaT matrix calculation
deltaT_mat <- data.frame(matrix(NA, 1, ncol(input1)))
colnames(deltaT_mat) <- c(colnames(input1)[1:model_num], "Ensemble")

for (i in 1:model_num) {
  deltaT_mat[1, i] <- -abs(mean(input2[indices, i]) - mean(input1[indices, i]))
}
deltaT_mat$Ensemble <- mean(as.numeric(deltaT_mat[1, 1:model_num]))

# Bi + Ri together
R_mat <- data.frame(matrix(NA, 1, model_num))
colnames(R_mat) <- paste0("B", 1:model_num)

# B1, 2, 3, 4 calculation
for (i in 1:model_num) {
  R_mat[1, i] <- mean(input1[indices, i]) - mean(input1[indices, 5])
}

# moving average calculation
mov_avg_list <- c()
for (i in 1:7306) { # (nrow(input1)-119)
  mov_avg_list <- c(mov_avg_list, mean(input1$X5[i:(i+3651)]))
}


mov_avg_diff <- max(mov_avg_list) - min(mov_avg_list)
# to make calculation easy setting 1st element of R1_avg as deltaT_Ensemble
R1_avg <- deltaT_mat[1, "Ensemble"]


#==================================
# Fortran을 이용한 반복 수행
# ==================================
# setwd("C:/Users/soooy/OneDrive/바탕 화면/Test")
setwd("E:/04. TalentPlatform/Github/TalentPlatform-R/src/fortran")

dimD = tibble(deltaT_mat) %>%
  tidyr::gather(key = "key", value = "val") %>%
  dplyr::select(val)

dimR = tibble(R_mat) %>%
  tidyr::gather(key = "key", value = "val") %>%
  dplyr::select(val)

dimAvg = tibble(R1_avg) %>%
  tidyr::gather(key = "key", value = "val") %>%
  dplyr::select(val)

dimMovAvg = tibble(mov_avg_diff) %>%
  tidyr::gather(key = "key", value = "val") %>%
  dplyr::select(val)

# utils::write.table(dimD, file = "C:/Users/soooy/OneDrive/바탕 화면/Test/input-dimD.dat", col.names = FALSE, row.names = FALSE)
# utils::write.table(dimR, file = "C:/Users/soooy/OneDrive/바탕 화면/Test/input-dimR.dat", col.names = FALSE, row.names = FALSE)
# utils::write.table(dimAvg, file = "C:/Users/soooy/OneDrive/바탕 화면/Test/input-dimAvg.dat", col.names = FALSE, row.names = FALSE)
# utils::write.table(dimMovAvg, file = "C:/Users/soooy/OneDrive/바탕 화면/Test/input-dimMovAvg.dat", col.names = FALSE, row.names = FALSE)

utils::write.table(dimD, file = "./input-dimD.dat", col.names = FALSE, row.names = FALSE)
utils::write.table(dimR, file = "./input-dimR.dat", col.names = FALSE, row.names = FALSE)
utils::write.table(dimAvg, file = "./input-dimAvg.dat", col.names = FALSE, row.names = FALSE)
utils::write.table(dimMovAvg, file = "./input-dimMovAvg.dat", col.names = FALSE, row.names = FALSE)


# getwd()
system(paste(
  "gfortran"
  , "./CallFortranInR_20210610.f90"
))

system(paste(
  "./a.exe"
))

# a<-read.table("C:/Users/soooy/OneDrive/바탕 화면/Test/result-dimR.dat")
# b<-read.table("C:/Users/soooy/OneDrive/바탕 화면/Test/result-dimD.dat")
# c<-read.table("C:/Users/soooy/OneDrive/바탕 화면/Test/result-dimAvg.dat")

a<-read.table("./result-dimR.dat")
b<-read.table("./result-dimD.dat")
c<-read.table("./result-dimAvg.dat")

# write.csv(a,"F:/R/tu/tttt/tut/GCM/Interpolation act/dataset/LSTM/REA/R_mat/NEAR/SSP245/SSP245 Near S1.csv")
# write.csv(b,"F:/R/tu/tttt/tut/GCM/Interpolation act/dataset/LSTM/REA/D_mat/NEAR/SSP245/SSP245 Near S1.csv")
# write.csv(c,"F:/R/tu/tttt/tut/GCM/Interpolation act/dataset/LSTM/REA/R_mat/NEAR/SSP245/SSP245 Near S1 AVG.csv")


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
# R을 이용한 이산화탄소 시계열 데이터 분석 (ARIMA, 평활법, 분해시계열) 및 보고서 작성

#================================================
# Set Env
#================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."
# globalVar$mapPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0162"

#================================================
# Main
#================================================
library(ggplot2)
library(tidyverse)
library(forecast)
library(GGally)
library(fpp2)
library(here)
library(tseries)

# 우리 행성의 역사를 통틀어 우리 행성은 기후의 많은 급격한 변화를 견뎌 왔습니다. 5 개의 간빙기 기간을 견디는 것부터 산업 혁명의 영향을 견디기까지 우리 지구와 대기는 인위적 영향으로 악용되었습니다. 
# 이 프로젝트 전체에서 Kaggle 웹 사이트를 통해 얻은 Mauna Loa Volcano의 데이터를 사용하여 대기 CO2 수준을 예측하는 ARIMA 모델을 만들었습니다.
# Mauna Loa 데이터 세트에는 이산화탄소 기록 및 계절별로 조정 된 기록과 함께 연도와 날짜가 포함됩니다.
# 데이터 세트와 이산화탄소 기록의 중요성은 연간 CO2 농도를 플롯 할 때 발견 된 사인파 패턴이 있다는 것입니다.
# 이러한 패턴은 연구자들에 의해 연구되었으며 식물과 나무의 계절별 성장 패턴을 따르는 것으로 밝혀졌습니다.
# 이는 잎과 식물이 떨어지고 자연적으로 CO2를 대기로 방출하기 때문에 겨울과 가을철에 CO2 농도가 증가하는 것으로 나타 났지만 잎이 자라는 봄과 여름철에는이 잎이 CO2를 적게 흡수하는 것으로 나타났습니다.
# 대기 CO2 농도. 또한, 현재 연구에 따르면 계절이 우리 대기 중 CO2 증가로 인해 일찍 시작되고 늦게 끝나는 것으로 나타났습니다. 이 연구에서 나는 이러한 정현파 패턴이 미래에 어떻게 변할 것인지 관찰하고 ARIMA 모델이 이러한 변화를 예측할 수있는 최상의 모델을 찾는 데 어떻게 도움을 줄 수 있는지 관찰 할 것입니다.

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0162_이산화탄소 농도_1999-2019.xlsx", sep = "/"))
data = openxlsx::read.xlsx(fileInfo, sheet = 1) %>% 
  tidyr::gather(-X1, key = "key", value = "val") %>% 
  dplyr::mutate(
    dtDate = readr::parse_date(paste(X1, key, sep = "-"), "%Y-%m월")
  ) %>% 
  dplyr::arrange(dtDate)

tsData = ts(data$val, start = c(1999, 1), frequency = 12)

# 시계열 안정성 진단 및 검정
# P값이 0.01로서 귀무가설 기각 (정상 시계열)
adf.test(tsData, alternative = c("stationary"), k = 0)
# Dickey-Fuller = -5.0135178, Lag order = 0, p-value = 0.01


# 자료에 대한 시계열 그래프 
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "1999-2019년 안면도에서 연도별 이산화탄소 농도 시계열")

autoplot(tsData) +
  geom_smooth(method = 'lm', se = TRUE) +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 1.0, size = 5) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.9, size = 5) +
  labs(
    x = "연도"
    , y = "이산화탄소 농도 [ppm]"
    , color = NULL
    , fill = NULL
    , subtitle = "1999-2019년 안면도에서 연도별 이산화탄소 농도 시계열"
  ) +
  theme(
    text = element_text(size = 18)
    , legend.position = "bottom"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


#******************************************
# (계절형) ARIMA 모형
#******************************************
# 시계열로부터 최종선택한 모형
bestModel = auto.arima(tsData, trace = TRUE)
# ARIMA(0,1,4)(1,1,2)[12]
# 비계절 : AR(0), 1차 차분, MA(4)
# 계절 : AR(1), 2차 차분 (24개월), MA(2)

# 모형의 통계적 유의성 검정
# X-squared = 0.071471882, df = 1, p-value = 0.7892057
Box.test(bestModel$residuals, lag = 1, type = "Ljung")

# 최종선택한 모형을 사용하여 구한 예측값(월별자료는 향후 12개월)을 나타내는 그래프
akimaData = forecast::forecast(bestModel, h = 12)

# 성능 비교
accuracy(akimaData)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "1999-2019년 안면도에서 연도별 이산화탄소 농도 시계열 + 향후 ARIMA 12개월 예측")

autoplot(akimaData) +
  geom_smooth(method = 'lm', se = TRUE) +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 1.0, size = 5) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.9, size = 5) +
  labs(
    x = "연도"
    , y = "이산화탄소 농도 [ppm]"
    , color = NULL
    , fill = NULL
    , subtitle = "1999-2019년 안면도에서 연도별 이산화탄소 농도 시계열 + 향후 ARIMA 12개월 예측"
  ) +
  theme(
    text = element_text(size = 18)
    , legend.position = "bottom"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


#******************************************
# 평활법
#******************************************
# 단순지수평활법(Simple Exponential Smoothing with 1 parameter)
# simpleModel = HoltWinters(tsData, alpha = 0.2, beta = FALSE, gamma = FALSE)

# 비계절적 Holt-Winters (with 2 parameter)
# multiModel = HoltWinters(tsData, gamma = FALSE)

# ETS 모델
etsModel = forecast::ets(tsData)

autoplot(etsModel)

etsData = forecast::forecast(etsModel, h = 12)

# 성능 비교
accuracy(etsData)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "1999-2019년 안면도에서 연도별 이산화탄소 농도 시계열 + 향후 ETS 12개월 예측")

autoplot(etsData) +
  geom_smooth(method = 'lm', se = TRUE) +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 1.0, size = 5) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.9, size = 5) +
  labs(
    x = "연도"
    , y = "이산화탄소 농도 [ppm]"
    , color = NULL
    , fill = NULL
    , subtitle = "1999-2019년 안면도에서 연도별 이산화탄소 농도 시계열 + 향후 ETS 12개월 예측"
  ) +
  theme(
    text = element_text(size = 18)
    , legend.position = "bottom"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)


#******************************************
# 분해시계열모형
#******************************************
# 시계열 요소 분해 시각화
autoplot(decompose(tsData))

# 계절 조정
decomp = stl(tsData, t.window=13, s.window="periodic", robust=TRUE)
seasonData = forecast(decomp)
# seasonData = forecast::seasadj(decomp)

# 성능 비교
accuracy(seasonData)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "1999-2019년 안면도에서 연도별 이산화탄소 농도 시계열 + 계절 조정")

autoplot(seasonData) +
  geom_smooth(method = 'lm', se = TRUE) +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 1.0, size = 5) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.9, size = 5) +
  labs(
    x = "연도"
    , y = "이산화탄소 농도 [ppm]"
    , color = NULL
    , fill = NULL
    , subtitle = "1999-2019년 안면도에서 연도별 이산화탄소 농도 시계열 + 계절 조정"
  ) +
  theme(
    text = element_text(size = 18)
    , legend.position = "bottom"
  ) +
  ggsave(filename = saveImg, width = 10, height = 8, dpi = 600)



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
# R을 이용한 2047-2054년에 대한 회귀식 시각화

#================================================
# Set Env
#================================================
# setwd("\\\\bluerosa/rosa/Partition 1/UF phd research with Ravi/2020UPenn_journal/LSH0146 _edited")

# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."
# globalVar$mapPath = "."

# serviceName = "CHWfuture"

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0179"

#================================================
# Main
#================================================
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(lubridate)
library(MASS)
library(scales)
library(dplyr)
library(hrbrthemes)
library(data.table)
library(ggpubr)
library(forcats)
library(lubridate)
library(openxlsx)

#****************************************
# 주 데이터
#****************************************
# fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0146_PAhourlyCHW.csv", sep = "/"))
fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0179_PAhourlyCHW.csv", sep = "/"))

# PAHourlyCHW <- read.csv(file = fileInfo, stringsAsFactors = TRUE)
PAHourlyCHW = data.table::fread(file = fileInfo)

ind = which(stringr::str_detect(PAHourlyCHW$type2, regex("office")))
PAHourlyCHW[ind, "type2"] = "office"
summary(PAHourlyCHW) 

PAHourlyCHW$weekday<-as.factor(PAHourlyCHW$weekday)
PAHourlyCHW$type2<-as.factor(PAHourlyCHW$type2)
PAHourlyCHW$YMDH2<-ymd_hms(PAHourlyCHW$YMDH)
summary(PAHourlyCHW) 

trainCHWL1 = PAHourlyCHW %>%
  dplyr::filter(
    ! as.numeric(type2) %in%  c(2, 3)
  ) %>% 
  dplyr::mutate(
    makeLegend = dplyr::case_when(
      stringr::str_detect(type2, regex("Education")) ~ "Education"
      , stringr::str_detect(type2, regex("Lab")) ~ "Lab"
      , stringr::str_detect(type2, regex("Lodge")) ~ "Lodging"
      , stringr::str_detect(type2, regex("office")) ~ "Office"
      , stringr::str_detect(type2, regex("public")) ~ "Public Assembly"
      , TRUE ~ "NA"
    )
  ) %>% 
  as.tibble()

dd = trainCHWL1 %>% 
  dplyr::filter(YMDH2 == lubridate::ymd_hms("2016-06-30 23:30:00"))


#-------------------------
# [ADD] 
trainCHW <-subset(trainCHWL1, trainCHWL1$YMDH2 < lubridate::ymd_hms("2016-06-30 23:30:00"))
summary(trainCHW)#2015-07-01 01:00:00 -2016-07-01 01:00:00

# [ADD] 
testCHW <-subset(trainCHWL1, trainCHWL1$YMDH2 > lubridate::ymd_hms("2016-06-30 23:30:00") & trainCHWL1$YMDH < lubridate::ymd_hms("2016-08-14 23:30:00"))
summary(testCHW)#2016-07-02 02:00:00 - 2016-08-14 14:00:00

lmCHW82type2 = lm(CHWEUI ~ type2 + poly(Height,2) + poly(Temp,2), data = trainCHW)
summary(lmCHW82type2)

#****************************************
# 보조 데이터
#****************************************
dataL1 = trainCHW %>% 
  dplyr::group_by(type2) %>% 
  dplyr::summarise(
    Height = mean(Height, na.rm = TRUE)
  )

fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0179_Future_Temp.xlsx", sep = "/"))
future = openxlsx::read.xlsx(fileInfo2, sheet = 1) %>% 
  tibble::as.tibble() %>% 
  dplyr::mutate(
    sDate = paste(Year, Month, Day, Hour, sep = "-")
    , dtDate = lubridate::ymd_h(sDate)
  ) %>% 
  dplyr::select(dtDate, Temp) 


typeList = dataL1$type2 %>% unique() %>% sort()

dataL2 = tibble::tibble()
for (type in typeList) {
  
  tmpData = future %>% 
    dplyr::mutate(
      type2 = type
    ) %>% 
    dplyr::left_join(dataL1, by = c("type2" = "type2"))
  
  dataL2 = dplyr::bind_rows(dataL2, tmpData)
}

# [ADD] [Case 1] 테스트셋 전처리
testCHWL1 = testCHW %>% # [ADD] 
  tibble::as.tibble() %>%
  dplyr::rename(
    dtDate = YMDH2
  ) %>% 
  dplyr::select(names(dataL2))

# [ADD] [Case 2] 테스트셋 전처리
testCHWL1 = testCHW %>% # [ADD] 
  tibble::as.tibble() %>%
  dplyr::group_by(YMDH2, type2) %>%
  dplyr::summarise(
    meanTemp = mean(Temp, na.rm = TRUE)
    , meanHeight = mean(Height, na.rm = TRUE)
  ) %>% 
  dplyr::rename(
    dtDate = YMDH2
    , Temp = meanTemp
    , Height = meanHeight
    ) %>% 
  dplyr::select(names(dataL2))


# [ADD] 트레이닝셋
trainCHWL1 = trainCHW %>%
  tibble::as.tibble() %>%
  dplyr::group_by(YMDH2, type2) %>%
  dplyr::summarise(
    meanCHWEUI = mean(CHWEUI, na.rm = TRUE)
  ) %>% 
  dplyr::rename(
    dtDate = YMDH2
    , pred = meanCHWEUI
    )

# 통합 데이터셋
dataL3 = dplyr::bind_rows(dataL2, testCHWL1) %>%  # [ADD] 
  modelr::add_predictions(lmCHW82type2) %>% 
  dplyr::bind_rows(trainCHWL1) %>% # [ADD] 
  dplyr::mutate(
    type3 = dplyr::case_when(
      dtDate < lubridate::ymd_h("2047-01-01 00") ~ "2015-2016"
      , lubridate::ymd_h("2047-01-01 00") <= dtDate & dtDate < lubridate::ymd_h("2054-01-01 00") ~ "2047"
      , lubridate::ymd_h("2054-01-01 00") <= dtDate ~ "2054"
      , TRUE ~ "NA"
    )
  )

# 통합 데이터셋 확인
dataL3$type3 %>% unique() %>% sort()

dataL3 %>% 
  dplyr::filter(dtDate == lubridate::ymd_h("2047-01-01 01"))

#****************************************
# 시각화
#****************************************
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "Energy consumption prediction by building type in 2047 and 2053")

# [ADD]
Sys.setlocale("LC_ALL", "English")

ggplot(dataL3, aes(x = dtDate, y = pred, color = type2)) +
  geom_line() +
  # geom_smooth(method = 'lm', se = TRUE) +
  # ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 1.0) +
  # ggpubr::stat_cor(label.x.npc = 0.8, label.y.npc = 1.0) +
  labs(
    x = "Date"
    , y = "Chilled water consumption (kBTU/GSF)"
    , color = NULL
    , fill = NULL
    , subtitle = "Energy consumption prediction by building type in 2047 and 2053"
  ) +
  # facet_wrap(~Year, ncol = 3, scale = "free") +
  # facet_wrap(~type3, nrow = 3, scale = "free_x") +
  facet_wrap(~type3, ncol = 3, scale = "free_x") +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
    , legend.position = "bottom"
  ) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)


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
# R을 이용한 자유 주제에 대한 보고서

# [R을 이용한 서울시 아파트 실거래가 회귀분석 및 주택가격 결정 요인]
# 약 364,917개의 데이터 처리를 위해 빅데이터 처리 연산프로그램인 R 프로그래밍을 활용
# 
# 자료수집 1
# - 자료 설명 : 법정동 코드 목록
# - 수집 방법 : 해당 URL에서 자료 다운로드
# - 자료 개수 : 46,180개
# - URL : https://www.code.go.kr/stdcode/regCodeL.do
# - 출처 : 행정표준코드관리시스템
# 
# 자료수집 2
# - 자료 설명 : 국토교통부_아파트매매 실거래자료
# - 수집 방법 : 공공데이터포털에서 오픈 API 자료 수집
# - 자료 기간 : 2017년 – 2020년 05월
# - 자료 개수 : 364,917개
# - URL : https://www.data.go.kr/data/15058747/openapi.do
# - 출처 : 공공데이터포털 (국토교통부)
# 
# 자료수집 3
# - 자료 설명 : 가구 특성정보 (+소득정보)
# - 자료 개수 : 39,094개
# - URL : https://www.bigdata-environment.kr/user/data_market/detail.do?id=8cee0160-2dff-11ea-9713-eb3e5186fb38
# - 출처 : 공공데이터포털 (국토교통부)
# 
# 아파트 실거래 가격 데이터를 이용하여 연소득당 거래금액 (주택 가격 결정 요인) 계산 및 기초통계 분석
# 특히 전체 및 법정동을 구분하여 히스토그램, 상자, 산점도 그래프뿐만 아니라 공간 분포  시각화 (TMAP) 수행
# 또한 주택 가격 결정 요인을 분석하기 위해서 다음과 같은 독립변수 및 종속변수를 설정하여 회귀분석을 수행
# - 독립변수 : 건축년도, 전용면적, 층, 법정동, 면적당 거래금액
# - 종속변수 : 연소득당 거래금액 (주택 가격 결정 요인)

#================================================
# Set Env
# #================================================
# globalVar = list()
# globalVar$inpPath = "."
# globalVar$figPath = "."
# globalVar$outPath = "."
# globalVar$mapPath = "."

rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0178"

#================================================
# Main
#================================================
library(ggplot2)
library(tidyverse)
library(httr)
library(rvest)
library(jsonlite)
library(RCurl)
library(readr)
library(magrittr)
library(ggrepel)
library(colorRamps)
library(ggpubr)
library(lm.beta)
library(ggpmisc)
# unloadNamespace('raster')
library(gstat)
library(sf)

cbMatlab = colorRamps::matlab.like(11)

# 공공데이터포털 API키
# reqDataKey = globalVar$dataKey
reqDataKey = "Ftj0WhfmnXN86rrVCPTGvlQJ%oJs9l+ZQjJzPgtc37yVPWuXs8UOP3kD2lTyy9DFInQZj2VvYFH1+Uh7gNgTLLA=="

# 요청 URL
reqUrl = "http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade"
# 요청 키
reqKey = stringr::str_c("?serviceKey=", RCurl::curlEscape(stringr::str_conv(reqDataKey, encoding = "UTF-8")))

# 서울에서 서울특별시 법정동 코드 읽기
codeInfo = Sys.glob(paste(globalVar$mapPath, "/admCode/법정동코드_전체자료.txt", sep = "/"))

codeList = readr::read_delim(codeInfo, delim = "\t", locale = locale("ko", encoding = "EUC-KR"), col_types = "ccc") %>%
  magrittr::set_colnames(c("EMD_CD", "addr", "isUse")) %>% 
  tidyr::separate(col = "addr", into = c("d1", "d2", "d3", "d4"), sep = " ") %>%
  dplyr::mutate(
    emdCd = stringr::str_sub(EMD_CD, 1, 5)
  ) %>% 
  dplyr::filter(
    stringr::str_detect(d1, regex("서울특별시"))
    , stringr::str_detect(isUse, regex("존재"))
    , is.na(d3)
    , is.na(d4)
  )

codeDistList = codeList %>%
  dplyr::distinct(emdCd)

# 날짜 기간
# dtDateList = seq(as.Date("2017-01-01"), as.Date(format(Sys.time(), "%Y-%m-%d")), "1 month")
dtDateList = seq(as.Date("2018-12-01"), as.Date(format(Sys.time(), "%Y-%m-%d")), "1 month")

fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0178_가구_특성정보_(+소득정보)_201211.csv", sep = "/"))
costData = readr::read_csv(file = fileInfo) %>%
  dplyr::mutate(
    emdCd = stringr::str_sub(as.character(raw_dn_cd), 1, 5)
  ) %>% 
  dplyr::group_by(emdCd) %>% 
  dplyr::summarise(
    meanCost = mean(avrg_income_amount_am, na.rm = TRUE)
  )

#***********************************************
# 공공데이터포털 API (자료 수집)
#***********************************************
# dataL1 = tibble::tibble()
# 
# for (i in 1:length(dtDateList)) {
#   for (j in 1:nrow(codeDistList)) {
#     
#     sDate = format(dtDateList[i], "%Y%m")
#     
#     # 요청 법정동
#     reqLawdCd = stringr::str_c("&LAWD_CD=", codeDistList[j, 'emdCd'])
#     
#     # 요청 날짜
#     reqYmd = stringr::str_c("&DEAL_YMD=", sDate)
#     
#     resData = httr::GET(
#       stringr::str_c(reqUrl, reqKey, reqLawdCd, reqYmd)
#     ) %>%
#       httr::content(as = "text", encoding = "UTF-8") %>%
#       jsonlite::fromJSON()
#     
#     resCode = resData$response$header$resultCode
#     if (resCode != "00") { next }
#     
#     resItems = resData$response$body$items
#     if (resItems == "") { next }
#     
#     cat(sprintf(
#       "dtDate : %10s | code : %5s"
#       , sDate
#       , codeList[j, 'emdCd']
#     ), "\n")
#     
#     resItem = resItems$item %>%
#       as.data.frame()
#     # readr::type_convert()
#     
#     dataL1 = dplyr::bind_rows(
#       dataL1
#       , data.frame(
#         'dtYm' = sDate
#         , 'emdCd' = codeDistList[j, 'emdCd']
#         , resItem
#       )
#     )
#   }
# }

#***********************************************
# 자료 저장
#***********************************************
# saveFile = sprintf("%s/%s_%s", globalVar$outPath, serviceName, "seoul apartment transaction.csv")
# readr::write_csv(x = dataL1, file = saveFile)

#***********************************************
# 데이터 전처리
#***********************************************
fileInfo = Sys.glob(paste(globalVar$inpPath, "LSH0178_seoul apartment transaction.csv", sep = "/"))

dataL2 = readr::read_csv(file = fileInfo) %>% 
  readr::type_convert() %>% 
  dplyr::mutate(
    지번2 = readr::parse_number(지번)
    , emdCd = as.character(emdCd)
  ) %>% 
  dplyr::left_join(codeList, by = c("emdCd" = "emdCd")) %>%
  dplyr::left_join(costData, by = c("emdCd" = "emdCd")) %>% 
  dplyr::mutate(
    addr = stringr::str_trim(paste(d1, d2, 아파트, 지번, seq = ' '))
    , val = 거래금액 / meanCost # 연소득당 거래금액
    , val2 = 거래금액 / 전용면적 # 면적당 거래금액
    , dtYear = lubridate::year(lubridate::ym(dtYm))
  )


dataL3 = dataL2 %>% 
  dplyr::group_by(d2) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  )

#***********************************************
# 통계 분석
#***********************************************
# 연소득당 거래금액 따른 기초 통계량
dataL2 %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE) # 평균값
    , medianVal = median(val, na.rm = TRUE) # 중앙값
    , sdVal = sd(val, na.rm = TRUE) # 표준편차
    , maxVal = max(val, na.rm = TRUE) # 최대값
    , minVal = min(val, na.rm = TRUE) # 최소값
    , cnt = n() # 개수
  ) %>%
  dplyr::arrange(desc(meanVal))

# 법정동에 따른 연소득당 거래금액 따른 기초 통계량
dataL2 %>%
  dplyr::group_by(d2) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE) # 평균값
    , medianVal = median(val, na.rm = TRUE) # 중앙값
    , sdVal = sd(val, na.rm = TRUE) # 표준편차
    , maxVal = max(val, na.rm = TRUE) # 최대값
    , minVal = min(val, na.rm = TRUE) # 최소값
    , cnt = n() # 개수
  ) %>%
  dplyr::arrange(desc(meanVal))


#**********************************************************
# 그래프 그리기(히스토그램, 상자 수염그림, 산점도 등)
#**********************************************************
# 연소득당 거래금액 따른 히스토그램
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "연소득당 거래금액 따른 히스토그램")

ggplot(dataL2, aes(x = val)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = 0.2) +
  geom_rug(aes(x = val, y = 0), position = position_jitter(height = 0)) +
  labs(x = "연소득당 거래금액", y = "밀도 함수", colour = NULL, fill = NULL, subtitle = "연소득당 거래금액 따른 히스토그램") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 12, height = 6, dpi = 600)

# 법정동에 따른 연소득당 거래금액 히스토그램
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "법정동에 따른 연소득당 거래금액 히스토그램")

ggplot(dataL3, aes(x = d2, y = meanVal, fill = meanVal)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = round(meanVal, 0)), vjust = 1.6, color = "white", size = 4) +
  labs(x = "법정동", y = "연소득당 거래금액", fill = NULL, subtitle = "법정동에 따른 연소득당 거래금액 히스토그램") +
  scale_fill_gradientn(colours = cbMatlab, na.value = NA) +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)


# 연소득당 거래금액 따른 상자 그림
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "연소득당 거래금액 따른 상자 그림")

ggplot(dataL2, aes(y = val)) +
  geom_boxplot() +
  labs(x = NULL, y = "연소득당 거래금액", colour = NULL, fill = NULL, subtitle = "연소득당 거래금액 따른 상자 그림") +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 12, height = 6, dpi = 600)

# 법정동에 따른 연소득당 거래금액 상자 그림
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "법정동에 따른 연소득당 거래금액 상자 그림")

ggplot(dataL2, aes(x = d2, y = val, color = d2)) +
  geom_boxplot() +
  labs(x = "법정동", y = "연소득당 거래금액", color = "법정동", fill = NULL, subtitle = "법정동에 따른 연소득당 거래금액 상자 그림") +
  # scale_colour_gradientn(colours = cbMatlab, na.value = NA) +
  theme(
    text = element_text(size = 18)
    , axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)

# 연소득당 거래금액 산점도
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "연소득당 거래금액 산점도")

ggpubr::ggscatter(
  dataL2, x = "meanCost", y = "거래금액"
  , add = "reg.line", conf.int = TRUE, scales = "free_x"
  # , facet.by = "전체 법정동"
  , add.params = list(color = "blue", fill = "lightblue")
) +
  labs(
    title = NULL
    , x = "연소득"
    , y = "거래금액"
    , color = NULL
    , subtitle = "연소득당 거래금액 산점도"
  ) +
  theme_bw() +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 1.0, size = 5) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.90, p.accuracy  =  0.01,  r.accuracy  =  0.01, size = 5) +
  theme(text = element_text(size = 18)) +
  ggsave(filename = saveImg, width = 8, height = 8, dpi = 600)

#***********************************************
# 지도 그리기
#***********************************************
addrList = dataL2$addr %>% unique() %>% sort() %>%
  as.tibble()

# 구글 API 하루 제한
# addrData = ggmap::mutate_geocode(addrList, value, source = "google")

# 각 주소에 따라 위/경도 반환
# for (i in 1:nrow(addrList)) {
#   addrData = ggmap::mutate_geocode(addrList[i, 'value'], value, source = "google")
# 
#   if (nrow(addrData) < 1) { next }
# 
#   readr::write_csv(x = addrData, file = saveFile, append = TRUE)
# }

saveFile = sprintf("%s/%s_%s.csv", globalVar$inpPath, serviceName, "seoul apartment transaction-addrData")
addrData =  readr::read_csv(file = saveFile, col_names = c("value", "lon", "lat"))

dataL4 = dataL2 %>% 
  dplyr::left_join(addrData, by = c("addr" = "value")) %>% 
  dplyr::filter(
    ! is.na(lon)
    , ! is.na(lat)
    , dplyr::between(lon, 120, 130)
    , dplyr::between(lat, 30, 40)
  ) %>% 
  dplyr::group_by(lon, lat, addr) %>% 
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  )


map = ggmap::get_map(
  location = c(lon = mean(dataL4$lon, na.rm = TRUE), lat = mean(dataL4$lat, na.rm = TRUE))
  , zoom = 12
  , maptype = "hybrid"
)

saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "연소득당 거래금액 지도 매핑")

ggmap(map, extent = "device") +
  geom_point(data = dataL4, aes(x = lon, y = lat, color = meanVal, size = meanVal, alpha = 0.3)) +
  scale_color_gradientn(colours = cbMatlab, na.value = NA) +
  labs(
    subtitle = NULL
    , x = NULL
    , y = NULL
    , fill = NULL
    , colour = NULL
    , title = NULL
    , size = NULL
  ) +
  scale_alpha(guide = 'none') +
  theme(
    text = element_text(size = 18)
  ) +
  ggsave(filename = saveImg, width = 10, height = 10, dpi = 600)


#***********************************************
# IDW 지도 그리기
#***********************************************
dtYearList = dataL2$dtYear %>% unique() %>% sort()

# dtYearInfo = 2017
for (dtYearInfo in dtYearList) {
  
  dataL5 = dataL2 %>% 
    dplyr::left_join(addrData, by = c("addr" = "value")) %>% 
    dplyr::filter(
      ! is.na(lon)
      , ! is.na(lat)
      , dplyr::between(lon, 120, 130)
      , dplyr::between(lat, 30, 40)
      , dtYear == dtYearInfo
    ) %>% 
    dplyr::group_by(lon, lat, addr) %>% 
    dplyr::summarise(
      meanVal = mean(val, na.rm = TRUE)
    )
  
  # 면적당 거래금액 지도 집중도
  spNewData = expand.grid(
    x = seq(from = min(dataL5$lon, na.rm = TRUE), to = max(dataL5$lon, na.rm = TRUE), by = 0.003)
    , y = seq(from = min(dataL5$lat, na.rm = TRUE), to = max(dataL5$lat, na.rm = TRUE), by = 0.003)
  )
  sp::coordinates(spNewData) = ~ x + y
  sp::gridded(spNewData) = TRUE
  
  spData = dataL5
  sp::coordinates(spData) = ~ lon + lat
  
  # IDW 학습 및 전처리수행
  spDataL1 = gstat::idw(
    formula = meanVal ~ 1
    , locations = spData
    , newdata = spNewData
    , nmax = 4
  ) %>%
    as.data.frame() %>%
    dplyr::rename(
      lon = x
      , lat = y
      , val = var1.pred
    ) %>%
    dplyr::select(-var1.var) %>% 
    as.tibble()
  
  summary(spDataL1)
  saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, stringr::str_c(dtYearInfo, "년 연소득당 거래금액 IDW 지도 매핑"))
  
  ggmap(map, extent = "device") +
    geom_tile(data = spDataL1, aes(x = lon, y = lat, fill = val, alpha = 0.2)) +
    # geom_raster(data = spDataL1, aes(x = lon, y = lat, fill = val, alpha = 0.2)) +
    # scale_color_gradientn(colours = cbMatlab, na.value = NA) +
    scale_fill_gradientn(colours = cbMatlab, na.value = NA) +
    labs(
      subtitle = stringr::str_c(dtYearInfo, "년 연소득당 거래금액 IDW 지도 매핑")
      , x = NULL
      , y = NULL
      , fill = NULL
      , colour = NULL
      , title = NULL
      , size = NULL
    ) +
    scale_alpha(guide = 'none') +
    theme(
      text = element_text(size = 18)
    ) +
    ggsave(filename = saveImg, width = 10, height = 10, dpi = 600)
  
}

#==========================================
# TMAP 주제도 그리기
#==========================================
mapInfo = Sys.glob(paste(globalVar$mapPath, "/koreaInfo/TL_SCCO_SIG.shp", sep = "/"))
mapShape = sf::st_read(mapInfo, options = "ENCODING=EUC-KR")

# 전체 법정동에 따른 연소득당 거래금액 주제도
dataL6 = dataL2 %>% 
  dplyr::left_join(addrData, by = c("addr" = "value")) %>% 
  dplyr::filter(
    ! is.na(lon)
    , ! is.na(lat)
    , dplyr::between(lon, 120, 130)
    , dplyr::between(lat, 30, 40)
  ) %>% 
  dplyr::group_by(emdCd) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  )

mapShapeL1 = mapShape %>% 
  dplyr::inner_join(dataL6, by = c("SIG_CD" = "emdCd"))

setTilte = "전체 법정동에 따른 연소득당 거래금액 주제도"
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName,  setTilte)

tmap::tmap_mode("view")

tmap::tm_shape(mapShapeL1) +
  tmap::tm_polygons(col = "meanVal", alpha = 0.5, palette = cbMatlab, legend.hist = TRUE, style = "cont") +
  tmap::tm_text(text = "SIG_KOR_NM") +
  tmap::tm_legend(outside = TRUE) + 
  tmap::tm_layout(title = setTilte) +
  tmap_save(filename = saveImg, width = 10, height = 10, dpi = 600)

# 연도별 법정동에 따른 연소득당 거래금액 주제도
# dtYearInfo = 2017
for (dtYearInfo in dtYearList) {

  dataL7 = dataL2 %>% 
    dplyr::left_join(addrData, by = c("addr" = "value")) %>% 
    dplyr::filter(
      ! is.na(lon)
      , ! is.na(lat)
      , dplyr::between(lon, 120, 130)
      , dplyr::between(lat, 30, 40)
      , dtYear == dtYearInfo
    ) %>% 
    dplyr::group_by(emdCd) %>%
    dplyr::summarise(
      meanVal = mean(val, na.rm = TRUE)
    )
  
  mapShapeL1 = mapShape %>% 
    dplyr::inner_join(dataL7, by = c("SIG_CD" = "emdCd"))

  setTilte = stringr::str_c(dtYearInfo, "년 법정동에 따른 연소득당 거래금액 주제도")
  saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, setTilte)
  
  tmap::tmap_mode("view")
  
  tmap::tm_shape(mapShapeL1) +
    tmap::tm_polygons(col = "meanVal", alpha = 0.5, palette = cbMatlab, legend.hist = TRUE, style = "cont") +
    tmap::tm_text(text = "SIG_KOR_NM") +
    tmap::tm_legend(outside = TRUE) + 
    tmap::tm_layout(title = setTilte) +
    tmap_save(filename = saveImg, width = 10, height = 10, dpi = 600)
}


#***********************************************
# 주택 가격 결정 요인을 위한 회귀분석
#***********************************************
dataL4 = dataL2 %>%
  dplyr::select(건축년도, 전용면적, 층, val2, d2, val)

#+++++++++++++++++++++++++++++++++++++++++++++++
# 전체 아파트
dataL5 = dataL4

# 중형 이상 아파트 (66 m2 이상)
# dataL5 = dataL4 %>% 
#   dplyr::filter(전용면적 >= 66) %>% 
#   dplyr::select(-전용면적)

# 소형 아파트 (66 m2 미만)
# dataL5 = dataL4 %>% 
#   dplyr::filter(전용면적 < 66) %>% 
#   dplyr::select(-전용면적)
#+++++++++++++++++++++++++++++++++++++++++++++++

# 선형회귀분석
lmFit = lm(val ~ ., data = dataL5)
summary(lmFit)

# 단계별 소거법
lmFitStep = MASS::stepAIC(lmFit, direction = "both")
summary(lmFitStep)

# Beta 회귀계수
lmBetaFit = lm.beta::lm.beta(lmFitStep)
lmBetaFit$standardized.coefficients %>% round(2) %>% sort() %>% rev()

# 산점도 그림
validData = data.frame(
  xAxis = predict(lmFitStep)
  , yAxis = dataL5$val
  , type = "전체 아파트"
  # , type = "중형 아파트"
  # , type = "소형 아파트"
  
)

# corVal = cor(validData$xAxis, validData$yAxis)
biasVal = Metrics::bias(validData$xAxis, validData$yAxis)
rmseVal = Metrics::rmse(validData$xAxis, validData$yAxis)

# 전체 아파트에 대한 주택가격 결정요인 (연소득당 거래금액) 예측 산점도
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "전체 아파트에 대한 주택가격 결정요인 예측 산점도")
# saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "중형 아파트에 대한 주택가격 결정요인 예측 산점도")
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "소형 아파트에 대한 주택가격 결정요인 예측 산점도")

ggscatter(
  validData, x = "xAxis", y = "yAxis", color = "black"
  , add = "reg.line", conf.int = TRUE
  , facet.by = "type"
  , add.params = list(color = "blue", fill = "lightblue")
) +
  theme_bw() +
  ggpubr::stat_regline_equation(label.x.npc = 0.0, label.y.npc = 1.0, size = 4) +
  ggpubr::stat_cor(label.x.npc = 0.0, label.y.npc = 0.9, size = 4) +
  ggpp::annotate("text_npc", npcx = 0.05, npcy = 0.8, label = sprintf("Bias = %s", round(biasVal, 2)), hjust = 0, size = 4) +
  ggpp::annotate("text_npc", npcx = 0.05, npcy = 0.7, label = sprintf("RMSE = %s", round(rmseVal, 2)), hjust = 0, size = 4) +
  # ggpp::annotate("text_npc", npcx = 0.05, npcy = 0.60, label = sprintf("Bias = %s", round(biasVal, 2)), hjust = 0, size = 4) +
  # ggpp::annotate("text_npc", npcx = 0.05, npcy = 0.55, label = sprintf("RMSE = %s", round(rmseVal, 2)), hjust = 0, size = 4) +
  labs(
    title = NULL
    , x = "예측"
    , y = "실측"
    # , subtitle = "전체 아파트에 대한 주택가격 결정요인 예측 산점도"
    # , subtitle = "중형 아파트에 대한 주택가격 결정요인 예측 산점도"
    , subtitle = "소형 아파트에 대한 주택가격 결정요인 예측 산점도"
  ) +
  theme(text = element_text(size = 16)) +
  ggsave(filename = saveImg, width = 6, height = 6, dpi = 600)


# 주택 가격 결정 요인을 위한 관계성
saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, "주택 가격 결정 요인을 위한 관계성")

dataL2 %>%
  dplyr::select(건축년도, 전용면적, 층, val2, val) %>% 
  dplyr::rename(
    "면적당거래금액" = val2
    , "연소득당거래금액" = val
  ) %>% 
  GGally::ggpairs(.) +
  theme(text = element_text(size = 18))

ggsave(filename = saveImg, width = 12, height = 8, dpi = 600)


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
# R을 이용한 머신러닝을 이용한 Pima 데이터 분석

#================================================
# Set Env
# #================================================
globalVar = list()
globalVar$inpPath = "."
globalVar$figPath = "."
globalVar$outPath = "."
globalVar$mapPath = "."

# rm(list = ls())
# prjName = "test"
# source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

serviceName = "LSH0180"

#================================================
# Main
#================================================
# install.packages("kernlab")
# install.packages("e1071")
# install.packages("reshape")

library(ggplot2)
library(kernlab)
library(e1071)   # e1071 : SVM 모형 구축을 위한 라이브러리 입니다.
library(caret)
library(reshape)
library(tidyverse)
library(neuralnet)

options(max.print=999999)   
# [ reached 'max' / getOption("max.print") -- omitted 656 rows ] 오류를 해결하기위한 조치를 취함.

# Train 데이터
fileInfo1 = Sys.glob(paste(globalVar$inpPath, "LSH0180_pima-indians-diabetes_train.data", sep = "/"))
pima_train = read.csv(fileInfo1, header=F)
# pima_train = read.csv("C:/Users/chemy/Desktop/데마_15주차과제/pima-indians-diabetes_train.data",header=F)

head(pima_train)
tail(pima_train)     
str(pima_train)

# 출력 변수 V9 = Class variable (0 or 1)  미리, factor 형으로 변환하여 삽입한다. 나중에 에러 안 나도록!!
pima_train$V9 <- as.factor(pima_train$V9)
str(pima_train$V9)

# Test 데이터
fileInfo2 = Sys.glob(paste(globalVar$inpPath, "LSH0180_pima-indians-diabetes_test.data", sep = "/"))
pima_test = read.csv(fileInfo2, header=F)
# pima_test = read.csv("C:/Users/chemy/Desktop/데마_15주차과제/pima-indians-diabetes_test.data",header=F)
head(pima_test)
tail(pima_test)     
str(pima_test)

# 마찬가지, 출력 변수는 미리 factor 형으로 형변환 시켜줍니다.
pima_test$V9 <- as.factor(pima_test$V9)
str(pima_test$V9)

idx <- createDataPartition(pima$V9, p=0.7, list=F)
pima_train <- pima[idx,]
pima_test <- pima[-idx,]

pima_train$V9 = as.factor(pima_train$V9)
pima_test$V9 = as.factor(pima_test$V9)

# 트레이닝 셋
dplyr::tbl_df(pima_train)

# 테스트 셋
dplyr::tbl_df(pima_test)

#*************************************************
# 모형 선정
#*************************************************
# 독립변수: V9 변수를 제외한 모든 변수
xVar = "."

# 종속변수: V9 변수
yVar = "V9"

form = paste0(yVar, " ~ ", xVar)

resultData = data.frame()

# pima_trainL1 = pima_train %>% 
#   readr::type_convert()
  

# summary(pima_train)

#*************************************************
# 로지스틱회귀모형 (GLM)
#*************************************************
glmFit = glm(form, data = pima_train, family = binomial)

# 회귀모형에 대한 요약 결과
summary(glmFit)

# 실제 파트타임
yObs = as.numeric(pima_test$V9) - 1

# 테스트셋을 이용한 예측 파트타임
yHat = predict.glm(glmFit, newdata = pima_test, type = "response")
yHatPred = ifelse(yHat > 0.5, 1, 0)

resultData = dplyr::bind_rows(
  resultData
  , data.frame(
    type = "GLM"
    , pred = yHatPred
    , real = yObs
  )
)

#*************************************************
# 서포트벡터머신 (SVM)
#*************************************************
# linear SVM 모델 만들기.
#  >>>  : 모델 학습과 최적의 cost 값을 구하는 과정입니다.
linear.tune <- tune.svm(V9~., data=pima_train, kernel="linear", 
                        cost=c(0.001, 0.01, 0.1, 1, 5, 10))
#  >>> tune.svm 은 cost 값을 찾는 함수 입니다.

summary(linear.tune)   # 최적의 cost값이 0.1로 나오네요!


# linear SVM 평가.
### best model 객체 만들기
best.linear <- linear.tune$best.model

# predict() 함수 이용 :
### 1. 모델에 적용할 때, newdata 에는 "분류하고자하는 데이터를 대입"
### 2. 모델의 정확도 계산하기
tune.test <- predict(best.linear, newdata=pima_test)
tune.test
table(tune.test, pima_test$V9)
confusionMatrix(tune.test, pima_test$V9)

# 테스트셋을 이용한 예측 파트타임
yHat = as.numeric(predict(best.linear, newdata = pima_test))
yHatPred = yHat - 1


resultData = dplyr::bind_rows(
  resultData
  , data.frame(
    type = "SVM"
    , pred = yHatPred
    , real = yObs
  )
)

#=====================================
# Plot
#=====================================
typeList = resultData$type %>% unique() %>% sort()

typeInfo = "GLM"
for (typeInfo in typeList) {
  
  resultDataL1 = resultData %>%
    dplyr::filter(type == typeInfo)
  
  xAxis = resultDataL1$pred
  yAxis = resultDataL1$real
  
  # 검증 측정을 위한 기초 설정
  lmPred = ROCR::prediction(xAxis, yAxis)
  
  # AUC 측정 : 1에 가까울수록 최고 성능
  getVal1 = ROCR::performance(lmPred, "auc")@y.values[[1]] %>% round(2)
  
  # 이항편차 측정 : 낮을수록 좋음
  getVal2 = abdiv::binomial_deviance(xAxis, yAxis) %>% round(2)
  
  cat(sprintf(
          "[%10s] AUC : %10s | abdiv : %5s"
          , typeInfo
          , getVal1
          , getVal2
        ), "\n")

  saveImg = sprintf("%s/%s_%s.png", globalVar$figPath, serviceName, paste(typeInfo, "ROC", sep = "-"))
  
  png(file = saveImg, width = 8, height = 8, units = "in", res = 600)
  
  # ROC 커브를 위한 설정
  perform = ROCR::performance(lmPred, "tpr", "fpr")
  plot(perform, main = paste0("[", typeInfo, "] ", 'ROC Curve'))
  
  dev.off()
}


