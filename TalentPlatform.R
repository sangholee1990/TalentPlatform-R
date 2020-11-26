
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
# globalVar$config = "D:/04. ?????/Github/TalentPlatform-R"
globalVar$inpConfig = paste(globalVar$config, 'INPUT', 'o2job', sep = '/')
globalVar$figConfig = paste(globalVar$config, 'FIG', 'o2job', sep = '/')
globalVar$outConfig = paste(globalVar$config, 'OUTPUT', 'o2job', sep = '/')
globalVar$logConfig = paste(globalVar$config, 'LOG', 'o2job', sep = '/')
globalVar$mapConfig = paste(globalVar$config, 'CONFIG', 'MAP_INFO', sep = '/')
globalVar$systemConfig = paste(globalVar$config, 'CONFIG', 'system.cfg', sep = '/')

# key
configInfo = yaml::yaml.load_file(iconv(globalVar$systemConfig, "UTF-8", "EUC-KR"))
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
    # ? ?? ??
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

# ??? ????
# update.packages(ask = FALSE)



#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

# ?? R ????? ???? ?? ?? ???? ?? ??? ???? ??.
# n (= 10)? ???? ?????? mid? ?????? final? ??? ????.
# mid??? final? ??? ??? ??, ??, ?? ??? ?? ????.
# mid? ??? ??? -1?? final? ??? ??? 1??.
# mid? ??? i (=1, ..., n)? ??? ????? ??? ??? -1 + ((2 * i) / (n + 1))??.
# ??? ??? R ????? n = 30? ??????? ???? ??? ??? ????.

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

  # ? ???
  lines(data$x, data$y, col = color[i])

  # ? ???
  points(data[c(1, 4),], col = color[i], pch = 19)
}
dev.off()

# openair::mydata?? ?? wd? ??? ???? ???? (wd = 0? ??? wd = 90? ?, wd = 180? ?, wd = 270? ???).
# wd? ?? ?????? ?? ?? ??? ???? ?? ??? ??? ?? ??? ??? ??? R ????? ????.
# k (=9)? ???? ?? ???????.
# ?? ?? ??? ???? ??? ??? 1? ??? ??.
# ??? 18 (=k)? ??? ??? ?????? ????.

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
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(ggpubr)

# 1) ?????? ???? ???? ??? ?????
# ?) ?? 10?? ???????? ????? ???? ???? ???? ?????.
data = read.csv("INPUT/o2job/???????.csv", header = TRUE, fileEncoding = "euc-kr")

dataL1 = data %>%
  tidyr::gather(-???, key = "key", value = "val") %>%
  dplyr::mutate(
    dtYear = readr::parse_datetime(key, "X%Y")
    , year = lubridate::year(dtYear)
  )


ggscatter(dataL1, x = "year", y = "val", add = "reg.line") +
  stat_cor(label.x = 2009, label.y = 110) +
  stat_regline_equation(label.x = 2009, label.y = 108) +
  theme_bw()


# ?) ?? 10?? ????? ????? ???? ???? ???? ?????.
data = read.csv("INPUT/o2job/????.csv", header = TRUE, fileEncoding = "euc-kr")

dataL1 = data %>%
  tidyr::gather(-?????, key = "key", value = "val") %>%
  dplyr::filter(????? == "?????(??, ????) (???)") %>%
  dplyr::mutate(
    dtYear = readr::parse_datetime(key, "X%Y")
    , year = lubridate::year(dtYear)
  )

ggscatter(dataL1, x = "year", y = "val", add = "reg.line") +
  stat_cor(label.x = 2009, label.y = 2000000) +
  stat_regline_equation(label.x = 2009, label.y = 1900000) +
  theme_bw()

# ?) ?? 10?? ?? ? ??? ????? ???? ???? ???? ?????.
data = read.csv("INPUT/o2job/??·??_G20.csv", header = TRUE, fileEncoding = "euc-kr")

dataL1 = data %>%
  tidyr::gather(-??, key = "key", value = "val") %>%
  dplyr::mutate(
    dtYear = readr::parse_datetime(key, "X%Y")
    , year = lubridate::year(dtYear)
  )

# ??
dataL1 %>%
  dplyr::filter(?? == "??") %>%
  ggscatter(x = "year", y = "val", add = "reg.line") +
  stat_cor(label.x = 2008, label.y = 600000) +
  stat_regline_equation(label.x = 2008, label.y = 550000) +
  theme_bw()

# ??
dataL1 %>%
  dplyr::filter(?? == "??") %>%
  ggscatter(x = "year", y = "val", add = "reg.line") +
  stat_cor(label.x = 2008, label.y = 600000) +
  stat_regline_equation(label.x = 2008, label.y = 550000) +
  theme_bw()

# 1) ??????? 88???? ???? ???? ?? ???? ??? 25?? ?? ??? 50 m ??? ??? ??? ?? ??? ??. ??? (1) ?? (2) ????, ??? ????? cm?, ??? kg??, 50 m ???? ? ??? ??? ???.

# dataN2 = data.frame(
#     num = c(10010, 10012, 10015, 10017, 10021, 10023, 10044, 10055, 10059, 10060, 10065, 10070, 10072, 10074, 10079, 10080, 10090, 10093, 10096, 10101, 10103, 10118, 10123, 10125, 10126)
#     , gender = c(1, 2, 1, 1, 2, 2, 1, 2, 1, 1, 1, 2, 1, 2, 2, 2, 1, 2, 2, 1, 1, 1, 2, 1, 1)
#     , kidney = c(184.0, 160.3, 179.3, 176.2, 166.4, 168.0, 177.0, 162.4, 170.9, 188.3, 174.3, 171.7, 185.3, 165.5, 172.2, 168.6, 176.0, 168.1, 165.9, 183.0, 163.2, 176.5, 165.3, 180.9, 176.5)
#     , weight = c(76.4, 57.2, 74.2, 68.2, 56.6, 64.8, 67.5, 51.2, 65.8, 77.5, 64.2, 62.6, 80.8, 64.5, 81.6, 68.0, 81.3, 72.3, 54.1, 84.0, 63.0, 68.3, 54.7, 96.0, 74.4)
#     , legLength = c(101.6, 90.2, 99.4, 97.1, 91.0, 92.9, 103.6, 95.0, 79.5, 103.1, 102.7, 99.6, 101.2, 93.5, 97.5, 94.0, 95.6, 95.4, 92.6, 98.4, 86.7, 102.6, 96.5, 103.5, 95.1)
#     , running = c(6.17, 6.87, 6.39, 6.77, 6.93, 7.15, 7.68, 7.50, 6.70, 6.58, 6.39, 6.92, 6.38, 6.91, 7.35, 7.12, 6.55, 7.26, 6.96, 6.48, 6.84, 6.00, 7.48, 6.71, 6.73)
# )

dataN2 = read.csv("INPUT/o2job/5?.txt", sep = "", header = TRUE, fileEncoding = "euc-kr")

# ?) ??? ??, ??, ????, 50m ???? ?????? ????.
dataN2 %>%
  dplyr::group_by(sex) %>%
  dplyr::summarise_all(funs(
    mean(., na.rm = TRUE) # ???
    , median(., na.rm = TRUE) # ???
    , sd(., na.rm = TRUE) # ????
    , max(., na.rm = TRUE) # ????
    , min(., na.rm = TRUE) # ????
  )) %>%
  dplyr::glimpse()

# ?) ??? ??, ??, ????, 50m ???? ??-? ??? ?????, ?? ??? ??? ?????.


# ??-? ??
ind = which(dataN2$sex == 1)

# ??? ?? ??
stem(dataN2[ind,]$height)
stem(dataN2[-ind,]$height)

# ??? ?? ??
stem(dataN2[ind,]$weight)
stem(dataN2[-ind,]$weight)

# ??? ?? ????
stem(dataN2[ind,]$leg)
stem(dataN2[-ind,]$leg)

# ??? ?? 50m ???
stem(dataN2[ind,]$run50)
stem(dataN2[-ind,]$run50)

# ?????
# ??? ?? ???
par(mfrow = c(1, 2))
hist(dataN2[ind,]$height, main = "??")
hist(dataN2[-ind,]$height, main = "??")

# ggplot(dataN2, aes(x = height, fill = as.factor(sex))) +
#     geom_histogram(alpha = 0.6) +
#     scale_fill_discrete(labels=c("??", "??"), name = "??")

# ??? ?? ??
par(mfrow = c(1, 2))
hist(dataN2[ind,]$weight, main = "??")
hist(dataN2[-ind,]$weight, main = "??")

# ggplot(dataN2, aes(x = weight, fill = as.factor(sex))) +
#     geom_histogram(alpha = 0.6) +
#     scale_fill_discrete(labels=c("??", "??"), name = "??")

# ??? ?? ????
par(mfrow = c(1, 2))
hist(dataN2[ind,]$leg, main = "??")
hist(dataN2[-ind,]$leg, main = "??")

# ggplot(dataN2, aes(x = leg, fill = as.factor(sex))) +
#     geom_histogram(alpha = 0.6) +
#     scale_fill_discrete(labels=c("??", "??"), name = "??")

# ??? ?? 50m ???
par(mfrow = c(1, 2))
hist(dataN2[ind,]$run50, main = "??")
hist(dataN2[-ind,]$run50, main = "??")

# ggplot(dataN2, aes(x = run50, fill = as.factor(sex))) +
#     geom_histogram(alpha = 0.6) +
#     scale_fill_discrete(labels=c("??", "??"), name = "??")


# ????
# ??? ?? ??
ggplot(dataN2, aes(x = height, fill = as.factor(sex))) +
  geom_boxplot(alpha = 0.6) +
  scale_fill_discrete(labels = c("??", "??"), name = "??")

# ??? ?? ??
ggplot(dataN2, aes(x = weight, fill = as.factor(sex))) +
  geom_boxplot(alpha = 0.6) +
  scale_fill_discrete(labels = c("??", "??"), name = "??")

# ??? ?? ????
ggplot(dataN2, aes(x = leg, fill = as.factor(sex))) +
  geom_boxplot(alpha = 0.6) +
  scale_fill_discrete(labels = c("??", "??"), name = "??")

# ??? ?? 50m ???
ggplot(dataN2, aes(x = run50, fill = as.factor(sex))) +
  geom_boxplot(alpha = 0.6) +
  scale_fill_discrete(labels = c("??", "??"), name = "??")

# ?) ??? 50m ???? ???? ??? ???? ??? ?????.
# ??? ?? ??
ggplot(dataN2, aes(x = height, y = run50, colour = as.factor(sex))) +
  geom_point(alpha = 0.6, size = 5) +
  scale_color_discrete(labels = c("??", "??"), name = "??")

cor(dataN2[ind,])
lm(dataN2[ind,]$run50 ~ dataN2[ind,]$height)

lm(dataN2[-ind,]$run50 ~ dataN2[-ind,]$height)
cor(dataN2[-ind,])

# ?. ??? ? ?? ????? 10? ??? ?? ? ??? ??? ???.
# dataN3 = data.frame(
#     A = c(198, 119, 174, 235, 134, 192, 124, 241, 158, 176)
#     , B = c(196, 159, 162, 178, 188, 169, 173, 183, 177, 152)
# )

dataN3 = read.csv("INPUT/o2job/6?.txt", sep = "", header = TRUE, fileEncoding = "euc-kr")

# 1) ? ??? ??? ?? ???, ???, ????? ????.
dataN3L1 = dataN3 %>%
  tidyr::gather(key = "key", value = "val")


dataN3L1 %>%
  dplyr::group_by(key) %>%
  dplyr::summarise_all(funs(
    mean(., na.rm = TRUE) # ???
    , median(., na.rm = TRUE) # ???
    , sd(., na.rm = TRUE) # ????
  )) %>%
  glimpse()

# 2) ? ??? ??? ?????, ??-? ??, ????? ???.

# ?????
ggplot(dataN3L1, aes(x = val, fill = key)) +
  geom_histogram(alpha = 0.6)

# ??-? ??
stem(dataN3$A)
stem(dataN3$B)


# ????
ggplot(dataN3L1, aes(x = val, fill = key)) +
  geom_boxplot(alpha = 0.6)

# 3) ?? ? ??? ???? ?????? ? ????

#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

#=========================================
# ?? 1
#=========================================
# 1. diabetes ????? ??? ?? ??? ??? ?? ??? ??? logCpeptide? ??? ??? ??? ??? ???? ?????? ??.
# p-?? ???? ??? ??? ????? ??? ?? ?? ? ???? ?????? ??? ??? marking??.

library(bootstrap)

data(diabetes)

# ???
ind = which(diabetes$age > 10)

groupA = diabetes[ind,]
groupB = diabetes[-ind,]

meanGroupA = mean(groupA$logCpeptide, na.rm = TRUE)
meanGroupB = mean(groupB$logCpeptide, na.rm = TRUE)

r = meanGroupA - meanGroupB
r

# ?? ??
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
# ?? 2
#=========================================

# ? ??? ??. ??? ????? ?? logCpeptide? ??? ?? ??? ?? logCpeptide ? ??? ?? ????? ???? ???? ????? ??.
# 95% ????? ???? ???? ?? ???? ?????? marking??.

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
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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


# 1?)
# https://github.com/rstudio/webdriver

# ?? ??
# install_phantomjs()
pjs = run_phantomjs()
pjs

ses = Session$new(port = pjs$port)
ses$go("https://onland.kbstar.com/quics?page=C059652&%EB%B2%95%EC%A0%95%EB%8F%99%EC%BD%94%EB%93%9C=11650108&%EB%A9%94%EC%9D%B8%EA%B2%80%EC%83%89%EC%97%AC%EB%B6%80=1&%EB%A9%94%EC%9D%B8%EA%B2%80%EC%83%89%ED%83%80%EC%9E%85=2&QSL=F")
ses$getUrl()
ses$getTitle()
ses$takeScreenshot()


# 2?)
# cd /c/selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 5000

remDr = remoteDriver(
  remoteServerAddr = "localhost"
  , port = 5000L
  , browserName = "chrome"
)


data = read_excel("INPUT/o2job/???_??_????.xlsx", sheet = "??? ?? ????")


# ?? ??
remDr$open()

# ?? ?? ??
remDr$navigate("https://onland.kbstar.com/quics?page=C059652")


i = 3

dataL1 = data.frame()

# for (i in 1:nrow(data)) {
foreach::foreach(i = 1:nrow(data), .combine = c) %do% {

  Sys.sleep(2)

  getRowData = data %>%
    tibble::rowid_to_column() %>%
    dplyr::filter(rowid == i) %>%
    dplyr::select(???:???)

  searchVal = paste(stringr::str_split(getRowData$???, pattern = " ")[[1]][3], getRowData$???)

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

  # ? ???
  totalNumberHouseholds = getXpathText('//*[@id="ccChangeArea"]/div[*]/table/tbody/tr[1]/td[2]')[1]
  # //*[@id="ccChangeArea"]/div[3]/table/tbody/tr[1]/td[2]
  # //*[@id="ccChangeArea"]/div[5]/table/tbody/tr[1]/td[2]

  # ? ????
  totalNumberParkingSpaces = getXpathText('//*[@id="ccChangeArea"]/div[*]/table/tbody/tr[2]/td[1]')[1]
  # //*[@id="ccChangeArea"]/div[3]/table/tbody/tr[2]/td[1]
  # //*[@id="ccChangeArea"]/div[5]/table/tbody/tr[2]/td[1]


  # ????
  heatingSystem = getXpathText('//*[@id="ccChangeArea"]/div[*]/table/tbody/tr[4]/td[1]')
  # //*[@id="ccChangeArea"]/div[3]/table/tbody/tr[4]/td[1]

  Sys.sleep(2)

  remDr$findElement(using = "xpath", value = '//*[@id="siseTabBtn"]/a')$clickElement()

  Sys.sleep(2)

  # ????
  structureFrontDoor = getXpathText('//*[@id="b062071"]/div[9]/table/tbody/tr[2]/td')


  if (length(totalNumberHouseholds) == 0) totalNumberHouseholds = NA
  if (length(totalNumberParkingSpaces) == 0) totalNumberParkingSpaces = NA
  if (length(heatingSystem) == 0) heatingSystem = NA
  if (length(structureFrontDoor) == 0) structureFrontDoor = NA

  setRowDataL1 = getRowData %>%
    dplyr::mutate(
      "????" = structureFrontDoor
      , "???????" = totalNumberParkingSpaces
      , "????" = totalNumberHouseholds
      , "????" = heatingSystem
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
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(Hmisc)
library(fitdistrplus)

data = read.csv("INPUT/o2job/???????_???_???_????_??.csv", sep = ",", header = TRUE, fileEncoding = "euc-kr")

# 1) ?? ??
summary(data)

# 1) ?? ??
Hmisc::describe(data)


#
# 2) ???? ????
# (1) normal
getVal = data$????
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

# 3) R ???? ??? ???? ??
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
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(tidyverse)
library(xlsx)
library(tidyverse)
library(readr)
library(data.table)

# 02.?? ???? ??? ???? 1996-2019
data = read.csv("INPUT/o2job/rank/browser-ww-quarterly-20093-20202.csv", sep = ",", header = TRUE, fileEncoding = "euc-kr")

dataL1 = data %>%
  tidyr::gather(-Date, key = "key", value = "val") %>%
  tidyr::spread(key = "Date", value = "val") %>%
  dplyr::arrange(desc(`2019-4`)) %>%
  dplyr::top_n(13)

xlsx::write.xlsx2(dataL1, file = "OUTPUT/o2job/Work.xlsx", sheetName = "02", append = TRUE, row.names = FALSE, col.names = TRUE)

#  03.?? ???? ?? ?? 1994-2019
data = read.csv("INPUT/o2job/rank/search_engine-ww-quarterly-20091-20202.csv", sep = ",", header = TRUE, fileEncoding = "euc-kr")

dataL1 = data %>%
  tidyr::gather(-Date, key = "key", value = "val") %>%
  tidyr::spread(key = "Date", value = "val") %>%
  dplyr::arrange(desc(`2019-4`)) %>%
  dplyr::top_n(13)

xlsx::write.xlsx2(dataL1, file = "OUTPUT/o2job/Work.xlsx", sheetName = "03", append = TRUE, row.names = FALSE, col.names = TRUE)

# 04.?? ???? ?? ???? 2003-2019
data = read.csv("INPUT/o2job/rank/social_media-ww-quarterly-20091-20202.csv", sep = ",", header = TRUE, fileEncoding = "euc-kr")

dataL1 = data %>%
  tidyr::gather(-Date, key = "key", value = "val") %>%
  tidyr::spread(key = "Date", value = "val") %>%
  dplyr::arrange(desc(`2019-4`)) %>%
  dplyr::top_n(13)

xlsx::write.xlsx2(dataL1, file = "OUTPUT/o2job/Work.xlsx", sheetName = "04", append = TRUE, row.names = FALSE, col.names = TRUE)

# 05.?? ??? ?? ???? ??
data = read.csv("INPUT/o2job/rank/?????????_??_?_?_?__20200612003125.csv", sep = ",", header = TRUE, fileEncoding = "euc-kr")

dataL1 = data %>%
  tidyr::gather(-?????, key = "key", value = "val") %>%
  dplyr::mutate(
    dtYear = readr::parse_datetime(key, "X%Y..%m")
    , year = lubridate::year(dtYear)
  ) %>%
  dplyr::group_by(?????, year) %>%
  dplyr::summarise(meanVal = mean(val, na.rm = TRUE)) %>%
  tidyr::spread(key = "year", value = "meanVal") %>%
  dplyr::arrange(desc(`2019`)) %>%
  as.data.frame()

xlsx::write.xlsx2(dataL1, file = "OUTPUT/o2job/Work.xlsx", sheetName = "05", append = TRUE, row.names = FALSE, col.names = TRUE)

# 10. ?? ???? ???? ?? 2015-2019
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


# 15. ?? ?? ?? ?? 10?
data = read.csv("INPUT/o2job/rank/??????.csv", sep = ",", header = TRUE, fileEncoding = "euc-kr")

dataL1 = data %>%
  tidyr::gather(-year, -name, key = "key", value = "val") %>%
  tidyr::spread(key = "year", value = "val")

xlsx::write.xlsx2(dataL1, file = "OUTPUT/o2job/Work.xlsx", sheetName = "15", append = TRUE, row.names = FALSE, col.names = TRUE)

# 15. ?? ?? ?? ?? 10?
# ????? (??): 25
# ??? (??): 20
# ??? (NC): 20
# ??? (??): 20
# ??? (KIA): 15
# ??? (SK): 13
# ??? (LG): 13
# ??? (??): 12.5
# ??? (??): 12.5
# ?? (SK): 12
# ??? (KT): 12
# ??? (KIA): 23
# ??? (??): 12
# ??? (LG): 10
# ??? (??): 8
# ??? (??): 7
# ??? (??): 4.7
# ??? (??): 4.2
# ??? (??): 4
# ??? (??): 4
# ??? (??): 3.7


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


# 2?)
# cd /c/selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 5000

remDr = remoteDriver(
  remoteServerAddr = "localhost"
  , port = 5000L
  , browserName = "chrome"
)


data = read_excel("INPUT/o2job/???_??_????.xlsx", sheet = "??? ?? ????")


# ?? ??
remDr$open()

# ?? ?? ??
remDr$navigate("https://onland.kbstar.com/quics?page=C059652")


i = 3

dataL1 = data.frame()

# for (i in 1:nrow(data)) {
foreach::foreach(i = 1:nrow(data), .combine = c) %do% {

  Sys.sleep(2)

  getRowData = data %>%
    tibble::rowid_to_column() %>%
    dplyr::filter(rowid == i) %>%
    dplyr::select(???:???)

  searchVal = paste(stringr::str_split(getRowData$???, pattern = " ")[[1]][3], getRowData$???)

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

  # ? ???
  totalNumberHouseholds = getXpathText('//*[@id="ccChangeArea"]/div[*]/table/tbody/tr[1]/td[2]')[1]
  # //*[@id="ccChangeArea"]/div[3]/table/tbody/tr[1]/td[2]
  # //*[@id="ccChangeArea"]/div[5]/table/tbody/tr[1]/td[2]

  # ? ????
  totalNumberParkingSpaces = getXpathText('//*[@id="ccChangeArea"]/div[*]/table/tbody/tr[2]/td[1]')[1]
  # //*[@id="ccChangeArea"]/div[3]/table/tbody/tr[2]/td[1]
  # //*[@id="ccChangeArea"]/div[5]/table/tbody/tr[2]/td[1]


  # ????
  heatingSystem = getXpathText('//*[@id="ccChangeArea"]/div[*]/table/tbody/tr[4]/td[1]')
  # //*[@id="ccChangeArea"]/div[3]/table/tbody/tr[4]/td[1]

  Sys.sleep(2)

  remDr$findElement(using = "xpath", value = '//*[@id="siseTabBtn"]/a')$clickElement()

  Sys.sleep(2)

  # ????
  structureFrontDoor = getXpathText('//*[@id="b062071"]/div[9]/table/tbody/tr[2]/td')


  if (length(totalNumberHouseholds) == 0) totalNumberHouseholds = NA
  if (length(totalNumberParkingSpaces) == 0) totalNumberParkingSpaces = NA
  if (length(heatingSystem) == 0) heatingSystem = NA
  if (length(structureFrontDoor) == 0) structureFrontDoor = NA

  setRowDataL1 = getRowData %>%
    dplyr::mutate(
      "????" = structureFrontDoor
      , "???????" = totalNumberParkingSpaces
      , "????" = totalNumberHouseholds
      , "????" = heatingSystem
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
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(tidyverse)

data1 = readr::read_table2("INPUT/o2job/report (3).txt")
# data2 = read_table2("INPUT/o2job/report (4).txt")
data2 = readr::read_table2("INPUT/o2job/report (6).txt")
# data3 = readr::read_table2("INPUT/o2job/report (7).txt")
data4 = readr::read_table2("INPUT/o2job/report (8).txt")

tmpData4 = data4 %>%
  dplyr::mutate(
    dtYear = readr::parse_datetime(stringr::str_sub(??_4??, 1, 4), "%Y")
    , ?? = lubridate::year(dtYear)
  ) %>%
  dplyr::select(-??_4??, -dtYear) %>%
  dplyr::group_by(???, ??) %>%
  dplyr::summarise_all(funs(
    sum(., na.rm = TRUE) # ??
  ))

data5 = readr::read_table2("INPUT/o2job/report (9).txt")
data6 = readr::read_table2("INPUT/o2job/report (10).txt")

data = data1 %>%
  dplyr::full_join(data2, by = c("??" = "??", "???" = "???")) %>%
  # dplyr::full_join(data3, by = c("??" = "??", "???" = "???")) %>%
  dplyr::full_join(tmpData4, by = c("??" = "??", "???" = "???")) %>%
  dplyr::full_join(data5, by = c("??" = "??", "???" = "???")) %>%
  dplyr::full_join(data6, by = c("??" = "??", "???" = "???"))


tmpData = data %>%
  dplyr::filter(??? != ??)

tmpDataL1 = tmpData %>%
  dplyr::select(-??, -???, -???, -????, -???_????, -???) %>%
  dplyr::mutate_all(funs(as.numeric))

dataL1 = data.frame(?? = tmpData$??, ??? = tmpData$???, tmpDataL1) %>%
  dplyr::group_by(???, ??) %>%
  dplyr::summarise_all(funs(
    sum(., na.rm = TRUE) # ??
  )) # %>%
# tidyr::gather(-???, key = "key", value = "val")

# ggplot


dataL2 = na.omit(dataL1)

# tidyr::spread(key = "??", value = "??_??")


unique(dataL2$???)
unique(dataL2$??)

dd = cor(dataL2[, -1])

# ?? ??
dataL3 = dataL2 %>%
  dplyr::group_by(???) %>%
  dplyr::summarise_all(funs(
    mean(., na.rm = TRUE) # ??
  ))

d4 = cor(dataL3[, -1])


# dplyr::filter(???=="???")

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
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(DescTools)
library(e1071)

data = data.frame(
  temp = c(38.8, 34.0, 39.0, 38.8, 36.2, 30.4, 32.2, 34.1, 35.0, 42.2, 37.3, 32.6, 31.6, 34.1, 34.1, 33.8, 35.8, 32.3, 36.3, 31.3)
  , sale = c(423000, 207900, 464600, 460000, 264500, 107500, 161600, 131200, 206000, 910400, 338600, 138300, 157400, 172100, 153000, 127200, 200600, 116100, 265200, 132500)
)

# 1. R? ??? ??? ?????.
# (1)
hist(data$sale)

# (2)
stem(data$sale)

# (3)
table(data$sale)

# (4) ??? (outlier)? ?????.
boxplot(data$sale)

# (5)
plot(data$temp, data$sale)

# 2. R? ??? ??? ?? ?????.
# (1) ??, ??? ??
mean(data$temp, na.rm = TRUE)
median(data$temp, na.rm = TRUE)
DescTools::Mode(data$temp, na.rm = TRUE)

# (2) ??, ??? ??
mean(data$sale, na.rm = TRUE)
median(data$sale, na.rm = TRUE)
DescTools::Mode(data$sale, na.rm = TRUE)

# (3) ?? ??, ????, ????
range(data$temp)
var(data$temp)
sd(data$temp)
range(data$temp)
sd(data$temp, na.rm = TRUE) / mean(data$temp, na.rm = TRUE)

# (4) ?? ??, ????, ????
range(data$sale)
var(data$sale)
sd(data$sale)
range(data$sale)
sd(data$sale, na.rm = TRUE) / mean(data$sale, na.rm = TRUE)

# (5) ??, ??
e1071::skewness(data$temp)
e1071::kurtosis(data$temp)

# (6) ??, ??
e1071::skewness(data$sale)
e1071::kurtosis(data$sale)

# 3. R? ???? ?????.
# (1) 56233.92x -1711021.14
lmFit = lm(sale ~ temp, data = data)
paste0(round(coef(lmFit)[2], 2), "x + ", round(coef(lmFit)[1], 2))

# (2) 0.8093442
cor(data$temp, data$sale)^2

# (3) p-value? 6.784e-08?? 0.001 ???? ??? ????? ?????
summary(lmFit)

# (4)
predict(lmFit, newdata = data.frame(temp = 30:40))

# 4. R? ??? ANOVA ?????.
data2 = data.frame(
  key = factor(c("LGU", "LGU", "LGU", "SKT", "SKT", "SKT", "SKT", "KT", "KT", "KT"))
  , val = c(5.3, 6.0, 6.7, 5.5, 6.2, 6.4, 5.7, 7.5, 7.2, 7.9)
)

# (1) aovResult?? P-value? 0.00839?? 0.05?? ?? ??? ?? 3?? ?? ??? ??? ??.
aovReult = aov(val ~ key, data = data2)
summary(aovReult)


# (2) ? ?? (LGU-KT, SKT-KT)? ?? SKT-LGU? p-value? 0.99?? ????? ???? ??
TukeyHSD(aovReult)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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


# ?? ??
remDr$open()

# ????????? ??
remDr$navigate("http://www.riss.kr/index.do")

getHandle = remDr$getWindowHandles()
setWindowTab(remDr, getHandle[[1]])

searchVal = "SW ??"

remDr$executeScript(paste0("$('#query').val('", searchVal, "')"))

# ?? ??
remDr$findElement(using = "xpath", value = '//*[@id="fullpage"]/div[1]/div/div/div[1]/div/div[2]/form/fieldset/div[1]/div/button')$clickElement()


# ???? ?? ??
remDr$findElement(using = "xpath", value = '//*[@id="tabMenu"]/div/ul/li[2]/a/span')$clickElement()


# ?? 1000?? ??
remDr$executeScript(paste0("$('#sortSelect2_top option:checked').val(1000);"))

# ???
remDr$executeScript("orderSearch('re_a_kor');")

# ????? ?? ?? ??
getPaper = getXpathText('//*[@id="divContent"]/div[2]/div/div[2]/div[2]/ul/li[*]/div[2]/p[1]/a')

# ????? ?? ?? Url ??
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
  # ??? ??
  getTagInst = getCssText('div p .instituteInfo')

  getKeyword = data.frame(??? = getTagInst) %>%
    dplyr::filter(??? != getWriter)

  if (nrow(getKeyword) == 1) {
    getKeyword = stringr::str_split(getKeyword, ",") %>%
      unlist()
  }

  data = data.frame(
    "??" = i
    , "??" = getPaper[i]
    , "url" = getHref[i]
    , "??" = getWriter
    , "???" = getKeyword
  )

  dataL1 = dplyr::bind_rows(dataL1, data)
}


dataL2 = dataL1 %>%
  dplyr::group_by(???) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame()

ggplot(dataL2, aes(label = ???, color = ???)) +
  geom_text_wordcloud() +
  theme_minimal()

fig = wordcloud2(data = dataL2)

# html? ????
saveWidget(fig, "fig.html", selfcontained = FALSE)

# ??? html ?????? png ??? ???? ??
webshot::webshot("fig.html", "Keyword.png", vwidth = 775, vheight = 550, delay = 10)

# ??
xlsx::write.xlsx2(dataL1, file = "OUTPUT/o2job/Keyword.xlsx", sheetName = "???", append = TRUE, row.names = FALSE, col.names = TRUE)

xlsx::write.xlsx2(dataL2, file = "OUTPUT/o2job/Keyword.xlsx", sheetName = "????", append = TRUE, row.names = FALSE, col.names = TRUE)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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


# horizons ? ?? ??? ( ???, ??, ??? )
plot(data_results, type = "prediction", horizons = c(1, 6, 12))
plot(data_results, type = "residual", horizons = c(1, 6, 12), windows = 1:4)
plot(data_results, type = "forecast_stability", windows = max(data_results$window_number))

# ???? ???? ??? (mae, mape, smape, ??..)
data_error <- forecastML::return_error(data_results, metrics = c("mae", "mape", "smape"))
data_error$error_global[, c("mae", "mape", "smape")] <- lapply(data_error$error_global[, c("mae", "mape", "smape")], round, 1)

DT::datatable(data_error$error_global, options = list(scrollX = TRUE))

# ????? ??? ???
# plot(data_error, data_results, type = "time", facet = ~ horizon, horizons = c(1, 6, 12))
# plot(data_error, data_results, type = "horizon", facet = ~ horizon, horizons = c(1, 6, 12))
# plot(data_error, data_results, type = "global")
plot(data_error, type = "window", facet = ~horizon, horizons = c(1, 6, 12))
plot(data_error, type = "horizon", facet = ~horizon, horizons = c(1, 6, 12))
plot(data_error, type = "global", facet = ~horizon)

# hyperparameters? ???? ?? model? hyperparameter? return ?? ?? ??
hyper_function <- function(model) {

  lambda_min <- model$model$lambda.min
  lambda_1se <- model$model$lambda.1se

  data_hyper <- data.frame("lambda_min" = lambda_min, "lambda_1se" = lambda_1se)
  return(data_hyper)
}

# return_hyper? ?? hyperparameter ????
data_hyper <- forecastML::return_hyper(model_results, hyper_function)

# hyperparameter? ?? ???
plot(data_hyper, data_results, data_error, type = "stability", horizons = c(1, 6, 12))
plot(data_hyper, data_results, data_error, type = "error", c(1, 6, 12))


# test data? ?? ??? ?? forecast dataset ??
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

# forecast dataset ??
DT::datatable(head(data_forecast_list$horizon_6), options = list(scrollX = TRUE))


# ???? ? ???? ??? law = 1 ??
for (i in seq_along(data_forecast_list)) {
  data_forecast_list[[i]]$law <- 1
}

# forecast dataset? ?? ??? ?? ??
data_forecast <- predict(model_results, model_results_2,
                         prediction_function = list(prediction_function, prediction_function_2),
                         data = data_forecast_list)


# horizons ??? forecast ??
data_forecast$DriversKilled_pred <- round(data_forecast$DriversKilled_pred, 0)
DT::datatable(head(data_forecast, 10), options = list(scrollX = TRUE))


# forecast dataset? ???? ???
# ? windows? ??? ??? ???? ??? plot?? 7?? ????? ???
plot(data_forecast,
     data_actual = data[-(1:150),],  # Actuals from the training and test data sets.
     actual_indices = dates[-(1:150)],
     horizons = c(1, 6, 12))

# forecast error ??
data_error <- forecastML::return_error(data_forecast,
                                       data_test = data_test,
                                       test_indices = dates[(nrow(data_train) + 1):length(dates)],
                                       metrics = c("mae", "mape", "smape", "mdape"))

data_error$error_by_horizon[, c("mae", "mape", "smape", "mdape")] <- lapply(data_error$error_by_horizon[, c("mae", "mape", "smape", "mdape")], round, 1)
DT::datatable(head(data_error$error_by_horizon, 10), options = list(scrollX = TRUE))  # LASSO? ??


# training dataset? ?? ???? ?? data list ??
data_list <- forecastML::create_lagged_df(data_train,
                                          outcome_col = outcome_col,
                                          type = "train",
                                          horizons = horizons,
                                          lookback = lookback,
                                          date = dates[1:nrow(data_train)],
                                          frequency = date_frequency,
                                          dynamic_features = dynamic_features
)

# training dataset ?? ??????? window_length=0 ?? ??
windows <- forecastML::create_windows(data_list, window_length = 0)
plot(windows, data_list, show_labels = TRUE) # ???

# training datasest ??
# LASSO? ??
model_results <- forecastML::train_model(data_list, windows, model_name = "LASSO", model_function)

# historical data ?? (training dataset? ??)
data_results <- predict(model_results, prediction_function = list(prediction_function), data = data_list)
DT::datatable(head(data_results, 10), options = list(scrollX = TRUE))

plot(data_results, type = "prediction", horizons = c(1, 6, 12)) # horizons ? ?? ???

# ?? ?? ?? return
data_error <- forecastML::return_error(data_results, metrics = c("mae", "mape", "mdape", "smape"),
                                       models = NULL)

data_error$error_global[, c("mae", "mape", "mdape", "smape")] <- lapply(data_error$error_global[, c("mae", "mape", "mdape", "smape")], round, 1)
DT::datatable(head(data_error$error_global), options = list(scrollX = TRUE))


######################################################################################

# ??? ????
data("data_seatbelts", package = "forecastML")
data <- data_seatbelts

date_frequency <- "1 month"  # Time step frequency.

# ?? sequence ??
dates <- seq(as.Date("1969-01-01"), as.Date("1984-12-01"), by = date_frequency)


data$PetrolPrice <- round(data$PetrolPrice, 3) # ??? 3???? ??
data <- data[, c("DriversKilled", "kms", "PetrolPrice", "law")]  # ?? ? ?? ??
DT::datatable(head(data, 5)) # ??? ??


# ??? 12? : test data
# 12? ?? : train data
data_train <- data[1:(nrow(data) - 12),]
data_test <- data[(nrow(data) - 12 + 1):nrow(data),]


# ??? Driverskilled ???
p <- ggplot(data, aes(x = dates, y = DriversKilled)) # plot
p <- p + geom_line() # ??? ??
p <- p + geom_vline(xintercept = dates[nrow(data_train)], color = "red", size = 1.1) # train test ???
p <- p + theme_bw() + xlab("Dataset index") # ?? ? x? label ??
p

outcome_col <- 1  # ???? ?? ??

# ?? ?? ??( 1-step-ahead, 3-step-ahead , ...)
horizons <- c(1, 3, 6, 12)

# ??? ??? ?? ???
lookback <- c(1:6, 9, 12, 15)

dynamic_features <- "law" # ?? ???? ???? ??


# horizons? lookback? ???? data_list ??
# horizons? ???? list ??
data_list <- forecastML::create_lagged_df(data_train,
                                          outcome_col = outcome_col,
                                          type = "train",
                                          horizons = horizons,
                                          lookback = lookback,
                                          date = dates[1:nrow(data_train)],
                                          frequency = date_frequency,
                                          dynamic_features = dynamic_features
)

# horizons = 6 ? ?? ??
DT::datatable(head(data_list$horizon_6, 10), options = list(scrollX = TRUE))

plot(data_list) # ? horizons ? ??? ??? ??? ?? ?? ???


# cross vaidation? ?? ?? ??
# window_length = 24  ==> ? block ? ??? ? = 24
windows <- forecastML::create_windows(lagged_df = data_list, window_length = 24, skip = 0,
                                      window_start = NULL, window_stop = NULL,
                                      include_partial_window = TRUE)

# ???
plot(windows, data_list, show_labels = TRUE)


# Example 1 - LASSO

# ???? LASSO? ???? model? return?? ?? ??
# ??? train_model() ? ?? ??
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
# ???? RandomForest? ???? model? return?? ?? ??
# ??? train_model() ? ?? ??
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

# ??? LASSO ??? ??? predict?? data.frame?? return ?? ?? ??
prediction_function <- function(model, data_features) {

  if (length(model$constant_features) > 1) {  # 'model' was passed as a list.
    data <- data[, -c(model$constant_features)]
  }

  x <- as.matrix(data_features, ncol = ncol(data_features))

  data_pred <- data.frame("y_pred" = predict(model$model, x, s = "lambda.min"))
  return(data_pred)
}

# Example 2 - Random Forest
# ??? RF ??? ??? predict?? data.frame?? return ?? ?? ??
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

# ? ??? horizons? ?? ??
# windows ? ??? ???
data_results <- predict(model_results, model_results_2, model_results_3, model_results_4, model_results_5,
                        prediction_function = list(prediction_function, prediction_function_2, prediction_function_3, prediction_function_4, prediction_function_5),
                        data = data_list)

# ??? ??? ??
data_results$DriversKilled_pred <- round(data_results$DriversKilled_pred, 0)

# ??? ??
DT::datatable(head(data_results, 30), options = list(scrollX = TRUE))


# horizons ? ?? ??? ( ???, ??, ??? )
plot(data_results, type = "prediction", horizons = c(1, 6, 12))
plot(data_results, type = "residual", horizons = c(1, 6, 12), windows = 5:7)
plot(data_results, type = "forecast_stability", windows = max(data_results$window_number))

# ???? ???? ??? (mae, mape, smape, ??..)
data_error <- forecastML::return_error(data_results, metrics = c("mae", "mape", "smape"))
data_error$error_global[, c("mae", "mape", "smape")] <- lapply(data_error$error_global[, c("mae", "mape", "smape")], round, 1)

DT::datatable(data_error$error_global, options = list(scrollX = TRUE))

# ????? ??? ???
# plot(data_error, data_results, type = "windows", facet = ~ horizon, horizons = c(1, 6, 12), windows = 5:7)
# plot(data_error, data_results, type = "horizon", facet = ~ horizon, horizons = c(1, 6, 12))
# plot(data_error, data_results, type = "global")

plot(data_error, type = "window", facet = ~horizon, horizons = c(1, 6, 12))
plot(data_error, type = "horizon", facet = ~horizon, horizons = c(1, 6, 12))
plot(data_error, type = "global", facet = ~horizon)

# ? ??? horizons? ?? ??
# windows ? ??? ???
data_results <- predict(model_results, model_results_2, model_results_4,
                        prediction_function = list(prediction_function, prediction_function_2, prediction_function_4),
                        data = data_list)

# ??? ??? ??
data_results$DriversKilled_pred <- round(data_results$DriversKilled_pred, 0)

# ??? ??
DT::datatable(head(data_results, 30), options = list(scrollX = TRUE))


# horizons ? ?? ??? ( ???, ??, ??? )
plot(data_results, type = "prediction", horizons = c(1, 6, 12))
plot(data_results, type = "residual", horizons = c(1, 6, 12), windows = 5:7)
plot(data_results, type = "forecast_stability", windows = max(data_results$window_number))

# ???? ???? ??? (mae, mape, smape, ??..)
data_error <- forecastML::return_error(data_results, metrics = c("mae", "mape", "smape"))
data_error$error_global[, c("mae", "mape", "smape")] <- lapply(data_error$error_global[, c("mae", "mape", "smape")], round, 1)

DT::datatable(data_error$error_global, options = list(scrollX = TRUE))

# ????? ??? ???
# plot(data_error, data_results, type = "windows", facet = ~ horizon, horizons = c(1, 6, 12))
# plot(data_error, data_results, type = "horizon", facet = ~ horizon, horizons = c(1, 6, 12))
# plot(data_error, data_results, type = "global")

plot(data_error, type = "window", facet = ~horizon, horizons = c(1, 6, 12))
plot(data_error, type = "horizon", facet = ~horizon, horizons = c(1, 6, 12))
plot(data_error, type = "global", facet = ~horizon)


# forecast data list ??
data_forecast_list <- forecastML::create_lagged_df(data_train,
                                                   outcome_col = outcome_col,
                                                   type = "forecast",
                                                   horizons = horizons,
                                                   lookback = lookback,
                                                   date = dates[1:nrow(data_train)],
                                                   frequency = date_frequency,
                                                   dynamic_features = dynamic_features
)

# ???? ? ?? (law = 1)
for (i in seq_along(data_forecast_list)) {
  data_forecast_list[[i]]$law <- 1
}

# training dataset ??? ??? ??? forecast data? ??
data_forecast <- predict(model_results, prediction_function = list(prediction_function), data = data_forecast_list)

# ?? ?? ???
plot(data_forecast,
     data_actual = data[-(1:150),],
     actual_indices = dates[-(1:150)])


# ?? ?? ?? return
data_error <- forecastML::return_error(data_forecast, data_test = data_test,
                                       test_indices = dates[(nrow(data_train) + 1):nrow(data)],
                                       metrics = c("mae", "mape", "mdape", "smape"))

data_error$error_by_horizon[, c("mae", "mape", "mdape", "smape")] <- lapply(data_error$error_by_horizon[, c("mae", "mape", "mdape", "smape")], round, 1)
data_error$error_global[, c("mae", "mape", "mdape", "smape")] <- lapply(data_error$error_global[, c("mae", "mape", "mdape", "smape")], round, 1)

DT::datatable(data_error$error_global, options = list(scrollX = TRUE))


# ? h-step-ahead forecasting? ??
data_combined <- forecastML::combine_forecasts(data_forecast)


data_actual <- data[dates >= as.Date("1980-01-01"),]
actual_indices <- dates[dates >= as.Date("1980-01-01")]

# ?? ?? ?? ???
plot(data_combined, data_actual = data_actual, actual_indices = actual_indices)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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


# ?? ??
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

          log4r::info(log, paste0("??? : ", round((i / nrow(setRootData)) * 100, 2), " %"))

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

# ??
# xlsx::write.xlsx2(dataL3, file = "OUTPUT/o2job/Qoo10.xlsx", sheetName = "G9SKIN", append = TRUE, row.names = FALSE, col.names = TRUE)

xlsx::write.xlsx2(dataL4, file = "OUTPUT/o2job/Qoo10.xlsx", sheetName = "berrisom", append = TRUE, row.names = FALSE, col.names = TRUE)

# }


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

# 1? ??
# ?       Jan Feb Mar Apr May Jun Jul  Aug Sep Oct Nov Dec
# ???? –10 –9   2    3   10  12  22  26   20  13   5   -8
#
# 1) weather?? ??? ???? ?? ???? ?? ?????.
weather = data.frame(
  key = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  , val = c(-10, -9, 2, 3, 10, 12, 22, 26, 20, 13, 5, -8)
)

# 2) 6? ????? ?????
ind = which(weather$key == "Jun")
weather[ind,]

# 3) 1?? 12?? ????? ??? ?????
ind = which(weather$key == "Jan" | weather$key == "Dec")
weather[ind,]

# 4) 1~6? ????? ??? ?????
ind = which(weather$key == "Jan" |
              weather$key == "Feb" |
              weather$key == "Mar" |
              weather$key == "Apr" |
              weather$key == "May" |
              weather$key == "Jun")
mean(weather[ind,]$val, na.rm = TRUE)

# 5) 7~12? ????? ?? ?????
ind = which(!(weather$key == "Jan" |
  weather$key == "Feb" |
  weather$key == "Mar" |
  weather$key == "Apr" |
  weather$key == "May" |
  weather$key == "Jun"))

sum(weather[ind,]$val, na.rm = TRUE)


# 2???
# 1) cat() ??? ???? 10,000?? ??? ?? ???? ?????

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

# 3???
#  ?? : Kim Lee Park Choi Jung Kang Cho Yoon Jang Yim
#  ?? : 100  84  93    99  87    83    76  89    99  78
#
# 1) names() ??? ???? score_student ??? ???? ??? ???? ?? ?? ??? ?????

score_student = c(100, 84, 93, 99, 87, 83, 76, 89, 99, 78)
names(score_student) = c("Kim", "Lee", "Park", "Choi", "Jung", "Kang", "Cho", "Yoon", "Jang", "Yim")

#
# 2) score_student ?? 95? ??? A+ , 90? ~ 94?? A- , 85? ~ 89?? B+, 80? ~ 84?? B-, 79? ??? C+ ? ???? score_grade ??? ?????

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
# 3) score_grade ?? ?? ??? ??? ?? ?????

# ?? 4?
ques_3 <- 50:100

# 1)ques_3 ?? 65?? ?? 85?? ?? ?? ?????
ind = which(ques_3 > 65 & ques_3 < 85)
print(min(ques_3[ind], na.rm = TRUE))

# 2)ques_3 ?? 9? ???? ? ???? 2? ?? ?? ?????.
ind = which(ques_3 %% 9 == 2)
print(ques_3[ind])

# 3) ques_3 ?? 3? ???? ?? 0?? ???? ?????
ind = which(ques_3 %% 3 == 0)
print(ques_3[ind])

# 4) ques_3 ?? ??? ?? ?? ?? ?????.
ind = which(ques_3 %% 2 == 0)
print(sum(ques_3[ind], na.rm = TRUE))

# 5) ques_3 ?? 2? 5? ???? ?? ?????.
ind = which(ques_3 %% 2 == 0 & ques_3 %% 5 == 0)
print(ques_3[ind])


# 5???
#      T S
#  [1,] 35 23
#  [2,] 41 21
#  [3,] 44 24
#  [4,] 32 22
#  [5,] 50 25
# 1) ?? ?? ??? ????(2? 5?) age? ?????

data = data.frame(
  "T" = c(35, 41, 44, 32, 50)
  , "S" = c(23, 21, 24, 22, 25)
)

age = as.matrix(data)


# 2)  age? ? ??? ?? Teacher, Student?  ?? ??? ??? ?? Class1, Class2, Class3, Class4, Class5? ?????.
rownames(age) = c("Class1", "Class2", "Class3", "Class4", "Class5")
colnames(age) = c("Teacher", "Student")

age


# 6? ??
# ??????? 1?? ?? 4?? ????? ????.
# ??????? ??? ???? ??? ????, ??? ???? ?? ????
#
#  ?????? 5??? ???? 5?? ?? ??????? ?? ?????
getVal(5, 0)

getVal = function(val, hour) {

  result = val * (4^hour)

  return(result)
}


# 7? ??
# ??? 2? ?? 9? ?? ??? ?? ???? ????? ?????

for (i in 2:9) {
  for (j in 1:9) {
    cat(i, " * ", j, " = ", i * j, "\n")
  }
}

# 8? ?? ???? ? ??? ?????
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
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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
  dplyr::select(????, ??, ??, ????, ???, ??, ??, ??????, ????, ????)
# dplyr::select(????, ??, ????, ????, ???, ??, ??, ??????)

dataL2 = na.omit(dataL1)

summary(dataL2)

# NA ? ??
ind = which(is.na(dataL1), arr.ind = TRUE)
naRowInd = sort(unique(ind[, 1]))
dataL1[naRowInd,]

# ???? + ?? + ?? + ???? + ???? + ??? + ?? + ??
lmFit = lm(?????? ~ ., data = dataL2)
summary(lmFit)

lmBetaFit = lm.beta::lm.beta(lmFit)

# Beta ????
round(lmBetaFit$standardized.coefficients, 3)

lm.beta::summary.lm.beta(lmBetaFit, standardized = TRUE)

# ???
# dataL3 = BBmisc::normalize(dataL2, method = "range", range = c(0, 1))
#
# lmFitNor = lm(?????? ~ ., data = dataL3)
#
# summary(lmFitNor)


# ?? ? ?? ?? ???? ?? ??
# stargazer(dataL2, type="text", title="?? ????", no.space = TRUE)

# ?? ? ?? ?? ???? ?? ??
# stargazer(dataL1, type="text", title="?? ????", no.space = FALSE)

# ideology ? ??? html ??? ??
# stargazer(dataL1, type = "html", title = "?? ????", out = "ideology.html")

#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggplot2)

# 1) ??? ??? ????? ? ???? ??? ??? ???
seoul = read.csv("INPUT/o2job/??? ????? 2005-2008? ????.csv")

dplyr::tbl_df(seoul)

# 2) ? ????? 2005~2008? ?? ??? ???
tmp1 = seoul %>%
  dplyr::group_by(o.name) %>%
  dplyr::summarise(meanVal = mean(temp, na.rm = TRUE))

dplyr::tbl_df(tmp1)

# 3) ???? ????? ?????? ????
barplot(tmp1$meanVal, names.arg = tmp1$o.name, col = "blue", main = "???? ??? ??")

# 4) ?? ??? ?? ?? 3? ???? ??3? ???? ??, ????? ????
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

# 5) 4?? ???? ??? ??? ??? ??? ?? ????
tmp2 = seoul %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(meanVal = mean(temp, na.rm = TRUE))

barplot(tmp2$meanVal, names.arg = tmp2$year, col = "red", main = "??? ?? ??")


# 6) ???? ??? ??? ????
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

# 7) ???? ????? ????? ??? ????
map = get_googlemap(center = cenVal, maptype = "roadmap", zoom = 11)

ggmap(map) +
  geom_point(data = dataL1, aes(x = long, y = lat, size = meanSize), alpha = 0.5, colour = c("red")) +
  geom_text(data = dataL1, aes(x = long, y = lat), size = 5, label = dataL1$o.name)

#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(ggplot2)
library(tidyverse)
library(dplyr)
library(spatstat)

# ????? ??? ???? ?? Y? ????, ???? ?? S_T? ??? ???? ????.  S_T? ??? 0?? 200???? K=100?? ????.

# K?
K = 100.0

# S_T ??? ??
s_t = seq(0, 200, 1)


# ??? ?? = max?(S_T-K,0)??
Y = c()
S_T = c()

for (s in s_t) {
  Y_part = max(s - K, 0)
  Y = append(Y, Y_part) # Y ? (??) ??
  S_T = append(S_T, s) # X ? ?? (??? ?? ??)
}

plot(S_T, Y)


# ???? ??? ??-??-??? ?? ?? ???? ???? C ? ?????? ??? ?? ??? ?? ?? ??? ????? ??? ?????.

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

# ?? ??? ??? ??????? ???? ???? ?? ???? ?? ??? ?? ???? ????(0->T) ??? ??? ???? ????.

N = 10000
dt = 1.0 / 1000.0
S = 100.0
K = 100.0
T1 = 0.5
sigma = 0.1
rf = 0.04

data = data.frame()

# ??? ??? ?? ? ?????? ??? ??
Vi = c()
Ci = c()

for (i in 1:N) {

  # ?? ??
  S_index = c(S)

  # ?? ??
  S_c = S_index[length(S_index)]

  for (t in seq(dt, T1, by = dt)) {
    S_part = S_c * exp((rf - (sigma**2 / 2.0)) * dt + (sigma * sqrt(dt) * rnorm(1, 0, 1)))
    S_index = append(S_index, S_part)
  }

  if (i <= 5) {
    result_part = data.frame(s_index = S_index, del = seq(0, T1, by = dt), N = i)
    data = rbind(data, result_part)
  }

  # ?? ??? ??? ?? ??
  Vi = append(Vi, max(S_index - K, 0))

  # ???? ?? ??? ?? ?? ??
  Ci = append(Ci, max(S_index - K, 0) / N)
}

C = exp(-rf * T1) * sum(Ci)

# ?? ??? ?? (C)
round(C, 2)

ggplot(data, aes(x = del, y = s_index, colour = N)) +
  geom_line()


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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

data = readxl::read_excel("INPUT/o2job/????_??.xlsx", sheet = "????")

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

# ?? ??
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

        # Url ?? ????
        getUrl = getRowData$url
        getName = getRowData$name
        getTag = getRowData$targetTag

        remDr$navigate(getUrl)

        # ??? ????
        getText = getCssText(getTag)

        getTextReplace = stringr::str_replace_all(getText, " ", "")

        # ?? ??? ??
        isKeywordList = stringr::str_detect(getTextReplace, "|??|??|???|??|???|OUT")

        isKeyword = FALSE

        if (length(isKeywordList) < 1) { isKeyword = TRUE }

        for (j in 1:length(isKeywordList)) {
          if (isKeywordList[j] == TRUE) { isKeyword = TRUE }
        }

        # ????
        # if (i == 15)  { isKeyword = FALSE }

        log4r::info(log, paste0("[", i, "] ", getName, " | ", "?? ??? ??  : ", isKeyword, " | ", "getText : ", getText))

        # ?? ???? ?? ??
        if (isKeyword == FALSE) {

          remDr$executeScript('alert("??????.");')

          # ??? ?? ??
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
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(faraway)
library(tidyverse)
library(grid)
library(readxl)
library(ggmap)

# ??2] worldcup ????? ??? [?? 2]? ?? worldcup_summary ??????? ?????.
# ?? ??? ? ???? ?? ??? ???? ??? ? ??? R ????? ?????.

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

# worldcup ????? ???? [?? 4]? ?? ???? ???? R ????? ?????.

ind = which(worldcup$Shots == max(worldcup$Shots, na.rm = TRUE))
maxDf = worldcup[ind,]

getName = paste0(rownames(maxDf), " ( ", maxDf$Team, ", ", maxDf$Position, " )")

ggplot(worldcup, aes(x = Time, y = Shots)) +
  geom_point() +
  geom_segment(data = d, mapping = aes(x = 400, y = 25, xend = maxDf$Time, yend = maxDf$Shots), arrow = arrow(), size = 1, color = "red") +
  annotate("text", x = 300, y = 25, label = getName, color = "red", fontface = 2)


# 3.?? 5
# [?? 5]? ?? <?????> ???(https://www.airkorea.or.kr )? ???? ????? ??? ??? ??? ?? ?? ??? ?? R ?????? ?????.
#
# 1.??? ?? ????? ??? ??? ??? ????? ?????. (5?)
# ???? (ex. ???)
# ?? (ex. ?? ??? ??? 426 ???? ?? 1?)
dataQ5 = readxl::read_excel("INPUT/o2job/Q5.xls", sheet = "Sheet1")

dataQ5L1 = data %>%
  dplyr::select(????, ?????)

dplyr::tbl_df(dataQ5L1)

# 2.? 1??? ??? ????? ??? ??? ?? ? ??? ??? ?? ? ?? ??? ????? ?????.

dataQ5L2 = ggmap::mutate_geocode(dataL1, ?????, source = "google")

dplyr::tbl_df(dataQ5L2)

# 3.? 2??? ??? ????? ???? ? ???? ???? ??? ?? ??? ?? ?????.  (?, ? ??? ? ??? ??? ??? ? ??? ?)


markerDf = data.frame(
  lon = dataQ5L2$lon
  , lat = dataQ5L2$lat
)

cenVal = c(mean(dataQ5L2$lon, na.rm = TRUE), mean(dataQ5L2$lat, na.rm = TRUE))
mapMarker = get_googlemap(center = cenVal, maptype = "roadmap", zoom = 11, markers = markerDf)

ggmap(mapMarker) +
  geom_text(data = dataQ5L2, aes(x = lon, y = lat), size = 5, label = dataL2$????)

# 4.?? 6
# <?????> ???(https://www.airkorea.or.kr)? ???? 2020? 4? ??? ????? ????? ??? ??? ?? ?? ??? ?? R ?????? ?????.

# 1.???? ??? ????? ??? ??? ????? ?? ? ?????.
dataQ6 = readxl::read_excel("INPUT/o2job/Q6.xls", sheet = "Sheet1")

dplyr::tbl_df(dataQ6)

# 2.? 1??? ??? 2020? 4? ????? ??? ????? ??? ??? ??? ???? ??? ?? ???? ?????.
#

endInd = stringr::str_locate(dataQ6$????, "\\]") - 1
dataQ6$name = stringr::str_sub(dataQ6$????, 2, endInd[, 1])

dataQ6L2 = dataQ6 %>%
  tidyr::gather(-???, -????, -name, key = "key", value = "val") %>%
  dplyr::ungroup()

dplyr::tbl_df(dataQ6L2)


# 3.? 2??? ??? ?? ???? ???? ??? ? ??? ????? ??? ?? ??? ??? ?????.
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
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(ggplot2)
library(graphics)
library(broom)

# 1. ggplot2? diamonds?? ??? ??? 500?? ?? ??? ??? ???? ??? ? ??? ???? ?? 10?? ?? 5?? 7?? ?? ??

n = 500
ind = sample(1:nrow(diamonds), n)

dimData = diamonds[ind,]
dimDataL1 = dimData[1:10, 1:7]

dplyr::tbl_df(dimDataL1)


# 2. R? ??? UCBAmission?? Dept?? ??? ???? ??? ???? ??? ??
ucbData = UCBAdmissions %>%
  broom::tidy() %>%
  group_by(Gender, Dept) %>%
  mutate(prop = n / sum(n)) %>%
  filter(Admit == "Admitted")

dplyr::tbl_df(ucbData)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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
# ???? + ?? ??
#======================================================
car.df %>%
  ggplot(aes(x = Price)) +
  geom_histogram(aes(y = ..density..), binwidth = 1000) +
  stat_function(fun = dnorm, args = list(mean = mean(car.df$Price, na.rm = TRUE), sd = sd(car.df$Price, na.rm = TRUE)), lwd = 2, col = 'red') +
  labs(title = "??? ??? ?? ??", x = "??? ??", y = "????(density)", subtitle = "??: ??") +
  scale_x_continuous(labels = scales::comma) +
  ggsave(filename = "FIG/o2job/Img_001.png", dpi = 600)

#======================================================
# ?? ??
#======================================================
car.df %>%
  ggplot(aes(x = Price)) +
  geom_histogram(aes(y = ..count..)) +
  labs(title = "??? ??? ?? ??", x = "??? ??", y = "????", subtitle = "??: ??") +
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
  labs(title = "?? ????? ??? ??? ??? ?? ??", x = "??? ??", y = "?? ??? ??", subtitle = "??: ??") +
  coord_equal() +
  ggsave(filename = "FIG/o2job/Img_003.png", width = 6, height = 6, dpi = 600)

#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================


######################################## ??? ? ????? ??/??/???? ?? ##############################
##  - ?????? : ??? ????? ?? ???? ?? n?? ???? n? ??? ????? ????
##  - ????? ??? ???? ??? ????
##  - ? ??????? ??/??/????? ???? ???? ??/??/????? ??? ? ??
##    -- ???? ??                  = ??????? ??
##    -- ???? ??/???           = ??????? ??
##    -- ???? ????/??(???) = ??????? ????
##  -> ?? ??? ???? ???? ? ? ??? ? ??  -> ????? ????
####################################################################################################################

set.seed(1)                   # ??? ??? ??? ????
roulette = c(800, 8000, 80000) # ?? ??
prob = c(4 / 8, 3 / 8, 1 / 8)

hist(X)  # ???? ????
cat(mean(X), var(X), sd(X), "\n")  # ???? ??/??/????


################################################### ??????? ??? ###########################################################

for (i in c(10, 100, 1000, 10000)) {
  DO = 100000    # Number of repetition
  N = i          # Number of sample

  # FALSE : ??? ??(??? ??) : ?? ?? ?? ?? ?? ? ?? ??
  sampleList = lapply(1:DO, function(i) sample(roulette, N, replace = TRUE, prob = prob)) # ?? ??
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
  lines(c(XX1, XX1), c(0, YY), lty = 1, col = 2); text(XX1, YY / 2, "+1?")
  lines(c(XX2, XX2), c(0, YY), lty = 1, col = 2); text(XX2, YY / 2, "-1?")
  lines(c(XX3, XX3), c(0, YY), lty = 1, col = 2); text(XX3, YY / 2, "+2?")
  lines(c(XX4, XX4), c(0, YY), lty = 1, col = 2); text(XX4, YY / 2, "-2?")
  lines(c(XX5, XX5), c(0, YY), lty = 1, col = 2); text(XX5, YY / 2, "+3?")
  lines(c(XX6, XX6), c(0, YY), lty = 1, col = 2); text(XX6, YY / 2, "-3?")
  lines(c(maxVal, maxVal), c(0, YY), lty = 1, col = 3); text(maxVal, YY / 2, "Max")
  lines(c(minVal, minVal), c(0, YY), lty = 1, col = 3); text(minVal, YY / 2, "Min")
  dev.off()
}

#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(tidyverse)

tmpData1 = read.csv("INPUT/o2job/gene_annotation.csv")
tmpData2 = read.csv("INPUT/o2job/phylostratigraphy_nr.csv")

# Phylostratigraphy? ??? ?? ???? ????? age?, phylogenetics? ???? ??? ???? ???? ?? ?????. ??? ???? ??????, phylogenetics? ??? ????? ??? ??? ? ??? ???? ???? ?? ?????.
#
# ??? ?? ??? 'phylostratigraphy_nr.csv'? ??? ??? ??? ??? ??? ???. ??? ??? ? ???? ??? ? ?????? 1?2?8???? ????? ?? ?? ??? ?? ???? ?????? ??? ????.
#
# Strata? ??? ??? ???, ? ????? ??? ??? ???? ??? ????? ?????. ??? Strata? 1??? ?? ?? ???? ???? ????? ???, ??? 7?? ??????? ???? ????? ????? ???. 2-6??? ?? ?? ??? ??? ????? ?????.
#
# ??? ??? ??? ??, ????? ??? ??? 'gene.fasta' , 'gene_annotation.csv' ???? ????, ???? ???? plot?? ???, ? ??? ???? ??? ? ??? ???.


# 1. ?????? chromosome (? 7?? chromosome ? ????? ?? ?? 1?? ?? (8???? ?? ??))?? ??? ? chromosome?? ?? strata? ????? ??? ???? ???? plot? ??.

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

# 2. ? strata? ??? ????? ?? ??(y?? log scale? ??)? ???? boxplot? ???? ? ??? ?? ??
ggplot(data, aes(x = Strata, y = log(Length), colour = as.factor(Strata))) +
  geom_boxplot() +
  labs(colour = "Strata") +
  ggsave(filename = "FIG/o2job/Img_006.png", width = 10, height = 6, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(tidyverse)
library(ggplot2)
library(lubridate)
library(readr)
library(data.table)
library(stringr)

data = data.table::fread("INPUT/o2job/??????_1_8???????????????_2015?.csv", header = TRUE)

dplyr::tbl_df(data)


dataL1 = data %>%
  dplyr::rename("00~" = "24~") %>%
  dplyr::mutate(
    ??? = stringr::str_replace_all(??, stringr::str_c("\\(", ???, "\\)"), "")
  ) %>%
  tidyr::gather(-??, -???, -??, -??, -???, key = "key", value = "val") %>%
  dplyr::mutate(
    sYmdH = stringr::str_c(??, " ", stringr::str_sub(key, 1, 2))
    , dtYmdH = readr::parse_datetime(sYmdH, "%Y-%m-%d %H")
    , dtYear = lubridate::year(dtYmdH)
    , dtMonth = lubridate::month(dtYmdH)
    , dtHour = lubridate::hour(dtYmdH)
    , nVal = stringr::str_replace_all(val, "\\,", "")
  ) %>%
  dplyr::mutate_at(vars(nVal), funs(as.numeric))

dplyr::tbl_df(dataL1)

dataL2 = dataL1 %>%
  dplyr::group_by(???, ??, dtYear, dtMonth, dtHour) %>%
  dplyr::summarise(
    sumVal = sum(nVal, na.rm = TRUE)
  )

dplyr::tbl_df(dataL2)

# 1. 1?? ??? ? ?? ???? ??? ??? ????.(ex )??? : ??)
# ex): 15? 1? ?? ????? ????,
# 15? 1? ?? ????? ????,
# 15? 12? ??? ??

# 0??? 24?? ??
ind = which(dataL2$dtHour == 0)
dataL2[ind,]$dtHour = 24

dtYearList = unique(dataL2$dtYear)
dtMonthList = unique(dataL2$dtMonth)
dtTypeList = unique(dataL2$??)
dtStationList = unique(dataL2$???)

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
    , ?? == dtTypeList[k]
    , ??? == dtStationList[l]
  ) %>%
  ggplot(aes(x = as.factor(dtHour), y = sumVal, fill = as.factor(dtHour))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sumVal), vjust = 1.6, color = "white", size = 3.5) +
  labs(
    x = "??"
    , y = "??? ?? [?]"
    , fill = "??"
    , title = titleLabel
  ) +
  ggsave(filename = saveName, width = 12, height = 8, dpi = 600)
# }
# }
# }
# }

# 2. R???? ???? ????.
#
# ???? ?? ?? ??, ?, ??, ???? ?? ?? ??
dataL3 = dataL2 %>%
  dplyr::filter(
    dtYear == 2015
    , dtMonth == 1
    , ?? == "??"
    , ??? == "????"
  )

titleLabel = paste0("[", dataL3$dtYear[1], "-", dataL3$dtMonth[1], "] ", dataL3$??[1], " : ", dataL3$???[1])
saveName = paste0("FIG/o2job/TMP/Img_", dataL3$dtYear[1], "_", ddataL3$dtMonth[1], "_", dataL3$??[1], "_", dataL3$???[1], ".png")

ggplot(dataL3, aes(x = as.factor(dtHour), y = sumVal, fill = as.factor(dtHour))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sumVal), vjust = 1.6, color = "white", size = 3.5) +
  labs(
    x = "??"
    , y = "??? ?? [?]"
    , fill = "??"
    , title = titleLabel
  ) +
  ggsave(filename = saveName, width = 12, height = 8, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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

# data = readxl::read_excel("INPUT/o2job/????_??.xlsx", sheet = "????")

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

# ?? ??
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
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(tidyverse)
library(readxl)
library(readr)
library(lubridate)
library(scales)
library(ggpubr)

data = readxl::read_excel("INPUT/o2job/6???.xlsx", sheet = "Sheet4")

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
  labs(title = "??? ????", x = "?? ?", y = "? ??", subtitle = "??: ??") +
  ggsave(filename = "FIG/o2job/Img_Case01.png", width = 8, height = 6, dpi = 600)


dataL1 %>%
  dplyr::filter(type == "case03") %>%
  ggscatter(x = "date", y = "val", color = "label", add = "reg.line", fullrange = TRUE, conf.int = FALSE) +
  # stat_regline_equation(aes(color = label), label.x = 1) +
  # stat_cor(aes(color = label), label.x = 4) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  labs(title = "? ?? ????(???)", x = "?? ?", y = "??", subtitle = "??: ??") +
  ggsave(filename = "FIG/o2job/Img_Case03.png", width = 8, height = 6, dpi = 600)

dataL1 %>%
  dplyr::filter(type == "case04") %>%
  ggscatter(x = "date", y = "val", color = "label", add = "reg.line", fullrange = TRUE, conf.int = FALSE) +
  # stat_regline_equation(aes(color = label), label.x = 1) +
  # stat_cor(aes(color = label), label.x = 4) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  labs(title = "? ?? ????(????)", x = "?? ?", y = "??", subtitle = "??: ??") +
  ggsave(filename = "FIG/o2job/Img_Case04.png", width = 8, height = 6, dpi = 600)


dataL1 %>%
  dplyr::filter(type == "case05") %>%
  ggscatter(x = "date", y = "val", color = "label", add = "reg.line", fullrange = TRUE, conf.int = FALSE) +
  # stat_regline_equation(aes(color = label), label.x = 1) +
  # stat_cor(aes(color = label), label.x = 4) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  scale_y_continuous(breaks = seq(18000, 24000, 1000)) +
  labs(title = "??? ??", x = "?? ?", y = "?? ?", subtitle = "??: ?") +
  ggsave(filename = "FIG/o2job/Img_Case05.png", width = 8, height = 6, dpi = 600)


dataL1 %>%
  dplyr::filter(type == "case06") %>%
  ggscatter(x = "date", y = "val", color = "label", add = "reg.line", fullrange = TRUE, conf.int = FALSE) +
  # stat_regline_equation(aes(color = label), label.x = 1) +
  # stat_cor(aes(color = label), label.x = 4) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  scale_y_continuous(breaks = seq(0, 50000, 10000)) +
  labs(title = "????", x = "?? ?", y = "??", subtitle = "??: ??") +
  ggsave(filename = "FIG/o2job/Img_Case06.png", width = 8, height = 6, dpi = 600)


dataL1 %>%
  dplyr::filter(type == "case07") %>%
  ggscatter(x = "date", y = "val", color = "label", add = "reg.line", fullrange = TRUE, conf.int = FALSE) +
  # stat_regline_equation(aes(color = label), label.x = 1) +
  # stat_cor(aes(color = label), label.x = 4) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  # scale_y_continuous(breaks=seq(18000, 24000, 1000)) +
  labs(title = "????(????) / ??? ??", x = "?? ?", y = "??", subtitle = "??: ??") +
  ggsave(filename = "FIG/o2job/Img_Case07.png", width = 8, height = 6, dpi = 600)


data2 = readxl::read_excel("INPUT/o2job/6???.xlsx", sheet = "Sheet2")

ggplot(data2, aes(x = key, y = val, fill = label)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "??? ????(?????)_6? ??", x = "", y = "????", subtitle = "??: ??", fill = "??") +
  ggsave(filename = "FIG/o2job/Img_Case02.png", width = 8, height = 6, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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


# ???? >

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
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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

# data = readxl::read_excel("INPUT/o2job/????_??.xlsx", sheet = "????")

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

# ?? ??
remDr$open()

getRootHandle = remDr$getWindowHandles()

# ?? URL? ??
remDr$navigate("https://play.google.com/store/apps/details?id=com.truefriend.neosmarta&showAllReviews=true")
remDr$navigate("https://play.google.com/store/apps/details?id=com.wooriwm.txsmart&showAllReviews=true")
remDr$navigate("https://play.google.com/store/apps/details?id=com.linkzen.app&showAllReviews=true")
remDr$navigate("https://play.google.com/store/apps/details?id=com.samsungpop.android.mpop&showAllReviews=true")
remDr$navigate("https://play.google.com/store/apps/details?id=com.miraeasset.trade&showAllReviews=true")

# css? body? element? ?? ??
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


# ??? ?? ?? ????
frontPage = remDr$getPageSource()

# ??? ?? ???? ?? ??? ?? ????
reviewNames = read_html(frontPage[[1]]) %>%
  html_nodes('.bAhLNe.kx8XBd') %>%
  html_nodes('.X43Kjb') %>%
  html_text()

# ??? ?? ???? ?? ?? ?? ? ?? ?? ????
reviewDates = read_html(frontPage[[1]]) %>%
  html_nodes('.bAhLNe.kx8XBd') %>%
  html_nodes('.p2TkOb') %>%
  html_text()

# ??? ?? ???? ?? ?? ?? ????
reviewComments = read_html(frontPage[[1]]) %>%
  html_nodes('.UD7Dzf') %>%
  html_text()

# ??? ??? ??
reviewData = data.frame(
  name = reviewNames
  , date = reviewDates
  , comment = reviewComments
  , type = "???????S"
)

# ?? ?? ??? CSV ??? ??
write.csv(reviewData, paste0("OUTPUT/o2job/GooglePlayReview03.csv"))

remDr$close()

#==================================================
# ??? ???
#==================================================

fileList = Sys.glob("OUTPUT/o2job/GooglePlayReview*.csv")

# fileList ??? ??
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

# html? ????
saveWidget(fig, "fig.html", selfcontained = FALSE)

# ??? html ?????? png ??? ???? ??
webshot::webshot("fig.html", "FIG/o2job/GooglePlay_Keyword.png", vwidth = 775, vheight = 550, delay = 10)

# ??
xlsx::write.xlsx2(dataL3, file = "OUTPUT/o2job/GooglePlay_Keyword.xlsx", sheetName = "keyword", append = FALSE, row.names = FALSE, col.names = TRUE)

library(xlsx)

# [1??] ?? ???? 1? ??
# ?? 4000

# [2??] ?? ?? ??
data = read.xlsx("OUTPUT/o2job/GooglePlay_Keyword.xlsx", sheetName = "keyword", encoding = "UTF-8")

head(data)
# token freq
# 1    ??  4000
# 2       ? 3375
# 3 ???? 2749
# 4     ?? 2284
# 5   ??? 1917
# 6     ?? 1589

# [3??]
fig = wordcloud2(data = data)

# [4??] html? ????
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# [5??] html ?????? png ??? ???? ??
webshot::webshot("fig.html", "FIG/o2job/GooglePlay_Keyword.png", vwidth = 775, vheight = 550, delay = 10)

#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

# ?????. ?????? ???? ???? ?? ???.
#
# ?? -> 0 (??), 1(??)
# ?? -> 23~64
# ?? -> 1(????) 2(???) 3(4??) 4(???)
# ????? -> 1(?????) 2(?????)
# ?? -> 1(??) 2(??)
# ? ?? -> 2~11
# ?? -> ??(1) ??(2) ??(3) 4(??) 5(??) 6(??) 7(??) 8(??) 9(???)
# ???? ?? ?? -> 0~35.07
# ??? ???? ?? ?? ??-> 0~14.1
#
# ????? ??? ??? ??? ????? ????? ??? ???. ???? ?? powerbi ?? corelation ? ?? ???? clustering ? ?? ??? ? ???? ?? corelation ? ??? ??? ?? ???? ??? ??? ?? logistics regression? ??? ????, ??(0,1)? ??? ??? ???? ???? ? ???? ??? ? ??? ???? ??? ??? ??? ?????? Powerbi ?? R? ?? ???? ???.
#
# ??? ?? ?? ?? ??? ?????
#     ??? ???? ??? ?? ??????.
#
# ??? ?? ? 100???? ??? ?? ??? ????


# ??? ?? ??? ???? ????
#     - ?? ??(???? : ?? / ???? : ?? ? ??) ? ?? ???? ?????? ??
# - ?? ???? P-value ? ???? ?? > ?? ?? ??

# ????? ??
library(xlsx)
library(MASS)
library(ROCR)
library(abdiv)

# ??? ???? ???? ??? ?? ?? ???? ?? ???? ?? ?? ?? ?? ??? ??? ?
# ? ? ??? ?? ? ??? ? ?????? ??
set.seed(3)

# ?? ?? ??
data = xlsx::read.xlsx("INPUT/o2job/LogisticsRegression.xlsx", sheetName = "in", encoding = "UTF-8")

# ?? ???? NA?? ??
dataL1 = na.omit(data)

# ??? ?? (number > factor)
dataL1$???? = factor(dataL1$????)

#=====================================================================
# ??? ?? ??
#=====================================================================
# Initial Model:
#     ???? ~ ?? + ?? + ???.?? + ?? + ??? + ?? +
#     ???? + ???.??.?? + ??
#
# Final Model:
#     ???? ~ ?? + ??? + ?? + ???? + ???.??.??

# ?? ??? ?? ???? ???? ??
# ???? : ???? ??? ?? ??
# ???? : ????
glmFitVarAll = glm(???? ~ ., data = dataL1, family = binomial)

# 1) ????? ?? ??
# rsStep = step(glmFitVarAll)
# summary(rsStep)

# 1) AIC ???? ?? ??
rsStepAic = MASS::stepAIC(glmFitVarAll, direction = "both")

# ??? ?? ??
summary(rsStepAic)

# ? ?? ?? ?? ?? ??
rsStepAic$anova

#=====================================================================
# ?? ? ??? ? ?? (60 : 40)
#=====================================================================
# ?? ? ??? ?? 60:40?? ??? ?? ??? ??
ind = sample(1:nrow(dataL1), nrow(dataL1) * 0.6)

# ?? ???? ?? ?? ??
trainData = dataL1[ind,]
testData = dataL1[-ind,]

# ?? ???? ??
dplyr::tbl_df(trainData)

# ??? ???? ??
dplyr::tbl_df(testData)

#=====================================================================
# ?? ??? ????
# ?? ???? ??? ???? ??
# ??? ???? ??? ?? ??
#=====================================================================
# ?? ??? ?? ???? ???? ??
# ???? : ???? ??? ?? ??
# ???? : ????
library(RmecabKo)
glmFit = glm(???? ~ ., data = trainData, family = binomial)

# ????? ?? ?? ??
summary(glmFit)

# ?? ????
yObs = as.numeric(as.character(testData$????))

# ????? ??? ?? ????
yHat = predict.glm(glmFit, newdata = testData, type = "response")
# yHat =ifelse(yHatPred > 0.5, 1, 0)

# ?? ??? ?? ?? ??
lmPred = ROCR::prediction(yHat, yObs)

# ROC ??? ?? ??
perform = ROCR::performance(lmPred, "tpr", "fpr")
plot(perform, main = 'ROC Curve')

# AUC ?? : 1? ????? ?? ?? : 0.81074
ROCR::performance(lmPred, "auc")@y.values[[1]]

# ???? ?? : ???? ?? : 19.61
abdiv::binomial_deviance(yObs, yHat)

#=====================================================================
# ??? ??? ???
# ?? ???? ??? ???? ??
# ??? ???? ??? ?? ??
#=====================================================================
## ?? ??? ?? ???? ???? ??
# ???? : ??, ??, ????, ???.??.??
# ???? : ????
glmFitSel = glm(???? ~ ?? + ?? + ???? + ???.??.??, data = trainData, family = binomial)

# ?? ????
yObs = as.numeric(as.character(testData$????))

# ????? ??? ?? ????
yHat = predict.glm(glmFitSel, newdata = testData, type = "response")
# yHat =ifelse(yHatPred > 0.5, 1, 0)

# ?? ??? ?? ?? ??
lmPred = ROCR::prediction(yHat, yObs)

# ROC ??? ?? ??
perform = ROCR::performance(lmPred, "tpr", "fpr")
plot(perform, main = 'ROC Curve')

# AUC ?? : 1? ????? ?? ?? : 0.82967
ROCR::performance(lmPred, "auc")@y.values[[1]]

# ???? ?? : ???? ?? : 19.338
abdiv::binomial_deviance(yObs, yHat)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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
# ??? ???
#=============================================
pageCnt = (1535 %/% 10) + 1

# ?? 1?, ??? ??
contextPath = paste0("https://www.10000recipe.com/recipe/list.html?q=", utils::URLencode(iconv("??", to = "UTF-8")))
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
  log4r::info(log, paste0("??? : ", round((i / nrow(data)) * 100, 2), " %"))
}


dataL3 = dataL2 %>%
  dplyr::filter(!token %in% c("??")) %>%
  dplyr::group_by(token) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame()

dplyr::tbl_df(dataL3)

# ??
xlsx::write.xlsx2(dataL3, file = "OUTPUT/o2job/10000_Recipe.xlsx", append = FALSE, row.names = FALSE, col.names = TRUE)

fig = wordcloud2::wordcloud2(data = dataL3)

# html? ????
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# ??? html ?????? png ??? ???? ??
webshot::webshot("fig.html", "FIG/o2job/10000_Recipe.png", vwidth = 775, vheight = 550, delay = 10)


#=============================================
# ??? ???
#=============================================
remDr = remoteDriver(
  remoteServerAddr = "localhost"
  , port = 5000L
  , browserName = "chrome"
)

# cd selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 5000

# ?? ??
remDr$open()

# NAVER ??
remDr$navigate("https://nid.naver.com/nidlogin.login?mode=form&url=https%3A%2F%2Fwww.naver.com")

# ??? ??
remDr$findElement(using = "id", value = "id")$setElementAttribute("value", "backjoi")

# ???? ??
remDr$findElement(using = "id", value = "pw")$setElementAttribute("value", "cjswo123!Q")

# ??? ??
remDr$findElement(using = "id", value = "log.login")$clickElement()


iCount = 1
isFlag = TRUE

dtEndDate = rev(format(seq(lubridate::ymd("2004-08-13"), lubridate::ymd("2020-08-13"), by = "1 years"), "%Y%m%d"))

urlInfo = paste0("https://post.naver.com/search/post.nhn?keyword=%EC%97%AC%EB%A6%84%20%EC%9A%94%EB%A6%AC&sortType=createDate.dsc&range=20000409000000:", dtEndDate[iCount], "235959")

remDr$navigate(urlInfo)

# css? body? element? ?? ??
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
  log4r::info(log, paste0("??? : ", round((i / nrow(data)) * 100, 2), " %"))
}


dataL3 = dataL2 %>%
  dplyr::filter(!token %in% c("??", "???", "??")) %>%
  dplyr::group_by(token) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame()

dplyr::tbl_df(dataL3)

# ??
xlsx::write.xlsx2(dataL3, file = "OUTPUT/o2job/Naver_Post.xlsx", append = FALSE, row.names = FALSE, col.names = TRUE)

fig = wordcloud2::wordcloud2(data = dataL3)

# html? ????
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# ??? html ?????? png ??? ???? ??
webshot::webshot("fig.html", "FIG/o2job/Naver_Post.png", vwidth = 775, vheight = 550, delay = 10)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

# ??? ??, ??, ?? ?? ?? ???? ??? ???? ?? ? ???? ?? ???? ??? ???? ?? ??? ??? ?????? (??? ???? ???)

# ??? ?? ??? ???? ????

# 1) ??? ??? ?? > ?? ??
#    - 6? ??? ?? : PTSD, ??, ????, ??, ???, ?? ? ???? ??
#    - ?? ?? : ?? 1?
#    - ?? ?? : ??, ??, ??, ??
# 2) ?? ?? ???? ?? : ???, ?? ??, ?? ??, ?? ??
# 3) ?? ??? : ?? ??, ??? ??? ??? ?? (? ??? ??)
# 4) ???? : ?? ?? ???
# 5) ?? ???? ??? : ?? ?? ???
# 6) ???? ?? : ?? ?? ???

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
# library(KoNLP) #????? "KoNLP" ???? ?????

# devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)
# Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_261')  # ??? JAVA version? ?? ?????
# buildDictionary(ext_dic = "woorimalsam")  # "woorimalsam" dic? ?????
# useNIADic()  # "NIADic" dic? ?????


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


keywordList = c("PTSD", "??", "????", "??", "???", "?? ? ???? ??")
cntList = c(2978, 33675, 159814, 512403, 1545, 5089)

# seq(1, 121, 10)

for (i in 1:length(keywordList)) {

  # ?? 1?, ??? ??
  contextPath = paste0("https://search.naver.com/search.naver?where=news&query=", utils::URLencode(iconv(keywordList[i], to = "UTF-8")), "&sm=tab_opt&sort=0&photo=0&field=0&reporter_article=&pd=5&ds=2019.08.09&de=2020.08.08&docid=&nso=so%3Ar%2Cp%3A1y%2Ca%3Aall&mynews=1&")

  urlInfo = paste0(contextPath, sprintf("start=%d&refresh_start=0", seq(1, cntList[i], 10)))

  log4r::info(log, paste0("??? : ", keywordList[i]))

  urlDtlInfo = urlInfo %>%
    purrr::map(~getUrlTagHref(.x, 'ul > li > dl > dd > a')) %>%
    unlist()

  log4r::info(log, paste0("urlDtlInfo : ", length(urlDtlInfo)))

  data = data.frame()
  for (urlInfo in urlDtlInfo) {

    html = read_html(urlInfo)

    #=================================================
    # ??
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
    # ??
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
    # ??
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
      log4r::info(log, paste0("?? ??? ????. : ", url))
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

  log4r::info(log, paste0("?? ?? : ", nrow(data)))

}


#==================================================
# ??? ???
#==================================================

fileList = Sys.glob("OUTPUT/o2job/Naver_News_Keyword_*.csv")

# fileList ??? ??
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
  dplyr::filter(type %in% c("????", "????", "????", "????"))

dplyr::glimpse(data)

dataL2 = data.frame()
foreach::foreach(i = 1:nrow(data), .combine = c) %do% {

  dataL1 = RcppMeCab::pos(as_utf8(data$contentInfo[i]), format = "data.frame") %>%
    dplyr::filter(pos == "NNG") %>%
    dplyr::select(token)

  tmpData = data.frame(did = i, keyword = data$keyword[i], dataL1)

  # dataL2 = dplyr::bind_rows(dataL2, data.frame(type = data$type[i], dataL1))
  # log4r::info(log, paste0("??? : ", round((i / nrow(data)) * 100, 2), " %"))

  readr::write_csv(x = tmpData, path = paste0("OUTPUT/o2job/Naver_News_Keyword4.csv"), append = TRUE)
}


#==================================================
# ??? ??
#==================================================

# dataL2 = readr::read_csv(file = paste0("OUTPUT/o2job/Naver_News_Keyword.csv"), col_names = c("token"))
# dataL2 = read.csv(file = paste0("OUTPUT/o2job/Naver_News_Keyword2.csv"), sep = ";", header = FALSE, col.names = c("type", "token"))
# dataL2 = readr::read_csv(file = paste0("OUTPUT/o2job/Naver_News_Keyword3.csv"), col_names = c("type", "token"))
dataL2 = readr::read_csv(file = paste0("OUTPUT/o2job/Naver_News_Keyword4.csv"), col_names = c("type", "keyword", "token"))

#==================================================
# ??? ??? ?? ???
#==================================================
keywordData = dataL2 %>%
  dplyr::filter(!token %in% c("?", "??")) %>%
  dplyr::group_by(token) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame() %>%
  dplyr::top_n(n = 50)

fig = wordcloud2::wordcloud2(data = keywordData)

# html? ????
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# ??? html ?????? png ??? ???? ??
webshot::webshot("fig.html", "FIG/o2job/Naver_News_Keyword.png", vwidth = 775, vheight = 550, delay = 10)


dataL3 = dataL2 %>%
  dplyr::filter(!token %in% c("?", "??")) %>%
  # dplyr::group_by(type, token) %>%
  dplyr::group_by(keyword, token) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame()

tbl_df(dataL3)

#==================================================
# TF ? TF-IDF ??
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

# html? ????
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# ??? html ?????? png ??? ???? ??
webshot::webshot("fig.html", "FIG/o2job/Naver_News_Keyword_TF_IDF.png", vwidth = 775, vheight = 550, delay = 10)

#==================================================
# ?????, ?????
#==================================================
# ?? ????? ???
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
  dplyr::mutate(degreeCent = tidygraph::centrality_degree()) %>% # ?? ???
  dplyr::mutate(eigenVectorCent = tidygraph::centrality_eigen()) %>% #?? ???
  as_tibble() %>%
  arrange(desc(degreeCent)) %>%
  as.data.frame()

xlsx::write.xlsx2(dataL7, file = "OUTPUT/o2job/Naver_News_Keyword_Result.xlsx", sheetName = "CENT", append = TRUE, row.names = FALSE, col.names = TRUE)

#==================================================
# CONCOR ??
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
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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

data_namuh <- read.csv("INPUT/o2job/????? ??(???? ??).csv")
data_bankis <- read.csv("INPUT/o2job/?????? (???? ??).csv")

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
# ?? ??
#==================================================
dataL2 = data.frame()
foreach::foreach(i = 1:nrow(data_namuh), .combine = c) %do% {

  dataL1 = RcppMeCab::pos(as_utf8(data$??[i]), format = "data.frame") %>%
    dplyr::filter(pos == "NNG") %>%
    dplyr::select(token)

  dataL2 = dplyr::bind_rows(dataL2, dataL1)

  # log4r::info(log, paste0("??? : ", round((i / nrow(data)) * 100, 2), " %"))
}


#==================================================
# ??? ??? ?? ???
#==================================================
keywordData = dataL2 %>%
  dplyr::group_by(token) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame()

xlsx::write.xlsx2(keywordData, file = "OUTPUT/o2job/App.xlsx", sheetName = "namuh", append = TRUE, row.names = FALSE, col.names = TRUE)
xlsx::write.xlsx2(keywordData, file = "OUTPUT/o2job/App.xlsx", sheetName = "bankis", append = TRUE, row.names = FALSE, col.names = TRUE)

# ?? 50?
fig = wordcloud2::wordcloud2(data = keywordData %>%
  dplyr::top_n(n = 50)
)

# html? ????
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# # ??? html ?????? png ??? ???? ??
# webshot::webshot("fig.html", "FIG/o2job/App_Namuh.png", vwidth = 775, vheight = 550, delay = 10)
webshot::webshot("fig.html", "FIG/o2job/App_Bankis.png", vwidth = 775, vheight = 550, delay = 10)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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

sheetList = c("?? ????(I21) ??")

data = readxl::read_excel("INPUT/o2job/2015-2019_?????_????-3.xlsx", sheet = sheetList)

dplyr::glimpse(data)

dataL1 = na.omit(data)

dataL2 = dataL1 %>%
  dplyr::rename("dtDateTime" = "????") %>%
  dplyr::mutate(
    year = lubridate::year(dtDateTime)
    , month = lubridate::month(dtDateTime)
    , day = lubridate::day(dtDateTime)
    , korAddr = stringr::str_c(`???(?,?)`, ???, ???, sep = " ")
  )

# dataL3 = ggmap::mutate_geocode(dataL2, korAddr, source="google") %>%
# as.data.frame()
dataL3 = readxl::read_excel("OUTPUT/o2job/?????_Result.xlsx", sheet = sheetList)

# ??, ????? ?? ?? ??
dataL4 = dataL3 %>%
  dplyr::group_by(korAddr, lon, lat) %>%
  dplyr::summarise(
    ?? = mean(??, na.rm = TRUE)
    , ???? = n()
  )

cenVal = c(mean(dataL4$lon, na.rm = TRUE), mean(dataL4$lat, na.rm = TRUE))
mapMarker = get_googlemap(center = cenVal, maptype = "hybrid", zoom = 9)

ggmap(mapMarker, extent = "device") +
  geom_point(data = dataL4, aes(x = lon, y = lat, size = ??, colour = ????), alpha = 0.75) +
  scale_color_gradientn(colours = rainbow(10)) +
  scale_radius() +
  labs(title = sheetList) +
  ggsave(filename = "FIG/o2job/Image_100.png", width = 8, height = 7, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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

#### ??? ??? ????

Keyword = readLines("INPUT/o2job/report/Keyword.txt")
Keyword = gsub("\\.", "", Keyword) # ????? ???? '.' ??

#### ?? ?? ??? ????

Remove = read.csv("INPUT/o2job/report/Remove.csv",
                  header = TRUE, stringsAsFactors = FALSE)

#### ?? ?? ??? ????

Replace = read.csv("INPUT/o2job/report/0401.csv")

#### ????, ?? ??

Keyword2 = Keyword %>%
  str_replace_all("[0-9]+", " ") %>%
  str_replace_all("\\s+", " ") %>%
  str_replace_all("http[a-zA-Z0-9]+", "") %>%
  str_replace_all('\n', ' ') %>%
  str_replace_all('\t', ' ')


##### gsub? ?? ?? ???? ??

Keyword2 = gsub("\\,", " ", Keyword2)
Keyword2 = gsub("[^?-?]", " ", Keyword2)
Keyword2 = gsub("??", "", Keyword2)
Keyword2 = gsub("??", "", Keyword2)
Keyword2 = gsub("??", "", Keyword2)
Keyword2 = gsub("???", "", Keyword2)
Keyword2 = gsub("??", "", Keyword2)
Keyword2 = gsub("??", "", Keyword2)
Keyword2 = gsub("??", "", Keyword2)
Keyword2 = gsub("??", "", Keyword2)
Keyword2 = gsub("??", "", Keyword2)
Keyword2 = gsub("??", "", Keyword2)
Keyword2 = gsub("??", "", Keyword2)
Keyword2 = gsub("???", "", Keyword2)
Keyword2 = gsub("??", "", Keyword2)
Keyword2 = gsub("??", "", Keyword2)

Keyword2_2 = Keyword2

#### ??? ??? ?? ?? ?? (??, 4? -> 4? ??)

R = c("4?", "??", "??", "???", "???", "??", "??", "???", "???", "??", "??")
R2 = c("4???", "??", "???", "???", "???", "??", "??", "???", "???", "4???", "????")

RR = c(Replace$word, R)
RR2 = c(Replace$??, R2)

Replace2 = data.frame(
  Before = RR,
  After = RR2
)

#### ? ??? ???? ??? ?? ?? ??

Keyword_V = c()

for (i in 1:length(Keyword2_2)) {

  if (i %% 100 == 0) {

    print(i)

  }

  STR = ""

  for (k in unlist(strsplit(Keyword2_2[i], " "))) {  # ??? ????? ???? ??

    #### ??? ???? ?? ????? ?? ??

    if (k != "") { # ??? ???? ""(??)? ???

      if (nchar(k) > 1) { # ??? ???? ???? 2? ??? ?

        for (s in 1:length(R)) {

          Index = grep(RR[s], k)

          if (length(Index) > 0) {

            k = RR2[s]

          }

        }

      }

    }

    STR = paste(STR, k) ## ??? ??? ?? ?? ???

  }

  Keyword_V[i] = STR

}

Keyword2_2[1] # Before
Keyword_V[1] # After

##### ??? ??? ?? ??

text = Corpus(VectorSource(Keyword_V)) # ??? ??? ?? ???(Corpus) ??

#### TermDocumentMatrix(??-?? ??) ??

tdm = TermDocumentMatrix(text,
                         control = list(tokenize = extractNoun))

#### ???? ?? ?? Counting

ft = data.frame(word = rownames(as.matrix(tdm)),
                freq = rowSums(as.matrix(tdm)))

##### ??? ?? ?? ??
Word_Freq = ft %>%
  mutate(word = as.character(word)) %>%
  filter(nchar(word) > 1) %>% # 2?? ?? ??? ???
  filter(!word %in% Remove$word) %>% # ?? ??? ?? ??? ??
  filter(freq > 10) %>% # ?? 11? ?? ??? ????? ???
  group_by(word) %>%
  summarise(freq = sum(freq)) %>% # ?? ???? ?? ?? ???, ?? ???? ???(?)
  arrange(-freq) # ???? ??? ??


## ??, ??, ?? ??? ??

Word_Freq2 = Word_Freq %>%
  filter(!word %in% c("??", "??", "??")) %>%
  filter(freq > 10)

## ???? -> ?????? ??

Word_Freq2$word[grep("????", Word_Freq2$word)] = "????"


## ? ?? ??? ?? ??? ??

Word_Freq2 = Word_Freq2 %>%
  mutate(word = as.character(word)) %>%
  filter(nchar(word) > 1) %>%
  filter(!word %in% Remove$word) %>%
  filter(freq > 70) %>%
  group_by(word) %>%
  summarise(freq = sum(freq)) %>%
  arrange(-freq)

### ?????? ???

wordcloud2(Word_Freq2, size = 0.6)

### ?? ??? 40? ??

Main_Keywords = Word_Freq2$word[1:40]


### ???? ?? ???
tdm1 = as.matrix(tdm)
tdm1 = tdm1[Main_Keywords,]
tam = tdm1 %*% t(tdm1)

qgraph(tam, labels = rownames(t(tam)), diag = F,
       layout = 'spring', edge.color = 'darkblue', vsize = 6)

#### ?? ?? csv ?? ??

write.csv(Word_Freq2,
          "OUTPUT/o2job/report/Word_Freq3.csv",
          row.names = FALSE)

####################

#### ?? ???

doc.list <- strsplit(Keyword_V, "[[:space:]]+") # ??? ????? ???? ??

## TermDocument Matrix ??

TDM_M = as.matrix(tdm)
TF = rowMeans(TDM_M) # ?????? ??
TF = sort(TF, decreasing = TRUE) # ????? ?? ?? ??? ??? ??
TF = TF[1:300] # ?? 300? ??? ??

term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)


term.table[1:50]

#####


TF2 = TF[nchar(names(TF)) > 1] # ??? 2? ?? ???? ???
TF2

Selected = names(term.table) %in% names(TF2) # ? 2??? ??? ??? ??
sum(Selected)
term.table = term.table[Selected]
vocab = names(term.table)

### ??? index ?? ??
### index? ?????? ?? index? ???? ?? ??
### ??? ???? ?? (??????? ???? ???)

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


### Topic??? TopicModeling ??

### Topic? ??? 3 ~ 10??? ?? ????? Topic Modeling ??

for (k in c(3, 4, 5, 6, 7, 8, 9, 10)) {

  print(k)

  K = k  # Topic ??
  G = 5000 # ?? ??
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

  ### ?? ?? ??

  write.csv(Topic_Words,
            paste0("OUTPUT/o2job/report/Topic_Words_", k, "topis.csv"),
            row.names = FALSE, fileEncoding = "CP949")

}


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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
# ? ?? ??
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
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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

# fileList ??? ??
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
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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

# ?????? ?? ?? ????
# http://kssc.kostat.go.kr/ksscNew_web/kssc/common/CommonBoardList.do?gubun=1&strCategoryNameCode=019&strBbsId=kascrr&categoryMenu=014


data = openxlsx::read.xlsx("INPUT/o2job/2015-2019_?????_????-3/2015-2019?????????2.xlsx", sheet = 1)

la = shapefile('INPUT/o2job/LSMD_ADM_SECT_UMD/LSMD_ADM_SECT_UMD_44.shp')
laData = ggplot2::fortify(la, region = 'EMD_CD')

# # rgeos::gIsValid(la)
# la <- rgeos::gBuffer(la, byid = TRUE, width = 0)
geo = spTransform(la, CRS("+proj=longlat"))
geoData = ggplot2::fortify(geo)

emdNmDB = la@data

# ???(?? 3?) > ???
# ??? > ???
dataL1 = data %>%
  dplyr::filter(
    !is.na(???)
    , `???(?,?)` == "????"
  ) %>%
  dplyr::left_join(emdNmDB, by = c("???" = "EMD_NM")) %>%
  dplyr::group_by(EMD_CD, ???) %>%
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
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

# F-??? F-?? (F-test)? ? ?? ??? ??? ??? ??? ???? ????(ANOVA, Analysis of Variance)? ????, (???? ????) ??? ??? ??? ????? ?(+)? ??? ??? ??, ???? ???? ????? ??? ? ???? ??? ?? ????.  (???? ??? ??? ???)

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

# F ???
fTest = var.test(reference, sample, conf.level = 0.95)

# F? ?? ??? 2 ??? ??? ??? ?? (? ??). ??? P < 0.05? ?? ?? ?? ??? ???? 2 ??? ??? ????, ? ??? ?? ? ????.
# F-test?? p-value 0.6328 (p > 0.05)?? ? ??? ? ????? ???. ??? 2 ??? ??? ???? t ??? ????.

fTest
plot(fTest) + xlim(-5, 15)

# ?? ??? 2 ??? ??? ??? ??.

# T ???
# ??? ?? O
tTest = t.test(reference, sample, conf.level = 0.95, var.equal = TRUE, paired = FALSE)

# ??? ?? X
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

# t = -3.5 | t???? ?? ??? ??
# df = 7.5294 | t? ???
# p-value = 0.008893 | 0.05?? ??? ????? ????. ? 2 ??? ??? ??? ??.
# alternative hypothesis: true difference in means is not equal to 0 | ?? ?? (??? 0? ???.)
# -0.0023325334 -0.0004674666 | 95% ????
# sample estimates | ?? ??? ???

tTest
plot(tTest)


# t = -3.5, df = 8, p-value = 0.008079
pt(q = -3.5, df = 8) * 2       # ?? ????? 2? ???.
## [1] 0.008079082

qt(p = (0.008079082 / 2), df = 8)    # ??? p-vale? ?? ? t ? ??
## [1] -3.5

# t ? ???
(mean(child) - mean(parent)) / (s / sqrt(n))
## [1] -2.878929

#=================================================
# ?? ??? ?? ??
#=================================================
diff <- y2 - y1
mn <- mean(diff)
s <- sd(diff)
n <- length(y1)
# ?? +- 95%??? ??? * ????(s / sqrt(n))
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
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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
library(KoNLP) #????? "KoNLP" ???? ?????

devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_261")
Sys.setenv(JAVA_HOME = 'C:\\Program Files\\Java\\jdk1.8.0_261')  # ??? JAVA version? ?? ?????
# buildDictionary(ext_dic = "woorimalsam")  # "woorimalsam" dic? ?????
# useNIADic()  # "NIADic" dic? ?????

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

# ?? ??
remDr$open()

getRootHandle = remDr$getWindowHandles()

# GLOWPICK ??
contextPath = paste0("https://www.glowpick.com/beauty/ranking?")
urlInfo = paste0(contextPath, "id=1&level=2&brand_category_id=")

# ??
remDr$navigate(urlInfo)

# ?? Url ?? ????
urlDtlInfoList = getXpathAttr('//*[@id="gp-list"]/div/section[2]/ul/li[*]/meta[2]', 'content')

i = 2
dataL1 = data.frame()

# foreach::foreach(i = 15:length(urlDtlInfoList), .combine=c) %do% {
for (i in 1:length(urlDtlInfoList)) {

  Sys.sleep(2)
  urlDtlInfo = urlDtlInfoList[i]
  remDr$navigate(urlDtlInfo)

  Sys.sleep(2)

  # ??
  title = getXpathText('//*[@id="gp-default-main"]/section/div/ul[1]/li[2]/section[1]/h1/span')

  # ???
  brand = getXpathText('//*[@id="gp-default-main"]/section/div/ul[1]/li[2]/section[1]/div[3]/span')

  # ?? ? ??
  tmpInfo = getXpathText('//*[@id="gp-default-main"]/section/div/ul[1]/li[2]/section[1]/div[2]/div[1]') %>%
    stringr::str_split("/") %>%
    unlist() %>%
    stringr::str_trim(side = "both")

  volume = tmpInfo[1]
  price = tmpInfo[2]

  # ??
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
  # ????
  #=====================
  isItem = FALSE
  # ?? ??
  for (k in 1:10) {
    xpathTag = paste0('//*[@id="gp-default-main"]/section/div/ul[1]/li[2]/section[', k, ']/div/span[2]/button/span[1]')

    popText = getXpathText(xpathTag)

    if (length(popText) > 0) {
      if (popText == "????") {
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

    # EWG ??? ??
    itemEwgLevel = getXpathText('//*[@id="gp-popup"]/div/section[2]/div[2]/ul/li[*]/div[1]/span[2]')

    # ???
    itemKor = getXpathText('//*[@id="gp-popup"]/div/section[2]/div[2]/ul/li[*]/div[2]/p[1]')

    # ???
    itemEng = getXpathText('//*[@id="gp-popup"]/div/section[2]/div[2]/ul/li[*]/div[2]/p[2]')

    # ??
    itemPurpose = getXpathText('//*[@id="gp-popup"]/div/section[2]/div[2]/ul/li[*]/div[2]/p[3]')

    # ?? ??
    remDr$findElement(using = "xpath", value = '//*[@id="gp-popup-bg"]/div/button')$clickElement()
  }

  #========================
  # ??
  #========================
  # ? ??
  totalScore = getXpathText('//*[@id="gp-default-main"]/section/div/ul[2]/li[2]/section/div/div/div[1]/div[1]')

  # ? ??
  totalCnt = getXpathText('//*[@id="gp-default-main"]/section/div/ul[2]/li[2]/section/div/div/div[1]/div[3]')

  # 5?? ?? ??
  stepScore = getXpathText('//*[@id="gp-default-main"]/section/div/ul[2]/li[2]/section/div/div/div[2]/ul/li[*]/div/p')

  #===============================
  # ??
  #===============================
  Sys.sleep(1)

  # "??? ???"?? ??
  remDr$findElement(using = "xpath", value = '//*[@id="gp-default-main"]/section/div/ul[2]/li[3]/section/div/div/button')$clickElement()
  remDr$findElement(using = "xpath", value = '//*[@id="gp-default-main"]/section/div/ul[2]/li[3]/section/div/div/div[2]/ul/li[3]')$clickElement()

  Sys.sleep(1)

  webElem = remDr$findElement("css", "body")
  remDr$findElement(using = "xpath", value = '//*[@id="gp-footer"]/div/div[1]/div[3]/figure/img')$clickElement()

  Sys.sleep(1)

  remDr$findElement(using = "xpath", value = '//*[@id="gp-footer"]/div/div[1]/div[3]/figure/img')$clickElement()


  # 10? ???
  # foreach::foreach(j = 1:10, .combine=c) %do% {
  for (j in 1:10) {
    Sys.sleep(0.2)

    # webElem$sendKeysToElement(list(key = "down_arrow"))
    webElem$sendKeysToElement(list(key = "home"))
    webElem$sendKeysToElement(list(key = "end"))
  }


  review = getXpathText('//*[@id="gp-default-main"]/section/div/ul[2]/li[5]/section/ul/li[*]/div/p')


  itemData = data.frame(itemEwgLevel, itemKor, itemEng, itemPurpose)

  # JSON ??
  itemJson = rjson::toJSON(itemData)

  # JSON?? DF ??
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
  , sheetName = "????2"
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
  , sheetName = "????2"
  , append = TRUE
  , row.names = FALSE
  , col.names = TRUE
)


# ? ??
dplyr::tbl_df(dataL1)

dataL3 = dataL1 %>%
  dplyr::distinct(cnt, title, itemJson)


#==============================================
# ?? ??
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
# 1. ?????? ??? ??? ?? ??? ?? ??? ?? ????? ?? ???? ??? ??

# ??? ??? ?? ???
dataL5 = dataDtlInfo %>%
  dplyr::group_by(itemKor) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame()

fig = wordcloud2::wordcloud2(data = dataL5)

# html? ????
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# ??? html ?????? png ??? ???? ??
webshot::webshot("fig.html", paste0(globalVar$figConfig, "/", "Glowpick_Crawling_Keyword.png"), vwidth = 775, vheight = 550)

xlsx::write.xlsx2(
  dataL5
  , file = paste0(globalVar$outConfig, "/", "Glowpick_CrawlingL2.xlsx")
  , sheetName = "???"
  , append = TRUE
  , row.names = FALSE
  , col.names = TRUE
)

# 2. ?? ?? ? ???? ??? ?? ? ??? ??? ?? ??? ??
library(SentimentAnalysis)
library(tidyverse)

# head(dataInfo$review)

library(stringr)
doc <- dataInfo$review
docs <- str_replace_all(doc, "[^0-9a-zA-Z?-??-??-?[:space:]]", " ")
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


res_sentiment <- analyzeSentiment(corp, #??? corpus,
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
  , sheetName = "??????4"
  , append = TRUE
  , row.names = FALSE
  , col.names = TRUE
)


# ?? (?? 10%)
dataL8 = dataL7 %>%
  head(10) %>%
  dplyr::left_join(dataDtlInfo, by = c("cnt" = "cnt"))

# ??? ??? ?? ???
dataL9 = dataL8 %>%
  dplyr::group_by(itemKor) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame()


xlsx::write.xlsx2(
  dataL9
  , file = paste0(globalVar$outConfig, "/", "Glowpick_CrawlingL2.xlsx")
  , sheetName = "??????3"
  , append = TRUE
  , row.names = FALSE
  , col.names = TRUE
)

fig = wordcloud2::wordcloud2(data = dataL9)

# html? ????
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# ??? html ?????? png ??? ???? ??
webshot::webshot("fig.html", paste0(globalVar$figConfig, "/", "Glowpick_Crawling_Keyword2.png"), vwidth = 775, vheight = 550, delay = 10)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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
# # COSMIC90_coding=. ???
dataL2 = dataL1 %>%
  dplyr::filter(!stringr::str_detect(INFO_167_1, "COSMIC90_coding"))

stringr::str_detect(dataL1$INFO, "COSMIC90_coding=.")
#
# dataL1$INFO
#
#
# dataL2$INFO_167

# ????? ?? ?? 8?? column??:
#     DP=>10
# AF <0.001 ?? AF =0, AF=. ?? ?????
#
#
# COSMIC90_coding=. ???
# COSMIC90_noncoding=. ???
# ?? ?? ??? ??????
#

#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(ggplot2)
library(tidyverse)
library(caret)
library(readr)
library(Metrics)

data = readr::read_csv(file = paste0(globalVar$inpConfig, "/", "insurance.csv"))

# 1) ??? ?????.
# (Hint : Summary ?? ??)

summary(data)

# ??? ?? ??
data$sex = as.factor(data$sex)
data$smoker = as.factor(data$smoker)
data$region = as.factor(data$region)

# 2) Train set? Test set?? ???? ?? ???
# (Hint : CreateDataPartition?? ??)

indexTrain = createDataPartition(data$charges, p = 0.7, list = FALSE)
trainData = data[indexTrain,]
testData = data[-indexTrain,]


# 3) lm()??? ??? ??????? ?????.
lmFit = lm(charges ~ ., data = trainData)

summary(lmFit)

# 4) Test set? ????, MAE? RMSE ?? ????.
testData %>%
  dplyr::summarise(
    rmse = Metrics::rmse(charges, predict(lmFit, newdata = testData))
    , mae = Metrics::mae(charges, predict(lmFit, newdata = testData))
  )

# 5) BMI ??? ????? ??????? ???? ??? ?? ?????.
lmFit = lm(charges ~ . + bmi * smoker, data = trainData)

# 6) 5)? ??? MAE? RMSE ?? ?? ?? ?? ??? ?????.

# ?? ??
testData %>%
  dplyr::summarise(
    rmse = Metrics::rmse(charges, predict(lmFit, newdata = testData))
    , mae = Metrics::mae(charges, predict(lmFit, newdata = testData))
  )

# 7) ? ??? ?? ???? ?????.
# Ex) A ??? ? ?? ???? ???? ??? ???/???.

lmFit = lm(charges ~ . + (bmi * sex), data = trainData)
summary(lmFit)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(tidyverse)
library(tools)
library(fs)

# 3???? ??2?? ??????
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
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

# 1. R Stduido ???? ??
# - ?????? Test ???? ??
#
# 2. Java ?? ??
# - cmd ??? java ?? ?? (java -version)
#
# 3. Java ???? ??
# - ?? ? ??
# - ????? KoNLP ????? ?? java 1.8?? ?? ??
# - ??? Java ???? ??
#
# 4. Java 1.8 ????
# - https://www.oracle.com/java/technologies/javase/javase-jdk8-downloads.html
# - jdk-8u261-windows-x64.exe
# - Open JRE? ??? Oracle Java ?? ??
# - ??? ???? ??
# - JRE ? JDK? ????? ??
# - cmd ??? java ?? ?? (java -version)
# C:\Users\inu>java -version
# java version "1.8.0_261"
# Java(TM) SE Runtime Environment (build 1.8.0_261-b12)
# Java HotSpot(TM) 64-Bit Server VM (build 25.261-b12, mixed mode)

# 5. Java ???? ??
# - JAVA_HOME : C:\Program Files\Java\jdk1.8.0_261
# - PATH : C:\Program Files\Java\jdk1.8.0_261\bin
# - cmd??? ???? ?? (echo %JAVA_HOME%)

# 6. Github?? ??? ? ?? ????? ?? ? ??
# - # install.packages("remotes")
# - library(remotes)

# 7. ??? ????? ??
# - install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"), type = "binary")
# - # install.packages("remotes")
# - library(remotes)
# - library(devtools)
# - # install.packages("rmarkdown")
# - library(rmarkdown)
# - # install.packages("RSQLite")
# - library(RSQLite)

# 8. rJava ??
# - library(rJava)

# 9. KoNLP ?? ? ??
# - # remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
# - library(KoNLP)

# 10. useNIADic() ??
# - # devtools::install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE, force = TRUE)
# - useNIADic()

# 11. ?? ??


#=====================================================
# KoNLP ????? (??) ??
#=====================================================
# 13? ?? ?? ? ?? ??
# 1) 11??? install_github? ?? ? ??
# 2) Rstudio ?? ? Test.Rproj ? ??
# 3) 13? ????? ??

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
# RcppMeCab ????? (?/??) ??
#=====================================================
# ????? ??
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
  # dplyr::filter(! token %in% c("??", "??", "??")) %>%
  dplyr::group_by(token) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame()

dplyr::tbl_df(dataL3)


# ??
xlsx::write.xlsx2(dataL3, file = paste0("XLSX/review_utf8_", format(Sys.time(), "%Y%m%d%H%M%S"), ".xlsx"), append = FALSE, row.names = FALSE, col.names = TRUE)

fig = wordcloud2::wordcloud2(data = dataL3)
fig

# html? ????
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# ??? html ?????? png ??? ???? ??
webshot::webshot("fig.html", paste0("FIG/review_utf8_", format(Sys.time(), "%Y%m%d%H%M%S"), ".png"), vwidth = 775, vheight = 550, delay = 10)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================
# 1. (25?) ? ? ? ??? ???(prime factor) 2? ??? ???? R ??? ??? ???
# ???? (?? ? ?? 2? 8? ??). ??? ? ? ??? ???? 2? ? ? ???

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

# 2. (25?) ???? ??? ? 300???, A, B, C, D, E ?? ???????? ?? ??
# (161, 13), (84, 19), (1, 5), (0, 3), (7, 7) ???. 30?? ??? ?????? ????
# ?? ???? ?? ???? ?? ? ???????? ???? 5*2 ?? ????.
# * sample() ??? ??. ??? ?????.

data = data.frame()
data = dplyr::bind_rows(
  data
  , data.frame(rowType = "A", colType = "???", val = seq(1, 161))
  , data.frame(rowType = "B", colType = "???", val = seq(1, 84))
  , data.frame(rowType = "C", colType = "???", val = seq(1, 19))
  , data.frame(rowType = "E", colType = "???", val = seq(1, 7))
  , data.frame(rowType = "A", colType = "????", val = seq(1, 13))
  , data.frame(rowType = "B", colType = "????", val = seq(1, 19))
  , data.frame(rowType = "C", colType = "????", val = seq(1, 5))
  , data.frame(rowType = "D", colType = "????", val = seq(1, 3))
  , data.frame(rowType = "E", colType = "????", val = seq(1, 7))
)

table(data$rowType, data$colType)

ind = sample(nrow(data), size = 30, replace = FALSE)
dataL1 = data[ind,]

table(dataL1$rowType, dataL1$colType)


# 3. (25?) ? ? ??? ?? ? ? ? ??? ? ? ?? ??? ? ? ??? ?? ?????? ??? R
# ??? ??? ????. ? ??? ?? ? ? ???? 5?? ?? ??? ???? (2? ??).
# * ?? sample() ??? ??? ? ??.

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


# 4. (25?) ??? ??? ??? ??? R ????? ????.
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
# paired? F? ??? Wilcoxon rank sum ??? ? ? T? ??? Wilcoxon signed
# rank ??? ? ? ???? R ????? ???? (??, ?? ?? ????? ??).

x = c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y = c(0.878, 0.647, 0.598, 2.05, 1.06, 1.28, 1.07, 3.14, 1.29)

wilcoxTestPairFalse = wilcox.test(x, y, paired = FALSE, alternative = "greater")
cat("W : ", wilcoxTestPairFalse$statistic, "\n")

wilcoxTestPairTrue = wilcox.test(x, y, paired = TRUE, alternative = "greater")
cat("V : ", wilcoxTestPairTrue$statistic, "\n")

#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

# [????]
# ????????.
# ??? ?? ???.

library(tidyverse)
library(reticulate)
library(stringr)
library(officer)
library(magrittr)
library(fs)
library(flextable)

# ?? (HWP)? ??? (txt)? ??
# https://cloudconvert.com/hwp-to-txt

#===============================
# R?? Anaconda3 ????
#===============================
# ???? ??
if (.Platform$OS.type == "windows") {
  Sys.setenv(RETICULATE_PYTHON = 'C:/ProgramData/Anaconda3/python.exe')
  Sys.setenv(PATH = paste("C:/ProgramData/Anaconda3/Library/bin", Sys.getenv()["PATH"], sep = ";"))
}

reticulate::py_discover_config()

reticulate::conda_list()
# name                                 python
# 1 Anaconda3 C:\\ProgramData\\Anaconda3\\python.exe

# ?? conda ??
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

# ????? ??
# from pykospacing import spacing
pykospacing = reticulate::import("pykospacing")

# pykospacing$spacing("???????????'1987'???????????10??????????????12?27?????1?10?????????R?KoNLP????????????????.")
pykospacing$spacing(stringr::str_remove_all("???? ??????? ??? ?????????? ???  ?????? ????? ?? ? ?? ? ? ? ? ? ? ?? ? ?????? ?? ??? ?????? ????? , ????  ??? ??? ???? ???? ? ????? ???? ???????????  ?? ????? ?? ?? ? ? ? ? ? ?", " "))

#===============================
# ? ???? ??
#===============================
# inpConfig :  chr "E:/02. ???/??iN/INPUT/o2job"
# fileList = Sys.glob(paste0(globalVar$inpConfig, "/DOC/*.txt"))
# fileList = Sys.glob(paste0(globalVar$inpConfig, "/DOC/1986?-1992?.txt"))
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
  # ?? ??
  #===============================
  # "D:/02. ???/??iN/OUTPUT/o2job/1986?.docx"
  outFile = paste(globalVar$outConfig, stringr::str_replace_all(fs::path_file(fileInfo), ".txt", "_1.docx"), sep = "/")

  doc = officer::read_docx() %>%
    officer::body_add_par(value = paste(dataL1[1:20000,]$value, "(LineBreak)(LineBreak)", collapse = ""))
  print(doc, target = outFile)

  outFile = paste(globalVar$outConfig, stringr::str_replace_all(fs::path_file(fileInfo), ".txt", "_2.docx"), sep = "/")

  doc = officer::read_docx() %>%
    officer::body_add_par(value = paste(dataL1[20000:nrow(dataL1),]$value, "(LineBreak)(LineBreak)", collapse = ""))

  print(doc, target = outFile)


  # ???? ??
  # Ctrl + H
  # "(LineBreak)(LineBreak)" to "^p" ??
}


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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
# R?? Anaconda3 ????
#===============================
# ???? ??
if (.Platform$OS.type == "windows") {
  Sys.setenv(RETICULATE_PYTHON = 'C:/ProgramData/Anaconda3/python.exe')
  Sys.setenv(PATH = paste("C:/ProgramData/Anaconda3/Library/bin", Sys.getenv()["PATH"], sep = ";"))
}


fileList = Sys.glob(paste0(globalVar$inpConfig, "/DOC/??_*.txt"))

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
  # dplyr::filter(! token %in% c("??", "??", "??")) %>%
  dplyr::group_by(token) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(desc(freq)) %>%
  as.data.frame()

dplyr::tbl_df(dataL3)

dataL3 = read.xlsx(file = paste(globalVar$outConfig, "DOC_Keyword.xlsx", sep = "/"), sheetName = "Sheet1", encoding = "UTF-8")

# ?? ??
# xlsx::write.xlsx2(dataL3, file = paste(globalVar$outConfig, "DOC_Keyword.xlsx", sep = "/"), append = FALSE, row.names = FALSE, col.names = TRUE)

fig = wordcloud2::wordcloud2(data = dataL3, size = 2)

# html? ????
htmlwidgets::saveWidget(fig, "fig.html", selfcontained = FALSE)

# ??? html ?????? png ??? ???? ??
webshot::webshot("fig.html", paste(globalVar$figConfig, "DOC_Keyword.png", sep = "/"), delay = 10)

dataL4 = dataL3 %>%
  dplyr::slice(1:100)

# ggsave? ?? ??? ??
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
# Purpose : ???? ???
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(MASS)
library(moonBook)
library(webr)
library(ggplot2)

# ?? ???? 50% ???? ??? ?? ??? ?? ???? ???? ?? ??? ???.
# ? ??? R? ???? ?? ??? ?? ??? ??? ?? 50 %? ?? ? ?? ??? LD50? ???

data = data.frame(
  dose = c(0.5, 1.0, 2.0, 5.0)
  , total = c(81, 84, 82, 80)
  , dead = c(11, 40, 66, 80)
  , deadRatio = c(13.6, 47.6, 80.5, 100.0)
)

glmModel = glm(log(dose) ~ cind(total - dead, dead), family = binomial(link = logit), data = data)
summary(glmModel)  # p-value almost agree for the b parameter

# 50% ??? ??? ?? (p)
xp = MASS::dose.p(glmModel, p = 0.50)

# ?? ??? ?? (estVal)
estVal = exp(cbind(xp))
estVal

# ? ??? 0.50? ?? ?? ??? ?? 1.0527???.

# A? B ? ?? ?? ??? ?? ??? ???? ??? ?????? 4?? ???? ? ??? ???
# ?? ? 5? ?? ???? ???? 10? ???? 5?? ?? ?????? ??? ????.

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

# P?? 0.47883??? ???? ???? ?? (? ?? ?? ??? ??)
# ??? ??? ?? (var.equal = TRUE)
fTest = var.test(val ~ type, data = dataL2)
fTest

plot(fTest) +
  xlim(0, 5) +
  ggsave(filename = paste(globalVar$figConfig, "fTest.png", sep = "/"), width = 10, height = 6, dpi = 600)

# P?? 0.054?? ???? ?? (? ?? ?? ??? ??? ??)
tTest = t.test(val ~ type, data = dataL2, var.equal = TRUE)
tTest

plot(tTest) +
  xlim(-5, 5) +
  ggsave(filename = paste(globalVar$figConfig, "tTest.png", sep = "/"), width = 10, height = 6, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? (??, ???)
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(readr)
library(tidyverse)
library(lubridate)
library(xlsx)

fileList = Sys.glob(paste(globalVar$inpConfig, "origin.csv", sep = "/"))

data = readr::read_csv(file = fileList, locale = locale("ko", encoding = "EUC-KR"))

# ??, ???,  ??, `?? ??(?)`, `?? ??(?)`
colnames(data) = c("date", "service", "user", "visitMin", "visitDay")

dataL1 = na.omit(data)

dataL2 = dataL1 %>%
    dplyr::mutate(
        sDate = as.character(date)
        , dtDate = readr::parse_date(sDate, "%Y%m")
        , dtMonth = lubridate::month(dtDate)
    )

# 11? ?? ???? ?? ?? ?? ? / ?? ???? / ?? ?? ??
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



# 11? ???? : ?? ?? ? / ?? ???? / ?? ?? ??
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

# ??? ? ?? ?? ?? ?? ??? ???
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

# ??? ? ?? ??? ??? ???? ?? (??? ??) ???? ???
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


# ??? ??? ?? ???? ?? ????
#	* ??? ?? ?? : (?? ??? 30 ?? AND ?? ?? 500? ???? ??)
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

# ???? ?? ??? ?? ???? ?? ??? ?
#	* ???? ?? ?? : (???? 5?? OR ?? ?? 10 ??? ??)
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

# ??? ???? ?? ?? ???? ?? ????
#	ex ) ??? 1? ?? 1111
#	      ??? 3? ?? 1111    > 1111??? ?? ??? 1, 3 ???? ???? ???? ??.

# ???? ?? ?? ??
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

# ??? ?? ?? ??
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
# Purpose : ???? (??, ???)
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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
# Purpose : ???? (??, ???)
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(ggplot2)
library(tidyverse)

data = ggplot2::midwest

# 1.(10?) popadults? ?? ??? ?? ??(number of adults), poptotal? ?? ??(total population)? ?????. midwest ???? '?? ?? ?? ??? ?? ???' ??(child_ratio)? ?????.

dataL1 = data %>%
    dplyr::mutate(child_ratio = (popadults / poptotal) * 100)

dataL1

# 2.(10?) ??? ?? ???? ?? ?? ?? 10? county(??)? ??? ?? ???? ?????.
dataL1 %>%
    dplyr::arrange(desc(child_ratio)) %>%
    dplyr::top_n(10) %>%
    dplyr::select(county, child_ratio)


# 3.(10?) ?? ???? ??? ?? ????? ?? ??(grade)? ????, ? ??? ? ?? ??? ??? ?????.
dataL1 %>%
    dplyr::mutate(grade = dplyr::case_when(
        child_ratio >= 45.0 ~ "large"
        , 30.0 <= child_ratio & child_ratio < 45.0 ~ "middle"
        , child_ratio < 30.0 ~ "small"
        , TRUE ~ "NA"
    )) %>%
    dplyr::group_by(grade) %>%
    dplyr::summarise(n = n())

# 4.(10?) popasian? ?? ??? ???? ??? ?????. '?? ?? ?? ???? ?? ???' ??(asian_ratio)? ????, ?? 5? ??? state(?), county(???), ???? ?? ???(asian_ratio)? ?????.

data %>%
    dplyr::mutate(asian_ratio = (popasian / poptotal) * 100) %>%
    dplyr::arrange(asian_ratio) %>%
    dplyr::top_n(-5) %>%
    dplyr::select(state, county, asian_ratio)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? (??, ???)
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(readr)
library(tidyverse)

# "E:/04. ?????/Github/TalentPlatform-R/INPUT/o2job ??? ???19 ??? ??.csv"
fileList = Sys.glob(paste(globalVar$inpConfig, "??? ???19 ??? ??.csv", sep = "/"))
data = readr::read_csv(file = fileList, locale = locale("ko", encoding = "EUC-KR"))


# ???? ??
dataL1 = data %>%
  dplyr::mutate(type = dplyr::case_when(
    stringr::str_detect(???, "??") & stringr::str_detect(???, "??") ~ "??"
    , stringr::str_detect(???, "???") ~ "???"
    , TRUE ~ "??"
  )) %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(cnt = n())

label = paste0(dataL1$type, " ", round((dataL1$cnt / sum(dataL1$cnt, na.rm = TRUE) * 100)), "%")
pie(dataL1$cnt, labels = label, col=rainbow(5), main="??? ??? ??")

# ??? ??
dataL2 = data %>%
  dplyr::group_by(??) %>%
  dplyr::summarise(cnt = n()) %>%
  dplyr::arrange(desc(cnt)) %>%
  dplyr::top_n(5)

barplot(dataL2$cnt, names.arg = dataL2$??, col = rainbow(5), main = "?? ??? ?")


# ???? ??? ??
dataL3 = data %>%
  dplyr::filter(?? %in% c("????", "???", "???")) %>%
  dplyr::mutate(type = dplyr::case_when(
    ?? == "??" ~ "??"
    , ?? == "??" ~ "??"
    , TRUE ~ "??"
  ))

count = table(dataL3$type, dataL3$??)
barplot(count, main="3? ?? ????", col=rainbow(3), legend = rownames(count))

#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? (??, ???)
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(xlsx)
library(MASS)
library(ROCR)
library(abdiv)
library(xlsx)
library(tidyverse)
library(stringr)
library(hablar)

fileList = Sys.glob(paste(globalVar$inpConfig, "20201018(??)??_?????_?1018.xlsx", sep = "/"))


data = xlsx::read.xlsx2(file = fileList, sheetIndex = 1)


# ??? ??
dataNA = data %>%
  readr::type_convert() %>%
  dplyr::select(everything()) %>%
  dplyr::summarise_all(funs(sum(is.na(.)))) %>%
  tidyr::gather(key = "key", value = "val") %>%
  dplyr::filter(val > 0)

paste(dataNA$key, collapse = ", ")

# ?? ???? NA?? ??
dataL1 = data %>%
    dplyr::select(- c( "ID", "??_??", "??_???", "??_??", "??_???", "??_???", "??_???", "??_???", "??_????", "??_????gr", "??_???", "??_??", "??_????", "??_????", "??_?????", "??_?????", "??_???20?yn", "??_????", "??_????", "??_??????", "??_????", "??_??????", "??_????", "??_???", "??_???", "??_??????", "??_??????123", "??_???")) %>%
    readr::type_convert() %>%
    na.omit()

# ??? ??
dataL2 = dataL1 %>%
  # dplyr::select(??_yn, dplyr::contains("yn")) %>%
  dplyr::select(??_yn, dplyr::contains("??")) %>%
  hablar::convert(fct(contains("yn")))

# "0=???
# 1=?????"

#=====================================================================
# ??? ?? ??
#=====================================================================
# Initial Model:
#     ??_yn ~ ??? ??
#
# Final Model:
#     ??_yn ~ ?? + ??? + ?? + ???? + ???.??.??

# ?? ??? ?? ???? ???? ??
# ???? : ??_yn ??? ?? ??
# ???? : ??_yn
glmFitVarAll = glm(??_yn ~ ., data = dataL2, family = binomial)
summary(glmFitVarAll)

# 1) ????? ?? ??
# stepRes = step(glmFitVarAll)
summary(stepRes)

# 1) AIC ???? ?? ??
stepAic = MASS::stepAIC(glmFitVarAll, direction = "both")

# ??? ?? ??
summary(stepAic)

# predict.glm(glmFitVarAll, type='response')

#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? (??, ???)
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(ggplot2)
library(tidyverse)
library(forcats)

data = data.frame(
  group = c("???", "???", "?????", "?????", "???", "???", "?????", "?????", "???", "???", "???", "?????", "?????", "?????", "???", "???", "?????", "?????", "???", "???", "?????", "?????", "???", "???", "?????", "?????")
  , variable = c("??", "??", "??", "??", "70? ??", "80? ??", "70? ??", "80? ??", "??/??", "???", "????", "??/??", "???", "????", "??", "??", "??", "??", "????", "????", "????", "????", "????", "???", "????", "???")
  , value = c(30, 70, 24, 76, 49, 51, 44, 56, 78, 20, 2, 82, 16, 2, 42, 58, 52, 48, 19, 81, 24, 76, 92, 8, 90, 10)
  , type = c("??", "??", "??", "??", "??", "??", "??", "??", "??", "??", "??", "??", "??", "??", "??(??)", "??(??)", "??(??)", "??(??)", "????", "????", "????", "????", "? ????", "? ????", "? ????", "? ????")
)

# type ??
data$type = forcats::fct_relevel(data$type, c("??", "??", "??", "??(??)", "????", "? ????"))

# variable ??
data$variable = forcats::fct_relevel(data$variable,  c("??", "??","70? ??", "80? ??", "??/??", "???", "????", "??", "??", "????", "????", "????", "???"))

ggplot(data, aes(x = variable, y = value, fill = group, label=round(value,1))) +
  geom_bar(position = "dodge", stat="identity") +
  theme(legend.position = "top") +
  geom_text(aes(group=group),position=position_dodge(width=0.9), size=5,vjust=-0.5,hjust=0.5) +
  ylim(0, 100) +
  facet_wrap( ~ type, scale="free") +
  labs(x = "??", y = "??", fill="", subtitle = "??? ??") +
  scale_fill_manual(values=c("#00bfc4", "#f8766d")) +
  theme(text = element_text(size=18)) +
  ggsave(filename = paste(globalVar$figConfig, "Img_020.png", sep="/"), width = 12, height = 8, dpi = 600)


# ???? ? ????
dataL1 = data.frame(
  minCi  = c(0.37, 0.21, 0.47, 0.31, 0.52, 0.72, 0.52, 0.94, 1.14, 0.91, 0.86, 0.62, 0.41, 0.76,  0.30, 0.29, 0.23, 2.31, 1.18,  0.87, 1.78)
  , rr = c(0.54, 0.45, 0.71, 0.52, 0.77, 0.86, 0.73, 1.05, 2.00, 1.22, 1.19, 0.83, 0.64, 0.87, 0.54, 0.49, 1.33, 6.00, 1.91, 1.48, 2.81)
  , maxCi = c(0.79, 0.95, 1.07, 0.86, 1.13, 1.03, 1.04, 1.17, 3.50, 1.62, 1.64, 1.11, 1.00, 1.00, 0.98, 0.82, 7.72, 15.57, 3.07, 2.51, 4.43)
  , p = c(0.001, 0.025, 0.083, 0.005, 0.167, 0.060, 0.425, 0.015, 0.204, 0.3, 0.194, 0.037, 0.021, 0.064, 0.064, 0.064, 0.749, 0.001, 0.008, 0.003, 0.003)
  , sampleNum = seq(1, 21)
  , type = c("??????", "??????", "????", "???????", "??????", "?? ??", "?? ??", "?????", "???? ??", "???? ??", "???????", "????", "????", "????", "?? ? 1-2?", "?? ? 3? ??", "??? ??", "?? ??", "??(3???)", "?? ? 1?", "?? ? 2? ??")
  , group = c("??", "??", "??", "??", "??", "??", "??", "??", "??", "??", "??", "??", "??", "??", "??", "??", "??", "??", "??", "??", "??")
) %>%
  dplyr::mutate(pVal = dplyr::case_when(
    p <= 0.01 ~ "0.01"
    , p <= 0.05 ~ "0.05"
    , p <= 0.10 ~ "0.10"
    , TRUE ~ ""
  ))

# pVal ??
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
  labs(x = "RR (95% CI)", y = "??", color="P-Value", fill="", subtitle = "???? ? ????") +
  theme(text = element_text(size=18)) +
  ggsave(filename = paste(globalVar$figConfig, "Img_021.png", sep="/"), width = 12, height = 8, dpi = 600)



# ??, ??? ? ??
dataL3 = data.frame(
  rr = c(0.85, 0.84, 0.63, 0.39, 0.12, 0.55, 0.40, 0.16)
  , minCi = c(0.54, 0.73, 0.50, 0.27, 0.05, 0.40, 0.25, 0.02)
  , maxCi = c(1.34, 0.97, 0.79, 0.56, 0.32, 0.76, 0.62, 1.03)
  , p = c(0.477, 0.004, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001)
  , sampleNum = seq(1, 8)
  , type = c("??? ??", "??? ??", "??? ??", "???? ? 1,2?", "???? ? 3? ??", "??", "?? ? 1,2?", "?? ? 3? ??")
  , group = c("??", "??", "??", "??", "??", "??", "??", "??")
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
  labs(x = "RR (95% CI)", y = "??", color="P-Value", fill="", subtitle = "??, ??? ? ??") +
  theme(text = element_text(size=18)) +
  ggsave(filename = paste(globalVar$figConfig, "Img_022.png", sep="/"), width = 12, height = 8, dpi = 600)

# ?? ? ??, ??? ??, ??
dataL4 = data.frame(
  group = c("???", "???", "?????", "?????", "???", "???", "?????", "?????", "???", "???", "?????", "?????")
  , variable = c("??", "??", "??", "??", "??", "??", "??", "??", "??", "??", "??", "??")
  , value = c(95, 5, 80, 20, 95, 5, 60, 40, 80, 20, 44, 56)
  , type = c("?? ? ??", "?? ? ??", "?? ? ??", "?? ? ??", "??? ??", "??? ??", "??? ??", "??? ??", "??", "??", "??", "??")
)

# type ??
dataL4$type = forcats::fct_relevel(dataL4$type, c("?? ? ??", "??? ??", "??"))

# variable ??
dataL4$variable = forcats::fct_relevel(dataL4$variable,  c("??", "??"))

ggplot(dataL4, aes(x = variable, y = value, fill = group, label=round(value,1))) +
  geom_bar(position = "dodge", stat="identity") +
  theme(legend.position = "top") +
  geom_text(aes(group=group),position=position_dodge(width=0.9), size=5,vjust=-0.5,hjust=0.5) +
  ylim(0, 100) +
  facet_wrap( ~ type, scale="free") +
  labs(x = "??", y = "??", fill="", subtitle = "??? ??") +
  scale_fill_manual(values=c("#00bfc4", "#f8766d")) +
  theme(text = element_text(size=18)) +
  ggsave(filename = paste(globalVar$figConfig, "Img_023.png", sep="/"), width = 12, height = 8, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? (??, ???)
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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

# ??, ??, ??, ??, ????, ???????
# HH02_01_A, HH02_01_B, EF1, EF11, Q60, Q69

# ??, ??, ??, ??, ????, ???????
# Q34 (????)
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

# Q57? (??)
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

# Q64? (????)
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
      HH02_01_A == 1 ~ "?"
      , HH02_01_A == 2 ~ "?"
      , TRUE ~ "NULL"
    )
    , age = (2020 - HH02_01_B) + 1
    , typeAge = dplyr::case_when(
      age < 30 ~ "30? ??"
      , 30 <= age & age < 40 ~ "30-40? ??"
      , 40 <= age & age < 50 ~ "40-50? ??"
      , 50 <= age & age < 60 ~ "50-60? ??"
      , 60 >= age ~ "60? ??"
      , TRUE ~ "NULL"
    )
    , typeEF1 = dplyr::case_when(
      # EF1 == 1 ~ "?? ?? ???? ?? ??"
      # , EF1 == 2 ~ "???? ??"
      EF1 == 1 ~ "NULL"
      , EF1 == 2 ~ "NULL"
      , EF1 == 3 ~ "??? ??"
      , EF1 == 4 ~ "???? ??"
      , EF1 == 5 ~ "?? ??? ??"
      , EF1 == 6 ~ "??? ??"
      , EF1 == 7 ~ "??? ?? ??"
      # , EF1 == 9 ~ "??"
      , EF1 == 9 ~ "NULL"
      , TRUE ~ "NULL"
    )
    , typeEF11 = dplyr::case_when(
      EF11 < 100 ~ "100? ??"
      , 100 <= EF11 & EF11 < 200 ~ "100-200? ??"
      , 200 <= EF11 & EF11 < 300 ~ "200-300? ??"
      , 300 <= EF11 & EF11 < 400 ~ "300-400? ??"
      , 400 <= EF11 & EF11 < 500 ~ "400-500? ??"
      , EF11 >= 500 ~ "500? ??"
      , TRUE ~ "NULL"
    )
    , typeQ60 = dplyr::case_when(
      Q60 == 1 ~ "?? ??"
      , Q60 == 2 ~ "?? ???"
      , Q60 == 3 ~ "????"
      , Q60 == 4 ~ "?? ???"
      # , Q60 == 5 ~ "?? ???"
      , Q60 == 5 ~ "NULL"
      # , Q60 == 8 ~ "??/???"
      # , Q60 == 9 ~ "??"
      , Q60 == 8 ~ "NULL"
      , Q60 == 9 ~ "NULL"
      , TRUE ~ "NULL"
    )
    , typeQ69 = dplyr::case_when(
      Q69 == 1 ~ "?? ????"
      , Q60 == 2 ~ "????"
      , Q60 == 3 ~ "?? ???? ???"
      , Q60 == 4 ~ "?? ???? ???"
      # , Q60 == 8 ~ "??/???"
      # , Q60 == 9 ~ "??"
      , Q60 == 8 ~ "NULL"
      , Q60 == 9 ~ "NULL"
      , TRUE ~ "NULL"
    )
    , typeQ34_1 = dplyr::case_when(
        Q34_1 == 1 ~ "?"
        , Q34_1 == 2 ~ "???"
        , TRUE ~ "NULL"
    )
      , typeQ34_2 = dplyr::case_when(
          Q34_2 == 1 ~ "?"
          , Q34_2 == 2 ~ "???"
          , TRUE ~ "NULL"
      )
      , typeQ34_3 = dplyr::case_when(
          Q34_3 == 1 ~ "?"
          , Q34_3 == 2 ~ "???"
          , TRUE ~ "NULL"
      )
      , typeQ34_4 = dplyr::case_when(
          Q34_4 == 1 ~ "?"
          , Q34_4 == 2 ~ "???"
          , TRUE ~ "NULL"
      )
      , typeQ34_5 = dplyr::case_when(
          Q34_5 == 1 ~ "?"
          , Q34_5 == 2 ~ "???"
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
# ?? ??? ? ?? (1000?)
#=====================================================================

set.seed(1)

# ?? ? ??? ?? 60:40?? ??? ?? ??? ??
ind = sample(1:nrow(dataL1), 1000)

# ?? ???? ?? ?? ??
dataL2 = dataL1[ind, ]

dplyr::tbl_df(dataL2)

#==================================
# Table 1
#==================================
# paste(unique(dataL2$typeQ34_1), collapse = ", ")

dataL2$typeHH02_01_A =forcats::fct_relevel(dataL2$typeHH02_01_A, c("?", "?"))
dataL2$typeAge = forcats::fct_relevel(dataL2$typeAge, c("30? ??", "30-40? ??", "40-50? ??", "50-60? ??", "60? ??"))
dataL2$typeEF1 =forcats::fct_relevel(dataL2$typeEF1, c("??? ??", "???? ??", "?? ??? ??", "??? ??", "??? ?? ??"))
dataL2$typeEF11 =forcats::fct_relevel(dataL2$typeEF11, c("100? ??", "100-200? ??", "200-300? ??", "300-400? ??", "400-500? ??", "500? ??"))
dataL2$typeQ60 =forcats::fct_relevel(dataL2$typeQ60, c("?? ??", "?? ???", "????", "?? ???"))
dataL2$typeQ69 =forcats::fct_relevel(dataL2$typeQ69, c("?? ????", "????", "?? ???? ???", "?? ???? ???"))
dataL2$typeQ34_1 =forcats::fct_relevel(dataL2$typeQ34_1, c("?", "???"))
dataL2$typeQ34_2 = forcats::fct_relevel(dataL2$typeQ34_2, c("?", "???"))
dataL2$typeQ34_3 =forcats::fct_relevel(dataL2$typeQ34_3, c("?", "???"))
dataL2$typeQ34_4 =forcats::fct_relevel(dataL2$typeQ34_4, c("?", "???"))
dataL2$typeQ34_5 =forcats::fct_relevel(dataL2$typeQ34_5, c("?", "???"))


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
      mean(., na.rm = TRUE) # ??
      , sd(., na.rm = TRUE) # ????
      , min(., na.rm = TRUE)
      , max(., na.rm = TRUE)
    )) %>%
    round(2) %>%
  dplyr::glimpse()


#==================================
# Table 3
#==================================
# ?? ??
typeList = c("typeHH02_01_A", "typeAge", "typeEF1", "typeEF11", "typeQ60", "typeQ69")
# typeNameList = c("??", "??", "??", "??", "????", "?????")
colList = dataL2 %>%
    dplyr::select(! dplyr::contains("type")) %>%
    # dplyr::select(dplyr::contains(c("Q34", "Q57", "Q64"))) %>%
    dplyr::select(dplyr::contains(c("Q34"))) %>%
    colnames()

# colNameList = c("?? ????? ??? ??", "?? ?? ??? ??", "?? ?????? ??", "??? ??? ??? ??", "?? ??? ??", "?? ??", "??, ?? ??, ???? ?? ??", "??? ?? ??", "???", "??? ?? ??", "??? ?? ??", "???? ?? ?? ??? ?? ??", "??? ?? ??", "?? ??? ?? ??", "?? ??(???? ?)? ?? ??", "????", "??? ?? ?? ??", "??", "??? ??")

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
            mean(., na.rm = TRUE) %>% round(2) # ??
            , sd(., na.rm = TRUE) %>% round(2) # ????
            # , n() # ?? ??
        ))

    dataL4 = dplyr::bind_rows(dataL4, tmpData2)
}

xlsx::write.xlsx2(dataL3, file = paste0(globalVar$outConfig, "/Survery.xlsx"), sheetName = "dataL3_3", append = TRUE, row.names = FALSE, col.names = TRUE)

xlsx::write.xlsx2(dataL4, file = paste0(globalVar$outConfig, "/Survery.xlsx"), sheetName = "dataL4_3", append = TRUE, row.names = FALSE, col.names = TRUE)

# ??
# dataL2 %>%
#   dplyr::group_by(typeHH02_01_A) %>%
#     dplyr::select(! dplyr::contains("type")) %>%
#     dplyr::select(dplyr::contains(c("Q34", "Q57", "Q64"))) %>%
#   dplyr::summarise_all(funs(
#     mean(., na.rm = TRUE) # ??
#     , sd(., na.rm = TRUE) # ????
#   )) %>%
#   dplyr::glimpse()

# aov(Q34_1 ~ typeHH02_01_A, data = dataL2) %>% summary
# aov(Q34_2 ~ typeHH02_01_A, data = dataL2) %>% summary
# aov(sumQ64 ~ typeHH02_01_A, data = dataL2) %>% summary
#
# # ??
# dataL2 %>%
#   dplyr::group_by(typeAge) %>%
#   dplyr::summarise_all(funs(
#     mean(., na.rm = TRUE) # ??
#     , sd(., na.rm = TRUE) # ????
#   )) %>%
#   dplyr::glimpse()
#
# aov(sumQ34 ~ typeAge, data = dataL2) %>% summary
# aov(sumQ57 ~ typeAge, data = dataL2) %>% summary
# aov(sumQ64 ~ typeAge, data = dataL2) %>% summary

# # ??
# dataL2 %>%
#   dplyr::group_by(typeEF1) %>%
#   dplyr::summarise_all(funs(
#     mean(., na.rm = TRUE) # ??
#     , sd(., na.rm = TRUE) # ????
#   )) %>%
#   dplyr::glimpse()
#
# aov(sumQ34 ~ typeEF1, data = dataL2) %>% summary
# aov(sumQ57 ~ typeEF1, data = dataL2) %>% summary
# aov(sumQ64 ~ typeEF1, data = dataL2) %>% summary

# ??
# dataL2 %>%
#   dplyr::group_by(typeEF11) %>%
#   dplyr::summarise_all(funs(
#     mean(., na.rm = TRUE) # ??
#     , sd(., na.rm = TRUE) # ????
#   )) %>%
#   dplyr::glimpse()
#
# aov(sumQ34 ~ typeEF11, data = dataL2) %>% summary
# aov(sumQ57 ~ typeEF11, data = dataL2) %>% summary
# aov(sumQ64 ~ typeEF11, data = dataL2) %>% summary

# ?? ??
# dataL2 %>%
#   dplyr::group_by(typeQ60) %>%
#   dplyr::summarise_all(funs(
#     mean(., na.rm = TRUE) # ??
#     , sd(., na.rm = TRUE) # ????
#   )) %>%
#   dplyr::glimpse()
#
# aov(sumQ34 ~ typeQ60, data = dataL2) %>% summary
# aov(sumQ57 ~ typeQ60, data = dataL2) %>% summary
# aov(sumQ64 ~ typeQ60, data = dataL2) %>% summary

# ?? ?? ???
# dataL2 %>%
#   dplyr::group_by(typeQ69) %>%
#   dplyr::summarise_all(funs(
#     mean(., na.rm = TRUE) # ??
#     , sd(., na.rm = TRUE) # ????
#   )) %>%
#   dplyr::glimpse()
#
# aov(sumQ34 ~ typeQ69, data = dataL2) %>% summary
# aov(sumQ57 ~ typeQ69, data = dataL2) %>% summary
# aov(sumQ64 ~ typeQ69, data = dataL2) %>% summary


#==================================
# Table 4
#==================================
# ????? ??
# cor.test(dataL2$sumQ34, dataL2$sumQ57)

# ????? ????
# cor.test(dataL2$sumQ34, dataL2$sumQ64)

colList = c("Q34_1", "Q34_2", "Q34_3", "Q34_4", "Q34_5")
colList2 = c("sumQ57", "sumQ64")

# ?? ??
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
# Purpose : ???? (??, ???)
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(readr)
library(xlsx)
library(readxl)
library(openxlsx)
library(tidyverse)
library(scales)

# ???, ???, ???, ???, ?????, ?????
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
    "??1 (???)" = var1
    , "??2 (???)" = var2
    , "??3 (???)" = var3
    ) %>%
  tidyr::gather(-dtDate, key = "key", value = "val")

ggplot(ggData, aes(x = dtDate, y = val, color = key, label=round(val, 0))) +
  geom_line(size = 2) +
  scale_x_date(labels = date_format("%y"), date_breaks = "1 years", limits = c(as.Date("1985-01-01"), as.Date("2019-01-01")), date_minor_breaks = "1 years") +
  ylim(0, 105) +
  labs(x = "??", y = "?? ??", color="", subtitle = "??? LDA-????? ?? ??") +
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
    n() # ??
    , mean(., na.rm = TRUE) # ??
    , sd(., na.rm = TRUE) # ????
    , sum(., na.rm = TRUE) # ??
    , min(., na.rm = TRUE) # ???
    , max(., na.rm = TRUE) # ???
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

# ??
lm(gdpNor ~ scaleVar1, data = dataL2) %>% summary()
lm(gdpNor ~ scaleVar2, data = dataL2) %>% summary()
lm(gdpNor ~ scaleVar3, data = dataL2) %>% summary()


lmFit = lm(gdpNor ~ scaleVar1*scaleVar2 + scaleVar1*scaleVar3 + scaleVar2*scaleVar3, data = dataL2)
# lmFit = lm(gdpNor ~ scaleVar1 + scaleVar2 + scaleVar3 + scaleVar1*scaleVar2 + scaleVar1*scaleVar3 + scaleVar2*scaleVar3, data = dataL2)
summary(lmFit)
MASS::stepAIC(lmFit, direction = "both") %>% summary()

stepFitGdp = MASS::stepAIC(lmFit, direction = "both")

# ??
lm(gdpSou ~ scaleVar1, data = dataL2) %>% summary()
lm(gdpSou ~ scaleVar2, data = dataL2) %>% summary()
lm(gdpSou ~ scaleVar3, data = dataL2) %>% summary()

lmFit = lm(gdpSou ~ scaleVar1 + scaleVar2 + scaleVar3, data = dataL2)
summary(lmFit)
MASS::stepAIC(lmFit, direction = "both") %>% summary()

#==================================
# ?????
#==================================
# ??
lm(gdpRatNor ~ scaleVar1, data = dataL2) %>% summary()
lm(gdpRatNor ~ scaleVar2, data = dataL2) %>% summary()
lm(gdpRatNor ~ scaleVar3, data = dataL2) %>% summary()

# lmFit = lm(gdpRatNor ~ scaleVar1 + scaleVar2 + scaleVar3 + scaleVar1*scaleVar2 + scaleVar1*scaleVar3 + scaleVar2*scaleVar3, data = dataL2)
lmFit = lm(gdpRatNor ~ scaleVar1*scaleVar2 + scaleVar1*scaleVar3 + scaleVar2*scaleVar3, data = dataL2)
summary(lmFit)
MASS::stepAIC(lmFit, direction = "both") %>% summary()
stepFitGdpRat = MASS::stepAIC(lmFit, direction = "both")

# ??
lm(gdpRatSou ~ scaleVar1, data = dataL2) %>% summary()
lm(gdpRatSou ~ scaleVar2, data = dataL2) %>% summary()
lm(gdpRatSou ~ scaleVar3, data = dataL2) %>% summary()

lmFit = lm(gdpRatSou ~ scaleVar1 + scaleVar2 + scaleVar3, data = dataL2)
summary(lmFit)
MASS::stepAIC(lmFit, direction = "both") %>% summary()

#==================================
# ???
#==================================
ggData = dataL2 %>%
  dplyr::mutate(
    stepFitGdp = predict(stepFitGdp)
    , stepFitGdpRat = predict(stepFitGdpRat)
    ) %>%
  dplyr::select(dtDate, gdpNor, stepFitGdp) %>%
  dplyr::rename(
    "GDP (??)" = gdpNor
    , "GDP (??) ??" = stepFitGdp
  ) %>%
  tidyr::gather(-dtDate, key = "key", value = "val")

ggplot(ggData, aes(x = dtDate, y = val, color = key, label=round(val, 0))) +
  geom_line(size = 2) +
  scale_x_date(labels = date_format("%y"), date_breaks = "1 years", limits = c(as.Date("1990-01-01"), as.Date("2018-01-01")), date_minor_breaks = "1 years") +
  labs(x = "??", y = "GDP", color="", subtitle = "?? GDP ???") +
  theme(text = element_text(size=18))  +
  ggsave(filename = paste(globalVar$figConfig, "Img_033.png", sep="/"), width = 12, height = 8, dpi = 600)


ggData = dataL2 %>%
  dplyr::mutate(
    stepFitGdp = predict(stepFitGdp)
    , stepFitGdpRat = predict(stepFitGdpRat)
  ) %>%
  dplyr::select(dtDate, gdpRatNor, stepFitGdpRat) %>%
  dplyr::rename(
    "?? ??? (??)" = gdpRatNor
    , "?? ??? (??) ??" = stepFitGdpRat
  ) %>%
  tidyr::gather(-dtDate, key = "key", value = "val")

ggplot(ggData, aes(x = dtDate, y = val, color = key, label=round(val, 0))) +
  geom_line(size = 2) +
  scale_x_date(labels = date_format("%y"), date_breaks = "1 years", limits = c(as.Date("1990-01-01"), as.Date("2018-01-01")), date_minor_breaks = "1 years") +
  labs(x = "??", y = "?? ???", color="", subtitle = "?? ?? ??? ???") +
  theme(text = element_text(size=18))  +
  ggsave(filename = paste(globalVar$figConfig, "Img_034.png", sep="/"), width = 12, height = 8, dpi = 600)

xlsx::write.xlsx2(ggData, file = paste0(globalVar$outConfig, "/Gdp_Rat.xlsx"), sheetName = "L5", append = TRUE, row.names = FALSE, col.names = TRUE)

#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? (??, ???)
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(ggplot2)
library(forcats)
library(tidyverse)
library(ggpubr)
library(Metrics)

data = mpg
# manufacturer : ????
# model : ?? ??
# displ : ???
# cyl :??? ??
# trans : ???? ??
# cty : ?? ??
# hwy : ???? ??
# fl : ?? ??
# class : ?? ??

# mpg ???? ggplot2 ???? ???? ?? ?? ?????. ??? ??? ???.(?? ??????  ??? ?)
# ??) ??? ????? ?? ??? ?? ???? ?????.
# 1) ?? ???? ??? ????.

ggData = data %>%
  dplyr::select(cty, manufacturer) %>%
  dplyr::group_by(manufacturer) %>%
  dplyr::summarise(meanCty = mean(cty, na.rm = TRUE)) %>%
  dplyr::arrange(meanCty)

ggData$manufacturer = forcats::fct_relevel(ggData$manufacturer, ggData$manufacturer)

ggplot(ggData, aes(x = manufacturer, y = meanCty, fill = manufacturer)) +
  geom_bar(position = "dodge", stat="identity") +
  geom_text(aes(label = round(meanCty, 2)), vjust = 1.6, color = "white", size = 4) +
  labs(x = "????", y = "?? ??", fill="", subtitle = "??? ????? ?? ?? ?? ???") +
  theme(text = element_text(size=18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figConfig, "Img_035.png", sep="/"), width = 12, height = 8, dpi = 600)

summary(ggData)

# 2)????? ?? ??? ????.
# 15? ????? ?? ?? ??? ?????? ????? ?? ?????.
# ? ?? ? ??/???? ?? 16.4 ? 11.3-24.4? ?????
# ?? ?? ?? TOP5? ?? honda, volkswagen, subaru, hyundai, toyota? ??
# jeep, mercury, dodge, land rover, lincoln ??? ???.

# 3) ????? ?? ??? ???? ??? ??? ??? ????.
# ?? ????? ?? ?? (?? ??: 16.4, ???? ??: 23.0) ? ??/??? (?? ??: 11.3-24.4, ???? ??: 16.5-32.6)? ? ??? ???.
# ?? ? ?? TOP5? ?? pontiac, audi, volkswagen, hyundai, honda? ??
# ford, land rover, dodge, mercury, jeep ??? ???.

dataL3 = data %>%
  dplyr::select(cty, manufacturer, hwy) %>%
  dplyr::group_by(manufacturer) %>%
  dplyr::summarise(
    meanCty = mean(cty, na.rm = TRUE)
    , meanHwy = mean(hwy, na.rm = TRUE)
    , diff = meanCty - meanHwy
  ) %>%
  dplyr::arrange(diff)

# ??? ??: ????? ???? ??? ???? ???? ?? ???? ?? ?????.
# ?? ?? ? ???? ??? ???? ???? ??????.
# ? ??? ?? (Bias) ? ??????? (RMSE)? ?? -6.58 ? 6.77?? ?? ??? ?????  ???? ???? 0.97?? 0.000 ??? ???? ?????.

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
  labs(title = "", x = "?? ??", y = "???? ??", subtitle = "?? ??? ???? ??? ???") +
  coord_equal() +
  theme(text = element_text(size=18)) +
  ggsave(filename = paste(globalVar$figConfig, "Img_036.png", sep="/"), width = 6, height = 6, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? (??, ???)
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

library(ggplot2)
library(forcats)
library(tidyverse)
library(ggpubr)
library(Metrics)
library(scales)
library(viridis)

data = data.frame(
  key1 = c("???", "???", "???", "???", "???1", "???2", "???3", "???4", "???5", "???6")
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
  labs(x = "", y = "??? [?? : ???]", fill="???", subtitle = "??? ?? ??? ???") +
  theme(text = element_text(size=18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(direction = -1) +
  ggsave(filename = paste(globalVar$figConfig, "Img_036.png", sep="/"), width = 12, height = 8, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? (??, ???)
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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
# ??
#==========================================
dataL1 = dplyr::bind_rows(
    data.frame(table(data$SEX_NM, data$INTRST_FASHN_AT), type =  "????")
    , data.frame(table(data$SEX_NM, data$INTRST_PC_AT), type = "????")
    , data.frame(table(data$SEX_NM, data$INTRST_HEALTH_AT), type = "????")
    , data.frame(table(data$SEX_NM, data$INTRST_WDOMOUTH_AT), type = "?????")
    , data.frame(table(data$SEX_NM, data$INTRST_QLITY_AT), type = "????")
    , data.frame(table(data$SEX_NM, data$INTRST_SAFE_AT), type = "????")
    , data.frame(table(data$SEX_NM, data$INTRST_EXPRN_AT), type = "????")
    , data.frame(table(data$SEX_NM, data$INTRST_BRAND_AT), type = "?????")
)

dataL2 = dataL1 %>%
    dplyr::group_by(type, Var1) %>%
    dplyr::summarise(cnt = sum(Freq, na.rm = TRUE))

dataL3 = dataL1 %>%
    dplyr::left_join(dataL2, by = c("type" = "type", "Var1" = "Var1")) %>%
    dplyr::mutate(
        val = (Freq / cnt) *  100.0
        , isYn = dplyr::case_when(
            Var2 == "Y" ~ "??"
            , Var2 == "N" ~ "??"
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
    labs(x = "??", y = "??", fill="", subtitle = "") +
    theme(text = element_text(size=18)) +
    ggsave(filename = paste(globalVar$figConfig, "Img_040.png", sep="/"), width = 15, height = 10, dpi = 600)

#==========================================
# ??
#==========================================
dataL1 = dplyr::bind_rows(
    data.frame(table(data$YEAR_NM, data$INTRST_FASHN_AT), type =  "????")
    , data.frame(table(data$YEAR_NM, data$INTRST_PC_AT), type = "????")
    , data.frame(table(data$YEAR_NM, data$INTRST_HEALTH_AT), type = "????")
    , data.frame(table(data$YEAR_NM, data$INTRST_WDOMOUTH_AT), type = "?????")
    , data.frame(table(data$YEAR_NM, data$INTRST_QLITY_AT), type = "????")
    , data.frame(table(data$YEAR_NM, data$INTRST_SAFE_AT), type = "????")
    , data.frame(table(data$YEAR_NM, data$INTRST_EXPRN_AT), type = "????")
    , data.frame(table(data$YEAR_NM, data$INTRST_BRAND_AT), type = "?????")
)

dataL2 = dataL1 %>%
    dplyr::group_by(type, Var1) %>%
    dplyr::summarise(cnt = sum(Freq, na.rm = TRUE))

dataL3 = dataL1 %>%
    dplyr::left_join(dataL2, by = c("type" = "type", "Var1" = "Var1")) %>%
    dplyr::mutate(
        val = (Freq / cnt) *  100.0
        , isYn = dplyr::case_when(
            Var2 == "Y" ~ "??"
            , Var2 == "N" ~ "??"
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
    labs(x = "??", y = "??", fill="", subtitle = "") +
    theme(text = element_text(size=18)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggsave(filename = paste(globalVar$figConfig, "Img_041.png", sep="/"), width = 15, height = 10, dpi = 600)


#==========================================
# ????
#==========================================
dataL1 = dplyr::bind_rows(
    data.frame(table(data$MRRG_NM, data$INTRST_FASHN_AT), type =  "????")
    , data.frame(table(data$MRRG_NM, data$INTRST_PC_AT), type = "????")
    , data.frame(table(data$MRRG_NM, data$INTRST_HEALTH_AT), type = "????")
    , data.frame(table(data$MRRG_NM, data$INTRST_WDOMOUTH_AT), type = "?????")
    , data.frame(table(data$MRRG_NM, data$INTRST_QLITY_AT), type = "????")
    , data.frame(table(data$MRRG_NM, data$INTRST_SAFE_AT), type = "????")
    , data.frame(table(data$MRRG_NM, data$INTRST_EXPRN_AT), type = "????")
    , data.frame(table(data$MRRG_NM, data$INTRST_BRAND_AT), type = "?????")
)

dataL2 = dataL1 %>%
    dplyr::group_by(type, Var1) %>%
    dplyr::summarise(cnt = sum(Freq, na.rm = TRUE))

dataL3 = dataL1 %>%
    dplyr::left_join(dataL2, by = c("type" = "type", "Var1" = "Var1")) %>%
    dplyr::mutate(
        val = (Freq / cnt) *  100.0
        , isYn = dplyr::case_when(
            Var2 == "Y" ~ "??"
            , Var2 == "N" ~ "??"
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
    labs(x = "????", y = "??", fill="", subtitle = "") +
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
#     labs(x = "????", y = "??", fill="", subtitle = "") +
#     theme(text = element_text(size=18)) +
#     # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#     ggsave(filename = paste(globalVar$figConfig, "Img_042.png", sep="/"), width = 15, height = 10, dpi = 600)




#==========================================
# ????
#==========================================
dataL1 = dplyr::bind_rows(
    data.frame(table(data$CHLDRN_NM, data$INTRST_FASHN_AT), type =  "????")
    , data.frame(table(data$CHLDRN_NM, data$INTRST_PC_AT), type = "????")
    , data.frame(table(data$CHLDRN_NM, data$INTRST_HEALTH_AT), type = "????")
    , data.frame(table(data$CHLDRN_NM, data$INTRST_WDOMOUTH_AT), type = "?????")
    , data.frame(table(data$CHLDRN_NM, data$INTRST_QLITY_AT), type = "????")
    , data.frame(table(data$CHLDRN_NM, data$INTRST_SAFE_AT), type = "????")
    , data.frame(table(data$CHLDRN_NM, data$INTRST_EXPRN_AT), type = "????")
    , data.frame(table(data$CHLDRN_NM, data$INTRST_BRAND_AT), type = "?????")
)

dataL2 = dataL1 %>%
    dplyr::group_by(type, Var1) %>%
    dplyr::summarise(cnt = sum(Freq, na.rm = TRUE))

dataL3 = dataL1 %>%
    dplyr::left_join(dataL2, by = c("type" = "type", "Var1" = "Var1")) %>%
    dplyr::mutate(
        val = (Freq / cnt) *  100.0
        , isYn = dplyr::case_when(
            Var2 == "Y" ~ "??"
            , Var2 == "N" ~ "??"
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
    labs(x = "????", y = "??", fill="", subtitle = "") +
    theme(text = element_text(size=18)) +
    # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggsave(filename = paste(globalVar$figConfig, "Img_043.png", sep="/"), width = 15, height = 10, dpi = 600)



#==========================================
# ????
#==========================================
dataL1 = dplyr::bind_rows(
    data.frame(table(data$ASSETS_IDEX_NM, data$INTRST_FASHN_AT), type =  "????")
    , data.frame(table(data$ASSETS_IDEX_NM, data$INTRST_PC_AT), type = "????")
    , data.frame(table(data$ASSETS_IDEX_NM, data$INTRST_HEALTH_AT), type = "????")
    , data.frame(table(data$ASSETS_IDEX_NM, data$INTRST_WDOMOUTH_AT), type = "?????")
    , data.frame(table(data$ASSETS_IDEX_NM, data$INTRST_QLITY_AT), type = "????")
    , data.frame(table(data$ASSETS_IDEX_NM, data$INTRST_SAFE_AT), type = "????")
    , data.frame(table(data$ASSETS_IDEX_NM, data$INTRST_EXPRN_AT), type = "????")
    , data.frame(table(data$ASSETS_IDEX_NM, data$INTRST_BRAND_AT), type = "?????")
)

dataL2 = dataL1 %>%
    dplyr::group_by(type, Var1) %>%
    dplyr::summarise(cnt = sum(Freq, na.rm = TRUE))

dataL3 = dataL1 %>%
    dplyr::left_join(dataL2, by = c("type" = "type", "Var1" = "Var1")) %>%
    dplyr::mutate(
        val = (Freq / cnt) *  100.0
        , isYn = dplyr::case_when(
            Var2 == "Y" ~ "??"
            , Var2 == "N" ~ "??"
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
    labs(x = "????", y = "??", fill="", subtitle = "") +
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
    #     labs(x = "????", y = "??", fill="", subtitle = "") +
    #     theme(text = element_text(size=18)) +
    #     ggsave(filename = paste(globalVar$figConfig, "Img_044.png", sep="/"), width = 15, height = 8, dpi = 600)


#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? (??, ???)
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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
p                   =     2500
imdb                =     dataset_imdb(num_words = p, skip_top = 00) #, skip_top = 10
train_data          =     imdb$train$x
train_labels        =     imdb$train$y
test_data           =     imdb$test$x
test_labels         =     imdb$test$y

##?? ???? ?? ??? ?????... ?? ?????, ?? ? ???? ??? ???? ?????.??? ??? ??? ??????..

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
# Purpose : ???? (??, ???)
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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

# 2?? ???? ???? ????.
# 1. ???? ? ??? ?? (???)
# 2. ???? ???? ???? ?? ???? ???? ?????? ????? ??? ??? ? ? ??.


summary(data)


# TRANS DATE : ????
# BILL NO. transaction ??
# SEQ
# P. TYPE ??
# STORE ??? ?
# BRAND ????
# PORDUCT GENDER ??? ??? ?? ??(??, ??, ??)
# STYLE ?????(????)
# COLOR ??(????)
# COLOR DESC. ?? ??(????)
# SIZE ???
# CATEGORY ????
# STYLE DESC. ?? ????
# Q'TY ?? ??
# DC
# GROSS AMOUNT ????
# NET AMOUNT ?? ?? ??
# NA. ??
# UID ?? ?? key(UID? ?? ?? ??? ?????? ?? ??? ? ? ????)
# CUST GENDER ??? ??
# AGE ??? ???
# BRI YEAR ??? ??? ??
# SEASON ?? ??
# COST ??


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

# 1. ???? ? ??? ?? (???)
ggplot(dataL2, aes(x = meanVal, colour = type, fill = type)) +
  geom_histogram(aes(y=..density..), binwidth = 0.2,  colour="black", fill="white") +
  geom_density(alpha = 0.2) +
  stat_function(fun = dnorm, args = list(mean = mean(dataL2$meanVal, na.rm = TRUE), sd = sd(dataL2$meanVal, na.rm = TRUE)), col = 'blue') +
  geom_rug(aes(x = meanVal, y = 0), position = position_jitter(height = 0)) +
  labs(x = "??? [?? : 1,000,000]", y = "?? ??", colour = "??", fill="??", subtitle = "") +
  xlim(0, 20) +
  ylim(0, 0.35) +
  theme(text = element_text(size=18)) +
  ggsave(filename = "FIG/o2job/Img_050.png", width = 12, height = 6, dpi = 600)



# 2. ???? ???? ???? ?? ???? ???? ?????? ????? ??? ??? ? ? ??.

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
  labs(x = "??", y = "??? [?? : 1,000,000]", shape="??", colour="??", fill="", subtitle = "") +
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
# Purpose : ???? (??, ???)
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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
    dplyr::filter(???? %in% c("??", "????")) %>%
    tidyr::gather(-c(X1, ????, ????, ??), key = "key", value = "val") %>%
    dplyr::mutate(
        dtDate = lubridate::parse_date_time2(val, "%m.%d")
        , dtMonth = lubridate::month(dtDate)
        , dtDay = lubridate::day(dtDate)
        , sDate = paste(??, dtMonth, dtDay, sep = "-")
        , dtReqDate = readr::parse_date(sDate, "%Y-%m-%d")
        , dtYear = lubridate::year(dtReqDate)
        , dtReqJul = lubridate::yday(dtReqDate)
    )

dataL1 = data %>%
    # dplyr::select(date, ??_AOT40, ??_temp, ??_RH) %>%
    dplyr::mutate(
        dtYear = lubridate::year(date)
        , dtJul = lubridate::yday(date)
    ) %>%
    # dplyr::left_join(data2L1, by = c("dtYear" = "dtYear")) %>%
    dplyr::left_join(data2L1, by = c("date" = "dtReqDate")) %>%
    dplyr::filter(
        ???? == "???"
        , ???? == "??"
        # , key == "??"
    ) %>%
    dplyr::mutate(
        dtDiff = dtReqJul - dtJul
    ) %>%
    dplyr::arrange(date) %>%
    na.omit() %>%
    dplyr::select(??_AOT40, ??_temp, ??_RH, dtReqJul) %>%
    dplyr::rename(
        aot = "??_AOT40"
        , temp = "??_temp"
        , rh = "??_RH"
    )


# ??? ?? (0 - 1 ??)
dataL1$aot = scales::rescale(dataL1$aot)
dataL1$temp = scales::rescale(dataL1$temp)
dataL1$rh = scales::rescale(dataL1$rh)


dataL1 %>%
    dplyr::select(??_AOT40, ?d?_temp, ??_RH, dtJul) %>%
    ggpairs(.) +
    theme(text = element_text(size=18)) +
    ggsave(filename = paste(globalVar$figConfig, "Img_060_ggpairs.png", sep="/"), width = 12, height = 8, dpi = 600)

dataL1 %>%
    dplyr::select(??_AOT40, ??_temp, ??_RH, dtJul) %>%
    cor()


trainData = dataL1
testData = dataL1

#====================================
# ??? ??
#====================================
ind = nrow(dataL1)

# ?? ? ??? ?? 60:40?? ??? ?? ??? ??
# ind = sample(1:nrow(dataL1), nrow(dataL1) * 0.6)

# ?? ???? ?? ?? ??
trainData = dataL1[-ind, ]
testData = dataL1[ind, ]

# ?? ???? ??
dplyr::tbl_df(trainData)

# ??? ???? ??
dplyr::tbl_df(testData)


xVar = "aot + temp + rh"
yVar = "dtReqJul"

form = paste0(yVar, " ~ ", xVar)
# form = "dtDiff ~ ??_AOT40 + ??_temp + ??_RH"


resultData = data.frame()

#===============================================
# ?????? ??
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
# neuralnet ??
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
# Deep Learning ??
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




# ????????
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
    labs(title = "", x = "??", y = "??", subtitle = "????????") +
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
    labs(title = "", x = "??", y = "??", subtitle = "?????") +
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
    labs(title = "", x = "??", y = "??", subtitle = "???") +
    coord_equal() +
    theme(text = element_text(size=18)) +
    ggsave(filename = paste(globalVar$figConfig, "Img_060_DL.png", sep="/"), width = 6, height = 6, dpi = 600)




#===============================================
# mlptrain, mlp ??
#===============================================

inDataL2 = dataL1 %>%
    dplyr::select(??_AOT40, ??_temp, ??_RH) %>%
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
# Purpose : ???? (??, ???)
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

#==========================================
# ??? ??
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

fileInfo = Sys.glob(paste(globalVar$inpConfig, "?????.xlsx", sep = "/"))
data_human <- xlsx::read.xlsx2(fileInfo, sheetIndex = 1)

data_human = data_human %>%
  dplyr::distinct()

data_human$id <- as.numeric(data_human$id)
full_data <- dplyr::inner_join(data_wind,data_human,by=c("id"))

## BWS ?? ???? ??? ##
questnum = 4.0
full_data_L1 <- BWS_SCORE(full_data)


## SEQ1 ??? ??? ?? (B-W? ??? BAR ???) ##

################# ?? ??? ???? #####################

# ?? ?? #
sex_full <- unique(full_data_L1$sex)
age_full <- unique(full_data_L1$age)
nfam_full <- unique(full_data_L1$nfam)
edu_full <- unique(full_data_L1$edu)
income_full <- unique(full_data_L1$income)
job_full <- unique(full_data_L1$job)
marr_full <- unique(full_data_L1$marr)


xindex <- c("B-W", "Mean(B-W)", "SQRT(B-W)")
yindex <- c("?", "?", "?", "????", "????", "??", "???", "???", "??", "??", "????", "???", "????")

## ?? ??? ?? ##
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

  ## ?? ??? ?? ##
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

  ## ?? ??? ?? ##
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

  ## ?? ??? ?? ##
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

  ## ?? ??? ?? ##
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

  ## ?? ??? ?? ##
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

  ## ?? ??? ?? ##
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

  ## ?? ??? ?? ##
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



################# ?? ??? ???? #####################



## ?? ??? ?? ##
full_data_PARTR_ALL <- full_data_L1 %>%
  dplyr::mutate(all = 1) %>%
  dplyr::group_by(all) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(all, BWSQRT01:BWSQRT13) %>%
  dplyr::rename(
    "?" = "BWSQRT01"
    , "?" = "BWSQRT02"
    , "?" = "BWSQRT03"
    , "????" = "BWSQRT04"
    , "????" = "BWSQRT05"
    , "??" = "BWSQRT06"
    , "???" = "BWSQRT07"
    , "???" = "BWSQRT08"
    , "??" = "BWSQRT09"
    , "??" = "BWSQRT10"
    , "????" = "BWSQRT11"
    , "???" = "BWSQRT12"
    , "????" = "BWSQRT13"
  )


dataL1_all = full_data_PARTR_ALL %>%
  tidyr::gather(-all, key="key", val="val")

dataL1_all$key = forcats::fct_relevel(dataL1_all$key, yindex)

ggplot(dataL1_all, aes(x = key, y = val, fill = key)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  geom_text(aes(x=key, y=val, label = round(val, 2)), size = 5, color="white", vjust=2, position = position_dodge(width=0.9)) +
  geom_hline(yintercept=1) +
  labs(x = "", y = "??? ??", fill="", subtitle = "[??] ??? ?? ??? ??") +
  theme(text = element_text(size=18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figConfig, "BWS/full.png", sep="/"), width = 12, height = 8, dpi = 600)


## ??? ?? ??? ??? ?? ##
full_data_PARTR_SEX <- full_data_L1 %>%
  dplyr::group_by(sex) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(sex,BWSQRT01:BWSQRT13) %>%
  dplyr::rename(
    "?" = "BWSQRT01"
    , "?" = "BWSQRT02"
    , "?" = "BWSQRT03"
    , "????" = "BWSQRT04"
    , "????" = "BWSQRT05"
    , "??" = "BWSQRT06"
    , "???" = "BWSQRT07"
    , "???" = "BWSQRT08"
    , "??" = "BWSQRT09"
    , "??" = "BWSQRT10"
    , "????" = "BWSQRT11"
    , "???" = "BWSQRT12"
    , "????" = "BWSQRT13"
  )

dataL1_sex = full_data_PARTR_SEX %>%
  tidyr::gather(-sex, key="key", val="val")

dataL1_sex$key = forcats::fct_relevel(dataL1_sex$key, yindex)

ggplot(dataL1_sex, aes(x = key, y = val, fill = sex)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  geom_text(aes(x=key, y=val, label = round(val, 1)), size = 5, color="white", vjust=2, position = position_dodge(width=0.9)) +
  geom_hline(yintercept=1) +
  labs(x = "", y = "??? ??", fill="", subtitle = "[??] ??? ?? ??? ??") +
  theme(text = element_text(size=18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figConfig, "BWS/sex.png", sep="/"), width = 12, height = 8, dpi = 600)


## ??? ?? ??? ??? ?? ##
full_data_PARTR_AGE <- full_data_L1 %>%
  dplyr::group_by(age) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(age,BWSQRT01:BWSQRT13) %>%
  dplyr::rename(
    "?" = "BWSQRT01"
    , "?" = "BWSQRT02"
    , "?" = "BWSQRT03"
    , "????" = "BWSQRT04"
    , "????" = "BWSQRT05"
    , "??" = "BWSQRT06"
    , "???" = "BWSQRT07"
    , "???" = "BWSQRT08"
    , "??" = "BWSQRT09"
    , "??" = "BWSQRT10"
    , "????" = "BWSQRT11"
    , "???" = "BWSQRT12"
    , "????" = "BWSQRT13"
  )


dataL1_age = full_data_PARTR_AGE %>%
  tidyr::gather(-age, key="key", val="val")

dataL1_age$key = forcats::fct_relevel(dataL1_age$key, yindex)

ggplot(dataL1_age, aes(x = key, y = val, fill = age)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  # geom_text(aes(x=key, y=val, label = round(val, 1)), size = 5, color="white", vjust=2, position = position_dodge(width=0.9),size = 2) +
  geom_hline(yintercept=1) +
  labs(x = "", y = "??? ??", fill="", subtitle = "[??] ??? ?? ??? ??") +
  theme(text = element_text(size=18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figConfig, "BWS/age.png", sep="/"), width = 12, height = 8, dpi = 600)


## nfam? ?? ??? ??? ?? ##
full_data_PARTR_nfam <- full_data_L1 %>%
  dplyr::group_by(nfam) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(nfam,BWSQRT01:BWSQRT13) %>%
  dplyr::rename(
    "?" = "BWSQRT01"
    , "?" = "BWSQRT02"
    , "?" = "BWSQRT03"
    , "????" = "BWSQRT04"
    , "????" = "BWSQRT05"
    , "??" = "BWSQRT06"
    , "???" = "BWSQRT07"
    , "???" = "BWSQRT08"
    , "??" = "BWSQRT09"
    , "??" = "BWSQRT10"
    , "????" = "BWSQRT11"
    , "???" = "BWSQRT12"
    , "????" = "BWSQRT13"
  )



dataL1_nfam = full_data_PARTR_nfam %>%
  tidyr::gather(-nfam, key="key", val="val")

dataL1_nfam$key = forcats::fct_relevel(dataL1_nfam$key, yindex)

ggplot(dataL1_nfam, aes(x = key, y = val, fill = nfam)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  geom_text(aes(x=key, y=val, label = round(val, 0)), size = 5, color="white", vjust=2, position = position_dodge(width=0.9),size = 2) +
  geom_hline(yintercept=1) +
  labs(x = "", y = "??? ??", fill="", subtitle = "??? ?? ??? ??") +
  theme(text = element_text(size=18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figConfig, "BWS/nfam.png", sep="/"), width = 12, height = 8, dpi = 600)


## edu? ?? ??? ??? ?? ##
full_data_PARTR_edu <- full_data_L1 %>%
  dplyr::group_by(edu) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(edu,BWSQRT01:BWSQRT13) %>%
  dplyr::rename(
    "?" = "BWSQRT01"
    , "?" = "BWSQRT02"
    , "?" = "BWSQRT03"
    , "????" = "BWSQRT04"
    , "????" = "BWSQRT05"
    , "??" = "BWSQRT06"
    , "???" = "BWSQRT07"
    , "???" = "BWSQRT08"
    , "??" = "BWSQRT09"
    , "??" = "BWSQRT10"
    , "????" = "BWSQRT11"
    , "???" = "BWSQRT12"
    , "????" = "BWSQRT13"
  )

dataL1_edu = full_data_PARTR_edu %>%
  tidyr::gather(-edu, key="key", val="val")

dataL1_edu$key = forcats::fct_relevel(dataL1_edu$key, yindex)

ggplot(dataL1_edu, aes(x = key, y = val, fill = edu)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  geom_text(aes(x=key, y=val, label = round(val, 0)), size = 5, color="white", vjust=2, position = position_dodge(width=0.9),size = 2) +
  geom_hline(yintercept=1) +
  labs(x = "", y = "??? ??", fill="", subtitle = "[??] ??? ?? ??? ??") +
  theme(text = element_text(size=18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figConfig, "BWS/edu.png", sep="/"), width = 12, height = 8, dpi = 600)



## income? ?? ??? ??? ?? ##
full_data_PARTR_income <- full_data_L1 %>%
  dplyr::group_by(income) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(income,BWSQRT01:BWSQRT13) %>%
  dplyr::rename(
    "?" = "BWSQRT01"
    , "?" = "BWSQRT02"
    , "?" = "BWSQRT03"
    , "????" = "BWSQRT04"
    , "????" = "BWSQRT05"
    , "??" = "BWSQRT06"
    , "???" = "BWSQRT07"
    , "???" = "BWSQRT08"
    , "??" = "BWSQRT09"
    , "??" = "BWSQRT10"
    , "????" = "BWSQRT11"
    , "???" = "BWSQRT12"
    , "????" = "BWSQRT13"
  )



dataL1_income = full_data_PARTR_income %>%
  tidyr::gather(-income, key="key", val="val")

dataL1_income$key = forcats::fct_relevel(dataL1_income$key, yindex)

ggplot(dataL1_income, aes(x = key, y = val, fill = income)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  geom_text(aes(x=key, y=val, label = round(val, 0)), size = 5, color="white", vjust=2, position = position_dodge(width=0.9),size = 2) +
  geom_hline(yintercept=1) +
  labs(x = "", y = "??? ??", fill="", subtitle = "[??] ??? ?? ??? ??") +
  theme(text = element_text(size=18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figConfig, "BWS/income.png", sep="/"), width = 12, height = 8, dpi = 600)



## job? ?? ??? ??? ?? ##
full_data_PARTR_job <- full_data_L1 %>%
  dplyr::group_by(job) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(job,BWSQRT01:BWSQRT13) %>%
  dplyr::rename(
    "?" = "BWSQRT01"
    , "?" = "BWSQRT02"
    , "?" = "BWSQRT03"
    , "????" = "BWSQRT04"
    , "????" = "BWSQRT05"
    , "??" = "BWSQRT06"
    , "???" = "BWSQRT07"
    , "???" = "BWSQRT08"
    , "??" = "BWSQRT09"
    , "??" = "BWSQRT10"
    , "????" = "BWSQRT11"
    , "???" = "BWSQRT12"
    , "????" = "BWSQRT13"
  )


dataL1_job = full_data_PARTR_job %>%
  tidyr::gather(-job, key="key", val="val")

dataL1_job$key = forcats::fct_relevel(dataL1_job$key, yindex)

ggplot(dataL1_job, aes(x = key, y = val, fill = job)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  geom_text(aes(x=key, y=val, label = round(val, 0)), size = 5, color="white", vjust=2, position = position_dodge(width=0.9),size = 2) +
  geom_hline(yintercept=1) +
  labs(x = "", y = "??? ??", fill="", subtitle = "[??] ??? ?? ??? ??") +
  theme(text = element_text(size=18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figConfig, "BWS/job.png", sep="/"), width = 12, height = 8, dpi = 600)



## marr? ?? ??? ??? ?? ##
full_data_PARTR_marr <- full_data_L1 %>%
  dplyr::group_by(marr) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>%
  dplyr::select(marr,BWSQRT01:BWSQRT13) %>%
  dplyr::rename(
    "?" = "BWSQRT01"
    , "?" = "BWSQRT02"
    , "?" = "BWSQRT03"
    , "????" = "BWSQRT04"
    , "????" = "BWSQRT05"
    , "??" = "BWSQRT06"
    , "???" = "BWSQRT07"
    , "???" = "BWSQRT08"
    , "??" = "BWSQRT09"
    , "??" = "BWSQRT10"
    , "????" = "BWSQRT11"
    , "???" = "BWSQRT12"
    , "????" = "BWSQRT13"
  )

dataL1_marr = full_data_PARTR_marr %>%
  tidyr::gather(-marr, key="key", val="val")

dataL1_marr$key = forcats::fct_relevel(dataL1_marr$key, yindex)

ggplot(dataL1_marr, aes(x = key, y = val, fill = marr)) +
  geom_bar(position="dodge", stat="identity", color="black") +
  geom_text(aes(x=key, y=val, label = round(val, 0)), size = 5, color="white", vjust=2, position = position_dodge(width=0.9),size = 2) +
  geom_hline(yintercept=1) +
  labs(x = "", y = "??? ??", fill="", subtitle = "??? ?? ??? ??") +
  theme(text = element_text(size=18)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(filename = paste(globalVar$figConfig, "BWS/marr.png", sep="/"), width = 12, height = 8, dpi = 600)



#======================================================
# ?? ??
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

# fileInfo = "E:/04. ?????/Github/TalentPlatform-R/INPUT/o2job/KYUNG/kyung1234.csv"

for (fileInfo in fileList) {

    fileName = tools::file_path_sans_ext(fs::path_file(fileInfo))
    data = read.csv(fileInfo, header=TRUE, sep=",", stringsAsFactors = FALSE)

    log4r::info(log, paste0("fileInfo : ", fileName))

    fileInfo2 = Sys.glob(paste(globalVar$inpConfig, "?????.xlsx", sep = "/"))
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
    # ???? (levelList)
    #=======================================
    for (levelInfo in levelList) {

      log4r::info(log, paste0("levelInfo : ", levelInfo))

      saveFile = paste0("WineType/WindType_", fileName, "_levelInfo_", levelInfo, ".csv")
      saveImg = paste0("WineType/WindType_", fileName, "_levelInfo_", levelInfo, ".png")

      dataL2 = dataL1 %>%
        dplyr::filter(level == levelInfo) %>%
        dplyr::select(- c("level", "fre", "day", "drinkp"))


      # ?? ??
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
    # ???? (freList)
    #=======================================
    for (freInfo in freList) {

      log4r::info(log, paste0("freInfo : ", freInfo))

      saveFile = paste0("WineType/WindType_", fileName, "_freInfo_", freInfo, ".csv")
      saveImg = paste0("WineType/WindType_", fileName, "_freInfo_", freInfo, ".png")

      dataL2 = dataL1 %>%
        dplyr::filter(fre == freInfo) %>%
        dplyr::select(- c("level", "fre", "day", "drinkp"))

      # ?? ??
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
    # ???? (dayList)
    #=======================================
    for (dayInfo in dayList) {

      log4r::info(log, paste0("dayInfo : ", dayInfo))

      saveFile = paste0("WineType/WindType_", fileName, "_dayInfo_", dayInfo, ".csv")
      saveImg = paste0("WineType/WindType_", fileName, "_dayInfo_", dayInfo, ".png")

      dataL2 = dataL1 %>%
        dplyr::filter(day == dayInfo) %>%
        dplyr::select(- c("level", "fre", "day", "drinkp"))


      # ?? ??
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
    # ???? (drinkpList)
    #=======================================
    for (drinkpInfo in drinkpList) {

      log4r::info(log, paste0("drinkpInfo : ", drinkpInfo))

      saveFile = paste0("WineType/WindType_", fileName, "_drinkpInfo_", drinkpInfo, ".csv")
      saveImg = paste0("WineType/WindType_", fileName, "_drinkpInfo_", drinkpInfo, ".png")

      dataL2 = dataL1 %>%
          dplyr::filter(drinkp == drinkpInfo) %>%
          dplyr::select(- c("level", "fre", "day", "drinkp"))


      # ?? ??
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
# Purpose : ???? (??, ???)
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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

fileInfo = Sys.glob(paste(globalVar$inpConfig, "2015-2019 ????? ????_L1.xlsx", sep = "/"))

#==========================================
# ??? 1
#==========================================
mapInfo = Sys.glob(paste(globalVar$mapConfig, "/KOREA_INFO/TL_SCCO_CTPRVN.shp", sep = "/"))
la = shapefile(mapInfo, encoding = "UTF-8")

geo = sp::spTransform(la, CRS("+proj=longlat"))
geoData = ggplot2::fortify(geo,region = 'CTPRVN_CD',region2 = "CTP_KOR_NM")
# head(geoData)

# ?? ?? ?? (??? - ??)
codeInfo = Sys.glob(paste(globalVar$mapConfig, "/ADM_CODE/?????_????.txt", sep = "/"))
code = read.table(codeInfo, sep="\t", header = TRUE, colClasses = "character")
colnames(code) = c("EMD_CD", "full_addr", "useflag")

code_L1 = code %>%
  tidyr::separate(col = "full_addr", into = c("d1", "d2", "d3", "d4", "d5"), sep=" ") %>%
  dplyr::filter(is.na(d3), is.na(d2)) %>%
  dplyr::mutate(code = str_sub(EMD_CD,1,2)) %>%
  dplyr::filter(useflag == "??") %>%
  dplyr::select(-c(EMD_CD, d2, d3, d4, d5, useflag)) %>%
  dplyr::rename(
    "si_do" = "d1"
    , "sido_code" = "code"
  )

unique(code_L1$si_do)

# ?? ? ??? ??
code_L2_deaseon = code_L1 %>%
  dplyr::filter(si_do %in%c("????", "????", "?????", "???????")) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(sido_code)) %>%
  dplyr::mutate(code = as.character(round(as.numeric(sido_code, 0))))

geoData_L1 = geoData %>%
  dplyr::inner_join(code_L2_deaseon, by = c("id" = "code"))


# ?? ???? ??? ?? (??)
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

# key5Info = "??????"
# yearInfo = "2015"
# key5Info = "???"
# key6Info = "??"


for (key5Info in key5List) {
  for (key6Info in key6List) {

    # ?? ??
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
          si_do == "????" ~ dplyr::if_else(! is.na(n) & n > 0, paste0(round(n, 0), "\n??"), "??")
          , si_do == "???????" ~ dplyr::if_else(! is.na(n) & n > 0, paste0(round(n, 0), "\n??"), "??")
          , si_do == "????" ~ dplyr::if_else(! is.na(n) & n > 0, paste0(round(n, 0), "\n??"), "??")
          , si_do == "?????" ~ dplyr::if_else(! is.na(n) & n > 0, paste0(round(n, 0), "\n??"), "??")
          , TRUE ~ ""
        )
        , xOffset = dplyr::case_when(
          si_do == "????" ~ 0.25
          , si_do == "????" ~ -0.35
          , si_do == "???????" ~ -0.015
          , TRUE ~ 0
        )
        , yOffset = dplyr::case_when(
          TRUE ~ 0
        )
      )

    # ???
    saveImg = paste0("TMP2/Img_076_", "2015-2019", "_", key6Info, "_", key5Info, ".png")
    plotSubTitle = paste0("[", key6Info, "] ", key5Info, " ????? ??? ??(", "2015-2019", ")")


    ggplot() +
      theme_bw() +
      coord_fixed(ratio = 1) +
      geom_polygon(data=dataL2, aes(x=long, y=lat, group=group, fill = n)) +
      scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
      geom_path(data=dataL2, aes(x=long,y=lat,group=group), colour='black',size = 0.5) +
      ggh4x::stat_midpoint(data=dataL2, aes(x=long + xOffset, y=lat + yOffset, group=sido_code, label = plotLabel), geom = "text", size = 4) +
      #ggh4x::stat_midpoint(data=geoData_L1, aes(x=long, y=lat, group=sigungu_code, label = sigungu_name), geom = "text", size = 3) + # ???
      labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "?? : ?", subtitle = NULL) +
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

    # ? ??? ??
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
              si_do == "????" ~ dplyr::if_else(! is.na(n) & n > 0, paste0(round(n, 0), "\n??"), "??")
              , si_do == "???????" ~ dplyr::if_else(! is.na(n) & n > 0, paste0(round(n, 0), "\n??"), "??")
              , si_do == "????" ~ dplyr::if_else(! is.na(n) & n > 0, paste0(round(n, 0), "\n??"), "??")
              , si_do == "?????" ~ dplyr::if_else(! is.na(n) & n > 0, paste0(round(n, 0), "\n??"), "??")
              , TRUE ~ ""
              )
            , xOffset = dplyr::case_when(
              si_do == "????" ~ 0.25
              , si_do == "????" ~ -0.35
              , si_do == "???????" ~ -0.015
              , TRUE ~ 0
              )
            , yOffset = dplyr::case_when(
              TRUE ~ 0
              )
            )

        # ???
        saveImg = paste0("TMP2/Img_076_", yearInfo, "_", key6Info, "_", key5Info, ".png")
        plotSubTitle = paste0("[", key6Info, "] ", key5Info, " ????? ??? ??(", yearInfo, ")")

        ggplot() +
          theme_bw() +
          coord_fixed(ratio = 1) +
          geom_polygon(data=dataL2, aes(x=long, y=lat, group=group, fill = n)) +
          scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
          geom_path(data=dataL2, aes(x=long, y=lat, group=group), colour='black',size = 0.5) +
          ggh4x::stat_midpoint(data=dataL2, aes(x=long + xOffset, y=lat + yOffset, group=sido_code, label = plotLabel), geom = "text", size = 4) +
          #ggh4x::stat_midpoint(data=geoData_L1, aes(x=long, y=lat, group=sigungu_code, label = sigungu_name), geom = "text", size = 3) + # ???
          labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "?? : ?", subtitle = NULL) +
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
# ??? 2
#==========================================
refKey = c("??", "??", "??", "??", "??", "??")

data = xlsx::read.xlsx2(file = fileInfo, sheetName = "G2") %>%
  readr::type_convert()

tmpData = data %>%
  dplyr::group_by(year, key2, key3) %>%
  dplyr::summarise(
    val = sum(val, na.rm = TRUE)
  ) %>%
  dplyr::bind_cols(key = "??")

dataL1 = data %>%
  bind_rows(tmpData)

key2List = unique(dataL1$key2)
key3List = unique(dataL1$key3)

# key2Info = "??"
# key3Info = "??????"

for (key2Info in key2List) {
  for (key3Info in key3List) {
    plotSubTitle = paste0("[", key2Info, "] ", key3Info, " ????? ??? ??? ??")
    saveImg = paste0("TMP2/Img_070_", key2Info, "_", key3Info, ".png")

    dataL2 = dataL1 %>%
      dplyr::filter(
        key2 == key2Info
        , key3 == key3Info
      )

    ind = which(dataL2$key == "??")

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
      labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, subtitle = "?? : %") +
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
# ??? 2.2
#==========================================
refKey = c("??", "??", "??")

data = xlsx::read.xlsx2(file = fileInfo, sheetName = "G2-2") %>%
  readr::type_convert()

tmpData = data %>%
  dplyr::group_by(year, key2, key3) %>%
  dplyr::summarise(
    val = sum(val, na.rm = TRUE)
  ) %>%
  dplyr::bind_cols(key = "??")

dataL1 = data %>%
  bind_rows(tmpData)

key2List = unique(dataL1$key2)
key3List = unique(dataL1$key3)

for (key2Info in key2List) {
  for (key3Info in key3List) {
    plotSubTitle = paste0("[", key2Info, "] ", key3Info, " ????? ?? ??? ??")
    saveImg = paste0("TMP2/Img_071_", key2Info, "_", key3Info, ".png")

    dataL2 = dataL1 %>%
      dplyr::filter(
        key2 == key2Info
        , key3 == key3Info
      )

    ind = which(dataL2$key == "??")

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
      labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, subtitle = "?? : %") +
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
# ??? 3
#==========================================
refKey = c("??", "??", "??")

data = xlsx::read.xlsx2(file = fileInfo, sheetName = "G3") %>%
  readr::type_convert()

tmpData = data %>%
  dplyr::group_by(year, key2, key3) %>%
  # dplyr::group_by(year) %>%
  dplyr::summarise(
    val = sum(val, na.rm = TRUE)
  ) %>%
  dplyr::bind_cols(key = "??")

dataL1 = data

yearList = unique(dataL1$year)
key2List = unique(dataL1$key2)
key3List = unique(dataL1$key3)

yearInfo = 2015
key2Info = "??"
key3Info = "???"

for (yearInfo in yearList) {
    for (key2Info in key2List) {
      for (key3Info in key3List) {

        plotSubTitle = paste0("[", key2Info, "] ", key3Info, " ????? ?????? ??(", yearInfo, ")")
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
              key == "??" ~  -(val / sumData$val) * 100
              , key == "??" ~  (val / sumData$val) * 100
              , TRUE ~ NA_real_
            )
            # , perValLabel =  sprintf("%.2f", abs(perVal))
            , perValLabel =  sprintf("%.1f", abs(perVal))
            , nudgePerVal = dplyr::case_when(
                 perVal > 0 ~  (perVal + 1.25)
                 , perVal < 0 ~  (perVal - 1.25)
                 , perVal == 0 & key == "??" ~ -1.1
                 , perVal == 0 & key == "??" ~ 1.1
                 , TRUE ~  NA_real_
              )
          )


        ggplot(dataL3, aes_string(x = "key4", y = "perVal", fill = "key")) +
          geom_bar(stat = "identity") +
          theme_bw() +
          labs(title = plotSubTitle, x = NULL, y = NULL, fill = NULL, colour = NULL, subtitle = "?? : %") +
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
# ??? 4 : ???? ??
#==========================================
mapInfo = Sys.glob(paste(globalVar$mapConfig, "/KOREA_INFO/TL_SCCO_SIG.shp", sep = "/"))
la = shapefile(mapInfo, encoding = "UTF-8")

geo = spTransform(la, CRS("+proj=longlat"))
head(geo)
geoData = ggplot2::fortify(geo,region = 'SIG_CD',region2 = "SIG_KOR_NM")
head(geoData)

codeInfo = Sys.glob(paste(globalVar$mapConfig, "/ADM_CODE/?????_????.txt", sep = "/"))
code = read.table(codeInfo, sep="\t", header = TRUE, colClasses = "character") %>%
  dplyr::rename(
    "EMD_CD" = "?????"
    , "full_addr" = "????"
    , "useflag" = "????"
  )

code_L1 = code %>%
  tidyr::separate(col = "full_addr",into = c("d1","d2","d3","d4","d5"), sep=" ") %>%
  dplyr::filter(is.na(d3), !is.na(d2)) %>%
  dplyr::mutate(code = str_sub(EMD_CD,1,5)) %>%
  dplyr::filter(useflag == "??") %>%
  dplyr::select(- c(EMD_CD, d3, d4, d5, useflag)) %>%
  dplyr::rename(
    "si_do" = "d1"
    , "sigungu_name" = "d2"
    , "sigungu_code" = "code"
  )


unique(code_L1$si_do)

# ?? ? ??? ??
geoData_L1 = code_L1 %>%
  dplyr::filter(si_do %in% c("????", "?????", "???????")) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(sigungu_code)) %>%
  dplyr::mutate(code = as.character(round(as.numeric(sigungu_code,0)))) %>%
  dplyr::inner_join(geoData, by = c("code" = "id"))

# ?? ???? ??? ?? (??)
data = openxlsx::read.xlsx(xlsxFile = fileInfo, sheet = "G1") %>%
  readr::type_convert() %>%
  dplyr::mutate(
    dtDate = readr::parse_date(as.character(date), "%Y-%m-%d")
    , dtYear = lubridate::year(dtDate)
  )

yearList = sort(unique(data$dtYear))
key5List = unique(data$key5)
key6List = unique(data$key6)

# addr1Info = "????"
# key5Info = "??????"
# key6Info = "??"


for (key5Info in key5List) {
  for (key6Info in key6List) {

    # ??
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
          sigungu_name == "???" ~ 0.275
          , sigungu_name == "???" ~ 0.275
          , sigungu_name == "???" ~ 0
          , sigungu_name == "???" ~ 0.05
          , sigungu_name == "???" ~ 0.05
          , sigungu_name == "???" ~ 0.05
          , TRUE ~ 0
        )
        , yOffset = dplyr::case_when(
          sigungu_name == "???" ~ -0.05
          , sigungu_name == "???" ~ 0.1
          , sigungu_name == "???" ~ 0
          , sigungu_name == "???" ~ -0.1
          , TRUE ~ 0
        )
      )

    # ???
    # saveImg = paste0("TMP2/Img_077.png")
    saveImg = paste0("TMP2/Img_077_", "2015-2019", "_", key6Info, "_", key5Info, ".png")
    plotSubTitle = paste0("[", key6Info, "] ", key5Info, " ????? ???? ??(", yearInfo, ")")

    ggplot() +
      theme_bw() +
      coord_fixed(ratio = 1) +
      geom_polygon(data=dataL2, aes(x=long, y=lat, group=group, fill = n)) +
      scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
      geom_path(data=dataL2, aes(x=long, y=lat, group=group), colour='black',size = 0.5) +
      ggh4x::stat_midpoint(data=dataL2, aes(x=long + xOffset, y=lat + yOffset, group=sigungu_code, label = plotLabel), geom = "text", size = 4) +
      labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "?? : ?", subtitle = NULL) +
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



    # ???
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
            sigungu_name == "???" ~ 0.275
            , sigungu_name == "???" ~ 0.275
            , sigungu_name == "???" ~ 0
            , sigungu_name == "???" ~ 0.05
            , sigungu_name == "???" ~ 0.05
            , sigungu_name == "???" ~ 0.05
            , TRUE ~ 0
          )
          , yOffset = dplyr::case_when(
            sigungu_name == "???" ~ -0.05
            , sigungu_name == "???" ~ 0.1
            , sigungu_name == "???" ~ 0
            , sigungu_name == "???" ~ -0.1
            , TRUE ~ 0
            )
        )

      # ???
      # saveImg = paste0("TMP2/Img_077.png")
      saveImg = paste0("TMP2/Img_077_", yearInfo, "_", key6Info, "_", key5Info, ".png")
      plotSubTitle = paste0("[", key6Info, "] ", key5Info, " ????? ???? ??(", yearInfo, ")")

      ggplot() +
        theme_bw() +
        coord_fixed(ratio = 1) +
        geom_polygon(data=dataL2, aes(x=long, y=lat, group=group, fill = n)) +
        scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
        geom_path(data=dataL2, aes(x=long, y=lat, group=group), colour='black',size = 0.5) +
        ggh4x::stat_midpoint(data=dataL2, aes(x=long + xOffset, y=lat + yOffset, group=sigungu_code, label = plotLabel), geom = "text", size = 4) +
        labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "?? : ?", subtitle = NULL) +
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
# ??? 5 : ??? ??
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

# key5Info = "??????"
# key6Info = "??"
# addr1Info = "????"

# ??
for (key6Info in key6List) {
    for (key5Info in key5List) {

      plotSubTitle = paste0("[", key6Info, "] ", key5Info, " ????? ??? ??(2015~2019)")
      saveImg = paste0("TMP2/Img_073_", key6Info, "_", key5Info, "_", "??", ".png")

      tmpData = dataL1 %>%
        dplyr::filter(
          key5 == key5Info
          , key6 == key6Info
        ) %>%
        dplyr::group_by(dtYear, key5, key6) %>%
        dplyr::summarise(cnt = n()) %>%
        dplyr::mutate(facetLabel = "??")

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
        labs(title = plotSubTitle, x = NULL, y = NULL, fill = NULL, colour = NULL, subtitle = "?? : %") +
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


# ???
for (key6Info in key6List) {
  for (key5Info in key5List) {
      for (addr1Info in addr1List) {

              plotSubTitle = paste0("[", key6Info, "] ", key5Info, " ????? ??? ??(2015~2019)")
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
                  labs(title = plotSubTitle, x = NULL, y = NULL, fill = NULL, colour = NULL, subtitle = "?? : %") +
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
# ??? 5 : ???? ??
#==========================================
key5List = unique(dataL1$key5)
key6List = unique(dataL1$key6)

addr1List = unique(dataL1$addr1)
addr2List = unique(dataL1$addr2)
addr3List = unique(dataL1$addr3)

# key5Info = "??????"
# key6Info = "??"
# addr1Info = "????"
# addr2Info = "???"

for (key6Info in key6List) {
  for (key5Info in key5List) {
    for (addr1Info in addr1List) {
        for (addr2Info in addr2List) {

                plotSubTitle = paste0("[", key6Info, "] ", key5Info, " ????? ???? ??(2015~2019)")
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
                    labs(title = plotSubTitle, x = NULL, y = NULL, fill = NULL, colour = NULL, subtitle = "?? : %") +
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
# ??? 6 : ???? ??
#==========================================
# ??? ??? ?? ??
mapInfo = Sys.glob(paste(globalVar$mapConfig, "/KOREA_INFO/bnd_dong_00_2019_2019_2Q.shp", sep = "/"))
la = shapefile(mapInfo, encoding = "UTF-8")
geo = sp::spTransform(la, CRS("+proj=longlat"))

geoData = ggplot2::fortify(geo, region = 'adm_dr_cd',region2 = "adm_dr_nm")
head(geoData)


# ?? ?? (???) ??
codeInfo = Sys.glob(paste(globalVar$mapConfig, "/ADM_CODE/adm_code.xlsx", sep = "/"))
code = openxlsx::read.xlsx(codeInfo, sheet = 1, startRow = 2)

code_L1 = code %>%
    dplyr::select("????", "????", "?????", "?????", "?????", "?????") %>%
    dplyr::rename(
        "si_do" = "????"
        , "si_do_name" = "????"
        , "sigungu_code" = "?????"
        , "sigungu_name" = "?????"
        , "emd_code" = "?????"
        , "emd_name" = "?????"
    )

dplyr::tbl_df(code_L1)

unique(code_L1$si_do_name)

# ?? ??? ??? ?? ??
geoData_L1 = code_L1 %>%
    # dplyr::filter(si_do_name %in% c("?????", "????")) %>%
    dplyr::filter(si_do_name %in% c("????")) %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(sigungu_code)) %>%
    # dplyr::mutate(code = as.character(round(as.numeric(sigungu_code,0)))) %>%
    dplyr::mutate(code = as.character(round(as.numeric(emd_code, 0)))) %>%
    dplyr::inner_join(geoData, by = c("code" = "id"))

dplyr::tbl_df(geoData_L1)

options(java.parameters = "-Xmx8192m")


# ?? ??? ??
data = openxlsx::read.xlsx(xlsxFile = fileInfo, sheet = "G1") %>%
  readr::type_convert() %>%
  dplyr::mutate(
    dtDate = readr::parse_date(as.character(date), "%Y-%m-%d")
    , dtYear = lubridate::year(dtDate)
  )

yearList = sort(unique(data$dtYear))
key5List = unique(data$key5)
key6List = unique(data$key6)

# addr1Info = "????"
# addr2Info = "???"
# yearInfo = 2018
# key5Info = "??????"
# key6Info = "??"


for (key5Info in key5List) {
  for (key6Info in key6List) {

    # ??
    tmpData = data %>%
      dplyr::group_by(key5, key6, addr1) %>%
      dplyr::summarise(sumVal = n()) %>%
      dplyr::filter(
        key5 == key5Info
        , key6 == key6Info
        , addr1 == "????"
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
          , stringr::str_detect(emd_name, regex("??.?")) ~ ""
          , stringr::str_detect(emd_name, regex("??.?")) ~ ""
          , stringr::str_detect(emd_name, regex("??.?")) ~ ""
          , stringr::str_detect(emd_name, regex("??.?")) ~ ""
          , stringr::str_detect(emd_name, regex("??.?")) ~ ""
          , stringr::str_detect(emd_name, regex("??.?")) ~ ""
          , stringr::str_detect(emd_name, regex("??.?")) ~ ""
          , stringr::str_detect(emd_name, regex("??.?")) ~ ""
          , TRUE ~ emd_name
        )
        , xOffset = dplyr::case_when(
          emd_name == "???" ~ 1.0
          , emd_name == "???" ~ 0.5
          , emd_name == "???" ~ 1.0
          , emd_name == "???" ~ 0.06
          , emd_name == "???" ~ 0
          , emd_name == "???" ~ 0.1
          , emd_name == "???" ~ -0.01
          , emd_name == "???" ~ -0.01
          , TRUE ~ 0
        )
        , yOffset = dplyr::case_when(
          emd_name == "???" ~ 0.02
          , emd_name == "???" ~ -0.02
          , emd_name == "???" ~ -0.02
          , emd_name == "???" ~ 0.02
          , emd_name == "???" ~ -0.02
          , emd_name == "???" ~ -0.02
          , emd_name == "???" ~ -0.02
          , emd_name == "???" ~ 0.01
          , emd_name == "???" ~ -0.03
          , emd_name == "???" ~ -0.045
          , emd_name == "???" ~ 0.01
          , emd_name == "???" ~ 0.05
          , emd_name == "??" ~ -0.04
          , emd_name == "???" ~ -0.03
          , emd_name == "???" ~ 0.02
          , emd_name == "???" ~ -0.02
          , emd_name == "???" ~ 0.005
          , TRUE ~ 0
        )
      )

    # ???
    # saveImg = paste0("TMP2/Img_078.png")
    saveImg = paste0("TMP2/Img_078_", "2015-2019", "_", key6Info, "_", key5Info, ".png")
    plotSubTitle = paste0("[", key6Info, "] ", key5Info, " ????? ???? ??(", "2015-2019", ")")


    ggplot() +
      theme_bw() +
      coord_fixed(ratio = 1) +
      geom_polygon(data=dataL2, aes(x=long, y=lat, group=group, fill = n)) +
      scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
      geom_path(data=dataL2, aes(x=long, y=lat, group=group), colour='black',size = 0.5) +
      ggh4x::stat_midpoint(data=dataL2, aes(x=long + xOffset, y=lat + yOffset, group= emd_name, label = plotLabel), geom = "text", size = 2) +
      labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "?? : ?", subtitle = NULL) +
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


    # ??? ???
    for (yearInfo in yearList) {

      tmpData = data %>%
        dplyr::group_by(key5, key6, addr1) %>%
        dplyr::summarise(sumVal = n()) %>%
        dplyr::filter(
          key5 == key5Info
          , key6 == key6Info
          , addr1 == "????"
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
            , stringr::str_detect(emd_name, regex("??.?")) ~ ""
            , stringr::str_detect(emd_name, regex("??.?")) ~ ""
            , stringr::str_detect(emd_name, regex("??.?")) ~ ""
            , stringr::str_detect(emd_name, regex("??.?")) ~ ""
            , stringr::str_detect(emd_name, regex("??.?")) ~ ""
            , stringr::str_detect(emd_name, regex("??.?")) ~ ""
            , stringr::str_detect(emd_name, regex("??.?")) ~ ""
            , stringr::str_detect(emd_name, regex("??.?")) ~ ""
            , TRUE ~ emd_name
          )
          , xOffset = dplyr::case_when(
            emd_name == "???" ~ 1.0
            , emd_name == "???" ~ 0.5
            , emd_name == "???" ~ 1.0
            , emd_name == "???" ~ 0.06
            , emd_name == "???" ~ 0
            , emd_name == "???" ~ 0.1
            , emd_name == "???" ~ -0.01
            , emd_name == "???" ~ -0.01
            , TRUE ~ 0
          )
          , yOffset = dplyr::case_when(
            emd_name == "???" ~ 0.02
            , emd_name == "???" ~ -0.02
            , emd_name == "???" ~ -0.02
            , emd_name == "???" ~ 0.02
            , emd_name == "???" ~ -0.02
            , emd_name == "???" ~ -0.02
            , emd_name == "???" ~ -0.02
            , emd_name == "???" ~ 0.01
            , emd_name == "???" ~ -0.03
            , emd_name == "???" ~ -0.045
            , emd_name == "???" ~ 0.01
            , emd_name == "???" ~ 0.05
            , emd_name == "??" ~ -0.04
            , emd_name == "???" ~ -0.03
            , emd_name == "???" ~ 0.02
            , emd_name == "???" ~ -0.02
            , emd_name == "???" ~ 0.005
            , TRUE ~ 0
          )
        )

      # ???
      saveImg = paste0("TMP2/Img_079_", yearInfo, "_", key6Info, "_", key5Info, ".png")
      plotSubTitle = paste0("[", key6Info, "] ", key5Info, " ????? ???? ??(", yearInfo, ")")


      ggplot() +
        theme_bw() +
        coord_fixed(ratio = 1) +
        geom_polygon(data=dataL2, aes(x=long, y=lat, group=group, fill = n)) +
        scale_fill_gradientn(colours = c("yellow", "orange", "Brown"), limits = c(0, max(dataL2$n, na.rm = TRUE)), na.value = "white") +
        geom_path(data=dataL2, aes(x=long, y=lat, group=group), colour='black',size = 0.5) +
        ggh4x::stat_midpoint(data=dataL2, aes(x=long + xOffset, y=lat + yOffset, group= emd_name, label = plotLabel), geom = "text", size = 2) +
        labs(title = plotSubTitle, x = NULL, y = NULL, colour = NULL, fill = "?? : ?", subtitle = NULL) +
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
# Purpose : ???? (??, ???)
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================
# ?? ?? <11?? ?? - 100?>
#    (1) ??? ??? ?? ??_??.sqlite ??? ??? ?, ??SmartLEAD? ??? ?(ex.D20514_???.sqlite)
# (2) ?? ???? : 2020.11.9(???) ~ 2020.11.15(???) 23:59 ??
# 1. SQLite? ???? ??_??.sqlite ??????? ??
# 2. dbplyr? ???? ??_??(??)? ??? ?? (ex. D20514_jinoo)
# <???? ??>
#
# ID : ??? ??
# GRADE : ??? ??
# NAME : ??? ??
# RESIDENCE : ??? ?? ??
# 3. dbplyr? ???? ? ?? ?? ??? ??(1??)
# ?? ??

# install.packages(c("DBI", "RSQLite", "tidyverse", "dbplyr"))
library(DBI)
library(RSQLite)
library(tidyverse)
library(dbplyr)

getwd()
dir.create("test")
list.dirs()

# ??? ??? ???? ??? ?
con = RSQLite::dbConnect(drv = RSQLite::SQLite(), dbname = "test/D20165255_???.sqlite")

DF = tibble(ID = c("D20165255"), GRADE = c("3"), NAME = c("???"), RESIDENCE = c(" ??? ???"))

DF %>% show()

# connection, ??? dataframe, ??? ?, ???? ??? ??
dplyr::copy_to(con, DF, "D20165255_JooHyuSung", temporary = FALSE)

# ??? ??? ??
dbListTables(con)

dplyr::tbl(con, "D20165255_JooHyuSung") %>%
    collect() %>%
    show()

dbDisconnect(con)

#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? (??, ???)
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
#===============================================================================================

#==========================================
# ????1
#==========================================
sDate = "1945-08-15"

format(as.Date(sDate), "%A")

#==========================================
# ????5
#==========================================
nN = as.numeric(readline("n? ?????"))
nP = as.numeric(readline("p? ?????"))

liSeq = seq(1, nN)
nSum = sum(liSeq ** nP, na.rm = TRUE)


#==========================================
# ????7
#==========================================
library(readr)
library(tidyverse)
library(rapport)
library(ggplot2)
library(scales)
library(pastecs)
library(psych)

# 2? ??
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

# ????
stat.desc(dfData2)[13, ]

# ????
boxplot(dfData2)

# ?????
hist(dfData2)

#==========================================
# ????8
#==========================================
dfData = read.csv(file = "./rpy/pima2.csv")
dfData2 = table(dfData$diabetes)

describe.by(dfData[ , c(3:9)], dfData$diabetes)

# ????
barplot(dfData2)

# ???
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

# ??
aggregate(. ~ diabetes + type, data=dfData6, mean, na.rm=TRUE)

# ????
aggregate(. ~ diabetes + type, data=dfData6, sd, na.rm=TRUE)


#==========================================
# ????9
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

# P?? 0.53??? ???? ???? ?? (? ??? ?? ??? ??)
# ??? ??? ?? (var.equal = TRUE)
fTest = var.test(value ~ key, data = dfData2)
fTest

# P?? 0.063?? ???? ?? (? ??? ??? ??)
tTest = t.test(value ~ key, data = dfData2, var.equal = TRUE)
tTest

#==========================================
# ????10
#==========================================
dfData = readr::read_csv(file = "./rpy/mtcars.csv")
dfData2 = tidyr::gather(dfData[ , c(2, 10)])

# P?? 0.01??? ???? ?? (? ???? ?? ??? ??)
# ??? ??? ?? ?? (var.equal = FALSE)
fTest = var.test(value ~ key, data = dfData2)
fTest

# P?? 0.01 ???? ???? ?? (? ???? ??? ??)
tTest = t.test(value ~ key, data = dfData2, var.equal = FALSE)
tTest


#==========================================
# ????11
#==========================================
dfData = readr::read_csv(file = "./rpy/computer.csv")

dfData2 = dfData %>%
  dplyr::select(erp, myct, mmax, cach, chmin, chmax, prpe)

# ???
pairs(dfData2)

# ???? ??
cor(dfData2)

# ?? ?? ????
modelFit = lm(erp ~ myct + mmax + cach + chmin + chmax, data = dfData2)
summary(modelFit)

#==========================================
# ????12
#==========================================
dfData = readr::read_csv(file = "./rpy/mtcars.csv")

modelFit = lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, data = dfData)

modelStepAic = MASS::stepAIC(modelFit, direction = "both")

summary(modelStepAic)


#==========================================
# ????13
#==========================================
dfData = readr::read_csv(file = "./rpy/bateriasoap.csv")

modelFit = aov(BacterialCounts ~ Method, data = dfData)

modelTest = TukeyHSD(modelFit)
modelTest

#==========================================
# ????14
#==========================================
dfData = readr::read_csv(file = "./rpy/downloading.csv")

modelFit = aov(`Time(Sec)` ~ TimeofDay, data = dfData)

modelTest = TukeyHSD(modelFit)
modelTest

#===============================================================================================
# Routine : Main R program
#
# Purpose : ???? (??, ???)
#
# Author : ??
#
# Revisions: V1.0 May 28, 2020 First release (MS. ??)
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

# 4? ?/?? ??
dataL1 = data %>%
  dplyr::mutate(diff = typeA - typeB)

# 4? ?/? ??
mean(dataL1$diff, na.rm = TRUE)

# ??? ??? ??? ??? ??? ??
hist(dataL1$diff, xlab = "Diff")

# ??? ??
# P?? 0.0002541??? ???? ?? (?? ??? ??? ??)
shapiro.test(dataL1$diff)

qqnorm(dataL1$diff)
qqline(dataL1$diff)

# F ??? ? T ???
dataL2 = data %>%
  tidyr::gather()

# P?? 0.003173??? ???? ?? (? ??? ?? ??? ??)
# ??? ??? ?? ?? (var.equal = FALSE)
fTest = var.test(value ~ key, data = dataL2)
fTest

plot(fTest) +
  xlim(0, 5) #+
  # ggsave(filename = paste(globalVar$figConfig, "fTest.png", sep = "/"), width = 10, height = 6, dpi = 600)

# P?? 0.1666?? ???? ???? ?? (? ??? ??? ??)
tTest = t.test(value ~ key, data = dataL2, var.equal = FALSE)
tTest

plot(tTest) +
  xlim(-5, 5) # +
  # ggsave(filename = paste(globalVar$figConfig, "tTest.png", sep = "/"), width = 10, height = 6, dpi = 600)