#=====================================
# Init Confiure
#=====================================
rm(list = ls())

prjName = "knowledgeIn"

source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")
# source(here::here("./src", "InitConfig.R"), encoding = "UTF-8")

showtext::showtext_opts(dpi = 100)
showtext::showtext.auto()

#===============================================================================================
# Routine : Main R program
#
# Purpose : NAVER 지식iN
#
# Author : MS. Sang-Ho Lee
#
# Revisions: V1.0 February 21, 2020 First release (MS. Sang-Ho Lee)
#===============================================================================================

# Set Option
options(digits = 5)
memory.limit(size = 9999999999999)

#
xAxis = seq(0, 10)
yAxis = seq(0, 10)
yAxis2 = seq(0, 10) + 2
yAxis3 = seq(0, 10) + 4
yAxis4 = seq(0, 10) + 6
yAxis5 = seq(0, 10) + 8

plot(xAxis, yAxis, xlim = c(0, 10), ylim = c(0, 20))
points(xAxis, yAxis2, col = "red")
points(xAxis, yAxis3, col = "blue")
points(xAxis, yAxis4, col = "green")
points(xAxis, yAxis5, col = "orange")

#
A = matrix(1:20, 5, 4)

#
# 라이브러리 읽기
library(tidyverse)

mpg %>%
  dplyr::filter(year == 2008) %>%
  dplyr::summarise(number = n())

mpg %>%
  dplyr::filter(class == "compact") %>%
  dplyr::group_by(manufacturer) %>%
  dplyr::summarise(number = n()) %>%
  dplyr::arrange(desc(number))

#
# 라이브러리 읽기
library(tidyverse)

st = data.frame(state.x77)

colnames(st) = c("popul", "income", "illit", "lifeExp", "murder", "hsGrad", "frost", "area")

# 문제1에 대한 답변
st %>%
  tibble::rownames_to_column() %>%
  dplyr::mutate(maxLifeExp = max(lifeExp, na.rm = T)) %>%
  dplyr::filter(lifeExp == maxLifeExp)

mt = data.frame(mtcars)

#  문제 2에 대한 답변
mt %>%
  tibble::rownames_to_column() %>%
  dplyr::mutate(maxMpg = max(mpg, na.rm = T)) %>%
  dplyr::filter(mpg == maxMpg)

# 문제 3에 대한 답변
mt %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(gear == 4) %>%
  dplyr::mutate(minMpg = min(mpg, na.rm = T)) %>%
  dplyr::filter(mpg == minMpg)


air = data.frame(airquality)

# 문제 4에 대한 답변
air %>%
  dplyr::mutate(maxTemp = max(Temp, na.rm = T)) %>%
  dplyr::filter(Temp == maxTemp) %>%
  dplyr::select(Month, Day)

# 문제 5에 대한 답변
air %>%
  dplyr::group_by(Month) %>%
  dplyr::summarise(maxWind = max(Wind, na.rm = T)) %>%
  dplyr::filter(Month == 6)

# 문제 6에 대한 답변
air %>%
  dplyr::group_by(Month) %>%
  dplyr::summarise(meanOzone = mean(Ozone, na.rm = T)) %>%
  dplyr::filter(Month == 5)


# 문제 7에 대한 답변
air %>%
  dplyr::filter(Ozone > 100) %>%
  dplyr::summarise(number = n())


sw = data.frame(swiss)

# 문제 8에 대한 답변
sw %>%
  tibble::rownames_to_column() %>%
  dplyr::mutate(maxAgri = max(Agriculture, na.rm = T)) %>%
  dplyr::filter(Agriculture == maxAgri)

# 문제 9에 대한 답변
sw %>%
  tibble::rownames_to_column() %>%
  dplyr::arrange(desc(Agriculture))

# 문제 10에 대한 답변
sw %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(Catholic >= 80) %>%
  dplyr::summarise(meanAgri = mean(Agriculture, na.rm = T))

# 문제 11에 대한 답변
sw %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(Examination < 20 && Agriculture < 50) %>%
  dplyr::select(rowname, Agriculture, Examination)

nNrorm = rnorm(n = 100, mean = 0, sd = 1)
hist(nNrorm)


nNrorm = rnorm(n = 100, mean = 0, sd = 10)
hist(nNrorm)

#
class(a)
class(st)

st[[1, 3]]

x = c(9, 0, 6, 7, 2)
x = c(10, 1, 10, 1, 10)
x = c(10, 10, 10, 10, 10)


v1 = 10:100

v1.a = v1[25:30]

length(v1.a)
mean(v1.a, na.rm = T)


x = c(1:10)

boxplot(x)
abline(h = boxplot.stats(x, 0.75)$conf[2], col = "red")


# 1번 답변
x = c(9, 0, 6, 7, 2)

which((x %% 2) == 1)

# 2번 답변
xRef = c(10, 10, 10, 10)

x = c(1, 10, 1, 10)
setequal(x, xRef)

x = c(10, 10, 10, 10)
setequal(x, xRef)


#

a = list("이상혁", "클리드", "칸")
b = list("에포트", "테디", "이상혁")

result = append(a, b)


# 패키지 설치
install.packages("measurements")

# 패키지 읽기
library(measurements)

# 1 파운드를 1 kg으로 환산
conv_unit(1, "lbs", "kg")

#
library(xtable)
library(magrittr)
library(moonBook)
library(snakecase)

options(xtable.floating = "FALSE")
options(xtable.time = "FALSE")

options(ztable.type = "html")
options(ztable.type = "html")

a = mytable(head(iris))

print(xtable(a), type = "html")


str_cars = capture.output(str(iris))
xtable(data.frame(str_cars))


a = diag(3)

a[1, 2] = 2

data <- c(25, -2, 4, -2, 4, 1, 4, 1, 9)
Sigma <- matrix(data = data, nrow = 3, ncol = 3, byrow = FALSE)

t(Sigma) %*% Sigma
cov(data)

b = diag(3)


Sigma %*% b
a[]

diag(X) <- 1

# 행렬 X의 대각 성분을 모두 1로한다.

diag(X) = 1
diag(X) <- c(1, 2)


#

library(dplyr)
library(ggplot2)


airquality %>%
  ggplot(aes(x = Month, y = Temp)) +
  geom_boxplot()

#

# 연도
year = seq(2010, 2015)

# 자살 건수
value = c(30, 40, 50, 60, 65, 70)

# 그래프 결과
plot(year, value)

#

# 패키지 설치
install.packages("gtools")

# 패키지 읽기
library(gtools)

nEven = even(1:100)

which(nEven)


install.packages("dplyr")

library(ggiraphExtra)
library(maps)
library(mapproj)
library(ggplot2)
library(tibble)


crime = rownames_to_column(USArrests, var = "state")
crime$state = tolower(crime$state)
states_map = map_data("state")

crime %>%
  ggChoropleth(aes(fill = Murder, map_id = state), map = states_map)

#


# 변수 초기화 및 설정
d1 = c(8, 8, 9, 10, 10)
d2 = c(10, 5, 3, 12, 21)


# 함수 정의
sum1 = function(d1, d2) {

  result = d1 + d2

  return(result)
}

sum2 = function(d1) {

  result = sum(d1)

  return(result)

}


sum1(d1, d2)
sum2(d1)

#
value = c(1:100)

hist(value)


data = c(2, 3, 16, 23, 14, 12, 4, 1, 2, 0, 0, 0, 6, 28, 31, 14, 4, 8, 2, 5)

stem(data, scale = 2)


data = c(71, 84, 68, 75, 91, 87, 63, 77, 81, 98, 57, 73, 74, 85, 50,
         62, 66, 78, 65, 59, 75, 89, 94, 93, 86, 61, 87, 74, 70, 67)

table(cut(data, 6))

hist(data, breaks = seq(50, 98, 8), freq = F)
lines(density(data), col = "red")


library(dplyr)


midwest %>%
  dplyr::mutate(ratio_child = popadults / poptotal * 100)


getValue = c("경기도", "경기")

setValue = replace(getValue, 1, c("경기"))

x = seq(-3, 3, 0.1)
y = dnorm(x, 0, 1)

plot(x, y, type = 'l', ylim = c(0, max(y)), ylab = "", main = "Probability Density Function")


x = seq(-pi, pi, 0.01)

plot(x, sin(x), col = "black", type = "l", ylab = "sin (x), cos (x), tan (x)", ylim = c(-1, 1))
lines(x, cos(x), col = "red")
lines(x, tan(x), col = "blue")

x = seq(-1, 1, length.out = 100)
y = x^2

plot(x, x, pch = "*", ylab = expression("x"^2))
lines(x, y, col = "red")


library(lubridate)

interval(
  start = ymd("2018-1-16"),
  end = ymd("2018-1-31")
)

dtStartDate = lubridate::ymd("2017/1/1")
dtEndDate = lubridate::ymd("2018/1/1")

dtDayDiff = lubridate::interval(dtStartDate, dtEndDate)

dtDayDiff / days(1)


iValue = c(1:10)
class(iValue)

nValue = as.numeric(iValue)
class(nValue)


paris(iris[1:4], main = "Title", pch = 20, bg = c("red", "green3", "blue")
[unclass(iris$Species)])


unclass(iris$Species)


library(ggplot2)
library(dplyr)

cars %>%
  ggplot(aes(x = speed, y = dist)) +
  geom_point() +
  scale_x_continuous(breaks = c(5, 6, 7, 9, 10, 15, 20, 25)) +
  scale_y_continuous(breaks = c(0, 50, 65, 75, 150))


# install.packages("primes")
library(primes)


?is_prime

generate_primes(min = 0, max = 100)


aa =
  function(x)
  {
    is_prime_vector(x)
  }

is_prime(0:100)


Is_Prime_Number = function(n) {
  if (n >= 2) {
    x = seq(2, n)
    prime_nums = c()
    for (i in seq(2, n)) {
      if (any(x == i)) {
        prime_nums = c(prime_nums, i)
        x = c(x[(x %% i) != 0], i)
      }
    }
    return(prime_nums)
  } else {
    stop("Input number should be at least 2.")
  }
}

Is_Prime_Number(100)

nValue = seq(1, 10, 0.1)
sample(nValue, 10)


library(ggplot2)
library(dplyr)

diamonds %>%
  dplyr::filter(price >= 10000) %>%
  dplyr::sample_n(10)

install.packages("arules")
library(arules)

load("product_by_user.RData")

trans = as(product_by_user$Product, "transactions")

trans


orders <- data.frame(
  transactionID = sample(1:500, 1000, replace = T),
  item = paste("item", sample(1:50, 1000, replace = T), sep = "")
)


iYear = seq(1993, 2001)
nPersonal = c(13.6, 13.7, 12.5, 11.9, 11.7, 11.5, 11.7, 13.3, 14.2)
nGroup = c(17.6, 18.4, 19.4, 18.8, 20.0, 20.3, 26.9, 21.5, 26.0)

maxY = max(nPersonal, nGroup)
minY = min(nPersonal, nGroup)

plot(iYear, nPersonal, ylim = c(minY, maxY), col = "black", type = "l", xlab = "연도", ylab = "가입자 수 [단위 : 100만]")
points(iYear, nGroup, col = "blue", type = "l")


install.packages("quadprog")
library(quadprog)

# optimal portfolio for investing in 3 stocks# beta_i : ratio of investment, nonnegative and sum to 1
# x: daily return for stocks: 0.002, 0.005, 0.01
# D: variability in the returns (covariance)

A = cbind(rep(1, 3), diag(rep(1, 3)))
D = matrix(c(.01, .002, .002, .002, .01, .002, .002, .002, .01), nrow = 3)
x = c(.002, .005, .01)
b = c(1, 0, 0, 0)

solve.QP(2 * D, x, A, b, meq = 1) # optimal strategy: invest 10.4%, 29.2%, 60.4% for stocks 1,2,3# optimal value is 0.002


x = 156
y = 1178

x2 = 1262
y2 = 69390

xy = 9203

r = (xy - ((x * y) / 20)) / ((x2 - ((x^2) / 20)) * (y2 - ((y^2) / 20)))
r

install.packages("rlang")
library(rlang)


library(dplyr)

bmi = function(height, weight) {
  round(weight / (height / 100)^2, digits = 2)
}

data = data.frame(
  height = c(179, 161, 165, 185, 158)
  , weight = c(75, 72, 60, 98, 65)

)
data

data %>%
  dplyr::mutate(bmi = bmi(height, weight)
    , result = case_when(
      bmi > 25 ~ "비만"
      , TRUE ~ "정상"
    )
  )

library(plyr)

iData = sample(1:6, 7, replace = TRUE)
iData

fData = factor(iData)
fData

rsData = plyr::mapvalues(fData, from = c(1:6), to = c("one", "two", "three", "four", "five", "six"))
rsData

table(rsData)


library(tidyverse)
library(readxl)
library(scales)

blank_theme = theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 14, face = "bold")
  )

df_life = read_excel("INPUT/sample_data.xlsx", sheet = "생활상태")

dplyr::glimpse(df_life)
dplyr::tbl_df(df_life)


# 성별에 대한 빈도
dfData = df_life %>%
  dplyr::group_by(Gender) %>%
  dplyr::summarise(nNumber = n()) %>%
  dplyr::mutate(type = case_when(
    Gender == 1 ~ "남자"
    , Gender == 2 ~ "여자"
    , TRUE ~ "null"
  ))

ggplot(data = dfData, aes(x = "", y = nNumber, fill = type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  blank_theme +
  theme(axis.text.x = element_blank()) +
  geom_text(aes(y = nNumber / 2.0 + c(0, cumsum(nNumber)[-length(nNumber)]),
                label = percent(nNumber / 100)), size = 5)

# 연령에 대한 빈도
df_life %>%
  dplyr::group_by(Age) %>%
  dplyr::summarise(nNumber = n()) %>%
  dplyr::mutate(type = case_when(
    Age == 1 ~ "1-40세 이하"
    , Age == 2 ~ "41-64세"
    , Age == 3 ~ "65세 이상"
    , TRUE ~ "null"
  )) %>%
  ggplot(aes(x = "", y = nNumber, fill = type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  blank_theme +
  theme(axis.text.x = element_blank()) +
  geom_text(aes(y = nNumber / 3 + c(0, cumsum(nNumber)[-length(nNumber)]),
                label = percent(nNumber / 100)), size = 5)


# 학력에 대한 빈도
df_life %>%
  dplyr::group_by(Study) %>%
  dplyr::summarise(nNumber = n()) %>%
  dplyr::mutate(type = case_when(
    Study == 1 ~ "고졸이하"
    , Study == 2 ~ "전문대졸"
    , Study == 3 ~ "대졸"
    , Study == 4 ~ "대학원졸"
    , TRUE ~ "null"
  )) %>%
  ggplot(aes(x = "", y = nNumber, fill = type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  blank_theme +
  theme(axis.text.x = element_blank()) +
  geom_text(aes(y = nNumber / 4 + c(0, cumsum(nNumber)[-length(nNumber)]),
                label = percent(nNumber / 100)), size = 5)

# 월 소득에 대한 빈도
df_life %>%
  dplyr::group_by(Income) %>%
  dplyr::summarise(nNumber = n()) %>%
  dplyr::mutate(type = case_when(
    Income == 1 ~ "1-200 만원 이하"
    , Income == 2 ~ "200-300 만원"
    , Income == 3 ~ "300-500 만원"
    , Income == 4 ~ "500 만원 이상"
    , TRUE ~ "null"
  )) %>%
  ggplot(aes(x = "", y = nNumber, fill = type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  blank_theme +
  theme(axis.text.x = element_blank()) +
  geom_text(aes(y = nNumber / 4 + c(0, cumsum(nNumber)[-length(nNumber)]),
                label = percent(nNumber / 100)), size = 5)

# 가정생활, 직업생활, 문화생활 만족도에 대한 평균과 표준편차
df_life %>%
  dplyr::summarise(meanHome = mean(Home_life, na.rm = T)
    , meanJob = mean(Job_life, na.rm = T)
    , meanCulture = mean(Culture_life, na.rm = T)
    , sdHome = sd(Home_life, na.rm = T)
    , sdJob = sd(Job_life, na.rm = T)
    , sdCulture = sd(Culture_life, na.rm = T)
  )

# Boxplot 그리기
df_life %>%
  tidyr::gather(-c(1:5), key = "key", value = "value") %>%
  ggplot(aes(x = key, y = value, fill = key)) +
  geom_boxplot(alpha = 0.7) +
  scale_x_discrete(name = "Type") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 11),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent") +
  labs(fill = "Type")


# 가정생활, 직업생활, 문화생활 만족도의 평균값을 전반적인 만족도 추가
df_life2 = df_life %>%
  dplyr::mutate(meanHome = mean(Home_life, na.rm = T)
    , meanJob = mean(Job_life, na.rm = T)
    , meanCulture = mean(Culture_life, na.rm = T)
  )
df_life2


Sys.which("R")
options(error)
options(show.error.messages = TRUE)
options(showErrorCalls = TRUE)


library(pracma)
library(numbers)

# 최대공약수
pracma::gcd(1, 24)

# 최소공배수
pracma::Lcm(12, 24)

# 3의 배수
seq(1, 100, 2)


library(numbers)

# 1번 답 (1)
iNumber = 0
iSum = 0
for (iCount in 1:100) {
  if (iCount %% 3 == 0) {
    iNumber = iNumber + 1
    iSum = iSum + iCount
  }
}

cat(iNumber, " ", iSum)

# 1번 답 (2)
nData = which(seq(1, 100, 1) %% 3 == 0)
cat(length(nData), " ", sum(nData, na.rm = T))

# 2번 답
for (iCount in 101:200) {
  if ((iCount %% 3 == 0) && (iCount %% 4 == 0)) {
    cat(iCount, "\n")
  }
}

# 3번 답
numbers::divisors(24)

# 4번 답
iSum = 1
for (iCount in 1:10) {
  iSum = iSum * iCount
}

cat(iSum)

# 5번 답
for (iCount in 1:100) {
  if (iCount %% 3 == 0) {
    rsResult = "*"
  } else {
    rsResult = iCount
  }

  cat(rsResult, "\n")
}

x <- rnorm(20, 0, 1)
y <- rnorm(25, 1, 4)


t_test <- function(x, y, delta0, alter) {

  delta0 = 0
  alter = 1

  meanx <- mean(x)
  meany <- mean(y)
  n1 <- length(x)
  n2 <- length(y)
  sdx <- sd(x)
  sdy <- sd(y)

  sp <- sqrt(((n1 - 1) * (sdx^2) + (n2 - 1) * (sdy^2)) / (n1 + n2 - 2))

  if (sdy / sdx > 1 / 2 | sdy / sdx < 2) {
    t <- ((meanx - meany) - delta0) / (sp * sqrt(1 / n1 + 1 / n2))
    df <- n1 + n2 - 2
  } else {
    t <- ((meanx - meany) - delta0) / (sqrt((sdx^2) / n1 + (sdy^2) / n2))
    df <- min(n1 - 1, n2 - 1)
  }

  if (alter == 1) {
    p <- pt(t, df, lower.tail = F)
  } else if (alter == 2) {
    p <- pt(t, df, lower.tail = T)
  } else {
    p <- 2 * pt(abs(t), df, lower.tail = F)
  }

  data <- c(meanx, meany, sdx, sdy, t, NA, df, NA, p, NA)
  table <- matrix(data, ncol = 5, nrow = 2)

  colnames(table) <- c("m", "s.d", "t", "df", "p")
  rownames(table) <- c("A", "B")

  print(table, na.print = "")

}


t_test(x, y, 0, 1)
t_test(x, y, 0, 2)
t_test(x, y, 0, 3)


install.packages("asbio")
library(asbio)

Y1 = rnorm(100, 15, 2)
Y2 = rnorm(100, 18, 3.2)
chi.plot(Y1, Y2)


install.packages("mvoutlier")
library(mvoutlier)

data(humus)
res = chisq.plot(log(humus[, c("Co", "Cu", "Ni")]))
res$outliers # these are the potential outliers


class <- c(rep(1, 40), rep(1, 43), rep(1, 67), rep(1, 50))
class <- c(40, 43, 67, 50)


data = data.frame(


)


library(dplyr)
library(ggplot2)

data = dplyr::tibble(
  key = c(1, 2, 3, 4)
  , value = c(40, 43, 67, 50)
  , name = c("1학년", "2학년", "3학년", "4학년")
)

plot(data$key, data$value)

data %>%
  ggplot2::ggplot(aes(x = key, y = value, colour = name)) +
  ggplot2::geom_point()


data$value


liData = list(
  rep(1, 40)
  , rep(1, 43)
  , rep(1, 67)
  , rep(1, 50)
)

class = c()
for (iCount in 1:length(liData)) {
  iNumber = length(liData[[iCount]])

  class = c(class, iNumber)
  cat(iNumber, "\n")
}

class


library(tidyverse)
library(gapminder)

# 1번 답
CarData = data.frame(mtcars)

CarData %>%
  ggplot(aes(x = disp, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm")

CarData %>%
  arrange(hp, desc(cyl))


# 2번 답
CData = data.frame(gapminder)

CData %>%
  dplyr::filter(continent == "Asia") %>%
  dplyr::filter(year == 2007) %>%
  dplyr::mutate(maxLifeExp = max(lifeExp, na.rm = T)) %>%
  dplyr::filter(lifeExp == maxLifeExp)

CData %>%
  dplyr::filter(continent == "Europe") %>%
  dplyr::filter(year == 2007) %>%
  dplyr::mutate(maxLifeExp = max(lifeExp, na.rm = T)) %>%
  dplyr::filter(lifeExp == maxLifeExp)

CData %>%
  dplyr::filter(year == 1997) %>%
  dplyr::mutate(maxLifeExp = max(lifeExp, na.rm = T)) %>%
  dplyr::filter(lifeExp == maxLifeExp)

CData %>%
  dplyr::filter(continent == "Asia") %>%
  dplyr::filter(year == 1992) %>%
  dplyr::summarise(meanLifeExp = mean(lifeExp, na.rm = T))

CData %>%
  dplyr::filter(year == 2007) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm")


dataL1 = CData %>%
  dplyr::filter(year == 2007)
lmFit = lm(dataL1$lifeExp ~ dataL1$gdpPercap)
lmFit


#
library(tidyverse)

data = read.csv("INPUT/출산율.csv")

# dplyr::glimpse(data)

# 연도별 평균 출산율
data %>%
  dplyr::group_by(연도) %>%
  dplyr::summarise(nMeanBirthRate = mean(출산율, na.rm = TRUE))


# 지역별 평균 출산율
dataL1 = data %>%
  dplyr::group_by(지역) %>%
  dplyr::summarise(nMeanBirthRate = mean(출산율, na.rm = TRUE))

dataL1

# 지역별 평균 출산율에서 출산율 최대/최소 지역
dataL1 %>%
  dplyr::filter(nMeanBirthRate == max(nMeanBirthRate, na.rm = TRUE)
                  | nMeanBirthRate == min(nMeanBirthRate, na.rm = TRUE))


#
library(tidyverse)

data = read.csv("INPUT/진료비.csv")

# 성별에 따른 평균 진료비
data %>%
  dplyr::group_by(성별) %>%
  dplyr::summarise(nMeanCost = mean(진료비, na.rm = TRUE))

# 지역구에 따른 평균 내원환자수
data %>%
  dplyr::group_by(지역구) %>%
  dplyr::summarise(nMeanPatient = mean(내원환자수, na.rm = TRUE))

#
library(tidyverse)
library(readxl)

data = read_excel("INPUT/satis.xlsx")

# 성별에 대한 빈도
dataL1 = data %>%
  dplyr::mutate(
    sGender = case_when(
      x1 == 1 ~ "남성"
      , x1 == 2 ~ "여성"
      , TRUE ~ "null"
    )
    , sUniv = case_when(
      x2 == 1 ~ "S대"
      , x2 == 2 ~ "Y대"
      , x2 == 3 ~ "K대"
      , x2 == 4 ~ "A대"
      , x2 == 5 ~ "B대"
      , x2 == 6 ~ "H대"
      , x2 == 7 ~ "M대"
      , TRUE ~ "null"
    )
    , sGrade = case_when(
      x3 == 1 ~ "1학년"
      , x3 == 2 ~ "2학년"
      , x3 == 3 ~ "3학년"
      , x3 == 4 ~ "4학년"
      , TRUE ~ "null"
    )
    , sPeriod = case_when(
      x4 == 1 ~ "5년 미만"
      , x4 == 2 ~ "5~10년"
      , x4 == 3 ~ "10~15년"
      , x4 == 4 ~ "15~20년"
      , x4 == 5 ~ "20년 이상"
      , TRUE ~ "null"
    )
    , sLifeStyle = case_when(
      x5 == 1 ~ "통합"
      , x5 == 2 ~ "하숙 및 자취"
      , x5 == 3 ~ "학교 기숙사"
      , x5 == 4 ~ "기타"
      , TRUE ~ "null"
    )
    , sSatType = case_when(
      x6 == 1 ~ "전혀 만족하지 않는다"
      , x6 == 2 ~ "만족하지 않는다"
      , x6 == 3 ~ "보통이다"
      , x6 == 4 ~ "만족한다"
      , x6 == 5 ~ "매우 만족한다"
      , TRUE ~ "null"
    )
  )


dplyr::glimpse(dataL1)

# 성별에 따른 대학생활의 전반적 만족도의 차이를 검증하시오.
dataL1 %>%
  dplyr::group_by(sGender) %>%
  dplyr::summarise(nSatType = mean(x6, na.rm = TRUE))

# 소속대학별 대학생활의 전반적 만족도의 차이를 검증하시오.
dataL1 %>%
  dplyr::group_by(sUniv) %>%
  dplyr::summarise(nSatType = mean(x6, na.rm = TRUE))

# 학년별 대학생활의 전반적 만족도의 차이를 검증하시오.
dataL1 %>%
  dplyr::group_by(sGrade) %>%
  dplyr::summarise(nSatType = mean(x6, na.rm = TRUE))

# 대학생활의 만족 정도에 대한 각각의 상관관계를 구하고
# 각 변수간에서 상관관계(0.4이상)가 있는 변수를 구하시오
dataL1 %>%
  dplyr::select(x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) %>%
  cor %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  dplyr::mutate(
    sRowName = case_when(
      rowname == "x6" ~ "대학생활의 전반적 만족 정도"
      , rowname == "x7" ~ "지역문화의 만족도"
      , rowname == "x8" ~ "통학수단의 만족도"
      , rowname == "x9" ~ "시내 교통의 만족도"
      , rowname == "x10" ~ "소속대학의 만족도"
      , rowname == "x11" ~ "전공의 만족도"
      , rowname == "x12" ~ "물가의 만족도"
      , rowname == "x13" ~ "전세(월세)등의 만족도"
      , rowname == "x14" ~ "치안의 만족도"
      , rowname == "x15" ~ "먹을거리, 음식 만족도"
      , TRUE ~ "null"
    )
  ) %>%
  dplyr::select(sRowName, x6) %>%
  dplyr::filter(x6 >= 0.4)

#
library(alr3)

lmFit = lm(Fertility ~ ., data = swiss)

# 분산팽창지수
vif(lmFit)

# 공차한계
1.0 / vif(lmFit)

#
VX = 83416.66666667

# 소수점 2자리까지 표시
round(VX, 2)

nValue = c(20160809103050)
nValue

# 숫자로 표시
format(nValue, scientific = FALSE)

# RData 저장
save(mtcars, file = "mtcars.RData")

# RData 읽기
load("mtcars.RData")

# RData를 csv로 변환
write.csv(mtcars, file = "mtcars.csv")


#

# 현재 작업 디렉터리 읽기
getwd()

# 현재 작업 디렉터리 셋팅
setwd(getwd())

# 파일 읽기 (현재 작업 디렉터리 하위에 위치)
df = read.csv(paste0(getwd(), "/", "파일이름"), header = TRUE)

#
library(haven)

haven01 <- read_sav("6areaeng.sav")

saveRDS(dataL1, file = "my_data.rds")


#====================================================================
#   내 PC
#====================================================================

dfDataA = data.frame(
  stretch = c(46, 54, 48, 50, 44, 42, 52)
  , distance = c(183, 217, 189, 208, 178, 150, 249)
)

dfDataB = data.frame(
  stretch = c(25, 45, 35, 40, 55, 60)
  , distance = c(71, 196, 127, 187, 249, 291)
)

minStretch = min(dfDataA$stretch, dfDataB$stretch)
maxStretch = max(dfDataA$stretch, dfDataB$stretch)

minDistance = min(dfDataA$distance, dfDataB$distance)
maxDistance = max(dfDataA$distance, dfDataB$distance)

plot(dfDataA$stretch, dfDataA$distance, xlab = "stretch", ylab = "distance", pch = "a", xlim = c(minStretch, maxStretch), ylim = c(minDistance, maxDistance))
points(dfDataB$stretch, dfDataB$distance, pch = "b")


var1 <- c(1:5)
var2 <- c(1, 2, 3)
var3 <- c("문자", "숫자")
var4 <- c("1", "5")


library(GISTools)
data(georgia)

#
# auto.shading
#
# Compute a rate for mapping and put it in a variable - re-scale to avoid
# a profusion of zeros
incomes <- georgia$MedInc / 1000
# Create a shading scheme
shades <- auto.shading(incomes, n = 6, cutter = rangeCuts, cols = brewer.pal(6, "Greens"))
# Draw a map based on this scheme
choropleth(georgia, incomes, shades)
# Add a legend based on this scheme
choro.legend(-82, 34.87, shades, fmt = "%4.1f", title = "Median Income (1000$'s)")
# Add a title to the map
title("Median Income in Georgia (1990)")
# ... and a little more explanatory text
text(-85.3, 30.4, "Source: US Census")
# ... and a north-point arrow
north.arrow(-80.9, 30.4, 0.1)
# and draw a box around it
box(which = "outer")


library(dplyr)
library(tibble)

st = data.frame(state.x77)

# 11번 답변
st %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(rowname == "Texas") %>%
  dplyr::select(rowname, Area)

# 12번 답변
st %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(rowname == "Ohio") %>%
  dplyr::select(rowname, Population, Income)

# 13번 답변
st %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(Population >= 5000) %>%
  dplyr::select(rowname)


# 14번 답변
st %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(Income >= 4500) %>%
  dplyr::select(rowname, Population, Income, Area)


# 15번 답변
st %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(Income >= 4500) %>%
  dplyr::summarise(count = n())


library(ggplot2)
library(dplyr)


carData = data.frame(mtcars)

carData %>%
  ggplot(aes(x = disp, y = mpg)) +
  geom_point()


carData %>%
  arrange(desc(hp), desc(cyl))


data = data.frame(
  x = c(1360, 1940, 1750, 1550, 1790, 1750, 2230, 1600, 1450, 1870, 2210, 1480)
  , y = c(278.5, 375.7, 339.5, 329.8, 295.6, 310.3, 460.5, 305.2, 288.6, 365.7, 425.3, 268.5)
)

xAxis = data$x
yAxis = data$y


# 상관계수
sumX = sum(xAxis, na.rm = T)
sumY = sum(yAxis, na.rm = T)
sumXY = sum(xAxis * yAxis, na.rm = T)
sumX2 = sum(xAxis^2, na.rm = T)
sumY2 = sum(yAxis^2, na.rm = T)

iNumber = length(xAxis)

step1 = (iNumber * sumXY) - (sumX * sumY)
step2 = (iNumber * sumX2) - (sumX * sumX)
step3 = (iNumber * sumY2) - (sumY * sumY)
step4 = sqrt(step2 * step3)

rsCor = step1 / step4
rsCor

# 공분산
meanX = mean(xAxis, na.rm = T)
meanY = mean(yAxis, na.rm = T)

anomalyX = xAxis - meanX
anomalyY = yAxis - meanY

rsCov = sum(anomalyX * anomalyY, na.rm = T) / (iNumber - 1)
rsCov


data = data.frame(
  id = seq(1, 20, 1)
  , age = c("10대", "10대", "20대", "20대", "10대", "20대", "20대", "30대", "30대", "10대"
    , "10대", "20대", "10대", "20대", "30대", "30대", "20대", "20대", "30대", "10대")
  , drink = c("A", "D", "D", "C", "D", "B", "B", "B", "C", "A"
    , "D", "D", "C", "D", "B", "A", "C", "C", "A", "B")
)
data

table(data$age)
table(data$age, data$drink)


# 현재 작업환경 경로 확인
getwd()

# 현재 작업환경 경로 설정
setwd("/Users/^^/Desktop")

# 현재 함수에 대한 옵션값 확인
read.csv

# 현재 함수에 대한 도움말 기능
?read.csv


library(tidyverse)

data = read_delim(file = "INPUT/zipIncome.csv", delim = ',')

v = data %>%
  select(MeanEducation, MeanHouseholdIncome) #pick the variable

# set up cut-off values
breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20)

# specify interval/bin labels
tags = c("[0-2)", "[2-4)", "[4-6)", "[6-8)", "[8-10)", "[10-12)", "[12-14)", "[14-16)", "[16-18)", "[18-20)")

# bucketing values into bins
group_tags = cut(v$MeanEducation, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = tags)

# inspect bins
summary(group_tags)

ggplot(data = as_tibble(group_tags), mapping = aes(x = value)) +
  geom_bar(fill = "bisque", color = "white", alpha = 0.7) +
  stat_count(geom = "text", aes(label = sprintf("%.4f", ..count.. / length(group_tags))), vjust = -0.5) +
  labs(x = 'mean education per house') +
  theme_minimal()


library(tidyverse)

df_raw <- data.frame(var1 = c(1, 2, 1), var2 = c(2, 3, 2))

df_raw %>%
  dplyr::rename(v2 = var2)

mpg %>%
  dplyr::mutate(total = (cty + hwy) / 2) %>%
  dplyr::select(total)


# 데이터 (data)에서 만족도 (sat)에 대한 평균 및 분산은 다음과 같다.
library(tidyverse)

data %>%
  dplyr::summarise(meanSat = mean(sat, na.rm = T)
    , varSat = var(sat, na.rm = T)
  )


library(ggplot2)

usedcars

graphics::text

curve(dnorm(x, mean = 0, sd = 1), from = -4, to = 4)


#========================================================
# 4a 답변
#========================================================
zValue = qnorm(0.05) # 5 %
zValue

# 저변 직선 부의 좌표
x = seq(-4, zValue, length = 50) # x 값을 50 분할
y = rep(0, 50) # y 좌표는 모두 0

# 확률 밀도 곡선의 좌표
revx = rev(x) # x 좌표를 반대 방향으로 정렬
dx = dnorm(revx) # x 좌표에 해당하는 곡선의 값 (y 좌표)
# 직선 부와 곡선 부의 좌표를 합병
xvals = c(x, revx)
yvals = c(y, dx)

# 표준 정규 분포를 그려 다각형을 채우기 추가
curve(dnorm(x, mean = 0, sd = 1), -4, 4)
polygon(xvals, yvals, col = "gray") # 왼쪽 2.5 % 기각 영역 채우기

# 오른쪽 5 % 영역
xvals = c(seq(4, -zValue, length = 50), seq(-zValue, 4, length = 50))
yvals = c(rep(0, 50), dnorm(seq(-zValue, 4, length = 50), mean = 0, sd = 1))
polygon(xvals, yvals, col = "gray")
text(-3, 0.1, "5 %")
text(3, 0.1, "5 %")

#========================================================
# 4b 답변
#========================================================
zValue = qnorm(0.025) # 2.5 %
zValue

# 저변 직선 부의 좌표
x = seq(-4, zValue, length = 50) # x 값을 50 분할
y = rep(0, 50) # y 좌표는 모두 0

# 확률 밀도 곡선의 좌표
revx = rev(x) # x 좌표를 반대 방향으로 정렬
dx = dnorm(revx) # x 좌표에 해당하는 곡선의 값 (y 좌표)

# 직선 부와 곡선 부의 좌표를 합병
xvals = c(x, revx)
yvals = c(y, dx)

# 표준 정규 분포를 그려 다각형을 채우기 추가
curve(dnorm(x, mean = 0, sd = 1), -4, 4)
polygon(xvals, yvals, col = "gray") # 왼쪽 2.5 % 기각 영역 채우기

# 오른쪽 2.5 % 영역
xvals = c(seq(4, -zValue, length = 50), seq(-zValue, 4, length = 50))
yvals = c(rep(0, 50), dnorm(seq(-zValue, 4, length = 50), mean = 0, sd = 1))
polygon(xvals, yvals, col = "gray")
text(-3, 0.1, "2.5 %")
text(3, 0.1, "2.5 %")


iris$Species

unclass(iris$Species)

library(tidyverse)


x = c("a", "b", "b", "b", "c", "c")

table(x)

y = table(x)

sort(y)

tData = sort(y, decreasing = T)
tData

tData[order(as.factor(names(tData)))]


library(dplyr)

bmi = function(height, weight) {
  round(weight / (height / 100)^2, digits = 2)
}

data = data.frame(
  height = c(250, 180, 165, 185, 130)
  , weight = c(75, 60, 60, 98, 95)

)
data


data %>%
  dplyr::mutate(bmi = bmi(height, weight)
    , result = case_when(
      bmi <= 18.5 ~ "저체중"
      , 18.5 < bmi & bmi <= 24.9 ~ "정상"
      , 25 <= bmi & bmi <= 29.9 ~ "과체중"
      , 30 <= bmi ~ "비만"
      , TRUE ~ "NULL"
    )
  )


dfData = data.frame(
  a = rnorm(10),
  b <- rnorm(10)
)

dfData


library(RmecabKo)

install_mecab("c:/mecab")

x = ('반갑습니다. 오늘 정말 날씨가 좋습니다. 전국공공노조협의회')
pos(x)


data = data.frame(
  height = c(100, 150, 200, 250, 300)
  , weight = c(80, 60, 70, 80, 50)

)

xAxis = data$height
yAxis = data$weight

lmFit = lm(yAxis ~ xAxis)
nPred = predict(lmFit, data, interval = "prediction")

# 예측구간
nFit = nPred[, 1]

# 하단 신뢰구간
nLwr = nPred[, 2]

# 상단 신뢰구간
nUpr = nPred[, 3]

plot(xAxis, yAxis, ylim = c(0, 150))
abline(lmFit, lwd = 2, col = "blue")
lines(xAxis, nLwr, col = "red")
lines(xAxis, nUpr, col = "red")


library(dplyr)


data = c(16, 25, 32, 15, 2, 1)
data

nIndex = which(data == max(data, na.rm = T))
data[nIndex] = 20
data

data = c(10, 20, 30, NA)
data

mean(data, na.rm = T)

#

install.packages("rattle.data")
library(rattle.data)

rattle.data::weather$Cloud3pm

#
# install.packages("DBI")
# install.packages("RMySQL")
library(DBI)
library(RMySQL)

sqlType = dbDriver("MySQL")

dbCon = dbConnect(sqlType, dbname = "test", user = "root", password = "root")

result = dbGetQuery(dbCon, "select * from employees")

head(result)

dbDisconnect(dbCon)

#
data = data.frame(
  addr = c("서울특별시 금천구 가산동"
    , "서울특별시 금천구 가산1동"
    , "서울특별시 금천구 가산1"
    , "서울특별시 금천구 가산"
  )

)

nRowCount = grep("*동$", data$addr)

grepl$addr[1]
aa = gregexpr("\\s", as.character(data$addr[1]))

names(aa)

# 1 5
# 7 9
# 11 13
nchar(as.character(data$addr[1]))

require(stringi)
require(stringr)

stri_length(data$addr[1])

as.character(data$addr[1])

string

gsub("*동$", data$addr[nRowCount])

data$addr[nRowCount]

match("*동$", data$addr)


#

install.packages("ggplot2")

library(ggplot2)

qplot


#
library(tidyverse)

data = data.frame(
  sDate = c("2017-01-03", "2017-01-04", "2017-01-05")
  , nValue = c(149.32, 154.73, 156.60)
)

dataL1 = data %>%
  dplyr::mutate(dtDate = as.Date(sDate, "%Y-%m-%d"))

dplyr::glimpse(dataL1)

lmFit = lm(nValue ~ dtDate, data = dataL1)

plot(dataL1$dtDate, dataL1$nValue)
abline(lmFit, col = "red")


#
install.packages("openxlsx")

library(openxlsx)

data = read.xlsx("INPUT/his.xlsx")

hist(data$medal, ylim = c(0, 300))

#
library(tidyverse)

data = data.frame(
  sRowNames = c("X2008", "X2009", "X2010")
)

data

dataL1 = data %>%
  dplyr::mutate(year = str_replace(sRowNames, "X", ""))

dataL1


#
library(tidyverse)

data = data.frame(
  sLiist = c("함부르크 대학교", "함부르크 대학교", "함부르크 대학교", "동경외국어대학교", "동경외국어대학교")
)

data

data %>%
  group_by(sLiist) %>%
  summarise(iNumber = n())

#


# 워드클라우드
names(data_cnt)

# 단어들의 빈도
freq = data_cnt

# 글자의 크기
scale = c(5, 1)

# 90도로 회전된 각도로 출력되는 단어의 비율
rot.per = 0.5

# 출력될 단어의 초소 빈도
min.freq = 7

# TRUE이면 랜덤으로 단어 출력, FALSE이면 빈도수가 큰 단어일수록 중앙에 배치
random.order = F

# TRUE이면 단어색은 랜덤순으로 정해지고, FALSE이면 빈도순으로 정해짐
random.color = T

# 가장 작은 빈도부터 큰 빈도까지의 단어색
colors = palete

#
data = data.frame(
  nAge = c(20, 23, 25)
  , nHeight = c(170, 180, 190)
  , nValue = c(149.32, 154.73, 156.60)
)

lmFit = lm(nValue ~ nAge + nHeight, data = data)
lmFit

#
nValue = rbinom(c(1:60000), size = 60000, prob = 0.2)
hist(nValue)

summary(nValue)

# nValue 변수에 대한 최대값이 12433이기 때문에 발생 확률은 0 %
pbinom(12589, size = 60000, prob = 0.2, lower.tail = F)

#
library(tidyverse)

data = data.frame(
  sDate = c("2018-11-01", "2018-11-02", "2019-12-01", "2019-12-02", "2019-12-03")
  , nValue = c("중국", "미국", "중국", "미국", "중국")
)

dataL1 = data %>%
  dplyr::mutate(dtDate = as.Date(sDate, "%Y-%m-%d")
    , sYear = format(dtDate, "%Y")
    , sMonth = format(dtDate, "%m")
    , sDay = format(dtDate, "%d")
  )

# 월별, 국가별에 따른 빈도
dataL2 = dataL1 %>%
  group_by(sYear, sMonth, nValue) %>%
  summarise(iNumber = n())

dataL2

# 연도별, 국가별에 따른 빈도
dataL3 = dataL1 %>%
  group_by(sYear, nValue) %>%
  summarise(iNumber = n())

dataL3

#
nValue = rbinom(c(1:60000), size = 60000, prob = 0.2)


#
library(ggplot2)

diamonds %>%
  ggplot()

#
library(openxlsx)

data = read.xlsx("INPUT/satis.xlsx")


#
library(stringr)

veSeq = seq(1, 1000, 1)

vTmpList = gsub("3|6|9", "짝", veSeq)
vList = gsub("0|1|2|4|5|7|8", "", vTmpList)

vList

#
veList = seq(600, 2130, 15)
veList

veConvList = floor(veList / 100) * 100
veConvList

#
data = data.frame(
  game = c(1, 2, 3)
  , result = c(3, 3, 3)
  , audience = c(13262, 6052, 3843)
)

plot(data$result, data$audience
  , main = "메달과 엘리트 선수 증가수의 산점도"
  , xlab = "result", ylab = "audience")

#
plot(iris$Species, iris$Sepal.Width
  , xlab = "", ylab = "Width")

#
getwd()
setwd(getwd())

#
library(plyr)

data = c(1, 1, 1, 2, 2)

faData = factor(data)

rsData = plyr::mapvalues(faData, from = c(1:2), c("사과", "바나나"))

table(rsData)


#
library(ggplot2)

qplot

#
library(tidyverse)
library(nycflights13)

flights

#
library(dplyr)

mtcars %>%
  dplyr::select(mpg)

#
library(jsonlite)
library(data.table)

jsData = fread("INPUT/News_Category_Dataset_v2.json", header = FALSE, sep = "]")

dfData = data.frame()

# for (iCount in dim(jsData)[1]) {
for (iCount in 1:5) {
  sRowData = paste0("[", jsData[iCount], "]")

  dfRowData = fromJSON(sRowData)

  dfData = dplyr::bind_rows(dfData, dfRowData)
}

#
library(readtext)

data = readtext("INPUT/Test.txt", text_field = "texts")

data

#
nn = 49; dt = 0.1

u = rnorm(1:51)

u[1] = 1

# u[1.5]=u[1]+1/2*dt*(u[1]+1)

u[2] = u[1] + dt * (u[1] + 1 / 2 * dt * (u[1] + 1) + 1)

for (i in 1:(nn)) {

  u[i + 2] = u[i] + 2 * dt * (u[i + 1] + 1)

  print(u[i + 2])

}

#
dfData = data.frame(
  sDate = c("2019-01-03", "2019-01-05")
  , nValue = c(210, 219.3)
)

dfDataL1 = dfData %>%
  dplyr::mutate(dtDate = as.Date(sDate, "%Y-%m-%d"))

plot(dfDataL1$dtDate, dfDataL1$nValue)

#
xminusxbar = matrix(0, 150, 4)
ma = matrix(0, 150, 1)

for (i in 1:nrow(xminusxbar)) {
  xminusxbar[i,] = as.matrix(df[i,] - cm)

  ma[i, 1] = xminusxbar[i,] %*% cr %*% t(t(xminusxbar[i,]))
}

#
library(tidyverse)

dfData = data.frame(
  x1 = c(1, 2, 3, 4, 5)
)

dfData

dfData %>%
  dplyr::mutate(x1 = "Mark")

#
nSum = 0

for (iCount in 1:10) {

  veList = seq(1, iCount, 1)
  nTmpSum = sum(veList, na.rm = TRUE)
  nSum = nSum + nTmpSum

  cat(nTmpSum, " = ", veList, "\n")
}

cat(nSum)

#
library(textreadr)
library(rvest)
library(tidyverse)
library(xml2)
library(magrittr)

meta_data_all = data.frame()

page_url = "http://www.yes24.com/24/category/bestseller?CategoryNumber=001&sumgb=09&year=2019&month=1&PageNumber=1"
page_url = "http://www.yes24.com/Product/Goods/66997133"

bookg_page = read_html(page_url)

aa = #category_layout tr").eq(0).find(".goodsTxtInfo > p > a


  bookg_page %>%
    html_nodes("#category_layout:eq(0) > .goodsTxtInfo > p > a:eq(0)") %>%
    html_text()

bookg_page %>%
  html_nodes(".goodsTxtInfo > p > a") %>%
  html_text()

toString(bookg_page %>%
           html_nodes("td.goodsTxtInfo > p > a")

           %>%
           html_text_collapse)


dd = bookg_page %>%
  html_nodes("td.goodsTxtInfo > p > a") %>%
  html_text() %>%
  str_replace("개", "")


#
library(tidyverse)
library(readxl)
library(stringr)

data = read_excel("INPUT/휴대폰 보유(2014_2018년).xlsx")

dataL1 = data %>%
  tidyr::gather(key = tmpType, value = "value", -type, -typeAtcl) %>%
  tidyr::separate(tmpType, into = c("typeNumber", "year"), sep = "_")

dataL2 = dataL1 %>%
  tidyr::spread(key = "typeNumber", value = "value") %>%
  dplyr::mutate(number = str_replace(number, ",", "")) %>%
  dplyr::mutate_at(vars(year), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(number), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(isYes), funs(as.numeric)) %>%
  dplyr::mutate_at(vars(isNo), funs(as.numeric)) %>%
  dplyr::mutate(nYesNumber = number * isYes)

# 연도별 성별에 따른 휴대폰 보유 수
dataL2 %>%
  dplyr::filter(type == "성별") %>%
  dplyr::group_by(year, typeAtcl) %>%
  dplyr::summarise(nSumYesNumber = sum(nYesNumber, na.rm = TRUE))

# 연도별 연령에 따른 휴대폰 보유 수
dataL2 %>%
  dplyr::filter(type == "연령") %>%
  dplyr::group_by(year, typeAtcl) %>%
  dplyr::summarise(nSumYesNumber = sum(nYesNumber, na.rm = TRUE))


# 연도별 직업에 따른 휴대폰 보유 수
dataL2 %>%
  dplyr::filter(type == "직업") %>%
  dplyr::group_by(year, typeAtcl) %>%
  dplyr::summarise(nSumYesNumber = sum(nYesNumber, na.rm = TRUE))


#

# Clear plots
if (!is.null(dev.list())) {
  dev.off()
}

# Clear console
cat("\014")

# Clear Workspace
rm(list = ls())

a = c(1, 3, 10)

b = c(3, 4, 5)

a %in% b

!(a %in% b)

#
library(tidyverse)

census.data = read.csv("INPUT/101_DT_1IN1002_F_2010.csv", header = TRUE, skip = 2, fileEncoding = "euc-kr")
names(census.data) = gsub("[[:punct:]]+", "_", gsub("[[:punct:]]$", "", names(census.data)))
names(census.data)

dplyr::glimpse(census.data)

dfDataL1 = census.data %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  dplyr::mutate(
    C행정구역별_읍면동 = str_trim(C행정구역별_읍면동, side = c("both"))
    , 행정구역별_읍면동 = str_trim(행정구역별_읍면동, side = c("both"))
    , C행정구역별_읍면동 = str_replace(C행정구역별_읍면동, "'", "")
  ) %>%
  dplyr::filter(nchar(C행정구역별_읍면동) == 5)

dplyr::glimpse(dfDataL1)

# 0 <- 39개가 나와야 맞는데....

dfDataL2 = dfDataL1 %>%
  dplyr::filter(행정구역별_읍면동 %in% c("동부", "읍부", "면부"))

nrow(dfDataL2)


# 302 <- 263개가 나와야 하는데...
dfDataL2 = dfDataL1 %>%
  dplyr::filter(!행정구역별_읍면동 %in% c("동부", "읍부", "면부"))

nrow(dfDataL2)


#

shout = function() {

  sReturn = "HELLO"

  return(sReturn)
}

shout()

#

vSeq = seq(-pi, pi, 0.01)

plot(vSeq, sin(vSeq), type = 'l', xlab = "xlab", ylab = "ylab")
points(vSeq, cos(vSeq))
legend("topright"
  , c("sin", "cos")
  , col = c("black", "black")
  , pch = c(1, NA)
  , lty = c(NA, 1)
  , cex = 0.8
)


#
data = data.frame(
  xAxis = c(1, 2, 3)
  , yAxis = c(1, 2, 4)
)

plot(data$xAxis, data$yAxis
     # 각 점의 모양
  , pch = 0
     # x축과 y축의 라벨
  , xlab = "X", ylab = "Y"
)


#
library(tidyverse)

dfData = data.frame(
  sType = c("남", "남", "남", "남", "여", "여", "여")
  , nAge = c(36, 60, 40, 80, 10, 15, 20)
)

dfData %>%
  dplyr::group_by(sType) %>%
  dplyr::summarise(nMedAge = median(nAge, na.rm = TRUE))

#
howfar = function(x, y) {
  nReturn = sqrt(x^2 + y^2)

  return(nReturn)
}

howfar(3, 4)

#
x = sample(c(0, 1), 7, replace = TRUE)
x

iNumber = length(x) - 2

for (iCount in 1:iNumber) {

  iEndCount = iCount + 2
  isTrue = all(x[iCount:iEndCount] == c(0, 0, 0))

  cat(paste0(iCount, "-", iEndIndex, " : "), isTrue, "\n")
}

#

fnResult = function(x) {

  if ((x %% 2) == 0) {
    nValue = x / 2
  } else {
    nValue = (x + 1) / 2
  }

  return(nValue)
}

fnResult(31)

#

library(tidyverse)

lab = scan("")

lab %>%
  as.data.frame() %>%
  dplyr::mutate(y = ifelse(lab >= 0, 1, -1))


#
dfData = OrchardSprays

#
# (1) 자료형태 : data.frame
class(dfData)

# (2) 변수 개수 : 4개
ncol(OrchardSprays)

# (3) 관측 개수 : 64개
nrow(OrchardSprays)

#
# install.packages("tm")  # for text mining
# install.packages("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator
# install.packages("RColorBrewer") # color palettes

library(wordcloud)
library(tm)
library(SnowballC)
library(RColorBrewer)

filePath = "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"

my1.txt = readLines(filePath)

# 행의 수 : 46
length(my1.txt)

# 빈도수 상위 5개 단어 : "will", "freedom", "ring", "dream", "day"

# 코퍼스로 데이터 읽기
docs = Corpus(VectorSource(my1.txt))

# 문서 내용 검사
inspect(docs)

#  텍스트 변환 (특수문자를 공백으로 변경 )
toSpace = content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs = tm_map(docs, toSpace, "/")
docs = tm_map(docs, toSpace, "@")
docs = tm_map(docs, toSpace, "\\|")

# 텍스트 전처리
# Convert the text to lower case
docs = tm_map(docs, content_transformer(tolower))
# Remove numbers
docs = tm_map(docs, removeNumbers)
# Remove english common stopwords
docs = tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs = tm_map(docs, removeWords, c("blabla1", "blabla2"))
# Remove punctuations
docs = tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs = tm_map(docs, stripWhitespace)

# 워드클라우드를 위한 문서 행렬 작성
dtm = TermDocumentMatrix(docs)
m = as.matrix(dtm)
v = sort(rowSums(m), decreasing = TRUE)
d = data.frame(word = names(v), freq = v)

# 상위 5개 단어
head(d, 5)

# 워드 클라우드 생성
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

#
dfData = quakes

#
# (1) 자료형태 : data.frame
class(dfData)

# (2) 변수 개수 : 5개
ncol(dfData)

# (3) 관측 개수 : 1000개
nrow(dfData)

# (4) 직선관계
lmFit = lm(long ~ mag, data = dfData)
coef(lmFit)[1]
nCor = cor(dfData$mag, dfData$long)
sMain = paste("R = ", round(nCor, 2)
  , ","
  , "Y = ", round(coef(lmFit)[2], 2), "x + ", round(coef(lmFit)[1], 2)
)

plot(dfData$mag, dfData$long, main = sMain)
abline(lmFit, col = 'blue')

#
library(MASS)

dfData = UScrime

# (1) 자료형태 : data.frame
str(dfData)

# (2) 변수 개수 : 16개
ncol(dfData)

# (3) 관측 개수 : 47개
nrow(dfData)

# (4) 상관계수 : 0.1773206
cor(dfData$U2, dfData$y)

# (5) 실업률의 평균 : 33.97872
mean(dfData$U2, na.rm = TRUE)

#
# (6) 상관계수 : 0.7459248
xAxis = dfData$U1
yAxis = dfData$U2

cor(xAxis, yAxis)
lmFit = lm(yAxis ~ xAxis)

plot(xAxis, yAxis)
abline(lmFit, col = 'blue')

# (7) 최대값 : 168, 최소값 : 3
max(dfData$Pop, na.rm = TRUE)
min(dfData$Pop, na.rm = TRUE)

# (8) 상관계수 : 0.4413199
xAxis = dfData$GDP
yAxis = dfData$y

cor(xAxis, yAxis)
lmFit = lm(yAxis ~ xAxis)

plot(xAxis, yAxis)
abline(lmFit, col = 'blue')

# (9) 상관계수 비교 : 0.7459248 vs 0.4413199
cor(dfData$U1, dfData$U2)
cor(dfData$GDP, dfData$y)

# (10) 상관계수 : -0.3899229
cor(dfData$Ed, dfData$Prob)

#
dfData = data.frame(
  nHeight = c(179, 161, 165, 185, 158)
  , nWeight = c(75, 72, 60, 98, 65)
)


# $ : 데이터 프레임에서 변수 추출
dfData$nHeight

library(dplyr)

# %>% : 파이프 연산자로서 dplyr에서 "그리고"와 동일
#       즉 dfData 그리고 (1줄) nHeight 변수 추출 (2줄)
dfData %>%
  dplyr::select(nHeight)


#
library(tidyverse)

mtcars %>%
  data.frame() %>%
  ggplot(aes(x = mpg)) +
  geom_histogram(binwidth = 0.01) +
  annotate(
    "rect"
    , xmin = -0.02
    , xmax = 0.02
    , ymin = 0
    , ymax = sum(mpg == 0, na.rm = TRUE) * 1.1
    , alpha = 0.3
    , fill = "red") +
  xlab(NULL)

#
lm(formula = as.numeric(a$income) ~ as.numeric(a$hours), data = a)

#
library(TTR)
macd = MACD(price, nFast = 15, nSlow = 30, nSig = 9, maType = SMA, percent = FALSE)


library(quantmod)

library(PerformanceAnalytics)

getSymbols("^NSEI")

chartSeries(NSEI, TA = NULL)

data = NSEI[, 4]

# NA 제거
dfDataL1 = na.omit(data)

macd = MACD(dfDataL1, nFast = 12, nSlow = 26, nSig = 9, maType = SMA, percent = FALSE)

plot(macd$macd - macd$signal)

#
Sys.setlocale("LC_ALL", "korean")

#
# 현재 R 프로그램에서 예약어로 사용중인 논리형 변수 (T/F)를 paramT로 변경함
BSM = function(S, paramT, K, sigma, Rf) {
  D1 = (log(S / K, base = exp(1)) + paramT * (Rf + 0.5 * sigma^2)) / sigma * paramT^0.5
  D2 = D1 - sigma * paramT^0.5
  print(S * pnorm(D1) - K * exp(-Rf * paramT) * pnorm(D2))
}

BSM(100, 0.5, 100, 0.1, 0.04)

install.packages("devtools")

#
dfData = data.frame(
  학교명 = c("강릉원주대학교", "강릉원주대학교1")
  , isTure = c(TRUE, FALSE)
)

nIndex = which(dfData$학교명 == "강릉원주대학교")
dfData[nIndex,]

#
library(tidyverse)

dfData = data.frame(
  nCount = seq(1, 1000, 1)
)


dfDataL2 = dfData %>%
  dplyr::mutate(
    isType = case_when(
      (nCount %% 3) == 0 ~ TRUE
      , TRUE ~ FALSE
    )
  )

dplyr::tbl_df(dfDataL2)

#

data = read.csv(file = "INPUT/csv_exam.txt", sep = " ", header = FALSE)

data

#

library(foreign)

dfData = read.spss(
  file = "INPUT/SPSS_Dataset.sav"
  , use.value.label = TRUE
  , to.data.frame = TRUE
)

head(dfData)

#

# 빈 목록 만들기
liList = list()

# 4 회
for (i in 1:4) {
  # 정규 난수가 벡터 생성성
  nRnorm = rnorm(10)

  # liList의 끝에 추가
  liList = c(liList, list(nRnorm))
}

liList


# 요소 수가 4개의 목록을 표시
str(liList)

#
library(KoNLP)

#

# 현재 라이브러리 경로 확인
.libPaths()

# 현재 라이브러리 경로 추가
.libPaths(c("C:/Users/indisystem/Documents/R/win-library/3.6"
  , "C:/Program Files/R/R-3.6.2/library")
)


#

# 소수점 셋째 자리 숫자가 5인 경우 R은 항상 가장 가까운 짝수로 반올림합니다.
# 예를 들어 round(1.5) 및 round(2.5)는 모두 2를 반환하나 round(-4.5)는 -4를 반환합니다.

round(1.5)
round(2.5)
round(-4.5)

#

dfData = data.frame(
  sKey = c("국어", "수학", "영어")
  , nValue = c(70, 100, 85)
)

xAxis = 1:nrow(dfData)
xAxisLabel = dfData$sKey
yAxis = dfData$nValue

plot(xAxis, yAxis, xaxt = "n", type = "b", xlab = "", ylab = "점수")
axis(1, at = xAxis, labels = xAxisLabel)


#
library(ggplot2)

dfData = data.frame(
  nXaxis = c(1, 2, 3, 4)
  , nYaxis = c(1, 4, 3, 4)
)


ggplot(data = dfData, aes(x = nXaxis, y = nYaxis, size = nYaxis)) +
  geom_point()

#
library(ggplot2)
library(sf)

mapShp = sf::read_sf("INPUT/CTPRVN_201905/TL_SCCO_CTPRVN.shp")


ggplot() +
  geom_sf(data = mapShp, aes(fill = CTP_ENG_NM), color = "black")


#
library(ggplot2)
library(tidyverse)

dfData = data.frame(
  xAxis = c(1:19)
  , nClass1 = c(2.2, 2, 4.5, 6, 3, 7, 2.5, 2, 1.5, 1, 3, 3, 3.5, 2, 3, 2, 2.5, 2.8, 2)
  , nClass2 = c(2, 2.8, 3, 4, 5, 4, 3, 4, 3, 4, 3, 2, rep(NA, 7))
  , nClass3 = c(3.1, 3, 3.5, 3.9, 3.1, 3.3, 3.4, 4, 4.3, 4.2, 4.1, 4, 4.5, 4.5, 3.8, 3.5, 3, rep(NA, 2))
)

dfDataL1 = dfData %>%
  tidyr::gather(key = type, value = "value", -xAxis)

ggplot(data = dfDataL1, aes(x = xAxis, y = value, colour = type)) +
  geom_point() +
  geom_line()

dfDataL2 = dfDataL1 %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(nMeanValue = mean(value, na.rm = TRUE))

ggplot(data = dfDataL2, aes(x = type, y = nMeanValue)) +
  geom_point()

#
library(NIADic)
library(rJava)
library(KoNLP)
# useNIADic()
library(memoise)
library(dplyr)

# install.packages("devtools")
# library(devtools)
# install_github('haven-jeon/NIADic/NIADic', build_vignettes = TRUE)

extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다")

#

library(ggplot2)

dfData = data.frame(
  xAxis = c(1:19)
  , nClass1 = c(2.2, 2, 4.5, 6, 3, 7, 2.5, 2, 1.5, 1, 3, 3, 3.5, 2, 3, 2, 2.5, 2.8, 2)
  , nClass2 = c(2, 2.8, 3, 4, 5, 4, 3, 4, 3, 4, 3, 2, rep(NA, 7))
  , nClass3 = c(3.1, 3, 3.5, 3.9, 3.1, 3.3, 3.4, 4, 4.3, 4.2, 4.1, 4, 4.5, 4.5, 3.8, 3.5, 3, rep(NA, 2))
)


for (iCount in 1:3) {
  sColName = colnames(dfData)[iCount + 1]

  ggplot(data = dfData, aes(x = xAxis, y = !!as.name(sColName))) +
    geom_point() +
    geom_line() +
    ggsave(filename = paste0("FIG/", sColName, ".png"))

}

#

# 현재 작업 디렉터리 읽기
getwd()

# 현재 작업 디렉터리 셋팅
setwd(getwd())

# 파일 읽기 (현재 작업 디렉터리 하위에 위치)
dfData = read_excel(paste0(getwd(), "/data/data_ex1.xlsx"), header = TRUE)

#

library(ggplot2)
library(sf)
library(tidyverse)
library(fs)
library(leaflet)

dfData = data.frame(
  sCity = c("서울특별시", "인천광역시", "경기도", "강원도", "대전광역시", "세종특별자치시", "충청남도", "충청북도", "광주광역시", "전라남도", "전라북도", "부산광역시", "울산광역시", "대구광역시", "경상남도", "경상북도", "제주특별자치도")
  , nVal = c(0.1, 0.5, 0.1, 0.5, 0.5, 0.1, 0.5, 0.5, 0.1, 0.1, 0.1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.1)
)

mapShp = sf::read_sf("INPUT/CTPRVN_201905/TL_SCCO_CTPRVN.shp", options = "ENCODING=CP949")

mapShpL1 = mapShp %>%
  dplyr::left_join(dfData, by = c("CTP_KOR_NM" = "sCity"))


ggplot(data = mapShpL1) +
  geom_sf(aes(fill = nVal), color = "black")


mapShpL2 = st_transform(mapShpL1, "+init=epsg:4326")
nVal = mapShpL1[["nVal"]]
fnPal = colorNumeric(palette = "viridis", domain = nVal)

leaflet::leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = mapShpL2
    , color = fnPal(nVal)
    , label = as.character(nVal)
    , stroke = FALSE
  ) %>%
  addLegend(pal = fnPal
    , values = nVal
    , title = "Value"
  )

# R프로그래밍에서 빈이라는게 뭔가요??

dfData = data.frame()

dfData

length(dfData)

# reorder이 순서를 어떻게 바꾸는건가요??

library(dplyr)

dfData = iris

dplyr::tbl_df(dfData)

# "Sepal.Length"를 기준으로 오름차순 정렬
dfData %>%
  dplyr::arrange(Sepal.Length) %>%
  tbl_df()

# "Sepal.Length"를 기준으로 내림차순 정렬
dfData %>%
  dplyr::arrange(desc(Sepal.Length)) %>%
  tbl_df()

# filter(flights, month %in% c(11,12))에서 (11,12)앞에붙은 c는 뭔가요??

library(dplyr)
library(nycflights13)

dfData = flights

dplyr::tbl_df(dfData)

# 11월 및 12월에 해당하는 데이터 정제
dfData %>%
  dplyr::filter(month %in% c(11, 12))

# 내용을 정정하눈 단축어라는데 뭘고치는거고 Cmd는 뭔가요? 명령프롬프트약자아닌가요...?
ctrl + alt + down
ctrl + alt + down
ctrl + alt + down
ctrl + alt + down
ctrl + alt + down

# 두번째 사진은 보기랑 비교해서 풀이 해주세요 ㅜㅜ
A = cbind(
  c(1, 2, 3)
  , c(4, 5, 6)
  , c(7, 8, 9)
)

colnames(A) = c("A", "B", "C")
rownames(A) = c("r1", "r2", "r3")

A[, "A"]
A[, "A"]
A[-c(2, 3),]
A[, -c(2:3)]

# "황소걸음" 2019 데이터 분석 준전문가 한 권으로 끝내기 P.241 에 해당하는 배깅 부분입니다.
# 교재에 나와있는 것과 똑같이 했는데 계속 에러가 뜨는데 어떻게 해결해야 할지 모르겠네요,,,

library(adabag)
library(rpart)

data(iris)

set.seed(1)


train = c(
  sample(1:50, 25)
  , sample(51:100, 25)
  , sample(101:150, 25)
)


iris.bagging = bagging(
  formula = Species ~ .
  , data = iris[train,]
  , mfinal = 10
  , control = rpart.control(maxdepth = 1)
)

iris.bagging


f = function(x, a) {
  return((x - a)^2)
}

f(1:2, 3)


# 여기서 .[[1]]이 뭔가요??

library(stringr)

dfData = "albcld" %>%
  str_split("\\/")

dfData[1]


# 9번 어떻게 하는건가요?
d1 = 1:50
d2 = 51:100

nSortD1 = sort(d1)[1:10]
nSortD2 = sort(d2)[1:10]

d3 = c(nSortD1, nSortD2)
d3

# 6-12모르겠어요ㅜㅜ

v1 = 51:90

# 6번
for (iCount in v1) {
  if ((iCount %% 7) == 3) {
    cat(iCount, "\n")
  }
}

# 7번
for (iCount in 1:length(v1)) {
  iVal = v1[iCount]
  if ((iVal %% 7) == 0) {

    iVal = 0
  }

  cat(iVal, "\n")
}

# 8번
iSum = 0
for (iVal in v1) {
  if ((iVal %% 2) == 0) {
    iSum = iSum + iVal
  }
}

cat(iSum, "\n")


# 12번
for (iCount in v1) {
  if ((iCount %% 7) != 0) {
    cat(iCount, "\n")
  }
}

# 혼자 공부 중인데 잘 모르겠어요
# 도와주세요ㅜㅜ

library(tidyverse)

dfData = data.frame(
  sKey = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
  , iVal = c(10, 8, 14, 15, 9, 10, 15, 12, 9, 7, 8, 7)
)

# (1) 풀이
dfData %>%
  dplyr::summarise(iSum = sum(iVal, na.rm = TRUE))

# (2) 풀이
dfData %>%
  dplyr::filter(sKey == "MAR")

# (3) 풀이
dfData %>%
  dplyr::filter(sKey %in% c("JUL", "SEP"))

# (4) 풀이
dfData %>%
  dplyr::filter(sKey %in% c("JAN", "FEB", "MAR", "APR", "MAY", "JUN")) %>%
  dplyr::summarise(iSum = sum(iVal, na.rm = TRUE))

# (5) 풀이
dfData %>%
  dplyr::filter(!sKey %in% c("JAN", "FEB", "MAR", "APR", "MAY", "JUN")) %>%
  dplyr::summarise(iSum = sum(iVal, na.rm = TRUE))

# 근데 제가 아직 if나 for이런 부분을 공부하지 않아서요ㅜㅜ
# 명령어를 간단하게 해주실 수 있을까요?

# 조건문 : if, else
# 조건에 따라 분류할 경우 if문, else문을 사용해야 합니다.
# "조건식"은 TRUE 또는 FALSE 논리 값을 하나만 반환하는 식을 넣어야 합니다.

x = 2

# if (조건식)
if (x > 0) {

  # 조건 표현식이 TRUE 일 때 실행되는 부분
  sum(1:x, na.rm = TRUE)
} else {

  # 조건식이 FALSE 때 실행되는 부분
  x = -x
  sum(1:x, na.rm = TRUE)
}

# 반복문 : for
# for (변수 in 목록)에서 목록 요소가 순차적으로 수행된다.
# 따라서 반복 횟수는 "목록의 개수"가 된다.

x = 0
# for (변수 in 목록)
for (i in c(1:5)) {

  # 벡터와리스트의 요소가 비우지 않는 한 표현식이 반복된다
  x = x + 1
}

x

# 산점도안에 결정계수(R-square)가 있고 R-square를 표현하는 line을 만드는 방법이 궁금합니다!

dfData = data.frame(
  iCal = c(1, 2, 3, 4, 5)
  , iVal = c(3, 6, 8, 10, 11)
)

xAxis = dfData$iCal
yAxis = dfData$iVal

# 산점도
plot(xAxis, yAxis)

# 선형 회귀선
oLmFit = lm(yAxis ~ xAxis)
abline(oLmFit, col = "red")

# 범례 생성
oSummary = summary(oLmFit)

nRsquare = oSummary$adj.r.squared
nPvalue = oSummary$coefficients[2, 4]

oLegend = vector("expression", 2)
oLegend[1] = substitute(expression(R^2 == nRsquareList),
                        list(nRsquareList = format(nRsquare, digits = 3)))[2]
oLegend[2] = substitute(expression(P - Value == nPvalueList),
                        list(nPvalueList = format(nPvalue, digits = 2)))[2]

legend("bottomright", legend = oLegend, bty = "n")


# R을 설치하고 dplyr 패키지를 설치하던 중 하기와 같은 메세지가 뜨면서 설치가 안되네요.
# Error in if (any(diff)) { : TRUE/FALSE가 필요한 곳에 값이 없습니다.


# r로 웹페이지 크롤링
# 이런 자료를 크롤링해서 엑셀로 정리하고싶은데 어떻게 해야하나요? 이게 1페이지인데 끝 페이지까지요

library(rvest)
library(tidyverse)
library(data.table)

Sys.setlocale("LC_ALL", "English")
options(encoding = "UTF-8")
Sys.setenv(LANG = "en_US.UTF-8")

sUrl = paste0("https://franchise.ftc.go.kr/user/extra/main/62/firMst/list/jsp/LayOutPage.do?column=&search=&searchFirRegNo=&selUpjong=&selIndus=&srow=999999&spage=1")

dfData = xml2::read_html(sUrl) %>%
  rvest::html_nodes(xpath = paste0('//*[@id="txt"]/table')) %>%
  rvest::html_table()

dfDataL1 = dfData[[1]]

dplyr::tbl_df(dfDataL1)

# Write Using L3 Data Frame
data.table::fwrite(
  dfDataL1
  , sep = ","
  , file = paste0("OUTPUT/dfDataL1_", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv")
  , append = FALSE
  , row.names = FALSE
  , col.names = TRUE
  , dateTimeAs = "write.csv"
  , na = NA
)

# https://franchise.ftc.go.kr/user/extra/main/62/firMst/list/jsp/LayOutPage.do?column=&search=&searchFirRegNo=&selUpjong=&selIndus=&srow=999999&spage=1
# 여기 정보공개서의 영업표지나 상호를 누르면 링크가 있어서 타고 연결이 되는데 그건 같이 못하나요? 같이 크롤링 되게요!

library(rvest)
library(tidyverse)
library(data.table)

Sys.setlocale("LC_ALL", "English")
options(encoding = "UTF-8")
Sys.setenv(LANG = "en_US.UTF-8")

fnGetUrlHref = function(sUrl, sXpath) {
  xml2::read_html(sUrl) %>%
    rvest::html_nodes(xpath = paste0(sXpath)) %>%
    rvest::html_attr("href")
}

sUrl = paste0("https://franchise.ftc.go.kr/user/extra/main/62/firMst/list/jsp/LayOutPage.do?column=&search=&searchFirRegNo=&selUpjong=&selIndus=&srow=9999&spage=1")

dfData = xml2::read_html(sUrl) %>%
  rvest::html_nodes(xpath = paste0('//*[@id="txt"]/table')) %>%
  rvest::html_table()

dfDataL1 = dfData[[1]] %>%
  dplyr::mutate(
    sNameLink = paste0("https://franchise.ftc.go.kr", fnGetUrlHref(sUrl, '//*[@id="txt"]/table/tbody/tr[*]/td[2]/a'))
    , sCompLink = paste0("https://franchise.ftc.go.kr", fnGetUrlHref(sUrl, '//*[@id="txt"]/table/tbody/tr[*]/td[3]/a'))
  )


dplyr::tbl_df(dfDataL1)


# library(rvest)
# library(pforeach)
#
# i = 1
#
# npforeach(i=1:2, .c=rbind)({
#   cat(i, "\n")
#   url <- sprintf("http://lain.gr.jp/voicedb/profile/list/cid/%d", i)
#
#   # Sys.sleep(10)
#   html <- html(url)
#
#   li_nodes <- html %>% html_nodes(xpath = '/html/body/div/div/div[2]/ul/li')
#
#   url <- li_nodes %>% html_nodes(xpath = "a") %>% html_attr("href")
#   name <- li_nodes %>% html_nodes(xpath = "a") %>% html_text %>% iconv("utf8", "cp932")
#   sex <- li_nodes %>% html_nodes(xpath = "img") %>% html_attr("title") %>% iconv("utf8", "cp932")
#
#   data.frame(url=url[-(1:4)], name=name[-(1:4)], sex)
# }) -> result


# 인터넷에 올라온 웹스크래핑 설명을 본 후 제가 직접 활용해보려 하니 바로 막혀버렸습니다 ㅠㅠ
# 지금 맞닥뜨린 문제는 입력해야 할 url의 소스 코드가 뭔지 모르겠다는 것입니다.
# 크롬 개발자 도구에서 해당 페이지 html의 어떤 태그를 html_nodes()에 넣어야 하고, 이 태그가 어떤 원리 또는 이유에서 선택되는지가 궁금합니다.
# 설명글에서는 그냥 크롬 개발자 도구 스크린샷 찍은 것을 보여준 후 이러이러 하니 "searchCont라는 클래스가 붙은 디비전(divisodn) 아래 링크가 들어 있다"와 같이 간단히 언급하고 넘어가서, 개발자 도구를 봐도 어떤 태그를 선택해야 하는지 모르겠습니다.. 흑
# 제가 해본 코드는 다음과 같습니다.
# 오유 시사게시판 게시글 내용을 스크래핑 해보려 했는데 망이네요 ㅜ
# 일단, 첫 번째 페이지 url의 html을 불러온 부분( html <- read_html(urls[1]) )부터 설명글과 달리 저는 xml_document가 아닌 html_document가 뜹니다 ;
# html2, html3 리스트에는 아무것도 들어가지 않은 걸로 뜨고요.

library(foreach)
library(rvest)
library(tidyverse)
library(data.table)
library(dplyr)

Sys.setlocale("LC_ALL", "English")
options(encoding = "UTF-8")
Sys.setenv(LANG = "en_US.UTF-8")

# Option
iPage = 10

fnGetUrlText = function(sUrl, sXpath) {
  read_html(sUrl) %>%
    html_nodes(xpath = paste0(sXpath)) %>%
    html_text()
}

dfDataL1 = data.frame()

foreach::foreach(iCount = 1:iPage, .combine = c) %do% {

  cat(iCount, "\n")
  sUrl = paste0("http://www.todayhumor.co.kr/board/list.php?table=sisa&page=", iCount)

  dfData = data.frame(
    arrNumber = fnGetUrlText(sUrl, '/html/body/div[4]/div/div/table/tbody/tr[*]/td[1]/a')[1:30]
    , arrTitle = fnGetUrlText(sUrl, '/html/body/div[4]/div/div/table/tbody/tr[*]/td[3]/a')
    , arrsName = fnGetUrlText(sUrl, '/html/body/div[4]/div/div/table/tbody/tr[*]/td[4]/a')
    , arrDate = fnGetUrlText(sUrl, '/html/body/div[4]/div/div/table/tbody/tr[*]/td[5]')
    , arrCount = fnGetUrlText(sUrl, '/html/body/div[4]/div/div/table/tbody/tr[*]/td[6]')
    , arrRecom = fnGetUrlText(sUrl, '/html/body/div[4]/div/div/table/tbody/tr[*]/td[7]')
  )

  dfDataL1 = dplyr::bind_rows(dfDataL1, dfData)
}

dplyr::tbl_df(dfDataL1)


# RA_gbm05 <- gbm(label ~ ., RA, distribution = "bernoulli", n.trees = 50, cv.folds=5)
# GBM 분석 코드를 작성을 하였고요.
# 이것을 토대로 교차 검증 값에 대한 점수 값을 구하려 합니다.
# 어떻게 해야 할 까요???
# 파이썬에서는"cross_val_score" 이런 코드를 사용 하던데,
# R 코드로는 어떻게 작성을 해야 할 까요???

library(rsample)      # data splitting
library(gbm)          # basic implementation
library(xgboost)      # a faster implementation of gbm
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # a java-based platform
library(pdp)          # model visualization
library(ggplot2)      # model visualization
library(lime)         # model visualization

# Create training (70%) and test (30%) sets for the AmesHousing::make_ames() data.
# Use set.seed for reproducibility

set.seed(123)
ames_split = initial_split(AmesHousing::make_ames(), prop = 0.7)
ames_train = training(ames_split)
ames_test = testing(ames_split)

# for reproducibility
set.seed(123)

# train GBM model
gbm.fit = gbm(
  formula = Sale_Price ~ .,
  distribution = "gaussian",
  data = ames_train,
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)

# print results
print(gbm.fit)
## gbm(formula = Sale_Price ~ ., distribution = "gaussian", data = ames_train,
##     n.trees = 10000, interaction.depth = 1, shrinkage = 0.001,
##     cv.folds = 5, verbose = FALSE, n.cores = NULL)
## A gradient boosted model with gaussian loss function.
## 10000 iterations were performed.
## The best cross-validation iteration was 10000.
## There were 80 predictors of which 45 had non-zero influence.


# get MSE and compute RMSE
sqrt(min(gbm.fit$cv.error))
## [1] 29133.33

# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit, method = "cv")


# R을 설치하고 dplyr 패키지를 설치하던 중
# 하기와 같은 메세지가 뜨면서 설치가 안되네요.
# Error in if (any(diff)) { : TRUE/FALSE가 필요한 곳에 값이 없습니다.
# 우찌 해야 될까요?

# R v3.4에서 발생되는 오류로 판단됩니다.

install.packages("tidyverse", dependencies = TRUE, repos = "http://cran.us.r-project.org")

# https://stackoverflow.com/questions/38948664/error-when-install-package-dplyr-in-r-error-in-install-packages-missing-value/44160760


library(KoNLP)

Sys.setenv(JAVA_HOME = "C:\\Users\\jmpark\\AppData\\Local\\Programs\\AdoptOpenJDK\\")
Sys.getenv("JAVA_HOME")

useSejongDic()

sSentence = "아버지가 방에 스르륵 들어가신다."

extractNoun(sSentence)


# 안녕하세요,
# 수강하고 있는 온라인 강의 과제때문에 GSS라는 사이트에서 데이터를 다운받아 R STUDIO의 rmd 파일로 작업을 해야 하는데요.
# load("gss.Rdata") 코드를 입력해서 데이터를 불러오면된다고 되어 있는데 계속 오류메세지가 뜹니다ㅜㅜ
# 오류메세지는 Error in readChar(con, 5L, useBytes = TRUE) : cannot open the connection 라고 나오고요.
# 데이터 파일 불러오는게 간단한 것 같은데 무슨 이유로 계속 안열리는지 모르겠어요 ㅜㅜ
# 무엇을 어떻게 만져야 되는지 알려주시면 정말 정말 감사드리겠습니다!!


# 즉 현재 작업 디렉터리를 기준으로 gss.Rdata을 찾지 못해서 발생된 에러입니다.
# 따라서 "getwd"를 통해 현재 작업 디렉터리를 확인하고 그에 따라 설정해주시면 됩니다.

getwd()

load("INPUT/gss.Rdata")

# R에서 비선형회귀분석 시에 F값이나 P값을 안 주는데,
# 이는 어떻게 구하나요?

dfData = data.frame(
  iCal = c(1, 2, 3, 4, 5)
  , iVal = c(3, 6, 8, 10, 11)
)

xAxis = dfData$iCal
xAxis2 = xAxis^2
yAxis = dfData$iVal

# 비선형 회귀
oLmFit = lm(yAxis ~ xAxis + xAxis2)

plot(xAxis, yAxis)
lines(xAxis, predict(oLmFit), col = 'red')

# 범례 생성
oSummary = summary(oLmFit)

nRsquare = oSummary$adj.r.squared
nPvalue = oSummary$coefficients[2, 4]
nFvalue = oSummary$fstatistic[1]

oLegend = vector("expression", 2)
oLegend[1] = substitute(expression(R^2 == nRsquareList),
                        list(nRsquareList = format(nRsquare, digits = 3)))[2]
oLegend[2] = substitute(expression(P - Value == nPvalueList),
                        list(nPvalueList = format(nPvalue, digits = 2)))[2]
oLegend[3] = substitute(expression(F - Value == nFvalueList),
                        list(nFvalueList = format(nFvalue, digits = 2)))[2]

legend("bottomright", legend = oLegend, bty = "n")

# X<-1:100
# sum(X>50)
# 이거 정답이 왜 50인가여
# Sum이니까 51부터 100까지 더해야하는거 아닌가요?

x = 1:100

# TRUE의 총 개수를 의미
sum(x > 50)

# TRUE에 대한 인덱스 추출
vList = which(x > 50)

sum(x[vList], na.rm = TRUE)

# 25번문제 어떻게 푸나요?

f = function(x, a) {
  return((x - a)^2)
}

f(1:2, 3)


# 인터넷에 올라온 웹스크래핑 설명을 본 후 제가 직접 활용해보려 하니 바로 막혀버렸습니다 ㅠㅠ
# 지금 맞닥뜨린 문제는 입력해야 할 url의 소스 코드가 뭔지 모르겠다는 것입니다.
# 크롬 개발자 도구에서 해당 페이지 html의 어떤 태그를 html_nodes()에 넣어야 하고, 이 태그가 어떤 원리 또는 이유에서 선택되는지가 궁금합니다.
# 설명글에서는 그냥 크롬 개발자 도구 스크린샷 찍은 것을 보여준 후 이러이러 하니 "searchCont라는 클래스가 붙은 디비전(divisodn) 아래 링크가 들어 있다"와 같이 간단히 언급하고 넘어가서, 개발자 도구를 봐도 어떤 태그를 선택해야 하는지 모르겠습니다.. 흑
#
# 제가 해본 코드는 다음과 같습니다.
# 오유 시사게시판 게시글 내용을 스크래핑 해보려 했는데 망이네요 ㅜ
# 일단, 첫 번째 페이지 url의 html을 불러온 부분( html <- read_html(urls[1]) )부터 설명글과 달리 저는 xml_document가 아닌 html_document가 뜹니다 ;
# html2, html3 리스트에는 아무것도 들어가지 않은 걸로 뜨고요.
#
# basic_url <- "http://www.todayhumor.co.kr/board/list.php?table=sisa&page="
# urls <- NULL
# for(x in 1:10){
#     urls[x] <- paste0(basic_url, x)
# }
#
# html <- read_html(urls[1])
# html2 <- html_nodes(html, '.view list_tr_sisa')
# html3 <- html_nodes(html2, 'a')
#
# 직접 지지고 볶아야 실력이 늘거라 생각하고 덤볐는데 넘 초반부터 막혀버렸네요 허허
# 고수님들 도움 좀 부탁드리겠습니다.


library(foreach)
library(rvest)
library(tidyverse)
library(data.table)
library(dplyr)

Sys.setlocale("LC_ALL", "English")
options(encoding = "UTF-8")
Sys.setenv(LANG = "en_US.UTF-8")

# Option
iPage = 10

fnGetUrlText = function(sUrl, sXpath) {
  read_html(sUrl) %>%
    html_nodes(xpath = paste0(sXpath)) %>%
    html_text()
}

dfDataL1 = data.frame()

foreach::foreach(iCount = 1:iPage, .combine = c) %do% {

  cat(iCount, "\n")
  sUrl = paste0("http://www.todayhumor.co.kr/board/list.php?table=sisa&page=", iCount)

  dfData = tibble::tibble(
    arrNumber = fnGetUrlText(sUrl, '/html/body/div[4]/div/div/table/tbody/tr[*]/td[1]/a')[1:30]
    , arrTitle = fnGetUrlText(sUrl, '/html/body/div[4]/div/div/table/tbody/tr[*]/td[3]/a')
    , arrsName = fnGetUrlText(sUrl, '/html/body/div[4]/div/div/table/tbody/tr[*]/td[4]/a')
    , arrDate = fnGetUrlText(sUrl, '/html/body/div[4]/div/div/table/tbody/tr[*]/td[5]')
    , arrCount = fnGetUrlText(sUrl, '/html/body/div[4]/div/div/table/tbody/tr[*]/td[6]')
    , arrRecom = fnGetUrlText(sUrl, '/html/body/div[4]/div/div/table/tbody/tr[*]/td[7]')
  )

  dfDataL1 = dplyr::bind_rows(dfDataL1, dfData)
}

dplyr::tbl_df(dfDataL1)


#
# 안녕하세요 기상데이터를 받으려고 하는데
# 대량의 데이터를 다운받으려고 합니다.
# Error in paste0(getwd(), "/", dirname, "/", file_name[count]) :
#     object 'file_name' not found
# 이렇게 에러가 나서요 ㅠ
#
# 코딩 확인 부탁드릴게요 ㅜㅜ
# -------------------------------------------------------------------------------------------------------------------

rm(list = ls())
dirname <- "climate_swat"
dir.create(dirname)
for (page_num in 1) {
  default_url <- "https://data.kma.go.kr/data/grnd/selectAwsRltmList.do?pgmNo=56&tabNo="
  lm_2_url <- paste0(default_url, page_num)
}
lm_2_html <- read_html(lm_2_url)
lm_2_table <- lm_2_html %>%
  html_nodes("bbsList table")

lm_2_file_url <- lm_2_table %>%
  html_nodes(".title") %>%
  html_node("a") %>%
  html_attr("href")

for (for_url in lm_2_file_url) {
  file_url <- read_html(for_url) %>%
    html_node(".float-right") %>%
    html_nodes("a") %>%
    html_attr("href")

  file_name <- read_html(for_url) %>%
    html_nodes(".float-right") %>%
    html_nodes("a") %>%
    html_text()
}

for (count in 1:2) {
  destfile_name <-
    paste0(getwd(), "/", dirname, "/", file_name[count])
  download.file(file_url[count], destfile = destfile_name)
}


# lm은 선형회귀식을 위한 거 아니에요?
#   저는 지금 nls를 이용해서 지수함수(y~a*exp(-b*x))를 피팅하고 있는데,
# nls는 p값과 f값을 제공하지 않아서,
# 그 값을 계산하고 싶은데요.
# 다른 방법이 있나요?

dfData = data.frame(
  iCal = c(1, 2, 3, 4, 5)
  , iVal = c(3, 6, 8, 10, 11)
)

# 비선형 회귀
oNls = nls(iVal ~ a * exp(-b * iCal), data = dfData, start = list(a = 1, b = 0))

yPred = predict(oNls)

plot(xAxis, yAxis)
lines(xAxis, yPred, col = 'red')

# 범례 생성
oCorTest = cor.test(xAxis, yPred)

nRsquare = oCorTest$estimate
nPvalue = oCorTest$p.value
nFvalue = oCorTest$statistic

oLegend = vector("expression", 2)
oLegend[1] = substitute(expression(R == nRsquareList),
                        list(nRsquareList = format(nRsquare, digits = 3)))[2]
oLegend[2] = substitute(expression(P - Value == nPvalueList),
                        list(nPvalueList = format(nPvalue, digits = 2)))[2]
oLegend[3] = substitute(expression(F - Value == nFvalueList),
                        list(nFvalueList = format(nFvalue, digits = 2)))[2]

legend("bottomright", legend = oLegend, bty = "n")

# 어떠한 과정을 거쳐 4 1 이 나오는지 알고 싶습니다ㅠㅠ x와 a 에 어떤 방식으로 값이 입력되는건가요??

# 안녕하세요? 해솔입니다.
#
# 해당 답변에 대한 소스 코드 및 실행 결과를 보내드리오니 확인 부탁드립니다.
#
# 알고리즘 흐름은 다음과 같습니다.
# f(1, 3) 호출 >>  (1 - 3)^2 계산 >> 숫자 4 반환
# f(2, 3) 호출 >>  (2 - 3)^2 계산 >> 숫자 1 반환
#
# 최종적으로 4 1로 반환 결과를 확인할 수 있습니다.
# 
# 좋은 하루 보내세요.
#
# 해솔 올림.


# 안녕하세요. 혹시 가능하면 또 하나 질문해도 될까요? 그 정보공개서 해당 업체의 링크를 타고 들어가면 대표번호가 있습니다. 그 대표번호만 끌고올 수 잇는 방법이 있을 까요?
#  현재 엑셀탭이 번호,상호, 영업표지, 대표자, 등록번호, 업종 순으로 되어있는데 그 옆에 링크안에 있는 내용인 대표번호가 탭으로 나오게 하고싶습니다 ㅠㅠ 마지막 부탁입니다. 귀찮게 해드려 정말 죄송합니다.

library(rvest)
library(tidyverse)
library(data.table)
library(purrr)

Sys.setlocale("LC_ALL", "English")

fnGetUrlHref = function(sUrl, sXpath) {
  xml2::read_html(paste0(sUrl)) %>%
    rvest::html_nodes(xpath = paste0(sXpath)) %>%
    rvest::html_attr("href")
}

fnGetUrlTable = function(sUrl, sXpath) {
  xml2::read_html(paste0(sUrl)) %>%
    rvest::html_nodes(xpath = paste0(sXpath)) %>%
    rvest::html_table()
}

fnGetUrlText = function(sUrl, sXpath) {
  xml2::read_html(sUrl) %>%
    rvest::html_nodes(xpath = paste0(sXpath)) %>%
    rvest::html_text() %>%
    gsub("^\\s+|\\s+$", "", .)
}

sUrl = paste0("https://franchise.ftc.go.kr/user/extra/main/62/firMst/list/jsp/LayOutPage.do?column=&search=&searchFirRegNo=&selUpjong=&selIndus=&srow=100&spage=1")

dfData = fnGetUrlTable(sUrl, '//*[@id="txt"]/table')

dfDataL1 = dfData[[1]] %>%
  dplyr::mutate(
    sNameLink = paste0("https://franchise.ftc.go.kr", fnGetUrlHref(sUrl, '//*[@id="txt"]/table/tbody/tr[*]/td[2]/a'))
    , sCompLink = paste0("https://franchise.ftc.go.kr", fnGetUrlHref(sUrl, '//*[@id="txt"]/table/tbody/tr[*]/td[3]/a'))
    , sPhoneNumber = purrr::map2(sNameLink, '//*[@id="txt"]/div[1]/div/table[1]/tbody/tr[3]/td[3]', fnGetUrlText)
  )

dplyr::tbl_df(dfDataL1)

# r프로그래밍 lubridate의 피리어드형
# hours의 결과값을 어떻게 읽어야하나요 왜두줄인지도....

library(lubridate)

# 2개 전달 인자
arrHour = lubridate::hours(c(12, 24))

# 1번째 결과 반환
arrHour[1]

# 2번째 결과 반환
arrHour[2]


# 1개 전달 인자
oHour = lubridate::hours(12)

# 결과 반환
oHour

# flights%>%count(week=floor_date(dep_time,"week"))%>%
# +  ggplot(aes(week,n))+geom_line()
# Error in object[[name, exact = TRUE]] : 첨자의 허용 범위를 벗어났습니다
# 어떤 부분이 오류가 난건가요??

library(nycflights13)
library(tidyverse)

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

dfFlights = nycflights13::flights %>%
  dplyr::filter(!is.na(dep_time), !is.na(arr_time)) %>%
  dplyr::mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>%
  dplyr::select(origin, dest, ends_with("delay"), ends_with("time"))

dfFlights %>%
  count(week = floor_date(dep_time, "week")) %>%
  ggplot(aes(week, n)) +
  geom_line()


# R에서 epanet2toolkit 패키지를 불러오려하는데 아래와 같은 오류가 뜹니다..ㅠㅠ 어떻게 해결하죠(as ‘lib’ is unspecified)

install.packages("epanet2toolkit", INSTALL_opts = "--install-tests")

install.packages("epanet2toolkit")

library(devtools)
devtools::install_github("bradleyjeck/epanet2toolkit", force = TRUE)

library(epanet2toolkit)

?epanet2toolkit

ENepanet("Net1.inp", "Net1.rpt")

#
# 아래와 같은 표가 있는데 time series 그래프를 그리고 싶습니다.
# 1) 셀을 합치지 않고 그래프를 그릴 수 있는 방법이 있나요?
# 2) 합쳐야 한다면 어떻게 하나요? make_date를 쓰니까 날짜까지 다 나오더군요.

library(tidyverse)
library(lubridate)

# 임의 데이터 생산
dfData = tibble::tibble(
  iYear = c(rep(1982, 12), 1983)
  , iMonth = c(seq(1, 12), 1)
  , nMeanTemp = rnorm(13) * 20
)

dfDataL1 = dfData %>%
  dplyr::mutate(
    nXranYm = iYear + ((iMonth - 1) / 12.0)
  )


xAxis = dfDataL1$nXranYm
yAxis = dfDataL1$nMeanTemp

plot(xAxis, yAxis)

oLmFit = lm(yAxis ~ xAxis)
abline(oLmFit, col = 'red')

# r 스튜디오에서 코드복사나 코드수정어떻게하나요?
# 코드복사하면 +가 같이 복사되던데 +가 복사되는건 크게 의미있는건아닌가요?

# 코드 복사의 경우 "<Ctrl + c>"을 수정하면 됩니다.
# 코드 수정의 경우 해당 변수에 마우스 더블 클릭을 통해 수정 가능합니다.
# R 프로그램에서 "+" 기호는 줄바꿈을 의미하기 때문에 해당 기호 포함하여 복사해야 합니다.

# x에 rnorm(10)을 넣은건데 function에 인수를 넣으면 어떻게되눈건가요?? function의 인수는 어디에쓰이는건기요??

rescale01 = function(x) {
  rng = range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

# rescale01(c(0, 5, 10))

nRnorm = c(rnorm(10))

nRnorm

rsResult = rescale01(nRnorm)

rsResult

# 안녕하세요. R과 R 스튜디오 모두 최신판으로 설치했고
# 사용하려는데 R 스튜디오와 R 모두에서 객체 생성이 되지 않습니다. 이유가 뭘까요...?

x <- 10

x

x = 9

x

# Rstudio 설치 후 라이브러리 인스톨 시 에러
# C아래 바로 폴더로 설치했구요(환경은 윈10, 64)
# 라이브러리 설치하면서,계속 아래와 같은 메시지가 떠서 타이핑이 불가능합니다.

# 이는 설치 경로에서 한글 파일이 존재하기 때문에 발생되는 문제입니다.
# 따라서 한글을 영어로 변경한 후에 Rstudio 및 R 재 설치를 부탁드립니다.

# r 데이터 병합, 추출 질문입니다.
# 이렇게 원하는 변수를 가지고 하나의 데이터로 합치는 방법이 궁금합니다.

library(tidyverse)

dfData = data.frame(
  nTime = c(1:5)
  , nNumber = c(5, 6, 2, 7, 1)
)

dplyr::tbl_df(dfData)

dfData2 = data.frame(
  nTime = c(1:6)
  , nSale = c(20, 15, 30, 40, 60, 50)
)

dplyr::tbl_df(dfData2)

dfDataL1 = dfData %>%
  dplyr::full_join(dfData2, by = c("nTime" = "nTime"))

dplyr::tbl_df(dfDataL1)

# R을 사용할 때,
# 데이터에서 날짜가 X1.22.20 이와 같이 chr 형태인 경우에는
# 어떻게 lubridate 패키지를 사용해서 날짜 형태로 바꿀 수 있나요?

library(lubridate)

sDate = "2020-03-09 12:25"
class(sDate)

dtDate = lubridate::ymd_hm(sDate)

dtDate
class(dtDate)

#
library(tidyverse)

mtcars

#

library(xml2)
library(rvest)
library(tidyverse)

Sys.setlocale("LC_ALL", "English")

fnGetUrlText = function(sUrl, sXpath) {
  xml2::read_html(paste0(sUrl)) %>%
    rvest::html_nodes(xpath = paste0(sXpath)) %>%
    rvest::html_text()
}

sUrl = "https://search.naver.com/search.naver?%27,%20%27sm=tab_hty.top&where=nexearch&query=%EC%82%BC%EC%84%B1%EC%A0%84%EC%9E%90"

sXpath = '//*[@id="nx_related_keywords"]/dl/dd[1]/ul/li[*]/a'

dfData = fnGetUrlText(sUrl, sXpath)

dfData

#
# 제가 R을 이용해 아래와 같은 코드로 그래프를 그리는데까지 성공했습니다.
# 근데 jiho 데이터는 다수의 변수(plant.length, grow.length 등)가 있는데 한번에 묶어서 일괄적으로 ddply()함수를 적용해서 b 라는 데이터프레임을 만들 수 있나요?

library(tidyverse)

dfData = read.csv("INPUT/jiho.csv", stringsAsFactors = FALSE)

dplyr::tbl_df(dfData)

# 속성 및 날짜에 따른 평균
dfDataL1 = dfData %>%
  tidyr::gather(key = "sKey", value = "nVal", c(-Name, -Day, -No)) %>%
  dplyr::mutate(dtDate = readr::parse_date(Day, "%Y-%m-%d")) %>%
  dplyr::group_by(sKey, dtDate) %>%
  dplyr::summarise(nMeanVal = mean(nVal, na.rm = TRUE))

dplyr::tbl_df(dfDataL1)

# 날짜에 따른 평균
dfDataL2 = dfData %>%
  tidyr::gather(key = "sKey", value = "nVal", c(-Name, -Day, -No)) %>%
  dplyr::mutate(dtDate = readr::parse_date(Day, "%Y-%m-%d")) %>%
  dplyr::group_by(dtDate) %>%
  dplyr::summarise(nMeanVal = mean(nVal, na.rm = TRUE))

dplyr::tbl_df(dfDataL2)


#
# 제가 이번학기에 R함수에 대해 배우는데 사용중인 변수 목록은 ls()를 통해 볼 수 있는데
# 아래 사진을 보면 ls() 후 x,y,z가 나와야 하는데 왜 저런 문자가 나오는거죠?
# 이해가 안가요..


# 변수 목록 삭제
rm(list = ls())

x = 1:10
y = 11:20
z = 21:30

# 변수 목록 확인
ls()

# 이런 형식이 나오는데 이것을 JSON형식으로 바꾸기 위해

library(rjson)

dfData = data.frame(
  source = c("banana", "banana")
  , target = c("pitch", "graph")
  , type = c("A", "B")
)

rjson::toJSON(dfData)

# 이 화면에 나오는 tiltle만 크롤링해서 프린트하고 싶은데 에러가 나오네요. 어디를 어떻게 고치면 될까요

library(rvest)
library(tidyverse)
library(xml2)

fnGetUrlText = function(sUrl, sXpath) {
  xml2::read_html(paste0(sUrl), encoding = "EUC-KR") %>%
    rvest::html_nodes(xpath = paste0(sXpath)) %>%
    rvest::html_text()
}

sUrl = "https://finance.naver.com/news/news_list.nhn?%27,%20%27mode=LSS2D&section_id=101&section_id2=258"

sXpath = '//*[@id="contentarea_left"]/ul/li[*]/dl/dd[*]/a'

dfData = fnGetUrlText(sUrl, sXpath)

dfData

#

library(dplyr)

dfData = data.frame(
  source = c("banana", "banana")
  , target = c("pitch", "graph")
  , type = c("A", "B")
)

#
dfData %>%
  dplyr::filter(type == "A")

dfData %>%
  dplyr::filter(type %in% c("A", "B"))

#

# v1에서 짝수에 대해서만 2를 곱하여 저장하시오.

v1 <- 51:90

nIndex = which(v1 %% 2 == 0)

nVal = v1[nIndex] * 2

nVal


# v1에서 7의 배수들을 제거한 후 v1의 내용을 출력하시오.

v1 <- 51:90

nIndex = which(v1 %% 7 != 0)

nVal = v1[nIndex]

nVal

# x <- c(1, 2, 2, 1, 3, 1) 인데 1은 male로 2는 femal로 3은 unknown으로 할려는데

library(dplyr)

x <- c(1, 2, 2, 1, 3, 1)

dfData = data.frame(x) %>%
  dplyr::mutate(
    type = case_when(
      x == 1 ~ "male"
      , x == 2 ~ "femal"
      , x == 3 ~ "unknown"
      , TRUE ~ "null"
    ))

dfData

#

library(readxl)

dfData = read_excel("INPUT/sample_data.xlsx", sheet = "생활상태")

dplyr::tbl_df(dfData)

# 1234523456345674567856789가 출력되게하는 방법이 궁금합니다!

nVal = c(
  seq(1, 5, 1)
  , seq(2, 6, 1)
  , seq(3, 7, 1)
  , seq(4, 8, 1)
  , seq(5, 9, 1)
)

nVal

# 1-100의 정수를 차례로 출력하되 3의 배수에서는 숫자 대신 "+" 출력하는 R 코드를 작성하시오.

nVal = seq(1, 100)

for (nRow in nVal) {

  if (nRow %% 3 == 0) {
    nRow = "+"
  }

  cat(nRow)

}

# 소수 (prime number)는 1과 자기 자신 외에는 나누어 떨어지지 않는 수를 말한다. 2-1000 사이의 소수를 출력하는 R 코드를 작성하시오.

prime_numbers = function(n) {
  if (n >= 2) {
    x = seq(2, n)
    prime_nums = c()

    for (i in seq(2, n)) {
      if (any(x == i)) {
        prime_nums = c(prime_nums, i)
        x = c(x[(x %% i) != 0], i)
      }
    }
    return(prime_nums)
  } else {
    stop("Input number should be at least 2.")
  }
}

prime_numbers(1000)


# for문을 이용하여 구구단 중 9단을 출력하는 R 코드를 작성하시오.

setVal = 9

for (iCount in 1:9) {
  for (jCount in 1:9) {

    if (iCount == setVal) {
      cat(iCount, " * ", jCount, " = ", iCount * jCount, "\n")
    }
  }
}


# while문을 이용하여 구구단 중 8단을 출력하는 R 코드를 작성하시오.

setVal = 8

iCount = 1
while (iCount <= 9) {

  jCount = 1

  while (jCount <= 9) {
    if (iCount == setVal) {
      cat(iCount, " * ", jCount, " = ", iCount * jCount, "\n")
    }
    jCount = jCount + 1
  }
  iCount = iCount + 1
}

# R프로그램으로 "1부터 100까지 3의 배수의 합"을 구하는 방법이 궁금합니다.!!

nVal = seq(1, 100)

nIndex = which(nVal %% 3 == 0)

sum(nVal[nIndex], na.rm = TRUE)

# 2020년 달력과 연동되게 평일은 1, 주말은 0으로 표기하여 2진변수를 만들어라

library(lubridate)
library(dplyr)

dtDate = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), "1 day")

dtDate

# nWday에 대해서
# 월-금 : 2 3 4 5 6
# 토-일 : 7 1

data.frame(dtDate) %>%
  dplyr::mutate(
    sWdayLabel = lubridate::wday(dtDate, label = TRUE)
    , nWday = lubridate::wday(dtDate)
    , type = case_when(
      2 <= nWday & nWday <= 6 ~ 1
      , TRUE ~ 0
    )
  )


# r 프로그래밍 마지막 iv) 에서 z[z %% 0.2 == 0 & z >= 0] 연산하면 왜 0.2, 0.8, 1.4, 2.0가 아니라 numeric(0) 가 뜨는지 잘 모르겠습니다 어디를 어떻게 고치면 될까요

library(numbers)

z = c(-1.0, -0.7, -0.4, -0.1, 0.2, 0.5, 0.8, 1.1, 1.4, 1.7, 2.0, -0.7, -0.7, -0.4, -0.4, -0.4, -0.4, -0.4, -0.1, -0.1, -0.1, -0.1, 5.5, -3.8)

nIndex = which(z %% 0.2 == 0 & z >= 0)

mod(c(-5:5), 5)

mod(abs(z), 0.2)

-1.0 / 0.2


z[nIndex]


# 제가 문제
# y=sin(3x)/(3x) x의 범위는 [-6,6]
# x=seq(from=-6,to=6,by=0.01)
# y=sin (3*x)/(3*x)
# plot(x,y,type="l")
# 짯는데
# 이렇게나왔거든여
# 근데 여기서 13개 점밖에 되지않다고하는데 이게 무슨뜻인가요?

x = seq(from = -6, to = 6, by = 1)
y = sin(3 * x) / (3 * x)

plot(x, y, type = "p")
points(x, y, type = "l")

# 사진에 나온 프로그램을 다운받아야되는데 뭐가 뭔지 모르겠는데 알려주세요ㅠㅠ

install.packages(c("readxl", "dply", "rgdal", "ggplot2", "ggmap", "rgeos", "raster", "rjava", "RJDBC", "rvest"))

# 1에서 100까지의 7의 배수의 합을 어떻게 구하나요? 7의 배수 나열하는 방법은 알겠는데 합을 ss로 지정한 그 다음에 어떻게 프로그래밍을 해야할 지 모르겠어요.

nVal = seq(1, 100)

nIndex = which(nVal %% 7 == 0)

nVal[nIndex]

sum(nVal[nIndex], na.rm = TRUE)

# 1)에서 만든 행렬을 ad 변수에 저장하고 제일 오른쪽에 매출 데이터(변수명: sales)를 추가하려고 한다. 매출은 1200, 1500, 2100 이다. R에서 cbind()/rbind()를 이용하여ad에 sales를 추가하고 다시ad에 저장 하세요.

ad = data.frame(
  facebook = c(900, 1200, 1900)
  , instagram = c(500, 800, 1800)
  , blog = c(400, 600, 700)
)

ad

ad = cbind(ad, data.frame(sales = c(1200, 1500, 2100)))

ad

#
library(EnvStats)

EnvStats::cdfPlot(param.list = list(mean = 5, sd = 4), main = "")

legend("topleft", legend = c("N(5, 4)"),
       col = c("black"), lwd = 3 * par("cex"))

title("CDF Plots for Normal Distributions")

#

# dfData = data.frame(
#     Age = c(22, 25)
#     , Name = c("James", "Mathew")
#     , Gender = c("M", "M")
# )


# R 프로그램에 prophet 설치하면 cran 미러를 선택하라 해서 cloud, seoul, ulsan 다 해봤는데 자꾸 에러가 뜨네요 도와주세요 정말 하나도 몰라요..

install.packages("prophet")

library(prophet)

# 2)번 x 크기순으로 20개씩 평균 구하는거까진 했는데요 그 이후 그래프를 어떻게 그려야할지 모르겠네요..x와 y의 길이가 다르다고 오류뜨네요. 어떤식으로 코드를 짜야할까요?
# 3)번도 접근방법을 모르겠네요..

library(dplyr)

dfData = data.frame(
  x = sample(100) / 100.0
  , y = sample(100)
)

dfData %>%
  dplyr::arrange(x) %>%
  dplyr::mutate(type = x / 20
  )

plot(dfData$x, dfData$y)

# 이렇게 떠서 재설치도 몇번해봤는데 안됩니다ㅠ 어떻게 해아하나요

# 대부분의 경우 이전 버전의 Windows 7을 사용할 때 발생된 오류입니다.
# 따라서 일반적으로 OS 업데이트를 설치하는 것이 추천드리나 그렇지 않은 경우 해당 OS에 맞는 Rstudio 사용하시면 됩니다 (링크 참조).
# https://rstudio.com/products/rstudio/older-versions/


# (예제 17) x <- c(1,2,3,4,5)으로 점그래프(산점도)를 3개 그리세요.
# 1) pch=18을 붉은 색(col="red")과 크기(cex=2)를 2로 하는 산점도
# 2) 문자 pch=15:19, 5개의 칼라 col= 1:5, 크기(cex=2)를 2로 하는 산점도
# 3) 문자 pch=15:19, 2개의 칼라 col= 1:2, 크기(cex=2)를 2로 하는 산점도(리사이클 룰(recycling rule): 벡터의 길이가 짧은 쪽은 반복해서 사용하는 규칙

x <- c(1, 2, 3, 4, 5)

plot(x, pch = 18, col = "red", cex = 2)
plot(x, pch = 15:19, col = 1:5, cex = 2)
plot(x, pch = 1:2, col = 1:5, cex = 2)

# CAD. DEVILLE       A   11385 14 3 3 4.0 31.5 20 4330 221 44 425 2.28
# CAD. ELDORADO      A   14500 14 2 2 3.5 30.0 16 3900 204 43 350 2.19
# CAD. SEVILLE       A   15906 21 3 3 3.0 30.0 13 4290 204 45 350 2.24
# CHEV. CHEVETTE     A    3299 29 3 3 2.5 26.0  9 2110 163 34 231 2.93
# CHEV. IMPALA       A    5705 16 4 4 4.0 29.5 20 3690 212 43 250 2.56
# 원데이터가 다음과 같이 되어있을 때
# 맨 앞에 문자열 변수를 공백없이 만들려고 앞의 세글자만 가져오려고 합니다.
# 어떻게 코드를 입력하면 될까요? 제발 도와주세요

library(stringr)

dfDtata = data.frame(
  sType = c("CAD. DEVILLE       A   11385 14 3 3 4.0 31.5 20 4330 221 44 425 2.28"
    , "CAD. ELDORADO      A   14500 14 2 2 3.5 30.0 16 3900 204 43 350 2.19"
  )
)

stringr::str_sub(dfDtata$sType, 1, 3)

# R을 거의 사용해 본 적 없는 초보자를 대상으로
# 몬티홀 문제 시뮬레이션 R 코드와 상세한 설명 부탁드려요.
# 내공 400 겁니다.

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
X = runif(10000, min = 0, max = 1)  # 난수 생성 (모집단 생성)

hist(X)  # 모집단의 빈도분포
cat(mean(X), var(X), sd(X), "\n")  # 모집단의 평균/분산/표준편차


################################################### 표본평균집단에 대해서 ###########################################################
for (i in c(10, 50, 100, 250)) {

  DO = 100000    # Number of repetition
  N = i          # Number of sample
  # N = 30

  ## 비복원 추출(무작위 정렬) : 한번 뽑은 것을 다시 뽑을 수 없는 추출
  Sort = lapply(1:DO, function(i) sample(X, N, replace = F))
  Sort_mean <- mapply(mean, Sort)

  ## FIG
  # nf <- layout(matrix(c(1,1),1,byrow=T), c(1,1), c(1,1)) ; layout.show(nf) ; par(mar=c(5,5,5,5)) ; par(cex=1.0)
  hist(Sort_mean, breaks = 50, xlab = "Sample Distribution Mean", xlim = c(0.3, 0.7), col = "light grey", border = "grey", xaxs = "i", yaxs = "i",
       main = paste0("Central Limit Theorem : number of sample = ", N, ", number of repetition = ", sprintf("%d", DO)))

  XX1 = mean(Sort_mean) + sd(Sort_mean)
  XX2 = mean(Sort_mean) - sd(Sort_mean)
  XX3 = mean(Sort_mean) + 2 * sd(Sort_mean)
  XX4 = mean(Sort_mean) - 2 * sd(Sort_mean)
  XX5 = mean(Sort_mean) + 3 * sd(Sort_mean)
  XX6 = mean(Sort_mean) - 3 * sd(Sort_mean)
  YY = max(hist(Sort_mean, breaks = 50, plot = F)$counts)
  lines(c(mean(Sort_mean), mean(Sort_mean)), c(0, YY), lty = 1, col = 4); text(mean(Sort_mean), YY / 2, "Mean")
  lines(c(XX1, XX1), c(0, YY), lty = 1, col = 2); text(XX1, YY / 2, "+1σ")
  lines(c(XX2, XX2), c(0, YY), lty = 1, col = 2); text(XX2, YY / 2, "-1σ")
  lines(c(XX3, XX3), c(0, YY), lty = 1, col = 2); text(XX3, YY / 2, "+2σ")
  lines(c(XX4, XX4), c(0, YY), lty = 1, col = 2); text(XX4, YY / 2, "-2σ")
  lines(c(XX5, XX5), c(0, YY), lty = 1, col = 2); text(XX5, YY / 2, "+3σ")
  lines(c(XX6, XX6), c(0, YY), lty = 1, col = 2); text(XX6, YY / 2, "-3σ")
  lines(c(max(Sort_mean), max(Sort_mean)), c(0, YY), lty = 1, col = 3); text(max(Sort_mean), YY / 2, "Max")
  lines(c(min(Sort_mean), min(Sort_mean)), c(0, YY), lty = 1, col = 3); text(min(Sort_mean), YY / 2, "Min")
}

# r언어 데이터프레임 airquality
# 여기서 1,2가 아닌 3월 1일, 7월 7일 형태로 출력하려면 어떻게 해야 하나요?

library(dplyr)

air = data.frame(airquality)

air %>%
  dplyr::filter(
    (Month == 3 & Day == 1) | (Month == 7 & Day == 7)
  )


# 과제 중에 스크립트를 논리형으로 변환하여 z_logic에 저장하라고 하는데 무슨 말인가요?

nVal = c(94, NA)

z_logic = as.logical(nVal)

z_logic

# r studio를 이용해서 정적분과 함수의 극한값 구하는 법 좀 알려주세요ㅠㅠㅠㅠ
# 아님 잘 설명해주는 유튜브 영상같은 게 있으시다면 추천 부탁드립니다

# 정적분
fn = function(x) { x^2 + 4 * x + 1 }

integrate(fn, lower = 0, upper = 3)


# 그 밑에부분을 다 실행시켜보면 하나같이
# Error in empty(.data) : 객체 'tips'를 찾을 수 없습니다
# 이런 오류가 계속 뜹니다ㅠ
# 어디가 잘못된걸까요?

# 설치 시에 발생된 문제로 판단됩니다.

# plyr 라이브러리 삭제
unlink(paste0(Sys.getenv('R_LIBS_USER'), '/plyr'), recursive = TRUE)

# 재 설치
install.packages('plyr')


# d<-data.frame(x=1:10,fac=fac,z=10:20)Error in data.frame(x = 1:10, fac = fac, z = 10:20) :   arguments imply differing number of rows: 10, 11
# 이게 대체 무슨 뜻인가요ㅜ
# 오류가 자꾸 떠요

fac = 30:40

d = data.frame(
  x = c(1:11)
  , fac = c(fac)
  , z = c(10:20)
)

d

# x<-1:100
# sum(x>50)
# 이렇게 출력해봤는데
# 50이라는 답이 나왔습니다.
# 그런데 왜 이렇게 나오는지 이해가 안가서 질문합니다!
# 자세히 설명부탁드려요!

x <- 1:100

nIndex = which(x > 50)

nIndex

# 객체들의 합
sum(x[nIndex], na.rm = TRUE)


# R studio 한글을 써서 저장시킨뒤 다시 키면 한글이 모두 '?'로 표시되어 있습니다 .
# utf-8로 설정되어있구요.. 어떤게 문제일까요

# 이 경우 EUC-KR로 저장한 소스 코드를 UTF-8로 변환 시 발생되는 문제입니다.
# 따라서 EUC-KR로 설정한 후 소스 코드를 복사 후 UTF-8로 다시 붙여넣기해야 합니다.


# 다음의 문제를해결하기 위한 R 코드를 작성하시오 (which 함수 사용)

library(dplyr)

# iris 데이터셋에서 꽃잎의 길이 (Petal.length)가 가장 큰 관측값 (행)의 내용을 보이시오.
iris %>%
  dplyr::filter(Petal.Length == max(Petal.Length, na.rm = TRUE))

# iris 데이터셋에서 꽃잎의 폭 (Petal.Width)이 0.3~0.4 사이인 관측값 (행)들의 내용을 보이시오.
iris %>%
  dplyr::filter(between(Petal.Width, 0.3, 0.4))

# v1에서 짝수에 대해서만 2를 곱하여 저장하시오.
v1 <- 51:90

nIndex = which(v1 %% 2 == 0)

nVal = v1[nIndex] * 2

nVal


# v1에서 7의 배수들을 제거한 후 v1의 내용을 출력하시오.
v1 <- 51:90

nIndex = which(v1 %% 7 != 0)

nVal = v1[nIndex]

nVal

# 다음은 영업 사원들의 판촉 전화 건수와 판매 건수이다.

dfData = data.frame(
  nKey = c(18, 40, 20, 30, 10, 10, 20, 21, 24, 30)
  , nVal = c(42, 66, 42, 54, 30, 30, 46, 49, 43, 63)
)

# R을 이용하여 판촉 전화 건수의 평균과 분산을 구하는 프로그램을 작성하고 프로그램과 결과를 캡쳐하여라.

mean(dfData$nKey, na.rm = TRUE)
var(dfData$nKey, na.rm = TRUE)

# R을 이용하여 판촉 전화 건수의 히스토그램을 그리는 프로그램을 작성하고 프로그램과 결과를 캡쳐하여라.

hist(dfData$nKey)

# R을 이용하여 산점도를 그리고 상관계수를 구하는 프로그램을 작성하고 프로그램과 결과를 캡쳐하여라.

plot(dfData$nKey, dfData$nVal)
cor(dfData$nKey, dfData$nVal)

# r프로그래밍 오류 질문이요 ㅠㅠ
# 이거 왜 오류가 뜨나요 ㅠㅠ 저 파일을 불러오기 했는데 자꾸 저렇게 뜨고 다른게 안되요 ㅠㅠ

data = read.csv(file = "INPUT/csv exam.txt", sep = " ", header = FALSE)

data

#

library("KoNLP")

# 1. 1에서 10000 사의의 수에서 짝수만을 가지고 벡터 객체 생성하시오. 그리고 벡터 객체의 모드를 확인하시오

val = 1:10000

index = which(val %% 2 == 0)

val[index]


# 2. (4.2, 6.3, 5.1, 7.2, 10.1, 9.9, 4.0, 15.5)를 숫자형 벡터 객체 만드시오.
# 그리고 이 숫자형 벡터 객체를 사용하여 세번째 수와 여섯번째 수를 결측치로 바꾸시오. 그리고 벡터 객체의 모드를 확인하시오(단 R에서의 결측치 처리는 NA로 해야 합니다. 예를 들어 y<-NA 로 하면 y스칼라가 결측치입니다.

val2 = c(4.2, 6.3, 5.1, 7.2, 10.1, 9.9, 4.0, 15.5)
val2[3] = NA
val2[6] = NA

val2

# 예를 들어서, 초단위로 부하율 값이 나타내는 CSV파일이 있는데
# 이것을 초단위의 데이터 평균내서 1분단위로 나타내는 데이터 값을 새로 추가하고싶을때는 어떻게 코딩을 해야하나요 ?

library(dplyr)

dfData = data.frame(
  Time = c("45:55.4", "45:56.4", "46:57.5", "46:57.8", "47:59.3")
  , Load = c(24, 65, 95, 65, 95)
)

dfDataL1 = dfData %>%
  dplyr::mutate(
    dtTime = readr::parse_time(as.character(Time), format = "%M:%OS")
    , sMinute = lubridate::minute(dtTime)
    , sSec = lubridate::second(dtTime)
  )

dplyr::tbl_df(dfDataL1)

dfDataL2 = dfDataL1 %>%
  dplyr::group_by(sMinute) %>%
  dplyr::summarise(nMeanLoad = mean(Load, na.rm = TRUE))

dplyr::tbl_df(dfDataL2)

# 10명의 몸무게를 저장한 벡터가 다음과 같을 때 질문을 해결하기 위한 R코드를 작성하시오.

weight <- c(69, 50, 55, 71, 89, 64, 59, 70, 71, 100)

# (1) 몸무게가 가장 큰 값은 몇 번째에 있나?
which(weight == max(weight, na.rm = TRUE))

# (2) 몸무게가 가장 작은 값은 몇 번째에 있나?
which(weight == min(weight, na.rm = TRUE))

# (3) 몸무게가 61에서 69사이인 값들은 몇 번째에 있나?
which(61 <= weight & weight <= 69)

# (4) 몸무게가 60 이하인 값들만 추출하여 weight.2에 저장하고 내용을 보이시오.
weight.2 = which(weight <= 60)
weight.2

# R csv불러올때 한글 깨지는데 어떻게 해야할까요? 한글을 영어로 불르면 잘 불러와지는데 한글로 하면 깨져서 와요ㅠㅠㅠㅠ

data = read.csv("INPUT/출산율.csv", encoding = "utf-8")
data = read.csv("INPUT/출산율.csv", encoding = "cp949")
data = read.csv("INPUT/출산율.csv", encoding = "euc-kr")

# R데이터 y=2x^+5x+10에 대해 x가 각각 6,8,10일때 y의 값을 각각 구한다.'
# 이문제인데 자꾸 오류가 나요 ㅠ ㅠ

x = c(6, 8, 10)
y = (x^2) + (5 * x) + 10
y

#
library(evir)

data(danish)

evir::records(danish)

#
data = scan("INPUT/csv exam.txt", what = "character")
data

dataL1 = read.csv(file = "INPUT/csv exam.txt", sep = " ", header = TRUE)
dataL1

# 그래서 이것을 다시 받아서 출력하려고 2번째 사진같이 입력하여 출력하였는데 보시는것처럼 '유효하지 않은 멀티바이트 문자열 3입니다' 라는 말과 함께 출력이 되질 않아 계속 시도해봐도 도저히 해결을 못하겠어서 질문드립니다..ㅠㅠ

data = read.csv(file = "INPUT/csv exam.txt", sep = " ", header = TRUE)
data

# 압축 파일 'Rstat.zip'의 압축을 풀었는데 R에서 R stat라고 불리는 패키지가 없다고 해서 질문드립니다.
# 패키지를 설치하려면 어떻게 해야 하는 건가요?
# 빠르게 답변해주시면 감사하겠습니다.

# library(stats)

# stats::

# (1:10)^c(1,2)를 계산하면
# [1] 1 4 3 16 5 36 7 64 9 100이 나오는데 어떤 원리로 계산된건가요?

(1:10)^c(1, 2)

# 좌측 1  ^ 우측 1 = 1
# 좌측 2  ^ 우측 2 = 4
# 좌측 3  ^ 우측 1 = 3
# 좌측 4  ^ 우측 2 = 16
# 좌측 5  ^ 우측 1 = 5
# 좌측 6  ^ 우측 2 = 36
# 좌측 7  ^ 우측 1 = 7
# 좌측 8  ^ 우측 2 = 64
# 좌측 9  ^ 우측 1 = 9
# 좌측 10 ^ 우측 2 = 100

# 난수 구할 때 U(-3.3) 이랑 DE(0.1) 구하려면 어떻게 쳐야 하나요ㅠㅠ

iNumber = 1000000

# 0-1 사이 난수 구하기
nVal = runif(iNumber)
hist(nVal)

# 정규분포에서 난수 생성
nVal2 = rnorm(n = iNumber, mean = 0, sd = 1)
hist(nVal2)

# R에서 제공하는 swiss 데이터셋은 스위스 47개 주의 경제 자료를 포함하고 있다. 이 데이터셋에 대해서 다음 문제를 해결하기 위한 코드를 작성하시오.

library(dplyr)

dfData = swiss

# (1) 데이터셋의 요약 정보를 보이시오.
summary(dfData)

# (2) 남성의 농업인 비율(Agriculture)이 가장 높은 주를 보이시오.
dfData %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(Agriculture == max(Agriculture, na.rm = TRUE))

# (3) 남성의 농업인 비율(Agriculture)을 내림차순으로 정렬하여 주의 이름과 함께 보이시오.
dfData %>%
  tibble::rownames_to_column() %>%
  dplyr::arrange(desc(Agriculture))

# (4) 카톨릭 신자의 비율(Catholic)이 80% 이상인 주들의 남성의 농업인 비율(Agriculture)을 보이시오.

dfData %>%
  tibble::rownames_to_column() %>%
  dplyr::filter(Catholic >= 80)

# [R]markdown을 word로 생성하는데 오류가 뜨네요ㅠㅠ
# output file: -2.knit.mdUnknown option -2.Unknown option -..Unknown option -u.Try pandoc.exe --help for more information.에러: pandoc document conversion failed with error 2실행이 정지되었습니다
# 혹시 어떻게 해결해야하는지 아시나요??ㅠㅠ

library(rmarkdown)
render("input.Rmd", word_document())

# specify an option for syntax highlighting
render("input.Rmd", word_document(highlight = "zenburn"))

# 다음 문제를 해결하기 위한 R 코드를 작성하시오. ( which( ) 함수 사용 ) (4 points)

# (1) iris 데이터셋에서 꽃잎의 길이(Petal.Length)가 가장 큰 관측값(행)의 내용을 보이시오.
index = which(iris$Petal.Length == max(iris$Petal.Length, na.rm = TRUE))

iris[index,]

# (2) iris 데이터셋에서 꽃잎의 폭(Petal.Width)이 0.3~0.4 사이인 관측값들의 내용을 보이시오.
index = which(0.3 <= iris$Petal.Width & iris$Petal.Width <= 0.4)

iris[index,]


# 4번문제 어떻게 푸나요? ㅠㅠ 제발 알려주세요

x = c(6, 8, 10)
y = (2 * (x^2)) + (5 * x) + 10

y


# 1에서 50 사이의 정수 중에서 3이나 5의 배수를 순서대로 모두 찾아 v3벡터를 만드시오.

var = 1:50

index = which((var %% 3 == 0) | (var %% 5 == 0))

v3 = var[index]

v3

# 0-5
# 5-10
# 10-15
# ~~
# 45-55
# 이렇게 10개의 표가 5의 간격으로 있을때 이거를 R프로그램을 이용해서 도수분포표를 만드는 명령어 좀 알려주세요..

library(tidyverse)

data = read.csv(file = "INPUT/zipIncome.csv")

var = data %>%
  select(MeanEducation, MeanHouseholdIncome) #pick the variable

# set up cut-off values
breaks = seq(0, 50, 5)

# specify interval/bin labels
tags = paste0("[", seq(0, 45, 5), "-", seq(5, 50, 5), ")")

# bucketing values into bins
group_tags = cut(var$MeanEducation, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = tags)

# inspect bins
summary(group_tags)

ggplot(data = as_tibble(group_tags), mapping = aes(x = value)) +
  geom_bar(fill = "bisque", color = "white", alpha = 0.7) +
  stat_count(geom = "text", aes(label = sprintf("%.4f", ..count.. / length(group_tags))), vjust = -0.5) +
  labs(x = 'mean education per house') +
  theme_minimal()

#
# 1.R 스튜디오 에서 log(1:5)  함수 입력하면
# 밑이 e? 2? 인  함수 값으로 계산 되는건가요 ????

log(1:5)


# 2.exp(1:5)  <<< 이거 뭔지 잘 모르겠어요 자세히 설명좀요 , 지수함수 인가요

exp(1:5)
# 2.718282 ^ (1:5)

# R언어 소스코드 및 실행결과좀 부탁드려요 처음이라 잘 모르겠네요..ㅜ
# 5) 1부터 100사이에 있는 정수를 생성하고 그 결과를 x에 할당하라.

x = 1:100

x

# 6) 5)에서 생성한 백터 x의 원소 수를 구하라.

length(x)

# 7) 5)에서 생성한 백터 x의 원소들의 합, 평균, 표준편차를 구하고, 그 결과를
# x_sum, x_mean, x_std변수에 저장하라.(sum(), mean(), sd() 함수를 이용하라.)

x_sum = sum(x, na.rm = TRUE)
x_mean = mean(x, na.rm = TRUE)
x_std = sd(x, na.rm = TRUE)

x_sum
x_mean
x_std

# 벡터의 크기가 9이고 벡터의 원소 중 처음 다섯 개는 1부터 10까지 홀수 값, 그 다음 네 개는 8부터 1까지 짝수 값을 갖는 벡터를 생성하는 법좀 가르쳐쥬세요 r프로그래밍 ㅠㅠ
# 벡터의 모양은 1 3 5 7 9 8 6 4 2 가 되도록!


evenVar = 1:8
oddVar = 1:10

evenIndex = which(evenVar %% 2 == 0)
oddIndex = which(oddVar %% 2 == 1)

val = c(oddVar[oddIndex], rev(evenVar[evenIndex]))

val

# Rstudio에서 C:/R/csv1.csv R폴더 안의 csv1.csv파일 여는 방법좀 알려주세요ㅠㅠ 사진처럼 하니까 안돼요

setwd("E:/02. 블로그/지식iN")

data = read.csv("INPUT/출산율.csv")

data

# 실행이 된 거 아닌가요?근데 왜 함수를 찾을 수 없다고 뜨는건가요?
library(readxl)

readxl::read_excel

# factor를 이제 막 배우고 있는데요
# 1을 하나, 2를 둘, 3을 셋 으로 바꾸려고 하는데
# 왜 안먹히는지 잘 모르겠어요 ㅠㅠㅠㅠ 뭔가요 제발알려주세요

library(plyr)

a = c(1, 2, 3, 3, 2, 1)

faData = factor(data)

rsData = plyr::mapvalues(faData, from = c(1:3), c("하나", "둘", "셋"))

rsData

table(rsData)

# csv 파일을 불러와서 하는데 행의 이름이 한글로 되어 있습니다.
# 그래서 하나하나 이렇게 해보는데 어떤 경우에는 저런 메시지가 뜨고 어떤 경우는 그냥 정상으로 넘어갑니다.
# 이 메시지의 의미는 무엇이며 왜 이런것이 뜨는지 알고 싶습니다.

data = read.csv("INPUT/출산율.csv", encoding = "utf-8")
data = read.csv("INPUT/출산율.csv", encoding = "cp949")
data = read.csv("INPUT/출산율.csv", encoding = "euc-kr")

# 2. 1의 조건에서 sepal.width와 sepal.length의 중앙값과 species의 종류별 개수를 구하시오.
# 1번은 됐는데 2번이 안 되네요ㅜㅜ 2번할때 어떤 함수(median, subset?)와 수식, 구하는 법을 알려주세요

median(iris$Sepal.Width, na.rm = TRUE)
median(iris$Sepal.Length, na.rm = TRUE)

table(iris$Species)

#
# R에서 while이랑 repeat 각각 중첩하여 사용해서 이 삼각형 나오게 하고 싶은데 어떻게 해야하나요ㅜ C언어나 자바, 파이썬으로 하는 건 많이 나오는데 R로 어떻게 하는지는 안나오네요ㅜㅜㅜㅜ
# R 고수님들 부탁드립니다!

i = 1
number = 10
while (i < number) {
  cat(rep("*", times = number - i), "\n")

  i = i + 1
}

# 1.PC에 R 프로그램을 설치하고 아래 계산들을 한 후 R consol화면을 캡쳐하여 한글파일로 제출하라.

# 1+2*3+4*5+6을 계산하라
1 + 2 * 3 + 4 * 5 + 6

# (1+2)*3+4*(5+6)을 계산하라
(1 + 2) * 3 + 4 * (5 + 6)

# ((1+2*3)+4)*5+6을 계산하라
((1 + 2 * 3) + 4) * 5 + 6

# 2.벡터의 크기가 9이다. 벡터의 원소 중 처음 다섯 개는 1부터 10까지 홀수 값, 그 다음 네 개는 8부터 1까지 짝수 값을 갖는 벡터를 생성하라.
# 벡터의 모양은  1 3 5 7 9 8 6 4 2 가 된다.
evenVar = 1:8
oddVar = 1:10

evenIndex = which(evenVar %% 2 == 0)
oddIndex = which(oddVar %% 2 == 1)

val = c(oddVar[oddIndex], rev(evenVar[evenIndex]))

val

# 문제를 어떻게 푸는지 모르겠어요ㅠ
# 1. x는 1부터 100까지의 수 중 3의 배수만 포함하고 y는 1부터 100까지의 수 중 5의 배수만 포함하는 벡터를 만들어라.

var = 1:100

index = which((var %% 3 == 0) | (var %% 5 == 0))

var[index]

# 2. x와 y의 교집합을 구하고, 해당 교집합에 포함된 수를 모두 합한 값을 구하여라.

index = which((var %% 3 == 0) & (var %% 5 == 0))

var[index]

sum(var[index], na.rm = TRUE)

# 각 A,B,C,D의 4명의 신입사원 데이터를 토대로 상자그림을 그려야합니다
# 완성된 상자그림, 프로그램에 사용된 코드와 실행결과도 같이 부탁드립니다

data = data.frame(
  A = c(2, 4, 4, 3, 2, 3, 4, 5, 1, 5)
  , B = c(3, 4, 3, 4, 5, 4, 4, 5, 5, 4)
  , C = c(3, 3, 3, 4, 4, 3, 2, 3, 3, 2)
  , D = c(3, 2, 2, 3, 4, 3, 4, 5, 5, 2)
)

boxplot(data)

# 제가 Rcmdr을 실행시켜야되는데..
# 저렇게 뜹니다ㅠㅠㅠ 방법좀알려쥬세요

library(Rcmdr)

Rcmdr::activateMenus()

# 어떻게 하는지 모르겠어요! 자꾸 오류떠서.. 하는 법 알려주세요!
x = c(6, 8, 10)
y = (2 * (x^2)) + (5 * x) + 10
y

# r스튜디오로 워드클라우드를 만들었는데 plot 창에 워드클라우드가 전체가 나오지않고 일부만 잘려 나와요 어떻게 해결하나요??

#
v1 <- 51:90

v1

index = which(v1 %% 2 == 0)

v2 = 2 * v1[index]

#
png(file = paste0("Img01.png"), 1200, 600)

plot(1:10)

dev.off()

#R스튜디오 설치했는데 맨 처음의 4분할 화면이 안 뜨네요ㅜㅜ
# 메모장 하나만 켜집니다
# 프로그램 두개 다 설치했습니다 어디서 에러가 난 걸까요?

# 다음의 자료는 대학교 2020학년도에 기초통계학을 수강한 20명의 시험결과이다.
# 69 78 89 94 50 93 98 81 73 88 78 84 68 90 83 87 81 74 58 69

# (1)계급간격이 5일 때, 도수분포표를 작성하시오.

# set up cut-off values
breaks = seq(0, 100, 5)

# specify interval/bin labels
tags = paste0("[", seq(0, 95, 5), "-", seq(5, 100, 5), ")")

data = c(69, 78, 89, 94, 50, 93, 98, 81, 73, 88, 78, 84, 68, 90, 83, 87, 81, 74, 58, 69)

groupTags = cut(data, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = tags)

summary(groupTags)

# (2)히스토그램을 작성해 보시오.
# 1번 문제 도수분포표는 해결했는데, 히스토그램을 도저히 모르겠네요..ㅠㅠ

ggplot(data = as_tibble(groupTags), mapping = aes(x = value)) +
  geom_bar(fill = "bisque", color = "white", alpha = 0.7) +
  stat_count(geom = "text", aes(label = sprintf("%.2f", ..count.. / length(group_tags))), vjust = -0.5) +
  labs(x = 'score per group') +
  theme_minimal()

#
# R프로그램(R스튜디오아님)으로 그래프를 그릴려는데 명령어를 잘 모르겠습니다,,
# 이게 히스토그램을 그리는 명령어가 맞나요?
# 이걸 입력하고 엔터를 치면 +가 나오고 그림은 나오지 않습니다..
# 다른 블로그에서는 새 스크립트를 누르고 입력하라던데 그래도 그래프는 나오지않더군요
# ㅠㅠ도와주세요

score = c(69, 78, 89, 94, 50, 93, 98, 81, 73, 88, 78, 84, 68, 90, 83, 87, 81, 74, 58, 69)

hist(score, breaks = seq(50, 100, 5), prob = TRUE)

# R 프로그래밍 d에서 3의 배수 만 출력 하세요.
# d <-100:200 기준

var = 100:200

index = which(var %% 3 == 0)

var[index]

#
# install.packages("readx1") 이걸 깔려고 하는데 오류 메세지로
# install.packages("readx1")
# Installing package into ‘C:/Users/ksmin/Documents/R/win-library/3.6’
# (as ‘lib’ is unspecified)
# Warning in install.packages :
# package ‘readx1’ is not available (for R version 3.6.3)
# 이렇게 떠요. 어떻게 해야 해결할 수 있나요?

library(readxl)

readxl::anchored()

#
# 다음과 같은 name벡터에서 성과 이름을 분리하여 각각 sname, fname에 따로 저장하라.
# name <- c("Lee Nayoung", "Song Hyekyo", "Jun Jihyun", "Kime Tahee")
# 답변해주신다면 정말 감사드립니다.

library(tidyr)

name <- c("Lee Nayoung", "Song Hyekyo", "Jun Jihyun", "Kime Tahee")

data = as.data.frame(name) %>%
  tidyr::separate(name, c("sname", "fname"), sep = ",")

data

#
# group<-read.table("C:/myfiles/dataset.txt", header=T)
# group
# names(group)
# plot(group$X, group$Y, pch=11, cex=2, col="red")
# 여기서 산점도의 점을 별모양으로 하고 그 안을 빨간색으로 채우고 싶은데 어떻게 입력해야 하나요


group = data.frame(
  X = 1:10
  , Y = 1:10
)

plot(group$X, group$Y, pch = 24, cex = 2, bg = "red", col = "red")
points(group$X, group$Y, pch = 25, cex = 2, bg = "red", col = "red")

# R언어 도와주세요 ... 급해요ㅠ
#연습문제 12
#x와 y의 공분산을 계산하시오.

#공분산
x <- c(420, 610, 625, 500, 400, 450, 550, 650, 480, 565)
y <- c(2.80, 3.60, 3.75, 3.00, 2.50, 2.70, 3.50, 3.90, 2.95, 3.30)
cov(x, y)

#(2)x와 y의 상관계수를 계산하시오.

#상관계수
cor(x, y)

#결정계수
R2 = cor(x, y)^2

#(3)최소자승법에 의한 직선식이 y=a+bx로 나온다고 할 때, a와 b의 값을 결정하시오.

#최소자승법
# win.graph(9, 6)
plot(formula = y ~ x,
     main = "homework",
     xlab = "엑스",
     ylab = "와이",
     xlim = c(420, 565),
     ylim = c(2.80, 3.30),
     col = "red")
grid()

line <- lm(formula = y ~ x)
abline(line)

# ▲▲▲▲ 여기까지는 잘 되는 것 같아요 ㅜㅜ
# 그런데 밑에 직선식이 안돼요 도와주세요 ㅠㅠ
# ▼▼▼▼ 밑에 식을 이용해서 답을 구해주세요 ..

text(440, 3.3,
     labels = paste0("Y = ", round(line$coef[[1]], 3), " + (", round(line$coef[[2]], 3),
                     ") * X"), col = 2)
text(430, 3.25, labels = paste0("R2 = ", round(R2, 4)), col = "red")

# R studio 로 자료구조 pop,push 구현 어떻게 하나요ㅠㅠ
# 간단하게라도 부탁드립니다!

library(flifo)


# push
push = function(x, values) (assign(as.character(substitute(x)), c(x, values), parent.frame()))

# pop
pop = function(x) (assign(as.character(substitute(x)), x[-length(x)], parent.frame()))

# example
z = 1:3

push(z, 4)

pop(z)
pop(z)

# V1~V5 까지의 변수를 합한후 평균내서 V10에 저장하려는데 V10<-(V1+V2+V3+V4+V5)/5 로 했는데
# 요인(factors)에 대하여 의미있는 ‘+’가 아닙니다. 라는 오류가 뜨네요 어떻게 해결할수 있을까요
V1 = factor(1)
V2 = factor(2)
V3 = factor(3)
V4 = factor(4)
V5 = factor(5)

as.numeric.factor = function(x) { as.numeric(levels(x))[x] }

as.numeric.factor(V1)

V10 <- sum(
  as.numeric.factor(V1)
  , as.numeric.factor(V2)
  , as.numeric.factor(V3)
  , as.numeric.factor(V4)
  , as.numeric.factor(V5)
  , na.rm = TRUE
) / 5.0

V10

# R프로그래밍으로 역행렬 구하기 , 오류가 나옵니다.

A <- matrix(nrow = 3, ncol = 3)
A

A[1, 1] <- 1 / sqrt(3)
A[1, 2] <- 1 / sqrt(3)
A[1, 3] <- 1 / sqrt(3)
A[2, 1] <- 1 / sqrt(2)
A[2, 2] <- 1 / sqrt(2)
A[2, 3] <- 0
A[3, 1] <- 1 / sqrt(6)
A[3, 2] <- 1 / sqrt(6)
A[3, 3] <- 2 / sqrt(6)

A

# 3X3이라는 A행렬을 만들었습니다.
# 그후에 역행렬을 얻기 위해 solve(A)를 해보았지만 결과가 오류가 나옵니다. 뭐가 잘못된건가요?
# 혹시나 해서 디터미넌트를 구해봤는데 0도 아니었습니다.

library(MASS)

MASS::ginv(A)

# X <- (1:10)*10
# 60이상인 값들의 평균을 구해야 하는데요
# mean 함수를 사용하고, 사용하지 않고 둘 다 구해야해요
# 도대체가 어떻게 하는지 모르겠어요 도와주세요

X <- (1:10) * 10

index = which(X >= 60)

mean(X[index], na.rm = TRUE)

# iris 자료로 그래프를 나타내려고 하는데 막대그래프로 나타내려고 하거든요ggplot2에서 종별 sepal.width sepal.length petal.width petal.length를 세로로? 세워서 어떻게 놓을까요..지금은 저렇게 일자로 점이 찍히는데

library(tidyr)
library(ggplot2)

data = tidyr::gather(iris, key, val, -Species)

ggplot(data = data, aes(x = Species, y = val, fill = key)) +
  geom_bar(stat = "identity", position = "dodge")

# R스튜디오 파이차트 변환하는중 요류 ....
# library(readxl)
# data <-read_excel("C:/scv123.xlsx")
# x <-c("456","510 ","1092","81","260","912","2075","162","1078","1035","810","200","550","490","469","429","1311","729","102","165","612","1520","850","660")
# label<-c("강남구","강동구","강북구","강서구","관악구","광진구","구로구","금천구","노원구","도봉구","동대문구","동작구","마포구","서대문구","서초구","성동구","성북구","송파구","양천구","영등포구","용산구","은평구","종로구","중구")

# pie(x,labels=label,main="서울특별시 구별 도서관수")
# Error in pie(x, labels = label, main = "서울특별시 구별 도서관수") :
#   'x'의 값은 반드시 양수이어야 합니다
# 파이차트로 변화하고  마지막 문장만 오류가 나네요.. 마지막 문장이 양수여야 한다디 ...먼말이죠 ..

x <- c(456, 510, 1092, 81, 260, 912, 2075, 162, 1078, 1035, 810, 200, 550, 490, 469, 429, 1311, 729, 102, 165, 612, 1520, 850, 660)

label <- c("강남구", "강동구", "강북구", "강서구", "관악구", "광진구", "구로구", "금천구", "노원구", "도봉구", "동대문구", "동작구", "마포구", "서대문구", "서초구", "성동구", "성북구", "송파구", "양천구", "영등포구", "용산구", "은평구", "종로구", "중구")

pie(x, labels = label, col = rainbow(length(label)), main = "서울특별시 구별 도서관수")

#
get_sun <- function(loc, st, ed) {

  if (!require(readxl)) {
    install.packages("readxl")
  }
  if (!require(httr)) {
    install.packages("httr")
  }
  library(readxl)
  library(httr)
  if (is.character(st) && length(grep("-", st)) != 0) {
    temp <- unlist(strsplit(st, "-"))
    sy <- as.numeric(temp[1])
    sm <- as.numeric(temp[2])

  } else {
    sy <- as.numeric(substr(st, 1, 4))
    sm <- as.numeric(substr(st, 5, 6))
  }

  if (is.character(ed) && length(grep("-", ed)) != 0) {
    temp <- unlist(strsplit(ed, "-"))
    ey <- as.numeric(temp[1])
    em <- as.numeric(temp[2])
  }else {
    ey <- as.numeric(substr(ed, 1, 4))
    em <- as.numeric(substr(ed, 5, 6))
  }

  URL_1 <- "https://astro.kasi.re.kr/life/sunmoon/excel?location="
  URL_2 <- URLencode(iconv(loc, to = "UTF-8"))
  URL_3 <- "&date="
  nt <- 1
  while (sy != ey || sm != em + 1) {
    URL_4 <- sy
    URL_5 <- ifelse(nchar(sm) == 1, paste0("0", sm), sm)
    URL <- paste0(URL_1, URL_2, URL_3, URL_4, URL_5)
    cat(URL, "\n")
    GET(URL, write_disk("temp.xlsx", overwrite = TRUE))
    if (is.null(nt)) {
      temp_sun <- data.frame(read_xlsx("temp.xlsx", skip = 1))
      temp_df <- rbind(temp_df, temp_sun[, c(1, 3, 5)])

    }
    else {
      temp_df <- data.frame(read_xlsx("temp.xlsx", skip = 1))[, c(1, 3, 5)]
      nt <- NULL
    }
    if (sm + 1 > 12) {
      sm <- 1
      sy <- sy + 1
    }
    else {
      sm <- sm + 1
    }
  }
  file.remove("temp.xlsx")
  temp_df <- cbind(temp_df, loc, stringsAsFactors = F)
  names(temp_df) <- c("날짜", "일출", "일몰", "지역")
  return(temp_df)
}

get_sun("서울특별시", "2017-10", "2018-06")


# https://astro.kasi.re.kr/life/sunmoon/excel?location=%EC%84%9C%EC%9A%B8%ED%8A%B9%EB%B3%84%EC%8B%9C&date=202003

# 100에서 200으로 구성된 벡터d를 생성하여~~~ d의 끝에서 10개의 값을 잘라내어 출력하시오 <<< 이게 문제인데
# d[-c(190:200)]를 대입하니 이상하게 나오더라구요. 혹시 틀린 코드인가요?

d = 100:200

endIndex = length(d)

result = d[(endIndex - 10):endIndex]

result

# 이부분에서 > for( jj in 1:4){ date.tank<-rbind(date.tank,te[[jj]])} 부분이 어떤식으로 되어있길래 위와 같은 것으로 print되는것인지 모르겠습니다. 자세히 알려주세요ㅠㅠㅠㅠ

date.te <- c('2018-03-01/01', '2019-03-02/04', '2017-03-01/04', '2015-03-02/05')

date.tank = NULL

for (jj in 1:length(date.te)) {

  te <- unlist(strsplit(date.te[jj], '/'))

  cat(te)

  date.tank <- rbind(date.tank, te)
}

#
# name=c("Kim", "Lee", "Park") 이 부분에서
# 왜 <- 안쓰고 =를 사용한건가요?
# name <- c(" ")
# age <- c( ) 한 다음
# x <- data.frame(name, age) 해야되는거 아닌가요?ㅠㅠ

# 안녕하세요. R프로그램을 처음 사용해보는 학생입니다.
# R프로그램에서 정규분포 곡선을 그릴때 문의사항이 있어 질문 올립니다.
# 사진과 같이 curve 명령문을 적으면 에러가 뜨는데 이유가 뭔지 궁금합니다 ㅜㅜ
# 그리고 에러가 안뜨게 정규분포 곡선을 추가하려면 어떤 식으로 적어야하나요?

y = c(47, 50, 48, 55, 60, 52, 53, 55, 48, 63)

hist(y, breaks = 46:63, freq = FALSE)
lines(dnorm(1:100, mean = mean(y), sd = sd(y)), col = "blue")

# R에 내재된 데이터인 "mtcars"를 이용해 누적막대그래프를 그리는 건데요.
# ggplot(mtcars,aes(x=factor(cyl)))+geom_bar(aes(fill=factor(gear)))
# 요렇게 해서 누적막대 그래프를 그리는 것을 성공했습니다. 사진은 아래와 같구요.
# 근데 궁금한게 제가 쓴 코드에서 fill이 무엇인지 알고 싶습니다.

# 여기서 fill은 기어 (gear)에 따른 색깔 표시 (범례 참조)를 의미합니다. 즉 fill을 넣지 않을 경우 기어에 따라 동일한 색으로 표시

library(ggplot2)

ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar()

# 1.R 패키지에 있는 'mtcars ’ 데이터를 이용하여 자동차 중량 (wt ) 대 연비
# (mpg) 의 plot 그래프를 그려라

library(ggplot2)
library(dplyr)

glimpse(mtcars)

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# 2.다음은 한 주 동안 상위 25 위까지 기록된 영화의 총 수입에 대한 자료
# 이다 단위 백만원 ). 이 자료에 대하여 밀도로 표시되는 히스토그램을
# 그려라
# 29.6 28.2 19.6 13.7 13.0 7.8 3.4 2.0 1.9 1.0 0.7 0.4 0.4 0.3 0.2

# 이 문제들이 오답인데요 왜 틀린지가 이해가 안되서 아시는분 그래프들 좀 주세요..ㅠ

data = data.frame(
  val = c(29.6, 28.2, 19.6, 13.7, 13.0, 7.8, 3.4, 2.0, 1.9, 1.0, 0.7, 0.4, 0.4, 0.3, 0.2)
)

ggplot(data, aes(x = val)) +
  geom_density(color = "darkblue", fill = "lightblue")

# 교수님께서 R만 사용하라고 하셔서 R studio는 못 쓰는 상황입니다.
# csv 파일을 불러오려 하는데 한 셀에 들어 있는 문자열 하나 빼고는 숫자입니다. 그런데 계속 Coffees라는 그 문자열이 계속 깨지는데 원인도 모르겠고 해결이 안되어 질문드립니다..ㅠㅠ 도대체 왜 자꾸 이런 현상이 발생하는 건가요?

# 이는 파일 읽을 경우 인코딩 타입을 미 설정하여 발생된 오류 입니다.
# 따라서 encoding를 설정해주시면 됩니다 (소스 코드 참조).

data = read.csv("INPUT/출산율.csv", encoding = "utf-8")
data = read.csv("INPUT/출산율.csv", encoding = "cp949")
data = read.csv("INPUT/출산율.csv", encoding = "euc-kr")

# 다음 코드를 이용해 지도를 그렸는데요 color을 white로 했는데 왜 핑크색으로 그려질까요? 제가 잘못하는거면 올바른 코드 아시는분 가르침 부탁드립니다.
ggplot() +
  geom_polygon(data = merge_result, aes(x = long, y = lat, group = group, fill = area, color = 'white'), alpha = .8) +
  labs(fill = "19년 시군별 경지면적")


ggplot() +
  geom_polygon(data = merge_result, aes(x = long, y = lat, group = group, fill = area), colour = "white", alpha = .8) +
  labs(fill = "19년 시군별 경지면적")


# anorexia Treat "Cont"로 정규성 검정하기
# 1) shapiro.test()로 를 하기

data = rnorm(100, mean = 5, sd = 3)

shapiro.test(data)

# 2) t.test() paired t-test .


# Data in two numeric vectors
# ++++++++++++++++++++++++++

# Weight of the mice before treatment
before = c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)

# Weight of the mice after treatment
after = c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)

# Create a data frame
data = data.frame(
  group = rep(c("before", "after"), each = 10),
  weight = c(before, after)
)

t.test(weight ~ group, data = data, paired = TRUE)

# 이거 코딩 어떻게쳐요?ㅠㅠ

#
# KOR ENG ATH HIST SOC MUSIC BIO EARTH PHY ART

# 90 85 73 80 85 65 78 50 68 96

# (1) 위 데이터를 score 벡터에 저장하시오(과목명은 데이터 이름으로 저장).

data = data.frame(
  key = c("KOR", "ENG", "ATH", "HIST", "SOC", "MUSIC", "BIO", "EARTH", "PHY", "ART")
  , val = c(90, 85, 73, 80, 85, 65, 78, 50, 68, 96)
)

# (2) score 벡터의 내용을 출력하시오.

score = data$val
score

# (3) 전체 성적의 평균과 중앙값을 각각 구하시오.

mean(score, na.rm = TRUE)
median(score, na.rm = TRUE)

# (4) 전체 성적의 표준편차를 출력하시오.

sd(score, na.rm = TRUE)

# (5) 가장 성적이 높은 과목의 이름을 출력하시오.

index = which(score == max(score, na.rm = TRUE))

data[index,]

# (6) 다음 조건을 만족하는 위 성적에 대한 히스토그램을 작성하시오. (그래프 제목: 학생 성적, 막대의 색: 보라색)

hist(score, main = "학생 성적", col = "violet")

#
library(lubridate)

# 변수 전체 삭제하는 명령어가 위와 같다고 알고 있는데
#
# 1. ls()는 현재 전체 변수 변환 명령
# 2. list에 전체 변수 대입
# 3. list가 chr type인데 remove 인것 같은데
#
# 질문
# rm(aaa=ls()) 혹은
# list=ls()
# rm(list) 와 같은 명령어는 왜 동작하지 않을까요??
#
#     rm(list=ls())가 세부적으로 왜 이렇게 동작하는지
# 설명 해 주실 전문가 님 계실까요??

list = ls()
list

rm()

# 문자열 (char)를 제외한 모든 열을 가져오려고 하는 것을 어떻게 사용해야하나요,,?
# df %>% select(., ) %>% show()
# 이렇게 썼는데 어떻게 해야 문자열을 제외한 모든열을 가지고 올 수 있게 할 수  있을까요??

library(dplyr)

mtcars %>%
  select_if(is.numeric) %>%
  glimpse()

mtcars %>%
  select_if(is.character) %>%
  glimpse()


# Error in file(file, "rt") : 커넥션을 열 수 없습니다
# 추가정보: 경고메시지(들):
#     In file(file, "rt") :
#     파일 '학생키몸무게등'를 열 수 없습니다: No such file or directory
#
#
# 라고 오류가 뜹니다.
#
#
# getwd() 로 작업위치 확인하고 그 폴더에 파일 넣고 해보기도하고
# setwd()로 직접 경로 지정하고 시도해봐도 안되고(c드라이브, d드라이브, 바탕화면, r실행시켰을때 지정되어있는 장소 전부 시도했습니다. 그때마다 그 위치에 있는 파일 속성 눌러서 경로확인하고 복붙하고 혹시몰라서 파일 이름까지 복붙해서 실행했습니다.
# scv파일로 변환할때 잘못한거가 싶어서 다시 시도해도 안되고,
# 영어만 인식하는가해서 영어이름으로 해도 안됩니다.
#
#
# 명령어 입력하는 장면 스크린샷으로 정리해서 첨부해드립니다.
#
#
# 다른 사람들은 다 되는데 교수님이랑 화면공유상태로 보여드려도 교수님도 모르시고, 해결책을 찾아볼려고 구글을 아무리 뒤져도 경로설정이 잘못됐다는데 이건 아닙니다.

fileDirName = paste0(getwd(), "/", "xxx.csv")

data = read.csv(fileDirName, sep = ",", header = TRUE)

# 데이터 R에 hflights가 있는데 이게 21열 227496행으로 구성되있음 19열이 취소된비행편인데 취소는 1 취소X는 0인데 여기서 1의 개수를 확인하는 함수가 있다면 뭔가요

library(dplyr)
library(hflights)

hflights %>%
  dplyr::group_by(Cancelled) %>%
  dplyr::summarise(n = n())

# r프로그램 잘 아시는 분 도와주세요 ㅠㅠ 계속 freq 입력하고 나면 멀쩡히 있던 막대그래프가 사라져요 ... 뭐가 문제ㄹ까요 대체 내공 1000걸ㅓ요 대학 과제인데 최대한 빨리 답변 부탁드려요ㅠㅠ


data = c(10, 11, 8, 5, 4, 6, 8, 10, 13)

hist(data, breaks = 4:13, freq = FALSE)
lines(dnorm(1:100, mean = mean(x), sd = sd(x)), col = "red")


# R프로그램에서 iris데이터를 사용해서 산점도 그린 코드입니다. 여기서 마지막에 범례를 추가할때
# Species에 해당하는 범주는 점(pch) 그리고 abline을 선(lty)로 표현해서 한 범례 박스안에 넣는 방법이 궁금합니다 ㅠㅠ

data = seq(-pi, pi, 0.01)

plot(data, sin(data), type = 'l', xlab = "xlab", ylab = "ylab")
points(data, cos(data))

legend("topright"
  , c("sin", "cos", "Y = aX + b")
  , col = c("black", "black")
  , pch = c(1, NA, NA)
  , lty = c(NA, 1, 2)
  , cex = 0.8
)

# 2번을 어떻게 해야하는지 모르겠내요. 가능하면 2~6다 궁금합니다.
getCount(100, 3)

getCount = function(n, x) {

  data = c(1:n)

  index = which((data %% x) == 0)

  return(length(index))
}

# 대한민국, 중국, 일본 세 나라의 1인당 국내총생산과 기대 수명을 전체 관측 기간에 걸쳐 나란히 출력하세요.
# continent를 제외한 열(변수)들을 나라별로 연도가 가장 최근 데이터부터 나타나게 출력하세요.

library(gapminder)

data = data.frame(gapminder)

data %>%
  dplyr::filter(country %in% c("Korea, Rep.", "Japan", "China")) %>%
  dplyr::arrange(country, desc(year)) %>%
  dplyr::select(-continent, -pop)


data %>%
  dplyr::group_by(year, continent) %>%
  dplyr::summarise(sumPop = sum(as.numeric(pop), na.rm = TRUE)) %>%
  tidyr::spread(key = continent, value = sumPop) %>%
  dplyr::filter(Africa > Europe) %>%
  dplyr::select(year, Africa)


# 안녕하세요 r질문드립니다.
# n<-3 m <- matrix(1:30, ncol=3) m
# m[sample(nrow(m), n),]
# question.
# How could you select n random rows from a matrix m where every selected row should be adjacent with increasing order?

# 선택한 모든 행이 증가하는 순서와 인접 해야하는 행렬 m에서 n 개의 임의 행을 어떻게 선택할 수 있습니까?

n = 3
m = matrix(1:30, ncol = 3)

# Lets create the train and test data set
ind = sample(1:nrow(m), n)

trainData = m[ind,]
testData = m[-ind,]

trainData
testData

# 이 문제들을 r studio를 통해 r코드로 풀어야 해요
# 도와주세요 내공 100 드립니다!

# 어느 대학교에 입학하는 학생 중 20명을 뽑아서 졸업후 진로에 대한 희망을 조사한 결과 다음의 자료를 얻었다. SAS를 이용하여 다음 물음에 답하여라.

data = data.frame(
  no = 1:20
  , val = c(1, 2, 2, 3, 1, 3, 1, 2, 4, 2, 3, 1, 1, 3, 3, 1, 2, 3, 4, 1)
)

dataL1 = data %>%
  dplyr::mutate(type = case_when(
    val == 1 ~ "대학원"
    , val == 2 ~ "취직"
    , val == 3 ~ "해외유학"
    , val == 4 ~ "군입대"
    , TRUE ~ "null"
  ))

# 도수분포표
table(dataL1$type)

# 원형 그래프
dataL1 %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(number = n()) %>%
  ggplot(aes(x = "", y = number, fill = type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  blank_theme +
  theme(axis.text.x = element_blank()) +
  geom_text(aes(y = number / 4 + c(0, cumsum(number)[-length(number)]),
                label = paste0(number, "명")), size = 5)


# 막대그래프
ggplot(dataL1, aes(x = val, fill = type)) +
  geom_histogram(aes(y = stat(count) / sum(count)), binwidth = 1, alpha = 0.6) +
  scale_y_continuous(labels = scales::percent)


# R프로그래밍 관련 질문입니다.
# 만약 다음행렬이 있으면
# 1열 2열 3열
# [1,]            과자        새우깡       1400원
# [2,]      아이스크림      투개더     2000원
# [3,]          과자           뻥튀기     1600원
# 과자 -> 새우깡 -> 1400원
# 아이스크림 -> 투개더 -> 2000원>
# ↘ 뻥튀기 1600원
# 이런식으로 네트워크(그래프)를 만들고 싶은데 어떻게 해야할까요??

library(igraph)
library(tidygraph)

data = data.frame(
  from = c("과자", "새우깡", "아이스크림", "투개더", "과자", "뻥튀기")
  , to = c("새우깡", "1400원", "투개더", "2000원", "뻥튀기", "1600원")
)

dataL1 = as_tbl_graph(data)

plot(dataL1)


# X<-1:100
# sum(x > 50)
# 이거 정답이 왜 50인가여
# Sum이니까 51부터 100까지 더해야하는거 아닌가요?

x = 1:100

# TRUE의 총 개수를 의미
sum(x > 50)

# TRUE에 대한 인덱스 추출
ind = which(x > 50)

sum(x[ind], na.rm = TRUE)

# x의 변수를 1부터 15까지 만들고, y의 변수는 –1부터 14까지 만드시오.
# (프로시저를 써주세요)
x = 1:15
y = -1:14

# 2. x의 제곱근에 대한 결과를 쓰시오.
sqrt(x)

# 3. y와 원주율을 곱한 결과를 쓰시오.
y * pi

# 4. x와 y의 기술통계에 대한 결과를 쓰시오.
summary(x)
summary(y)

# 5. 산점도를 그리시오
# (x의 난수는 1500, y의 난수는 1500으로 만들어 산점도를 그리시오.)
xAxis = runif(1500)
yAxis = runif(1500)

plot(xAxis, yAxis)

# v1<-51:90
# v1에서 7의배수들의 값을 0으로 변경하라는 문제인데....
# v1[v1%%==0]<-0
# 여기서 뭘 어떻게더 바꿔야 할 지 모르겠습니다.
# 도와주세요ㅠㅠㅠ

v1 = 51:90

ind = which(v1 %% 7 == 0)

v1[ind] = 0

v1

#
A = 10
B = 12
C = 2

data = c(A, B, C)


# 1) 세 변수 A,B,C에 각각 값을 대입하고 세 변수 중 가장 큰 수와 가장 작은 수를 출력하는 프로그램을 작성하시오

getMaxMinVal = function(data) {

  # 가장 큰 수
  maxVal = max(data, na.rm = TRUE)

  # 가장 작은 수
  minVal = min(data, na.rm = TRUE)

  return(c(maxVal, minVal))
}

getMaxMinVal(data)


# 최대공약수
pracma::gcd(2, 24)


# 두가지 문제인데 어떻게 하면 될까요 ㅠㅠ


# 행동위험요인 시스템은 매년 미국에서 시행되는 대규모 전화 설문 조사이다.
# 이 조사에서는 응답자들의 현재 건강 상태 및 그들의 건강과 관련된 생활 습관 등을 조사한다.
# 이 조사에 관한 자세한 내용은 BRFSS의 웹사이트에서 확인할 수 있다.
# 주어진 자료는 2000년도에 시행된 20000명의 BRFSS 조사 데이터의 일부이며 전체 200개 이상의 항목 중에서 간추린 9개의 항목을 포함하고 있다.
# inch로 되어 있는 신장을 cm로 바꾸고 pound로 되어 있는 체중을 kg으로 바꿔 새로운 변수를 만든 뒤 아래 물음에 답하시오.

library(readtext)
library(measurements)
library(dplyr)
library(ggplot2)
library(ggpubr)

data = read.csv("INPUT/cdc.txt", sep = " ", header = TRUE)

dataL1 = data %>%
  dplyr::mutate(
    heightCm = conv_unit(height, "inch", "cm")
    , weightKg = conv_unit(weight, "lbs", "kg")
    , wtdesireKg = conv_unit(wtdesire, "lbs", "kg")
    , diff = wtdesireKg - weightKg
    , genderType = case_when(
      gender == "m" ~ "남성"
      , gender == "f" ~ "여성"
      , TRUE ~ "NULL"
    )
  )

# 1. genhlth 변수에 대해 적절한 방법을 이용하여 요약해보자.
table(dataL1$genhlth)

# 2. height 변수와 이 변수를 cm로 바꾼 변수에 대한 수치적 요약 값을 구해보자
summary(dataL1$height)
summary(dataL1$heightCm)

# 두 변수의 평균과 분산을 각각 구하시오. 구해진 값을 가지고 평균과 분산의 성질에 관하여 설명하시오.
mean(dataL1$height, na.rm = TRUE)
mean(dataL1$heightCm, na.rm = TRUE)
var(dataL1$height, na.rm = TRUE)
var(dataL1$heightCm, na.rm = TRUE)

# 3. weight_kg 변수와 wtdesire_kg 변수의 산점도를 그려보고 두 변수 사이에는 어떠한 관계가 존재한다고 보여지는지 기술하시오.

ggscatter(dataL1, x = "weightKg", y = "wtdesireKg", add = "reg.line") +
  stat_cor(label.x = 5, label.y = 300) +
  stat_regline_equation(label.x = 5, label.y = 275) +
  theme_bw()

# 두 변수의 상관계수를 구해보고 산점도와 연결하여 기술하시오.
cor(dataL1$weightKg, dataL1$wtdesireKg)

# 4. wtdesire_kg-weight_kg를 계산하여 새로운 변수를 만들어보자. 이 변수의 분포는 어떠한가?
# 수치적 요약과 그래프 요약을 통해 살펴보자. 이것이 의미하는 바를 무언인지 기술하시오.

# 수치적 요약
summary(dataL1$diff)

# 그래프 요약
hist(dataL1$diff)

# 5. age 변수를 이용하여 히스토그램을 그려보자. 그리고 구간의 수를 50, 100으로 바꿔가며 동일한 히스토그램을 그린 후 비교해보자

# 구간의 수 : 50
breaks = seq(1, 100, length.out = 50)

groupTags = cut(dataL1$age, breaks = breaks, include.lowest = TRUE, right = FALSE)

ggplot(data = as_tibble(groupTags), mapping = aes(x = value)) +
  geom_bar(fill = "bisque", color = "white", alpha = 0.7) +
  stat_count(geom = "text", aes(label = sprintf("%.2f", ..count.. / length(groupTags))), vjust = -0.5) +
  labs(x = 'score per group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 구간의 수 : 100
breaks = seq(1, 100, length.out = 100)

groupTags = cut(dataL1$age, breaks = breaks, include.lowest = TRUE, right = FALSE)

ggplot(data = as_tibble(groupTags), mapping = aes(x = value)) +
  geom_bar(fill = "bisque", color = "white", alpha = 0.7) +
  stat_count(geom = "text", aes(label = sprintf("%.2f", ..count.. / length(groupTags))), vjust = -0.5) +
  labs(x = 'score per group') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 6. height_cm의 상자그림을 그리되 신장은 성별에 따라 차이가 있으니 성별로 나눠서 상자그림을 2개 그리시오.
ggplot(dataL1, aes(x = genderType, y = heightCm, fill = genderType)) +
  geom_boxplot()

# corrplot에 cor 을 색과 숫자로 표기하고 싶은데 , p-value가 0.05가 넘으면 색이 아닌 숫자만 지우고 싶습니다. 어떻게 해야 할까요?
require(Hmisc)
require(corrplot)

corData = Hmisc::rcorr(as.matrix(mtcars))

corMat = corData$r
pMat = corData$P

corrplot(corMat, type = "upper", order = "hclust", method = "number",
         p.mat = pMat, sig.level = 0.05, insig = "blank")

#
# 아래 식까지 해봤는데 오류뜨네요. 왜 그런지 아시나요?

df_fruit = data.frame(
  fruit = c("사과", "딸기", "수박")
  , money = c(1800, 1500, 3000)
  , income = c(24, 38, 13)
)

df_fruit

# r 스튜디오 질문인데요
# 오렌지 데이터 세트 를 출력해서  age가 1000 이상이면서 circumference가 140이하인 데이터를 추출해circle1에 할당하고,circle1의 age의 합과 circumference 평균값을 구해보자.을 구해보라는데요
# 에초에 데이터세트 출력(오렌지) 를 어케하는질모르겟어요 답변좀욧

library(dplyr)

circle1 = as.data.frame(Orange) %>%
  dplyr::filter(
    age >= 1000
    , circumference <= 140
  )

circle1

# circle1의 age의 합
sum(circle1$age, na.rm = TRUE)

# circle1의 circumference 평균값
mean(circle1$circumference, na.rm = TRUE)

# month <- 1:12
# name <- month.name
# (Months <- data.frame(month,name))
# "1월", "2월", ....."12월","None"을 값으로 갖는 열 namek를 추가하시오.
# Months <- data.frame(Months, data.frame(namek= 까지는 알겠는데 1월2월~12월 none는 어떻게 추가하나요.

library(lubridate)

monthVal = 1:12

namek = lubridate::month(monthVal, label = TRUE, abbr = FALSE)

namek

# R스튜디오에서 어떤 식을 넣고 돌려도 객체를 찾을 수 없다고만 뜨네요.

# x3 <- c(1, 2, 3)
# x3

# 이런 간단한 식도 [1] 1 2 3으로 안 나오고 객체 x3을 찾을 수 없습니다 라고 떠요.
# 어디가 문제인가요?

x3 = c(1, 2, 3)

ls.str()

# [1] 2 2 2 2 2
# [1] 3 3 3
# [1] 4
# [1] 3
# [1] 2 2 2
# [1] 1 1 1 1 1
# 을 만들거나 이게 5회 반복되어있는 형태는 만드는 방법을 알겠는데
# 각 행의 숫자 반복을 지정하려면 어떻게 해야하나요?

rowVal = c(2, 3, 4, 3, 2, 1)
repVal = c(5, 3, 1, 1, 3, 5)

for (i in 1:length(rowVal)) {
  cat(rep(rowVal[i], repVal[i]), "\n")
}

#
# R스튜디오를 공부한지 얼마 안됐는데요
# 엑셀에 있는 데이터들을 R로 불러와서 데이터프레임의 형식으로
# 어떻게 만드나요?ㅠㅠ

# R에서 직접 데이터프레임 만드는건 알겠는데
# 엑셀에 있는 데이터를 R에서 데이터프레임으로 만드는 법을 모르겠네요..

# 우선은
# library(readxl)
# dat <- read_excel("파일")
# View(dat)

# 이렇게 해서 다른 창으로 표만 볼 수 있는 상태인데
# 이거 각각 데이터들을 아래 Console에서 보려면 어떻게 해야하나요?!

library(dplyr)

data = read.csv("INPUT/cdc.txt", sep = " ", header = TRUE)

# 모든 보기
data

# 열 기준으로 요약 보기
dplyr::tbl_df(data)

# 행 기준으로 요약 보기
dplyr::glimpse(data)

# 이렇게 한글이 이상하게 뜹니다. 어떻게해야하나요?

data = read.csv("INPUT/출산율.csv", encoding = "utf-8")
data = read.csv("INPUT/출산율.csv", encoding = "cp949")
data = read.csv("INPUT/출산율.csv", encoding = "euc-kr")

#
# df.x 라는 데이터의 address_sigungu의 변수에 저런 내용들이 있는데요
# 청주서원구라는 내용을 서원구로 바꾸는것은 어떻게 식을세워야 하나요?
# 내공 100드리겠습니다 ㅠㅠ 알려주세요

library(dplyr)
library(stringr)

data = data.frame(
  name = c("청주서원구", "청주청원구", "청주흥덕구")
)

dataL1 = data %>%
  dplyr::mutate(
    nameNew = stringr::str_replace(name, "청주", "")
  )

dataL1

# A 라는 데이터 세트에서 ab의 변수 값이 20 이상이면 "P", 20 미만이면 "F" 값으로 A 데이터 세트에 ab20_PF 변수를 생성하려면 어떻게 입력해야 하나요?
# ifelse함수가 들어가야 되는건 알지만 계속 인수가 누락 됐다고 나와서요...

library(dplyr)

A = data.frame(
  ab = c(10, 20, 30, 40, 50)
)

ab20_PF = A %>%
  dplyr::mutate(
    abType = case_when(
      ab >= 20 ~ "P"
      , ab < 20 ~ "F"
      , TRUE ~ "NA"
    )

  )

ab20_PF

# 1부터 10까지를 원소로 갖는 벡터 x10 생성하고 반복문을 이용하여 1 다음에 100, 2 다음에 200, ... 등을 추가하시오
# 혹시 이거 어떻게 하는지 아시는분 계신가요? 급합니다..ㅠㅠ

x10 = 1:10

data = data.frame()

for (i in 1:length(x10)) {

  data = dplyr::bind_rows(
    data
    , data.frame(c(i, i * 100))
  )
}

data

# 이 문제만 도저히 모르겠어요.. 살려주세요
# 6월달에 발생한 가장 강한 바람(Wind)의 세기를 보이시오.

library(dplyr)

airquality %>%
  dplyr::group_by(Month) %>%
  dplyr::summarise(maxWind = max(Wind, na.rm = TRUE)) %>%
  dplyr::filter(Month == 6)

#
library(pracma)

pracma::taylor(sin, x0 = 0, n = 8)

# 1.다음의 문제를 해결하기 위한 R코드를 작성하시오
# 10!을 출력하시오.(단,factorial() 함수를 이용하지 않는다.)

sum = 1
for (i in 1:10) {
  sum = sum * i
}

sum

# 2.for문을 이용하여 구구단 2단에서 9단까지 출력하는 R코드를 작성하시오.

for (i in 2:9) {
  for (j in 1:9) {
    cat(i, " * ", j, " = ", i * j, "\n")
  }
}


# 3. 1~100의 정수를 차례로 출력하되 3의 배수에서는 숫자 대신 '*'을 출력하는 R코드를 작성하시오(출력은 가로방향으로 한다.)

for (i in 1:100) {

  val = ifelse((i %% 3 == 0), "*", i)

  cat(paste0(val, " "))
}

# 그래프에 수식으로 된 텍스트를 추가하려고 하는데 계속 에러가 뜹니다ㅠㅠ
# 뭐가 문제인 지 모르겠어요
# 루트2파이 분의 1이라는 수식을 해당 위치에 추가하고 싶은데 계속 에러가 떠요ㅠ 뭐가 문제일까요?

# text(1.5,0.35,expression_(frac(1,sqrt(2*pi)))

plot(1:10, 1:10)
text(1.5, 5.35, expression(frac(1, sqrt(2 * pi))))

#
# 안녕하십니까? 블로그에서 R 프로그래밍에 대해 많은 참조를 하고 있습니다.
# 저는 R을 이용한 통계학을 공부하고 있는 대학원생입니다.
# 표본크기를 2부터 50까지 변화시켜 가며 각 n마다 100개의 표본을 취하여 표본평균을 계산한 것을 시각화해야 하는데, 아무리 생각해도 잘 되지가 않아 메일을 보냅니다.
# 반복문 안에 코드를 완성해야 하는데 조언을 부탁드려도 되겠습니까?

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
X = runif(10000, min = 0, max = 1)  # 난수 생성 (모집단 생성)

hist(X)  # 모집단의 빈도분포
cat(mean(X), var(X), sd(X), "\n")  # 모집단의 평균/분산/표준편차


################################################### 표본평균집단에 대해서 ###########################################################


# for (i in c(10, 50, 100, 250)) {
for (i in seq(2, 50, 2)) {

  DO = 100000    # Number of repetition
  N = i          # Number of sample
  # N = 30

  ## 비복원 추출(무작위 정렬) : 한번 뽑은 것을 다시 뽑을 수 없는 추출
  Sort = lapply(1:DO, function(i) sample(X, N, replace = FALSE))
  Sort_mean = mapply(mean, Sort)

  # FIG
  hist(Sort_mean, breaks = 50, xlab = "Sample Distribution Mean", xlim = c(0.3, 0.7), col = "light grey", border = "grey", xaxs = "i", yaxs = "i",
       main = paste0("Central Limit Theorem : number of sample = ", N, ", number of repetition = ", sprintf("%d", DO)))

  XX1 = mean(Sort_mean) + sd(Sort_mean)
  XX2 = mean(Sort_mean) - sd(Sort_mean)
  XX3 = mean(Sort_mean) + 2 * sd(Sort_mean)
  XX4 = mean(Sort_mean) - 2 * sd(Sort_mean)
  XX5 = mean(Sort_mean) + 3 * sd(Sort_mean)
  XX6 = mean(Sort_mean) - 3 * sd(Sort_mean)

  YY = max(hist(Sort_mean, breaks = 50, plot = F)$counts)
  lines(c(mean(Sort_mean), mean(Sort_mean)), c(0, YY), lty = 1, col = 4); text(mean(Sort_mean), YY / 2, "Mean")
  lines(c(XX1, XX1), c(0, YY), lty = 1, col = 2); text(XX1, YY / 2, "+1σ")
  lines(c(XX2, XX2), c(0, YY), lty = 1, col = 2); text(XX2, YY / 2, "-1σ")
  lines(c(XX3, XX3), c(0, YY), lty = 1, col = 2); text(XX3, YY / 2, "+2σ")
  lines(c(XX4, XX4), c(0, YY), lty = 1, col = 2); text(XX4, YY / 2, "-2σ")
  lines(c(XX5, XX5), c(0, YY), lty = 1, col = 2); text(XX5, YY / 2, "+3σ")
  lines(c(XX6, XX6), c(0, YY), lty = 1, col = 2); text(XX6, YY / 2, "-3σ")
  lines(c(max(Sort_mean), max(Sort_mean)), c(0, YY), lty = 1, col = 3); text(max(Sort_mean), YY / 2, "Max")
  lines(c(min(Sort_mean), min(Sort_mean)), c(0, YY), lty = 1, col = 3); text(min(Sort_mean), YY / 2, "Min")

  # 0.1초 지연
  Sys.sleep(0.1)
}

# 제가 가지고 있는 데이터셋 내에 있는 변수 (날짜와 시간) 에서 년도만 추출하여 새로운 변수를 만들어야 합니다.
# 전에 지식인에서 답변주신 분이 알려주신대로 strsplit을 아래와 같이 사용했는데요 (hubway_trips_small이 데이터파일이름이고, start_date가 날짜 및 시간으로 되어있는 변수이름입니다)
# 이렇게 하면 year라는 새 변수에 각 년도가 나와야하는데, 아래와 같이 2013년도로만 쭉 나옵니다. 위 코드에서 a와 b라는 새 변수를 만드는 것까지는 잘 된 것 같은데 c부터 한가지로 통일되더니 year에서도 2013년도로 통일되었습니다.
# a와 b까지 잘 된 것 처럼 c에서 그리고 마지막에 year까지 기존 변수의 값들을 유지해서 새 변수를 어떻게 만들 수 있을까요? 고수님들 제발 도와주세요...
# 그리고 저 year변수의 값이 2013 하나인채로, 아래와 같이 모델을 돌렸더니 이런 에러가 뜨는데, 저 변수 내 값이 2013이라는 값 하나뿐이라서 뜨는 에러일까요? year변수를 제대로 잘 만들고 돌리면 괜찮을까요?

library(dplyr)
library(lubridate)

dfData = data.frame(
  sDateTime = c("4/13/2013 13:05:00", "5/23/2013 16:24:00", "5/1/2013 15:15:00")
)

dfDataL1 = dfData %>%
  dplyr::mutate(
    dtDateTime = lubridate::parse_date_time2(sDateTime, orders = "%m/%d/%Y %H:%M:%S")
    , year = lubridate::year(dtDateTime)
    , month = lubridate::month(dtDateTime)
    , day = lubridate::day(dtDateTime)
  )

dfDataL1

# 이걸 구하고 싶은데 R에서 오류가 나요 ㅠㅠ 혹시 따로 설치해야하는 패키지가 있나요?

HPR = function(begin.price, end.price, dividend) {
  result = (end.price - begin.price + dividend) / begin.price

  return(result)
}

prob <- c(0.3, 0.4, 0.2, 0.1)
begin.price <- 100
end.price <- c(126.50, 110, 89.75, 46)
cash.dividend <- c(4.5, 4, 3.5, 2)
hpr.value <- HPR(begin.price, end.price, cash.dividend)
hpr.mean <- sum(hpr.value * prob)
hpr.var <- sum((hpr.value - hpr.mean)^2 * prob)
df.hpr <- data.frame(hpr.value, hpr.mean, hpr.var)
list(HPR = hpr.value, Mean = hpr.mean, Var = hpr.var)

# R studio 평균구하기

# summarise에서 mean할 경우 NA가 포함되어서 발생한 오류입니다.
# 따라서 mean(공무원.주중, na.rm = TRUE)로 처리하시면 됩니다.

# 안녕하세요 해솔님 저는 R스튜디오를 혼자 공부하는 학생입니다.
# 응용문제를 다운받아서 푸는와중에  답이없는 문제를 발견했고 혼자풀지 못하여 지식인과 구글링을하던중
# 해솔님의 친절하고 이해하기 쉬운 답변을 보게되었습니다.
# 무례하게나마 메일로 질문을 혹시 도오ㅏ주실수있을까하여 연락드립니다.
# 시간이되신다면 도와주실수있을까요? 죄송합니다

# ※ 공공 데이터 포털www.data.go.kr은 공공 기관이 생성·취득하여 관리하는 공공 데이터를 파일·오픈 API·시각화 등 다양한
# 방식으로 데이터를 얻을 수 있는 통합 창구다. 이 곳에 공개되어 있는 방대 한 건강 검진 기록 데이터를 요약하는
# 가공을 해보자.
#
# 위와 같이 [건강검진정보] 다운로드 페이지에서 전체를 선택하여 다운로드한 후 압축을 풀면 2002~2016년의 관측 데이터가 NHIS_OPEN_GJ_2002.csv, NHIS_OPEN_GJ_2003.csv,…, NHIS_OPEN_GJ_2016이라는 파일명으로 기록되어 있다. 본문의 예와 같이 여러 해에 걸친 다양 한 속성의 관측값이 별도의 파일들로 저장되어 있다. 따라서 데이터를 종합적으로 탐색하려면 각 연 도별 데이터를 읽어들여 정제하고 적절히 정리하여 통합하는 가공 과정이 필요하다.
#
# 2. 연도별 데이터 파일을 읽어들여 NHIS2002 ~ NHIS2016라는 변수 이름으로 저장하라.

library(tidyverse)
library(readr)
library(data.table)

fileList = Sys.glob("INPUT/NHIS_OPEN_GJ/*")

data = fileList %>%
  purrr::map(data.table::fread) %>%
  purrr::reduce(dplyr::bind_rows)

dplyr::glimpse(data)

# 2.데이터 파일과 함께 다운로드된 사용자 매뉴얼 파일의 내용을 참고하여 결측값을 제거하고 각 열의 데이터형을 알맞게 변환하여 정리하라.
# : 데이터 파일에 있는 것 중 사용자 매뉴얼을 확인하시면 해당 결측치 내용이 있을 껍니다. 이에 결측치하시면 되는데. factor 함수를 이용하시면 됩니다.
# factor를 사용하시려면 인스톨로 factor를 하셔야 합니다.
# 성별코드, 5세이하 연령대 코드 시도코도를 확인해보세요.

dataL1 = data %>%
  dplyr::mutate_at(vars(성별코드), funs(as.factor)) %>%
  dplyr::mutate_at(vars(시도코드), funs(as.factor)) %>%
  dplyr::mutate_at(vars(`연령대코드(5세단위)`), funs(as.factor))

dplyr::glimpse(dataL1)

# 4. 2002년 데이터에서 성별, 시도, 연령대 순으로 요약된 통계값을 구하라.
# : 그룹을 정해서 하셔야 하는데 기준은 연도, 성별, 시도, 연령대가 될 것이며
# 각각의 변수들의 평균값을 구해서 통계를 구하시면 됩니다.

dataL2 = dataL1 %>%
  dplyr::filter(기준년도 == "2002") %>%
  dplyr::select(-가입자일련번호) %>%
  dplyr::group_by(성별코드, 시도코드, `연령대코드(5세단위)`) %>%
  dplyr::summarise_all(list(mean = mean))

dplyr::glimpse(dataL2)

# 5. 2003~2016년 데이터에서 03 문제와 같은 방법으로 요약된 통계값을 구하라.
# 전체 관측 기간에 대한 요약 통계값을 데이터 프레임 변수 하나에 병합하라.(단, 특정 해의 누락 된 검사 항목은 결측값 으로 표시한다.)
# 문제의 요약 통계값을 데이터 프레임으로 merge 함수를 이용

dataL3 = dataL1 %>%
  dplyr::filter(dplyr::between(기준년도, 2003, 2016)) %>%
  dplyr::select(-가입자일련번호) %>%
  dplyr::group_by(기준년도, 성별코드, 시도코드, `연령대코드(5세단위)`) %>%
  dplyr::summarise_all(list(mean = mean)) %>%
  # tidyr::gather(key = "key", value = "val")
  tidyr::gather(-c(1:4), key = "key", value = "val") # group_by 용도

dplyr::tbl_df(dataL3)


# 안녕하세요 해솔님
# R 코딩을 공부하다가 궁금한게 생겨 지식인을 찾아보던중, 해솔님을 알게되어 블로그를 통해 연락드립니다
# 제가 예제문제를 공부하던중, 잘모르겠는 부분이 있어서요
# 코딩을 해보았긴 했는데 결과값이 잘 안나와 이렇게 죄송함을 무릅쓰고 메일 보냅니다.
# 혹시 도움을 받을 수 있을까 해서요.
# 괜찮으시다면 답변 부탁드리겠습니다
# 감사합니다 ^^

# 1. 다음표는 00회사 직장인을 대상으로 조사한 연봉(Salary)과 근무연수(Year)이다. 성별(Gender)에 따른 연봉차이가 있을 것으로 예상되어 그 차이를 보기위해 한 그래프에 그룹별로 묶어 그리려고 한다.

# *1번문제 표는 맨아래 참고해주세요*
# 1-1.  근무연수에 따른 연봉을 나타내는 산점도 그래프 (x-축 이름: Year, y-축 이름: Salary($), 메인타이틀: Salary difference in gender)
# 1-2. 남성(M)은 초록색 실선으로 연결
# 1-3. 여성(F)은 주황색 점선으로 연결
# 1-4. 좌측 위에 범례 표시 (점선 중 선만 표시)

library(dplyr)
library(ggplot2)

data = data.frame(
  Gender = c(1, 1, 2, 2, 2, 1, 1, 2, 2, 1)
  , Salary = c(90000, 51000, 60000, 65000, 81000, 93000, 57000, 78000, 44000, 76000)
  , Year = c(12, 5, 8, 6, 11, 10, 6, 9, 4, 9)
)

dataL1 = data %>%
  dplyr::mutate(type = case_when(
    Gender == 1 ~ "M"
    , Gender == 2 ~ "F"
    , TRUE ~ "null"
  ))

ggplot(dataL1, aes(x = Year, y = Salary, color = as.factor(Gender))) +
  geom_line(aes(linetype = as.factor(Gender))) +
  labs(title = "Salary difference in gender")

# 2. 그래프를 생성하여 그룹별로 chull 함수를 이용하여 묶으시오
# 2-1. 표준정규분포를 따르는 난수를 생성하여 matrix(100x2) 로 dot1 에 저장
# 2-2. 구간[0,1]인 일양분포를 따르는 난수를 생성하여 matrix(50x2)로 dot2 에 저장
# 2-3. dot1은 밀도가 15, 선 기울기 각도는 45, 보라색, 다각형으로 묶음
# 2-4. dot2는 밀도가 30, 선 기울기 각도는 30, 금색, 다각형으로 묶음
# 2-5. 그래프 타이틀은 "Random"
# 2-6. 그래프와 겹치치 않게 범례 표시.

# 예를들어서 상관계수가 0~1 까지 데이터로 있을 때 이를 4개로 나눠서 그려야 하는데
# 0.25 간격으로 가시화

library(tidyverse)
library(ggplot2)

data = read.csv("INPUT/full_result1.csv")

dataL1 = data %>%
  tidyr::gather(-c(1:2), key = "key", value = "val") %>%
  dplyr::filter(key == "cor")

ggplot(dataL1, aes(x = val)) +
  geom_histogram(binwidth = 0.05, boundary = 0, closed = "left") +
  scale_x_continuous(breaks = seq(0, 1, 0.05))

# 예를들어서 8월 1일 00시, 03시 ... 21시 이렇게 있으면 이걸 전부 묶어서 각 격자에대해서 일 최고기온을 찾아야 되고
# 일 최고기온이 그 격자에서 25도가 넘었으면
# 각 격자별 여름일수 구하기

library(tidyverse)
library(ggplot2)
library(data.table)

# dtDate = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-31"), "1 day")
# sDate = format(dtDate, "%Y%m%d")

# 1일 목록 조회 (20170701)
fileList = Sys.glob("INPUT/KMAPP/*201707*")

data = fileList %>%
  purrr::map(data.table::fread) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::select(lon, lat, temp)

dataL1 = data %>%
  dplyr::group_by(lon, lat) %>%
  dplyr::summarise(
    maxTemp = max(temp - 273.15, na.rm = TRUE)
    , number = sum(maxTemp > 25)
  )


# 안녕하세요.
#
# 해솔님 자료를 보면서 R컴퓨팅을 배우고 있는 직장인 겸 학생입니다.
#
# 혼자 해결이 안되는 질문이 있어서요.
#
# 제가 가지고 있는 데이터는 엑셀형식으로국가별 날짜별 질병의 발생 데이터 인데요. (지금은 csv로 변환하였습니다.)
# (header는 날짜, 발생수, 국가)
#
# 날짜가 01/01/1990 과 같은 방식으로 되어있고,
# 발생수는 숫자
# 국가는 국가명
# 으로 되어있는 데이터 입니다.
#
# 이 데이터를 갖고 특정 몇몇 국가의 질병발생수에 대해 시계열선그래프를 그리고 싶은데요...
#
# 1) 전 세계 국가 중에서 특정 국가 데이터만 어떻게 뽑나요?
#     2) 시계열 데이터를 만들기 위해서는 1990-01-01방식으로 바꿔야하나요?
#     그러면, seq(as.Date)함수를 이용한 후 clind를 하면 되는 건가요??
#
#     혹시 몰라서...
# https://data.europa.eu/euodp/en/data/dataset/covid-19-coronavirus-data/resource/55e8f966-d5c8-438e-85bc-c7a5a26f4863
# 링크를 남김니다.
#
# 너무 어렵네요 ㅜㅜ
# 해솔님 도와주시면 정말 감사하겠습니다!!

# 좋은 주말 되세요


library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)

# 데이터 읽기
data = read_excel("INPUT/COVID-19-geographic-disbtribution-worldwide.xlsx")

# 데이터 전처리
dataL1 = data %>%
  dplyr::filter(countryterritoryCode %in% c("KOR", "JPN", "CHN", "USA")) %>%
  dplyr::mutate(dtDate = as.Date(lubridate::parse_date_time2(as.character(dateRep), "%d/%m/%Y"))) %>%
  dplyr::arrange(dtDate)

# 가시화
ggplot(dataL1, aes(x = dtDate, y = deaths)) +
  geom_point() +
  scale_x_date(date_labels = "%Y-%m-%d") +
  facet_wrap(~countryterritoryCode)

# R프로그래밍 Plotly 패키지에 대한 설명을 알려주세요.
# 패키지 설명 , 구성 , 예시 등등 설명부탁드립니다.

# plotly 라는 인터랙티브 charts를 만드는 패키지입니다.
# 또한 시각화 콘텐츠에 접근할 수있는 패키지로서 오픈 소스입니다.

library(plotly)

plot_ly(iris, x = ~Sepal.Length, y = ~Sepal.Width, color = ~Species, type = "scatter")

# 시간_r이라는 함수의 형태인데요 여기서 0부터 23시까지의 시간 각각의 횟수를 더해 열이 시간과 총 횟수 두 개인 데이터 프레임을 만들고 싶은데 어떻게 하면 되나요?

library(dplyr)

data = data.frame(
  time = c(1, 2, 3)
  , count1 = c(35550, 33881, 38919)
  , count2 = c(27888, 27338, 31956)
  , count3 = c(20191, 20564, 22811)
)

data %>%
  dplyr::group_by(time) %>%
  dplyr::summarise_all(
    funs(sum(., na.rm = TRUE)
      , n())
  )


# 네이버 뉴스 속보

library(rvest)
library(stringr)
library(tidyverse)
library(dplyr)
library(magrittr)

Sys.setlocale("LC_ALL", "English")
options(encoding = "UTF-8")
Sys.setenv(LANG = "en_US.UTF-8")

url = "https://news.naver.com/main/list.nhn?mode=LSD&mid=sec&sid1=001&date=20200523&page=222"

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

url = c("https://news.naver.com/main/list.nhn?mode=LSD&mid=sec&sid1=001&date=20200523&page=222")


getUrlText(url, '//*[@id="main_content"]/div[2]/ul[*]/li[*]/dl/dt[2]/a')
# getUrlHref(url, '//*[@id="main_content"]/div[2]/ul[*]/li[*]/dl/dt[2]/a')


url = sprintf("https://news.naver.com/main/list.nhn?mode=LSD&mid=sec&sid1=001&date=20200523&page=%d", seq(1:100))

data = url %>%
  purrr::map(~getUrlText(.x, '//*[@id="main_content"]/div[2]/ul[*]/li[*]/dl/dt[2]/a')) %>%
  unlist() %>%
  as.data.frame()


#

rep(c(2, 4, 2), each = 3)

rowVal = c(2, 3, 4, 3, 2, 1)
repVal = c(5, 3, 1, 1, 3, 5)

j = 0


i = 0
while (i < length(rowVal)) {
  j = 1
  repeat {

    cat(rep(rowVal[i], repVal[j]), "\n")

    j = j + 1

    if (j > rowVal[j]) {
      break
    }

  }


  i = i + 1
}

#
# 안녕하세요 얼마전 R 공부를 시작한 학생입니다
# 조금 막히는 부분이 있어서 질문드립니다.

# str()로 데이터를 확인했을 때

# job int 1 2 1 4 16 1 ...

# 대략 이런식으로 나왔습니다
# 직업이 숫자일리는 없으니 factor형처럼 보이는데 다른 설명이 없어서요

# 1이면 의사 2면 경찰 이런식으로 데이터를 제공하는 사람이 주석을 달아주지 않으면 해석이 불가능한 데이터인가요?
# 아니면 혹시 다른 방법이 있는지..

# 설명을 잘 못해서 어처구니 없는 질문이라고 생각하실 수도 있지만 의견 부탁드립니다 ㅠㅠㅠ

library(dplyr)
library(plyr)

# 일반적으로 의사, 경찰와 같은 문자형으로 변수를 할당할 경우 숫자형/Factor형에 비해 큰 용량을 차지합니다.
# 따라서 숫자형/Factor형으로 나타되면 코드 정보가 꼭 필요합니다.
# 1 : 의사
# 2 : 경찰

data = data.frame(
  job = c(1, 2, 1, 4)
)

# 코드 정보 매핑
data %>%
  dplyr::mutate(
    name = case_when(
      job == 1 ~ "의사"
      , job == 2 ~ "경찰"
      , job == 4 ~ "교사"
      , TRUE ~ "null"
    )
  )


# Factor형으로 매핑
factor = as.factor(data$job)

result = plyr::mapvalues(factor, from = c(1, 2, 1, 4), to = c("의사", "경찰", "교사"))

result

# 정답은 4번인데 이해가 안가는데 해설좀 부탁드립니다..
# 1 2
# 1 1 4
# 2 2 5
# 3 3 6
# 첫째줄 나타내면 위의 매트릭스 형태로 만들어지고

# 둘째줄은 m[2,3&6] 으로 나오는데 왜 4번이죠..ㅜㅜ

m = matrix(1:6, nrow = 3)
m[m[, 1] > 1 & m[, 2] > 5,]

# m[, 1] > 1의 경우 1보다 큰 수 (즉 2, 3행)
# m[ , 2] > 5의 경우 5보다 큰 수 (3행)
# 이러한 2 조건의 만족하는 것은 3행입니다.

# 사진의 결과들이  if문으로 해도 나올 수 있게 바꿔야하는데
# 어떻게 하는지 모르겠어요ㅜㅜㅜ
# 저는 계속 오류 아님 경고가 나더라고요ㅜㅜㅜ

iris[iris$Species == "setosa",]
iris[iris$Sepal.Length > 7.5,]
iris[iris$Sepal.Length > 5.1 & iris$Sepal.Width > 3.9,]
iris[iris$Sepal.Length > 7.5,]
iris[iris$Sepal.Length > 7.6, c(3, 4)]

# 제가 질문한거는 올려주신 결과가
# if문으로 어떻게 입력해야 되는거에요? 였어요ㅜㅜㅜ

for (i in 1:nrow(iris)) {

  rowData = iris[i,]

  if (rowData$Species == "setosa") print(rowData)
}

for (i in 1:nrow(iris)) {

  rowData = iris[i,]

  if (rowData$Sepal.Length > 7.5) print(rowData)
}

for (i in 1:nrow(iris)) {

  rowData = iris[i,]

  if (rowData$Sepal.Length > 5.1 & rowData$Sepal.Width > 3.9) print(rowData)

}

for (i in 1:nrow(iris)) {

  rowData = iris[i,]

  if (rowData$Sepal.Length > 7.5) print(rowData)

}

for (i in 1:nrow(iris)) {

  rowData = iris[i,]

  if (rowData$Sepal.Length > 7.6) print(rowData[, c(3, 4)])

}


# R언어 평균, 중앙값, 표본분산, 표본표준편차, 변동계수 구해주세요
data = c(54, 57, 55, 23, 51, 64, 90, 51, 52, 45, 15, 10, 82)

# 평균
mean(data, na.rm = TRUE)

# 중앙값
median(data, na.rm = TRUE)

# 표본분산
var(data, na.rm = TRUE)

# 표본표준편차
sd(data, na.rm = TRUE)

# 변동계수
sd(data, na.rm = TRUE) / mean(data, na.rm = TRUE)

# 소수점이였던 변수값들이 ts 적용해서 gdp.time확인하니깐..
# 다 무슨 30이상의 이상한 숫자가 되었습니다.
# 데이터가 안 읽혀서 as.factor로 변경해서 읽혔더니 gdp 까지는 잘 읽었는데 ts적용한 이후부터 값이 멋대로 바뀌었습니다..
# 고수님들 도와주세요..

# ts함수 이용해서 시계열을 그리려고 합니다.

data = read.csv("INPUT/gdpq.csv", header = TRUE)

gdpData = ts(data, start = 1982, frequency = 4)

plot(gdpData[, 1] / 1000, ylab = "GDP(조원)", xlab = "연도")

lines(gdpData[, 2] / 1000, col = "red")


# R Studio 에서 주식 데이타 정리
# 안녕하세요.
# 우선 아래의 코드를 통해서 주식 데이타를 가져왔습니다.

# 위 코드로 데이타를 받으면

# Open  High  Low Close Volume AdjClose  이 순서대로 각각의 회사의 데이타를 가져올 수 있습니다.
# 제가 원하는건 Open, High, Low.... 각각의 데이타를 한곳으로 몰고 싶습니다;

# Open (ABG) Open(ACH)...Open(X)    High(ABG) High(ACH)...High(X)  .....그 외의 것들도 마찬가지로요.

# 간단히 복사하는 코드를 쓸 수도 있을거 같은데 도움 주시면 감사하겠습니다.
참고로

# Low_Raw <- NULL
# for(ticker in tickers)
#   Low_Raw <- cbind(Low_Raw, getSymbols(ticker, from = mdate, to = edate, auto.assign = F)[,1])
#
# 이렇게는 오류가 나는 부분이 많아서 못 쓸거 같습니다.

library(quantmod)
library(timetk)
library(dplyr)
library(magrittr)

mdate <- "2016-01-04"
edate <- "2016-03-09"
tickers <- c("ABG", "ACH", "ADM", "AEG", "AEM") # 실제는 2000개가 넘는 회사를 다운받습니다.

colName = c("Open", "High", "Low", "Close", "Volume", "Adjusted")

High_Raw = data.frame()

for (ticker in tickers) {

  tmpData = getSymbols(ticker, from = mdate, to = edate, auto.assign = F) %>%
    as.data.frame() %>%
    magrittr::set_names(x = ., value = colName)

  High_Raw = dplyr::bind_rows(High_Raw, tmpData)
}

dplyr::tbl_df(High_Raw)


# library(mlbench)
# data(PimaIndiansDiabetes)
# set.seed(100)을 실행한 후 전체 데이터(관측값)에서 임의로 60%를 추출하여
# train에 저장하고 나머지 40%는 test에 저장하라고 나와있는데 찾아보니깐
# sample을 하라고 까지는 찾았는데 어떻게 추출해서 변수에 담을수 있는건지를 모르겠어요
# 해결법좀 알려주세요

library(mlbench)

data(PimaIndiansDiabetes)

set.seed(100)

data = PimaIndiansDiabetes
size = as.integer(nrow(data) * 0.6)

ind = sample(1:nrow(data), size)

trainDF = data[ind,]
testDF = data[-ind,]

# 자꾸 이렇게 뜨는데 실행시킬 방법이 없을까요?
# 이유라도 알려주시면 감사하겠습니다.

library(dplyr)

data = data.frame(
  만나이 = as.factor(c(10, 20, 30, 40, 50, 60, 70, 80, 100))
)

data %>%
  dplyr::filter(as.character(만나이) >= 13)

# Rstudio에서 한글로 작성된 메모장 파일을 readLines("메모.txt")로 불러왔는데 글씨가 다 깨져있네요 ㅠㅠ
# 영어로 해서 저장하면 잘 불러와지는데 한글로 하면 깨져요... 어떻게 해야하나요 ㅠㅠㅠㅠ
readLines(file("INPUT/Memo.txt", encoding = "utf-8"))
readLines(file("INPUT/Memo.txt", encoding = "cp949"))
readLines(file("INPUT/Memo.txt", encoding = "euc-kr"))


# 문제가 이건데 각 대륙의gdpPercap의 평균을 구하려면 어떻게 해야하나요?
# 1. gapminder 데이터에서 각 대륙의 gdpPercap의 평균값을 plotting하고 범례를 추가하시오.

library(gapminder)
library(dplyr)
library(ggplot2)

data = gapminder %>%
  dplyr::group_by(continent) %>%
  dplyr::summarise(meanGdp = mean(gdpPercap, na.rm = TRUE))

ggplot(data, aes(x = continent, y = meanGdp, colour = continent)) +
  geom_point(size = 5)


# 2. gapminder 데이터에서 1952년의 gdpPercap과 lifeExp의 대륙별 평균을 추출한 후, 가로축에는 gdpPercap, 세로축에는 lifeExp를 나타낸 그래프로 ggplot2를 사용하여 시각화 하라.

dataL1 = gapminder %>%
  dplyr::filter(year == 1952) %>%
  dplyr::group_by(continent) %>%
  dplyr::summarise(
    meanGdp = mean(gdpPercap, na.rm = TRUE)
    , meanLife = mean(lifeExp, na.rm = TRUE)
  )

ggplot(dataL1, aes(x = meanGdp, y = meanLife, colour = continent)) +
  geom_point(size = 5)

# 보기도 어떻게 너오는지 알려주세요 내공겁니다
A = cbind(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))
colnames(A) = c("A", "B", "C")
rownames(A) = c("r1", "r2", "r3")

A[, "A"]
A[-c(2, 3),]
A[, 1]
A[, -(2:3)]

# 피어슨 상관계수를 계산하는 함수를 완성하시오.
# 함수명 : person_r
# 함수 파라미터 : x 벡터, y 벡터

pearson_r <- function(x, y) {
  cor(x, y, method = "pearson")
}

#

x = mtcars$mpg
y = mtcars$disp

pearson_r(x, y)

pp = read.csv(file = "INPUT/pu.csv")
pp

# **첨부한 csv파일을 활용하였습니다.

# class(pp)
# str(pp)
# pp$"총인구" <- as.integer( pp$"총인구")
# str(mct)
#인구수가 110만이상인 도시 추출
ct <- pp %>%
  filter(총인구 > 1100000)
# 광역시만 추출
mct <- ct[c(1, 2, 4, 6, 8, 9, 10),]
mct
pp <- mct$"총인구"
pp
pc <- mct$"행정구역별"
pc
df <- data.frame(pp, pc)
df

# 아래와 같이 표를 그려봤습니다, 근데 왼쪽에 있는 y축값이 저렇게 뜨는데 어떻게 바꿀수있나요?

library(scales)

ggplot(df, aes(x = pc, y = pp)) +
  geom_bar(stat = "identity",      # 막대 높이, y축값
           width = 0.5,            # 막대 폭
           fill = "darkred") +
  scale_y_continuous(breaks = seq(0, 10000000, 2500000), labels = scales::comma_format(big.mark = ',', decimal.mark = '.')) +
  ggtitle("행정구역별 총인구수") +
  theme(plot.title = element_text(size = 25,
                                  face = "bold",
                                  colour = "darkred")) +
  labs(x = "행정구역별", y = "총인구수")

# R프로그램에서 웹사이트 크롤링 할때 1월달만 나오지 않고 1~12월까지 나오게 하려면 코드를 어떻게 수정해야 하나요ㅠㅠ
# 알라딘 베스트셀러 2019년 1월 12월 1-200위 크롤링

library(rvest)
library(stringr)
library(tidyverse)
library(dplyr)
library(magrittr)

Sys.setlocale("LC_ALL", "English")
options(encoding = "UTF-8")
Sys.setenv(LANG = "en_US.UTF-8")

getUrlText = function(url, css) {
  xml2::read_html(url) %>%
    rvest::html_nodes(css = paste0(css)) %>%
    # rvest::html_nodes(xpath = paste0(xpath)) %>%
    rvest::html_text() %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_trim(side = "both")
}

dtDateList = seq.Date(as.Date("2019-01-01"), as.Date("2019-12-31"), "1 month")

data = data.frame()

for (i in 1:length(dtDateList)) {

  sYear = format(dtDateList[i], "%Y")
  sMonth = format(dtDateList[i], "%m")

  urlList = sprintf("https://www.aladin.co.kr/shop/common/wbest.aspx?BestType=Bestseller&BranchType=1&CID=0&Year=%s&Month=%s&Week=5&page=%d&cnt=1000&SortOrder=1", sYear, sMonth, seq(1:4))

  data = urlList %>%
    purrr::map(~getUrlText(.x, 'a.bo3 > b')) %>%
    unlist() %>%
    as.data.frame() %>%
    dplyr::mutate(
      year = sYear
      , month = sMonth
    ) %>%
    dplyr::bind_rows(data)
}

dataL1 = data %>%
  dplyr::arrange(year, month)

dplyr::tbl_df(dataL1)


# 네이버 뉴스 크롤링
library(rvest)
library(stringr)
library(tidyverse)
library(dplyr)
library(magrittr)

dtDate = seq.Date(as.Date("2019-01-01"), as.Date("2019-12-31"), "1 day")
sDate = format(dtDate, "%Y%m%d")

# Sys.setlocale("LC_ALL")
# options(encoding = "UTF-8")
# Sys.setenv(LANG = "en_US.UTF-8")

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

# R프로그램으로 프로그램짜기
# 주사위 8번 던질때 홀짝 분포를 히스토 그램으로 나타내고 싶습니다.

set.seed(1)                   # 난수로 생성된 수열을 고정시킴
X = 1:6  # 난수 생성 (모집단 생성)

# hist(X)  # 모집단의 빈도분포
# cat(mean(X), var(X), sd(X), "\n")  # 모집단의 평균/분산/표준편차


################################################### 표본평균집단에 대해서 ###########################################################
for (i in c(10, 50, 100, 250)) {

  DO = 1000    # Number of repetition
  # N = i          # Number of sample
  N = 8

  ## 비복원 추출(무작위 정렬) : 한번 뽑은 것을 다시 뽑을 수 없는 추출
  Sort = lapply(1:DO, function(i) sample(X, N, replace = TRUE))


  mapply(function(x, y) seq_len(x) + y,
         c(a = 1, b = 2, c = 3),  # names from first
         c(A = 10, B = 0, C = -10))


  Sort
  mapply(rep, 1:4, 4:1)


  Sort_mean = mapply(mean, Sort)

  mapply(mean, Sort)

  lapply(Sort, "%%", 2)

  ## FIG
  # nf <- layout(matrix(c(1,1),1,byrow=T), c(1,1), c(1,1)) ; layout.show(nf) ; par(mar=c(5,5,5,5)) ; par(cex=1.0)
  hist(Sort_mean, breaks = 50, xlab = "Sample Distribution Mean", xlim = c(0.3, 0.7), col = "light grey", border = "grey", xaxs = "i", yaxs = "i",
       main = paste0("Central Limit Theorem : number of sample = ", N, ", number of repetition = ", sprintf("%d", DO)))

  XX1 = mean(Sort_mean) + sd(Sort_mean)
  XX2 = mean(Sort_mean) - sd(Sort_mean)
  XX3 = mean(Sort_mean) + 2 * sd(Sort_mean)
  XX4 = mean(Sort_mean) - 2 * sd(Sort_mean)
  XX5 = mean(Sort_mean) + 3 * sd(Sort_mean)
  XX6 = mean(Sort_mean) - 3 * sd(Sort_mean)
  YY = max(hist(Sort_mean, breaks = 50, plot = F)$counts)
  lines(c(mean(Sort_mean), mean(Sort_mean)), c(0, YY), lty = 1, col = 4); text(mean(Sort_mean), YY / 2, "Mean")
  lines(c(XX1, XX1), c(0, YY), lty = 1, col = 2); text(XX1, YY / 2, "+1σ")
  lines(c(XX2, XX2), c(0, YY), lty = 1, col = 2); text(XX2, YY / 2, "-1σ")
  lines(c(XX3, XX3), c(0, YY), lty = 1, col = 2); text(XX3, YY / 2, "+2σ")
  lines(c(XX4, XX4), c(0, YY), lty = 1, col = 2); text(XX4, YY / 2, "-2σ")
  lines(c(XX5, XX5), c(0, YY), lty = 1, col = 2); text(XX5, YY / 2, "+3σ")
  lines(c(XX6, XX6), c(0, YY), lty = 1, col = 2); text(XX6, YY / 2, "-3σ")
  lines(c(max(Sort_mean), max(Sort_mean)), c(0, YY), lty = 1, col = 3); text(max(Sort_mean), YY / 2, "Max")
  lines(c(min(Sort_mean), min(Sort_mean)), c(0, YY), lty = 1, col = 3); text(min(Sort_mean), YY / 2, "Min")
}

#
# R 프로젝트 파일 변환 할수있나요?
# R프로젝트 파일 MS 워드나 한글 2020 파일로 나올수있게 변환가능한가요?...

library(officer)
library(ggplot2)

docx = read_docx()
#段落を追加
body_add_par(x = docx, value = "Word Test", style = "Normal", pos = "on")
body_add_par(x = docx, value = paste(1, 3, 5, 99999, 8, collapse = " "), style = "Normal", pos = "after")

cursor_reach(x = TestDocx2, keyword = 99999)
body_remove(x = TestDocx2)

print(TestDocx2, target = "TEST2.docx")

# 임시 표 생성
n = 15
TestData = data.frame(
  "Group" = sample(paste0("Group", 1:5), n, replace = TRUE)
  , "x" = sample(c(1:100), n, replace = TRUE)
  , "y" = sample(c(1:200), n, replace = TRUE)
)

src = tempfile(fileext = ".png")

# 임시 그림 생성
ggplot(TestData, aes(x, y)) +
  geom_point() +
  ggsave(src)

# 그림 생성
cursor_reach(x = docx, keyword = "TEST")
body_add_img(x = docx, src = src, width = 3, height = 3, style = "centered", pos = "after")

# 테이블 생성
body_add_table(x = docx, value = TestData, style = "Light List Accent 2", pos = "after", first_row = TRUE, first_column = FALSE, last_row = FALSE, last_column = FALSE, no_hband = FALSE, no_vband = TRUE)

print(docx, target = "OUTPUT/Test.docx")

#
library(plyr)
library(dplyr)

data = sample(1:30, 30, replace = TRUE)
tableData = table(data)


aa = as_data_frame(tableData) %>%
  dplyr::mutate(val = if_else(as.numeric(data) > 10, 10, 0)) %>%
  dplyr::select(data, val) %>%
  as.data.frame.table()

dd = table(aa)


library(data.table)
DF = data.frame(x = rep(c("x", "y", "z"), each = 2), y = c(1, 3, 6), row.names = LETTERS[1:6])
as.data.table(DF)
as.data.table(DF, keep.rownames = TRUE)
as.data.table(DF, keep.rownames = "rownames")

# home이라는 데이터셋을 사용하여 문제를 풀라는데 home이라는 데이터셋이 없어요.
# 그럼 home이라는 데이터를 다운받아야되는건가요? 아니면 만들수가 있는건가요?
# 다운받아야되는거라면 위치좀 알려주세요 저도 찾아봤는데 없어요.
# 만들수 있는거라면 어떤식으로 만들어야되는지 알려주세요.
# 처음에는 data.frame으로 하는줄 알았는데 안되서 올립니다.
# 빠른답변좀요.

# library(UsingR)
library(UsingR)

data(home)

home

# 1~30중 4의배수 또는 9의배수일 확률 R로 작성해야하는데
# 뭐가 틀린지 모르겠네요..

S <- c(1:30); S
B <- subset(S, (S %% 4) == 0 | (S %% 9) == 0); B

length(B) / length(S)

# apply 함수를 이용해서 각 열에서 숫자 1이 몇 개 있는지 개수를 출력해야 하는데요.
# length(which(mydata[1]=="1")) 로 일일이 출력하면 숫자가 잘 나오는데
# apply(mydata, 2, function(x) length(which(mydata[2]=="1")))로 하면 아예 다른 엄청 큰 수가 나옵니다.
# 왜 이런 건가요? 그리고 어떻게 고쳐야 하나요?

data = data.frame(
  key = c(1, 2, 3, 4, 1, 2, 3)
)
data

apply(data, 2, function(x) { sum(ifelse(x == 1, 1, 0)) })


# 데이터 내에서 일차함수의 기울기 값 만을 추출하고 싶은데
# R코드 관련해서 어떻게 작성하는지 아시는 분 있으신가요??

lmFit = lm(hp ~ mpg, data = mtcars)
lmFit

#  기울기
coef(lmFit)[2]

# 절편
coef(lmFit)[1]

#
# p(Z<z1) = 0.9474
# 에서 z1을 R에서 어떻게 구하죠... ?̊̈

rnormData = rnorm(n = 1000, mean = 0, sd = 1)

hist(rnormData)

conf = quantile(rnormData, p = 0.9474)
conf

text(conf, 0, "|", col = "red", cex = 2.0)


# hist()함수 쓸때 옵션중에 breaks=옵션이 계급간격을 조정해준다고 하는데 어떤구간을 얘기하는 건가요 ? breaks 옵션을간격을 조정해봐도 계급구간이 옵션을 준 숫자만큼 변형이 잘안되서요 어떤기준인지 알려주세요
# 그리고 probability , prob 옵션도 설명 부탁드려요 ~

data = c(71, 84, 68, 75, 91, 87, 63, 77, 81, 98, 57, 73, 74, 85, 50, 62, 66, 78, 65, 59, 75, 89, 94, 93, 86, 61, 87, 74, 70, 67)

# 빈도 분포
hist(data, breaks = seq(50, 98, 8), probability = FALSE)

# 밀도함수
hist(data, breaks = seq(50, 98, 8), probability = TRUE)

# 제가 R Studio 쌩 초보인데 csv 파일을 어떻게 불러오나요?
# 그리고 데이터객체 이름 정하고 정한 이름을 작업폴더에 csv 파일로 저장하는 법 알려주실 수 있을까요.

data = read.csv("INPUT/출산율.csv", encoding = "utf-8")
data = read.csv("INPUT/출산율.csv", encoding = "cp949")
data = read.csv("INPUT/출산율.csv", encoding = "euc-kr")

data

#

# 코드는 이렇게 작성하고
# 엑셀은 단순히 판매량(아이스크림이라 그런지 기본이 억대입니다.)만 작성하고 R로 불러왔습니다.
# 안보이는 부분은
# hist(icsale, main="icecream sales volume", xlab="sales volume, ylab="frequency"로 작성했습니다.
# 근데 실행하니깐 x는 반드시 숫자이어야 한다고 하네요ㅜㅜ
# 어디가 잘못 된건지 모르겠습니다

hist(as.numeric(icsale$판매량), main = "icecream sales volume", xlab = "sales volume", ylab = "frequency")

# 안녕하세요, 고수님들!!
# R Studio 왕초보자입니다.
# 다름이 아니라 학교과제중에
# 저기 제가 불러온 표에서 placebo에 관한 평균과 wirkstoff에 관한 평균을 구하라는 문제가 있는데요,
# 도대체 변수를 어떻게 지정하고 어떻게 평균을 구해야하는지 막막해서 질문을 올립니다.
# 제발 답변 꼭 부탁드립니다.!!!

data = data.frame(
  num = c(10010, 10012, 10015, 10017, 10021, 10023, 10044, 10055, 10059, 10060, 10065, 10070, 10072, 10074, 10079, 10080, 10090, 10093, 10096, 10101, 10103, 10118, 10123, 10125, 10126)
  , gender = c(1, 2, 1, 1, 2, 2, 1, 2, 1, 1, 1, 2, 1, 2, 2, 2, 1, 2, 2, 1, 1, 1, 2, 1, 1)
  , kidney = c(184.0, 160.3, 179.3, 176.2, 166.4, 168.0, 177.0, 162.4, 170.9, 188.3, 174.3, 171.7, 185.3, 165.5, 172.2, 168.6, 176.0, 168.1, 165.9, 183.0, 163.2, 176.5, 165.3, 180.9, 176.5)
  , weight = c(76.4, 57.2, 74.2, 68.2, 56.6, 64.8, 67.5, 51.2, 65.8, 77.5, 64.2, 62.6, 80.8, 64.5, 81.6, 68.0, 81.3, 72.3, 54.1, 84.0, 63.0, 68.3, 54.7, 96.0, 74.4)
  , legLength = c(101.6, 90.2, 99.4, 97.1, 91.0, 92.9, 103.6, 95.0, 79.5, 103.1, 102.7, 99.6, 101.2, 93.5, 97.5, 94.0, 95.6, 95.4, 92.6, 98.4, 86.7, 102.6, 96.5, 103.5, 95.1)
  , running = c(6.17, 6.87, 6.39, 6.77, 6.93, 7.15, 7.68, 7.50, 6.70, 6.58, 6.39, 6.92, 6.38, 6.91, 7.35, 7.12, 6.55, 7.26, 6.96, 6.48, 6.84, 6.00, 7.48, 6.71, 6.73)
)

# kidney 평균
mean(data$kidney, na.rm = TRUE)

# weight 평균
mean(data$weight, na.rm = TRUE)

# [,1]         [,2]
# [1,] "1서소문동"   "6개포동"
# [2,] "2태평로"     "7개포동"
# [3,] "3서초동"     "8관훈동"
# [4,] "4관훈동"     "9남대문"
# [5,] "5분당삼평"   "10남대문"

# 안녕하세요
# R에서 위 행렬을  한줄의 벡터로 만들려고 합니다
# as.vector 쓰면 1열 전체 뒤에 2열이 붙어서 고민인데요
# 1 6 2 7 3 8 ... 순으로 벡터를 만드는 방법이 없을까요?

# 예시)
# "1서소문동","6개포동","2태평로","7개포동","3","8"...

data = data.frame(
  col1 = c("1서소문동", "2태평로", "3서초동")
  , col2 = c("6개포동", "7개포동", "8관훈동")
)

dataL1 = c()

for (i in 1:nrow(data)) {
  for (j in 1:ncol(data)) {
    val = data[i, j]

    dataL1 = append(dataL1, val)
  }
}

dataL1

# R에서 제공하는 trees 데이터셋에 대해 다음 문제를 해결하기 위한 R 코드를 작성하시오.

# (1) 나무의 지름(Girth)과 높이(Height)에 대해 산점도와 상관계수를 보이시오.
library(ggpubr)
library(GGally)
library(ggplot2)

ggscatter(trees, x = "Girth", y = "Height", add = "reg.line") +
  stat_cor(label.x = 7, label.y = 90) +
  stat_regline_equation(label.x = 7, label.y = 85) +
  theme_bw()

# (2) trees데이터셋에 존재하는 3개 변수 간의 산점도와 상관계수를 보이시오.
ggpairs(trees)


# trees데이터셋에 존재하는 3개 변수 간의 산점도와 상관계수를 보이시오
# 인데 이게
# cor(iris[ , 1:3])이 맞는지 아닌지 모르겠네요
cor(trees)

# 데이터를 가공하는데 궁금증이 생겨 질문남깁니다.
# 예를들어
# 1 1 1 1
# 2 2 2 2
# 3 3 3 3
# 과 같은 데이터가 존재할때 이를
# 1
# 1
# 1
# 1
# 2
# 2
# 2
# 2
# 3
# 3
# 3
# 3
# 과 같은 모양으로 바꾸는 함수는 어떤걸 써야할까요?
#     for문과 cbind를 함께 사용하고 t를 써서 방향을 돌려서 데이터를 뽑아내는걸 생각했는데 뜻대로 되지를 않네요. 데이터의 갯수가 3만개가 넘어 for문 사용이 필수적인 상황입니다.
# 도움을 받을 수 있을까요?


data = data.frame(
  col1 = c(1, 2, 3)
  , col2 = c(1, 2, 3)
)

dataL1 = data.frame()


for (i in 1:nrow(data)) {
  for (j in 1:ncol(data)) {

    val = data.frame(data[i, j])

    dataL1 = dplyr::bind_rows(dataL1, val)
  }
}

colnames(dataL1) = c("val")
dataL1

# R프로그래밍 5x5의 data.frame 이나 matrix를 만들려고 하는데 초기화된 값을
# function함수로 만들어 놓고 싶은데 코드를 어떻게 구성해야 하나요 ??

initData = function(initVal, ncol, nrow) {
  maData = matrix(initVal, ncol = ncol, nrow = nrow)

  return(maData)
}

maData = initData(2, 5, 5)
maData


# R에서 급여는 급여끼리 묶어서 합계를 구하고 평균을 구하는 방법을 알고 싶어요
# aggregate 함수 써서 사용하는거 맞나요?
# 급여입원료, 급여 진찰료 등등 급여 들어 있는 글자(노란색으로 밑줄친 부분)끼리 모여서 합계 , 평균 구하는 방법 알려주세요

library(dplyr)

data = data.frame(
  급여진찰료 = c(1, 2, 3)
  , 진찰료 = c(1, 2, 3)
  , 급여입원료 = c(4, 5, 6)
)


data %>%
  dplyr::select(starts_with("급여")) %>%
  dplyr::summarise_all(funs(
    sum(., na.rm = TRUE) # 합계
    , mean(., na.rm = TRUE) # 평균값
  )) %>%
  dplyr::glimpse()

# R에서 set.seed(1)
# x <-c("T","R","I","A","N","G","L","E","S")
# sample(x,n,replace=T,prob=NULL)
# n=10
# 을 이용해서 T,R,I,A,N,G,L,E,S 을 복원 추출로 10번 뽑는 함수를 만들었는데요
# 혹시 9개단어 각각의 대해서 10번 복원 추출했을시 2번 이상 뽑힌 적이 있는 지 여부를 TRUE,FLASE로
# 출력하는 방법을 알려주실수 있나요ㅠㅠ

set.seed(1)
x <- c("T", "R", "I", "A", "N", "G", "L", "E", "S")
n = 10

data = data.frame(
  key = sample(x, n, replace = T, prob = NULL)
)

dataL1 = data %>%
  dplyr::group_by(key) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(
    isFlag = dplyr::if_else(n > 1, TRUE, FALSE)
  )

dplyr::tbl_df(dataL1)

#
# 풀다가 전혀 감이 안와서 물어보는데
# mlbench 패키지에서 제공하는 Ionosphere 데이터셋에 대해 문제를 해결하기 위한 R 코드를 작성하라는 문제에요!

# (1) 다음과 같이 Ionospere 데이터셋을 myds에 저장하시오.

library(mlbench)
library(dplyr)

data("Ionosphere")
myds <- Ionosphere


# (2) myds에서 class와 V1열의 값을 그룹으로 하여 V3~V10 열의 값들의 표준편차를 출력하시오. (주의 : 집계 작업시 팩터 타입의 열은 제외해야 한다.)
# 이 문제를 어떻게 풀어야 할지 감이 안와서 도와주세요...

dataL1 = myds %>%
  dplyr::select(V1, V3:V10) %>%
  dplyr::group_by(V1) %>%
  dplyr::summarise_all(funs(
    sd(., na.rm = TRUE) # 표준편차
  ))

dplyr::tbl_df(dataL1)

# 함수를 만드는 과정에서 if else를 쓰는데 function(al)이라 하면 al이 "a","b","c","d" 이거 가 아닌 문자를 입력했을때 '입력하신 문자가 없습니다'를 출력하고 abcd가 맞다면 그에 맞는 함수값을 내고 싶은데
# R에서의 또는 연산자인 |이것을 쓰자니 논리 연산이 아니라 적용이 안됩니다. 어떻게 해야할까요.
# 문제가 생긴 부분을 질문 답글에 써놓았습니다.

getVal = function(param) {

  compList = c("a", "b", "c", "d")

  for (i in 1:length(compList)) {
    if (param == compList[i]) {
      return(param)
    }
  }

  return("입력하신 문자가 없습니다")
}

# 테스트 수행
checkList = c("a", "b", "c", "d", "e")

for (i in 1:length(checkList)) {
  cat(getVal(checkList[i]), "\n")
}

# R언어로 자판기같은  코드를 짜려고 하는데
# R언어는 다른 언어들보다 흔하지 않아서
# 짜는 틀조차 시작을 못하겠어요. 혹시 기본 틀을 어떻게 만들어야 할지 도와주세요ㅠ
# 예를 들면 커피 브랜드와 커피을 입력하면 결과값으로 가격이 출력되는 코드
# 부탁드려요

library(dplyr)

coffeeData = data.frame(
  "브랜드" = c("백다방", "백다방", "스타벅스", "스타벅스")
  , "종류" = c("아이스 아메리카노", "따뜻한 아메리카노", "아이스 아메리카노", "따뜻한 아메리카노")
  , "가격" = c(1000, 2000, 3000, 4000)
)

getVal = function(inBrand, inType) {

  resultData = coffeeData %>%
    dplyr::filter(
      브랜드 == inBrand
      , 종류 == inType
    )

  returnVal = ifelse(length(resultData$가격) == 1, resultData$가격, NA)

  return(returnVal)
}

getVal("백다방", "아이스 아메리카노")
getVal("백다방", "아이스 아메리카노2")

# 이런 그래프를 r 스튜디오에서 어떻게 만드나요? 단, 그래프 위에 I 자 처럼 생긴 막대는 빼고요
# Grouped Bar Plot
counts = table(mtcars$vs, mtcars$gear)
barplot(counts, main = "Car Distribution by Gears and VS",
        xlab = "Number of Gears", col = c("darkblue", "red"),
        legend = rownames(counts), beside = TRUE)

# # 1부터 10까지 자연수의 평균을 반복문을 사용해서 구하는 거 어떻게 하나요?
sum = 0
list = c(1:10)

for (i in list) {
  sum = sum + i
}

mean = sum / length(list)
mean

# R에서 사용자 지정 함수로는 주어진 수의 짝수/홀수를 구분하는 함수를 많이 만들었었는데, 특정 패키지에 이러한 함수가 내장되어있다고 하는데 혹시 그 패키지가 어떤 것이고, 함수명이 무엇인지 알 수 있을까요? 내공 100 겁니다.
library(gtools)

x = c(1:10)
x

# 홀수
gtools::odd(x)

# 짝수
gtools::even(x)

# 1부터10까지 자연수의 평균, 분산, 표준편차를 구하시오 (단, 반 복문을 사용하시오)
# 반복문을 사용해야 되는데

list = seq(0, 9, 1)

# 평균
sum = 0
for (i in list) {
  sum = sum + i
}

mean = sum / length(list)
mean

# 분산
varSum = 0
for (i in list) {
  varSum = varSum + ((i - mean)^2)
}

var = varSum / (length(list) - 1)
var

# 표준편차
sd = sqrt(var)
sd

# 제가 가지고 있는 시간을 시간대별로 구분하고 싶은데 어떻게 하면 좋을까요
# 6시~8시를 t에 넣고, 8시~10시를 t1에 넣고 이런식으로 하고 싶어요
library(dplyr)
library(lubridate)

dtDateTime = seq(as.POSIXct("2020-01-01 00:00"), as.POSIXct("2020-01-02 00:00"), "1 hour") %>%
  as.tibble()

data = dtDateTime %>%
  dplyr::mutate(
    dtHour = lubridate::hour(value)
    , type = case_when(
      dtHour >= 6 & dtHour < 8 ~ "t"
      , dtHour >= 8 & dtHour < 10 ~ "t1"
    )
  )
data

# 사진과 같이 생긴 자료의 상자그림을 그리려고 합니다!
# 저 자료를 r에 어떻게 입력하면 좋을까요??ㅠㅜ
library(ggvis)

mtcars %>%
  ggvis(x = ~mpg, y = ~wt) %>%
  layer_points()

# 사진과 같이 생긴 자료의 상자그림을 그리려고 합니다!
# 저 자료를 r에 어떻게 입력하면 좋을까요??ㅠㅜ

library(ggplot2)

ggplot(mtcars, aes(x = mpg, y = wt)) +
  geom_point() +
  scale_x_continuous(breaks = c(10, 11, 12, 13), labels = c(10, 11, 12, 13))

# is.even(x)가 어느 패키지를 로드해야 사용할 수 있는지 궁금합니다
library(schoolmath)

x = c(1, 2, 3, 4, 5, 6, 7)
is.even(x)

# 프로그램 R에서 factor가 자동적으로 생성되지 않습니다 ㅠ
tips = read.csv('https://raw.githubusercontent.com/mwaskom/seaborn-data/master/tips.csv', stringsAsFactors = TRUE)

str(tips)


# R Studio를 사용합니다
# 제가 원하는 내용은 3개의  지도가 하나의 지도로 나타나는 것입니다.
# 코드는

library(readxl)
library(leaflet)
library(tidyverse)

metroa <- read_excel('INPUT/knowledgeIn/서울지하철2호선위경도정보.xlsx', sheet = 2)
metrob <- read_excel('INPUT/knowledgeIn/서울지하철2호선위경도정보.xlsx', sheet = 3)
metroc <- read_excel('INPUT/knowledgeIn/서울지하철2호선위경도정보.xlsx', sheet = 4)

# data = data.frame()
# data = dplyr::bind_rows(data, data.frame(metroa, type = "A"))
# data = dplyr::bind_rows(data, data.frame(metroa, type = "B"))
# data = dplyr::bind_rows(data, data.frame(metroa, type = "C"))
#
# data %>%
#     leaflet() %>%
#     addTiles() %>%
#     setView(mean(metroa$LON), mean(metroa$LAT), zoom=12) %>%
#     addPolylines(~LON, ~LAT, weight=7, color='red', group = "type", popup = ~type) %>%
#     addCircles(~LON, ~LAT, weight=7, color='black', group = "type", popup = ~type)%>%
#     addLegend(colors = 'black',labels = ~type, title='서울지하철2호선')
#
# metro2<-leaflet(metrob)%>%
#     addTiles()%>%
#     setView(mean(metroa$LON),mean(metroa$LAT),zoom=12)%>%
# addPolylines(~LON,~LAT,weight=7,color='red',
#              popup = metroa$역명)%>%
#     addCircles(~LON,~LAT,weight=7,color='black',
#                popup = metroa$역명)%>%
#     addLegend(colors = 'black',labels = '역명',title='서울지하철2호선')
#
# metro2<-leaflet(metroc)%>%
#     addTiles()%>%
#     setView(mean(metroa$LON),mean(metroa$LAT),zoom=12)%>%
# addPolylines(~LON,~LAT,weight=7,color='red',
#              popup = metroa$역명)%>%
#     addCircles(~LON,~LAT,weight=7,color='black',
#                popup = metroa$역명)%>%
#     addLegend(colors = 'black',labels = '역명',title='서울지하철2호선')

metro2
# 입니다.

# 긴데 간단하게 설명하면 metro2에 들어가는 3개의 그림이 모두 같은데 삽입되는 데이터가 metro a,b,c라는 것만 다릅니다.

# r프로그래밍으로 줄기 잎 그림 그려야하는데 초보자라 잘 모르겠네요.. 데이터는 이렇게 주어졌습니다!

data = c(70, 72, 75, 64, 58, 82, 80, 82, 76, 75, 68, 65, 57, 78, 85, 72)

stem(data)


# R studio 사용하려는데    작업 디렉토리 지정한다는게 저장한다는걸 의미하는 건가요??

# getwd()
# setwd()   이거 두개의 개념도 잘 이해가 안됩니다 ㅠㅠㅠ

# 현재 작업 디렉터리 확인
getwd()

# 작업 디렉터리 설정
setwd("E:/02. 블로그/지식iN")

# R Stiduo 에서 getData 함수를 사용하려하는데요. 함수를 찾을 수 없다고 나옵니다.
# getData를 사용하기 위한 별도 library 가 필요한지 알려주시면 감사하겠습니다.

library(raster)

raster::getData

# R에서 OpenAPI 불러오기(XML형식)
# 안녕하세요.
# 과제하다가
# 모르는것이 생겨서 여쭤봅니다.

# 전체코드

# API 실시간 데이터
library(XML)
library(rjson)
library(RCurl)
library(plyr)
library(jsonlite)
library(rvest)
library(dplyr)

# url 인자
baseurl <- "http://openapi.airkorea.or.kr/openapi/services/rest/ArpltnInforInqireSvc/getCtprvnRltmMesureDnsty?"

sidoName <- "서울"
pageNo <- "1" # 페이지 번호
numOfRows <- "10" # 한 페이지 결과 수
serviceKey <- globalVar$dataKey
ver <- "1.3"
returnType = "json"

# url 조합
requestURL <- paste0(baseurl,
                     "sidoName=", sidoName,
                     "&pageNo=", pageNo,
                     "&numOfRows=", numOfRows,
                     "&ServiceKey=", serviceKey,
                     "&ver=", ver,
                     "&_returnType=", returnType
)
requestURL

reqData = jsonlite::fromJSON(requestURL)

if (length(reqData$list) > 0) {
  data = data.frame(reqData$list)

  dplyr::tbl_df(data)
}

# 바로 위 table <- rbind.fill(tmp, table)에서 오류가 생깁니다.
# "All inputs to rbind.fill must be data.frames"
# 그래서 두 인자를 as.data.frame으로 먹였는데도 같은 오류가 나옵니다.
# 이걸 어찌해야하나 싶습니다...ㅠㅠ


# 안녕하세요! 공모전 때문에 R STUDIO를 사용해서 데이터 전처리를 거치고 있는 대학생입니다.
# 주최측에서 데이터를 반출하려면 무조건 어떤 항목에 대한 집계과정을 거쳐야 한다고 하여 연월을 삭제하고 전체년도에 대해서 소분류 업종명 기준으로 20대~60대 성별 카드매출 데이터를 만드는 중입니다.
# 그러나 제가 R에 무지하여 검은색 박스로 표시한 것처럼 소분류 업종별로 하나씩 나타나지 않고 업종이 중복되는 상황이 발생하였습니다.
# 이렇게 중복되는 행을 하나의 행으로 집계하려면 어떻게 해야할까요?
# 도와주세요ㅠㅠㅠㅠ

data = data.frame(
  id = c(1, 1, 1, 2, 2, 2, 3)
  , stopSequence = c(1, 2, 3, 3, 1, 4, 3)
)

dataL1 = data %>%
  tidyr::gather(key = "key", value = "val")

dataL2 = dataL1 %>%
  dplyr::group_by(key) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  )


# r프로그래밍으로 bar chart를 만들었는데
# png(file = "파일 이름")  ~~~
# 으로 저장하니까
# 원랜 이렇게 나와야하는데.. 길이 때문인가요?
# legned그것도 만들어서 크게 상관은 없지만 좀 찝찝해서요

saveFile = 'Image_01.png'

png(file = paste(globalVar$figConfig, saveFile, sep = '/'), width = 6, height = 4, units = "in", res = 1200)

# Grouped Bar Plot
counts = table(mtcars$vs, mtcars$gear)
barplot(counts, main = "Car Distribution by Gears and VS",
        xlab = "Number of Gears", col = c("darkblue", "red"),
        legend = rownames(counts), beside = TRUE)

dev.off()


# 안녕하세요 포로리님 ㅜR 스튜디오 질문이 있습니다 오늘 갓 입문한 초보입니다
# 교수님께서 a: 21(70%) b:1 (3%) c:8 (27%) 의 각 참여비율을 구해라고 과제를 내주셨는데 그 참여비율이 21 *0.7 이렇게 함수를 작성하면 나오는 값이 14.7이 나오는데 참여비율 이렇게 구하는게 맞나요?!
#     또한 a / c 의 나머지값을 구하라는데 a%%14.7하면 6.3이 나옵니다 근데 c%% 2.16하면 1.51 이렇게 나오는데 이게 맞는건가요?! ㅜㅜㅜ 정말 간절합니다 ㅜㅜㅜ

total = 30

# 각 사업에 대한 참여 비율
a = round(21 / total, 2) * 100
b = round(1 / total, 2) * 100
c = round(8 / total, 2) * 100

a; b; c

# 나머지 구하기
total %% 21


# 통계에서 Estimate, pr 등 각각의 분석 값이 무엇을 의미하는지 알려주세요!
# Estimate (회귀계수)의 경우 (10615.0 * (독립변수)) - 8659.2  = (종속변수)를 의미하고
# 각 회귀계수에 대한 유의성 (Pr)은 0.0387 및 <2e-16로서 통계적으로 유의합니다.
# 전체 회귀모형에 대한 설명력 (R-squared) 및 유의성 (p-value)는 0.9954 및 2.2e-16로서 유의합니다.

library(tidyverse)
library(easycsv)
library(Hmisc)
library(lubridate)

# fileList = Sys.glob("review.txt")
# "E:/02. 블로그/지식iN/INPUT/knowledgeIn"
fileList = Sys.glob(paste0(globalVar$inpConfig, "/Input/*.dat"))

for (fileInfo in fileList) {
  data = readr::read_csv(file = file, skip = 3)

  colnames(data) = c("TOA5", "3880", "CR3000", "3880_2", "CR3000.Std.31.03", "CPU:171114_kopri_ec2.CR3", "2214", "met"
    , "TIMESTAMP", "RECORD", "CM3UP_Avg", "CM3DN_Avg", "CG3UP_Avg", "CG3DN_Avg", "CNR4TC_Avg", "CNR4TK_Avg", "NetRs_Avg", "NetRl_Avg", "Albedo_Avg", "UpTot_Avg", "DnTot_Avg", "NetTot_Avg", "CG3UpCo_Avg", "CG3DnCo_Avg", "AirTC_Avg", "RH", "Period(1)", "Period(2)", "Period(3)", "Period(4)", "VWC(1)", "VWC(2)", "VWC(3)", "VWC(4)", "shf_Avg(1)", "shf_Avg(2)", "shf_Avg(3)", "shf_cal(1)", "shf_cal(2)", "shf_cal(3)", "soilT_Avg(1)", "soilT_Avg(2)", "BP_kPa", "WS", "WD", "WD_std")

  dataL1 = data %>%
    dplyr::mutate(
      dtDateTime = format(TOA5, "%Y-%m-%d %H%M")
    ) %>%
    dplyr::select(dtDateTime, 3, 4, 15, 16, 17, 18, 23, 24, 25, 26, 27, 28, 29, 33, 34, 35, 36, 37)


  outFileInfo = stringr::str_replace_all(fileInfo, "Input", "Output")
  readr::write_csv(x = dataL1, path = outFileInfo)
}


#==========================================
# 교재 연습문제 2장
#==========================================
library(readr)
library(lubridate)

dtDate = readr::parse_date("1945-08-15", "%Y-%m-%d")

getWday = lubridate::wday(dtDate, label = TRUE, abbr = FALSE)
getWday

#==========================================
# 교재 연습문제 4장
#==========================================
mywage = function(weeklyWorkingTime) {
  refHourlyWage = 1.0
  refWeeklyWorkingTime = 40

  if (weeklyWorkingTime > refWeeklyWorkingTime) {
    workingTime = refWeeklyWorkingTime + ((weeklyWorkingTime - refWeeklyWorkingTime) * 1.5)
  } else {
    workingTime = weeklyWorkingTime
  }

  result = refHourlyWage * workingTime

  return(result)
}

mywage(10)
mywage(50)
mywage(60)
mywage(70)

#==========================================
# 교재 연습문제 5장
#==========================================
n = as.numeric(readline("[INFO] n : "))
p = as.numeric(readline("[INFO] p : "))

seqList = seq(1, n)
sum = sum(seqList**p, na.rm = TRUE)


#==========================================
# 교재 연습문제 8장
#==========================================
library(readr)
library(tidyverse)
library(rapport)
library(ggplot2)
library(scales)

# 2번 문제
fileInfo = Sys.glob(paste0(globalVar$inpConfig, "/rpy/nutrient2.csv"))
data = readr::read_csv(file = fileInfo)

dataL1 = data %>%
  dplyr::na_if(0) %>%
  dplyr::select(everything()) %>%
  dplyr::summarise_all(funs(sum(is.na(.))))

dataL1

# summary(dataL1)
dataL2 = data %>%
  dplyr::na_if(0) %>%
  dplyr::select(everything()) %>%
  dplyr::summarise_all(funs(
    mean(., na.rm = TRUE) # 평균
    , sd(., na.rm = TRUE) # 표준편차
    , min(., na.rm = TRUE) # 최솟값
    , max(., na.rm = TRUE) # 최댓값
    , median(., na.rm = TRUE) # 중앙값
    , quantile(., 0.25, na.rm = TRUE) # 제1사분위수
    , quantile(., 0.75, na.rm = TRUE) # 제3사분위수
  ))

dplyr::glimpse(dataL2)

dataL2 = data %>%
  dplyr::na_if(0) %>%
  tidyr::gather(-id, key = "key", value = "val")

# 상자그림
ggplot(dataL2, aes(x = key, y = val, fill = key)) +
  geom_boxplot(alpha = 0.6) +
  ylim(0, 5000)

# 히스토그램
ggplot(dataL2, aes(x = val, fill = key)) +
  geom_histogram(binwidth = 100, alpha = 0.6) +
  xlim(0, 5000) +
  ylim(0, 1000)


# 3번 문제
fileList = Sys.glob(paste0(globalVar$inpConfig, "/rpy/pima2.csv"))
data = readr::read_csv(file = fileList)

dataL1 = data %>%
  dplyr::group_by(diabetes) %>%
  dplyr::summarise(cnt = n()) %>%
  dplyr::mutate(ratio = (cnt / sum(cnt, na.rm = TRUE)) * 100)

# 막대그림
ggplot(dataL1, aes(x = diabetes, y = cnt, fill = diabetes)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = cnt), vjust = 1.6, color = "white", size = 5)

# 원그림
ggplot(dataL1, aes(x = "", y = ratio, fill = diabetes)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme(axis.text.x = element_blank()) +
  geom_text(aes(y = ratio / 2.0 + c(0, cumsum(ratio)[-length(ratio)]),
                label = scales::percent(ratio / 100)), size = 5) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 14, face = "bold")
  )


# 2번 문제
dataL2 = data %>%
  dplyr::na_if(0) %>%
  dplyr::select(glucose, pregnant, triceps, insulin, mass, pedigree, age, diabetes) %>%
  dplyr::group_by(diabetes) %>%
  dplyr::summarise_all(funs(
    mean(., na.rm = TRUE) # 평균
    , min(., na.rm = TRUE) # 최솟값
    , max(., na.rm = TRUE) # 최댓값
    , median(., na.rm = TRUE) # 중앙값
    , quantile(., 0.25, na.rm = TRUE) # 제1사분위수
    , quantile(., 0.75, na.rm = TRUE) # 제3사분위수
  ))

dplyr::glimpse(dataL2)


dataL3 = data %>%
  dplyr::na_if(0) %>%
  dplyr::select(glucose, pregnant, triceps, insulin, mass, pedigree, age, diabetes) %>%
  tidyr::gather(-diabetes, key = "key", value = "val")

# 히스토그램
ggplot(dataL3, aes(x = val, fill = diabetes)) +
  geom_histogram(binwidth = 50, alpha = 0.6) +
  facet_wrap(~key)

# 상자 그림
ggplot(dataL3, aes(x = key, y = val, fill = diabetes)) +
  geom_boxplot()

# 3번 문제
dataL4 = data %>%
  dplyr::mutate(type = dplyr::case_when(
    20 <= age & age <= 30 ~ "20-30"
    , 31 <= age & age <= 40 ~ "31-40"
    , 41 <= age & age <= 50 ~ "41-50"
    , TRUE ~ "50+"
  ))

tableL4 = table(dataL4$diabetes, dataL4$type)

ggplot(dataL4, aes(x = type, fill = diabetes)) +
  geom_bar(position = "dodge")

# 4번 문제
dataL4 = data %>%
  dplyr::mutate(type = dplyr::case_when(
    0 <= pregnant & pregnant <= 5 ~ "0-5"
    , 6 <= pregnant & pregnant <= 10 ~ "6-10"
    , TRUE ~ "10+"
  ))

tableL4 = table(dataL4$diabetes, dataL4$type)

ggplot(dataL4, aes(x = type, fill = diabetes)) +
  geom_bar(position = "dodge")

# 5번 문제
dataL5 = dataL4 %>%
  dplyr::group_by(diabetes, type) %>%
  dplyr::summarise_all(funs(
    mean(., na.rm = TRUE) # 평균
    , sd(., na.rm = TRUE) # 표준편차
  ))

dplyr::glimpse(dataL5)


#==========================================
# 교재 연습문제 9장
#==========================================
library(MASS)
library(moonBook)
library(webr)
library(ggplot2)
library(tidyverse)

# 2번 문제
data = data.frame(
  Placebo = c(105, 119, 100, 97, 96, 101, 94, 95, 98)
  , Caffeine = c(96, 99, 94, 89, 96, 93, 88, 105, 88)
)

dataL1 = data %>%
  tidyr::gather(key = "key", value = "val")

# P값이 0.53으로서 귀무가설 기각하지 못함 (두 캡슐의 분산 차이가 없다)
# 따라서 등분산 조건 (var.equal = TRUE)
fTest = var.test(val ~ key, data = dataL1)

plot(fTest) +
  xlim(0, 3) +
  ggsave(filename = paste(globalVar$figConfig, "fTest.png", sep = "/"), width = 10, height = 6, dpi = 600)

# P값이 0.063로서 귀무가설 기각 (두 캡슐의 차이가 있다)
tTest = t.test(val ~ key, data = dataL1, var.equal = TRUE)
tTest

plot(tTest) +
  xlim(-5, 5) +
  ggsave(filename = paste(globalVar$figConfig, "tTest.png", sep = "/"), width = 10, height = 6, dpi = 600)


# 3번 문제
fileInfo = Sys.glob(paste0(globalVar$inpConfig, "/rpy/mtcars.csv"))
data = readr::read_csv(file = fileList)

# 자동차 기어의 종류
dataL1 = data %>%
  dplyr::select(am, mpg) %>%
  tidyr::gather(key = "key", value = "val")

# P값이 0.01으로서 귀무가설 기각 (두 변수간의 분산 차이가 있다)
# 따라서 상이한 분산 조건 (var.equal = FALSE)
fTest = var.test(val ~ key, data = dataL1)
fTest

plot(fTest) +
  xlim(0, 3) +
  ggsave(filename = paste(globalVar$figConfig, "fTest.png", sep = "/"), width = 10, height = 6, dpi = 600)

# P값이 0.01 이하로서 귀무가설 기각 (두 변수간의 차이가 있다)
tTest = t.test(val ~ key, data = dataL1, var.equal = FALSE)
tTest

plot(tTest) +
  xlim(-5, 5) +
  ggsave(filename = paste(globalVar$figConfig, "tTest.png", sep = "/"), width = 10, height = 6, dpi = 600)


# 자동차 엔진 종류
dataL1 = data %>%
  dplyr::select(vs, mpg) %>%
  tidyr::gather(key = "key", value = "val")

# P값이 0.01 이하로서 귀무가설 기각 (두 변수간의 분산 차이가 있다)
# 따라서 상이한 분산 조건 (var.equal = FALSE)
fTest = var.test(val ~ key, data = dataL1)
fTest

plot(fTest) +
  xlim(0, 3) +
  ggsave(filename = paste(globalVar$figConfig, "fTest.png", sep = "/"), width = 10, height = 6, dpi = 600)

# P값이 0.01 이하로서 귀무가설 기각 (두 변수간의 차이가 있다)
tTest = t.test(val ~ key, data = dataL1, var.equal = FALSE)
tTest

plot(tTest) +
  xlim(-5, 5) +
  ggsave(filename = paste(globalVar$figConfig, "tTest.png", sep = "/"), width = 10, height = 6, dpi = 600)


#==========================================
# 교재 연습문제 10장
#==========================================
library(GGally)

# 연습문제 1번
fileInfo = Sys.glob(paste0(globalVar$inpConfig, "/rpy/computer.csv"))
data = readr::read_csv(file = fileInfo)

dataL1 = data %>%
  dplyr::select(erp, myct, mmax, cach, chmin, chmax, prpe)

# 산점도
ggpairs(dataL1)

# 상관계수 행렬
cor(dataL1)

# 다중 선형 회귀모형
lmFit = lm(erp ~ myct + mmax + cach + chmin + chmax, data = dataL1)
summary(lmFit)

# 연습문제 2번
fileInfo = Sys.glob(paste0(globalVar$inpConfig, "/rpy/mtcars.csv"))
data = readr::read_csv(file = fileInfo)

dataL1 = data %>%
  dplyr::select(-X1)

lmFit = lm(mpg ~ ., data = dataL1)

stepAic = MASS::stepAIC(lmFit, direction = "both")

summary(stepAic)


#==========================================
# 교재 연습문제 11장
#==========================================
# 연습문제 1번
fileInfo = Sys.glob(paste0(globalVar$inpConfig, "/rpy/bateriasoap.csv"))
data = readr::read_csv(file = fileInfo)

data$Method = as.factor(data$Method)

fit = aov(BacterialCounts ~ Method, data = data)

tukeyTest = TukeyHSD(fit)
tukeyTest

plot(tukeyTest)

# 연습문제 2번
fileInfo = Sys.glob(paste0(globalVar$inpConfig, "/rpy/downloading.csv"))
data = readr::read_csv(file = fileInfo)

data$TimeofDay = as.factor(data$TimeofDay)

fit = aov(`Time(Sec)` ~ TimeofDay, data = data)

tukeyTest = TukeyHSD(fit)
tukeyTest

plot(tukeyTest)

# 2007년, 2012년, 2017년 인플레이션이 높은 5개 지역을 표시하는 바 그래프를 그리시오.
# 그릴 때 각 연도별로 인플레이션이 높은 지역순으로 나열하시오.

library(tidytext)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(forcats)

fileList = Sys.glob(paste0(globalVar$inpConfig, "/inflation.xlsx"))
data = readxl::read_excel(path = fileList, sheet = "데이터")

dataL1 = data %>%
  dplyr::na_if("-") %>%
  na.omit() %>%
  tidyr::gather(-region, key = "year", value = "val") %>%
  readr::type_convert()


dataL2 = dataL1 %>%
  filter(year %in% c(2007, 2012, 2017)) %>%
  group_by(year) %>%
  top_n(5, region) %>%
  ungroup()

dataL3 = dataL2 %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(meanVal = mean(val, na.rm = TRUE)) %>%
  dplyr::arrange(desc(meanVal))


dataL2$region = forcats::fct_relevel(dataL2$region, dataL3$region)

ggplot(dataL2, aes(x = as.factor(year), y = val, fill = region)) +
  geom_col(stat = "identity", position = "dodge") +
  facet_wrap(. ~ region, ncol = 5, scale = "free") +
  labs(x = "지역", y = "연도", fill = "")

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
# 기상관측자료 분석 문의사항

serviceName = "QUE0003"

library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(moonBook)
library(webr)
library(tidyverse)
library(ggstatsplot)
library(useful)


#================================================
# Set Env
#================================================
globalVar = new.env()
globalVar$inpConfig = "."
globalVar$figConfig = "."
globalVar$outConfig = "."
globalVar$logConfig = "."
globalVar$mapConfig = "."

#================================================
# Main
#================================================
fileInfo = Sys.glob(paste(globalVar$inpConfig, "8월 소형백엽상과 기존차광통분석.xlsx", sep = "/"))

data = openxlsx::read.xlsx(fileInfo, sheet = 1) %>%
  dplyr::rename(ref = 소형백엽상)

dataL1 = data %>%
  dplyr::mutate(
    dtDate = readr::parse_date(sDate, "%Y-%m-%d")
  ) %>%
  na.omit()


typeList = sort(unique(dataL1$type))
colList = setdiff(colnames(dataL1), c("sDate", "type", "ref", "dtDate"))
dataL3 = data.frame()

typeInfo = "최대값"
colInfo = "P-1"


for (typeInfo in typeList) {
  for (colInfo in colList) {

    dataL2 = dataL1 %>%
      dplyr::filter(type == typeInfo) %>%
      dplyr::select(colInfo, ref) %>%
      tidyr::gather(key = "key", value = "value")

    if (nrow(dataL2) < 1) { next }

    # P값이 2.2204e-16으로서 귀무가설 기각 (두 특성의 분산 차이가 있다)
    # 따라서 상이한 분산 조건 (var.equal = FALSE)
    fTest = var.test(value ~ key, data = dataL2, conf.level = 0.95)
    fTest

    setLabel = paste0(typeInfo, "_", colInfo, "-", "소형백엽상")
    saveImg = sprintf("%s/%s_%s_%s.png", globalVar$figConfig, serviceName, "F-Test", setLabel)

    plot(fTest) +
      xlim(0, 5) +
      ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

    isVarEqual = TRUE
    setVarResult = sprintf("P값이 %s로서 귀무가설 채택 (동일한 분산: 두 일사계의 분산 차이가 없다)", round(fTest$p.value, 3))

    if (fTest$p.value < 0.05) {
      isVarEqual = FALSE
      setVarResult = sprintf("P값이 %s로서 귀무가설 기각 (상이한 분산 : 두 일사계의 분산 차이가 없다)", round(fTest$p.value, 3))
    }


    # P값이 0.054로서 귀무가설 기각 (두 특성은 차이가 있다)
    tTest = t.test(value ~ key, data = dataL2, var.equal = isVarEqual)

    saveImg = sprintf("%s/%s_%s_%s.png", globalVar$figConfig, serviceName, "T-Test", setLabel)
    plot(tTest) +
      xlim(-5, 5) +
      ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)

    setResult = sprintf("P값이 %s로서 귀무가설 채택 (두 일사계는 차이가 없다)", round(tTest$p.value, 3))
    if (tTest$p.value < 0.05) { sprintf("P값이 %s로서 귀무가설 기각 (두 일사계는 차이가 있다)", round(tTest$p.value, 3)) }

    tmpData = data.frame(
      "setLabel" = setLabel
      , "fVal" = fTest$statistic
      , "fPval" = fTest$p.value
      , "fResult" = setVarResult
      , "tVal" = tTest$statistic
      , "tPval" = tTest$p.value
      , "tResult" = setResult
    )

    dataL3 = dplyr::bind_rows(dataL3, tmpData)
  }
}

# XLSX 파일 생성
saveXlsxFile = sprintf("%s/%s_%s.xlsx", globalVar$outConfig, serviceName, "통계 결과")

wb = openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "table")
openxlsx::writeData(wb, "table", dataL3, startRow = 1, startCol = 1, colNames = TRUE, rowNames = FALSE)

openxlsx::saveWorkbook(wb, file = saveXlsxFile, overwrite = TRUE)


#******************************************************************
# 라디오존데 정보를 활용한 상대습도 등고선 가시화 자료 도움요청
# 고도 0 ~ 7km까지의 온도와 상대습도의 등고선 가시화 이미지
#******************************************************************

serviceName = "QUE0002"

library(tidyverse)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(fs)
library(moonBook)
library(webr)
library(tidyverse)
library(ggstatsplot)
library(useful)
library(data.table)
library(tidyverse)
library(lubridate)
library(RadioSonde)
library(MBA)
library(ggrepel)
library(timeDate)
library(metR)
library(scales)
library(humidity)

fileInfo = Sys.glob(paste(globalVar$inpConfig, "OBS_SONDE_F00508_20210111083449.csv", sep = "/"))

# 지점,지점명,일시(UTC),기압(hPa),고도(gpm),기온(°C),이슬점온도(°C),풍향(deg),풍속(knot)
data = readr::read_csv(file = fileInfo, locale = locale("ko", encoding = "EUC-KR")) %>%
  magrittr::set_colnames(c("stationNum", "stationName", "dtDateTime", "level", "height", "temp", "dewTemp", "windSpeed", "windDir")
  )

# L1 Processing Using Data Frame
dataL1 = data %>%
  dplyr::mutate(
    dtXran = lubridate::decimal_date(dtDateTime)
    , rh = humidity::RH(temp, dewTemp, isK = FALSE)
  ) %>%
  na.omit()

#=====================================================================
# Temperature Interpolation Using Multilevel B-Spline Approximation
#=====================================================================
dataL2 = dataL1 %>%
  dplyr::select(dtXran, height, temp) %>%
  MBA::mba.surf(no.X = 1000, no.Y = 500, extend = TRUE, sp = TRUE)

dataL3 = dataL2 %>%
  as.data.frame() %>%
  dplyr::mutate(
    xAxis = lubridate::date_decimal(xyz.est.x)
  ) %>%
  dplyr::rename(
    yAxis = xyz.est.y
    , zAxis = xyz.est.z
  )

summary(dataL3)
dplyr::glimpse(dataL3)

# Set Value for Visualization
cbMatlab = colorRamps::matlab.like(11)
xAxisMin = min(dataL3$xAxis, na.rm = TRUE)
xAxisMax = max(dataL3$xAxis, na.rm = TRUE)

saveImg = sprintf("%s/%s_%s", globalVar$figConfig, serviceName, "Temperature_Visualization_Using_ggplot2.png")


Sys.setlocale("LC_TIME", "english")

# Visualization Using ggplot2
ggplot(data = dataL3, aes(x = xAxis, y = yAxis / 1000, fill = zAxis, z = zAxis)) +
  theme_bw() +
  geom_tile() +
  metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, rotate = TRUE, na.rm = TRUE) +
  geom_contour(color = "black", alpha = 0.3) +
  scale_fill_gradientn(colours = cbMatlab, limits = c(-50, 25), breaks = seq(-50, 25, 25), na.value = NA) +
  scale_x_datetime(breaks = seq(xAxisMin, xAxisMax, "month"), labels = date_format("%b-%d\n%Y", tz = "Asia/Seoul"), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 7, 1), limits = c(0, 7), expand = c(0, 0)) +
  labs(
    subtitle = "Temperature Visualization Using ggplot2"
    , x = "Date [Month-Day Year]"
    , y = "Altitude [km]"
    , fill = "Temperature"
    , colour = NULL
    , title = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "black")
    , plot.subtitle = element_text(face = "bold", size = 18, color = "black")
    , axis.title.x = element_text(face = "bold", size = 18, colour = "black")
    , axis.title.y = element_text(face = "bold", size = 18, colour = "black", angle = 90)
    , axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 18, colour = "black")
    , axis.text.y = element_text(face = "bold", size = 18, colour = "black")
    , legend.title = element_text(face = "bold", size = 14, colour = "black")
    , legend.position = c(0, 1)
    , legend.justification = c(0, 0.96)
    , legend.key = element_blank()
    , legend.text = element_text(size = 14, face = "bold", colour = "black")
    , legend.background = element_blank()
    , text = element_text(family = fontEng)
  ) +
  ggsave(filename = saveImg, width = 10, height = 6, dpi = 600)


#=====================================================================
#   Relative Humidity Using Multilevel B-Spline Approximation
#=====================================================================
dataL2 = dataL1 %>%
  dplyr::select(dtXran, height, rh) %>%
  MBA::mba.surf(no.X = 1000, no.Y = 500, extend = TRUE, sp = TRUE)

dataL3 = dataL2 %>%
  as.data.frame() %>%
  dplyr::mutate(
    xAxis = lubridate::date_decimal(xyz.est.x)
  ) %>%
  dplyr::rename(
    yAxis = xyz.est.y
    , zAxis = xyz.est.z
  ) %>%
  dplyr::filter(
    zAxis > 0
  )

summary(dataL3)

dplyr::glimpse(dataL3)

# Set Value for Visualization
cbMatlab = colorRamps::matlab.like(11)
xAxisMin = min(dataL3$xAxis, na.rm = TRUE)
xAxisMax = max(dataL3$xAxis, na.rm = TRUE)

saveImg = sprintf("%s/%s_%s", globalVar$figConfig, serviceName, "Relative_Humidity_Visualization_Using_ggplot2.png")

Sys.setlocale("LC_TIME", "english")

# Visualization Using ggplot2
ggplot(data = dataL3, aes(x = xAxis, y = yAxis / 1000, fill = zAxis, z = zAxis)) +
  theme_bw() +
  geom_tile() +
  # metR::geom_text_contour(stroke = 0.2, check_overlap = TRUE, rotate = TRUE, na.rm = TRUE) +
  geom_contour(color = "black", alpha = 0.3) +
  scale_fill_gradientn(colours = cbMatlab, limits = c(0, 100), breaks = seq(0, 100, 20), na.value = NA) +
  scale_x_datetime(breaks = seq(xAxisMin, xAxisMax, "month"), labels = date_format("%b-%d\n%Y", tz = "Asia/Seoul"), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 7, 1), limits = c(0, 7), expand = c(0, 0)) +
  labs(
    subtitle = "Relative Humidity Visualization Using ggplot2"
    , x = "Date [Month-Day Year]"
    , y = "Altitude [km]"
    , fill = "Relative Humidity [%]"
    , colour = NULL
    , title = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "black")
    , plot.subtitle = element_text(face = "bold", size = 18, color = "black")
    , axis.title.x = element_text(face = "bold", size = 18, colour = "black")
    , axis.title.y = element_text(face = "bold", size = 18, colour = "black", angle = 90)
    , axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 18, colour = "black")
    , axis.text.y = element_text(face = "bold", size = 18, colour = "black")
    , legend.title = element_text(face = "bold", size = 14, colour = "black")
    , legend.position = c(0, 1)
    , legend.justification = c(0, 0.96)
    , legend.key = element_blank()
    , legend.text = element_text(size = 14, face = "bold", colour = "black")
    , legend.background = element_blank()
    , text = element_text(family = fontEng)
  ) +
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

rm(list = ls())

prjName = "knowledgeIn"

source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R", "InitConfig.R"), encoding = "UTF-8")


require(raster)
require(sp)
require(MODIS)

# FLASH_SSF_Terra-FM1-MODIS_Version4A_401400.2019040100.hdf

library(gdalUtils)

fileInfo = Sys.glob(paste(globalVar$inpConfig, "FLASH_SSF_Terra-FM1-MODIS_Version4A_401400.2019040100.hdf", sep = "/"))

sds <- get_subdatasets(fileInfo)
name <- sds[1]

filename = "dd.tif"
gdal_translate(sds[1], dst_dataset = filename)

gdalinfo(fileInfo)

readGDAL

get_subdatasets()

library(rgdal)
install.packages("rgdal")
rgdal::GDALinfo(fileInfo)
rgdal::readGDAL()

sds = get_subdatasets(fileInfo)
Optical_Depth_047 = read_hdf(f, grep("grid1km:Optical_Depth_047", sds))


sds <- get_subdatasets(fileInfo)

library(rhdf5)


# BiocManager::install("rhdf5")

library(sf)
library(rhdf5)
library(raster)
library(rasterVis)


# http://hdfeos.org/software/h4cflib.php

fileInfo

getwd()

h4tonccf = "C:/Program Files/HDF_Group/H4H5/2.2.5/bin/h4toh5convert.exe"
inFile = "E:/04. TalentPlatform/Github/TalentPlatform-R/RESOURCES/INPUT/knowledgeIn/FLASH_SSF_Terra-FM1-MODIS_Version4A_401400.2019040100.hdf"
outFile = "E:/04. TalentPlatform/Github/TalentPlatform-R/RESOURCES/INPUT/knowledgeIn/FLASH_SSF_Terra-FM1-MODIS_Version4A_401400.2019040100.nc"
fileInfo

# cdo -f nc copy ERA_interim-sfc-201606.grib grib.nc

system(paste(h4tonccf, inFile, outFile))


#
# ./h4toh5convert.exe "E:/04. TalentPlatform/Github/TalentPlatform-R/RESOURCES/INPUT/knowledgeIn/FLASH_SSF_Terra-FM1-MODIS_Version4A_401400.2019040100.hdf"


fileInfo = Sys.glob(paste(globalVar$inpConfig, "FLASH_SSF_Terra-FM1-MODIS_Version4A_401400.2019040100.h5", sep = "/"))


library(ncdf4)
library(fields)
library(maps)
library(maptools)
library(ncdf4)
library(raster)
library(ggmap)
library(akima)
library(reshape)
library(RNetCDF)


fileInfo = Sys.glob(paste(globalVar$inpConfig, "FLASH_SSF_Terra-FM1-MODIS_Version4A_401400.2019040100.hdf", sep = "/"))


ncdf4::nc_open(fileInfo)
RNetCDF::open.nc(fileInfo)


# library(h5)
library(rhdf5)
library(raster)

fileInfo = Sys.glob(paste(globalVar$inpConfig, "FLASH_SSF_Terra-FM1-MODIS_Version4A_401400.2019040100.h5", sep = "/"))

fileHeader = rhdf5::h5ls(fileInfo)

# fileHeader$name
lat = h5read(fileInfo, "/Time and Position/Colatitude of CERES FOV at surface")
lon = h5read(fileInfo, "/Time and Position/Longitude of CERES FOV at surface")
val = h5read(fileInfo, "/Full Footprint Area/Surface skin temperature")
time = h5read(fileInfo, "/Time and Position/Time of observation")

summary(time)
summary(lat)

data = data.frame(lat, lon, val) %>%
  dplyr::mutate(
    refLat = round(lat, 0)
    , refLon = round(lon, 0)
  )

dataL1 = data %>%
  dplyr::group_by(refLat, refLon) %>%
  dplyr::summarise(
    meanVal = mean(val, na.rm = TRUE)
  )


# h1 <- h5read(fileInfo, "/Full Footprint Area/Surface skin temperature")

# h1 <- h5read(fileInfo, "/Full Footprint Area/Surface skin temperature")
# class(h1); str(h1)

library(oce)

data(coastlineWorld)
## make a colormap that looks like the website
col <- colorRampPalette(c("purple", "#00007F", "blue",
                          "#007FFF", "cyan", "#7FFF7F",
                          "yellow", "#FF7F00", "red", "#7F0000"))
summary(var)

ggplot(data = dataL1, aes(x = refLon, y = refLat, colour = meanVal)) +
  # borders("world") +
  geom_point()
# scale_colour_gradient2(name = "topog", low="blue", mid="green", high="red", na.value="white", midpoint=60) +
geom_raster(interpolate = TRUE, na.rm = TRUE)
# geom_point(data = data, aes(x = lon, y = lat, colour=var), na.rm=TRUE)
# geom_ra(data = data, aes(x = lon, y = lat, colour=var), na.rm=TRUE)


oce::imagep(data$lon, data$lat, data$var, zlim = c(190, 300), zlab = 'log10(Chl)')


polygon(coastlineWorld[['lon']], coastlineWorld[['lat']], col = 'grey')


plt <- levelplot(UVI, margin = F, par.settings = mapTheme, main = "UV Index")


class(h1); str(h1)

matrix <- matrix(1:16, nrow = 4)
library(raster)
r <- raster(as.matrix(h1)
)
image(r)


UVI <- raster(h1)
UVI

image(UVI)

h5readAttributes(hdf_file, name = "UVI_field")


#================================================
# 요구사항
#================================================
# 오류가 생겼는데 어떻게 해결하는지 모르겠어요ㅜㅜ
# 그리고 엑셀파일 읽을수 있는 패키지소개해주세요 tead xlsx빼고요!

library(openxlsx)

openxlsx::read.xlsx

#================================================
# 요구사항
#================================================
# > x = c(1,3,5,7,9)
# > y = c(2,3,5,7,11,13)
# 에서 y[x] 값을 구한다고 하면

# 답이 2 5 11 NA NA 이렇게 나오는데
# 이 뜻이 뭔가요?

# []의 의미가 예를 들어 y[1]이면 y객체의 첫번째 값인 2를 뽑는다는 것으로 알고 있는데 y에서 x번째 값을 추출한다는게 왜 저렇게 나오는지 이해가 잘 가지 않습니다.

x = c(1, 3, 5, 7, 9)
y = c(2, 3, 5, 7, 11, 13)

y[x]

# x값의 경우 현재 인덱스값을 의미합니다.
# 따라서 y값의 경우 인덱스가 1-6까지 존재하기 때문에
# x에 대응되지 않는 7, 9는 NA값으로 표시됩니다. 

#================================================
# 요구사항
#================================================
# 밑에 숫자랑 왼쪽에 frequency 랑 숫자가 나와야하는데 짤려서 나와요!
# 위에 histogram of x 도 반짤려서 나오는데 어떻게 하면 되나요? 따로 설정을 해야한다면 알려주세요

x = 1:100

hist(x)

# 해당 Zoom 버튼을 보기
# 그림 영역을 늘리기

#================================================
# 요구사항
#================================================
# R 커맨더 mdi와 sdi 차이

# MDI의 경우 R Console 창이 1개 뜨고 이후 그래프를 그리면 그 내부에 그래프 창 열림 (엑셀 소프트웨어)
# 반면에 SDI는 그래프를 그리면 R graphics 창이 별도로 열림

#================================================
# 요구사항
#================================================
A = matrix(c(1, 2, 3, 4), ncol = 2)
b = c(1, 5)

# solve의 입력 파라미터 (b)가 숫자형이기 때문에 숫자로 반환
solve(A, b)

# solve의 입력 파라미터 (b)가 없기 때문에 단위 행렬로 간주되어 행렬로 반환
solve(A) %*% b

# solve 함수의 매뉴얼 참조
# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/solve

#================================================
# 요구사항
#================================================
# https://mjgim.me/2017/05/01/CVM.html 에서 있는 이중양분선택볍 R코드 예제인데요..

# data  setting
rt1s <- c(1000, 3000, 6000, 15000)
rtus <- c(3000, 6000, 15000, 40000)
rtls <- c(500, 1000, 3000, 6000)
yys <- c(18, 10, 6, 2)
yns <- c(25, 19, 14, 18)
nys <- c(3, 13, 8, 5)
nns <- c(23, 34, 49, 49)

#  program  area  
max_bid <- max(max(rt1s), max(rtus), max(rtls))
t1s <- log(rt1s)
tus <- log(rtus)
tls <- log(rtls)
gc <- function(t, a, b)  1 / (1 + exp(a - b * t))
pyy <- function(t1, tu, tl, a, b)  1 - gc(tu, a, b)
pyn <- function(t1, tu, tl, a, b) { gc(tu, a, b) - gc(t1, a, b); }
pny <- function(t1, tu, tl, a, b)  gc(t1, a, b) - gc(tl, a, b)
pnn <- function(t1, tu, tl, a, b)  gc(tl, a, b)

ll0 <- function(a, b, t1, tu, tl, yy, yn, ny, nn) {
  yy * log(pyy(t1, tu, tl, a, b)) +
    yn * log(pyn(t1, tu, tl, a, b)) +
    ny * log(pny(t1, tu, tl, a, b)) +
    nn * log(pnn(t1, tu, tl, a, b)) }

ll_creator <- function(t1s, tus, tls, yys, yns, nys, nns) {
  function(par) { sum(ll0(par[1], par[2], t1s, tus, tls, yys, yns, nys, nns))
  } }

ll <- ll_creator(t1s, tus, tls, yys, yns, nys, nns)
res = optim(par = c(5, 2), fn = ll, control = list(fnscale = -1), hessian = TRUE)
var_cov <- -solve(res$hessian)
a <- res$par[1]
b <- res$par[2]
step <- 100
delta <- max_bid / step
bids <- seq(delta, max_bid, by = delta)
bids <- append(bids, 0.001, after = 0)
estimates <- 1 - gc(log(bids), a, b)
cs <- (estimates[1:step] + estimates[2:(step + 1)]) * delta / 2

#  results  
mean_of_wtp <- sum(cs)
median_of_wtp <- exp(a / b)
# 계수들은 다음과 같이 얻을 수 있다.

mean_of_wtp
median_of_wtp

#================================================
# 요구사항
#================================================
# 이렇게 코드가 있을때 다른건 다 이해가 됐는데 맨 마지막줄을 실행시켜보니
# 객체 waiting을 찾을 수 없다는데 왜그런건지 정확히 모르겠어요ㅠㅠ

library(MASS)

attach(geyser)
detach(geyser)

hist(geyser)

#================================================
# 요구사항
#================================================
# > library(help="base")
# 경고메시지(들):
# In file.show(outFile, delete.file = TRUE, title = gettextf("Documentation for package %s",  :
# 기본 패키치 목록 출력을 할려고 하는데 자꾸 이 문장이 뜨면서 안되네요.. 뭐가 문제일까요? r언어를 삭제하고 다시 깔아도 같은 이유로 출력이 안되네요...

packageList = installed.packages(.Library, priority = "high")

head(packageList)

#================================================
# 요구사항
#================================================
# R프로그램 히스토그램을
# h=깊이 <- c(깊이,~~~~~)
# hist(~~~~) 이런 식으로 채워 넣어서( 다 적기엔 번거로워 저렇게 적은 겁니다)
# 히스토그램을 만들었어요
# 근데 만들 때 주어진 표본 값이 있어서 그걸 입력하고
# 중앙값을 넣으려고 하는데
# text(h$mids,h$counts,h$counts, adj=c(0.5,-0.5))
# 로 넣으면 자꾸 에러가 떠요
# 주어진 표본 값이 있을 땐 중앙값을 다른 방법으로 넣어야 하나요?
#   h라는 이름의 데이터를 저장하고 텍스트 값을 넣어야 하나요?
#   설명을 듣고 들어도 이해가 안되네요ㅠ
# 그리고 히스토그램 x축에서 0부터 그래프가 시작되는 거 말고 띄우고 나서 그래프가 그려지는 건 어떻게 해결해야 하나요? 끝 부분도 그렇게 되게하고싶어요


#================================================
# 요구사항
#================================================
# R을 이용해서 다음의 내용을 코드로 작성하고 답을 구하시오.
# Y=2x^2+5x+10에 대해 x가 각각 6,8,10일때 y의 값을 각각 구한다.

x = c(6, 8, 10)
y = (2 * (x^2)) + (5 * x) + 10

y

#================================================
# 요구사항
#================================================
# 알 스튜디오 과제 제출해야 하는 대학생 입니다ㅜㅜ
# a<-2 라고 넣고 a가 뭐냐고 런 누르면 2라고 콘솔창에 떠야 되잖아요?! 근데 계속 에러 뜨면서 객체 a를 찾을 수 없다고 뜨네요ㅜㅜ r 잘 아시는 분 제발 도움ㅜㅜㅜㅜ

a = 2

print(a)

#================================================
# 요구사항
#================================================
# 이때, 패키지가 (readxl)이 아닌 다른 패키지로 엑셀 파일을 불러올 수 있나요?
# 혹시 있다면 최대한 많이 알려주시면 감사하겠습니다! 내공 많이 걸겠습니다!!

library(openxlsx)
library(xlsx)

openxlsx::read.xlsx
xlsx::read.xlsx
xlsx::read.xlsx2

#================================================
# 요구사항
#================================================
# 아래와 같은 표를 모자이크 플릇으로 어떻게 만드나요? 
# 품목	판매
# 셔츠	100개 
# 바지	300개
# 치마	250개
# 코트	655개
# 벨트	120개

data = data.frame(
  key = c("셔츠", "바지", "치마", "코트", "벨트")
  , val = c(100, 300, 250, 655, 120)
)

table(data)

mosaicplot(~key + val, data = data, color = TRUE)


#================================================
# 요구사항
#================================================
# Rstudio 에서 위 사진처럼 밑에 출력이 되어야하는데 아래사진처럼 계속 에러만 뜹니다 ㅜㅜㅜ 똑같이 분명히 srt1 <-“a” 적었는데 말이죠ㅠㅠ 어떻게 해야 위 사진처럼 에러가 안뜨고 출력되는지 궁금합니다 ㅜㅜ

str1 <- "a"
str1

#================================================
# 요구사항
#================================================
# 제가 태블릿에 R스튜디오를 다운받아서 공부하고싶은데 무조건 window로 해야하나요?

# 태블릿 PC에 영구적 손상을 입힐 것이라고 해서 추천하지 않습니다.
# 그럼에도 불구하고 설치할 경우 다음 링크를 참조해주시기 바랍니다.

#================================================
# 요구사항
#================================================
# 그런데 저기 빨간 색으로 동그라미 친 처음 부분과 끝 부분에도 공백을 두고 싶습니다.
# 또 y축 값에도 중간에 값을 더 추가하고 싶은데 어떻게 해야 하나요?

PM2.5 <- c(21.9, 23.1, 34.4, 35.2, 39.5, 28.7, 34.8, 15.6, 45.1, 27.7, 24.9, 32.6)

hist(PM2.5, main = '2017년 1분기~2019년 4분기 PM2.5 데이터 분포', xlab = 'PM2.5', ylab = '빈도수')

xLabel = c(17.5, 22.5, 27.5, 32.5, 37.5)
yLabel = c(0.25, 1.25, 2.25, 3.25, 4.25)

for (i in 1:length(xLabel)) {
  lines(c(xLabel[i], xLabel[i]), c(0, 0.2), lty = 1, col = "red")
  text(xLabel[i], 0.4, xLabel[i], col = "red")

  lines(c(15, 15.5), c(yLabel[i], yLabel[i]), lty = 1, col = "blue")
  text(16.5, yLabel[i], yLabel[i], col = "blue")
}

# labelList = c(17.5, 22.5, 27.5, 32.5, 37.5)
# 
# for (labelInfo in labelList) {
#   lines(c(labelInfo, labelInfo), c(0, 0.2), lty = 1, col = "red")
#   text(labelInfo, 0.4, labelInfo, col = "red")
# }

#================================================
# 요구사항
#================================================
# R studio를 설치를 했고 install,packages.("ggpot2")를 설치할려고 이렇게 쳤더니 오류가 뜨며 안돼요
# 어떻게 해야된나요? 급해요 ㅠㅠ

# ggpot2을 ggplot2으로 변경하시면 됩니다.

install.packages("ggplot2")

#================================================
# 요구사항
#================================================
# 분명히 패키지 rcmdr 설치했거든요... 
# 그래서 패키지 불러오면 자꾸 저렇게 뜨는데 어떻게 해야 할까요???ㅠㅠㅠㅠ급합니다ㅠㅠ

# 해당 패키지를 추가로 설치하시면 됩니다.
install.packages("rio")

library(rio)

#================================================
# 요구사항
#================================================
# Rstudio에서 R 프로그램 개발 시 프로젝트를 만들어 작업하는 게 유용한 이유

# 자유로이 사용
# 무료로서 오픈 소스 R과 통합된 개발 환경을 제공
# RStudio는 편집기가 내장되어 있고 (서버 포함) 모든 플랫폼에서 동작 가능
# 특히 버젼 제어 및 프로젝트 관리 같은 많은 앞선 기능을 제공

#================================================
# 요구사항
#================================================
# R 프로그램에서 <- , = 의 차이

# R 초기 버전의 경우 서로 다른 역할이나 지금은 기능이 동일한다.
# 특히 함수 내에서 입력 파라미터는 =만 사용 가능합니다 (data.frame 참조).

#================================================
# 요구사항
#================================================
# x <- c("ㄱ","ㄴ","ㄷ") 일 때
# ㄷ을 ㄹ로 바꾸는 방법이 뭔가요??

# library(stringi)
# 
# x <- c("ㄱ", "ㄴ", "ㄹ")
# 
# b = stringr::str_replace_all(x, "ㄱ", "ㄷ")
# cat(stri_unescape_unicode(gsub(b[1], string)))
# 
# gsub(b[1], string)
# 
# stri_unescape_unicode(b[1])
# 
# 
# stringi::stri_enc_detect(b[1])
# stringi::stri_enc_detect(b[1])
# 
# Iconv(b[1], from = "UTF-8")
# 
# enc2utf8(b[1])
# enc2native(b[1])
# Encoding(b[1])
# 
# iconv(b[1], "UTF-8", "EUC-KR")
# guess_encoding(b[1], n_max = 100)


latin1 <- iconv(b[1], to = "latin1")
paste(latin1, "(latin1):", pryr::bits(latin1))


library(readr)
guess_encoding("data/iris_euc_kr.csv", n_max = 100)

#================================================
# 요구사항
#================================================
# 벡터 내의 특정 셀의 값을 변경하기
# 먼저, 길이가 50인 벡터 X를 만든 후 각 셀의 값은 1부터 2씩 증가하도록 조정해 보시오.
# 예) 1, 3, 5, 7, 9

X = 1:50

for (i in X) {
  if (i %% 2 == 1) print(i)
}

# 위에서 만든 벡터 X에서 셀의 값이 10보다 작은 경우 셀의 값을 0으로 교체한 후, 그 값을 출력하시오.

ind = which(X < 10)
X[ind] = 0

X

#================================================
# 요구사항
#================================================
# 순서대로 코드를 만들어야되요! 아래 내용 R 코드좀 부탁드립니다 ...
# 1. 1/sqrt(n*(n-1)) 식의 값을 ve라는 이름의 벡터로 만들고, n은 2부터 50까지로 계산함

n = 2:50

ve = 1 / sqrt(n * (n - 1))

# 2. 각각의 ve 값을 data 자료 h행의 값들과 곱하고, 이 값들은 pr 이라고 이름지음
c(t(outer(data$h, ve)))활용

pr = c(t(outer(n, ve))) %>%
data.frame()

# 3. pr 의 데이터 셋을 만듬
pr

#================================================
# 요구사항
#================================================
# CDNowsms 1992년도에 설립된 닷컴의 대표적인 온라인 유통회사로서 , 
# 2002년도에 아마존에 매각되었다.
# 1997년 1월부터 1998년 6월 까지의 6919건의 거래에서 각 거래별 CD 판매량에 대해 
# 히스토그램을 그려 분포를 살펴봅니다.
# CDNow 거래 데이터의 차트 출력과 분포를 파악
# https://raw.githubusercontent.com/cran/BTYD/master/data/cdnowElog.csv

library(readr)
data = readr::read_csv(file = "https://raw.githubusercontent.com/cran/BTYD/master/data/cdnowElog.csv", locale = locale("ko", encoding = "UTF-8"))

# 일별 CD 판매량
tableDf = table(data$date, data$cds)
barplot(tableDf)

#================================================
# 요구사항
#================================================
# 숫자만 읽고 싶다면 어떻게 해야 되나요...?
# 원래 파일은 마지막 사진인데 r에서 csv 파일을 읽으면 자료가 2번째 사진처럼 보입니다...
# 남자 여자가 아닌 가장 큰 수인 max()를 읽고 싶어요ㅜㅜ
max(data21[, -1], na.rm = TRUE)

#================================================
# 요구사항
#================================================
# R프로그램에서 html문서로 만들려면 어떻게해야하나요?
# 순서를 가르쳐주세요.

#================================================
# 요구사항
#================================================
# 산점도 그리고 난 후 각 점을 식별하는 소스코드 알려주세요 .
data = data.frame(
x = 1:10
  , y = 1:10
)

data


#' List the latest files available in the Coperniucs 5p catalogue
#'
#' This function search inside the current catalogue of archived products for the Platform Sentinel 5P (https://scihub.copernicus.eu/catalogueview/S5P/)
#' and returns a list of the archived .csv files of a specific month and year.
#' @param month Desired month in mm format (April would be "04", December would be "12")
#' @param year Desired year in yyyy format ("2020" not "20")
#' @keywords list
#' @export
#' @examples
#' listlast("04","2020")
listlast<-function(month, year){

fullpath<-paste0("https://scihub.copernicus.eu/catalogueview/S5P/", year, "/", month, "/")
lista<-xml2::read_html(fullpath)
nodes<-trimws(rvest::html_text(rvest::html_nodes(lista, "a")))
return (nodes[6:length(nodes)])}

#' Get a list of 5p products
#'
#' The functions works after listlast(month, year) function. After retrieving the list of available .csv files with listlast(),
#' the get5plist() functions allows to retrieve a list of the available products described inside a specific .csv file.
#' @param month Desired month in mm format (April would be "04", December would be "12")
#' @param year Desired year in yyyy format ("2020" not "20")
#' @param number Desired .csv file index (from 1 to the maximum number of files returned by listlast() function)
#' @keywords list
#' @export
#' @examples
#' get5plist("04","2020","3")
get5plist<-function(month, year, number){
nodes<-listlast(month, year)
fullpath<-paste0("https://scihub.copernicus.eu/catalogueview/S5P/", year, "/", month, "/")
leggo<-(nodes[as.numeric(number)+5])
leggopath<-paste0(fullpath, leggo)
leggoresult<-read.csv(leggopath)
return(leggoresult)
}

#' Get a specific S5p product
#'
#' The functions works after get5plist() function, it retrives a specific S5P product and allows to save it
#' @param month Desired month in mm format (April would be "04", December would be "12")
#' @param year Desired year in yyyy format ("2020" not "20")
#' @param number Desired .csv file index (from 1 to the maximum number of files returned by listlast() function)
#' @param id Desired S5P file index (from 1 to the maximum number of files returned by get5plist() function)
#' @param fn Desired name of the file to be used when saving the product to the local working directory
#' @keywords list
#' @export
#' @examples
#' get5p("04","2020","3","1","lastday.ncf")
get5p<-function(month, year, number, id, fn)
{
leggo<-get5plist(month, year, number)
p2<-paste0("https://s5pguest:s5pguest@s5phub.copernicus.eu/dhus/odata/v1/Products('", leggo$Id[id], "')/$value")
download.file(p2, as.character(leggo$Name[id]))
}

get5p("04", "2020", "3", "1", "lastday.ncf")


#' Get the latest S5p products (lat long search)
#'
#' The function works by searching the latest 10 products available for a specific location (lat long).
#' It allows to download a specific product after identifying it.
#' @param lat Latitude (degrees) to search at
#' @param lon Longitude (degrees) to search at
#' @param id=NULL If the id parameter is omitted, the function returns a list of up to 10 S5p products available at the given coordinates,
#'  if an id is specified the functions downloads the product with the indicated id. If id=-1 then all the found products will be downloaded automatically.
#' @keywords list, download
#' @export
#' @examples
#' get5p_latlon("44","12")
#' get5p_latlon("44","12",1)

lat = 44
lon = 12

get5p_latlon<-function(lat, lon, id=NULL)
{
leggo<-paste0("https://s5phub.copernicus.eu/dhus/search?q=footprint:\"Intersects(", lat, ",", lon, ")\"")
lista<-xml2::read_html(httr::GET(leggo, httr::authenticate("s5pguest", "s5pguest")))
sommario<-trimws(rvest::html_text(rvest::html_nodes(lista, "summary")))
nodes<-trimws(rvest::html_text(rvest::html_nodes(lista, "id")))
nodes2<-trimws(rvest::html_text(rvest::html_nodes(lista, "title")))
nodi<-data.frame(desc=sommario, name=nodes2[2:length(nodes2)], id=nodes[2:length(nodes)])
if (!is.null(id)){
if(id!="-1"){
leggo<-nodi$id[id]
p2<-paste0("https://s5pguest:s5pguest@s5phub.copernicus.eu/dhus/odata/v1/Products('", leggo, "')/$value")
nomefile<-paste0(as.character(nodi$name[id]), ".nc")
print(paste0("Downloading ", nomefile))
download.file(p2, as.character(nodi$name[id]))

print(paste0(nomefile, " successfully saved"))
}
if(id=="-1"){
for(i in 1:length(nodi$id)){
leggo<-nodi$id[i]
print(paste0("Downloading file ", i, "/", length(nodi$id)))
p2<-paste0("https://s5pguest:s5pguest@s5phub.copernicus.eu/dhus/odata/v1/Products('", leggo, "')/$value")
nomefile<-paste0(as.character(nodi$name[i]), ".nc")
print(paste0("Downloading ", nomefile))
download.file(p2, nomefile)
print(paste0(nomefile, " successfully saved"))
} }

}
return (nodi)
}
#' Get a S5p product
#'
#' This function allows to inspect a downloaded S5p file or plots it
#' @param fil Name of the file to inspect/plot
#' @param variable If the field is NULL then the function will return a list containing all the informations of the .nc file.
#' Normally the variables names are included as PRODUCT/variable. If the variable name is given as parameter then the function
#' will plot that variable
#' @keywords list, download, plot
#' @export
#' @examples
#' plot5p("S5P_OFFL_L2__AER_LH_20200527T115918_20200527T134048_13579_01_010302_20200529T045906.nc")
#' plot5p("S5P_OFFL_L2__AER_LH_20200527T115918_20200527T134048_13579_01_010302_20200529T045906.nc","aerosol_index_340_380")

plot5p<-function(fil, variable=NULL){
nc<-ncdf4::nc_open(fil)
lat<-ncdf4::ncvar_get(nc, "PRODUCT/latitude")
if(!is.null(variable)){
lon<-ncdf4::ncvar_get(nc, "PRODUCT/longitude")
pro<-ncdf4::ncvar_get(nc, paste0("PRODUCT/", variable))
plottami<-data.frame(lat=as.vector(lat), lon=as.vector(lon), valore=as.vector(pro))
ggplot2::ggplot(plottami, aes(y=lat, x=lon, fill=valore)) +
ggplot2::geom_tile(width=1, height=1) +
ggplot2::borders('world', xlim=range(plottami$lon), ylim=range(plottami$lat),
colour='gray90', size=.2)}
return(nc)
}


#================================================
# 요구사항
#================================================
# 지정 경로는 c:// user / documents로 되어 있고요
# 내문서에 r이라는 폴더를 만들고 불러울 문서 examscore.scv를 저장했습니다.
# r폴더에 들어가 more -> set a working directory를 눌렀는데 작업디렉토리를 변경할 수 없습니다
# 라는 에러가 뜹니다ㅜㅜ 해결방안이 무엇일까요?

setwd("C:/Users/documents")
getwd()
read.csv("./examscore.csv")

#================================================
# 요구사항
#================================================
# 다음문제들을 풀어주세요! 파일로 올려주셔도되고 코드입력방법을 순서대로 알려주셔도 됩니다 부탁드립니다 내공 100 걸겠습니다!

#================================================
# 요구사항
#================================================
# R프로그래밍을 공부하는 학생입니다.
# 강의를 듣다 모르는 문제가 있는데 어떻게 해결을 해야 하는지 질문드립니다.

# cars 데이터프레임 이용하여 다음 R 프로그램을 작성하시오. 

# 1. dist_kind 컬럼을 아래 조건에 맞게 추가하시오.
# - dist값이 20이하이면 "L"
# - dist값이 20초과 80미만이면 "M"
# - dist값이 80이상이면 "H"
library(tidyverse)

carsL1 = cars %>%
dplyr::mutate(dist_kind = case_when(
dist <= 20 ~ "L"
  , dist > 20 & dist < 80 ~ "M"
  , dist >= 80 ~ "H"
  , TRUE ~ "null"
))

# 2. dist_kind 컬럼을 이용하여 막대그래프(Bar Chart)를 작성하시오.
carsL2 = table(carsL1$dist_kind)
barplot(carsL2)

#================================================
# 요구사항
#================================================
# 25.0 +- 0.5 라는 범위의 데이터 값을 받았는데
# 이 값들 중에 저 범위에 포함되지 않는 값들이 있습니다.
# 이 비율을 알아내서 히스토그램에 표시해야하는데 방법을 모르겠습니다.
# 
# 히스토그램에 표시하는법도 알려주시면 감사하겠습니다 ㅠ

sampleData = 25 + rnorm(n = 100, mean = 0, sd = 1)
hist(sampleData)

xLabel = c(24, 24.5, 25, 25.5, 26)

for (i in 1:length(xLabel)) {
lines(c(xLabel[i], xLabel[i]), c(0, 0.2), lty = 1, col = "red")
text(xLabel[i], 0.4, xLabel[i], col = "red")
}

#================================================
# 요구사항
#================================================
# 20개의 TRUE로 구성된 vc.3를 생성하고 vc.3의 내용을 출력하는 코드를 작성하시오.
vc.3 = rep(TRUE, 20)

print(vc.3)

#================================================
# 요구사항
#================================================
# 첫번째가 교수님 화면이고 두번째가 제가 실행한 화면입니다.
# 뭐가 문제인지 실행이되질 않고 오류만 떠요.
# 왜 함수를 못 찾는지 아시는 분 있을까요?
#   도와주세요.

install.packages("readr")
readr::read_csv()

#================================================
# 요구사항
#================================================
# R프로그래밍 별찍기 정삼각형 만들기
# 사진에 표시한 부분처럼 정삼각형을 만드려면 코드를 어떻게 만들어야하나요?

for (i in 1:4) {
cat(paste(rep("*", i), collapse = ""), "\n")
}

#================================================
# 요구사항
#================================================
# 파일을 택스트 형식으로 저장하고 readline을했는데 계속 저렇게 뜨네요 뭐 어떤게 문제인거고 해결방법이 뭔가요 ㅜㅜㅜ

setwd("C:/Users/park/Desktop")

#================================================
# 요구사항
#================================================
# 뭐가 문제인거에요??패키지가 안깔려요ㅠ

# R 및 R Studio 실행/설치 시 관리자 권한으로 실행해주시면 됩니다.

#================================================
# 요구사항
#================================================
library(readxl)

fileInfo = Sys.glob(paste(globalVar$inpPath, "seoul.xlsx", sep = "/"))
data = readxl::read_excel(fileInfo, sheet = "Sheet1")