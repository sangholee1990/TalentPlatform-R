# ================================================
# 초기 환경변수 설정
# ================================================
# env = "local"  # 로컬 : 원도우 환경, 작업환경 (현재 소스 코드 환경 시 .) 설정
env = "dev"  # 개발 : 원도우 환경, 작업환경 (사용자 환경 시 contextPath) 설정
# env = "oper"  # 운영 : 리눅스 환경, 작업환경 (사용자 환경 시 contextPath) 설정

prjName = "gwnu"
serviceName = "PRJ0002"

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
  source(file.path(contextPath, "src", "InitConfig.R"))
}


#================================================
# 비즈니스 로직 수행
#================================================
# library(colorspace)
# try(detach("package:plyr", unload = TRUE), silent = TRUE)
# try(detach("package:dplyr", unload = TRUE), silent = TRUE)
library(plyr)
library(dplyr)
library(psych)
library(pastecs)
library(stringr)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(GGally)
library(glmnet)
library(alr3)
library(ISLR)

options(digits = 15)


# data = read.csv('Irra_clear_all.dat', header = F, sep = "", skip = 0)  # DLR, Irr
# DLR, Irr
fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "Irra_clear_all.dat"))
data = read.csv(fileInfo, header = FALSE, sep = "", skip = 0)



# data = read.csv('Irra_clear_all_1.dat', header=F, sep="", skip=0)  # DLR, Rad
# DLR, Rad
# fileInfo = Sys.glob(file.path(globalVar$inpPath, serviceName, "Irra_clear_all_1.dat"))
# data = read.csv(fileInfo, header = FALSE, sep = "", skip = 0)

colnames(data) = c("idatm", "iaer", "vis", "raa", "vza", "dlr", "ch08", "ch09", "ch10", "ch11", "ch12", "ch13", "ch14", "ch15", "ch16")
head(data)

# data = na.omit(data)

summary(data)

# pairs(data)
# cor(data)

plot(data$ch08, data$dlr)
lm.fit = lm(data$dlr ~ data$ch08)
abline(lm.fit, col = 'red')
summary(lm.fit)


plot(data$vza)
plot(data_L1$idatm, data_L1$dlr)
plot(data_L1$iaer, data_L1$dlr)
plot(data_L1$vis, data_L1$dlr)
plot(data_L1$raa, data_L1$dlr)
plot(data_L1$vza, data_L1$dlr)

cols = rainbow(10)
plot(data_L1$idatm, data_L1$dlr, ylim = c(0, 10), type = 'l')
points(data_L1$idatm, data_L1$ch08, col = cols[9], type = 'l')
points(data_L1$idatm, data_L1$ch09, col = cols[1], type = 'l')
points(data_L1$idatm, data_L1$ch10, col = cols[2], type = 'l')
points(data_L1$idatm, data_L1$ch11, col = cols[3], type = 'l')
points(data_L1$idatm, data_L1$ch12, col = cols[4], type = 'l')
points(data_L1$idatm, data_L1$ch13, col = cols[5], type = 'l')
points(data_L1$idatm, data_L1$ch14, col = cols[6], type = 'l')
points(data_L1$idatm, data_L1$ch15, col = cols[7], type = 'l')
points(data_L1$idatm, data_L1$ch16, col = cols[8], type = 'l')


plot(data_L1$iaer, data_L1$dlr, ylim = c(0, 10), type = 'l')
points(data_L1$iaer, data_L1$ch08, col = cols[9], type = 'l')
points(data_L1$iaer, data_L1$ch09, col = cols[1], type = 'l')
points(data_L1$iaer, data_L1$ch10, col = cols[2], type = 'l')
points(data_L1$iaer, data_L1$ch11, col = cols[3], type = 'l')
points(data_L1$iaer, data_L1$ch12, col = cols[4], type = 'l')
points(data_L1$iaer, data_L1$ch13, col = cols[5], type = 'l')
points(data_L1$iaer, data_L1$ch14, col = cols[6], type = 'l')
points(data_L1$iaer, data_L1$ch15, col = cols[7], type = 'l')
points(data_L1$iaer, data_L1$ch16, col = cols[8], type = 'l')

plot(data_L1$ch15, data_L1$dlr)

data_L1 = data %>%
  # filter( idatm == 2) %>%  # 1, 2, 3, 4, 5, 6
  # filter( iaer == 0) %>%    # 1, 2, 3, 4
  filter(vis == 5) %>%     # 5, 10, 15, 20
  filter(raa == 10) %>%
  filter(vza == 0)

X = data_L1$ch12
Y = data_L1$dlr


X = data_L1$ch14
Y = data_L1$dlr
plot(X, Y)
lm.fit = lm(Y ~ X)
lm.fit2 = lm(Y ~ I(X) + I(X^2))
abline(lm.fit, col = 'red')
Yfit2 = coef(lm.fit2)[1] +
  (coef(lm.fit2)[2] * X) +
  (coef(lm.fit2)[3] * (X^2))
points(X, Yfit2, col = 'blue')

summary(lm.fit2)
summary(lm.fit)


lm.fit = lm(dlr ~ I(ch08) +
  I(ch09) +
  I(ch10) +
  I(ch11) +
  I(ch12) +
  I(ch13) +
  I(ch14) +
  I(ch15), data = data_L1); lm.fit
formatC(coef(lm.fit), format = "E")