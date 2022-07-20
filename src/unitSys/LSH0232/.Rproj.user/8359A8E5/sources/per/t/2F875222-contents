# rm(list=ls(all=T))
# setwd("D:\\Google Drive\\UOS\\2021\\2학기\\머신러닝\\5\\Data")
# setwd("E:/04. TalentPlatform/Github/TalentPlatform-R/src/subject/LSH0232")

library(spdep); library(rgdal); library(foreign) 
library(car)
library(RColorBrewer); library(classInt)
library(spatialreg)

##Mapping function
mapping.seq <- function(polys, x, nclass, main="") {  
  pal.red <- brewer.pal(nclass, "Reds")
  q.n <- classIntervals(x, nclass, style="quantile") 
  cols.red <- findColours(q.n, pal.red)
  plot(polys, col=cols.red)
  brks <- round(q.n$brks,2)
  leg <- paste(brks[-(nclass+1)], brks[-1], sep=" - ")
  legend("bottomright", fill=pal.red, legend=leg, bty="n")
  if (!missing(main)) title(main)
}

##Read Shapefile
sample.shp <- readOGR(dsn = ".", layer = "Seoul_dong", encoding = 'ESRI Shapefile')
sample.df <- read.dbf('Seoul_dong.dbf')
sample.df$Div <- as.factor(sample.df$Div)
attach(sample.df)

##bivariate regression
simple.lm <- lm(Price~M_priv, data = sample.df)
summary(simple.lm)

#manually check for parameter estimation
n <- nrow(sample.df)
(RSS <- sum((simple.lm$residuals)^2))
(RSE <- sqrt(RSS/(n-2)))
TSS.X <- sum((M_priv-mean(M_priv))^2)
(SE1 <- sqrt((RSS/(n-2))/TSS.X))
(t.val <- simple.lm$coefficients[2]/SE1)
confint(simple.lm)

#
#Model accuracy
#
#RSE
(RSE <- sqrt(RSS/(n-2)))

#R.squared
TSS.Y <- sum((Price-mean(Price))^2)
(R2 <-1-(RSS/TSS.Y)) 
plot(simple.lm$fitted.values~Price)
abline(a=0, b=1, col='red')
R <- cor(simple.lm$fitted.values, Price)
R^2

#globalF-test
f.t <- ((TSS.Y-RSS))/(RSS/(n-2))

##Adding interaction variable
#prepare Plot
divSymbol <- ifelse(Div==levels(Div)[1],15,16)      # Symbols & colors for well type
divCol <- ifelse(Div==levels(Div)[1],"red","blue")  

# Regression with intercept dummy
dummy.lm1 <- lm(Price~M_priv+Div, data = sample.df)
summary(dummy.lm1)
plot(M_priv, Price, pch=divSymbol, col=divCol)
abline(dummy.lm1$coef[1],dummy.lm1$coef[2],col="red")
abline(dummy.lm1$coef[1]+dummy.lm1$coef[3],dummy.lm1$coef[2],col="blue")
legend("topleft",legend=c("그외","강남"), col=c("red","blue"),pch=c(15,16))

# Regression with slope dummy
dummy.lm2 <- lm(Price~M_priv+M_priv:Div, data = sample.df)
summary(dummy.lm2)
plot(M_priv, Price, pch=divSymbol, col=divCol)
abline(dummy.lm2$coef[1],dummy.lm2$coef[2],col="red")
abline(dummy.lm2$coef[1], dummy.lm2$coef[2]+dummy.lm2$coef[3],col="blue")
legend("topleft",legend=c("그외","강남"), col=c("red","blue"),pch=c(15,16))

# Regression with both
dummy.lm3 <- lm(Price~M_priv+M_priv*Div, data = sample.df)
summary(dummy.lm3)
plot(M_priv, Price, pch=divSymbol, col=divCol)
abline(dummy.lm3$coef[1],dummy.lm3$coef[2],col="red")
abline(dummy.lm3$coef[1]+dummy.lm3$coef[3], dummy.lm3$coef[2]+dummy.lm3$coef[4],col="blue")
legend("topleft",legend=c("그외","강남"), col=c("red","blue"),pch=c(15,16))

#Polynomial function
sample.lm1 <- lm(Price~M_priv+Sub)
summary(sample.lm1)

sample.lm2 <- lm(Price~M_priv+I(Sub^2))
summary(sample.lm2) ##not improved

sample.lm3 <- lm(log(Price)~M_priv+Sub)
summary(sample.lm3) ##not improved

##Multiple linear regression
mult.lm1 <- lm(Price~M_priv+H_univ+E_prog+Year+Park+Sub+Nurser+Hospit+Culture)
summary(mult.lm1)
plot(mult.lm1)
vif(mult.lm1)

##autocorrelation
mapping.seq(sample.shp, mult.lm1$residuals, 6, "SA in residuals")
nb <- poly2nb(sample.shp, queen=T)
sample.listw <- nb2listw(nb, style = 'W')
lm.morantest(mult.lm1, sample.listw)

sample.sar <- spautolm(Price~M_priv+H_univ+E_prog+Year+Park+Sub+Nurser+Hospit+Culture, listw=sample.listw, family='SAR')
summary(sample.sar)
mapping.seq(sample.shp, residuals(sample.sar), 6, "SA in residuals")
moran.test(residuals(sample.sar), sample.listw, zero.policy = T)

