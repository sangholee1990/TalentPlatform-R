
# setwd("E:/04. TalentPlatform/Github/TalentPlatform-R/src/subject/LSH0232")

# rm(list=ls())                
library(AER)
data("CPS1985")
##
## Demonstration that one can get a perfect R^2 by random features
##
summary(lm(wage~education+experience, data=CPS1985))  
( n <- nrow(CPS1985) )
set.seed(6)                               
stdNorm <- matrix(rnorm(n*(n-2)), nrow=n)  
funnyReg <- lm(wage~stdNorm, data=CPS1985) 
summary(funnyReg)

##
## Investigate degree of linear dependence
##
corMat <- cor(stdNorm)
det(corMat)
eigval <- eigen(corMat, only.values = T)$values
plot(eigval, type="l", main="Eigenvalue Spectrum")
abline(h=c(0,1), col=c("black","red"), lty=c(1,5))

##
vec.test.mse <- vector(mode='numeric', n-2)
vec.train.mse <- vector(mode='numeric', n-2)
vec.r2 <- vector(mode='numeric', n-2)

set.seed(6)
train.seed <- sample(n, 267, replace = F)

for(i in 1:(n-2)){
  train.lm <- lm(wage[train.seed]~stdNorm[train.seed,1:i], data=CPS1985) 
  vec.train.mse[i] <- mean(residuals(train.lm)^2)
  vec.r2[i] <- summary(train.lm)$r.squared
  
  newdata <- data.frame(stdNorm[-train.seed,1:i])
  pred.values <- predict(train.lm, newdata) 
  vec.test.mse[i] <- mean((CPS1985$wage[-train.seed]-pred.values)^2)
  
}

plot(1:(n-2), vec.test.mse, ylim=c(0, max(vec.test.mse)), col='blue', type='l', xlab="# of features")
lines(1:(n-2), vec.train.mse, col='red', type='l')

plot(1:(n-2), vec.r2,  col='blue', type='l', xlab="# of features")

