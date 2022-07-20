rm(list=ls())                          # Clear environment
# setwd("D:\\Google Drive\\UOS\\2021\\2학기\\머신러닝\\2\\Data")
# setwd("E:/04. TalentPlatform/Github/TalentPlatform-R/src/unitSys/LSH0232")

library(rgdal) 
library(fields)

##Read Shapefile
sample.shp <- readOGR(dsn = ".", layer = "Seoul_Grid")
sample.df <- data.frame(sample.shp)

set.seed(9) 
train.seed <- sample(nrow(sample.df), 200, replace = F)
train.df <- sample.df[train.seed,]
test.df <- sample.df[-train.seed,]

##
## Linear Regression
##
lm.sample <- lm(PrArea~BuiltY+M_Priv, data=train.df)
summary(lm.sample)

#train MSE
mean((train.df$PrArea-lm.sample$fitted.values)^2)
mean(residuals(lm.sample)^2)

#test MSE
pred.values <- predict(lm.sample, test.df)
mean((test.df$PrArea-pred.values)^2)


##
## Thin-Plate Spline Smoothing
##
train.Y <- as.vector(train.df$PrArea)
train.X <- as.matrix(cbind(train.df$BuiltY, train.df$M_Priv))
polysmooth <- Tps(train.X, train.Y, df=90)    
surface(polysmooth, main="Thin-Plate Smoother")
mean(residuals(polysmooth)^2)

#Test MSE
test.Y <- as.vector(test.df$PrArea)
test.X <- as.matrix(cbind(test.df$BuiltY, test.df$M_Priv))
pred.values <- predict(polysmooth, x=test.X, Y=test.Y)
mean((test.df$PrArea-pred.values)^2)

##Find optimal
n.sim <- 90
vec.test.mse <- vector(mode='numeric', n.sim)
vec.train.mse <- vector(mode='numeric', n.sim)

for(i in 1:n.sim){
  polysmooth <- Tps(train.X, train.Y, df=i+2)
  vec.train.mse[i] <- mean(residuals(polysmooth)^2)
  
  pred.values <- predict(polysmooth, x=test.X, Y=test.Y)
  vec.test.mse[i] <- mean((test.df$PrArea-pred.values)^2)
}

plot(1:n.sim, vec.test.mse, ylim=c(0, max(vec.test.mse)), col='blue', type='o', pch=19, cex=0.5)
lines(1:n.sim, vec.train.mse, col='red', type='o', pch=19, cex=0.5)

(best.df <- which.min(vec.test.mse)+2)
min(vec.test.mse)
