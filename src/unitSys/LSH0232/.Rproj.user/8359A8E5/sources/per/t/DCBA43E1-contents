##
## Chapter 6: Regularization with Ridge Regression and the Lasso
##

# rm(list=ls(all=T))
# setwd("D:\\Google Drive\\UOS\\2021\\2학기\\머신러닝\\5\\Data")
# setwd("E:/04. TalentPlatform/Github/TalentPlatform-R/src/subject/LSH0232")

library(spdep); library(rgdal); library(foreign) 
library(car); library(MASS)
library(RColorBrewer); library(classInt)
library(glmnet)

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
x <- as.matrix(sample.df[,c("Year", "Nurser", "Hospit", "Park",   
                  "Culture", "Sub","H_univ", "M_priv","E_prog")])
y <- sample.df$Price

##
## Ridge Regression. Note x variables are standardize by default in glmnet                            
##
grid <- 10^seq(10,-2,length=100)                  # search grid for lambda
help("glmnet")
ridge.mod <- glmnet(x, y, alpha=0, lambda=grid)   # alpha=0 => Ridge regression

dim(coef(ridge.mod))                              # 10 coefficients by 100 lambdas

ridge.mod$lambda[50]                              # lambda value
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))               # l2-norm

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

## coefficients using the predict function allows for lambda-values not in grid
help(predict.glmnet)
predict(ridge.mod, s=50, type="coefficients")[1:10,]

## split df into train and test samples
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]
y.train <- y[train]

## Baseline lm model MSE
dfTrain <- data.frame(y.train, x[train, ])
dfTest <- data.frame(y.test, x[test, ])
lm.01 <- lm(y.train~., data=dfTrain)
summary(lm.01)
pred.lm <- predict(lm.01, newdata=dfTest)
mean((pred.lm-y.test)^2)                     # Baseline MSE

##
## Evaluate MSE on predicted test data for different lambdas
##
ridge.mod<- glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
help("plot.glmnet")
plot(ridge.mod, xvar="lambda", label=TRUE)

## Basic lm model with lambda=0
ridge.pred <- predict(ridge.mod, s=0, exact=T, newx=x[test,], x=x[train,] ,y=y[train])
mean((ridge.pred-y.test)^2)

ridge.pred <- predict(ridge.mod, s=4, type="response", newx=x[test,])    # lambda=4
mean((ridge.pred-y.test)^2)                       # prediction MSE
mean((mean(y[train])-y.test)^2)                   # MSE if only intercept

ridge.pred <- predict(ridge.mod, s=1e10, type="response", newx=x[test,]) # lambda=1e10
mean((ridge.pred-y.test)^2)

##
## Find best lambda by n-fold cross evaluation
##
help("cv.glmnet")
cv.out <- cv.glmnet(x[train,], y[train], alpha=0) # procedure choosen sequence of lambdas
help("plot.cv.glmnet")
plot(cv.out)
(bestlam <- cv.out$lambda.min)
log(bestlam)

ridge.pred <- predict(ridge.mod, s=bestlam, type="response", newx=x[test,])
mean((ridge.pred-y.test)^2)

##
## The Lasso obtained with alpha=1
##
lasso.mod <- glmnet(x[train,], y[train], alpha=1)
plot(lasso.mod, xvar="lambda", label=TRUE)

lasso.coef <- predict(lasso.mod, type="coefficients", s=exp(4))[1:10,]
lasso.coef
lasso.coef[lasso.coef!=0]

cv.out <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
(bestlam <- cv.out$lambda.min)
log(bestlam)

lasso.pred <- predict(lasso.mod, s=bestlam, type="response", newx=x[test,])
mean((lasso.pred-y.test)^2)
lasso.mod <- glmnet(x, y, alpha=1, lambda=grid)
lasso.coef <- predict(lasso.mod, type="coefficients", s=bestlam)[1:10,]
lasso.coef
lasso.coef[lasso.coef!=0]

#
##ESF with regularization
#
##Create spatial weights matrix
sample.nb <- poly2nb(sample.shp, queen = T)
sample.n <- length(sample.nb)
sample.listb <- nb2listw(sample.nb, style='B')
sample.listw <- nb2listw(sample.nb, style='W')

##Generate MBM
B <- listw2mat(sample.listb)
M <- diag(sample.n) - matrix(1/sample.n, sample.n, sample.n)
MBM <- M%*%B%*%M

##Extract Eigenvectors and eigenvalues
eig <- eigen(MBM)
EV <- as.data.frame( eig$vectors[,]); colnames(EV) <- paste('EV', 1:NCOL(EV), sep='')

##Select Candiate EVs
np <- length(eig$values[eig$values/eig$values[1]>0.20])
EV <- EV[,1:np]

##Linear regression based on AIC
attach(sample.df)

lm.res <- lm(Price~M_priv+H_univ+E_prog+Year+Park+Sub+Nurser+Hospit+Culture, data = EV)
summary(lm.res)

#residual map
mapping.seq(sample.shp, lm.res$residuals, nclass=9)
lm.morantest(lm.res, listw=sample.listw)

##Implement MESF
esf.full <- lm(Price~M_priv+H_univ+E_prog+Year+Park+Sub+Nurser+Hospit+Culture+., data = EV)
sample.esf <- stepAIC(lm.res, scope=list(upper= esf.full), direction='forward')
summary(sample.esf)
detach(sample.df)

##MESF with lasso
x.ev <- as.matrix(cbind(x, EV))
lasso.mod <- glmnet(x.ev[train,], y[train], alpha=1)
plot(lasso.mod, xvar="lambda", label=TRUE)

cv.out <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
(bestlam <- cv.out$lambda.min)
log(bestlam)

lasso.coef <- predict(lasso.mod, type="coefficients", s=bestlam)[1:122,]
lasso.coef
length(lasso.coef[lasso.coef!=0])

lasso.pred <- predict(lasso.mod, s=bestlam, type="response", newx=x.ev[test,])
mean((lasso.pred-y.test)^2)

