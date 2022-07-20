# rm(list=ls(all=TRUE))

library(spdep); library(maptools)
library(MASS)
library(RColorBrewer); library(classInt)

# setwd("D:\\Google Drive\\UOS\\2021\\2학기\\머신러닝\\3\\Data")
# setwd("E:/04. TalentPlatform/Github/TalentPlatform-R/src/subject/LSH0232")

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

##Load data
sample.shp <- readShapePoly("Seoul_dong.shp")
sample.df <- as.data.frame(sample.shp)
plot(sample.shp, col="grey")

##Create spatial weights matrix
sample.nb <- poly2nb(sample.shp, queen = T)
sample.n <- length(sample.nb)
print(sample.nb)
sample.listw <- nb2listw(sample.nb, style='W')
sample.listb <- nb2listw(sample.nb, style='B')

##Generate MBM
B <- listw2mat(sample.listb)
M <- diag(sample.n) - matrix(1/sample.n, sample.n, sample.n)
MBM <- M%*%B%*%M

##Extract Eigenvectors and eigenvalues
eig <- eigen(MBM)
plot(1:423, eig$values)
round(cor(eig$vectors[,99], eig$vectors[,3]), 6)

##Drawing eigenvector maps
# mapping.seq(sample.shp, x=eig$vectors[,1], nclass=9)
# moran.test(eig$vectors[,1], sample.listb, randomisation=TRUE)
# 
# mapping.seq(sample.shp, x=eig$vectors[,423], nclass=9)
# moran.test(eig$vectors[,423], sample.listb, randomisation=TRUE)

# 제2성분 지도 결과
saveImg = sprintf("%s/%s_%s.png", ".", "LSH0232", "제2주성분의 성분점수")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
mapping.seq(sample.shp, x=eig$vectors[,2], nclass=9)
moran.test(eig$vectors[,2], sample.listb, randomisation=TRUE)
dev.off()

##Relationship between eigenvalues and Moran's I
mc.res <- vector('numeric', sample.n)
for(i in 1:sample.n){
  mc.res[i] <- moran.test(eig$vectors[,i], sample.listb, randomisation=TRUE)$estimate[1]
}
plot(eig$values, mc.res)

##
##Drawing EV maps with W matrix
##
# 연접성 기반 이항 공간가중행렬
W <- listw2mat(sample.listw)
sum(apply(W, 1, sum))
MWM <- M%*%W%*%M
eig <- eigen(MWM)

# 역거리 기반 공간가중행렬
D <- sp::spDists(sample.shp)/1000  
D <- 1/D
diag(D)<-0
MBM <- M%*%D%*%M
eig <- eigen(MBM)

##Drawing eigenvector maps
# 강한 양의 공간적 자기상관
# saveImg = sprintf("%s/%s_%s.png", ".", "LSH0232", "강한 양의 공간적 자기상관")
saveImg = sprintf("%s/%s_%s.png", ".", "LSH0232", "역거리-강한 양의 공간적 자기상관")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
mapping.seq(sample.shp, x=eig$vectors[,1], nclass=9)
moran.test(eig$vectors[,1], sample.listw, randomisation=TRUE)
dev.off()

# 낮은 음의 공간적 자기상관
# saveImg = sprintf("%s/%s_%s.png", ".", "LSH0232", "낮은 음의 공간적 자기상관")
saveImg = sprintf("%s/%s_%s.png", ".", "LSH0232", "역거리-낮은 음의 공간적 자기상관")
png(file = saveImg, width = 10, height = 8, units = "in", res = 600)
mapping.seq(sample.shp, x=eig$vectors[,423], nclass=9)
moran.test(eig$vectors[,423], sample.listw, randomisation=TRUE)
dev.off()

##Relationship between eigenvalues and Moran's I
mc.res <- vector('numeric', sample.n)
for(i in 1:sample.n){
  mc.res[i] <- moran.test(eig$vectors[,i], sample.listw, randomisation=TRUE)$estimate[1]
}
plot(eig$values, mc.res)

##
## Moran eigenvector spatial filtering
##
eig <- eigen(MBM)
EV <- as.data.frame( eig$vectors[,]); colnames(EV) <- paste('EV', 1:NCOL(EV), sep='')

##Select Candiate EVs
np <- length(eig$values[eig$values/eig$values[1]>0.20])
EV <- EV[,1:np]

##Linear regression 
attach(sample.df)

hist(Price)
hist(log(Price))
lm.res <- lm(log(Price)~H_univ+M_priv+Nurser, data = EV)
summary(lm.res)

#residual map
mapping.seq(sample.shp, lm.res$residuals, nclass=9)
lm.morantest(lm.res, listw=sample.listw)

##Implement MESF
esf.full <- lm(log(Price)~H_univ+M_priv+Nurser+., data = EV)
sample.esf <- stepAIC(lm.res, scope=list(upper= esf.full), direction='forward')
summary(sample.esf)

##selected spatial filter #not applicable, draw in SAAR
#mapping.seq(sample.shp, sample.df$sfilter, nclass=9)

#residual map
mapping.seq(sample.shp, sample.esf$residuals, nclass=9)
lm.morantest(sample.esf, listw=sample.listw)

detach(sample.df)
