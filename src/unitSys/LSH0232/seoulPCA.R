# rm(list=ls())
library(car)
library(rgdal) 
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

##Read Shapefile
sample.shp <- readOGR(dsn = ".", layer = "Seoul_dong")
sample.df <- data.frame(sample.shp)
seoul <- sample.df[,c("Price", "Year", "Nurser", "Hospit", "Park",   
                      "Culture", "Sub","H_univ", "M_priv","E_prog")]

plot(sample.shp, col="grey")
mapping.seq(sample.shp, x=seoul$H_univ, nclass=9)

scatterplotMatrix(seoul)
corSeoul <- cor(seoul)            # correlation matrix
print(corSeoul, digits=2)

##
## Principal component analysis on correlation matrix including component scores
##
?princomp # study online help
pcSeoul <- princomp(seoul, cor=TRUE, scores=TRUE)
summary(pcSeoul, loadings=TRUE, cutoff=0.3)

apply(pcSeoul$loadings^2,1,sum)         # row-sum of squared loadings
apply(pcSeoul$loadings^2,2,sum)         # column-sum of squared loadings           

round(pcSeoul$sdev^2, 2)                # eigenvalues = variance explained by component
round(pcSeoul$sdev^2, 2)/sum(pcSeoul$sdev^2)

pcLoad <- pcSeoul$loadings %*% diag(pcSeoul$sdev) # loadings (colums scaled by eigenvalue)
print(pcLoad, digits=2)
apply(pcLoad^2,2,sum)                             # squared loadings of factors = variance explained

## Component scores
( pcScores <- pcSeoul$scores )            # observation value associated with each component
print(round(cor(pcScores), 2))            # uncorrelated component scores
round(apply(pcScores,2,var) * 422/423, 2) # Each component has the eigenvalue as variance

( pcLoad2 <- cor(seoul,pcScores[,1:2]) ) # see loading of first two components

## Descriptive plots
screeplot(pcSeoul, type="lines")        # Scree graph
abline(h=1,lty=2)
biplot(pcSeoul)                         # unrotated components
abline(h=0,v=0, lty=5)

##
## Principle component analysis using matrix expressions
##
zSeoul <- scale(seoul)    
zSeoul
round(mean(zSeoul[,1]), 5)
scatterplotMatrix(zSeoul)   

( corSeoul <- t(zSeoul) %*% zSeoul /423)
eigSys <- eigen(corSeoul, symmetric=TRUE)   # eigen-system decomposition of correlation matrix
eigVal <- eigSys$values                     # eigenvalues
eigVec <- eigSys$vectors                    # eigenvectors equivalent with rotation matrix A
pcScores <- zSeoul %*% eigVec               # Component scores 
( pcLoad <- cor(zSeoul,pcScores[,1:2]) )

cat("Eigenvalues:\n");eigVal
cat("Percent Explained Variance:\n");cumsum(eigVal)/sum(eigVal)
cat("Rotation A:\n");eigVec
cat("Component Scores:\n");round(pcScores,3)
cat("Componet Loadings:\n");pcLoad

