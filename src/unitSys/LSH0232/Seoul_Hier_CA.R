
rm(list=ls(all=T))

# setwd("D:\\Google Drive\\UOS\\2021\\2학기\\머신러닝\\4\\Data")
setwd("E:/04. TalentPlatform/Github/TalentPlatform-R/src/subject/LSH0232")

library(spdep); library(rgdal); library(foreign) 
library(RColorBrewer); library(classInt)
library(tidyr); library(ggplot2)
library(ClustGeo)

clusterStats <- function(df, class, func=mean, plotMeans=TRUE){
  ## 
  ## Input:  df         original metric variables
  ##         class      cluster membership of observations by cutree(tree, K)
  ##         func       function to be evaluated in each cluster
  ##         plotMeans  function to plot means of variables by cluster
  ##
  ## Return: data-frame with values of variables in each cluster
  ##
  require(ggplot2); require(tidyr)
  df <- scale(df)
  df <- data.frame(Cluster=class, df)
  nofcol <- ncol(df)
  nofclass <- length(levels(class))
  resDf <- df[1:nofclass, ]
  for (j in 2:nofcol){
    res <- aggregate(df[, j], list(class), func) 
    resDf[, j] <- res[1:nofclass, 2]
  } 
  resDf["Cluster"] <- as.factor(paste("Cluster",1:nofclass, sep=" "))
  
  if (plotMeans){
    centers <- resDf[,-1]
    centers <- as.data.frame(t(centers))
    names(centers) <- paste("Cluster",1:nofclass, sep=" ")
    centers$Symbol <- row.names(centers)
    centers <- tidyr::gather(centers, "Cluster", "Mean", -Symbol)
    centers$Color <- centers$Mean > 0
    meansPlot <- ggplot(centers, aes(x=Symbol, y=Mean, fill=Color)) +
      geom_bar(stat='identity', position='identity', width=0.5) +
      facet_grid(Cluster ~ ., scales='free_y')
    plot(meansPlot)
  }
  return(resDf)
} ## end:aggFunc

##Read Shapefile
sample.shp <- readOGR(dsn = ".", layer = "Seoul_dong", encoding = 'ESRI Shapefile')
sample.df <- read.dbf('Seoul_dong.dbf')
sample.df <- data.frame(sample.shp)

## select variables for dissimilarity
xVars <- sample.df[,c("Price", "Year", "Nurser", "Hospit", "Park",   
                      "Culture", "Sub","H_univ", "M_priv","E_prog")]
row.names(xVars)<-sample.df$ADM_DR_NM

##
##Hierarchical clustering
##

## Perform z-transformation
zXvars <- scale(xVars)
featDist <- dist(zXvars)

tree <- hclust(featDist, method='ward.D2')
par(mfrow=c(1,1))
plot(tree)
K <- 3
rect.hclust(tree, k=K)

##Clustering with different metric
#featDist <- as.dist((1 - cor(t(zXvars)))/2) 
#tree <- hclust(featDist, method='complete') 

##visualizing results
neighClus <- cutree(tree, K)
table(neighClus)

clustMeans <- clusterStats(xVars, as.factor(neighClus))
clustMeans

#map results  
pal.Qual <- brewer.pal(12, "Set3")
map.col <- pal.Qual[neighClus]
qual.Name <- levels(as.factor(neighClus))

plot(sample.shp, col=map.col)
legend("bottomleft", title = "Classes", legend = qual.Name, cex = 1, 
       fill = pal.Qual[1:length(qual.Name)], bty = "n", ncol = 1)
box()

##
##Spatial Cluster analysis
##
##
## Generate graph and spherical distance matrices
##
nb <- poly2nb(sample.shp, queen=T)                 # extract first order neighbors links
B <- nb2mat(nb, style="B")                            # convert neighbor list to binary matrix
#sphDist <- sp::spDists(sample.shp, longlat=T)             # spherical distance matrix among tracts in km

##
## Visulize first order neighbors
##
plot(sample.shp, col="palegreen3", border=grey(0.9), axes=T) 
plot(nb, coords=coordinates(sample.shp), pch=19, cex=0.1, col="blue", add=T)
title("Spatial Neighbors Links among Dong") 

#calculation freature distance matrix
geoDist<- 1-B; diag(geoDist) <- 0; geoDist <- as.dist(geoDist)
#geoDist <- as.dist(sphDist)

##evaluate mixture of DO AND D1
range.alpha <- seq(0,1,0.1)
K <- 4
par(mfrow=c(1,1))
cr <- choicealpha(featDist, geoDist, range.alpha, K, graph=TRUE)  # normalized values

## Perform spatial cluster analysis
tree <- hclustgeo(featDist, geoDist, alpha=0.1)
plot(tree, hang=-1)
rect.hclust(tree, k=K)

##visualizing results ##same as above
neighClus <- cutree(tree, K)
table(neighClus)

clustMeans <- clusterStats(xVars, as.factor(neighClus))
clustMeans

#map results  
map.col <- pal.Qual[neighClus]
qual.Name <- levels(as.factor(neighClus))

plot(sample.shp, col=map.col)
legend("bottomleft", title = "", legend = qual.Name, cex = 1, 
       fill = pal.Qual[1:length(qual.Name)], bty = "n", ncol = 1)
box()


