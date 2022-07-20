# rm(list=ls(all=T))
# setwd("D:\\Google Drive\\UOS\\2021\\2학기\\머신러닝\\4\\Data")
# setwd("E:/04. TalentPlatform/Github/TalentPlatform-R/src/subject/LSH0232")

library(NbClust);library(vegan) ##Optimal k selection
library(rgdal) 
library(RColorBrewer); library(classInt)
library(ggplot2)

##Read Shapefile
sample.shp <- readOGR(dsn = ".", layer = "Seoul_dong")
sample.df <- data.frame(sample.shp)
xVars <- sample.df[,c("Price", "Year", "Nurser", "Hospit", "Park",   
                      "Culture", "Sub","H_univ", "M_priv","E_prog")]
zXvars <- scale(xVars)
##
##K-means clustering
##

# optimal k for cluster
wss <- c()

for (i in 1:9){
  cl <- kmeans(zXvars, centers = i)
  wss[i] <- cl$tot.withinss
}
plot(1:9, wss, type="b", xlab="Number of Cluster", ylab="within groups sum of squares")

nb <- NbClust(zXvars, min.nc = 2, max.nc = 10, method = "kmeans")
hist(nb$Best.nc[1,], breaks = 10) ##limited to 10.
model1 <- cascadeKM(zXvars, 1, 10, iter = 15)
plot(model1, sortg = TRUE) # END determin number of K 

K <- 3
neighClus <- kmeans(zXvars, centers = K)

##Mapping
pal.Qual <- brewer.pal(12, "Set3")
map.col <- pal.Qual[neighClus$cluster]
qual.Name <- levels(as.factor(neighClus$cluster))
par(mfrow=c(1,1))
plot(sample.shp, col=map.col)
legend("bottomleft", title = "", legend = qual.Name, cex = 1, 
       fill = pal.Qual[1:length(qual.Name)], bty = "n", ncol = 1)

##Summary clusters
p <- length(xVars)
sum.res <- NULL
cls <- as.factor(1:K)

for(i in 1:p){
  var.mean <- neighClus$centers[,i]
  tmp.res <- cbind(rep(i, K), var.mean, cls)
  sum.res <- rbind(sum.res, tmp.res)
}

sum.res <- data.frame(sum.res)
sum.res$cls <- as.factor(sum.res$cls)
sum.res$color <- 0
sum.res$color[which(sum.res$var.mean>0)] <- 1

ggplot(sum.res, aes(x=V1, y=var.mean, fill=as.factor(color))) +
  geom_bar(stat='identity', position='identity', width=0.5)+ 
  facet_grid(cls ~ ., scales='free_y')+
  scale_x_discrete(limits=factor(c(1:p)), labels=colnames(xVars))+
  labs(y="Average", x="Variables")


