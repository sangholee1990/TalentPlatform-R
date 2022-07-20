##
## Brett Lantz, Machine Learning with R. Packt, 2019
## Chapter 9: Clustering with k-means
## Objective: Finding Teen Market Segments
##
# rm(list=ls(all=T))                          # Clear environment

## Data: Social media use of 30,000 high school teens in 2006
## Their posts were scanned for the frequency of 38 keywords from the domains:
## [a] extracurricular activities,
## [b] fashion
## [c] religion
## [d] romance
## [e] antisocial behavior

# setwd("D:\\Google Drive\\UOS\\2021\\2학기\\머신러닝\\4\\Data")
# setwd("E:/04. TalentPlatform/Github/TalentPlatform-R/src/subject/LSH0232")

teens <- read.csv("snsdata.csv")
# View(teens)
##
## Exploring and cleaning the data
## Problem: To many records will be lost if on drops records with just one NA
##
table(teens$gender)                       # look at missing gender data
table(teens$gender, useNA = "ifany")
summary(teens$age)                        # look at missing data for age variable

## eliminate age outliers
teens$age <- ifelse(teens$age >= 13 & teens$age < 20,
                     teens$age, NA)
summary(teens$age)

## reassign missing gender values to "unknown"
teens$female <- ifelse(teens$gender == "F" &
                         !is.na(teens$gender), 1, 0)

teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)
teens$no_gender <- factor(teens$no_gender, levels=c(0,1), labels=c("response","no response"))

table(teens$gender, useNA = "ifany")      # check the recoding work
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

## finding the mean age by cohort
mean(teens$age)               # doesn't work
mean(teens$age, na.rm = TRUE) # works

aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)  # age by cohort

## create a vector with the average age for each gradyear, repeated by person
ave_age <- ave(teens$age, teens$gradyear,
                 FUN = function(x) mean(x, na.rm = TRUE))

teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

## check the summary results to ensure missing values are eliminated
summary(teens$age)

## recode expected graduation year
teens$grade <- factor(teens$gradyear, levels=c(2006,2007,2008,2009),
                      labels=c("senior","junior","sophomore","freshman"))
##
## Training a model on the data
##

## create a z-score standardized data frame for easier interpretation
interests <- teens[5:40]
summary(interests)            # word frequencies highly positively skewed
interestsZ <- as.data.frame(lapply(interests, scale))
summary(interestsZ)

## create the clusters using k-means
set.seed(2345)
teenClusters <- kmeans(interestsZ, 5)

##
## Evaluate model performance: intra cluster homogeneity and between cluster heterogeneity
##
teenClusters$size                     # look at the size of the clusters
teenClusters$totss
teenClusters$withinss
teenClusters$tot.withinss
teenClusters$betweenss

##
## Evaluate meaning of clusters by their feature means
## Investigate above and below average responses (value zero) by group
##
round(teenClusters$centers, 1)        # look at the cluster centers
clustCent <- teenClusters$centers
clustCent[clustCent > -0.1 & clustCent < 0.4] <- NA
round(t(clustCent), 1)

##
##  Attributes of respondents by cluster membership
##
teens$cluster <- teenClusters$cluster                # label records by cluster IDs
mean(teens$age)
aggregate(data = teens, age ~ cluster, mean)         # mean age by cluster
mean(teens$female)
aggregate(data = teens, female ~ cluster, mean)      # proportion of females by cluster
mean(teens$friends)
aggregate(data = teens, friends ~ cluster, mean)     # mean number of friends by cluster

