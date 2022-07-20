# Example for Jags-Ymet-XmetMulti-Mrobust.R 
#------------------------------------------------------------------------------- 
# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
# UNCOMMENT ONE OF THE FOLLOWING SECTIONS (In RStudio, select and ctrl-shift-C)
#.............................................................................
myData = read.csv( file="SURVEYRESULT.csv" )


yName = "SATISFY" 
xName = c("DISCOMFORT","COMPLAIN","SENSE","FRUN","FDROP","ARUN","ADROP","LRUN","LDROP","DRUN","DDROP")


fileNameRoot = "SURVEYRESULT" 
numSavedSteps=5000 ; thinSteps=2 # increase for higher ESS
lmInfo <- lm( myData[,yName] ~ myData[,xName[1]] + myData[,xName[2]] + myData[,xName[3]] + myData[,xName[4]] + myData[,xName[5]] 
              + myData[,xName[6]] + myData[,xName[7]] + myData[,xName[8]] + myData[,xName[9]] + myData[,xName[10]] + myData[,xName[11]] , data=myData )


#.............................................................................
# myData = read.csv( file="Movies.csv" ) # Real data
# yName = "Rating" ; xName = c("Year","Length")
# # convert 1/2 scale ratings to integers:
# myData$Rating = as.numeric(as.factor(myData$Rating))
# fileNameRoot = "Movies-" 
# numSavedSteps=5000 ; thinSteps=1 # increase for higher ESS
# lmInfo <- lm( myData[,yName] ~ myData[,xName[1]] + myData[,xName[2]] , data=myData )
#.............................................................................
# myData = read.csv( file="OrdinalProbitData-Movies.csv" ) # Fictitious data
# yName = "Rating" ; xName = c("Year","Length")
# fileNameRoot = "OrdinalProbitData-Movies-" 
# numSavedSteps=5000 ; thinSteps=1 # increase for higher ESS
# lmInfo <- lm( myData[,yName] ~ myData[,xName[1]] + myData[,xName[2]] , data=myData )
#.............................................................................
# myData = read.csv("HappinessAssetsDebt.csv")
# yName = "Happiness"
# xName = c("Assets","Debt")[1]
# # # get random subset of reduced size:
# # subRows = sample( 1:nrow(myData) )
# # subRows = subRows[1:500]
# # myData = myData[subRows,]
# fileNameRoot = paste0("HappinessAssetsDebt-",paste(xName,collapse="-"),"-") 
# numSavedSteps=12000 ; thinSteps=5
#.............................................................................
graphFileType = "eps" 
#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("SURVEY.R")
#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
#startTime = proc.time()
mcmcCoda = genMCMC( data = myData , xName=xName , yName=yName ,
                    numSavedSteps=numSavedSteps , thinSteps=thinSteps , 
                    saveName=fileNameRoot )
stopTime = proc.time()
#duration = stopTime - startTime
#show(duration)

codaSamples = as.mcmc.list( mcmcCoda)
mcmcMat = as.matrix(codaSamples,chains=TRUE)
write.csv( mcmcMat , file=paste(fileNameRoot,"Mcmc.csv",sep="") )
