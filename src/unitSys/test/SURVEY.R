# Jags-Yord-XmetMulti-Mnormal.R 
# Accompanies the book:
#   Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition: 
#   A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.

source("DBDA2E-utilities.R")

#===============================================================================

genMCMC = function( data , xName , yName , 
                    numSavedSteps=10000 , thinSteps=1 , saveName=NULL ,
                    runjagsMethod=runjagsMethodDefault , 
                    nChains=nChainsDefault ) { 
  
  data = myData
  xName=xName
  yName=yName
  numSavedSteps=numSavedSteps
  thinSteps=thinSteps
  saveName=fileNameRoot
  runjagsMethod=runjagsMethodDefault
  nChains=nChainsDefault
  
  #-----------------------------------------------------------------------------
  # THE DATA.
  # Convert data file columns to generic x,y variable names for model:
  y = data[,yName]
  x = as.factor(data[,xName])
  x1 = as.numeric(as.factor(data[,xName[1]]))
  x1levels = levels(as.factor(data[,xName[1]]))
  x2 = as.numeric(as.factor(data[,xName[2]]))
  x2levels = levels(as.factor(data[,xName[2]]))
  x3 = as.numeric(as.factor(data[,xName[3]]))
  x3levels = levels(as.factor(data[,xName[3]]))
  x4 = as.numeric(as.factor(data[,xName[4]]))
  x4levels = levels(as.factor(data[,xName[4]]))
  x5 = as.numeric(as.factor(data[,xName[5]]))
  x5levels = levels(as.factor(data[,xName[5]]))
  x6 = as.numeric(as.factor(data[,xName[6]]))
  x6levels = levels(as.factor(data[,xName[6]]))
  x7 = as.numeric(as.factor(data[,xName[7]]))
  x7levels = levels(as.factor(data[,xName[7]]))
  x8 = as.numeric(as.factor(data[,xName[8]]))
  x8levels = levels(as.factor(data[,xName[8]]))
  x9 = as.numeric(as.factor(data[,xName[9]]))
  x9levels = levels(as.factor(data[,xName[9]]))
  x10 = as.numeric(as.factor(data[,xName[10]]))
  x10levels = levels(as.factor(data[,xName[10]]))
  x11 = as.numeric(as.factor(data[,xName[11]]))
  x11levels = levels(as.factor(data[,xName[11]]))
#  Ntotal = length(y)
  Nx1Lvl = length(unique(data[,xName[1]]))
  Nx2Lvl = length(unique(data[,xName[2]]))
  Nx3Lvl = length(unique(data[,xName[3]]))
  Nx4Lvl = length(unique(data[,xName[4]]))
  Nx5Lvl = length(unique(data[,xName[5]]))
  Nx6Lvl = length(unique(data[,xName[6]]))
  Nx7Lvl = length(unique(data[,xName[7]]))
  Nx8Lvl = length(unique(data[,xName[8]]))
  Nx9Lvl = length(unique(data[,xName[9]]))
  Nx10Lvl = length(unique(data[,xName[10]]))
  Nx11Lvl = length(unique(data[,xName[11]]))
  # Compute scale properties of data, for passing into prior to make the prior
  # vague on the scale of the data. 
  # For prior on baseline, etc.:
  yMean = mean(y)
  ySD = sd(y)
  # For hyper-prior on deflections:
  agammaShRa = unlist( gammaShRaFromModeSD( mode=sd(y)/2 , sd=2*sd(y) ) )
  
  # Do some checking that data make sense:
#  if ( any( !is.finite(x) ) ) { stop("All x values must be finite.") }
#  if ( any( y!=round(y) ) ) { stop("All y values must be integers (whole numbers).") }
#  if ( any( y < 1 ) ) { stop("All y values must be 1 or larger.") }
  # COMPRESS OUT ANY EMPTY VALUES OF Y:
#  yOrig=y
#  y=as.numeric(factor(y,levels=names(table(y))))
#  if ( any(y != yOrig) ) { 
#    warning("*** WARNING: Y RE-CODED TO REMOVE EMPTY LEVELS ***")
#  }
  nYlevels = max(y)  
  thresh = rep(NA,nYlevels-1)
  thresh[1] = 1 + 0.5
  thresh[nYlevels-1] = nYlevels-1 + 0.5
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    y = y ,
    x1 = x1 ,
    x2 = x2 ,
    x3 = x3 ,
    x4 = x4 ,
    x5 = x5 ,
    x6 = x6 ,
    x7 = x7 ,
    x8 = x8 ,
    x9 = x9 ,
    x10 = x10 ,
    x11 = x11 ,
    # Ntotal = dim(x)[1] ,
    Ntotal = length(x1) ,
    Nx1Lvl = Nx1Lvl ,
    Nx2Lvl = Nx2Lvl ,
    Nx3Lvl = Nx3Lvl ,
    Nx4Lvl = Nx4Lvl ,
    Nx5Lvl = Nx5Lvl ,
    Nx6Lvl = Nx6Lvl ,
    Nx7Lvl = Nx7Lvl ,
    Nx8Lvl = Nx8Lvl ,
    Nx9Lvl = Nx9Lvl ,
    Nx10Lvl = Nx10Lvl ,
    Nx11Lvl = Nx11Lvl ,
    nYlevels = nYlevels ,
    thresh = thresh ,
    Nx = dim(x)[2] ,
    # data properties for scaling the prior:
    yMean = yMean ,
    ySD = ySD ,
    agammaShRa = agammaShRa 
  )

  #-----------------------------------------------------------------------------
  # THE MODEL.
  modelString = "
  # Standardize the data:
  data {
    for ( j in 1:Nx ) {
      xm[j]  <- mean(x[,j])
      xsd[j] <-   sd(x[,j])
      for ( i in 1:Ntotal ) {
        zx[i,j] <- ( x[i,j] - xm[j] ) / xsd[j]
      }
    }
  }
  # Specify the model for standardized data:
  model {
    for ( i in 1:Ntotal ) {
      y[i] ~ dcat( pr[i,1:nYlevels] )
      pr[i,1] <- pnorm( thresh[1] , mu[i] , 1/Sigma^2 )
      for ( k in 2:(nYlevels-1) ) {
        pr[i,k] <- max( 0 ,  pnorm( thresh[ k ] , mu[i] , 1/Sigma^2 )
                           - pnorm( thresh[k-1] , mu[i] , 1/Sigma^2 ) )
      }
      pr[i,nYlevels] <- 1 - pnorm( thresh[nYlevels-1] , mu[i] , 1/Sigma^2 )
      mu[i] <- a0 + a1[x1[i]] + a2[x2[i]] + a3[x3[i]] + a4[x4[i]] + a5[x5[i]]
                 + a6[x6[i]] + a7[x7[i]] + a8[x8[i]] + a9[x9[i]] + a10[x10[i]]
                 + a11[x11[i]]
    }
    Sigma ~ dunif( nYlevels/1000 , nYlevels*10 )
    a0 ~ dnorm( yMean , 1/(ySD*5)^2 ) 
    #
    for ( j1 in 1:Nx1Lvl ) { a1[j1] ~ dnorm( 0.0 , 1/a1SD^2 ) }
    a1SD ~ dgamma(agammaShRa[1],agammaShRa[2]) # or try a folded t (Cauchy)
    #
    for ( j2 in 1:Nx2Lvl ) { a2[j2] ~ dnorm( 0.0 , 1/a2SD^2 ) }
    a2SD ~ dgamma(agammaShRa[1],agammaShRa[2]) # or try a folded t (Cauchy)
    #
    for ( j3 in 1:Nx3Lvl ) { a3[j3] ~ dnorm( 0.0 , 1/a3SD^2 ) }
    a3SD ~ dgamma(agammaShRa[1],agammaShRa[2]) # or try a folded t (Cauchy)
    #
    for ( j4 in 1:Nx4Lvl ) { a4[j4] ~ dnorm( 0.0 , 1/a4SD^2 ) }
    a4SD ~ dgamma(agammaShRa[1],agammaShRa[2]) # or try a folded t (Cauchy)
    #
    for ( j5 in 1:Nx5Lvl ) { a5[j5] ~ dnorm( 0.0 , 1/a5SD^2 ) }
    a5SD ~ dgamma(agammaShRa[1],agammaShRa[2]) # or try a folded t (Cauchy)
    #
    for ( j6 in 1:Nx6Lvl ) { a6[j6] ~ dnorm( 0.0 , 1/a6SD^2 ) }
    a6SD ~ dgamma(agammaShRa[1],agammaShRa[2]) # or try a folded t (Cauchy)
    #
    for ( j7 in 1:Nx7Lvl ) { a7[j7] ~ dnorm( 0.0 , 1/a7SD^2 ) }
    a7SD ~ dgamma(agammaShRa[1],agammaShRa[2]) # or try a folded t (Cauchy)
    #
    for ( j8 in 1:Nx8Lvl ) { a8[j8] ~ dnorm( 0.0 , 1/a8SD^2 ) }
    a8SD ~ dgamma(agammaShRa[1],agammaShRa[2]) # or try a folded t (Cauchy)
    #
    for ( j9 in 1:Nx9Lvl ) { a9[j9] ~ dnorm( 0.0 , 1/a9SD^2 ) }
    a9SD ~ dgamma(agammaShRa[1],agammaShRa[2]) # or try a folded t (Cauchy)
    #
    for ( j10 in 1:Nx10Lvl ) { a10[j10] ~ dnorm( 0.0 , 1/a10SD^2 ) }
    a10SD ~ dgamma(agammaShRa[1],agammaShRa[2]) # or try a folded t (Cauchy)
    #
    for ( j11 in 1:Nx11Lvl ) { a11[j11] ~ dnorm( 0.0 , 1/a11SD^2 ) }
    a11SD ~ dgamma(agammaShRa[1],agammaShRa[2]) # or try a folded t (Cauchy)
    #
    
    # Convert a0,a1[],a2[] to sum-to-zero b0,b1[],b2[] :
    for ( j1 in 1:Nx1Lvl ) {
      for ( j11 in 1:Nx11Lvl ) {
        # m[j1,j11] <- a0 + a1[j1] + a2[j2] + a3[j3] + a4[j4] + a5[j5] + a6[j6] + a7[j7]+ a8[j8] + a9[j9] + a10[j10] + a11[j11] # cell means 
        m[j1,j11] <- sum(a0 + a1[j1], a2[j2], a3[j3], a4[j4], a5[j5], a6[j6], a7[j7], a8[j8], a9[j9], a10[j10], a11[j11], na.rm = TRUE)
      }
    }
    b0 <- mean( 1:Nx1Lvl,1:Nx2Lvl] )
    for ( j1 in 1:Nx1Lvl ) { b1[j1] <- mean( m[j1,1:Nx2Lvl] ) - b0 }
    for ( j2 in 1:Nx2Lvl ) { b2[j2] <- mean( m[1:Nx2Lvl,j2] ) - b0 }
    for ( j3 in 1:Nx3Lvl ) { b3[j3] <- mean( m[1:Nx3Lvl,j3] ) - b0 }
    for ( j4 in 1:Nx4Lvl ) { b4[j4] <- mean( m[1:Nx4Lvl,j4] ) - b0 }
    for ( j5 in 1:Nx5Lvl ) { b5[j5] <- mean( m[1:Nx5Lvl,j5] ) - b0 }
    for ( j6 in 1:Nx6Lvl ) { b6[j6] <- mean( m[1:Nx6Lvl,j6] ) - b0 }
    for ( j7 in 1:Nx7Lvl ) { b7[j7] <- mean( m[1:Nx7Lvl,j7] ) - b0 }
    for ( j8 in 1:Nx8Lvl ) { b8[j8] <- mean( m[1:Nx8Lvl,j8] ) - b0 }
    for ( j9 in 1:Nx9Lvl ) { b9[j9] <- mean( m[1:Nx9Lvl,j9] ) - b0 }
    for ( j10 in 1:Nx10Lvl ) { b10[j10] <- mean( m[1:Nx10Lvl,j10] ) - b0 }
    for ( j11 in 1:Nx11Lvl ) { b11[j11] <- mean( m[1:Nx11Lvl,j11] ) - b0 }
        
    for ( k in 2:(nYlevels-2) ) {  # 1 and nYlevels-1 are fixed
      thresh[k] ~ dnorm( k+0.5 , 1/2^2 )
    }
  }
  " # close quote for modelString
  # Write out modelString to a text file
  writeLines( modelString , con="TEMPmodel.txt" )
  #-----------------------------------------------------------------------------
  # INTIALIZE THE CHAINS.
  # Let JAGS do it...
  #-----------------------------------------------------------------------------
  # RUN THE CHAINS
  parameters = c( "b0" ,  "b1" ,  "b2" , "b3" , "b4" ,  "b5" ,  "b6" ,  "b7" 
                  ,  "b8" ,  "b9" ,  "b10" ,  "b11" , "Sigma", "thresh")
  adaptSteps = 500  # Number of steps to "tune" the samplers
  burnInSteps = 1000
  runJagsOut <- run.jags( method=runjagsMethod ,
                          model="TEMPmodel.txt" , 
                          monitor=parameters , 
                          data=dataList ,  
                          #inits=initsList , 
                          n.chains=nChains ,
                          adapt=adaptSteps ,
                          burnin=burnInSteps , 
                          sample=ceiling(numSavedSteps/nChains) ,
                          thin=thinSteps ,
                          summarise=FALSE ,
                          plots=FALSE )
  codaSamples = as.mcmc.list( runJagsOut )
  # resulting codaSamples object has these indices: 
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  if ( !is.null(saveName) ) {
    save( codaSamples , file=paste(saveName,"Mcmc.Rdata",sep="") )
  }
  return( codaSamples )
} # end function

#===============================================================================
