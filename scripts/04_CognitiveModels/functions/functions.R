# Create a functio nwhich will tune the alphas for a lasso model
tuneAlpha <- function(x, y, alphaSequence, nFolds){
  # Prep the outputs 
  enet.optparam<-matrix(nrow=length(alphaSequence),ncol=3)
  colnames(enet.optparam)<-c("Alpha","Lambda","CVM")
 
  # Now make sure our data is in the rate format
  inputData <- as.matrix(x)

  # Lets iterate through each alpha an print the output to our output data frame
  count=1
  for(a in alphaSequence){
    enet.alphas.cv <- cv.glmnet(inputData, y, alpha=a, standardize=F, nfolds=nFolds, maxit=100000000)
    enet.optparam[count,] <- c(a,
                        enet.alphas.cv$lambda.min,
                        enet.alphas.cv$cvm[enet.alphas.cv$lambda==enet.alphas.cv$lambda.min])
    count <- count + 1
    
  }
  optval <- enet.optparam[which(enet.optparam[,3]==min(enet.optparam[,3])),]
  optAlpha <- optval[1]
  optLambda <- optval[2]
  output <- cbind(optAlpha, optLambda)
  return(output)
}


# Create a function which will take a y and an input set of data (x)
# run cv lasso with alpha tuning
runLassoforHiLo <- function(x, y, trainingIterations = 100, nCor=3, nofFolds=10, alphaSequence=seq(0,1,by=0.05)){
    # First thing we have to do is prepare our output
    outputMatrix <- matrix(0, ncol(x), trainingIterations+1)
    outputMatrix[,1] <- colnames(x)
    
    # Now we need to make sure our inputs are the correct formats
    inputData <- as.matrix(x)
    
    # Now set up our parallel environment
    cl <- makeCluster(nCor)
    registerDoParallel(cl)
    
    # Now lets run through each iteration and perform the following steps
    # 1.) Tune to find the optimal alpha for each fold - using tuneAlpha function
    # 2.) create a model using glmnet
    # 3.) return the betas as a numeric vector
    # All of this is going to be done inparallel ... so should be good
    output <- foreach(i=seq(1,trainingIterations), .combine='rbind') %dopar% {
        # First load required library(s)
        source('/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R')
        source('/home/adrose/varSelectionHiLo/scripts/functions.R')
        install_load('glmnet', 'bootstrap', 'psych')
        
        # First find the optimum values to use for glmnet
        optVals <- tuneAlpha(inputData, y, alphaSequence, nofFolds)
        
        
        # Now we need to create our final model with our opt vals
        mod <- glmnet(inputData, y, standardize=F, alpha=optVals[1], lambda=optVals[2], maxit=100000000)
        
        # Now return our values
        valsToReturn <- as.numeric(mod$beta)
    }
    # Kill our cluster
    stopCluster(cl)
    
    # Now return our output  
    outputMatrix[,2:ncol(outputMatrix)] <- output
    return(outputMatrix)
}

# Now create a vector of the optVals
returnOptAlpha <- function(x, y, trainingIterations = 100, nCor=3, nofFolds=10, alphaSequence=seq(0,1,by=0.05)){
  # Now we need to make sure our inputs are the correct formats
  inputData <- as.matrix(x)

  # Now set up our parallel environment
  cl <- makeCluster(nCor)
  registerDoParallel(cl)

  # Now lets run through each iteration and perform the following steps
  # 1.) Tune to find the optimal alpha for each fold - using tuneAlpha function
  # 2.) create a model using glmnet
  # 3.) return the betas as a numeric vector
  # All of this is going to be done inparallel ... so should be good
  output <- foreach(i=seq(1,trainingIterations), .combine='c') %dopar% {
    # First load required library(s)
    source('/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R')
    source('/home/adrose/varSelectionHiLo/scripts/functions.R')
    install_load('glmnet', 'bootstrap', 'psych')

    # First find the optimum values to use for glmnet
    tuneAlpha(inputData, y, alphaSequence, nofFolds)[1]

  }
  # Kill our cluster
  stopCluster(cl)

  # Now return our output  
  outputMatrix <- output
  return(outputMatrix)
}

# Now create a function which will return a booliean value for which 
# rows meet the cut off threshold for times slected 
# by our lasso model 
rmFat <- function(outputFromrunLasso, imagingData, cutoffToApply=NULL){
  # see if we need to declare our own cut off
  if(identical(cutoffToApply,NULL)=='TRUE'){
    cutoffToApply<-median(returnSelection(outputFromrunLasso))
  }  
  # First create our bool vector
  boo.vec <- rep('FALSE', nrow(outputFromrunLasso))
  sumIndex <- returnSelection(outputFromrunLasso)
  boo.vec[which(sumIndex>=cutoffToApply)] <- 'TRUE'
  index <- which(boo.vec=='TRUE')
  # Now apply our boo vec to the imaging data
  output <- imagingData[,index]
  return(output)
}

returnSelection <- function(outputFromrunLasso){
  sumIndex <- rowSums(abs(sign(apply(outputFromrunLasso[,2:ncol(outputFromrunLasso)], 
              2, function(x) as.numeric(as.character(x))))))
  return(sumIndex)
}

plotSelection <- function(inputBetaMatrix, graphTitle){
  values <- as.data.frame(table(returnSelection(inputBetaMatrix)))
  output <- ggplot(values, aes(x=Var1, y=Freq)) + 
    geom_bar(stat='identity') + 
    ggtitle(graphTitle)
  return(output)
}

# These functions need to be declared in order to run the next function
theta.fit <- function(x,y){lsfit(x,y)}
theta.predict <- function(modm,x){cbind(1,x)%*%modm$coef}


# Now create a function which will compute our model fit's metrics
computeModelFitMetrics <- function(x, y, seedValue=1, nGroups=10, returnBetas=F){
  set.seed(seedValue)
  # First thing we need to do is run the final model
  modm <- lm(y ~ as.matrix(x))
  
  # Now get some values
  n <- dim(x)[1]
  p <- dim(x)[2]
  
  # Now do a crossval of our model
  modmCV <- crossval(x, y, theta.fit, theta.predict,ngroup=nGroups)
  
  # Now get our output metrics 
  rawRSquared <- cor(y, modm$fitted.values)^2
  cvRSquared <- cor(y, modmCV$cv.fit)^2
  rawICC <- ICC(cbind(y, modm$fitted.values))$results[4,2]
  cvICC <- ICC(cbind(y, modmCV$cv.fit))$results[4,2]
  rawRMSE <- sqrt(mean((y-modm$fitted.values)^2))
  cvRMSE <- sqrt(mean((y-modmCV$cv.fit)^2))
  adjRSquared <- cor(y, modm$fitted.values)^2 - 
                (1 - cor(y, modm$fitted.values)^2)*(p/(n-p-1))
  
  # now create our output
  output <- as.data.frame(cbind(n,p,rawRSquared,cvRSquared,rawICC,cvICC,rawRMSE,cvRMSE,adjRSquared))
   colnames(output) <- c('n','p','R2', 'CVR2', 'ICC', 'CVICC', 'RMSE', 'CVRMSE', 'ADJR2')
  # Now see if the user would like the betas to be returned
  if(returnBetas=='TRUE'){
    outputList <- list()
    outputList[[1]] <- output
    outputList[[2]] <- coefficients(modm)[order(coefficients(modm), decreasing=T)]
    output <- outputList
  }
  return(output)
}

# Now create a function which will compute our model fit's metrics w/ a predefined trianing and validation data sets
computeModelFitMetrics2 <- function(xTrain, xValid, yTrain,yValid,returnBetas=F){
  # First thing we need to do is run the final model
  tmp1 <- as.data.frame(cbind(yTrain, xTrain))
  tmp2 <- as.data.frame(cbind(yValid, xValid)) 
  modm <- lm(yTrain ~ ., tmp1)
  modmValid <- predict(modm, newdata=tmp2)
  # Now get some values
  n <- dim(xTrain)[1]
  p <- dim(xTrain)[2]  
  # Now get our output metrics 
  rawRSquared <- cor(yTrain, modm$fitted.values)^2
  cvRSquared <- cor(yValid, modmValid)^2
  rawICC <- ICC(cbind(yTrain, modm$fitted.values))$results[4,2]
  cvICC <- ICC(cbind(yValid, modmValid))$results[4,2]
  rawRMSE <- sqrt(mean((yTrain-modm$fitted.values)^2))
  cvRMSE <- sqrt(mean((yValid-modmValid)^2))
  adjRSquared <- cor(yTrain, modm$fitted.values)^2 - 
                (1 - cor(yTrain, modm$fitted.values)^2)*(p/(n-p-1))  
  # now create our output
  output <- as.data.frame(cbind(n,p,rawRSquared,cvRSquared,rawICC,cvICC,rawRMSE,cvRMSE,adjRSquared))
   colnames(output) <- c('n','p','R2', 'CVR2', 'ICC', 'CVICC', 'RMSE', 'CVRMSE', 'ADJR2')
  # Now see if the user would like the betas to be returned
  if(returnBetas=='TRUE'){
    outputList <- list()
    outputList[[1]] <- output
    outputList[[2]] <- coefficients(modm)
    output <- outputList
  }
  return(output)
}

# Create a function which will find the superset of all ROI's
# and then return the full model
returnFullModel <- function(inputMaleMetrics, inputFemaleMetrics){
  # Find all of the male ROI's
  maleAllRegions <- names(inputMaleMetrics[[2]])
  # Rm the intercept
  maleAllRegions <- maleAllRegions[-grep('Intercept', maleAllRegions)]
  # now remove all fields before jlf
  maleAllRegions <- strSplitMatrixReturn(maleAllRegions, ')')[,2]
  # Now do the same for the female regions
  femaleAllRegions <- names(inputFemaleMetrics[[2]])
  femaleAllRegions <- femaleAllRegions[-grep('Intercept', femaleAllRegions)]
  femaleAllRegions <- strSplitMatrixReturn(femaleAllRegions, ')')[,2]
  # now append and find all unique values
  allRegions <- unique(append(maleAllRegions, femaleAllRegions))
  # Now create the formula
  formulaOut <- paste(allRegions, collapse='+')
  formulaOut <- paste('F1_Exec_Comp_Cog_Accuracy ~', formulaOut, sep='')
  formulaOut <- as.formula(formulaOut)
  return(formulaOut)
}

# create a boot strap function which will return the coefficients
bs <- function(formula, data, indices) {
    d <- data[indices,] # allows boot to select sample
    fit <- lm(formula, data=d)
    return(coef(fit))
}
