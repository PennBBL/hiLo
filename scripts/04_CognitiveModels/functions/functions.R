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
        install_load('glmnet', 'bootstrap', 'psych', 'caret')
	        
	# Now create our BS sample
	sampleVals <- as.numeric(unlist(createDataPartition(y)))
	bootY <- y[sampleVals]
        bootX <- inputData[sampleVals,]

        # First find the optimum values to use for glmnet
        optVals <- tuneAlpha(inputData, y, alphaSequence, nofFolds)
        
        
        # Now we need to create our final model with our opt vals
        mod <- glmnet(bootX, bootY, standardize=F, alpha=optVals[1], lambda=optVals[2], maxit=100000000)
        
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
rmFat <- function(outputFromrunLasso, imagingData, percentileToApply){
  # Find our cut off from the provided percentile
  cutoffToApply <- quantile(returnSelectionCol(outputFromrunLasso), .5) 
  # First create our bool vector
  boo.vec <- rep('FALSE', nrow(outputFromrunLasso))
  sumIndex <- returnSelectionCol(outputFromrunLasso)
  boo.vec[which(sumIndex>=cutoffToApply)] <- 'TRUE'
  index <- which(boo.vec=='TRUE')
  # Now apply our boo vec to the imaging data
  output <- imagingData[,index]

  return(output)
}

returnSelectionCol <- function(outputFromrunLasso){
  sumIndex <- rowSums(abs(sign(apply(outputFromrunLasso[,2:ncol(outputFromrunLasso)], 
              2, function(x) as.numeric(as.character(x))))))
  sumIndex <- cbind(rownames(outputFromrunLasso), sumIndex)
  return(sumIndex)
}

returnSelectionRow <- function(outputFromrunLasso){
  sumIndex <- colSums(abs(sign(apply(outputFromrunLasso[,2:ncol(outputFromrunLasso)], 
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
  if(!identical(dim(x), NULL)){
    n <- dim(x)[1]
    p <- dim(x)[2]
  }
  if(identical(dim(x), NULL)){
    n <- length(x)
    p <- 1
  }
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
    outVals <- coef(fit)
    outVals <- outVals[-grep('ntercept', names(outVals))]
    return(outVals)
}

# Create a function to claculate stand error
# Create a function which will give me the standard error 
stand_err <- function(input_vector){
  tmp <- sd(input_vector,na.rm=T)/sqrt(sum(!is.na(input_vector)))
  return(tmp)
}
has.interaction <- function(x,terms){
    out <- sapply(terms,function(i){
        sum(1-(strsplit(x,":")[[1]] %in% strsplit(i,":")[[1]]))==0
    })
    return(sum(out)>0)
}

# Function Model.select
# model is the lm object of the full model
# keep is a list of model terms to keep in the model at all times
# sig gives the significance for removal of a variable. Can be 0.1 too (see SPSS)
# verbose=T gives the F-tests, dropped var and resulting model after 
model.select <- function(model,keep,sig=0.05,verbose=F){
      counter=1
      # check input
      if(!is(model,"lm")) stop(paste(deparse(substitute(model)),"is not an lm object\n"))
      # calculate scope for drop1 function
      terms <- attr(model$terms,"term.labels")
      if(missing(keep)){ # set scopevars to all terms
          scopevars <- terms
      } else{            # select the scopevars if keep is used
          index <- match(keep,terms)
          # check if all is specified correctly
          if(sum(is.na(index))>0){
              novar <- keep[is.na(index)]
              warning(paste(
                  c(novar,"cannot be found in the model",
                  "\nThese terms are ignored in the model selection."),
                  collapse=" "))
              index <- as.vector(na.omit(index))
          }
          scopevars <- terms[-index]
      }

      # Backward model selection : 

      while(T){
          # extract the test statistics from drop.
          test <- drop1(model, scope=scopevars,test="F")

          if(verbose){
              cat("-------------STEP ",counter,"-------------\n",
              "The drop statistics : \n")
              print(test)
          }

          pval <- test[,dim(test)[2]]

          names(pval) <- rownames(test)
          pval <- sort(pval,decreasing=T)

          if(sum(is.na(pval))>0) stop(paste("Model",
              deparse(substitute(model)),"is invalid. Check if all coefficients are estimated."))

          # check if all significant
          if(pval[1]<sig) break # stops the loop if all remaining vars are sign.

          # select var to drop
          i=1
          while(T){
              dropvar <- names(pval)[i]
              check.terms <- terms[-match(dropvar,terms)]
              x <- has.interaction(dropvar,check.terms)
              if(x){i=i+1;next} else {break}              
          } # end while(T) drop var

          if(pval[i]<sig) break # stops the loop if var to remove is significant

          if(verbose){
             cat("\n--------\nTerm dropped in step",counter,":",dropvar,"\n--------\n\n")              
          }

          #update terms, scopevars and model
          scopevars <- scopevars[-match(dropvar,scopevars)]
          terms <- terms[-match(dropvar,terms)]

          formul <- as.formula(paste(".~.-",dropvar))
          model <- update(model,formul)

          if(length(scopevars)==0) {
              warning("All variables are thrown out of the model.\n",
              "No model could be specified.")
              return()
          }
          counter=counter+1
      } # end while(T) main loop
      return(model)
}

returnCVStepFit <- function(dataFrame, genderID, grepID, pValue=.05, iterationCount=1000, nCor=3, selectionPercent=.75, returnBetas=TRUE, residVals=FALSE, regressWithin=TRUE){
  # Prepare our data
  isolatedGender <- dataFrame[which(dataFrame$sex==genderID),]
  colsOfInterest <- grep(grepID, names(isolatedGender))
  valuesToUse <- scale(isolatedGender[,colsOfInterest])[,1:length(colsOfInterest)]
  outcomeVal <- scale(isolatedGender$F1_Exec_Comp_Cog_Accuracy)
  dataToUse <- as.data.frame(cbind(outcomeVal, valuesToUse))
  dataToUse <- dataToUse[complete.cases(dataToUse),]

  # Now set up our parallel environment
  cl <- makeCluster(nCor)
  registerDoParallel(cl)

  # Bootstrap forward p value step wise model selection
  output <- foreach(i=seq(1,iterationCount), .combine='cbind') %dopar% {
      # First load required library(s)
      source('/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R')
      source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
      install_load('caret', 'SignifReg')
      
      # create our model
      folds <- createResample(dataToUse$V1,list=T)
      index <- unlist(folds[1])
      dataTrain <- dataToUse[index,]
      stepVAR <- SignifReg(scope=V1~., data=dataTrain, alpha=pValue, direction="forward", criterion="p-value")
      
      # now create our output vector
      outCol <- matrix(0, nrow=dim(dataToUse)[2], ncol=1)
      rownames(outCol) <- colnames(dataToUse)
      selectedIndex <- match(colnames(stepVAR$model)[2:dim(stepVAR$model)[2]], rownames(outCol))
      outCol[selectedIndex,] <- 1

      # Now print our output
      outCol
  }
  
  # Kill our cluster
  stopCluster(cl)
  
  # Now select the varaibles that were selected more then the mode value for variables selected.
  outputSelected <- as.matrix(returnSelectionCol(output)[-1], ncol=1, nrow=dim(valuesToUse)[2])
  rownames(outputSelected) <- colnames(valuesToUse)
  modeValue <- quantile(returnSelectionCol(output), selectionPercent) 
  indexToUse <- unique(c(1,which(returnSelectionCol(output) >=modeValue)))
  dataToUse <- dataToUse[,indexToUse]
    
  # Now get our model
  modelOut <- as.formula(paste('V1 ~', paste(colnames(dataToUse)[2:dim(dataToUse)[2]], collapse='+')))
  # Now produce our final models
  x <- dataToUse[,2:dim(dataToUse)[2]]
  if(regressWithin=="TRUE"){
    x <- regressWithinModality(x, grepPattern=grepID)
  }
  y <- dataToUse[,1]
  modm <- lm(y ~ as.matrix(x))
  
  # Grab our n and p values
  # Now get some values
  if(!identical(dim(x), NULL)){
    n <- dim(x)[1]
    p <- dim(x)[2]
  }
  if(identical(dim(x), NULL)){
    n <- length(x)
    p <- 1
  }

  # Now do a crossval of our model
  modmCV <- crossval(x, y, theta.fit, theta.predict,ngroup=10)

  # Now get our output metrics 
  rawRSquared <- cor(y, modm$fitted.values)^2
  cvRSquared <- cor(y, modmCV$cv.fit)^2
  rawICC <- ICC(cbind(y, modm$fitted.values))$results[4,2]
  cvICC <- ICC(cbind(y, modmCV$cv.fit))$results[4,2]
  rawRMSE <- sqrt(mean((y-modm$fitted.values)^2))
  cvRMSE <- sqrt(mean((y-modmCV$cv.fit)^2))
  adjRSquared <- cor(y, modm$fitted.values)^2 - 
                (1 - cor(y, modm$fitted.values)^2)*(p/(n-p-1))

  # Now prepare our output
  output <- as.data.frame(cbind(n,p,rawRSquared,cvRSquared,rawICC,cvICC,rawRMSE,cvRMSE,adjRSquared))
  colnames(output) <- c('n','p','R2', 'CVR2', 'ICC', 'CVICC', 'RMSE', 'CVRMSE', 'ADJR2')
  if(returnBetas=='TRUE'){
    outputList <- list()
    outputList[[1]] <- output
    outputList[[2]] <- coefficients(modm)
    outputList[[3]] <- outputSelected
    output <- outputList
  }
  return(output)  
}

has.interaction <- function(x,terms){
    out <- sapply(terms,function(i){
        sum(1-(strsplit(x,":")[[1]] %in% strsplit(i,":")[[1]]))==0
    })
    return(sum(out)>0)
}

returnSelectionN <- function(dataFrame, genderID, grepID, pValue=.05, iterationCount=1000, nCor=3, dir='forward'){
  # Prepare our data
  isolatedGender <- dataFrame[which(dataFrame$sex==genderID),]
  colsOfInterest <- grep(grepID, names(isolatedGender))
  valuesToUse <- scale(isolatedGender[,colsOfInterest])[,1:length(colsOfInterest)]
  outcomeVal <- scale(isolatedGender$F1_Exec_Comp_Cog_Accuracy)
  dataToUse <- as.data.frame(cbind(outcomeVal, valuesToUse))
  dataToUse <- dataToUse[complete.cases(dataToUse),]

  # Now set up our parallel environment
  cl <- makeCluster(nCor)
  registerDoParallel(cl)

  # Bootstrap forward p value step wise model selection
  output <- foreach(i=seq(1,iterationCount), .combine='cbind',.export=ls(envir=globalenv()),.errorhandling = 'remove') %dopar% {
      # First load required library(s)
      source('/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R')
      source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
      install_load('caret', 'SignifReg', 'MASS')
      
      # create our model
      folds <- createResample(dataToUse$V1,list=T)
      index <- unlist(folds[1])
      dataTrain <- dataToUse[index,]
      if(dir=='forward'){
        stepVAR <- SignifReg(scope=V1~., data=dataTrain, alpha=pValue, direction='forward', criterion="p-value")
      }
      if(dir=='backward'){
        fitVals <- lm(V1~., data=dataTrain)
        stepVAR <- model.select(fitVals, verbose=T)
      }
      if(dir=='both'){
        fitVals <- lm(V1~., data=dataTrain)
        stepVAR <- stepAIC(fitVals, direction='both')
      }
      
      # now create our output vector
      outCol <- matrix(0, nrow=dim(dataToUse)[2], ncol=1)
      rownames(outCol) <- colnames(dataToUse)
      selectedIndex <- match(colnames(stepVAR$model)[2:dim(stepVAR$model)[2]], rownames(outCol))
      outCol[selectedIndex,] <- 1

      # Now print our output
      outCol
  }
  
  # Kill our cluster
  stopCluster(cl)

  # Now return the output binary matrix
  return(output)  
}

returnFitMetricsFromModel <- function(lmOutput){
  lengthVal <- dim(lmOutput$model)[2]
  x <- as.matrix(lmOutput$model[,2:lengthVal])
  y <- lmOutput$model[,1]
  modm <- lmOutput
  # Grab our n and p values
  # Now get some values
  if(!identical(dim(x), NULL)){
    n <- dim(x)[1]
    p <- dim(x)[2]
  }
  if(identical(dim(x), NULL)){
    n <- length(x)
    p <- 1
  }

  # Now do a crossval of our model
  modmCV <- crossval(x, y, theta.fit, theta.predict,ngroup=10)

  # Now get our output metrics 
  rawRSquared <- cor(y, modm$fitted.values)^2
  cvRSquared <- cor(y, modmCV$cv.fit)^2
  rawICC <- ICC(cbind(y, modm$fitted.values))$results[4,2]
  cvICC <- ICC(cbind(y, modmCV$cv.fit))$results[4,2]
  rawRMSE <- sqrt(mean((y-modm$fitted.values)^2))
  cvRMSE <- sqrt(mean((y-modmCV$cv.fit)^2))
  adjRSquared <- cor(y, modm$fitted.values)^2 - 
                (1 - cor(y, modm$fitted.values)^2)*(p/(n-p-1))

  # Now prepare our output
  output <- as.data.frame(cbind(n,p,rawRSquared,cvRSquared,rawICC,cvICC,rawRMSE,cvRMSE,adjRSquared))
  colnames(output) <- c('n','p','R2', 'CVR2', 'ICC', 'CVICC', 'RMSE', 'CVRMSE', 'ADJR2')
  return(output)
}

# Now declare a function to combine all of the importance outputs
returnAllOut <- function(genderVal, modalityVal, genderName){
  # Declare all of the csv names
  stepN <- paste(genderVal, 'OutSelectVals.csv', sep='')
  stepBeta <- paste(genderVal, 'OutBetaVals.csv', sep='')
  rfImportance <- paste(genderName, 'RFImportance.csv', sep='')
  rfImportance2 <- paste(genderName, 'RFImportance2.csv', sep='')
  lasBeta <- paste('all', genderName, 'Beta.csv', sep='')
  lasSel <- paste('all', genderName, 'Select.csv', sep='')
  nameVals <- c(stepN, stepBeta, rfImportance, rfImportance2, lasBeta, lasSel)
  # Now reach each csv and take out the correct column value
  outData <- matrix(NA, nrow=72, ncol=6)
  tmp <- read.csv(stepN)
  rownames(outData) <- tmp[,1]
  colVal <- c(1,1,1,1,2,2)
  q <- 1
  for( i in nameVals){
    tmp <- read.csv(i)
    colnames(tmp) <- tolower(colnames(tmp))
    modVal <- grep(modalityVal, colnames(tmp))
    outCol <- tmp[match(rownames(outData), tmp[,colVal[q]]),modVal[1]]
    outData[,q] <- outCol
    q <- q + 1
  }
  # Now find any NA mistakes by finding the modeal value across the row
  # If it is NA then set the entire row to NA
  for(i in seq(1,72)){
    modeVal <- Mode(outData[i,])
    if(is.na(modeVal)){
      outData[i,] <-rep(NA, 6) 
    }
  }
  colnames(outData) <- c('AustinN', 'AllBeta', 'RfMSE', 'RfPurity', 'LassoBeta', 'LassoN')
  outData[,2] <- abs(outData[,2])
  outData[,5] <- abs(outData[,5])
  outData <- scale(outData)[1:72,]
  return(outData)
}

returnAllOutMR <- function(genderVal, modalityVal, genderName){
  # Declare all of the csv names
  stepN <- paste(genderVal, 'OutSelectVals.csv', sep='')
  stepBeta <- paste(genderVal, 'OutBetaVals.csv', sep='')
  rfImportance <- paste(genderName, 'RFImportanceModalReg.csv', sep='')
  rfImportance2 <- paste(genderName, 'RFImportanceModalReg2.csv', sep='')
  lasBeta <- paste('all', genderName, 'BetaMR.csv', sep='')
  lasSel <- paste('all', genderName, 'SelectNMR.csv', sep='')
  nameVals <- c(stepN, stepBeta, rfImportance, rfImportance2, lasBeta, lasSel)
  # Now reach each csv and take out the correct column value
  outData <- matrix(NA, nrow=72, ncol=6)
  tmp <- read.csv(stepN)
  rownames(outData) <- tmp[,1]
  checkVals <- c(0,1,0,0,0,0)
  colVal <- c(1,1,1,1,2,2)
  q <- 1
  for( i in nameVals){
    tmp <- read.csv(i)
    colnames(tmp) <- tolower(colnames(tmp))
    modVal <- grep(modalityVal, colnames(tmp))
    if(checkVals[q] == 1){
      if(length(modVal) > 1){
        modVal <- modVal[2]
      }
    }
    outCol <- tmp[match(rownames(outData), tmp[,colVal[q]]),modVal[1]]
    outData[,q] <- outCol
    q <- q + 1
  }
  # Now find any NA mistakes by finding the modeal value across the row
  # If it is NA then set the entire row to NA
  for(i in seq(1,72)){
    modeVal <- Mode(outData[i,])
    if(is.na(modeVal)){
      outData[i,] <-rep(NA, 6) 
    }
  }
  colnames(outData) <- c('AustinN', 'AllBeta', 'RfMSE', 'RfPurity', 'LassoBeta', 'LassoN')
  outData[,2] <- abs(outData[,2])
  outData[,5] <- abs(outData[,5])
  outData <- scale(outData)[1:72,]
  return(outData)
}

# Now create a rough function to create violin plots from the z score data
playTheViolin <- function(zScoreVals, xValue){
  plotData <- melt(zScoreVals)
  plotData[plotData=='NaN'] <- NA
  plotData <- plotData[complete.cases(plotData),]
  orderVals <- summarySE(plotData, measurevar='value', groupvars='Var1', na.rm=T)
  orderVals <- orderVals[order(orderVals$value, decreasing=T),1]
  plotData$Var1 <- factor(plotData$Var1, levels=orderVals)
  # Now plot the data
  outPlot <- ggplot(plotData, aes(x=Var1, y=value)) +
    geom_violin() + 
    stat_summary(fun.y=mean, geom="point", shape=23) + 
    stat_summary(fun.y=median, geom="point", size=2, color="red") + 
    theme(axis.text.x=element_text(angle=90)) + 
    labs(y="z score", x=xValue)
  return(outPlot)
}

playTheViola <- function(zScoreVals, xValue){
  plotData <- zScoreVals
  plotData[plotData=='NaN'] <- NA
  plotData <- plotData[complete.cases(plotData),]
  orderVals <- summarySE(plotData, measurevar='value', groupvars='Var1', na.rm=T)
  orderVals <- orderVals[order(orderVals$value, decreasing=T),1]
  plotData$Var1 <- factor(plotData$Var1, levels=orderVals)
  # Now plot the data
  outPlot <- ggplot(plotData, aes(x=Var1, y=value)) +
    geom_violin() + 
    stat_summary(fun.y=mean, geom="point", shape=23) + 
    stat_summary(fun.y=median, geom="point", size=2, color="red") + 
    theme(axis.text.x=element_text(angle=90)) + 
    labs(y="z score", x=xValue)
  return(outPlot)
}

# Now create a function which will bootstrap lm 100 times and return 
# the mean beta weight - must be able to return weights from coefficent function
bootStrapBetaWeight <- function(y, x, iterationCount=100, nCor=3){
  # Now set up our parallel environment
  cl <- makeCluster(nCor)
  registerDoParallel(cl)

  # Bootstrap forward p value step wise model selection
  output <- foreach(i=seq(1,iterationCount), .combine='cbind') %dopar% {
      # First load required library(s)
      source('/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R')
      source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
      install_load('caret', 'SignifReg')
      
      # Create some static data points
      dataToUse <- as.data.frame(x)
      dataToUse$y <- y

      # create our model
      index <- createDataPartition(y,list=F)
      dataTrain <- dataToUse[index,]
      modOut <-  lm(y~., data=dataTrain)
      
      # return the output
      coefficients(modOut)
  }
  
  # Kill our cluster
  stopCluster(cl)
  outputBeta <- apply(output, 1, mean)
  # Now run a regression to cretae an output variable
  output <- lm(y ~ x)  
  output$coefficients <- outputBeta  

  # Now return the output binary matrix
  return(output) 
}

## I am going to try and build a function which wil do this across each of the folds
### Steps involved in this function include:
###	1.) Creaeting 10 folds to train w/in (caret: createFolds)
###	2.) Train a cv.glmnet w/in the training fold **Tune the lmabda using cv.glmnet**
###		Then train the model using glmnet
###	3.) Get the fit statistics in the left out data set - this is what we return
###	4.) Run 1-3 for each fold - find the average of the fit stats 
### This function should return a crossvallidated fit value for each of the input data set 
runRidgeOnAll <- function(x, y, nFold=10, lambdaSeq=10^seq(3, -2, by = -.1), rmLmSummary=TRUE, outcome="gaussian"){
  # The first thing we have to do is split our data into 10 folds
  folds <- createFolds(y, k=nFold, list=T, returnTrain=T)
  
  # Now declare the output variables
  outputCvValsR <- rep(NA, length(y))
  outputCvBeta <- matrix(0, nrow=dim(x)[2], ncol=10)
  outputCvValsL <- rep(NA, length(y))  
  trainSeq <- seq(1, length(y))
  # Now we need to loop thorugh each fold and get our output fit stats
  for(i in 1:nFold){
    index <- unlist(folds[[i]])
    trainX <- as.matrix(x)[index,]
    trainY <- as.vector(y)[index]
    testX <- as.matrix(x)[-index,]
    # Now grab our lambda to use 
    fit.cv <- cv.glmnet(x=trainX, y=trainY, alpha=0, lambda=lambdaSeq, nfolds=10, family=outcome)
    lambdaVal <- fit.cv$lambda[which(fit.cv$cvm==min(fit.cv$cvm))]
    modelFit <- glmnet(x=trainX, y=trainY, alpha=0, lambda=lambdaVal, family=outcome)

    # Now get our prediction values in the test values
    outputCvValsR[trainSeq[-index]] <- predict(modelFit, testX)
    outputCvBeta[,i] <- coef(modelFit)[-1]

    # Now do the same with linear regression
    tmpDF <- as.data.frame(cbind(trainY, trainX))

    # Check to see if summary metric flag is set to T
    if(rmLmSummary){
      grepVals <- c('Mean', 'ICV')
      valsToRm <- NULL
      for(nameVal in grepVals){
        grepOut <- grep(nameVal, names(tmpDF))
        valsToRm <- append(valsToRm, grepOut)
      }
      tmpDF <- tmpDF[,-valsToRm]
    }
    lmMod <- lm(trainY~., data=tmpDF)
    # Now get the predictide values
    outputCvValsL[trainSeq[-index]] <- predict(lmMod, newdata=as.data.frame(testX))
  }

  # Now return the output CvVals
  output <- list()
  output[[1]] <- outputCvValsR
  output[[2]] <- outputCvValsL
  output[[3]] <- outputCvBeta
  return(output)
}

# Now write a function to run through the austin n values and build a model
# based on the n hierarchy. 
# The inputs will be: 1. the austin n values 2. the the x values 3. the y values to predict
buildAustinModel <- function(austinValues, predVals, outVals, addSummary=TRUE, breakValue=.1, returnStepVals=FALSE, nIters=100, stepSize=1){
  # First organize the austin values
  selectionN <- returnSelectionCol(austinValues)
  #selectionN[,1] <- rownames(selectionN)
  selectionN <- selectionN[order(as.numeric(selectionN[,2]), decreasing=T),]

  # Now get the model order
  modelOrder <- selectionN[,1]
  
  # Get our static value
  staticValue <- grep('ICV', colnames(predVals))
  staticValue <- append(staticValue, grep('Mean', colnames(predVals)))

  # Now initialize a model
  initModel <- runRidgeOnAll(x = predVals[,c(staticValue, which(colnames(predVals) %in% modelOrder[1]))], y = outVals)
  initValue <- cor(initModel[[1]], outVals)^2

  # Now run the model building procedure through a while loop
  # Now get some static variables for the chi squared computation
  n <- dim(predVals)[1]
  modelStep <- 2
  pValue <- 0 
  cvValueOut <- NULL
  modelValueOut <- paste(colnames(predVals[,c(staticValue, which(colnames(predVals) %in% modelOrder[1]))]), collapse='+')
  oldP <- 2
  while(pValue < breakValue){
    # First get our model input values
    modelValues <- modelOrder[1:modelStep]  
    
    # Now build our model
    newModel <- runRidgeOnAll(x = predVals[,c(staticValue, which(colnames(predVals) %in% modelValues))], y = outVals)
    newValue <- cor(newModel[[1]], outVals)^2

    # Now get the length of p
    newP <- length(c(staticValue, which(colnames(predVals) %in% modelValues)))

    # Now compute the F statistic
    diffInR <- newValue - initValue
    denominatorForDiffInR <- oldP - newP
    distanceFromOne <- 1 - newValue
    degreeFree <- n - length(modelValues) 
    denomValue <- distanceFromOne / degreeFree
    fValue <- diffInR / denomValue
    pValue <- pf(fValue, 1, degreeFree, lower.tail=F)
    print(newValue)
    #print(pValue)
  
    # If we have gotten this far we are still building our model
    # Now we need to export the new model to the old variable
    cvValueOut <- append(cvValueOut, newValue)
    modelValueOut <- rbind(modelValueOut,  paste(colnames(predVals[,c(staticValue, which(colnames(predVals) %in% modelValues))]), collapse='+'))
    initModel <- newModel
    initValue <- newValue
    modelStep <- modelStep + stepSize
    oldP <- length(c(staticValue, which(colnames(predVals) %in% modelValues)))

    # Now check for a forced break
    if(modelStep > nIters){
      break
    }
  }

  # Now return all of the output!
  outVal1 <- modelStep - 1
  outVal2 <- n
  outVal3 <- paste(modelValues[1:(length(modelValues)-stepSize)], collapse='+')
  outVal4 <- max(cvValueOut)

  # Now Export the values!
  output <- list()
  output[[1]] <- cbind(outVal1, outVal2, outVal3, outVal4)
  output[[2]] <- cvValueOut
  output[[3]] <- modelValueOut
  return(output)
}


# Now build a model to run tpot output
runTpotModel <- function(inputData, inputGender){
  inputData <- inputData[which(inputData$sex==inputGender),]
  dataI <- inputData[,c(grep('F1_Exec_Comp_Cog_Accuracy', names(inputData)), grep('_jlf_', names(inputData)))]

  # Now split into train test
  index <- createFolds(dataI$F1_Exec_Comp_Cog_Accuracy, 5)$Fold1
  dataTest <- dataI[index,]
  dataI <- dataI[-index,]

  # Now make sure we only have complete data 
  dataI <- dataI[complete.cases(dataI),]
  dataTest <- dataTest[complete.cases(dataTest),]

  # Now run Austin
  selectVals <- returnSelectionN(inputData[index,], grepID='_jlf_', genderID=inputGender, nCor=3, iterationCount=100)
  valsToUse <- which(as.numeric(returnSelectionCol(selectVals)[,2]) > 24)
  nameVals <- rownames(selectVals)[valsToUse]
  colVals <- which(colnames(dataI) %in% nameVals)
  dataI <- dataI[c(1, colVals)]
  dataTest <- dataTest[,c(1, colVals)]

  # Now build a regression tree
  tree1 <- rpart(F1_Exec_Comp_Cog_Accuracy~.,method='anova',data=dataI,control=rpart.control(minsplit=10,maxdepth=1))
  dataI$outVal <- predict(tree1)
  dataTest$outVal <- predict(tree1, dataTest)

  # Now scale the values
  dataI <- scale(dataI)
  dataTest <- scale(dataTest)  

  # Now build an enet model 
  tmpVal <- cv.glmnet(y=dataI[,1], x=dataI[,2:dim(dataI)[2]], alpha=.55)
  finalMod <- glmnet(y=dataI[,1], x=dataI[,2:dim(dataI)[2]],alpha=.55,lambda=tmpVal$lambda.min)

  # Now get some fit metrics 
  outTrainR2 <- cor(predict(finalMod, dataI[,2:dim(dataI)[2]]), dataI[,1], use='complete')^2
  outTestR2 <- cor(predict(finalMod, dataTest[,2:dim(dataTest)[2]]), dataTest[,1], use='complete')^2

  # Now return the out val
  outVal <- cbind(outTrainR2, outTestR2, dim(dataI)[2], dim(dataTest)[1])
  return(outVal)
}
