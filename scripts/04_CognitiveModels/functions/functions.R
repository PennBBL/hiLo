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

model.select <- function(model,keep,sig=0.05,verbose=F, data=NULL){
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
      folds <- createDataPartition(dataToUse$V1,list=T)
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

returnSelection <- function(dataFrame, genderID, grepID, pValue=.05, iterationCount=1000, nCor=3, selectionPercent=.75, returnBetas=TRUE){
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
      folds <- createFolds(dataToUse$V1, k=4, list=T, returnTrain=T)
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
      folds <- createFolds(y, k=4, list=T, returnTrain=T)
      index <- unlist(folds[1])
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
