# Declare a function to bn the performance groups 
returnPerfBin <- function(data) {
  
  data$F1_Exec_Comp_Cog_Accuracy
  quantiles <- quantile(data$F1_Exec_Comp_Cog_Accuracy, c(0,.34,.67,1))
  
  data$perfBin <- 0
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy < quantiles[2])] <- 3
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy >= quantiles[2] &
                          data$F1_Exec_Comp_Cog_Accuracy <= quantiles[3])] <- 1
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy > quantiles[3])] <- 1
  return(data)
}


# Now add our backward model selection function
# which will also need the check interaction function
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


## Helper functions for previous function
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

# Now build a binomial ridge reg model builder
runRidgeOnAllLogit <- function(x, y, nFold=10, lambdaSeq=10^seq(3, -2, by = -.1)){
  # The first thing we have to do is split our data into 10 folds
  folds <- createFolds(y, k=nFold, list=T, returnTrain=T)
  
  # Now declare the output variable
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
    fit.cv <- cv.glmnet(x=trainX, y=trainY, alpha=0, lambda=lambdaSeq, nfolds=10, family="binomial")
    lambdaVal <- fit.cv$lambda[which(fit.cv$cvm==min(fit.cv$cvm))]
    modelFit <- glmnet(x=trainX, y=trainY, alpha=0, lambda=lambdaVal, family="binomial")

    # Now get our prediction values in the test values
    outputCvValsR[trainSeq[-index]] <- predict(modelFit, testX, type='response')
    outputCvBeta[,i] <- coef(modelFit)[-1]

  }

  # Now return the output CvVals
  output <- list()
  output[[1]] <- outputCvValsR
  output[[2]] <- outputCvBeta
  return(output)
}

# Now I need to create a function to run the austin algorithim 
returnSelectionN <- function(dataFrame, genderID, grepID, pValue=.05, iterationCount=1000, nCor=3, dir='backward'){
  # Prepare our data
  isolatedGender <- dataFrame[which(dataFrame$sex==genderID),]
  colsOfInterest <- grep(grepID, names(isolatedGender))
  valuesToUse <- scale(isolatedGender[,colsOfInterest])[,1:length(colsOfInterest)]
  outcomeVal <- scale(isolatedGender$perfBin)
  dataToUse <- as.data.frame(cbind(outcomeVal, valuesToUse))
  dataToUse <- dataToUse[complete.cases(dataToUse),]
  
  # Now I need to remove any cols with summary metrics
  colsToRm <- grep('Mean', colnames(dataToUse))
  colsToRm <- append(grep('ICV', colnames(dataToUse)), colsToRm)
  dataToUse <- dataToUse[,-colsToRm]

  # Now set up our parallel environment
  cl <- makeCluster(nCor)
  registerDoParallel(cl)

  # Bootstrap forward p value step wise model selection
  output <- foreach(i=seq(1,iterationCount), .combine='cbind',.export=ls(envir=globalenv()),.errorhandling = 'remove') %dopar% {
      # First load required library(s)
      source('/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R')
      source('/home/adrose/hiLo/scripts/07_Classification/functions/functions.R')
      install_load('caret', 'SignifReg', 'MASS')
      
      # create our model
      folds <- createResample(dataToUse$V1,list=T)
      index <- unlist(folds[1])
      dataTrain <- dataToUse[index,]
      fitVals <- glm(V1~., data=dataTrain)
      stepVAR <- model.select(fitVals, verbose=T)
      
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



# Now build a model to run through each step and return a cv AUC value
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
  initModel <- runRidgeOnAllLogit(x = predVals[,c(staticValue, which(colnames(predVals) %in% modelOrder[1]))], y = outVals)
  initRoc <- roc(outVals ~ initModel[[1]])

  # Now run the model building procedure through a while loop
  # Now get some static variables for the chi squared computation
  modelStep <- 2
  pValue <- 0 
  cvValueOut <- NULL
  modelValueOut <- paste(colnames(predVals[,c(staticValue, which(colnames(predVals) %in% modelOrder[1]))]), collapse='+')
  while(pValue < breakValue){
    # First get our model input values
    modelValues <- modelOrder[1:modelStep]  
    
    # Now build our model
    newModel <- runRidgeOnAllLogit(x = predVals[,c(staticValue, which(colnames(predVals) %in% modelValues))], y = outVals)
    newRoc <- roc(outVals ~ newModel[[1]])
    newValue <- auc(newRoc)
    print(paste('Current Step:', modelStep, auc(newRoc)))
    # Now compute the delong test
    pValueAll <- roc.test(newRoc, initRoc, method='d')
    pValue <- pValueAll$p.value

    # If we have gotten this far we are still building our model
    # Now we need to export the new model to the old variable
    cvValueOut <- append(cvValueOut, newValue)
    modelValueOut <- rbind(modelValueOut,  paste(colnames(predVals[,c(staticValue, which(colnames(predVals) %in% modelValues))]), collapse='+'))
    initModel <- newModel
    initRoc <- newRoc
    modelStep <- modelStep + stepSize
    oldP <- length(c(staticValue, which(colnames(predVals) %in% modelValues)))

    # Now check for a forced break
    if(modelStep > nIters){
      break
    }
  }

  # Now return all of the output!
  outVal1 <- modelStep - 1
  outVal2 <- length(outVals)
  outVal3 <- paste(modelValues[1:(length(modelValues)-stepSize)], collapse='+')
  outVal4 <- max(cvValueOut)

  # Now Export the values!
  output <- list()
  output[[1]] <- cbind(outVal1, outVal2, outVal3, outVal4)
  output[[2]] <- cvValueOut
  output[[3]] <- modelValueOut
  return(output)
}

returnSelectionCol <- function(outputFromrunLasso){
  sumIndex <- rowSums(abs(sign(apply(outputFromrunLasso[,2:ncol(outputFromrunLasso)], 
              2, function(x) as.numeric(as.character(x))))))
  sumIndex <- cbind(rownames(outputFromrunLasso), sumIndex)
  return(sumIndex)
}
