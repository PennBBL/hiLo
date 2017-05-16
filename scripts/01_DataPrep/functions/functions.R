# Functions for the data prep script
# This first function is going to average left and right hemisphere data
# the basis for this function function came from "/home/adrose/corMatrixAcrossModalitiesByLobe/scripts"
# although I may alter it... not sure at the moment 
averageLeftAndRight <- function(dataFrame){
  # Now get the right data
  dataFrame.right <- dataFrame[, grep('_R_', names(dataFrame))]
  dataFrame.tmp <- dataFrame[, -grep('_R_', names(dataFrame))]
  if(!identical(integer(0),grep('_right', names(dataFrame.tmp)))){
  dataFrame.right <- cbind(dataFrame.right, dataFrame.tmp[, grep('_right', names(dataFrame.tmp))])
  dataFrame.tmp <- dataFrame.tmp[,-grep('_right', names(dataFrame.tmp))]
  }
  if(!identical(integer(0),grep('_rh_', names(dataFrame.tmp)))){
  dataFrame.right <- cbind(dataFrame.right, dataFrame.tmp[, grep('_rh_', names(dataFrame.tmp))])
  dataFrame.tmp <- dataFrame.tmp[,-grep('_rh_', names(dataFrame.tmp))]
  }  
  if(dim(dataFrame.tmp)[2] == 0){
  dataFrame.tmp <- dataFrame
  dataFrame.right <- dataFrame.tmp[, grep('_rh_', names(dataFrame.tmp))]
  dataFrame.tmp <- dataFrame.tmp[,-grep('_rh_', names(dataFrame.tmp))]
  }  
  # First do the left data
  dataFrame.left <- dataFrame.tmp[, grep('_L_', names(dataFrame.tmp))]
  dataFrame.tmp <- dataFrame.tmp[, -grep('_L_', names(dataFrame.tmp))]
  if(!identical(integer(0),grep('_left', names(dataFrame.tmp)))){
  dataFrame.left <- cbind(dataFrame.left, dataFrame.tmp[, grep('_left', names(dataFrame.tmp))])
  dataFrame.tmp <- dataFrame.tmp[,-grep('_left', names(dataFrame.tmp))]
  }
  if(!identical(integer(0),grep('_lh_', names(dataFrame.tmp)))){
  dataFrame.left <- cbind(dataFrame.left, dataFrame.tmp[, grep('_lh_', names(dataFrame.tmp))])
  dataFrame.tmp <- dataFrame.tmp[,-grep('_lh_', names(dataFrame.tmp))]
  }
  if(dim(dataFrame.tmp)[2] == 0){
  dataFrame.tmp <- dataFrame
  dataFrame.left <- dataFrame.tmp[, grep('_lh', names(dataFrame.tmp))]
  dataFrame.tmp <- dataFrame.tmp[,-grep('_lh_', names(dataFrame.tmp))]
  dataFrame.tmp <- dataFrame.tmp[,-grep('_rh_', names(dataFrame.tmp))]
  } 
  # Now combine the data frames
  dataFrame.meaned <- (dataFrame.left + dataFrame.right)/2

  # Now remove the left and right indeices from the names of the meaned data frame
  colnames(dataFrame.meaned) <- gsub(pattern='_L_', replacement = '_', x = colnames(dataFrame.meaned), fixed = TRUE)
  colnames(dataFrame.meaned) <- gsub(pattern='_R_', replacement = '_', x = colnames(dataFrame.meaned), fixed = TRUE)
  colnames(dataFrame.meaned) <- gsub(pattern='_left', replacement = '', x = colnames(dataFrame.meaned), fixed = TRUE)
  colnames(dataFrame.meaned) <- gsub(pattern='_right', replacement = '', x = colnames(dataFrame.meaned), fixed = TRUE)
  colnames(dataFrame.meaned) <- gsub(pattern='_rh_', replacement = '_', x = colnames(dataFrame.meaned), fixed = TRUE)
  colnames(dataFrame.meaned) <- gsub(pattern='_lh_', replacement = '_', x = colnames(dataFrame.meaned), fixed = TRUE)
  # Now rm the left and right values and append the meaned values
  indexToRm <- grep('_L_', names(dataFrame))
  indexToRm <- append(indexToRm, grep('_R_', names(dataFrame)))
  indexToRm <- append(indexToRm, grep('_left', names(dataFrame)))
  indexToRm <- append(indexToRm, grep('_right', names(dataFrame)))
  indexToRm <- append(indexToRm, grep('_rh_', names(dataFrame)))
  indexToRm <- append(indexToRm, grep('_lh_', names(dataFrame)))
  # Now prep the output 
  output <- dataFrame[,-indexToRm]
  # Now combine our average values
  output <- cbind(output, dataFrame.meaned)
  # Now return the output
  return(output)
}


# Now I need to make a function which will regress out age from a column
regressOutAge <- function(valuesToBeRegressed, ageColumn, qualityColumn){
  age <- scale(ageColumn)[,1]
  #ageSqu <- scale(ageColumn)[,1]^2
  #ageCub <- scale(ageColumn)[,1]^3
  quality <- qualityColumn
  newValues <- lm(valuesToBeRegressed ~ age + quality)$residuals#+ ageSqu + ageCub + quality)$residuals
  return(newValues)
}

# Now I need to create a function which regresses out data quality metrics
regressOutQuality <- function(dataFrame, modalityName, qualityName){
  # DEclare a littl function to get the residuals
  miniFunc <- function(valsColumn, qualityColumn){
    residuals(lm(valsColumn ~ qualityColumn))
  }
  valsToLoop <- grep(modalityName, names(dataFrame))
  qualityColumn <- grep(qualityName, names(dataFrame))
  if (length(qualityColumn) > 1){ qualityColumn <- qualityColumn[1] }
  output <- dataFrame 
  output[,valsToLoop] <- apply(output[,valsToLoop], 2, function(x) miniFunc(x, output[,qualityColumn]))
  return(output)
}

# I need to make a quick function which will take a value of interest
# and a list of grep outputs, I will then try to find the best match using gsub
# should be quick and dirty 
# **** THIS FUNCTION WILL ONLY WORK CORRECTLY AND ACCURATLEY****
# **** IF THE PREFIX AND SUFFIX ARE THE SAME FOR EACH INPUT STRING*****
findBestCharacterMatch <- function(valOfInterest, valuesOutcome){
  foo<-gsub(pattern=valOfInterest, replacement='', x=valuesOutcome)
  # Now we need to find the minimum value - with the assumption that  we 
  # will rm the most elements for the pattern that matchs ours 
  minValue <- min(nchar(foo))
  # Now find which one matchs the min value and return that 
  output <- which(nchar(foo)==minValue)  
}

# Now I need to make a function which will regress out volume from non volume data
regressOutVolume <- function(volumeDataFrame, modalityOfInterestDataFrame, grepPattern){
  # First thing we need to do is find the correct volume column
  # to regress our data with
  valuesToGrep <- names(modalityOfInterestDataFrame)[grep(grepPattern, names(modalityOfInterestDataFrame))]
  justROINames <- sub(pattern=paste(grepPattern, '_', sep=''), replacement='', x=valuesToGrep, fixed=TRUE)


  output.values <- as.data.frame(cbind(modalityOfInterestDataFrame$bblid, 
                   modalityOfInterestDataFrame$scanid))

  # Now we have the names we need to go through each ROI name and find the matching pairs
  # run the lm and output the residuals
  for(roi.name in justROINames){
    volume.values.col <- grep(roi.name, names(volumeDataFrame), fixed=TRUE)
    modality.values.col <- grep(roi.name, names(modalityOfInterestDataFrame), fixed=TRUE)
    if(length(volume.values.col) > 1){
      tmp <- findBestCharacterMatch(roi.name,names(volumeDataFrame)[volume.values.col])
      volume.values.col <- volume.values.col[tmp]
    }   
    if(length(modality.values.col) > 1){
      tmp <- findBestCharacterMatch(roi.name,names(modalityOfInterestDataFrame)[modality.values.col])
      modality.values.col <- modality.values.col[tmp]
    }  

    # Now print some feedback so I know things are working correctly
    print(paste("We will now regress ", 
          names(modalityOfInterestDataFrame)[modality.values.col], 
          " with ",
          names(volumeDataFrame)[volume.values.col],
          sep=''))

    ## Now I need to make sure that our elements have the same length
    ## This is going to be super effing dumb but it will work
    modalityData <- as.data.frame(cbind(modalityOfInterestDataFrame$bblid, 
                          modalityOfInterestDataFrame$scanid, 
                          modalityOfInterestDataFrame[,modality.values.col]))
    volumeData <- as.data.frame(cbind(volumeDataFrame$bblid,
                        volumeDataFrame$scanid,
                        volumeDataFrame[,volume.values.col]))
    dataOfInterest <- merge(modalityData, volumeData, by=c('V1', 'V2'))
  

    # Now we can regress out the values I think!
    tmp.values <- lm(dataOfInterest[,3] ~ dataOfInterest[,4])$residuals
    if(length(tmp.values) != nrow(dataOfInterest)){
      print(roi.name)
    }

    # Now create a tmp data frame to use to combine the output with missing 
    # values 
    bblid.index <- dataOfInterest[complete.cases(dataOfInterest[,3]),1]
    toAdd <- rep(NA, dim(dataOfInterest)[1])
    toAdd[match(bblid.index, output.values[,1])] <- tmp.values[match(bblid.index, output.values[,1])]

    # Now attach this to the output
    output.values <- cbind(output.values, toAdd)
  }

  # prepare the output
  output.data.frame <- modalityOfInterestDataFrame
  columnsToOverWrite <- grep(grepPattern, names(output.data.frame))
  output.data.frame[,columnsToOverWrite] <- output.values[,3:ncol(output.values)]
  return(output.data.frame)
}

# Now I need to make a function which will regress out volume from non volume data
regressOutVolumeAndAge <- function(volumeDataFrame, modalityOfInterestDataFrame, grepPattern){
  # First thing we need to do is find the correct volume column
  # to regress our data with
  valuesToGrep <- names(modalityOfInterestDataFrame)[grep(grepPattern, names(modalityOfInterestDataFrame))]
  justROINames <- sub(pattern=paste(grepPattern, '_', sep=''), replacement='', x=valuesToGrep, fixed=TRUE)

  output.values <- as.data.frame(cbind(modalityOfInterestDataFrame$bblid, 
                   modalityOfInterestDataFrame$scanid))

  # Now we have the names we need to go through each ROI name and find the matching pairs
  # run the lm and output the residuals
  for(roi.name in justROINames){
    volume.values.col <- grep(roi.name, names(volumeDataFrame), fixed=TRUE)
    modality.values.col <- grep(roi.name, names(modalityOfInterestDataFrame), fixed=TRUE)
    if(length(volume.values.col) > 1){
      tmp <- findBestCharacterMatch(roi.name,names(volumeDataFrame)[volume.values.col])
      volume.values.col <- volume.values.col[tmp]
    }   
    if(length(modality.values.col) > 1){
      tmp <- findBestCharacterMatch(roi.name,names(modalityOfInterestDataFrame)[modality.values.col])
      modality.values.col <- modality.values.col[tmp]
    }  

    # Now print some feedback so I know things are working correctly
    print(paste("We will now regress ", 
          names(modalityOfInterestDataFrame)[modality.values.col], 
          " with ",
          names(volumeDataFrame)[volume.values.col],
          sep=''))

    ## Now I need to make sure that our elements have the same length
    ## This is going to be super effing dumb but it will work
    modalityData <- as.data.frame(cbind(modalityOfInterestDataFrame$bblid, 
                          modalityOfInterestDataFrame$scanid, 
                          scale(modalityOfInterestDataFrame$ageAtGo1Scan)[,1],
			  scale(modalityOfInterestDataFrame$ageAtGo1Scan)[,1]^2,
                          scale(modalityOfInterestDataFrame$ageAtGo1Scan)[,1]^3,
                          modalityOfInterestDataFrame[,modality.values.col]))
    colnames(modalityData) <- c('bblid', 'scanid', 'age', 'agesq', 'agecub', 'modalityCol')
    modalityData <- apply(modalityData, 2, function(x) as.numeric(as.character(x)))
    volumeData <- as.data.frame(cbind(volumeDataFrame$bblid,
                        volumeDataFrame$scanid,
                        volumeDataFrame[,volume.values.col]))
    colnames(volumeData) <- c('bblid', 'scanid', 'volumeCol')
    dataOfInterest <- merge(modalityData, volumeData, by=c('bblid', 'scanid'))
  

    # Now we can regress out the values I think!
    
    tmp.values <- lm(modalityCol ~ volumeCol + age
                                          + agesq + agecub, data=dataOfInterest)$residuals
    if(length(tmp.values) != nrow(dataOfInterest)){
      print(roi.name)
    }

    # Now create a tmp data frame to use to combine the output with missing 
    # values 
    bblid.index <- dataOfInterest[complete.cases(dataOfInterest[,3]),1]
    toAdd <- rep(NA, dim(dataOfInterest)[1])
    toAdd[match(bblid.index, output.values[,1])] <- tmp.values[match(bblid.index, output.values[,1])]

    # Now attach this to the output
    output.values <- cbind(output.values, toAdd)
  }

  # prepare the output
  output.data.frame <- modalityOfInterestDataFrame
  columnsToOverWrite <- grep(grepPattern, names(output.data.frame))
  output.data.frame[,columnsToOverWrite] <- output.values[,3:ncol(output.values)]
  return(output.data.frame)
}




# Now create a function which will regress within modality
# I am going to limit this analysis to only fully complete data sets
# It has to bee this way because lm performs list wise deletion 
regressWithinModality <- function(dataFrameToRegress, grepPattern){
  # Make sure our input data is a data frame
  dataFrameToRegress <- as.data.frame(dataFrameToRegress)
  # First create a temporary dataFrame limited to only our values of interest
  colsOfInterest <- grep(grepPattern, names(dataFrameToRegress))
  toWorkWith <- dataFrameToRegress[complete.cases(dataFrameToRegress[,colsOfInterest]), colsOfInterest]
  
  # Now I will use apply to create the regressed data frame values
  #toWorkWithOutput <- apply(toWorkWith, 2, function(x) lm(x ~ . ,data=toWorkWith)$residuals)
  toWorkWithOutput <- NULL

  # Now loop thorugh each column and regress within modality
  for(i in seq(1, dim(toWorkWith)[2])){
    tmpFormula <- as.formula(paste(names(toWorkWith)[i], '~.', sep=''))
    newValues <- lm(tmpFormula, data=toWorkWith)$residuals
    toWorkWithOutput <- cbind(toWorkWithOutput, newValues)
  }
  colnames(toWorkWithOutput) <- names(dataFrameToRegress)[colsOfInterest]
  # Now prepare our output
  outputDataFrame <- dataFrameToRegress[complete.cases(dataFrameToRegress[,colsOfInterest]),]
  outputDataFrame[, colsOfInterest] <- toWorkWithOutput
  return(outputDataFrame)
}

# Now create a function to regress out TBV from all of the DTI data
regressOutTBV <- function(valuesToBeRegressed, tbvCol) {
output <- rep(NA, length(valuesToBeRegressed))
newValues <- unname(lm(valuesToBeRegressed ~ tbvCol)$residuals)
newValuesIndex <- as.numeric(names(lm(valuesToBeRegressed ~ tbvCol)$residuals))
output[newValuesIndex] <- newValues
return(output)
}



averageLeftAndRight1 <- function(dataFrame) {
colnames(dataFrame) <- gsub(pattern='dti_dtitk_jhulabel_rd', x=colnames(dataFrame), replacement='')
colnames(dataFrame) <- gsub(pattern='dti_dtitk_jhutracts_rd', x=colnames(dataFrame), replacement='')
dataFrame.right <- dataFrame[, grep("_R_", names(dataFrame))]
dataFrame.tmp <- dataFrame[, -grep("_R_", names(dataFrame))]
if (!identical(integer(0), grep("_right", names(dataFrame.tmp)))) {

dataFrame.right <- cbind(dataFrame.right, dataFrame.tmp[, 


grep("_right", names(dataFrame.tmp))])

dataFrame.tmp <- dataFrame.tmp[, -grep("_right", names(dataFrame.tmp))]

}
if (!identical(integer(0), grep("_rh_", names(dataFrame.tmp)))) {

dataFrame.right <- cbind(dataFrame.right, dataFrame.tmp[, 


grep("_rh_", names(dataFrame.tmp))])

dataFrame.tmp <- dataFrame.tmp[, -grep("_rh_", names(dataFrame.tmp))]

}
if (dim(dataFrame.tmp)[2] == 0) {

dataFrame.tmp <- dataFrame

dataFrame.right <- dataFrame.tmp[, grep("_rh_", names(dataFrame.tmp))]

dataFrame.tmp <- dataFrame.tmp[, -grep("_rh_", names(dataFrame.tmp))]

}
if (dim(dataFrame.tmp)[2] == 0) {

dataFrame.tmp <- dataFrame

dataFrame.right <- dataFrame.tmp[, grep("_r$", names(dataFrame.tmp))]

dataFrame.tmp <- dataFrame.tmp[, -grep("_r$", names(dataFrame.tmp))]

}
dataFrame.left <- dataFrame.tmp[, grep("_L_", names(dataFrame.tmp))]
dataFrame.tmp <- dataFrame.tmp[, -grep("_L_", names(dataFrame.tmp))]
if (!identical(integer(0), grep("_left", names(dataFrame.tmp)))) {

dataFrame.left <- cbind(dataFrame.left, dataFrame.tmp[, 


grep("_left", names(dataFrame.tmp))])

dataFrame.tmp <- dataFrame.tmp[, -grep("_left", names(dataFrame.tmp))]

}
if (!identical(integer(0), grep("_lh_", names(dataFrame.tmp)))) {

dataFrame.left <- cbind(dataFrame.left, dataFrame.tmp[, 


grep("_lh_", names(dataFrame.tmp))])

dataFrame.tmp <- dataFrame.tmp[, -grep("_lh_", names(dataFrame.tmp))]

}
if (dim(dataFrame.tmp)[2] == 0) {

dataFrame.tmp <- dataFrame

dataFrame.left <- dataFrame.tmp[, grep("_lh", names(dataFrame.tmp))]

dataFrame.tmp <- dataFrame.tmp[, -grep("_lh_", names(dataFrame.tmp))]

dataFrame.tmp <- dataFrame.tmp[, -grep("_rh_", names(dataFrame.tmp))]

}
if (dim(dataFrame.tmp)[2] == 0) {

dataFrame.tmp <- dataFrame

dataFrame.left <- dataFrame.tmp[, grep("_l$", names(dataFrame.tmp))]

dataFrame.tmp <- dataFrame.tmp[, -grep("_l$", names(dataFrame.tmp))]

dataFrame.tmp <- dataFrame.tmp[, -grep("_r", names(dataFrame.tmp))]

}
dataFrame.meaned <- (dataFrame.left + dataFrame.right)/2
colnames(dataFrame.meaned) <- gsub(pattern = "_L_", replacement = "_", 

x = colnames(dataFrame.meaned), fixed = TRUE)
colnames(dataFrame.meaned) <- gsub(pattern = "_R_", replacement = "_", 

x = colnames(dataFrame.meaned), fixed = TRUE)
colnames(dataFrame.meaned) <- gsub(pattern = "_left", replacement = "", 

x = colnames(dataFrame.meaned), fixed = TRUE)
colnames(dataFrame.meaned) <- gsub(pattern = "_right", replacement = "", 

x = colnames(dataFrame.meaned), fixed = TRUE)
colnames(dataFrame.meaned) <- gsub(pattern = "_rh_", replacement = "_", 

x = colnames(dataFrame.meaned), fixed = TRUE)
colnames(dataFrame.meaned) <- gsub(pattern = "_lh_", replacement = "_", 

x = colnames(dataFrame.meaned), fixed = TRUE)
colnames(dataFrame.meaned) <- gsub(pattern = "_r", replacement = "", 

x = colnames(dataFrame.meaned), fixed = TRUE)
colnames(dataFrame.meaned) <- gsub(pattern = "_l", replacement = "", 

x = colnames(dataFrame.meaned), fixed = TRUE)
indexToRm <- grep("_L_", names(dataFrame))
indexToRm <- append(indexToRm, grep("_R_", names(dataFrame)))
indexToRm <- append(indexToRm, grep("_left", names(dataFrame)))
indexToRm <- append(indexToRm, grep("_right", names(dataFrame)))
indexToRm <- append(indexToRm, grep("_rh_", names(dataFrame)))
indexToRm <- append(indexToRm, grep("_lh_", names(dataFrame)))
indexToRm <- append(indexToRm, grep("_r", names(dataFrame)))
indexToRm <- append(indexToRm, grep("_l", names(dataFrame)))
output <- dataFrame[, -indexToRm]
output <- cbind(output, dataFrame.meaned)
return(output)
}


