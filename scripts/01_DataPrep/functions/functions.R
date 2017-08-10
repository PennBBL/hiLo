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

# Now make a function to regress out age but rm all quality indicies
regressOutAgeNoQA <- function(valuesToBeRegressed, ageColumn, qualityColumn){
  age <- scale(ageColumn)[,1]
  ageSqu <- scale(ageColumn)[,1]^2
  ageCub <- scale(ageColumn)[,1]^3
  newValues <- lm(valuesToBeRegressed ~ age + ageSqu + ageCub)$residuals#+ ageSqu + ageCub + quality)$residuals
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
  if(identical(NULL, dim(dataFrameToRegress))){
    return(dataFrameToRegress)
  }
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

# Declare a function which will return the performance bin
returnPercentileGroup <- function(groupLevel,df_row, tmp_df){
  quantileTmp <- quantile(df_row, na.rm=TRUE)
  if(groupLevel == "lo"){
    quantileTmplow <- getElement(quantileTmp,"0%")
    quantileTmphigh <- getElement(quantileTmp,"25%")
  }
  if(groupLevel == "me"){
    quantileTmplow <- getElement(quantileTmp,"25%")
    quantileTmphigh  <- getElement(quantileTmp,"75%")
  }
  if(groupLevel =="hi"){
    quantileTmplow <- getElement(quantileTmp,"75%")
    quantileTmphigh <- getElement(quantileTmp,"100%") + 1
  }
  df_to_return <- which(df_row >= quantileTmplow & df_row < quantileTmphigh)
  df_to_return  <- tmp_df[df_to_return,]
  df_to_return$groupFactorLevel <- groupLevel
  return(df_to_return)
}

# Declare a function which will return the global mean 
returnGlobalMean <- function(dataFrame,modalityColVal){
  toReturn <- mean(dataFrame[,modalityColVal], na.rm=TRUE)
  return(toReturn)
}

# Create a function which will return the global sd
returnGlobalSd <- function(dataFrame,modalityColVal){
  toReturn <- sd(dataFrame[,modalityColVal], na.rm=TRUE)
  return(toReturn)
}

# Declare a function which will find the lobe value
findLobe <- function(grepPattern){
  # Declare the rois that we will grep through
  rois<-c("Thal","Putamen","Caudate","Pallidum",  # Basal Ganglia
          "Accumbens", "BasFor", # Basal Ganglia
          "PHG","Hip","PIns","SCA","AIns", # Limbic
          "ACgG","PCgG","Ent","Amygdala","MCgG", # Limbic
          "FO","MFC","MOrG","POrG","OrIFG","TrIFG","AOrG","OpIFG","GRe", # Frontal Orbital
          "FRP", "LOrG", # Frontal Orbital
          "PrG","MSFG","SMC","MFG","SFG", # Frontal Dorsal
          "FuG","PT","PP","ITG","CO","MTG","TMP","STG","TTG", # Temporal
          "PCu","PoG","AnG","PO","SPL","MPrG", # Parietal
          "SMG","MPoG", # Parietal
          "IOG","Cun","LiG","OFuG","MOG","Calc","OCP","SOG", # Occiptal
          "Cerebellum_Exterior", "Cerebellar_Vermal_Lobules_I.V", "Cerebellar_Vermal_Lobules_VI.VII", "Cerebellar_Vermal_Lobules_VIII.X", # Cerebellum
          "Limbic_Lobe_WM", "Insular_Lobe_WM", "Frontal_Lobe_WM", # WM
          "Parietal_Lobe_WM", "Occipital_Lobe_WM", "Temporal_Lobe_WM")  # WM


  # Declare the index key which corresponds to lobe values
  index.key <- c(1,7,17,28,33,42,50,58,62,68)

  # Now find where the pattern matches to the roi list
  for(pattern.match.variable in 1:length(rois)){
    nuclei.to.grep <- rois[pattern.match.variable]
    grep.output <- grep(nuclei.to.grep ,grepPattern)    
    if(!identical(integer(0), grep.output)){
      break
    }
    if(pattern.match.variable==67){
      pattern.match.variable <- 68
    }
  }
  OFuGCheck <- grep("OFuG", grepPattern)
  if(!identical(integer(0), OFuGCheck)){
    pattern.match.variable <- 53 
  }
  lobe.group <- findInterval(pattern.match.variable, index.key)
  return(lobe.group)
}

findLobeWM <- function(grepPattern){
  # Declare the rois that we will grep through
  rois <-c("cgc","cgh","slf","ss","ec","fnx","fnx_st","sfo","uf", #Association Fiber
                 "gcc","bcc","scc","tap", #Commisseral Fiber
                 "alic","plic","ptr","scr","pcr","rlic","cp","acr", #Projection Fiber
                 "cst","pct","mel","icp","mcp","scp") #Brainstem
  # Declare the index key which corresponds to lobe values
  index.key <- c(1,10,14,22)

  # Now find where the pattern matches to the roi list
  for(pattern.match.variable in 1:length(rois)){
    nuclei.to.grep <- rois[pattern.match.variable]
    grep.output <- grep(nuclei.to.grep ,grepPattern)    
    if(!identical(integer(0), grep.output)){
      break
    }
    if(pattern.match.variable==57){
      pattern.match.variable <- 60
    }
  }
  lobe.group <- findInterval(pattern.match.variable, index.key)
  return(lobe.group)
}


addAgeBins <- function(ageValues, dataFrame, lowerAge, upperAge, ageBinName){
  ageValuesOfInterest <- which(ageValues > lowerAge & ageValues <= upperAge)
  dataFrameOfInterest <- dataFrame[ageValuesOfInterest,]  
  toAdd <- rep(ageBinName, length(ageValuesOfInterest))
  dataFrameOfInterest$ageBin <- toAdd
  return(dataFrameOfInterest)
}

addAgeBin <- function(df, ageColumn, youngUpper, middleUpper, olderLower){
  output.young <- addAgeBins(ageColumn, df, 0, youngUpper, 'Childhood')
  output.middle <- addAgeBins(ageColumn, df, youngUpper+1, middleUpper, 'Adolescence')
  output.older <- addAgeBins(ageColumn, df, middleUpper+1, 999, 'Early Adulthood')
  output <- rbind(output.young, output.middle, output.older)
  return(output)
}

outputLongFormat4way <- function(dataFrame, modalityName, ageBand){
  outputDataframe <- data.frame(bblid=character(), sex=character(), age_bin=numeric(),
                                lobe=numeric(), roi=character(), z_score=numeric(), perf_bin=character())
  col.of.interest <- grep(modalityName, names(dataFrame))
  #dataFrame <- dataFrame[complete.cases(dataFrame[,col.of.interest]),]
  sex.col <- grep("sex", names(dataFrame))
  bblid.col <- grep("bblid", names(dataFrame))[1]
  factor.col <- dataFrame$F1_Exec_Comp_Cog_Accuracy
  me.vals <- returnPercentileGroup('me', factor.col, dataFrame)
  hi.vals <- returnPercentileGroup('hi', factor.col, dataFrame)
  lo.vals <- returnPercentileGroup('lo', factor.col, dataFrame)
  for(temp.gender in c(1,2)){
    temp.me.vals <- me.vals[which(me.vals[,sex.col] == temp.gender & me.vals$ageBin == ageBand),]
    temp.lo.vals <- lo.vals[which(lo.vals[,sex.col] == temp.gender & lo.vals$ageBin == ageBand),]
    temp.hi.vals <- hi.vals[which(hi.vals[,sex.col] == temp.gender & hi.vals$ageBin == ageBand),]
    for(temp.col in col.of.interest){
      # Find the global values
      me.val.mean <- returnGlobalMean(me.vals, temp.col)
      me.val.sd <- returnGlobalSd(me.vals, temp.col)
      roi.name <- names(dataFrame)[temp.col]
      lobe.value <- findLobe(roi.name)
      
      # Now find the lo values
      lo.val.df.z.score <- (temp.lo.vals[,temp.col] - me.val.mean) / me.val.sd
      lo.val.df.bblid <- temp.lo.vals[,bblid.col]
      lo.val.df.sex <- rep(temp.gender, length(lo.val.df.z.score))
      lo.val.df.age.bin <- rep(ageBand, length(lo.val.df.z.score))
      lo.val.df.lobe <- rep(lobe.value, length(lo.val.df.z.score))
      lo.val.df.roi <- rep(roi.name, length(lo.val.df.z.score))
      lo.val.df.perf.bin <- rep('lo', length(lo.val.df.z.score))
      lo.val.df <- cbind(lo.val.df.bblid, lo.val.df.sex, lo.val.df.age.bin, lo.val.df.lobe,
                         lo.val.df.roi, lo.val.df.z.score, lo.val.df.perf.bin)
      colnames(lo.val.df) <- c("bblid", "sex", "age_bin", "lobe", "roi", "z_score", "perf_bin")
      outputDataframe <- rbind(outputDataframe, lo.val.df)

      # Now find the hi values
      hi.val.df.z.score <- (temp.hi.vals[,temp.col] - me.val.mean) / me.val.sd
      hi.val.df.bblid <- temp.hi.vals[,bblid.col]
      hi.val.df.sex <- rep(temp.gender, length(hi.val.df.z.score))
      hi.val.df.age.bin <- rep(ageBand, length(hi.val.df.z.score))
      hi.val.df.lobe <- rep(lobe.value, length(hi.val.df.z.score))
      hi.val.df.roi <- rep(roi.name, length(hi.val.df.z.score))
      hi.val.df.perf.bin <- rep('hi', length(hi.val.df.z.score))
      hi.val.df <- cbind(hi.val.df.bblid, hi.val.df.sex, hi.val.df.age.bin, hi.val.df.lobe,
                         hi.val.df.roi, hi.val.df.z.score, hi.val.df.perf.bin)
      colnames(hi.val.df) <- c("bblid", "sex", "age_bin", "lobe", "roi", "z_score", "perf_bin")
      outputDataframe <- rbind(outputDataframe, hi.val.df)
    }
  }
  return(outputDataframe)
}



# Now produce a function to run the long way format but no z scoring
outputLongFormat4wayNoZ <- function(dataFrame, modalityName, ageBand){
  outputDataframe <- data.frame(bblid=character(), sex=character(), age_bin=numeric(),
                                lobe=numeric(), roi=character(), z_score=numeric(), perf_bin=character())
  col.of.interest <- grep(modalityName, names(dataFrame))
  #dataFrame <- dataFrame[complete.cases(dataFrame[,col.of.interest]),]
  sex.col <- grep("sex", names(dataFrame))
  bblid.col <- grep("bblid", names(dataFrame))[1]
  factor.col <- dataFrame$F1_Exec_Comp_Cog_Accuracy
  me.vals <- returnPercentileGroup('me', factor.col, dataFrame)
  hi.vals <- returnPercentileGroup('hi', factor.col, dataFrame)
  lo.vals <- returnPercentileGroup('lo', factor.col, dataFrame)
  for(temp.gender in c(1,2)){
    temp.me.vals <- me.vals[which(me.vals[,sex.col] == temp.gender & me.vals$ageBin == ageBand),]
    temp.lo.vals <- lo.vals[which(lo.vals[,sex.col] == temp.gender & lo.vals$ageBin == ageBand),]
    temp.hi.vals <- hi.vals[which(hi.vals[,sex.col] == temp.gender & hi.vals$ageBin == ageBand),]
    for(temp.col in col.of.interest){
      # Find the global values
      me.val.mean <- 0
      me.val.sd <- 1
      roi.name <- names(dataFrame)[temp.col]
      lobe.value <- findLobe(roi.name)
      
      # Now find the lo values
      lo.val.df.z.score <- (temp.lo.vals[,temp.col] - me.val.mean) / me.val.sd
      lo.val.df.bblid <- temp.lo.vals[,bblid.col]
      lo.val.df.sex <- rep(temp.gender, length(lo.val.df.z.score))
      lo.val.df.age.bin <- rep(ageBand, length(lo.val.df.z.score))
      lo.val.df.lobe <- rep(lobe.value, length(lo.val.df.z.score))
      lo.val.df.roi <- rep(roi.name, length(lo.val.df.z.score))
      lo.val.df.perf.bin <- rep('lo', length(lo.val.df.z.score))
      lo.val.df <- cbind(lo.val.df.bblid, lo.val.df.sex, lo.val.df.age.bin, lo.val.df.lobe,
                         lo.val.df.roi, lo.val.df.z.score, lo.val.df.perf.bin)
      colnames(lo.val.df) <- c("bblid", "sex", "age_bin", "lobe", "roi", "z_score", "perf_bin")
      outputDataframe <- rbind(outputDataframe, lo.val.df)

      # Now find the me values
      me.val.df.z.score <- (temp.me.vals[,temp.col] - me.val.mean) / me.val.sd
      me.val.df.bblid <- temp.me.vals[,bblid.col]
      me.val.df.sex <- rep(temp.gender, length(me.val.df.z.score))
      me.val.df.age.bin <- rep(ageBand, length(me.val.df.z.score))
      me.val.df.lobe <- rep(lobe.value, length(me.val.df.z.score))
      me.val.df.roi <- rep(roi.name, length(me.val.df.z.score))
      me.val.df.perf.bin <- rep('me', length(me.val.df.z.score))
      me.val.df <- cbind(me.val.df.bblid, me.val.df.sex, me.val.df.age.bin, me.val.df.lobe,
                         me.val.df.roi, me.val.df.z.score, me.val.df.perf.bin)
      colnames(me.val.df) <- c("bblid", "sex", "age_bin", "lobe", "roi", "z_score", "perf_bin")
      outputDataframe <- rbind(outputDataframe, me.val.df)
      

      # Now find the hi values
      hi.val.df.z.score <- (temp.hi.vals[,temp.col] - me.val.mean) / me.val.sd
      hi.val.df.bblid <- temp.hi.vals[,bblid.col]
      hi.val.df.sex <- rep(temp.gender, length(hi.val.df.z.score))
      hi.val.df.age.bin <- rep(ageBand, length(hi.val.df.z.score))
      hi.val.df.lobe <- rep(lobe.value, length(hi.val.df.z.score))
      hi.val.df.roi <- rep(roi.name, length(hi.val.df.z.score))
      hi.val.df.perf.bin <- rep('hi', length(hi.val.df.z.score))
      hi.val.df <- cbind(hi.val.df.bblid, hi.val.df.sex, hi.val.df.age.bin, hi.val.df.lobe,
                         hi.val.df.roi, hi.val.df.z.score, hi.val.df.perf.bin)
      colnames(hi.val.df) <- c("bblid", "sex", "age_bin", "lobe", "roi", "z_score", "perf_bin")
      outputDataframe <- rbind(outputDataframe, hi.val.df)
    }
  }
  return(outputDataframe)
}
