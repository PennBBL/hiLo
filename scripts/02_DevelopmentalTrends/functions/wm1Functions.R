reorganizeWM1LobeOrder <- function(dataFrame, lobeOfInterest){
  if(lobeOfInterest == "Association Fiber"){
    inputRois <- c("cgc","slf","ss","ec")
    outputRois <- c("cgc","slf","ss","ec")
    tmpDF <- which(dataFrame$lobe==lobeOfInterest)
    tmpDF <- dataFrame[tmpDF,]
  }
  if(lobeOfInterest == "Commissural Fiber"){
    inputRois <- c("gcc","bcc","scc")
    outputRois <- c("gcc","bcc","scc")
    tmpDF <- which(dataFrame$lobe==lobeOfInterest)
    tmpDF <- dataFrame[tmpDF,]
  }  
  if(lobeOfInterest == "Projection Fiber"){
    inputRois <- c("alic","plic","ptr","scr","pcr","rlic")
    outputRois <- c("alic","plic","ptr","scr","pcr","rlic")
    tmpDF <- which(dataFrame$lobe==lobeOfInterest)
    tmpDF <- dataFrame[tmpDF,]
  }
  if(lobeOfInterest == "Other"){
    inputRois <- c("cst","pct","mel","icp","mcp","scp","cp","acr","cgh","fnx_st","fnx","sfo","uf","tap")
    outputRois <- c("cst","pct","mel","icp","mcp","scp","cp","acr","cgh","fnx_st","fnx","sfo","uf","tap")
    tmpDF <- which(dataFrame$lobe==lobeOfInterest)
    tmpDF <- dataFrame[tmpDF,]
  }
  # Now resort the rois so they match the outputROIs order and character strings This for loop will correct the strings and also add an index which will we will order later
  for(grepPattern in 1:length(inputRois)){
    tmpDFRowsOfInterest <- grep(inputRois[grepPattern], tmpDF$ROI, fixed=TRUE)
    if(length(tmpDFRowsOfInterest)>13){
      tmpDFRowsOfInterest <- tmpDFRowsOfInterest[-c(7:12,19:24)]
    }
    rowsToBind <- tmpDF[tmpDFRowsOfInterest,]
    outputOrder <- grep(inputRois[grepPattern], outputRois)
    if(lobeOfInterest == "Other"){
      tmpDF <- tmpDF[-tmpDFRowsOfInterest,]
      if(grepPattern == 7){
        outputOrder <- 7
      }
      if(grepPattern == 11){
        outputOrder <- 11
      }
    }
    rowsToBind$Order <- rep(outputOrder,nrow(rowsToBind))
    rowsToBind$ROI <- outputRois[outputOrder]
    if(grepPattern == 1){
      tmp <- rowsToBind
    }
    else{
      tmp <- rbind(tmp,rowsToBind)    
    }
  }
  # Now order the tmp data frame
  outputDF <- tmp[order(tmp$Order),]
  # Now return the data frame
  return(outputDF)
}



organizeWM1ROINames <- function(dataFrame){
  # Now prime a output data frame
  outputDataFrame <- dataFrame
  na.column <- rep(NA,length(outputDataFrame$ROI))
  outputDataFrame$ROI_readable <- na.column
  
  # Now add a lobe to each row
  outputDataFrame$lobe <- na.column
  index.key <- c(1,5,8,14)
  for(pattern.match.variable in 1:length(rois_wm1_full)){
    roi.to.grep <- rois_wm1_full[pattern.match.variable]
    pattern.locations <- grep(roi.to.grep, outputDataFrame$ROI)
    for(lobe.index in pattern.locations){
      lobe.group <- findInterval(pattern.match.variable, index.key)
      outputDataFrame$lobe[lobe.index] <- lobe.group
    }
  }
  # Now change all of the names based on the index
  outputDataFrame$lobe[outputDataFrame$lobe==1] <- "Association Fiber"
  outputDataFrame$lobe[outputDataFrame$lobe==2] <- "Commissural Fiber"
  outputDataFrame$lobe[outputDataFrame$lobe==3] <- "Projection Fiber"
  outputDataFrame$lobe[outputDataFrame$lobe==4] <- "Other"

  # Now resort the roi's
  tempDF <- reorganizeWM1LobeOrder(outputDataFrame,"Association Fiber")
  tempDF <- rbind(tempDF, reorganizeWM1LobeOrder(outputDataFrame, "Commissural Fiber"))
  tempDF <- rbind(tempDF, reorganizeWM1LobeOrder(outputDataFrame,"Projection Fiber"))  
  tempDF <- rbind(tempDF, reorganizeWM1LobeOrder(outputDataFrame,"Other"))
  tempDF$ROI <- factor(tempDF$ROI, levels=unique(as.character(tempDF$ROI)))
  tempDF$lobe <- factor(tempDF$lobe, levels=c("Association Fiber","Commissural Fiber", "Projection Fiber", "Other"))
  outputDataFrame <- tempDF

  return(outputDataFrame)
}


