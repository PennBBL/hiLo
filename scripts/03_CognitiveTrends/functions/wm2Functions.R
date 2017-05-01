reorganizeWM2LobeOrder <- function(dataFrame, lobeOfInterest){
  if(lobeOfInterest == "WM"){
    inputRois <- c("FRO_WM","TEM_WM","PAR_WM","OCC_WM","corpus_callosum","Cer_WM")
    outputRois <- c("Cer_WM","FRO_WM","TEM_WM","PAR_WM","OCC_WM","corpus_callosum")
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



organizeWM2ROINames <- function(dataFrame){
  rois_wm2_full<-c("FRO_WM","TEM_WM","PAR_WM","OCC_WM","corpus_callosum","Cer_WM")
  # Now prime a output data frame
  outputDataFrame <- dataFrame
  na.column <- rep(NA,length(outputDataFrame$ROI))
  outputDataFrame$ROI_readable <- na.column
  
  # Now add a lobe to each row
  outputDataFrame$lobe <- na.column
  index.key <- c(1)
  for(pattern.match.variable in 1:length(rois_wm2_full)){
    roi.to.grep <- rois_wm2_full[pattern.match.variable]
    pattern.locations <- grep(roi.to.grep, outputDataFrame$ROI)
    for(lobe.index in pattern.locations){
      lobe.group <- findInterval(pattern.match.variable, index.key)
      outputDataFrame$lobe[lobe.index] <- lobe.group
    }
  }
  # Now change all of the names based on the index
  outputDataFrame$lobe[outputDataFrame$lobe==1] <- "WM"

  # Now resort the roi's
  tempDF <- reorganizeWM2LobeOrder(outputDataFrame, "WM")
  tempDF$ROI <- revalue(tempDF$ROI, 
                       c("corpus_callosum"="CC","FRO_WM"="FRO","TEM_WM"="TEM","PAR_WM"="PAR","OCC_WM"="OCC","Cer_WM"="Cer"))
  tempDF$ROI <- factor(tempDF$ROI, levels=c("Cer" ,"FRO", "TEM", "PAR", "OCC", "CC"))
  tempDF$lobe <- factor(tempDF$lobe, levels=c("WM"))
  outputDataFrame <- tempDF

  return(outputDataFrame)
}



