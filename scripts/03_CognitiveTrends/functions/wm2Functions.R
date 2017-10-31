reorganizeWM2LobeOrder <- function(dataFrame, lobeOfInterest){
  if(lobeOfInterest == "WM"){
    inputRois <- c("Frontal_Lobe_WM","Temporal_Lobe_WM","Parietal_Lobe_WM","Occipital_Lobe_WM","Limbic_Lobe_WM", "Insular_Lobe_WM")
    outputRois <- c("Frontal_Lobe_WM","Temporal_Lobe_WM","Parietal_Lobe_WM","Occipital_Lobe_WM","Limbic_Lobe_WM", "Insular_Lobe_WM")
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
  rois_wm2_full<-c("Frontal_Lobe_WM","Temporal_Lobe_WM","Parietal_Lobe_WM","Occipital_Lobe_WM","Limbic_Lobe_WM", "Insular_Lobe_WM")
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
                       c("Frontal_Lobe_WM"="FRO WM","Temporal_Lobe_WM"="TEM WM","Parietal_Lobe_WM"="PAR WM","Occipital_Lobe_WM"="OCC WM","Insular_Lobe_WM"="Insular WM", "Limbic_Lobe_WM"="Limbic WM"))
  #tempDF$ROI <- factor(tempDF$ROI, levels=c("Cer" ,"FRO", "TEM", "PAR", "OCC", "CC"))
  #tempDF$lobe <- factor(tempDF$lobe, levels=c("WM"))
  outputDataFrame <- tempDF

  return(outputDataFrame)
}



