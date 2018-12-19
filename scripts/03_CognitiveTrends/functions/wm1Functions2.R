rois_wm1_full<-c("cgc","cgh","slf","ss","ec","fnx","fnx_st","sfo","uf", #Association Fiber
                 "gcc","bcc","scc","tap", #Commisseral Fiber
                 "alic","plic","ptr","scr","pcr","rlic","cp","acr", #Projection Fiber
                 "cst","pct","mel","icp","mcp","scp") #Brainstem


reorganizeWM1LobeOrder <- function(dataFrame, lobeOfInterest){
  if(lobeOfInterest == "Association Fiber"){
    inputRois <- c("cgc","cgh","slf","ss","ec","fnx","fnx_st","sfo","uf")
    #outputRois <- c("cgc","cgh","slf","ss","ec","fnx","fnx_st","sfo","uf")
    outputRois <- c("ss","cgh","slf","fnx_st","cgc","fnx","uf","ec","sfo")
    tmpDF <- which(dataFrame$lobe==lobeOfInterest)
    tmpDF <- dataFrame[tmpDF,]
  }
  if(lobeOfInterest == "Commissural Fiber"){
    inputRois <- c("gcc","bcc","scc","tap")
    #outputRois <- c("gcc","bcc","scc","tap")
    outputRois <- c("tap", "scc", "bcc", "gcc")
    tmpDF <- which(dataFrame$lobe==lobeOfInterest)
    tmpDF <- dataFrame[tmpDF,]
  }  
  if(lobeOfInterest == "Projection Fiber"){
    inputRois <- c("alic","plic","ptr","scr","pcr","rlic","cp","acr")
    #outputRois <- c("alic","plic","ptr","scr","pcr","rlic","cp","acr")
    outputRois <- c("ptr","pcr","rlic","cp","plic","scr","alic","acr")
    tmpDF <- which(dataFrame$lobe==lobeOfInterest)
    tmpDF <- dataFrame[tmpDF,]
  }
  if(lobeOfInterest == "Brainstem"){
    inputRois <- c("cst","pct","mel","icp","mcp","scp")
    #outputRois <- c("cst","pct","mel","icp","mcp","scp")
    outputRois <- c("icp","scp","mcp","mel","pct","cst")
    tmpDF <- which(dataFrame$lobe==lobeOfInterest)
    tmpDF <- dataFrame[tmpDF,]
  }
  # Now resort the rois so they match the outputROIs order and character strings This for loop will correct the strings and also add an index which will we will order later
  for(grepPattern in 1:length(inputRois)){
    tmpDFRowsOfInterest <- grep(inputRois[grepPattern], tmpDF$ROI, fixed=TRUE)
    rowsToBind <- tmpDF[tmpDFRowsOfInterest,]
    #rowsToBinq <- rowsToBind[grep("label",rowsToBind$ROI),]
    outputOrder <- grep(inputRois[grepPattern], outputRois)
    if(lobeOfInterest == "Association Fiber"){
      if(grepPattern == 6 ){
        rowsToBind <- rowsToBind[- grep("fnx_st",rowsToBind$ROI),]
        outputOrder <- 6
      }
    }
    if(lobeOfInterest == "Projection Fiber"){
      if(grepPattern == 6){
        outputOrder <- 3
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
  index.key <- c(1,10,14,22)
  for(pattern.match.variable in 1:length(rois_wm1_full)){
    roi.to.grep <- rois_wm1_full[pattern.match.variable]
    pattern.locations <- grep(roi.to.grep, outputDataFrame$ROI)
    for(lobe.index in pattern.locations){
      lobe.group <- findInterval(pattern.match.variable, index.key)
      outputDataFrame$lobe[lobe.index] <- lobe.group
      outputDataFrame$ROI_readable[lobe.index] <- outputDataFrame$ROI[lobe.index]
    }
  }
  # Now change all of the names based on the index
  outputDataFrame$lobe[outputDataFrame$lobe==1] <- "Association Fiber"
  outputDataFrame$lobe[outputDataFrame$lobe==2] <- "Commissural Fiber"
  outputDataFrame$lobe[outputDataFrame$lobe==3] <- "Projection Fiber"
  outputDataFrame$lobe[outputDataFrame$lobe==4] <- "Brainstem"

  # Now resort the roi's
  tempDF <- reorganizeWM1LobeOrder(outputDataFrame,"Association Fiber")
  tempDF <- rbind(tempDF, reorganizeWM1LobeOrder(outputDataFrame, "Commissural Fiber"))
  tempDF <- rbind(tempDF, reorganizeWM1LobeOrder(outputDataFrame,"Projection Fiber"))  
  tempDF <- rbind(tempDF, reorganizeWM1LobeOrder(outputDataFrame,"Brainstem"))
  tempDF$ROI <- factor(tempDF$ROI, levels=unique(as.character(tempDF$ROI)))
  tempDF$lobe <- factor(tempDF$lobe, levels=c("Brainstem","Commissural Fiber", "Projection Fiber", "Association Fiber"))
  outputDataFrame <- tempDF

  return(outputDataFrame)
}


