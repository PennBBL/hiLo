
## Declare some functions
# Declare a function which will add age bins to a data column given an age column
addAgeBins <- function(ageValues, dataFrame, lowerAge, upperAge, ageBinName){
  ageValuesOfInterest <- which(ageValues > lowerAge & ageValues <= upperAge)
  dataFrameOfInterest <- dataFrame[ageValuesOfInterest,]  
  toAdd <- rep(ageBinName, length(ageValuesOfInterest))
  dataFrameOfInterest$ageBin <- toAdd
  return(dataFrameOfInterest)
}

# Declare a function which will return the performance group 
returnPercentileGroup <- function(groupLevel,df_row, tmp_df, staticFlag=FALSE){
  if(staticFlag=='FALSE'){
  quantileTmp <- quantile(df_row, probs = c(0,.33,.66,1), na.rm=TRUE)
  if(groupLevel == "lo"){
    quantileTmplow <- getElement(quantileTmp,"0%")
    quantileTmphigh <- getElement(quantileTmp,"33%")
  }
  if(groupLevel == "me"){
    quantileTmplow <- getElement(quantileTmp,"33%")
    quantileTmphigh  <- getElement(quantileTmp,"66%")
  }
  if(groupLevel =="hi"){
    quantileTmplow <- getElement(quantileTmp,"66%")
    quantileTmphigh <- getElement(quantileTmp,"100%") + 1
  }
  df_to_return <- which(df_row >= quantileTmplow & df_row < quantileTmphigh)
  df_to_return  <- tmp_df[df_to_return,]
  df_to_return$groupFactorLevel <- groupLevel
  }
  if(staticFlag=='TRUE'){
    df_to_return <- merge(tmp_df, static.perf.bin)
    df_to_return <- df_to_return[which(df_to_return$groupFactorLevel==groupLevel),]
  }
  return(df_to_return)
}

# Create a function which will give me the standard error 
stand_err <- function(input_vector){
  tmp <- sd(input_vector,na.rm=T)/sqrt(sum(!is.na(input_vector)))
  return(tmp)
}

# Create a function which will output the standardized data frame for all subjects 
standardizePerfGroups <- function(dataFrame, modalityName, ageGroup, factorValue){
  ## Output dataframe needs to be editied.. not entierly sure of direction of the function though at the moment
  outputDataframe <- data.frame(ROI=character(), groupLevel=numeric(),
                                meanValue=numeric(), standErrValue=numeric(), 
                                standardDeviation=numeric(), gender=numeric(),
                                ageBin=numeric())
  col.of.interest <- grep(paste('^',modalityName, sep=''), names(dataFrame))
  dataFrame <- dataFrame[complete.cases(dataFrame[,col.of.interest]),]
  sexCol <- grep("sex",names(dataFrame))
  factor.col <- dataFrame[,factorValue]
  me.vals <- returnPercentileGroup('me', factor.col, dataFrame, staticFlag=FALSE)
  me.vals <- me.vals[which(me.vals$ageBin==ageGroup),]
  hi.vals <- returnPercentileGroup('hi', factor.col, dataFrame, staticFlag=FALSE)
  hi.vals <- hi.vals[which(hi.vals$ageBin==ageGroup),]
  lo.vals <- returnPercentileGroup('lo', factor.col, dataFrame, staticFlag=FALSE)
  lo.vals <- lo.vals[which(lo.vals$ageBin==ageGroup),]
  for(temp.gender in c(1,2)){
    me.vals.tmp <- me.vals[which(me.vals[,sexCol]==temp.gender & me.vals$ageBin == ageGroup),]
    hi.vals.tmp <- hi.vals[which(hi.vals[,sexCol]==temp.gender & hi.vals$ageBin == ageGroup),]
    lo.vals.tmp <- lo.vals[which(lo.vals[,sexCol]==temp.gender & lo.vals$ageBin == ageGroup),]
    for(temp.roi.col in col.of.interest){
        nuclei.name <- names(dataFrame)[temp.roi.col]

        # Find middle group means
        tmp.me.group.mean <- mean(me.vals.tmp[,temp.roi.col], na.rm=TRUE)
        tmp.me.group.sd <- sd(me.vals.tmp[,temp.roi.col], na.rm=TRUE)
        tmp.me.group.standerr <- stand_err(me.vals[,temp.roi.col])
        tmp.me.group.z.score.mean <- mean(unlist((me.vals.tmp[temp.roi.col] - tmp.me.group.mean) / tmp.me.group.sd), na.rm=TRUE)
        tmp.me.group.z.score.sd <- stand_err(unlist((me.vals.tmp[temp.roi.col] - tmp.me.group.mean) / tmp.me.group.sd))

        # Create me output df row
        tmp.me.output.row <- cbind(nuclei.name, 'me', tmp.me.group.z.score.mean, 
                                   tmp.me.group.standerr, tmp.me.group.z.score.sd,temp.gender, ageGroup)
        tmp.me.output.row <- do.call(data.frame,setNames(as.list(tmp.me.output.row), names(outputDataframe)))

        # Find hi group mean values
        tmp.hi.group.z.score.mean <- mean(unlist((hi.vals.tmp[,temp.roi.col] - tmp.me.group.mean) / tmp.me.group.sd), na.rm=TRUE)
        tmp.hi.group.standerr <- stand_err(unlist((hi.vals.tmp[,temp.roi.col] - tmp.me.group.mean) / tmp.me.group.sd))
        tmp.hi.group.z.score.sd <- sd(unlist((hi.vals.tmp[,temp.roi.col] - tmp.me.group.mean) / tmp.me.group.sd), na.rm=TRUE)

        # create hi output df row 
        tmp.hi.output.row <- cbind(nuclei.name, 'hi', tmp.hi.group.z.score.mean,
                                   tmp.hi.group.standerr, tmp.hi.group.z.score.sd, temp.gender, ageGroup)
        tmp.hi.output.row <- do.call(data.frame,setNames(as.list(tmp.hi.output.row), names(outputDataframe)))

        # Find lo group mean values
        tmp.lo.group.z.score.mean <-  mean(unlist((lo.vals.tmp[,temp.roi.col] - tmp.me.group.mean) / tmp.me.group.sd), na.rm=TRUE)
        tmp.lo.group.standerr <- stand_err(unlist((lo.vals.tmp[,temp.roi.col] - tmp.me.group.mean) / tmp.me.group.sd))
        tmp.lo.group.z.score.sd <- sd(unlist((lo.vals.tmp[,temp.roi.col] - tmp.me.group.mean) / tmp.me.group.sd), na.rm=TRUE)

        # create lo output df row
        tmp.lo.output.row <- cbind(nuclei.name, 'lo', tmp.lo.group.z.score.mean,
                                   tmp.lo.group.standerr, tmp.lo.group.z.score.sd, temp.gender, ageGroup)
        tmp.lo.output.row <- do.call(data.frame,setNames(as.list(tmp.lo.output.row), names(outputDataframe)))

        # export the values
        outputDataframe <- rbind(outputDataframe, tmp.lo.output.row, tmp.me.output.row, tmp.hi.output.row)  
    }    
  }
  return(outputDataframe)
}

# Create a function which will output the standardized data frame for all subjects for the supplementary figures
#### This will only work with F1_Exec_Comp_Cog_Accuracy
standardizeSuppPerfGroups <- function(dataFrame, modalityName, ageGroup, factorValue){
  ## Output dataframe needs to be editied.. not entierly sure of direction of the function though at the moment
  outputDataframe <- data.frame(ROI=character(), groupLevel=numeric(),
                                meanValue=numeric(), standErrValue=numeric(), 
                                standardDeviation=numeric(), gender=numeric(),
                                ageBin=numeric())
  col.of.interest <- grep(paste('^',modalityName, sep=''), names(dataFrame))
  dataFrame <- dataFrame[complete.cases(dataFrame[,col.of.interest]),]
  sexCol <- grep("sex",names(dataFrame))
  factor.col <- dataFrame[,c(factorValue)]
  me.vals <- returnPercentileGroup('me', factor.col, dataFrame)
  me.vals <- me.vals[which(me.vals$ageBin==ageGroup),]
  hi.vals <- returnPercentileGroup('hi', factor.col, dataFrame)
  hi.vals <- hi.vals[which(hi.vals$ageBin==ageGroup),]
  lo.vals <- returnPercentileGroup('lo', factor.col, dataFrame)
  lo.vals <- lo.vals[which(lo.vals$ageBin==ageGroup),]
  for(temp.gender in c(1, 2)){
    me.vals.tmp <- me.vals[which(me.vals$ageBin == ageGroup & me.vals$sex==temp.gender),]
    hi.vals.tmp <- hi.vals[which(hi.vals$ageBin == ageGroup & me.vals$sex==temp.gender),]
    lo.vals.tmp <- lo.vals[which(lo.vals$ageBin == ageGroup & me.vals$sex==temp.gender),]
    for(temp.roi.col in col.of.interest){
        nuclei.name <- names(dataFrame)[temp.roi.col]

        # Find middle group means
        tmp.me.group.mean <- mean(me.vals.tmp[,temp.roi.col], na.rm=TRUE)
        tmp.me.group.sd <- sd(me.vals.tmp[,temp.roi.col], na.rm=TRUE)
        tmp.me.group.standerr <- stand_err(me.vals[,temp.roi.col])
        tmp.me.group.z.score.mean <- mean(unlist((me.vals.tmp[temp.roi.col] - tmp.me.group.mean) / tmp.me.group.sd), na.rm=TRUE)
        tmp.me.group.z.score.sd <- sd(unlist((me.vals.tmp[temp.roi.col] - tmp.me.group.mean) / tmp.me.group.sd), na.rm=TRUE)

        # Create me output df row
        tmp.me.output.row <- cbind(nuclei.name, 'me', tmp.me.group.z.score.mean, 
                                   tmp.me.group.standerr, tmp.me.group.z.score.sd,temp.gender, ageGroup)
        tmp.me.output.row <- do.call(data.frame,setNames(as.list(tmp.me.output.row), names(outputDataframe)))

        # Find hi group mean values
        tmp.hi.group.z.score.mean <- mean(unlist((hi.vals.tmp[,temp.roi.col] - tmp.me.group.mean) / tmp.me.group.sd), na.rm=TRUE)
        tmp.hi.group.standerr <- stand_err(hi.vals.tmp[,temp.roi.col])
        tmp.hi.group.z.score.sd <- sd(unlist((hi.vals.tmp[,temp.roi.col] - tmp.me.group.mean) / tmp.me.group.sd), na.rm=TRUE)

        # create hi output df row 
        tmp.hi.output.row <- cbind(nuclei.name, 'hi', tmp.hi.group.z.score.mean,
                                   tmp.hi.group.standerr, tmp.hi.group.z.score.sd, temp.gender, ageGroup)
        tmp.hi.output.row <- do.call(data.frame,setNames(as.list(tmp.hi.output.row), names(outputDataframe)))

        # Find lo group mean values
        tmp.lo.group.z.score.mean <-  mean(unlist((lo.vals.tmp[,temp.roi.col] - tmp.me.group.mean) / tmp.me.group.sd), na.rm=TRUE)
        tmp.lo.group.standerr <- stand_err(lo.vals.tmp[,temp.roi.col])
        tmp.lo.group.z.score.sd <- sd(unlist((lo.vals.tmp[,temp.roi.col] - tmp.me.group.mean) / tmp.me.group.sd), na.rm=TRUE)

        # create lo output df row
        tmp.lo.output.row <- cbind(nuclei.name, 'lo', tmp.lo.group.z.score.mean,
                                   tmp.lo.group.standerr, tmp.lo.group.z.score.sd, temp.gender, ageGroup)
        tmp.lo.output.row <- do.call(data.frame,setNames(as.list(tmp.lo.output.row), names(outputDataframe)))

        # export the values
        outputDataframe <- rbind(outputDataframe, tmp.lo.output.row, tmp.me.output.row, tmp.hi.output.row)  
    }    
  }
  return(outputDataframe)
}


# Create a function which will reorder the ROI names and order 
reorganizeLobeOrder <- function(dataFrame, lobeOfInterest){
  if(lobeOfInterest == "Basal Ganglia"){
    inputRois <- c("Thal","Putamen","Caudate","Pallidum","Accumbens")
    #outputRois <- c("Caudate","Thalamus_Proper","BasFor","Pallidum","Accumbens_Area","Putamen")
    #outputRois <- c("BasForebr","Accumbens_Area","Pallidum","Putamen","Thalamus_Proper","Caudate")
    outputRois <- c("Putamen",  "Pallidum", "Caudate", "Accumbens_Area", "Thalamus_Proper")
    tmpDF <- which(dataFrame$lobe==lobeOfInterest)
    tmpDF <- dataFrame[tmpDF,]
  }
  if(lobeOfInterest == "Limbic"){
    inputRois <- c("PHG","Hipp","PIns","SCA","AIns","ACgG","PCgG","Ent","Amygdala","MCgG")
    #outputRois <- c("PHG","PCgG","Hip","ACgG","SCA","Ains","Pins","Ent","MCgG","Amygdala")
    outputRois <- c("Ent","PHG","Amygdala","SCA","Hipp","AIns","PIns","ACgG","PCgG","MCgG")
    tmpDF <- which(dataFrame$lobe==lobeOfInterest)
    tmpDF <- dataFrame[tmpDF,]
  }
  if(lobeOfInterest == "Frontal Orbital"){
    inputRois <- c("FO","MFC","MOrG","POrG","OrIFG","TrIFG","AOrG","OpIFG","GRe","FRP", "LOrG")
    #outputRois <- c("POrG","AOrG","FO","OrIFG","OpIFG","MFC","MOrG","LOrG","TrIFG","FRP","Gre")
    outputRois <- c("GRe","MOrG","POrG","MFC","LOrG","AOrG","OrIFG","FRP","FO","TrIFG","OpIFG")
    tmpDF <- which(dataFrame$lobe==lobeOfInterest)
    tmpDF <- dataFrame[tmpDF,]
  }
  if(lobeOfInterest == "Frontal Dorsal"){
    inputRois <- c("PrG","MSFG","SMC","MFG","SFG")
    #outputRois <- c("MSFG","PrG","MFG","SMC","SFG")
    outputRois <- c("MSFG","MFG","PrG","SFG","SMC")
    tmpDF <- which(dataFrame$lobe==lobeOfInterest)
    tmpDF <- dataFrame[tmpDF,]
  }
  if(lobeOfInterest == "Temporal"){
    inputRois <- c("FuG","PT","PP","ITG","CO","MTG","TMP","STG","TTG")
    #outputRois <- c("FuG","MTG","CO","TTG","PT","ITG","TMP","STG","PP")
    outputRois <- c("TMP","FuG","ITG","MTG","PP","STG","TTG","CO","PT")
    tmpDF <- which(dataFrame$lobe==lobeOfInterest)
    tmpDF <- dataFrame[tmpDF,]
  }
  if(lobeOfInterest == "Parietal"){
    inputRois <- c("PCu","PoG","AnG","PO","SPL","MPrG","SMG","MPoG")
    #outputRois <- c("Pcu","PoG","MPrG","SPL","MPoG","PO","SMG","AnG")
    outputRois <- c("PO","AnG","PCu","SMG","PoG","SPL","MPrG","MPoG")
    tmpDF <- which(dataFrame$lobe==lobeOfInterest)
    tmpDF <- dataFrame[tmpDF,]
  }
  if(lobeOfInterest == "Occipital"){
    inputRois <- c("IOG","Cun","LiG","OFuG","MOG","Calc","OCP","SOG")
    #outputRois <- c("LiG","MOG","OFuG","SOG","IOG","OCP","Cun","Calc")
    outputRois <- c("OFuG","LiG","IOG","OCP","Calc","Cun","MOG","SOG")
    tmpDF <- which(dataFrame$lobe==lobeOfInterest)
    tmpDF <- dataFrame[tmpDF,]
  }
  if(lobeOfInterest == "Cerebellum"){
    inputRois <- c("Cerebellum_Exterior", "Cerebellar_Vermal_Lobules_I.V", "Cerebellar_Vermal_Lobules_VI.VII", "Cerebellar_Vermal_Lobules_VIII.X")
    outputRois <- c("Cerebellum_Exterior", "Cerebellar_Vermal_Lobules_I.V", "Cerebellar_Vermal_Lobules_VI.VII", "Cerebellar_Vermal_Lobules_VIII.X")
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
    if(lobeOfInterest=="Frontal Dorsal"){
      tmpDF <- tmpDF[-tmpDFRowsOfInterest,]
      if(grepPattern==5){
        outputOrder<-4
      }
    }
    if(lobeOfInterest=="Parietal"){
      if(grepPattern==2){
        excessFat <- grep("MPoG", tmpDF$ROI[tmpDFRowsOfInterest])
        tmpDFRowsOfInterest <- tmpDFRowsOfInterest[-excessFat] 
        rowsToBind <- tmpDF[tmpDFRowsOfInterest,]
        outputOrder<-5       
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
  outputDF <- tmp[order(tmp$Order, decreasing = TRUE),]
  # Now return the data frame
  return(outputDF)
}


# Create a function whichh will modify the ROI names 
organizeROINames <- function(dataFrame, cerebellum=F){
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
          "Cerebellum_Exterior", "Cerebellar_Vermal_Lobules_I.V", "Cerebellar_Vermal_Lobules_VI.VII", "Cerebellar_Vermal_Lobules_VIII.X") # Cerebellum



  # Now prime a output data frame
  outputDataFrame <- dataFrame
  na.column <- rep(NA,length(outputDataFrame$ROI))
  outputDataFrame$ROI_readable <- na.column
  
  # Now go through each ROI value and rm the extrenous fat
  for(roi.to.rename in 1:length(outputDataFrame$ROI)){
    delimited.name <- unlist(strsplit(as.character(outputDataFrame$ROI[roi.to.rename]),"_"))
    delimited.name <- paste(delimited.name, collapse="_")
    outputDataFrame$ROI_readable[roi.to.rename] <- delimited.name
  }

  # Now add a lobe to each row
  outputDataFrame$lobe <- na.column
  index.key <- c(1,7,17,28,33,42,50,58)
  for(pattern.match.variable in 1:length(rois)){
    roi.to.grep <- rois[pattern.match.variable]
    pattern.locations <- grep(roi.to.grep, outputDataFrame$ROI_readable)
    for(lobe.index in pattern.locations){
      lobe.group <- findInterval(pattern.match.variable, index.key)
      outputDataFrame$lobe[lobe.index] <- lobe.group
    }
  }
  # Now change all of the names based on the index
  outputDataFrame$lobe[outputDataFrame$lobe==1] <- "Basal Ganglia"
  outputDataFrame$lobe[outputDataFrame$lobe==2] <- "Limbic"
  outputDataFrame$lobe[outputDataFrame$lobe==3] <- "Frontal Orbital"
  outputDataFrame$lobe[outputDataFrame$lobe==4] <- "Frontal Dorsal"
  outputDataFrame$lobe[outputDataFrame$lobe==5] <- "Temporal"
  outputDataFrame$lobe[outputDataFrame$lobe==6] <- "Parietal"
  outputDataFrame$lobe[outputDataFrame$lobe==7] <- "Occipital"
  outputDataFrame$lobe[outputDataFrame$lobe==8] <- "Cerebellum"
  outputDataFrame$lobe[is.na(outputDataFrame$lobe)] <- "Misc."

  # Fix the POrG issue
  porgRows <- grep("POrG", outputDataFrame$ROI_readable)  
  outputDataFrame$lobe[porgRows] <- "Frontal Orbital"

 
  outputDataFrame$meanValue <- as.numeric(as.character(outputDataFrame$meanValue))
  return(outputDataFrame)
}

# create a function which will subtract hi from lo data
subtractHiFromLo <- function(dataFrame){
  # First split the data frames into sex
  sexSplit <- split(dataFrame, dataFrame$gender)
  tmp1 <- sexSplit$'1'
  tmp2 <- sexSplit$'2'
  groupSplitMale <- split(tmp1, tmp1$groupLevel)
  groupSplitFemale <- split(tmp2, tmp2$groupLevel)
  # Now create the male High - Low data Frame
  highMale <- groupSplitMale$hi
  lowMale <- groupSplitMale$lo
  maleDifference <- as.numeric(as.character(highMale$meanValue)) - as.numeric(as.character(lowMale$meanValue))
  maleDataFrame <- highMale
  maleDataFrame$zScoreCombined <- NULL
  maleDataFrame$zScoreDifference <- maleDifference
  maleDataFrame$sex <-"M" 
  # Now create the female High - Low data frame
  highFemale <- groupSplitFemale$hi
  lowFemale <- groupSplitFemale$lo
  femaleDifference <- as.numeric(as.character(highFemale$meanValue)) - as.numeric(as.character(lowFemale$meanValue))
  femaleDataFrame <- highFemale
  femaleDataFrame$zScoreCombined <- NULL
  femaleDataFrame$zScoreDifference <- femaleDifference
  femaleDataFrame$sex <-"F"
  # Now combine the two genders
  outputDataFrame <- rbind(femaleDataFrame, maleDataFrame)
  outputDataFrame$groupLevel <- "Hi-Lo"
  outputDataFrame$gender <- as.factor(outputDataFrame$gender)
  # Now return the data frame
  return(outputDataFrame)
}

# addAgeBins function
addAgeBin <- function(df, ageColumn, youngUpper, middleUpper, olderLower){
  output.young <- addAgeBins(ageColumn, df, 0, youngUpper, 'Childhood')
  output.middle <- addAgeBins(ageColumn, df, youngUpper, middleUpper, 'Adolescence')
  output.older <- addAgeBins(ageColumn, df, middleUpper, 999, 'Early Adulthood')
  output <- rbind(output.young, output.middle, output.older)
  return(output)
}

# Create a function whichh will modify the ROI names 
organizeROINamesCT <- function(dataFrame){
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
          "Cerebellum_Exterior", "Cerebellar_Vermal_Lobules_I.V", "Cerebellar_Vermal_Lobules_VI.VII", "Cerebellar_Vermal_Lobules_VIII.X") # Cerebellum




  # Now prime a output data frame
  outputDataFrame <- dataFrame
  na.column <- rep(NA,length(outputDataFrame$ROI))
  outputDataFrame$ROI_readable <- na.column
  
  # Now go through each ROI value and rm the extrenous fat
  for(roi.to.rename in 1:length(outputDataFrame$ROI)){
    delimited.name <- unlist(strsplit(as.character(outputDataFrame$ROI[roi.to.rename]),"_"))
    delimited.name <- paste(delimited.name, collapse="_")
    outputDataFrame$ROI_readable[roi.to.rename] <- delimited.name
  }

  # Now add a lobe to each row
  outputDataFrame$lobe <- na.column
  index.key <- c(1,7,17,28,33,42,50,58)
  for(pattern.match.variable in 1:length(rois)){
    roi.to.grep <- rois[pattern.match.variable]
    pattern.locations <- grep(roi.to.grep, outputDataFrame$ROI_readable)
    for(lobe.index in pattern.locations){
      lobe.group <- findInterval(pattern.match.variable, index.key)
      outputDataFrame$lobe[lobe.index] <- lobe.group
    }
  }
  # Now change all of the names based on the index
  outputDataFrame$lobe[outputDataFrame$lobe==1] <- "Basal Ganglia"
  outputDataFrame$lobe[outputDataFrame$lobe==2] <- "Limbic"
  outputDataFrame$lobe[outputDataFrame$lobe==3] <- "Frontal Orbital"
  outputDataFrame$lobe[outputDataFrame$lobe==4] <- "Frontal Dorsal"
  outputDataFrame$lobe[outputDataFrame$lobe==5] <- "Temporal"
  outputDataFrame$lobe[outputDataFrame$lobe==6] <- "Parietal"
  outputDataFrame$lobe[outputDataFrame$lobe==7] <- "Occipital"
  outputDataFrame$lobe[outputDataFrame$lobe==8] <- "Cerebellum"

  # Fix the POrG issue
  porgRows <- grep("POrG", outputDataFrame$ROI_readable)  
  outputDataFrame$lobe[porgRows] <- "Frontal Orbital"



  # Now resort the roi's
  #tempDF <- reorganizeLobeOrder(outputDataFrame,"Basal Ganglia")
  #tempDF$ROI <- revalue(tempDF$ROI, c("Putamen"="Put", "Accumbens_Area"="NA", "Pallidum"="GP", "BasForebr"="BasF","Thalamus_Proper"="Tha","Caudate"="CN"))
  #tempDF <- reorganizeLobeOrder(outputDataFrame,"Limbic")
  #tempDF$ROI <- revalue(tempDF$ROI, c("Amygdala"="Amy"))
  tempDF <- reorganizeLobeOrder(outputDataFrame,"Frontal Orbital")
  tempDF <- rbind(tempDF, reorganizeLobeOrder(outputDataFrame,"Frontal Dorsal"))
  tempDF <- rbind(tempDF, reorganizeLobeOrder(outputDataFrame,"Temporal"))
  tempDF <- rbind(tempDF, reorganizeLobeOrder(outputDataFrame,"Parietal"))
  tempDF <- rbind(tempDF, reorganizeLobeOrder(outputDataFrame,"Occipital"))
  #tempDF <- rbind(tempDF, reorganizeLobeOrder(outputDataFrame,"Cerebellum"))
  tempDF$ROI <- factor(tempDF$ROI, levels=rev(unique(as.character(tempDF$ROI))))
  tempDF$lobe <- factor(tempDF$lobe, levels=c("Basal Ganglia","Limbic", "Frontal Orbital", "Frontal Dorsal", "Temporal", "Parietal","Occipital", "Cerebellum"))
  outputDataFrame <- tempDF
  outputDataFrame$meanValue <- as.numeric(as.character(outputDataFrame$meanValue))
  return(outputDataFrame)
}

