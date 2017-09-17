
## Declare functions here
# Create a function which will return the global mean
returnGlobalMean <- function(dataFrame,modalityColVal){
  toReturn <- mean(dataFrame[,modalityColVal], na.rm=TRUE)
  return(toReturn)
}
# Create a function which will return the global sd
returnGlobalSd <- function(dataFrame,modalityColVal){
  toReturn <- sd(dataFrame[,modalityColVal], na.rm=TRUE)
  return(toReturn)
}
# Cretae a function which will return the male's mean value
returnMaleMean <- function(dataFrame,modalityColVal){
  sexCol <- grep("sex",names(dataFrame))
  toReturn <- mean(dataFrame[which(dataFrame[,sexCol]==1),modalityColVal],na.rm=TRUE)
  return(toReturn)
}
# Cretae a function which will reutnr the female's mean value
returnFemaleMean <- function(dataFrame,modalityColVal){
  sexCol <- grep("sex",names(dataFrame))
  toReturn <- mean(dataFrame[which(dataFrame[,sexCol]==2),modalityColVal],na.rm=TRUE)
  return(toReturn)
}

# Create a function which will standardize the modality of interest
standardizeModalityForBothSex <- function(modalityName,suffixName,dataFrame, ageBand){
  # First declare a output data frame
  outputDataFrame <- data.frame(ROI=character(),gender=character(), meanValue=numeric(), standErr=numeric())
  # Find the columns that we are interested in
  col.of.interest <- grep(paste('^',modalityName, sep=''), names(dataFrame))
  correct.suffix.index <- grep(suffixName, names(dataFrame)[col.of.interest])
  col.of.interest <- col.of.interest[correct.suffix.index]
  sexCol <- grep("sex",names(dataFrame))
  # Now go through and standardize each nuclei for the specified modality for each sex
  # Start with male data
  for(specific.roi in col.of.interest){
    # Declare the global information
    nuclei.mean <- returnGlobalMean(dataFrame, specific.roi)
    nuclei.sd <- returnGlobalSd(dataFrame, specific.roi)
    nuclei.name <- names(dataFrame)[specific.roi]
    sex <- "male"
    # Standardize and create output row for male data below here
    tmp.male.df <- dataFrame[which(dataFrame[,sexCol]==1 & dataFrame$ageBin == ageBand) ,specific.roi] 
    tmp.male.df.mean <- mean((tmp.male.df - nuclei.mean) / nuclei.sd, na.rm=TRUE)
    tmp.male.df.stand.err <- sd(tmp.male.df, na.rm=TRUE) / sqrt(sum(!is.na(tmp.male.df)))
    tmp.male.df.row <- cbind(nuclei.name,sex,tmp.male.df.mean, tmp.male.df.stand.err)
    tmp.male.df.row <- do.call(data.frame,setNames(as.list(tmp.male.df.row),names(outputDataFrame)))
    # Standardize and create output row for female data below here 
    sex <- "female"
    tmp.female.df <- dataFrame[which(dataFrame[,sexCol]==2 & dataFrame$ageBin == ageBand) ,specific.roi]
    tmp.female.df.mean <- mean((tmp.female.df - nuclei.mean) / nuclei.sd, na.rm=TRUE)
    tmp.female.df.stand.err <- sd(tmp.female.df, na.rm=TRUE) / sqrt(sum(!is.na(tmp.female.df)))
    tmp.female.df.row <- cbind(nuclei.name,sex,tmp.female.df.mean, tmp.female.df.stand.err)
    tmp.female.df.row <- do.call(data.frame,setNames(as.list(tmp.female.df.row),names(outputDataFrame)))
    # Now rbind to the output data frame
    outputDataFrame <- rbind(outputDataFrame, tmp.male.df.row, tmp.female.df.row)
  }
  return(outputDataFrame)
}



# Now create a wrapper script which will return just the data frame we are interested in 
returnStandardizedGenderMeans <- function(datFra, modality, suffix, age.bin){
  tmp.df <- standardizeModalityForBothSex(modality, suffix, datFra,age.bin)
  tmp.df <- organizeROINames(tmp.df)
  tmp.df$meanValue <- as.numeric(as.character(tmp.df$meanValue))
  tmp.df$standErr <- as.numeric(as.character(tmp.df$standErr))
  tmp.df$ageBin <- rep(age.bin,length(tmp.df$meanValue))
  tmp.df$tissue <- rep('gm', length(tmp.df$meanValue))
  return(tmp.df)
}


# This will plot the developmental graph horizontally 
createGGPlotImage <- function(dataFrame, plotTitle, lower_order, upper_order, increment){
  plotToReturn <- ggplot(dataFrame, aes(x=ROI, y=meanValue, group=gender)) +
      geom_line(aes(linetype=gender,color=gender), size=3) +
      geom_point(aes(shape=gender, color=gender, xmax=max(upper_order)), size=1.5) +
      scale_y_continuous(limits=c(lower_order, upper_order), 
                           breaks=round(seq(lower_order,upper_order,increment), digits=2)) +
      xlab("ROI") +
      ylab("Z-Score") +
      geom_hline(aes(yintercept=0), linetype="longdash", colour="black", size=0.5) +
      scale_colour_manual(name = "gender",
                          values=c("male"="blue","female"="red")) +
      scale_linetype_manual(name = "gender", values = c("male" = "solid", "female" = "solid")) +
      #scale_shape_manual(values=c(16), guide=FALSE) +
      theme_bw() +
      theme(legend.position="none") +
      theme(text=element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1, face="bold"), 
      axis.text.y = element_text(face="bold"), axis.title.x = element_text(face="bold"), axis.title.y = element_text(face="bold"),
      plot.title = element_text(face="bold")) +
      facet_grid(ageBin ~ lobe, scales="free", space="free_x") +
      ggtitle(plotTitle)
  
  return(plotToReturn)
}

# Create a function which will reorder the ROI names and order 
reorganizeLobeOrder <- function(dataFrame, lobeOfInterest){
  if(lobeOfInterest == "Basal Ganglia"){
    inputRois <- c("Thal","Putamen","Caudate","Pallidum","Accumbens")
    #outputRois <- c("Caudate","Thalamus_Proper","BasFor","Pallidum","Accumbens_Area","Putamen")
    #outputRois <- c("BasForebr","Accumbens_Area","Pallidum","Putamen","Thalamus_Proper","Caudate")
    outputRois <- c("Putamen", "Pallidum","Caudate", "Accumbens_Area", "Thalamus_Proper")
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

  # Fix the POrG issue
  porgRows <- grep("POrG", outputDataFrame$ROI_readable)  
  outputDataFrame$lobe[porgRows] <- "Frontal Orbital"



  # Now resort the roi's
  tempDF <- reorganizeLobeOrder(outputDataFrame,"Basal Ganglia")
  tempDF$ROI <- revalue(tempDF$ROI, c("Putamen"="Put", "Accumbens_Area"="NA", "Pallidum"="GP","Thalamus_Proper"="Tha","Caudate"="CN"))
  tempDF <- rbind(tempDF, reorganizeLobeOrder(outputDataFrame,"Limbic"))
  tempDF$ROI <- revalue(tempDF$ROI, c("Amygdala"="Amy"))
  tempDF <- rbind(tempDF, reorganizeLobeOrder(outputDataFrame,"Frontal Orbital"))
  tempDF <- rbind(tempDF, reorganizeLobeOrder(outputDataFrame,"Frontal Dorsal"))
  tempDF <- rbind(tempDF, reorganizeLobeOrder(outputDataFrame,"Temporal"))
  tempDF <- rbind(tempDF, reorganizeLobeOrder(outputDataFrame,"Parietal"))
  tempDF <- rbind(tempDF, reorganizeLobeOrder(outputDataFrame,"Occipital"))
  if(cerebellum == 'TRUE'){
    tempDF <- rbind(tempDF, reorganizeLobeOrder(outputDataFrame,"Cerebellum"))
  }
  tempDF$ROI <- factor(tempDF$ROI, levels=rev(unique(as.character(tempDF$ROI))))
  tempDF$lobe <- factor(tempDF$lobe, levels=c("Basal Ganglia","Limbic", "Frontal Orbital", "Frontal Dorsal", "Temporal", "Parietal","Occipital", "Cerebellum"))
  outputDataFrame <- tempDF
  outputDataFrame$meanValue <- as.numeric(as.character(outputDataFrame$meanValue))
  return(outputDataFrame)
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
  output.middle <- addAgeBins(ageColumn, df, youngUpper, middleUpper, 'Adolescence')
  output.older <- addAgeBins(ageColumn, df, middleUpper, 999, 'Early Adulthood')
  output <- rbind(output.young, output.middle, output.older)
  return(output)
}



## Declare some variables here
modalities <- c("vol","gmd","cbf","rsRH","dti")
suffices<-c("","_i_vr","_i_vr","_vr","_vr")


rois<-c("Thal","Putamen","Caudate","Pallidum",  # Basal Ganglia
        "Accumbens", "BasFor", # Basal Ganglia
        "PHG","Hip","Pins","SCA","Ains", # Limbic
        "ACgG","PCgG","Ent","Amygdala","MCgG", # Limbic
        "FO","MFC","MOrG","POrG","OrIFG","TrIFG","AOrG","OpIFG","Gre", # Frontal Orbital
        "FRP", "LOrG", # Frontal Orbital
        "PrG","MSFG","SMC","MFG","SFG", # Frontal Dorsal
        "FuG","PT","PP","ITG","CO","MTG","TMP","STG","TTG", # Temporal
        "Pcu","PoG","AnG","PO","SPL","MPrG", # Parietal
        "SMG","MPoG", # Parietal
        "IOG","Cun","LiG","OFuG","MOG","Calc","OCP","SOG", # Occiptal
        "Cer_Exterior", "CerVer_I_V", "CerVer_VI_VII", "CerVer_VIII_X") # Cerebellum

rois_wm2_full<-c("FRO_WM","TEM_WM","PAR_WM","OCC_WM","corpus_callosum", "Cer_WM")

upper_order <- 1



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
  tempDF$lobe <- factor(tempDF$lobe, levels=c("Basal Ganglia","Limbic", "Frontal Orbital", "Frontal Dorsal", "Temporal", "Parietal","Occipital"))
  outputDataFrame <- tempDF
  outputDataFrame$meanValue <- as.numeric(as.character(outputDataFrame$meanValue))
  return(outputDataFrame)
}



# Now create a return standard.... for CT
returnStandardizedGenderMeansCT <- function(datFra, modality, suffix, age.bin){
  tmp.df <- standardizeModalityForBothSex(modality, suffix, datFra,age.bin)
  tmp.df <- organizeROINamesCT(tmp.df)
  tmp.df$meanValue <- as.numeric(as.character(tmp.df$meanValue))
  tmp.df$standErr <- as.numeric(as.character(tmp.df$standErr))
  tmp.df$ageBin <- rep(age.bin,length(tmp.df$meanValue))
  tmp.df$tissue <- rep('gm', length(tmp.df$meanValue))
  return(tmp.df)
}





## Now add all of the functions for the dti labels down here
returnStandardizedWM1GenderMeans <- function(datFra, age.bin){
  tmp.df <- standardizeModalityForBothSex("dti_dtitk_jhulabel", "", datFra, age.bin)
  tmp.df <- organizeWM1ROINames(tmp.df)
  tmp.df$meanValue <- as.numeric(as.character(tmp.df$meanValue))
  tmp.df$standErr <- as.numeric(as.character(tmp.df$standErr))
  tmp.df$ageBin <- rep(age.bin,length(tmp.df$meanValue))
  tmp.df$tissue <- rep('wm', length(tmp.df$meanValue))
  return(tmp.df)
} 

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


