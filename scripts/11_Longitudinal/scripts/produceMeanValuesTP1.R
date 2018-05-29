## AFGR May 2018

## This script will be used to prepare mean values from go1 looking at our longitudinal labels
## This will be done for volume, cbf, gmd, alff, reho, and trace similar to hi-lo but with our labels
## No differences will be taken

## Load library(s)
install_load('ggplot2', 'reshape2', 'plyr')

## Declare functions
regressOutAge <- function(valuesToBeRegressed, ageColumn){
    # First declare an output column
    newValues <- rep(NA, length(valuesToBeRegressed))
    index <- which(complete.cases(valuesToBeRegressed))
    age <- scale(ageColumn)[,1]
    ageSqu <- scale(ageColumn)[,1]^2
    ageCub <- scale(ageColumn)[,1]^3
    newValues[index] <- lm(valuesToBeRegressed ~ age + ageSqu + ageCub)$residuals
    return(newValues)
}

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
  MPrGCheck <- grep("MPrG", grepPattern)
  if(!identical(integer(0), MPrGCheck)){
    pattern.match.variable <- 47
  }
  lobe.group <- findInterval(pattern.match.variable, index.key)
  return(lobe.group)
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
    tmpDFRowsOfInterest <- grep(paste("_", inputRois[grepPattern], sep=''), tmpDF$variable, fixed=TRUE)
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

## Now create a function which will plot all of our values
createGGPlotImage <- function(dataFrame, plotTitle, minValue, maxValue){
  plotToReturn <- ggplot(dataFrame, aes(y=value, x=ROI, group=group)) +
      geom_line(aes(linetype=group,color=group), size=1.5) +
      geom_point(aes(shape=group, color=group), size=1.5) +
      geom_errorbar(aes(x=ROI, ymin=value-se, ymax=value+se, color=group)) + 
      scale_y_continuous(limits=c(minValue, maxValue), 
                           breaks=round(seq(minValue, maxValue,.25), digits=2)) +
      xlab("ROI") +
      ylab("Effect Size") +
      geom_hline(aes(yintercept=0), linetype="solid", colour="black", size=0.5) +
      scale_colour_manual(name = "group",
                          values=c("Emergent"="orange","TD"="blue","Persister"="red","Resilient"="green")) +
      scale_linetype_manual(name = "group", values = c("Emergent"="solid","TD"="solid","Persister"="solid","Resilient"="solid")) +
      scale_shape_manual(name="group", values=c("Emergent"="18","TD"="9","Persister"="17","Resilient"="18")) +
      theme_bw() +
      theme(legend.position="top") +
      theme(text=element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1, face="bold"), 
      axis.text.y = element_text(face="bold", size=24), axis.title.x = element_text(face="bold", size=28),
      axis.title.y = element_text(face="bold", size=28),
      plot.title = element_text(face="bold", size=28), strip.text.x = element_text(size=15)) +
      facet_grid(Presentation ~ lobe, scales="free", space="free_x") +
      ggtitle(plotTitle)
  
  return(plotToReturn)
}

## Now create a function which will plot all of our values
createGGPlotImage2 <- function(dataFrame, plotTitle, minValue, maxValue){
  dataFrame$group <- revalue(dataFrame$group, c("High ERS // High Psychosis"="High Psychosis // Adverse","High ERS // Low Psychosis"="Low PS // Adverse","Low ERS // High Psychosis"="High PS // Benign","Low ERS // Low Psychosis"="Low PS // Benign"))
  plotToReturn <- ggplot(dataFrame, aes(y=value, x=ROI, group=group)) +
      geom_line(aes(linetype=group,color=group), size=1.5) +
      geom_point(aes(shape=group, color=group), size=2.5) +
      geom_errorbar(aes(x=ROI, ymin=value-se, ymax=value+se, color=group)) + 
      scale_y_continuous(limits=c(minValue, maxValue), 
                           breaks=round(seq(minValue, maxValue,.25), digits=2)) +
      xlab("ROI") +
      ylab("Effect Size") +
      geom_hline(aes(yintercept=0), linetype="solid", colour="black", size=0.5) +
      scale_colour_manual(name = "group",
                          values=c("High Psychosis // Adverse"="orange","Low PS // Adverse"="light blue","High PS // Benign"="red","Low PS // Benign"="blue")) +
      scale_linetype_manual(name = "group", values = c("High Psychosis // Adverse"="solid","Low PS // Adverse"="solid","High PS // Benign"="solid","Low PS // Benign"="solid")) +
      scale_shape_manual(name="group", values=c("High Psychosis // Adverse"="18","Low PS // Adverse"="18","High PS // Benign"="17","Low PS // Benign"="9")) +
      theme_bw() +
      theme(legend.position="top") +
      theme(text=element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1, face="bold"), 
      axis.text.y = element_text(face="bold", size=24), axis.title.x = element_text(face="bold", size=28),
      axis.title.y = element_text(face="bold", size=28),
      plot.title = element_text(face="bold", size=28), strip.text.x = element_text(size=15)) +
      facet_grid(. ~ lobe, scales="free", space="free_x") +
      ggtitle(plotTitle)
  
  return(plotToReturn)
}

## Load the data
img.data <-  read.csv("/home/adrose/forRuben/data/n2416_imagingDataDump_2018-05-04.csv")
long.labels <- read.csv("/home/adrose/dataPrepForHiLoPaper/data/n2416ClinicalDemoPsycho/pnc_diagnosis_categorical_20170526.csv")
all.data <- merge(img.data, long.labels, by='bblid')
all.data <- all.data[which(all.data$tpvalue==1),]

## Now prepare all of our imaging values
## I am going to do this in left and right values individually
## and then finally I will take the average across hemisphere and produce that as well
## Start with the right values
names.index <- names(all.data)[grep('jlf', names(all.data))]

## Age regress the values here
img.values.ar <- all.data[,names.index]
img.values.ar$group <- all.data$pncGrpPsychosis.y
img.values.ar$sex <- all.data$sex
img.values.ar$age <- all.data$scanageMonths
img.values.ar[,names.index] <- apply(img.values.ar[,names.index], 2, function(x) regressOutAge(x, img.values.ar$age)) 

## Now average across hemispheres
img.values.ar <- averageLeftAndRight(img.values.ar)

## Now scale these values
names.index <- names(img.values.ar)[grep('jlf', names(img.values.ar))]
img.values.ar.male <- apply(img.values.ar[which(img.values.ar$sex==1),names.index], 2, function(x) as.numeric(scale(x)))
img.values.ar.female <- apply(img.values.ar[which(img.values.ar$sex==2),names.index], 2, function(x) as.numeric(scale(x)))
img.values.ar[which(img.values.ar$sex==1),names.index] <- img.values.ar.male
img.values.ar[which(img.values.ar$sex==2),names.index] <- img.values.ar.female
img.values.ar <- data.frame(img.values.ar)
img.values.ar <- img.values.ar[-which(img.values.ar$group=='' | img.values.ar$group=='Flux'),]
img.values.ar$Presentation <- 'TD'
img.values.ar$Presentation[which(img.values.ar$group=="Persister" | img.values.ar$group=="Resilient")] <- 'PS'

## Now calculate the mean values for each of our labels for these guys
output.data.frame <- NULL
for(i in names.index){
  tmp.data <- melt(summarySE(data=img.values.ar, measurevar=i, groupvars=c('sex', 'group'), na.rm=T), id=c('sex', 'group', 'N', 'se', 'sd', 'ci'), measure.vars=c(i))
  output.data.frame <- rbind(output.data.frame, tmp.data)
}
output.data.frame$modality <- NA
## Now find the attach a modality factor
modalities <- c('vol', 'gmd','cbf', 'tr','alff', 'reho', 'ct', 'cortcon', 'rd', 'fa', 'ad')
for(m in modalities){
  # create our grep pattern
  gp <- paste("_", m, "_", sep='')
  output.data.frame$modality[grep(gp, output.data.frame$variable)] <- m
}

## Now attach our pretty names
output.data.frame$lobe <- NA
for(i in unique(output.data.frame$variable)){
  output.data.frame$lobe[which(output.data.frame$variable==i)] <- findLobe(i) 
}
output.data.frame$lobe[output.data.frame$lobe==1] <- "Basal Ganglia"
output.data.frame$lobe[output.data.frame$lobe==2] <- "Limbic"
output.data.frame$lobe[output.data.frame$lobe==3] <- "Frontal Orbital"
output.data.frame$lobe[output.data.frame$lobe==4] <- "Frontal Dorsal"
output.data.frame$lobe[output.data.frame$lobe==5] <- "Temporal"
output.data.frame$lobe[output.data.frame$lobe==6] <- "Parietal"
output.data.frame$lobe[output.data.frame$lobe==7] <- "Occipital"
output.data.frame$lobe[output.data.frame$lobe==8] <- "Cerebellum"
output.data.frame$lobe[output.data.frame$lobe==9] <- "WM Lobe"

# Now add a Presentation variable
output.data.frame$Presentation <- 'TD'
output.data.frame$Presentation[which(output.data.frame$group=="Persister" | output.data.frame$group=="Resilient")] <- 'PS'
output.data.frame$PresentationGender <- paste(output.data.frame$Presentation, output.data.frame$sex, sep='')
## Now isolate our individual modalities and produce our pretty names
lobes.to.iter <- c("Basal Ganglia","Limbic","Frontal Orbital","Frontal Dorsal","Temporal","Parietal","Occipital","Cerebellum")
modalities <- c('vol', 'gmd','cbf', 'tr','alff', 'reho')
min.val <- c(-.7,-.5,-.5,-.6,-.5,-.5)
max.val <- c(.5,.5,.6,.9,.8,.7)
index <- 1
for(m in modalities){
  tmp.data <- output.data.frame[which(output.data.frame$modality==m),]
  tmp.vals.to.plot <- NULL
  if(m %in% c('vol', 'gmd', 'tr', 'reho', 'alff')){
    for(x in lobes.to.iter){
      # First produce isolate our modality of interest
      new.values <- reorganizeLobeOrder(tmp.data, x)
      tmp.vals.to.plot <- rbind(tmp.vals.to.plot, new.values)
    }
    tmp.vals.to.plot$ROI <- revalue(tmp.vals.to.plot$ROI, c("Putamen"="Put", "Accumbens_Area"="NA", "Pallidum"="GP","Thalamus_Proper"="Tha","Caudate"="CN"))
    tmp.vals.to.plot$ROI <- revalue(tmp.vals.to.plot$ROI,c("Cerebellum_Exterior"="Cer Ext", "Cerebellar_Vermal_Lobules_I.V"="Vermis 1-5", "Cerebellar_Vermal_Lobules_VI.VII"="Vermis 6-7", "Cerebellar_Vermal_Lobules_VIII.X"="Vermis 8-10"))
  }
  else{
    for(x in lobes.to.iter[1:7]){
      # First produce isolate our modality of interest
      new.values <- reorganizeLobeOrder(tmp.data, x)
      tmp.vals.to.plot <- rbind(tmp.vals.to.plot, new.values)
    }
    tmp.vals.to.plot$ROI <- revalue(tmp.vals.to.plot$ROI, c("Putamen"="Put", "Accumbens_Area"="NA", "Pallidum"="GP","Thalamus_Proper"="Tha","Caudate"="CN"))
  }  
  tmp.vals.to.plot$lobe <- factor(tmp.vals.to.plot$lobe, levels=c("Basal Ganglia","Limbic", "Frontal Orbital", "Frontal Dorsal", "Temporal", "Parietal","Occipital", "Cerebellum"))
  # Now create our plots
  male.plot <- createGGPlotImage(dataFrame=tmp.vals.to.plot[which(tmp.vals.to.plot$sex==1),], plotTitle='Male',minValue=min.val[index], maxValue=max.val[index])
  female.plot <- createGGPlotImage(dataFrame=tmp.vals.to.plot[which(tmp.vals.to.plot$sex==2),], plotTitle='Female',minValue=min.val[index], maxValue=max.val[index])
  pdf(paste(m, "WithinGender.pdf", sep=''), height=14, width=36)
  multiplot(male.plot, female.plot, cols=2)
  dev.off()
  print(m)
    index <- index + 1
}
## Now do the same thing w/o sex 
## Calculate the mean values for each of our labels for these guys
output.data.frame <- NULL
for(i in names.index){
  tmp.data <- melt(summarySE(data=img.values.ar, measurevar=i, groupvars=c('group'), na.rm=T), id=c('group', 'N', 'se', 'sd', 'ci'), measure.vars=c(i))
  output.data.frame <- rbind(output.data.frame, tmp.data)
}
output.data.frame$modality <- NA
## Now find the attach a modality factor
modalities <- c('vol', 'ct', 'gmd', 'cortcon', 'cbf', 'tr', 'rd', 'ad', 'fa', 'alff', 'reho')
for(m in modalities){
  # create our grep pattern
  gp <- paste("_", m, "_", sep='')
  output.data.frame$modality[grep(gp, output.data.frame$variable)] <- m
}

## Now attach our pretty names
output.data.frame$lobe <- NA
for(i in unique(output.data.frame$variable)){
  output.data.frame$lobe[which(output.data.frame$variable==i)] <- findLobe(i) 
}
output.data.frame$lobe[output.data.frame$lobe==1] <- "Basal Ganglia"
output.data.frame$lobe[output.data.frame$lobe==2] <- "Limbic"
output.data.frame$lobe[output.data.frame$lobe==3] <- "Frontal Orbital"
output.data.frame$lobe[output.data.frame$lobe==4] <- "Frontal Dorsal"
output.data.frame$lobe[output.data.frame$lobe==5] <- "Temporal"
output.data.frame$lobe[output.data.frame$lobe==6] <- "Parietal"
output.data.frame$lobe[output.data.frame$lobe==7] <- "Occipital"
output.data.frame$lobe[output.data.frame$lobe==8] <- "Cerebellum"
output.data.frame$lobe[output.data.frame$lobe==9] <- "WM Lobe"

# Now add a Presentation variable
output.data.frame$Presentation <- 'TD'
output.data.frame$Presentation[which(output.data.frame$group=="Persister" | output.data.frame$group=="Resilient")] <- 'PS'
output.data.frame$PresentationGender <- paste(output.data.frame$Presentation, output.data.frame$sex, sep='')
## Now isolate our individual modalities and produce our pretty names
lobes.to.iter <- c("Basal Ganglia","Limbic","Frontal Orbital","Frontal Dorsal","Temporal","Parietal","Occipital","Cerebellum")
modalities <- c('vol', 'gmd','cbf', 'tr','alff', 'reho')
min.val <- c(-.7,-.5,-.5,-.6,-.5,-.5)
max.val <- c(.5,.5,.6,.9,.8,.7)
index <- 1
for(m in modalities){
  tmp.data <- output.data.frame[which(output.data.frame$modality==m),]
  tmp.vals.to.plot <- NULL
  if(m %in% c('vol', 'gmd', 'tr', 'reho', 'alff')){
    for(x in lobes.to.iter){
      # First produce isolate our modality of interest
      new.values <- reorganizeLobeOrder(tmp.data, x)
      tmp.vals.to.plot <- rbind(tmp.vals.to.plot, new.values)
    }
    tmp.vals.to.plot$ROI <- revalue(tmp.vals.to.plot$ROI, c("Putamen"="Put", "Accumbens_Area"="NA", "Pallidum"="GP","Thalamus_Proper"="Tha","Caudate"="CN"))
    tmp.vals.to.plot$ROI <- revalue(tmp.vals.to.plot$ROI,c("Cerebellum_Exterior"="Cer Ext", "Cerebellar_Vermal_Lobules_I.V"="Vermis 1-5", "Cerebellar_Vermal_Lobules_VI.VII"="Vermis 6-7", "Cerebellar_Vermal_Lobules_VIII.X"="Vermis 8-10"))
  }
  else{
    for(x in lobes.to.iter[1:7]){
      # First produce isolate our modality of interest
      new.values <- reorganizeLobeOrder(tmp.data, x)
      tmp.vals.to.plot <- rbind(tmp.vals.to.plot, new.values)
    }
    tmp.vals.to.plot$ROI <- revalue(tmp.vals.to.plot$ROI, c("Putamen"="Put", "Accumbens_Area"="NA", "Pallidum"="GP","Thalamus_Proper"="Tha","Caudate"="CN"))
  }
  
  tmp.vals.to.plot$lobe <- factor(tmp.vals.to.plot$lobe, levels=c("Basal Ganglia","Limbic", "Frontal Orbital", "Frontal Dorsal", "Temporal", "Parietal","Occipital", "Cerebellum"))
  # Now create our plots
  out.plot <- createGGPlotImage(dataFrame=tmp.vals.to.plot, plotTitle='',minValue=min.val[index], maxValue=max.val[index])
  pdf(paste(m, "AcrossGender.pdf", sep=''), height=12, width=18)
  print(out.plot)
  dev.off()
  print(m)
  index <- index + 1
}

## Now do our PS and ERS quartile bins
## First load our ERS and PS data
bin.cont.data <- read.csv('/home/tymoore/Gene-Environment_Long_Data_for_Ruben_3.csv')
# Isolate our cognitive domain to only one value
bin.cont.data <- bin.cont.data[which(bin.cont.data$Cognitive_Domain=="abf_efficiency"),]
img.data.2 <- merge(img.data, bin.cont.data, by='bblid')
img.data.2 <- img.data.2[which(img.data.2$tpvalue==1),]
## Now prepare our imaging values
names.index <- names(all.data)[grep('jlf', names(all.data))]

## Age regress the values here
img.values.ar <- img.data.2[,names.index]
img.values.ar$group <- img.data.2$ERS_Psychosis_Quartile
img.values.ar$sex <- img.data.2$sex
img.values.ar$age <- img.data.2$scanageMonths
img.values.ar[,names.index] <- apply(img.values.ar[,names.index], 2, function(x) regressOutAge(x, img.values.ar$age)) 

## Now average across hemispheres
img.values.ar <- averageLeftAndRight(img.values.ar)

## Now scale these values
names.index <- names(img.values.ar)[grep('jlf', names(img.values.ar))]
img.values.ar.male <- apply(img.values.ar[which(img.values.ar$sex==1),names.index], 2, function(x) as.numeric(scale(x)))
img.values.ar.female <- apply(img.values.ar[which(img.values.ar$sex==2),names.index], 2, function(x) as.numeric(scale(x)))
img.values.ar[which(img.values.ar$sex==1),names.index] <- img.values.ar.male
img.values.ar[which(img.values.ar$sex==2),names.index] <- img.values.ar.female
img.values.ar <- data.frame(img.values.ar)
img.values.ar <- img.values.ar[-which(img.values.ar$group==''),]

## Now calculate the mean values for each of our labels for these guys
output.data.frame <- NULL
for(i in names.index){
  tmp.data <- melt(summarySE(data=img.values.ar, measurevar=i, groupvars=c('group'), na.rm=T), id=c('group', 'N', 'se', 'sd', 'ci'), measure.vars=c(i))
  output.data.frame <- rbind(output.data.frame, tmp.data)
}
output.data.frame$modality <- NA
## Now find the attach a modality factor
modalities <- c('vol', 'ct', 'gmd', 'cortcon', 'cbf', 'tr', 'rd', 'ad', 'fa', 'alff', 'reho')
for(m in modalities){
  # create our grep pattern
  gp <- paste("_", m, "_", sep='')
  output.data.frame$modality[grep(gp, output.data.frame$variable)] <- m
}

## Now attach our pretty names
output.data.frame$lobe <- NA
for(i in unique(output.data.frame$variable)){
  output.data.frame$lobe[which(output.data.frame$variable==i)] <- findLobe(i) 
}
output.data.frame$lobe[output.data.frame$lobe==1] <- "Basal Ganglia"
output.data.frame$lobe[output.data.frame$lobe==2] <- "Limbic"
output.data.frame$lobe[output.data.frame$lobe==3] <- "Frontal Orbital"
output.data.frame$lobe[output.data.frame$lobe==4] <- "Frontal Dorsal"
output.data.frame$lobe[output.data.frame$lobe==5] <- "Temporal"
output.data.frame$lobe[output.data.frame$lobe==6] <- "Parietal"
output.data.frame$lobe[output.data.frame$lobe==7] <- "Occipital"
output.data.frame$lobe[output.data.frame$lobe==8] <- "Cerebellum"
output.data.frame$lobe[output.data.frame$lobe==9] <- "WM Lobe"

## Now isolate our individual modalities and produce our pretty names
lobes.to.iter <- c("Basal Ganglia","Limbic","Frontal Orbital","Frontal Dorsal","Temporal","Parietal","Occipital","Cerebellum")
modalities <- c('vol', 'gmd','cbf', 'tr','alff', 'reho')
min.val <- c(-.7,-.7,-.5,-.6,-.5,-.5)
max.val <- c(.8,.7,.6,.9,.6,.6)
index <- 1
for(m in modalities){
  tmp.data <- output.data.frame[which(output.data.frame$modality==m),]
  tmp.vals.to.plot <- NULL
  if(m %in% c('vol', 'gmd', 'tr', 'reho', 'alff')){
    for(x in lobes.to.iter){
      # First produce isolate our modality of interest
      new.values <- reorganizeLobeOrder(tmp.data, x)
      tmp.vals.to.plot <- rbind(tmp.vals.to.plot, new.values)
    }
    tmp.vals.to.plot$ROI <- revalue(tmp.vals.to.plot$ROI, c("Putamen"="Put", "Accumbens_Area"="NA", "Pallidum"="GP","Thalamus_Proper"="Tha","Caudate"="CN"))
    tmp.vals.to.plot$ROI <- revalue(tmp.vals.to.plot$ROI,c("Cerebellum_Exterior"="Cer Ext", "Cerebellar_Vermal_Lobules_I.V"="Vermis 1-5", "Cerebellar_Vermal_Lobules_VI.VII"="Vermis 6-7", "Cerebellar_Vermal_Lobules_VIII.X"="Vermis 8-10"))
  }
  else{
    for(x in lobes.to.iter[1:7]){
      # First produce isolate our modality of interest
      new.values <- reorganizeLobeOrder(tmp.data, x)
      tmp.vals.to.plot <- rbind(tmp.vals.to.plot, new.values)
    }
    tmp.vals.to.plot$ROI <- revalue(tmp.vals.to.plot$ROI, c("Putamen"="Put", "Accumbens_Area"="NA", "Pallidum"="GP","Thalamus_Proper"="Tha","Caudate"="CN"))
  }
  
  tmp.vals.to.plot$lobe <- factor(tmp.vals.to.plot$lobe, levels=c("Basal Ganglia","Limbic", "Frontal Orbital", "Frontal Dorsal", "Temporal", "Parietal","Occipital", "Cerebellum"))
  # Now create our plots
  out.plot <- createGGPlotImage2(dataFrame=tmp.vals.to.plot, plotTitle='',minValue=min.val[index], maxValue=max.val[index])
  pdf(paste(m, "ERSandPSQuartiles.pdf", sep=''), height=12, width=18)
  print(out.plot)
  dev.off()
  print(m)
  index <- index + 1
}

