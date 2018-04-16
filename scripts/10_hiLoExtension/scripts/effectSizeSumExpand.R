## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm2Functions.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm1Functions2.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/functions-forJLF.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'ggrepel', 'irr')

## Load data
vol.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/volumeData.csv')
cbf.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/cbfData.csv')
gmd.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/gmdData.csv')
tr.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/jlfTRData.csv')

## Declare any functions
returnPerfBin <- function(data, inputCol) {
    
    quantiles <- quantile(data[,inputCol], c(0,.33,.67,1))
    
    data$perfBin <- 0
    data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy < quantiles[2])] <- 'lo'
    data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy >= quantiles[2] &
    data$F1_Exec_Comp_Cog_Accuracy <= quantiles[3])] <- 'me'
    data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy > quantiles[3])] <- 'hi'
    return(data)
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
    #if(length(tmpDFRowsOfInterest)>13){
    #  tmpDFRowsOfInterest <- tmpDFRowsOfInterest[-c(7:12,19:24)]
    #}
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

## Now in a loop go through all of the values of the factor scores
## that we want to return ROI importance for
tmpDF <- vol.modal.data.age.reg
allData <- NULL
for(s in names(tmpDF)[7:16]){

## Create our static perf bin
tmpDF <- vol.modal.data.age.reg
tmpDF <- returnPerfBin(tmpDF, s)
outCol <- tmpDF[,c('bblid','scanid','perfBin')]
colnames(outCol)[3] <- paste('perfCol', 1, sep='')
static.perf.bin <- outCol
colnames(static.perf.bin) <- c('bblid', 'scanid', 'groupFactorLevel')
rm(tmpDF)

## Now add age bin
cbf.modal.data.age.reg$ageBin <- 'Age Regressed'
vol.modal.data.age.reg$ageBin <- 'Age Regressed'
gmd.modal.data.age.reg$ageBin <- 'Age Regressed'
tr.modal.data.age.reg$ageBin <- 'Age Regressed'

## Now produce all our loop values
data.names <- c('vol','cbf','gmd','tr')
grepVals <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_','dti_jlf_tr_')
wmAdd <- c(1,1,0,1)
cerebellumValues <- c('TRUE', 'FALSE', 'TRUE', 'TRUE')
## Now prepare an output matrix
data <- doEverythingEver(vol.modal.data.age.reg, 'mprage_jlf_vol_', 0, 999, 'Age Regressed', cerebellumIn=T, optionalRace=NULL)
data <- rbind(data, doEverythingEverWM(vol.modal.data.age.reg, 'mprage_jlf_vol_', 0, 999, 'Age Regressed', cerebellumIn=T, optionalRace=NULL))
data$hemisphere <- "R"
index <- rep(0, dim(data)[1])
for(i in 1:length(index)){
  tmpVal <- strSplitMatrixReturn(data$ROI_readable, "mprage_jlf_vol_")[i,2]
  tmpVal2 <- strSplitMatrixReturn(tmpVal, "_")[,1]
  if(tmpVal2=="L"){
    index[i] <- 1
  }
}
data$hemisphere[which(index==1)] <- "L"
output.data <- data[,c(1,6,8,9,11,13)]

for(q in 2:4){
    print(data.names[q])
    dfName <- paste(data.names[q], ".modal.data.age.reg", sep='')
    data <- doEverythingEver(get(dfName), grepVals[q], 0, 999, 'Age Regressed', cerebellumIn=cerebellumValues[q], optionalRace=NULL)
    if(wmAdd[q]==1){
      data <- rbind(data, doEverythingEverWM(get(dfName), grepVals[q], 0, 999, 'Age Regressed', optionalRace=NULL))
    }
    data$hemisphere <- "R"
    index <- rep(0, dim(data)[1])
    for(i in 1:length(index)){
      tmpVal <- strSplitMatrixReturn(data$ROI_readable, grepVals[q])[i,2]
      tmpVal2 <- strSplitMatrixReturn(tmpVal, "_")[,1]
      if(tmpVal2=="L"){
        index[i] <- 1
      }
    }
    data$hemisphere[which(index==1)] <- "L"
    output.data <- merge(output.data, data, by=c('ROI', 'Gender', 'lobe', 'hemisphere'), suffixes=c('', data.names[q]), all=T)
}

# Now combine the values
# Make sure the take the absolute for cbf and tr
output.data$sumEffectSize <- rowSums(cbind(output.data$zScoreDifference,abs(output.data$zScoreDifferencecbf),output.data$zScoreDifferencegmd,abs(output.data$zScoreDifferencetr)),na.rm=T)
output.data$ROI <- paste(output.data$ROI, output.data$hemisphere)
output.data <- reshape(data=output.data, direction="wide", idvar="ROI", timevar='Gender', v.names='sumEffectSize')
output.data$lobe <- revalue(output.data$lobe, replace=c("Basal Ganglia"="Basalstriatal", "Limbic"="Limbic", "Frontal Orbital"="Frontal", "Frontal Dorsal"="Frontal", "Temporal"="Temporal", "Parietal"="Parietal", "Occipital"="Occipital","Cerebellum"="Cerebellum"))
toWrite <- output.data[,c(1,2,4,33,34)]
if(identical(dim(allData), NULL)){
  allData <- toWrite
  colnames(allData)[4:5] <- paste(s, colnames(toWrite)[4:5], sep='_')
}
else{
  colnames(toWrite)[4:5] <- paste(s, colnames(toWrite)[4:5], sep='_')
  allData <- cbind(allData, toWrite[,4:5])
}
write.csv(toWrite, paste(s, "SummedES.csv", sep=''), quote=F, row.names=F)
}

# Now create a corellation matrix for these values
corrplot(cor(allData[,grep('Male', names(allData))], method='s'))
corrplot(cor(allData[,grep('Female', names(allData))], method='s'))

## It looks like there is more varaince across genders than there is across factor scores 
## for this exercise which is kinda a bummer.
