# Load library(s)
source('/home/adrose/hiLo/scripts/01_DataPrep/functions/functions.R')
install_load('psych')

# Now load the data
data.values <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/mgiData/n550_mgi_demo_dx_2013-12-13.csv')
modal.scores <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/mgiData/MGI_FScores.csv')
volume.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawDataMGI/n1601_antsCtVol_jlfVol.csv')
volume.data <- volume.data[,-grep('Cerebral_White_Matter', names(volume.data))]
volume.data <- volume.data[,-grep("4th_Ventricle", names(volume.data))]
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawDataMGI/n1601_jlfGMD.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawDataMGI/n1601_jlfCt.csv')
cc.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawDataMGI/n1601_jlfCc.csv')

# Create the age regressed overall scores here
modal.scores <- merge(data.values, modal.scores, by='bblid')
modal.scores$Overall_EfficiencyAR <- lm(Overall_Efficiency ~ age + age^2 + age^3, data=modal.scores)$residuals

# Now find subjects that we should not use
bblid.index <- modal.scores$bblid
bblid.index <- modal.scores$bblid[modal.scores$bblid %in% data.values$bblid[which(data.values$Affected.Unaffected=='Unaffected')]]

# Now create a for loop to do everything for our GM values
dataVals <- c('gmd.data', 'ct.data', 'cc.data', 'volume.data')
outNames <- c('gmdData.csv', 'ctData.csv','ccData.csv', 'volumeData.csv')
modalNames <- c('mprage_jlf_gmd', 'mprage_jlf_ct', 'mprage_jlf_cortcon', 'mprage_jlf_vol')
excludeVals <- c('averageRating', 'averageRating', 'averageRating', 'averageRating')
outputMeanLR <- "/home/adrose/dataPrepForHiLoPaper/data/meanLRMGI/"
outputMeanLRAgeReg <- "/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegMGI/"
outputMeanLRAgeRegModReg <- "/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegModalRegMGI/"

# Now I am going to create a for loop which will output all of the required CSV's 
for(i in 1:length(dataVals)){
  # First load the data set, and create all of our output names
  tmpData <- get(dataVals[i])
  outMean <- paste(outputMeanLR, outNames[i], sep='')
  outAge <- paste(outputMeanLRAgeReg, outNames[i], sep='')
  outMod <- paste(outputMeanLRAgeRegModReg, outNames[i], sep='')

  # Now apply our immediete restrictions
  tmpData <- tmpData[tmpData$bblid %in% bblid.index,]
  tmpData <- tmpData[which(tmpData[excludeVals[i]]!=0), ]
  tmpData <- merge(modal.scores, tmpData, by='bblid')

  # Now attach our demographic data
  tmpData$sex <- data.values$Gender[match(tmpData$bblid, data.values$bblid)]
  tmpData$ageAtGo1Scan <- data.values$age[match(tmpData$bblid, data.values$bblid)]

  # produce our avgLR
  tmpLR <- averageLeftAndRight(tmpData)
  
  # Now produce our ageReg vals 
  tmpAR <- tmpData
  tmpAR[,grep(modalNames[i], names(tmpAR))] <- apply(tmpAR[,grep(modalNames[i], names(tmpAR))], 2, function(x) regressOutAgeNoQAMGI(x, tmpAR$ageAtGo1Scan))
  tmpAR <- averageLeftAndRight(tmpAR)

  # Now produce a modality regressed data frame
  tmpMR <- regressWithinModality(tmpAR, modalNames[i])

  # Now write the csvs
  write.csv(tmpLR, outMean, quote=F, row.names=F)
  write.csv(tmpAR, outAge, quote=F, row.names=F)
  write.csv(tmpMR, outMod, quote=F, row.names=F)
}
