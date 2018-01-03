# This script is going to be used to produce the hi - lo graphs from jlf data
# hi - lo will just not die....

## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm2Functions.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm1Functions2.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/functions-forJLF.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table')

# Declare any functions
# Now load the data
vol.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRMGI/volumeData.csv')
vol.modal.data$F1_Exec_Comp_Cog_Accuracy <- vol.modal.data$Overall_EfficiencyAR
gmd.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRMGI/gmdData.csv')
gmd.modal.data$F1_Exec_Comp_Cog_Accuracy <- gmd.modal.data$Overall_EfficiencyAR
ct.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRMGI/ctData.csv')
ct.modal.data$F1_Exec_Comp_Cog_Accuracy <- ct.modal.data$Overall_EfficiencyAR
cc.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRMGI/ccData.csv')
cc.modal.data$F1_Exec_Comp_Cog_Accuracy <- cc.modal.data$Overall_EfficiencyAR
vol.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegMGI/volumeData.csv')
vol.modal.data.age.reg$F1_Exec_Comp_Cog_Accuracy <- vol.modal.data.age.reg$Overall_EfficiencyAR
gmd.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegMGI/gmdData.csv')
gmd.modal.data.age.reg$F1_Exec_Comp_Cog_Accuracy <- gmd.modal.data.age.reg$Overall_EfficiencyAR
ct.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegMGI/ctData.csv')
ct.modal.data.age.reg$F1_Exec_Comp_Cog_Accuracy <- ct.modal.data.age.reg$Overall_EfficiencyAR
cc.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegMGI/ccData.csv')
cc.modal.data.age.reg$F1_Exec_Comp_Cog_Accuracy <- cc.modal.data.age.reg$Overall_EfficiencyAR

vol.modal.data.age.reg$ageBin <- 'Age Regressed'
gmd.modal.data.age.reg$ageBin <- 'Age Regressed'
ct.modal.data.age.reg$ageBin <- 'Age Regressed'
cc.modal.data.age.reg$ageBin <- 'Age Regressed'


# Now produce the age reg values
age.reg.vol <- doEverythingEver(vol.modal.data.age.reg, 'mprage_jlf_vol', 0, 999, 'Age Regressed', cerebellum=T,optionalRace=NULL)
# Now do GMD
age.reg.gmd <- doEverythingEver(gmd.modal.data.age.reg, 'mprage_jlf_gmd', 0, 999, 'Age Regressed', cerebellum=T,optionalRace=NULL)
#Lets do CT
age.reg.ct <- doEverythingEverCT(ct.modal.data.age.reg, 'mprage_jlf_ct', 0, 999, 'Age Regressed',optionalRace=NULL)
## CC, i thought you would never ask 
age.reg.cc <- doEverythingEverCT(cc.modal.data.age.reg, 'mprage_jlf_cortcon', 0, 999, 'Age Regressed',optionalRace=NULL)

volPlotAgeReg <- createGGPlotImage(age.reg.vol, 'Volume Hi-Lo JLF Data Age Reg', -1, 1, .2)
gmdPlotAgeReg <- createGGPlotImage(age.reg.gmd, 'GMD Hi-Lo JLF Data Age Reg', -1, 1, .2)
ctPlotAgeReg <- createGGPlotImage(age.reg.ct, 'CT Hi-Lo JLF Data Age Reg', -1, 1, .2)
ccPlotAgeReg <- createGGPlotImage(age.reg.cc, 'CC Hi-Lo JLF Data Age Reg', -1, 1, .2)

pdf('ageRegHi-LoGraphsMGI.pdf', width=20, height=20)
volPlotAgeReg
gmdPlotAgeReg
ctPlotAgeReg
#ccPlotAgeReg
dev.off()


# Now do the age bins - age bins are don a little differently for MGI as detailed below
addMGIAgeBin <- function(x){
  x$ageBin <- 'Childhood'
  x$ageBin[x$ageAtGo1Scan > 12] <- '12-29'
  x$ageBin[x$ageAtGo1Scan > 29.1] <- '29-55'
  x$ageBin[x$ageAtGo1Scan > 55] <- '56+'
  return(x)
}

# Now run through all the modalities and add the age bins 
vol.modal.data <- addMGIAgeBin(vol.modal.data)
gmd.modal.data <- addMGIAgeBin(gmd.modal.data)
ct.modal.data <- addMGIAgeBin(ct.modal.data)

# Now produce each hi-lo by age bin for each modality
volData <- doEverythingEver(vol.modal.data, 'mprage_jlf_vol_', 12, 29, '12-29',cerebellum=T)
gmdData <- doEverythingEver(gmd.modal.data, 'mprage_jlf_gmd_', 12, 29, '12-29',cerebellum=T)
ctData <- doEverythingEverCT(ct.modal.data, 'mprage_jlf_ct_', 12, 29, '12-29')

# Now plot em
volPlot <- createGGPlotImage(volData, 'Volume Hi-Lo JLF Data 12-29', -2, 2, .2)
gmdPlot <- createGGPlotImage(gmdData, 'GMD Hi-Lo JLF Data 12-29', -2, 2, .2)
ctPlot <- createGGPlotImage(ctData, 'CT Hi-Lo JLF Data 12-29', -2, 2, .2)

# Now print em
pdf('mgi12-29HiLo.pdf', width=20, height=20)
volPlot
gmdPlot
ctPlot
dev.off()

# Now change the age bin of interest
volData <- doEverythingEver(vol.modal.data, 'mprage_jlf_vol_', 29.1, 55, '29-55',cerebellum=T)
gmdData <- doEverythingEver(gmd.modal.data, 'mprage_jlf_gmd_', 29.1, 55, '29-55',cerebellum=T)
ctData <- doEverythingEverCT(ct.modal.data, 'mprage_jlf_ct_', 29.1, 55, '29-55')

# Now plot em
volPlot <- createGGPlotImage(volData, 'Volume Hi-Lo JLF Data 29-55', -2, 2, .2)
gmdPlot <- createGGPlotImage(gmdData, 'GMD Hi-Lo JLF Data 29-55', -2, 2, .2)
ctPlot <- createGGPlotImage(ctData, 'CT Hi-Lo JLF Data 29-55', -2, 2, .2)

# Now print em
pdf('mgi29-55HiLo.pdf', width=20, height=20)
volPlot
gmdPlot
ctPlot
dev.off()


# Now the highest age bin
volData <- doEverythingEver(vol.modal.data, 'mprage_jlf_vol_', 55.1, 900, '56+',cerebellum=T)
gmdData <- doEverythingEver(gmd.modal.data, 'mprage_jlf_gmd_', 55.1, 900, '56+',cerebellum=T)
ctData <- doEverythingEverCT(ct.modal.data, 'mprage_jlf_ct_', 55.1, 900, '56+')

# Now plot em
volPlot <- createGGPlotImage(volData, 'Volume Hi-Lo JLF Data 56+', -3.5, 3.5, .2)
gmdPlot <- createGGPlotImage(gmdData, 'GMD Hi-Lo JLF Data 56+', -3.5, 3.5, .2)
ctPlot <- createGGPlotImage(ctData, 'CT Hi-Lo JLF Data 56+', -3.5, 3.5, .2)

# Now print em
pdf('mgi56+HiLo.pdf', width=20, height=20)
volPlot
gmdPlot
ctPlot
dev.off()
