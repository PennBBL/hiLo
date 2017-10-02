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
gmd.modal.data$F1_Exec_Comp_Cog_Accuracy <- gmd.modal.data$Overall_Efficiency
ct.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRMGI/ctData.csv')
ct.modal.data$F1_Exec_Comp_Cog_Accuracy <- ct.modal.data$Overall_Efficiency
cc.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRMGI/ccData.csv')
cc.modal.data$F1_Exec_Comp_Cog_Accuracy <- cc.modal.data$Overall_Efficiency
vol.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegMGI/volumeData.csv')
vol.modal.data.age.reg$F1_Exec_Comp_Cog_Accuracy <- vol.modal.data.age.reg$Overall_Efficiency
gmd.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegMGI/gmdData.csv')
gmd.modal.data.age.reg$F1_Exec_Comp_Cog_Accuracy <- gmd.modal.data.age.reg$Overall_Efficiency
ct.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegMGI/ctData.csv')
ct.modal.data.age.reg$F1_Exec_Comp_Cog_Accuracy <- ct.modal.data.age.reg$Overall_Efficiency
cc.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeRegMGI/ccData.csv')
cc.modal.data.age.reg$F1_Exec_Comp_Cog_Accuracy <- cc.modal.data.age.reg$Overall_Efficiency

vol.modal.data.age.reg$ageBin <- 'Age Regressed'
gmd.modal.data.age.reg$ageBin <- 'Age Regressed'
ct.modal.data.age.reg$ageBin <- 'Age Regressed'
cc.modal.data.age.reg$ageBin <- 'Age Regressed'


vol.modal.data$ageBin <- 'Age Regressed'
gmd.modal.data$ageBin <- 'Age Regressed'
ct.modal.data$ageBin <- 'Age Regressed'
cc.modal.data$ageBin <- 'Age Regressed'


# Now produce the age reg values
age.reg.vol <- doEverythingEver(vol.modal.data.age.reg, 'mprage_jlf_vol', 0, 999, 'Age Regressed', cerebellum=F,optionalRace=NULL)
# Now do GMD
age.reg.gmd <- doEverythingEver(gmd.modal.data.age.reg, 'mprage_jlf_gmd', 0, 999, 'Age Regressed', cerebellum=F,optionalRace=NULL)
#Lets do CT
age.reg.ct <- doEverythingEverCT(ct.modal.data.age.reg, 'mprage_jlf_ct', 0, 999, 'Age Regressed',optionalRace=NULL)
## CC, i thought you would never ask 
age.reg.cc <- doEverythingEverCT(cc.modal.data.age.reg, 'mprage_jlf_cortcon', 0, 999, 'Age Regressed',optionalRace=NULL)

volPlotAgeReg <- createGGPlotImage(age.reg.vol, 'Volume Hi-Lo JLF Data Age Reg', -1, 1, .2)
gmdPlotAgeReg <- createGGPlotImage(age.reg.gmd, 'GMD Hi-Lo JLF Data Age Reg', -1, 1, .2)
ctPlotAgeReg <- createGGPlotImage(age.reg.ct, 'CT Hi-Lo JLF Data Age Reg', -1, 1, .2)
ccPlotAgeReg <- createGGPlotImage(age.reg.cc, 'CC Hi-Lo JLF Data Age Reg', -1.2, 1, .2)

pdf('ageRegHi-LoGraphsMGI.pdf', width=20, height=20)
volPlotAgeReg
gmdPlotAgeReg
ctPlotAgeReg
ccPlotAgeReg
dev.off()
