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
vol.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/volumeData.csv')
cbf.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/cbfData.csv')
gmd.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/gmdData.csv')
ct.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/ctData.csv')
reho.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/rehoData.csv')
alff.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/alffData.csv')
ad.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfADData.csv')
fa.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfFAData.csv')
rd.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfRDData.csv')
tr.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfTRData.csv')
vol.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
cbf.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv')
gmd.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
ct.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/ctData.csv')
reho.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/rehoData.csv')
alff.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/alffData.csv')
ad.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfADData.csv')
fa.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfFAData.csv')
rd.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfRDData.csv')
tr.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv')

# Now add age bins
vol.modal.data <- addAgeBin(vol.modal.data, vol.modal.data$ageAtGo1Scan, 167, 215, 216)
cbf.modal.data <- addAgeBin(cbf.modal.data, cbf.modal.data$ageAtGo1Scan, 167, 215, 216)
gmd.modal.data <- addAgeBin(gmd.modal.data, gmd.modal.data$ageAtGo1Scan, 167, 215, 216)
ct.modal.data <- addAgeBin(ct.modal.data, ct.modal.data$ageAtGo1Scan, 167, 215, 216)
reho.modal.data <- addAgeBin(reho.modal.data, reho.modal.data$ageAtGo1Scan, 167, 215, 216)
alff.modal.data <- addAgeBin(alff.modal.data, alff.modal.data$ageAtGo1Scan, 167, 215, 216)
ad.modal.data <- addAgeBin(ad.modal.data, ad.modal.data$ageAtGo1Scan, 167, 215, 216)
fa.modal.data <- addAgeBin(fa.modal.data, fa.modal.data$ageAtGo1Scan, 167, 215, 216)
rd.modal.data <- addAgeBin(rd.modal.data, rd.modal.data$ageAtGo1Scan, 167, 215, 216)
tr.modal.data <- addAgeBin(tr.modal.data, tr.modal.data$ageAtGo1Scan, 167, 215, 216)
cbf.modal.data.age.reg$ageBin <- rep('Age Regressed', nrow(cbf.modal.data.age.reg))
vol.modal.data.age.reg$ageBin <- rep('Age Regressed', nrow(vol.modal.data.age.reg))
gmd.modal.data.age.reg$ageBin <- rep('Age Regressed', nrow(gmd.modal.data.age.reg))
#gmd.modal.data.age.reg <- addAgeBin(gmd.modal.data.age.reg, gmd.modal.data.age.reg$ageAtGo1Scan, 167, 215, 216)
ct.modal.data.age.reg$ageBin <- rep('Age Regressed', nrow(ct.modal.data.age.reg))
reho.modal.data.age.reg$ageBin <- rep('Age Regressed', nrow(reho.modal.data.age.reg))
alff.modal.data.age.reg$ageBin <- rep('Age Regressed', nrow(alff.modal.data.age.reg))
ad.modal.data.age.reg$ageBin <- 'Age Regressed'
fa.modal.data.age.reg$ageBin <- 'Age Regressed'
rd.modal.data.age.reg$ageBin <- 'Age Regressed'
tr.modal.data.age.reg$ageBin <- 'Age Regressed'

## Now prep the data 
# Start with volume
child.volume <- doEverythingEver(vol.modal.data, 'mprage_jlf_vol', 0, 167, 'Childhood', cerebellum=F)
adol.volume <- doEverythingEver(vol.modal.data, 'mprage_jlf_vol', 168, 215, 'Adolescence', cerebellum=F)
adult.volume <- doEverythingEver(vol.modal.data, 'mprage_jlf_vol', 216, 999, 'Early Adulthood', cerebellum=F)
all.vol <- rbind(child.volume, adol.volume, adult.volume)
# Now produce the age reg values
age.reg.vol <- doEverythingEver(vol.modal.data.age.reg, 'mprage_jlf_vol', 0, 999, 'Age Regressed', cerebellum=F)


# Now move to cbf
child.cbf <- doEverythingEver(cbf.modal.data, 'pcasl_jlf_cbf', 0, 167, 'Childhood', cerebellum=F)
adol.cbf <- doEverythingEver(cbf.modal.data, 'pcasl_jlf_cbf', 168, 215, 'Adolescence', cerebellum=F)
adult.cbf <- doEverythingEver(cbf.modal.data, 'pcasl_jlf_cbf', 216, 999, 'Early Adulthood', cerebellum=F)
all.cbf <- rbind(child.cbf, adol.cbf, adult.cbf)
# Now produce the age reg values
age.reg.cbf <- doEverythingEver(cbf.modal.data.age.reg, 'pcasl_jlf_cbf', 0, 999, 'Age Regressed', cerebellum=F)

# Now do GMD
child.gmd <- doEverythingEver(gmd.modal.data, 'mprage_jlf_gmd', 0, 167, 'Childhood', cerebellum=F)
adol.gmd <- doEverythingEver(gmd.modal.data, 'mprage_jlf_gmd', 168, 215, 'Adolescence', cerebellum=F)
adult.gmd <- doEverythingEver(gmd.modal.data, 'mprage_jlf_gmd', 216, 999, 'Early Adulthood', cerebellum=F)
all.gmd <- rbind(child.gmd, adol.gmd, adult.gmd)
# Now do age regressed
age.reg.gmd <- doEverythingEver(gmd.modal.data.age.reg, 'mprage_jlf_gmd', 0, 999, 'Age Regressed', cerebellum=F)
#child.gmd.reg <- doEverythingEver(gmd.modal.data.age.reg, 'mprage_jlf_gmd', 0, 167, 'Childhood')
#adol.gmd.reg <- doEverythingEver(gmd.modal.data.age.reg, 'mprage_jlf_gmd', 168, 215, 'Adolescence')
#adult.gmd.reg <- doEverythingEver(gmd.modal.data.age.reg, 'mprage_jlf_gmd', 216, 999, 'Early Adulthood')
#age.reg.gmd <- rbind(child.gmd.reg, adol.gmd.reg, adult.gmd.reg)

# Now try CT - disclaimer I don't think this will pass organizeLobe and other bs
child.ct <- doEverythingEverCT(ct.modal.data, 'mprage_jlf_ct', 0, 167, 'Childhood')
adol.ct <- doEverythingEverCT(ct.modal.data, 'mprage_jlf_ct', 168, 215, 'Adolescence')
adult.ct <- doEverythingEverCT(ct.modal.data, 'mprage_jlf_ct', 216, 999, 'Early Adulthood')
all.ct <- rbind(child.ct, adol.ct, adult.ct)
# Now do age regressed
age.reg.ct <- doEverythingEverCT(ct.modal.data.age.reg, 'mprage_jlf_ct', 0, 999, 'Age Regressed')

# Now do reho
child.rh <- doEverythingEver(reho.modal.data, 'rest_jlf_reho', 0, 167, 'Childhood', cerebellum=F)
adol.rh <- doEverythingEver(reho.modal.data, 'rest_jlf_reho', 168, 215, 'Adolescence', cerebellum=F)
adult.rh <- doEverythingEver(reho.modal.data, 'rest_jlf_reho', 216, 999, 'Early Adulthood', cerebellum=F)
all.rh <- rbind(child.rh, adol.rh, adult.rh)
# Now do age regressed
age.reg.rh <- doEverythingEver(reho.modal.data.age.reg, 'rest_jlf_reho', 0, 999, 'Age Regressed', cerebellum=F)


# Now ALFF
child.al <- doEverythingEver(alff.modal.data, 'rest_jlf_alff', 0, 167, 'Childhood', cerebellum=F)
adol.al <- doEverythingEver(alff.modal.data, 'rest_jlf_alff', 168, 215, 'Adolescence', cerebellum=F)
adult.al <- doEverythingEver(alff.modal.data, 'rest_jlf_alff', 216, 999, 'Early Adulthood', cerebellum=F)
all.al <- rbind(child.al, adol.al, adult.al)
# Now do age regressed
age.reg.al <- doEverythingEver(alff.modal.data.age.reg, 'rest_jlf_alff', 0, 999, 'Age Regressed', cerebellum=F)

# Now AD
child.ad <- doEverythingEver(ad.modal.data, 'dti_jlf_ad', 0, 167, 'Childhood', cerebellum=F)
adol.ad <- doEverythingEver(ad.modal.data, 'dti_jlf_ad', 168, 215, 'Adolescence', cerebellum=F)
adult.ad <- doEverythingEver(ad.modal.data, 'dti_jlf_ad', 216, 999, 'Early Adulthood', cerebellum=F)
all.ad <- rbind(child.ad, adol.ad, adult.ad)
# Now age reg
age.reg.ad <- doEverythingEver(ad.modal.data.age.reg, 'dti_jlf_ad', 0, 167, 'Age Regressed', cerebellum=F)

# Now onto RD
child.rd <- doEverythingEver(rd.modal.data, 'dti_jlf_rd', 0, 167, 'Childhood', cerebellum=F)
adol.rd <- doEverythingEver(rd.modal.data, 'dti_jlf_rd', 168, 215, 'Adolescence', cerebellum=F)
adult.rd <- doEverythingEver(rd.modal.data, 'dti_jlf_rd', 216, 999, 'Early Adulthood', cerebellum=F)
all.rd <- rbind(child.rd, adol.rd, adult.rd)
# Now age reg
age.reg.rd <- doEverythingEver(rd.modal.data.age.reg, 'dti_jlf_rd', 0, 167, 'Age Regressed', cerebellum=F)

# Now onto TR
child.tr <- doEverythingEver(tr.modal.data, 'dti_jlf_tr', 0, 167, 'Childhood', cerebellum=F)
adol.tr <- doEverythingEver(tr.modal.data, 'dti_jlf_tr', 168, 215, 'Adolescence', cerebellum=F)
adult.tr <- doEverythingEver(tr.modal.data, 'dti_jlf_tr', 216, 999, 'Early Adulthood', cerebellum=F)
all.tr <- rbind(child.tr, adol.tr, adult.tr)
# Now age reg
age.reg.tr <- doEverythingEver(tr.modal.data.age.reg, 'dti_jlf_tr', 0, 167, 'Age Regressed', cerebellum=F)

# Now plot dis shiiizzz
volPlot <- createGGPlotImage(all.vol, 'Volume Hi-Lo JLF Data', -.8, 1.4, .2)
cbfPlot <- createGGPlotImage(all.cbf, 'CBF Hi-Lo JLF Data', -.8, .8, .2)
gmdPlot <- createGGPlotImage(all.gmd, 'GMD Hi-Lo JLF Data', -.8, 1.4, .2)
ctPlot <- createGGPlotImage(all.ct, 'CT Hi-Lo JLF Data', -.8, 1.4, .2)
rhPlot <- createGGPlotImage(all.rh, 'ReHo Hi-Lo JLF Data', -.8, 1.4, .2)
alPlot <- createGGPlotImage(all.al, 'ALFF Hi-Lo JLF Data', -.8, 1.4, .2)
adPlot <- createGGPlotImage(all.ad, 'AD Hi-Lo JLF Data', -1.6 , 1.2, .2)
rdPlot <- createGGPlotImage(all.rd, 'RD Hi-Lo JLF Data', -1.6, 1.2, .2)
trPlot <- createGGPlotImage(all.tr, 'TR Hi-Lo JLF Data', -1.6, 1.2, .2)

volPlotAgeReg <- createGGPlotImage(age.reg.vol, 'Volume Hi-Lo JLF Data Age Reg', -1, 1, .2)
cbfPlotAgeReg <- createGGPlotImage(age.reg.cbf, 'CBF Hi-Lo JLF Data Age Reg', -1, 1, .2)
gmdPlotAgeReg <- createGGPlotImage(age.reg.gmd, 'GMD Hi-Lo JLF Data Age Reg', -1, 1, .2)
ctPlotAgeReg <- createGGPlotImage(age.reg.ct, 'CT Hi-Lo JLF Data Age Reg', -1, 1, .2)
rhPlotAgeReg <- createGGPlotImage(age.reg.rh, 'ReHo Hi-Lo JLF Data Age Reg', -1, 1, .2)
alPlotAgeReg <- createGGPlotImage(age.reg.al, 'ALFF Hi-Lo JLF Data Age Reg', -1, 1, .2)
adPlotAgeReg <- createGGPlotImage(age.reg.ad, 'AD Hi-Lo JLF Data Age Reg', -1 , 1, .2)
rdPlotAgeReg <- createGGPlotImage(age.reg.rd, 'RD Hi-Lo JLF Data Age Reg', -1, 1, .2)
trPlotAgeReg <- createGGPlotImage(age.reg.tr, 'TR Hi-Lo JLF Data Age Reg', -1, 1, .2)

# Now print our output
pdf('noAgeRegHi-LoGraphs.pdf', width=20, height=20)
volPlot
cbfPlot
gmdPlot
ctPlot
rhPlot
alPlot
adPlot
rdPlot
trPlot
dev.off()

pdf('ageRegHi-LoGraphs.pdf', width=20, height=20)
volPlotAgeReg
cbfPlotAgeReg
gmdPlotAgeReg
ctPlotAgeReg
rhPlotAgeReg
alPlotAgeReg
adPlotAgeReg
rdPlotAgeReg
trPlotAgeReg
dev.off()


### Now do the WM labels down here 

# Change the workhorse function
doEverythingEver <- function(df, modalityGrepPattern, lowerAge.e, upperAge.e, ageBinName.e){
  #tmp <- addAgeBins(df$ageAtGo1Scan, df, lowerAge.e, upperAge.e, ageBinName.e)
  tmp <- standardizePerfGroups(df, modalityGrepPattern, ageBinName.e)
  tmp <- organizeWM1ROINames(tmp)
  tmp <- subtractHiFromLo(tmp)
  tmp$gender <- revalue(tmp$gender, c('1'='Male', '2'='Female'))
  tmp$meanValue <- as.numeric(as.character(tmp$meanValue))
  colnames(tmp)[6] <- 'Gender'
  levels(tmp$ageBin) <- rev(levels(tmp$ageBin))
  return(tmp)
}
# STart by loading the data 
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jhuADLabelsData.csv')
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jhuFALabelsData.csv')
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jhuRDLabelsData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jhuTRLabelsData.csv')

# Now add age bin
ad.data <- addAgeBin(ad.data, ad.data$ageAtGo1Scan, 167, 215, 216)
fa.data <- addAgeBin(fa.data, fa.data$ageAtGo1Scan, 167, 215, 216)
rd.data <- addAgeBin(rd.data, rd.data$ageAtGo1Scan, 167, 215, 216)
tr.data <- addAgeBin(tr.data, tr.data$ageAtGo1Scan, 167, 215, 216)

# Now process deeez guys 
# Start with AD
colnames(ad.data)[69] <- 'dti_dtitk_jhulabel_ad_rlic'
child.ad <- doEverythingEver(ad.data, 'dti_dtitk_jhulabel_ad', 0, 167, 'Childhood')
adol.ad <- doEverythingEver(ad.data, 'dti_dtitk_jhulabel_ad', 168, 215, 'Adolescence')
adult.ad <- doEverythingEver(ad.data, 'dti_dtitk_jhulabel_ad', 216, 999, 'Early Adulthood')
all.ad <- rbind(child.ad, adol.ad, adult.ad)
levels(all.ad$ageBin) <- c("Early Adulthood","Adolescence","Childhood")
# Now FA
colnames(fa.data)[69] <- 'dti_dtitk_jhulabel_fa_rlic'
child.fa <- doEverythingEver(fa.data, 'dti_dtitk_jhulabel_fa', 0, 167, 'Childhood')
adol.fa <- doEverythingEver(fa.data, 'dti_dtitk_jhulabel_fa', 168, 215, 'Adolescence')
adult.fa <- doEverythingEver(fa.data, 'dti_dtitk_jhulabel_fa', 216, 999, 'Early Adulthood')
all.fa <- rbind(child.fa, adol.fa, adult.fa)
levels(all.fa$ageBin) <- c("Early Adulthood","Adolescence","Childhood")
# Now RD
child.rd <- doEverythingEver(rd.data, 'dti_dtitk_jhulabels_rd', 0, 167, 'Childhood')
adol.rd <- doEverythingEver(rd.data, 'dti_dtitk_jhulabels_rd', 168, 215, 'Adolescence')
adult.rd <- doEverythingEver(rd.data, 'dti_dtitk_jhulabels_rd', 216, 999, 'Early Adulthood')
all.rd <- rbind(child.rd, adol.rd, adult.rd)
levels(all.rd$ageBin) <- c("Early Adulthood","Adolescence","Childhood")
# Now do tr data
colnames(tr.data)[69] <- "dti_dtitk_jhulabel_tr_rlic"
child.tr <- doEverythingEver(tr.data, 'dti_dtitk_jhulabel_tr', 0, 167, 'Childhood')
adol.tr <- doEverythingEver(tr.data, 'dti_dtitk_jhulabel_tr', 168, 215, 'Adolescence')
adult.tr <- doEverythingEver(tr.data, 'dti_dtitk_jhulabel_tr', 216, 999, 'Early Adulthood')
all.tr <- rbind(child.tr, adol.tr, adult.tr)
levels(all.tr$ageBin) <- c("Early Adulthood","Adolescence","Childhood")
# Now plot em 
adPlot <- createGGPlotImage(all.ad, 'AD Hi-Lo JLF Data', -.6 , 1, .2)
faPlot <- createGGPlotImage(all.fa, 'FA Hi-Lo JLF Data', -1 , 1, .2)
rdPlot <- createGGPlotImage(all.rd, 'RD Hi-Lo JLF Data', -.8, 1, .2)
trPlot <- createGGPlotImage(all.tr, 'TR Hi-Lo JLF Data', -.7, 1, .2)

# Now print em out 
pdf('noAgeRegHi-LoGraphs-LoWMLabels.pdf', width=20, height=20)
adPlot
faPlot
rdPlot
trPlot
dev.off()

# Now do age regressed everything
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuADLabelsData.csv')
colnames(ad.data)[69] <- 'dti_dtitk_jhulabel_ad_rlic'
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuFALabelsData.csv')
colnames(fa.data)[69] <- 'dti_dtitk_jhulabel_fa_rlic'
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuRDLabelsData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jhuTRLabelsData.csv')
colnames(tr.data)[69] <- "dti_dtitk_jhulabel_tr_rlic"
ad.data$ageBin <- 'Age Regressed'
fa.data$ageBin <- 'Age Regressed'
rd.data$ageBin <- 'Age Regressed'
tr.data$ageBin <- 'Age Regressed'
ad.data.age.reg <- doEverythingEver(ad.data, 'dti_dtitk_jhulabel_ad', 0, 167, 'Age Regressed')
fa.data.age.reg <- doEverythingEver(fa.data, 'dti_dtitk_jhulabel_fa', 0, 167, 'Age Regressed')
rd.data.age.reg <- doEverythingEver(rd.data, 'dti_dtitk_jhulabels_rd', 0, 167, 'Age Regressed')
tr.data.age.reg <- doEverythingEver(tr.data, 'dti_dtitk_jhulabel_tr', 0, 167, 'Age Regressed')
adPlot <- createGGPlotImage(ad.data.age.reg, 'AD Hi-Lo JLF Data Age Reg', -.6 , 1, .2)
faPlot <- createGGPlotImage(fa.data.age.reg, 'FA Hi-Lo JLF Data Age Reg', -1 , 1, .2)
rdPlot <- createGGPlotImage(rd.data.age.reg, 'RD Hi-Lo JLF Data Age Reg', -.8, 1, .2)
trPlot <- createGGPlotImage(tr.data.age.reg, 'TR Hi-Lo JLF Data Age Reg', -.7, 1, .2)

# Now print em out 
pdf('ageRegHi-LoGraphs-LoWMLabels.pdf', width=20, height=20)
adPlot
faPlot
rdPlot
trPlot
dev.off()
