# AFGR June 2017

# This script will be used to produce intermodal heat maps for the hi lo project

#Load library(s)
source("/home/adrose/hiLo/scripts/06_IntermodalCor/functions/functions.R")
install_load('ggplot2','scales', 'corrplot', 'psych', 'reshape2')

## Load data here
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/volumeData.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/cbfData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/gmdData.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/ctData.csv')
cc.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/ccData.csv') 
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/rehoData.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/alffData.csv')
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfADData.csv')
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfFAData.csv')
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfRDData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfTRData.csv')

# Now merge all of the data
all.data <- merge(vol.data, cbf.data, by=intersect(names(vol.data), names(cbf.data)), all=T)
all.data <- merge(all.data, gmd.data, by=intersect(names(all.data), names(gmd.data)), all=T)
all.data <- merge(all.data, ct.data, by=intersect(names(all.data), names(ct.data)), all=T)
all.data <- merge(all.data, cc.data, by=intersect(names(all.data), names(cc.data)), all=T)
all.data <- merge(all.data, reho.data, by=intersect(names(all.data), names(reho.data)), all=T)
all.data <- merge(all.data, alff.data, by=intersect(names(all.data), names(alff.data)), all=T)
#all.data <- merge(all.data, ad.data, by=intersect(names(all.data), names(ad.data)), all=T)
#all.data <- merge(all.data, fa.data, by=intersect(names(all.data), names(fa.data)), all=T)
#all.data <- merge(all.data, rd.data, by=intersect(names(all.data), names(rd.data)), all=T)
all.data <- merge(all.data, tr.data, by=intersect(names(all.data), names(tr.data)), all=T)

# Now produce all of the heat maps
grepPattern1 <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_', 'mprage_jlf_ct_','mprage_jlf_cortcon_', 'rest_jlf_reho_', 'rest_jlf_alff_','dti_jlf_tr_')
for(p in 1:length(grepPattern1)){
  modalName <- rev(strSplitMatrixReturn(grepPattern1[p], '_'))[1]
  pdf(paste(modalName, '-heatMaps.pdf', sep=''), width=20, height=20)
  for(n in 1:length(grepPattern1)){
    tmp <- createHeatMap(grepPattern1[p], grepPattern1[n]) 
    print(tmp) 
  }
  dev.off()
}

for(p in 1:length(grepPattern1)){
  modalName <- rev(strSplitMatrixReturn(grepPattern1[p], '_'))[1]
  pdf(paste(modalName, '-sigHeatMaps.pdf', sep=''), width=20, height=20)
  for(n in 1:length(grepPattern1)){
    tmp <- createSigHeatMap(grepPattern1[p], grepPattern1[n]) 
    print(tmp) 
  }
  dev.off()
}
