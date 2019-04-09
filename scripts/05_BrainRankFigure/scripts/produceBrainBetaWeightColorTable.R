## Load library(s)
source('/home/adrose/hiLo/scripts/05_BrainRankFigure/functions/functions.R')

## Load data
all.dat <- read.csv('../../08_importanceExplore/scripts/ridgeImpMale.csv')
all.dat$X <- as.character(all.dat$X)
## Now I need to fix the meaned hemisphere thing I got going on for the output scripts
left.vals <- all.dat
left.vals$X <- gsub(x=left.vals$X, pattern='mprage_jlf_vol_', replacement='mprage_jlf_vol_L_')
left.vals$X <- gsub(x=left.vals$X, pattern='mprage_jlf_gmd_', replacement='mprage_jlf_gmd_L_')
left.vals$X <- gsub(x=left.vals$X, pattern='pcasl_jlf_cbf_', replacement='pcasl_jlf_cbf_L_')
left.vals$X <- gsub(x=left.vals$X, pattern='dti_jlf_tr_', replacement='dti_jlf_tr_L_')
left.vals$X <- gsub(x=left.vals$X, pattern='rest_jlf_alff_', replacement='rest_jlf_alff_L_')
left.vals$X <- gsub(x=left.vals$X, pattern='rest_jlf_reho_', replacement='rest_jlf_reho_L_')

## Now do the same thing for the right
right.vals <- all.dat
right.vals$X <- gsub(x=right.vals$X, pattern='mprage_jlf_vol_', replacement='mprage_jlf_vol_R_')
right.vals$X <- gsub(x=right.vals$X, pattern='mprage_jlf_gmd_', replacement='mprage_jlf_gmd_R_')
right.vals$X <- gsub(x=right.vals$X, pattern='pcasl_jlf_cbf_', replacement='pcasl_jlf_cbf_R_')
right.vals$X <- gsub(x=right.vals$X, pattern='dti_jlf_tr_', replacement='dti_jlf_tr_R_')
right.vals$X <- gsub(x=right.vals$X, pattern='rest_jlf_alff_', replacement='rest_jlf_alff_R_')
right.vals$X <- gsub(x=right.vals$X, pattern='rest_jlf_reho_', replacement='rest_jlf_reho_R_')

## Now combine all of these
all.dat <- rbind(all.dat, left.vals, right.vals)

## Now produce each modalities beta weight color table
maxVal <- 3
minVal <- -3
grep.vals <- c('vol', 'cbf', 'gmd', 'tr', 'fa', 'all.data', 'reho', 'alff')
for(i in grep.vals){
  # First isolate our values
  toMake <- all.dat[grep(i , all.dat$X.1),]
  # Now produce the color table
  writeColorTableandKey(inputData=toMake, inputColumn=2, outName=paste("male",i, sep=''), minTmp=c(minVal, 0), maxTmp=c(0, maxVal))
}

## Now do females
all.dat <- read.csv('../../08_importanceExplore/scripts/ridgeImpFemale.csv')
all.dat$X <- as.character(all.dat$X)
## Now I need to fix the meaned hemisphere thing I got going on for the output scripts
left.vals <- all.dat
left.vals$X <- gsub(x=left.vals$X, pattern='mprage_jlf_vol_', replacement='mprage_jlf_vol_L_')
left.vals$X <- gsub(x=left.vals$X, pattern='mprage_jlf_gmd_', replacement='mprage_jlf_gmd_L_')
left.vals$X <- gsub(x=left.vals$X, pattern='pcasl_jlf_cbf_', replacement='pcasl_jlf_cbf_L_')
left.vals$X <- gsub(x=left.vals$X, pattern='dti_jlf_tr_', replacement='dti_jlf_tr_L_')
left.vals$X <- gsub(x=left.vals$X, pattern='rest_jlf_alff_', replacement='rest_jlf_alff_L_')
left.vals$X <- gsub(x=left.vals$X, pattern='rest_jlf_reho_', replacement='rest_jlf_reho_L_')

## Now do the same thing for the right
right.vals <- all.dat
right.vals$X <- gsub(x=right.vals$X, pattern='mprage_jlf_vol_', replacement='mprage_jlf_vol_R_')
right.vals$X <- gsub(x=right.vals$X, pattern='mprage_jlf_gmd_', replacement='mprage_jlf_gmd_R_')
right.vals$X <- gsub(x=right.vals$X, pattern='pcasl_jlf_cbf_', replacement='pcasl_jlf_cbf_R_')
right.vals$X <- gsub(x=right.vals$X, pattern='dti_jlf_tr_', replacement='dti_jlf_tr_R_')
right.vals$X <- gsub(x=right.vals$X, pattern='rest_jlf_alff_', replacement='rest_jlf_alff_R_')
right.vals$X <- gsub(x=right.vals$X, pattern='rest_jlf_reho_', replacement='rest_jlf_reho_R_')

## Now combine all of these
all.dat <- rbind(all.dat, left.vals, right.vals)
## Now produce each modalities beta weight color table
maxVal <- 3
minVal <- -3
grep.vals <- c('vol', 'cbf', 'gmd', 'tr', 'fa', 'all.data', 'reho', 'alff')
for(i in grep.vals){
  # First isolate our values
  toMake <- all.dat[grep(i , all.dat$X.1),]
  # Now produce the color table
  writeColorTableandKey(inputData=toMake, inputColumn=2, outName=paste("female",i, sep=''), minTmp=c(minVal, 0), maxTmp=c(0, maxVal))
}
