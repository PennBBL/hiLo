# AFGR December 2017

# This script will be used to explore volume differences in lateral components across 
# the right handed age regrssed volume data

# Load library(s)
install_load('ggplot2')
source('/home/adrose/hiLo/scripts/09_lateralityExplore/functions/functions.R')

# Now load all the data we need
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageReg/volumeData.csv')
hand.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_demographics_go1_20161212.csv')

all.dat <- merge(vol.data, hand.data)

# Now I need to grab all of the right hemisphere ROI's from the right handed people
right.hemi <- all.dat[which(all.dat$handednessv2==1 & all.dat$sex==1),grep('_R_', names(all.dat))]
left.hemi <- all.dat[which(all.dat$handednessv2==1 & all.dat$sex==1),grep('_L_', names(all.dat))]

# Now lets run through each ROI and do a t.test
output <- NULL
for(i in 1:dim(right.hemi)[2]){
  tmp <- t.test(x=right.hemi[,i], y=left.hemi[,i], paired=T)
  outRow <- c(names(right.hemi)[i], tmp$statistic, tmp$p.value)
  #print(outRow)
  output <- rbind(output, outRow)
}
output <- cbind(output, p.adjust(as.numeric(output[,3]), method='fdr'))
# Now write a itk snap image for these guys
writeColorTableandKey(inputData=output, inputColumn=2, outName='rightMale')


# Now do female right hand
right.hemi <- all.dat[which(all.dat$handednessv2==1 & all.dat$sex==2),grep('_R_', names(all.dat))]
left.hemi <- all.dat[which(all.dat$handednessv2==1 & all.dat$sex==2),grep('_L_', names(all.dat))]

# Now lets run through each ROI and do a t.test
output <- NULL
for(i in 1:dim(right.hemi)[2]){
  tmp <- t.test(x=right.hemi[,i], y=left.hemi[,i], paired=T)
  outRow <- c(names(right.hemi)[i], tmp$statistic, tmp$p.value)
  #print(outRow)
  output <- rbind(output, outRow)
}
output <- cbind(output, p.adjust(as.numeric(output[,3]), method='fdr'))
# Now write a itk snap image for these guys
writeColorTableandKey(inputData=output, inputColumn=2, outName='rightFemale')

# Now do male left handers
right.hemi <- all.dat[which(all.dat$handednessv2>1 & all.dat$sex==1),grep('_R_', names(all.dat))]
left.hemi <- all.dat[which(all.dat$handednessv2>1 & all.dat$sex==1),grep('_L_', names(all.dat))]

# Now lets run through each ROI and do a t.test
output <- NULL
for(i in 1:dim(right.hemi)[2]){
  tmp <- t.test(x=right.hemi[,i], y=left.hemi[,i], paired=T)
  outRow <- c(names(right.hemi)[i], tmp$statistic, tmp$p.value)
  #print(outRow)
  output <- rbind(output, outRow)
}
output <- cbind(output, p.adjust(as.numeric(output[,3]), method='fdr'))
# Now write a itk snap image for these guys
writeColorTableandKey(inputData=output, inputColumn=2, outName='leftMale')

# Now finally left female
right.hemi <- all.dat[which(all.dat$handednessv2>1 & all.dat$sex==2),grep('_R_', names(all.dat))]
left.hemi <- all.dat[which(all.dat$handednessv2>1 & all.dat$sex==2),grep('_L_', names(all.dat))]

# Now lets run through each ROI and do a t.test
output <- NULL
for(i in 1:dim(right.hemi)[2]){
  tmp <- t.test(x=right.hemi[,i], y=left.hemi[,i], paired=T)
  outRow <- c(names(right.hemi)[i], tmp$statistic, tmp$p.value)
  #print(outRow)
  output <- rbind(output, outRow)
}
output <- cbind(output, p.adjust(as.numeric(output[,3]), method='fdr'))
# Now write a itk snap image for these guys
writeColorTableandKey(inputData=output, inputColumn=2, outName='leftFemale')
