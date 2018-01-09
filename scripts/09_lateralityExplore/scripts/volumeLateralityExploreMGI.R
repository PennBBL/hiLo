# AFGR December 2017

# This script will be used to explore volume differences in lateral components across 
# the right handed age regrssed volume data

# Load library(s)
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'ggrepel')
source('/home/adrose/hiLo/scripts/09_lateralityExplore/functions/functions.R')

# Now load all the data we need
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/ageRegMGI/volumeData.csv')
#hand.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/preRaw2017/n1601_demographics_go1_20161212.csv')

all.dat <- vol.data
allOut <- NULL
# Now I need to grab all of the right hemisphere ROI's from the right handed people
right.hemi <- all.dat[which(all.dat$sex==1),grep('_R_', names(all.dat))]
left.hemi <- all.dat[which(all.dat$sex==1),grep('_L_', names(all.dat))]

# Now lets run through each ROI and do a t.test
output <- NULL
for(i in 1:dim(right.hemi)[2]){
  tmp <- t.test(x=right.hemi[,i], y=left.hemi[,i], paired=T)
  outRow <- c(names(right.hemi)[i], tmp$statistic, tmp$p.value, 'rightMale')
  #print(outRow)
  output <- rbind(output, outRow)
}
output <- cbind(output, p.adjust(as.numeric(output[,3]), method='fdr'))
outputMale <- as.data.frame(output)
allOut <- rbind(allOut, output)
# Now write a itk snap image for these guys
writeColorTableandKey(inputData=output, inputColumn=2, outName='rightMale')


# Now do female right hand
right.hemi <- all.dat[which(all.dat$sex==2),grep('_R_', names(all.dat))]
left.hemi <- all.dat[which(all.dat$sex==2),grep('_L_', names(all.dat))]

# Now lets run through each ROI and do a t.test
output <- NULL
for(i in 1:dim(right.hemi)[2]){
  tmp <- t.test(x=right.hemi[,i], y=left.hemi[,i], paired=T)
  outRow <- c(names(right.hemi)[i], tmp$statistic, tmp$p.value, 'rightFemale')
  #print(outRow)
  output <- rbind(output, outRow)
}
output <- cbind(output, p.adjust(as.numeric(output[,3]), method='fdr'))
outputFemale <- as.data.frame(output)
allOut <- rbind(allOut, output)
# Now write a itk snap image for these guys
writeColorTableandKey(inputData=output, inputColumn=2, outName='rightFemale')


# Now plot our t values males vs females
toplot <- merge(outputMale, outputFemale, by="V1", suffixes=c(".male", ".female"))
toplot$V1 <- gsub(x=toplot$V1, pattern='mprage_jlf_vol_R_', replacement='')
toplot$t.male <- as.numeric(as.character(toplot$t.male))
toplot$t.female <- as.numeric(as.character(toplot$t.female))
outPlot <- ggplot(toplot, aes(x=t.male, y=t.female)) + geom_point() +  geom_label_repel(aes(label=V1,size=3.5),box.padding=unit(0.35,"lines"),point.padding=unit(0.5,"lines"))
pdf('testLater.pdf', height=14, width=14)
print(outPlot)
dev.off()
