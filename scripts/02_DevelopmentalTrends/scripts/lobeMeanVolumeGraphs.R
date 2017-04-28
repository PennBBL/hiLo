# AFGR May 20th 2016

# This script is going to be used to produce some mean bar graphs
# It will produce the mean volume for the 4 lobes by WM and GM
# This will also include the deep gm areas

## Load library(s)
source("/home/adrose/R/x86_64-redhat-linux-gnu-library/helperFunctions/afgrHelpFunc.R")
install_load('ggplot2','scales')

## Load the data
vol.data <- read.csv('/home/analysis/redcap_data/201602/go1/n1601_go1_datarel_020716.csv')

## Prepare the age bins
vol.data$ageBin <- rep("NA", nrow(vol.data))
vol.data$ageBin[which(vol.data$ageAtGo1Scan < 168)] <- "CH"
vol.data$ageBin[which(vol.data$ageAtGo1Scan >= 168 & vol.data$ageAtGo1Scan < 216)] <- "PP"
vol.data$ageBin[which(vol.data$ageAtGo1Scan >= 216)] <- "YA"



## Declare the lobe regions
all.vol.regions <- grep('mprage_mars_vol', names(vol.data))

# Start with the frontal lobe
frontal.areas <- NULL
frontal.areas.rois <- c("FO","MFC","MOrG","POrG","OrIFG","TrIFG","AOrG","OpIFG","Gre",
                        "FRP","LOrG","PrG","MSFG","SMC","MFG","SFG","ACgG")
# In this for loop we will append the roi column indices to the frontal lobe variable
for(roi.name in frontal.areas.rois){
  frontal.areas <- append(frontal.areas, grep(roi.name, names(vol.data)[grep('mprage_mars_vol', names(vol.data))]))
  print(grep(roi.name, names(vol.data)[grep('mprage_mars_vol', names(vol.data))]))
}
# Now make sure we don't have any repeats 
frontal.areas <- unique(frontal.areas)
frontal.areas <- all.vol.regions[frontal.areas]

# Now do the parietal lobe
parietal.areas <- NULL
parietal.areas.rois <- c("Pcu","PoG","AnG","PO","SPL","MPrG","SMG","MPoG","PCgG","MCgG")
for(roi.name in parietal.areas.rois){
  parietal.areas <- append(parietal.areas, grep(roi.name, names(vol.data)[grep('mprage_mars_vol', names(vol.data))])) 
}
parietal.areas <- unique(parietal.areas)
parietal.areas <- all.vol.regions[parietal.areas]

# Now do the temporal lobe
temporal.areas <- NULL
temporal.areas.rois <- c("FuG","PT","PP","ITG","CO","MTG","TMP",
                         "STG","TTG","PHG","Pins","Ains","Ent")
for(roi.name in temporal.areas.rois){
  temporal.areas <- append(temporal.areas, grep(roi.name, names(vol.data)[grep('mprage_mars_vol', names(vol.data))])) 
}
temporal.areas <- unique(temporal.areas)
temporal.areas <- all.vol.regions[temporal.areas]

# Now do the occipital lobe
occipital.areas <- NULL
occipital.areas.rois <- c("IOG","Cun","LiG","OFuG","MOG","Calc","OCP","SOG")
for(roi.name in occipital.areas.rois){
  occipital.areas <- append(occipital.areas, grep(roi.name, names(vol.data)[grep('mprage_mars_vol', names(vol.data))]))  
}
occipital.areas <- unique(occipital.areas)
occipital.areas <- all.vol.regions[occipital.areas]


# Now do the deep gm
deep.gm.areas <- NULL
deep.gm.areas.rois <- c("Hip","SCA","Amygdala","Thal","Putamen",
                      "Caudate","Pallidum","Accumbens","BasFor")
for(roi.name in deep.gm.areas.rois){
  deep.gm.areas <- append(deep.gm.areas, grep(roi.name, names(vol.data)[grep('mprage_mars_vol', names(vol.data))]))  
  print(grep(roi.name, names(vol.data)[grep('mprage_mars_vol', names(vol.data))]))
}
deep.gm.areas <- unique(deep.gm.areas)
deep.gm.areas <- all.vol.regions[deep.gm.areas]


# Now do the averages across rows 
vol.data$frontal.mean <- apply(vol.data[,frontal.areas], 1, function(x) mean(sum(x), na.rm=T))
vol.data$parietal.mean <- apply(vol.data[,parietal.areas],1, function(x) mean(sum(x), na.rm=T))
vol.data$temporal.mean <- apply(vol.data[,temporal.areas],1, function(x) mean(sum(x), na.rm=T))
vol.data$occipital.mean <- apply(vol.data[,occipital.areas],1, function(x) mean(sum(x), na.rm=T))
vol.data$deep.gm.mean <- apply(vol.data[,deep.gm.areas],1, function(x) mean(sum(x), na.rm=T))

# Now produce summarySE's for each lobe for each gender
# Start with males
m.1 <- summarySE(vol.data[which(vol.data$sex==1),], groupvars='ageBin',
                 measurevar='frontal.mean', na.rm=T)
colnames(m.1)[3] <- 'mean'
m.2 <- summarySE(vol.data[which(vol.data$sex==1),], groupvars='ageBin',
                 measurevar='parietal.mean', na.rm=T)
colnames(m.2)[3] <- 'mean'
m.3 <- summarySE(vol.data[which(vol.data$sex==1),], groupvars='ageBin',
                 measurevar='temporal.mean', na.rm=T)
colnames(m.3)[3] <- 'mean'
m.4 <- summarySE(vol.data[which(vol.data$sex==1),], groupvars='ageBin',
                 measurevar='occipital.mean', na.rm=T)
colnames(m.4)[3] <- 'mean'
m.5 <- summarySE(vol.data[which(vol.data$sex==1),], groupvars='ageBin',
                 measurevar='deep.gm.mean', na.rm=T)
colnames(m.5)[3] <- 'mean'

male.means <- rbind(m.1, m.2, m.3, m.4, m.5)
lobe <- c(rep('Frontal', 3), rep('Parietal', 3), rep('Temporal', 3),
          rep('Occipital', 3), rep('DGM', 3))
male.means <- cbind(male.means, lobe)
# Now do Females
f.1 <- summarySE(vol.data[which(vol.data$sex==2),], groupvars='ageBin',
                 measurevar='frontal.mean', na.rm=T)
colnames(f.1)[3] <- 'mean'
f.2 <- summarySE(vol.data[which(vol.data$sex==2),], groupvars='ageBin',
                 measurevar='parietal.mean', na.rm=T)
colnames(f.2)[3] <- 'mean'
f.3 <- summarySE(vol.data[which(vol.data$sex==2),], groupvars='ageBin',
                 measurevar='temporal.mean', na.rm=T)
colnames(f.3)[3] <- 'mean'
f.4 <- summarySE(vol.data[which(vol.data$sex==2),], groupvars='ageBin',
                 measurevar='occipital.mean', na.rm=T)
colnames(f.4)[3] <- 'mean'
f.5 <- summarySE(vol.data[which(vol.data$sex==2),], groupvars='ageBin',
                 measurevar='deep.gm.mean', na.rm=T)
colnames(f.5)[3] <- 'mean'
female.means <- rbind(f.1, f.2, f.3, f.4, f.5)
female.means <- cbind(female.means, lobe)

### Now produce the plots for each gender
maleBarPlot <- ggplot(male.means, aes(x=factor(lobe), y=mean, fill=ageBin)) +
                     geom_bar(stat='identity', position=position_dodge()) + 
                     geom_errorbar(aes(x=factor(lobe), ymin=mean-se, ymax=mean+se),
                     width=.2, position=position_dodge(.9)) +
                     ylab('Volume (mm3)')+
		     xlab("Lobe") +
                     scale_y_continuous(limits=c(25, 250),oob=rescale_none) +
                     ggtitle('Male GM Volume Means')

femaleBarPlot <- ggplot(female.means, aes(x=factor(lobe), y=mean, fill=ageBin)) +
                     geom_bar(stat='identity', position=position_dodge()) + 
                     geom_errorbar(aes(x=factor(lobe), ymin=mean-se, ymax=mean+se),
                     width=.2, position=position_dodge(.9))+
                     ylab('Volume (mm3)') +
		     xlab("Lobe") +
                     scale_y_continuous(limits=c(25, 250),oob=rescale_none) +
                     ggtitle('Female GM Volume Means')

# Now print them to their respective pdf's
pdf('maleGMLobeVolumes.pdf')
print(maleBarPlot)
dev.off()
pdf('femaleGMLobeVolumes.pdf')
print(femaleBarPlot)
dev.off()


# Now do the white matter
vol.data$frontal.wm.sum <- vol.data$mprage_mars_vol_frontalLobe_WM_right + vol.data$mprage_mars_vol_frontalLobe_WM_left

vol.data$parietal.wm.sum <- vol.data$mprage_mars_vol_parietalLobe_WM_right + vol.data$mprage_mars_vol_parietalLobe_WM_left

vol.data$temporal.wm.sum <- vol.data$mprage_mars_vol_temporalLobe_WM_right + vol.data$mprage_mars_vol_temporalLobe_WM_left

vol.data$occipital.wm.sum <- vol.data$mprage_mars_vol_occipitalLobe_WM_right + vol.data$mprage_mars_vol_occipitalLobe_WM_left

# Now produce the means for each age bin by sex
# start with males
m.1 <- summarySE(vol.data[which(vol.data$sex==1),], groupvars='ageBin',
                 measurevar='frontal.wm.sum', na.rm=T)
colnames(m.1)[3] <- 'mean'
m.2 <- summarySE(vol.data[which(vol.data$sex==1),], groupvars='ageBin',
                 measurevar='parietal.wm.sum', na.rm=T)
colnames(m.2)[3] <- 'mean'
m.3 <- summarySE(vol.data[which(vol.data$sex==1),], groupvars='ageBin',
                 measurevar='temporal.wm.sum', na.rm=T)
colnames(m.3)[3] <- 'mean'
m.4 <- summarySE(vol.data[which(vol.data$sex==1),], groupvars='ageBin',
                 measurevar='occipital.wm.sum', na.rm=T)
colnames(m.4)[3] <- 'mean'

male.sums <- rbind(m.1, m.2, m.3, m.4)
lobe <- c(rep('Frontal', 3), rep('Parietal', 3), rep('Temporal', 3),
          rep('Occipital', 3))
male.sums <- cbind(male.sums, lobe)

# Now do Females
f.1 <- summarySE(vol.data[which(vol.data$sex==2),], groupvars='ageBin',
                 measurevar='frontal.wm.sum', na.rm=T)
colnames(f.1)[3] <- 'mean'
f.2 <- summarySE(vol.data[which(vol.data$sex==2),], groupvars='ageBin',
                 measurevar='parietal.wm.sum', na.rm=T)
colnames(f.2)[3] <- 'mean'
f.3 <- summarySE(vol.data[which(vol.data$sex==2),], groupvars='ageBin',
                 measurevar='temporal.wm.sum', na.rm=T)
colnames(f.3)[3] <- 'mean'
f.4 <- summarySE(vol.data[which(vol.data$sex==2),], groupvars='ageBin',
                 measurevar='occipital.wm.sum', na.rm=T)
colnames(f.4)[3] <- 'mean'
female.sums <- rbind(f.1, f.2, f.3, f.4)
female.sums <- cbind(female.sums, lobe)

# Now create the plots
maleBarPlot <- ggplot(male.sums, aes(x=factor(lobe), y=mean, fill=ageBin)) +
                     geom_bar(stat='identity', position=position_dodge()) + 
                     geom_errorbar(aes(x=factor(lobe), ymin=mean-se, ymax=mean+se),
                     width=.2, position=position_dodge(.9)) +
                     ylab('Volume (mm3)')+
		     xlab("Lobe") +
                     scale_y_continuous(limits=c(25, 250),oob=rescale_none) +
                     ggtitle('Male WM Volume Means')

femaleBarPlot <- ggplot(female.sums, aes(x=factor(lobe), y=mean, fill=ageBin)) +
                     geom_bar(stat='identity', position=position_dodge()) + 
                     geom_errorbar(aes(x=factor(lobe), ymin=mean-se, ymax=mean+se),
                     width=.2, position=position_dodge(.9))+
                     ylab('Volume (mm3)') +
		     xlab("Lobe") +
                     scale_y_continuous(limits=c(25, 250),oob=rescale_none) +
                     ggtitle('Female WM Volume Means')

# Now print them to their respective pdf's
pdf('maleWMLobeVolumes.pdf')
print(maleBarPlot)
dev.off()
pdf('femaleWMLobeVolumes.pdf')
print(femaleBarPlot)
dev.off()

