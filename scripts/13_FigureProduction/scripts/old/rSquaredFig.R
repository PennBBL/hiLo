### This script produces Figure 7 in the second submission
###
### Ellyn Butler
### February 10, 2020

## Load the libraries
library('psych')
library('reshape2')
library('ggplot2')
source('~/Documents/ButlerPlotFuncs/plotFuncs.R')

################################ Males ################################
## Read the data
# ~/Documents/hiLo/scripts/04_CognitiveModels/scripts/
allR <- read.csv('~/Documents/hiLo/data/allIndivRValsMALE.csv')
allRN <- read.csv('~/Documents/hiLo/data/allIndivRValsMALEPERM.csv')

## Create some variables
outName <- c('Volume', 'CBF', 'GMD', 'MD', 'ReHo', 'ALFF','All')

## Create a histogram of null vs real r-squared values for each domain
allR$Outcome <- 'Real'
allRN$Outcome <- 'Fake'
toPlot <- rbind(allR, allRN)
toPlot <- melt(toPlot, id.vars=c('V1','V4','V5','V2','Outcome','X'))
# Now fix the names of the modalities
toPlot$variable <- as.character(toPlot$variable)
## Now also test for differences between real and fake labels
output.t.vals <- NULL
for(i in 1:length(outName)){
  ## Now find and change names of the new variables
  toPlot[which(toPlot$V1==i),'V1'] <- outName[i]
  ## Now perform a t test between the real and fake labels
  out.t.val <- c(outName[i], t.test(x=toPlot[which(toPlot$V1==outName[i] & toPlot$Outcome=='Real'),'value'], y=toPlot[which(toPlot$V1==outName[i] & toPlot$Outcome=='Fake'),'value'],alternative='greater')[c('statistic')])
  output.t.vals <- rbind(output.t.vals, out.t.val)
}
toPlot$V1 <- factor(toPlot$V1, levels=c("Volume", "GMD", "MD", "CBF", "ALFF", "ReHo", "All"))
toPlotVals <- summarySE(data=toPlot, groupvars=c('V1','Outcome'), measurevar='value')
#toPlotVals$X <- -.05
#toPlotVals$Y <- 200
#toPlotVals$Y[which(toPlotVals$Outcome=='Fake')] <- 100
#toPlotVals$value <- format(round(toPlotVals$value, digits=2),nsmall=2)
#toPlotVals$value <- paste("Mean Value =", toPlotVals$value, sep='')
out.plot.male <- ggplot(toPlot, aes(x=value, group=Outcome, fill=Outcome)) +
  geom_density(data=subset(toPlot,Outcome=='Real'), fill="steelblue2") +
  geom_density(data=subset(toPlot,Outcome=='Fake'), fill="black") +
  theme_linedraw() +
  facet_grid(V1 ~ .) +
  coord_cartesian(ylim=c(0,250),xlim=c(-.1,.25)) +
  ggtitle("Male") +
  xlab(bquote('CV R'^2)) + theme(axis.text.y = element_text(size=7), legend.position="none") +
  ylab("") +
  geom_vline(data = toPlotVals[toPlotVals$Outcome == "Fake", ], mapping = aes(xintercept = value), linetype = "dashed", color="black") +
  geom_vline(data = toPlotVals[toPlotVals$Outcome == "Real", ], mapping = aes(xintercept = value), linetype = "dashed", color="steelblue2")
  ## Now add the mean values
  #geom_text(data=toPlotVals,aes(x=X,y=Y,color=Outcome,label=value))


################################ Females ################################
allR <- read.csv('~/Documents/hiLo/data/allIndivRValsFemale.csv')
allRN <- read.csv('~/Documents/hiLo/data/allIndivRValsFEMALEPERM.csv')

## Create a histogram of null vs real r-squared values for each domain
allR$Outcome <- 'Real'
allRN$Outcome <- 'Fake'
toPlot <- rbind(allR, allRN)
toPlot <- melt(toPlot, id.vars=c('V1','V4','V5','V2','Outcome','X'))
# Now fix the names of the modalities
toPlot$variable <- as.character(toPlot$variable)
## Now also test for differences between real and fake labels
output.t.vals <- NULL
for(i in 1:length(outName)){
  ## Now find and change names of the new variables
  toPlot[which(toPlot$V1==i),'V1'] <- outName[i]
  ## Now perform a t test between the real and fake labels
  out.t.val <- c(outName[i], t.test(x=toPlot[which(toPlot$V1==outName[i] & toPlot$Outcome=='Real'),'value'], y=toPlot[which(toPlot$V1==outName[i] & toPlot$Outcome=='Fake'),'value'],alternative='greater')[c('statistic')])
  output.t.vals <- rbind(output.t.vals, out.t.val)
}
toPlot$V1 <- factor(toPlot$V1, levels=c("Volume", "GMD", "MD", "CBF", "ALFF", "ReHo", "All"))
toPlotVals <- summarySE(data=toPlot, groupvars=c('V1','Outcome'), measurevar='value')
#toPlotVals$X <- -.05
#toPlotVals$Y <- 200
#toPlotVals$Y[which(toPlotVals$Outcome=='Fake')] <- 100
#toPlotVals$value <- format(round(toPlotVals$value, digits=2),nsmall=2)
#toPlotVals$value <- paste("Mean Value =", toPlotVals$value, sep='')
out.plot.female <- ggplot(toPlot, aes(x=value, group=Outcome, fill=Outcome)) +
  geom_density(data=subset(toPlot,Outcome=='Real'), fill="violetred1") +
  geom_density(data=subset(toPlot,Outcome=='Fake'), fill="black") +
  theme_linedraw() +
  facet_grid(V1 ~ .) +
  coord_cartesian(ylim=c(0,250),xlim=c(-.1,.25)) +
  ggtitle("Female") +
  xlab(bquote('CV R'^2)) + theme(axis.text.y = element_text(size=7), legend.position="none") +
  ylab("") +
  geom_vline(data = toPlotVals[toPlotVals$Outcome == "Fake", ], mapping = aes(xintercept = value), linetype = "dashed", color="black") +
  geom_vline(data = toPlotVals[toPlotVals$Outcome == "Real", ], mapping = aes(xintercept = value), linetype = "dashed", color="violetred1")
  ## Now add the mean values
  #geom_text(data=toPlotVals,aes(x=X,y=Y,color=Outcome,label=value))

png("~/Documents/hiLo/plots/figure7_color.png", height=120, width=160, units='mm', res=800)
grid.arrange(out.plot.female, out.plot.male, ncol=2)
dev.off()

# TO DO:
# 1) Get rid of "Mean Value" on each plot, and replace with vertical lines
# 2) Change the theme to linedraw DONE
# 3) Change colors (black versus pink and blue)
# 4) Add activation
