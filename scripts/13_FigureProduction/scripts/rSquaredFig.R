## Load the libraries
install_load('psych','reshape2','ggplot2')

## Read the data
allR <- read.csv('./allIndivRValsMALE.csv')
allRN <- read.csv('./allIndivRValsMALEPERM.csv')

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
toPlotVals$X <- -.05
toPlotVals$Y <- 200
toPlotVals$Y[which(toPlotVals$Outcome=='Fake')] <- 100
toPlotVals$value <- format(round(toPlotVals$value, digits=2),nsmall=2)
toPlotVals$value <- paste("Mean Value =", toPlotVals$value, sep='')
out.plot.male <- ggplot(toPlot, aes(x=value, group=Outcome, fill=Outcome)) +
  geom_density(data=subset(toPlot,Outcome=='Real')) +
  geom_density(data=subset(toPlot,Outcome=='Fake')) +
  theme_linedraw() +
  facet_grid(V1 ~ .) +
  coord_cartesian(ylim=c(0,250),xlim=c(-.1,.25)) +
  ggtitle("Male") +
  xlab(bquote('CV R'^2)) + theme(legend.position="none") +
  ylab("") +
  ## Now add the mean values
  geom_text(data=toPlotVals,aes(x=X,y=Y,color=Outcome,label=value))

## Now do females
allR <- read.csv('./allIndivRValsFemale.csv')
allRN <- read.csv('./allIndivRValsFEMALEPERM.csv')

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
toPlotVals$X <- -.05
toPlotVals$Y <- 200
toPlotVals$Y[which(toPlotVals$Outcome=='Fake')] <- 100
toPlotVals$value <- format(round(toPlotVals$value, digits=2),nsmall=2)
toPlotVals$value <- paste("Mean Value =", toPlotVals$value, sep='')
out.plot.female <- ggplot(toPlot, aes(x=value, group=Outcome, fill=Outcome)) +
  geom_density(data=subset(toPlot,Outcome=='Real')) +
  geom_density(data=subset(toPlot,Outcome=='Fake')) +
  theme_linedraw() +
  facet_grid(V1 ~ .) +
  coord_cartesian(ylim=c(0,250),xlim=c(-.1,.25)) +
  ggtitle("Female") +
  ylab("") +
  xlab(bquote('CV R'^2)) + theme(legend.position="none") +
  ## Now add the mean values
  geom_text(data=toPlotVals,aes(x=X,y=Y,color=Outcome,label=value))

png("~/Documents/hiLo/plots/figure5_color.png", height=100, width=180, units='mm', res=800)
multiplot(out.plot.male, out.plot.female, cols=2)
dev.off()

# TO DO:
# 1) Get rid of "Mean Value" on each plot, and replace with vertical lines
# 2) Change the theme to linedraw DONE
# 3) Add activation
