## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm2Functions.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm1Functions2.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/functions-forJLF.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'ggrepel')

## Load data
vol.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/volumeData.csv')
cbf.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/cbfData.csv')
gmd.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/gmdData.csv')
tr.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfTRData.csv')

## Declare any functions
returnPerfBin <- function(data) {
    
    data$F1_Exec_Comp_Cog_Accuracy
    quantiles <- quantile(data$F1_Exec_Comp_Cog_Accuracy, c(0,.33,.67,1))
    
    data$perfBin <- 0
    data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy < quantiles[2])] <- 'lo'
    data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy >= quantiles[2] &
    data$F1_Exec_Comp_Cog_Accuracy <= quantiles[3])] <- 'me'
    data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy > quantiles[3])] <- 'hi'
    return(data)
}

## Create our static perf bin
tmpDF <- vol.modal.data.age.reg
tmpDF <- returnPerfBin(tmpDF)
outCol <- tmpDF[,c('bblid','scanid','perfBin')]
colnames(outCol)[3] <- paste('perfCol', 1, sep='')
static.perf.bin <- outCol
colnames(static.perf.bin) <- c('bblid', 'scanid', 'groupFactorLevel')
rm(tmpDF)

## Now add age bin
cbf.modal.data.age.reg$ageBin <- 'Age Regressed'
vol.modal.data.age.reg$ageBin <- 'Age Regressed'
gmd.modal.data.age.reg$ageBin <- 'Age Regressed'
tr.modal.data.age.reg$ageBin <- 'Age Regressed'

## Now produce all our loop values
data.names <- c('vol','cbf','gmd','tr')
grepVals <- c('mprage_jlf_vol_', 'pcasl_jlf_cbf_', 'mprage_jlf_gmd_','dti_jlf_tr_')
wmAdd <- c(0,0,0,0)
cerebellumValues <- c('FALSE', 'FALSE', 'FALSE', 'FALSE')
## Now prepare an output matrix
data <- doEverythingEver(vol.modal.data.age.reg, 'mprage_jlf_vol_', 0, 999, 'Age Regressed', cerebellumIn=F, optionalRace=NULL)
output.data <- data[,c(1,6,8,9,11)]

for(q in 2:4){
    print(data.names[q])
    dfName <- paste(data.names[q], ".modal.data.age.reg", sep='')
    data <- doEverythingEver(get(dfName), grepVals[q], 0, 999, 'Age Regressed', cerebellumIn=cerebellumValues[q], optionalRace=NULL)
    output.data <- merge(output.data, data, by=c('ROI', 'Gender', 'lobe'), suffixes=c('', data.names[q]))
}

# Now combine the values
# Make sure the take the absolute for cbf and tr
output.data$sumEffectSize <- output.data$zScoreDifference + abs(output.data$zScoreDifferencecbf) + output.data$zScoreDifferencegmd + abs(output.data$zScoreDifferencetr)
output.data <- reshape(data=output.data, direction="wide", idvar="ROI", timevar='sex', v.names='sumEffectSize')
output.data$lobe <- revalue(output.data$lobe, replace=c("Basal Ganglia"="Basalstriatal", "Limbic"="Limbic", "Frontal Orbital"="Frontal", "Frontal Dorsal"="Frontal", "Temporal"="Temporal", "Parietal"="Parietal", "Occipital"="Occipital"))
# Now make a scatter plot with these values
corVal <- paste("r = ", round(cor(output.data$sumEffectSize.M, output.data$sumEffectSize.F), digits=2))
plotOut <-  ggplot(output.data, aes(x=sumEffectSize.M, y=sumEffectSize.F)) +
    geom_point(aes(fill=lobe)) +
    geom_abline(intercept=0, slope=1) +
    geom_smooth(method=lm) +
  geom_label_repel(aes(label=ROI,color=lobe,size=3.5),box.padding=unit(0.5,"lines"),point.padding=unit(0.25,"lines")) +
    coord_cartesian(xlim=c(0, 2.5), ylim=c(0, 2.5)) +
    xlab("Male z-score difference") +
    ylab("Female z-score difference") +
    geom_hline(yintercept = 0 , linetype=3) +
    geom_vline(xintercept = 0 , linetype=3) +
    theme_bw() +
    theme(legend.position="none") +
    ggtitle("Summed Effect Sizes") +
    geom_text(aes(x=-Inf, y=Inf, hjust=0, vjust=1, label=corVal))

## Now print the scatter plot
png("summedES.png", height=8, width=8, units='in', res=300)
print(plotOut)
dev.off()
