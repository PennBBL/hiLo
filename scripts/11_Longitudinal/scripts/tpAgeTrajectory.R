# AFGR May 2018 

# This scirpt will be used to explore if the cross sectional age trends are different across our TD groups 
# across the timepoints
# So this will restirct the analyses to anyone with a goassessDxpmr7 of TD

## Load library(s)
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table', 'utils','lme4')

## Load data
all.data <- read.csv('/home/adrose/forRuben/data/n2416_imagingDataDump_2018-05-04.csv')
all.data$DOSCAN <- as.character(all.data$DOSCAN)
all.data$DOSCAN <- as.Date(all.data$DOSCAN, "%m/%d/%y")

## Now plot age trajectories for TD subjects in all individual tp values across sex
summaryMetrics <- c('mprage_jlf_vol_TBV', 'mprage_jlf_vol_TBGM', 'mprage_jlf_vol_TBWM', 'mprage_jlf_ct_MeanCT','mprage_jlf_gmd_MeanGMD', 'pcasl_jlf_cbf_MeanWholeBrainCBF', 'rest_jlf_reho_MeanReho', 'rest_jlf_alff_MeanALFF')
minVal <- c(800000, 500000, 300000,2.5,.7,25,.05,200)
maxVal <- c(1610000,1000000,700000,4.5,.95,110,.3,1000)
pdf('tpAgeTrends.pdf', height=20, width=28)
options(digits=7)
index <- 1
for(i in summaryMetrics){
  colVal <- grep(i, names(all.data))
  if(length(colVal)>1){
    colVal <- colVal[7]
  }
  toPlot <- all.data[complete.cases(all.data[,colVal]),]
  toPlot <- toPlot[-which(toPlot$goassessDxpmr7=="TD"),]
  toPlot$pncGrpPsych <- factor(toPlot$pncGrpPsych)
  pasta.plot.one <- ggplot(toPlot[which(toPlot$sex==1),], aes(x=scanageMonths/12, y=toPlot[which(toPlot$sex==1),colVal])) +
    geom_point() +
    geom_smooth(method='lm') +
    ylab(i) +
    theme_bw() +
    theme(legend.position="bottom") +
    ggtitle("Male") +
    coord_cartesian(ylim=c(minVal[index], maxVal[index])) +
    facet_grid(.~tpvalue)
pasta.plot.two <- ggplot(toPlot[which(toPlot$sex==2),], aes(x=scanageMonths/12, y=toPlot[which(toPlot$sex==2),colVal])) +
    geom_point() +
    geom_smooth(method='lm') +
    ylab(i) +
    theme_bw() +
    ggtitle("Female") +
    theme(legend.position="bottom") +
    coord_cartesian(ylim=c(minVal[index], maxVal[index])) +
    facet_grid(.~tpvalue)
  multiplot(pasta.plot.one,pasta.plot.two, cols=2)
  index <- index + 1
}
dev.off()

# Now compare these values statistically
