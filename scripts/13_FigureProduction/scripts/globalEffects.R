#########################################################################
# Load library(s)
# and declare functions
#########################################################################
library('ggplot2')
library('psych')
library('effsize')
source('~/Documents/hiLo/scripts/02_DevelopmentalTrends/functions/functions.R')
source('~/Documents/ButlerPlotFuncs/plotFuncs.R')

#########################################################################
# Load data
#########################################################################
img.data <- read.csv('~/Documents/hiLo/data/n1601_hiLoDataDump_2018-09-20.csv')

#########################################################################
# Add in the metrics for NBack and IdEmo
#########################################################################

# Load nback and idemo data
nback.data <- read.csv('~/Documents/hiLo/data/meanLR/nbackData.csv')
idemo.data <- read.csv('~/Documents/hiLo/data/meanLR/idemoData.csv')
vol.data <- read.csv('~/Documents/hiLo/data/meanLR/volumeData.csv')

# Get (subset of) limbic IdEmo activation
shortregs <-  c('AIns', 'Amygdala', 'Ent')
volregs <- paste0('mprage_jlf_vol_', shortregs)
idemoregs <- paste0('sigchange_cope1_task_mean_miccai_ave_', shortregs)
vol.data <- vol.data[vol.data$bblid %in% idemo.data$bblid,]
row.names(vol.data) <- 1:nrow(vol.data)

idemo.data <- arrange(idemo.data, bblid)
vol.data <- arrange(vol.data, bblid)

row.names(idemo.data) <- 1:nrow(idemo.data)
row.names(vol.data) <- 1:nrow(vol.data)

tmp <- 0
for (i in 1:length(volregs)) {
  tmp <- tmp + vol.data[,volregs[i]]*idemo.data[,idemoregs[i]]
}
tmp <- tmp/sum(vol.data[, volregs])
idemo.data$sigchange_cope1_task_mean_miccai_ave_Limbic <- tmp

idemo.data <- idemo.data[,c('bblid', grep('sigchange', colnames(idemo.data), value=TRUE),
  grep('idemo', colnames(idemo.data), value=TRUE))]

# Merge data
img.data <- merge(img.data, nback.data, all=T)
img.data <- merge(img.data, idemo.data, all=T)
img.data <- img.data[-which(is.na(img.data$F1_Exec_Comp_Cog_Accuracy)),]
img.data <- img.data[-which(abs(img.data$ageAtCnb1 - img.data$ageAtScan1) > 12),]
row.names(img.data) <- 1:nrow(img.data)

img.data$sex <- factor(img.data$sex)
img.data$sex <- revalue(img.data$sex, c('2'='Female', '1'='Male'))


#########################################################################
# Add age bin
#########################################################################

img.data$ageYrs <- img.data$ageAtScan1/12
img.data$ageBin <- NA
img.data[img.data$ageYrs < 13, 'ageBin'] <- 'Children'
img.data[img.data$ageYrs >= 13 & img.data$ageYrs < 18, 'ageBin'] <- 'Adolesc'
img.data[img.data$ageYrs >= 18, 'ageBin'] <- 'Adults'


#########################################################################
# Add performance bin
#########################################################################

splits <- quantile(img.data$F1_Exec_Comp_Cog_Accuracy, c(.33, .66))
img.data$perfBin <- NA
for (i in 1:nrow(img.data)) {
	if (img.data[i, 'F1_Exec_Comp_Cog_Accuracy'] <= splits[[1]]) {
		img.data[i, 'perfBin'] <- 'Lo'
	} else if (img.data[i, 'F1_Exec_Comp_Cog_Accuracy'] > splits[[1]] & img.data[i, 'F1_Exec_Comp_Cog_Accuracy'] <= splits[[2]]) {
		img.data[i, 'perfBin'] <- 'Me'
	} else {
		img.data[i, 'perfBin'] <- 'Hi'
	}
}


#########################################################################
# Panel A
# this will be mean GLOBAL values across ages w/in perf bins
#########################################################################

global.values <- c('mprage_jlf_vol_TBV','mprage_jlf_gmd_MeanGMD','dti_jlf_tr_MeanTR',
  'pcasl_jlf_cbf_MeanGMCBF','rest_jlf_alff_MeanALFF','rest_jlf_reho_MeanReho',
  'sigchange_contrast4_2back0back_mean_miccai_ave_MFG',
  'sigchange_cope1_task_mean_miccai_ave_Limbic')
short.val <- c('TBV','GMD','MD','CBF','ALFF','ReHo', 'NBack', 'IdEmo')
valsToPlot <- NULL
img.data$mprage_jlf_vol_TBV <- img.data$mprage_jlf_vol_TBV/1000000

index <- 1
for (i in global.values) {
  tmpRow <- summarySE(data=img.data, groupvars=c('perfBin','sex'), measurevar=i, na.rm=T)
  colnames(tmpRow)[4] <- 'mean'
  tmpRow$Value <- short.val[index]
  index <- index + 1
  valsToPlot <- rbind(valsToPlot, tmpRow)
}
valsToPlot$Value <- factor(valsToPlot$Value, levels=short.val)
valsToPlot$perfBin <- factor(valsToPlot$perfBin, levels=c('Lo','Me','Hi'))
valsToPlot$sex <- factor(valsToPlot$sex)
#valsToPlot$sex <- revalue(valsToPlot$sex, c('2'='Female', '1'='Male'))

## Now prepare the plots in a loop
## this will be done in a loop because of the wild diff in axis values
yLimLower <- c(1,.79,2.1,61,450,.15,0,0)
yLimUpper <- c(1.4,.82,2.5,70,575,.18,.6,.00025)
index <- 1
for (i in short.val) {
  ## First grab the values
  toPlot <- valsToPlot[which(valsToPlot$Value==i),]
  ## Now create the plot
  outPlot <- ggplot(toPlot, aes(x=perfBin, y=mean, group=sex, fill=sex)) +
	geom_bar(stat='identity',
           position=position_dodge()) +
	scale_fill_manual(name = 'sex',
                          values=c('Female'='gray62', 'Male'='black')) +
	geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci),position=position_dodge(.9), width=.2) +
	coord_cartesian(ylim=c(yLimLower[index],yLimUpper[index])) +
	theme_bw() +
	theme(legend.position='none',plot.title = element_text(hjust = 0.5),text = element_text(size=8,face='bold')) +
	xlab('') +
	ylab('')
  if (index == 1) {
    outPlot <- outPlot + ggtitle(paste(short.val[index], '(liters)'))
  } else {
    outPlot <- outPlot + ggtitle(i)
  }
  index <- index+1
  assign(paste0(i, 'Plot'),outPlot)
}

#########################################################################
# Panel B
# This will be cohen d values across the age bins
#########################################################################
cohenValues <- NULL
sex.values <- c('Female', 'Male')
age.values <- c('Children', 'Adolesc', 'Adults')
index <- 1
for (g in global.values) {
  for (s in sex.values) {
    for (a in age.values) {
      ## Isolate the data
      tmp.data <- img.data[which(img.data$ageBin==a & img.data$sex==s),]
      tmp.data <- tmp.data[-which(tmp.data$perfBin=='Me'),]
      tmp.data$perfBin <- factor(tmp.data$perfBin)
      tmp.data$perfBin <- relevel(tmp.data$perfBin, 'Lo')
      vals <- cohen.d(x=tmp.data[,g], group=tmp.data$perfBin) #August 31, 2020: f, na.rm, and pooled no longer valid arguments
      #print(paste(a,s))
      #print(summarySE(data=tmp.data, groupvars='perfBin', measurevar=g, na.rm=T))
      output.row <- c(g,a,s,short.val[index], vals$cohen.d, vals$conf.int) #vals$estimate -> vals$cohen.d[2]
      cohenValues <- rbind(cohenValues,output.row)
    }
  }
  index <- index + 1
}
cohenValues <- data.frame(cohenValues)
names(cohenValues) <- c('Region', 'AgeGroup', 'Sex', 'Modality', 'LCI', 'Effect', 'UCI')
cohenValues$AgeGroup <- factor(cohenValues$AgeGroup, levels=c('Children','Adolesc','Adults'))
cohenValues[, c('LCI', 'Effect', 'UCI')] <- apply(cohenValues[, c('LCI', 'Effect', 'UCI')],
  2, function(x) as.numeric(as.character(x)))
row.names(cohenValues) <- 1:nrow(cohenValues)


## Now create the plots
index <- 1
for (g in global.values) {
  toPlot <- cohenValues[which(cohenValues$Region==g),]
  toPlot$Sex <- relevel(toPlot$Sex, 'Male')
  outPlot <- ggplot(toPlot, aes(x=AgeGroup, y=Effect, group=Sex, fill=Sex)) + #y=Lo*-1
      geom_bar(stat='identity', position=position_dodge()) +
	   scale_fill_manual(name = 'sex', values=c('Female'='gray62', 'Male'='black')) +
     scale_y_continuous(limits=c(-.8, 1.4), breaks=round(seq(-.8,1.4,.2), digits=2)) +
	   theme_bw() +
	   theme(legend.position='none', plot.title = element_text(hjust = 0.5),
      text = element_text(size=8,face='bold')) +
	   xlab('') + ylab('') #+ ggtitle(short.val[index])
	if (!(index %in% c(1, 4))) {
	 outPlot <- outPlot +
	  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
	      axis.text.y=element_text(color='white'))
	} else {
    outPlot <- outPlot + ylab('Effect Size')
  }
  assign(paste0(short.val[index], 'PlotD'), outPlot)
  index <- index + 1
}

## Now plot everything
tiff('~/Documents/hiLo/plots/figure1.tif',width=180, height=170, units='mm', res=300)
grid.arrange(
  grobs=list(TBVPlot,GMDPlot,MDPlot,
          TBVPlotD,GMDPlotD,MDPlotD,
          NBackPlot,IdEmoPlot,
          CBFPlot,ALFFPlot,ReHoPlot,CBFPlotD,
          ALFFPlotD,ReHoPlotD,
          NBackPlotD,IdEmoPlotD),
  layout_matrix = rbind(c(1, 2, 3, 7),
                        c(4, 5, 6, 15),
                        c(9, 10, 11, 8),
                        c(12, 13, 14, 16))
)
dev.off()











## Now plot one with a legend
#outPlot <- ggplot(toPlot,aes(x=V2,y=Lo*-1,group=V3,fill=V3)) +
#    geom_bar(stat='identity', position=position_dodge()) +
#	scale_fill_manual(name = 'sex',
#                          values=c('1'='blue','2'='red')) +
#    scale_y_continuous(limits=c(-.8, 1.4),
#                           breaks=round(seq(-.8,1.4,.2), digits=2)) +
#	theme_bw() +
#	theme(legend.position='bottom',plot.title = element_text(hjust = 0.5),text = element_text(size=20,face='bold'),axis.text.x = element_text(size=16),legend.text=element_text(size=16)) +
#	xlab('') +
#	ylab('')

#library(gtable)
#g <- ggplotGrob(outPlot)
#s <- gtable_filter(g, 'axis-b|guide', trim=F)  # use trim depending on need
#png('/Users/butellyn/Documents/hiLo/plots/figure8_color.png',width=220, height=120, units='mm', res=800)
#grid.newpage()
#grid.draw(s)
#dev.off()
