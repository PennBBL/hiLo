#########################################################################
# Load library(s)
# and declare functions
#########################################################################
install_load('ggplot2','psych','effsize')
source("/home/adrose/hiLo/scripts/02_DevelopmentalTrends/functions/functions.R")
returnPerfBin <- function(data) {
  
  data$F1_Exec_Comp_Cog_Accuracy
  quantiles <- quantile(data$F1_Exec_Comp_Cog_Accuracy, c(0,.33,.67,1))
  
  data$perfBin <- 0
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy < quantiles[2])] <- 'Lo'
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy >= quantiles[2] &
                          data$F1_Exec_Comp_Cog_Accuracy <= quantiles[3])] <- 'Me'
  data$perfBin[which(data$F1_Exec_Comp_Cog_Accuracy > quantiles[3])] <- 'Hi'
  return(data)
}

#########################################################################
# Load data
#########################################################################
img.data <- read.csv('/home/adrose/forRuben/data/tmp/n1601_hiLoDataDump_2018-09-20.csv')
img.data <- img.data[-which(is.na(img.data$F1_Exec_Comp_Cog_Accuracy)),]
img.data <- img.data[-which(abs(img.data$ageAtCnb1 - img.data$ageAtScan1 ) >12),]

#########################################################################
# Add performance bin
#########################################################################
img.data <- returnPerfBin(img.data)

#########################################################################
# Add age bin
#########################################################################
img.data <- addAgeBin(img.data, img.data$ageAtScan1, 167, 215, 216)

#########################################################################
# Panel A
# this will be mean GLOBAL values across ages w/in perf bins
#########################################################################
global.values <- c('mprage_jlf_vol_ICV','mprage_jlf_gmd_MeanGMD','dti_jlf_tr_MeanWholeBrainTR','pcasl_jlf_cbf_MeanGMCBF','rest_jlf_alff_MeanALFF','rest_jlf_reho_MeanReho')
short.val <- c('Volume','GMD','MD','CBF','ALFF','ReHo')
valsToPlot <- NULL
index <- 1
for(i in global.values){
  tmpRow <- summarySE(data=img.data, groupvars=c('perfBin','sex'), measurevar=i, na.rm=T)
  colnames(tmpRow)[4] <- 'mean'
  tmpRow$Value <- short.val[index]
  index <- index + 1
  valsToPlot <- rbind(valsToPlot, tmpRow)
}
valsToPlot$Value <- factor(valsToPlot$Value, levels=short.val)
valsToPlot$perfBin <- factor(valsToPlot$perfBin, levels=c('Lo','Me','Hi'))
valsToPlot$sex <- factor(valsToPlot$sex,levels=c(1,2))
## Now prepare the plots in a loop
## this will be done in a loop because of the wild diff in axis values
yLimLower <- c(1300000,.79,2.1,61,450,.15)
yLimUpper <- c(1600000,.82,2.4,70,575,.18)
index <- 1
for(i in short.val){
  ## First grab the values
  toPlot <- valsToPlot[which(valsToPlot$Value==i),]
  ## Now create the plot
  outPlot <- ggplot(toPlot, aes(x=perfBin,y=mean,group=sex,fill=sex)) +
	geom_bar(stat="identity", 
           position=position_dodge()) +
	scale_fill_manual(name = "sex",
                          values=c("1"="blue","2"="red")) +
	geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci),position=position_dodge(.9), width=.2) + 
	coord_cartesian(ylim=c(yLimLower[index],yLimUpper[index])) +
	theme_bw() +
	theme(legend.position="none",plot.title = element_text(hjust = 0.5),text = element_text(size=16,face='bold')) +
	xlab("") +
	ylab("") +
	ggtitle(i)
  index <- index+1
  assign(paste(i, "Plot",sep=''),outPlot)
}

#########################################################################
# Panel B
# This will be cohen d values across the age bins
#########################################################################
cohenValues <- NULL
global.values <- c('mprage_jlf_vol_ICV','mprage_jlf_gmd_MeanGMD','dti_jlf_tr_MeanWholeBrainTR','pcasl_jlf_cbf_MeanWholeBrainCBF','rest_jlf_alff_MeanALFF','rest_jlf_reho_MeanReho')
sex.values <- c(1,2)
age.values <- c("Childhood","Adolescence","Early Adulthood")
index <- 1
for(g in global.values){
  for(a in age.values){
    for(s in sex.values){
      ## Isolate the data
      tmp.data <- img.data[which(img.data$ageBin==a & img.data$sex==s),]
      tmp.data <- tmp.data[-which(tmp.data$perfBin=='Me'),]
      vals <- cohen.d(d=tmp.data[,g], f=tmp.data$perfBin,na.rm=T)
      output.row <- c(g,a,s,short.val[index],vals$estimate,vals$conf.int)
      cohenValues <- rbind(cohenValues,output.row)
    }
  }
  index <- index + 1
}
cohenValues <- data.frame(cohenValues)
cohenValues$V2 <- factor(cohenValues$V2, levels=c("Childhood","Adolescence","Early Adulthood"))
cohenValues$V2 <- revalue(cohenValues$V2,c("Childhood"="Children","Adolescence"="Adolescents","Early Adulthood"="Young Adults"))
cohenValues[,5:7] <- apply(cohenValues[,5:7],2,function(x) as.numeric(as.character(x)))

## Now create the plots
index <- 1
for(g in global.values){
  toPlot <- cohenValues[which(cohenValues$V1==g),]
  outPlot <- ggplot(toPlot,aes(x=V2,y=Hi,group=V3,fill=V3)) +	
    geom_bar(stat="identity", position=position_dodge()) +
	scale_fill_manual(name = "sex",
                          values=c("1"="blue","2"="red")) +
    scale_y_continuous(limits=c(-.8, 1.4), 
                           breaks=round(seq(-.8,1.4,.2), digits=2)) +
	theme_bw() +
	theme(legend.position="none",plot.title = element_text(hjust = 0.5),text = element_text(size=16,face='bold'),axis.text.x = element_text(size=8)) +
	xlab("") +
	ylab("")
	#ggtitle(short.val[index])
	if(index>1){
	outPlot <- outPlot +
	theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
	axis.text.y=element_text(color='white'))
	}
  assign(paste(short.val[index], "PlotD",sep=''),outPlot)
  index <- index +1
}

## Now plot everything
png("globalEffects.png", width=24,height=10,units='in',res=300)
multiplot(VolumePlot,VolumePlotD,GMDPlot,GMDPlotD,MDPlot,MDPlotD,CBFPlot,CBFPlotD,ALFFPlot,ALFFPlotD,ReHoPlot,ReHoPlotD,cols=6)
dev.off()
