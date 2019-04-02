## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm2Functions.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm1Functions2.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/functions-forJLF.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table')

# Declare any functions
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

## Load the data
vol.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/volumeData.csv')
vol.modal.data <- addAgeBin(vol.modal.data, vol.modal.data$ageAtGo1Scan, 167, 215, 216)
cbf.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/cbfData.csv')
cbf.modal.data <- addAgeBin(cbf.modal.data, cbf.modal.data$ageAtGo1Scan, 167, 215, 216)
reho.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/rehoData.csv')
reho.modal.data <- addAgeBin(reho.modal.data, reho.modal.data$ageAtGo1Scan, 167, 215, 216)
alff.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/alffData.csv')
alff.modal.data <- addAgeBin(alff.modal.data, alff.modal.data$ageAtGo1Scan, 167, 215, 216)

# Now create a static perf bin variable
tmpDF <- vol.modal.data
tmpDF <- returnPerfBin(tmpDF)
outCol <- tmpDF[,c('bblid','scanid','perfBin')]
colnames(outCol)[3] <- paste('perfCol', 1, sep='')
static.perf.bin <- outCol
colnames(static.perf.bin) <- c('bblid', 'scanid', 'groupFactorLevel')
rm(tmpDF)

## Now create our values to plot
# start with CBF
child.cbf <- doEverythingEver(cbf.modal.data, 'pcasl_jlf_cbf', 0, 167, 'Childhood', cerebellum=F,optionalRace=NULL)
adol.cbf <- doEverythingEver(cbf.modal.data, 'pcasl_jlf_cbf', 168, 215, 'Adolescence', cerebellum=F,optionalRace=NULL)
adult.cbf <- doEverythingEver(cbf.modal.data, 'pcasl_jlf_cbf', 216, 999, 'Early Adulthood', cerebellum=F,optionalRace=NULL)
child.cbfWM <- doEverythingEverWM(cbf.modal.data, 'pcasl_jlf_cbf', 0, 167, 'Childhood', cerebellum=F,optionalRace=NULL)
adol.cbfWM <- doEverythingEverWM(cbf.modal.data, 'pcasl_jlf_cbf', 168, 215, 'Adolescence', cerebellum=F,optionalRace=NULL)
adult.cbfWM <- doEverythingEverWM(cbf.modal.data, 'pcasl_jlf_cbf', 216, 999, 'Early Adulthood', cerebellum=F,optionalRace=NULL)
all.cbf <- rbind(child.cbf, adol.cbf, adult.cbf)
all.cbf <- rbind(all.cbf,child.cbfWM, adol.cbfWM, adult.cbfWM)

# Now onto reho
child.rh <- doEverythingEver(reho.modal.data, 'rest_jlf_reho', 0, 167, 'Childhood', cerebellum=T,optionalRace=NULL)
adol.rh <- doEverythingEver(reho.modal.data, 'rest_jlf_reho', 168, 215, 'Adolescence', cerebellum=T,optionalRace=NULL)
adult.rh <- doEverythingEver(reho.modal.data, 'rest_jlf_reho', 216, 999, 'Early Adulthood', cerebellum=T,optionalRace=NULL)
all.rh <- rbind(child.rh, adol.rh, adult.rh,child.cbfWM, adol.cbfWM, adult.cbfWM)
all.rh$zScoreDifference[ which(all.rh$lobe=='WM')] <- NA

## Now add the cerebellum to cbf
all.cbf <- rbind(all.cbf, all.rh[which(all.rh$lobe=='Cerebellum'),])
all.cbf$zScoreDifference[ which(all.cbf$lobe=='Cerebellum')] <- NA

# Now do alff
child.al <- doEverythingEver(alff.modal.data, 'rest_jlf_alff', 0, 167, 'Childhood', cerebellum=T,optionalRace=NULL)
adol.al <- doEverythingEver(alff.modal.data, 'rest_jlf_alff', 168, 215, 'Adolescence', cerebellum=T,optionalRace=NULL)
adult.al <- doEverythingEver(alff.modal.data, 'rest_jlf_alff', 216, 999, 'Early Adulthood', cerebellum=T,optionalRace=NULL)
all.al <- rbind(child.al, adol.al, adult.al,child.cbfWM, adol.cbfWM, adult.cbfWM)
all.al$zScoreDifference[ which(all.al$lobe=='WM')] <- NA

## Now clean up some factors
## Now clean some factors so they are pb ready
all.cbf$ageBin <- revalue(all.cbf$ageBin,c("Childhood"="Children","Adolescence"="Adolescents","Early Adulthood"="Young Adults"))
all.cbf$lobe <- factor(all.cbf$lobe, levels=c('Cerebellum','Basal Ganglia','Limbic','Frontal Orbital','Frontal Dorsal','Temporal','Parietal','Occipital','WM'))
all.cbf$ROI <- revalue(all.cbf$ROI, c("FRO WM"="FRO","TEM WM"="TEM","PAR WM"="PAR","OCC WM"="OCC","Limbic WM"="Limbic","Insular WM"="Insular"))
all.cbf$lobe <- revalue(all.cbf$lobe, c('Basal Ganglia'='Basal/ST','Frontal Orbital'='Frontal','Frontal Dorsal'='Frontal'))
all.rh$ageBin <- revalue(all.rh$ageBin,c("Childhood"="Children","Adolescence"="Adolescents","Early Adulthood"="Young Adults"))
all.rh$lobe <- factor(all.rh$lobe, levels=c('Cerebellum','Basal Ganglia','Limbic','Frontal Orbital','Frontal Dorsal','Temporal','Parietal','Occipital','WM'))
all.rh$lobe <- revalue(all.rh$lobe, c('Basal Ganglia'='Basal/ST','Frontal Orbital'='Frontal','Frontal Dorsal'='Frontal'))
all.al$ageBin <- revalue(all.al$ageBin,c("Childhood"="Children","Adolescence"="Adolescents","Early Adulthood"="Young Adults"))
all.al$lobe <- factor(all.al$lobe, levels=c('Cerebellum','Basal Ganglia','Limbic','Frontal Orbital','Frontal Dorsal','Temporal','Parietal','Occipital','WM'))
all.al$ROI <- revalue(all.al$ROI, c("FRO WM"="FRO","TEM WM"="TEM","PAR WM"="PAR","OCC WM"="OCC","Limbic WM"="Limbic","Insular WM"="Insular"))
all.al$lobe <- revalue(all.al$lobe, c('Basal Ganglia'='Basal/ST','Frontal Orbital'='Frontal','Frontal Dorsal'='Frontal'))

outPlot <- ggplot(all.cbf, aes(y=zScoreDifference, x=ROI, group=Gender)) +
      geom_line(aes(linetype=Gender,color=Gender), size=1) +
      geom_point(aes(shape=Gender, color=Gender), size=1.5) +
      #geom_errorbar(aes(ymin=meanValue-standErrValue, ymax=meanValue+standErrValue)) + 
      scale_y_continuous(limits=c(-.8, .4), 
                           breaks=round(seq(-.4,1.4,.4), digits=2)) +
      xlab("ROI") +
      ylab("Effect Size") +
      geom_hline(aes(yintercept=0), linetype="longdash", colour="black", size=0.5) +
      scale_colour_manual(name = "Gender",
                          values=c("Male"="blue","Female"="red")) +
      scale_linetype_manual(name = "Gender", values = c("Male" = "solid", "Female" = "solid")) +
      #scale_shape_manual(values=c(16), guide=FALSE) +
      theme_bw() +
      theme(legend.position="none") +
      theme(axis.text.x = element_blank(), 
      axis.text.y = element_text(face="bold", size=8), axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank(), strip.text.x = element_text(size=10)) +
      facet_grid(ageBin ~ lobe, scales="free", space="free_x")

outPlot2 <- ggplot(all.rh, aes(y=zScoreDifference, x=ROI, group=Gender)) +
      geom_line(aes(linetype=Gender,color=Gender), size=1) +
      geom_point(aes(shape=Gender, color=Gender), size=1.5) +
      #geom_errorbar(aes(ymin=meanValue-standErrValue, ymax=meanValue+standErrValue)) + 
      scale_y_continuous(limits=c(-.9, .6), 
                           breaks=round(seq(-1.2,1.4,.4), digits=2)) +
      xlab("ROI") +
      ylab("Effect Size") +
      geom_hline(aes(yintercept=0), linetype="longdash", colour="black", size=0.5) +
      scale_colour_manual(name = "Gender",
                          values=c("Male"="blue","Female"="red")) +
      scale_linetype_manual(name = "Gender", values = c("Male" = "solid", "Female" = "solid")) +
      #scale_shape_manual(values=c(16), guide=FALSE) +
      theme_bw() +
      theme(legend.position="none") +
      theme(axis.text.x = element_blank(), 
      axis.text.y = element_text(face="bold", size=8), axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank(), strip.text.x = element_text(size=10)) +
      facet_grid(ageBin ~ lobe, scales="free", space="free_x")

outPlot3 <- ggplot(all.al, aes(y=zScoreDifference, x=ROI, group=Gender)) +
      geom_line(aes(linetype=Gender,color=Gender), size=1) +
      geom_point(aes(shape=Gender, color=Gender), size=1.5) +
      #geom_errorbar(aes(ymin=meanValue-standErrValue, ymax=meanValue+standErrValue)) + 
      scale_y_continuous(limits=c(-.8, .88), 
                           breaks=round(seq(-1.4,1.4,.4), digits=2)) +
      xlab("ROI") +
      ylab("Effect Size") +
      geom_hline(aes(yintercept=0), linetype="longdash", colour="black", size=0.5) +
      scale_colour_manual(name = "Gender",
                          values=c("Male"="blue","Female"="red")) +
      scale_linetype_manual(name = "Gender", values = c("Male" = "solid", "Female" = "solid")) +
      #scale_shape_manual(values=c(16), guide=FALSE) +
      theme_bw() +
      theme(legend.position="none") +
      theme(axis.text.x = element_blank(), 
      axis.text.y = element_text(face="bold", size=8), axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank(), strip.text.x = element_text(size=10)) +
      facet_grid(ageBin ~ lobe, scales="free", space="free_x")


png("cbfRegionalEffects.png", width=14,height=5,units='in',res=300)
outPlot
dev.off()
png("rhRegionalEffects.png", width=14,height=5,units='in',res=300)
outPlot2
dev.off()
png("alRegionalEffects.png", width=14,height=5,units='in',res=300)
outPlot3
dev.off()

## Now make a plot with x axis text
outPlot <- ggplot(all.cbf, aes(y=zScoreDifference, x=ROI, group=Gender)) +
      geom_line(aes(linetype=Gender,color=Gender), size=1) +
      geom_point(aes(shape=Gender, color=Gender), size=1.5) +
      #geom_errorbar(aes(ymin=meanValue-standErrValue, ymax=meanValue+standErrValue)) + 
      scale_y_continuous(limits=c(-.4, 1.4), 
                           breaks=round(seq(-.4,1.4,.4), digits=2)) +
      xlab("ROI") +
      ylab("Effect Size") +
      geom_hline(aes(yintercept=0), linetype="longdash", colour="black", size=0.5) +
      scale_colour_manual(name = "Gender",
                          values=c("Male"="blue","Female"="red")) +
      scale_linetype_manual(name = "Gender", values = c("Male" = "solid", "Female" = "solid")) +
      #scale_shape_manual(values=c(16), guide=FALSE) +
      theme_bw() +
      theme(legend.position="none") +
      theme(axis.text.x = element_text(angle = 45,hjust = 1, face = "bold"), 
      axis.text.y = element_text(face="bold", size=8), axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank(), strip.text.x = element_text(size=10)) +
      facet_grid(ageBin ~ lobe, scales="free", space="free_x")

library(gtable)
g <- ggplotGrob(outPlot)
s <- gtable_filter(g, 'xlab|axis-b', trim=F)  # use trim depending on need
png("outXAxisValues.png",width=14, height=5, units='in', res=300)
grid.newpage()
grid.draw(s)
dev.off()
