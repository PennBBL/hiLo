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
vol.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
vol.modal.data.age.reg$ageBin <- 'Age Regressed'
gmd.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/gmdData.csv')
gmd.modal.data <- addAgeBin(gmd.modal.data, gmd.modal.data$ageAtGo1Scan, 167, 215, 216)
tr.modal.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfTRData.csv')
tr.modal.data <- tr.modal.data[-which(tr.modal.data$dti_jlf_tr_Thalamus_Proper>2.8),]
tr.modal.data <- addAgeBin(tr.modal.data, tr.modal.data$ageAtGo1Scan, 167, 215, 216)

# Now create a static perf bin variable
tmpDF <- vol.modal.data
tmpDF <- returnPerfBin(tmpDF)
outCol <- tmpDF[,c('bblid','scanid','perfBin')]
colnames(outCol)[3] <- paste('perfCol', 1, sep='')
static.perf.bin <- outCol
colnames(static.perf.bin) <- c('bblid', 'scanid', 'groupFactorLevel')
rm(tmpDF)

## Now load the performance groups I have sent Ruben in the past
tmpDF <- read.csv("/home/gur/gursas/GO/perfBinsForRuben.csv")
tmpDF <- tmpDF[,1:3]
colnames(tmpDF)[3] <- 'groupFactorLevel'
tmpDF$groupFactorLevel <- plyr::revalue(factor(tmpDF$groupFactorLevel), c("1"="lo", "2"="me","3"="hi"))
static.perf.bin <- tmpDF

## Create our values to plot
# Start with volume
child.volume <- doEverythingEver(vol.modal.data, 'mprage_jlf_vol', 0, 167, 'Childhood', cerebellum=T,optionalRace=NULL)
adol.volume <- doEverythingEver(vol.modal.data, 'mprage_jlf_vol', 168, 215, 'Adolescence', cerebellum=T,optionalRace=NULL)
adult.volume <- doEverythingEver(vol.modal.data, 'mprage_jlf_vol', 216, 999, 'Early Adulthood', cerebellum=T,optionalRace=NULL)
child.volumeWM <- doEverythingEverWM(vol.modal.data, 'mprage_jlf_vol', 0, 167, 'Childhood', cerebellum=F,optionalRace=NULL)
adol.volumeWM <- doEverythingEverWM(vol.modal.data, 'mprage_jlf_vol', 168, 215, 'Adolescence', cerebellum=F,optionalRace=NULL)
adult.volumeWM <- doEverythingEverWM(vol.modal.data, 'mprage_jlf_vol', 216, 999, 'Early Adulthood', cerebellum=F,optionalRace=NULL)
all.vol <- rbind(child.volume, adol.volume, adult.volume)
all.vol <- rbind(all.vol,child.volumeWM, adol.volumeWM, adult.volumeWM)

## Now do the age regressed version
age.reg.vol <- doEverythingEver(vol.modal.data.age.reg, 'mprage_jlf_vol', 0, 999, 'Age Regressed', cerebellum=T,optionalRace=NULL)
age.reg.vol <- rbind(age.reg.vol,doEverythingEverWM(vol.modal.data.age.reg, 'mprage_jlf_vol', 0, 999, 'Age Regressed', cerebellum=T,optionalRace=NULL))
all.vol <- age.reg.vol
all.vol$ageBin <- "Age Regressed"

# Now on to GMD
child.gmd <- doEverythingEver(gmd.modal.data, 'mprage_jlf_gmd', 0, 167, 'Childhood', cerebellum=T,optionalRace=NULL)
adol.gmd <- doEverythingEver(gmd.modal.data, 'mprage_jlf_gmd', 168, 215, 'Adolescence', cerebellum=T,optionalRace=NULL)
adult.gmd <- doEverythingEver(gmd.modal.data, 'mprage_jlf_gmd', 216, 999, 'Early Adulthood', cerebellum=T,optionalRace=NULL)
all.gmd <- rbind(child.gmd, adol.gmd, adult.gmd)
all.gmd <- rbind(all.gmd,child.volumeWM, adol.volumeWM, adult.volumeWM)
all.gmd$zScoreDifference[ which(all.gmd$lobe=='WM')] <- NA

# Now onto tr
child.tr <- doEverythingEver(tr.modal.data, 'dti_jlf_tr', 0, 167, 'Childhood', cerebellum=T,optionalRace=NULL)
adol.tr <- doEverythingEver(tr.modal.data, 'dti_jlf_tr', 168, 215, 'Adolescence', cerebellum=T,optionalRace=NULL)
adult.tr <- doEverythingEver(tr.modal.data, 'dti_jlf_tr', 216, 999, 'Early Adulthood', cerebellum=T,optionalRace=NULL)
child.trWM <- doEverythingEverWM(tr.modal.data, 'dti_jlf_tr', 0, 167, 'Childhood', cerebellum=F,optionalRace=NULL)
adol.trWM <- doEverythingEverWM(tr.modal.data, 'dti_jlf_tr', 168, 215, 'Adolescence', cerebellum=F,optionalRace=NULL)
adult.trWM <- doEverythingEverWM(tr.modal.data, 'dti_jlf_tr', 216, 999, 'Early Adulthood', cerebellum=F,optionalRace=NULL)
all.tr <- rbind(child.tr, adol.tr, adult.tr)
all.tr <- rbind(all.tr, child.trWM, adol.trWM, adult.trWM)

## Now clean some factors so they are pb ready
all.vol$ageBin <- revalue(all.vol$ageBin,c("Childhood"="Children","Adolescence"="Adolescents","Early Adulthood"="Young Adults","Age Reregssed" = "Age Regressed"))
all.vol$lobe <- factor(all.vol$lobe, levels=c('Cerebellum','Basal Ganglia','Limbic','Frontal Orbital','Frontal Dorsal','Temporal','Parietal','Occipital','WM'))
all.vol$ROI <- revalue(all.vol$ROI, c("FRO WM"="FRO","TEM WM"="TEM","PAR WM"="PAR","OCC WM"="OCC","Limbic WM"="Limbic","Insular WM"="Insular"))
all.vol$lobe <- revalue(all.vol$lobe, c('Basal Ganglia'='Basal/ST','Frontal Orbital'='Frontal','Frontal Dorsal'='Frontal'))
all.gmd$ageBin <- revalue(all.gmd$ageBin,c("Childhood"="Children","Adolescence"="Adolescents","Early Adulthood"="Young Adults"))
all.gmd$lobe <- factor(all.gmd$lobe, levels=c('Cerebellum','Basal Ganglia','Limbic','Frontal Orbital','Frontal Dorsal','Temporal','Parietal','Occipital','WM'))
all.gmd$lobe <- revalue(all.gmd$lobe, c('Basal Ganglia'='Basal/ST','Frontal Orbital'='Frontal','Frontal Dorsal'='Frontal'))
all.tr$ageBin <- revalue(all.tr$ageBin,c("Childhood"="Children","Adolescence"="Adolescents","Early Adulthood"="Young Adults"))
all.tr$lobe <- factor(all.tr$lobe, levels=c('Cerebellum','Basal Ganglia','Limbic','Frontal Orbital','Frontal Dorsal','Temporal','Parietal','Occipital','WM'))
all.tr$ROI <- revalue(all.tr$ROI, c("FRO WM"="FRO","TEM WM"="TEM","PAR WM"="PAR","OCC WM"="OCC","Limbic WM"="Limbic","Insular WM"="Insular"))
all.tr$lobe <- revalue(all.tr$lobe, c('Basal Ganglia'='Basal/ST','Frontal Orbital'='Frontal','Frontal Dorsal'='Frontal'))

outPlot <- ggplot(all.vol, aes(y=zScoreDifference, x=ROI, group=Gender, fill=Gender)) +
      geom_line(aes(linetype=Gender,color=Gender), size=1) +
      geom_point(aes(shape=Gender, color=Gender), size=2.5) +
      #geom_errorbar(aes(ymin=meanValue-standErrValue, ymax=meanValue+standErrValue)) + 
      scale_y_continuous(limits=c(0, 1), 
                           breaks=round(seq(-.4,1.4,.4), digits=2)) +
      xlab("ROI") +
      ylab("Effect Size") +
      geom_hline(aes(yintercept=0), linetype="longdash", colour="black", size=0.5) +
      scale_colour_manual(name = "Gender",
                          values=c("Male"="blue","Female"="red")) +
      scale_linetype_manual(name = "Gender", values = c("Male" = "solid", "Female" = "solid")) +
      scale_shape_manual(name = "Gender", values = c("Male"=16,"Female"=15)) +
      #scale_shape_manual(values=c(16), guide=FALSE) +
      theme_bw() +
      theme(legend.position="none") +
      theme(axis.text.x = element_blank(), 
      axis.text.y = element_text(face="bold", size=8), axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank(), strip.text.x = element_text(size=10)) +
      facet_grid(ageBin ~ lobe, scales="free", space="free_x")

outPlot2 <- ggplot(all.gmd, aes(y=zScoreDifference, x=ROI, group=Gender)) +
      geom_line(aes(linetype=Gender,color=Gender), size=1) +
      geom_point(aes(shape=Gender, color=Gender), size=2.5) +
      #geom_errorbar(aes(ymin=meanValue-standErrValue, ymax=meanValue+standErrValue)) + 
      scale_y_continuous(limits=c(-.4, 1.5), 
                           breaks=round(seq(-.4,1.4,.4), digits=2)) +
      xlab("ROI") +
      ylab("Effect Size") +
      geom_hline(aes(yintercept=0), linetype="longdash", colour="black", size=0.5) +
      scale_colour_manual(name = "Gender",
                          values=c("Male"="blue","Female"="red")) +
      scale_linetype_manual(name = "Gender", values = c("Male" = "solid", "Female" = "solid")) +
      scale_shape_manual(name = "Gender", values = c("Male"=16,"Female"=15 )) +
      #scale_shape_manual(values=c(16), guide=FALSE) +
      theme_bw() +
      theme(legend.position="none") +
      theme(axis.text.x = element_blank(), 
      axis.text.y = element_text(face="bold", size=8), axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank(), strip.text.x = element_text(size=10)) +
      facet_grid(ageBin ~ lobe, scales="free", space="free_x")

outPlot3 <- ggplot(all.tr, aes(y=zScoreDifference, x=ROI, group=Gender)) +
      geom_line(aes(linetype=Gender,color=Gender), size=1) +
      geom_point(aes(shape=Gender, color=Gender), size=2.5) +
      #geom_errorbar(aes(ymin=meanValue-standErrValue, ymax=meanValue+standErrValue)) + 
      scale_y_continuous(limits=c(-1.4, 1), 
                           breaks=round(seq(-1.4,1.4,.4), digits=2)) +
      xlab("ROI") +
      ylab("Effect Size") +
      geom_hline(aes(yintercept=0), linetype="longdash", colour="black", size=0.5) +
      scale_colour_manual(name = "Gender",
                          values=c("Male"="blue","Female"="red")) +
      scale_linetype_manual(name = "Gender", values = c("Male" = "solid", "Female" = "solid")) +
      scale_shape_manual(name = "Gender", values = c("Male"=16,"Female"=15)) +
      #scale_shape_manual(values=c(16), guide=FALSE) +
      theme_bw() +
      theme(legend.position="none") +
      theme(axis.text.x = element_blank(), 
      axis.text.y = element_text(face="bold", size=8), axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank(), strip.text.x = element_text(size=10)) +
      facet_grid(ageBin ~ lobe, scales="free", space="free_x")


png("volumeRegionalEffects.png", width=14,height=5,units='in',res=300)
outPlot
dev.off()
png("gmdRegionalEffects.png", width=14,height=5,units='in',res=300)
outPlot2
dev.off()
png("trRegionalEffects.png", width=14,height=5,units='in',res=300)
outPlot3
dev.off()

## Now make a plot with x axis text
colnames(all.vol)[6] <- 'Sex'
outPlot <- ggplot(all.vol, aes(y=zScoreDifference, x=ROI, group=Sex)) +
      geom_line(aes(linetype=Sex,color=Sex), size=1) +
      geom_point(aes(shape=Sex, color=Sex), size=2.5) +
      #geom_errorbar(aes(ymin=meanValue-standErrValue, ymax=meanValue+standErrValue)) + 
      scale_y_continuous(limits=c(-.4, 1.4), 
                           breaks=round(seq(-.4,1.4,.4), digits=2)) +
      xlab("ROI") +
      ylab("Effect Size") +
      geom_hline(aes(yintercept=0), linetype="longdash", colour="black", size=0.5) +
      scale_colour_manual(name = "Sex",
                          values=c("Male"="blue","Female"="red")) +
      scale_linetype_manual(name = "Sex", values = c("Male" = "solid", "Female" = "solid")) +
      scale_shape_manual(name = "Sex", values = c("Male"=16,"Female"=15)) +
      #scale_shape_manual(values=c(16), guide=FALSE) +
      theme_bw() +
      theme(legend.position="bottom") +
      theme(axis.text.x = element_text(angle = 45,hjust = 1, face = "bold"), 
      axis.text.y = element_text(face="bold", size=8), axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_blank(), strip.text.x = element_text(size=10),legend.text=element_text(size=16)) +
      facet_grid(ageBin ~ lobe, scales="free", space="free_x")

library(gtable)
g <- ggplotGrob(outPlot)
s <- gtable_filter(g, 'xlab|axis-b|guide', trim=F)  # use trim depending on need
png("outXAxisValues.png",width=14, height=5, units='in', res=300)
grid.newpage()
grid.draw(s)
dev.off()
