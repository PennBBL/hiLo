# This script is going to be used to produce the hi - lo graphs from jlf data
# hi - lo will just not die....
## Load Library(s)
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm2Functions.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm1Functions2.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/functions-forJLF.R")
install_load('plyr', 'ggplot2', 'reshape2', 'grid', 'gridExtra', 'labeling', 'data.table')

# Declare any functions
# Now load the data
vol.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv')
cbf.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv')
gmd.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv')
tr.modal.data.age.reg <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv')

vol.modal.data.age.reg$ageBin <- 'Age Regressed'
gmd.modal.data.age.reg$ageBin <- 'Age Regressed'
cbf.modal.data.age.reg$ageBin <- 'Age Regressed'
tr.modal.data.age.reg$ageBin <- 'Age Regressed'

## Now prep the data 
age.reg.vol <- doEverythingEver(vol.modal.data.age.reg, 'mprage_jlf_vol', 0, 999, 'Age Regressed', cerebellum=F,optionalRace=NULL)
age.reg.vol$modal <- 'Volume'
age.reg.cbf <- doEverythingEver(cbf.modal.data.age.reg, 'pcasl_jlf_cbf', 0, 999, 'Age Regressed', cerebellum=F,optionalRace=NULL)
age.reg.cbf$modal <- 'CBF'
age.reg.gmd <- doEverythingEver(gmd.modal.data.age.reg, 'mprage_jlf_gmd', 0, 999, 'Age Regressed', cerebellum=F,optionalRace=NULL)
age.reg.gmd$modal <- 'GMD'
age.reg.tr <- doEverythingEver(tr.modal.data.age.reg, 'dti_jlf_tr', 0, 167, 'Age Regressed', cerebellum=F,optionalRace=NULL)
age.reg.tr$modal <- 'TR'

#Now combine all data 
toPlot <- rbind(age.reg.vol, age.reg.cbf, age.reg.gmd, age.reg.tr)
toPlot$modal <- factor(toPlot$modal, levels=c('Volume', 'GMD', 'CBF', 'TR'))
toPlot$group <- paste(toPlot$groupLevel, toPlot$Gender)
plotToReturn <- ggplot(toPlot, aes(y=meanValue, x=ROI, group=group, color=group)) +
      geom_line( size=5) +
      geom_point(size=1.5) +
      xlab("ROI") +
      ylab("z-score") +
      geom_hline(aes(yintercept=0), linetype="longdash", colour="black", size=0.5) +
      scale_colour_manual(name = "group",
                          values=c("hi Male"="blue1","hi Female"="red1", "lo Male"="turquoise", "lo Female"="orange")) +
      theme_bw() +
      theme(legend.position="top") +
      theme(text=element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1, face="bold"), 
      axis.text.y = element_text(face="bold", size=24), axis.title.x = element_text(face="bold", size=28),
      axis.title.y = element_text(face="bold", size=28),
      plot.title = element_text(face="bold", size=28), strip.text.x = element_text(size=15)) +
      coord_cartesian(ylim = c(-.6, .6)) + 
      facet_grid(modal ~ lobe, scales="free", space="free_x")

pdf('testingHiLo.pdf', height=20, width=20)
plotToReturn
dev.off()
