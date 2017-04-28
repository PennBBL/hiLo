# This script is just going to be used to produce some age*sex cbf interaction graphs
# really quick and dirty 

# Load library(s)
source('/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R')
source('/home/adrose/dataPrepForHiLoPaper/scripts/functions.R')
install_load('ggplot2', 'visreg', 'gamm4')

# Load data
data.values <- read.csv('/home/analysis/redcap_data/201602/go1/n1601_go1_datarel_020716.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/rawData/n1601_jlfCbf.csv')

# Prep the data 
cbf.data$ageAtGo1Scan <- data.values$ageAtGo1Scan[match(cbf.data$bblid, data.values$bblid)]
cbf.data$sex <- data.values$sex[match(cbf.data$bblid, data.values$bblid)]
cbf.data <- cbf.data[which(cbf.data$pcaslExclude!=1),]

# Now plot some shiz
pdf('cbfCheck.pdf')
fit1 <- lm(pcasl_jlf_cbf_R_Hippocampus ~ ageAtGo1Scan*sex, data=cbf.data)
visreg(fit1, 'ageAtGo1Scan', by='sex', overlay=T)
fit2 <- lm(pcasl_jlf_cbf_L_Hippocampus ~ ageAtGo1Scan*sex, data=cbf.data)
visreg(fit2, 'ageAtGo1Scan', by='sex', overlay=T)
fit3 <- lm(pcasl_jlf_cbf_R_Ains ~ ageAtGo1Scan*sex, data=cbf.data)
visreg(fit3, 'ageAtGo1Scan', by='sex', overlay=T)
fit4 <- lm(pcasl_jlf_cbf_L_Ains ~ ageAtGo1Scan*sex, data=cbf.data)
visreg(fit4, 'ageAtGo1Scan', by='sex', overlay=T)
fit5 <- lm(pcasl_jlf_cbf_R_SFG ~ ageAtGo1Scan*sex, data=cbf.data)
visreg(fit5, 'ageAtGo1Scan', by='sex', overlay=T)
fit6 <- lm(pcasl_jlf_cbf_R_SFG ~ ageAtGo1Scan*sex, data=cbf.data)
visreg(fit6, 'ageAtGo1Scan', by='sex', overlay=T)
dev.off()


pdf('cbfCheckNonLinear.pdf')
gm.test <- gam(pcasl_jlf_cbf_R_Hippocampus ~ s(ageAtGo1Scan) + sex + s(ageAtGo1Scan, by=sex), data=cbf.data) 
visreg(gm.test, 'ageAtGo1Scan', by='sex', overlay=T, main='R Hippocampus')
gm.test <- gam(pcasl_jlf_cbf_L_Hippocampus ~ s(ageAtGo1Scan) + sex + s(ageAtGo1Scan, by=sex), data=cbf.data) 
visreg(gm.test, 'ageAtGo1Scan', by='sex', overlay=T, main='L Hippocampus')
gm.test <- gam(pcasl_jlf_cbf_R_Ains ~ s(ageAtGo1Scan) + sex + s(ageAtGo1Scan, by=sex), data=cbf.data) 
visreg(gm.test, 'ageAtGo1Scan', by='sex', overlay=T, main='R Anteroir Insula')
gm.test <- gam(pcasl_jlf_cbf_L_Ains ~ s(ageAtGo1Scan) + sex + s(ageAtGo1Scan, by=sex), data=cbf.data) 
visreg(gm.test, 'ageAtGo1Scan', by='sex', overlay=T, main='L Anterior Insula')
gm.test <- gam(pcasl_jlf_cbf_R_SFG ~ s(ageAtGo1Scan) + sex + s(ageAtGo1Scan, by=sex), data=cbf.data) 
visreg(gm.test, 'ageAtGo1Scan', by='sex', overlay=T, main='R Superior Frontal Gyrus')
gm.test <- gam(pcasl_jlf_cbf_L_SFG ~ s(ageAtGo1Scan) + sex + s(ageAtGo1Scan, by=sex), data=cbf.data) 
visreg(gm.test, 'ageAtGo1Scan', by='sex', overlay=T, main='L Superior Frontal Gyrus')
dev.off()
