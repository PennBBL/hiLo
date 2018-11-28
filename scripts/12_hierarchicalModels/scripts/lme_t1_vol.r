

library(reshape2)
library(plyr)
library(nlme)
library(lme4)
library(lattice)
#library(effects)


#demographics
demo<-read.csv("/home/analysis/psycha1/pnc/n1601_demographics_go1_20161212.csv")
demo$diff<-abs(demo$ageAtScan1-demo$ageAtCnb1)

#remove those that were scanned > 1 year apart from the CNB
demo2<-demo[demo$diff<=12,]#1354 

#define age bins
#Childhood= 0-167
#Adolescence=168-215
#Adulthood=216+
demo2$ageBin<-NA
demo2$ageBin[demo2$ageAtScan1<=167]<-1
demo2$ageBin[demo2$ageAtScan1>167 & demo2$ageAtScan1<216]<-2
demo2$ageBin[demo2$ageAtScan1>=216]<-3

#health
health<-read.csv("/home/analysis/psycha1/pnc/n1601_health_20161214.csv")
health2<-health[health$incidentalFindingExclude==0,]
#remove incidental findings
demo3<-demo2[demo2$bblid %in% health2$bblid,]#1336
##cnb factor scores
#cnb<-read.csv("/home/analysis/psycha1/pnc/n1601_cnb_factor_scores_tymoore_20151006.csv")
#age regressed
cnb<-read.csv("/home/tymoore/CNB_Factor_Scores_GO1-GO2-GO3.csv")
cnb1<-cnb[cnb$timepoint==1,]#9418
cnb2<-merge(demo3,cnb1,by="bblid")#1333



#read original neuroimaging data
vol<-read.csv("/home/analysis/psycha1/pnc/n1601_jlfAntsCTIntersectionVol_20170412.csv")
wmvol<-read.csv("/home/analysis/psycha1/pnc/n1601_jlfWmVol_20170412.csv")
vol<-merge(vol,wmvol,by=c("bblid","scanid"))
volqa<-read.csv("/home/analysis/psycha1/pnc/n1601_t1QaData_20170306.csv")
vol<-merge(vol,volqa,by=c("bblid","scanid"))
vol2<-vol[vol$t1Exclude==0,]#1540
vol3<-merge(cnb2,vol2,by=c("bblid","scanid"))#1280


roiselectvol<-c("mprage_jlf_vol_L_Putamen","mprage_jlf_vol_L_Pallidum","mprage_jlf_vol_L_Caudate","mprage_jlf_vol_L_Accumbens_Area",
"mprage_jlf_vol_L_Thalamus_Proper","mprage_jlf_vol_L_Ent","mprage_jlf_vol_L_PHG","mprage_jlf_vol_L_Amygdala","mprage_jlf_vol_L_Hippocampus",
"mprage_jlf_vol_L_SCA","mprage_jlf_vol_L_AIns","mprage_jlf_vol_L_PIns","mprage_jlf_vol_L_ACgG","mprage_jlf_vol_L_MCgG",
"mprage_jlf_vol_L_PCgG","mprage_jlf_vol_L_GRe","mprage_jlf_vol_L_MOrG","mprage_jlf_vol_L_POrG","mprage_jlf_vol_L_MFC",
"mprage_jlf_vol_L_LOrG","mprage_jlf_vol_L_AOrG","mprage_jlf_vol_L_OrIFG","mprage_jlf_vol_L_FRP","mprage_jlf_vol_L_FO",
"mprage_jlf_vol_L_TrIFG","mprage_jlf_vol_L_OpIFG","mprage_jlf_vol_L_MSFG","mprage_jlf_vol_L_MFG","mprage_jlf_vol_L_PrG",
"mprage_jlf_vol_L_SFG","mprage_jlf_vol_L_SMC","mprage_jlf_vol_L_TMP","mprage_jlf_vol_L_FuG","mprage_jlf_vol_L_ITG",
"mprage_jlf_vol_L_MTG","mprage_jlf_vol_L_PP","mprage_jlf_vol_L_STG","mprage_jlf_vol_L_TTG","mprage_jlf_vol_L_CO","mprage_jlf_vol_L_PT",
"mprage_jlf_vol_L_PO","mprage_jlf_vol_L_AnG","mprage_jlf_vol_L_PCu","mprage_jlf_vol_L_SMG","mprage_jlf_vol_L_PoG","mprage_jlf_vol_L_SPL",
"mprage_jlf_vol_L_MPrG","mprage_jlf_vol_L_MPoG","mprage_jlf_vol_L_OFuG","mprage_jlf_vol_L_LiG","mprage_jlf_vol_L_IOG",
"mprage_jlf_vol_L_OCP","mprage_jlf_vol_L_Calc","mprage_jlf_vol_L_Cun","mprage_jlf_vol_L_MOG","mprage_jlf_vol_L_SOG",
"mprage_jlf_vol_R_Putamen","mprage_jlf_vol_R_Pallidum","mprage_jlf_vol_R_Caudate","mprage_jlf_vol_R_Accumbens_Area",
"mprage_jlf_vol_R_Thalamus_Proper","mprage_jlf_vol_R_Ent","mprage_jlf_vol_R_PHG","mprage_jlf_vol_R_Amygdala","mprage_jlf_vol_R_Hippocampus",
"mprage_jlf_vol_R_SCA","mprage_jlf_vol_R_AIns","mprage_jlf_vol_R_PIns","mprage_jlf_vol_R_ACgG","mprage_jlf_vol_R_MCgG",
"mprage_jlf_vol_R_PCgG","mprage_jlf_vol_R_GRe","mprage_jlf_vol_R_MOrG","mprage_jlf_vol_R_POrG","mprage_jlf_vol_R_MFC",
"mprage_jlf_vol_R_LOrG","mprage_jlf_vol_R_AOrG","mprage_jlf_vol_R_OrIFG","mprage_jlf_vol_R_FRP","mprage_jlf_vol_R_FO",
"mprage_jlf_vol_R_TrIFG","mprage_jlf_vol_R_OpIFG","mprage_jlf_vol_R_MSFG","mprage_jlf_vol_R_MFG","mprage_jlf_vol_R_PrG",
"mprage_jlf_vol_R_SFG","mprage_jlf_vol_R_SMC","mprage_jlf_vol_R_TMP","mprage_jlf_vol_R_FuG","mprage_jlf_vol_R_ITG",
"mprage_jlf_vol_R_MTG","mprage_jlf_vol_R_PP","mprage_jlf_vol_R_STG","mprage_jlf_vol_R_TTG","mprage_jlf_vol_R_CO",
"mprage_jlf_vol_R_PT","mprage_jlf_vol_R_PO","mprage_jlf_vol_R_AnG","mprage_jlf_vol_R_PCu","mprage_jlf_vol_R_SMG",
"mprage_jlf_vol_R_PoG","mprage_jlf_vol_R_SPL","mprage_jlf_vol_R_MPrG","mprage_jlf_vol_R_MPoG","mprage_jlf_vol_R_OFuG",
"mprage_jlf_vol_R_LiG","mprage_jlf_vol_R_IOG","mprage_jlf_vol_R_OCP","mprage_jlf_vol_R_Calc","mprage_jlf_vol_R_Cun",
"mprage_jlf_vol_R_MOG","mprage_jlf_vol_R_SOG")

#wide to long data frame
vol4<-melt(vol3[,c("bblid","scanid","sex","race2","ageBin","F1_Exec_Comp_Cog_Accuracy",roiselectvol)], id.vars=c("bblid", "scanid","sex","race2","ageBin","F1_Exec_Comp_Cog_Accuracy"))
names(vol4)[7:8]<-c("roi","vol")

#lobe definitions
rois_L_ST<-c("mprage_jlf_vol_L_Accumbens_Area","mprage_jlf_vol_L_Caudate","mprage_jlf_vol_L_Pallidum","mprage_jlf_vol_L_Putamen","mprage_jlf_vol_L_Thalamus_Proper")

rois_R_ST<-c("mprage_jlf_vol_R_Accumbens_Area","mprage_jlf_vol_R_Caudate","mprage_jlf_vol_R_Pallidum","mprage_jlf_vol_R_Putamen","mprage_jlf_vol_R_Thalamus_Proper")

rois_L_Lim<-c("mprage_jlf_vol_L_Amygdala","mprage_jlf_vol_L_Hippocampus","mprage_jlf_vol_L_PHG","mprage_jlf_vol_L_ACgG","mprage_jlf_vol_L_GRe","mprage_jlf_vol_L_MCgG","mprage_jlf_vol_L_PCgG")

rois_R_Lim<-c("mprage_jlf_vol_R_Amygdala","mprage_jlf_vol_R_Hippocampus","mprage_jlf_vol_R_PHG","mprage_jlf_vol_R_ACgG","mprage_jlf_vol_R_GRe","mprage_jlf_vol_R_MCgG","mprage_jlf_vol_R_PCgG")

rois_L_FrOrb<-c("mprage_jlf_vol_L_AOrG","mprage_jlf_vol_L_POrG","mprage_jlf_vol_L_FO","mprage_jlf_vol_L_FRP","mprage_jlf_vol_L_LOrG","mprage_jlf_vol_L_MOrG",
"mprage_jlf_vol_L_OpIFG")

rois_R_FrOrb<-c("mprage_jlf_vol_R_AOrG","mprage_jlf_vol_R_POrG","mprage_jlf_vol_R_FO","mprage_jlf_vol_R_FRP","mprage_jlf_vol_R_LOrG",
"mprage_jlf_vol_R_MOrG","mprage_jlf_vol_R_OpIFG")

rois_L_FrDors<-c("mprage_jlf_vol_L_MSFG","","","mprage_jlf_vol_L_PrG","mprage_jlf_vol_L_MFC","mprage_jlf_vol_L_MFG","mprage_jlf_vol_L_SFG","mprage_jlf_vol_L_SMC",
"mprage_jlf_vol_L_OrIFG","mprage_jlf_vol_L_MPrG","mprage_jlf_vol_L_TrIFG")

rois_R_FrDors<-c("mprage_jlf_vol_R_MSFG","","mprage_jlf_vol_R_PrG","mprage_jlf_vol_R_MFC","mprage_jlf_vol_R_MFG","mprage_jlf_vol_R_SFG","mprage_jlf_vol_R_SMC","mprage_jlf_vol_R_OrIFG","mprage_jlf_vol_R_MPrG","mprage_jlf_vol_R_TrIFG")


rois_L_Tem<-c("mprage_jlf_vol_L_ITG","mprage_jlf_vol_L_MTG","mprage_jlf_vol_L_PP","mprage_jlf_vol_L_PT","mprage_jlf_vol_L_AIns","mprage_jlf_vol_L_PIns",
"mprage_jlf_vol_L_SCA","mprage_jlf_vol_L_STG","mprage_jlf_vol_L_TMP","mprage_jlf_vol_L_TTG","mprage_jlf_vol_L_Ent","mprage_jlf_vol_L_FuG")

rois_R_Tem<-c("mprage_jlf_vol_R_ITG","mprage_jlf_vol_R_MTG","mprage_jlf_vol_R_PP","mprage_jlf_vol_R_PT","mprage_jlf_vol_R_AIns","mprage_jlf_vol_R_PIns","mprage_jlf_vol_R_SCA","mprage_jlf_vol_R_STG","mprage_jlf_vol_R_TMP","mprage_jlf_vol_R_TTG","mprage_jlf_vol_R_Ent","mprage_jlf_vol_R_FuG")


rois_L_Occ<-c("mprage_jlf_vol_L_Calc","mprage_jlf_vol_L_IOG","mprage_jlf_vol_L_Cun","mprage_jlf_vol_L_LiG","mprage_jlf_vol_L_MOG","mprage_jlf_vol_L_OCP","mprage_jlf_vol_L_OFuG","mprage_jlf_vol_L_SOG")


rois_R_Occ<-c("mprage_jlf_vol_R_Calc","mprage_jlf_vol_R_IOG","mprage_jlf_vol_R_Cun","mprage_jlf_vol_R_LiG","mprage_jlf_vol_R_MOG","mprage_jlf_vol_R_OCP","mprage_jlf_vol_R_OFuG","mprage_jlf_vol_R_SOG")


rois_L_Par<-c("mprage_jlf_vol_L_AnG","mprage_jlf_vol_L_GEe","mprage_jlf_vol_L_MPoG","mprage_jlf_vol_L_PCu","mprage_jlf_vol_L_PO","mprage_jlf_vol_L_PoG",
"mprage_jlf_vol_L_SMG","mprage_jlf_vol_L_SPL","mprage_jlf_vol_L_CO")

rois_R_Par<-c("mprage_jlf_vol_R_AnG","mprage_jlf_vol_R_GRe","mprage_jlf_vol_R_MPoG","mprage_jlf_vol_R_PCu","mprage_jlf_vol_R_PO","mprage_jlf_vol_R_PoG","mprage_jlf_vol_R_SMG","mprage_jlf_vol_R_SPL","mprage_jlf_vol_R_CO")



vol4$lobe<-vol4$roi
vol4$lobe<-as.character(vol4$lobe)
vol4$lobe[vol4$lobe %in% rois_L_ST]<-"lST"
vol4$lobe[vol4$lobe %in% rois_R_ST]<-"rST"
vol4$lobe[vol4$lobe %in% rois_L_Lim]<-"lLim"
vol4$lobe[vol4$lobe %in% rois_R_Lim]<-"rLim"
vol4$lobe[vol4$lobe %in% rois_L_FrOrb]<-"lFrOrb"
vol4$lobe[vol4$lobe %in% rois_R_FrOrb]<-"rFrOrb"
vol4$lobe[vol4$lobe %in% rois_L_FrDors]<-"lFrDors"
vol4$lobe[vol4$lobe %in% rois_R_FrDors]<-"rFrDors"
vol4$lobe[vol4$lobe %in% rois_L_Tem]<-"lTem"
vol4$lobe[vol4$lobe %in% rois_R_Tem]<-"rTem"
vol4$lobe[vol4$lobe %in% rois_L_Occ]<-"lOcc"
vol4$lobe[vol4$lobe %in% rois_R_Occ]<-"rOcc"
vol4$lobe[vol4$lobe %in% rois_L_Par]<-"lPar"
vol4$lobe[vol4$lobe %in% rois_R_Par]<-"rPar"
vol4$lobe<-factor(vol4$lobe)

#get base ROI name


#at roi level
vol4$roi2<-vol4$roi
vol4$roi2<-gsub("_L_", "",vol4$roi2)
vol4$roi2<-gsub("_R_", "",vol4$roi2)
vol4$roi2<-gsub("mprage_jlf_vol_", "",vol4$roi2)
vol4$roi2<-gsub("mprage_jlf_vol", "",vol4$roi2)
vol4$roi2<-factor(vol4$roi2)

#get hemisphere
vol4$roi<-factor(vol4$roi)
vol4$hem<-sapply(vol4$roi, function(x) unlist(strsplit(as.character(x),"_"))[4])
vol4$hem[vol4$hem!="R" & vol4$hem!="L"]<-NA
vol4$hem<-factor(vol4$hem)

#merge performance bins

#performance bin categories from Adon
pcat<-read.csv("~/perfBinsForRuben.csv")
vol4<-merge(pcat,vol4, by=c("bblid","scanid"))

#write out for other modalities to use
write.csv(vol4,file="finalvol.csv",na="",row.names=F)

#explicitly calculate the sum by lobe for each bblid
vol5<-ddply(vol4,c('bblid','lobe'),summarise,sum=sum(vol))
vol5<-merge(vol3[,c("bblid","ageBin","sex")],vol5,by="bblid")
vol5<-merge(vol5,pcat,by=c("bblid"))


#get base lobe name
vol5$lobe2<-vol5$lobe
vol5$lobe2<-gsub("l", "",vol5$lobe2)
vol5$lobe2<-gsub("r", "",vol5$lobe2)
vol5$lobe2<-factor(vol5$lobe2)
vol5$hem<-sapply(vol5$lobe, function(x) substr(x,start=1,stop=1))

#mixed model testing perf by lobe
vol5$lobe<-as.factor(vol5$lobe)
vol5$ageBin<-as.factor(vol5$ageBin)
vol5$perfColStatic<-as.factor(vol5$perfCol1)
vol5$hem<-as.factor(vol5$hem)

library(lmerTest)
m1.lme=lmer(sum ~ ageBin*sex*perfColStatic*lobe2*hem+(1|bblid), REML=FALSE, data=vol5)
anova.m1 <- lmerTest::anova(m1.lme, ddf = "Satterthwaite", type = 3)
write.csv(anova.m1, file=paste0("lme_lobe_anova_out_","vol",".csv"))
#posthoc interaction effects using least square means
detach(package:lmerTest)
library(lsmeans)
sink(file = paste0("lsmout_",modality,".txt"))
lsm1 <- lsmeans(m1.lme, ~ lobe|perfColVolume)
pairs(lsm1)
sink()
#model diagnostics: plot random effects 
pdf(paste0("lme_lobe_randomeffects_","vol",".pdf"))
qqmath(ranef(m1.lme), strip = FALSE)$bblid
dev.off()



#if lobe by performance bin is significant then run mixed model for each roi within lobe
library(lmerTest)

resultsAnova <- vector("list", length(levels(vol4$lobe2)))
names(resultsAnova)<-levels(vol4$lobe2)
pdf(paste0("lme_roi_randomeffects_","vol",".pdf"))
for(i in 1:length(levels(vol4$lobe2))){
data<-vol4[which(vol4$lobe2==levels(vol4$lobe2)[i]),]
data$ageBin<-as.factor(data$ageBin)
data$hem<-as.factor(data$hem)
data$perfColStatic<-as.factor(data$perfColStatic)
data$roi2<-as.factor(data$roi2)
m2.lme <- lmer(value~ageBin * sex * perfColStatic *roi2 *hem + (1|bblid),REML=FALSE,data=data)
anova.m2 <- lmerTest::anova(m2.lme, ddf = "Satterthwaite", type = 3)
resultsAnova[[i]] <- anova.m2
qqmath(ranef(m2.lme), strip = FALSE)$bblid
#plot(Effect(c("roi2","perfColStatic"),m2.lme),multiline=TRUE)
}
dev.off()
#lapply(resultsAnova, write, "lme_roi_anova_out_vol.txt", append=TRUE, ncolumns=1000)
sink(file = paste0("lme_roi_anova_out_","vol",".txt"))
print(resultsAnova)
sink()



