#########################################################################
# Define our lobes
#########################################################################
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

## declare lobe names
lobe.names <- c("ST","Lim","FrOrb","FrDors","Tem","Occ","Par")

#########################################################################
# Load data
#########################################################################
source('/home/adrose/hiLo/scripts/01_DataPrep/functions/functions.R')
source("/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm2Functions.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/wm1Functions2.R")
source("/home/adrose/hiLo/scripts/03_CognitiveTrends/functions/functions-forJLF.R")
x <- read.csv('/home/adrose/forRuben/data/tmp/n1601_imagingDataDump_2018-09-20.csv')

#########################################################################
# Run volume
#########################################################################
out.hemi.vals <- matrix(NA, nrow=1601, ncol=(length(lobe.names)*2)+2)
out.hemi.vals[,1] <- x$bblid
out.hemi.vals[,2] <- x$scanid
colnames(out.hemi.vals) <- c('bblid','scanid',rep(lobe.names,2))
colInd <- 3
for(i in lobe.names){
  print(i)
  ## Now add hemisphere
  for(h in c("L","R")){
    ## Declare our hemi name
    hemi.var <- paste("rois_",h,"_",i, sep='')
    val.names <- get(hemi.var)
    ## Now rowSum these values
    out.variable <- rowSums(x[,which(names(x) %in% val.names)])
    out.hemi.vals[,colInd] <- out.variable
    colnames(out.hemi.vals)[colInd] <- paste("mprage_vol_",hemi.var,sep='')
    colInd <- colInd + 1
  }
}

#########################################################################
# Run all other vals
#########################################################################
modal.names <- c('mprage_gmd','pcasl_cbf','dti_tr', 'rest_reho', 'rest_alff')
all.out <- out.hemi.vals
for(m in modal.names){
  out.hemi.vals <- matrix(NA, nrow=1601, ncol=(length(lobe.names)*2))
  colnames(out.hemi.vals) <- rep(lobe.names,2)
  colInd <- 1
  for(i in lobe.names){
    for(h in c("L","R")){
      ## First declare our volume variable values
      hemi.var <- paste("rois_",h,"_",i, sep='')
      val.names <- get(hemi.var)
      ## Now do our modality specific
      val.names.2 <- gsub(pattern="mprage", x=val.names, replacement=strSplitMatrixReturn(m, "_")[1])
      val.names.2 <- gsub(pattern="vol", x=val.names.2, replacement=strSplitMatrixReturn(m, "_")[2])
      # Now calculate our weighted means
      out.variable <- NULL
      for(q in 1:1601){
        # Caluclate the weighted mean
        tmp.val <- weighted.mean(x[q,which(names(x) %in% val.names.2)], w=x[q,which(names(x) %in% val.names)], na.rm=T)
        out.variable <- append(out.variable, tmp.val)
      }
      out.hemi.vals[,colInd] <- out.variable
      colnames(out.hemi.vals)[colInd] <- paste(m,hemi.var,sep='_')
      colInd <- colInd + 1
    }
  }
  all.out <- cbind(all.out, out.hemi.vals)
}

