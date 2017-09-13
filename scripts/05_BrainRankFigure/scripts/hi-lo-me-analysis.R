library(psych)

calculateDeltaHiMeLo <- function(data, suffix) {
  
  data$F1_Exec_Comp_Cog_Accuracy
  quantiles <- quantile(data$F1_Exec_Comp_Cog_Accuracy, c(0,.3333,.6666,1))
  
  data$PerformanceGroup <- 0
  data$PerformanceGroup[which(data$F1_Exec_Comp_Cog_Accuracy < quantiles[2])] <- 1
  data$PerformanceGroup[which(data$F1_Exec_Comp_Cog_Accuracy >= quantiles[2] &
                          data$F1_Exec_Comp_Cog_Accuracy < quantiles[3])] <- 2
  data$PerformanceGroup[which(data$F1_Exec_Comp_Cog_Accuracy >= quantiles[3])] <- 3
  
  roi.index <- grep(pattern = suffix, x = names(data))
  
  
  output <- as.data.frame(matrix(NA, 
                                 nrow = length(roi.index), 
                                 ncol= (4)))
  
  j <- 1
  for (i in roi.index) {
    temp.matrix <- describeBy(scale(data[,i]), group=data$PerformanceGroup, mat = T)
    meanvals <- temp.matrix$mean
    output[j,1] <- names(data)[i]
    output[j,2] <- meanvals[1] - meanvals[2]
    output[j,3] <- meanvals[3] - meanvals[2]
    output[j,4] <- meanvals[3] - meanvals[1]
    
    j <- j + 1
  }
  
  names(output) <- c("roi","me-lo","hi-me","hi-lo")
  return(output)
}


data.cbf <- read.csv("/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/cbfData.csv")
suffix.cbf <- "pcasl_jlf_cbf_"

data.cbf.male <- data.cbf[which(data.cbf$sex == 1), ]
output.cbf.male <- calculateDeltaHiMeLo(data.cbf.male, suffix.cbf)

data.cbf.female <- data.cbf[which(data.cbf$sex == 2), ]
output.cbf.female <- calculateDeltaHiMeLo(data.cbf.female, suffix.cbf)

data.vol <- read.csv("/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/volumeData.csv")
suffix.vol <- "mprage_jlf_vol_"

data.vol.male <- data.vol[which(data.vol$sex == 1), ]
output.vol.male <- calculateDeltaHiMeLo(data.vol.male, suffix.vol)

data.vol.female <- data.vol[which(data.vol$sex == 2), ]
output.vol.female <- calculateDeltaHiMeLo(data.vol.female, suffix.vol)

data.gmd <- read.csv("/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/gmdData.csv")
suffix.gmd <- "mprage_jlf_gmd_"

data.gmd.male <- data.gmd[which(data.gmd$sex == 1), ]
output.gmd.male <- calculateDeltaHiMeLo(data.gmd.male, suffix.gmd)

data.gmd.female <- data.gmd[which(data.gmd$sex == 2), ]
output.gmd.female <- calculateDeltaHiMeLo(data.gmd.female, suffix.gmd)

data.tr <- read.csv("/home/adrose/dataPrepForHiLoPaper/data/meanLRVolandAgeReg/jlfTRData.csv")
suffix.tr <- "dti_jlf_tr_"

data.tr.male <- data.tr[which(data.tr$sex == 1), ]
output.tr.male <- calculateDeltaHiMeLo(data.tr.male, suffix.tr)

data.tr.female <- data.tr[which(data.tr$sex == 2), ]
output.tr.female <- calculateDeltaHiMeLo(data.tr.female, suffix.tr)

write.csv(output.vol.male, "output_hi-me-low_vol_male.csv")
write.csv(output.vol.female, "output_hi-me-low_vol_female.csv")
write.csv(output.gmd.male, "output_hi-me-low_gmd_male.csv")
write.csv(output.gmd.female, "output_hi-me-low_gmd_female.csv")
write.csv(output.cbf.male, "output_hi-me-low_cbf_male.csv")
write.csv(output.cbf.female, "output_hi-me-low_cbf_female.csv")
write.csv(output.tr.male, "output_hi-me-low_tr_male.csv")
write.csv(output.tr.female, "output_hi-me-low_tr_female.csv")

