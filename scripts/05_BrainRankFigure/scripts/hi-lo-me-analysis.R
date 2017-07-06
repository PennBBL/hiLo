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
    output[j,2] <- meanvals[2] - meanvals[1]
    output[j,3] <- meanvals[3] - meanvals[2]
    output[j,4] <- meanvals[3] - meanvals[1]
    
    j <- j + 1
  }
  
  names(output) <- c("roi","me-lo","hi-me","hi-lo")
  return(output)
}


data.cbf <- read.csv("~/hi-lo/data/cbfData.csv-imputed")
suffix.cbf <- "pcasl_jlf_cbf_"
output.cbf <- calculateDeltaHiMeLo(data.cbf, suffix.cbf)

data.vol <- read.csv("~/hi-lo/data/volumeData.csv")
suffix.vol <- "mprage_jlf_vol_"
output.vol <- calculateDeltaHiMeLo(data.vol, suffix.vol)

data.gmd <- read.csv("~/hi-lo/data/gmdData.csv")
suffix.gmd <- "mprage_jlf_gmd_"
output.gmd <- calculateDeltaHiMeLo(data.gmd, suffix.gmd)


write.csv(output.vol, "~/hi-lo/output_hi-me-low_vol.csv")
write.csv(output.gmd, "~/hi-lo/output_hi-me-low_gmd.csv")
write.csv(output.cbf, "~/hi-lo/output_hi-me-low_cbf.csv")