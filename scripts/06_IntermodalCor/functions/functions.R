# find lobe function
# Should be used to place ROI into proper lobe
# Lobes will indlcude frontal, occipital, temporal, and occipital
# Also going to throw deep gm into the mix 
# Declare a function which will find the lobe value
findLobe <- function(grepPattern){
  # Declare the rois that we will grep through
  rois<-c("Thal","Putamen","Caudate","Pallidum",  # Basal Ganglia
          "Accumbens", "BasFor", # Basal Ganglia
          "PHG","Hip","PIns","SCA","AIns", # Limbic
          "ACgG","PCgG","Ent","Amygdala","MCgG", # Limbic
          "FO","MFC","MOrG","POrG","OrIFG","TrIFG","AOrG","OpIFG","GRe", # Frontal Orbital
          "FRP", "LOrG", # Frontal Orbital
          "PrG","MSFG","SMC","MFG","SFG", # Frontal Dorsal
          "FuG","PT","PP","ITG","CO","MTG","TMP","STG","TTG", # Temporal
          "PCu","PoG","AnG","PO","SPL","MPrG", # Parietal
          "SMG","MPoG", # Parietal
          "IOG","Cun","LiG","OFuG","MOG","Calc","OCP","SOG", # Occiptal
          "Cerebellum_Exterior", "Cerebellar_Vermal_Lobules_I.V", "Cerebellar_Vermal_Lobules_VI.VII", "Cerebellar_Vermal_Lobules_VIII.X") # Cerebellum


  # Declare the index key which corresponds to lobe values
  index.key <- c(1,7,17,28,33,42,50,58,62)

  # Now find where the pattern matches to the roi list
  for(pattern.match.variable in 1:length(rois)){
    nuclei.to.grep <- rois[pattern.match.variable]
    grep.output <- grep(nuclei.to.grep ,grepPattern)    
    if(!identical(integer(0), grep.output)){
      break
    }
    if(pattern.match.variable==61){
      pattern.match.variable <- 62
    }
  }
  lobe.group <- findInterval(pattern.match.variable, index.key)
  return(lobe.group)
}

## Now create a function which will output a row of obe indices for an input data frame
outputLobeRow <- function(dataFrame){
  # Prime an output row
  na.row <- rep('NA', ncol(dataFrame))
  # Now create a value with all of the data frame names
  name.values <- names(dataFrame)
  # Now cycle thorugh the names and 
  for(col.index in 1:ncol(dataFrame)){
    val.to.grep <- name.values[col.index]
    lobeValue <- findLobe(val.to.grep)
    na.row[col.index] <- lobeValue
  }
  # Now return the row
  return(na.row)
}

returnSigRMatrix <- function(dataFrame1, dataFrame2){
  rawCorMatrix <- cor(dataFrame1, dataFrame2, use="complete")
  sigValsMatrix <- corr.test(dataFrame1, dataFrame2)$p
  sigIndex <- apply(sigValsMatrix, 2, function(x){ifelse(x<0.05,1,NA)})
  multOutput <- sigIndex * rawCorMatrix
  return(multOutput)
}

plotHeatMap <- function(dataFrame1, dataFrame2, xlab, ylab, lowVal, highVal){
  # First prep the data
  tmp <- melt(cor(dataFrame1, dataFrame2, use="complete"))
  levels(tmp$Var1) <- rev(colnames(dataFrame1))
  levels(tmp$Var2) <- rev(colnames(dataFrame2))

  # Now find the minimum value
  #lowVal <- signif(min(tmp$value),digits=2) - .01
  #highVal <- signif(max(tmp$value),digits=2) + .01
  
  # Now create the image
  qplot(x=Var1, y=Var2, data=tmp, fill=value, geom="tile") + 
    #annotate("rect", ymin=.5, ymax=4.5, xmin= 0.5, xmax=4.5, fill=NA, color="black") + 
    #annotate("rect", ymin=4.5, ymax=12.5, xmin= 4.5, xmax=12.5, fill=NA, color="black") + 
    #annotate("rect", ymin=12.5, ymax=20.5, xmin= 12.5, xmax=20.5, fill=NA, color="black") + 
    #annotate("rect", ymin=20.5, ymax=34.5, xmin= 20.5, xmax=34.5, fill=NA, color="black") + 
    #annotate("rect", ymin=34.5, ymax=43.5, xmin= 34.5, xmax=43.5, fill=NA, color="black") + 
    #annotate("rect", ymin=43.5, ymax=61.5, xmin= 43.5, xmax=61.5, fill=NA, color="black") + 
    theme(text=element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1, face="bold"),
          axis.text.y = element_text(face="bold")) +
    xlab(xlab) +
    ylab(ylab) + 
    scale_fill_gradient2(low="blue", high="red", limits=c(lowVal, highVal))
}

plotSigHeatMap <- function(dataFrame1, dataFrame2, xlab, ylab, lowVal, highVal){
  # First prep the data
  tmp <- melt(returnSigRMatrix(dataFrame1, dataFrame2))
  levels(tmp$Var1) <- rev(colnames(dataFrame1))
  levels(tmp$Var2) <- rev(colnames(dataFrame2))

  # Now find the minimum value and max
  #lowVal <- signif(min(tmp$value, na.rm=T),digits=2) - .01
  #highVal <- signif(max(tmp$value, na.rm=T),digits=2) + .01

  # Now create the image
  qplot(x=Var1, y=Var2, data=tmp, fill=value, geom="tile") + 
    annotate("rect", ymin=.5, ymax=4.5, xmin= 0.5, xmax=4.5, fill=NA, color="black") + 
    annotate("rect", ymin=4.5, ymax=12.5, xmin= 4.5, xmax=12.5, fill=NA, color="black") + 
    annotate("rect", ymin=12.5, ymax=20.5, xmin= 12.5, xmax=20.5, fill=NA, color="black") + 
    annotate("rect", ymin=20.5, ymax=34.5, xmin= 20.5, xmax=34.5, fill=NA, color="black") + 
    annotate("rect", ymin=34.5, ymax=43.5, xmin= 34.5, xmax=43.5, fill=NA, color="black") + 
    annotate("rect", ymin=43.5, ymax=61.5, xmin= 43.5, xmax=61.5, fill=NA, color="black") + 
    theme(text=element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1, face="bold"),
          axis.text.y = element_text(face="bold")) +
    xlab(xlab) +
    ylab(ylab) + 
    scale_fill_gradient2(low="blue", high="red", limits=c(lowVal, highVal))    

}

# Create a function which will rm outliers given an input sd threshold
rmOutliers <- function(inputArray, SDcount){
  # First find the mean 
  inputMean <- mean(inputArray)
  # Now find the SD
  inputSD <- sd(inputArray)
  # Now find which values are below mean - (SDcount*SD)
  rmIndex <- which(inputArray < inputMean - (SDcount * inputSD)) 
  # Now find which are above
  rmIndex <- append(rmIndex, which(inputArray > inputMean - (SDcount * inputSD)))
  # Now return the input with the values outlier values masked 
  output <- inputArray
  output[rmIndex] <- 'NA'
  return(output)
}
