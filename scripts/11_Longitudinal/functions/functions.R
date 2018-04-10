## Create a function which will return the mean and standard deviation from 
## age groups, whenn there are at least 20 to a group
## To do this it will rrequire an input data frame
## with a column with age in years
## The output from this function will be a vector with the age bin index returned, the lnegth of the input vector
returnAgeGroup <- function(ageValues, upperLimit=264){
  # First thing is to collapse any age values greater then 22 into the 22 age bin
  ageValues[ageValues>upperLimit] <- 264

  # First grab the n values
  nValues <- table(round(ageValues/12, digits=0))

  # Now find any that have less than 20
  smallCheck <- which(nValues<20)
  scL <- length(smallCheck)
  # If we have any with n less then 20 move that bin into it's closest neighbor  
  while(scL>0){
    # First take the smallest bin and inflate it + 1
    binValue <- as.numeric(names(nValues)[smallCheck[1]])
    # Now find the subjects with the lowest bin value and add 1 to them
    valsToAdd <- which(round(ageValues/12, digits=0)==binValue)
    vtaLC <- length(valsToAdd)
    # Now add one to each of these 
    while(vtaLC>0){
      ageValues[valsToAdd] <- ageValues[valsToAdd] + 1
      valsToAdd <- which(round(ageValues/12, digits=0)==binValue)
      vtaLC <- length(valsToAdd)
    }
  # Now we need to again check to see if we satisfy errything
  nValues <- table(round(ageValues/12, digits=0))
  smallCheck <- which(nValues<20)
  scL <- length(smallCheck)
  }
  # Now we need to return a vector with the bins that each person belongs to
  # In order to do this we need loop thorugh every age value
  # given in the nValues and return the age values that collapse within the 
  # range for the value
  loopValues <- as.numeric(names(nValues))
  outputVector <- rep(NA, length(ageValues))
  for(loopValue in loopValues){
    outputVector[which(round(ageValues/12, digits=)==loopValue)] <- loopValue
  } 
  # Now return this guy
  return(outputVector)
}

## Now I need to write a function which will output the mean and standard deviation 
## for each of my age bins
## The inputs to this function will be 
returnMeanSDValues <- function(valueCol, ageCol, na.rm=TRUE){
  # First initialize the output
  outputMatrix <- matrix(NA, length(unique(ageCol)), 3)
  # Now loop through each of the value Col and find the associated ageCol vals
  # then simply return the mean and sd for each of em
  rowValue <- 1
  for(ageValue in sort(unique(ageCol))){
    # First find the index for values 
    indexVals <- which(ageCol==ageValue)
    # Now find mean and sd
    meanValue <- mean(valueCol[indexVals], na.rm=na.rm)
    sdValue <- sd(valueCol[indexVals],na.rm=na.rm)
    # Now export em
    outputMatrix[rowValue,] <- c(ageValue,meanValue,sdValue)
    rowValue <- rowValue + 1
  }
  # Now return the output matrix
  return(outputMatrix)
}

## Now create a function which will z score all of the values within an age bin
## Across all timepoints
## The input to this function will be a 
##  1. A matrix with the age values, mean, and standar deviation values
##  2. The values to be z-scored
##  3. the age groups
applyMeanandSD <- function(ageMeanandSDMatrix, valueCol, ageCol){
  ## The first thing we need to do is prepare the output
  outputValues <- rep(NA, length(valueCol))
  globalMean <- mean(valueCol, na.rm=TRUE)
  ## Now we need to make sure that our age and value columns match up
  if(length(valueCol)!=length(ageCol)){
    stop("You done messed up a-aron")    
  }
  ## Now go through every value in the age index and z score them based on the 
  ## appropriate age index
  for(rowValue in seq(1, dim(ageMeanandSDMatrix)[1])){
    ## First thing we need to do is find the indices for our age ppl
    index <- which(ageCol==ageMeanandSDMatrix[rowValue,1])

    ## Now z score the values based
    scaledValues <- as.numeric(scale(valueCol[index], center=ageMeanandSDMatrix[rowValue,2], scale=ageMeanandSDMatrix[rowValue,3]))
    
    ## Now add the global mean back
    scaledValues <- scaledValues + globalMean   
 
    ## Now export these values to the output values vector
    outputValues[index] <- scaledValues
  }
  ## Now return the outputValues
  return(outputValues)
}

## Now I need to declare a function which will scale the data between 0-1 and then add one
range12 <- function(x){
  # Now make sure we have some standard deviation
  # If no standard deviation return 1
  if( is.na(sd(x, na.rm=T)) == 'TRUE'){
    output <- rep(1, length(x))
    return(output)
  }
  else if (sd(x, na.rm=T) < 0 ){
    output <- rep(1, length(x))
    return(output)
  }
  else
    (x-min(x, na.rm=T))/diff(range(x, na.rm=T)) + 1
}
