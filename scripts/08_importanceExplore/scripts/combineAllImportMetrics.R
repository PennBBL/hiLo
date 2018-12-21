## This script will be used to compare all of the impotance metrics for the hi lo project

## Load library(s)
install_load('ggplot2')
## The first thing we have to do is produce all of the importance metrics
## Little bash fandangling
system("for i in `ls prep*R` ; do Rscript ${i} ; done")

## Now we will read all of the male values and produce the correlation matrix
randImp <- read.csv('./randForImpMale.csv')
relifImp <- read.csv('./reliefFImpMale.csv')
ridgeImp <- read.csv('./ridgeImpMale.csv')
sregImp <- read.csv('selfRegImpMale.csv')
efImp <- read.csv('./effSizeImp.csv')

## Now isolate the male eff size variables
efImp <- efImp[which(efImp$sex=='M'),]
