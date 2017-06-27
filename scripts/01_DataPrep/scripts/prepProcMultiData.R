# AFGR June 2017
# This script will be used to prepare the input CSV files for the proc multi sas scripts
# These scripts will be looing at 4 way interactions and 3 way interactions. The interactions include:
# age_bin*sex*roi*perf_bin
# sex*perf_bin*age_bin
# The input to this script will be the averaged L and R ROI values for all modalities

# load library(s)
source('/home/adrose/adroseHelperScripts/R/afgrHelpFunc.R')
source('/home/adrose/hiLo/scripts/01_DataPrep/functions/functions.R')
install_load('reshape2')

## Load data here
vol.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/volumeData.csv')
cbf.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/cbfData.csv')
gmd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/gmdData.csv')
ct.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/ctData.csv')
reho.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/rehoData.csv')
alff.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/alffData.csv')
ad.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfADData.csv')
fa.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfFAData.csv')
rd.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfRDData.csv')
tr.data <- read.csv('/home/adrose/dataPrepForHiLoPaper/data/meanLR/jlfTRData.csv')

## Now attach age bin to all of our data 
vol.data <- addAgeBin(vol.data, vol.data$ageAtGo1Scan, 167, 215, 216)
cbf.data <- addAgeBin(cbf.data, cbf.data$ageAtGo1Scan, 167, 215, 216)
gmd.data <- addAgeBin(gmd.data, gmd.data$ageAtGo1Scan, 167, 215, 216)
ct.data <- addAgeBin(ct.data, ct.data$ageAtGo1Scan, 167, 215, 216)
reho.data <- addAgeBin(reho.data, reho.data$ageAtGo1Scan, 167, 215, 216)
alff.data <- addAgeBin(alff.data, alff.data$ageAtGo1Scan, 167, 215, 216)
ad.data <- addAgeBin(ad.data, ad.data$ageAtGo1Scan, 167, 215, 216)
fa.data <- addAgeBin(fa.data, fa.data$ageAtGo1Scan, 167, 215, 216)
rd.data <- addAgeBin(rd.data, rd.data$ageAtGo1Scan, 167, 215, 216)
tr.data <- addAgeBin(tr.data, tr.data$ageAtGo1Scan, 167, 215, 216)
