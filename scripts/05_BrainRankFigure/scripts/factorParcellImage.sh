#!/bin/bash

# This script will be used to produce the FA segmentation from stathis' segmentation factor loadings 

# Declare any statics here
baseDataDir="/home/arosen/hiLo/data/05_BrainRankFigure/factorLabels/"
vals=(gmd ct reho) 
scriptVal="/data/joy/BBL/applications/xcpEngine/utils/val2mask.R"
parcMask="/data/joy/BBL/studies/pnc/template/parc1625_EDG_PNC.nii.gz"


# Now loop through each modality and produce the segmented image
for tmp in "${vals[@]}" ; do 
  pathVal="${baseDataDir}${tmp}*csv"
  initValue=1
  for tmp2 in `ls ${pathVal}` ; do 
    # First isolate all of the values of interest
    ${scriptVal} -i ${parcMask} -v `cat ${tmp2}` -o ${tmp}${initValue}.nii.gz 
    fslmaths ${baseDatDir}${tmp}${initValue}.nii.gz -mul ${initValue} ${tmp}${initValue}.nii.gz
    initValue=$((initValue+1))
    echo ${initValue}
  done
  # Now add all of the masks together
  # But first create a blank image
  fslmaths ${parcMask} -sub ${parcMask} blankImage.nii.gz
  baseCall="fslmaths blankImage.nii.gz"
  pathVal="${tmp}*nii.gz"
  for tmp2 in `ls ${pathVal}` ; do
    baseCall=`echo ${baseCall} -add ${tmp2}`
  done 
  baseCall=`echo ${baseCall} ${tmp}_combinedImage.nii.gz`
  ${baseCall}
done
