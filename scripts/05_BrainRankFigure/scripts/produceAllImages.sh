#!/bin/bash
# This is gong to be a quick and dirty script to produce all of the beta weight brain images for the hi lo project
baseDir="/home/arosen/hiLo/data/05_BrainRankFigure/"
rData="${baseDir}rOutput/"
modalityVals=(1 2 3 4) #ct reho alff tr)
genderValues=(1 2)
contrast=(2 3 5)
scriptName="/home/arosen/hiLo/scripts/05_BrainRankFigure/scripts/makeZScoreJLFPNCTemplateImage.sh"
foo() {
  baseDir="/home/arosen/hiLo/data/05_BrainRankFigure/"
  rData="${baseDir}rOutput/"
  m=$1
  g=$2
  c=$3
  scriptName="/home/arosen/hiLo/scripts/05_BrainRankFigure/scripts/makeZScoreJLFPNCTemplateImage.sh"
  inputCSV="${rData}${g}${m}${c}-KEY.csv"
  mkdir -p ${baseDir}/imagingFigures/${c}/${g}/${m}/ 
  cd ${baseDir}/imagingFigures/${c}/${g}/${m}/ 
  ${scriptName} ${inputCSV} 4 1 
  fslmaths outputImage.nii.gz -edge -bin tmp.nii.gz
  fslmaths outputImage.nii.gz -mul tmp.nii.gz edge.nii.gz
  fslmaths outputImage.nii.gz -sub edge.nii.gz outputImage.nii.gz
  fslmaths tmp.nii.gz -mul 5000 tmp.nii.gz
  fslmaths outputImage.nii.gz -add tmp.nii.gz outputImage.nii.gz 
  rm tmp.nii.gz edge.nii.gz
  echo "All done"
}
for s in ${contrast[*]} ; do
  for r in ${genderValues[*]} ; do 
    for q in ${modalityVals[*]} ; do 
      foo $q $r $s &  
    done
  done
done
exit 66

for s in ${contrast[*]} ; do
  for r in ${genderValues[*]} ; do 
    for q in ${modalityVals[*]} ; do 
      cd /home/arosen/hiLo/data/05_BrainRankFigure/imagingFigures/$s/$r/$q/
      itksnap -g /share/apps/fsl/5.0.8/data/standard/MNI152_T1_1mm_brain.nii.gz -s /home/arosen/hiLo/data/05_BrainRankFigure/imagingFigures/$s/$r/$q/outputImage.nii.gz -l /home/arosen/hiLo/data/05_BrainRankFigure/rOutput/$r$q$s-ColorTable.txt -z 1.6
    done
  done
done


# Now do the beta weight images down here
baseDir="/home/arosen/hiLo/data/05_BrainRankFigure/"
rData="/home/arosen/hiLo/data/05_BrainRankFigure/betaWeight/"
modalityVals=(vol.data cbf.data gmd.data tr.data alff.data reho.data)
genderValues=(Male Female)
scriptName="/home/arosen/hiLo/scripts/05_BrainRankFigure/scripts/makeZScoreJLFPNCTemplateImage.sh"
for g in ${genderValues[*]} ; do 
  for m in ${modalityVals[*]} ; do
    inputCSV="${rData}${m}.${g}-KEY.csv"
    mkdir -p ${baseDir}/imagingFiguresBeta/${g}/${m}/
    cd ${baseDir}/imagingFiguresBeta/${g}/${m}/
    ${scriptName} ${inputCSV} 3 0 &
  done
done
for g in ${genderValues[*]} ; do 
  for m in ${modalityVals[*]} ; do
    inputCSV="${rData}${m}.${g}-KEY.csv"
    mkdir -p ${baseDir}/imagingFiguresBetaITK/${g}/${m}/
    cd ${baseDir}/imagingFiguresBetaITK/${g}/${m}/
    ${scriptName} ${inputCSV} 4 1 &
  done
done 
exit 67

# Now finally the hi - lo images -_-
foo() {
  baseDir="/home/arosen/hiLo/data/05_BrainRankFigure/hiMinusLoFigure/"
  rData="/home/arosen/hiLo/data/05_BrainRankFigure/hiMinusLoFigure/input/"
  m=$1
  g=$2
  scriptName="/home/arosen/pncMJPS/scripts/09_brainImages/scripts/makeZScoreJLFPNCTemplateImage.sh"
  inputCSV="${rData}${g}${m}-KEY.csv"
  mkdir -p ${baseDir}${g}/${m}/
  cd ${baseDir}${g}/${m}/
  ${scriptName} ${inputCSV} 4 1 
  fslmaths outputImage.nii.gz -edge -bin tmp.nii.gz
  fslmaths outputImage.nii.gz -mul tmp.nii.gz edge.nii.gz
  fslmaths outputImage.nii.gz -sub edge.nii.gz outputImage.nii.gz
  fslmaths tmp.nii.gz -mul 5000 tmp.nii.gz
  fslmaths outputImage.nii.gz -add tmp.nii.gz outputImage.nii.gz 
  rm tmp.nii.gz edge.nii.gz
  echo "All done"
}

loopVal1=(vol cbf gmd tr)
loopVal2=(M F)
for x in ${loopVal1[*]} ; do
  for q in ${loopVal2[*]} ; do 
    foo $x $q &
  done
done
