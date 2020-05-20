### This script takes in annot files for the Miccai/Oasis label set and turns
### them into a 3d atlas in ggseg, which is then converted to a 2d atlas
###
### Ellyn Butler
### May 19, 2020

# Load libraries
library('ggseg')
library('ggsegExtra')

dt <- make_aparc_2_3datlas(annot="mic", subject="fsaverage") # Steps 1-2 completed
