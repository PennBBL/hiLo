### This script creates a figure to replace Figure 2 in the manuscript
### using Nilearn's plot_roi function
###
### Ellyn Butler
### January 2, 2020

from nilearn import plotting
import numpy as np
import plotnine

# Load contrasts (Hi-Lo, Male/Female, 8 modalities = 16)
imgpath = "/Users/butellyn/Documents/hiLo/data/hilo_images/"
outputpath = "/Users/butellyn/Documents/hiLo/plots/"
pnc_template = nilearn.image.load_img(imgpath+)

mods = ["Volume", "GMD", "MD", "CBF", "ALFF", "ReHo", "NBack", "IdEmo"]

d = {}
for gender in ["Male", "Female"]:
    for mod in mods:
        filename = imgpath+gender+"_"+mod+".nii.gz"
        thisimg = nilearn.image.load_img(filename)
        d[gender+"_"+mod] = plotting.plot_roi(thisimg, bg_img=pnc_template,
            cmap=plotting.cm.bwr_r, title=gender+": "+mod)
        d[gender+"_"+mod].savefig(outputpath + gender + "_" + mod + ".png")
        d[gender+"_"+mod].close()



#fig, axes = plt.subplots(nrows=3, ncols=2, figsize=(7, 7))
