library(ggplot2)
library(plyr)
library(dplyr)
library(readr)
library(circular)

# THIS SCRIPT IS NOW OBSELETE AND HAS BEEN INTEGRATED INTO plotOrientationRegions
#set directory path and load in data
fullpath <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filepath <- paste(fullpath,"Data", "Kim", sep="/")


spindles = read.csv(paste(filepath,"AurA_June7_A_3_SpindlePole.csv", sep="/"))

#Optional:Subset to left side


# spindles_left = filter(spindles, AreaShape_Center_X > 1824)
# spindles = spindles_left
# Make orientation data circular                                        
spindleOrientationA = circular(spindles$AreaShape_Orientation,type = "angles", units = "degrees",zero = pi/2)
spindleOrientationB = circular(spindles$AreaShape_Orientation + 180,type = "angles", units = "degrees",zero = pi/2)
spindleOrientation = c(spindleOrientationA,spindleOrientationB)

rose.diag(spindleOrientation, pch = 16, cex = 1, axes = TRUE, shrink = 1, bins = 24, tcl=0,
          col = "cadetblue3", border = "cadetblue", radii.scale = "linear", prop = 14, tol = 0.02, tcl.text = 0.075)

# test for signifcance
### rayleigh.test(x=spindleOrientationA)###
kuiper.test(x=spindleOrientation)
           