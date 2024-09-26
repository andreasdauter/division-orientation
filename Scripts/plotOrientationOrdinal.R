library(ggplot2)
library(plyr)
library(readr)
library(circular)

#set directory path and load in data
fullpath <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filepath <- paste(fullpath,"Data", sep="/")
all_samples <- list.files(path=filepath, pattern = "*.csv", full.names=TRUE)

spindles = read.csv(all_samples[3])



# Make orientation data circular
spindleOrientationA = circular(spindles$AreaShape_Orientation,type = "angles", units = "degrees",zero = pi/2)
spindleOrientationB = circular(spindles$AreaShape_Orientation + 180,type = "angles", units = "degrees",zero = pi/2)
spindleOrientation = c(spindleOrientationA,spindleOrientationB)

rose.diag(spindleOrientation, pch = 16, cex = 1, axes = TRUE, shrink = 1, bins = 24,
          col = "cadetblue3", border = "cadetblue3", radii.scale = "linear", prop = 14, tol = 0.02, tcl.text = 0.075)

# test for significance
### rayleigh.test(x=spindleOrientationA)###
kuiper.test(x=spindleOrientation)
           