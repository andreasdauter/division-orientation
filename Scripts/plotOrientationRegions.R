library(ggplot2)
library(plyr)
library(readr)
library(circular)

#set directory path and load in data
fullpath <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filepath <- paste(fullpath,"Data", sep="/")
all_samples <- list.files(path=filepath, pattern = "*.csv", full.names=TRUE)
#Select your sample
spindles = read.csv(all_samples[1])

###Subset sample by position along the vertical axis###
#divisions = 5
#total_rows = nrow(spindles)/divisions
#region_rows = total_rows / divisions
#spindle_regions = split(spindles, rep(1:ceiling(total_rows), each=region_rows, length.out=(total_rows)))




# Make orientation data circular in each subset
spindleOrientationA = circular(spindles$AreaShape_Orientation,type = "angles", units = "degrees",zero = pi/2)
spindleOrientationB = circular(spindles$AreaShape_Orientation + 180,type = "angles", units = "degrees",zero = pi/2)
spindleOrientation = c(spindleOrientationA,spindleOrientationB)

rose.diag(spindleOrientation, pch = 16, cex = 1, axes = TRUE, shrink = 1, bins = 24,
          col = "cadetblue3", border = "cadetblue", radii.scale = "linear", prop = 14, tol = 0.02, tcl.text = 0.075, add = TRUE)

# test for significance
kuiper.test(x=spindleOrientation)
          