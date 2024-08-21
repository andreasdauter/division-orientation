library(ggplot2)
library(plyr)
library(dplyr)
library(readr)
library(circular)
library(purrr)
#library(tidyverse)

#set directory path and load in data
fullpath <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filepath <- paste(fullpath,"Data", sep="/")
all_samples <- list.files(path=filepath, pattern = "*.csv", full.names=TRUE)
#Select your sample
spindles = read.csv(all_samples[1])

###Subset sample by position along the vertical axis###

# Split df by position along z axis
spindle_z = split(spindles, spindles$FileName_Spindle)

#Set number of desired divisions
divisions = 6
#Divide dataset into (mostly) equally sized groups of z slices
total_z = length(spindle_z)
region_z = total_z / divisions
spindle_regions = split(spindle_z, rep(1:floor(total_z), each=region_z, length.out=(total_z)))

#Specify colours to iterate through
colour_list = c("red", "orange", "yellow", "green", "cyan", "blue")

###Generate one rose plot per region###
for(i in 1:length(spindle_regions)){
  # Make orientation data circular in each subset
  region_spindles = Reduce(full_join, Reduce(full_join, spindle_regions[i]))  
  spindleOrientationA = circular(region_spindles$AreaShape_Orientation,type = "angles", units = "degrees",zero = pi/2)
  spindleOrientationB = circular(region_spindles$AreaShape_Orientation + 180,type = "angles", units = "degrees",zero = pi/2)
  spindleOrientation = c(spindleOrientationA,spindleOrientationB)
  if (i==1) {
  rose.diag(spindleOrientation, pch = 16, cex = 1, axes = TRUE, shrink = 1, bins = 24,
            col = colour_list[i], border = "darkgray", radii.scale = "linear", prop = 14, tol = 0.02, tcl.text = 0.075, add=FALSE, alpha = 0.5)
  } else {
  rose.diag(spindleOrientation, pch = 16, cex = 1, axes = TRUE, shrink = 1, bins = 24,
            col = colour_list[i], border = "darkgray", radii.scale = "linear", prop = 14, tol = 0.02, tcl.text = 0.075, add=TRUE, alpha = 0.5)    
  }
  # test for significance
  print(i)
  kuiper.test(x=spindleOrientation)
}
test = Reduce(full_join, Reduce(full_join, spindle_regions[1]))         

