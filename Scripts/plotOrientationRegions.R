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
spindles = read.csv(all_samples[3])

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
  #colour_list = c("red", "orange", "yellow", "green", "cyan", "blue")
  colour_list = hcl.colors(length(spindle_regions), palette = "Zissou 1")
  #colour_list = c("#f94144", "#f3722c", "#f8961e", "#90be6d", "#43aa8b", "#277da1")
  #colour_list = rainbow(6)
#Set up multipanel view
par(mfrow = c(2,3))
###Generate one rose plot per region###
for(i in 1:length(spindle_regions)){
  # Make orientation data circular in each subset
  region_spindles = Reduce(full_join, Reduce(full_join, spindle_regions[i]))  
  spindleOrientationA = circular(region_spindles$AreaShape_Orientation,type = "angles", units = "degrees",zero = pi/2)
  spindleOrientationB = circular(region_spindles$AreaShape_Orientation + 180,type = "angles", units = "degrees",zero = pi/2)
  spindleOrientation = c(spindleOrientationA,spindleOrientationB)
  rose.diag(spindleOrientation, pch = 13, cex = 1, axes = TRUE, shrink = 1, bins = 24,
            col = colour_list[i], border = "grey5", radii.scale = "linear", prop = 12, tol = 0.02, tcl.text = -0.1, add=FALSE)
  # test for significance
  print(i)
  print(kuiper.test(x=spindleOrientation))
}
