library(ggplot2)
library(plyr)
library(dplyr)
library(readr)
library(circular)
library(purrr)
#library(tidyverse)

#set directory path and load in data
fullpath <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filepath <- paste(fullpath,"Data", "Kim", sep="/")
all_samples <- list.files(path=filepath, pattern = "*.csv", full.names=TRUE)
#Select your sample
spindles = read.csv(paste(filepath,"AurA_June7_A_3_SpindlePole.csv", sep="/"))
#spindles = filter(spindles, AreaShape_Center_X > 1824)
###Subset sample by position along the vertical axis###
# Split df by position along z axis
spindle_z = split(spindles, spindles$SpindlePole_FileName_Spindle)
 
#Set number of desired divisions
divisions = 6
#Divide dataset into (mostly) equally sized groups of z slices
total_z = length(spindle_z)
region_z = total_z / divisions
spindle_regions = split(spindle_z, rep(1:ceiling(total_z), each=region_z, length.out=(total_z)))
#spindles = spindles %>% dplyr::select(-trans)
#write.csv(spindles, paste(filepath,"AurA_June7_1-3_SpindlePole.csv", sep="/"))
#Specify colours to iterate through
  #colour_list = c("red", "orange", "yellow", "green", "cyan", "blue")
  #colour_list = hcl.colors(length(spindle_regions), palette = "Zissou 1")
  colour_list = c("#3B99B1", "#5ba683", "#b2d162", "#E9B31F", "#E78100", "#F5191C")
  #colour_list = c("#EF5350", "#f8961e","#FFCA28", "#9CCC65", "#64B5F6", "#9575CD")
  #colour_list = rainbow(6)
#Set up multipanel view
par(mfrow = c(2,3))
###Generate one rose plot per region###
for(i in 1:length(spindle_regions)){
  # select bin
  bin = 7 - i
  # Make orientation data circular in each subset
  region_spindles = Reduce(full_join, Reduce(full_join, spindle_regions[bin]))  
  spindleOrientationA = circular(region_spindles$SpindleAngle,type = "angles", units = "degrees",zero = pi/2)
  spindleOrientationB = circular(region_spindles$SpindleAngle + 180,type = "angles", units = "degrees",zero = pi/2)
  spindleOrientation = c(spindleOrientationA,spindleOrientationB)
  rose.diag(spindleOrientation, pch = 13, cex = 1, axes = TRUE, shrink = 1.2, bins = 24,
            col = colour_list[bin], border = "grey5", radii.scale = "linear", prop = 10, tol = 0.02, tcl.text = -0.2, add=FALSE)
  # test for significance
  print(i)
  print(kuiper.test(x=spindleOrientation))
}
 


#### ALTERNATE VERSION: Use this block if you are starting with unit vectors in 3D ####
spindles = bind_rows(all_angles)

###Subset sample by position along the vertical axis###

##NOTE: THIS IS CURRENTLY CONFIGURED FOR THE CORONAL PLANE.
#       To configure for other planes, use the following guide:
#       Coronal: t_split = t_z, theta = XY planes
#       Sagittal: t_split = t_x, theta = ZY planes
#       transverse: t_split = t_y, theta = XZ planes

# Define which transformed axis to split by: t_x, t_y, or t_z
spindles$t_split = spindles$t_y

# Reconstruct in-plane angle projections from the 3D unit vector
# For different planes, remember to set avec_rot_x, avec_rot_y, and avec_rot_z accordingly here.
spindles$theta =(atan2(spindles$avec_rot_z, spindles$avec_rot_x))

# In order to prevent visualization artifacts, we only include projected angles sufficiently close to the 3D angle in spindles_subset
spindles_subset <- spindles %>%
  rowwise() %>%
  mutate(
    # Normalize raw and rotated vectors. NOTE: the only difference between norm_proj and norm_3D is that norm_proj is lacking one term. THIS MUST MATCH THE DIMENSIONS IN THETA.
    norm_proj = sqrt(avec_rot_x^2 + avec_rot_y^2*0 + avec_rot_z^2),
    norm_3D = sqrt(avec_rot_x^2 + avec_rot_y^2 + avec_rot_z^2),
    
    # Compute dot product of normalized vectors
    dot = (avec_rot_x / norm_proj) * (avec_rot_x / norm_3D) +
      (avec_raw_y*0 / norm_proj) * (avec_rot_y / norm_3D) +
      (avec_raw_z / norm_proj) * (avec_rot_z / norm_3D),
    
    # Clamp to [-1, 1] to avoid NaNs
    dot = max(min(dot, 1), -1),
    
    # Compute angle
    angle = acos(dot)
  ) %>%
  ungroup() %>%
  filter(angle < pi/4 | angle > 3*pi/4)
#Set number of desired divisions
divisions = 6

# Find the minima and maxima of the split coordinate, and identify breakpoints for 6 sections
breaks = seq(min(spindles_subset$t_split, na.rm = TRUE), max(spindles_subset$t_split, na.rm = TRUE), length.out = divisions + 1)
# Cut the data into bins
spindles_subset$z_bin = cut(spindles_subset$t_split, breaks = breaks, include.lowest = TRUE)
# Split df by position along z axis
spindle_regions = split(spindles_subset, spindles$z_bin)


#Specify colours to iterate through
colour_list = c("#3B99B1", "#5ba683", "#b2d162", "#E9B31F", "#E78100", "#F5191C")

#Set up multipanel view
par(mfrow = c(2,3))
###Generate one rose plot per region###
for(i in 1:length(spindle_regions)){
  # select bin
  bin = divisions + 1 - i
  # Make orientation data circular in each subset
  spindleOrientationA = circular(spindle_regions[[i]]$theta,type = "angles", units = "radians", zero = pi/2)
  spindleOrientationB = circular(spindle_regions[[i]]$theta + pi,type = "angles", units = "radians", zero = pi/2)
  spindleOrientation = c(spindleOrientationA,spindleOrientationB)
  rose.diag(spindleOrientation, pch = 13, cex = 1, axes = TRUE, shrink = 2, bins = 24,
            col = colour_list[bin], border = "grey5", radii.scale = "linear", prop = 10, tol = 0.02, tcl.text = -0.2, add=FALSE)
  # test for significance
  print(i)
  print(kuiper.test(x=spindleOrientation))
}


## Temp code to plot out the difference (error) between angles
#Set up multipanel view
par(mfrow = c(2,3))
###Generate one rose plot per region###
for(i in 1:length(spindle_regions)){
  # select bin
  bin = divisions + 1 - i 
  # Make orientation data circular in each subset
  angleError = circular(spindle_regions[[i]]$angle,type = "angles", units = "radians", zero = pi/2)
  rose.diag(angleError, pch = 13, cex = 1, axes = TRUE, shrink = 1.2, bins = 24,
            col = colour_list[bin], border = "grey5", radii.scale = "linear", prop = 10, tol = 0.02, tcl.text = -0.2, add=FALSE)
}
