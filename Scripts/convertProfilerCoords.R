# Quick script for angle coordinate adjustment before calculating alignment
library(purrr)
library(dplyr)
library(stringr)
library(ggplot2)
library(circular)
library(bpnreg)
library(Directional)
library(Morpho)
library(car)
library(biotools)
library(ggridges)

# Helper functions
  # Function to calculate the Euclidean distance
  euclidean_distance = function(coord1, coord2) {
    sqrt(sum((coord1 - coord2)^2))
  }

  
  
# Load in data

  # Define paths
  fullpath = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
  filepath = paste(fullpath,"Data/Angles", "Mandibles", sep="/")
  # Change names of files to load that contain coordinates.
  point_files = list.files(path = filepath, pattern = "\\.csv$", full.names = TRUE)
  sample_list = lapply(point_files, read.csv)
  names(sample_list) = basename(point_files)

# Define ratios of Z thickness to XY pixel size (NOT in biological units)
zscales = c(16.7, 16.7, 16.7, 16.7)

# Extract Z position from filename and convert to real position
sample_list <- lapply(sample_list, function(df) {
  df %>%
    mutate(
      z_slice = as.integer(str_extract(FileName_Spindle, "(?<=_z)\\d{3,4}(?=c)"))
    )
})

# Map zscales to each sample and multiply through, storing the finished coordinate in Location_Center_Z
sample_list <- map2(sample_list, zscales, ~ .x %>% mutate(Location_Center_Z = z_slice * .y))

# Define and apply offsets by sample
sample_offsets = matrix(c(4684, 3644, #XY offsets for first sample
                          2744, 6048, #XY offsets for second sample... 
                          941, 4538, 
                          3809, 523), nrow = length(sample_list), ncol = 2, byrow = TRUE)

sample_list <- map2(sample_list_original, seq_along(sample_list_original), ~ {
  .x %>%
    mutate(
      Location_Center_X = Location_Center_X + sample_offsets[.y, 1],
      Location_Center_Y = Location_Center_Y + sample_offsets[.y, 2]
    )
})
# Save as new structure, eliminating unnecessary features.
sample_list <- lapply(sample_list, function(df) {
  df %>%
    dplyr::select(-c(z_slice, 
           Number_Object_Number, 
           ObjectNumber, 
           ImageNumber, 
           PathName_Spindle))
})

write.csv(write.csv(spindles, paste(filepath,"AurA_June7_1-3_SpindlePole.csv", sep="/")))