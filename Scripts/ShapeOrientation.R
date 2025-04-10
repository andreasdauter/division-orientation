# This is for comparison of E10-E10.5 shape change with preceding and anteceding patterns of orientation

# Libraries
library(Morpho)
library(ggplot)
library(geomorph)
library(tiff)
library(EBImage)
library(readr)
library(shapes)
library(shapes)

# Load in data
fullpath = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filepath = paste(fullpath,"Data", sep="/")
tiff_path = paste(filepath,"Volumes", sep="/")
lm_path = paste(filepath,"Landmarks", sep="/")

# List all TIFF files
tiff_files = list.files(tiff_path, pattern = "\\.tiff", full.names = TRUE)

# Load TIFF volumes into a list
volumes_list = lapply(tiff_files, function(file) {
  readImage(file)  # Reads TIFF as a 3D array (x, y, z)
})
## This will be used for visualization later

# Load Landmarks
# List all landmark files
landmark_files = list.files(lm_path, pattern = "\\.csv", full.names = TRUE)
sample_names = list.files(lm_path, pattern = "\\.csv", full.names = FALSE)
landmark_data = lapply(landmark_files, read_csv)


# Convert CSVs to LM arrays
landmark_array = array(NA, dim = c(nrow(landmark_data[[1]]), 3, length(landmark_data)))

# Fill the array
for (i in 1:length(landmark_data)) {
  landmark_array[, , i] = as.matrix(landmark_data[[i]][, 1:3])  # Assuming columns 1 & 2 are x and y
}

# Define vector of midline LMs
midline_LMs = c(1,2,3,4,5)
# Define paired LMs
paired_LMs = matrix(c(
  6, 23,
  7, 24,
  8, 25,
  9, 26,
  10, 27,
  11, 28,
  12, 29,
  13, 30,
  14, 31,
  15, 32,
  16, 33,
  17, 34,
  18, 35,
  19, 36,
  20, 37,
  21, 38,
  22, 29
), ncol = 2, byrow = TRUE)
# Run GPA on all samples, paired, with ProcSym
gpa_sym = procSym(landmark_array, paired = paired_LMs)



# Retrieve coordinates and subset into two groups by age
proc_coords = gpa_sym$rotated
# Subsetting legend
# Create shape avg for two groups separately from mu

# Ordinary Procrustes Analysis with procOPA on each sample to the mean. procOPA$R stores the rotations matrix




# Testing


