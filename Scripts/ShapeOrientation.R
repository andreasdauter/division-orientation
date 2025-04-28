# This is for comparison of E10-E10.5 shape change with preceding and anteceding patterns of orientation

# Libraries
library(Morpho)
library(ggplot2)
library(geomorph)
library(tiff)
library(EBImage)
library(readr)
library(shapes)

# Load in data
fullpath = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filepath = paste(fullpath,"Data", sep="/")
e10_tiff_path = paste(filepath,"Volumes", "e10", sep="/")
e105_tiff_path = paste(filepath,"Volumes", "e10_5", sep="/")
lm_path = paste(filepath,"Landmarks", sep="/")

# List all TIFF files for both ages
e10_tiff_files = list.files(e10_tiff_path, pattern = "\\.tiff", full.names = TRUE)
e105_tiff_files = list.files(e105_tiff_path, pattern = "\\.tiff", full.names = TRUE)
# Load TIFF volumes into a list
e10_volumes_list = lapply(e10_tiff_files, function(file) {
  readImage(file)  # Reads TIFF as a 3D array (x, y, z)
})
e105_volumes_list = lapply(e105_tiff_files, function(file) {
  readImage(file)  # Reads TIFF as a 3D array (x, y, z)
})
## This will be used for visualization later

# Load Landmarks in one dataframe
sample_names = list.files(lm_path, pattern = "\\.csv", full.names = FALSE, recursive = TRUE)

lm_df = data.frame(
  file_name = basename(sample_names),
  age = dirname(sample_names),
  stringsAsFactors = FALSE
)
# Subset by age and store coordinates
lms_e10 = subset(lm_df, age == "e10")
lms_e10$file_name = paste0(lm_path, "/e10/", lms_e10$file_name)
lms_e10_data = lapply(lms_e10$file_name, read_csv)

lms_e105 = subset(lm_df, age == "e10_5")
lms_e105$file_name = paste0(lm_path, "/e10_5/", lms_e105$file_name)
lms_e105_data = lapply(lms_e105$file_name, read_csv)

# Convert CSVs to LM arrays
lm_e10_array = array(NA, dim = c(nrow(lms_e10_data[[1]]), 3, length(lms_e10_data)))
lm_e105_array = array(NA, dim = c(nrow(lms_e105_data[[1]]), 3, length(lms_e105_data)))

# Fill the array
for (i in 1:length(lm_e10_array)) {
  lm_e10_array[, , i] = as.matrix(lms_e10_data[[i]][, 1:3])
}

for (i in 1:length(lm_e105_array)) {
  lm_e105_array[, , i] = as.matrix(lms_e105_data[[i]][, 1:3])
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
gpa_e10 = procSym(lm_e10_array, paired = paired_LMs)
gpa_e105 = procSym(lm_e105_array, paired = paired_LMs)


# Retrieve coordinates and subset into two groups by age
proc_coords = gpa_e10$rotated

# Create shape avg for two groups separately
mean_e10 = gpa_e10$mshape

# Ordinary Procrustes Analysis with procOPA on each sample to the mean. procOPA$R stores the rotations matrix

# Set up array to hold rotation matrices
nsamples = dim(lm_e10_array)[3]
OPA_Rotations = array(NA, dim = c(3, 3, nsamples))

# Perform an Ordinary procrustes analysis between the mean sample (mean_e10) and each individual sample (page of the array)
# and store the 3x3 rotation matrix from the sample to the mean in the corresponding page of OPA_Rotations 
for (i in 1:nsamples) {
  OPA_result = procOPA(mean_e10, lm_e10_array[, , i])
  OPA_Rotations[,,i] = OPA_result$R
}

#TODO: 


# Testing

test_OPA = procOPA(mean_e10, lm_e10_array[, , 1])
test_rotations = test_OPA$R
dim(test_rotations)
