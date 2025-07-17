# This is for comparison of E10-E10.5 shape change with preceding and anteceding patterns of orientation

# Libraries
library(Morpho)
library(ggplot2)
library(geomorph)
library(tiff)
library(EBImage)
library(readr)
library(shapes)
library(circular)
library(bpnreg)
library(Directional)
library(rgl)

library(Rvcg)
library(magick)
library(Evomorph)
library(vegan)
library(ggbiplot)
library(factoextra)
library(gt)
library(abind)
library(stringr)
library(dplyr)
library(DescTools)
library(cowplot)

# A few helper functions
deg_to_rad <- function(deg) {
  return(deg * pi / 180)
}
plot_3d_LMs <- function(LMs, color) {
  # this will plot the landmarks in the specified color
  # need to already have a rgl window open
  rgl::plot3d(LMs[,,1], aspect = "iso", type = "s", size=.5, col = color, add = T)
  rgl::text3d(x = LMs[,1,1],
              y = LMs[,2,1],
              z = LMs[,3,1],
              texts = c(1:dim(LMs)[1]),
              cex = 1.5, offset = 0.5, pos = 1)
}
# Load in data
fullpath = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filepath = paste(fullpath,"Data", sep="/")
e10_tiff_path = paste(filepath,"Volumes", "e10", sep="/")
e105_tiff_path = paste(filepath,"Volumes", "e10_5", sep="/")
lm_path = paste(filepath,"Landmarks", sep="/")
angles_path = paste(filepath,"Angles", "e10", sep="/")

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
for (i in 1:dim(lm_e10_array)[3]) {
  lm_e10_array[, , i] = as.matrix(lms_e10_data[[i]][, 1:3])
}

for (i in 1:dim(lm_e105_array)[3]) {
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



#### PART 3 ####
##Transformation of angles into a common 3D space
# A note: Each rotation matrix is a composite of rotations in all three axes from each sample to the mean. The order matters. these were applied in ZXY order, which is standard.
# However, this means the angle vector can be constructed from the simple cos and sin for x and y, without needing to decompose it further. z is always 0 for in-plane annotations
# We will construct a unit vector for each angle, and transform that according to the rotation matrix.
# Note that this ONLY transforms the angle- not the coordinate. That needs to be done separately, right from the OPA (TODO)

### Load in all angle lists and loop through each to convert angles to vectors and transform based on OPA rotations. Angles are agnostic to scale and translation,
### So we'll just convert the angles themselves here and reposition them after.
angle_files = list.files(angles_path, pattern = "\\.csv$", full.names = TRUE)
all_angles = lapply(angle_files, read.csv)
# Initialize empty matrix of the correct dimensions.


for (i in 1:length(all_angles)){
  # Load one sample at a time and convert angle data back to circular numeric, in radians.
  all_angles[[i]]$SpindleAngle = as.circular(all_angles[[i]]$SpindleAngle, units = "degrees", type = "angles")
  angles_degrees = circular(all_angles[[i]]$SpindleAngle, units = "degrees")
  angle_radians = conversion.circular(angles_degrees, units = "radians")
  # Construct unit vector from each angle by adding a few columns on. First is the angle in radians, followed by the unrotated components of each unit vector. This is kinda clean, actually.
  all_angles[[i]] = all_angles[[i]] %>%
    rowwise() %>%
    mutate(
      angle_radians = conversion.circular(angles_degrees, units = "radians"),
      avec_raw_x = cos(angle_radians),
      avec_raw_y = sin(angle_radians),
      avec_raw_z = 0
    )
}

## May need later: directly create vector from angle
#angle_vector = vector(length = 3)
#angle_vector[1] = cos(this_angle)
#angle_vector[2] = sin(this_angle)
#angle_vector[3] = 0
# Test code to verify unit vector construction
test_angle = -0.551
test_vector = vector(length = 3)
test_vector[1] = cos(test_angle)
test_vector[2] = sin(test_angle)
test_vector[3] = 0


transformed_test_vector = test_vector %*% OPA_Rotations[,,1]
angle_coords = #Coordinate list of angles, as an array with the same dimensions (ncells x 3 x nsamples)

## Transformation of coordinates into the same space
for (i in 1:dim(lm_e10_array)[3]) {
  # 1. Get original landmarks
  lm_raw <- lm_e10_array[, , i]
  
  # 2. Get the GPA-transformed landmarks
  lm_aligned <- gpa_e10$rotated[, , i]
  
  # 3. Compute translation: centering
  centroid <- colMeans(lm_raw)
  lm_centered <- sweep(lm_raw, 2, centroid, "-")
  
  # 4. Compute centroid size
  cs <- sqrt(sum(lm_centered^2))
  lm_scaled <- lm_centered / cs
  
  # 5. Compute rotation matrix (can maybe change to use OPA method)
  # Use SVD to align scaled -> aligned
  svd_result <- svd(t(lm_scaled) %*% lm_aligned)
  rot_matrix <- svd_result$v %*% t(svd_result$u)
  
  # 6. Apply the same transformation to auxiliary points- in this case, coordinates for each angle in the same order
  aux_raw <- angle_coords[[i]]  # m x 3 matrix
  
  # Step-by-step transform:
  aux_centered <- sweep(aux_raw, 2, centroid, "-")
  aux_scaled <- aux_centered / cs
  aux_transformed <- aux_scaled %*% rot_matrix
  
  # Store or use aux_transformed
  transformed_aux[[i]] <- aux_transformed
}
# These are the x, y, and z components of each unit vector. This is now a 3D angle, but it can be projected into each axis with these values. This negates the need to have images in a common orientation.

#A test for tomorrow- take a few sample angles, rotate them along with a landmark set, and plot it all.
#Then, wrap the vector construction in a function and apply it to all our angles. Append these to our main DF.
open3d(zoom = 0.75, windowRect = c(0, 0, 700, 700)) 
# plot the decimated head mesh
rgl::shade3d(head_mesh_spec1_dec, color = "gray", alpha =0.9)
# plot the landmarks in blue
plot_3d_LMs(lm_e105_array, 'darkblue')




# TODO: Integrate positional orientation. Need clarification on angle inputs and ROI inputs (probably schedule a short call with Nick [I did this and I'm still confused lol])

# Testing

test_OPA = procOPA(mean_e10, lm_e10_array[, , 1])
test_rotations = test_OPA$R
dim(test_rotations)

this_sample = proc_coords[,,1]
this_sample
dim(this_sample)
