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

# helper: read a single landmark .csv. Tried to make this resistant to formatting changes
read_csv_landmarks <- function(file){
  df <- read.csv(file, header = TRUE)
  # try common column names
  if(all(c("x","y","z") %in% names(df))){
    lma =(as.matrix(df[,c("x","y","z")]))
  } else if(all(c("X","Y","Z") %in% names(df))){
    lma =(as.matrix(df[,c("x","y","z")]))
  } else if(all(c("r","a","s") %in% names(df))){
    lma = (as.matrix(df[,c("r","a","s")]))
  } else {
    # if no column names are found, take first three numeric columns
    nums <- sapply(df, is.numeric)
    lma = (as.matrix(df[, which(nums)[1:3] ]))
  }
  colnames(lma) = c("x","y","z")
  return(lma)
}

# read landmarks and meshes; assume matching names (without extension)
read_all_landmarks <- function(files){
  L <- list()
  for(f in files){
    pts <- read_csv_landmarks(f)
    L[[tools::file_path_sans_ext(basename(f))]] <- pts
  }
  return(L)
}

# Build landmark array
build_landmark_array <- function(Llist){
  n <- length(Llist)
  p <- nrow(Llist[[1]])
  arr <- array(NA, dim = c(p,3,n))
  i <- 1
  for(name in names(Llist)){
    arr[,,i] <- as.matrix(Llist[[name]])
    i <- i + 1
  }
  return(arr)
}
# This is basically a manual point transformation to a whole list in a DF, given a centroid, centroid size, and 3x3 rotation matrix
transform_point <- function(x, y, z, centroid, cs, R) {
  vec <- c(x, y, z)
  vec_centered <- (vec - centroid) / cs
  as.list(vec_centered %*% R)
}
# Load in data
fullpath = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filepath = paste(fullpath,"Data", "Kim", sep="/")
e10_tiff_path = paste(filepath,"Volumes", "e10", sep="/")
e105_tiff_path = paste(filepath,"Volumes", "e10_5", sep="/")
lm_path = paste(filepath,"Landmarks", sep="/")
angles_path = paste(filepath,"Mandibles", "e10", sep="/")

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
lms_e10 = subset(lm_df, age == "E10")
lms_e10$file_name = paste0(lm_path, "/E10/", lms_e10$file_name)
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

# If running on mandible alone:
gpa_e10 = procSym(lm_e10_array)

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

#Testing: Realized that the annotations were made on cropped images and therefore coordinate correspondance was lost. Can fix this simply by re-adding the coordinate of the top right pixel to the x and y values. z is unaffected.
all_angles_original = all_angles
# Manually defining offsets
offset_names = c("A1", "A2", "A3")
x_offset = c(0, 1219, 1453)
y_offset = c(0, 4072, 1193)
#z_factor is a correction for a previous plane ratio error
z_factor = c(3.5, 1, 1)
offsets = data.frame(offset_names,x_offset,y_offset,z_factor)
for (i in seq_along(all_angles)) {
  all_angles[[i]]$SpindlePole_Location_Center_X <- all_angles[[i]]$SpindlePole_Location_Center_X + offsets$x_offset[i]
  all_angles[[i]]$SpindlePole_Location_Center_Y <- all_angles[[i]]$SpindlePole_Location_Center_Y + offsets$y_offset[i]
  all_angles[[i]]$SpindlePole_Location_Center_Z <- all_angles[[i]]$SpindlePole_Location_Center_Z * offsets$z_factor[i]
}


for (i in seq_along(all_angles)){
  # Load one sample at a time and convert angle data back to circular numeric, in radians.
  all_angles[[i]]$SpindleAngle = all_angles[[i]]$SpindlePole_AreaShape_Orientation
  all_angles[[i]]$SpindleAngle = as.circular(all_angles[[i]]$SpindleAngle, units = "degrees", type = "angles")
  #angles_degrees = circular(all_angles[[i]]$SpindleAngle, units = "degrees")
  # angle_radians = conversion.circular(angles_degrees, units = "radians")
  # Construct unit vector from each angle by adding a few columns on. First is the angle in radians, followed by the unrotated components of each unit vector. This is kinda clean, actually.
  all_angles[[i]] = all_angles[[i]] %>%
    rowwise() %>%
    mutate(
      angle_radians = conversion.circular(SpindleAngle, units = "radians"),
      avec_raw_x = cos(angle_radians),
      avec_raw_y = sin(angle_radians),
      avec_raw_z = 0
    )
}

# The all_angles df now has three additional columns that, together, make a unit vector of the original angle. 
# These just need to be reconstructed and multiplied by the corresponding sample's rotation matrix to get the new angles
# VERY IMPORTANT that the LMs and angles for the same samples are loaded in the same order.
for (i in 1:length(all_angles)){
  all_angles[[i]] = all_angles[[i]] %>%
    rowwise() %>%
    mutate(
      vec_rot = list(OPA_Rotations[,,1] %*% c(avec_raw_x, avec_raw_y, avec_raw_z)),
      avec_rot_x = vec_rot[1],
      avec_rot_y = vec_rot[2],
      avec_rot_z = vec_rot[3]
    ) %>%
    ungroup() %>%
    dplyr::select(-vec_rot)
}

## May need later: directly create vector from angle
#angle_vector = vector(length = 3)
#angle_vector[1] = cos(this_angle)
#angle_vector[2] = sin(this_angle)
#angle_vector[3] = 0

 #Coordinate list of angles, as an array with the same dimensions (ncells x 3 x nsamples)

## Transformation of coordinates into the same space
for (i in 1:length(all_angles)){
  
  # 1. Get original landmarks
  lm_raw <- lm_e10_array[, , i]
  
  # 2. Get the GPA-transformed landmarks
  lm_aligned <- gpa_e10$rotated[, , i]
  
  # 3. Get centroid
  sample_centroid <- colMeans(lm_raw)
  lm_centered <- sweep(lm_raw, 2, sample_centroid, "-")
  
  # 4. Get centroid size
  sample_cs <- sqrt(sum(lm_centered^2))
  lm_scaled <- lm_centered / sample_cs
  
  # 6. Transform all points by centroid (translation), centroid size (scaling), and rotation matrix (from OPA)
  all_angles[[i]] = all_angles[[i]] %>%
  rowwise() %>%
  mutate(
    trans = list(transform_point(x = SpindlePole_Location_Center_X, y = SpindlePole_Location_Center_Y, z = SpindlePole_Location_Center_Z, centroid = sample_centroid, cs = sample_cs, R = OPA_Rotations[,,i])),
    t_x = trans[[1]],
    t_y = trans[[2]],
    t_z = trans[[3]]
  ) %>%
    # Remove temporary trans coordinates
  #select(-trans) %>%
  ungroup()
}

#A test- take a few sample angles, rotate them along with a landmark set, and plot it all.
open3d(zoom = 0.75, windowRect = c(0, 0, 700, 700)) 
# plot the decimated head mesh
# rgl::shade3d(head_mesh_spec1_dec, color = "gray", alpha =0.9)
# plot the landmarks in blue
plot3d(gpa_e10$mshape, type = "s", radius = 0.01, col = "blue", xlab = "X", ylab = "Y", zlab = "Z")
text3d(gpa_e10$mshape, texts = as.character(1:nrow(gpa_e10$mshape)), adj = c(1, 1), cex = 0.8, col = "black")
#plot_3d_LMs(lms_e10, 'darkblue')
close3d()


#### PART 4 ####
### Geometric morphometric analysis of shape change and mitotic orientation

## Load in data: mandible landmarks from E10.0 and E10.5 volumes, and meshes from tissue segmentations
# This analysis uses a reduced 6-landmark scheme for the mandible alone.
  # ---------------------------
  # Paths to edit
  gmpath = paste(fullpath,"Data", "GM", sep="/")
  landmark_dir_e10 <- paste(gmpath,"Landmarks", "e10", sep="/") # folder with .csv files for e10
  landmark_dir_e105 <- paste(gmpath,"Landmarks", "e105", sep="/") # folder with .csv files for e105
  mesh_dir_e10 <- paste(gmpath,"Meshes", "e10", sep="/") # folder with .ply meshes for e10
  mesh_dir_e105 <- paste(gmpath,"Meshes", "e105", sep="/") # folder with .ply meshes for e105
  # ---------------------------
  # list files by group
  lm_files_e10 <- list.files(landmark_dir_e10, pattern = "\\.csv$", full.names = TRUE)
  lm_files_e105 <- list.files(landmark_dir_e105, pattern = "\\.csv$", full.names = TRUE)
  mesh_files_e10 <- list.files(mesh_dir_e10, pattern = "\\.ply$", full.names = TRUE)
  mesh_files_e105 <- list.files(mesh_dir_e105, pattern = "\\.ply$", full.names = TRUE)
  
  
  # Test for missing files
  if(length(lm_files_e10) == 0 || length(lm_files_e105) == 0) stop("No e10 or e105 landmark files found in subfolders")
  
  # Load landmarks into an array
  L_e10 <- read_all_landmarks(lm_files_e10)
  L_e105 <- read_all_landmarks(lm_files_e105)
  
  # Load meshes into a list
  M_e10 <- list()
  for(m in mesh_files_e10){
    name <- tools::file_path_sans_ext(basename(m))
    M_e10[[name]] <- Rvcg::vcgPlyRead(m, updateNormals=TRUE, clean=FALSE)
  }
  M_e105 <- list()
  for(m in mesh_files_e105){
    name <- tools::file_path_sans_ext(basename(m))
    M_e105[[name]] <- Rvcg::vcgPlyRead(m, updateNormals=TRUE, clean=FALSE)
  }
  
  
  # Combine landmarks into one list and check that each landmark set has a mesh
  L_all <- c(L_e10, L_e105)
  M_all <- c(M_e10, M_e105)
  
  common_names <- intersect(names(L_all), names(M_all))
  if(length(common_names) < length(L_all)) warning("Some landmarks or meshes do not have matching names; using intersection")
  
  
  # Reorder lists to common names
  L_all <- L_all[common_names]
  M_all <- M_all[common_names]

  #Build landmark array with all samples
  land_arr <- build_landmark_array(L_all)
  
  
  # create group vector aligned to columns of land_arr that describes which sample belongs to which age, derived form separated age groups
  group_vec <- ifelse(names(L_all) %in% names(L_e10), "e10", "e105")
# Register samples
  # Run GPA
  gps <- gpagen(land_arr, ProcD = FALSE)
  
  # After GPA, compute group mean shapes
  mean_e10 <- mshape(gps$coords[,,group_vec=="e10"]) # p x 3
  mean_e105 <- mshape(gps$coords[,,group_vec=="e105"])
  
  # Visualize mean shapes, if you want
  open3d()
  shade3d(M_e10[[1]], alpha = 0.7) # This currently shows first e10 mesh

# Propagate surface semilandmarks across the mandible

# Generate shape average for each group separately

# Generate vectors of growth


# Plot vectors of growth on average mesh


# Based on 3D angle registrations, generate an average mitotic angle at each landmark position and plot (ask david for arrow code?)


#### PART 5 ####
### Positional Orientation in the MdP

