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
library(compositions)
library(colorRamps)
library(RColorBrewer)

source("GitHub/division-orientation/Scripts/heatmapGIF.R")

# A few helper functions
deg_to_rad = function(deg) {
  return(deg * pi / 180)
}
plot_3d_LMs = function(LMs, color) {
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
read_csv_landmarks = function(file){
  df = read.csv(file, header = TRUE)
  # try common column names
  if(all(c("x","y","z") %in% names(df))){
    lma =(as.matrix(df[,c("x","y","z")]))
  } else if(all(c("X","Y","Z") %in% names(df))){
    lma =(as.matrix(df[,c("x","y","z")]))
  } else if(all(c("r","a","s") %in% names(df))){
    lma = (as.matrix(df[,c("r","a","s")]))
  } else {
    # if no column names are found, take first three numeric columns
    nums = sapply(df, is.numeric)
    lma = (as.matrix(df[, which(nums)[1:3] ]))
  }
  colnames(lma) = c("x","y","z")
  return(lma)
}

# Calculate distance between two angles
angle_between <- function(u, v) {
  cosang <- sum(u * v) / (sqrt(sum(u^2)) * sqrt(sum(v^2)))
  acos(pmin(pmax(cosang, -1), 1)) * 180 / pi
}

# read landmarks and meshes; assume matching names (without extension)
read_all_landmarks = function(files){
  L = list()
  for(f in files){
    pts = read_csv_landmarks(f)
    L[[tools::file_path_sans_ext(basename(f))]] = pts
  }
  return(L)
}

# Build landmark array
build_landmark_array = function(LM_list){
  n = length(LM_list)
  p = nrow(LM_list[[1]])
  lm_arr = array(NA, dim = c(p,3,n))
  for(i in 1:length(LM_list)){
    lm_arr[,,i] = as.matrix(LM_list[[i]])
    i = i + 1
  }
  return(lm_arr)
}

# This is basically a manual point transformation to a whole list in a DF, given a centroid, centroid size, and 3x3 rotation matrix
transform_point = function(x, y, z, centroid, cs, R) {
  vec = c(x, y, z)
  vec_centered = (vec - centroid) / cs
  as.list(vec_centered %*% R)
}
# Load in data
fullpath = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filepath = paste(fullpath,"Data", sep="/")
#e10_tiff_path = paste(filepath,"Volumes", "e10", sep="/")
#e105_tiff_path = paste(filepath,"Volumes", "e10_5", sep="/")
lm_cell_path = paste(filepath, "GM", "Landmarks", "e105_cell", sep="/")
angles_path = paste(filepath,"Angles", "Mandibles", sep="/")

# List all TIFF files for both ages
#e10_tiff_files = list.files(e10_tiff_path, pattern = "\\.tiff", full.names = TRUE)
#e105_tiff_files = list.files(e105_tiff_path, pattern = "\\.tiff", full.names = TRUE)
# Load TIFF volumes into a list
#e10_volumes_list = lapply(e10_tiff_files, function(file) {
#  readImage(file)  # Reads TIFF as a 3D array (x, y, z)
#})
#e105_volumes_list = lapply(e105_tiff_files, function(file) {
#  readImage(file)  # Reads TIFF as a 3D array (x, y, z)
#})
## This will be used for visualization later

# Load Landmarks in one dataframe
sample_names = list.files(lm_cell_path, pattern = "\\.csv", full.names = FALSE, recursive = TRUE)

lm_df = data.frame(
  file_name = basename(sample_names),
  age = "e105",
  stringsAsFactors = FALSE
)
# Subset by age and store coordinates
lms_e10 = subset(lm_df, age == "E10")
lms_e10$file_name = paste0(lm_cell_path, "/E10/", lms_e10$file_name)
lms_e10_data = lapply(lms_e10$file_name, read_csv)

lms_e105 = subset(lm_df, age == "e105")
lms_e105$file_name = paste0(lm_cell_path, "/", lms_e105$file_name)
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
gpa_e105 = procSym(L_cell)

# Retrieve coordinates and subset into two groups by age
proc_coords = gpa_e105$rotated

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


# We start with an adapted version of the registrations for the integrated dataset, using L_cell

  # Retrieve coordinates for each rotated sample using group_vec
  proc_coords = gps$coords[,,which(group_vec=="cell")]
  original_coords = land_arr[,,which(group_vec=="cell")]
  
  # Landmark coordinates are currently in real space, while cell coordinates are in voxel space. Must convert original LMs to voxel space
    # Note: This has now been corrected in the dataset but I'm leaving the code here in case I need it in the future
  # original_coords_test = original_coords
  #voxFactor = 0.00021
  #original_coords[,1,] = original_coords_test[,1,]/voxFactor
  #original_coords[,2,] = original_coords_test[,2,]/voxFactor
  #original_coords[,3,] = original_coords_test[,3,]/voxFactor
  # Ordinary Procrustes Analysis with procOPA on each sample to the mean. procOPA$R stores the rotations matrix
  
  # Set up array to hold rotation matrices
  nsamples = dim(proc_coords)[3]
  OPA_Rotations = array(NA, dim = c(3, 3, nsamples))
  
  # Perform an Ordinary procrustes analysis between the mean sample (mean_e105) and each individual sample (page of the array)
  # and store the 3x3 rotation matrix from the sample to the mean in the corresponding page of OPA_Rotations 
  for (i in 1:nsamples) {
    OPA_result = procOPA(mean_e105, original_coords[, , i])
    OPA_Rotations[,,i] = OPA_result$R
  }
  
### Load in all angle lists and loop through each to convert angles to vectors and transform based on OPA rotations. Angles are agnostic to scale and translation,
### So we'll just convert the angles themselves here and reposition them after.
angle_files = list.files(angles_path, pattern = "\\.csv$", full.names = TRUE)
all_angles = lapply(angle_files, read.csv)

for (i in seq_along(all_angles)){
  # Load one sample at a time and convert angle data back to circular numeric, in radians.
  all_angles[[i]]$SpindleAngle = all_angles[[i]]$AreaShape_Orientation
  all_angles[[i]]$SpindleAngle = as.circular(all_angles[[i]]$SpindleAngle, units = "degrees", type = "angles")
  #angles_degrees = circular(all_angles[[i]]$SpindleAngle, units = "degrees")
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

 #Coordinate list of angles, as an array with the same dimensions (ncells x 3 x nsamples)
## Transformation of coordinates into the same space
for (i in 1:length(all_angles)){
  
  # 1. Get original landmarks
  lm_raw = original_coords[, , i]
  
  # 2. Get the GPA-transformed landmarks
  lm_aligned = proc_coords[, , i]
  
  # 3. Get centroid
  sample_centroid = colMeans(lm_raw)
  lm_centered = sweep(lm_raw, 2, sample_centroid, "-")
  
  # 4. Get centroid size
  sample_cs = sqrt(sum(lm_centered^2))
  lm_scaled = lm_centered / sample_cs
  
  # 6. Transform all points by centroid (translation), centroid size (scaling), and rotation matrix (from OPA)
  all_angles[[i]] = all_angles[[i]] %>%
  rowwise() %>%
  mutate(
    trans = list(transform_point(x = Location_Center_X, y = Location_Center_Y, z = Location_Center_Z, centroid = sample_centroid, cs = sample_cs, R = OPA_Rotations[,,i])),
    t_x = trans[[1]],
    t_y = trans[[2]],
    t_z = trans[[3]]
  ) %>%
    # Remove temporary trans coordinates
  #select(-trans) %>%
  ungroup()
}

all_angles[[3]] = all_angles[[3]] %>% dplyr::select(!(c(X, X.1)))
# Take the coordinates out for a test plot
angles_flat = bind_rows(all_angles)
#temp- remove later
angles_A1 = all_angles[[1]]
write.csv(angles_A1, paste(filepath,"AurA_A1_AngleTest_axisflip_invert.csv", sep="/"))
angles_A1 = read.csv(paste(filepath,"AurA_A1_AngleTest.csv", sep="/"))
angles_A1$r = -10560-angles_A1$r
angles_A1$a = -7104-angles_A1$a
# end of temp
cell_coords = xyz.coords(x = angles_flat$t_x, y = angles_flat$t_y, z = angles_flat$t_z)
cell_coords_original = xyz.coords(x = angles_flat$Location_Center_X, y = angles_flat$Location_Center_Y, z = angles_flat$Location_Center_Z)
open3d()
plot3d(cell_coords, size = 1, col = "blue")
plot3d(cell_coords_original, size = 1, col = "red", add = TRUE)
plot3d(proc_coords[,,1], size = 10, col = 'cyan', add = TRUE)
plot3d(proc_coords[,,2], size = 10, col = 'cyan3', add = TRUE)
plot3d(proc_coords[,,3], size = 10, col = 'cyan4', add = TRUE)
plot3d(original_coords[,,1], size = 10, col = 'yellow', add = TRUE)
plot3d(original_coords[,,2], size = 10, col = 'orange', add = TRUE)
plot3d(original_coords[,,3], size = 10, col = 'brown', add = TRUE)
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
# This analysis uses a reduced 7-landmark scheme for the mandible alone.
  # ---------------------------
  # Paths to edit
  gmpath = paste(fullpath,"Data", "GM", sep="/")
  landmark_dir_e10 = paste(gmpath,"Landmarks", "e10", sep="/") # folder with .csv files for e10
  landmark_dir_e105 = paste(gmpath,"Landmarks", "e105", sep="/") # folder with .csv files for e105
  landmark_dir_cell = paste(gmpath,"Landmarks", "e105_cell", sep="/") # folder with .csv files for e105 samples with cell data
  landmark_dir_e11 = paste(gmpath,"Landmarks", "e11", sep="/") # folder with .csv files for e11
  landmark_dir_atlas = paste(gmpath,"Landmarks", "atlas", sep="/") # folder with .csv files for atlases by age
  mesh_dir_e10 = paste(gmpath,"Meshes", "e10", sep="/") # folder with .ply meshes for e10
  mesh_dir_e105 = paste(gmpath,"Meshes", "e105", sep="/") # folder with .ply meshes for e105
  mesh_dir_e11 = paste(gmpath,"Meshes", "e11", sep="/") # folder with .ply meshes for e105
  mesh_dir_atlas = paste(gmpath,"Meshes", "atlas", sep="/") # folder with .ply meshes for atlases by age
  # ---------------------------
  # list files by group
  lm_files_e10 = list.files(landmark_dir_e10, pattern = "\\.csv$", full.names = TRUE)
  lm_files_e105 = list.files(landmark_dir_e105, pattern = "\\.csv$", full.names = TRUE)
  lm_files_e11 = list.files(landmark_dir_e11, pattern = "\\.csv$", full.names = TRUE)
  lm_files_cell = list.files(landmark_dir_cell, pattern = "\\.csv$", full.names = TRUE)
  lm_files_atlas = list.files(landmark_dir_atlas, pattern = "\\.csv$", full.names = TRUE)
  mesh_files_e10 = list.files(mesh_dir_e10, pattern = "\\.ply$", full.names = TRUE)
  mesh_files_e105 = list.files(mesh_dir_e105, pattern = "\\.ply$", full.names = TRUE)
  mesh_files_e11 = list.files(mesh_dir_e11, pattern = "\\.ply$", full.names = TRUE)
  mesh_files_atlas = list.files(mesh_dir_atlas, pattern = "\\.ply$", full.names = TRUE)
  
  
  # Test for missing files
  if(length(lm_files_e10) == 0 || length(lm_files_e105) == 0 || length(lm_files_e11) == 0) stop("At least one age point is empty")
  
  # Load landmarks into an array
  L_e10 = read_all_landmarks(lm_files_e10)
  L_e105 = read_all_landmarks(lm_files_e105)
  L_e11 = read_all_landmarks(lm_files_e11)
  L_cell = read_all_landmarks(lm_files_cell)
  L_atlas = read_all_landmarks(lm_files_atlas)
  
  # Load meshes into a list
  M_e10 = list()
  for(m in mesh_files_e10){
    name = tools::file_path_sans_ext(basename(m))
    M_e10[[name]] = file2mesh(m, clean=FALSE)
  }
  M_e105 = list()
  for(m in mesh_files_e105){
    name = tools::file_path_sans_ext(basename(m))
    M_e105[[name]] = file2mesh(m, clean=FALSE)
  }
  M_e11 = list()
  for(m in mesh_files_e11){
    name = tools::file_path_sans_ext(basename(m))
    M_e11[[name]] = file2mesh(m, clean=FALSE)
  }
  M_atlas= list()
  for(m in mesh_files_atlas){
    name = tools::file_path_sans_ext(basename(m))
    M_atlas[[name]] = file2mesh(m, clean=FALSE)
  }
  
  
  # Combine landmarks into one list and check that each landmark set has a mesh
  L_shape = c(L_e10, L_e105, L_e11)
    
  
  M_all = c(M_e10, M_e105, M_e11)
  # NOTE: If using meshes from 3DSlicer, you may need to convert between RAS and LPS:
  M_all = lapply(M_all, LPS2RAS)
  #common_names = intersect(names(L_all), names(M_all))
  #if(length(common_names) < length(L_all)) warning("Some landmarks or meshes do not have matching names; using intersection")
  
  
  # Exclude any nonintersecting samples
  L_shape = L_shape[common_names]
  M_all = M_all[common_names]
  # Now we can add landmarks for the cell sets and atlases with them
  L_all = c(L_shape, L_cell)
  #Build landmark array with all samples
  land_arr = build_landmark_array(L_all)
  
  
  # create group vector aligned to columns of land_arr that describes which sample belongs to which age, derived from separated age groups
  group_vec = ifelse(names(L_all) %in% names(L_e10), "e10",
                     ifelse(names(L_all) %in% names(L_e105), "e105",
                            ifelse(names(L_all) %in% names(L_e11), "e11", 
                                   ifelse(names(L_all) %in% names(L_cell), "cell",
                                          ifelse(names(L_all) %in% names(L_atlas), "atlas", "NA")))))
# Register samples
  # Run GPA
  gps = gpagen(land_arr, ProcD = FALSE)
  
  # After GPA, compute group mean shapes for each group seperately
  mean_e10 = mshape(gps$coords[,,which(group_vec=="e10")]) # p x 3
  mean_e105 = mshape(gps$coords[,,which(group_vec=="e105")])
  mean_e11 = mshape(gps$coords[,,which(group_vec=="e11")])
  mean_all = mshape(gps$coords)
  
  # Visualize mean shapes, if you want
  open3d()
  shade3d(M_all$Dec2_E105_3, alpha = 0.7, color = "grey70", specular = 1) 
  # Plot sample landmarks
  plot3d(L_all$Dec2_E105_3, size = 10, col = "red", add=TRUE)
  # Plot avg landmarks
  plot3d(mean_e105, size = 5, add=TRUE)

  
  #TODO: Atlas meshes that do not come from slicer must be converted between coordinate systems
  M_atlas = lapply(M_atlas, LPS2RAS)
  
  #Atlas generation by morphing an atlas mesh to the average LMs
  e10_mean_shape = tps3d(M_atlas$e10_atlas, L_atlas$e10_atlas, mean_e10)
  e105_mean_shape = tps3d(M_atlas$e105_atlas, L_atlas$e105_atlas, mean_e105)
  e11_mean_shape = tps3d(M_atlas$e11_atlas, L_atlas$e11_atlas, mean_e11)
  
  e105_from_e10 = tps3d(M_atlas$e10_atlas, L_atlas$e10_atlas, mean_e105)
  e10_from_e105 = tps3d(M_atlas$e105_atlas, L_atlas$e105_atlas, mean_e10)
  e11_from_e105 = tps3d(M_atlas$e105_atlas, L_atlas$e105_atlas, mean_e11)
  e105_from_e11 = tps3d(M_atlas$e11_atlas, L_atlas$e11_atlas, mean_e105)
  e11_from_e10 = tps3d(M_atlas$e10_atlas, L_atlas$e10_atlas, mean_e11)
  

  
  shade3d(e105_mean_shape, alpha = 0.7, color = "white", specular = 1)
  shade3d(e105_from_e10, alpha = 0.7, color = "yellow", specular = 1)
  plot3d(mean_e10, size = 10, col = "red", add=TRUE)
  
  shade3d(e10_mean_shape, alpha = 0.8, color = "white", specular = 1)
  plot3d(mean_e10, size = 10, col = "red", add=TRUE)
# Plot vectors of growth on average mesh
  # Draw arrows from e10 mean to e105 mean
  growth_vecs <- mean_e105 - mean_e10
  gvec_scale_factor <- 2
  # shade3d(M_all[[1]], alpha = 0.7, color = "grey70")
  #gvec_scale_factor <- 0.1 * max(dist(mean_e10)) 
  for(i in 1:nrow(mean_e10)){
    start <- mean_e10[i,]
    end <- mean_e10[i,] + growth_vecs[i,]*gvec_scale_factor
    segments3d(rbind(start, end), col="blue", add = TRUE)
    # arrow3d(start, end, type = "extrusion", col = "blue")
  }
# Repeat for e105-e11
  # Draw arrows from e10 mean to e105 mean
  growth_vecs = mean_e11 - mean_e105
  gvec_scale_factor = 1
  #gvec_scale_factor <- 0.1 * max(dist(mean_e10)) 
  for(i in 1:nrow(mean_e105)){
    start = mean_e105[i,]
    end = mean_e105[i,] + growth_vecs[i,]*gvec_scale_factor
    segments3d(rbind(start, end), col="blue")
  }
  
#Mesh distance between e105 and e11 atlases
  #Start with setting up our heatmap pallete
  colExtremes = c("#0288D1", "#FAFAFA", "#D32F2F")

  col_ramp = colorRampPalette(c(colExtremes[1],colExtremes[2],colExtremes[3]))
  col = col_ramp(100)
  
  fixedColFun <- function(vals, minVal = -0.1, maxVal = 0.1, palette = col) {
    scaled <- (vals - minVal) / (maxVal - minVal)  # scale to [0, 1]
    scaled[scaled < 0] <- 0
    scaled[scaled > 1] <- 1
    palette[ceiling(scaled * (length(palette) - 1)) + 1]
  }

  colFun <- function(vals) fixedColFun(vals, minVal = -0.1, maxVal = 0.1, palette = col)
  
  
  meshDist(e105_mean_shape, e11_mean_shape, lim = c(-.2, .2), shade = TRUE, displace = FALSE, userMatrix = front, steps = 10, rampcolors = col)
  meshDist(e105_mean_shape, e11_from_e105, lim = c(-.2, .2), shade = TRUE, displace = FALSE, userMatrix = front, steps = 10, rampcolors = col)
  meshDist(e105_from_e11, e11_mean_shape, lim = c(-.2, .2), shade = TRUE, displace = FALSE, userMatrix = front, steps = 10, rampcolors = col)
  
  meshDist(e10_mean_shape, e105_from_e10, lim = c(-.2, .2), shade = TRUE, displace = FALSE, userMatrix = front, steps = 10, rampcolors = col)
  meshDist(e10_mean_shape, e105_mean_shape, lim = c(-.2, .2), shade = FALSE, displace = TRUE, userMatrix = front, steps = 10, rampcolors = col)
  meshDist(e10_from_e105, e105_mean_shape, lim = c(-.2, .2), shade = TRUE, displace = FALSE, userMatrix = front, steps = 10, rampcolors = col)
  
  shade3d(e10_mean_shape, alpha = 0.7, color = "white", specular = 1)
  shade3d(e105_mean_shape, alpha = 0.7, color = "blue", specular = 1)
  
  open3d()
  shade3d(e10_mean_shape, col = "white", specular = 1, alpha = 0.5)
  shade3d(e105_mean_shape, col = "red", specular = 1, alpha = 0.5)
  plot3d(mean_e10, col = "black", type = "s", specular = 1, add = TRUE, size = 1)
  plot3d(mean_e105, col = "red", type = "s", specular = 1, add = TRUE, size = 1)
  
  open3d()
  shade3d(e10_from_e105, col = "white", specular = 1, alpha = 0.8, userMatrix = front)
  plot3d(mean_e10, col = "red", type = "s", specular = 1, add = TRUE, size = 1)

  open3d()
  shade3d(e105_from_e10, col = "white", specular = 1, alpha = 0.8, userMatrix = front)
  plot3d(mean_e105, col = "red", type = "s", specular = 1, add = TRUE, size = 1)
  
  open3d()
  shade3d(e11_from_e105, col = "white", specular = 1, alpha = 0.8, userMatrix = front)
  plot3d(mean_e11, col = "red", type = "s", specular = 1, add = TRUE, size = 1)
  
  
  front = par3d()$userMatrix
  heatmapPretty(sub1 = e105_mean_shape, sub2 = e10_mean_shape, 
                path = "GitHub/division-orientation/Figures/heatmapTest.png", userMatrix = front, legend_name = "Closest point distance",bg = "white",
                limit = 0.3)
  
  sub1 = e11_mean_shape
  sub2 = e105_mean_shape
  path = "GitHub/division-orientation/Figures/heatmapTest.png"
  userMatrix = front
  legend_name = "Closest point distance"
  bg = "white"
  limit = 0.3
  colExtremes=c("#0288D1", "#D32F2F")
  legend=TRUE
  legend_orientation="vertical"
  
  limit=NULL
  
  
  
  
  
# Based on 3D angle registrations, generate an average mitotic angle at each landmark position and plot (ask david for arrow code?)

#TODO: Replace all above functions with real atlases (thanks Alejandro)
  # Temporary best meshes:
  # E10.0: Dec2_e10_12
  # E10.5: Dec2_E105_3
  # E11.0: Feb12_E115_2
  
  
  
  
  
  
#### PART 5 ####
### Positional Orientation in the MdP
  


