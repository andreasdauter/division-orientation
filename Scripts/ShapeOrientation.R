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
library(rayvertex)
library(RANN)
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
library(dbscan)

source("GitHub/division-orientation/Scripts/heatmapGIF.R")


#### HELPER FUNCTIONS ####
# Quick convert degrees to radians
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

# Calculate distance between two 2d angles
angle_between <- function(u, v) {
  cosang <- sum(u * v) / (sqrt(sum(u^2)) * sqrt(sum(v^2)))
  acos(pmin(pmax(cosang, -1), 1)) * 180 / pi
}

#Calculate distance between two 3d angles
angle_between_3d <- function(u, v) {
  # Ensure numeric vectors
  u <- as.numeric(u)
  v <- as.numeric(v)
  
  # Normalize (safety step, in case vectors are not unit length)
  u <- u / sqrt(sum(u^2))
  v <- v / sqrt(sum(v^2))
  
  # Dot product
  dot <- sum(u * v)
  
  # Clamp to avoid numerical issues (dot might be slightly >1 or < -1 due to rounding)
  dot <- max(min(dot, 1.0), -1.0)
  
  # Angle in radians → convert to degrees
  theta <- acos(dot) * 180 / pi
  return(theta)
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

# This is a function used in Part 4 to generate a nesting isomorphic set of surfaces.
shrink_mesh <- function(mesh, factors, center) {
  # mesh: mesh3d object
  # factors: numeric vector of scaling factors (e.g. c(1.0, 0.75, 0.5))
  # center: numeric vector of length 3 (x,y,z)
  
  verts <- t(mesh$vb[1:3, ])   # n_vertices × 3
  
  lapply(factors, function(f) {
    verts_scaled <- sweep(verts, 2, center, FUN = "-") * f + 
      matrix(center, nrow(verts), 3, byrow = TRUE)
    
    mesh_new <- mesh
    mesh_new$vb[1:3, ] <- t(verts_scaled)
    return(mesh_new)
  })
}

# Gaussian smooth function in 3D
gaussian_smooth_vertices <- function(values, vertices, sigma = 0.1, k = 1) {
  n <- nrow(vertices)
  # nearest neighbours for each vertex (includes self as first column)
  nn <- nn2(vertices, vertices, k = k + 1)  # k+1 so that nn$nn.idx[,1] is self
  idxs <- nn$nn.idx    # n x (k+1)
  dists <- nn$nn.dists # n x (k+1)
  
  smoothed <- numeric(n)
  for (i in seq_len(n)) {
    # indices and distances (including self)
    neigh_idx <- idxs[i, ]
    neigh_dist <- dists[i, ]
    w <- exp(-0.5 * (neigh_dist / sigma)^2)
    w <- w / sum(w)
    smoothed[i] <- sum(values[neigh_idx] * w)
  }
  return(smoothed)
}


#### PART 1 ####
### Data preparation and GM
# Load in data
fullpath = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filepath = paste(fullpath,"Data", sep="/")
lm_cell_path = paste(filepath, "GM", "Landmarks", "e105_cell", sep="/")
angles_path = paste(filepath,"Angles", "Mandibles", sep="/")

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

#common_names = intersect(names(L_all), names(M_all))
#if(length(common_names) < length(L_all)) warning("Some landmarks or meshes do not have matching names; using intersection")
# Convert atlas meshes to LPS
M_all = lapply(M_all, LPS2RAS)
M_atlas = lapply(M_atlas, LPS2RAS)

# Exclude any nonintersecting samples
L_shape = L_shape[common_names]
M_all = M_all[common_names]
# Now we can add landmarks for the cell sets and atlases with them
L_all = c(L_shape, L_cell)
#Build landmark array with all samples
land_arr_rps = build_landmark_array(L_all)

# NOTE: If using meshes from 3DSlicer, you may need to convert landmarks between RAS and LPS:
land_arr = land_arr_rps
land_arr[,c(1,2),] = -land_arr[,c(1,2),]

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

#Atlas generation by morphing an atlas mesh to the average LMs
e10_mean_shape = tps3d(M_atlas$e10_atlas, L_atlas$e10_atlas, mean_e10)
e105_mean_shape = tps3d(M_atlas$e105_atlas, L_atlas$e105_atlas, mean_e105)
e11_mean_shape = tps3d(M_atlas$e11_atlas, L_atlas$e11_atlas, mean_e11)

# 

#### PART 2 ####
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

# This section is just plots to test that the coordinates all good moved into the correct space.
angles_flat = bind_rows(all_angles)

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

# plot the landmarks in blue
plot3d(gpa_e10$mshape, type = "s", radius = 0.01, col = "blue", xlab = "X", ylab = "Y", zlab = "Z")
text3d(gpa_e10$mshape, texts = as.character(1:nrow(gpa_e10$mshape)), adj = c(1, 1), cex = 0.8, col = "black")
close3d()


#### PART 3 ####
### Geometric morphometric analysis of shape change

  # Convert atlas meshes to LPS
  M_all = lapply(M_all, LPS2RAS)
  M_atlas = lapply(M_atlas, LPS2RAS)
  # Quick test plot: Mean shape and LMs
  shade3d(e105_mean_shape, alpha = 0.7, color = "white", specular = 1, add=TRUE)
  plot3d(mean_e105, size = 10, col = "red", add=TRUE)

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
  
  
  
#### PART 4 ####
### Relating mitotic orientation to shape change
  
# For each cell, average the unit vector of all angles within a defined area (200 microns, to start) and store as avg_x, avg_y, and avg_z
    # NOTE: Alignment is a RELATIVE value and does depend on the choice of radius here
  align_radius = 0.2
  
  # Preallocate storage for the average orientation vector components, since the calculation will take a long time.
  avg_x = numeric(nrow(angles_flat))
  avg_y = numeric(nrow(angles_flat))
  avg_z = numeric(nrow(angles_flat))
  
  # Very handy nearest neighbours function from dbscan. Requires coordinates as a matrix
  angle_coords = as.matrix(angles_flat[, c("t_x", "t_y", "t_z")])
  neighbours = frNN(angle_coords, eps = align_radius)
  
  # Loop through each cell, computing the average orientation vector in the alignment radius
  for (i in seq_along(neighbors$id)) {
    idx <- neighbours$id[[i]]  # indices of neighbors within radius
    avg_x[i] <- mean(angles_flat$avec_rot_x[idx])
    avg_y[i] <- mean(angles_flat$avec_rot_y[idx])
    avg_z[i] <- mean(angles_flat$avec_rot_z[idx])
  }
  
  # Add angles back into the dataframe
  angles_flat$avg_x <- avg_x
  angles_flat$avg_y <- avg_y
  angles_flat$avg_z <- avg_z
  
# Calculate the smallest angle between the two 3D unit vectors and store this score as "alignment"
  
  angles_flat$alignment <- mapply(function(ax, ay, az, gx, gy, gz) {
    theta <- angle_between_3d(
      c(ax, ay, az),
      c(gx, gy, gz)
    )
    # Data is directionless: angle and its supplement are equivalent. As such, we look for the lowest.
    min(theta, 180 - theta)
  },
  ax = angles_flat$avec_rot_x,
  ay = angles_flat$avec_rot_y,
  az = angles_flat$avec_rot_z,
  gx = angles_flat$avg_x,
  gy = angles_flat$avg_y,
  gz = angles_flat$avg_z)
  
# Outer plots: mitotic orientation and alignment
  # At each vertex of the mean E10.5 mesh, calculate an average alignment score and mitotic angle from every cell within the same radius.
  # I can likely borrow logic here from the alignment calculation itself
  #Pull out vertices of the mesh
  outer_verts <- t(e105_mean_shape$vb[1:3, ])
  
  vertex_neighbours <- frNN(angle_coords, query = outer_verts, eps = align_radius)
  # Preallocate local alignment, then average alignment of nearby cells at each vertex
  local_alignment <- numeric(nrow(outer_verts))
  
  for (i in seq_along(vertex_neighbours$id)) {
    idx <- vertex_neighbours$id[[i]]
    if (length(idx) > 0) {
      local_alignment[i] <- mean(angles_flat$alignment[idx])
    } else {
      local_alignment[i] <- NA  # If there are no cells nearby, do not assign an alignment value
    }
  }
  
  align_col_ramp <- colorRampPalette(c("red", "yellow"))
  
  # Make colours for each vertex
  ncol <- 100
  pal = align_col_ramp(ncol)
  local_alignment_scaled <- (local_alignment - min(local_alignment, na.rm = TRUE)) / (max(local_alignment, na.rm = TRUE) - min(local_alignment, na.rm = TRUE))
  col_idx <- round(local_alignment_scaled * (ncol - 1)) + 1
  vertex_cols <- pal[col_idx]

  # Apply to mesh
  shade3d(e105_mean_shape, col = vertex_cols, specular = 1, userMatrix = 1, add = TRUE)
# Shell plots
  # Define the centerpoint for shrinking at the very back of the tissue
  shell_center = c(0, 0, -0.75)
  # Shrink the shell by 1.5x the alignment radius, such that every cell is included at least once in the radius calculation.
  #mesh_inner_test = shrink_mesh(e105_mean_shape, factor = 0.75)
  #shade3d(mesh_inner_test, color = "white", specular = 1, userMatrix = 1, add = TRUE)
  
  # Generated a nesting-doll series
  #shell_factors <- c(1.0, 0.75, 0.5, 0.25)  # outer to inner
  largest_shell = 1.0
  smallest_shell = 0.5
  
  shell_factors <- c(largest_shell, largest_shell - ((largest_shell - smallest_shell)/3), largest_shell - ((largest_shell - smallest_shell)/3*2), smallest_shell)
  shell_meshes <- lapply(shell_factors, function(f) shrink_mesh(e105_mean_shape, f, shell_center))
  # Repeat local alignment on each of these nested shells. Store in a list, where each page is one shell
  local_alignment_list <- vector("list", length(shell_meshes))
  
  for (m in seq_along(shell_meshes)) {
    verts <- t(meshes[[m]]$vb[1:3, ])
    
    neighbors <- frNN(angle_coords, query = verts, eps = align_radius)
    
    local_alignment <- sapply(neighbors$id, function(idx) {
      if (length(idx) > 0) {
        mean(angles_flat$alignment[idx])
      } else {
        NA
      }
    })
    
    local_alignment_list[[m]] <- local_alignment
  }
  
  # For each shell, scale the values on the same axis
    #Find global max and min
  all_align <- unlist(local_alignment_list)
  alignment_min <- min(all_align, na.rm = TRUE)
  alignment_max <- max(all_align, na.rm = TRUE)
  
   align_col <- function(values) {
     vals_scaled <- (values - alignment_min) / (alignment_max - alignment_min) # Scale alignment values to global maxima
     col_idx <- round(vals_scaled * (ncol - 1)) + 1
     return(pal[col_idx])
   }
   
   vertex_cols_alignment <- vector("list", length(meshes))
   for (m in seq_along(shell_meshes)) {
     vertex_cols_alignment[[m]] <- align_col(local_alignment_list[[m]])
   }

  # View meshes with alignment, one at a time or together
  shade3d(shell_meshes[[1]][[1]], col = vertex_cols_alignment[[1]], specular = 1, userMatrix = 1, add = TRUE)
  shade3d(shell_meshes[[2]][[1]], col = vertex_cols_alignment[[2]], specular = 1, userMatrix = 1, add = TRUE)
  shade3d(shell_meshes[[3]][[1]], col = vertex_cols_alignment[[3]], specular = 1, userMatrix = 1, add = TRUE)
  shade3d(shell_meshes[[4]][[1]], col = vertex_cols_alignment[[4]], specular = 1, userMatrix = 1, add = TRUE)
  
  # View meshes without alignment
  open3d()
  cols <- c("red", "orange", "green", "blue")
  for (i in seq_along(shell_meshes)) {
    shade3d(shell_meshes[[i]][[1]], col = cols[i], alpha = 1)  # translucent shells
  }
  
  # To verify that all cells are inlcuded, plot them over the nesting set
  plot3d(cell_coords, size = 1, col = "blue", add = TRUE)
  axes3d()
  
  
  
  ### Moving on: Instead of alignment, let's now make a david plot (arrow at each vertex that describes average orientation)
  n_verts = nrow(outer_verts)
  # Coords stored in angle_coords
  angle_vecs = as.matrix(angles_flat[, c("avec_rot_x", "avec_rot_y", "avec_rot_z")])
  
  # Compute average orientation at each vertex of the e10.5 mean shape mesh
  vertex_vecs <- matrix(NA, nrow = n_verts, ncol = 3)
  
  for (i in seq_len(n_verts)) {
    v <- outer_verts[i, ]
    
    # Which cells are within radius
    dists <- sqrt(rowSums((angle_coords - matrix(v, nrow(angle_coords), 3, byrow = TRUE))^2))
    idx <- which(dists <= align_radius)
    
    if (length(idx) > 0) {
      # Mean of orientation vectors
      mean_vec <- colMeans(angle_vecs[idx, , drop = FALSE])
      # Normalize to unit length
      mean_vec <- mean_vec / sqrt(sum(mean_vec^2))
      vertex_vecs[i, ] <- mean_vec
    } else {
      vertex_vecs[i, ] <- c(NA, NA, NA)
    }
  }
  
  
#Plotting! We're going to convert this to a single matrix for faster plotting
  # Scale vectors by alignment score
  max_len <- 0.01 * mean(diff(range(outer_verts)))
  arrow_lengths <- max_len * (local_alignment / max(local_alignment, na.rm = TRUE)) hn
  
  # Compute end points
  ends <- outer_verts + vertex_vecs * arrow_lengths
  
  # Interleave start and end points for segments3d
  segments_matrix <- matrix(NA, nrow = n_verts * 2, ncol = 3)
  segments_matrix[seq(1, n_verts*2, by = 2), ] <- outer_verts
  segments_matrix[seq(2, n_verts*2, by = 2), ] <- ends
  
  # Interleave colors for each segment
  # segments3d expects a vector of colors corresponding to each row pair
  segment_colors <- rep(vertex_cols, each = 2)
  
  # Open 3D window and plot
  open3d()
  shade3d(e105_mean_shape, color = "grey80", alpha = 0.3)
  segments3d(segments_matrix, col = segment_colors, lwd = 2)
  # TODO: Investigate potential bug with angles beign projected to a plane
  
  
  
  
#### PART 5 ####
### Positional Orientation in the MdP
  
# Note: All angles are stored at this point in angles_flat
  
# 1. Ray construction from coords and vectors
 rays = list(vb = t(angle_coords), normals = t(angle_vecs))
  
# 2. Calculate ray intersections with the mean mesh
  class(rays) = "mesh3d"
  ray_hits <- vcgRaySearch(x = rays, mesh = e105_mean_shape, mindist = TRUE, threads = 2)
  
  
  #Filter invalid intersections out
  hit_mask <- ray_hits$quality == 1
  hit_points = t(ray_hits$vb[1:3, hit_mask, drop = FALSE])
    #This is what we'll use for the rest of our analysis- nx3 matrix that contains the coordinates of hit points on the mesh.
  
# 3. Match intersections to nearest vertex of the mesh
  mesh_vertices = t(e105_mean_shape$vb[1:3,, drop = FALSE])
  
  nn_hits = nn2(mesh_vertices, hit_points, k=1)
  #nearest_vertex_idx stores the closest vertex ID for each hit point
  nearest_vertex_idx = nn_hits$nn.idx[,]
  
  # raw counts per vertex- simplest representation
  po_counts = integer(nrow(mesh_vertices))
  po_counts[] <- tabulate(nearest_vertex_idx, nbins = nrow(mesh_vertices))
  
# 4. Because we have far fewer hits than vertices, we must smooth over neighbours. We do thsi with a gaussian-weighting
  # Note: This will have to be visualized from the interior view
  
  # First: Define bounding box and set sigma as a fraction of this
  sigma_fraction = 0.1
  bbox <- apply(mesh_vertices, 2, range)
  diag_len <- sqrt(sum((bbox[2,] - bbox[1,])^2))
  gs_sigma <- diag_len * sigma_fraction
 
  
  
  po_counts_smoothed <- gaussian_smooth_vertices(po_counts, mesh_vertices, sigma = gs_sigma, k = 200)
  # K is the number of nearest neighbours used for smoothing- adjust as needed
  
# 5. Visualize
  # First, set colour scale
  counts_norm <- counts_smoothed - min(counts_smoothed, na.rm = TRUE)
  if (max(counts_norm, na.rm = TRUE) > 0) {
    counts_norm <- counts_norm / max(counts_norm, na.rm = TRUE)
  } else {
    counts_norm[] <- 0
  }
  po_col_ramp <- colorRampPalette(c("gray", "magenta4"))
  po_cols <- po_col_ramp(100)[pmax(1, pmin(100, as.integer(cut(counts_norm, breaks = 100, labels = FALSE))))]
  
  
  # Visualize
  open3d()
  shade3d(e105_mean_shape, color = po_cols, meshColor = "vertices", specular=1)

