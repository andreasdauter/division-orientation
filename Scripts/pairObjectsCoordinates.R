# Load necessary library
library(dplyr)
library(stringr)
library(ggplot2)
library(circular)
library(bpnreg)
library(Directional)

# Function to calculate the Euclidean distance
euclidean_distance = function(coord1, coord2) {
  sqrt(sum((coord1 - coord2)^2))
}

# Function to find the closest points and return paired row numbers
pair_closest_points = function(df1, df2) {
  # Initialize a dataframe to store pairings
  paired_points = data.frame(Row1 = integer(), Row2 = integer())
  
  # Iterate over each row in df1
  for (i in 1:nrow(df1)) {
    point1 = df1[i, c("Location_Center_X", "Location_Center_Y", "Location_Center_Z")]
    
    # Calculate distances to all points in df2
    distances = apply(df2[, c("Location_Center_X", "Location_Center_Y", "Location_Center_Z")], 1, function(row) euclidean_distance(point1, row))
    
    # Find the row number of the closest point in df2
    closest_row = which.min(distances)
    
    # Add the pairing to the result dataframe
    paired_points = paired_points %>% 
      add_row(Row1 = i, Row2 = closest_row)
  }
  
  return(paired_points)
}
# Function to calculate the lowest difference between two angles for Alignment
angle_diff = function(a, b) {
  # Because our data is bidirectional, we must check to see if the opposite azimuth is closer, since it is arbitrary. Remove the diff1/diff2 comparison if you are working with unidirectional data
  a = as.circular(a, unit = "degrees", type = "angles")
  b = as.circular(b, unit = "degrees", type = "angles")
  diff1 = abs(a - b) %% 360
  diff2 = abs(a - (b - 180)) %% 360
  diff3 = abs(b - (a - 180)) %% 360
  print(diff1,diff2,diff3)
  diff = as.circular(min(diff1, diff2, diff3), unit = "degrees", type="angles")
  print(di)
  ifelse(diff > 180, 360 - diff, diff)
}
#####
# Load in both dataframes
fullpath = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filepath = paste(fullpath,"Data", sep="/")
all_samples = list.files(path=filepath, pattern = "*.csv", full.names=TRUE)
# Change names of files to load that contain coordinates.
points1 = read.csv(paste(filepath,"CellGeom_SpindlePole.csv", sep="/"))
points2 = read.csv(paste(filepath,"CellGeom_CellShape.csv", sep="/"))

# Convert z-slices to z coordinates
zscale_factor = 5 #ratio of z-slice thickness to xy pixel size

for (i in 1:nrow(points1)) {
  #Fetch the Z-stack position from the filename. NOTE: this only works for 3-digit slice numbers in the standard zeiss name format. Adjust indices below to change this.
  z_stack = as.numeric(str_sub(points1$FileName[i], -10, -8))
  # Convert into real coordinates and save
  z_coord = z_stack * zscale_factor
  points1$Location_Center_Z[i] = z_coord

}
# Repeat for second dataframe
for (i in 1:nrow(points2)) {
  #Fetch the Z-stack position from the filename
  z_stack = as.numeric(str_sub(points2$FileName[i], -10, -8))
  # Convert into real coordinates and save
  z_coord = z_stack * zscale_factor
  points2$Location_Center_Z[i] = z_coord
  
}

# Find closest pairs by XYZ coords, indexed by row number
paired_rows = pair_closest_points(points1, points2)

                               # Prep both individual dataframes for pairing
# Assign name prefixes to every column in each for later identification
ID_points1 = "SpindlePole"
ID_points2 = "NucShape"
names(points1) = paste0(ID_points1, "_", names(points1))
names(points2) = paste0(ID_points2, "_", names(points2))

# Initialize new dataframe for both sets of columns
merged_columns = union(names(points1), names(points2))

paired_df = data.frame(matrix(ncol = length(merged_columns), nrow = length(paired_rows)))
colnames(paired_df) = merged_columns

# Merge data by pulling lines by index into the paired dataframe and creating new rows
for (i in 1:nrow(paired_rows)) {
  merged_line = c(points1[as.numeric(paired_rows[i,1]),], points2[as.numeric(paired_rows[i,2]),])
  paired_df[i,] = merged_line
}
# Convert circular data to the correct data type
paired_df$SpindlePole_AreaShape_Orientation = as.circular(paired_df$SpindlePole_AreaShape_Orientation, units = "degrees")
paired_df$NucShape_AreaShape_Orientation = as.circular(paired_df$NucShape_AreaShape_Orientation, units = "degrees")
# TODO: MAJOR PROBLEM: Currently, only 'upper' component of angle is being considered. Angle doubling?

# TODO: Add two new variables- SliceMean and Alignment- to each row
# Trying to practice dplyr for this. First add SliceMean as the circular mean of all angles that share that
# Because our data is diametrically bidirectional, we apply angle doubling
paired_df = paired_df %>%
  group_by(SpindlePole_Location_Center_Z) %>%
  mutate(SliceMean = (mean.circular(SpindlePole_AreaShape_Orientation*2)/2)) %>%
  ungroup()
# Alignment: The absolute difference between the SliceMean and each individual angles
paired_df = paired_df %>%
  rowwise() %>%
  mutate(Alignment = angle_diff(a=SliceMean, b=SpindlePole_AreaShape_Orientation))


 # Save the merged df for later or load it back in
write.csv(paired_df, paste(filepath,"paired_geometry_orientation.csv", sep="/"))

#####################################
# If you've already generated a paired DF, you can load it in and start here
paired_df = read.csv(paste(filepath,"paired_geometry_orientation.csv", sep="/"))




# Data cleaning: All pairs should be within the same Z-plane
cleaned_paired_df = paired_df[paired_df$SpindlePole_Location_Center_Z == paired_df$NucShape_Location_Center_Z,]
# Cull all rows for which the distance between a cell and its paired vector is unreasonable (See euclidean distances below)
# This threshold comes from the 95 percentile of a manually annotated sample
distance_threshold = 25
# Convert circular data to the correct data type
cleaned_paired_df$SpindlePole_AreaShape_Orientation = as.circular(cleaned_paired_df$SpindlePole_AreaShape_Orientation, units = "degrees")
cleaned_paired_df$NucShape_AreaShape_Orientation = as.circular(cleaned_paired_df$NucShape_AreaShape_Orientation, units = "degrees")






# Sanity check: plot euclidean distances between paired centers
e_distances = vector(mode="numeric", length = nrow(cleaned_paired_df))

for (i in 1:nrow(paired_df)) {
  e_distances[i] = euclidean_distance(paired_df[i, c("SpindlePole_Location_Center_X", "SpindlePole_Location_Center_Y", "SpindlePole_Location_Center_Z")],paired_df[i, c("NucShape_Location_Center_X", "NucShape_Location_Center_Y", "NucShape_Location_Center_Z")])
}
close_distances = which(e_distances <= distance_threshold)
#Subset pairs to those less than a reasonable distance apart
cleaned_paired_df = cleaned_paired_df[close_distances,]
  # sd(e_distances)
  # mean(e_distances)
  # summary(e_distances)
  # e_df = as.data.frame(e_distances)
  # ggplot(e_df, aes(x=e_distances)) + geom_histogram()
  # quantile(e_distances, 0.95)
  # boxplot(e_distances)

###### The next set of analyses are modular- run as needed ######

###### Coordination of orientation (scored by slice)
  # Initialize empty dataframe for variance
  var_by_slice = data.frame(matrix(ncol = 3, nrow = 0))
  colnames(var_by_slice) = c("Z-Coord", "Variance", "Orientation")
  #Populate the dataframe with variance of orientation by slice
  #TODO: Fix this to use circular methods and angle doubling. This is junk right now.
  for (z in unique(cleaned_paired_df$SpindlePole_Location_Center_Z)) {
    current_z = cleaned_paired_df[cleaned_paired_df$SpindlePole_Location_Center_Z == z, ]
    angles_z = circular(current_z$SpindlePole_AreaShape_Orientation,type = "angles", units = "degrees")
    var_z = var(angles_z)
    row_vec = c(z,var_z, mean(angles_z))
    var_by_slice = rbind(var_by_slice, row_vec)
  }
  colnames(var_by_slice) = c("ZCoord", "Variance", "Orientation")
    # Plot variance by Z position
  ggplot(var_by_slice, aes(x=ZCoord, y =Orientation)) + geom_point() + geom_errorbar(aes(ymin = Orientation-Variance, ymax = Orientation + Variance))

###### What features maximally covary with orientation?
  # Initialize df to record correlations
  feature_correlations = data.frame(variable = character(), correlation = numeric(), p_value = numeric(), stringsAsFactors = FALSE)
  # Circular-linear correlations
  for (var in names(cleaned_paired_df)) {
    clctest = circlin.cor(cleaned_paired_df$SpindlePole_AreaShape_Orientation, cleaned_paired_df[[var]], rads = FALSE)
    
    feature_correlations = rbind(feature_correlations, data.frame(variable = var, correlation = clctest[1], p_value = clctest[2]))
    
  }
  
  # Alternate approach- subset variables first. circlin.cor is already multivariate.
  feature_subset = cleaned_paired_df %>% select(NucShape_AreaShape_Area, NucShape_AreaShape_Eccentricity, NucShape_AreaShape_MajorAxisLength, NucShape_AreaShape_MinorAxisLength, NucShape_AreaShape_MeanRadius, NucShape_AreaShape_Perimeter)
  #NOTE: This analysis must be done on ALIGNMENT- paramaterized as the angular distance between each cells orientation and the mean.
###### Orientation of nuclei vs division
cor(cleaned_paired_df$SpindlePole_AreaShape_Orientation, cleaned_paired_df$NucShape_AreaShape_Orientation)

# testing
circtest = as.circular(c(359, 1), type = "angles", units = "degrees")
circtest = circtest*2
mean.circular(circtest)
angle_diff = circular(350, units = "degrees") - circular(10, units = "degrees")
angle_diff = conversion.circular(angle_diff, modulo = "asis", units = "degrees")


