# Load necessary library
library(dplyr)
library(stringr)
library(ggplot2)

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

# Save the merged df for later or load it back in
write.csv(paste(filepath,"paired_geometry_orientation.csv", sep="/"))

#####################################
# If you've already generated a paired DF, you can load it in and start here
paired_df = read.csv(paste(filepath,"paired_geometry_orientation.csv", sep="/"))

# Data cleaning: All pairs should be within the same Z-plane
cleaned_paired_df = paired_df[paired_df$SpindlePole_Location_Center_Z == paired_df$NucShape_Location_Center_Z,]



# Sanity check: plot euclidean distances between paired centers
e_distances = vector(mode="numeric", length = nrow(cleaned_paired_df))

for (i in 1:nrow(paired_df)) {
  e_distances[i] = euclidean_distance(paired_df[i, c("SpindlePole_Location_Center_X", "SpindlePole_Location_Center_Y", "SpindlePole_Location_Center_Z")],paired_df[i, c("NucShape_Location_Center_X", "NucShape_Location_Center_Y", "NucShape_Location_Center_Z")])
}
sd(e_distances)
mean(e_distances)
summary(e_distances)
e_df = as.data.frame(e_distances)
ggplot(e_df, aes(x=e_distances)) + geom_histogram()
quantile(e_distances, 0.95)
boxplot(e_distances)
which       