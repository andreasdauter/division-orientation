# Load necessary library
library(dplyr)
library(stringr)

# Function to calculate the Euclidean distance
euclidean_distance <- function(coord1, coord2) {
  sqrt(sum((coord1 - coord2)^2))
}

# Function to find the closest points and return paired row numbers
pair_closest_points <- function(df1, df2) {
  # Initialize a dataframe to store pairings
  paired_points <- data.frame(Row1 = integer(), Row2 = integer())
  
  # Iterate over each row in df1
  for (i in 1:nrow(df1)) {
    point1 <- df1[i, c("Location_Center_X", "Location_Center_Y", "Location_Center_Z")]
    
    # Calculate distances to all points in df2
    distances <- apply(df2[, c("Location_Center_X", "Location_Center_Y", "Location_Center_Z")], 1, function(row) euclidean_distance(point1, row))
    
    # Find the row number of the closest point in df2
    closest_row <- which.min(distances)
    
    # Add the pairing to the result dataframe
    paired_points <- paired_points %>% 
      add_row(Row1 = i, Row2 = closest_row)
  }
  
  return(paired_points)
}

#####
# Load in both dataframes
fullpath <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filepath <- paste(fullpath,"Data", sep="/")
all_samples <- list.files(path=filepath, pattern = "*.csv", full.names=TRUE)
# Change names of files to load that contain coordinates.
points1 = read.csv(paste(filepath,"CellGeom_SpindlePole.csv", sep="/"))
points2 = read.csv(paste(filepath,"CellGeom_CellShape.csv", sep="/"))

# Convert z-slices to z coordinates
zscale_factor = 5 #ratio of z-slice thickness to xy pixel size

for (i in 1:nrow(points1)) {
  #Fetch the Z-stack position from the filename
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

  

# Find closest pairs using row numbers
paired_rows <- pair_closest_points(df1, df2)

# Print the result
print(paired_rows)



