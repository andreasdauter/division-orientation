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
    point1 <- df1[i, c("x", "y", "z")]
    
    # Calculate distances to all points in df2
    distances <- apply(df2[, c("x", "y", "z")], 1, function(row) euclidean_distance(point1, row))
    
    # Find the row number of the closest point in df2
    closest_row <- which.min(distances)
    
    # Add the pairing to the result dataframe
    paired_points <- paired_points %>% 
      add_row(Row1 = i, Row2 = closest_row)
  }
  
  return(paired_points)
}

# Load in both dataframes


# Convert z-slice to z coordinate
zscale_factor = 5 #ratio of z-slice thickness to xy pixel size


# Find closest pairs using row numbers
paired_rows <- pair_closest_points(df1, df2)

# Print the result
print(paired_rows)


test_string = ("AurA_June7_A_3_frontface_z155c1.tiff")
z_stack = as.numeric(str_sub(test_string, -10, -8))
