
library(EBImage)



matching_label_pairs <- function(matrix1, matrix2, min_pixels = 0) {
  # Ensure the input matrices are matrices
  matrix1 <- as.matrix(matrix1)
  matrix2 <- as.matrix(matrix2)
  
  # Get unique labels in matrix1
  unique_labels_matrix1 <- unique(as.vector(matrix1))
  
  # Initialize a list to store matching label pairs
  matching_pairs <- list()
  
  # Iterate over unique labels in matrix1
  for (label1 in unique_labels_matrix1) {
    # Find indices where the label appears in matrix1
    indices_matrix1 <- which(matrix1 == label1, arr.ind = TRUE)
    
    # Extract corresponding labels from matrix2
    corresponding_labels_matrix2 <- matrix2[indices_matrix1]
    
    # Iterate over unique labels in corresponding_labels_matrix2
    for (label2 in unique(corresponding_labels_matrix2)) {
      # Count number of pixels for the current label pair
      n_pixels <- sum(corresponding_labels_matrix2 == label2)
      
      # Check if n_pixels exceeds min_pixels threshold
      if (n_pixels > min_pixels) {
        matching_pairs <- append(matching_pairs, list(c(label1, label2)))
      }
    }
  }
  
  # Convert list of pairs to a matrix for clarity
  matching_pairs_matrix <- do.call(rbind, matching_pairs)
  
  # Filter pairs where label1 is not zero
  matching_pairs_non_zero_left <- matching_pairs_matrix[matching_pairs_matrix[, 1] != 0, , drop = FALSE]
  
  # Filter pairs where both labels are not zero
  matching_pairs_non_zero <- matching_pairs_non_zero_left[matching_pairs_non_zero_left[, 2] != 0, , drop = FALSE]
  
  # Return the results as a list
  return(list(
    matching_pairs = matching_pairs_matrix,
    matching_pairs_non_zero_left = matching_pairs_non_zero_left,
    matching_pairs_non_zero = matching_pairs_non_zero
  ))
}

# Generate labelmaps for spindles and nuclei
### TO DO: use 'bwlabel' from EBImage

# Pair nuclei with cells that most closely match

# use the pair list to join the dataframes

# Export joined dataframes for use in other scripts




