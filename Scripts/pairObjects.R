
library(EBImage)
library(ggplot2)
library(plyr)
library(readr)


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

# Load in data
fullpath <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
nucpath <- paste(fullpath,"Data/CellGeom", sep="/")
spindlepath <- paste(fullpath,"Data/Spindles", sep="/")
nuc_slices <- list.files(path=nucpath, pattern = "*.tiff", full.names=TRUE)
spindle_slices <- list.files(path=spindlepath, pattern = "*.tiff", full.names=TRUE)

#Note: Everything from here will be in a loop iterating through each slice, for speed
# Generate labelmaps for spindles and nuclei
spindle_labeled = bwlabel(spindle_image)
nuc_labeled = bwlabel(nuc_image)

# Pair nuclei with cells that most closely match
matched_spindles <- matching_label_pairs(spindle_labeled, nuc_labeled, min_pixels = 5)
# use the pair list to join the dataframes
### This requires doing this on the DFs rather than the images. Will contact Lucas after the break.
# Export joined dataframes for use in other scripts




