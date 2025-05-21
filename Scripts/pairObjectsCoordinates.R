# Load necessary library
library(dplyr)
library(stringr)
library(ggplot2)
library(circular)
library(bpnreg)
library(Directional)
library(Morpho)
library(car)
library(biotools)
library(ggridges)

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
  # Because our data is bidirectional, we must check to see if the opposite azimuth is closer. Remove the diff1/diff2/diff3 comparison if you are working with unidirectional data
  a = as.circular(a, units = "degrees", type = "angles")
  b = as.circular(b, units = "degrees", type = "angles") 
  diff1 = abs(a - b) %% 360
  diff2 = abs(a - (b - 180)) %% 360
  diff3 = abs(b - (a - 180)) %% 360
  diff = as.circular(min(diff1, diff2, diff3), units = "degrees", type="angles")
  ifelse(diff > 180, 360 - diff, diff)
}
#####
# Load in both dataframes
fullpath = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
filepath = paste(fullpath,"Data", sep="/")
# Change names of files to load that contain coordinates.
points1 = read.csv(paste(filepath,"Orientation_SpindlePole_A5.csv", sep="/"))
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
paired_df$SpindlePole_AreaShape_Orientation = as.circular(paired_df$SpindlePole_AreaShape_Orientation, units = "degrees", type = "angles")
paired_df$NucShape_AreaShape_Orientation = as.circular(paired_df$NucShape_AreaShape_Orientation, units = "degrees", type = "angles")

# Add two new variables- SliceMean and Alignment- to each row
# Trying to practice dplyr for this. First add SliceMean as the circular mean of all angles that share that

# Because our data is diametrically bidirectional, we apply angle doubling

#NOTE: The code for this first pass works on a slicewise mean. We've since moved to larger bins
# paired_df = paired_df %>%
#   group_by(SpindlePole_Location_Center_Z) %>%
#   mutate(SliceMean = (mean.circular(SpindlePole_AreaShape_Orientation*2)/2)) %>%
#   ungroup()

# Alignment: The absolute difference between the SliceMean and each individual angles
#paired_df = paired_df %>%
# rowwise() %>%
# mutate(Alignment = angle_diff(a=SliceMean, b=SpindlePole_AreaShape_Orientation))

# Define number of bins and assign each row a bin ID
bin_count = 6
paired_df = paired_df %>%
  mutate(avg_bin = ntile(row_number(paired_df), bin_count)) %>%
  group_by(avg_bin) %>%
  mutate(SliceMean = (mean.circular(SpindlePole_AreaShape_Orientation*2)/2)) %>%
  ungroup()

# Alignment: The absolute difference between the Bin Mean (SliceMean) and each individual angles
 paired_df = paired_df %>%
   rowwise() %>%
   mutate(Alignment = angle_diff(a=SliceMean, b=SpindlePole_AreaShape_Orientation))

is.circular(paired_df$SpindlePole_AreaShape_Orientation)
is.circular(paired_df$SliceMean)
# For easy access- spindlepole orientation as circular
paired_df = paired_df %>%
  mutate(SpindleAngle = SpindlePole_AreaShape_Orientation)


 # Save the merged df for later or load it back in
write.csv(paired_df, paste(filepath,"paired_geometry_orientation_12bin.csv", sep="/"))




#####################################
# If you've already generated a paired DF, you can load it in and start here
paired_df = read.csv(paste(filepath,"paired_geometry_orientation.csv", sep="/"))
# Convert circular data to the correct data type
paired_df$SpindlePole_AreaShape_Orientation = as.circular(paired_df$SpindlePole_AreaShape_Orientation, units = "degrees", type = "angles")
paired_df$NucShape_AreaShape_Orientation = as.circular(paired_df$NucShape_AreaShape_Orientation, units = "degrees", type = "angles")
paired_df$SpindleAngle = as.circular(paired_df$SpindleAngle, units = "degrees", type = "angles")
paired_df$SliceMean = as.circular(paired_df$SliceMean, units = "degrees", type = "angles")
# Copy the original Df for reference
raw_data = paired_df



# Data cleaning: All pairs should be within the same Z-plane
paired_df = paired_df[paired_df$SpindlePole_Location_Center_Z == paired_df$NucShape_Location_Center_Z,]
# Cull all rows for which the distance between a cell and its paired vector is unreasonable (See euclidean distances below)
# This threshold comes from the 95 percentile of a manually annotated sample
distance_threshold = 25
# Convert circular data to the correct data type
paired_df$SpindlePole_AreaShape_Orientation = as.circular(paired_df$SpindlePole_AreaShape_Orientation, units = "degrees", type = "angles")
paired_df$NucShape_AreaShape_Orientation = as.circular(paired_df$NucShape_AreaShape_Orientation, units = "degrees", type = "angles")
# Plot euclidean distances between paired centers
e_distances = vector(mode="numeric", length = nrow(paired_df))

for (i in 1:nrow(paired_df)) {
  e_distances[i] = euclidean_distance(paired_df[i, c("SpindlePole_Location_Center_X", "SpindlePole_Location_Center_Y", "SpindlePole_Location_Center_Z")],paired_df[i, c("NucShape_Location_Center_X", "NucShape_Location_Center_Y", "NucShape_Location_Center_Z")])
}
close_distances = which(e_distances <= distance_threshold)
#Subset pairs to those less than a reasonable distance apart
paired_df = paired_df[close_distances,]
  # e_df = as.data.frame(e_distances)
  # ggplot(e_df, aes(x=e_distances)) + geom_histogram()
  # quantile(e_distances, 0.95)
  # boxplot(e_distances)

###### The next set of analyses are modular- run as needed ######

###### Coordination of orientation (scored by slice)
#TODO: Change this section to use circular variance
  # Initialize empty dataframe for variance
  var_by_slice = data.frame(matrix(ncol = 3, nrow = 0))
  colnames(var_by_slice) = c("Z-Coord", "Variance", "Orientation")
  #Populate the dataframe with variance of orientation by slice
  #TODO: Fix this to use circular methods and angle doubling. This is junk right now.
  for (z in unique(paired_df$SpindlePole_Location_Center_Z)) {
    current_z = paired_df[paired_df$SpindlePole_Location_Center_Z == z, ]
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
  feature_subset = paired_df %>% select(Alignment, NucShape_AreaShape_Area, NucShape_AreaShape_Eccentricity, NucShape_AreaShape_MajorAxisLength, NucShape_AreaShape_MinorAxisLength, NucShape_AreaShape_MeanRadius, NucShape_AreaShape_Perimeter)
  # Linear correlations with significance testing per variable
  for (var in names(feature_subset)) {
    cortest = cor.test(feature_subset$Alignment, feature_subset[[var]], method = "spearman")
    
    feature_correlations = rbind(feature_correlations, data.frame(variable = var, correlation = cortest$estimate, p_value = cortest$p.value))
    
  }
  
  feature_subset = paired_df %>% select(Alignment, NucShape_AreaShape_Area, NucShape_AreaShape_Eccentricity, NucShape_AreaShape_MajorAxisLength, NucShape_AreaShape_MinorAxisLength, NucShape_AreaShape_MeanRadius, NucShape_AreaShape_Perimeter)
  # Additional Feature: AspectRatio, defined as the ratio between the major and minor axes
  feature_subset = feature_subset %>%
    rowwise() %>%
    mutate(AspectRatio = (NucShape_AreaShape_MajorAxisLength / NucShape_AreaShape_MinorAxisLength))
  
correlations2 = feature_correlations



##### PCA of alignment in feature space
# Cut out redundant features
features_pca = feature_subset %>% select(Alignment, NucShape_AreaShape_Area, NucShape_AreaShape_Eccentricity, NucShape_AreaShape_MeanRadius, NucShape_AreaShape_Perimeter, AspectRatio)
features_pca = feature_subset %>% select(Alignment, NucShape_AreaShape_Area, NucShape_AreaShape_Eccentricity, NucShape_AreaShape_Perimeter, AspectRatio)
# use the correlation matrix rather than the covariance matrix, since the scales are different
nuc_pca = prcomp(features_pca[,-1], retx = TRUE, scale. = TRUE)
# Eigenvectors
evecs = nuc_pca$rotation
# Eigenvalues
evals = nuc_pca$sdev^2
prop.evals <- round(evals/sum(evals),5)*100
# PC Scores
scores = nuc_pca$x
dim(scores)
length(prop.evals)

pc_df = cbind(features_pca, scores)

# PC plots
ggplot(pc_df, aes(x=PC1, y = PC2, color = Alignment)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "yellow", high = "red") +
  theme_minimal() + 
  labs(color = "Alignment", x = "PC1", y = "PC2", title = "PCA Plot of Nuclear Geometry")

ggplot(pc_df, aes(x=PC2, y = PC3, color = Alignment)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "yellow", high = "red") +
  theme_minimal() + 
  labs(color = "Alignment", x = "PC2", y = "PC3", title = "PCA Plot of Nuclear Geometry")

ggplot(pc_df, aes(x=PC3, y = PC4, color = Alignment)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "yellow", high = "red") +
  theme_minimal() + 
  labs(color = "Alignment", x = "PC3", y = "PC4", title = "PCA Plot of Nuclear Geometry")


#Optional: Plot by positional bin
pc_df$Group = as.character(paired_df$avg_bin)

colour_list = c("1" = "#3B99B1", "2" = "#5ba683", "3" = "#b2d162", "4" = "#E9B31F", "5" = "#E78100", "6" = "#F5191C")

ggplot(pc_df, aes(x=PC1, y = PC2, color = Group)) +
  geom_point(size = 3) +
  scale_color_manual(values = colour_list) +
  theme_minimal() + 
  labs(color = "ML Position", x = "PC1", y = "PC2", title = "PCA Plot of Nuclear Geometry")

ggplot(pc_df, aes(x=PC2, y = PC3, color = Group)) +
  geom_point(size = 3) +
  scale_color_manual(values = colour_list) +
  theme_minimal() + 
  labs(color = "ML Position", x = "PC2", y = "PC3", title = "PCA Plot of Nuclear Geometry")

ggplot(pc_df, aes(x=PC3, y = PC4, color = Group)) +
  geom_point(size = 3) +
  scale_color_manual(values = colour_list) +
  theme_minimal() + 
  labs(color = "ML Position", x = "PC3", y = "PC4", title = "PCA Plot of Nuclear Geometry")


# Looks like variance might be different- use Levene's test to evaluate. We'll try BoxM test after
pc_df$Group = as.factor(pc_df$Group)
leveneTest(PC4 ~ Group, data = pc_df)

#Variances are different between PCs 1 and 4 after Bonferroni Correction






###### Orientation of nuclei vs division
cor(paired_df$SpindlePole_AreaShape_Orientation, paired_df$NucShape_AreaShape_Orientation)
 
# testing
circtest = as.circular(c(359, 1), type = "angles", units = "degrees")
circtest = circtest*2
mean.circular(circtest)
angle_diff = circular(350, units = "degrees") - circular(10, units = "degrees")
angle_diff = conversion.circular(angle_diff, modulo = "asis", units = "degrees")


ggplot(paired_df, aes(x=Alignment)) + geom_histogram()

ctest = cor(feature_subset$Alignment, feature_subset$NucShape_AreaShape_Eccentricity)






######## Alignment across the mediolateral axis

# Add a binwise mean to the dataframe
paired_df = paired_df %>%
 group_by(avg_bin) %>%
 mutate(BinMeanAngle = (mean.circular(SpindlePole_AreaShape_Orientation*2)/2)) %>%
 ungroup()
# To make the graph a little more readable, slicewise mean alignment
paired_df = paired_df %>%
  group_by(SpindlePole_Location_Center_Z) %>%
  mutate(SliceMeanAlignment = (mean(Alignment))) %>%
  ungroup()
# We will still use individual alignment measures to create a ribbon

# Alignment vs Z position
paired_df$avg_bin = as.factor(paired_df$avg_bin)

#Boxplot
ggplot(paired_df, aes(x=avg_bin, y=Alignment)) +
  geom_boxplot(notch=TRUE)

# Joyplot
ggplot(paired_df, aes(x=Alignment, fill=avg_bin, y = avg_bin)) +
  geom_density_ridges(scale = 2,alpha = 0.8)+
  geom_segment(data = mean_alignments, aes(x = mean_alignment, xend = mean_alignment,
                                            y = as.numeric(avg_bin),
                                           yend = as.numeric(avg_bin) + 0.5,
                                           color = avg_bin),
               inherit.aes = FALSE) +
  labs(x = "Alignment", y = "Mediolateral Position") +
  theme_ridges()

mean_alignments <- paired_df %>%
  group_by(avg_bin) %>%
  summarize(mean_alignment = mean(Alignment))

ggplot(paired_df, aes(x=avg_bin, y = Alignment)) +
  geom_point(size = 3) +
  theme_minimal() + 
  labs(x = "Mediolateral Position", y = "Alignment", title = "Mitotic alignment across the left mandible")
 
lapply(split(paired_df, factor(paired_df$avg_bin)), function(x)t.test(paired_df$NucShape_AreaShape_Orientation, paired_df$SpindlePole_AreaShape_Orientation, paired=TRUE))
t.test(paired_df$NucShape_AreaShape_Orientation, paired_df$SpindlePole_AreaShape_Orientation)


# Alignment Plotting
hist(paired_df$Alignment)
