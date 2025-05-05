### First, libraries
library(circular)

# df assistance
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

# needed for masking out unwanted regions
library(RImageJROI)
library(sf)
library(sfnetworks)
library(lwgeom)
library(Morpho)
library(smoothr)

# needed for 'globe' plots
library(terra)

# graphing
library(ggplot2)
library(RColorBrewer)
library(Hmisc)
library(svglite)
library(magick)

##Starting with pasting in relevant functions from Nicks work (https://github.com/njhanne/FGFR_Downstream_Branches/tree/main/Golgi_orientation)
# Most of these still have to be adapted
get_positional_angle <- function(df_temp, octile_zips) {
  sample_names <- unique(df_temp$old_filename_generic_noside)
  if (is.null(sample_names)) {
    sample_names <- unique(df_temp$old_filename_generic_noside.x)
  }
  for (image in 1:length(sample_names)) {
    octile_zip <- octile_zips %>% filter(str_starts(octile_zips[,1], sample_names[image]))
    if (length(octile_zip[[1]] != 0)) {
      octile_rois <- read.ijzip(file.path("./imagej_rois/overview_octiles/", octile_zip[[1]]), verbose = FALSE)
      octile_linestrings <- st_sfc(lapply(octile_rois, function(x) st_linestring(x$coords, dim="XY")))
      rows <- which(df_temp$old_filename_generic_noside == sample_names[image] & !is.na(df_temp$nuclei_centroidx_overview))
      if (length(rows) == 0) {
        rows <- which(df_temp$old_filename_generic_noside.x == sample_names[image] & !is.na(df_temp$nuclei_centroidx_overview))
      }
      xmax <- attributes(octile_linestrings)$bbox[['xmax']]
      positional_angles <- data.frame(matrix(ncol=3, nrow=length(rows)))
      print(octile_zip[[1]])
      for (nuc_pair in 1:length(rows)) {
        row <- df_temp[rows[nuc_pair],]
        # print(row)
        extended_line <- extend_line(row, xmax)
        # intersects gives list of number of intersections b/w extended line and each octile, 
        # the which gives the index for the octile that contains intersection
        intersected_segments <- st_intersects(octile_linestrings, extended_line)
        intersected_segment <- which(lapply(intersected_segments, function(x) unlist(x)) != 0)
        if (length(intersected_segment != 0)) {
          intersection_stats <- distance_along_octile(octile_linestrings, extended_line, intersected_segment[1])
          positional_angles[nuc_pair, 1] <- intersection_stats[1][[1]]
          positional_angles[nuc_pair, 2] <- intersection_stats[2][[1]][1]
          positional_angles[nuc_pair, 3] <- intersection_stats[2][[1]][2]
          # print(positional_angle*180/pi)
          # p <- ggplot() + geom_sf(data = octile_linestrings, color = 'black') +
          #   geom_sf(data = octile_linestrings[intersected_segment[1]], color = 'blue') +
          #   geom_point(x = row$nuclei_centroidx_overview, y = row$nuclei_centroidy_overview, aes(color = 'red')) +
          #   geom_sf(data = st_intersection(octile_linestrings, extended_line), color='red') +
          #   geom_sf(data = extended_line)
          # print('pause')
        }
        else {
          print(nuc_pair)
          # p <- ggplot() + geom_sf(data = octile_linestrings, color = 'black') + geom_sf(data = extended_line)
        }
      }
      df_temp[rows,]$positional_angle <- positional_angles[,1]
      df_temp[rows,]$intersectionx <- positional_angles[,2]
      df_temp[rows,]$intersectiony <- positional_angles[,3]
    }
  }
  return(df_temp)
}


extend_line <- function(df_row, xmax) {
  extended_x <- df_row$nuclei_centroidx_overview + df_row$unit_x * 4*xmax 
  extended_y <- df_row$nuclei_centroidy_overview + df_row$unit_y * 4*xmax
  extended_line <- st_linestring(matrix(c(df_row$nuclei_centroidx_overview, df_row$nuclei_centroidy_overview, extended_x, extended_y), 2, 2, byrow=TRUE), dim="XY")
  return(extended_line)
}


distance_along_octile <- function(linestrings, extended_line, linestring_i) {
  # https://stackoverflow.com/a/77688302
  # I'm sure there are other ways of doing this but the sfnetworks library looks 
  # like it will be much simpler!
  
  # putting the first element here in case it hits two octiles. This could maybe happen
  # near boundaries, and they should be close enough together that the angle
  # will be nearly the same anyway.
  intersected_point <- st_intersection(linestrings[linestring_i], extended_line)
  intersected_point <- st_cast(intersected_point, 'POINT')[1]
  octile_network <- as_sfnetwork(linestrings[linestring_i][1])
  subnet <- st_network_blend(octile_network, intersected_point)
  length_table <- subnet %>% activate("edges") %>% st_as_sf() %>% mutate(Length = st_length(x))
  ratio <- pi/4 - (length_table$Length[2] / (length_table$Length[1]+length_table$Length[2]) * pi/4)
  ratio <- pi + pi/4*(linestring_i-1) + ratio
  return(c(ratio,intersected_point))
}


positional_angle_to_xy <- function(linestrings, pos_angle) {
  # this is basically the inverse of the 'distance along octile' function
  # we want to get the xy position in a linestring from the angle
  # https://stackoverflow.com/a/77688302
  
  # would this be better as just a lookup table? IDK
  # check 'approx()' function. This code is slow 
  # aight I changed it and it's literally 1000x faster lol
  
  # this is a bit obfuscated - the positional angle plus pi rotates it cw so '0' is 180
  # but now all the 180-360 will be 360-540, so we modulo with a full circle 
  # so that they will be 0-180 instead. Divide by pi/4 (1/8 of circle) to get the octile
  # and floor it so it's an integer not a float, then add 1 since it is 1-8 not 0-7
  
  linestring_i <- floor(((pos_angle+pi) %% (2*pi)) / (pi/4)) + 1
  
  ratio <- (pos_angle %% (pi/4)) / (pi/4)
  (pt <- st_linesubstring(linestrings[linestring_i], from = 0, to = ratio) %>% st_endpoint())
  return(pt)
}

#Will need everything down to this for sure
generate_overview_positional_LUT <- function(overview_octile_rois, slices = 720) {
  overview_rois <- read.ijzip(file.path("./imagej_rois/overview_octiles/", overview_octile_rois), verbose = FALSE)
  linestrings <- st_sfc(lapply(overview_rois, function(x) st_linestring(x$coords, dim="XY")))
  
  # https://stackoverflow.com/a/72533271
  # https://stackoverflow.com/a/72267454
  bbox <- st_bbox(linestrings) %>% st_as_sfc()
  
  polygon <- bbox %>% lwgeom::st_split(linestrings) %>% st_collection_extract("POLYGON")
  poly_lms <- polygon[2] # hopefully this isn't random!
  smooth_poly <- smoothr::smooth(poly_lms, method = "ksmooth", smoothness = 20)
  smooth_linestring <- st_cast(smooth_poly, 'LINESTRING')
  endpts <- st_line_sample(linestrings, sample=0)
  st_nearest_points(endpts, smooth_linestring) %>% {. ->> connecting_linestrings}
  new_endpts <- st_line_sample(connecting_linestrings, sample = 1)
  net <- as_sfnetwork(smooth_linestring)
  net <- st_network_blend(net, st_cast(new_endpts, 'POINT'))
  net <- convert(net, to_spatial_smooth, protect = new_endpts)
  smooth_linestrings <- net %>% activate(edges) %>% st_as_sf()
  smooth_linestrings <- st_sfc(smooth_linestrings[[4]])
  
  
  LUT <- setNames(data.frame(matrix(ncol = 3, nrow = slices)), c('pos_angle', 'overview_x', 'overview_y'))
  LUT$pos_angle <- seq(0, 2*pi, length.out = slices)
  
  for (slice in 1:slices) {
    pt_temp <- positional_angle_to_xy(smooth_linestrings, LUT[slice,]$pos_angle)
    LUT[slice,]$overview_x <- pt_temp[[1]][1]
    LUT[slice,]$overview_y <- pt_temp[[1]][2]
  }
  return(LUT)
}


convert_directional_angle_overview_LUT <- function(df_temp, overview_pos_LUT) {
  df_temp$overview_intersectionx <- NA
  df_temp$overview_intersectiony <- NA
  
  df_temp$overview_intersectionx <- approx(overview_pos_LUT$pos_angle, overview_pos_LUT$overview_x, xout = df_temp$positional_angle)$y
  df_temp$overview_intersectiony <- approx(overview_pos_LUT$pos_angle, overview_pos_LUT$overview_y, xout = df_temp$positional_angle)$y
  return(df_temp)
} 


get_positional_angle_from_intersection <- function(df_temp, octile_zip, extended_linestrings) {
  octile_rois <- read.ijzip(file.path("./imagej_rois/overview_octiles/", octile_zip[[1]]), verbose = FALSE)
  octile_linestrings <- st_sfc(lapply(octile_rois, function(x) st_linestring(x$coords, dim="XY")))
  intersection_stats_df <- data.frame(nrow=nrow(df_temp), ncol = 3)
  for (row in 1:nrow(df_temp)) {
    intersected_segments <- st_intersects(octile_linestrings, extended_linestrings[row])
    intersected_segment <- which(lapply(intersected_segments, function(x) unlist(x)) != 0)
    if (length(intersected_segment != 0)) {
      intersection_stat <- distance_along_octile(octile_linestrings, extended_linestrings[row], intersected_segment[1])
      intersection_stats_df[row,1] <- intersection_stat[1][[1]]
      intersection_stats_df[row,2] <- intersection_stat[2][[1]][1]
      intersection_stats_df[row,3] <- intersection_stat[2][[1]][2]
      # print(positional_angle*180/pi)
      # p <- ggplot() + geom_sf(data = octile_linestrings, color = 'black') +
      #   geom_sf(data = octile_linestrings[intersected_segment[1]], color = 'blue') +
      #   geom_point(x = row$nuclei_centroidx_overview, y = row$nuclei_centroidy_overview, aes(color = 'red')) +
      #   geom_sf(data = st_intersection(octile_linestrings, extended_line), color='red') +
      #   geom_sf(data = extended_line)
      # print('pause')
    }
  }
  return(intersection_stats_df)
}