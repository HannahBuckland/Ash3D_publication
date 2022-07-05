# Functions to extract process ASCII files and "unwrap" them into long format
# Author: Hannah Buckland
# Date: 15/06/2022

# Written for the manuscript "Modelling the transport and deposition of ash 
# following a Magnitude 7 eruption: the distal Mazama tephra" 
# Submitted to Bull. Volc


# Read in required packages

library(tidyverse)
library(here)
library(ggmap)
library(sf)
library(rgdal)
library(rgeos)
library(patchwork)
library(broom)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(Cairo)
library(viridis)
library(data.table)
library(ggnewscale)


# Set of three functions that get grid info from ASCII files

lat_info <- function(filename){
  
  ascii_grid <- read.table(filename,
                           nrows = 4)
  
  ascii_cellsize <- read.table(filename,
                               skip = 4,
                               nrows = 1)
  # Get grid coordinates
  lat_min <- ascii_grid[4,]$V2
  
  lat_n <- ascii_grid[2,]$V2
  
  cell_size <- ascii_cellsize$V3
  
  lat_coord <- seq(from = (lat_min +(cell_size/2)),
                   to = (lat_min-(cell_size/2)) + (cell_size*lat_n),
                   by =  cell_size)
  
  lat_coord <- rev(lat_coord) # reverse latitude (start from top down)
  
  return(lat_coord)
}

long_info <- function(filename){
  
  ascii_grid <- read.table(filename,
                           nrows = 4)
  
  ascii_cellsize <- read.table(filename,
                               skip = 4,
                               nrows = 1)
  # Get grid coordinates
  long_min <- (ascii_grid[3,]$V2-360)
  
  long_n <- ascii_grid[1,]$V2
  
  cell_size <- ascii_cellsize$V3
  
  long_coord <- seq(from = (long_min +(cell_size/2)),
                    to = (long_min-(cell_size/2)) + (cell_size*long_n),
                    by =  cell_size)
  
  return(long_coord)
}

esrii_numb_info <- function(filename){
  
  # Read in ascii data but skipping grid information
  
  esrii_ascii <- read.table(filename,
                            skip = 6)
  
  ascii_rows <- nrow(esrii_ascii)
  
  esrii_numb <- seq(from=1,
                    to=ascii_rows,
                    by=9)
  
  return(esrii_numb)
}

# Function to read in the ascii data
read_ascii <- function(filename){
  
  esrii_ascii <- read.table(filename,
                            skip = 6)
  
  return(esrii_ascii)
  
}

# For each item of the ascii_files list you need to do the same thing with 
# another function:

# Function to 'unwrap' each ascii file
ascii_process <- function(esrii_numb,esrii_data) {
  
  esrii_data <- esrii_data
  
  # select the ascii file in chunks of 9
  esrii_col <- esrii_data[esrii_numb:(esrii_numb+8),]
  
  # give each chunk an id based on the longitude
  esrii_col <- esrii_col %>%
    mutate(id = paste0("long", seq(from = esrii_numb,
                                   to = esrii_numb,
                                   by = 1)))
  
  # pivot the chunk into a long format
  esrii_col_long <- pivot_longer(esrii_col,
                                 cols = -c("id"))
  
  # assign each row a longitude coordinate
  esrii_col_long <- esrii_col_long %>%
    mutate(long_val = as.numeric(long_coord))
  
  # return the unwrapped chunk
  return(esrii_col_long)
  
}

# Function that applies the unwrap function to each chunk of ascii data in each 
# ascii file
apply_ascii_process <- function(esrii_data) {
  
  # from the input get the necessary variables from the list
  esrii_numb <- esrii_numb
  lat_coord <- lat_coord
  long_coord <- long_coord
  
  
  # apply the ascii_process function
  esrii_cols_out <- lapply(esrii_numb,esrii_data,
                           FUN = ascii_process)
  
  names(esrii_cols_out) <- lat_coord
  
  esrii_rbind <- data.table::rbindlist(esrii_cols_out,
                                       idcol = TRUE)
  
  esrii_rbind <- as.data.frame(esrii_rbind)
  esrii_tidy <- esrii_rbind %>%
    mutate(lat_val = as.numeric(.id),
           thick_mm = value) %>%
    dplyr::select(lat_val,long_val,thick_mm)
  
  return(esrii_tidy)
  
}
