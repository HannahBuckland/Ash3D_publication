# Function to read in thickness files in Ash3D fixed width format and tidy into
# easier to work with format
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


# List the thickness files making sure not to apply function to existing 
# "tidied" files
files <- grep(list.files(here("sample_thickness"),
                         full.names = TRUE), 
              pattern='tidy', 
              invert=TRUE, 
              value=TRUE)

clean_thickness <- function(thickness){
  
  thick_file <- thickness
  thick_name <- tools::file_path_sans_ext(thickness)
  
  # Read in awkward fixed width file format and excluding text file preamble rows
  thick_fw <- read.fwf(thick_file, 
                       widths = c(9,29,7,10,2,22,8,9,4,22,9,9,8,9,6,12,12,12,12,12,12,12,12,12,12,12), 
                       header = FALSE,
                       skip = 18)
  
  # Exclude the bottom rows that are not location rows 
  # (only include rows with B21 prefix)
  thick_fw <- thick_fw %>%
    filter(grepl("B21",V1))
  
  # Only interested in the B21 number and modelled thickness
  thick_select <- thick_fw %>%
    dplyr::select(V1,V13) %>%
    rename(B21_numb = V1,
           modelled_mm = V13) %>%
    mutate(B21_numb = gsub(" ","", B21_numb)) %>%
    distinct
  
  # Write the tidied data to new .txt files with suffix "tidy"
  write.table(thick_select,
              file=paste0(thick_name,"_tidy"),
              quote=FALSE,
              sep="\t",
              row.names=FALSE,
              col.names=TRUE)
}

# Apply the function to all the thickness files
fun_out <- lapply(files,FUN=clean_thickness)
