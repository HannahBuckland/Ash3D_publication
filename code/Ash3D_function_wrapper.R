# Script that acts as a wrapper for functions that produce plots for 
# Ash3D publication

# Author: Hannah Buckland
# Date: 15/06/2022

# Written for the manuscript "Modelling the transport and deposition of ash 
# following a Magnitude 7 eruption: the distal Mazama tephra" 
# Submitted to Bull. Volc

# source the user specific functions
source(here("code","ascii_process_functions.R"))
source(here("code","isopach_map_function.R"))
source(here("code","pyle_plots_function.R"))
source(here("code","thickness_process_function.R"))

# Read in all deposit files with full file names
depositfiles <- list.files(here("DepositFiles",
                                "ESRI_ASCII"), 
                           pattern = "Run",
                           full.names = TRUE)

# Read in all contour files with full file names
contoursfiles <- list.files(here("contour_polygons"), 
                            pattern = "MZRun",
                            full.names = TRUE)

# Read in all thicknesses files with full file names

thicknessfiles <- list.files(here("sample_thickness"), 
                             pattern = "_tidy",
                             full.names = TRUE)

# Extract string for naming list entries
runname <- str_extract(depositfiles,
                       "(?<=ASCII/MZ).*(?=.txt)")
thickname <- str_extract(thicknessfiles,
                         "(?<=ness/MZ).*(?=_tidy)")


# Get grid information and coordinates from ascii file
lat_check <- lapply(depositfiles,
                    FUN=lat_info)

long_check <- lapply(depositfiles,
                     FUN=long_info)

esrii_numb_check <- lapply(depositfiles,
                           FUN=esrii_numb_info)

# get unique coordinates that should be the same for each ascii file
## NOTE in future I should add functionality to flag if there are ascii files 
## with different dimensions
lat_coord <- unique(unlist(lat_check))
long_coord <- unique(unlist(long_check))
esrii_numb <- unique(unlist(esrii_numb_check))

# Apply read_ascii function to all Ash3D runs listed in depositfiles
ascii_files <- lapply(depositfiles,
                      FUN=read_ascii)

names(ascii_files) <- runname # give them names using the runname variable

# Apply the unwrapping function to each ascii file in deposit files

ascii_unwrapped <- lapply(ascii_files,
                          FUN = apply_ascii_process)

names(ascii_unwrapped) <- runname

# Apply plotting code to get mapped contour plots for each run
contour_plots <- lapply(ascii_unwrapped,
                        FUN = ascii_plot)
names(contour_plots) <- runname # give them names

# Apply Pyle process function to contour files for each run
pyle_out <- lapply(contoursfiles,
                   FUN=pyle_data_process)
names(pyle_out) <- runname

# Apply thickness process function to modified thickness files for GSD runs
thickness_out <- lapply(thicknessfiles, 
                        FUN=thickness_data_process)
names(thickness_out) <- thickname


