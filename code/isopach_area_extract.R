# Script to extract the area within the 1cm isopach for each run for the
# supplementary files
# Author: Hannah Buckland
# Date: 15/06/2022

# Written for the manuscript "Modelling the transport and deposition of ash 
# following a Magnitude 7 eruption: the distal Mazama tephra" 
# Submitted to Bull. Volc

# Source the pyle_plots function
source(here("code","pyle_plots_function.R"))

# Read in all contour files with full file names
contoursfiles <- list.files(here("contour_polygons"), 
                            pattern = "MZRun",
                            full.names = TRUE)

# Get runname for each file
runname <- str_extract(contoursfiles,
                       "(?<=polygons/MZ).*(?=.txt)")

# Apply Pyle process function to contour files for each run
pyle_out <- lapply(contoursfiles,FUN=pyle_data_process)
names(pyle_out) <- runname

# Extract the area within the 1 cm isopach for each run
onecm_isopachs <- sapply(pyle_out,function(x) x[2])
onecm_rbind <- data.frame(onecm = unlist(onecm_isopachs))
rownames(onecm_rbind) <- runname
colnames(onecm_rbind) <- "Area_1cm_isopach_(km2)"
write.csv(onecm_rbind,here("supp_data","1cm_area.csv"))
