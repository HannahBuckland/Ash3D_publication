# Script to extract fit parameters and thickness at specific sites for 
# supplementary files
# Author: Hannah Buckland
# Date: 15/06/2022

# Written for the manuscript "Modelling the transport and deposition of ash 
# following a Magnitude 7 eruption: the distal Mazama tephra" 
# Submitted to Bull. Volc

# Source the thickness_process function
source(here("code","thickness_process_function.R"))

# Read in all thicknesses files with full file names
thicknessfiles <- list.files(here("sample_thickness"), 
                             pattern = "_tidy",
                             full.names = TRUE)

# Extract the run name for applying to the function output
thickname <- str_extract(thicknessfiles,
                         "(?<=ness/MZ).*(?=_tidy)")

# Apply thickness process function to modified thickness files for GSD runs
thickness_out <- lapply(thicknessfiles, FUN=thickness_data_process)
names(thickness_out) <- thickname


# Extract the fit parameters for each run

RMSE_out <- sapply(thickness_out,function(x) x[4])
RMSE_rbind <- data.frame(RMSE=unlist(RMSE_out))
rownames(RMSE_rbind) <- thickname
R2_out <- sapply(thickness_out,function(x) x[5])
R2_rbind <- data.frame(R2=unlist(R2_out))
rownames(R2_rbind) <- thickname

# Extract the thickness at specific sites for each run

site42_out <- sapply(thickness_out,function(x) x[6])
site42_rbind <- rbindlist(site42_out)
rownames(site42_rbind) <- thickname
site73_out <- sapply(thickness_out,function(x) x[7])
site73_rbind <- rbindlist(site73_out)
rownames(site73_rbind) <- thickname
site177_out <- sapply(thickness_out,function(x) x[8])
site177_rbind <- rbindlist(site177_out)
rownames(site177_rbind) <- thickname
site291_out <- sapply(thickness_out,function(x) x[9])
site291_rbind <- rbindlist(site291_out)
rownames(site291_rbind) <- thickname

# Amalgamate the fit parameters and thicknesses into one data frame
params <- cbind(RMSE_rbind,R2_rbind,site73_rbind,site177_rbind,site291_rbind)
rownames(params) <- thickname
colnames(params) <- c("RMSE","R2","Site73_thick","Site177_thick","Site291_thick")
write.csv(params,
          here("supp_data","fit_params.csv"),
          row.names = FALSE)
