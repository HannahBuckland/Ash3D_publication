# Function to produce isopach maps from ASCII files 
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

# Function that returns isopach plots used in manuscript
ascii_plot <- function(ascii_unwrap){
  
  # Read in sfs of countries for plotting
  statesdata <- ne_states(country = c("United States of America",
                                      "Canada",
                                      "Mexico"),
                          returnclass = 'sf')
  
  # Read in observed data
  fieldlocs <- read.csv(here("data",
                             "distal_db.csv"))
  
  
  isopachs <- c(10,50,100,200,300) # choose the contours to plot
  contours_breaks <-  c(10,50,100,200,300,Inf)
  breaks <- c(0,isopachs,1190)
  
  contour_colours <- c("#264653","#2a9d8f","#e9c46a","#f4a261","#e63946")
  
  fieldloc_plot <- fieldlocs %>%
    dplyr::select(c("Long","Lat","Measured.thickness..cm.")) %>%
    rename(thick_cm = Measured.thickness..cm.) %>%
    mutate(thick_mm = thick_cm*10,
           thick_f = .bincode(thick_mm, breaks=breaks),
           thick_f = paste0("bin_",thick_f),
           thick_f = factor(thick_f,
                            levels = c("bin_1","bin_2","bin_3","bin_4","bin_5","bin_6")),
           thick_plot = ifelse(grepl("bin_1",thick_f),"thick_min",
                               ifelse(grepl("bin_2",thick_f),"thick10",
                                      ifelse(grepl("bin_3",thick_f),"thick50",
                                             ifelse(grepl("bin_4",thick_f),"thick100",
                                                    ifelse(grepl("bin_5",thick_f),"thick200",
                                                           "thick300"))))),
           thick_plot = factor(thick_plot,
                               levels = c("thick_min","thick10","thick50","thick100","thick200","thick300")))
  
  obs_sf <- st_as_sf(fieldloc_plot, coords=c("Long","Lat")) # convert to spatial data
  obs_sf <- st_set_crs(obs_sf,4326) # set coordinate reference system to lat long
  
  obs_sf <- st_as_sf(fieldloc_plot, coords=c("Long","Lat")) # convert to spatial data
  obs_sf <- st_set_crs(obs_sf,4326) # set coordinate reference system to lat long
  
  CL <- data.frame(latitude=42.9446,longitude=-122.1090)
  
  CL <- st_as_sf(CL,
                 coords = c("longitude",
                            "latitude"),crs = 4326, 
                 agr = "constant")
  
  iso_colours <- c(thick_min = "grey80",
                   thick10 = "#264653",
                   thick50 = "#2a9d8f",
                   thick100 = "#e9c46a",
                   thick200 = "#f4a261",
                   thick300 = "#e63946")
  
  loc_labels <- c(thick_min="<10",
                  thick10="10-49",
                  thick50="50-99",
                  thick100="100-199",
                  thick200="200-299",
                  thick300=">300")
  
  locmap <- ggplot(data=statesdata) +
    geom_sf(fill = "grey90",
            colour = "grey70") +
    geom_contour_filled(data = ascii_unwrap,
                        aes(x=long_val,
                            y=lat_val, 
                            z= thick_mm,
                            fill=..level..,
                            colour = ..level..),
                        breaks = contours_breaks,
                        alpha = 0.25) +
    scale_fill_manual(values = contour_colours, 
                      labels = c("10-49","50-99","100-199","200-299",">300"),
                      name = "Ash3D (mm)") +
    scale_colour_manual(values = contour_colours, 
                        labels = c("10-49","50-99","100-199","200-299",">300"),
                        name = "Ash3D (mm)") +
    guides(colour = guide_legend(override.aes = list(size = 0.5,
                                                     fill = contour_colours,
                                                     alpha=0.25))) +
    new_scale_fill() +
    geom_sf(data = obs_sf, 
            inherit.aes = FALSE,
            aes(fill = thick_plot),
            size=2, 
            pch = 21) +
    scale_fill_manual(values = iso_colours,
                      labels = c("<10","10-49","50-99","100-199","200-299",">300"),
                      name = "Obs. (mm)") +
    geom_sf(data = CL,
            colour = "white",
            fill="black",
            pch=24,
            size =2) +
    coord_sf(xlim=c(-126,-104),
             ylim=c(37.5,56.5),
             expand=FALSE) +
    guides(fill = guide_legend(override.aes = list(pch=21,size = 4, fill = iso_colours))) +
    xlab(NULL) +
    ylab(NULL) +
    scale_x_continuous(breaks = c(-110,-120)) +
    theme(legend.position = "none",
          legend.title = element_text(size=10),
          legend.text = element_text(size=8),
          axis.text=element_text(colour="black", size = 9),
          plot.background = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour="black",fill = NA))
  
  return(locmap)  
  
}