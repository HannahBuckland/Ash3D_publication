# Function to produce Pyle plots from contour polygon files 
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

# Function to compute areas of each isopach, fit exponential function to data 
# and produce pyle plot

pyle_data_process <- function(contours){
  
  distalcfs <- read.csv(here("exponential_fits",
                             "cfs.csv")) # read in exponential fits to other isopachs
  
  
  x <- read.table(contours,
                  fill = TRUE,
                  stringsAsFactors = FALSE)
  # Get name of run
  runname <- str_extract(contours,
                         "(?<=polygons/MZ).*(?=.txt)")
  runname <- as.list(runname)
  
  x_clean <- x[!grepl(">",x$V1),] # remove rows with "<" (breaks in original file for new thickness contour)
  x_clean <- x_clean[,-4] # removes empty column
  x_clean <- x_clean %>%
    mutate_if(is.character,
              as.numeric) # converts characters to numerics
  colnames(x_clean) <- c("long",
                         "lat",
                         "thick_mm") # renames columns
  
  isopachs <- c(10,50,100,200,300) # choose the contours to plot
  breaks <- c(0,isopachs,1190)
  iso_labs <- paste0("thick",isopachs) # get names for factor levels
  
  # Filtering for isopachs of interest
  x_plot <- x_clean %>%
    filter(thick_mm %in% isopachs) %>%
    mutate(thick_f = paste0("thick",thick_mm),
           thick_f = factor(thick_f,
                            levels = c("thick_min","thick10","thick50","thick100","thick200","thick300"))) # generate factorised column for plotting
  
  #levels(x_plot$thick_f) <- co("thick10","thick50","thick100","thick200","thick300") # rename levels
  
  xys <- st_as_sf(x_plot,
                  coords=c("long","lat")) # convert to spatial data
  latlong <- "+init=epsg:4326" # CRS I want things in
  
  # generate polygons for each isopach
  polys <- st_sf(
    aggregate(
      xys$geometry,
      list(xys$thick_mm),
      function(g){
        st_cast(st_combine(g),"POLYGON")
      }
    ))  
  
  polys_latlong <- st_set_crs(polys,4326) # set coordinate reference system to lat long
  polys_latlong$thick_f <- paste0("thick",polys_latlong$Group.1) 
  polys_latlong <- polys_latlong %>%
    mutate(thick_plot = factor(thick_f,
                               levels = c("thick_min","thick10","thick50","thick100","thick200","thick300"))) # generate factorised column for plotting
  
  
  polys_latlong_spdf <- as_Spatial(polys_latlong) # convert to spatial data frame
  utm <- "+proj=utm +zone=10" # projected CRS
  polys_utm <- spTransform(polys_latlong_spdf,utm) # reproject to equal area
  
  # data for Pyle plot, calculate sqrt(area) of each polygon and convert thickness to metres
  poly_area <- data.frame(sqrta_m=sqrt(gArea(polys_utm,byid = TRUE)),
                          thick_m=(polys_utm$Group.1)/1000,
                          thick_f=factor(polys_utm$Group.1,levels = polys_utm$Group.1))
  levels(poly_area$thick_f) <- iso_labs
  
  # set variables for exponential fit
  thick <- poly_area$thick_m
  sqrta <- poly_area$sqrta_m
  
  xvalues <- seq(0,1500e3,50) # range of sqrt area values for model fit
  
  exponential.model <- lm(log(thick) ~ sqrta) # generate linear model to fit data
  linmod <- summary(exponential.model) # summarise the data with intercept and slope values
  
  fit <- exp(predict(exponential.model, list(sqrta=xvalues))) # generate exp model values
  dffit <- data.frame(xvalues, fit)      # make exponential fit a dataframe
  
  int <- exp(coef(exponential.model)["(Intercept)"]) # assigning intercept to variable name
  pow <- coef(exponential.model)['sqrta']             # assinging slope to variable name
  
  cf <- data.frame(int,pow)
  colnames(cf) <- c("T0","negk")
  
  V <- round((((2*cf$T0)/ (cf$negk^2)) *1e-9),2) # calculate the volume using the fit parameters
  
  # Other fits
  expBuck <- distalcfs$T0[1]*(exp(distalcfs$negK[1]*xvalues))
  Buckdffit <- data.frame(xvalues, fit=expBuck, id = "Buckand, 2020")      # make exponential fit a dataframe
  
  expLid <- distalcfs$T0[2]*(exp(distalcfs$negK[2]*xvalues))
  Liddffit <- data.frame(xvalues, fit=expLid, id = "Lidstrom, 1971")      # make exponential fit a dataframe
  
  expYoung <- distalcfs$T0[3]*(exp(distalcfs$negK[3]*xvalues))
  Youngdffit <- data.frame(xvalues, fit=expYoung, id = "Young, 1990")      # make exponential fit a dataframe
  
  otherfits <- rbind(Buckdffit,Liddffit,Youngdffit)
  
  iso_colours <- c(thick_min = "grey80",
                   thick10 = "#264653",
                   thick50 = "#2a9d8f",
                   thick100 = "#e9c46a",
                   thick200 = "#f4a261",
                   thick300 = "#e63946")
  
  
  pub_lab <- c("Ash3D",
               "Buckland et al. (2020)",
               "Lidstrom (1971)", 
               "Young (1990")
  
  loc_labels <- c(thick_min="<10",
                  thick10="10-49",
                  thick50="50-99",
                  thick100="100-199",
                  thick200="200-299",
                  thick300=">300")
  
  Pyleplt <- ggplot() +
    geom_path(data = otherfits,
              aes(x=xvalues,y=fit, colour = id)) +
    geom_path(data = dffit,
              aes(x=xvalues,y=fit,colour = "Ash3D"),size =1) +
    geom_point(data = poly_area,
               aes(x=sqrta_m, 
                   y= thick_m, 
                   fill = thick_f),
               pch = 22,
               size = 2.5) +
    scale_y_log10(limits = c(0.001,10),
                  labels = c(0.001,0.01,0.1,1,10),
                  breaks = c(0.001,0.01,0.1,1,10)) +
    scale_x_continuous(expand =c(0,0),
                       limits = c(0,1500e3),
                       labels = seq(0,1500,by=500)) +
    scale_fill_manual(values = iso_colours, labels = loc_labels) +
    scale_colour_manual(values = c("black","grey5","grey50","grey80"),labels = pub_lab)+
    xlab(label = expression(paste("Area"^"1/2", "(km)"))) +
    ylab(label = "Thickness (m)") +
    guides(fill = FALSE,
           colour = guide_legend(title = "Isopachs",
                                 override.aes = list(size = c(1,0.5,0.5,0.5)))) +
    theme_bw() +
    theme(aspect.ratio = 0.7,
          legend.position = "none",
          legend.title = element_text(size=10),
          legend.text = element_text(size=8),
          axis.text = element_text(colour="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = margin(0.5,0.5,0.5,0.5,"cm"))
  
  out <- list(vol=V,
              onecm_area = signif(((poly_area[1,1]^2)/1e6),3),
              plot = Pyleplt)
  return(out)
  
}