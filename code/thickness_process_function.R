# Function to compare modelled and measured thickness and produce plots for 
# manuscript
# Requires that you have already run the "tidy_thickness.R" script
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


thickness_data_process <- function(thickness){
  
  thickDat <- thickness
  
  distalDat <- read.csv(here("data","distal_db.csv"))
  
  distalDat <- distalDat %>%
    mutate(B21_numb = paste0("B21_",Number.in.Buckland.et.al...2020..Database))
  
  filt <- distalDat$B21_numb
  
  thickread <- read.table(thickDat,
                          header=TRUE,
                          "\t",
                          stringsAsFactors = FALSE)
  
  runname <- str_extract(thickDat,"(?<=thickness/MZ).*(?=_tidy)")
  
  isopachs <- c(10,50,100,200,300) # choose the contours to plot
  breaks <- c(0,isopachs,12000)
  
  thick_filt <- thickread %>%
    filter(B21_numb %in% filt) 
  
  thick_filt <- thick_filt %>%
    inner_join(distalDat %>%
                 dplyr::select(B21_numb,Measured.thickness..cm.), 
               by = "B21_numb") %>%
    rename(measured_cm = Measured.thickness..cm.) %>%
    mutate(modelled_cm = modelled_mm/10,
           measured_mm = measured_cm*10) %>%
    mutate(thick_f = .bincode(measured_mm, breaks=breaks),
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
  
  iso_colours <- c(thick_min = "grey80",
                   thick10 = "#264653",
                   thick50 = "#2a9d8f",
                   thick100 = "#e9c46a",
                   thick200 = "#f4a261",
                   thick300 = "#e63946")
  
  # Calculating fit parameters
  
  obs <- thick_filt$modelled_mm
  mod <- thick_filt$measured_mm
  
  rsq <- function(x, y) summary(lm(y~x))$r.squared
  R2_value <- round(rsq(obs, mod),3)
  
  
  RMSE <- round((sqrt(sum((obs-mod)^2)))/length(mod),3)
  
  fitlab <- data.frame(x=25,y=440)
  runlab <- data.frame(x=480,y=440)
  
  fitlablog <- data.frame(x=0.01,y=300)
  runlablog <- data.frame(x=470,y=0.02)
  
  axislabs <- c(0.01,0.1,1,10,100)
  
  thick_B21_42 <- thick_filt %>%
    filter(B21_numb == "B21_42") %>%
    dplyr::select(modelled_mm) 
  thick_B21_73 <- thick_filt %>%
    filter(B21_numb == "B21_73") %>%
    dplyr::select(modelled_mm)
  thick_B21_177 <- thick_filt %>%
    filter(B21_numb == "B21_177") %>%
    dplyr::select(modelled_mm)
  thick_B21_291 <- thick_filt %>%
    filter(B21_numb == "B21_291") %>%
    dplyr::select(modelled_mm) 
  
  xyplt <- ggplot() +
    geom_abline(intercept=0, linetype = "solid") +
    geom_segment(data= NULL,
                 aes(x=-1,
                     y=-4,
                     xend=125,
                     yend=500), linetype="dashed",colour = "grey30") +
    geom_segment(data= NULL,
                 aes(x=-4,
                     y=-1,
                     xend=500,
                     yend=125), linetype="dashed",colour = "grey30") +
    geom_point(data=thick_filt,
               aes(x=measured_mm,y=modelled_mm,fill = thick_plot),pch=21,size=2) +
    scale_y_continuous(expand=c(0.001,0.001)) +
    scale_x_continuous(expand=c(0.001,0.001)) +
    geom_label(data=fitlab,aes(x=x,y=y),
               label=paste("R2 = ",R2_value),
               size=4,
               hjust=0,
               label.padding = unit(0.5, "lines"),
               label.r = unit(0, "lines"),
               label.size = NA,
               fill="white",
               colour="black") +
    geom_label(data=runlab,aes(x=x,y=y),
               label=runname,
               size=4,
               hjust=1,
               label.padding = unit(0.5, "lines"),
               label.r = unit(0, "lines")) +
    xlab("Measured thickness (mm)") +
    ylab("Simulated thickness (mm)") +
    coord_cartesian(xlim=c(0,500),ylim=c(0,500)) +
    scale_fill_manual(values = iso_colours,labels = c("<10","10-49","50-99","100-199","200-299",">300")) +
    theme(legend.position = "none",
          aspect.ratio = 1,
          axis.text=element_text(colour="black"),
          plot.background = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour="black",fill=NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  xypltLOG <- ggplot() +
    geom_segment(data= NULL,
                 aes(x=0.00001,
                     y=0.00004,
                     xend=1250,
                     yend=5000), linetype="dashed",colour = "grey30") +
    geom_segment(data= NULL,
                 aes(x=0.00004,
                     y=0.00001,
                     xend=5000,
                     yend=1250), linetype="dashed",colour = "grey30") +
    geom_abline(intercept=0, linetype = "solid") +
    geom_point(data=thick_filt,
               aes(x=measured_mm,y=modelled_mm,fill = thick_plot),pch=21,size=2) +
    scale_y_continuous(trans="log10",breaks=axislabs,labels=axislabs) +
    scale_x_continuous(trans="log10",breaks=axislabs,labels=axislabs) +
    coord_cartesian(xlim=c(0.01,500),ylim=c(0.01,500)) +
    scale_fill_manual(values = iso_colours,labels = c("<10","10-49","50-99","100-199","200-299",">300")) +
    geom_label(data=fitlablog,aes(x=x,y=y),
               label=paste("R2 = ",R2_value),
               size=4,
               hjust=0,
               label.padding = unit(0.5, "lines"),
               label.r = unit(0, "lines"),
               label.size = NA,
               fill="white",
               colour="black") +
    geom_label(data=runlablog,aes(x=x,y=y),
               label=runname,
               size=4,
               hjust=1,
               label.padding = unit(0.5, "lines"),
               label.r = unit(0, "lines")) +
    xlab("Measured thickness (mm)") +
    ylab("Simulated thickness (mm)") +
    theme(legend.position = "none",
          aspect.ratio = 1,
          axis.text=element_text(colour="black"),
          plot.background = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour="black",fill=NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  out <- list(thick_filt,xyplt, xypltLOG,RMSE,R2_value,thick_B21_42,thick_B21_73,thick_B21_177,thick_B21_291)
  names(out) <- c("dat","linear","log","RMSE","R2","SITE42","Site73","Site177","SITE291")
  return(out)
  
  
}

