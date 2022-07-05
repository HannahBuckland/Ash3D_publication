# Script that runs all data processing and plotting functions from wrapper
# then it individually builds the plots for the resubmitted manuscript 
# Please note that some post editing of the plots was then carried out in 
# Adobe illustrator to ensure the spacing and legends were legible

# Author: Hannah Buckland
# Date: 24/06/2022

# Written for the manuscript "Modelling the transport and deposition of ash 
# following a Magnitude 7 eruption: the distal Mazama tephra" 
# Submitted to Bull. Volc


library(here)

source(here("code","Ash3D_function_wrapper.R"))

# Set up some labelling dataframes
runlab <- data.frame(lat=39,
                     long=-104)

paramlab <- data.frame(lat=41,
                       long=-104)


##### Figure 8 in the main text - Diffusion coefficient ####

default_ash3d <- contour_plots$Run001 + 
  geom_label(data=paramlab,
             aes(x=long,y=lat),
             label=expression(paste(K, " = ","0 ", m^2,s^-1)),
             size=2.5,
             label.size = NA,
             hjust=1,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0, "lines")) +
  geom_label(data=runlab,
             aes(x=long,y=lat),
             label="Run001",
             size=3,
             hjust=1,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) 

default_diffusion1000 <- contour_plots$Run002 +  
  geom_label(data=paramlab,
             aes(x=long,y=lat),
             label=expression(paste(K, " = ","1000 ", m^2,s^-1)),
             size=2.5,
             label.size = NA,
             hjust=1,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0, "lines")) +
  geom_label(data=runlab,
             aes(x=long,y=lat),
             label="Run002",
             size=3,
             hjust=1,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

default_diffusion10000 <- contour_plots$Run003 +
  geom_label(data=paramlab,
             aes(x=long,y=lat),
             label=expression(paste(K, " = ","10000 ", m^2,s^-1)),
             size=2.5,
             label.size = NA,
             hjust=1,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0, "lines")) +
  geom_label(data=runlab,
             aes(x=long,y=lat),
             label="Run004",
             size=3,
             hjust=1,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

pyle_default <-pyle_out$Run001$plot
pyle_diffusion1000 <- pyle_out$Run002$plot +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

pyle_diffusion10000 <- pyle_out$Run004$plot + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

diffusion <- default_ash3d + default_diffusion1000 + default_diffusion10000 + 
  pyle_default +  pyle_diffusion1000 + pyle_diffusion10000 + 
  plot_layout(width=c(),heights = c())

ggsave("plots/Fig_08_Diffusion_comp.pdf",diffusion, width = 160,height=400,units = "mm")

##### Figure 9 in the main text - Umbrella spreading regime ####

diffusion_only <- contour_plots$Run002 +  
  geom_label(data=paramlab,
             aes(x=long,y=lat),
             label="No umbrella",
             size=2.5,
             label.size = NA,
             hjust=1,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0, "lines")) +
  geom_label(data=runlab,
             aes(x=long,y=lat),
             label="Run002",
             size=3,
             hjust=1,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

diffusion_webster <- contour_plots$Run010 +  
  geom_label(data=paramlab,
             aes(x=long,y=lat),
             label="Webster et al.",
             size=2.5,
             label.size = NA,
             hjust=1,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0, "lines")) +
  geom_label(data=runlab,
             aes(x=long,y=lat),
             label="Run010",
             size=3,
             hjust=1,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

diffusion_costa <- contour_plots$Run006 +  
  geom_label(data=paramlab,
             aes(x=long,y=lat),
             label="Costa et al.",
             size=2.5,
             label.size = NA,
             hjust=1,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0, "lines")) +
  geom_label(data=runlab,
             aes(x=long,y=lat),
             label="Run006",
             size=3,
             hjust=1,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

pyle_diffusiononly <-pyle_out$Run002$plot
pyle_webster <- pyle_out$Run010$plot +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

pyle_costa <- pyle_out$Run006$plot + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

umbrella <- diffusion_only + diffusion_webster + diffusion_costa + 
  pyle_diffusiononly +  pyle_webster + pyle_costa + 
  plot_layout(width=c(),heights = c())

ggsave("plots/Fig09_Umbrella_comp.pdf",umbrella, width = 160,height=400,units = "mm")

##### Figure 10 in the main text - Grain size distribution #####

defaultGSD <- contour_plots$Run006 +  
  geom_label(data=paramlab,
             aes(x=long,y=lat),
             label="Default GSD",
             size=2.5,
             label.size = NA,
             hjust=1,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0, "lines")) +
  geom_label(data=runlab,
             aes(x=long,y=lat),
             label="Run006",
             size=3,
             hjust=1,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) +
  theme(axis.text.x = element_blank())

yellowGSD <- contour_plots$Run014 +  
  geom_label(data=paramlab,
             aes(x=long,y=lat),
             label="GSD_M14",
             size=2.5,
             label.size = NA,
             hjust=1,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0, "lines")) +
  geom_label(data=runlab,
             aes(x=long,y=lat),
             label="Run014",
             size=3,
             hjust=1,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank())

HB_biGSD <- contour_plots$Run022 +  
  geom_label(data=paramlab,
             aes(x=long,y=lat),
             label="GSD_B21_B",
             size=2.5,
             label.size = NA,
             hjust=1,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0, "lines")) +
  geom_label(data=runlab,
             aes(x=long,y=lat),
             label="Run022",
             size=3,
             hjust=1,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) +
  theme()

HB_uniGSD <- contour_plots$Run030 +  
  geom_label(data=paramlab,
             aes(x=long,y=lat),
             label="GSD_B21_U",
             size=2.5,
             label.size = NA,
             hjust=1,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0, "lines")) +
  geom_label(data=runlab,
             aes(x=long,y=lat),
             label="Run030",
             size=3,
             hjust=1,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())


grainsize <- defaultGSD + yellowGSD + HB_biGSD + HB_uniGSD +
  plot_layout(width=c(),heights = c())

ggsave("plots/Fig_10_GSD_comp.pdf", grainsize, width = 100,height=200,units = "mm")


##### Figure 11 in the main text - logarithmic thickness comparison ####

defaultGSD_log <- thickness_out$Run006$log +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
yellowGSD_log <- thickness_out$Run014$log +
  theme(axis.title = element_blank(),
        axis.text = element_blank())
HB_biGSD_log <- thickness_out$Run022$log 
HB_uniGSD_log <- thickness_out$Run030$log +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

obs_measLOG <- defaultGSD_log + yellowGSD_log + HB_biGSD_log + HB_uniGSD_log +
  plot_annotation(tag_levels = "a")

ggsave("plots/Fig11_obs_measLOG.pdf", obs_measLOG, width = 140,height=200,units = "mm")

##### Figure 12 in the main text - linear thickness comparison ####

defaultGSD_linear <- thickness_out$Run006$linear +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
yellowGSD_linear <- thickness_out$Run014$linear +
  theme(axis.title = element_blank(),
        axis.text = element_blank())
HB_biGSD_linear <- thickness_out$Run022$linear 
HB_uniGSD_linear <- thickness_out$Run030$linear +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

obs_measLIN <- defaultGSD_linear + yellowGSD_linear + HB_biGSD_linear + 
  HB_uniGSD_linear +
  plot_annotation(tag_levels = "a")

ggsave("plots/Fig12_obs_measLIN.pdf", obs_measLIN, width = 140,height=200,units = "mm")

# Supplementary plots

##### Figure S5 -Deposit density #####

run006_density <- contour_plots$Run006 + 
  geom_label(data=paramlab,
             aes(x=long,y=lat),
             label=expression(paste(rho[d], " = ","1000 ", kgm^-3)),
             size=2.5,
             label.size = NA,
             hjust=1,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0, "lines")) +
  geom_label(data=runlab,
             aes(x=long,y=lat),
             label="Run006",
             size=3,
             hjust=1,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) 

run037_density <- contour_plots$Run037 +
  geom_label(data=paramlab,
             aes(x=long,y=lat),
             label = expression(paste(rho[d], " = ","700 ", kgm^-3)),
             size=2.5,
             label.size = NA,
             hjust=1,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0, "lines")) +
  geom_label(data=runlab,
             aes(x=long,y=lat),
             label="Run037",
             size=3,
             hjust=1,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

pyle_density <-pyle_out$Run006$plot

pyle_density700 <- pyle_out$Run037$plot + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

density <- run006_density + run037_density + pyle_density + pyle_density700 + 
  plot_layout(width=c(),heights = c())

ggsave("plots/FigS5_density_comp.pdf",density, width = 120,height=300,units = "mm")


##### Figure S6 - Umbrella spreading height #####

spread_30 <- contour_plots$Run006 + 
  geom_label(data=paramlab,
             aes(x=long,y=lat),
             label=expression(paste(H[u], " = ","30 km")),
             size=2.5,
             label.size = NA,
             hjust=1,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0, "lines")) +
  geom_label(data=runlab,
             aes(x=long,y=lat),
             label="Run006",
             size=3,
             hjust=1,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) 

spread_25 <- contour_plots$Run049 +  
  geom_label(data=paramlab,
             aes(x=long,y=lat),
             label=expression(paste(H[u], " = ","25 km")),
             size=2.5,
             label.size = NA,
             hjust=1,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0, "lines")) +
  geom_label(data=runlab,
             aes(x=long,y=lat),
             label="Run049",
             size=3,
             hjust=1,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

spread_15 <- contour_plots$Run051 +
  geom_label(data=paramlab,
             aes(x=long,y=lat),
             label=expression(paste(H[u], " = ","15 km")),
             size=2.5,
             label.size = NA,
             hjust=1,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0, "lines")) +
  geom_label(data=runlab,
             aes(x=long,y=lat),
             label="Run051",
             size=3,
             hjust=1,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

pyle_30km <-pyle_out$Run006$plot
pyle_25km <- pyle_out$Run049$plot +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

pyle_15km <- pyle_out$Run051$plot + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())

plumeheight <- spread_30 + spread_25 + spread_15 + pyle_30km +  pyle_25km + pyle_15km + 
  plot_layout(width=c(),heights = c())

ggsave("plots/FigS6_plumeheight_comp.pdf",plumeheight, width = 160,height=400,units = "mm")


##### Figure S7 - Percentage of fines aggregated ####

fullagg <- contour_plots$Run030 +  
  geom_label(data=paramlab,
             aes(x=long,y=lat),
             label="100% agg.",
             size=2.5,
             label.size = NA,
             hjust=1,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0, "lines")) +
  geom_label(data=runlab,
             aes(x=long,y=lat),
             label="Run030",
             size=3,
             hjust=1,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) +
  theme(axis.text.x = element_blank())

eightyperc <- contour_plots$Run044 +  
  geom_label(data=paramlab,
             aes(x=long,y=lat),
             label="80% agg.",
             size=2.5,
             label.size = NA,
             hjust=1,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0, "lines")) +
  geom_label(data=runlab,
             aes(x=long,y=lat),
             label="Run044",
             size=3,
             hjust=1,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank())

sixtyperc <- contour_plots$Run046 +  
  geom_label(data=paramlab,
             aes(x=long,y=lat),
             label="60% agg.",
             size=2.5,
             label.size = NA,
             hjust=1,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0, "lines")) +
  geom_label(data=runlab,
             aes(x=long,y=lat),
             label="Run046",
             size=3,
             hjust=1,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) +
  theme()

zeroperc <- contour_plots$Run048 +  
  geom_label(data=paramlab,
             aes(x=long,y=lat),
             label="0% agg.",
             size=2.5,
             label.size = NA,
             hjust=1,
             label.padding = unit(0.1, "lines"),
             label.r = unit(0, "lines")) +
  geom_label(data=runlab,
             aes(x=long,y=lat),
             label="Run048",
             size=3,
             hjust=1,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())


aggregation <- fullagg + eightyperc + sixtyperc + zeroperc +
  plot_layout(width=c(),heights = c())

ggsave("plots/FigS7_agg_comp.pdf", aggregation, width = 100,height=200,units = "mm")
