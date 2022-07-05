# Script to produce Figure 2c in the main text
# Please note that some post editing of the plot was then carried out in
# Adobe illustrator to ensure the spacing and legends were legible
# Adapted from MATLAB script from L Mastin

# Author: Hannah Buckland
# Date: 14/07/2021

# Written for the manuscript "Modelling the transport and deposition of ash
# following a Magnitude 7 eruption: the distal Mazama tephra"
# Submitted to Bull. Volc

library(ggplot2)
library(tidyverse)
library(data.table)

# Suzuki equation
k <- list(8,12)       #Suzuki constants
A <- 1        #integration constant
z <- seq(0,1,by=0.01) # height, normalized to total plume height

suzuki_function <- function(k){
  suzuki_out <- data.frame(z=z,
                           k=as.character(k),
                           dMdz=A*k*(1-z)*exp(-k*(1-z)))
  suzuki_out <- suzuki_out %>%
    arrange(z)
  return(suzuki_out)
}

fun_out <- lapply(k, suzuki_function)
suzuki_out_long <- rbindlist(fun_out)

suzuki_plot <- ggplot(suzuki_out_long) +
  geom_line(aes(y=dMdz,x=z,colour=k)) +
  coord_flip() +
  xlab("Relative height in plume (z/HT)") +
  ylab("Mass Distribution (dM/dz)") +
  scale_x_continuous(expand =c(0,0)) +
  scale_colour_manual(values = c("#3366CC","#FF9900")) +
  theme_bw() +
  theme(aspect.ratio = 1.7,
        legend.position = c(0.85,0.2),
        text = element_text(size=12,colour="black"),
        axis.text = element_text(size=10,colour = "black"),
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5,"cm"))

ggsave("plots/Fig03_suzuki_dist.pdf",suzuki_plot,width=87,height=100,units = "mm")
