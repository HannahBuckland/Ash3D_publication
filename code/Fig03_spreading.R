# Script to plot Fig03 in main text, Costa versus Webster spreading
# Please note that some post editing of the plot was then carried out in
# Adobe illustrator to ensure the spacing and legends were legible

# Author: Hannah Buckland
# Date: 14/07/2021

# Written for the manuscript "Modelling the transport and deposition of ash
# following a Magnitude 7 eruption: the distal Mazama tephra"
# Submitted to Bull. Volc


library(tidyverse)

R <- 1
r <- seq(0.01,1,by=0.001)

r_R <- r/R
R_r <- R/r

costa <- (3/4)*R_r*(1+(1/3)*r_R^2)
webster <- sqrt(R_r)

res <- data.frame(x=r_R,costa=costa,webster=webster)
res_long <- pivot_longer(res,cols=!x)


ticks <- 1:9
ooms <- 10^(0:2) # define the OOMs (orders of magnitudes)
breaks <- as.vector(ticks %o% ooms)

# select the y axis labels to show
show.labels <- c(T,F,F,F,F,F,F,F,F)
labels <- as.character(breaks * show.labels)
labels <- gsub("^0$", "", labels)

spreading <- ggplot() +
  geom_line(data=res_long,
            aes(x=x,y=value,colour=name),size=1.5) +
  scale_colour_manual(values = c("coral","darkcyan"),labels = c("Costa et al. (2013)","Webster et al. (2020)")) +
  scale_y_log10(breaks=breaks, labels = labels) +
  scale_x_continuous(expand=c(0.01,0.01)) +
  xlab("\nr / R") +
  ylab(expression(paste("u"["r"]," / ","u"["R"]))) + 
  labs(colour = "Umbrella Spreading") +
  coord_cartesian(xlim=c(0.01,1),ylim=c(1,100)) +
  theme(aspect.ratio =1,
        plot.background = element_blank(),
        legend.position = c(0.7,0.8),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        axis.text = element_text(colour="black",size=12),
        axis.title = element_text(colour="black",size=14),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(colour="grey95"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, colour = "black"),
        plot.margin = margin(0.5,0.5,0.5,0.5,"cm"))

ggsave("plots/Fig03_costa_webster.pdf",spreading, width=87,height=100,units = "mm")


