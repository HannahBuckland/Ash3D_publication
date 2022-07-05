# Script to produce Figure 4 in the main text
# Please note that some post editing of the plot was then carried out in
# Adobe illustrator to ensure the spacing and legends were legible

# Author: Hannah Buckland
# Date: 14/07/2021

# Written for the manuscript "Modelling the transport and deposition of ash
# following a Magnitude 7 eruption: the distal Mazama tephra"
# Submitted to Bull. Volc

library(tidyverse)
library(ggmap)
library(sf)
library(rgdal)
library(rgeos)
library(patchwork)
library(broom)

# Pyle plots of Buckland et al. 2020 isopachs

pyle_dat <- read.csv("data/rootarea_thick.csv", header = FALSE)
colnames(pyle_dat) <- c("thick_m", "sqrta_km")

intersect <- 136.84 # know point of intersection from AshCalc
xvalues <- seq(0, 1000, length.out = 1000)
lm1_xvalues <-
  seq(0, intersect, length.out = 1000) # range of sqrt area values for model
lm2_xvalues <-
  seq(intersect, 1000, length.out = 1000) # range of sqrt area values for model

distal100_2_lm1_coef <- 5.524
distal100_2_lm1_exp <- 0.02184

distal100_2_lm2_coef <- 0.386
distal100_2_lm2_exp <- 0.002397

distal100_oneseg <-
  distal100_2_lm2_coef * (exp(-distal100_2_lm2_exp * xvalues))
distal100_lm1 <-
  distal100_2_lm1_coef * (exp(-distal100_2_lm1_exp * lm1_xvalues))
distal100_lm2 <-
  distal100_2_lm2_coef * (exp(-distal100_2_lm2_exp * lm2_xvalues))

oneseg_df <- data.frame(sqrta_km = xvalues, thick_m = distal100_oneseg)
lm1_df <- data.frame(sqrta_km = lm1_xvalues, thick_m = distal100_lm1)
lm2_df <- data.frame(sqrta_km = lm2_xvalues, thick_m = distal100_lm2)


oneseg <- ggplot() +
  geom_ribbon(
    data = oneseg_df,
    aes(x = sqrta_km, ymin = 0.01, ymax = thick_m),
    fill = "red",
    alpha = 0.5
  ) +
  geom_line(data = oneseg_df,
            aes(x = sqrta_km, y = thick_m)) +
  geom_point(data = pyle_dat, aes(x = sqrta_km, y = thick_m), size = 2) +
  scale_y_continuous(
    expand = c(0.01, 0.01),
    trans = "log10",
    limits = c(0.01, 10)
  ) +
  ylab("Thickness (m)") +
  scale_x_continuous(expand = c(0.01, 0.01), sec.axis =) +
  xlab(expression(paste("Area" ^ "1/2", "(km)"))) +
  labs(fill = NULL) +
  theme(
    aspect.ratio = 0.6,
    panel.background = element_rect(colour = "black", fill = NA),
    text = element_text(size = 14),
    plot.margin = margin(1, 1, 0.1, 1, unit = "cm"),
    panel.spacing = margin(1, 1, 1, 1, unit = "cm"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(colour = "black", size = 12),
    axis.title.x = element_blank(),
    legend.position = c(0.75, 0.85),
    legend.text.align = 0,
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


twoseg <- ggplot() +
  geom_ribbon(
    data = lm1_df,
    aes(
      x = sqrta_km,
      ymin = 0.01,
      ymax = thick_m,
      fill = "Segment 0"
    ),
    alpha = 0.5
  ) +
  geom_line(data = lm1_df, aes(x = sqrta_km, y = thick_m)) +
  geom_ribbon(
    data = lm2_df,
    aes(
      x = sqrta_km,
      ymin = 0.01,
      ymax = thick_m,
      fill = "Segment 1"
    ),
    alpha = 0.5
  ) +
  geom_line(data = lm2_df, aes(x = sqrta_km, y = thick_m)) +
  geom_point(data = pyle_dat, aes(x = sqrta_km, y = thick_m), size = 2) +
  geom_segment(
    data = data.frame(
      x = intersect,
      xend = intersect,
      y = min(lm1_df$thick_m),
      yend = 0.01
    ),
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    linetype = "dotted"
  ) +
  scale_y_continuous(
    expand = c(0.01, 0.01),
    trans = "log10",
    limits = c(0.01, 10)
  ) +
  ylab("Thickness (m)") +
  scale_x_continuous(expand = c(0.01, 0.01), sec.axis =) +
  xlab(expression(paste("Area" ^ "1/2", "(km)"))) +
  scale_fill_manual(
    values = c("#D3D0CB", "#44BBA4"),
    labels = c(
      expression("Segment 0 = 19 km" ^ "2"),
      expression("Segment 1 = 129 km" ^ "2")
    )
  ) +
  labs(fill = NULL) +
  theme(
    aspect.ratio = 0.6,
    panel.background = element_rect(colour = "black", fill = NA),
    text = element_text(size = 14),
    plot.margin = margin(0, 1, 1, 1, unit = "cm"),
    panel.spacing = margin(1, 1, 1, 1, unit = "cm"),
    axis.text.x = element_text(colour = "black", size = 12),
    axis.text.y = element_text(colour = "black", size = 12),
    axis.title.x = element_text(vjust = -0.5),
    legend.position = c(0.7, 0.8),
    legend.text.align = 0,
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

fig04 <- oneseg + twoseg + 
  plot_layout(ncol = 1, nrow = 2) + 
  plot_annotation(tag_levels = "a")

ggsave("plots/Fig04_volume.pdf",out,width=87,height=100,units = "mm")

