# Script for plotting figure 1 for Ash3D modelling paper
# Author: Hannah Buckland
# 03/11/2021

# Read in packages
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(tidyverse)
library(patchwork)
library(sf)
library(viridis)
library(ggspatial)
library(data.table)
library(raster)
library(cowplot)
library(here)

# Read in csv of Mazama locality data base from Buckland et al. (2020)
# plus Jensen et al. (2021) cryptotephra sites
MLDB <- read.csv(here("data", "Mazlocalityinfo2022.csv"))

# Read in csv of dital sites where thickness likely primary
thick_dat <-
  read.csv(here("data", "distal_db.csv")) # distal thicknesses used

thick_breaks <- c(0, 1, 5, 10, 20, 30, Inf) # Thickness breaks (cm)

# Filter based on limit of integration of Pyle plot and
# Filter to only include sites where there is thickness data
MLDB_thick <- MLDB %>%
  filter(dist >= 136.84e3) %>%
  filter(Thickness >= 0)

# Pick out some key sites referenced in the main text
MLDB_important <- MLDB %>%
  filter(Number %in% c(42, 73, 177, 291))

# Filter cryptotephra site for zoomed out plots
MLDB_crypto <- MLDB %>%
  filter(dist >= 1500e3)

# Convert to spatial datasets
MLDB_sf <- st_as_sf(
  MLDB_thick,
  coords = c("Longitude", "Latitude"),
  crs = 4326,
  agr = "constant"
)

imp_locs <- st_as_sf(
  MLDB_important,
  coords = c("Longitude", "Latitude"),
  crs = 4326,
  agr = "constant"
) # convert to spatial dataset
crypto_sf <- st_as_sf(
  MLDB_crypto,
  coords = c("Longitude", "Latitude"),
  crs = 4326,
  agr = "constant"
) # convert to spatial dataset

# Bin the thickness into the isopach breaks
thick_dat <- thick_dat %>%
  mutate(
    Thick_f = .bincode(Measured.thickness..cm., breaks = thick_breaks),
    Thick_f = paste0("bin_", Thick_f)
  ) # bin according to breaks above

# convert binned thickness data into spatial dataset
maz_sf <- st_as_sf(
  thick_dat,
  coords = c("Long", "Lat"),
  crs = 4326,
  agr = "constant"
) # convert to spatial dataset

# Extract the shapefiles for plotting using rnaturalearth package
countrydata <-
  ne_countries(
    country = c(
      "Canada",
      "United States of America",
      "Greenland",
      "Iceland",
      "Russia"
    ),
    scale = "small",
    returnclass = 'sf'
  )
statesdata <-
  ne_states(country = c("United States of America", "Canada"),
            returnclass = 'sf')

# input the coordinates of Crater Lake
volcanoes <- data.frame(name = "Mazama",
                        latitude = 42.9446,
                        longitude = -122.1090)

# convert to spatial dataset
volc_sf <- st_as_sf(
  volcanoes,
  coords = c("longitude", "latitude"),
  crs = 4326,
  agr = "constant"
)

# Read in isopach shapefiles
minisopachs <-
  st_read(here("isopach_shapefiles", "1cm_isopach.shp")) # 1cm hand drawn
thickisos <-
  st_read(here("isopach_shapefiles", "buckland_isopachs.shp")) # Buckland et al. 2020 spline isopachs

# set bounding box of difference zoomed maps
limMAT <-
  matrix(
    c(-126, 37.5, -126, 56.5, -104, 56.5, -104, 37.5, -126, 37.5),
    ncol = 2,
    byrow = TRUE
  )
zoomMAT <-
  matrix(
    c(
      -122.5,
      42.5,
      -122.5,
      44.3,
      -120.8,
      44.3,
      -120.8,
      42.5,
      -122.5,
      42.5
    ),
    ncol = 2,
    byrow = TRUE
  )

bbox <- st_polygon(list(limMAT)) %>%
  st_sfc(crs = 4326) # convert to spatial dataset
bbox.zoom <- st_polygon(list(zoomMAT)) %>%
  st_sfc(crs = 4326) # convert to spatial dataset

# Reproject to get in North America equal area projection for zoomed out plots
countries_proj <- st_transform(countrydata, crs = st_crs(minisopachs))

# Plotting code ---------
theme <-
  theme_set(
    theme(
      legend.position = "none",
      legend.title = element_text(size = 10, hjust = 0),
      legend.text = element_text(size = 9, hjust = 0),
      legend.key.size = unit(0.3, "cm"),
      legend.background = element_rect(fill = "grey99", colour =
                                         "grey88"),
      axis.text = element_text(colour = "black"),
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )
  )

# Nicer labels for legend
thicklabs <- c(
  bin_1 = "0-1",
  bin_2 = "1 - 5",
  bin_3 = "5 - 10",
  bin_4 = "10 - 20",
  bin_5 = "20 - 30",
  bin_6 = ">30"
)

# colours for different thickness bins
colourscale <- c(
  bin_1 = "grey80",
  bin_2 = "#264653",
  bin_3 = "#2a9d8f",
  bin_4 = "#e9c46a",
  bin_5 = "#f4a261",
  bin_6 = "#e63946"
)


zoomout_plot <- ggplot(countries_proj) +
  geom_sf(fill = "#F4F9E9",
          colour = "grey70",
          size = 0.2) +
  geom_sf(
    data = thickisos,
    fill = NA,
    colour = "black",
    size = 0.4
  ) +
  geom_sf(
    data = minisopachs,
    fill = NA,
    colour = "black",
    size = 0.4
  ) +
  geom_sf(
    data = bbox,
    fill = NA,
    lwd = 1,
    colour = "#E63B2E"
  ) +
  geom_sf(
    data = volc_sf,
    fill = "red",
    colour = "white",
    shape = 24,
    size = 5,
    stroke = 1
  ) +
  geom_sf(
    data = crypto_sf,
    pch = 21,
    fill = "white",
    colour = "black",
    size = 4,
    stroke = 1
  ) +
  geom_sf_text(
    data = crypto_sf,
    aes(label = Source),
    colour = "black",
    size = 4,
    nudge_y = 200000,
    nudge_x = 300000,
    hjust = 1
  ) +
  xlab(NULL) +
  ylab(NULL) +
  coord_sf(xlim = c(-2400000, 3200000),
           ylim = c(-650000, 4200000)) +
  theme(
    panel.background = element_rect(fill = "#B4D6D3"),
    legend.position = c(0.85, 0.25)
  )



thick_plot <- ggplot(data = statesdata) +
  geom_sf(fill = "#F4F9E9",
          colour = "grey70",
          size = 0.2) +
  geom_sf(
    data = thickisos,
    fill = NA,
    colour = "black",
    size = 0.4
  ) +
  geom_sf(
    data = minisopachs,
    fill = NA,
    colour = "black",
    size = 0.4
  ) +
  geom_sf(
    data = volc_sf,
    fill = "black",
    shape = 24,
    size = 4
  ) +
  geom_sf(data = MLDB_sf,
          aes(
            colour = P_or_S,
            shape = P_or_S,
            size = P_or_S
          ),
          fill = "grey50") +
  geom_sf(
    data = maz_sf,
    aes(fill = Thick_f),
    pch = 21,
    colour = "black",
    size = 4
  ) +
  geom_sf_text(
    data = imp_locs,
    aes(label = Number),
    nudge_x = 0.5,
    nudge_y = 0.5,
    size = 4
  ) +
  scale_shape_manual(
    values = c(23, 21, 25, 1),
    labels = c("Mixed", "Primary", "Secondary", "Unknown")
  ) +
  scale_size_manual(
    values = c(2.5, 2.5, 2.5, 1.5),
    labels = c("Mixed", "Primary", "Secondary", "Unknown")
  ) +
  scale_colour_manual(
    values = c("white", "white", "white", "grey50"),
    labels = c("Mixed", "Primary", "Secondary", "Unknown")
  ) +
  scale_fill_manual(values = colourscale, labels = thicklabs) +
  labs(
    colour = "Deposit Type",
    size = "Deposit Type",
    shape = "Deposit Type",
    fill = "Measured\nthickness (cm)"
  ) +
  xlab(NULL) +
  ylab(NULL) +
  coord_sf(xlim = c(-126, -100), ylim = c(37.5, 56.5)) +
  theme(
    panel.background = element_rect(fill = "#B4D6D3"),
    legend.position = c(0.85, 0.2)
  )

mapfig <- thick_plot  + zoomout_plot +
  plot_annotation(tag_levels = "a") +
  plot_layout(widths = c(1, 0.65))
