#!/usr/bin/env Rscript

library(tidyverse)
library(rnaturalearth)
library(viridis)

## FigureS1  - MAP LOCATIONS

world <- ne_countries(scale = "medium", returnclass = "sf")



#create data frame with 4 locations
my_locs <- data.frame(location = c("Vigo (sampling site)",
                                    "Vigo (ERA5 + suncalc)", 
                                    "Port-en-Bessin (sampling site)", 
                                    "Port-en-Bessin (ERA5 + suncalc)", 
                                    "Dinard (Radiometer)", 
                                    "Dinard (ERA5 + suncalc)"),
                      lat = c(42.200, 42.201, 
                                49.352,49.351, 
                                48.628, 48.640),
                      long = c(-8.780,-8.785,
                                 -0.769, -0.748, 
                                -2.027, -2.0288))

#all plots in single PDF file
pdf("02_visuals/FigureS1_map_locations_insets.pdf", width = 7, height = 7)

#plot the world map
p1 <- ggplot() +
  geom_sf(data = world, fill = "#f9f9f9", color = "black") +
  geom_point(data = my_locs, aes(x = long, y = lat, color = location), size = 5) +
  coord_sf(xlim = c(-11, 10), ylim = c(40, 53)) + #set limits in the plot 
  theme_bw() +
  scale_color_viridis(discrete = TRUE) + 
  theme(legend.position = "bottom") + 
  xlab("Longitude") + 
  ylab("Latitude")

print(p1)

#inset Dinard 

p2 <- ggplot() +
  geom_sf(data = world, fill = "#f9f9f9", color = "black") +
  geom_point(data = my_locs, aes(x = long, y = lat, color = location), size = 5) +
  coord_sf(xlim = c(-2.2, -1.8), ylim = c(48.5, 48.75)) + #set limits in the plot 
  theme_bw() +
  scale_color_viridis(discrete = TRUE) + 
  theme(legend.position = "bottom") + 
  xlab("Longitude") + 
  ylab("Latitude") 

print(p2)

#inset Vigo 

p3 <- ggplot() +
  geom_sf(data = world, fill = "#f9f9f9", color = "black") +
  geom_point(data = my_locs, aes(x = long, y = lat, color = location), size = 5) +
  coord_sf(xlim = c(-8.85, -8.7), ylim = c(42.15, 42.25)) + #set limits in the plot 
  theme_bw() +
  scale_color_viridis(discrete = TRUE) + 
  theme(legend.position = "bottom") + 
  xlab("Longitude") + 
  ylab("Latitude")

print(p3)

#inset Por 

p4 <- ggplot() +
  geom_sf(data = world, fill = "#f9f9f9", color = "black") +
  geom_point(data = my_locs, aes(x = long, y = lat, color = location), size = 5) +
  coord_sf(xlim = c(-8.85, -8.7), ylim = c(42.15, 42.25)) + #set limits in the plot 
  theme_bw() +
  scale_color_viridis(discrete = TRUE) + 
  theme(legend.position = "bottom") + 
  xlab("Longitude") + 
  ylab("Latitude")

print(p4)

dev.off()
