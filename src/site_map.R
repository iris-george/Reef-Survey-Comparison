########## SURVEY COMPARISON PROJECT SURVEY SITE MAP ##########
########## 
##########
# This file creates a map of all the Florida Keys reef sites where survey
# data was obtained from. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-07-23
##########
##########


# Set-Up =======================================================================

# packages
library(mapdata)
library(maps)
library(maptools)
library(ggmap)
library(ggrepel)
library(raster)
library(ggthemes)
library(ggsn)
library(rgeos)
library(rgdal)
library(tidyverse)
library(cowplot)
library(here)

# internet data
us <- getData("GADM",country="USA",level=1)

# filter for Florida
states <- c('Florida')
us.states <- us[us$NAME_1 %in% states,]

# site data
keys_fish_data = read_csv(here("./clean_data/survey_sites.csv"))


# Make Themes ==================================================================

fte_theme_map_small <- function(){
  color.background = 'grey90'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill= 'white',color = 'white')) +
    theme(plot.background = element_rect(fill = color.background, 
                                         color = color.background)) +
    theme(panel.border = element_rect(colour = 'black')) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 15, 
                                    vjust = 1.25)) +
    theme(axis.text.x = element_blank()) + 
    theme(axis.text.y = element_blank()) + 
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.line.x = element_line(color="black", size = 0.15),
          axis.line.y = element_line(color="black", size = 0.15)) 
}
fte_theme_map_sites <- function(){
  color.background = 'white'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill = 'white', color = 'white')) +
    theme(plot.background = element_rect(fill=color.background,
                                         color = color.background)) +
    theme(panel.border = element_rect(colour = 'black')) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_line(color="black", size = 0.15)) +
    theme(plot.title = element_text(color = color.title, size = 15, 
                                    vjust = 1.25)) +
    theme(axis.text.x = element_text(size = 18, color = color.axis.text, 
                                     angle = 90)) + 
    theme(axis.text.y = element_text(size = 18, color = color.axis.text)) + 
    theme(axis.title.x = element_text(size = 20, color = color.axis.title, 
                                      vjust = 0)) +
    theme(axis.title.y = element_text(size = 20, color = color.axis.title, 
                                      vjust = 1.25)) +
    theme(plot.title = element_blank()) +
    theme(axis.line.x = element_line(color="black", size = 0.15),
          axis.line.y = element_line(color="black", size = 0.15)) +
    theme(legend.position = c(0.88, 0.28),
          legend.background = element_rect(colour = 'black'),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 12))
}


# Map of Surrounding Area ======================================================

continent = ggplot()+
  geom_polygon(data = us,aes(x=long,y=lat,group=group), colour = 'grey40', 
               #put in the acutual shape file
               size = 0.01, fill = 'grey90')+
  geom_polygon(data = us.states,aes(x=long,y=lat,group=group), 
               colour = 'midnightblue', size = 0.01, #put in FL shapefile
               fill = 'grey70')+
  coord_cartesian(xlim = c(-93.9,-75.85), ylim = c(24.1,32.5)) + 
  #delimit where we are
  fte_theme_map_small() + #bring in the map
  annotate("rect", xmin = -79, xmax = -82, ymin = 24.5, ymax = 25.5, 
           alpha = .7)+ #shaded study area
  annotate('text', x = -91, y = 25.8, label = 'Gulf of Mexico', size = 4)+
  annotate('text', x = -78, y = 31.2, label = 'Atlantic Ocean', size = 4)+
  annotate('text', x = -87, y = 27.8, label = 'Study Area', size = 5)+
  annotate('segment',x=-87, y=27.28, xend=-82.2, yend=25.1, 
           arrow=arrow(length = unit(0.04, "npc")), #arrow
           alpha = 0.8, size=1.1, color="black")+
  scalebar(x.min = -93, x.max = -85, y.min = 24.5, y.max = 25.5, dist = 250, 
           dist_unit = 'km', st.size = 3.6, #add scalebar
           transform = TRUE, model = 'WGS84', location = 'bottomleft', 
           st.dist = 0.42, height = 0.18) 
#transform = TRUE assumes coordinates are in decimal degrees


# Map of Study Area ============================================================

site_map = ggplot() +
  geom_polygon(data = us.states,aes(x=long,y=lat,group=group), 
               colour = 'grey40', size = 0.01, 
               fill = 'grey60') +
  coord_cartesian(xlim = c(-81.1, -80.0), ylim = c(24.75,25.75)) +
  geom_point(data = keys_fish_data, aes(x = X, y = Y)) +
  labs(x = 'Longitude', y = 'Latitude') +
  north(location = 'topright', scale = 0.9, symbol = 12, #add north arrow
        x.min = -80.09, x.max = -80.0, y.min = 25.65, y.max = 25.75) +
  scalebar(x.min = -80.80, x.max = -81.1, y.min = 24.875, y.max = 24.96, 
           dist = 10, dist_unit = 'km', #add scalebar
           transform = TRUE, model = 'WGS84', location = 'bottomleft', 
           st.dist = 0.49, height = 0.18) 
#transform = TRUE assumes coordinates are in decimal degrees


# Inset Maps ===================================================================

#make the one plot inset with the other
insetmap = ggdraw()+
  draw_plot(site_map) + 
  draw_plot(continent, x=0.1, y=0.62, width=0.38, height=0.35) 

# save map
ggsave(here("./visuals/survey_site_map.png"), insetmap)
