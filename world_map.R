# ## Purpose: HMS 520 Final Project about World map
# Authors: Su-Miao Lai and Dibbya Biswa
# Contact: sml10@uw.edu
# History: 
#         Date created: 9 December 2022
#         Date rewritten:
#         Last modified:

# set working directory
setwd("/Users/smlai/Desktop/HMS520_final_project/")
rm(list=ls())

library("readr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("maps")
library("rbokeh")
library("widgetframe")
library("tidyverse")


## load data
gbd5q0 <- read.csv("data/childMortAndCovars.csv")
country_codes <- read.csv("data/country_codes.csv") 
# merge gbddata and country codes
gbd5q0 <- merge(gbd5q0, country_codes, stringsAsFactors = FALSE, by.x= "country", by.y = "country")  
# Only explore data in 2010
gbd5q0_2010 <- dplyr::filter(gbd5q0 , year == 2010)
## Interactive map with country, gbdRegion, under5Deaths
gbd5q0_2010$under5Deaths <- prettyNum(gbd5q0_2010$under5Deaths, big.mark = ",")
plot <- suppressWarnings(figure(width = 800, height = 450, padding_factor = 0) %>%
                           ly_map("world", col = "lightgray") %>%
                           ly_points(longitude, latitude, data = gbd5q0_2010, size = 5,
                                     hover = c(country, gbdRegion, under5Deaths)))
widgetframe::frameWidget(plot,width=600,height=400)

# -------------------------------------------------

# create data for world_map using map_data() function
world_map <- map_data("world")
names(world_map)[names(world_map) == 'region'] <- 'country'
mapdata <- left_join(world_map, gbd5q0_2010, by = "country")

# create world map using ggplot() function
# geo_ploygon() for tfr
fig <- ggplot(mapdata, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = tfr), color = "black") + 
  labs(title = "World Map for Total Fertility Rates") + 
  scale_fill_gradient(name = "Total Fertility Rates", low = "yellow", high = "red") +
    theme(legend.position='bottom')

ggsave("tfr.png", plot = fig, height = 8, width = 10, units = "in")
 
# -------------------------------------------------   
# geo_ploygon() for hexp_pc
fig <- ggplot(mapdata, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = hexp_pc), color = "black") + 
      labs(title = "World Map for Health Expenditure per capita") +
      scale_fill_gradient(name = "Health Expenditure per capita", low = "lightpink", high = "purple") +
  theme(legend.position='bottom')
    
ggsave("health_exp.png", plot = fig, height = 8, width = 10, units = "in")

