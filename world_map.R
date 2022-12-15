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


# ------------------ Create map for under 5 mortality in South Asia 
# South Asia Contries
sa.countries <- c(
  "Afghanistan", "Bangladesh", "Bhutan" , "India", "Nepal" ,
  "Pakistan", "Sri Lanka", "Maldives"
)
# Retrievethe map data
sa.maps <- map_data("world", region = sa.countries)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
gbd5q0_2010_sa <- gbd5q0_2010 %>% 
   select(country, under5MR, water, hexp_pc, education, sanitation, sdi) 

colnames(gbd5q0_2010_sa)[1] <- "region"

# merge 
sa_mapdata <- left_join(sa.maps, gbd5q0_2010_sa, stringsAsFactors = FALSE, by ="region") 

region.lab.data <- sa.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

# map for under 5 mortality rate 
ggplot(sa_mapdata, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = under5MR))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  ggtitle("Under 5 Mortality Rate for South Asia") +
  guides(fill=guide_legend("Under 5 mortality rate")) +
  theme(legend.position="bottom")

ggsave("map_south_asia_5q0.png", height = 8, width = 10, units = "in")

# map for education
ggplot(sa_mapdata, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = education))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  ggtitle("Education for South Asia") +
  guides(fill=guide_legend("Education (years per capita) aggregated by age (15+) and sex")) +
  theme(legend.position="bottom")

ggsave("map_south_asia_education.png", height = 8, width = 10, units = "in")

# map for sanitation
ggplot(sa_mapdata, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = sanitation))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  ggtitle("Percent of population with access to improved toilet types for South Asia") +
  guides(fill=guide_legend("Percent of population with access to improved toilet types as defined by the Joint Monitoring Program")) +
 theme(legend.position="bottom")

ggsave("map_santation.png", height = 8, width = 10, units = "in")


# map for hexp_pc
ggplot(sa_mapdata, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = hexp_pc))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  ggtitle("Health expenditure per capita for South Asia") +
  guides(fill=guide_legend("Health expenditure per capita")) +
  theme(legend.position="bottom")

ggsave("map_hexp_pc.png", height = 8, width = 10, units = "in")

# map for water
ggplot(sa_mapdata, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = water))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  ggtitle("Percent of population with access to improved water sources for South Asia") +
  guides(fill=guide_legend("Percent of population with access to improved water sources as defined by the Joint Monitoring Program")) +
  theme(legend.position="bottom")

ggsave("map_water.png", height = 8, width = 10, units = "in")

# ---------------------------Interactive Map ---------------------------
## World map to display the information about country, gbdRegion, under5Deaths
gbd5q0_2010$under5Deaths <- prettyNum(gbd5q0_2010$under5Deaths, big.mark = ",")
plot <- suppressWarnings(figure(width = 800, height = 450, padding_factor = 0) %>%
                           ly_map("world", col = "lightgray") %>%
                           ly_points(longitude, latitude, data = gbd5q0_2010, size = 5,
                                     hover = c(country, gbdRegion, under5Deaths)))

widgetframe::frameWidget(plot,width=600,height=400)

# --------------------- Linear Regression Model ---------------------------
# Build a model to investigate the association between under 5 mortality rate 
# and covariates (education, sdi, water, sanitation) 
# IV: Under 5 mortality rate
# DV: education, sdi, water, sanitation

mod <- glm(under5MR ~ sdi + water + sanitation, family = gaussian, data = gbd5q0_2010)
summary(mod)

# --------------------------- ---------------------------
