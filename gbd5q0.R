# ## Purpose: HMS 520 Final Project about Child Mortality Plots
# Authors: Su-Miao Lai and Dibbya Biswa
# Contact: sml10@uw.edu and dibbyb@uw.edu
# History: 
#         Date created: 7 December 2022
#         Date rewritten:
#         Last modified:

# set working directory
setwd("/Users/smlai/Desktop/HMS520_final_project/")
rm(list=ls())

# load library
library("readr")
library("dplyr")
library("ggplot2")
library("readxl")

dataDir <- "~/Desktop/HMS520_final_project/data/"
gbd5q0 <- NULL

# Combine multiple years datasets
for (x in seq(1970, 2010, 10)) {
  gbd5q0 <- rbind(gbd5q0, read.csv(paste0(dataDir, "gbdChildMortality_", x, "s.csv"), stringsAsFactors = FALSE))
}

# Merge covariate dataset to gbd5q0
gbd5q0 <- merge(gbd5q0, read.csv(paste0(dataDir, "countryCovars.csv"), stringsAsFactors = FALSE),
                   by.x = c("iso", "year"), by.y = c("iso3", "year"))

library(ggpubr)
p1 <- gbd5q0 %>% filter(gbdRegion == "Asia, South") %>%
  ggplot(aes(x = year, y = under5MR)) +
  scale_y_log10() +
  geom_point() + 
  facet_wrap(vars(country)) +
  labs(x ="Year", y = "Mortality Rate for Children under 5 years old") 

p2 <- gbd5q0 %>% filter(gbdRegion == "Sub-Saharan Africa, West") %>%
  ggplot(aes(x = year, y = under5MR)) +
  scale_y_log10() +
  geom_point() + 
  facet_wrap(vars(country)) +
  labs(x ="Year", y = "Mortality Rate for Children under 5 years old")

p2 <- gbd5q0 %>% filter(gbdRegion == "Sub-Saharan Africa, East") %>%
  ggplot(aes(x = year, y = under5MR)) +
  scale_y_log10() +
  geom_point() + 
  facet_wrap(vars(country)) +
  labs(x ="Year", y = "Mortality Rate for Children under 5 years old")

p3 <- gbd5q0 %>% filter(gbdRegion == "Caribbean") %>%
  ggplot(aes(x = year, y = under5MR)) +
  scale_y_log10() +
  geom_point() + 
  facet_wrap(vars(country)) +
  labs(x ="Year", y = "Mortality Rate for Children under 5 years old")

p4 <- gbd5q0 %>% filter(gbdRegion == "Europe, Western") %>%
  ggplot(aes(x = year, y = under5MR)) +
  scale_y_log10() +
  geom_point() + 
  facet_wrap(vars(country)) +
  labs(x ="Year", y = "Mortality Rate for Children under 5 years old")

fig <- ggarrange(p1, p2, p3, p4,
                 labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

fig <- annotate_figure(fig,
                top = text_grob("A:Asia, South; B:Sub-Saharan Africa, East; C:Caribbean; D:Europe, Western", color = "black", face = "bold", size = 25),
                bottom = text_grob("GBD study", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
)

ggsave("under5MR.png", plot = fig, height = 15, width = 15, units = "in")


#--------------------------Time-series---------------------------- 

p <- ggplot(data = gbd5q0,
            mapping = aes(x = year, y = neoMR)) +
      geom_line(color = "gray70", 
                     mapping = aes(group = country)) + 
      geom_smooth(mapping = aes(group = gbdRegion),
                       se = FALSE) +
      labs(x = "", y = "Rate per 100,000 population",
          title = "Country-Level Neonatal (ages 0 to 27 days) mortality rate, per 100,000
 by GBD Region, 1970-2010") +
  facet_wrap(~ reorder(gbdRegion, -under5MR, na.rm = TRUE), nrow  = 3)

ggsave("neoMR.png", plot = p, height = 15, width = 15, units = "in")

# MR for 4 populations- neoMR, postneoMR, age1_5MR, under5MR, gbdRegion

gbd5q0_mrs <- gbd5q0 %>% 
  select(year, country,neoMR, postneoMR, age1_5MR, under5MR, gbdRegion)

gbd5q0_long <- pivot_longer(
  gbd5q0_mrs,
  cols = c("neoMR", "postneoMR", "age1_5MR", "under5MR"),
  names_to = "MR_group",
  values_to = "value"
)

gbd5q0_long <- gbd5q0_long %>% filter(gbdRegion == "Asia, South" )

fig <- ggplot(gbd5q0_long, aes(x = year , y = value,  shape = country)) +
  geom_point(color="black", fill="white") +
  facet_wrap(vars(MR_group), nrow = 1)

ggsave("4mrs_asia_south.png", plot = fig, height = 8, width = 8, units = "in")

