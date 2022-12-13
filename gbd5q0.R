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
df <- NULL

# Combine multiple years datasets
for (x in seq(1970, 2010, 10)) {
  df <- rbind(df, read.csv(paste0(dataDir, "gbdChildMortality_", x, "s.csv"), stringsAsFactors = FALSE))
}

# Merge covariate dataset to gbd5q0
df <- merge(df, read.csv(paste0(dataDir, "countryCovars.csv"), stringsAsFactors = FALSE),
                   by.x = c("iso", "year"), by.y = c("iso3", "year"))
write.csv(df, "data/childMortAndCovars.csv")
gbd5q0 <- read.csv("data/childMortAndCovars.csv")

#--------------------------Time-series---------------------------- 

# Under 5 mortality rate in South Aisa 1970-2010
fig <- gbd5q0 %>% filter(gbdRegion == "Asia, South") %>%
  ggplot(aes(x = year, y = under5MR)) +
  geom_point(color = "blue", size = 4) + 
  facet_wrap(vars(country)) +
  labs(x ="Year", y = "Mortality Rate for Children under 5 years old") 

ggsave("under5MR_south_asia.png", plot = fig, height = 15, width = 15, units = "in")

# Mortality rate at GBD Region 1970-2010
p <- ggplot(data = gbd5q0,
            mapping = aes(x = year, y = under5MR)) +
      geom_line(color = "gray70", 
                     mapping = aes(group = country)) + 
      geom_smooth(mapping = aes(group = gbdRegion),
                       se = FALSE) +
      labs(x = "", y = "Rate per 100,000 population",
          title = "Mortality rate among children ages 0 to 4.999, per 100,000 by GBD Region, 1970-2010") +
  facet_wrap(~ reorder(gbdRegion, -under5MR, na.rm = TRUE), nrow  = 3)

ggsave("under5MR.png", plot = p, height = 15, width = 15, units = "in")

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




