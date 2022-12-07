# ## Purpose: HMS 520 Final Project about Child Mortality
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


# load data

dataDir <- "~/Desktop/HMS520_final_project/data/"
gbd5q0 <- NULL


for (x in seq(1970, 2010, 10)) {
  gbd5q0 <- rbind(gbd5q0, read.csv(paste0(dataDir, "gbdChildMortality_", x, "s.csv"), stringsAsFactors = FALSE))
}

gbd5q0 <- merge(gbd5q0, read.csv(paste0(dataDir, "countryCovars.csv"), stringsAsFactors = FALSE),
                   by.x = c("iso", "year"), by.y = c("iso3", "year"))





