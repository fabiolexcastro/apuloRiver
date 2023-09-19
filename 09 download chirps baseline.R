


## Extract values from the points
## September 19 th 2023

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, spatialEco, terra, RSAGA, fs, sf, readxl, openxlsx, tidyverse, glue, gtools, RColorBrewer)

install.packages('chirps')
library(chirps)

library(remotes)
remotes::install_github("mikejohnson51/climateR")

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)


