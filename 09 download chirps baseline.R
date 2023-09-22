


## Extract values from the points
## September 19 th 2023

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, remotes, chirps, spatialEco, terra, RSAGA, fs, sf, readxl, openxlsx, tidyverse, glue, gtools, RColorBrewer)

# remotes::install_github("mikejohnson51/climateR")

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
bsin <- terra::vect('./shp/Cuenca/Cuenca_Río_Apulo.shp')
plot(bsin)
bsin <- terra::vect('../shp/Cuenca/Cuenca_Río_Apulo.shp')

chrp <- climateR::getCHIRPS(AOI = st_as_sf(bsin), startDate = '1983-01-01', endDate = '2014-12-31')



