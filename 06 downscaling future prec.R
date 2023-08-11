
## Downscaling future for precipitation variable
## Aug 11th 2023

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, spatialEco, terra, RSAGA, fs, sf, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)


# Load data ---------------------------------------------------------------

# Basin shapefile 
bsin <- terra::vect('./shp/Cuenca/Cuenca_Río_Apulo.shp')
plot(bsin)

# Climate data list
mdel <- 'EC-Earth3'
path <- './tif/nasa/cmip6/historical/{mdel}/pr/{mdel}'
dirs <- dir_ls(path, type = 'directory')
dir_ls(dirs[1], regexp = '.tif$')