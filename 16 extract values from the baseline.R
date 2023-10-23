


# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, remotes, readxl, xlsx, openxlsx, chirps, spatialEco, terra, RSAGA, fs, sf, readxl, openxlsx, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

path <- '../data/tif/chirts/bsl/raw'
fles <- dir_ls(path, regexp = '.tif$')
vars <- c('Tmax', 'Tmin')

# Coordinates
bsin <- terra::vect('./shp/Cuenca/Cuenca_Río_Apulo.shp')

# Basin shapefile
pnts <- read.xlsx('./data/tbl/Subcuencas Coordenadas.xlsx')


