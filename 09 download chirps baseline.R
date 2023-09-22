


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
chrp <- chrp[[1]]

# Write  ------------------------------------------------------------------
terra::writeRaster(x = chrp, filename = './tif/chirps/raw/chirps_apulo-bsin.tif', overwrite = TRUE)

# Read the points ---------------------------------------------------------
pnts <- read.xlsx('./data/tbl/Subecuencas Coordenadas.xlsx')
pnts <- dplyr::select(pnts, Long_, Lat, Subbasin)

plot(chrp[[5]])
points(pnts$Long_, pnts$Lat, pch = 16, col = 'red')

clls <- terra::extract(chrp[[5]], pnts[,c('Long_', 'Lat')], cell = T)
clls
nlyr(chrp)

# Temperature -------------------------------------------------------------

dtes <- seq(as.Date('1983-01-01', format = '%Y-%m-%d'), as.Date('2014-12-31', format = '%Y-%m-%d'), by = 'day')
dtes <- seq(as.Date('1983-01-01', format = '%Y-%m-%d'), as.Date('1983-12-31', format = '%Y-%m-%d'), by = 'day')

chrx <- get_chirts(object = bsin, dates = dtes, var = 'Tmax', as.raster = TRUE)



#






