
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, rgeos, readxl, openxlsx, gtools, stringr, glue)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

# Vector data
bsin <- vect('./shp/Cuenca/Cuenca_Río_Apulo.shp')

# Raster data
srtm <- rast('./tif/srtm/fill/srtm_z07_fill.tif')
rraw.bsl <- rast('./tif/nasa/cmip6/historical/ACCESS-CM2/tasmax/cund_tasmax_day_ACCESS-CM2_historical_r1i1p1f1_gn_1974.nc')
rraw.ftr <- rast('./data/tif/nasa/cmip6/ssp245/ACCESS-CM2/tasmax/cund_tasmax_day_ACCESS-CM2_ssp245_r1i1p1f1_gn_2015.nc')

rgwr.bsl <- rast('./tif/nasa/cmip6/historical/ACCESS-CM2/tasmax/down-cund_tasmax_day_ACCESS-CM2_historical_r1i1p1f1_gn_1974.nc')
rdwn.ftr <- rast('./data/tif/nasa/cmip6/ssp245/ACCESS-CM2/tasmax/down/tasmax_2015.tif')

# Extract by mask ---------------------------------------------------------
srtm <- terra::crop(srtm, bsin)
rraw.bsl <- terra::crop(rraw.bsl, bsin)
rraw.ftr <- terra::crop(rraw.ftr, bsin)
rgwr.bsl <- terra::crop(rgwr.bsl, bsin)
rdwn.ftr <- terra::crop(rdwn.ftr, bsin)

plot(srtm[[1]])
plot(rraw.bsl[[1]])















