
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

# Plotting ----------------------------------------------------------------
plot(srtm[[1]])
plot(rraw.bsl[[1]])
plot(rraw.ftr[[1]])
plot(rgwr.bsl[[1]])
plot(rdwn.ftr[[1]])

# Raster to table ---------------------------------------------------------
srtm.tble <- terra::as.data.frame(srtm, xy = T) %>% as_tibble()
rraw.bsl.tbl <- terra::as.data.frame(rraw.bsl, xy = T) %>% as_tibble()
rraw.ftr.tbl <- terra::as.data.frame(rraw.ftr, xy = T) %>% as_tibble()
rgwr.bsl.tbl <- terra::as.data.frame(rgwr.bsl, xy = T) %>% as_tibble()
rdwn.ftr.tbl <- terra::as.data.frame(rdwn.ftr, xy = T) %>% as_tibble()

# Selecting just January  -------------------------------------------------
rraw.bsl.tbl <- rraw.bsl.tbl %>% dplyr::select(1:33)
rraw.ftr.tbl <- rraw.ftr.tbl %>% dplyr::select(1:33)
rgwr.bsl.tbl <- rgwr.bsl.tbl %>% dplyr::select(1:33)
rdwn.ftr.tbl <- rdwn.ftr.tbl %>% dplyr::select(1:33)
























