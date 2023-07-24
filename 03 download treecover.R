
## Downscaling treecover data
## Jul 14 th 2023

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, gfcanalysis, spatialEco, terra, fs, sf, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
bsin <- terra::vect('shp/Cuenca/Cuenca_Río_Apulo.shp')
dpto <- terra::vect('shp/Base/dptos.gpkg')
cund <- dpto[dpto$DPTO_CNMBR == 'CUNDINAMARCA',]

# Download Hansen deforestation -------------------------------------------
tles <- calc_gfc_tiles(aoi = st_as_sf(cund))
dir.create('tmpr')
frst <- gfcanalysis::download_tiles(tiles = tles, output_folder = 'tmpr')
frst <- gfcanalysis::extract_gfc(aoi = tles, data_folder = 'tmpr')
frst <- rast(frst)
frst <- crop(frst, cund)
frst <- mask(frst, cund)

dir.create('tif/forest', recursive = T)










