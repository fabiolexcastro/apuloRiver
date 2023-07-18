

## Downscaling historic climate
## Jul 18th 2023

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, terra, fs, sf, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
bsin <- terra::vect('shp/Cuenca/Cuenca_Río_Apulo.shp')
dpto <- terra::vect('shp/Base/dptos.gpkg')
cund <- dpto[dpto$DPTO_CNMBR == 'CUNDINAMARCA',]

# Download SRTM - Elevatr library -----------------------------------------





