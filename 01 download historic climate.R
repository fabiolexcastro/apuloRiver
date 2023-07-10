
## Download climate models - Historical
## Jul 7th 2023

# Load libraries ----------------------------------------------------------
install.packages('pacman')
library(pacman)
pacman::p_load(terra, fs, sf, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
bsin <- terra::vect('shp/Cuenca/Cuenca_Río_Apulo.shp')
dpto <- terra::vect('shp/Base/dptos.gpkg')

plot(bsin)
plot(dpto)

cund <- dpto[dpto$DPTO_CNMBR == 'CUNDINAMARCA',]

# A simple plot
plot(cund)
plot(bsin, add = T, col = 'red')
