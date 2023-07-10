
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

# Parameters to download --------------------------------------------------
vars <- c('pr', 'tasmax', 'tasmin')
mdls <- c('ACCESS-CM2', 'ACCESS-ESM1-5', 'BCC-CSM2-MR', 'CanESM5', 'CESM2-WACCM', 'CESM2', 'CMCC-CM2-SR5', 'CMCC-ESM2', 'CNRM-ESM2', 'CNRM-ESM2', 'CNRM-CM6-1', 'CNRM-ESM2-1', 'EC-Earth3-Veg-LR')

# Functions ---------------------------------------------------------------
down <- function(var, mdl, ab1, ab2){

  # Proof
  var <- 'tasmin'
  mdl <- 'ACCESS-CM2'
  ab1 <- 'r1i1p1f1'
  ab2 <- 'gn'
  
  
  
}





