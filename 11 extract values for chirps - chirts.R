

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, remotes, chirps, spatialEco, terra, RSAGA, fs, sf, readxl, openxlsx, tidyverse, glue, gtools, RColorBrewer)

# remotes::install_github("mikejohnson51/climateR")
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
bsin <- terra::vect('./shp/Cuenca/Cuenca_Río_Apulo.shp')
path.chrt <- '../data/tif/chirts/bsl/raw'
fles <- dir_ls(path.chrt, regexp = '.tif$')
year <- 1983:2014
pnts <- read.xlsx('./data/tbl/Subcuencas Coordenadas.xlsx')

head(pnts)

pnts <- pnts %>% dplyr::select(Subbasin, Long_, Lat)

extr.vles <- function(yr, vr){
  
  yr <- 1983 # Correr y borrar
  vr <- 'Tmin'
  
  cat('To process: ', yr, '\n')
  fls <- grep(yr, fles, value = T)
  fls <- grep(vr, fls, value = T)
  fls <- as.character(fls)
  rst <- rast(fls)
  vls <- terra::extract(v)
  
}