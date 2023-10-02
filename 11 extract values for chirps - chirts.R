

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

# Functions to use --------------------------------------------------------
extr.vles <- function(yr, vr){
  
  # yr <- 1983 # Correr y borrar
  # vr <- 'Tmin'
  
  cat('To process: ', yr, '\n')
  fls <- grep(yr, fles, value = T)
  fls <- grep(vr, fls, value = T)
  fls <- grep('bsin_', fls, value = T)
  fls <- as.character(fls)
  rst <- rast(fls)
  vls <- terra::extract(rst, pnts[,c('Long_', 'Lat')])
  vls <- cbind(pnts, vls)
  vls <- as_tibble(vls)
  library(tidyr)
  library(stringr)
  vls <- gather(vls, var, value, -Subbasin, -Long_, -Lat, -ID)
  vls <- mutate(vls, date = str_sub(var, 6, nchar(var)))
  vls <- mutate(vls, date = as.Date(date, format = '%Y.%m.%d'))
  vls <- mutate(vls, variable = str_sub(var, 1, 4))
  vls <- mutate(vls, variable = tolower(variable))
  vls <- dplyr::select(vls, Subbasin, Long_, Lat, variable, date, value)
  cat('Done!\n')
  return(vls)
  
}

# To extract the values  --------------------------------------------------

# Minimum temperatura
tmin.vles <- map(.x = 1:length(year), .f = function(i){
  extr.vles(yr = year[i], vr = 'Tmin')
})

tmin.vles <- bind_rows(tmin.vles)
write.csv(tmin.vles, './tbl_chirts-tmin.csv', row.names = FALSE)#

# Maximum temperature 
tmax.vles <- map(.x = 1:length(year), .f = function(i){
  extr.vles(yr = year[i], vr = 'Tmax')
})
tmax.vles <- bind_rows(vles)
write.csv(tmax.vles, './tbl_chirts-tmax.csv', row.names = FALSE)



