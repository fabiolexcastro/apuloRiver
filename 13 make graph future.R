
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, rgeos, readxl, openxlsx, gtools, stringr, glue)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

path <- './data/tif/nasa/cmip6'
ssps <- as.character(dir_ls(path, type = 'directory'))
vars <- c('pr', 'tasmin', 'tasmax')
mdls <- c('ACCESS-CM2', 'CanESM5', 'EC-Earth3', 'INM-CM4-8', 'MRI-ESM2-0')

# Stations
pnts <- read.xlsx('./data/tbl/Subcuencas Coordenadas.xlsx')
pnts <- dplyr::select(pnts, Long_, Lat, Subbasin)

# Function to use ---------------------------------------------------------
extr.vles <- function(sspe, varb){
  
  # sspe <- 'ssp245'
  # varb <- 'pr'
  
  cat('To process: ', sspe, ' ', varb, '\n')
  fles <- dir_ls('./data/tbl', regexp = '.xlsx$')
  fles <- grep(varb, fles, value = T)
  fles <- grep(sspe, fles, value = T)
  fles <- as.character(fles)
  fles
  tble <- map(fles, read.xlsx)
  tble <- bind_rows(tble)
  table(tble$model)
  dtes <- openxlsx::convertToDate(tble$date)
  tble <- mutate(tble, date = dtes)
  tble <- mutate(tble, date = as.Date(date, format = '%Y-%m-%d'))
  colnames(tble) <- c('var', 'model', 'date', 'v1', 'v2', 'v3', 'v4')
  tble <- as_tibble(tble)
  tble <- mutate(tble, ssp = sspe)
  cat('Done!\n')
  
}

# Precipitation -----------------------------------------------------------
prec <- map_dfr(.x = 1:2, .f = function(s){
  extr.vles(sspe = ssps[s], varb = 'prec')
})





