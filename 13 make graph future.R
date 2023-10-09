
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
  
  sspe <- 'ssp245'
  varb <- 'pr'
  
  fles <- dir_ls('./data/tbl', regexp = '.xlsx$')
  fles <- grep(varb, fles, value = T)
  fles <- grep(sspe, fles, value = T)
  fles <- as.character(fles)
  fles
  tble <- read.xlsx(fles)
  head(tble)

  
  
  
  
  
}


#



