
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
extr.vles <- function(sspe, mdel){
  
  sspe <- 'ssp245'
  mdel <- mdls[1]
  
  fles <- grep(sspe, dir_ls(path), value = TRUE) %>% 
    dir_ls() %>% 
    as.character() %>% 
    grep(mdel, ., value = T) %>% 
    dir_ls() %>% 
    as.character() %>% 
    map(., dir_ls) %>% 
    unlist() %>% 
    as.character() %>% 
    grep('down', ., value = T) %>% 
    map(., dir_ls) %>% 
    unlist() %>% 
    as.character() %>% 
    grep('.tif$', ., value = T)
  
  map(.x = 1:3, .f = function(v){
    
    v <- 1 # Correr y borrar
    cat('To process:', varb[v], '\n')
    fls <- grep(varb[v], fles, value = T)
    var <- varb[v]
    
  })
  
  
  
  
  
}


#



