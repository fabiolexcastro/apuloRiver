

## Downscaling future for temperature variables
## Jul 18th 2023

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, spatialEco, terra, RSAGA, fs, sf, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

path.bsln <- './tif/nasa/cmip6/historical'
path.ftre <- './tif/nasa/cmip6/'
vars <- c('tasmax', 'tasmin')

dirs.bsln <- dir_ls(path.bsln, type = 'directory')
dirs.ftre <- dir_ls(path.ftre, type = 'directory')
ssps <- c('ssp245', 'ssp585')

# Function to use ---------------------------------------------------------

down.ftre <- function(dir){
  
  dir <- dirs[1] # Correr y borrar
  
  cat('To process: ', dir, '\n')
  fls <- dir_ls(dir) %>% 
    as.character()
  
  
}

















