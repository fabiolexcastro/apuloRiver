

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
path.ftre <- './data/tif/nasa/cmip6/ssp245'
vars <- c('tasmax', 'tasmin')

dirs.bsln <- dir_ls(path.bsln, type = 'directory')
ssps <- c('ssp245', 'ssp585')

dirs.bsln.raw <- './tif/nasa/cmip6/historical/'

# Function to use ---------------------------------------------------------

down.ftre <- function(dir){
  
  dir <- dirs.bsln[2] # Correr y borrar

  mdl <- basename(dir)
  cat('To process: ', dir, '\n')
  
  # Historic dataset (downscaling)
  fls.hst <- dir_ls(dir) %>% 
    as.character() %>% 
    grep('tas', ., value = T) %>% 
    map(.x = ., dir_ls, regexp = '.nc$') %>% 
    map(.x = ., as.character) %>% 
    unlist() %>% 
    as.character()

  # Historic dataset (original)
  fls.hst.raw <- dir_ls(dirs.bsln.raw) %>% 
    grep(mdl, ., value = T) %>% 
    dir_ls(., type = 'directory') %>% 
    grep('tas', ., value = T) %>% 
    map(.x = ., dir_ls, regexp = '.nc$') %>% 
    unlist() %>%
    as.character() %>%
    grep('/cund_tas', ., value = T)
  
  # Future dataset 
  fls.ftr <- dir_ls(path.ftre) %>% 
    grep(mdl, ., value = T) %>% 
    dir_ls(.) %>% 
    as.character() %>% 
    grep('tas', ., value = T) %>% 
    map(., dir_ls, regexp = '.nc$') %>% 
    unlist() %>% 
    as.character()

  # To tidy the years
  yrs.hst <- basename(fls.hst) %>% 
    str_split(., pattern = '_') %>%
    map_chr(., 8) %>% 
    gsub('.nc$', '', .) %>% 
    unique()

  yrs.hst.raw <- basename(fls.hst.raw) %>% 
    str_split(., pattern = '_') %>% 
    map_chr(., 8) %>% 
    gsub('.nc$', '', .) %>% 
    unique()

  yrs.ftr <- basename(fls.ftr) %>% 
    str_split(., pattern = '_') %>% 
    map_chr(., 8) %>% 
    gsub('.nc$', '', .) %>% 
    unique()
  
  
  
}

















