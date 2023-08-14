
## Downscaling future for precipitation variable
## Aug 11th 2023

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, spatialEco, terra, RSAGA, fs, sf, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

# Basin shapefile 
bsin <- terra::vect('./shp/Cuenca/Cuenca_Río_Apulo.shp')
plot(bsin)

# Climate data list
mdel <- 'EC-Earth3'
path <- glue('./tif/nasa/cmip6/historical/{mdel}/pr/{mdel}')#a
dirs.bsln <- dir_ls(path, type = 'directory')
dir_ls(dirs.bsln[1], regexp = '.tif$')

dirs.bsln.raw <- glue('./tif/nasa/cmip6/historical/{mdel}/pr')

path.ftre <- './data/tif/nasa/cmip6/ssp245'

# Function to use ---------------------------------------------------------
down.ftre <- function(dir){
  
  dir <- dirs.bsln[1]
  
  mdl <- mdel
  cat('To process ', basename(dir), '\n')
  
  # Historic dataset (downscaling)
  fls.hst <- dir_ls(dir) %>% 
    as.character() %>% 
    grep('down', ., value = T) %>% 
    mixedsort()
  
  # Historic dataset original 
  fls.hst.raw <- dir_ls(dirs.bsln.raw) %>% 
    as.character() %>% 
    grep('.nc$', ., value = T)
  fls.hst.raw
  
  # Future dataset
  fls.ftr <- dir_ls(path.ftre) %>% 
    grep(mdel, ., value = T) %>% 
    dir_ls() %>% 
    as.character() %>% 
    grep('pr', ., value = T) %>% 
    dir_ls() %>% 
    as.character() %>% 
    grep('.nc$', ., value = T) 
  fls.ftr
  
  # To tidy the years
  yrs.hst <- basename(fls.hst) %>% 
    str_split(., pattern = '_') %>% 
    map_chr(., 8) %>% 
    gsub('.nc$', '', .) %>% 
    unique()
  
  yrs.ftr <- basename(fls.ftr)
  
  
  
}