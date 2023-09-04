
## Extract values from the points
## September 09th 2023

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, spatialEco, terra, RSAGA, fs, sf, readxl, openxlsx, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load libraries ----------------------------------------------------------
tble <- read.xlsx('./data/tbl/Subcuencas Coordenadas.xlsx')
head(tble)
tble <- as_tibble(tble)
tble

# Spatial data
bsin <- terra::vect('./shp/Cuenca/Cuenca_Río_Apulo.shp')

# A simple plot
plot(bsin)
points(tble$Long_, tble$Lat, pch = 16, col = 'red')

# Directories
vars <- c('pr', 'tasmax', 'tasmin')
path.bsln <- './tif/nasa/cmip6/historical'
dirs.bsln <- dir_ls(path.bsln, type = 'directory') %>% as.character()
path.bsln
dirs.bsln

# Functions  --------------------------------------------------------------
extrac.prec.hist <- function(dir){
  
  dir <- dirs.bsln[1] # Correr y comentar 
  
  cat('To process: ', dir, '\n')
  drs <- dir_ls(dir) %>% 
    grep('pr', ., value = T) %>% 
    dir_ls(., type = 'directory') %>% 
    dir_ls(., type = 'directory') %>% 
    as.character()
  
}



