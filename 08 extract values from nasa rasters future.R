
## Extract values from the points
## September 15 th 2023

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, spatialEco, terra, RSAGA, fs, sf, readxl, openxlsx, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
tble <- read.xlsx('./data/tbl/Subcuencas Coordenadas.xlsx')
tble <- as_tibble(tble)
tble

# Spatial data
bsin <- terra::vect('./shp/Cuenca/Cuenca_Río_Apulo.shp')

# A simple plot
plot(bsin)
points(tble$Long_, tble$Lat, pch = 16, col = 'red')

# Directories
vars <- c('pr', 'tasmax', 'tasmin')
path.ftre <- './data/tif/nasa/cmip6'
dirs.ssps <- dir_ls(path.ftre, type = 'directory') %>% as.character()
mdls <- dir_ls(dirs.ssps)[1:5]

# Functions  --------------------------------------------------------------
extrac.prec.ftre <- function(ssp, mdl){
  
  dir <- dirs.ssps[1] # Correr y comentar
  mdl <- mdls[1] # Correr y comentar 
  
  fles <- dirs.ssps %>% 
    dir_ls(., type = 'directory') %>% 
    grep(mdl, ., value = T) %>% 
    as.character() %>% 
    dir_ls() %>% 
    grep('pr', ., value = T) %>% 
    dir_ls(type = 'directory') %>% 
    grep('down',., value = T) %>% 
    dir_ls(., regexp = '.tif$') %>% 
    as.character()
    
  tbls <- map(.x = 1:length(fles), .f = function(i){
    
    i <- 1 # Correr y comentar
    
    cat('To make the year number: ', i, '\n')
    fle <- fles[i] 
    rst <- terra::rast(fle)
    plot(rst)
    
    vls <- map(.x = 1:nlyr(rst), .f = function(x){
      rst[[x]] %>%
        terra::extract(., tble[,c('Long_', 'Lat')]) %>% 
        as_tibble() %>% 
        gather(var, value, -ID)
    }) %>% 
      bind_rows()
    
    vls <- mutate(vls, value = ifelse(is.na(value), 0, value))
    vls <- mutate(vls, model = mdl, date = var)
    vls <- mutate(vls, var = 'prec')
    vls <- spread(vls, ID, value)
    
  })
  
  
 
}