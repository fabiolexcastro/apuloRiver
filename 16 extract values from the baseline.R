
# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, remotes, readxl, xlsx, openxlsx, chirps, spatialEco, terra, RSAGA, fs, sf, readxl, openxlsx, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

path <- '../data/tif/chirts/bsl/raw'
fles <- dir_ls(path, regexp = '.tif$')
vars <- c('Tmax', 'Tmin')

# Coordinates
bsin <- terra::vect('./shp/Cuenca/Cuenca_Río_Apulo.shp')

# Basin shapefile
pnts <- read.xlsx('./data/tbl/Subcuencas Coordenadas.xlsx')

# Function to aggregate to monthly ----------------------------------------
agg.mnt <- function(var){
  
  var <- 'Tmin' # Correr y borrar
  
  fls <- grep(var, fles, value = T) %>% 
    as.character()
  
  yrs <- fls %>% 
    basename() %>% 
    str_split(., pattern = '_') %>% 
    map(., 2) %>% 
    unlist() %>% 
    str_split(., '\\.') %>% 
    map_chr(2) %>% 
    unique()
  
  rst <- map(.x = 1:length(yrs), .f = function(y){
    
    cat('To process: ', yrs[y], '\n')
    year <- yrs[y]
    fl <- grep(year, fls, value = T)
    
    rs <- map(.x = 1:12, .f = function(m){
      
      cat('To process: ', m, '\n')
      m <- ifelse(m < 10, paste0('0', m), as.character(m))
      f <- grep(paste0(year, '.', m, '.'), fl, value = T)
      r <- rast(f)
      a <- mean(r)
      return(a)
      
    }) %>% 
      reduce(., c)
    
    names(rs) <- glue('{var}_{yrs[y]}-{1:12}')
    
    vl <- terra::extract(rs, pnts[,c('Long_', 'Lat')])
    vl <- cbind(pnts[,c('Subbasin', 'Long_', 'Lat')], vl)
    vl <- as_tibble(vl)
    vl <- gather(vl, var, value, -Subbasin, -Long_, -Lat, -ID)
    vl <- dplyr::select(vl, -ID)
    vl
    return(vl)
    
  })
  
  
  
  
  
  
}





