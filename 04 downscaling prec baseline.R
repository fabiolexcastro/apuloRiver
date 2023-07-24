


## Downscaling historic climate - precipitation
## Jul 18th 2023

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, spatialEco, terra, fs, sf, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
bsin <- terra::vect('shp/Cuenca/Cuenca_Río_Apulo.shp')
dpto <- terra::vect('shp/Base/dptos.gpkg')
cund <- dpto[dpto$DPTO_CNMBR == 'CUNDINAMARCA',]

frst <- terra::rast('tif/forest/hansen-stck_raw.tif')

# List the files
path <- 'tif/nasa/cmip6/historical'
dirs <- dir_ls(path, type = 'directory')
mdls <- basename(dirs)

# Function to use ---------------------------------------------------------
down <- function(dir){
  
  dir <- dirs[1] # Correr y borrar 
  
  cat('To process: ', basename(dir), '\n')
  fls <- dir_ls(dir) %>% 
    grep('pr', ., value = T) %>% 
    as.character() %>% 
    dir_ls(.) %>% 
    grep('.nc$', ., value = T)
  
  map(.x = 1:length(fls), .f = function(i){
    
    i <- 1 # Correr y borrar 
    
    cat('To process: ', basename(fls[i]), '\n')
    fle <- fls[i]
    fle <- as.character(fle)
    rst <- terra::rast(fle)
    rst <- rst * 86400
    
    rsl <- map(.x = 1:nlyr(rst), .f = function(j){
      
      cat(j, '\t')
      r <- rst[[j]]
      d <- raster.downscale(x = frst, y = r)$downscale
      return(d)
      
    }) %>% 
      reduce(., c)
    
  })
  
  
}
