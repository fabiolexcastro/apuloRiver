


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
mask <- terra::rast('tif/srtm/fill/srtm_z07_fill.tif')
frst <- terra::resample(frst, mask, method = 'bilinear')

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
    grep('.nc$', ., value = T) %>% 
    as.character()
  
  map(.x = 1:length(fls), .f = function(i){
    
    cat('To process: ', basename(fls[i]), '\n')
    fle <- fls[i]
    fle <- as.character(fle)
    rst <- terra::rast(fle)
    rst <- rst * 86400
    
    rsl <- map(.x = 1:nlyr(rst), .f = function(j){
      
      cat(j, '\t')
      r <- rst[[j]]
      v <- as.data.frame(r, xy = T) %>% pull(3) %>% unique()
      
      if(length(v) == 1 | max(v) < 1.8){
        cat('Values = 1\n')
        d <- terra::resample(r, frst, method = 'bilinear')
      } else {
        cat('Values > 1\n')
        d <- raster.downscale(x = frst, y = r, p = 0.9, se = F)
        d <- d$downscale
      }
      d <- terra::crop(d, bsin)
      d <- terra::mask(d, bsin)
      return(d)
      
    }) %>% 
      reduce(., c)
    
    yea <- fle %>% basename() %>% str_split(., '_') %>% map_chr(8) %>% parse_number()
    dts <- seq(as.Date(glue('{yea}-01-01'), format = '%Y-%m-%d'), as.Date(glue('{yea}-12-31'), format = '%Y-%m-%d'), by = 'day')
    terra::time(rsl) <- dts
    out <- gsub('cund_', 'down-cund_', fle)
    terra::writeRaster(x = rsl, filename = out, overwrite = TRUE)
    cat('Done!\n')
    
  })
  
  
}
