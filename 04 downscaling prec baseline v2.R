
## Downscaling historic climate - precipitation
## Jul 18th 2023

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, spatialEco, terra, RSAGA, fs, sf, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# R saga environment
# envr <- rsaga.env(path = 'C:/Program Files/SAGA')
envr <- rsaga.env(path = 'C:/SAGA/saga-8.0.0_x64')

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
  
  # dir <- dirs[1] # Correr y borrar
  
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
    yea <- gsub('.nc', '', map_chr(str_split(basename(fle), '_'), 8))
    
    rsl <- map(.x = 1:nlyr(rst), .f = function(j){
      
      cat('To process: ', j, '\n')
      tow <- glue('tmpr/to-saga/{yea}'); dir_create(tow)
      terra::writeRaster(x = rst[[j]], filename = glue('{tow}/{names(rst[[j]])}.tif'), overwrite = TRUE)
      finp <- glue('{tow}/{names(rst[[j]])}.tif')
      fout <- glue('{tow}/down_{names(rst[[j]])}.tif')
      
      trr <- rsaga.geoprocessor(lib = 'statistics_regression', module = 'GWR for Grid Downscaling',
        param = list(PREDICTORS = 'tif/forest/hansen-stck_raw.tif',
                     REGRESSION = fout,
                     DEPENDENT = finp), env = envr)
      
      plot(rast(fout))
      cat('Done!\n')
           
    }) 
    

  })
  
  
}


# Check the results  ------------------------------------------------------
dirs <- dir_ls('tmpr/to-saga')
year <- basename(dirs)
mdel <- 'ACCESS-CM2'

to.copy <- function(dir){
  
  cat('To process: ', basename(dir), '\n')
  fls <- dir_ls(dir, regexp = '.tif$')
  fls <- as.character(fls)
  fls <- grep('down', fls, value = T)
  fls <- mixedsort(fls)
  
  cat('Read as a raster file\n')
  rst <- map(fls, function(i){
    cat(i, '\t')
    i %>% 
      terra::rast() %>% 
      terra::crop(., bsin) %>% 
      terra::mask(., bsin)
  })
  rst <- reduce(rst, c)
  dts <- seq(as.Date(paste0(year, '-01-01'), format = '%Y-%m-%d'), as.Date(paste0(year, '-12-31'), format = '%Y-%m-%d'), by = 'day')
  names(rst) <- dts
  
  
  

  
}







