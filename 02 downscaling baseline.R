

## Downscaling historic climate
## Jul 18th 2023

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, terra, fs, sf, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
bsin <- terra::vect('shp/Cuenca/Cuenca_Río_Apulo.shp')
dpto <- terra::vect('shp/Base/dptos.gpkg')
cund <- dpto[dpto$DPTO_CNMBR == 'CUNDINAMARCA',]

# Download SRTM - Elevatr library -----------------------------------------
sr.07 <- elevatr::get_elev_raster(locations = st_as_sf(cund), z = 7)
sr.07 <- rast(sr.07)
sr.07 <- sr.07 * 1
sr.07 <- crop(sr.07, cund)
sr.07 <- mask(sr.07, cund)

dir.create('tif/srtm/raw', recursive = T)
terra::writeRaster(x = sr.07, filename = 'tif/srtm/raw/srtm_z07.tif')

# Read the fill SRTM ------------------------------------------------------
sr.07 <- rast('tif/srtm/fill/srtm_z07_fill.tif')
sr.07 <- sr.07[[1]]
sr.07[sr.07 < 0] <- 0
plot(sr.07)
sr.07 <- crop(sr.07, cund)
sr.07 <- mask(sr.07, cund)

terra::writeRaster(x = sr.07, filename = 'tif/srtm/raw/srtm_z07.tif', overwrite = TRUE)

# Read the results --------------------------------------------------------
srtm <- rast('tif/srtm/fill/srtm_z07_fill.tif')
plot(srtm)

# List the files  ---------------------------------------------------------

path <- 'tif/nasa/cmip6/historical'
dirs <- dir_ls(path, type = 'directory')
mdls <- basename(dirs)

# Temperature 
d <- 1 # Correr y borrar

map(.x = 1:length(dirs), .f = function(d){
  
  dir <- dirs[d]
  dir <- dir_ls(dir, type = 'directory')
  dir <- grep('tas', dir, value = T)
  dir <- as.character(dir)
  fls <- map(dir, dir_ls, regexp = '.nc$')
  fls <- unlist(fls)
  fls <- grep('cund', fls, value = T)
  
  f <- 1 # Correr y borrar
  
  rst <- map(.x = 1:length(fls), .f = function(f){
    
    fl <- fls[f]
    
  })
  
})






