
## Download climate models - Historical
## Jul 7th 2023

# Load libraries ----------------------------------------------------------
install.packages('pacman')
library(pacman)
pacman::p_load(terra, fs, sf, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
bsin <- terra::vect('shp/Cuenca/Cuenca_Río_Apulo.shp')
dpto <- terra::vect('shp/Base/dptos.gpkg')

plot(bsin)
plot(dpto)

cund <- dpto[dpto$DPTO_CNMBR == 'CUNDINAMARCA',]

# A simple plot
plot(cund)
plot(bsin, add = T, col = 'red')

# Parameters to download --------------------------------------------------
vars <- c('pr', 'tasmax', 'tasmin')
mdls <- c('ACCESS-CM2', 'ACCESS-ESM1-5', 'BCC-CSM2-MR', 'CanESM5', 'CESM2-WACCM', 'CESM2', 'CMCC-CM2-SR5', 'CMCC-ESM2', 'CNRM-ESM2', 'CNRM-ESM2', 'CNRM-CM6-1', 'CNRM-ESM2-1', 'EC-Earth3-Veg-LR')

# Functions ---------------------------------------------------------------
down <- function(var, mdl, ab1, ab2){

  # Proof
  var <- 'tasmin'
  mdl <- 'ACCESS-CM2'
  ab1 <- 'r1i1p1f1'
  ab2 <- 'gn'
  
  # Start 
  cat('To process: ', var, ' ', mdl, '\n')
  root <- 'https://nex-gddp-cmip6.s3-us-west-2.amazonaws.com/NEX-GDDP-CMIP6'
  urlw <- glue('{root}/{mdl}/historical/{ab1}/{var}/{var}_day_{mdl}_historical_{ab1}_{ab2}_{1974:2014}.nc')
  urlw <- as.character(urlw)
  dirs <- glue('tif/nasa/cmip6/historical/{mdl}/{var}/{basename(urlw)}')
  dirs <- as.character(dirs)
  dir  <- unique(dirname(dirs))
  dir_create(dir)
  
  map(.x = 1:length(urlw), .f = function(i){
    
    cat('To process: ', i, '\n')
    url <- urlw[i]
    out <- dirs[i]
    download.file(url = url, destfile = out, mode = 'wb')
    cat('Done!\n')
    
  })
  
  rm(i)
  
  # To extract by mask (Cundinamarca)
  fles <- as.character(dir_ls(dir, regexp = '.nc$'))
  head(fles)
  
  map(.x = 1:length(fles), .f = function(i){
    
    cat("To extract by mask: ", basename(fles[i]), '\n')
    rst <- rast(fles[i])
    rst <- rotate(rst)
    plot(rst[[1]])
    
  })
  
  
  
  
  
  
  
  
  
  
}





