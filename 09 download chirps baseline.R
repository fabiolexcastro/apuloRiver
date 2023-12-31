

## Extract values from the points
## September 19 th 2023

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, remotes, chirps, spatialEco, terra, RSAGA, fs, sf, readxl, openxlsx, tidyverse, glue, gtools, RColorBrewer)

# remotes::install_github("mikejohnson51/climateR")
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
bsin <- terra::vect('./shp/Cuenca/Cuenca_Río_Apulo.shp')
plot(bsin)
bsin <- terra::vect('../shp/Cuenca/Cuenca_Río_Apulo.shp')

chrp <- climateR::getCHIRPS(AOI = st_as_sf(bsin), startDate = '1983-01-01', endDate = '2014-12-31')
chrp <- chrp[[1]]

# Write  ------------------------------------------------------------------
terra::writeRaster(x = chrp, filename = './tif/chirps/raw/chirps_apulo-bsin.tif', overwrite = TRUE)

# Read the points ---------------------------------------------------------
pnts <- read.xlsx('./data/tbl/Subecuencas Coordenadas.xlsx')
pnts <- dplyr::select(pnts, Long_, Lat, Subbasin)

plot(chrp[[5]])
points(pnts$Long_, pnts$Lat, pch = 16, col = 'red')

clls <- terra::extract(chrp[[5]], pnts[,c('Long_', 'Lat')], cell = T)
clls
nlyr(chrp)

# Temperature -------------------------------------------------------------
dtes <- seq(as.Date('1983-01-01', format = '%Y-%m-%d'), as.Date('2014-12-31', format = '%Y-%m-%d'), by = 'day')
path <- 'https://data.chc.ucsb.edu/products/CHIRTSdaily/v1.0/global_tifs_p05/'
vars <- c('Tmin', 'Tmax')

down.chrt <- function(varb, year){
  
  # varb <- vars[1]
  # year <- 1983
  
  cat('To process: ', varb, ' ', year, '\n')
  seqn <- seq(as.Date(glue('{year}-01-01'), format = '%Y-%m-%d'), as.Date(glue('{year}-12-31'), format = '%Y-%m-%d'), by = 'day')
  seqn <- gsub('-', '.', seqn)
  urls <- as.character(glue('{path}{varb}/{year}/{varb}.{seqn}.tif'))
  
  map(.x = 1:length(urls), .f = function(u){
    
    cat('To download: ', basename(urls[u]), '\n')
    lnk <- urls[u]
    out <- glue('../data/tif/chirts/bsl/raw')
    
    dir_create(out)
    out <- glue('{out}/{basename(lnk)}')
    
    download.file(url = lnk, destfile = out, mode = 'wb')
    
    rst <- terra::rast(out)
    plot(rst)
    rst <- terra::crop(rst, bsin)
    terra::writeRaster(x = rst, filename = glue('../data/tif/chirts/bsl/raw/bsin_{basename(out)}'), overwrite = TRUE)
    
    file.remove(out)
    rm(lnk, out, rst)
    gc(reset = TRUE)
    
    Sys.sleep(3)
    
  })
  
  cat('Finish!\n')
  
  
}

# To download CHIRT - All in one step
yrs <- 1974:2014

# Minimum temperature
map(.x = 1:length(year), .f = function(i){
  down.chrt(varb = 'Tmin', year = yrs[i])  
})

# Maximum temperature
map(.x = 1:length(year), .f = function(i){
  down.chrt(varb = 'Tmax', year = yrs[i])  
})







