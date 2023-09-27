

# Check chirps dataset

# Load libraries
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, rgeos, gtools, readxl, openxlsx, stringr, lubridate)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)


# Load data ---------------------------------------------------------------
path <- './tif/chirps/raw/chirps_apulo-bsin.tif'
rstr <- rast(path)

rstr
plot(rstr)

# Points
pnts <- read.xlsx('./data/tbl/Subecuencas Coordenadas.xlsx')
pnts <- dplyr::select(pnts, Long_, Lat, Subbasin)

head(pnts)

# To extract the values ---------------------------------------------------
names(rstr)
dtes <- names(rstr) %>% str_split(., pattern = '_') %>% map(., 2)
dtes <- unlist(dtes)
year <- str_sub(dtes, 1, 4)
year <- unique(year)

# Function ----------------------------------------------------------------
extract.values <- function(yr){
  
  yr <- year[1]
  
  rst <- rstr[[grep(paste0(yr, '-'), names(rstr), value = FALSE)]]
  vls <- terra::extract(rst, pnts[,1:2])
  vls <- cbind(pnts, vls)
  vls <- as_tibble(vls)
  vls <- gather(vls, var, value, -Long_, -Lat, -Subbasin, -ID)
  vls <- dplyr::select(vls, -ID, Subbasin, Lon = Long_, Lat = Lat, var, value)
  vls <- mutate(vls, date = str_sub(var, 8, nchar(var)))
  
  
  
}


