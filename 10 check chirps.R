

# Check chirps dataset

# Load libraries
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, rgeos, gtools, stringr, lubridate)

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



