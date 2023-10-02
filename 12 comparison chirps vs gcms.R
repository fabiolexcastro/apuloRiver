

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, remotes, readxl, xlsx, openxlsx, chirps, spatialEco, terra, RSAGA, fs, sf, readxl, openxlsx, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
path <- './data/tbl'
fles <- dir_ls(path)
fles <- as.character(fles)

# Precipitation -----------------------------------------------------------
fles.prec <- grep('prec', fles, value = T)
prec.bsln <- grep('prec-hist', fles.prec, value = T)
prec.ftre <- grep('prec-ftre', fles.prec, value = T)

# Baseline 
prec.bsln <- map(.x = prec.bsln, .f = function(i){
  
  i <- 1 # Correr y borrar
  
  cat('To process: ', i, '\n')
  file <- prec.bsln[i]
  tble <- openxlsx::read.xlsx(file)
  dtes <- openxlsx::convertToDate(tble$date)
  tble <- mutate(tble, date = dtes)
  tble <- as_tibble(tble)
  tble[is.na(tble)] <- 0
  
})

prec.bsln <- map(.x = 1:length(prec.bsln), .f = fucntion(i){
  
  
  
  
  
})



# Future 
prec.ftre <- map(.x = prec.ftre, .f = read.xlsx)

