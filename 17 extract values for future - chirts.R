

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, rgeos, readxl, openxlsx, gtools, stringr, glue)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# List the files ----------------------------------------------------------
fles <- dir_ls('./data/tbl') %>% as.character()
fles.tasm.ftre <- grep('tas', fles, value = T) %>% grep('ftre', ., value = T)

fles.tmin.ftre <- grep('tasmin', fles.tasm.ftre, value = T)
fles.tmax.ftre <- grep('tasmax', fles.tasm.ftre, value = T)

# Function to use ---------------------------------------------------------
tidy.tble <- function(file){
  
  # file <- fles.tmin.ftre[[1]] # Correr y borrar
  
  cat('To process: ', basename(file), '\n')
  tble <- read.xlsx(file)
  tble <- as_tibble(tble)
  dtes <- openxlsx::convertToDate(tble$date)
  tble <- mutate(tble, date = dtes)
  tble <- gather(tble, station, value, -date, -model, -variable)
  tble <- mutate(tble, year = year(date), month = month(date))
  smmr <- tble %>% group_by(variable, model, station, year, month) %>% dplyr::summarise(value = mean(value, na.rm = T)) %>% ungroup()
  smmr.year <- smmr %>% group_by(variable, model, station, year) %>% dplyr::summarise(value = mean(value, na.rm = T)) %>% ungroup()
  return(smmr.year)
  
}

# To tidy the tables ------------------------------------------------------
tmin.tble <- map(fles.tmin.ftre, tidy.tble)
tmin.tble <- bind_rows(tmin.tble)
write.csv(tmin.tble, 'data/tbl/values_stts_tasm/Tmin_stts_ftre.csv', row.names = FALSE)

tmax.tble <- map(fles.tmax.ftre, tidy.tble)
tmax.tble <- bind_rows(tmax.tble)
write.csv(tmax.tble, 'data/tbl/values_stts_tasm/Tmax_stts_ftre.csv', row.names = FALSE)


# History GCMs  -----------------------------------------------------------
fles
fles.hist <- grep('hist', fles, value = T)
fles.hist <- grep(paste0(c('tmax', 'tmin'), collapse = '|'), fles.hist, value = T)
fles.hist.tmin <- grep('tmin', fles.hist, value = T)
fles.hist.tmax <- grep('tmax', fles.hist, value = T)


# Minimum temperature
tidy.tble.hist.tmin <- function(file){
  
  # file <- fles.hist[1] # Correr y borrar
  
  cat('To process: ', basename(file), '\n')
  tble <- read.xlsx(file)
  tble <- as_tibble(tble)  
  dtes <- openxlsx::convertToDate(tble$date)
  tble <- mutate(tble, date = dtes)
  nmes <- colnames(tble)
  
  if(any(nmes == 'var')){
    print('var exists')
    colnames(tble)[1] <- 'variable'
  }
  
  varb <- unique(tble$variable)
  tble <- dplyr::select(tble, -variable)
  
  tble <- gather(tble, station, value, -date, -year, -day, -model)
  tble <- mutate(tble, month = month(date))
  tble <- mutate(tble, variable = varb)
  smmr <- tble %>% group_by(variable, station, year, month) %>% dplyr::summarise(value = mean(value, na.rm = T)) %>% ungroup()
  smmr.year <- smmr %>% group_by(variable, station, year) %>% dplyr::summarise(value = mean(value, na.rm = T)) %>% ungroup()
  mdel <- str_split(file, '_') %>% map(2) %>% unlist()
  smmr.year <- mutate(smmr.year, model = mdel)
  cat('Done!\n')
  return(smmr.year)
  
  
}
tbls.hist.tmin <- map(.x = fles.hist.tmin, .f = tidy.tble.hist.tmin)
tbls.hist.tmin <- bind_rows(tbls.hist.tmin)

# Maximum temperature
tidy.tble.hist.tmax <- function(file){
  
  # file <- fles.hist[1] # Correr y borrar
  cat('To process: ', basename(file), '\n')
  tble <- read.xlsx(file)
  tble <- as_tibble(tble)  
  dtes <- openxlsx::convertToDate(tble$date)
  tble <- mutate(tble, date = dtes)
  tble <- gather(tble, var, value, -variable, -year, -date, -day)
  
}


