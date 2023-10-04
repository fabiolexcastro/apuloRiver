

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
mdls <- c('ACCESS-CM2', 'CanESM5', 'EC-Earth3', 'INM-CM4-8', 'MRI-ESM2-0')
prec.bsln <- grep('prec-hist', fles.prec, value = T)
prec.bsln <- grep(paste0(mdls, collapse = '|'), prec.bsln, value = T)
prec.ftre <- grep('prec-ftre', fles.prec, value = T)
prec.ftre <- grep(paste0(mdls, collapse = '|'), prec.ftre, value = T)

# Baseline 
prec.bsln <- map(.x = 1:length(prec.bsln), .f = function(i){
  
  cat('To process: ', i, '\n')
  file <- prec.bsln[i]
  tble <- openxlsx::read.xlsx(file)
  dtes <- openxlsx::convertToDate(tble$date)
  tble <- mutate(tble, date = dtes)
  tble <- as_tibble(tble)
  tble[is.na(tble)] <- 0
  cat('Done!\n')
  return(tble)
  
})

prec.bsln <- bind_rows(prec.bsln)
unique(prec.bsln$model)
write.xlsx(prec.bsln, './data/tbl/values-sts_GCMs_prec-hist.xlsx')

# Baseline
prec.chrp <- grep('values_chirps-bsln_raw.csv', fles, value = T)
prec.chrp <- read_csv(prec.chrp)
head(prec.chrp)
tail(prec.chrp)

c('Lon', 'Lat', 'Subbasin', 'value', 'date')

colnames(prec.chrp)
colnames(prec.bsln)

obsr <- prec.chrp %>% dplyr::select(Subbasin, date, value)
mdel <- prec.bsln %>% dplyr::select(model, date, `1`, `2`, `3`, `4`)
mdel <- mdel %>% gather(Subbasin, value, -model, -date)

obsr <- mutate(obsr, model = 'Baseline', type = 'Real')
obsr <- relocate(obsr, model, type, date, Subbasin, value)
obsr
mdel <- mutate(mdel, type = 'Modelled')
mdel <- relocate(mdel, model, type, date, Subbasin, value)
mdel <- mutate(mdel, Subbasin = as.character(Subbasin))
obsr <- mutate(obsr, Subbasin = as.character(Subbasin))
prec <- bind_rows(obsr, mdel)
prec %>% filter(Subbasin == '1' & date == '1983-01-01')

write.xlsx(prec, './data/values_stts_GCMs-BSLN_prec.xlsx')

# Comparison
p_load(hydroGOF)
calcNASH <- function(bs){
  
  bs <- 1
  cat('To process: ', bs, '\n')
  
  tbl <- filter(prec, Subbasin == as.character(bs))
  cmb <- tibble(obsr = 'Baseline', model = mdls)
  
  map(.x = 1:nrow(cmb), .f = function(i){
    
    cm <- cmb[i,]
    md <- cm$model
    tb <- tbl %>% filter(model %in% c('Baseline', md))
    tb <- tb %>% spread(model, value)
    
    
  })
  
  
  
}



