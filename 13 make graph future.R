
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, rgeos, readxl, openxlsx, gtools, stringr, glue)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
path <- './data/tif/nasa/cmip6'
ssps <- as.character(dir_ls(path, type = 'directory'))
vars <- c('pr', 'tasmin', 'tasmax')
mdls <- c('ACCESS-CM2', 'CanESM5', 'EC-Earth3', 'INM-CM4-8', 'MRI-ESM2-0')

# Stations
pnts <- read.xlsx('./data/tbl/Subcuencas Coordenadas.xlsx')
pnts <- dplyr::select(pnts, Long_, Lat, Subbasin)

# Function to use ---------------------------------------------------------
extr.vles <- function(sspe, varb){
  
  # sspe <- 'ssp245'
  # varb <- 'pr'
  
  cat('To process: ', sspe, ' ', varb, '\n')
  fles <- dir_ls('./data/tbl', regexp = '.xlsx$')
  fles <- grep(varb, fles, value = T)
  fles <- grep(sspe, fles, value = T)
  fles <- as.character(fles)
  fles
  tble <- map(fles, read.xlsx)
  tble <- bind_rows(tble)
  table(tble$model)
  dtes <- openxlsx::convertToDate(tble$date)
  tble <- mutate(tble, date = dtes)
  tble <- mutate(tble, date = as.Date(date, format = '%Y-%m-%d'))
  colnames(tble) <- c('var', 'model', 'date', 'v1', 'v2', 'v3', 'v4')
  tble <- as_tibble(tble)
  tble <- mutate(tble, ssp = sspe)
  cat('Done!\n')
  return(tble)
  
}

# Precipitation -----------------------------------------------------------
mdls <- c('ssp245', 'ssp585')
prec <- map_dfr(.x = 1:2, .f = function(s){
  extr.vles(sspe = mdls[s], varb = 'prec')
})

prec <- gather(prec, station, value, -var, -model, -date, -ssp)
smmr.prec <- prec %>% 
  mutate(year = year(date), 
         month = month(date)) %>% 
  group_by(var, model, ssp, station, year) %>%
  dplyr::summarise(value = sum(value, na.rm = T)) %>% 
  ungroup() 

make.graph <- function(sp){
  
  # sp <- 'ssp245'
  # tbl <- filter(smmr.prec, ssp == sp)
  
  ggl <- ggplot(data = tbl, aes(x = year, y = value, col = model)) + 
    geom_line() + 
    facet_wrap(~station) + 
    labs(x = 'Año', y = 'Prec. (mm)', col = 'GCM') +
    ggtitle(label = paste0('Precipitación - ', sp)) +
    theme_minimal() + 
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5, size = 7), 
          axis.text.x = element_text(size = 7), 
          plot.title = element_text(face = 'bold', hjust = 0.5),
          strip.text = element_text(face = 'bold'), 
          legend.position = 'bottom')
  
  ggsave(plot = ggl, filename = glue('../png/prec_ftr_{sp}.png'), units = 'in', width = 9, height = 7, dpi = 300)
  cat('Done!\n')
  
}



