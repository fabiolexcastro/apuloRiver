

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, remotes, readxl, xlsx, openxlsx, chirps, spatialEco, terra, RSAGA, fs, sf, readxl, openxlsx, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
path <- './data/tbl/values_stts_tasm'
fles <- dir_ls(path)
fles <- as.character(fles)
fles <- grep('ftre', fles, value = T)

# Tidy the table ----------------------------------------------------------
tmin <- grep('Tmin', fles, value = T) %>% read_csv()
tmax <- grep('Tmax', fles, value = T) %>% read_csv()

tmin <- read_csv('Tmin_stts_ftre.csv')


# Function ----------------------------------------------------------------
make.graph <- function(tble){
  
  tble <- tmin
  
  smmr <- tble %>% group_by(variable, model, station, year) %>% dplyr::summarise(value = mean(value)) %>% ungroup()
  smmr %>% filter(model == 'ACCESS-CM2') %>% filter(station == 1) %>% View()
  
  # Start the analysis
  glne <- ggplot(data = tble, aes(x = year, y = value, col = model)) + 
    geom_line() + 
    facet_wrap(.~station) + 
    theme_minimal() +
    labs(x = 'Año', y = 'Temperatura (°C)', col = '') +
    theme(legend.position = 'bottom', 
          legend.key.width = unit(3, 'line'), 
          strip.text = element_text(face = 'bold'))
  
  glne
  
  
  
}









