

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
  mdls <- unique(tble$model)
  
  tble <- map_dfr(.x = 1:5, .f = function(i){
    tbl <- tble %>% filter(model == mdls[i])
    tbl <- map_dfr(.x = 1:4, .f = function(j){
      tbl <- tbl %>% filter(station == j)
      tbl <- mutate(tbl, ssp = c(rep('ssp245', 85), rep('ssp585', 85)))
    })
    return(tbl)
  })
  
  # Start the analysis
  glne_245 <- ggplot(data = tble %>% filter(ssp == 'ssp245'), aes(x = year, y = value, col = model)) + 
    geom_line() + 
    facet_wrap(.~station) + 
    ggtitle(label = 'SSP: 245') +
    theme_minimal() +
    labs(x = 'Año', y = 'Temperatura (°C)', col = '') +
    theme(legend.position = 'bottom', 
          plot.title = element_text(face = 'bold', hjust = 0.5),
          legend.key.width = unit(3, 'line'), 
          strip.text = element_text(face = 'bold'))
  
  glne_585 <- ggplot(data = tble %>% filter(ssp == 'ssp585'), aes(x = year, y = value, col = model)) + 
    geom_line() + 
    facet_wrap(.~station) + 
    ggtitle(label = 'SSP: 585') +
    theme_minimal() +
    labs(x = 'Año', y = 'Temperatura (°C)', col = '') +
    theme(legend.position = 'bottom', 
          plot.title = element_text(face = 'bold', hjust = 0.5),
          legend.key.width = unit(3, 'line'), 
          strip.text = element_text(face = 'bold'))
  
  ggsave(plot = glne_245, filename = '../png/tasm/tmin_ftre_ssp245.png', units = 'in', width = 12, height = 9, dpi = 300)
  ggsave(plot = glne_585, filename = '../png/tasm/tmin_ftre_ssp585.png', units = 'in', width = 12, height = 9, dpi = 300)
  cat('Done\n')
  
}









