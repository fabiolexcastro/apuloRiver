

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

# CHIRTS ------------------------------------------------------------------
fles.chrt <- grep('chirts', fles, value = T)
tbls.chrt <- map(fles.chrt, read_csv)
tbls.chrt <- bind_rows(tbls.chrt)
tbls.chrt <- dplyr::select(tbls.chrt, Subbasin, variable, date, value)
tbls.chrt <- mutate(tbls.chrt, year = str_sub(date, 1, 4))
tbls.chrt <- tbls.chrt %>% group_by(Subbasin, variable, year) %>% dplyr::summarise(value = mean(value)) %>% ungroup()

# GCMs baseline -----------------------------------------------------------
fles.gcms.bsln <- grep('hist-gcms', fles, value = T)
tbls.gcms.bsln <- map(fles.gcms.bsln, read_csv)
tbls.gcms.bsln <- bind_rows(tbls.gcms.bsln)

head(tbls.chrt)
head(tbls.gcms.bsln)
colnames(tbls.chrt)[1] <- 'station'
tbls.chrt <- mutate(tbls.chrt, variable = ifelse(variable == 'Tmax', 'tmax', 'tmin'))
tbls.chrt <- mutate(tbls.chrt, model = 'CHIRTS')
tbls.chrt <- dplyr::select(tbls.chrt, variable, station, year, value, model)

# Join both tables into only one ------------------------------------------
tbls.chrt <- mutate(tbls.chrt, year = as.numeric(year))
tbls.bsln <- bind_rows(tbls.chrt, tbls.gcms.bsln)
tbls.bsln <- filter(tbls.bsln, year %in% 1983:2014)

tbls.bsln
table(tbls.bsln$model)

range(tbls.chrt$year)
range(tbls.bsln$year)

write.csv(tbls.bsln, './enviar_fabio5.csv', row.names = FALSE)

tbls.bsln <- read_csv('./enviar_fabio5.csv')

# To make the graph  ------------------------------------------------------
make.graph <- function(var, stt){
  
  # var <- 'tmin'
  # stt <- '1'
  
  tbl <- tbls.bsln %>% filter(variable == var & station == stt)
  tbl <- mutate(tbl, model = factor(model, levels = c('CHIRTS', 'ACCESS-CM2', 'CanESM5', 'EC-Earth3', 'INM-CM4-8', 'MRI-ESM2-0')))
  
  chr <- tbl %>% filter(model == 'CHIRTS') %>% group_by(variable, station, year, model) %>% dplyr::summarise(value = mean(value, na.rm = T)) %>% ungroup()
  tbl <- rbind(chr, tbl %>% filter(model != 'CHIRTS'))
  
  ttl <- ifelse(var == 'tmin', 'Temperatura mínima', 'Temperatura máxima')

  ggl <- ggplot(data = tbl, aes(x = year, y = value, col = model)) + 
    geom_line() +
    facet_wrap(~model, ncol = 1, nrow = 6) +
    ggtitle(label = glue('{ttl} - Coordenada: {stt}')) +
    labs(x = 'Año', y = 'Temperatura (°C)', col = '') +
    theme_minimal() + 
    theme(legend.position = 'bottom', 
          plot.title = element_text(size = 16, hjust = 0.5))
  
  dir <- glue('./png/tasm'); dir_create(dir)
  
  ggsave(plot = ggl, filename = glue('./png/tasm/{var}_{stt}.png'), units = 'in', width = 7, height = 13, dpi = 300)
  
  
}

# Tmin 
map(.x = 1:4, .f = function(i){
  make.graph(var = 'tmin', stt = i)
})

# Tmax
map(.x = 1:4, .f = function(i){
  make.graph(var = 'tasmax', stt = i)
})



# RMSE value --------------------------------------------------------------

p_load(hydroGOF)
mdls <- c('ACCESS-CM2', 'CanESM5', 'EC-Earth3', 'INM-CM4-8', 'MRI-ESM2-0')

calcRMSE <- function(bs){
  
  cat('To process: ', bs, '\n')
  
  tbl <- filter(tbls.bsln, station == bs)
  cmb <- tibble(obsr = 'CHIRTS', model = mdls)
  
}









