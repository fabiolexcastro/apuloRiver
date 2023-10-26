

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, remotes, Metrics, readxl, xlsx, openxlsx, chirps, spatialEco, terra, RSAGA, fs, sf, readxl, openxlsx, tidyverse, glue, gtools, RColorBrewer)

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
mdls <- c('ACCESS-CM2', 'CanESM5', 'EC-Earth3', 'INM-CM4-8', 'MRI-ESM2-0')

# To apply the function tmin
calcRMSE <- function(bs, vr){
  
  # bs <- 1
  # vr <- 'tmin'
  
  cat('To process: ', bs, vr, '\n')
  
  tbl <- filter(tbls.bsln, station == bs & variable == vr)
  cmb <- tibble(obsr = 'CHIRTS', model = mdls)
  
  nsh <- map_dfr(.x = 1:nrow(cmb), .f = function(i){
    
    cm <- cmb[i,]
    md <- cm$model
    tb <- tbl %>% filter(model %in% c('CHIRTS', md))
    ch <- tb %>% filter(model == 'CHIRTS') %>% group_by(variable, station, year, model) %>% summarise(value = mean(value)) %>% ungroup() 
    tb <- rbind(ch, tb %>% filter(model != 'CHIRTS'))
    tb <- tb %>% filter(year >= 1983)
    tb <- tb %>% spread(model, value)
    colnames(tb) <- c('variable', 'subbasin', 'year', 'obsr', 'mdel')
    rm <- Metrics::rmse(predicted = pull(tb, mdel), actual = pull(tb, obsr))
    rs <- tibble(model = md, rmse = rm)
    cat('Done!\n')
    return(rs)
    
  }) %>% 
    mutate(basin = bs, variable = vr)
  return(nsh)
  
}
rslt.tmin <- map(1:4, calcRMSE, vr = 'tmin') %>% bind_rows()

# To apply the function tmax
tmax <- read_csv('./data/tbl/values_stts_tasm/Tmax_stts_chirts.csv')
tmax.smmr <- tmax %>% mutate(year = str_sub(date, 1, 4)) %>% group_by(Subbasin, variable, year) %>% dplyr::summarise(value = mean(value, na.rm = T)) %>% ungroup()


head(tmax)


calcRMSE <- function(bs, vr){
  
  bs <- 1
  vr <- 'tasmax'
  
  cat('To process: ', bs, vr, '\n')
  
  unique(tbls.bsln$model)
  
  tbl <- filter(tbls.bsln, station == bs & variable == vr)
  unique(tbl$model)
  
  cmb <- tibble(obsr = 'CHIRTS', model = mdls)
  
  nsh <- map_dfr(.x = 1:nrow(cmb), .f = function(i){
    
    cm <- cmb[i,]
    md <- cm$model
    tb <- tbl %>% filter(model %in% c('CHIRTS', md))
    tb <- tb %>% filter(year >= 1983)
    tb <- tb %>% spread(model, value)
    colnames(tb) <- c('variable', 'subbasin', 'year', 'obsr', 'mdel')
    rm <- Metrics::rmse(predicted = pull(tb, mdel), actual = pull(tb, obsr))
    rs <- tibble(model = md, rmse = rm)
    cat('Done!\n')
    return(rs)
    
  }) %>% 
    mutate(basin = bs, variable = vr)
  return(nsh)
  
}
rslt.tmax <- map(1:4, calcRMSE, vr = 'tasmax') %>% bind_rows()





