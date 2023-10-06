
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
  
  cat('To process: ', bs, '\n')
  
  tbl <- filter(prec, Subbasin == as.character(bs))
  cmb <- tibble(obsr = 'Baseline', model = mdls)
  
  nsh <- map(.x = 1:nrow(cmb), .f = function(i){
    
    cm <- cmb[i,]
    md <- cm$model
    tb <- tbl %>% filter(model %in% c('Baseline', md))
    tb <- tb %>% dplyr::select(-type)
    tb <- tb %>% spread(model, value)
    tb <- tb %>% mutate(year = year(date))
    tb <- tb %>% filter(year >= 1983)
    colnames(tb) <- c('date', 'subbasin', 'mdel', 'obsr', 'year')
    ns <- NSE(sim = pull(tb, mdel), obs = pull(tb, obsr), na.rm = T)
    rm <- rmse(sim = pull(tb, mdel), obs = pull(tb, obsr), na.rm = T)
    rs <- tibble(model = md, nash = ns, rmse = rm)
    cat('Done!\n')
    return(rs)
    
  })
  
  nsh <- bind_rows(nsh)
  nsh <- nsh %>% mutate(basin = bs)
  cat('Analysis done!\n')
  return(nsh)
  
}

# To apply the function 
nash <- map(1:4, calcNASH)
nash <- bind_rows(nash)

write.csv(nash, './enviarfabio.csv')

nash <- read_csv('./enviarfabio.csv')

topn <- nash %>% 
  mutate(basin = as.character(basin)) %>% 
  group_by(basin) %>% 
  slice_min(rmse, with_ties = FALSE) %>% 
  ungroup()

topn

write.csv(prec, './enviarfabio2.csv', row.names = FALSE)

# To make the graph  ------------------------------------------------------
library(lubridate)

prec <- read_csv('./enviarfabio2.csv')
prec <- mutate(prec, Subbasin = as.character(Subbasin))
unique(prec$model)
unique(prec$type)

smmr <- prec %>% 
  mutate(year = year(date), month = month(date)) %>% 
  group_by(year, month, Subbasin, model, type) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  mutate(month = ifelse(month < 10, paste0('0', month), as.numeric(month))) %>% 
  mutate(date = paste0(year, '-', month, '-01'), 
         date = as.Date(date, format = '%Y-%m-%d')) %>% 
  filter(year > 1983) %>% 
  mutate(model = ifelse(model == 'Baseline', 'CHIRPS', model), 
         model = factor(model, levels = c('CHIRPS', 'ACCESS-CM2', 'CanESM5', 'EC-Earth3', 'INM-CM4-8', 'MRI-ESM2-0')))

dir.create('./png')

make.graph <- function(bsin){
  
  bsin <- '1'
  
  gprec <- ggplot(data = smmr %>% filter(Subbasin == bsin), aes(x = date, y = value, group = model, col = model)) + 
    geom_line() +
    facet_wrap(.~ model, nrow = 6) +
    # facet_wrap(.~ model, nrow = 6) +
    labs(x = 'Fecha', y = 'Precipitación (mm)', col = '') +
    theme_minimal() +
    theme(legend.position = 'bottom', 
          strip.text = element_text(face = 'bold', hjust = 0.5, size = 12))

  ggsave(plot = gprec, filename = glue('./png/graph_prec_{bsin}_v1.png'), units = 'in', width = 8, height = 16, dpi = 300)
  
  gpre2 <- ggplot(data = smmr %>% filter(Subbasin == bsin, model %in% c('CHIRPS', 'CanESM5')), aes(x = date, y = value, group = model, col = model)) + 
    geom_line() +
    labs(x = 'Fecha', y = 'Precipitación (mm)', col = '') +
    theme_minimal() +
    theme(legend.position = 'bottom', 
          strip.text = element_text(face = 'bold', hjust = 0.5, size = 12))
  
  ggsave(plot = gpre2, filename = glue('./png/graph_prec_{bsin}_v2.png'), units = 'in', width = 12, height = 7, dpi = 300)
  
}

gprec <- ggplot(data = smmr, aes(x = date, y = value, group = model, col = model)) + 
  # geom_line() +
  geom_smooth(se = FALSE, method = 'loess') +
  facet_wrap(.~ Subbasin, nrow = 6) +
  # facet_wrap(.~ model, nrow = 6) +
  labs(x = 'Fecha', y = 'Precipitación (mm)', col = '') +
  theme_minimal() +
  theme(legend.position = 'bottom', 
        strip.text = element_text(face = 'bold', hjust = 0.5, size = 12))

ggsave(plot = gprec, filename = './png/graph_prec-models_all-basins.png', units = 'in', width = 9, height = 20, dpi = 300)

