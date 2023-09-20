
## Extract values from the points
## September 15 th 2023

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, spatialEco, terra, RSAGA, fs, sf, readxl, openxlsx, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
tble <- read.xlsx('./data/tbl/Subcuencas Coordenadas.xlsx')
tble <- as_tibble(tble)
tble

# Spatial data
bsin <- terra::vect('./shp/Cuenca/Cuenca_Río_Apulo.shp')

# A simple plot
plot(bsin)
points(tble$Long_, tble$Lat, pch = 16, col = 'red')

# Directories
vars <- c('pr', 'tasmax', 'tasmin')
path.ftre <- './data/tif/nasa/cmip6'
dirs.ssps <- dir_ls(path.ftre, type = 'directory') %>% as.character()
mdls <- dir_ls(dirs.ssps)[1:5]
mdls <- basename(mdls)

# To check the periods
prds <- tibble(hist = c(1974:2014, 1974:2014, 2010:2014), 
               hist_raw = c(1974:2014, 1974:2014, 2010:2014), 
               ftre = c(2015:(2015+40), 2055:2095, 2096:2100))

# Functions  --------------------------------------------------------------

# Precipitation -----------------------------------------------------------
extrac.prec.ftre <- function(ssp, mdl){
  
  # ssp <- 'ssp245' # Correr y comentar
  # mdl <- mdls[1] # Correr y comentar 
  
  fles <- dirs.ssps %>% 
    dir_ls(., type = 'directory') %>% 
    grep(mdl, ., value = T) %>% 
    grep(ssp, ., value = T) %>% 
    as.character() %>% 
    dir_ls() %>% 
    grep('pr', ., value = T) %>% 
    dir_ls(type = 'directory') %>% 
    grep('down',., value = T) %>% 
    dir_ls(., regexp = '.tif$') %>% 
    as.character()
    
  tbls <- map(.x = 1:length(fles), .f = function(i){
    
    # i <- 1 # Correr y comentar
    
    cat('To make the year number: ', i, '\n')
    fle <- fles[i] 
    rst <- terra::rast(fle)
    rst <- ifel(rst < 0, 0, rst)
    plot(rst)
    
    vls <- map(.x = 1:nlyr(rst), .f = function(x){
      rst[[x]] %>%
        terra::extract(., tble[,c('Long_', 'Lat')]) %>% 
        as_tibble() %>% 
        gather(var, value, -ID)
    }) %>% 
      bind_rows()
    
    yea <- pull(vls, 5) %>% str_sub(., 1, 4) %>% unique()
    prd <- prds %>% filter(hist == yea)
    
    vls <- mutate(vls, value = ifelse(is.na(value), 0, value))
    vls <- mutate(vls, model = basename(mdl), date = var)
    vls <- mutate(vls, var = 'prec')
    vls <- spread(vls, ID, value)
    return(vls)
    
  })
  
  tbls <- bind_rows(tbls)
  tbls
  p_load(xlsx, readx, openxlsx)
  tbls <- as.data.frame(tbls)
  openxlsx::write.xlsx(x = tbls, file = glue('./data/tbl/values-sts_{basename(mdl)}_prec-ftre-{basename(dirs.ssps)}.xlsx'))
  cat('Done!\n')
 
}

prec.mdl1.1 <- extrac.prec.hist(ssp = ssps[1], mdl = mdls[1])
prec.mdl1.2 <- extrac.prec.hist(ssp = ssps[2], mdl = mdls[1])

prec.mdl2.1 <- extrac.prec.hist(ssp = ssps[1], mdl = mdls[2])
prec.mdl2.2 <- extrac.prec.hist(ssp = ssps[2], mdl = mdls[2])

prec.mdl3.1 <- extrac.prec.hist(ssp = ssps[1], mdl = mdls[3])
prec.mdl3.2 <- extrac.prec.hist(ssp = ssps[2], mdl = mdls[3])

prec.mdl4.1 <- extrac.prec.hist(ssp = ssps[1], mdl = mdls[4])
prec.mdl4.2 <- extrac.prec.hist(ssp = ssps[2], mdl = mdls[4])

prec.mdl5.1 <- extrac.prec.hist(ssp = ssps[1], mdl = mdls[5])
prec.mdl5.2 <- extrac.prec.hist(ssp = ssps[2], mdl = mdls[5])

# Temperature -------------------------------------------------------------
extract.tasm.ftre <- function(var, ssp, mdl){
  
  # var <- 'tasmax' # Correr y comentar
  # ssp <- 'ssp245' # Correr y comentar
  # mdl <- mdls[1] # Correr y comentar
  
  fles <- dirs.ssps %>% 
    dir_ls(., type = 'directory') %>% 
    grep(mdl, ., value = T) %>% 
    grep(ssp, ., value = T) %>% 
    as.character() %>% 
    dir_ls() %>% 
    grep(var, ., value = T) %>% 
    dir_ls(type = 'directory') %>% 
    grep('down',., value = T) %>% 
    dir_ls(., regexp = '.tif$') %>% 
    as.character()
  
  tbls <- map(.x = 1:length(fles), .f = function(i){
    
    # i <- 1 # Correr y comentar
    
    cat('To make the year number: ', i, '\n')
    fle <- fles[i] 
    rst <- terra::rast(fle)
    rst <- ifel(rst < 0, 0, rst)
    plot(rst)
    
    vls <- map(.x = 1:nlyr(rst), .f = function(x){
      rst[[x]] %>%
        terra::extract(., tble[,c('Long_', 'Lat')]) %>% 
        as_tibble() %>% 
        gather(var, value, -ID)
    }) %>% 
      bind_rows()
    
    vls <- mutate(vls, value = ifelse(is.na(value), 0, value))
    vls <- mutate(vls, model = basename(mdl), date = var)
    vls <- mutate(vls, var = 'prec')
    vls <- spread(vls, ID, value)
    return(vls)
    
  })
  
  tbls <- bind_rows(tbls)
  tbls
  p_load(xlsx, readx, openxlsx)
  write.xlsx(as.data.frame(tbls), glue('./data/tbl/values-sts_{basename(mdl)}_{var}-ftre-{ssp}.xlsx'))
  cat('Done!\n')
  
}

vars <- c('tasmax', 'tasmin')

# ACCESS-ESM1
map(.x = 1:2, .f = function(v){
  extract.tasm.ftre(var = vars[v], ssp = ssps[1], mdl = mdls[1])
})

# 2nd
map(.x = 1:2, .f = function(v){
  extract.tasm.ftre(var = vars[v], ssp = ssps[1], mdl = mdls[2])
})




