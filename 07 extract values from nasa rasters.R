
## Extract values from the points
## September 09th 2023

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, spatialEco, terra, RSAGA, fs, sf, readxl, openxlsx, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load libraries ----------------------------------------------------------
tble <- read.xlsx('./data/tbl/Subcuencas Coordenadas.xlsx')
head(tble)
tble <- as_tibble(tble)
tble

# Spatial data
bsin <- terra::vect('./shp/Cuenca/Cuenca_Río_Apulo.shp')

# A simple plot
plot(bsin)
points(tble$Long_, tble$Lat, pch = 16, col = 'red')

# Directories
vars <- c('pr', 'tasmax', 'tasmin')
path.bsln <- './tif/nasa/cmip6/historical'
dirs.bsln <- dir_ls(path.bsln, type = 'directory') %>% as.character()
path.bsln
dirs.bsln

# Functions  --------------------------------------------------------------
extrac.prec.hist <- function(dir){
  
  # dir <- dirs.bsln[1] # Correr y comentar 
  
  cat('To process: ', dir, '\n')
  drs <- dir_ls(dir) %>% 
    grep('pr', ., value = T) %>% 
    dir_ls(., type = 'directory') %>% 
    dir_ls(., type = 'directory') %>% 
    as.character()
  
  tbl <- map(.x = 1:length(drs), .f = function(i){
    
    # i <- 1 # Correr y borrar
    
    fls <- drs[i] %>% 
      dir_ls() %>% 
      grep('down_', ., value = T) %>% 
      grep('.tif$', ., value = T) %>% 
      as.character() %>% 
      mixedsort() 
    
    rst <- map(fls, rast)
    
    vls <- map(.x = 1:length(rst), .f = function(x){
      rst[[x]] %>%
        terra::extract(., tble[,c('Long_', 'Lat')]) %>% 
        as_tibble() %>% 
        gather(var, value, -ID)
    }) %>% 
      bind_rows()
    
    mdl <- dirname(drs) %>% unique()
    mdl
    
    vls <- mutate(vls, model = basename(mdl))
    vls <- inner_join(vls, tble[,c('Long_', 'Lat', 'Subbasin')], by = c('ID' = 'Subbasin'))
    vls <- relocate(vls, model, Long_, Lat, ID, var, value)
    vls <- mutate(vls, year = basename(drs[i]))
    vls
    cat('Done! ')
    return(vls)
    
    
  })
  
  yrs <- basename(drs)
  yrs <- str_split(yrs, '_')
  yrs <- map(yrs, 8)
  yrs <- gsub('.nc', '',)
  
  dts <- map(yrs, .f = function(i){
    sqn <- seq(as.Date(paste0(i, '-01-01'), format = '%Y-%m-%d'), as.Date(paste0(i, '-12-31'), format = '%Y-%m-%d'), by = 'day')
    return(sqn)
  })  

  dfm <- map(.x = 1:length(tbl), .f = function(i){

    # i <- 1 # Correr y borrar
    cat('To process:', i, '\n') 
    tb <- tbl[[i]]
    tb <- dplyr::select(tb, -Long_, -Lat)
    tb <- spread(tb, ID, value)
    tb <- mutate(tb, day = parse_number(var), day = as.numeric(day))
    tb <- tb %>% arrange(day)
    yr <- unique(tb$year)
    yr <- as.numeric(yr)
    
    if(leap_year(yr)){
      print('Leap year')
      dt <- dts[[i]]
      dt <- dt[-grep('02-29', dt, value = FALSE)]
    } else { 
      print('No leap year')
      dt <- dts[[i]]  
    }
    
    tb <- mutate(tb, date = dt)
    cat('Date added\t')
    return(tb)
    
  })

  dfm <- bind_rows(dfm)
  dfm <- gather(dfm, stt, value, -var, -model, -year, -day, -date)
  dfm <- mutate(dfm, value = ifelse(value < 0, 0, value))
  dfm <- spread(dfm, stt, value)
  dfm <- mutate(dfm, variable = 'prec')
  head(dfm)
  p_load(xlsx, readx, openxlsx)
  write.xlsx(dfm, glue('./data/tbl/values-sts_{basename(dir)}_prec-hist.xlsx'))
  cat('Done!\n')
  
}
prec.mdl1 <- extrac.prec.hist(dirs.bsln[1])
prec.mdl2 <- extrac.prec.hist(dirs.bsln[2])
prec.mdl3 <- extrac.prec.hist(dirs.bsln[3])
prec.mdl4 <- extrac.prec.hist(dirs.bsln[4])
prec.mdl5 <- extrac.prec.hist(dirs.bsln[5])

# Temperatura máxima -------------------------------
extrac.tmax.hist <- function(dir){
  
  # dir <- dirs.bsln[1] # Correr y comentar 
  
  cat('To process: ', dir, '\n')
  fls <- dir_ls(dir) %>% 
    grep('tasmax', ., value = T) %>% 
    dir_ls(., regexp = '.nc$') %>% 
    grep('down', ., value = T) %>% 
    as.character()
  
  tbl <- map(.x = 1:41, .f = function(i){
    
    # i <- 1 # Correr y borrar
    
    cat('To make the year number: ', i, '\n')
    fle <- fls[i] %>% grep('tasmax_day', ., value = T) 
    rst <- terra::rast(fle)
    
    vls <- map(.x = 1:nlyr(rst), .f = function(x){
      rst[[x]] %>%
        terra::extract(., tble[,c('Long_', 'Lat')]) %>% 
        as_tibble() %>% 
        gather(var, value, -ID)
    }) %>% 
      bind_rows()
    
    mdl <- dirname(drs) %>% unique()
    mdl
    
    vls <- mutate(vls, model = basename(mdl))
    vls <- spread(vls, ID, value)
    vls <- mutate(vls, day = parse_number(var))
    vls <- vls %>% arrange(day)
    yea <- basename(drs[i]) %>% str_sub(., start = nchar(.) -6, end = nchar(.) - 3)
    vls <- mutate(vls, year = yea)
    sqn <- seq(as.Date(paste0(yea, '-01-01'), format = '%Y-%m-%d'), as.Date(paste0(yea, '-12-31'), format = '%Y-%m-%d'), by = 'day')#
    yea <- as.numeric(yea)
    
    if(leap_year(yr)){
      print('Leap year')
      dt <- sqn
      dt <- dt[-grep('02-29', dt, value = FALSE)]
    } else { 
      print('No leap year')
      dt <- sqn[[i]]  
    }
    
    vls <- mutate(vls, date = dt)
    vls <- dplyr::select(vls, -var)
    colnames(vls)[1] <- 'variable'
    cat('Done! ')
    return(vls)
    
    
  })

  dfm <- tbl
  dfm <- bind_rows(dfm)
  head(dfm)
  p_load(xlsx, readx, openxlsx)
  write.xlsx(dfm, glue('./data/tbl/values-sts_{basename(dir)}_tmax-hist.xlsx'))
  cat('Done!\n')
  
}

tmax.mdl1 <- extrac.tmax.hist(dirs.bsln[1])
tmax.mdl2 <- extrac.tmax.hist(dirs.bsln[2])
tmax.mdl3 <- extrac.tmax.hist(dirs.bsln[3])
tmax.mdl4 <- extrac.tmax.hist(dirs.bsln[4])
tmax.mdl5 <- extrac.tmax.hist(dirs.bsln[5])

# Temperatura mínima
extrac.tmin.hist <- function(dir){
  
  # dir <- dirs.bsln[1] # Correr y comentar 
  
  cat('To process: ', dir, '\n')
  fls <- dir_ls(dir) %>% 
    grep('tasmin', ., value = T) %>% 
    dir_ls(., regexp = '.nc$') %>% 
    grep('down', ., value = T) %>% 
    as.character()
  
  tbl <- map(.x = 1:length(fls), .f = function(i){
    
    # i <- 1 # Correr y borrar
    cat('To make the year number: ', i, '\n')
    
    fle <- fls[i] %>% grep('tasmin_day', ., value = T) 
    rst <- terra::rast(fle)
    
    vls <- map(.x = 1:nlyr(rst), .f = function(x){
      rst[[x]] %>%
        terra::extract(., tble[,c('Long_', 'Lat')]) %>% 
        as_tibble() %>% 
        gather(var, value, -ID)
    }) %>% 
      bind_rows()
    

    mdl <- basename(dir) %>% unique() 
    mdl
    
    vls <- mutate(vls, model = basename(mdl))
    vls <- spread(vls, ID, value)
    vls <- mutate(vls, day = parse_number(var))
    vls <- vls %>% arrange(day)
    yea <- fle %>% basename() %>% str_sub(., start = nchar(.) - 6, end = nchar(.) - 3)
    yea
    yea <- as.numeric(yea)
    vls <- mutate(vls, year = yea)
    
    sqn <- seq(as.Date(paste0(yea, '-01-01'), format = '%Y-%m-%d'), as.Date(paste0(yea, '-12-31'), format = '%Y-%m-%d'), by = 'day')#
    
    if(leap_year(yea)){
      print('Leap year')
      dt <- sqn
      dt <- dt[-grep('02-29', dt, value = FALSE)]
    } else { 
      print('No leap year')
      dt <- sqn 
    }
    
    vls <- mutate(vls, date = dt)
    vls <- mutate(vls, var = 'tmin')
    cat('Done! ')
    return(vls)
    
  })
  
  dfm <- tbl
  dfm <- bind_rows(dfm)
  head(dfm)
  p_load(xlsx, readx, openxlsx)
  write.xlsx(dfm, glue('./data/tbl/values-sts_{basename(dir)}_tmin-hist.xlsx'))
  cat('Done!\n')
  
}









