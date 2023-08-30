## Downscaling future for precipitation variable
## Aug 11th 2023

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, spatialEco, terra, RSAGA, fs, sf, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

# Basin shapefile 
bsin <- terra::vect('./shp/Cuenca/Cuenca_Río_Apulo.shp')
plot(bsin)

# Climate data list
mdel <- 'EC-Earth3'
path <- glue('./tif/nasa/cmip6/historical/{mdel}/pr/{mdel}')#a
dirs.bsln <- dir_ls(path, type = 'directory')
dir_ls(dirs.bsln[1], regexp = '.tif$')

dirs.bsln.raw <- glue('./tif/nasa/cmip6/historical/{mdel}/pr')

path.ftre <- './data/tif/nasa/cmip6/ssp245'

# Function to use ---------------------------------------------------------
down.ftre <- function(dir){
  
  dir <- dirs.bsln[1]
  
  mdl <- mdel
  cat('To process ', basename(dir), '\n')
  
  # Historic dataset (downscaling)
  fls.hst <- dir_ls(dir) %>% 
    as.character() %>% 
    grep('down', ., value = T) %>% 
    mixedsort()
  
  # Historic dataset original 
  fls.hst.raw <- dir_ls(dirs.bsln.raw) %>% 
    as.character() %>% 
    grep('.nc$', ., value = T)
  fls.hst.raw
  
  # Future dataset
  fls.ftr <- dir_ls(path.ftre) %>% 
    grep(mdel, ., value = T) %>% 
    dir_ls() %>% 
    as.character() %>% 
    grep('pr', ., value = T) %>% 
    dir_ls() %>% 
    as.character() %>% 
    grep('.nc$', ., value = T) 
  fls.ftr
  
  # To tidy the years
  prd <- tibble(hist = c(1974:2014, 1974:2014, 2010:2014), 
                hist_raw = c(1974:2014, 1974:2014, 2010:2014),
                ftre = c(2015:(2015+40), 2055:2095, 2096:2100))
  
  yrs.hst <- prd$hist
  yrs.hst.raw <- prd$hist_raw
  yrs.ftr <- prd$ftre
  
  # To read as a raster file into a map function
  map(.x = 1:nrow(prd), .f = function(i){
    
    d <- 1 # Correr y borrar
    
    cat('To process: ', d, '\n')
    pr <- prd[d,]
    yr.bs <- as.numeric(pr[,1])
    yr.ft <- as.numeric(pr[,3])
    
    fl.bs <- grep(yr.bs, fls.hst, value = T) %>% grep('down', ., value = T) 
    fl.bs.rw <- grep(yr.bs, fls.hst.raw, value = T) 
    fl.ft <- grep(yr.ft, fls.ftr, value = T)
    
    rs.bs <- rast(fl.bs)
    plot(rs.bs) # Correr y borrar
    rs.bs.rw <- rast(fl.bs.rw)
    rs.ft <- rast(fl.ft)
    
    dts.bs <- seq(as.Date(paste0(yr.bs, '-01-01'), format = '%Y-%m-%d'), as.Date(paste0(yr.bs, '-12-31'), format = '%Y-%m-%d'), by = 'day')
    dts.ft <- seq(as.Date(paste0(yr.ft, '-01-01'), format = '%Y-%m-%d'), as.Date(paste0(yr.ft, '-12-31'), format = '%Y-%m-%d'), by = 'day')
    
    if(leap_year(yr.bs)){
      print('leap year basename')
      dts.bs <- dts.bs[-grep('02-29', dts.bs, value = F)]
    }
    
    if(leap_year(yr.ft)){
      print('leap year future')  
      dts.ft <- dts.ft[-grep('02-29', dts.ft, value = F)]
    }
    
    time(rs.bs) <- dts.bs
    time(rs.bs.rw) <- dts.bs
    time(rs.ft) <- dts.ft
    
    anml <- map(.x = 1:12, .f = function(m){
      
      #m <- 1 # Correr y borrar
      
      print(month.abb[m])
      mn <- ifelse(m < 10, paste0('0', m), as.character(m))
      
      # To filtering the monts
      r.bs <- rs.bs[[grep(glue('-{mn}-'), time(rs.bs), value = FALSE)]]
      r.bs.r <- rs.bs.rw[[grep(glue('-{mn}-'), time(rs.bs.rw), value = FALSE)]]
      r.ft <- rs.ft[[grep(glue('-{mn}-'), time(rs.ft), value = FALSE)]]
      
      r.bs <- terra::app(r.bs, sum, na.rm = T)
      r.bs.r <- terra::app(r.bs.r, sum, na.rm = T)
      r.ft <- terra::app(r.ft, sum, na.rm = T)
      
      if(nrow(as.data.frame(r.bs)) == 0){
        cat('NA ')
        r.bs[] <- 0
        r.bs <- terra::crop(r.bs, bsin)
        r.bs <- terra::mask(r.bs, bsin)
      }  
      
      plot(c(r.bs.r, r.ft))
      
      r.bs.r <- r.bs.r * 86400
      r.ft <- r.ft * 86400
      
      #calculate anomaly in fraction
      anom <- (r.ft - r.bs.r)/r.bs.r
      plot(anom)
      
      # Truncate the top 2% of anomaly values
      thr <- as.numeric(terra::global(x = anom, fun = stats::quantile, probs = 0.98, na.rm = T))
      anom[anom >= thr] <- thr
      
      crd <- terra::as.data.frame(anom, xy = T)
      names(crd)[3] <- 'mean'      
      crd <- mutate(crd, mean = ifelse(is.infinite(mean), 0, mean))
      
      library(fields)
      tps <- fields::Tps(x = crd[,c('x', 'y')], Y = crd[,'mean']) 
      ref <- r.bs[[1]] * 0 + 1
      plot(ref)
      int <- terra::interpolate(object = rast(ref), model = tps, fun = predict)
      int <- terra::mask(int, mask = ref)
      int <- terra::crop(int, bsin)
      int <- terra::mask(int, bsin)
      cat('Done\n')
      return(int)
      
    })
    
    anml <- reduce(anml, c)
    names(anml) <- glue('anomalies_{1:12}')
    rm(m)
    
    fnal <- map(.x = 1:12, .f = function(m){
      
      m <- 1 # Correr y borrar
      m <- ifelse(m < 10, paste0('0', m), as.character(m))
      rs.bs.m <- rs.bs[[grep(paste0('-', m, '-'), time(rs.bs), value = FALSE)]]
      rs.bs.m[rs.bs.m == -9999] <- NA
      rs.bs.m <- terra::crop(rs.bs.m, bsin) %>% terra::mask(., bsin)
      fn <- rs.bs.m * (1 + anml[[as.numeric(m)]])
      names(fn) <- time(fn)
      return(fn)
      
    })
    
    fnal <- reduce(fnal, c)
    fnal
    plot(fnal)
    
    
    
    
    
    
  })
  
  
  
}