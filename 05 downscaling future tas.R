
# Radicad 979669046
## Downscaling future for temperature variables
## Jul 18th 2023

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, spatialEco, terra, RSAGA, fs, sf, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

path.bsln <- './tif/nasa/cmip6/historical'
path.ftre <- './data/tif/nasa/cmip6/ssp245'
vars <- c('tasmax', 'tasmin')

dirs.bsln <- dir_ls(path.bsln, type = 'directory')
ssps <- c('ssp245', 'ssp585')

dirs.bsln.raw <- './tif/nasa/cmip6/historical/'

# Basin shapefile 
bsin <- terra::vect('./shp/Cuenca/Cuenca_Río_Apulo.shp')
plot(bsin)

# Function to use ---------------------------------------------------------
#
down.ftre <- function(dir){
  
  # dir <- dirs.bsln[2] # Correr y borrar

  mdl <- basename(dir)
  cat('To process: ', dir, '\n')
  
  # Historic dataset (downscaling)
  fls.hst <- dir_ls(dir) %>% 
    as.character() %>% 
    grep('tas', ., value = T) %>% 
    map(.x = ., dir_ls, regexp = '.nc$') %>% 
    map(.x = ., as.character) %>% 
    unlist() %>% 
    as.character() %>% 
    grep('down-cund', ., value = T)

  # Historic dataset (original)
  fls.hst.raw <- dir_ls(dirs.bsln.raw) %>% 
    grep(mdl, ., value = T) %>% 
    dir_ls(., type = 'directory') %>% 
    grep('tas', ., value = T) %>% 
    map(.x = ., dir_ls, regexp = '.nc$') %>% 
    unlist() %>%
    as.character() %>%
    grep('/cund_tas', ., value = T)
  
  # Future dataset 
  fls.ftr <- dir_ls(path.ftre) %>% 
    grep(mdl, ., value = T) %>% 
    dir_ls(.) %>% 
    as.character() %>% 
    grep('tas', ., value = T) %>% 
    map(., dir_ls, regexp = '.nc$') %>% 
    unlist() %>% 
    as.character() %>% 
    grep('/cund_', ., value = T)

  # To tidy the years
  yrs.hst <- basename(fls.hst) %>% 
    str_split(., pattern = '_') %>%
    map_chr(., 8) %>% 
    gsub('.nc$', '', .) %>% 
    unique()

  yrs.hst.raw <- basename(fls.hst.raw) %>% 
    str_split(., pattern = '_') %>% 
    map_chr(., 8) %>% 
    gsub('.nc$', '', .) %>% 
    unique()

  yrs.ftr <- basename(fls.ftr) %>% 
    str_split(., pattern = '_') %>% 
    map_chr(., 8) %>% 
    gsub('.nc$', '', .) %>% 
    unique()
  
  length(yrs.hst)
  length(yrs.ftr)
  
  prd <- tibble(hist = c(yrs.hst.raw, yrs.hst.raw, 2010:2014), 
                hist_raw = c(yrs.hst, yrs.hst, 2010:2014), 
                ftre = c(2015:(2015+40), 2055:2095, 2096:2100))

  # To read as a raster file into a map function
  map(.x = 1:nrow(prd), .f = function(d){
    
    # d <- 1 # Correr y borrar 
    
    cat('To process: ', d, '\n')
    pr <- prd[d,]
    yr.bs <- as.numeric(pr[,1])
    yr.ft <- as.numeric(pr[,3])
    
    fl.bs <- grep(yr.bs, fls.hst, value = T) %>% grep('down', ., value = T) 
    fl.bs.rw <- grep(yr.bs, fls.hst.raw, value = T) 
    fl.ft <- grep(yr.ft, fls.ftr, value = T)
    
    vrs <- c('tasmin', 'tasmax')
    
    map(.x = 1:length(vrs), .f = function(v){
      
      # v <- 1 # Correr y borrar
      vr <- vrs[v]
      rs.bs <- fl.bs %>% grep(vr, ., value = T) %>% terra::rast()
      rs.bs.rw <- fl.bs.rw %>% grep(vr, ., value = T) %>% terra::rast()
      rs.ft <- fl.ft %>% grep(vr, ., value = T) %>% terra::rast()
      
      dts.bs <- seq(as.Date(paste0(yr.bs, '-01-01'), format = '%Y-%m-%d'), as.Date(paste0(yr.bs, '-12-31'), format = '%Y-%m-%d'), by = 'day')
      dts.ft <- seq(as.Date(paste0(yr.ft, '-01-01'), format = '%Y-%m-%d'), as.Date(paste0(yr.ft, '-12-31'), format = '%Y-%m-%d'), by = 'day')
      
      if(leap_year(yr.bs)){
        dts.bs <- dts.bs[-grep('02-29', dts.bs, value = F)]
      }
      
      if(leap_year(yr.ft)){
        dts.ft <- dts.ft[-grep('02-29', dts.ft, value = F)]
      }#
      
      time(rs.bs) <- dts.bs
      time(rs.bs.rw) <- dts.bs
      time(rs.ft) <- dts.ft
      
      anml <- map(.x = 1:12, .f = function(m){
        
        # m <- 1 # Correr y borrar
        mn <- ifelse(m < 10, glue('0{m}'), as.character(m))
        
        # To filtering the monts
        r.bs <- rs.bs[[grep(glue('-{mn}-'), time(rs.bs), value = FALSE)]]
        r.bs.r <- rs.bs.rw[[grep(glue('-{mn}-'), time(rs.bs.rw), value = FALSE)]]
        r.ft <- rs.ft[[grep(glue('-{mn}-'), time(rs.ft), value = FALSE)]]
        
        plot(r.bs[[1]])
        plot(r.bs.r[[1]])
        plot(r.ft[[1]])
        
        r.bs.r <- mean(r.bs.r)
        r.bs <- mean(r.bs)
        r.ft <- mean(r.ft)

        r.bs.r <- r.bs.r - 273.15
        r.ft <- r.ft - 273.15

        plot(c(r.bs.r, r.ft))

        anom <- r.ft - r.bs.r
        thr <- as.numeric(terra::global(x = anom, fun = stats::quantile, probs = 0.98, na.rm = T))
        anom[anom >= thr] <- thr
        crds <- terra::as.data.frame(anom, xy = T)
        names(crds)[3] <- 'mean'
        
        library(fields)
        tps <- fields::Tps(x = crds[,c('x', 'y')], Y = crds[,'mean']) 
        ref <- r.bs[[1]] * 0 + 1
        plot(ref)
        int <- terra::interpolate(object = rast(ref), model = tps, fun = predict)
        int <- terra::mask(int, mask = ref)
        int <- terra::crop(int, bsin)
        int <- terra::mask(int, bsin)
        cat('Done: ', m, '\n')
        return(int)

      })

      anml <- reduce(anml, c)
      names(anml) <- glue('anomalies_{1:12}')
      rm(m)
      
      fnal <- map(.x = 1:12, .f = function(m){

          # m <- 1 # Correr y borrar
          m <- ifelse(m < 10, paste0('0', m), as.character(m))
          rs.bs.m <- rs.bs[[grep(paste0('-', m, '-'), time(rs.bs), value = FALSE)]]
          fn <- rs.bs.m + anml[[as.numeric(m)]]
          names(fn) <- time(fn)
          return(fn)
        
      })
    
      fnal
      fnal <- reduce(fnal, c)
      fnal
      
      # Problem with the NaN
      map(.x = 1:nlyr(fnal), .f = function(z){
        
        z <- 1 # Correr y borrar
        cat('Layer: ', i, '\n')
        fnl <- fnal[[i]]
        vls <- values(fnal)
        
      })
      
      
      
      # To write the final raster 
      dir.out <- glue('./data/tif/nasa/cmip6/ssp245/{mdl}/{vr}/down/{vr}_{yr.ft}.tif')
      dir_create(dirname(dir.out))
      terra::writeRaster(x = fnal, filename = dir.out, overwrite = TRUE, filetype = 'GTiff')
      cat('Done!\n')
      
    })
    
  })
  
}

down.ftre(dir = dirs[2])
down.ftre(dir = dirs[3])
down.ftre(dir = dirs[4])
down.ftre(dir = dirs[5])















