
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, rgeos, readxl, openxlsx, gtools, stringr, glue, geodata)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

# Vector data
bsin <- vect('./shp/Cuenca/Cuenca_Río_Apulo.shp')
dpto <- vect('./shp/Base/dptos.gpkg')
cndn <- dpto[dpto$DPTO_CNMBR == 'CUNDINAMARCA',]
plot(cndn)

# Raster data
srtm <- rast('./tif/srtm/fill/srtm_z07_fill.tif')
rraw.bsl <- rast('./tif/nasa/cmip6/historical/ACCESS-CM2/tasmax/cund_tasmax_day_ACCESS-CM2_historical_r1i1p1f1_gn_1974.nc')
rraw.ftr <- rast('./data/tif/nasa/cmip6/ssp245/ACCESS-CM2/tasmax/cund_tasmax_day_ACCESS-CM2_ssp245_r1i1p1f1_gn_2015.nc')

rgwr.bsl <- rast('./tif/nasa/cmip6/historical/ACCESS-CM2/tasmax/down-cund_tasmax_day_ACCESS-CM2_historical_r1i1p1f1_gn_1974.nc')
rdwn.ftr <- rast('./data/tif/nasa/cmip6/ssp245/ACCESS-CM2/tasmax/down/tasmax_2015.tif')

# Plotting ----------------------------------------------------------------
plot(srtm[[1]])
plot(rraw.bsl[[1]])
plot(rraw.ftr[[1]])
plot(rgwr.bsl[[1]])
plot(rdwn.ftr[[1]])

# Raster to table ---------------------------------------------------------
srtm.tble <- terra::as.data.frame(srtm, xy = T) %>% as_tibble()
rraw.bsl.tbl <- terra::as.data.frame(rraw.bsl, xy = T) %>% as_tibble()
rraw.ftr.tbl <- terra::as.data.frame(rraw.ftr, xy = T) %>% as_tibble()
rgwr.bsl.tbl <- terra::as.data.frame(rgwr.bsl, xy = T) %>% as_tibble()
rdwn.ftr.tbl <- terra::as.data.frame(rdwn.ftr, xy = T) %>% as_tibble()

# Selecting just January  -------------------------------------------------
rraw.bsl.tbl <- rraw.bsl.tbl %>% dplyr::select(1:33)
rraw.ftr.tbl <- rraw.ftr.tbl %>% dplyr::select(1:33)
rgwr.bsl.tbl <- rgwr.bsl.tbl %>% dplyr::select(1:33)
rdwn.ftr.tbl <- rdwn.ftr.tbl %>% dplyr::select(1:33)

rraw.bsl.tbl.avg <- rraw.bsl.tbl %>% mutate(gid = 1:nrow(.)) %>% gather(var, value, -gid, -x, -y)
rraw.bsl.tbl.avg <- rraw.bsl.tbl.avg %>% group_by(gid, x, y) %>% dplyr::summarise(value = mean(value, na.rm = T)) %>% ungroup()
rraw.bsl.tbl.avg <- rraw.bsl.tbl.avg %>% mutate(value = value - 273.15)

rraw.ftr.tbl.avg <- rraw.ftr.tbl %>% mutate(gid = 1:nrow(.)) %>% gather(var, value, -gid, -x, -y)
rraw.ftr.tbl.avg <- rraw.ftr.tbl.avg %>% group_by(gid, x, y) %>% dplyr::summarise(value = mean(value, na.rm = T)) %>% ungroup()
rraw.ftr.tbl.avg <- rraw.ftr.tbl.avg %>% mutate(value = value - 273.15)
rraw.ftr.tbl.avg

rgwr.bsl.tbl.avg <- rgwr.bsl.tbl %>% mutate(gid = 1:nrow(.)) %>% gather(var, value, -gid, -x, -y)
rgwr.bsl.tbl.avg <- rgwr.bsl.tbl.avg %>% group_by(gid, x, y) %>% dplyr::summarise(value = mean(value, na.rm = T)) %>% ungroup()  
rgwr.bsl.tbl.avg <- rgwr.bsl.tbl.avg %>% mutate(value = value - 273.15)

rdwn.ftr.tbl.avg <- rdwn.ftr.tbl %>% mutate(gid = 1:nrow(.)) %>% gather(var, value, -gid, -x, -y)
rdwn.ftr.tbl.avg <- rdwn.ftr.tbl.avg %>% group_by(gid, x, y) %>% dplyr::summarise(value = mean(value, na.rm = T)) %>% ungroup()  
rdwn.ftr.tbl.avg

# Now to make the maps ----------------------------------------------------
p_load(ggmap, ggspatial)
bbox <- as.numeric(c(-74.57, 4.51, -74.35, 4.85))
ggbx <- get_stamenmap(bbox, maptype = 'terrain', zoom = 10)

ggmap(ggbx, alpha = 0.5) + 
  geom_sf(data = st_as_sf(bsin), fill = NA, col = 'red', inherit.aes = FALSE) + 
  coord_sf(xlim = c(-74.57, -75.35)) +
  theme()

# SRTM --------------------------------------------------------------------
srtm.tble <- srtm.tble %>% setNames(c('x', 'y', 'value'))

gsrtm <- ggplot() + 
  geom_tile(data = srtm.tble, aes(x = x, y = y, fill = value)) + 
  scale_fill_gradientn(colors = terrain.colors(10)) +
  geom_sf(data = st_as_sf(bsin), fill = NA, col = 'grey50') +
  coord_sf() + 
  theme_minimal() +
  theme(legend.position = 'bottom', 
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.x = element_text(hjust = 0.5),
        text = element_text(family = 'Gill Sans MT'))

gsrtm





























