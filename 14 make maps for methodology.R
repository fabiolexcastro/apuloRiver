
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, OpenStreetMap, tidyverse, gtools, stringr, glue, geodata)

# install.packages('rJava')

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

# Vector data
bsin <- vect('./shp/Cuenca/Cuenca_Río_Apulo.shp')
dpto <- vect('./shp/Base/dptos.gpkg')
cndn <- dpto[dpto$DPTO_CNMBR == 'CUNDINAMARCA',]

# Raster data
srtm <- rast('./tif/srtm/fill/srtm_z07_fill.tif')
rraw.bsl <- rast('./tif/nasa/cmip6/historical/ACCESS-CM2/tasmax/cund_tasmax_day_ACCESS-CM2_historical_r1i1p1f1_gn_1974.nc')
rraw.ftr <- rast('./data/tif/nasa/cmip6/ssp245/ACCESS-CM2/tasmax/cund_tasmax_day_ACCESS-CM2_ssp245_r1i1p1f1_gn_2015.nc')

rgwr.bsl <- rast('./tif/nasa/cmip6/historical/ACCESS-CM2/tasmax/down-cund_tasmax_day_ACCESS-CM2_historical_r1i1p1f1_gn_1974.nc')
rdwn.ftr <- rast('./data/tif/nasa/cmip6/ssp245/ACCESS-CM2/tasmax/down/tasmax_2015.tif')

terra::writeRaster(x = mean(rraw.bsl[[1:31]]), filename = './rawbsl_tmpr.tif')

# Basemap -----------------------------------------------------------------
library(OpenStreetMap)
LAT1 =  3.7 ; LAT2 = 5.9
LON1 = -74.9 ; LON2 = -73
map <- openmap(c(LAT2,LON1), c(LAT1,LON2), zoom = NULL, type = c("osm", "stamen-toner", "stamen-terrain","stamen-watercolor", "esri","esri-topo", 'esri-physical', 'esri-shaded')[8], mergeTiles = TRUE)
map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

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
p_load(ggspatial, RColorBrewer)

# SRTM --------------------------------------------------------------------
srtm.tble <- srtm.tble %>% setNames(c('x', 'y', 'value'))

gsrtm <- autoplot(map.latlon) + 
  geom_tile(data = srtm.tble, aes(x = x, y = y, fill = value)) + 
  scale_fill_gradientn(colors = terrain.colors(10)) +
  geom_sf(data = st_as_sf(bsin), fill = NA, col = 'red', inherit.aes = FALSE) + 
  geom_sf(data = st_as_sf(cndn), fill = NA, col = 'grey30', inherit.aes = FALSE) +
  coord_sf(xlim = c(-74.88, -73.04), ylim = c(3.73, 5.83)) +
  labs(x = 'Lon', y = 'Lat', fill = 'Altitud (m.s.n.m)') +
  theme_minimal() +
  theme(legend.position = 'bottom', 
        legend.key.width = unit(2.5, 'line'),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.x = element_text(hjust = 0.5),
        text = element_text(family = 'Gill Sans MT')) +
  guides(fill = guide_legend( 
    direction = 'horizontal',
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )) 

gsrtm
dir_create('./png/maps')
ggsave(plot = gsrtm, filename = './png/maps/srtm_cundinamarca.jpg', units = 'in', width = 9, height = 7, dpi = 300)
  
# Temperature raw baseline ------------------------------------------------
graw.bsl <- autoplot(map.latlon) + 
  geom_tile(data = rraw.bsl.tbl.avg, aes(x = x, y = y, fill = value)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'YlOrRd')) +
  geom_sf(data = st_as_sf(bsin), fill = NA, col = 'red', inherit.aes = FALSE) + 
  geom_sf(data = st_as_sf(cndn), fill = NA, col = 'grey30', inherit.aes = FALSE) +
  coord_sf(xlim = c(-74.88, -73.04), ylim = c(3.73, 5.83)) +
  labs(x = 'Lon', y = 'Lat', fill = 'Temperatura (°C)') +
  theme_minimal() +
  theme(legend.position = 'bottom', 
        legend.key.width = unit(2.5, 'line'),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.x = element_text(hjust = 0.5),
        text = element_text(family = 'Gill Sans MT')) +
  guides(fill = guide_legend( 
    direction = 'horizontal',
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )) 

graw.bsl
ggsave(plot = graw.bsl, filename = './png/maps/temp_raw-bsl_cundinamarca.jpg', units = 'in', width = 9, height = 7, dpi = 300)

# Temperature downscaling GWR baseline ------------------------------------

rgwr.bsl <- rast('./rawbsl_tmpr_GWR.tif')
plot(rgwr.bsl)
rgwr.bsl <- rgwr.bsl - 273.15
rgwr.bsl.tbl.avg <- terra::as.data.frame(rgwr.bsl, xy = T) %>% as_tibble() %>% setNames(c('x', 'y', 'value'))

ggwr.bsl <- autoplot(map.latlon) + 
  geom_tile(data = rgwr.bsl.tbl.avg, aes(x = x, y = y, fill = value)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'YlOrRd')) +
  geom_sf(data = st_as_sf(bsin), fill = NA, col = 'grey90', inherit.aes = FALSE) + 
  geom_sf(data = st_as_sf(cndn), fill = NA, col = 'grey30', inherit.aes = FALSE) +
  coord_sf(xlim = c(-74.88, -73.04), ylim = c(3.73, 5.83)) +
  labs(x = 'Lon', y = 'Lat', fill = 'Temperatura (°C)') +
  theme_minimal() +
  theme(legend.position = 'bottom', 
        legend.key.width = unit(2.5, 'line'),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.x = element_text(hjust = 0.5),
        text = element_text(family = 'Gill Sans MT')) +
  guides(fill = guide_legend( 
    direction = 'horizontal',
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )) 

ggsave(plot = ggwr.bsl, filename = './png/maps/temp_gwr-bsl_cundinamarca.jpg', units = 'in', width = 9, height = 7, dpi = 300)


# To calculate the anomaly  -----------------------------------------------
rraw.dfr <- mean(rraw.ftr[[1:31]]) - mean(rraw.bsl[[1:31]])
rraw.dfr.tbl <- terra::as.data.frame(rraw.dfr, xy = T) %>% as_tibble() %>% setNames(c('x', 'y', 'value'))

gdfr.bsl <- autoplot(map.latlon) + 
  geom_tile(data = rraw.dfr.tbl, aes(x = x, y = y, fill = value)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'YlOrRd')) +
  geom_sf(data = st_as_sf(bsin), fill = NA, col = 'grey90', inherit.aes = FALSE) + 
  geom_sf(data = st_as_sf(cndn), fill = NA, col = 'grey30', inherit.aes = FALSE) +
  coord_sf(xlim = c(-74.88, -73.04), ylim = c(3.73, 5.83)) +
  labs(x = 'Lon', y = 'Lat', fill = 'Delta temperatura (°C)') +
  theme_minimal() +
  theme(legend.position = 'bottom', 
        legend.key.width = unit(2.5, 'line'),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.x = element_text(hjust = 0.5),
        text = element_text(family = 'Gill Sans MT')) +
  guides(fill = guide_legend( 
    direction = 'horizontal',
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )) 

ggsave(plot = gdfr.bsl, filename = './png/maps/temp_dfr-ftr-bsl_cundinamarca.jpg', units = 'in', width = 9, height = 7, dpi = 300)

# To get the centroids ----------------------------------------------------

gcnt <- autoplot(map.latlon) + 
  geom_point(data = rraw.dfr.tbl, aes(x = x, y = y), col = 'black') + 
  geom_sf(data = st_as_sf(bsin), fill = NA, col = 'grey90', inherit.aes = FALSE) + 
  geom_sf(data = st_as_sf(cndn), fill = NA, col = 'grey30', inherit.aes = FALSE) +
  coord_sf(xlim = c(-74.88, -73.04), ylim = c(3.73, 5.83)) +
  labs(x = 'Lon', y = 'Lat') +
  theme_minimal() +
  theme(legend.position = 'bottom', 
        legend.key.width = unit(2.5, 'line'),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.x = element_text(hjust = 0.5),
        text = element_text(family = 'Gill Sans MT')) +
  guides(fill = guide_legend( 
    direction = 'horizontal',
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )) 

ggsave(plot = gcnt, filename = './png/maps/temp_dfr-cnt_cundinamarca.jpg', units = 'in', width = 9, height = 7, dpi = 300)








