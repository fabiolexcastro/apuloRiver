

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

