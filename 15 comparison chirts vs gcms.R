

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

# CHIRTS 
fles.chrt <- grep('chirts', fles, value = T)
tbls.chrt <- map(fles.chrt, read_csv)
tbls.chrt <- bind_rows(tbls.chrt)
tbls.chrt <- dplyr::select(tbls.chrt, Subbasin, variable, date, value)
tbls.chrt <- mutate(tbls.chrt, year = str_sub(date, 1, 4))
tbls.chrt <- tbls.chrt %>% group_by(Subbasin, variable, year) %>% dplyr::summarise(value = mean(value)) %>% ungroup()