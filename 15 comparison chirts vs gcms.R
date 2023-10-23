

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

# List temperature files Baseline 
fles.bsln <- dir_ls('./data/tbl', regexp = '.csv$')
tmin.bsln <- grep('Tmin', fles.bsln, value = T)
tmax.bsln <- grep('Tmax', fles.bsln, value = T)



