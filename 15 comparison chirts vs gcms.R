

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(elevatr, remotes, readxl, xlsx, openxlsx, chirps, spatialEco, terra, RSAGA, fs, sf, readxl, openxlsx, tidyverse, glue, gtools, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
path <- './data/tbl'
fles <- dir_ls(path, regexp = '.csv')
fles <- as.character(fles)

fles.tasm <- grep('tas', fles, value = TRUE)



