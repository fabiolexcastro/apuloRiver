

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

# Precipitation -----------------------------------------------------------
fles.prec <- grep('prec', fles, value = T)
prec.bsln <- grep('prec-hist', fles.prec, value = T)
prec.ftre <- grep('prec-ftre', fles.prec, value = T)

# Baseline 
prec.bsln <- map(.x = prec.bsln, .f = read.xlsx)

# Future 
prec.ftre <- map(.x = prec.ftre, .f = read.xlsx)