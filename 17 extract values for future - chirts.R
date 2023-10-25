

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, tidyverse, rgeos, readxl, openxlsx, gtools, stringr, glue)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# List the files ----------------------------------------------------------
fles <- dir_ls('./data/tbl') %>% as.character()
fles.tasm.ftre <- grep('tas', fles, value = T) %>% grep('ftre', ., value = T)
