# Program: normalize_data.R
# Purpose: Use min max normalization to normal numeric data

# Load packages -----------------------------------------------------------
library(tidyverse)

# Load data ---------------------------------------------------------------
binetflow_bf   <- list.files("data/", "\\d\\.rds$", full.names = TRUE)
binetflow_base <- binetflow_bf %>% 
  map(read_rds) %>% 
  set_names(gsub("data/(.+)\\.rds", "\\1", binetflow_bf))

binetflow_rf     <- list.files("data/", "_rolled\\.rds$", full.names = TRUE)
binetflow_rolled <- binetflow_rf %>% 
  map(read_rds) %>% 
  set_names(gsub("data/(.+)\\.rds", "\\1", binetflow_rf))

# Define normalization functions ------------------------------------------
min_ <- partial(min, na.rm = TRUE)
max_ <- partial(max, na.rm = TRUE)

normalize_min_max <- . %>% 
  mutate_if(is.numeric, funs((. - min_(.)) / (max_(.) - min_(.))))

# Normalize data ----------------------------------------------------------
binetflow_base_norm <- binetflow_base %>% 
  map(normalize_min_max) %>% 
  set_names(paste0(names(.), "_normalized"))

binetflow_rolled_norm <- binetflow_rolled %>% 
  map(normalize_min_max) %>% 
  set_names(paste0(names(.), "_normalized"))

# Save normalized data ----------------------------------------------------
c(binetflow_base_norm, binetflow_rolled_norm) %>% 
  walk2(names(.), ~ write_rds(.x, sprintf("data/%s.rds", .y)))
