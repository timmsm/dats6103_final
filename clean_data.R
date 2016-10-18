# Program: clean_data.R
# Purpose: Clean binetflow files for use in data mining

# Load packages -----------------------------------------------------------
library(lubridate)
library(tidyverse)

# Read binetflow files ----------------------------------------------------
binetflow_cols <- cols(
  Dur       = col_double(),
  sTos      = col_integer(),
  dTos      = col_integer(),
  TotPkts   = col_double(),
  TotBytes  = col_double(),
  SrcBytes  = col_double(),
  .default  = col_character()
)

binetflow_files <- list.files("data/", "\\.binetflow$", full.names = TRUE)
binetflow_data  <- binetflow_files %>% 
  map(read_csv, col_types = binetflow_cols) %>% 
  set_names(gsub("data//(.+)\\.binetflow$", "\\1", binetflow_files))

# Define cleaning functions -----------------------------------------------
clean_binetflow <- . %>%   
  set_names(gsub("([a-z])([A-Z])", "\\1_\\2", names(.))) %>% 
  set_names(tolower(names(.))) %>% 
  dmap_at("start_time", ymd_hms) %>% 
  group_by(src_addr) %>% 
  do(mutate_at(
    ., vars(start_time), funs(interval(min(.), max(.)) %>% time_length())
  )) %>% 
  ungroup() %>% 
  mutate(
    label = gsub("^flow=.*(Background|Normal|Botnet).*", "\\1", label),
    pct_src_bytes = src_bytes / tot_bytes
  ) %>% 
  select(start_time, dur, src_addr, state, tot_pkts, src_bytes, tot_bytes,
         pct_src_bytes, label)

roll_src_addr <- . %>% 
  select(-state, -dur) %>% 
  group_by(src_addr, label) %>% 
  summarize(
    start_time = head(start_time, 1),
    tot_pkts   = sum(tot_pkts),
    src_bytes  = sum(src_bytes),
    tot_bytes  = sum(tot_bytes)
  ) %>% 
  ungroup() %>% 
  dmap_at("start_time", ~ if_else(.x == 0, 1, .x)) %>% 
  mutate_if(is_numeric, funs(. / start_time)) %>% 
  mutate(pct_src_bytes = src_bytes / tot_bytes) %>% 
  select(-src_bytes, -tot_bytes)

# Clean binetflow files ---------------------------------------------------
binetflow_clean <- binetflow_data %>% 
  map(clean_binetflow)

binetflow_rolled <- binetflow_clean %>% 
  map(roll_src_addr) %>% 
  set_names(paste0(names(.), "_rolled"))

# Write cleaned files to disk ---------------------------------------------
c(binetflow_clean, binetflow_rolled) %>% 
  walk2(names(.), ~ write_rds(.x, sprintf("data/%s.rds", .y)))
