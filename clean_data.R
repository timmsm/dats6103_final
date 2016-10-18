# Program: clean_data.R
# Purpose: Clean binetflow files for use in data mining

# Load packages -----------------------------------------------------------
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
  mutate(
    label = gsub("^flow=.*(Background|Normal|Botnet).*", "\\1", label),
    pct_src_bytes = src_bytes / tot_bytes
  ) %>% 
  select(dur, src_addr, state, tot_pkts, src_bytes, tot_bytes, pct_src_bytes, label)

roll_src_addr <- . %>% 
  select(-state) %>% 
  group_by(src_addr, label) %>% 
  summarize(
    dur       = sum(dur),
    tot_pkts  = sum(tot_pkts),
    src_bytes = sum(src_bytes),
    tot_bytes = sum(tot_bytes)
  ) %>% 
  ungroup() %>% 
  mutate_if(is_numeric, funs(. / dur)) %>% 
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
