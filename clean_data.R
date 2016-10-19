# Program: clean_data.R
# Purpose: Clean binetflow files for use in data mining

# Load packages -----------------------------------------------------------
library(tidyverse)

# Read binetflow files ----------------------------------------------------

# Define the column types for the binetflow files
binetflow_cols <- cols(
  Dur       = col_double(),
  sTos      = col_integer(),
  dTos      = col_integer(),
  TotPkts   = col_double(),
  TotBytes  = col_double(),
  SrcBytes  = col_double(),
  .default  = col_character()
)

# Read each binetflow file into a list, parse the columns, and name the items
# of the list using the file name
binetflow_files <- list.files("data/", "\\.binetflow$", full.names = TRUE)
binetflow_data  <- binetflow_files %>% 
  map(read_csv, col_types = binetflow_cols) %>% 
  set_names(gsub("data/(.+)\\.binetflow$", "\\1", binetflow_files))

# Define cleaning functions -----------------------------------------------
clean_binetflow <- . %>%   
  set_names(gsub("([a-z])([A-Z])", "\\1_\\2", names(.))) %>% # set variable names to
  set_names(tolower(names(.))) %>%                           # snake case
  mutate( # Simplfy the label text and create source to total bytes ratio
    label = gsub("^flow=.*(Background|Normal|Botnet).*", "\\1", label),
    pct_src_bytes = src_bytes / tot_bytes
  ) %>% 
  select(-start_time, -dst_addr, -s_tos, -d_tos) # drop irrelevant variables

roll_src_addr <- . %>% 
  select(-state) %>% 
  group_by(src_addr, label) %>% # roll up data to source address, label level
  summarize( # sum of each numeric variable
    dur       = sum(dur),
    tot_pkts  = sum(tot_pkts),
    src_bytes = sum(src_bytes),
    tot_bytes = sum(tot_bytes)
  ) %>% 
  ungroup() %>% 
  mutate_if(is_numeric, funs(. / dur)) %>%  # add "velocity" by deviding by time
  mutate(pct_src_bytes = src_bytes / tot_bytes) %>%  # create bytes ratio
  select(-src_bytes, -tot_bytes) # drop unneccessary variables
  
# Clean binetflow files ---------------------------------------------------
binetflow_clean <- binetflow_data %>% # apply cleaning functions to data files
  map(clean_binetflow)

binetflow_rolled <- binetflow_clean %>% # apply rollup function to cleaned files
  map(roll_src_addr) %>% 
  set_names(paste0(names(.), "_rolled"))

# Write cleaned files to disk ---------------------------------------------
c(binetflow_clean, binetflow_rolled) %>% 
  map(~ dmap_if(.x, is_character, as.factor)) %>% # convert characters to factors
  walk2(names(.), ~ write_rds(.x, sprintf("data/%s.rds", .y))) # save datasets as rds files
