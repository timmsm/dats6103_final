# Program: visualize_eda.R
# Purpose: Visualize binetflow data for exploratory data analysis

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

# Define visualization functions ------------------------------------------
create_boxplots <- function(data, filename) {
  data %>% 
    select(dur, tot_pkts, pct_src_bytes, label) %>% 
    gather(column, value, -label) %>% 
    ggplot(aes(x = column, y = value)) +
    geom_boxplot() +
    facet_grid(~ label) +
    labs(x = "", y = "") +
    ggsave(sprintf("plots/%s_boxplot.png", filename))
  
  data
}

create_density <- function(data, filename) {
  for (col in c("dur", "tot_pkts", "pct_src_bytes")) {
    ggplot(data, aes_string(x = col)) +
      geom_density(fill = "light blue", fill = "light blue") +
      facet_grid(~ label) +
      ggsave(sprintf("plots/%s_%s_density.png", filename, col))
  }
  
  data
}

# Visualize data ----------------------------------------------------------
plot_all <- . %>% 
  walk2(names(.), create_boxplots) %>% 
  walk2(names(.), create_density)

list(binetflow_base, binetflow_rolled) %>% 
  walk(plot_all)
