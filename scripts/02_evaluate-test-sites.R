# Purpose: evaluate the subsets of sites selected for testing, to see
# if some of the unrealistic relationships we're trying to fix can 
# be re-created with just these sites

# Author: Martin Holdrege

# Started: Sept. 26, 2024

# params ------------------------------------------------------------------

run <- "fire1_eind1_c4grass1_co20_2311"
graze <- 'Moderate'
dir_graze <- "../../grazing_effects" # path to grazing effects directory
# dependencies ------------------------------------------------------------

library(tidyverse)
source('R/paths.R')
source("R/fig_params.R")

# read in data ------------------------------------------------------------

# test sites, file created in scripts/01_select-sites.R
tsites1 <- read_csv("data/sites_for_testing.R",
                    show_col_types = FALSE)

# summarized data for biomass plots

# created in grazing_effets/scripts/02_summarize_bio.R
bio <- readRDS(file.path(dir_graze, 'data_processed/site_means/summarize_bio.RDS'))
pft5_bio2 <- bio$pft5_bio2
# list of dataframes generated grazing_effects/scripts/examine_transpiration.R
df_list0 <- readRDS(file.path(
  dir_graze, 
  "data_processed/site_means/transpiration_dfs_fire1eind1c4grass0C020grazeMCurrent"))


# prepare dataframes ------------------------------------------------------

site_ids <- tsites1$site
df_list1 <- map(df_list0, function(df) {
  filter(df, site %in% site_ids) 
})

# just light grazing, and current conditions

bio_simple1 <- pft5_bio2 %>% 
  filter(RCP == 'Current',
         run == !!run,
         graze == !!graze,
         site %in% site_ids) %>% 
  select(-psp) %>% 
  pivot_longer(cols = c('PPT', 'Temp', 'CorrTP2'),
               names_to = 'clim_var',
               values_to = 'clim_value') %>% 
  mutate(clim_var = c('PPT' = 'MAP',
                      'Temp' = 'MAT',
                      'CorrTP2' = 'T-P Correlation')[clim_var] # lookup vector
  )

for (pft in c('Sagebrush', 'Pherb', 'Cheatgrass')) {
  g <- bio_simple1 %>% 
    filter(PFT == pft) %>% 
    ggplot(aes(clim_value, biomass)) +
    geom_point() +
    geom_smooth(se = FALSE) +
    facet_wrap(~clim_var,
               scales = 'free_x') +
    labs(y = lab_bio0,
         x = NULL,
         title = pft,
         caption = paste0(
           run, 
           '\n', graze, ' grazing, historical time period'
         ))
  print(g)
}

