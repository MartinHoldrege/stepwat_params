# Purpose: select sites for a limited set of simulations that test paramaters,
# those should include the 15 sites used for validation in Palmquist et al. 2018.

# Author: Martin Holdrege

# Script Started: Sept 25, 2024


# params ------------------------------------------------------------------

n_sites <- 20 # target number of sites to select (including the 15 from Pennington)

# dependencies ------------------------------------------------------------

library(tidyverse)
library(sf)

# read in data ------------------------------------------------------------

# file from KP, these are the the coordinates for the 15 sites that were part of Victoria Pennington’s MS thesis (Bill Lauenroth's student). 
sites_vict <- read_csv("data-raw/victoria.sites.latlong.csv")

# the 200 sites that have been used for all recent region-wide STEPWAT2 simulations
sites_200 <- read_csv("../../grazing_effects/data_raw/site_locations.csv")

# climate (for site selection)
# file created in grazing_effects/scripts/00_query_weather_db.R
clim1 <- read_csv("../../grazing_effects/data_processed/site_means/dbWeather_200sites.csv")

# convert to sf -----------------------------------------------------------

sf_vict <- sf::st_as_sf(sites_vict, coords = c("Longitude", "Latitude"),
             crs = 'EPSG:4326') %>% 
  select(-State, -Category, -Site)

sf_200 <- sf::st_as_sf(sites_200, coords = c("X_WGS84", "Y_WGS84"),
                       crs = 'EPSG:4326') 

# id site numbers of the 15 sites -----------------------------------------

nearest_index <- st_nearest_feature(sf_vict, sf_200)

distances <- st_distance(sf_vict, sf_200[nearest_index, ], by_element = TRUE)

# make sure distances are close (< 1 km) so that sites are actually matching
# coordinates not being identical could be due to actual field site locations
# vs grid-cell centers used for simulations?
stopifnot(as.numeric(distances) < 1000) 

sf_vict$site <- sf_200$site_id[nearest_index]

# plot(sf_200$geometry)
# plot(sf_vict$geometry, add = TRUE, col = 'blue')


# prep clim df ------------------------------------------------------------

clim2 <- clim1 %>% 
  rename(site = Site_id, MAT = MAT_C, MAP = MAP_mm) %>% 
  mutate(field_site = site %in% sf_vict$site) %>% 
  arrange(site) %>% 
  select(-CorrTP)

stopifnot(1:200 %in% clim2$site) # weather db has sites codes from 1-200

# select additional sites -------------------------------------------------
# selection based on coverage of climate

# use the 15 sites, and then iteratively select additional sites that 
#are most different


clim_norm <- clim2 %>% 
  select(CorrTP2, MAT, MAP) %>% 
  as.matrix() %>% 
  scale() 

clim_norm[ , 'CorrTP2'] <- clim_norm[ , 'CorrTP2']*2 # weighting precip seasonality more
# --so make sure so sites along seasonality gradient are prioritized

row.names(clim_norm) <- clim2$site

# using greedy algorithm to find sites dissimilar to the already existing
# 15 sites
selected_sites <- clim2$site[clim2$field_site]  # Randomly select the first site

for (i in sum(clim2$field_site):(n_sites - 1)) {
  # Calculate distances between selected sites and all other sites
  not_selected <- clim2$site[!clim2$site %in% selected_sites]
  distances <- rdist::cdist(clim_norm[not_selected, ], clim_norm[selected_sites, ])
  # Find the minimum distance to any already-selected site
  min_distances <- apply(distances, 1, min)
  # Select the site that is furthest from any of the already-selected sites
  next_site <- not_selected[which.max(min_distances)]
  selected_sites <- c(selected_sites, next_site)
}

clim3 <- clim2 %>% 
  mutate(type = case_when(
    field_site ~ 'field site',
    site %in% selected_sites ~ 'additional selected',
    TRUE ~ 'other'))

# visualize climate envelope 
point <- geom_point(aes(color = type, shape = type))
ggplot(clim3, aes(CorrTP2, MAT)) +
  point

ggplot(clim3, aes(CorrTP2, MAP)) +
  point 

ggplot(clim3, aes(MAT, MAP)) +
  point 


# save outpput ------------------------------------------------------------

write_csv(clim3, 'data/sites_for_testing.R')
