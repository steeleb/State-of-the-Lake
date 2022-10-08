#map of limited sites referenced in figures

library(sf)
library(tmap)
library(tidyverse)
library(readxl)

#features dir
feat_dir <- 'C:/Users/steeleb/Dropbox/travel/gis/project/Sunapee/'

#read in station locations
lmp_locs <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/station_location_details.csv')

lmp_lake_names <- read_xls('https://github.com/Lake-Sunapee-Protective-Association/LMP/main/raw%20data%20files/station%20locations/Cove%20%20Deep%20+%20Buoy%20WQ%20Sample%20Point%20GPS%20Coordinates.xls')
lmp_trib_names <- 

lmp_shortlist <- lmp_locs %>% 
  filter(last_year == 2020 & first_year < 1995)

# bring in spatial layers ----
lake <- read_sf(file.path(feat_dir, 'hydrography/LS_shore_WGS.shp'))
watershed <- read_sf(file.path(feat_dir, 'watersheds/NH_hydro_Sunapee/Lake_Sunapee_watershed.shp'))
watershed <- st_transform(watershed, crs = 'epsg:4326')
streams <- read_sf(file.path(feat_dir, 'hydrography/streams.shp'))
streams <- st_transform(streams, crs = 'epsg:4326')
waterbodies <- read_sf(file.path(feat_dir, 'hydrography/waterbodies open water.shp'))
waterbodies <- st_transform(waterbodies, crs = 'epsg:4326')
roads <- read_sf(file.path(feat_dir, 'roads/roads_sun_wshed.shp'))
roads <- st_transform(roads, crs = 'epsg:4326')

lmp <- st_as_sf(lmp_shortlist, 
                coords = c('lon_dd', 'lat_dd'),
                crs = 'epsg:4326') %>% 
  mutate(sub_site_type = case_when(site_type == 'stream' ~ 'tributary',
                                   TRUE ~ sub_site_type))

tm_shape(watershed) +
  tm_polygons(col = 'white')+
  tm_shape(streams) +
  tm_lines(col = 'darkblue') +
  tm_shape(waterbodies) +
  tm_polygons(col = 'lightblue') +
  tm_shape(roads) +
  tm_lines(col = 'lightgrey', size = 'NO_LANES') +
  tm_shape(lmp) +
  tm_dots(shape = 'sub_site_type', col = 'sub_site_type', size = 1, labels = name) +
  tm_layout(legend.outside = T)
