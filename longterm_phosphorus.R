# code to visualize long term phosphorus in lake sunapee

library(tidyverse)
library(sf)
library(tmap)
library(raster)
library(gifski)

# read in LMP record
lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/LSPALMP_1986-2020_v2021-03-29.csv')

#read in station locations
lmp_locs <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/station_location_details.csv')

#point to local spatial files folder
gis_dir <- 'F:/GIS_data_general/US_data/project_data/Sunapee/'

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/max_summer_tp/'


## filter and clean up TP ####
#filter for TP
unique(lmp$parameter)

lmp_tp <- lmp %>% 
  filter(parameter == 'TP_mgl') %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(year = format(date, '%Y')) %>% 
  mutate(month = as.numeric(format(date, '%m')))

#filter jun - sept
lmp_summer_tp = lmp_tp %>% 
  filter(month >=6 & month <=9)

#filter for in-lake and longterm sites; epi and integrated only
lmp_tp_lake <- lmp_summer_tp %>% 
  filter(site_type == 'lake') %>% 
  filter(station == 10 |
           station == 20 |
           station == 30 |
           station == 60 |
           station == 70 |
           station == 80 |
           station == 90 |
           station == 110 |
           station == 200 |
           station == 210 |
           station == 220 |
           station == 230) %>% 
  filter(layer == 'E' | layer == 'I')

ggplot(lmp_tp_lake, aes(x = date, y = value)) +
  geom_point() +
  facet_grid(station ~.) +
  theme_bw()

#aggregate to median value
lmp_medtp_lake <- lmp_tp_lake %>% 
  group_by(station, year) %>% 
  summarize(med_tp_mgl = median(value))

ggplot(lmp_medtp_lake, aes(x = year, y = med_tp_mgl, color = station)) +
  geom_point() +
  theme_bw()

#aggregate to max tp
lmp_maxtp_lake <- lmp_tp_lake %>% 
  group_by(station, year) %>% 
  summarize(max_tp_mgl = max(value))

ggplot(lmp_maxtp_lake, aes(x = year, y = max_tp_mgl, color = station)) +
  geom_point() +
  theme_bw()

## get station list and apply loc info ####
stationlist <- data.frame(unique(lmp_tp_lake$station))
colnames(stationlist) = 'station'

stationlist <- left_join(stationlist, lmp_locs)

# apply station info to 2 datasets
lmp_medtp_lake <- full_join(lmp_medtp_lake, stationlist)
lmp_maxtp_lake <- full_join(lmp_maxtp_lake, stationlist)


## load the spatial layers ####

#sunapee shoreline
sunapee_shore = st_read(file.path(gis_dir, 'hydrography/LS_shore_WGS.shp'))
#bathy
sun_bathy <- raster(file.path(gis_dir, 'Sunapee Bathymetry/raster_files/originals/sun_ras_z_m'))
#table to sf for med and max tp
medtp <- st_as_sf(lmp_medtp_lake, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')
maxtp <- st_as_sf(lmp_maxtp_lake, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')

#define bounding box fo vis
#get bounding box of bathy
bbox_sunapee <- st_bbox(sun_bathy) # current bounding box

xrange <- bbox_sunapee$xmax - bbox_sunapee$xmin # range of x values
yrange <- bbox_sunapee$ymax - bbox_sunapee$ymin # range of y values

#create a new one and modify
bbox_sun_new <- st_bbox(sun_bathy) 
bbox_sun_new[1] <- bbox_sun_new[1] - (0.40 * xrange) # xmin - left
bbox_sun_new[3] <- bbox_sun_new[3] + (0.1 * xrange) # xmax - right
bbox_sun_new[2] <- bbox_sun_new[2] - (0.025 * yrange) # ymin - bottom
bbox_sun_new[4] <- bbox_sun_new[4] + (0.05 * yrange) # ymax - top

tm_shape(sun_bathy, bbox = bbox_sun_new)+
  tm_raster(palette = 'Blues',
            title = 'lake depth\n(meters)')+tm_shape(sunapee_shore) +
  tm_borders() +
  tm_shape(maxtp) +
  tm_bubbles('max_tp_mgl',
             col = 'green',
             title.size = 'maximum summer\ntotal phosphorus\n(mg/L)',
             border.col = 'black') +
  tm_layout(legend.position = c('left', 'bottom'),
            paste0('Maximum Summer Total Phosphorus ', yr))

## visualize ####

#store faceted max tp - ncol and nrow must be 1
tp_max_facet <- tm_shape(sun_bathy, bbox = bbox_sun_new)+
  tm_raster(palette = 'Blues',
            title = 'lake depth\n(meters)')+
  tm_shape(sunapee_shore) +
  tm_borders() +
  tm_shape(subset(maxtp, year >= 2000)) +
  tm_bubbles('max_tp_mgl',
             col = 'green',
             title.size = 'maximum summer\ntotal phosphorus\n(mg/L)',
             border.col = 'black', 
             style = 'fixed',
             size.max = 0.25,
             n = 5) +
  tm_layout(legend.position = c('left', 'bottom'),
            'Maximum Summer Total Phosphorus') +
  tm_facets(by = 'year',
            ncol = 1,
            nrow = 1)

#export gif
tmap_animation(tp_max_facet,
               filename = file.path(dump_dir, 'max_tp_summer_2000_2020.gif'),
               fps = 1,
               dpi = 300)



