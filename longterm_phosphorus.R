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
gis_dir <- 'C:/Users/steeleb/Dropbox/travel/gis/project/Sunapee/'

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_tp/'


# filter and clean up TP for inlake TP ####
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

#aggregate to median, max, mean, 3rd quartile value
lmp_agg_lake <- lmp_tp_lake %>% 
  group_by(station, year) %>% 
  summarize(med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value)*1000,
            mean_tp_ugl = mean(value)*1000,
            thquan_tp_ugl = quantile(value, 0.75)*1000)

ggplot(lmp_agg_lake, aes(x = year, y = max_tp_ugl, color = station)) +
  geom_point() +
  theme_bw()

ggplot(lmp_agg_lake, aes(x = year, y = mean_tp_ugl, color = station)) +
  geom_point() +
  theme_bw()

ggplot(lmp_agg_lake, aes(x = year, y = med_tp_ugl, color = station)) +
  geom_point() +
  theme_bw()

ggplot(lmp_agg_lake, aes(x = year, y = thquan_tp_ugl, color = station)) +
  geom_point() +
  theme_bw()


## get station list and apply loc info ####
stationlist <- data.frame(unique(lmp_tp_lake$station))
colnames(stationlist) = 'station'

stationlist <- left_join(stationlist, lmp_locs)

# apply station info to 2 datasets
lmp_agg_lake <- full_join(lmp_agg_lake, stationlist)

## load the spatial layers ####

#sunapee shoreline
sunapee_shore = st_read(file.path(gis_dir, 'hydrography/LS_shore_WGS.shp'))
#bathy
sun_bathy <- raster(file.path(gis_dir, 'Sunapee Bathymetry/raster_files/originals/sun_ras_z_m'))
#sunpaee streams
sun_stream <- st_read(file.path(gis_dir, 'hydrography/streams.shp'))
sun_stream_wgs <- st_transform(sun_stream, crs = 'EPSG:4326')
#sunapee open water
sun_ws_water <- st_read(file.path(gis_dir, 'hydrography/waterbodies open water.shp'))
sun_ws_water_wgs <- st_transform(sun_ws_water, crs = 'EPSG:4326')

#table to sf for med and max tp
aggtp <- st_as_sf(lmp_agg_lake, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')

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

## visualize in paneled plots ####

paneled_maxtp = tm_shape(sun_bathy, bbox = bbox_sun_new) + tm_raster(palette = 'Blues',
            title = 'lake depth\n(meters)',
            contrast = c(0, 0.5)) +
  tm_shape(sunapee_shore) + tm_borders() +
  tm_shape(subset(aggtp, subset = year >=1997)) +
  tm_bubbles('max_tp_ugl',
             col = 'green',
             title.size = 'maximum summer\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_maxtp
tmap_save(paneled_maxtp, filename = file.path(dump_dir, 'max_tp_summer_paneled_1997_2020.png'))

paneled_meantp = tm_shape(sun_bathy, bbox = bbox_sun_new) + tm_raster(palette = 'Blues',
                                                                     title = 'lake depth\n(meters)',
                                                                     contrast = c(0, 0.5)) +
  tm_shape(sunapee_shore) + tm_borders() +
  tm_shape(subset(aggtp, subset = year >=1997)) +
  tm_bubbles('mean_tp_ugl',
             col = 'green',
             title.size = 'mean summer\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_meantp
tmap_save(paneled_meantp, filename = file.path(dump_dir, 'mean_tp_summer_paneled_1997_2020.png'))

paneled_medtp = tm_shape(sun_bathy, bbox = bbox_sun_new) + tm_raster(palette = 'Blues',
                                                                      title = 'lake depth\n(meters)',
                                                                      contrast = c(0, 0.5)) +
  tm_shape(sunapee_shore) + tm_borders() +
  tm_shape(subset(aggtp, subset = year >=1997)) +
  tm_bubbles('med_tp_ugl',
             col = 'green',
             title.size = 'median summer\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_medtp
tmap_save(paneled_medtp, filename = file.path(dump_dir, 'med_tp_summer_paneled_1997_2020.png'))

paneled_thquantp = tm_shape(sun_bathy, bbox = bbox_sun_new) + tm_raster(palette = 'Blues',
                                                                      title = 'lake depth\n(meters)',
                                                                      contrast = c(0, 0.5)) +
  tm_shape(sunapee_shore) + tm_borders() +
  tm_shape(subset(aggtp, subset = year >=1997)) +
  tm_bubbles('thquan_tp_ugl',
             col = 'green',
             title.size = 'third quantile summer\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_thquantp
tmap_save(paneled_thquantp, filename = file.path(dump_dir, 'thquan_tp_summer_paneled_1997_2020.png'))


## visualize in animated plot ####

#store faceted max tp - ncol and nrow must be 1
tp_max_facet <- tm_shape(sun_stream_wgs, bbox = bbox_sun_new) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons(border.col = 'blue', col = '#D9F9FA') +
  tm_shape(sun_bathy)+
  tm_raster(palette = 'Blues',
            title = 'lake depth\n(meters)',
            contrast = c(0, 0.5)) +
  tm_shape(sunapee_shore) + tm_borders() +
  tm_shape(subset(aggtp, subset = year >=1997)) +
  tm_bubbles('max_tp_ugl',
             col = 'green',
             title.size = 'maximum summer\ntotal phosphorus\n(mg/L)',
             border.col = 'black',
             scale = 3) +
  tm_facets(by = 'year',
            ncol = 1,
            nrow = 1) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')

#export gif
tmap_animation(tp_max_facet,
               filename = file.path(dump_dir, 'max_tp_summer_ani_1997_2020_v3.gif'),
               fps = 1,
               dpi = 300)


#store faceted med tp - ncol and nrow must be 1
tp_med_facet <- tm_shape(sun_stream_wgs, bbox = bbox_sun_new) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons(border.col = 'blue', col = '#D9F9FA') +
  tm_shape(sun_bathy)+
  tm_raster(palette = 'Blues',
            title = 'lake depth\n(meters)',
            contrast = c(0, 0.5)) +
  tm_shape(sunapee_shore) + tm_borders() +
  tm_shape(subset(aggtp, subset = year >=1997)) +
  tm_bubbles('med_tp_ugl',
             col = 'green',
             title.size = 'median summer\ntotal phosphorus\n(mg/L)',
             border.col = 'black',
             scale = 3) +
  tm_facets(by = 'year',
            ncol = 1,
            nrow = 1)+
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')


#export gif
tmap_animation(tp_med_facet,
               filename = file.path(dump_dir, 'med_tp_summer_ani_1997_2020_v3.gif'),
               fps = 1,
               dpi = 300)



#store faceted mean tp - ncol and nrow must be 1
tp_mean_facet <- tm_shape(sun_stream_wgs, bbox = bbox_sun_new) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons(border.col = 'blue', col = '#D9F9FA') +
  tm_shape(sun_bathy)+
  tm_raster(palette = 'Blues',
            title = 'lake depth\n(meters)',
            contrast = c(0, 0.5)) +
  tm_shape(sunapee_shore) + tm_borders() +
  tm_shape(subset(aggtp, subset = year >=1997)) +
  tm_bubbles('mean_tp_ugl',
             col = 'green',
             title.size = 'mean summer\ntotal phosphorus\n(mg/L)',
             border.col = 'black',
             scale = 3) +
  tm_facets(by = 'year',
            ncol = 1,
            nrow = 1)+
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')


#export gif
tmap_animation(tp_mean_facet,
               filename = file.path(dump_dir, 'mean_tp_summer_ani_1997_2020_v3.gif'),
               fps = 1,
               dpi = 300)


#store faceted thquan tp - ncol and nrow must be 1
tp_thquan_facet <- tm_shape(sun_stream_wgs, bbox = bbox_sun_new) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons(border.col = 'blue', col = '#D9F9FA') +
  tm_shape(sun_bathy)+
  tm_raster(palette = 'Blues',
            title = 'lake depth\n(meters)',
            contrast = c(0, 0.5)) +
  tm_shape(sunapee_shore) + tm_borders() +
  tm_shape(subset(aggtp, subset = year >=1997)) +
  tm_bubbles('thquan_tp_ugl',
             col = 'green',
             title.size = 'third quantile summer\ntotal phosphorus\n(mg/L)',
             border.col = 'black',
             scale = 3) +
  tm_facets(by = 'year',
            ncol = 1,
            nrow = 1)+
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')


#export gif
tmap_animation(tp_thquan_facet,
               filename = file.path(dump_dir, 'thquan_tp_summer_ani_1997_2020_v3.gif'),
               fps = 1,
               dpi = 300)


# look at TP from streams ####
lmp_summer_tp_stream <- lmp_summer_tp %>% 
  filter(site_type == 'stream') %>% 
  arrange(station)


stream_locs = read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/station_location_details.csv')
stream_locs <- stream_locs %>% 
  filter(site_type == 'stream' &
           status == 'ongoing' &
           last_year == 2020 &
           first_year <= 1994 &
           !is.na(lat_dd))

lmp_summer_tp_stream <- right_join(lmp_summer_tp_stream, stream_locs)

unique(lmp_summer_tp_stream$station)

ggplot(lmp_summer_tp_stream, aes(x = date, y = value)) +
  geom_point() +
  facet_grid(station ~ .)

#drop a few more incomplete streams
lmp_summer_tp_stream <- lmp_summer_tp_stream %>% 
  filter(station != 715 &
           station != 1415)

ggplot(lmp_summer_tp_stream, aes(x = date, y = value)) +
  geom_point() +
  facet_grid(station ~ .)

#aggregate and change units
agg_tp_stream <- lmp_summer_tp_stream %>% 
  group_by(station, year, lat_dd, lon_dd) %>% 
  summarize(n = n(),
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value)*1000,
            mean_tp_ugl = mean(value)*1000,
            thquan_tp_ugl = quantile(value, 0.75)*1000) %>% 
  filter(n > 3) 

## visualize stream TP in paneled plots ----

#table to sf for med and max tp
aggtp_stream <- st_as_sf(agg_tp_stream, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')

# sunapee watershed
sun_ws <- st_read(file.path(gis_dir, 'watersheds/NH_hydro_Sunapee/Lake_Sunapee_watershed.shp'))
sun_ws_wgs <- st_transform(sun_ws, crs = 'EPSG:4326')

bbox_sun_ws <- st_bbox(sun_ws_wgs)

## visualize in paneled plots ####
paneled_maxtp_stream = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggtp_stream, subset = year >=1997)) +
  tm_bubbles('max_tp_ugl',
             col = 'green',
             title.size = 'maximum summer\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_maxtp_stream

tmap_save(paneled_maxtp_stream, filename = file.path(dump_dir, 'max_tp_summer_stream_paneled_1997_2020.png'))

paneled_meantp_stream = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggtp_stream, subset = year >=1997)) +
  tm_bubbles('mean_tp_ugl',
             col = 'green',
             title.size = 'mean summer\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_meantp_stream

tmap_save(paneled_meantp_stream, filename = file.path(dump_dir, 'mean_tp_summer_stream_paneled_1997_2020.png'))

paneled_medtp_stream = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggtp_stream, subset = year >=1997)) +
  tm_bubbles('med_tp_ugl',
             col = 'green',
             title.size = 'median summer\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_medtp_stream

tmap_save(paneled_medtp_stream, filename = file.path(dump_dir, 'med_tp_summer_stream_paneled_1997_2020.png'))

paneled_thquantp_stream = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggtp_stream, subset = year >=1997)) +
  tm_bubbles('thquan_tp_ugl',
             col = 'green',
             title.size = 'third quantile summer\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_thquantp_stream

tmap_save(paneled_thquantp_stream, filename = file.path(dump_dir, 'thquan_tp_summer_stream_paneled_1997_2020.png'))

## vis in animated plots ----

#store faceted max tp - ncol and nrow must be 1
tp_max_stream_facet <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggtp_stream, subset = year >=1997)) +
  tm_bubbles('max_tp_ugl',
             col = 'green',
             title.size = 'maximum summer\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 3) +
  tm_facets(by = 'year',
            ncol = 1,
            nrow = 1) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')

#export gif
tmap_animation(tp_max_stream_facet,
               filename = file.path(dump_dir, 'max_tp_summer_stream_ani_1997_2020.gif'),
               fps = 1,
               dpi = 300)

#store faceted med tp - ncol and nrow must be 1
tp_med_stream_facet <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggtp_stream, subset = year >=1997)) +
  tm_bubbles('med_tp_ugl',
             col = 'green',
             title.size = 'median summer\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 3) +
  tm_facets(by = 'year',
            ncol = 1,
            nrow = 1) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')

#export gif
tmap_animation(tp_med_stream_facet,
               filename = file.path(dump_dir, 'med_tp_summer_stream_ani_1997_2020.gif'),
               fps = 1,
               dpi = 300)

#store faceted mean tp - ncol and nrow must be 1
tp_mean_stream_facet <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggtp_stream, subset = year >=1997)) +
  tm_bubbles('mean_tp_ugl',
             col = 'green',
             title.size = 'mean summer\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 3) +
  tm_facets(by = 'year',
            ncol = 1,
            nrow = 1) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')

#export gif
tmap_animation(tp_mean_stream_facet,
               filename = file.path(dump_dir, 'mean_tp_summer_stream_ani_1997_2020.gif'),
               fps = 1,
               dpi = 300)

#store faceted thquan tp - ncol and nrow must be 1
tp_thquan_stream_facet <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggtp_stream, subset = year >=1997)) +
  tm_bubbles('thquan_tp_ugl',
             col = 'green',
             title.size = 'third quantile summer\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 3) +
  tm_facets(by = 'year',
            ncol = 1,
            nrow = 1) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')

#export gif
tmap_animation(tp_thquan_stream_facet,
               filename = file.path(dump_dir, 'thquan_tp_summer_stream_ani_1997_2020.gif'),
               fps = 1,
               dpi = 300)


# both together (paneled only) ----

#max tp stream and lake
paneled_maxtp_stream_lake = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggtp_stream, subset = year >=1997)) +
  tm_symbols(size = 'max_tp_ugl',
             col = 'green',
             shape = 24,
             title.size = 'maximum summer\nstream\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_shape(subset(aggtp, subset = year >=1997)) +
  tm_symbols(size = 'max_tp_ugl',
             col = 'yellow',
             shape = 21,
             title.size = 'maximum summer\nin lake\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_maxtp_stream_lake

tmap_save(paneled_maxtp_stream_lake, filename = file.path(dump_dir, 'max_tp_summer_streamlake_paneled_1997_2020.png'))

#mean tp stream and lake
paneled_meantp_stream_lake = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggtp_stream, subset = year >=1997)) +
  tm_symbols(size = 'mean_tp_ugl',
             col = 'green',
             shape = 24,
             title.size = 'mean summer\nstream\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_shape(subset(aggtp, subset = year >=1997)) +
  tm_symbols(size = 'mean_tp_ugl',
             col = 'yellow',
             shape = 21,
             title.size = 'mean summer\nin lake\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_meantp_stream_lake

tmap_save(paneled_meantp_stream_lake, filename = file.path(dump_dir, 'mean_tp_summer_streamlake_paneled_1997_2020.png'))

#med tp stream and lake
paneled_medtp_stream_lake = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggtp_stream, subset = year >=1997)) +
  tm_symbols(size = 'med_tp_ugl',
             col = 'green',
             shape = 24,
             title.size = 'medain summer\nstream\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_shape(subset(aggtp, subset = year >=1997)) +
  tm_symbols(size = 'med_tp_ugl',
             col = 'yellow',
             shape = 21,
             title.size = 'median summer\nin lake\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_medtp_stream_lake

tmap_save(paneled_medtp_stream_lake, filename = file.path(dump_dir, 'med_tp_summer_streamlake_paneled_1997_2020.png'))

#thquan tp stream and lake
paneled_thquantp_stream_lake = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggtp_stream, subset = year >=1997)) +
  tm_symbols(size = 'thquan_tp_ugl',
             col = 'green',
             shape = 24,
             title.size = 'third quantile\nsummer stream\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_shape(subset(aggtp, subset = year >=1997)) +
  tm_symbols(size = 'thquan_tp_ugl',
             col = 'yellow',
             shape = 21,
             title.size = 'third quantile\nsummer in lake\ntotal phosphorus\n(ug/L)',
             border.col = 'black',
             scale = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_thquantp_stream_lake

tmap_save(paneled_thquantp_stream_lake, filename = file.path(dump_dir, 'thquan_tp_summer_streamlake_paneled_1997_2020.png'))


