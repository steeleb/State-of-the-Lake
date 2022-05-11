# code to visualize long term phosphorus in lake sunapee

library(sf)
library(tmap)
library(raster)
library(gifski)
library(tidyverse)

# read in LMP record
lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/LSPALMP_1986-2020_v2021-03-29.csv')

#read in station locations
lmp_locs <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/station_location_details.csv')

#point to local spatial files folder
gis_dir <- 'C:/Users/steeleb/Dropbox/travel/gis/project/Sunapee/'

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_cond/'


# filter and clean up cond for inlake cond ####
#filter for cond
unique(lmp$parameter)

lmp_cond <- lmp %>% 
  filter(parameter == 'cond_uScm') %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(year = format(date, '%Y')) %>% 
  mutate(month = as.numeric(format(date, '%m')))

#filter jun - sept
lmp_summer_cond = lmp_cond %>% 
  filter(month >=6 & month <=9)

#filter for in-lake and longterm sites; epi and integrated only
lmp_cond_lake <- lmp_summer_cond %>% 
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

ggplot(lmp_cond_lake, aes(x = date, y = value)) +
  geom_point() +
  facet_grid(station ~.) +
  theme_bw()

#aggregate to median, max, mean, 3rd quartile value
lmp_agg_lake <- lmp_cond_lake %>% 
  group_by(station, year) %>% 
  summarize(med_cond_uScm = median(value),
            max_cond_uScm = max(value),
            mean_cond_uScm = mean(value),
            thquan_cond_uScm = quantile(value, 0.75))

lmp_agg_lake %>% 
  mutate(loc = case_when(station < 200 ~ 'near-shore',
                         TRUE ~ 'deep')) %>% 
  ggplot(., aes(x = year, y = mean_cond_uScm, shape = loc)) +
  geom_point(size = 2) +
  theme_bw()



## get station list and apply loc info ####
stationlist <- data.frame(unique(lmp_cond_lake$station))
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

#table to sf for med and max cond
aggcond <- st_as_sf(lmp_agg_lake, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')

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

paneled_meancond = tm_shape(sun_bathy, bbox = bbox_sun_new) + tm_raster(palette = 'Blues',
                                                                     title = 'lake depth\n(meters)',
                                                                     contrast = c(0, 0.5)) +
  tm_shape(sunapee_shore) + tm_borders() +
  tm_shape(subset(aggcond, subset = year >=1997)) +
  tm_bubbles(col = 'mean_cond_uScm',
             title.col = 'mean summer\nconductivity\n(uS/cm)',
             border.col = 'black') +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_meancond

tmap_save(paneled_meancond, filename = file.path(dump_dir, 'mean_cond_summer_paneled_1997_2020.png'))


paneled_medcond = tm_shape(sun_bathy, bbox = bbox_sun_new) + tm_raster(palette = 'Blues',
                                                                        title = 'lake depth\n(meters)',
                                                                        contrast = c(0, 0.5)) +
  tm_shape(sunapee_shore) + tm_borders() +
  tm_shape(subset(aggcond, subset = year >=1997)) +
  tm_bubbles(col = 'med_cond_uScm',
             title.col = 'median summer\nconductivity\n(uS/cm)',
             border.col = 'black') +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_medcond

tmap_save(paneled_medcond, filename = file.path(dump_dir, 'med_cond_summer_paneled_1997_2020.png'))




## visualize in animated plot ####


#store faceted med cond - ncol and nrow must be 1
cond_med_facet <- tm_shape(sun_bathy, bbox = bbox_sun_new) + tm_raster(palette = 'Blues',
                                                                       title = 'lake depth\n(meters)',
                                                                       contrast = c(0, 0.5)) +
  tm_shape(sunapee_shore) + tm_borders() +
  tm_shape(subset(aggcond, subset = year >=1997)) +
  tm_bubbles(col = 'med_cond_uScm',
             title.col = 'median summer\nconductivity\n(uS/cm)',
             border.col = 'black') +
  tm_facets(by = 'year',
            ncol = 1,
            nrow = 1)+
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')


#export gif
tmap_animation(cond_med_facet,
               filename = file.path(dump_dir, 'med_cond_summer_ani_1997_2020_v3.gif'),
               fps = 1,
               dpi = 300)



#store faceted mean cond - ncol and nrow must be 1
cond_mean_facet <- tm_shape(sun_bathy, bbox = bbox_sun_new) + tm_raster(palette = 'Blues',
                                                                        title = 'lake depth\n(meters)',
                                                                        contrast = c(0, 0.5)) +
  tm_shape(sunapee_shore) + tm_borders() +
  tm_shape(subset(aggcond, subset = year >=1997)) +
  tm_bubbles(col = 'mean_cond_uScm',
             title.col = 'mean summer\nconductivity\n(uS/cm)',
             border.col = 'black') +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold') +
  tm_facets(by = 'year',
            ncol = 1,
            nrow = 1)+
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')


#export gif
tmap_animation(cond_mean_facet,
               filename = file.path(dump_dir, 'mean_cond_summer_ani_1997_2020_v3.gif'),
               fps = 1,
               dpi = 300)



# look at cond from streams ####
lmp_summer_cond_stream <- lmp_summer_cond %>% 
  filter(site_type == 'stream') %>% 
  arrange(station)


stream_locs = read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/station_location_details.csv')
stream_locs <- stream_locs %>% 
  filter(site_type == 'stream' &
           status == 'ongoing' &
           last_year == 2020 &
           first_year <= 1994 &
           !is.na(lat_dd))

lmp_summer_cond_stream <- right_join(lmp_summer_cond_stream, stream_locs)

unique(lmp_summer_cond_stream$station)

ggplot(lmp_summer_cond_stream, aes(x = date, y = value)) +
  geom_point() +
  facet_grid(station ~ .)

#drop a few more incomplete streams
lmp_summer_cond_stream <- lmp_summer_cond_stream %>% 
  filter(station != 715 &
           station != 1415)

ggplot(lmp_summer_cond_stream, aes(x = date, y = value)) +
  geom_point() +
  facet_grid(station ~ .)

#aggregate and change units
agg_cond_stream <- lmp_summer_cond_stream %>% 
  group_by(station, year, lat_dd, lon_dd) %>% 
  summarize(n = n(),
            med_cond_uScm = median(value),
            max_cond_uScm = max(value),
            mean_cond_uScm = mean(value),
            thquan_cond_uScm = quantile(value, 0.75)) %>% 
  filter(n > 3) 

## visualize stream cond in paneled plots ----

#table to sf for med and max cond
aggcond_stream <- st_as_sf(agg_cond_stream, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')

# sunapee watershed
sun_ws <- st_read(file.path(gis_dir, 'watersheds/NH_hydro_Sunapee/Lake_Sunapee_watershed.shp'))
sun_ws_wgs <- st_transform(sun_ws, crs = 'EPSG:4326')

bbox_sun_ws <- st_bbox(sun_ws_wgs)

## visualize in paneled plots ####
paneled_maxcond_stream = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'grey') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggcond_stream, subset = year >=1997)) +
  tm_bubbles('max_cond_uScm',
             col = 'green',
             title.size = 'maximum summer\nconductivity\n(uS/cm)',
             border.col = 'black',
             scale = 3) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_maxcond_stream

tmap_save(paneled_maxcond_stream, filename = file.path(dump_dir, 'max_cond_summer_stream_paneled_1997_2020.png'))

paneled_meancond_stream = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'grey') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggcond_stream, subset = year >=1997)) +
  tm_bubbles('mean_cond_uScm',
             col = 'green',
             title.size = 'mean summer\nconductivity\n(uS/cm)',
             border.col = 'black',
             scale = 3) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_meancond_stream

tmap_save(paneled_meancond_stream, filename = file.path(dump_dir, 'mean_cond_summer_stream_paneled_1997_2020.png'))

paneled_medcond_stream = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'grey') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggcond_stream, subset = year >=1997)) +
  tm_bubbles('med_cond_uScm',
             col = 'green',
             title.size = 'median summer\nconductivity\n(uS/cm)',
             border.col = 'black',
             scale = 3) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_medcond_stream

tmap_save(paneled_medcond_stream, filename = file.path(dump_dir, 'med_cond_summer_stream_paneled_1997_2020.png'))

paneled_thquancond_stream = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'grey') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggcond_stream, subset = year >=1997)) +
  tm_bubbles('thquan_cond_uScm',
             col = 'green',
             title.size = 'third quantile summer\nconductivity\n(uS/cm)',
             border.col = 'black',
             scale = 3) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_thquancond_stream

tmap_save(paneled_thquancond_stream, filename = file.path(dump_dir, 'thquan_cond_summer_stream_paneled_1997_2020.png'))

## vis in animated plots ----

#store faceted max cond - ncol and nrow must be 1
cond_max_stream_facet <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggcond_stream, subset = year >=1997)) +
  tm_bubbles('max_cond_uScm',
             col = 'green',
             title.size = 'maximum summer\nconductivity\n(uS/cm)',
             border.col = 'black',
             scale = 3) +
  tm_facets(by = 'year',
            ncol = 1,
            nrow = 1) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')

#export gif
tmap_animation(cond_max_stream_facet,
               filename = file.path(dump_dir, 'max_cond_summer_stream_ani_1997_2020.gif'),
               fps = 1,
               dpi = 300)

#store faceted med cond - ncol and nrow must be 1
cond_med_stream_facet <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggcond_stream, subset = year >=1997)) +
  tm_bubbles('med_cond_uScm',
             col = 'green',
             title.size = 'median summer\nconductivity\n(uS/cm)',
             border.col = 'black',
             scale = 3) +
  tm_facets(by = 'year',
            ncol = 1,
            nrow = 1) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')

#export gif
tmap_animation(cond_med_stream_facet,
               filename = file.path(dump_dir, 'med_cond_summer_stream_ani_1997_2020.gif'),
               fps = 1,
               dpi = 300)

#store faceted mean cond - ncol and nrow must be 1
cond_mean_stream_facet <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggcond_stream, subset = year >=1997)) +
  tm_bubbles('mean_cond_uScm',
             col = 'green',
             title.size = 'mean summer\nconductivity\n(uS/cm)',
             border.col = 'black',
             scale = 3) +
  tm_facets(by = 'year',
            ncol = 1,
            nrow = 1) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')

#export gif
tmap_animation(cond_mean_stream_facet,
               filename = file.path(dump_dir, 'mean_cond_summer_stream_ani_1997_2020.gif'),
               fps = 1,
               dpi = 300)

#store faceted thquan cond - ncol and nrow must be 1
cond_thquan_stream_facet <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggcond_stream, subset = year >=1997)) +
  tm_bubbles('thquan_cond_uScm',
             col = 'green',
             title.size = 'third quantile summer\nconductivity\n(uS/cm)',
             border.col = 'black',
             scale = 3) +
  tm_facets(by = 'year',
            ncol = 1,
            nrow = 1) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')

#export gif
tmap_animation(cond_thquan_stream_facet,
               filename = file.path(dump_dir, 'thquan_cond_summer_stream_ani_1997_2020.gif'),
               fps = 1,
               dpi = 300)


# both together (paneled only) ----


#mean cond stream and lake
paneled_meancond_stream_lake = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'grey') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggcond_stream, subset = year >=1997)) +
    tm_symbols(size = 'mean_cond_uScm',
               col = 'green',
               shape = 21,
               title.size = 'mean summer\nstream\nconductivity\n(uS/cm)',
               border.col = 'black',
               scale = 3) +
    tm_facets(by = 'year',
              ncol = 8) +
  tm_shape(subset(aggcond, subset = year >=1997)) +
    tm_symbols(col = 'mean_cond_uScm',
               size = 0.7,
               shape = 24,
               alpha = 0.7,
               title.col = 'mean summer\nin lake\nconductivity\n(uS/cm)',
               border.col = 'black') +
    tm_facets(by = 'year',
              ncol = 8) +
  
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_meancond_stream_lake

tmap_save(paneled_meancond_stream_lake, filename = file.path(dump_dir, 'mean_cond_summer_streamlake_paneled_1997_2020.png'))


#med cond stream and lake
paneled_medcond_stream_lake = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'grey') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggcond_stream, subset = year >=1997)) +
  tm_symbols(size = 'med_cond_uScm',
             col = 'green',
             shape = 21,
             title.size = 'median summer\nstream\nconductivity\n(uS/cm)',
             border.col = 'black',
             scale = 3) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_shape(subset(aggcond, subset = year >=1997)) +
  tm_symbols(col = 'med_cond_uScm',
             size = 0.7,
             shape = 24,
             alpha = 0.7,
             title.col = 'median summer\nin lake\nconductivity\n(uS/cm)',
             border.col = 'black') +
  tm_facets(by = 'year',
            ncol = 8) +
  
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold')
paneled_medcond_stream_lake

tmap_save(paneled_medcond_stream_lake, filename = file.path(dump_dir, 'med_cond_summer_streamlake_paneled_1997_2020.png'))



# single-panel 10-year average ----
lmp_cond_lake <- left_join(lmp_cond_lake, lmp_locs)
cond <- full_join(lmp_cond_lake, lmp_summer_cond_stream) %>% 
  filter(year > 2010) 
  
unique(cond$month)

cond_summary <- cond %>% 
  group_by(station, site_type, lat_dd, lon_dd) %>% 
  summarize(n = n(),
            mean_cond_uScm = mean(value),
            med_cond_uScm = median(value)) %>% 
  filter(!is.na(lat_dd)) %>% 
cond_summary <- st_as_sf(cond_summary, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')


lt_cond_ave <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(cond_summary) +
  tm_symbols(size = 'mean_cond_uScm',
             col = 'site_type',
             shape = 21,
             title.size = 'average summer\nconductivity\n(uS/cm)',
             border.col = 'black',
             scale = 3,
             shapes.legend.fill = 'white') +
  tm_layout(legend.outside = T,
            legend.title.fontface = 'bold',
            legend.title.size = 1.5,
            legend.text.size = 1,
            title = 'Average Summer\nConductivity\n(Jun-Sept,\n2010-2020)\n ',
            title.fontface = 'bold')
tmap_save(lt_cond_ave, filename = file.path(dump_dir, 'average_longterm_conductivity_2010-2020.png'))

lt_cond_med <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(cond_summary) +
  tm_symbols(size = 'med_cond_uScm',
             col = 'site_type',
             shape = 21,
             title.size = 'median summer\nconductivity\n(uS/cm)',
             border.col = 'black',
             scale = 3,
             shapes.legend.fill = 'white') +
  tm_layout(legend.outside = T,
            legend.title.fontface = 'bold',
            legend.title.size = 1.5,
            legend.text.size = 1,
            title = 'Median Summer\nConductivity\n(Jun-Sept,\n2010-2020)\n ',
            title.fontface = 'bold')
tmap_save(lt_cond_med, filename = file.path(dump_dir, 'median_longterm_conductivity_2010-2020.png'))


# create scatterplot of in-lake deep/shallow and stream input (of those which inlet to lake) over time----
lmp_summer_cond_deep <- lmp_summer_cond %>% 
  filter((station == 200 |
            station == 210 |
            station == 220 |
            station == 230)
         & site_type == 'lake')

lmp_summer_cond_shallow <- lmp_summer_cond %>% 
  filter(station < 100 & site_type == 'lake')
unique(lmp_summer_cond_shallow$station)

lmp_summer_cond_inlet <- lmp_summer_cond_stream %>% 
  filter(station > 250 & station <1000 & 
           site_type == 'stream'&
           status == 'ongoing' &
           last_year == 2020 &
           first_year <= 1994 &
           !is.na(lat_dd) &
           station != 680)#this one is quite a bit upstream
unique(lmp_summer_cond_inlet$station)

## aggregate and join ----
lmp_cond_deep_agg <- lmp_summer_cond_deep %>% 
  group_by(year) %>% 
  summarize(n = n(),
            med_cond_uScm = median(value),
            max_cond_uScm = max(value),
            mean_cond_uScm = mean(value),
            thquan_cond_uScm = quantile(value, 0.75)) %>% 
  mutate(data = 'deep in-lake')

lmp_cond_shallow_agg <- lmp_summer_cond_shallow %>% 
  group_by(year) %>% 
  summarize(n = n(),
            med_cond_uScm = median(value),
            max_cond_uScm = max(value),
            mean_cond_uScm = mean(value),
            thquan_cond_uScm = quantile(value, 0.75)) %>% 
  mutate(data = 'shallow in-lake')

lmp_cond_inlet_agg <- lmp_summer_cond_inlet %>% 
  group_by(year) %>% 
  summarize(n = n(),
            med_cond_uScm = median(value),
            max_cond_uScm = max(value),
            mean_cond_uScm = mean(value),
            thquan_cond_uScm = quantile(value, 0.75)) %>% 
  mutate(data = 'stream inlet')

lmp_cond_aggyear <- full_join(lmp_cond_deep_agg, lmp_cond_shallow_agg) %>% 
  full_join(., lmp_cond_inlet_agg) %>% 
  mutate(data = factor(data, levels = c('deep in-lake', 'shallow in-lake', 'stream inlet')))

## plot mean and median ----
ggplot(lmp_cond_aggyear, aes(x = as.numeric(year), y = mean_cond_uScm)) +
  geom_smooth(color = 'dark grey', se = F, aes(color = data)) +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual conductivity (uS/cm)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_avecond.png'),
       height = 6,
       width = 9,
       dpi = 300,
       units ='in')

ggplot(lmp_cond_aggyear, aes(x = as.numeric(year), y = med_cond_uScm)) +
  geom_smooth(color = 'dark grey', se = F, aes(color = data)) +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual conductivity (uS/cm)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_medcond.png'),
       height = 6,
       width = 9,
       dpi = 300,
       units ='in')

