# code to visualize long term phosphorus in lake sunapee

library(sf)
library(tmap)
library(raster)
# library(gifski)
library(tidyverse)
library(ggthemes)

# read in LMP record
lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/LSPALMP_1986-2020_v2021-03-29.csv')

#read in station locations
lmp_locs <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/station_location_details.csv')

#point to local spatial files folder
gis_dir <- 'C:/Users/steeleb/Dropbox/travel/gis/project/Sunapee/'

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_tp/'


# create a color ramp to better match oligo-meso-eutro-hypereutro scale
tpramp = c('#D0FAFB', '#D0FBF7', '#D0FBF1', '#ADF8D6', '#B4F8D2', '#B4FAAF', '#B6E28D', '#A7CF82', '#9AC373', '#8EBB63','#559E12', '#448B02')
tpval = c(0, 3, 5, 7, 10, 15, 20, 30, 50, 80, 150, 250)

tpramp2 = c('#f6e8c3','#dfc27d','#bf812d', '#8c510a')
tpramp3 = c('#c7eae5','#80cdc1','#35978f','#01665e')

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
  summarize(n = n(),
            med_tp_ugl = median(value)*1000,
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
#sunpaee tribs
sun_trib <- st_read(file.path(gis_dir, 'hydrography/streams.shp'))
sun_trib_wgs <- st_transform(sun_trib, crs = 'EPSG:4326')
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
bbox_sun_new[1] <- bbox_sun_new[1] - (0.1 * xrange) # xmin - left
bbox_sun_new[3] <- bbox_sun_new[3] + (0.1 * xrange) # xmax - right
bbox_sun_new[2] <- bbox_sun_new[2] - (0.05 * yrange) # ymin - bottom
bbox_sun_new[4] <- bbox_sun_new[4] + (0.05 * yrange) # ymax - top

## visualize in paneled plots ####

# paneled_maxtp = tm_shape(sunapee_shore, bbox = st_bbox(sunapee_shore)) + 
#     tm_borders() +
#     tm_fill() +
#   tm_shape(subset(aggtp, subset = year >=1997)) +
#     tm_dots('max_tp_ugl',
#             palette = tpramp,
#             breaks = tpval,
#             title = 'maximum summer\ntotal phosphorus\n(ug/L)',
#             shape = 21,
#             border.col = 'black',
#             size = 2) +
#   tm_facets(by = 'year',
#             ncol = 8) +
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold',
#             panel.label.bg.color = 'white')
# paneled_maxtp
# tmap_save(paneled_maxtp, filename = file.path(dump_dir, 'max_tp_summer_paneled_1997_2020.png'),
#           width = 8,
#           height = 6,
#           units = 'in',
#           dpi = 300)

paneled_meantp = tm_shape(sunapee_shore, bbox = st_bbox(sunapee_shore)) + 
  tm_borders() +
  tm_fill() +
  tm_shape(subset(aggtp, subset = year >=1997)) +
  tm_dots('mean_tp_ugl',
          palette = tpramp3,
          breaks = tpval,
          title = 'average summer\ntotal phosphorus\n(ug/L)',
          shape = 24,
          border.col = 'black',
          size = 1.5) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color = 'white')
paneled_meantp
tmap_save(paneled_meantp, filename = file.path(dump_dir, 'mean_tp_summer_paneled_1997_2020.png'),
          width = 8,
          height = 6,
          units = 'in',
          dpi = 300)

paneled_medtp = tm_shape(sunapee_shore, bbox = st_bbox(sunapee_shore)) + 
  tm_borders() +
  tm_fill ()+
  tm_shape(subset(aggtp, subset = year >=1997)) +
  tm_dots('med_tp_ugl',
          palette = tpramp3,
          breaks = tpval,
          title = 'median summer\ntotal phosphorus\n(ug/L)',
          shape = 24,
          border.col = 'black',
          size = 1.5) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color = 'white')
paneled_medtp
tmap_save(paneled_medtp, filename = file.path(dump_dir, 'med_tp_summer_paneled_1997_2020.png'),
          width = 8,
          height = 6,
          units = 'in',
          dpi = 300)

# paneled_thquantp = tm_shape(sunapee_shore, bbox = st_bbox(sunapee_shore)) + 
#   tm_borders() +
#   tm_fill()+
#   tm_shape(subset(aggtp, subset = year >=1997)) +
#   tm_dots('thquan_tp_ugl',
#           palette = tpramp,
#           breaks = tpval,
#           title = 'third quantile summer\ntotal phosphorus\n(ug/L)',
#           shape = 21,
#           border.col = 'black',
#           size = 2) +
#   tm_facets(by = 'year',
#             ncol = 8) +
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold',
#             panel.label.bg.color = 'white')
# paneled_thquantp
# tmap_save(paneled_thquantp, 
#           filename = file.path(dump_dir, 'thquan_tp_summer_paneled_1997_2020.png'),
#           width = 8,
#           height = 6,
#           units = 'in',
#           dpi = 300)


## visualize in animated plot ####

# #store faceted max tp - ncol and nrow must be 1
# tp_max_facet <- tm_shape(sun_trib_wgs, bbox = bbox_sun_new) + 
#     tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + 
#     tm_polygons(border.col = 'blue', col = '#D9F9FA') +
#   tm_shape(sun_bathy)+
#     tm_raster(palette = 'Blues',
#               title = 'lake depth\n(meters)',
#               contrast = c(0, 0.5)) +
#   tm_shape(sunapee_shore) + 
#     tm_borders() +
#   tm_shape(subset(aggtp, subset = year >=1997)) +
#     tm_bubbles('max_tp_ugl',
#                col = 'green',
#                title.size = 'maximum summer\ntotal phosphorus\n(mg/L)',
#                border.col = 'black',
#                scale = 3) +
#   tm_facets(by = 'year',
#             ncol = 1,
#             nrow = 1) +
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold')
# 
# #export gif
# tmap_animation(tp_max_facet,
#                filename = file.path(dump_dir, 'max_tp_summer_ani_1997_2020_v3.gif'),
#                fps = 1,
#                dpi = 300)
# 
# 
# #store faceted med tp - ncol and nrow must be 1
# tp_med_facet <- tm_shape(sun_trib_wgs, bbox = bbox_sun_new) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons(border.col = 'blue', col = '#D9F9FA') +
#   tm_shape(sun_bathy)+
#   tm_raster(palette = 'Blues',
#             title = 'lake depth\n(meters)',
#             contrast = c(0, 0.5)) +
#   tm_shape(sunapee_shore) + tm_borders() +
#   tm_shape(subset(aggtp, subset = year >=1997)) +
#   tm_bubbles('med_tp_ugl',
#              col = 'green',
#              title.size = 'median summer\ntotal phosphorus\n(mg/L)',
#              border.col = 'black',
#              scale = 3) +
#   tm_facets(by = 'year',
#             ncol = 1,
#             nrow = 1)+
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold')
# 
# 
# #export gif
# tmap_animation(tp_med_facet,
#                filename = file.path(dump_dir, 'med_tp_summer_ani_1997_2020_v3.gif'),
#                fps = 1,
#                dpi = 300)
# 
# 
# 
# #store faceted mean tp - ncol and nrow must be 1
# tp_mean_facet <- tm_shape(sun_trib_wgs, bbox = bbox_sun_new) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons(border.col = 'blue', col = '#D9F9FA') +
#   tm_shape(sun_bathy)+
#   tm_raster(palette = 'Blues',
#             title = 'lake depth\n(meters)',
#             contrast = c(0, 0.5)) +
#   tm_shape(sunapee_shore) + tm_borders() +
#   tm_shape(subset(aggtp, subset = year >=1997)) +
#   tm_bubbles('mean_tp_ugl',
#              col = 'green',
#              title.size = 'mean summer\ntotal phosphorus\n(mg/L)',
#              border.col = 'black',
#              scale = 3) +
#   tm_facets(by = 'year',
#             ncol = 1,
#             nrow = 1)+
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold')
# 
# 
# #export gif
# tmap_animation(tp_mean_facet,
#                filename = file.path(dump_dir, 'mean_tp_summer_ani_1997_2020_v3.gif'),
#                fps = 1,
#                dpi = 300)
# 
# 
# #store faceted thquan tp - ncol and nrow must be 1
# tp_thquan_facet <- tm_shape(sun_trib_wgs, bbox = bbox_sun_new) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons(border.col = 'blue', col = '#D9F9FA') +
#   tm_shape(sun_bathy)+
#   tm_raster(palette = 'Blues',
#             title = 'lake depth\n(meters)',
#             contrast = c(0, 0.5)) +
#   tm_shape(sunapee_shore) + tm_borders() +
#   tm_shape(subset(aggtp, subset = year >=1997)) +
#   tm_bubbles('thquan_tp_ugl',
#              col = 'green',
#              title.size = 'third quantile summer\ntotal phosphorus\n(mg/L)',
#              border.col = 'black',
#              scale = 3) +
#   tm_facets(by = 'year',
#             ncol = 1,
#             nrow = 1)+
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold')
# 
# 
# #export gif
# tmap_animation(tp_thquan_facet,
#                filename = file.path(dump_dir, 'thquan_tp_summer_ani_1997_2020_v3.gif'),
#                fps = 1,
#                dpi = 300)


# look at TP from tribs ####
lmp_summer_tp_trib <- lmp_summer_tp %>% 
  filter(site_type == 'stream') %>% 
  arrange(station) %>% 
  mutate(site_type = 'tributary')


trib_locs = read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/station_location_details.csv')
trib_locs <- trib_locs %>% 
  filter(site_type == 'stream' &
           status == 'ongoing' &
           last_year == 2020 &
           first_year <= 1994 &
           !is.na(lat_dd)) %>% 
  mutate(site_type = 'tributary')

lmp_summer_tp_trib <- right_join(lmp_summer_tp_trib, trib_locs)

unique(lmp_summer_tp_trib$station)

ggplot(lmp_summer_tp_trib, aes(x = date, y = value)) +
  geom_point() +
  facet_grid(station ~ .)

#drop a few more incomplete tribs
lmp_summer_tp_trib <- lmp_summer_tp_trib %>% 
  filter(station != 715 &
           station != 1415)

ggplot(lmp_summer_tp_trib, aes(x = date, y = value)) +
  geom_point() +
  facet_grid(station ~ .)

#aggregate and change units
agg_tp_trib <- lmp_summer_tp_trib %>% 
  group_by(station, year, lat_dd, lon_dd) %>% 
  summarize(n = n(),
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value)*1000,
            mean_tp_ugl = mean(value)*1000,
            thquan_tp_ugl = quantile(value, 0.75)*1000) %>% 
  filter(n > 3) 

## visualize trib TP in paneled plots ----

#table to sf for med and max tp
aggtp_trib <- st_as_sf(agg_tp_trib, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')

# sunapee watershed
sun_ws <- st_read(file.path(gis_dir, 'watersheds/NH_hydro_Sunapee/Lake_Sunapee_watershed.shp'))
sun_ws_wgs <- st_transform(sun_ws, crs = 'EPSG:4326')

bbox_sun_ws <- st_bbox(sun_ws_wgs)

## visualize in paneled plots ####
# paneled_maxtp_trib = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + 
#     tm_polygons() +
#   tm_shape(sun_ws_wgs) + 
#     tm_borders() +
#   tm_shape(sun_trib_wgs) + 
#     tm_lines(col = 'grey') +
#   tm_shape(sun_ws_water_wgs) + 
#     tm_polygons() +
#   tm_shape(subset(aggtp_trib, subset = year >=1997)) +
#     tm_dots('max_tp_ugl',
#                palette = tpramp2,
#                title.size = 'maximum summer\ntotal phosphorus\n(ug/L)',
#                border.col = 'black',
#               shape = 21,
#                size = 2) +
#   tm_facets(by = 'year',
#             ncol = 8) +
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold',
#             panel.label.bg.color ='white')
# paneled_maxtp_trib
# 
# tmap_save(paneled_maxtp_trib, 
#           filename = file.path(dump_dir, 'max_tp_summer_trib_paneled_1997_2020.png'),
#           width = 9,
#           height = 6,
#           units = 'in',
#           dpi = 300)

paneled_meantp_trib = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_trib_wgs) + tm_lines(col = 'grey') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggtp_trib, subset = year >=1997)) +
      tm_dots('max_tp_ugl',
                 palette = tpramp2,
                 title.size = 'maximum summer\ntotal phosphorus\n(ug/L)',
                 border.col = 'black',
                shape = 21,
              size = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color ='white')
paneled_meantp_trib

tmap_save(paneled_meantp_trib, filename = file.path(dump_dir, 'mean_tp_summer_trib_paneled_1997_2020.png'),
          width = 9,
          height = 6,
          units = 'in',
          dpi = 300)

paneled_medtp_trib = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_trib_wgs) + tm_lines(col = 'grey') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggtp_trib, subset = year >=1997)) +
  tm_dots('max_tp_ugl',
          palette = tpramp2,
          title.size = 'maximum summer\ntotal phosphorus\n(ug/L)',
          border.col = 'black',
          shape = 21,
          size = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color ='white')
paneled_medtp_trib

tmap_save(paneled_medtp_trib, filename = file.path(dump_dir, 'med_tp_summer_trib_paneled_1997_2020.png'),
          width = 9,
          height = 6,
          units = 'in',
          dpi = 300)

# paneled_thquantp_trib = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
#   tm_shape(sun_ws_wgs) + tm_borders() +
#   tm_shape(sun_trib_wgs) + tm_lines(col = 'grey') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons() +
#   tm_shape(subset(aggtp_trib, subset = year >=1997)) +
#   tm_bubbles('thquan_tp_ugl',
#              col = 'green',
#              title.size = 'third quantile summer\ntotal phosphorus\n(ug/L)',
#              border.col = 'black',
#              scale = 3) +
#   tm_facets(by = 'year',
#             ncol = 8) +
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold',
#             panel.label.bg.color ='white')
# paneled_thquantp_trib
# 
# tmap_save(paneled_thquantp_trib, filename = file.path(dump_dir, 'thquan_tp_summer_trib_paneled_1997_2020.png'),
#           width = 9,
#           height = 6,
#           units = 'in',
#           dpi = 300)

## vis in animated plots ----

# #store faceted max tp - ncol and nrow must be 1
# tp_max_trib_facet <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
#   tm_shape(sun_ws_wgs) + tm_borders() +
#   tm_shape(sun_trib_wgs) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons() +
#   tm_shape(subset(aggtp_trib, subset = year >=1997)) +
#   tm_bubbles('max_tp_ugl',
#              col = 'green',
#              title.size = 'maximum summer\ntotal phosphorus\n(ug/L)',
#              border.col = 'black',
#              scale = 3) +
#   tm_facets(by = 'year',
#             ncol = 1,
#             nrow = 1) +
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold')
# 
# #export gif
# tmap_animation(tp_max_trib_facet,
#                filename = file.path(dump_dir, 'max_tp_summer_trib_ani_1997_2020.gif'),
#                fps = 1,
#                dpi = 300)
# 
# #store faceted med tp - ncol and nrow must be 1
# tp_med_trib_facet <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
#   tm_shape(sun_ws_wgs) + tm_borders() +
#   tm_shape(sun_trib_wgs) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons() +
#   tm_shape(subset(aggtp_trib, subset = year >=1997)) +
#   tm_bubbles('med_tp_ugl',
#              col = 'green',
#              title.size = 'median summer\ntotal phosphorus\n(ug/L)',
#              border.col = 'black',
#              scale = 3) +
#   tm_facets(by = 'year',
#             ncol = 1,
#             nrow = 1) +
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold')
# 
# #export gif
# tmap_animation(tp_med_trib_facet,
#                filename = file.path(dump_dir, 'med_tp_summer_trib_ani_1997_2020.gif'),
#                fps = 1,
#                dpi = 300)
# 
# #store faceted mean tp - ncol and nrow must be 1
# tp_mean_trib_facet <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
#   tm_shape(sun_ws_wgs) + tm_borders() +
#   tm_shape(sun_trib_wgs) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons() +
#   tm_shape(subset(aggtp_trib, subset = year >=1997)) +
#   tm_bubbles('mean_tp_ugl',
#              col = 'green',
#              title.size = 'mean summer\ntotal phosphorus\n(ug/L)',
#              border.col = 'black',
#              scale = 3) +
#   tm_facets(by = 'year',
#             ncol = 1,
#             nrow = 1) +
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold')
# 
# #export gif
# tmap_animation(tp_mean_trib_facet,
#                filename = file.path(dump_dir, 'mean_tp_summer_trib_ani_1997_2020.gif'),
#                fps = 1,
#                dpi = 300)
# 
# #store faceted thquan tp - ncol and nrow must be 1
# tp_thquan_trib_facet <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
#   tm_shape(sun_ws_wgs) + tm_borders() +
#   tm_shape(sun_trib_wgs) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons() +
#   tm_shape(subset(aggtp_trib, subset = year >=1997)) +
#   tm_bubbles('thquan_tp_ugl',
#              col = 'green',
#              title.size = 'third quantile summer\ntotal phosphorus\n(ug/L)',
#              border.col = 'black',
#              scale = 3) +
#   tm_facets(by = 'year',
#             ncol = 1,
#             nrow = 1) +
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold')
# 
# #export gif
# tmap_animation(tp_thquan_trib_facet,
#                filename = file.path(dump_dir, 'thquan_tp_summer_trib_ani_1997_2020.gif'),
#                fps = 1,
#                dpi = 300)



# single-panel 10-year average ----
lmp_tp_lake <- left_join(lmp_tp_lake, lmp_locs)
tp2010 <- full_join(lmp_tp_lake, lmp_summer_tp_trib) %>% 
  filter(year > 2010) 

unique(tp2010$month)

tp_summary_2010 <- tp2010 %>% 
  group_by(station, site_type, lat_dd, lon_dd) %>% 
  summarize(n = n(),
            mean_tp_ugl = mean(value)*1000,
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value*1000),
            thquan_tp_ugl = quantile(value, 0.75)*1000) %>% 
  filter(!is.na(lat_dd))

tp_summary_2010_trib = tp_summary_2010 %>% 
  filter(site_type == 'tributary')
tp_summary_2010_lake = tp_summary_2010 %>% 
  filter(site_type == 'lake')

tp_summary_2010_trib <- st_as_sf(tp_summary_2010_trib, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')
tp_summary_2010_lake <- st_as_sf(tp_summary_2010_lake, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')


lt_tp_ave <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_trib_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(tp_summary_2010_trib) +
  tm_symbols(col = 'mean_tp_ugl',
             shape = 21,
             palette = tpramp2,
             title.col = 'average summer\ntotal phosphorus\n(trib)\n(ug/L)',
             border.col = 'black') +
  tm_shape(tp_summary_2010_lake) +
  tm_symbols(col = 'mean_tp_ugl',
             shape = 24,
             palette = tpramp3,
             title.col = 'average summer\ntotal phosphorus\n(lake)\n(ug/L)',
             border.col = 'black') +
  tm_layout(legend.outside = T,
            legend.title.fontface = 'bold',
            legend.title.size = 1,
            legend.text.size = 1,
            title = 'Average Summer\ntotal phosphorus\n(Jun-Sept,\n2011-2020)\n ',
            title.fontface = 'bold')
lt_tp_ave
tmap_save(lt_tp_ave, 
          filename = file.path(dump_dir, 'average_longterm_tp_2010-2020.png'),
          height = 6, width =5, dpi = 300)

lt_tp_med <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_trib_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(tp_summary_2010_trib) +
  tm_symbols(col = 'med_tp_ugl',
             shape = 21,
             palette = tpramp2,
             title.col = 'median summer\ntotal phosphorus\n(trib)\n(ug/L)',
             border.col = 'black') +
  tm_shape(tp_summary_2010_lake) +
  tm_symbols(col = 'med_tp_ugl',
             shape = 24,
             palette = tpramp3,
             title.col = 'median summer\ntotal phosphorus\n(lake)\n(ug/L)',
             border.col = 'black') +
  tm_layout(legend.outside = T,
            legend.title.fontface = 'bold',
            legend.title.size = 1,
            legend.text.size = 1,
            title = 'Median Summer\ntotal phosphorus\n(Jun-Sept,\n2011-2020)\n ',
            title.fontface = 'bold')
lt_tp_med
tmap_save(lt_tp_med, filename = file.path(dump_dir, 'median_longterm_tp_2010-2020.png'),
          height = 6, width =5, dpi = 300)

# lt_tp_max <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
#   tm_shape(sun_ws_wgs) + tm_borders() +
#   tm_shape(sun_trib_wgs) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons() +
#   tm_shape(tp_summary_2010) +
#   tm_symbols(size = 'max_tp_ugl',
#              col = 'site_type',
#              shape = 21,
#              title.size = 'maximum summer\ntotal phosphorus\n(ug/L)',
#              border.col = 'black',
#              scale = 3,
#              shapes.legend.fill = 'white') +
#   tm_layout(legend.outside = T,
#             legend.title.fontface = 'bold',
#             legend.title.size = 1.5,
#             legend.text.size = 1,
#             title = 'Maximum Summer\ntotal phosphorus\n(Jun-Sept,\n2010-2020)\n ',
#             title.fontface = 'bold')
# lt_tp_max
# tmap_save(lt_tp_max, filename = file.path(dump_dir, 'max_longterm_tp_2010-2020.png'))
# 
# lt_tp_thquan <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
#   tm_shape(sun_ws_wgs) + tm_borders() +
#   tm_shape(sun_trib_wgs) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons() +
#   tm_shape(tp_summary_2010) +
#   tm_symbols(size = 'thquan_tp_ugl',
#              col = 'site_type',
#              shape = 21,
#              title.size = 'third quantile summer\ntotal phosphorus\n(ug/L)',
#              border.col = 'black',
#              scale = 3,
#              shapes.legend.fill = 'white') +
#   tm_layout(legend.outside = T,
#             legend.title.fontface = 'bold',
#             legend.title.size = 1.5,
#             legend.text.size = 1,
#             title = 'Third Quantile Summer\ntotal phosphorus\n(Jun-Sept,\n2010-2020)\n ',
#             title.fontface = 'bold')
# lt_tp_thquan
# tmap_save(lt_tp_thquan, filename = file.path(dump_dir, 'thquan_longterm_tp_2010-2020.png'))


# both together (paneled only) ----
tp_summer_ls <-left_join(lmp_tp_lake, stationlist) %>% 
  full_join(.,  lmp_summer_tp_trib) %>% 
  filter(year >= 1997)

unique(tp_summer_ls$month)
unique(tp_summer_ls$site_type)

tp_summary_ls <- tp_summer_ls %>% 
  group_by(station, year, site_type, lat_dd, lon_dd) %>% 
  summarize(n = n(),
            mean_tp_ugl = mean(value)*1000,
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value*1000),
            thquan_tp_ugl = quantile(value, 0.75)*1000) %>% 
  filter(!is.na(lat_dd))

tp_summary_trib = tp_summary_ls %>% 
  filter(site_type == 'tributary')

tp_summary_lake = tp_summary_ls %>% 
  filter(site_type == 'lake')

tp_summary_trib <- st_as_sf(tp_summary_trib, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')
tp_summary_lake <- st_as_sf(tp_summary_lake, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')

#average tp
lt_tp_ave_panel <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_trib_wgs) + tm_lines(col = 'grey') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(tp_summary_trib) +
  tm_symbols(col = 'mean_tp_ugl',
             pal = tpramp2,
             shape = 21,
             title.col = 'average summer\ntotal phosphorus (trib)\n(ug/L)',
             border.col = 'black') +
  tm_shape(tp_summary_lake) +
  tm_symbols(col = 'mean_tp_ugl',
             pal = tpramp3,
             shape = 24,
             title.col = 'average summer\ntotal phosphorus (lake)\n(ug/L)',
             border.col = 'black') +
  tm_layout(legend.outside = T,
            legend.title.fontface = 'bold',
            # legend.title.size = 1.5,
            # legend.text.size = 1,
            title = 'Average Summer\ntotal phosphorus\n(Jun-Sept,\n1997-2020)\n ',
            title.fontface = 'bold')+
  tm_facets(by = 'year',
            ncol =8)+
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color = 'white')
lt_tp_ave_panel

tmap_save(lt_tp_ave_panel,
          filename = file.path(dump_dir, 'paneled_ave_tp_laketrib.png'),
          width = 8,
          height = 6,
          units = 'in',
          dpi = 300)


#median tp
lt_tp_med_panel <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_trib_wgs) + tm_lines(col = 'grey') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(tp_summary_trib) +
  tm_symbols(col = 'med_tp_ugl',
             pal = tpramp2,
             shape = 21,
             title.col = 'median summer\ntotal phosphorus (trib)\n(ug/L)',
             border.col = 'black') +
  tm_shape(tp_summary_lake) +
  tm_symbols(col = 'med_tp_ugl',
             pal = tpramp3,
             shape = 24,
             title.col = 'median summer\ntotal phosphorus (lake)\n(ug/L)',
             border.col = 'black') +  tm_layout(legend.outside = T,
            legend.title.fontface = 'bold',
            # legend.title.size = 1.5,
            # legend.text.size = 1,
            title = 'Median Summer\ntotal phosphorus\n(Jun-Sept,\n1997-2020)\n ',
            title.fontface = 'bold')+
  tm_facets(by = 'year',
            ncol =8)+
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color = 'white')
lt_tp_med_panel

tmap_save(lt_tp_med_panel, filename = file.path(dump_dir, 'paneled_med_tp_laketrib.png'),
          width = 8,
          height = 6,
          units = 'in',
          dpi = 300)

# create scatterplot of in-lake deep/shallow and trib input (of those which inlet to lake) over time----

lmp_summer_tp_deep <- lmp_summer_tp %>% 
  filter((station == 200 |
            station == 210 |
            station == 220 |
            station == 230)
         & site_type == 'lake')

lmp_summer_tp_shallow <- lmp_summer_tp %>% 
  filter(station < 100 & site_type == 'lake')
unique(lmp_summer_tp_shallow$station)

lmp_summer_tp_inlet <- lmp_summer_tp_trib %>% 
  filter(station > 250 & station <1000 & 
           site_type == 'tributary'&
           status == 'ongoing' &
           last_year == 2020 &
           first_year <= 1994 &
           !is.na(lat_dd) &
           station != 680)#this one is quite a bit upstream
unique(lmp_summer_tp_inlet$station)

## aggregate and join ----
lmp_tp_deep_agg <- lmp_summer_tp_deep %>% 
  group_by(year, station) %>% 
  summarize(n = n(),
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value)*1000,
            mean_tp_ugl = mean(value)*1000,
            thquan_tp_ugl = quantile(value, 0.75)*1000) %>% 
  mutate(data = 'deep in-lake')

lmp_tp_shallow_agg <- lmp_summer_tp_shallow %>% 
  group_by(year, station) %>% 
  summarize(n = n(),
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value)*1000,
            mean_tp_ugl = mean(value)*1000,
            thquan_tp_ugl = quantile(value, 0.75)*1000) %>% 
  mutate(data = 'shallow in-lake')

lmp_tp_inlet_agg <- lmp_summer_tp_inlet %>% 
  group_by(year, station) %>% 
  summarize(n = n(),
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value)*1000,
            mean_tp_ugl = mean(value)*1000,
            thquan_tp_ugl = quantile(value, 0.75)*1000) %>% 
  mutate(data = 'tributary')

lmp_tp_aggyear <- full_join(lmp_tp_deep_agg, lmp_tp_shallow_agg) %>% 
  full_join(., lmp_tp_inlet_agg) %>% 
  mutate(data = factor(data, levels = c( 'tributary', 'shallow in-lake','deep in-lake')))


## aggregate and join by year----
lmp_tp_deep_agg_yr <- lmp_summer_tp_deep %>% 
  group_by(year) %>% 
  summarize(n = n(),
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value)*1000,
            mean_tp_ugl = mean(value)*1000,
            thquan_tp_ugl = quantile(value, 0.75)*1000) %>% 
  mutate(data = 'deep in-lake')

lmp_tp_shallow_agg_yr <- lmp_summer_tp_shallow %>% 
  group_by(year) %>% 
  summarize(n = n(),
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value)*1000,
            mean_tp_ugl = mean(value)*1000,
            thquan_tp_ugl = quantile(value, 0.75)*1000) %>% 
  mutate(data = 'shallow in-lake')

lmp_tp_inlet_agg_yr <- lmp_summer_tp_inlet %>% 
  group_by(year) %>% 
  summarize(n = n(),
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value)*1000,
            mean_tp_ugl = mean(value)*1000,
            thquan_tp_ugl = quantile(value, 0.75)*1000) %>% 
  mutate(data = 'tributary')

lmp_tp_aggyear_yr <- full_join(lmp_tp_deep_agg_yr, lmp_tp_shallow_agg_yr) %>% 
  full_join(., lmp_tp_inlet_agg_yr) %>% 
  mutate(data = factor(data, levels = c( 'tributary', 'shallow in-lake','deep in-lake')))

## plot mean and median ----
ggplot(lmp_tp_aggyear, aes(x = as.numeric(year), y = mean_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = data)) +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_aveTPpersite.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')
ggplot(lmp_tp_aggyear, aes(x = as.numeric(year), y = mean_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_aveTPpersite_noloess.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')

ggplot(lmp_tp_aggyear_yr, aes(x = as.numeric(year), y = mean_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = data)) +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_aveTPperyear.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')
ggplot(lmp_tp_aggyear_yr, aes(x = as.numeric(year), y = mean_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_aveTPperyear_noloess.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')

ggplot(lmp_tp_aggyear, aes(x = as.numeric(year), y = med_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = data)) +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_medTPpersite.png'),
       height = 6,
       width = 9,
       dpi = 300,
       units ='in')
ggplot(lmp_tp_aggyear, aes(x = as.numeric(year), y = med_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_medTPpersite_noloess.png'),
       height = 6,
       width = 9,
       dpi = 300,
       units ='in')

ggplot(lmp_tp_aggyear_yr, aes(x = as.numeric(year), y = med_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = data)) +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_medTPperyear.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')
ggplot(lmp_tp_aggyear_yr, aes(x = as.numeric(year), y = med_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_medTPperyear_noloess.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')
