# code to visualize long term turbidity in lake sunapee

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
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_turb/'


#purple pallette
ppal = c('#efbbff', '#d896ff', '#be29ec', '#800080')
bpal = c('#d2e1e6', '#c1e3ed', '#78aed3', '#4f77aa')

# filter and clean up turbidity for inlake turb ####
#filter for turb
unique(lmp$parameter)

lmp_turb <- lmp %>% 
  filter(parameter == 'turb_NTU') %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(year = format(date, '%Y')) %>% 
  mutate(month = as.numeric(format(date, '%m')))

#filter jun - sept
lmp_summer_turb = lmp_turb %>% 
  filter(month >=6 & month <=9)

#filter for in-lake and longterm sites; epi and integrated only
lmp_turb_lake <- lmp_summer_turb %>% 
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

ggplot(lmp_turb_lake, aes(x = date, y = value)) +
  geom_point() +
  facet_grid(station ~.) +
  theme_bw()

#aggregate to median, max, mean, 3rd quartile value
lmp_agg_lake <- lmp_turb_lake %>% 
  group_by(station, year) %>% 
  summarize(n = n(),
            med_turb_ntu = median(value),
            max_turb_ntu = max(value),
            mean_turb_ntu = mean(value),
            thquan_turb_ntu = quantile(value, 0.75))

# write_csv(lmp_agg_lake, 'c:/Users/steeleb/Desktop/sunapee aggregated turb.csv')

ggplot(lmp_agg_lake, aes(x = year, y = max_turb_ntu, color = station)) +
  geom_point() +
  theme_bw()

ggplot(lmp_agg_lake, aes(x = year, y = mean_turb_ntu, color = station)) +
  geom_point() +
  theme_bw()

ggplot(lmp_agg_lake, aes(x = year, y = med_turb_ntu, color = station)) +
  geom_point() +
  theme_bw()

ggplot(lmp_agg_lake, aes(x = year, y = thquan_turb_ntu, color = station)) +
  geom_point() +
  theme_bw()

## get station list and apply loc info ####
stationlist <- data.frame(unique(lmp_turb_lake$station))
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

#table to sf for med and max turb
aggturb <- st_as_sf(lmp_agg_lake, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')

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

paneled_meanturb = tm_shape(sunapee_shore, bbox = st_bbox(sunapee_shore)) + 
  tm_borders() +
  tm_fill() +
  tm_shape(subset(aggturb, subset = year >=1997)) +
  tm_dots('mean_turb_ntu',
          title = 'average summer\nturbidity\n(NTU)',
          shape = 21,
          palette = bpal,
          border.col = 'black',
          size = 1.5) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color = 'white')
paneled_meanturb
tmap_save(paneled_meanturb, filename = file.path(dump_dir, 'mean_turb_summer_paneled_1997_2020.png'),
          width = 8,
          height = 6,
          units = 'in',
          dpi = 300)

paneled_medturb = tm_shape(sunapee_shore, bbox = st_bbox(sunapee_shore)) + 
  tm_borders() +
  tm_fill ()+
  tm_shape(subset(aggturb, subset = year >=1997)) +
  tm_dots('med_turb_ntu',
          palette = bpal,
          title = 'median summer\nturbidity\n(NTU)',
          shape = 21,
          border.col = 'black',
          size = 1.5) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color = 'white')
paneled_medturb
tmap_save(paneled_medturb, filename = file.path(dump_dir, 'med_turb_summer_paneled_1997_2020.png'),
          width = 8,
          height = 6,
          units = 'in',
          dpi = 300)

# look at turb from streams ####
lmp_summer_turb_stream <- lmp_summer_turb %>% 
  filter(site_type == 'stream') %>% 
  arrange(station)


stream_locs = read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/station_location_details.csv')
stream_locs <- stream_locs %>% 
  filter(site_type == 'stream' &
           status == 'ongoing' &
           last_year == 2020 &
           first_year <= 1994 &
           !is.na(lat_dd))

lmp_summer_turb_stream <- right_join(lmp_summer_turb_stream, stream_locs)

unique(lmp_summer_turb_stream$station)

ggplot(lmp_summer_turb_stream, aes(x = date, y = value)) +
  geom_point() +
  facet_grid(station ~ .)

#drop a few more incomplete streams
lmp_summer_turb_stream <- lmp_summer_turb_stream %>% 
  filter(station != 715 &
           station != 1415)

ggplot(lmp_summer_turb_stream, aes(x = date, y = value)) +
  geom_point() +
  facet_grid(station ~ .)

#aggregate and change units
agg_turb_stream <- lmp_summer_turb_stream %>% 
  group_by(station, year, lat_dd, lon_dd) %>% 
  summarize(n = n(),
            med_turb_ntu = median(value),
            max_turb_ntu = max(value),
            mean_turb_ntu = mean(value),
            thquan_turb_ntu = quantile(value, 0.75)) %>% 
  filter(n > 3) 

## visualize stream turb in paneled plots ----

#table to sf for med and max turb
aggturb_stream <- st_as_sf(agg_turb_stream, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')

# sunapee watershed
sun_ws <- st_read(file.path(gis_dir, 'watersheds/NH_hydro_Sunapee/Lake_Sunapee_watershed.shp'))
sun_ws_wgs <- st_transform(sun_ws, crs = 'EPSG:4326')

bbox_sun_ws <- st_bbox(sun_ws_wgs)

## visualize in paneled plots ####
paneled_meanturb_stream = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'grey') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggturb_stream, subset = year >=1997)) +
  tm_dots('mean_turb_ntu',
          palette = ppal,
          title = 'average summer\nturbidity\n(NTU)',
          border.col = 'black',
          shape = 24,
          size = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color ='white')
paneled_meanturb_stream
tmap_save(paneled_meanturb_stream, filename = file.path(dump_dir, 'mean_turb_summer_stream_paneled_1997_2020.png'),
          width = 9,
          height = 6,
          units = 'in',
          dpi = 300)

paneled_medturb_stream = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'grey') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggturb_stream, subset = year >=1997)) +
  tm_dots('med_turb_ntu',
          palette = ppal,
          title = 'median summer\nturbidity\n(NTU)',
          border.col = 'black',
          shape = 24,
          size = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color ='white')
paneled_medturb_stream

tmap_save(paneled_medturb_stream, filename = file.path(dump_dir, 'med_turb_summer_stream_paneled_1997_2020.png'),
          width = 9,
          height = 6,
          units = 'in',
          dpi = 300)


# single-panel 10-year average ----
lmp_turb_lake <- left_join(lmp_turb_lake, lmp_locs)
turb2010 <- full_join(lmp_turb_lake, lmp_summer_turb_stream) %>% 
  filter(year > 2010) 

unique(turb2010$month)

turb_summary_2010 <- turb2010 %>% 
  group_by(station, site_type, lat_dd, lon_dd) %>% 
  summarize(n = n(),
            mean_turb_ntu = mean(value),
            med_turb_ntu = median(value),
            max_turb_ntu = max(value),
            thquan_turb_ntu = quantile(value, 0.75)) %>% 
  filter(!is.na(lat_dd))

turb_summary_2010_stream = turb_summary_2010 %>% 
  filter(site_type == 'stream')
turb_summary_2010_lake = turb_summary_2010 %>% 
  filter(site_type == 'lake')

turb_summary_2010_stream <- st_as_sf(turb_summary_2010_stream, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')
turb_summary_2010_lake <- st_as_sf(turb_summary_2010_lake, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')


lt_turb_ave <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(turb_summary_2010_stream) +
  tm_symbols(col = 'mean_turb_ntu',
             shape = 21,
             palette = ppal,
             title.col = 'average summer\nturbidity\n(stream)\n(NTU)',
             border.col = 'black') +
  tm_shape(turb_summary_2010_lake) +
  tm_symbols(col = 'mean_turb_ntu',
             shape = 24,
             palette = bpal,
             title.col = 'average summer\nturbidity\n(lake)\n(NTU)',
             border.col = 'black') +
  tm_layout(legend.outside = T,
            legend.title.fontface = 'bold',
            legend.title.size = 1,
            legend.text.size = 1,
            title = 'Average Summer\nTurbidity\n(Jun-Sept,\n2011-2020)\n ',
            title.fontface = 'bold')
lt_turb_ave
tmap_save(lt_turb_ave, 
          filename = file.path(dump_dir, 'average_longterm_turb_2010-2020.png'),
          height = 6, width =5, dpi = 300)

lt_turb_med <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(turb_summary_2010_stream) +
  tm_symbols(col = 'med_turb_ntu',
             shape = 21,
             palette = ppal,
             title.col = 'median summer\nturbidity\n(stream)\n(NTU)',
             border.col = 'black') +
  tm_shape(turb_summary_2010_lake) +
  tm_symbols(col = 'med_turb_ntu',
             shape = 24,
             palette = bpal,
             title.col = 'median summer\nturbidity\n(lake)\n(NTU)',
             border.col = 'black') +
  tm_layout(legend.outside = T,
            legend.title.fontface = 'bold',
            legend.title.size = 1,
            legend.text.size = 1,
            title = 'Median Summer\nTurbidity\n(Jun-Sept,\n2011-2020)\n ',
            title.fontface = 'bold')
lt_turb_med
tmap_save(lt_turb_med, filename = file.path(dump_dir, 'median_longterm_turb_2010-2020.png'),
          height = 6, width =5, dpi = 300)


# create scatterplot of in-lake deep/shallow and stream input (of those which inlet to lake) over time----

lmp_summer_turb_deep <- lmp_summer_turb %>% 
  filter((station == 200 |
            station == 210 |
            station == 220 |
            station == 230)
         & site_type == 'lake')

lmp_summer_turb_shallow <- lmp_summer_turb %>% 
  filter(station < 100 & site_type == 'lake')
unique(lmp_summer_turb_shallow$station)

lmp_summer_turb_inlet <- lmp_summer_turb_stream %>% 
  filter(station > 250 & station <1000 & 
           site_type == 'stream'&
           status == 'ongoing' &
           last_year == 2020 &
           first_year <= 1994 &
           !is.na(lat_dd) &
           station != 680)#this one is quite a bit upstream
unique(lmp_summer_turb_inlet$station)

## aggregate and join ----
lmp_turb_deep_agg <- lmp_summer_turb_deep %>% 
  group_by(year, station) %>% 
  summarize(n = n(),
            med_turb_ntu = median(value),
            max_turb_ntu = max(value),
            mean_turb_ntu = mean(value),
            thquan_turb_ntu = quantile(value, 0.75)) %>% 
  mutate(data = 'deep in-lake')

lmp_turb_shallow_agg <- lmp_summer_turb_shallow %>% 
  group_by(year, station) %>% 
  summarize(n = n(),
            med_turb_ntu = median(value),
            max_turb_ntu = max(value),
            mean_turb_ntu = mean(value),
            thquan_turb_ntu = quantile(value, 0.75)) %>% 
  mutate(data = 'shallow in-lake')

lmp_turb_inlet_agg <- lmp_summer_turb_inlet %>% 
  group_by(year, station) %>% 
  summarize(n = n(),
            med_turb_ntu = median(value),
            max_turb_ntu = max(value),
            mean_turb_ntu = mean(value),
            thquan_turb_ntu = quantile(value, 0.75)) %>% 
  mutate(data = 'stream')

lmp_turb_aggyear <- full_join(lmp_turb_deep_agg, lmp_turb_shallow_agg) %>% 
  full_join(., lmp_turb_inlet_agg) %>% 
  mutate(data = factor(data, levels = c( 'stream', 'shallow in-lake','deep in-lake')))

## plot mean and median ----
ggplot(lmp_turb_aggyear, aes(x = as.numeric(year), y = mean_turb_ntu)) +
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
       y = 'average annual turbidity per site per year (NTU)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_turb.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')

ggplot(lmp_turb_aggyear, aes(x = as.numeric(year), y = mean_turb_ntu)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = data)) +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ ., scales = 'free_y') +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual turbidity per site per year (NTU)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_turb_freey.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')
