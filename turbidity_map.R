#mapped turbidity data

library(sf)
library(tmap)
library(raster)
# library(gifski)
library(tidyverse)
library(ggthemes)
library(gghighlight)

#point to local spatial files folder
gis_dir <- 'C:/Users/steeleb/Dropbox/travel/gis/project/Sunapee/'

# read in LMP record
lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/LSPALMP_1986-2021_v2023-01-22.csv')

#read in station locations
lmp_locs <- read.csv('C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/lmp_shortlist.csv')

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_turb/'

#purple pallette
ppal = c('#efbbff', '#d896ff', '#be29ec', '#800080')
bpal = c('#d2e1e6', '#c1e3ed', '#78aed3', '#4f77aa')

# filter and clean up turbidity for inlake turb ####
#filter for turb
unique(lmp$parameter)

lmp_turb <- lmp %>% 
  filter(parameter == 'turbidity_NTU') %>% 
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

## get station list and apply loc info ####
stationlist <- data.frame(unique(lmp_turb_lake$station))
colnames(stationlist) = 'station'

stationlist <- left_join(stationlist, lmp_locs)

# apply station info to 2 datasets
lmp_agg_lake <- full_join(lmp_agg_lake, stationlist)

# look at turb from streams ####
lmp_summer_turb_stream <- lmp_summer_turb %>% 
  filter(site_type == 'stream') %>% 
  arrange(station)


stream_locs = read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/primary%20files/station_location_details.csv')
stream_locs <- stream_locs %>% 
  filter(site_type == 'stream' &
           status == 'ongoing' &
           last_year == 2022 &
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
startyear = 1995
endyear = 2022

paneled_meanturb = tm_shape(sunapee_shore, bbox = st_bbox(sunapee_shore)) + 
  tm_borders() +
  tm_fill() +
  tm_shape(subset(aggturb, subset = year >=startdate)) +
  tm_dots('mean_turb_ntu',
          title = 'average summer\nturbidity\n(NTU)',
          shape = 21,
          palette = bpal,
          border.col = 'black',
          size = 1.5) +
  tm_facets(by = 'year',
            ncol = 7) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color = 'white')
paneled_meanturb
tmap_save(paneled_meanturb, 
          filename = file.path(dump_dir, 
                               paste0('mean_turb_summer_paneled_',
                                      startyear,
                                      '-',
                                      endyear,
                                      '.png')),
          width = 8,
          height = 6,
          units = 'in',
          dpi = 300)

paneled_medturb = tm_shape(sunapee_shore, bbox = st_bbox(sunapee_shore)) + 
  tm_borders() +
  tm_fill ()+
  tm_shape(subset(aggturb, subset = year >=startyear)) +
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
tmap_save(paneled_medturb, 
          filename = file.path(dump_dir, 
                               paste0('med_turb_summer_paneled_',
                                      startyear,
                                      '-',
                                      endyear,
                                      '.png')),
          width = 8,
          height = 6,
          units = 'in',
          dpi = 300)


turb_summary_10yr_stream <- st_as_sf(turb_summary_10yr_stream, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')
turb_summary_10yr_lake <- st_as_sf(turb_summary_10yr_lake, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')


lt_turb_ave <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(turb_summary_10yr_stream) +
  tm_symbols(col = 'mean_turb_ntu',
             shape = 21,
             palette = ppal,
             title.col = 'average summer\nturbidity\n(stream)\n(NTU)',
             border.col = 'black') +
  tm_shape(turb_summary_10yr_lake) +
  tm_symbols(col = 'mean_turb_ntu',
             shape = 24,
             palette = bpal,
             title.col = 'average summer\nturbidity\n(lake)\n(NTU)',
             border.col = 'black') +
  tm_layout(legend.outside = T,
            legend.title.fontface = 'bold',
            legend.title.size = 1,
            legend.text.size = 1,
            title = paste0('Average Summer\nTurbidity\n(Jun-Sept,\n',
                           startyear,
                           '-',
                           endyear,
                           ')\n '),
            title.fontface = 'bold')
lt_turb_ave
tmap_save(lt_turb_ave, 
          filename = file.path(dump_dir, 
                               paste0('average_longterm_turb_',
                                      startyear,
                                      '-',
                                      endyear,
                                      '.png')),
          height = 6, width =5, dpi = 300)

lt_turb_med <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(turb_summary_10yr_stream) +
  tm_symbols(col = 'med_turb_ntu',
             shape = 21,
             palette = ppal,
             title.col = 'median summer\nturbidity\n(stream)\n(NTU)',
             border.col = 'black') +
  tm_shape(turb_summary_10yr_lake) +
  tm_symbols(col = 'med_turb_ntu',
             shape = 24,
             palette = bpal,
             title.col = 'median summer\nturbidity\n(lake)\n(NTU)',
             border.col = 'black') +
  tm_layout(legend.outside = T,
            legend.title.fontface = 'bold',
            legend.title.size = 1,
            legend.text.size = 1,
            title = paste0('Median Summer\nTurbidity\n(Jun-Sept,\n',
                           startyear,
                           '-',
                           endyear,
                           ')\n '),
            title.fontface = 'bold')
lt_turb_med
tmap_save(lt_turb_med, 
          filename = file.path(dump_dir, 
                               paste0('median_longterm_turb_',
                                      startyear,
                                      '-',
                                      endyear,
                                      '.png')),
          height = 6, width =5, dpi = 300)

