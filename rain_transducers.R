# figure of rain storms and transducers for Sunapee SOTL

library(tidyverse)
library(ggthemes)
library(rnoaa)
library(cowplot)
library(sf)
library(tmap)

#save final theme for ggplot
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom

final_theme_2=theme_bw() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"),
        plot.title=element_text(size=12, face='bold', hjust=0.5)) #save as a grom

figdir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/transducers/'
gisdir = 'C:/Users/steeleb/Dropbox/travel/gis/project/Sunapee/'

## load data, collate ----

### read in transducer data from EDI ----

op_505 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.466.4&entityid=797a8fd504a0ef7b014bf2a096de1fc4')
cj_665 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.466.4&entityid=ae62a65a829d0500392bac93f8f648ce')
bs_788 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.466.4&entityid=4a5998ea80758ae0bba74c662de4f44a')
bn_790 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.466.4&entityid=16b0190ad87138ef9b521bc9891d0d89')
pb_800 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.466.4&entityid=e769fd3aa48e835ae5ef8873f383cd76')
kh_805 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.466.4&entityid=b6d137f0a87fec884c009b9b19203e3e')
hs_830 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.466.4&entityid=09712468e25bd9c59c80507272542850')

# limit to 2018 data
op_505_2018 = op_505 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  filter(datetime >= as.Date('2018-01-01'))

cj_665_2018 = cj_665 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  filter(datetime >= as.Date('2018-01-01'))

bs_788_2018 = bs_788 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  filter(datetime >= as.Date('2018-01-01'))

bn_790_2018 = bn_790 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  filter(datetime >= as.Date('2018-01-01'))

pb_800_2018 = pb_800 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  filter(datetime >= as.Date('2018-01-01'))

kh_805_2018 = kh_805 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  filter(datetime >= as.Date('2018-01-01'))

hs_830_2018 = hs_830 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  filter(datetime >= as.Date('2018-01-01')) %>% 
  filter(!is.na(depth_m))

# join all data
trans_2018 = full_join(op_505_2018, cj_665_2018) %>% 
  full_join(bs_788_2018) %>% 
  full_join(bn_790_2018) %>% 
  full_join(pb_800_2018) %>% 
  full_join(kh_805_2018) %>% 
  full_join(hs_830_2018) %>% 
  mutate(stream_no = factor(stream_no))

### read in precip data from NOAA ----
precip_sun <- ghcnd_search('US1NHSL0008', date_min = '2018-01-01', date_max = '2019-01-01', var = 'prcp')$prcp 

precip_sun <- precip_sun %>% 
  mutate(precip_mm = prcp/10)

### read in temp data from EDI buoy data ----
sun_met_2018 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.234.6&entityid=3cd9f7a61bc570314dc10c0c6bb35fb6')%>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC'))
head(sun_met_2018)

## visualization using ggplot ----

#first quarter of year transducer data
trans <- ggplot(subset(trans_2018, 
              subset = datetime < as.Date('2018-04-01')),
       aes(x = datetime, y = depth_m, color = stream_no)) +
  geom_path(size = 1) +
  labs(x = NULL,
       y = 'stream depth\n(meters)') +
  final_theme +
  scale_color_colorblind() +
  theme(legend.position = 'none')
trans

#first quarter of year precip data
precip <- ggplot(subset(precip_sun, 
              subset = date < as.Date('2018-04-01')), 
       aes(x = date, y = precip_mm)) +
  geom_col(color = '#0009FF', fill = '#0009FF') +
  final_theme +
  labs(x = NULL,
       y = 'total daily\nprecipitation (mm)')
precip

#first quarter of year temperature data
airtemp <- ggplot(subset(sun_met_2018,
              subset = datetime < as.Date('2018-04-01')), 
       aes(x = datetime, y = airTemperature_degC)) +
  geom_abline(slope = 0, intercept = 0,
              lty = 2,
              color = 'grey',
              size = 1) +
  geom_path(size = 1,
            aes(color = airTemperature_degC)) +
  scale_color_gradient(low = '#96FAFD',
                       high = '#FCA597') +
  final_theme +
  theme(legend.position = 'none')+
  labs(x = NULL,
       y = 'air temperature\n(\u00B0C)')
airtemp

legend = get_legend(ggplot(subset(trans_2018, 
                                  subset = datetime < as.Date('2018-04-01')),
                           aes(x = datetime, y = depth_m, color = stream_no)) +
                      geom_path(size = 1) +
                      labs(x = NULL,
                           y = 'stream depth (meters)') +
                      final_theme +
                      scale_color_colorblind(name = 'stream\nnumber'))

legend = plot_grid(legend,NULL,NULL,
                   ncol = 1)

plot = plot_grid(trans, precip, airtemp,
          ncol = 1)

plot_leg = plot_grid(plot, legend,
                     rel_widths = c(0.9, 0.1))

title = ggdraw() + draw_label('Early 2018 Stream Depth and Weather Observations',
                              fontface = 'bold')

plot_grid(title, plot_leg,
          ncol = 1,
          rel_heights = c(0.05, 0.95))

ggsave(file.path(figdir, 'early2018_transducer_precip_temp.png'),
       width = 10,
       height = 7,
       units = 'in',
       dpi = 300)

## visualization using tmap ----

### load sf/st layers ----
trans_locs = st_read(file.path(gisdir, 'study_sites/pressure_transducers/transducer_locs_v2020-01-16b.shp'))
trans_locs_wgs = st_transform(trans_locs, crs = 'EPSG:4326')

#add some x/y mods for mapping with labels
trans_locs_wgs <- trans_locs_wgs %>% 
  mutate(name_abbv = case_when(grepl('505', ident) ~ 'Otter Pond - 505',
                               grepl('665', ident) ~ 'Chandler Johnson - 665',
                               grepl('788', ident) ~ 'Blodget South - 788',
                               grepl('790', ident) ~ 'Blodget North - 790',
                               grepl('805', ident) ~ 'King Hill - 805',
                               grepl('830', ident) ~ 'Herrick South - 830',
                               TRUE ~ NA_character_)) %>% 
  filter(!is.na(name_abbv)) %>% 
  mutate(ymod = case_when(grepl('788', ident) ~ -0.75,
                          grepl('790', ident) ~ 0.75,
                          TRUE ~ 1))

#sunapee shoreline
sunapee_shore = st_read(file.path(gisdir, 'hydrography/LS_shore_WGS.shp'))

#sunpaee streams
sun_stream <- st_read(file.path(gisdir, 'hydrography/streams.shp'))
sun_stream_wgs <- st_transform(sun_stream, crs = 'EPSG:4326')
#sunapee open water
sun_ws_water <- st_read(file.path(gisdir, 'hydrography/waterbodies open water.shp'))
sun_ws_water_wgs <- st_transform(sun_ws_water, crs = 'EPSG:4326')

# sunapee watershed
sun_ws <- st_read(file.path(gisdir, 'watersheds/NH_hydro_Sunapee/Lake_Sunapee_watershed.shp'))
sun_ws_wgs <- st_transform(sun_ws, crs = 'EPSG:4326')

bbox_sun_ws <- st_bbox(sun_ws_wgs)

xrange <- bbox_sun_ws$xmax - bbox_sun_ws$xmin # range of x values
yrange <- bbox_sun_ws$ymax - bbox_sun_ws$ymin # range of y values

#create a new one and modify
bbox_ws_new <- bbox_sun_ws
# bbox_ws_new[1] <- bbox_ws_new[1] - (0.40 * xrange) # xmin - left
bbox_ws_new[3] <- bbox_ws_new[3] + (0.1 * xrange) # xmax - right
# bbox_ws_new[2] <- bbox_ws_new[2] - (0.025 * yrange) # ymin - bottom
# bbox_ws_new[4] <- bbox_ws_new[4] + (0.05 * yrange) # ymax - top

transducer_map <- tm_shape(sun_ws_wgs, bbox = bbox_ws_new) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(sunapee_shore) + tm_polygons() +
  tm_shape(trans_locs_wgs) + 
    tm_symbols(shape = 21,
               col = 'green',
               size = 1) +
    tm_text('name_abbv',
            ymod = 'ymod',
            fontface = 'bold',
            just = 'left')

transducer_map

tmap_save(transducer_map, file.path(figdir, 'sunapee_transducer_locs.png'))


### make map with .png of 1-year of trans data ----
trans_2018_no800 <- trans_2018 %>% 
  filter(stream_no != 800) %>% 
  mutate(stream_nameno = case_when(stream_no == 505 ~ 'Otter Pond\n505',
                                   stream_no == 665 ~ 'Chandler Johnson\n665',
                                   stream_no == 788 ~ 'Blodget South\n788',
                                   stream_no == 790 ~ 'Blodget North\n790',
                                   stream_no == 805 ~ 'King Hill\n805',
                                   stream_no == 830 ~ 'Herrick South\n830',
                                   TRUE ~ '')) %>% 
mutate(stream_nameno = factor(stream_nameno, levels = c('Otter Pond\n505', 'Herrick South\n830', 'King Hill\n805',
                                                    'Blodget North\n790', 'Blodget South\n788', 'Chandler Johnson\n665'))) 
  
trans_2018_facet <- ggplot(trans_2018_no800,
                aes(x = datetime, y = depth_m)) +
  geom_path(size = 1) +
  facet_grid(stream_nameno ~ .) +
  labs(x = NULL,
       y = 'stream depth\n(meters)') +
  scale_x_datetime(limits = c(as.POSIXct('2018-01-01'), as.POSIXct('2019-01-01'))) +
  final_theme_2 
trans_2018_facet

#2018 of year precip data
precip_sun$station = 'Sunapee\nGHCN'
precip_2018 <- ggplot(precip_sun, 
                 aes(x = date, y = precip_mm)) +
  geom_col(color = '#0009FF', fill = '#0009FF') +
  facet_grid(station ~ .) +
  final_theme_2 +
  labs(x = NULL,
       y = 'total daily\nprecipitation (mm)')
precip_2018

# store stream and precip data together
precip_trans_2018 <- plot_grid(precip_2018, trans_2018_facet,
          rel_heights = c(1,5.5),
          ncol = 1)

#convert transducer map to tm_grob
bbox_ws_new[3] <- bbox_ws_new[3] + (0.1 * xrange) # xmax - right
transducer_map <- tm_shape(sun_ws_wgs, bbox = bbox_ws_new) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(sunapee_shore) + tm_polygons() +
  tm_shape(trans_locs_wgs) + 
  tm_symbols(shape = 21,
             col = 'green',
             size = 1) +
  tm_text('name_abbv',
          ymod = 'ymod',
          xmod = 1,
          fontface = 'bold',
          size = 0.75,
          just = 'left')

trans_map = tmap_grob(transducer_map)

map_trans_precip = plot_grid(trans_map, precip_trans_2018,
          rel_widths = c(0.3,0.7))
map_trans_precip

ggsave(file.path(figdir, 'map_precip_transducer_2018.png'), 
       dpi = 300,
       height = 8,
       width = 12,
       units = 'in')

