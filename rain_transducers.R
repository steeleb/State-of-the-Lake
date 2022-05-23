# figure of rain storms and transducers for Sunapee SOTL

library(zoo)
library(tidyverse)
library(ggthemes)
library(rnoaa)
library(cowplot)
library(sf)
library(tmap)
library(gganimate)

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
tempdir = 'C:/Users/steeleb/Documents/GitHub/Ewing-Stream-Transducers/L1_files/script4/'

## load data, collate ----

### read in transducer data from EDI through 2018 ----

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

### read in data stored locally 2018-2021 ----
op_505_temp <- read.csv(file.path(tempdir, '505_OP_L1_2018-12_2021-11_temp.csv'))
cj_665_temp <- read.csv(file.path(tempdir, '665_CJ_L1_2018-11_2021-11_temp.csv'))
bs_788_temp <- read.csv(file.path(tempdir, '788_BS_L1_2018-11_2021-11_temp.csv'))
bn_790_temp <- read.csv(file.path(tempdir, '790_BN_L1_2018-11_2021-11_temp.csv'))
pb_800_temp <- read.csv(file.path(tempdir, '800_PB_L1_2018-11_2021-11_temp.csv'))
kh_805_temp <- read.csv(file.path(tempdir, '805_KH_L1_2018-11_2021-11_temp.csv'))
hs_830_temp <- read.csv(file.path(tempdir, '830_HS_L1_2018-11_2021-11_temp.csv'))

# join all data
trans_temp = full_join(op_505_temp, cj_665_temp) %>%
  full_join(bs_788_temp) %>%
  full_join(bn_790_temp) %>%
  full_join(pb_800_temp) %>%
  full_join(kh_805_temp) %>%
  full_join(hs_830_temp) %>%
  mutate(stream_no = factor(stream_no),
         datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  filter(!is.na(depth_m)) %>% 
  select(datetime, stream_no, temp_C, depth_m, flag_refpres, flag_depth)
head(trans_temp)

#look at flags
unique(trans_temp$flag_depth)

# recode rn and s to na
trans_temp <- trans_temp %>% 
  mutate(depth_m = case_when(flag_depth == 's' | flag_depth == 'rn' ~ NA_real_,
                             TRUE ~ depth_m))

### read in precip data from NOAA ----
precip_sun <- ghcnd_search('US1NHSL0008', date_min = '2018-01-01', date_max = '2022-01-01', var = 'prcp')$prcp 

precip_sun <- precip_sun %>% 
  mutate(precip_mm = prcp/10)

#attribute to noon
precip_sun <- precip_sun %>% 
  mutate(datetime = as.POSIXct(paste(date, '12:00', sep = ' '), tz = 'UTC'))

### read in temp data from EDI buoy data ----
sun_met_2018 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.234.6&entityid=3cd9f7a61bc570314dc10c0c6bb35fb6')%>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC'))
head(sun_met_2018)

sun_met_2020 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.234.6&entityid=39ce71340197c34b8bad295fe83c85bf')%>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC'))
head(sun_met_2020)

## visualization of early 2018 using ggplot ----

#first quarter of 2018 transducer data
trans2018 <- ggplot(subset(trans_2018, 
              subset = datetime < as.Date('2018-04-01')),
       aes(x = datetime, y = depth_m, color = stream_no)) +
  geom_path(size = 1) +
  labs(x = NULL,
       y = 'stream depth\n(meters)') +
  final_theme +
  scale_color_colorblind() +
  theme(legend.position = 'none')
trans2018

#first quarter of year precip data
precip2018 <- ggplot(subset(precip_sun, 
              subset = date < as.Date('2018-04-01')), 
       aes(x = date, y = precip_mm)) +
  geom_col(color = '#0009FF', fill = '#0009FF') +
  final_theme +
  labs(x = NULL,
       y = 'total daily\nprecipitation (mm)')
precip2018

#first quarter of year temperature data
airtemp2018 <- ggplot(subset(sun_met_2018,
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
airtemp2018

legend = get_legend(ggplot(subset(trans_2018, 
                                  subset = datetime < as.Date('2018-04-01')),
                           aes(x = datetime, y = depth_m, color = stream_no)) +
                      geom_path(size = 1) +
                      labs(x = NULL,
                           y = 'stream depth (meters)') +
                      final_theme +
                      scale_color_colorblind(name = 'stream\nnumber'))

legend2018 = plot_grid(legend,NULL,NULL,
                   ncol = 1)

plot2018 = plot_grid(trans2018, precip2018, airtemp2018,
          ncol = 1)

plot_leg2018 = plot_grid(plot2018, legend2018,
                     rel_widths = c(0.9, 0.1))

title = ggdraw() + draw_label('Early 2018 Stream Depth and Weather Observations',
                              fontface = 'bold')

plot_grid(title, plot_leg2018,
          ncol = 1,
          rel_heights = c(0.05, 0.95))

ggsave(file.path(figdir, 'early2018_transducer_precip_temp.png'),
       width = 10,
       height = 7,
       units = 'in',
       dpi = 300)

## visualization of early 2020 using ggplot ----

#third quarter of 2020 transducer data
trans_data_smooth <- trans_temp %>% 
  group_by(stream_no) %>% 
  arrange(datetime) %>% 
  mutate(smooth_depth_m = rollmean(depth_m, k = 12, fill = NA))
trans2020 <- ggplot(subset(trans_data_smooth, 
                           subset =  datetime >= as.Date('2020-06-26') & datetime < as.Date('2020-07-08')),
                    aes(x = datetime, y = smooth_depth_m, color = stream_no)) +
  geom_path(size = 1) +
  labs(x = NULL,
       y = 'stream depth\n(meters)') +
  final_theme +
  scale_color_colorblind() +
  theme(legend.position = 'none')
trans2020

#third quarter of year precip data
precip2020 <- ggplot(subset(precip_sun, 
                            subset = datetime < as.Date('2020-07-08')&
                              datetime >= as.Date('2020-06-26')), 
                     aes(x = datetime, y = precip_mm)) +
  geom_col(color = '#0009FF', fill = '#0009FF') +
  final_theme +
  labs(x = NULL,
       y = 'total daily\nprecipitation (mm)')
precip2020

temp2020 <- ggplot(subset(trans_data_smooth, 
                          subset =  datetime >= as.Date('2020-06-26') & datetime < as.Date('2020-07-08')),
                   aes(x = datetime, y = temp_C, color = stream_no)) +
  geom_path(size = 1) +
  labs(x = NULL,
       y = 'stream temperature\n(\u00B0C)') +
  final_theme +
  scale_color_colorblind() +
  theme(legend.position = 'none')
temp2020

legend = get_legend(ggplot(subset(trans_temp, 
                                  subset = datetime < as.Date('2020-04-01')),
                           aes(x = datetime, y = depth_m, color = stream_no)) +
                      geom_path(size = 1) +
                      labs(x = NULL,
                           y = 'stream depth (meters)') +
                      final_theme +
                      scale_color_colorblind(name = 'stream\nnumber'))

legend2020 = plot_grid(legend,NULL,
                       ncol = 1)

plot2020 = plot_grid(trans2020, precip2020, temp2020,
                     ncol = 1)

plot_leg2020 = plot_grid(plot2020, legend2020,
                         rel_widths = c(0.9, 0.1))

title = ggdraw() + draw_label('Late Season 2020 Stream Depth, Temperature and Weather Observations',
                              fontface = 'bold')

plot_grid(title, plot_leg2020,
          ncol = 1,
          rel_heights = c(0.05, 0.95))

ggsave(file.path(figdir, 'late2020_transducer_precip_temp.png'),
       width = 10,
       height = 7,
       units = 'in',
       dpi = 300)

## visualization using tmap ----

### load sf/st layers ----
trans_locs = st_read(file.path(gisdir, 'study_sites/pressure_transducers/transducer_locs_v2022-05-18.shp'))
trans_locs_wgs = st_transform(trans_locs, crs = 'EPSG:4326')

#add some x/y mods for mapping with labels
trans_locs_wgs <- trans_locs_wgs %>% 
  mutate(name_abbv = case_when(grepl('505', ident) ~ 'Otter Pond - 505',
                               grepl('665', ident) ~ 'Chandler Johnson - 665',
                               grepl('788', ident) ~ 'Blodget South - 788',
                               grepl('790', ident) ~ 'Blodget North - 790',
                               grepl('800', ident) ~ 'Pike Brook - 800',
                               grepl('805', ident) ~ 'King Hill - 805',
                               grepl('830', ident) ~ 'Herrick South - 830',
                               TRUE ~ NA_character_)) %>% 
  filter(!is.na(name_abbv)) %>% 
  mutate(ymod = case_when(grepl('788', ident) ~ -0.75,
                          grepl('790', ident) ~ 0.75,
                          grepl('800', ident) ~ -0.75,
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
  tm_shape(sun_stream_wgs) + tm_lines(col = 'grey') +
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

tmap_save(transducer_map, file.path(figdir, 'sunapee_transducer_locs_v2022.png'))


### make map with .png of 1-year of trans data ----
trans_2020 <- trans_temp %>% 
  mutate(stream_nameno = case_when(stream_no == 505 ~ 'Otter\nPond\n505',
                                   stream_no == 665 ~ 'Chandler\nJohnson\n665',
                                   stream_no == 788 ~ 'Blodget\nSouth\n788',
                                   stream_no == 790 ~ 'Blodget\nNorth\n790',
                                   stream_no == 800 ~ 'Pike\nBrook\n800',
                                   stream_no == 805 ~ 'King\nHill\n805',
                                   stream_no == 830 ~ 'Herrick\nSouth\n830',
                                   TRUE ~ '')) %>% 
mutate(stream_nameno = factor(stream_nameno, levels = c('Otter\nPond\n505', 'Herrick\nSouth\n830', 'King\nHill\n805', 'Pike\nBrook\n800',
                                                    'Blodget\nNorth\n790', 'Blodget\nSouth\n788', 'Chandler\nJohnson\n665'))) %>% 
  filter(datetime >= as.Date('2020-01-01') & datetime < as.Date('2021-01-01'))
  
trans_2020_facet <- ggplot(trans_2020,
                aes(x = datetime, y = depth_m)) +
  geom_path(size = 1) +
  facet_grid(stream_nameno ~ .) +
  labs(x = NULL,
       y = 'stream depth\n(meters)') +
  scale_x_datetime(limits = c(as.POSIXct('2020-01-01'), as.POSIXct('2021-01-01'))) +
  final_theme_2 
trans_2020_facet

#2020 of year precip data
precip_sun$station = 'Sunapee\n \nGHCN'
precip_2020 <- precip_sun %>% 
  filter(date >= as.Date('2020-01-01') & date < as.Date('2021-01-01')) %>% 
    ggplot(., 
                 aes(x = date, y = precip_mm)) +
  geom_col(color = '#0009FF', fill = '#0009FF') +
  facet_grid(station ~ .) +
  final_theme_2 +
  labs(x = NULL,
       y = 'total daily\nprecipitation (mm)')
precip_2020

#fix width of precip 
precip_2020_widthfix <- plot_grid(NULL, precip_2020,
          rel_widths = c(0.002, 0.99),
          nrow = 1)
# store stream and precip data together
precip_trans_2020 <- plot_grid(precip_2020_widthfix, trans_2020_facet,
          rel_heights = c(1,5.5),
          ncol = 1)

#convert transducer map to tm_grob
bbox_ws_new[3] <- bbox_ws_new[3] + (0.1 * xrange) # xmax - right
transducer_map <- tm_shape(sun_ws_wgs, bbox = bbox_ws_new) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'grey') +
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

map_trans_precip = plot_grid(trans_map, precip_trans_2020,
          rel_widths = c(0.5,0.5))
map_trans_precip

ggsave(file.path(figdir, 'map_precip_transducer_2020.png'), 
       dpi = 300,
       height = 8,
       width = 12,
       units = 'in')


### make map with bubbles increasing with higher flow and precip ----

precip_2018_ani = precip_2018 + transition_states(date) + shadow_mark()
precip_2018_ani

trans_locs <- trans_locs %>% 
  mutate(stream_no = case_when(grepl('505', ident) ~ 505,
                               grepl('665', ident) ~ 665,
                               grepl('788', ident) ~ 788,
                               grepl('790', ident) ~ 790,
                               grepl('805', ident) ~ 805,
                               grepl('830', ident) ~ 830,
                               TRUE ~ NA_real_)) %>% 
  mutate(stream_no = factor(stream_no))

#get max daily value
trans_2018_daily <- trans_2018 %>% 
  mutate(date = as.Date(datetime)) %>% 
  group_by(stream_no, date) %>% 
  summarise(max_depth_m = max(depth_m, na.rm = T),
            mean_depth_m = mean(depth_m, na.rm = T))
#join with locdata
trans_2018_loc <- left_join(trans_2018_daily, trans_locs)
#make into sf  
trans_2018_loc_wgs = st_as_sf(trans_2018_loc, crs = 'EPSG:4326')
  
#store animation by day
animated_2018 = tm_shape(sun_ws_wgs, bbox = bbox_ws_new) + tm_borders() +
  tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(sunapee_shore) + tm_polygons() +
  tm_shape(trans_2018_loc_wgs) + 
    tm_symbols(shape = 21,
             col = 'blue',
             size = 'max_depth_m',
             scale = 3) +
  tm_facets(by = 'date',
            ncol = 1,
            nrow = 1)

#export gif
tmap_animation(animated_2018,
               filename = file.path(dump_dir, 'streams_maxdepth_animated_2018.gif'),
               fps = 1,
               dpi = 300)


