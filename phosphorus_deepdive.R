# code to visualize long term phosphorus in lake sunapee

# library(sf)
# library(tmap)
# library(raster)
# # library(gifski)
library(tidyverse)
library(ggthemes)
library(gghighlight)

# read in LMP record
lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/LSPALMP_1986-2020_v2021-03-29.csv')

#read in station locations
lmp_locs <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/station_location_details.csv')

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_tp/'

# look at differences in TP across streams and shallow sites ----

## load in-lake data ----
lmp_tp <- lmp %>% 
  filter(parameter == 'TP_mgl') %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(year = format(date, '%Y')) %>% 
  mutate(month = as.numeric(format(date, '%m')))

#filter jun - sept
lmp_summer_tp = lmp_tp %>% 
  filter(month >=6 & month <=9)

#filter for in-lake and longterm sites; epi and integrated only
lmp_tp_shallow <- lmp_summer_tp %>% 
  filter(site_type == 'lake') %>% 
  filter(station == 10 |
           station == 20 |
           station == 30 |
           station == 60 |
           station == 70 |
           station == 80 |
           station == 90 |
           station == 110) %>% 
  mutate(tp_ugl = value *1000)

## load stream data ----
lmp_summer_tp_stream <- lmp_summer_tp %>% 
  filter(site_type == 'stream') %>% 
  arrange(station) %>% 
  mutate(site_type = 'tributary')


stream_locs = read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/station_location_details.csv')
stream_locs <- stream_locs %>% 
  filter(site_type == 'stream' &
           status == 'ongoing' &
           last_year == 2020 &
           first_year <= 1994 &
           !is.na(lat_dd)) %>% 
  mutate(site_type = 'tributary')

lmp_summer_tp_stream <- right_join(lmp_summer_tp_stream, stream_locs)

unique(lmp_summer_tp_stream$station)

ggplot(lmp_summer_tp_stream, aes(x = date, y = value)) +
  geom_point() +
  facet_grid(station ~ .)

#drop a few more incomplete streams and only the inlet streams
lmp_summer_tp_stream <- lmp_summer_tp_stream %>% 
  filter(station != 715 &
           station < 1000)

# convert to ugl
lmp_summer_tp_stream <- lmp_summer_tp_stream %>% 
  mutate(tp_ugl = value *1000)

## plot data ----
ggplot(lmp_tp_shallow, aes(x = as.numeric(year), y = tp_ugl)) +
  geom_point(shape = 17, color = '#E69F00') +
  facet_grid(station ~.) +
  gghighlight(tp_ugl > 30, use_direct_label = FALSE, calculate_per_facet = TRUE) + #highlight the values above 20ug/L, the approximate average from the past few years
  labs(x = NULL,
       y = paste0('Total Phosphorus (µg/L)')) +
  theme_bw()
ggsave(filename = file.path(dump_dir, 'alldata_allsites_shallow_tp.png'),
          width = 4.5,
          height = 6,
          units = 'in',
          dpi = 300)
ggplot(lmp_tp_shallow, aes(x = as.numeric(year), y = tp_ugl)) +
  geom_point(shape = 17, color = '#E69F00') +
  facet_grid(station ~., scales= 'free_y') +
  gghighlight(tp_ugl > 30,  use_direct_label = FALSE, calculate_per_facet = TRUE) + #highlight the values above 20ug/L, the approximate average from the past few years
  labs(x = NULL,
       y = paste0('Total Phosphorus (µg/L)')) +
  theme_bw()
ggsave(filename = file.path(dump_dir, 'alldata_allsites_shallow_tp_freey.png'),
       width = 4.5,
       height = 6,
       units = 'in',
       dpi = 300)
lmp_tp_shallow %>% 
  filter(tp_ugl < 80) %>% 
  ggplot(., aes(x = as.numeric(year), y = tp_ugl)) +
  geom_point(shape = 17, color = '#E69F00') +
  facet_grid(station ~., scales= 'free_y') +
  gghighlight(tp_ugl > 30,  use_direct_label = FALSE, calculate_per_facet = TRUE) + #highlight the values above 20ug/L, the approximate average from the past few years
  labs(x = NULL,
       y = paste0('Total Phosphorus (µg/L)')) +
  theme_bw()
ggsave(filename = file.path(dump_dir, 'alldata_allsites_shallow_tp_freey_lt80.png'),
       width = 4.5,
       height = 6,
       units = 'in',
       dpi = 300)
lmp_tp_shallow %>% 
  filter(tp_ugl < 80) %>% 
  ggplot(., aes(x = as.numeric(year), y = tp_ugl)) +
  geom_point(shape = 17, color = '#E69F00') +
  facet_grid(station ~.) +
  gghighlight(tp_ugl > 30,  use_direct_label = FALSE, calculate_per_facet = TRUE) + #highlight the values above 20ug/L, the approximate average from the past few years
  labs(x = NULL,
       y = paste0('Total Phosphorus (µg/L)')) +
  theme_bw()
ggsave(filename = file.path(dump_dir, 'alldata_allsites_shallow_tp_lt80.png'),
       width = 4.5,
       height = 6,
       units = 'in',
       dpi = 300)

ggplot(lmp_summer_tp_stream, aes(x = as.numeric(year), y = tp_ugl)) +
  geom_point() +
  facet_grid(station ~.) +
  gghighlight(tp_ugl > 25,  use_direct_label = FALSE, calculate_per_facet = TRUE) + #highlight the values above 25ug/L, the approximate average from the past few years
  labs(x = NULL,
       y = paste0('Total Phosphorus (µg/L)')) +
  theme_bw()
ggsave(filename = file.path(dump_dir, 'alldata_allsites_stream_tp.png'),
       width = 4.5,
       height = 8,
       units = 'in',
       dpi = 300)
ggplot(lmp_summer_tp_stream, aes(x = as.numeric(year), y = tp_ugl)) +
  geom_point() +
  facet_grid(station ~.) +
  scale_y_continuous(limits = c(0, ))
  gghighlight(tp_ugl > 25,  use_direct_label = FALSE, calculate_per_facet = TRUE) + #highlight the values above 25ug/L, the approximate average from the past few years
  labs(x = NULL,
       y = paste0('Total Phosphorus (µg/L)')) +
  theme_bw()
ggsave(filename = file.path(dump_dir, 'alldata_allsites_stream_tp.png'),
       width = 4.5,
       height = 8,
       units = 'in',
       dpi = 300)

#filter outliers
lmp_summer_tp_stream %>% 
  filter(tp_ugl < 400) %>% 
  ggplot(., aes(x = as.numeric(year), y = tp_ugl)) +
  geom_point() +
  facet_grid(station ~., scales = 'free_y') +
  gghighlight(tp_ugl > 25,  use_direct_label = FALSE, calculate_per_facet = TRUE) + #highlight the values above 25ug/L, the approximate average from the past few years
  labs(x = NULL,
       y = paste0('Total Phosphorus (µg/L)')) +
  theme_bw()
ggsave(filename = file.path(dump_dir, 'alldata_allsites_stream_tp_freey_lt400.png'),
       width = 4.5,
       height = 8,
       units = 'in',
       dpi = 300)

