# 1999 rainfall and turbidity in streams

library(tidyverse)
library(ggthemes)
library(gghighlight)

# read in LMP record
lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/LSPALMP_1986-2020_v2021-03-29.csv')

#point to dump directory
met_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/weather/PRISM data/'
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_turb/'

# load in stream turbidity ----
lmp_turb <- lmp %>% 
  filter(parameter == 'turb_NTU') %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(year = format(date, '%Y')) %>% 
  mutate(month = as.numeric(format(date, '%m')))

#filter jun - sept
lmp_summer_turb = lmp_turb %>% 
  filter(month >=6 & month <=9)

lmp_summer_turb_stream <- lmp_summer_turb %>% 
  filter(site_type == 'stream') %>% 
  arrange(station)

stream_locs = read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/station_location_details.csv')
stream_locs <- stream_locs %>% 
  filter(site_type == 'stream' &
           status == 'ongoing' &
           last_year == 2020 &3
           first_year <= 1994 &
           !is.na(lat_dd))

lmp_summer_turb_stream <- right_join(lmp_summer_turb_stream, stream_locs)

unique(lmp_summer_turb_stream$station)

#drop a few more incomplete streams and only the inlet streams
lmp_summer_turb_stream <- lmp_summer_turb_stream %>% 
  filter(station != 715 &
           station < 1000)

lmp_turb_stream_1999 <- lmp_summer_turb_stream %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date>= as.Date('1999-06-01') &
           date < as.Date('1999-10-01'))

### read in precip data from NOAA ----
precip_sun <- read.csv(file.path(met_dir, 'PRISM_ppt_tmin_tmean_tmax_stable_4km_19850101_20171231_nointerp.csv'), skip = 10) %>% 
  rename(date = Date,
         precip_mm = ppt..mm.) %>% 
  mutate(date = as.Date(date, format = '%m/%d/%Y'))

precip_1999 <- precip_sun %>% 
  filter(date >= as.Date('1999-06-01') &
           date < as.Date('1999-10-01'))

precip_1999 <- precip_1999 %>% 
  dplyr::select(date, precip_mm)

## join precip and stream data ----
precip_turb <- full_join(precip_1999, lmp_turb_stream_1999) %>% 
  mutate(station_date = paste(as.character(station), as.character(date), sep = ', ')) %>% 
  dplyr::select(date, precip_mm, value, station_date) %>% 
  rename(turb_NTU = value)

ggplot(precip_turb, aes(x = date)) +
  geom_col(aes(y = precip_mm), fill = 'dark grey', inherit.aes = T) +
  geom_point(aes(y = turb_NTU), size = 2, color ='black') +
  gghighlight(turb_NTU > 50 , unhighlighted_params = list(color = 'black'), label_key = station_date) +
  labs(x = '1999', y = 'PRISM precipitation (mm)\nturbidity (NTU)') +
  theme_bw() +
  scale_x_date(limits = c(as.Date('1999-06-01'), as.Date('1999-10-01')))

ggsave(file.path(dump_dir, '1999_precip_turbidity_stream.png'),
       width = 8,
       height = 6,
       units = 'in',
       dpi = 300)
