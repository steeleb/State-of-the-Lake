# script to summarize conductivity for visualization

library(tidyverse)

# read in LMP record
lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/LSPALMP_1986-2020_v2021-03-29.csv')

#read in station locations
lmp_locs <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/station_location_details.csv')

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
  filter(!is.na(value)) %>% 
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


# look at cond from streams ####
lmp_summer_cond_stream <- lmp_summer_cond %>% 
  filter(site_type == 'stream') %>% 
  arrange(station)


stream_locs = read.csv('C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/lmp_shortlist.csv')

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
  filter(!is.na(value)) %>% 
  summarize(n = n(),
            med_cond_uScm = median(value),
            max_cond_uScm = max(value),
            mean_cond_uScm = mean(value),
            thquan_cond_uScm = quantile(value, 0.75)) %>% 
  filter(n > 3) 

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

## join data sources ----
lmp_agg_lake <- right_join(lmp_agg_lake, lmp_locs)
head(lmp_agg_lake)
agg_cond_stream <- right_join(agg_cond_stream, lmp_locs)
head(agg_cond_stream)

ws_cond <- full_join(lmp_agg_lake, agg_cond_stream) %>% 
  mutate(data = case_when(site_type == 'stream' ~ 'tributary',
                          site_type == 'lake' & sub_site_type == 'cove' ~ 'shallow in-lake',
                          site_type == 'lake' & sub_site_type == 'deep' ~ 'deep in-lake')) %>% 
  mutate(data = factor(data, levels = c('tributary', 'shallow in-lake', 'deep in-lake')))

