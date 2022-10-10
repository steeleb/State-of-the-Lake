# code to summarize chla data for visualization

library(tidyverse)

# read in LMP record
lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/LSPALMP_1986-2020_v2021-03-29.csv')

#read in station locations
lmp_locs = read.csv('C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/lmp_shortlist.csv')

# filter and clean up chla for inlake chla ####
#filter for chla
unique(lmp$parameter)

lmp_chla <- lmp %>% 
  filter(parameter == 'chla_ugl') %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(year = format(date, '%Y')) %>% 
  mutate(month = as.numeric(format(date, '%m')))

#filter jun - sept
lmp_summer_chla = lmp_chla %>% 
  filter(month >=6 & month <=9)

#add location info
lmp_summer_chla <- lmp_summer_chla %>% 
  mutate(loc_type = case_when(station < 200 ~ 'near-shore',
                              TRUE ~ 'deep')) %>% 
  filter(loc_type != 'other') %>% 
  filter(!is.na(value))
unique(lmp_summer_chla$station)
#no layer info to worry about

ggplot(lmp_summer_chla, aes(x = date, y = value, color = loc_type)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

#aggregate to median, max, mean, 3rd quartile value
lmp_agg_lake <- lmp_summer_chla %>% 
  group_by(station, year, loc_type) %>% 
  summarize(n = n(),
            med_chla_ugl = median(value),
            max_chla_ugl = max(value),
            mean_chla_ugl = mean(value),
            thquan_chla_ugl = quantile(value, 0.75))

ggplot(lmp_agg_lake, aes(x = as.numeric(year), y = med_chla_ugl, color = loc_type)) +
  geom_point() +
  geom_smooth()+
  theme_bw()

ggplot(lmp_agg_lake, aes(x = as.numeric(year), y = med_chla_ugl, color = loc_type)) +
  geom_point() +
  geom_smooth()+
  theme_bw()


## get station list and apply loc info ####
stationlist <- data.frame(unique(lmp_summer_chla$station))
colnames(stationlist) = 'station'

stationlist <- left_join(stationlist, lmp_locs)

# apply station info to 2 datasets
lmp_agg_lake <- full_join(lmp_agg_lake, stationlist) %>% 
  filter(!is.na(lat_dd))

# create scatterplot of in-lake deep/shallow and stream input (of those which inlet to lake) over time----

lmp_summer_chla_deep <- lmp_summer_chla %>% 
  filter((station == 200 |
            station == 210 |
            station == 220 |
            station == 230)
         & site_type == 'lake')

lmp_summer_chla_shallow <- lmp_summer_chla %>% 
  filter(station < 200 & site_type == 'lake')
unique(lmp_summer_chla_shallow$station)


## aggregate and join ----
lmp_chla_deep_agg <- lmp_summer_chla_deep %>% 
  group_by(year, station) %>% 
  summarize(n = n(),
            med_chla_ugl = median(value),
            max_chla_ugl = max(value),
            mean_chla_ugl = mean(value),
            thquan_chla_ugl = quantile(value, 0.75)) %>% 
  mutate(data = 'deep')

lmp_chla_shallow_agg <- lmp_summer_chla_shallow %>% 
  group_by(year, station) %>% 
  summarize(n = n(),
            med_chla_ugl = median(value),
            max_chla_ugl = max(value),
            mean_chla_ugl = mean(value),
            thquan_chla_ugl = quantile(value, 0.75)) %>% 
  mutate(data = 'shallow')

lmp_chla_aggyear <- full_join(lmp_chla_deep_agg, lmp_chla_shallow_agg) %>% 
  mutate(data = factor(data, levels = c('shallow', 'deep')))

## aggregate and join by year ----
lmp_chla_deep_agg_yr <- lmp_summer_chla_deep %>% 
  group_by(year) %>% 
  summarize(n = n(),
            med_chla_ugl = median(value),
            max_chla_ugl = max(value),
            mean_chla_ugl = mean(value),
            thquan_chla_ugl = quantile(value, 0.75)) %>% 
  mutate(data = 'deep')

lmp_chla_shallow_agg_yr <- lmp_summer_chla_shallow %>% 
  group_by(year) %>% 
  summarize(n = n(),
            med_chla_ugl = median(value),
            max_chla_ugl = max(value),
            mean_chla_ugl = mean(value),
            thquan_chla_ugl = quantile(value, 0.75)) %>% 
  mutate(data = 'shallow')

lmp_chla_aggyear_yr <- full_join(lmp_chla_deep_agg_yr, lmp_chla_shallow_agg_yr) %>% 
  mutate(data = factor(data, levels = c('shallow', 'deep')))


