# code to visualize long term phosphorus in lake sunapee

library(tidyverse)

# read in LMP record
lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/LSPALMP_1986-2020_v2021-03-29.csv')

#read in station locations
lmp_locs = read.csv('C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/lmp_shortlist.csv')

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
lmp_tp_lake <- left_join(lmp_locs, lmp_summer_tp) %>% 
  filter(site_type == 'lake') %>% 
  filter(sub_site_type == 'cove' | (sub_site_type == 'deep' & layer == 'E'))
  
ggplot(lmp_tp_lake, aes(x = date, y = value)) +
  geom_point() +
  facet_grid(station ~.) +
  theme_bw()

#aggregate to median, max, mean, 3rd quartile value
lmp_agg_tp_lake <- lmp_tp_lake %>% 
  group_by(station, year) %>% 
  summarize(n = n(),
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value)*1000,
            mean_tp_ugl = mean(value)*1000,
            thquan_tp_ugl = quantile(value, 0.75)*1000)

# apply station info to 2 datasets
lmp_agg_tp_lake <- left_join(lmp_agg_tp_lake, lmp_locs)


# look at TP from tribs ####
lmp_summer_tp_trib <- lmp_summer_tp %>% 
  filter(site_type == 'stream')

unique(lmp_summer_tp_trib$station)

ggplot(lmp_summer_tp_trib, aes(x = date, y = value)) +
  geom_point() +
  facet_grid(station ~ .)

#aggregate and change units
agg_tp_trib <- lmp_summer_tp_trib %>% 
  group_by(station, year) %>% 
  summarize(n = n(),
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value)*1000,
            mean_tp_ugl = mean(value)*1000,
            thquan_tp_ugl = quantile(value, 0.75)*1000) %>% 
  filter(n > 3) 

agg_tp_trib <- agg_tp_trib %>% 
  left_join(lmp_locs, .) %>% 
  filter(site_type == 'stream') %>% 
  mutate(site_type = 'tributary')

# join data sources ----
ws_tp <- full_join(lmp_agg_tp_lake, agg_tp_trib) %>% 
  mutate(data = case_when(site_type == 'tributary' ~ 'tributary',
                          site_type == 'lake' & sub_site_type == 'cove' ~ 'shallow in-lake',
                          site_type == 'lake' & sub_site_type == 'deep' ~ 'deep in-lake')) %>% 
  mutate(data = factor(data, levels = c('tributary', 'shallow in-lake', 'deep in-lake'))) %>% 
  select(-X)


