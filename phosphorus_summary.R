# code to visualize long term phosphorus in lake sunapee

library(tidyverse)

# read in LMP record
lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/primary%20files/LSPALMP_1986-2022_v2023-01-22.csv')

#read in station locations
lmp_locs = read.csv('C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/lmp_shortlist.csv')

# filter and clean up TP for inlake TP ####
#filter for TP
unique(lmp$parameter)

lmp_tp <- lmp %>% 
  filter(parameter == 'phosphorusTotal_mgl') %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(year = format(date, '%Y')) %>% 
  mutate(month = as.numeric(format(date, '%m')))

#filter jun - sept
lmp_summer_tp = lmp_tp %>% 
  filter(month >=6 & month <=9)

#grab only select locations
lmp_summer_tp_select <- lmp_summer_tp %>% 
  left_join(lmp_locs, .) 

#grab epi samples OR integrated at cove
lmp_summer_tp_select <- lmp_summer_tp_select %>% 
  filter(site_type == 'stream' | (site_type == 'lake' & layer == 'E') | sub_site_type == 'cove') %>% 
  filter(!is.na(value))

## aggregate and join by year/site ----
lmp_tp_aggyearsite <- lmp_summer_tp_select %>% 
  mutate(year = format(as.Date(date), '%Y')) %>% 
  group_by(year, station, site_type, sub_site_type, lat_dd, lon_dd) %>% 
  summarize(n = n(),
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value)*1000,
            mean_tp_ugl = mean(value)*1000,
            thquan_tp_ugl = quantile(value, 0.75)*1000) 

lmp_tp_aggyearsite <-lmp_tp_aggyearsite %>% 
  mutate(sub_site_type = factor(sub_site_type, levels = c( 'tributary', 'cove','deep')))


## aggregate and join by year ----
lmp_tp_aggyear <- lmp_summer_tp_select %>% 
  group_by(year, sub_site_type) %>% 
  summarize(n = n(),
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value)*1000,
            mean_tp_ugl = mean(value)*1000,
            thquan_tp_ugl = quantile(value, 0.75)*1000) 

lmp_tp_aggyear <-lmp_tp_aggyear %>% 
  mutate(sub_site_type = factor(sub_site_type, levels = c( 'tributary', 'cove','deep')))

