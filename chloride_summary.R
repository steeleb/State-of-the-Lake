# code to summarize cl data for visualization

library(tidyverse)

# read in LMP record
lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/primary%20files/LSPALMP_1986-2023_v2024-01-20.csv')

#read in station locations
lmp_locs = read.csv('C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/lmp_shortlist.csv') %>% 
  mutate(site_type = if_else(site_type == "stream", "tributary", site_type))

# filter and clean up cl for inlake cl ####
#filter for cl
unique(lmp$parameter)

lmp_cl <- lmp %>% 
  filter(parameter == 'chloride_mgl') %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(year = format(date, '%Y')) %>% 
  mutate(month = as.numeric(format(date, '%m')))

lmp_ltjune_cl <- lmp_cl %>% 
  filter(month < 6)

#filter jun - sept
lmp_summer_cl = lmp_cl %>% 
  filter(month >=6 & month <=9)

lmp_summer_cl_select <- lmp_summer_cl %>% 
  left_join(lmp_locs, .) 

lmp_ltjune_cl_select <- lmp_ltjune_cl %>% 
  left_join(lmp_locs, .) 


## aggregate and join by year/site for early season values ----
lmp_cl_aggyearsite_ltjune <- lmp_ltjune_cl_select %>% 
  filter(!is.na(value)) %>% 
  group_by(year, station, site_type, sub_site_type, lat_dd, lon_dd) %>% 
  summarize(n = n(),
            med_cl_ugl = median(value),
            max_cl_ugl = max(value),
            mean_cl_ugl = mean(value),
            thquan_cl_ugl = quantile(value, 0.75)) 

lmp_cl_aggyearsite_ltjune <-lmp_cl_aggyearsite_ltjune %>% 
  mutate(sub_site_type = factor(sub_site_type, levels = c( 'tributary', 'cove','deep')))


## aggregate and join by year/site ----
lmp_cl_aggyearsite <- lmp_summer_cl_select %>% 
  filter(!is.na(value)) %>% 
  group_by(year, station, site_type, sub_site_type, lat_dd, lon_dd) %>% 
  summarize(n = n(),
            med_cl_ugl = median(value),
            max_cl_ugl = max(value),
            mean_cl_ugl = mean(value),
            thquan_cl_ugl = quantile(value, 0.75)) 

lmp_cl_aggyearsite <-lmp_cl_aggyearsite %>% 
  mutate(sub_site_type = factor(sub_site_type, levels = c( 'tributary', 'cove','deep')))


## aggregate and join by year ----
lmp_cl_aggyear <- lmp_summer_cl_select %>% 
  filter(!is.na(value)) %>% 
  group_by(year, sub_site_type) %>% 
  summarize(n = n(),
            med_cl_ugl = median(value),
            max_cl_ugl = max(value),
            mean_cl_ugl = mean(value),
            thquan_cl_ugl = quantile(value, 0.75)) 

lmp_cl_aggyear <-lmp_cl_aggyear %>% 
  mutate(sub_site_type = factor(sub_site_type, levels = c( 'tributary', 'cove','deep')))



