# script to summarize conductivity for visualization

library(tidyverse)

# filter and clean up cond for inlake cond ####
lmp_cond <- lmp %>% 
  filter(parameter == 'specificConductance_microSiemenPerCentimeter') %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(year = format(date, '%Y')) %>% 
  mutate(month = as.numeric(format(date, '%m')))

#filter jun - sept
lmp_summer_cond = lmp_cond %>% 
  filter(month >=6 & month <=9)

#grab only select locations
lmp_summer_cond_select <- lmp_summer_cond %>% 
  left_join(lmp_locs, .) 

#grab epi samples OR integrated at cove
lmp_summer_cond_select <- lmp_summer_cond_select %>% 
  filter(site_type == 'tributary' | 
           (site_type == 'lake' & layer == 'E') | 
           sub_site_type == 'cove')

## aggregate and join by year/site ----
lmp_cond_aggyearsite <- lmp_summer_cond_select %>% 
  filter(!is.na(value)) %>% 
  group_by(year, station, site_type, sub_site_type, lat_dd, lon_dd) %>% 
  summarize(n = n(),
            med_cond_uScm = median(value),
            max_cond_uScm = max(value),
            mean_cond_uScm = mean(value),
            thquan_cond_uScm = quantile(value, 0.75)) 

lmp_cond_aggyearsite <-lmp_cond_aggyearsite %>% 
  mutate(sub_site_type = factor(sub_site_type, levels = c( 'tributary', 'cove','deep')))


## aggregate and join by year ----
lmp_cond_aggyear <- lmp_summer_cond_select %>% 
  filter(!is.na(value)) %>% 
  group_by(year, sub_site_type) %>% 
  summarize(n = n(),
            med_cond_uScm = median(value),
            max_cond_uScm = max(value),
            mean_cond_uScm = mean(value),
            thquan_cond_uScm = quantile(value, 0.75)) 

lmp_cond_aggyear <-lmp_cond_aggyear %>% 
  mutate(sub_site_type = factor(sub_site_type, levels = c( 'tributary', 'cove','deep')))

