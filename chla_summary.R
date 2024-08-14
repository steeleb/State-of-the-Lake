# code to summarize chla data for visualization

library(tidyverse)

# filter and clean up chla for inlake chla ####
lmp_chla <- lmp %>% 
  filter(parameter == 'chlorophyll_a_ugl') %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(year = format(date, '%Y')) %>% 
  mutate(month = as.numeric(format(date, '%m')))

#filter jun - sept
lmp_summer_chla = lmp_chla %>% 
  filter(month >=6 & month <=9)

lmp_summer_chla_select <- lmp_summer_chla %>% 
  left_join(lmp_locs, .) 

## aggregate and join by year/site ----
lmp_chla_aggyearsite <- lmp_summer_chla_select %>% 
  filter(!is.na(value)) %>% 
  group_by(year, station, site_type, sub_site_type, lat_dd, lon_dd) %>% 
  summarize(n = n(),
            med_chla_ugl = median(value),
            max_chla_ugl = max(value),
            mean_chla_ugl = mean(value),
            thquan_chla_ugl = quantile(value, 0.75)) %>% 
  filter(n >= 3)

lmp_chla_aggyearsite <-lmp_chla_aggyearsite %>% 
  mutate(sub_site_type = factor(sub_site_type, levels = c( 'tributary', 'cove','deep')))


## aggregate and join by year ----
lmp_chla_aggyear <- lmp_summer_chla_select %>% 
  filter(!is.na(value)) %>% 
  group_by(year, sub_site_type) %>% 
  summarize(n = n(),
            med_chla_ugl = median(value),
            max_chla_ugl = max(value),
            mean_chla_ugl = mean(value),
            thquan_chla_ugl = quantile(value, 0.75)) %>% 
  filter(n >= 3)

lmp_chla_aggyear <-lmp_chla_aggyear %>% 
  mutate(sub_site_type = factor(sub_site_type, levels = c( 'tributary', 'cove','deep')))



