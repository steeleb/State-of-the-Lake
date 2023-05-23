# code to visualize long term turbidity in lake sunapee

library(tidyverse)

# read in LMP record
lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/primary%20files/LSPALMP_1986-2022_v2023-01-22.csv')

#read in station locations
lmp_locs = read.csv('C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/lmp_shortlist.csv')


# filter and clean up turbidity for inlake turb ####
#filter for turb
unique(lmp$parameter)

lmp_turb <- lmp %>% 
  filter(parameter == 'turbidity_NTU') %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(year = format(date, '%Y')) %>% 
  mutate(month = as.numeric(format(date, '%m')))

#filter jun - sept
lmp_summer_turb = lmp_turb %>% 
  filter(month >=6 & month <=9)

lmp_summer_turb_select <- lmp_summer_turb %>% 
  left_join(lmp_locs, .) 

## aggregate and join by year/site ----
lmp_turb_aggyearsite <- lmp_summer_turb_select %>% 
  group_by(year, station, site_type, sub_site_type, lat_dd, lon_dd) %>% 
  summarize(n = n(),
            med_turb_NTU = median(value),
            max_turb_NTU = max(value),
            mean_turb_NTU = mean(value),
            thquan_turb_NTU = quantile(value, 0.75)) %>% 
  filter(n >= 3)

lmp_turb_aggyearsite <-lmp_turb_aggyearsite %>% 
  mutate(sub_site_type = factor(sub_site_type, levels = c( 'tributary', 'cove','deep')))


## aggregate and join by year ----
lmp_turb_aggyear <- lmp_summer_turb_select %>% 
  group_by(year, sub_site_type) %>% 
  summarize(n = n(),
            med_turb_NTU = median(value),
            max_turb_NTU = max(value),
            mean_turb_NTU = mean(value),
            thquan_turb_NTU = quantile(value, 0.75)) %>% 
  filter(n >= 3)

lmp_turb_aggyear <-lmp_turb_aggyear %>% 
  mutate(sub_site_type = factor(sub_site_type, levels = c( 'tributary', 'cove','deep')))

