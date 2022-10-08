#tp and chla relationships - short story, nothing to see here.

library(tidyverse)
library(ggthemes)
library(gghighlight)

# read in LMP record
lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/LSPALMP_1986-2020_v2021-03-29.csv')

#read in station locations
lmp_locs <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/station_location_details.csv')

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/tp_chla/'

## load in-lake data ----
lmp_select <- lmp %>% 
  filter((parameter == 'TP_mgl' & layer == 'E') | 
           (parameter == 'TP_mgl' & station <200) | 
           parameter == 'chla_ugl' | 
           (parameter == 'cond_uScm' & layer == 'E') | 
           (parameter == 'cond_uScm' & station <200)) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(year = format(date, '%Y')) %>% 
  mutate(month = as.numeric(format(date, '%m')))

#filter jun - sept
lmp_summer_select = lmp_select %>% 
  filter(month >=6 & month <=9)

#filter for in-lake shallow (all integrated sample)
lmp_select_shallow <- lmp_summer_select %>% 
  filter(site_type == 'lake') %>% 
  filter(station == 10 |
           station == 20 |
           station == 30 |
           station == 60 |
           station == 70 |
           station == 80 |
           station == 90 |
           station == 110) %>% 
  mutate(site_type = 'shallow in lake')

#filter for in-lake and longterm sites; epi and integrated only
lmp_select_deep <- lmp_summer_select %>% 
  filter(site_type == 'lake') %>% 
  filter(station == 200 |
           station == 210 |
           station == 220 |
           station == 230 ) %>% 
  mutate(site_type = 'deep in lake')

lmp_select_join <- full_join(lmp_select_shallow, lmp_select_deep) %>% 
  select(station, site_type, date, value, parameter, year) %>% 
  filter(!is.na(site_type))

lmp_tp_select <-  lmp_select_join %>% 
  filter(parameter == 'TP_mgl') %>% 
  mutate(tp_ugl = value*1000) %>% 
  select(-parameter, -value) %>% 
  group_by(station, site_type, year) %>% 
  summarize(tp_ugl = mean(tp_ugl, na.rm = T))
lmp_chla_select <- lmp_select_join %>% 
  filter(parameter == 'chla_ugl') %>% 
  rename(chla_ugl = value) %>% 
  select(-parameter)%>% 
  group_by(station, site_type, year) %>% 
  summarize(chla_ugl = mean(chla_ugl, na.rm = T))
lmp_cond_select <- lmp_select_join %>% 
  filter(parameter == 'cond_uScm') %>% 
  rename(cond_uScm = value) %>% 
  group_by(station, site_type, year) %>% 
  summarize(cond_uScm = mean(cond_uScm, na.rm = T))

lmp_select_join <- full_join(lmp_tp_select, lmp_chla_select) %>% 
  full_join(., lmp_cond_select)

ggplot(lmp_select_join, aes(x = chla_ugl, y = tp_ugl)) + 
  geom_point() +
  facet_grid(site_type ~ ., scales = 'free_y') +
  theme_bw() +
  labs(x = 'average annual chlorophyll-a (ug/L)',
       y = 'average annual total phosphorus (ug/L)')
ggplot(lmp_select_join, aes(x = chla_ugl, y = cond_uScm)) + 
  geom_point() +
  facet_grid(site_type ~ ., scales = 'free_y') +
  theme_bw() +
  labs(x = 'average annual chlorophyll-a (ug/L)',
       y = 'average annual conductivity (uS/cm)')
ggplot(lmp_select_join, aes(x = cond_uScm, y = tp_ugl)) + 
  geom_point() +
  facet_grid(site_type ~ ., scales = 'free_y') +
  theme_bw() +
  labs(x = 'average annual conductivity (uS/cm)',
       y = 'average annual total phosphorus (ug/L)')
