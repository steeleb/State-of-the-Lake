# deep dive into chlorophyll-a

library(tidyverse)
library(ggthemes)
library(gghighlight)

# read in LMP record
lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/LSPALMP_1986-2020_v2021-03-29.csv')

#read in station locations
lmp_locs <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/station_location_details.csv')

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_chla/'

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

#no layer info to worry about

ggplot(lmp_summer_chla, aes(x = date, y = value, color = loc_type)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

## facet by location ----
lmp_summer_chla %>% 
  filter(station == 200 |station == 210 | station == 220 |station == 230) %>% 
  ggplot(., aes(x = as.numeric(year), y = value)) +
  geom_point(color = '#E69F00') +
  gghighlight(value>2) +
  facet_grid(station ~ .) +
  geom_abline(slope = 0, intercept = 1, lty= 2)+
  theme_bw() +
  labs(x = NULL, y = 'chlorophyll-a (µg/L)')
ggsave(filename = file.path(dump_dir, 'alldata_allsites_deep_chla.png'),
       width = 4.5,
       height = 6,
       units = 'in',
       dpi = 300)

lmp_summer_chla %>% 
  filter(station == 10 |
           station == 20 |
           station == 30 |
           station == 60 |
           station == 70 |
           station == 80 |
           station == 90 |
           station == 110)%>% 
  ggplot(., aes(x = as.numeric(year), y = value)) +
  geom_point() +
  gghighlight(value>2) +
  facet_grid(station ~ .) +
  geom_abline(slope = 0, intercept = 1, lty= 2)+
  theme_bw() +
  labs(x = NULL, y = 'chlorophyll-a (µg/L)')
ggsave(filename = file.path(dump_dir, 'alldata_allsites_shallow_chla.png'),
       width = 4.5,
       height = 6,
       units = 'in',
       dpi = 300)
