library(tidyverse)
library(readxl)

#read in station locations
lmp_locs <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/station_location_details.csv')

lmp_lake_names <- read_xls('C:/Users/steeleb/Dropbox/travel/gis/project/Sunapee/study_sites/long_term_monitor_sites/Cove  Deep + Buoy WQ Sample Point GPS Coordinates.xls',
                           skip = 3) %>% 
  rename(station = WAYPOINT) %>% 
  select(station, Name) %>% 
  filter(station != 'Buoy' & station != '100.09999999999999') %>% 
  mutate(station = as.numeric(station))

lmp_trib_names <- read_xls('C:/Users/steeleb/Dropbox/travel/gis/project/Sunapee/study_sites/tributary_points/tributary_pts_alt.xls') %>% 
  rename(station = stream_no,
         Name = stream_name) %>% 
  select(station, Name)

names <- full_join(lmp_lake_names, lmp_trib_names)

lmp_shortlist <- lmp_locs %>% 
  filter(last_year == 2020 & first_year < 1995) %>% 
  left_join(., names) 

lmp_shortlist <- lmp_shortlist %>% 
  filter(!is.na(Name)) %>% 
  mutate(Name = paste0(station, ' - ', Name))%>% 
  mutate(sub_site_type = case_when(site_type == 'stream' ~ 'tributary',
                                   TRUE ~ sub_site_type))

write.csv(lmp_shortlist, 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/lmp_shortlist.csv')