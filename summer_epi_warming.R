# visualize summer epilimnion warming from LMP record

library(tidyverse)
library(rnoaa)

# read in LMP record
lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/LSPALMP_1986-2020_v2021-03-29.csv')

#read in station locations
lmp_locs <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/station_location_details.csv')

#point to dump directory
fig_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_epi_warming/'

# get min/max temp from newport weather station
newport_85_20 <-  ghcnd_search(stationid = 'USC00275868',
             date_min = '1985-01-01',
             date_max = '2020-01-01')
newport_min <- newport_85_20$tmin %>% 
  mutate(temp_min_degC = tmin/10) %>% 
  filter(!is.na(temp_min_degC))
newport_max <- newport_85_20$tmax %>% 
  mutate(temp_max_degC = tmax/10) %>% 
  filter(!is.na(temp_max_degC))


## filter and clean up temp ####
#filter for temp
unique(lmp$parameter)

lmp_temp <- lmp %>% 
  filter(parameter == 'temp_C') %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(year = as.numeric(format(date, '%Y'))) %>% 
  mutate(month = as.numeric(format(date, '%m')))

range(lmp_temp$month, na.rm = T)

#filter jun - sept
lmp_summer_temp = lmp_temp %>% 
  filter(month >=6 & month <=9)

unique(lmp_summer_temp$station)

#filter for deep sites; surface and bottom
lmp_temp_lake_surf <- lmp_summer_temp %>% 
  filter(site_type == 'lake') %>% 
  filter(station == 200 |
           station == 210 |
           station == 220 |
           station == 230) %>% 
  filter(depth_m <=1) %>% 
  group_by(year, month) %>% 
  summarize(med_surf_temp_C = median(value))


lmp_temp_lake_bottom <- lmp_summer_temp %>% 
  filter(site_type == 'lake') %>% 
  filter(station == 200 |
           station == 210 |
           station == 220 |
           station == 230) %>% 
  filter(depth_m > 20)%>% 
  group_by(year, month) %>% 
  summarize(med_bott_temp_C = median(value))

lmp_summer_aggtemp = full_join(lmp_temp_lake_surf, lmp_temp_lake_bottom) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value',-c('year', 'month')) %>% 
  filter(year >1990)

# median of max and min per month
newport_min <- newport_min %>% 
  mutate(date = as.Date(date), 
         year = as.numeric(format(date, '%Y')),
         month = as.numeric(format(date, '%m')))

newport_max <- newport_max %>% 
  mutate(date = as.Date(date), 
         year = as.numeric(format(date, '%Y')),
         month = as.numeric(format(date, '%m'))) 

newport_agg = full_join(newport_min, newport_max) %>%
  select(year, month, date, temp_min_degC, temp_max_degC) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -c('year', 'month', 'date'))

newport_summer_agg = newport_agg %>% 
  filter(month >=6 &
           month < 10) %>% 
  mutate(year_var = paste0(year, variable))
         

## visualize ----

ggplot(lmp_summer_aggtemp, aes(x = year, y = value)) +
  geom_boxplot(data = newport_summer_agg, aes(y = value, fill = variable, group = year_var)) +
  scale_fill_manual(values = c('#FAA5A5', '#C4F5FC')) +
  geom_point(data = lmp_summer_aggtemp, aes(color = variable), size = 4) +
  scale_color_manual(values = c('#0521F7' , '#F72205'))+
  facet_grid(month ~ .)+
  labs(y = NULL, 
       x = NULL)+
  theme_bw() +
  theme(legend.position = 'none')



ggsave(file.path(fig_dir, 'water_air_temp.png'),
       height = 6, 
       width = 10,
       units = 'in',
       dpi = 300)
