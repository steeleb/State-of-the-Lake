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

#linear model
lm_temp_surf = lm(lmp_temp_lake_surf$med_surf_temp_C ~ lmp_temp_lake_surf$year)
summary(lm_temp_surf)

#linear models by month

lmp_temp_lake_surf_6 = lmp_temp_lake_surf %>% 
  filter(month == 6)
lm_6_surftemp = lm(lmp_temp_lake_surf_6$med_surf_temp_C ~ lmp_temp_lake_surf_6$year)
summary(lm_6_surftemp)

lmp_temp_lake_surf_7 = lmp_temp_lake_surf %>% 
  filter(month == 7)
lm_7_surftemp = lm(lmp_temp_lake_surf_7$med_surf_temp_C ~ lmp_temp_lake_surf_7$year)
summary(lm_7_surftemp)
#statistically significant warming of 0.12 deg/year

lmp_temp_lake_surf_8 = lmp_temp_lake_surf %>% 
  filter(month == 8)
lm_8_surftemp = lm(lmp_temp_lake_surf_8$med_surf_temp_C ~ lmp_temp_lake_surf_8$year)
summary(lm_8_surftemp)
#statistically significant warming of 0.05 deg/year

lmp_temp_lake_surf_9 = lmp_temp_lake_surf %>% 
  filter(month == 9)
lm_9_surftemp = lm(lmp_temp_lake_surf_9$med_surf_temp_C ~ lmp_temp_lake_surf_9$year)
summary(lm_9_surftemp)


lmp_temp_lake_bottom <- lmp_summer_temp %>% 
  filter(site_type == 'lake') %>% 
  filter(station == 200 |
           station == 210 |
           station == 220 |
           station == 230) %>% 
  filter(depth_m > 20)%>% 
  group_by(year, month) %>% 
  summarize(med_bott_temp_C = median(value))

#linear model
lm_temp_bott = lm(lmp_temp_lake_bottom$med_bott_temp_C ~ lmp_temp_lake_bottom$year)
summary(lm_temp_bott)

#linear models by month

lmp_temp_lake_bottom_6 = lmp_temp_lake_bottom %>% 
  filter(month == 6)
lm_6_bottomtemp = lm(lmp_temp_lake_bottom_6$med_bott_temp_C ~ lmp_temp_lake_bottom_6$year)
summary(lm_6_bottomtemp)

lmp_temp_lake_bottom_7 = lmp_temp_lake_bottom %>% 
  filter(month == 7)
lm_7_bottomtemp = lm(lmp_temp_lake_bottom_7$med_bott_temp_C ~ lmp_temp_lake_bottom_7$year)
summary(lm_7_bottomtemp)

lmp_temp_lake_bottom_8 = lmp_temp_lake_bottom %>% 
  filter(month == 8)
lm_8_bottomtemp = lm(lmp_temp_lake_bottom_8$med_bott_temp_C ~ lmp_temp_lake_bottom_8$year)
summary(lm_8_bottomtemp)

lmp_temp_lake_bottom_9 = lmp_temp_lake_bottom %>% 
  filter(month == 9)
lm_9_bottomtemp = lm(lmp_temp_lake_bottom_9$med_bott_temp_C ~ lmp_temp_lake_bottom_9$year)
summary(lm_9_bottomtemp)

#no warming at bottom depths

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

#model o fall data
newport_summer_agg_min <- newport_summer_agg %>% 
  filter(variable == 'temp_min_degC')
lm_newport_min = lm(newport_summer_agg_min$value ~ newport_summer_agg_min$year)
summary(lm_newport_min)      
# ss 0.01 deg/year

#model by month
newport_summer_agg_min_6 = newport_summer_agg_min %>% 
  filter(month == 6)
lm_newport_min_6 = lm(newport_summer_agg_min_6$value ~ newport_summer_agg_min_6$year)
summary(lm_newport_min_6)  
#no ss

newport_summer_agg_min_7 = newport_summer_agg_min %>% 
  filter(month == 7)
lm_newport_min_7 = lm(newport_summer_agg_min_7$value ~ newport_summer_agg_min_7$year)
summary(lm_newport_min_7)      
# ss 0.08/year

newport_summer_agg_min_8 = newport_summer_agg_min %>% 
  filter(month == 8)
lm_newport_min_8 = lm(newport_summer_agg_min_8$value ~ newport_summer_agg_min_8$year)
summary(lm_newport_min_8)      
# ss 0.06/year

newport_summer_agg_min_9 = newport_summer_agg_min %>% 
  filter(month == 9)
lm_newport_min_9 = lm(newport_summer_agg_min_9$value ~ newport_summer_agg_min_9$year)
summary(lm_newport_min_9)      
## ss 0.11 deg/year

#model o fall data
newport_summer_agg_max <- newport_summer_agg %>% 
  filter(variable == 'temp_max_degC')
lm_newport_max = lm(newport_summer_agg_max$value ~ newport_summer_agg_max$year)
summary(lm_newport_max)      
# ss 0.02 deg/year

#model by month
newport_summer_agg_max_6 = newport_summer_agg_max %>% 
  filter(month == 6)
lm_newport_max_6 = lm(newport_summer_agg_max_6$value ~ newport_summer_agg_max_6$year)
summary(lm_newport_max_6)  
# ss -0.04/year

newport_summer_agg_max_7 = newport_summer_agg_max %>% 
  filter(month == 7)
lm_newport_max_7 = lm(newport_summer_agg_max_7$value ~ newport_summer_agg_max_7$year)
summary(lm_newport_max_7)      
# ss 0.04/year

newport_summer_agg_max_8 = newport_summer_agg_max %>% 
  filter(month == 8)
lm_newport_max_8 = lm(newport_summer_agg_max_8$value ~ newport_summer_agg_max_8$year)
summary(lm_newport_max_8)      
# no ss

newport_summer_agg_max_9 = newport_summer_agg_max %>% 
  filter(month == 9)
lm_newport_max_9 = lm(newport_summer_agg_max_9$value ~ newport_summer_agg_max_9$year)
summary(lm_newport_max_9)      
## ss 0.07 deg/year

### summarize in table ----

data_names = c('GHCN max', 'GHCN min', 'LMP surface', 'LMP bottom')
summer_vals = data.frame(`Jun-Sept` = c(round(summary(lm_newport_max)$coefficients[2,1], digits =2),
                                        round(summary(lm_newport_min)$coefficients[2,1], digits =2),
                                        0, 0))
jun_vals = data.frame(Jun = c(round(summary(lm_newport_max_6)$coefficients[2,1], digits =2),
                              0, 0, 0))
jul_vals = data.frame(Jul = c(round(summary(lm_newport_max_7)$coefficients[2,1], digits =2),
             round(summary(lm_newport_min_7)$coefficients[2,1], digits =2),
             round(summary(lm_7_surftemp)$coefficients[2,1], digits =2),
             0))
aug_vals = data.frame(Aug = c(round(summary(lm_newport_max_8)$coefficients[2,1], digits =2),
             round(summary(lm_newport_min_8)$coefficients[2,1], digits =2),
             round(summary(lm_8_surftemp)$coefficients[2,1], digits =2),
             0))
sep_vals = data.frame(Sep = c(round(summary(lm_newport_max_9)$coefficients[2,1], digits =2),
             round(summary(lm_newport_min_9)$coefficients[2,1], digits =2),
             0, 0))

summary_temp <- data.frame(data_names,summer_vals, jun_vals, jul_vals, aug_vals, sep_vals)
write_csv(summary_temp, file.path(fig_dir, 'temp_trends.csv'))

## visualize ----

# #both max and min
# ggplot(lmp_summer_aggtemp, aes(x = year, y = value)) +
#   geom_boxplot(data = newport_summer_agg, aes(y = value, fill = variable, group = year_var)) +
#   scale_fill_manual(values = c('#FAA5A5', '#C4F5FC')) +
#   geom_point(data = lmp_summer_aggtemp, aes(color = variable), size = 4) +
#   scale_color_manual(values = c('#0521F7' , '#F72205'))+
#   facet_grid(month ~ .)+
#   labs(y = NULL, 
#        x = NULL)+
#   theme_bw() +
#   theme(legend.position = 'none')
# 
# ggsave(file.path(fig_dir, 'water_air_temp.png'),
#        height = 6, 
#        width = 10,
#        units = 'in',
#        dpi = 300)

#plot by month, use ribbon (SD around mean value)

sd_6_newport = sd() * sqrt()
ggplot(lmp_temp_lake_surf_6, aes(x = year, y = value)) +
  geom_ribbon(data = newport_summer_agg_max_6, aes(y = value, fill = variable, group = year_var)) +
  scale_fill_manual(values = c('#FAA5A5', '#C4F5FC')) +
  geom_point(data = lmp_summer_aggtemp, aes(color = variable), size = 4) +
  scale_color_manual(values = c('#0521F7' , '#F72205'))+
  facet_grid(month ~ .)+
  labs(y = NULL,
       x = NULL)+
  theme_bw() +
  theme(legend.position = 'none')
