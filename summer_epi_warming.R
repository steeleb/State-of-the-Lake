# visualize summer epilimnion warming from LMP record

library(tidyverse)
library(rnoaa)
library(ggthemes)

#add final theme for formatting ggplots
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom

# read in LMP record
lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/LSPALMP_1986-2020_v2021-03-29.csv')

#read in station locations
lmp_locs <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/station_location_details.csv')

#point to dump directory
fig_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_epi_warming/'
ncdc_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/weather/NCDC/Lebanon NH/'

### air temp data ----

#load temp from NCDC
leb <- read.csv(file.path(ncdc_dir, 'NCDC_lebanon_daily_1998-2021.csv')) %>% 
  select(date, DailyAverageDryBulbTemperature) %>% 
  mutate(date = as.Date(date),
         DailyAverageDryBulbTemperature = as.numeric(DailyAverageDryBulbTemperature), # coercion error okay
         airTempC = (DailyAverageDryBulbTemperature-32)/1.8) %>% 
  select(date, airTempC)

leb_summer <- leb %>% 
  mutate(year = as.numeric(format(date, '%Y')),
         month = as.numeric(format(date, '%m'))) %>% 
  filter(month >=6 & month <10)

ggplot(leb_summer, aes(x = year,  y = airTempC)) +
  geom_jitter(color = 'grey', width = 0.1) +
  geom_boxplot(aes(group = year), alpha = 0.5) +
  geom_smooth(method = 'lm', se = F, lty = 2) +
  labs(x = NULL,
       y = 'average daily air temperature',
       title = 'Summer (Jun-Sept) Air Temperature at Lebanon Airport') +
  final_theme +
  scale_y_continuous(breaks = c(10, 20, 30), labels = c('10°C | 50°F', '20°C | 68°F', '30°C | 86°F')) +
  theme(axis.text.y = element_text(angle = 45))

ggsave(file.path(fig_dir, 'leb_summer_temp.png'),
       width = 9,
       height = 4,
       dpi = 300,
       units = 'in')

summary(lm(leb_summer$airTempC ~ leb_summer$year))

#linear models per month
leb_jun <- leb_summer %>% 
  filter(month == 6)
summary(lm(leb_jun$airTempC ~ leb_jun$year))
#ss 0.05/y
leb_jul <- leb_summer %>% 
  filter(month == 7)
summary(lm(leb_jul$airTempC ~ leb_jul$year))
#ss 0.12/y
leb_aug <- leb_summer %>% 
  filter(month == 8)
summary(lm(leb_aug$airTempC ~ leb_aug$year))
#ss 0.08/y
leb_sep <- leb_summer %>% 
  filter(month == 9)
summary(lm(leb_sep$airTempC ~ leb_sep$year))
#ss 0.07/y

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

#### filter for deep sites at surface ----
lmp_temp_lake_surf <- lmp_summer_temp %>% 
  filter(site_type == 'lake') %>% 
  filter(station == 200 |
           station == 210 |
           station == 220 |
           station == 230) %>%
  filter(depth_m <=0.5) 

ggplot(lmp_temp_lake_surf, aes(x = year, y = value)) +
  geom_jitter(color = 'grey', width = 0.1) +
  geom_boxplot(aes(group = year), alpha = 0.5) +
  geom_smooth(method = 'lm', se = F) +
  labs(x = NULL, 
       y = 'surface water temperature',
       title = 'Summer (Jun-Sept) Surface Water Temperature at Deep Sites\nat Lake Sunapee') +
  final_theme+
  scale_y_continuous(breaks = c(15, 20, 25), labels = c('15°C | 59°F', '20°C | 68°F', '25°C | 77°F')) +
  theme(axis.text.y = element_text(angle = 45))

ggsave(file.path(fig_dir, 'lmp_deep_surface_summer_temp.png'),
       width = 9,
       height = 4.3,
       dpi = 300,
       units = 'in')


#linear model
lm_temp_surf = lm(lmp_temp_lake_surf$value ~ lmp_temp_lake_surf$year)
summary(lm_temp_surf)
#statistically significant increase 0.02/y 

#linear models by month

lmp_temp_lake_surf_6 = lmp_temp_lake_surf %>% 
  filter(month == 6)
lm_6_surftemp = lm(lmp_temp_lake_surf_6$value ~ lmp_temp_lake_surf_6$year)
summary(lm_6_surftemp)

lmp_temp_lake_surf_7 = lmp_temp_lake_surf %>% 
  filter(month == 7)
lm_7_surftemp = lm(lmp_temp_lake_surf_7$value ~ lmp_temp_lake_surf_7$year)
summary(lm_7_surftemp)
#statistically significant warming of 0.12 deg/year
ggplot(lmp_temp_lake_surf_7, aes(x = year, y = value)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F, lty=2) +
  final_theme +
  labs(x = NULL,
       y = 'water temperature',
       title = 'July Lake Sunapee deep sites surface temperature') +
  scale_y_continuous(breaks = c(10, 15, 20, 25, 30), 
                     labels = c('10°C | 50°F', '15°C | 59°F', '20°C | 68°F', '25°C | 77°F','30°C | 86°F'),
                     limits = c(15, 30)) +
  theme(axis.text.y = element_text(angle = 20))
ggsave(file.path(fig_dir, 'lmp_july_water_temp_hist.png'),
       width = 9,
       height = 3,
       dpi = 300,
       units = 'in')

lmp_temp_lake_surf_8 = lmp_temp_lake_surf %>% 
  filter(month == 8)
lm_8_surftemp = lm(lmp_temp_lake_surf_8$value ~ lmp_temp_lake_surf_8$year)
summary(lm_8_surftemp)
#statistically significant warming of 0.05 deg/year
ggplot(lmp_temp_lake_surf_8, aes(x = year, y = value)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F, lty=2) +
  final_theme +
  labs(x = NULL,
       y = 'water temperature',
       title = 'August Lake Sunapee deep sites surface temperature') +
  scale_y_continuous(breaks = c(10, 15, 20, 25, 30), 
                     labels = c('10°C | 50°F', '15°C | 59°F', '20°C | 68°F', '25°C | 77°F','30°C | 86°F'),
                     limits = c(15, 30)) +
  theme(axis.text.y = element_text(angle = 20))
ggsave(file.path(fig_dir, 'lmp_aug_water_temp_hist.png'),
       width = 9,
       height = 3,
       dpi = 300,
       units = 'in')

lmp_temp_lake_surf_9 = lmp_temp_lake_surf %>% 
  filter(month == 9)
lm_9_surftemp = lm(lmp_temp_lake_surf_9$value ~ lmp_temp_lake_surf_9$year)
summary(lm_9_surftemp)





### plot surf temp and air temp together (use buoy data) ----
buoywater_2021 <- read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.499.3&entityid=e69f75756b69ae1b7a6d20f7c4370671')
buoymet_2021 <- read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.234.6&entityid=15dcd106aee68b11a4fd102166efda06')

buoy_surface <- buoywater_2021 %>% 
  filter(location == 'loon') %>% 
  select(datetime, waterTemperature_degC_0p1m) %>% 
  filter(!is.na(waterTemperature_degC_0p1m)) %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC'))

buoy_surface_hourly = buoy_surface %>% 
  mutate(date = as.Date(datetime), 
         hour = format(datetime, '%H')) %>% 
  group_by(date, hour) %>% 
  summarize(waterTemperature_mean_degC_0p1m = mean(waterTemperature_degC_0p1m)) %>% 
  mutate(datetime = as.POSIXct(paste0(date, ' ', hour, ':00'), tz = 'UTC'))

mindate = as.Date(min(buoy_surface$datetime))
maxdate = as.Date(max(buoy_surface$datetime))

buoymet_match_hourly <- buoymet_2021 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC'),
         date = as.Date(datetime), 
         hour = format(datetime, '%H')) %>% 
  filter(datetime >= mindate & datetime <= maxdate) %>% 
  select(datetime, airTemperature_degC, date, hour) %>% 
  group_by(date, hour) %>% 
  filter(!is.na(airTemperature_degC)) %>% 
  summarize(airTemperature_mean_degC = mean(airTemperature_degC)) %>% 
  mutate(datetime = as.POSIXct(paste0(date, ' ', hour, ':00'), tz = 'UTC'))

buoy_alltemp <- full_join(buoy_surface_hourly, buoymet_match_hourly) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value', 
               -c(datetime, date, hour)) %>% 
  mutate(variable = case_when(variable == 'airTemperature_mean_degC' ~ 'air temperature',
                              variable == 'waterTemperature_mean_degC_0p1m' ~ 'near surface water temperature'))

ggplot(buoy_alltemp, aes(x = datetime, y = value, color = variable)) +
  geom_line() +
  geom_smooth(se = F, lty = 2) +
  # facet_grid(variable ~ .) +
  final_theme +
  labs(x = NULL, 
       y = 'temperature',
       title = '2021 LSPA/GLEON Buoy Data') +
  theme(legend.position = 'bottom') +
    scale_color_manual(values = c('grey', 'dark blue'), name = ' ') +
  scale_y_continuous(breaks = c(10, 15, 20, 25, 30), 
                     labels = c('10°C | 50°F', '15°C | 59°F', '20°C | 68°F', '25°C | 77°F','30°C | 86°F'),
                     limits = c(10, 30)) +
  theme(axis.text.y = element_text(angle = 30))

  ggsave(file.path(fig_dir, 'buoy_airwater_temp_2021.png'),
       width = 10,
       height = 4,
       dpi = 300,
       units = 'in')

### number of days with temp >30degC
# get max temp from newport weather station
newport_92_20 <-  ghcnd_search(stationid = 'USC00275868',
                               date_min = '1992-01-01',
                               date_max = '2022-01-01')
newport_max <- newport_92_20$tmax %>% 
  mutate(temp_max_degC = tmax/10) %>% 
  filter(!is.na(temp_max_degC))

newport_max <-newport_max %>% 
  mutate(date = as.Date(date),
         year = as.numeric(format(date, '%Y')),
         month = as.numeric(format(date, '%m'))) 

ggplot(newport_max, aes(x = date, y = temp_max_degC)) +
  geom_point()

newport_gt30 <- newport_max %>% 
  filter(temp_max_degC >=30) %>% 
  group_by(year) %>% 
  summarize(n_gt30 = n())

year = seq(from = 1992, to =2021, by = 1)

yearlist <- data.frame(year)

newport_gt30 <- full_join(yearlist, newport_gt30) %>% 
  mutate(n_gt30 = case_when(is.na(n_gt30) ~ 0, 
                            TRUE ~ as.numeric(n_gt30)))

ggplot(newport_gt30, aes(x = year,y = as.numeric(n_gt30))) +
  geom_bar(stat = 'identity') +
  labs(x = NULL,
       y = 'number of days with\nmaximum air temperature \u2265 30°C | 86°F') +
  final_theme +
  theme(legend.position = 'none')

ggsave(file.path(fig_dir, 'summer_temp_gt30.png'),
       width = 8,
       height = 4,
       dpi = 300,
       units = 'in')

