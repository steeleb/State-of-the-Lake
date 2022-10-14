library(tidyverse)
library(clifro)

final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom


sunLMP <- read.delim('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/LSPALMP_1986-2020_v2021-03-29.csv', sep = ',') %>% 
  mutate(date = as.Date(date))

sunbuoy_wind <- read.csv('C:/Users/steeleb/Dropbox/EDI_submissions/sunapee_buoy_weather_2007_2020_v03March2021/data/2007-2020_wind_L1.csv')

lake <- sunLMP %>% 
  filter(station == 200 | station == 210 | station == 220 | station == 230)

chla <- lake %>% 
  filter(parameter == 'chla_ugl')

summerchla <- chla %>% 
  mutate(month = as.numeric(format(as.Date(date), '%m')),
         year = as.numeric(format(as.Date(date), '%Y'))) %>% 
  filter(month > 5 & month < 10 & year >1990)

ggplot(summerchla, aes(x = year, y = value)) + 
  geom_boxplot(aes(group = year)) +
  labs(x = NULL, y = 'chlorophyll-a (ug/L)',
       title = 'summer epilimnion chlorophyll-a concentrations at deep sites\nin Lake Sunapee') +
  final_theme


# tp <- lake %>% 
#   filter(parameter == 'TP_mgl')
# 
# summertp_epi <- tp %>% 
#   mutate(month = as.numeric(format(as.Date(date), '%m')),
#          year = as.numeric(format(as.Date(date), '%Y'))) %>% 
#   filter(month > 5 & month < 10 & year >1990 & layer == 'E')
# summertp_hypo <- tp %>% 
#   mutate(month = as.numeric(format(as.Date(date), '%m')),
#          year = as.numeric(format(as.Date(date), '%Y'))) %>% 
#   filter(month > 5 & month < 10 & year >1990 & layer == 'H')
# 
# ggplot(summertp_epi, aes(x = year, y = value, color = station)) + 
#   geom_boxplot(aes(group = year)) +
#   labs(x = NULL, y = 'total phosphorus (mg/L)',
#        title = 'epilimnion total phosphorus concentrations at deep sites\nin Lake Sunapee') +
#   final_theme
# ggplot(summertp_hypo, aes(x = year, y = value, color = station)) + 
#   geom_boxplot(aes(group = year)) +
#   labs(x = NULL, y = 'total phosphorus (mg/L)',
#        title = 'hypolimnion total phosphorus concentrations at deep sites\nin Lake Sunapee') +
#   final_theme


lake_surf_temp <- lake %>% 
  filter(parameter == 'temp_C') %>% 
  filter(depth_m < 1) %>% 
  mutate(month = as.numeric(format(as.Date(date), '%m')),
         year = as.numeric(format(as.Date(date), '%Y')),
         station = as.character(station))  %>% 
  group_by(station, month, year) %>% 
  summarise(mean_surftemp_C = mean(value)) %>% 
  filter(month >5 & month <10 & year > 1990 & year < 2020) %>% 
  mutate(month = factor(month, levels = c(6,7,8,9), labels = c('Jun', 'Jul', 'Aug', 'Sept')))
  
ggplot(lake_surf_temp, aes(x = year, y = mean_surftemp_C, color = station, fill = station)) + 
  geom_line() +
  geom_point() +
  facet_grid(month ~ .) +
  labs(x = NULL, y = 'water temperature (deg C)',
       title = 'surface temperature at deep sites\nin Lake Sunapee') +
  scale_x_continuous(limits = c(1990,2020), breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  final_theme

lake_deep_temp <- lake %>% 
  filter(parameter == 'temp_C' & depth_m>10) %>% 
  mutate(value = case_when(station == 200 & date == 	
                             '2009-08-12' & depth_m == 18 ~ NA_real_,
                           station == 210 & date == '2009-09-01' & depth_m == 30 ~ NA_real_,
                           TRUE ~ value)) %>% 
  filter(!is.na(value)) %>% 
  arrange(-depth_m) %>% 
  mutate(month = as.numeric(format(as.Date(date), '%m')),
         year = as.numeric(format(as.Date(date), '%Y')),
         station = as.character(station))  %>% 
  group_by(station, month, year) %>% 
  mutate(deep_surftemp_C = first(value)) %>% 
  filter(month >5 & month <10 & year > 2000 & year < 2020) %>% 
  mutate(month = factor(month, levels = c(6,7,8,9), labels = c('Jun', 'Jul', 'Aug', 'Sept')))

ggplot(lake_deep_temp, aes(x = year, y = deep_surftemp_C, color = station, fill = station)) + 
  geom_line() +
  geom_point() +
  facet_grid(month ~ .) +
  labs(x = NULL, y = 'water temperature (deg C)',
       title = 'bottom temperature at deep sites\nin Lake Sunapee') +
  scale_x_continuous(limits = c(2000,2020), breaks = c(2000, 2005, 2010, 2015, 2020)) +
  final_theme

#### wind data ####
wind_2019 <- sunbuoy_wind %>% 
  mutate(datetime = as.POSIXct(datetime, tz='UTC')) %>% 
  filter(datetime >= '2019-01-01'& datetime <'2020-01-01') %>% 
  mutate(date = as.Date(datetime)) %>% 
  mutate(month = factor(format(date, '%b'), levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')))
wind_2019

ggplot(wind_2019, aes (x = datetime, y = AveWindDir_deg)) +
  geom_point()
ggplot(wind_2019, aes (x = datetime, y = AveWindSp_ms)) +
  geom_point()

windRose(wind_2019, ws = 'AveWindSp_ms', wd = 'AveWindDir_deg', paddle = F, type = 'month', annotate=F, offset = 5)
