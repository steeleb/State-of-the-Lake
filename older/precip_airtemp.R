precip_temp <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/weather/NCDC_newport_entirerecord_052018.xlsx', 
          sheet = 'NCDC_newport_entirerecord',
          skip=2,
          col_names = c('station', 'station_name', 'state', 'country', 'date', 
                        'mdpr', 'mdpr_flag', 'mdsf', 'mdsf_flag', 'dapr', 
                        'dapr_flag', 'precip_mm', 'precip_flag', 'precip_qualflag', 'time', 
                        'snwd', 'snwd_flag', 'snow_mm', 'snow_flag','time2', 
                        'temp_max', 'tempmax_flag', 'temp_min', 'tempmin_flag', 'temp_obs', 
                        'tempobs_flag'),
          col_types = c('text', 'text', 'text', 'text','text', 
                        'numeric', 'text', 'numeric', 'text', 'numeric', 
                        'text', 'numeric', 'text', 'text', 'text',
                        'numeric', 'text', 'numeric', 'text', 'text',
                        'numeric', 'text', 'numeric', 'text', 'numeric',
                        'text'),
          na = c('-9999', '9999')) %>% 
  mutate(date = as.Date(date, '%Y%m%d'))


temp <- precip_temp %>% 
  select(date, temp_min, temp_max) %>% 
  gather(min_max, temp_degC, -date) %>% 
  mutate(min_max = case_when(min_max == 'temp_max' ~ 'maximum daily temperature',
                             min_max == 'temp_min'~ 'minimum daily temperature',
                             TRUE ~ '')) %>% 
  filter(date >= '1991-01-01' & date < '2018-01-01')

ggplot(temp, aes(x = date, y= temp_degC, color =min_max, group = min_max)) +
  geom_point() +
  geom_smooth(method = 'lm', se=F, color = 'black', size = 0.5, linetype = 2) +
  labs(title = 'Newport historical daily air temperature',
       x = NULL,
       y = 'air temperature (degrees C)') +
  scale_x_date(date_minor_breaks = '1 year') +
  final_theme +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank())




precip <- precip_temp %>% 
  select(date, precip_mm) %>% 
  mutate(month = as.numeric(format(date, '%m')),
         year = as.numeric (format(date, '%Y')),
         season = case_when(month == 1 ~ 0,
                            month == 2 ~ 0,
                            month == 3 ~ 0.25, 
                            month == 4 ~ 0.25,
                            month == 5 ~ 0.25,
                            month == 6 ~ 0.5,
                            month == 7 ~ 0.5,
                            month == 8 ~ 0.5,
                            month == 9 ~ 0.75,
                            month == 10 ~ 0.75,
                            month == 11 ~ 0.75,
                            month == 12 ~ 1))

precip_monthly <- precip %>% 
  group_by(year, month) %>% 
  summarise(total_rainfall_cm = sum(precip_mm, na.rm = T)/10,
           n_rain = length(!is.na(precip_mm))) %>% 
  ungroup() %>% 
  mutate(mo_yr = as.Date(paste(year, month, '01', sep = '-'), '%Y-%m-%d'),
         total_rainfall_cm = case_when(n_rain <0.75*30 ~ NA_real_,
                                       TRUE ~ total_rainfall_cm))

ggplot(precip_monthly, aes(x = mo_yr, y= total_rainfall_cm)) +
  geom_bar(stat='identity') +
  geom_smooth(method = 'lm', se=F, color = 'black', size = 0.5, linetype = 2) +
  labs(title = 'Newport monthly precipitation ',
       x = NULL,
       y = 'total monthly precipitation (cm/month)') +
  final_theme +
  scale_x_date(limits = c(as.Date('1920-01-01'), as.Date('2020-01-01')), 
               breaks = c(as.Date('1920-01-01'), as.Date('1930-01-01'), as.Date('1940-01-01'), as.Date('1950-01-01'), as.Date('1960-01-01'),
                          as.Date('1970-01-01'), as.Date('1980-01-01'),  as.Date('1990-01-01'),as.Date('2000-01-01'), as.Date('2010-01-01'),as.Date('2020-01-01')),
               labels = c('1920', '1930', '1940', '1950', '1960', '1970', '1980', '1990', '2000', '2010', '2020')) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank())

ggplot(subset(precip_monthly, 
              subset=(mo_yr>=as.Date('1990-01-01') & mo_yr < as.Date('2018-01-01'))),
       aes(x = mo_yr, y= total_rainfall_cm)) +
  geom_bar(stat='identity') +
  geom_smooth(method = 'lm', se=F, color = 'black', size = 0.5, linetype = 2) +
  labs(title = 'Newport monthly precipitation 1990-2017 ',
       x = NULL,
       y = 'total monthly precipitation (cm/month)') +
  final_theme +
  scale_x_date(limits = c(as.Date('1990-01-01'), as.Date('2020-01-01')), 
               breaks = c(as.Date('1990-01-01'),as.Date('1995-01-01'),as.Date('2000-01-01'),as.Date('2005-01-01'), as.Date('2010-01-01'),as.Date('2015-01-01'),as.Date('2020-01-01')),
               labels = c('1990', '1995', '2000', '2005', '2010', '2015', '2020')) +
    theme(legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid.minor = element_blank())

precip_seasonal <- precip %>% 
  mutate(year_season = year + season) %>% 
  group_by(year_season) %>% 
  summarise(total_rainfall_cm = sum(precip_mm, na.rm = T)/10,
            n_rain = length(!is.na(precip_mm))) %>% 
  ungroup() %>% 
  mutate(total_rainfall_cm = case_when(n_rain <0.75*90 ~ NA_real_,
                                       TRUE ~ total_rainfall_cm))

ggplot(precip_seasonal, aes(x = year_season, y= total_rainfall_cm)) +
  geom_bar(stat='identity') +
  geom_smooth(method = 'lm', se=F, color = 'black', size = 0.5, linetype = 2) +
  labs(title = 'Newport seasonal precipitation ',
       x = NULL,
       y = 'total seasonal precipitation (cm/month)') +
  final_theme +
  # scale_x_date(limits = c(as.Date('1920-01-01'), as.Date('2020-01-01')), 
  #              breaks = c(as.Date('1920-01-01'), as.Date('1930-01-01'), as.Date('1940-01-01'), as.Date('1950-01-01'), as.Date('1960-01-01'),
  #                         as.Date('1970-01-01'), as.Date('1980-01-01'),  as.Date('1990-01-01'),as.Date('2000-01-01'), as.Date('2010-01-01'),as.Date('2020-01-01')),
  #              labels = c('1920', '1930', '1940', '1950', '1960', '1970', '1980', '1990', '2000', '2010', '2020')) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank())

ggplot(subset(precip_seasonal, 
              subset=(year_season>=1990 & year_season<2018)),
       aes(x = year_season, y= total_rainfall_cm)) +
  geom_bar(stat='identity') +
  geom_smooth(method = 'lm', se=F, color = 'black', size = 0.5, linetype = 2) +
  labs(title = 'Newport seasonal precipitation ',
       x = NULL,
       y = 'total seasonal precipitation (cm/month)') +
  final_theme +
  # scale_x_date(limits = c(as.Date('1990-01-01'), as.Date('2020-01-01')), 
  #              breaks = c(as.Date('1990-01-01'),as.Date('1995-01-01'),as.Date('2000-01-01'),as.Date('2005-01-01'), as.Date('2010-01-01'),as.Date('2015-01-01'),as.Date('2020-01-01')),
  #              labels = c('1990', '1995', '2000', '2005', '2010', '2015', '2020')) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid.minor = element_blank())

precip_seasonal_mean <- precip %>% 
  mutate(year_season = year + season) %>% 
  group_by(year_season) %>% 
  summarise(total_rainfall_cm = sum(precip_mm, na.rm = T)/10,
            n_rain = length(!is.na(precip_mm))) %>% 
  ungroup() %>% 
  mutate(total_rainfall_cm = case_when(n_rain <0.75*90 ~ NA_real_,
                                       TRUE ~ total_rainfall_cm),
         year = as.integer(year_season),
         season = year_season - year) %>% 
  mutate(season = case_when(season == 0 ~ 'Winter',
                            season == 0.25 ~ 'Spring',
                            season == 0.5 ~ 'Summer', 
                            season == 0.75 ~ 'Fall')) %>% 
  mutate(decade = case_when(year >= 1930 & year < 1940 ~ '1930s',
                            year >= 1940 & year < 1950 ~ '1940s',
                            year >= 1950 & year < 1960 ~ '1950s',
                            year >= 1960 & year < 1970 ~ '1960s',
                            year >= 1970 & year < 1980 ~ '1970s',
                            year >= 1980 & year < 1990 ~ '1980s',
                            year >= 1990 & year < 2000 ~ '1990s',
                            year >= 2000 & year < 2010 ~ '2000s',
                            year >= 2010 & year < 2020 ~ '2010s',
                            TRUE ~ '')) %>% 
  filter(decade != '') %>% 
  group_by(season, decade) %>% 
  summarise(ave_rainfall_cm = mean(total_rainfall_cm, na.rm = T),
            n_rain = length(!is.na(total_rainfall_cm))) %>% 
  ungroup() %>% 
  mutate(season = factor(season, levels = c('Spring', 'Summer', 'Fall', 'Winter')))

ggplot(precip_seasonal_mean, aes(x = decade, y= ave_rainfall_cm/2.54, fill = ave_rainfall_cm)) +
  geom_bar(stat='identity') +
  facet_grid(season ~ .) +
  labs(title = 'Newport mean seasonal precipitation by decade',
       x = NULL,
       y = 'mean seasonal precipitation (inches/season)') +
  final_theme +
  scale_fill_continuous(low = '#56B1F7', high = '#132B43') +
  theme(legend.position = 'none')

####annual precip####
precip_annual <- precip %>% 
  group_by(year) %>% 
  summarise(total_annual_cm = sum(precip_mm, na.rm = T)/10,
            n_rain = length(!is.na(precip_mm))) %>% 
  ungroup() %>% 
  mutate(total_annual_cm = case_when(n_rain <0.80*365 ~ NA_real_,
                                       TRUE ~ total_annual_cm))

ggplot(precip_annual, aes(x = year, y= total_annual_cm)) +
  geom_bar(stat='identity') +
  geom_smooth(method = 'lm', se=F, color = 'black', size = 0.5, linetype = 2) +
  labs(title = 'Newport annual precipitation ',
       x = NULL,
       y = 'total annual precipitation (cm/year)') +
  scale_x_continuous(limits = c(1920, 2020),
                     breaks = c(1920,1930, 1940, 1950,1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  final_theme +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank())

ggplot(subset(precip_annual,
              (year>=1990 & year < 2018)),
       aes(x = year, y= total_annual_cm)) +
  geom_bar(stat='identity') +
  geom_smooth(method = 'lm', se=F, color = 'black', size = 0.5, linetype = 2) +
  labs(title = 'Newport annual precipitation 1990-2017',
       x = NULL,
       y = 'total annual precipitation (cm/year)') +
  scale_x_continuous(limits = c(1990, 2020),
                     breaks = c(1990,1995, 2000, 2005,2010, 2015, 2020)) +
  final_theme +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid.minor = element_blank())

#### calculate number of extreme events per year - extreme event is > 2" per day ####
precip_events <- precip %>% 
  mutate(event = case_when(precip_mm >= 50.8 ~ 1,
                           TRUE ~ NA_real_)) %>% 
  group_by(year) %>% 
  summarise(n_days = length(!is.na(precip_mm)),
            n_events = sum(event, na.rm = T)) 

ggplot(subset(precip_events,
              subset=(year >= 1930 & year < 2018)),
       aes(x = year, y = n_events)) +
  geom_smooth(method = 'lm', se=F, color = 'blue') +
  geom_bar(stat = 'identity') +
  labs(title = 'number of extreme rainfall events per year',
       x = NULL,
       y = 'number of extreme rainfall events') +
  final_theme

ggplot(subset(precip_events,
              subset=(year >= 1975 & year < 2018)),
       aes(x = year, y = n_events)) +
  geom_smooth(method = 'lm', se=F, color = 'blue') +
  geom_bar(stat = 'identity') +
  labs(title = 'number of extreme rainfall events per year',
       x = NULL,
       y = 'number of extreme rainfall events') +
  final_theme


#### calculate number of extreme events per season - extremem event is > 2" per day ####
precip_events_season <- precip %>% 
  mutate(event = case_when(precip_mm >= 50.8 ~ 1,
                           TRUE ~ NA_real_)) %>% 
  mutate(year_season = year + season) %>% 
  group_by(year_season) %>% 
  summarise(n_days = length(!is.na(precip_mm)),
            n_events = sum(event, na.rm = T)) 

ggplot(subset(precip_events_season,
              subset=(year_season >= 1930 & year_season < 2018)),
       aes(x = year_season, y = n_events)) +
  geom_smooth(method = 'lm', se=F, color = 'blue') +
  geom_bar(stat = 'identity') +
  labs(title = 'number of extreme rainfall events per season',
       x = NULL,
       y = 'number of extreme rainfall events') +
  final_theme


ggplot(subset(precip_events_season,
              subset=(year_season >= 1975 & year_season < 2018)),
       aes(x = year_season, y = n_events)) +
  geom_smooth(method = 'lm', se=F, color = 'blue') +
  geom_bar(stat = 'identity') +
  labs(title = 'number of extreme rainfall events per season',
       x = NULL,
       y = 'number of extreme rainfall events') +
  final_theme

####proportion of total rainfall from extreme events -- annual ####
precip_events_annual_pro <- precip %>% 
  mutate(event = case_when(precip_mm >= 50.8 ~ 1,
                           TRUE ~ NA_real_)) %>% 
  group_by(year) %>% 
  summarise(n_days = length(!is.na(precip_mm)),
            n_events = sum(event, na.rm = T)) 

precip_annual_events <- precip %>% 
  mutate(event = case_when(precip_mm >= 50.8 ~ 1,
                           TRUE ~ NA_real_)) %>% 
  filter(event == 1) %>% 
  group_by(year) %>% 
  summarise(event_precip_mm = sum(precip_mm, na.rm = T)) 

precip_annual <- precip %>% 
  mutate(event = case_when(precip_mm >= 50.8 ~ 1,
                           TRUE ~ NA_real_)) %>% 
  filter(is.na(event)) %>% 
  group_by(year) %>% 
  summarise(season_precip_mm = sum(precip_mm, na.rm = T)) 

precip_events_annual_pro <- full_join(precip_events_annual_pro, precip_annual_events) %>% 
  full_join(., precip_annual) %>% 
  mutate(pro_precip_events = event_precip_mm / (season_precip_mm + event_precip_mm))

ggplot(subset(precip_events_annual_pro,
              subset=(year >= 1930 & year < 2018)),
       aes(x = year, y = pro_precip_events)) +
  geom_smooth(method = 'lm', se=F, color = 'blue') +
  geom_bar(stat = 'identity') +
  labs(title = 'proportion of total annual precipitation from extreme events',
       x = NULL,
       y = 'proportion of total annual precipitation as extreme events') +
  final_theme

####proportion of total rainfall from extreme events -- annual by decade ####
precip_events_annual_pro_decade <- precip %>% 
  mutate(event = case_when(precip_mm >= 50.8 ~ 1,
                           TRUE ~ NA_real_)) %>% 
  group_by(year) %>% 
  summarise(n_days = length(!is.na(precip_mm)),
            n_events = sum(event, na.rm = T))  %>% 
  mutate(decade = case_when(year >= 1930 & year < 1940 ~ '1930s',
                            year >= 1940 & year < 1950 ~ '1940s',
                            year >= 1950 & year < 1960 ~ '1950s',
                            year >= 1960 & year < 1970 ~ '1960s',
                            year >= 1970 & year < 1980 ~ '1970s',
                            year >= 1980 & year < 1990 ~ '1980s',
                            year >= 1990 & year < 2000 ~ '1990s',
                            year >= 2000 & year < 2010 ~ '2000s',
                            year >= 2010 & year < 2020 ~ '2010s',
                            TRUE ~ '')) %>% 
  filter(decade != '') %>% 
  group_by(decade) %>% 
  summarise(ave_events = mean(n_events, na.rm = T)) %>% 
  ungroup() 


precip_annual_events_decade <- precip %>% 
  mutate(event = case_when(precip_mm >= 50.8 ~ 1,
                           TRUE ~ NA_real_)) %>% 
  filter(event == 1) %>% 
  group_by(year) %>% 
  summarise(event_precip_mm = sum(precip_mm, na.rm = T))  %>% 
  mutate(decade = case_when(year >= 1930 & year < 1940 ~ '1930s',
                            year >= 1940 & year < 1950 ~ '1940s',
                            year >= 1950 & year < 1960 ~ '1950s',
                            year >= 1960 & year < 1970 ~ '1960s',
                            year >= 1970 & year < 1980 ~ '1970s',
                            year >= 1980 & year < 1990 ~ '1980s',
                            year >= 1990 & year < 2000 ~ '1990s',
                            year >= 2000 & year < 2010 ~ '2000s',
                            year >= 2010 & year < 2020 ~ '2010s',
                            TRUE ~ '')) %>% 
  filter(decade != '') %>% 
  group_by(decade) %>% 
  summarise(ave_event_precip_cm = mean(event_precip_mm, na.rm = T)/10) %>% 
  ungroup() 


precip_annual_decade <- precip %>% 
  mutate(event = case_when(precip_mm >= 50.8 ~ 1,
                           TRUE ~ NA_real_)) %>% 
  filter(is.na(event)) %>% 
  group_by(year) %>% 
  summarise(annual_precip_mm = sum(precip_mm, na.rm = T))  %>% 
  mutate(decade = case_when(year >= 1930 & year < 1940 ~ '1930s',
                            year >= 1940 & year < 1950 ~ '1940s',
                            year >= 1950 & year < 1960 ~ '1950s',
                            year >= 1960 & year < 1970 ~ '1960s',
                            year >= 1970 & year < 1980 ~ '1970s',
                            year >= 1980 & year < 1990 ~ '1980s',
                            year >= 1990 & year < 2000 ~ '1990s',
                            year >= 2000 & year < 2010 ~ '2000s',
                            year >= 2010 & year < 2020 ~ '2010s',
                            TRUE ~ '')) %>% 
  filter(decade != '') %>% 
  group_by(decade) %>% 
  summarise(ave_annual_precip_cm = mean(annual_precip_mm, na.rm = T)/10) %>% 
  ungroup() 


precip_events_annual_pro_decade <- full_join(precip_events_annual_pro_decade, precip_annual_events_decade) %>% 
  full_join(., precip_annual_decade) %>% 
  mutate(percent_precip_events = (ave_event_precip_cm / (ave_annual_precip_cm + ave_event_precip_cm))*100)

ggplot(subset(precip_events_annual_pro_decade),
       aes(x = decade, y = percent_precip_events, fill = percent_precip_events)) +
  geom_smooth(method = 'lm', se=F, color = 'blue') +
  geom_bar(stat = 'identity') +
  labs(title = 'percent of total annual precipitation attributed to extreme events by decade',
       x = NULL,
       y = 'percent of total annual precipitation from extreme events') +
  final_theme +
  scale_fill_continuous(low = '#56B1F7', high = '#132B43') +
  theme(legend.position = 'none')


####proportion of total rainfall from extreme events by season####
precip_events_season_pro <- precip %>% 
  mutate(event = case_when(precip_mm >= 50.8 ~ 1,
                           TRUE ~ NA_real_)) %>% 
  mutate(year_season = year + season) %>% 
  group_by(year_season) %>% 
  summarise(n_days = length(!is.na(precip_mm)),
            n_events = sum(event, na.rm = T)) 

precip_events <- precip %>% 
  mutate(event = case_when(precip_mm >= 50.8 ~ 1,
                           TRUE ~ NA_real_)) %>% 
  filter(event == 1) %>% 
  mutate(year_season = year + season) %>% 
  group_by(year_season) %>% 
  summarise(event_precip_mm = sum(precip_mm, na.rm = T)) 

precip_seasonal <- precip %>% 
  mutate(event = case_when(precip_mm >= 50.8 ~ 1,
                           TRUE ~ NA_real_)) %>% 
  filter(is.na(event)) %>% 
  mutate(year_season = year + season) %>% 
  group_by(year_season) %>% 
  summarise(season_precip_mm = sum(precip_mm, na.rm = T)) 

precip_events_season_pro <- full_join(precip_events_season_pro, precip_events) %>% 
  full_join(., precip_seasonal) %>% 
  mutate(pro_precip_events = event_precip_mm / (season_precip_mm + event_precip_mm))

ggplot(subset(precip_events_season_pro,
              subset=(year_season >= 1930 & year_season < 2018)),
       aes(x = year_season, y = pro_precip_events)) +
  geom_smooth(method = 'lm', se=F, color = 'blue') +
  geom_bar(stat = 'identity') +
  labs(title = 'proportion of total seasonal precipitation from extreme events',
       x = NULL,
       y = 'proportion of total seasonal precipitation as extreme events') +
  final_theme


####proportion of seasonal events by decade####
precip_events_season_pro_decade <- precip %>% 
  mutate(event = case_when(precip_mm >= 50.8 ~ 1,
                           TRUE ~ NA_real_)) %>% 
  mutate(year_season = year + season) %>% 
  group_by(year_season) %>% 
  summarise(n_days = length(!is.na(precip_mm)),
            n_events = sum(event, na.rm = T),
            total_precip_cm = sum(precip_mm, na.rm=T)/10) %>% 
  ungroup() %>% 
  mutate(year = as.integer(year_season),
       season = year_season - year) %>% 
  mutate(season = case_when(season == 0 ~ 'Winter',
                            season == 0.25 ~ 'Spring',
                            season == 0.5 ~ 'Summer', 
                            season == 0.75 ~ 'Fall')) %>% 
  mutate(decade = case_when(year >= 1930 & year < 1940 ~ '1930s',
                            year >= 1940 & year < 1950 ~ '1940s',
                            year >= 1950 & year < 1960 ~ '1950s',
                            year >= 1960 & year < 1970 ~ '1960s',
                            year >= 1970 & year < 1980 ~ '1970s',
                            year >= 1980 & year < 1990 ~ '1980s',
                            year >= 1990 & year < 2000 ~ '1990s',
                            year >= 2000 & year < 2010 ~ '2000s',
                            year >= 2010 & year < 2020 ~ '2010s',
                            TRUE ~ '')) %>% 
  filter(decade != '') %>% 
  group_by(season, decade) %>% 
  summarise(ave_precip_cm = mean(total_precip_cm, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(season = factor(season, levels = c('Spring', 'Summer', 'Fall', 'Winter')))

precip_events_season_decade <- precip %>% 
  mutate(event = case_when(precip_mm >= 50.8 ~ 1,
                           TRUE ~ NA_real_)) %>% 
  filter(event == 1) %>% 
  mutate(year_season = year + season) %>% 
  group_by(year_season) %>% 
  summarise(event_precip_cm = sum(precip_mm, na.rm = T)/10) %>% 
  ungroup() %>% 
  mutate(year = as.integer(year_season),
         season = year_season - year) %>% 
  mutate(season = case_when(season == 0 ~ 'Winter',
                            season == 0.25 ~ 'Spring',
                            season == 0.5 ~ 'Summer', 
                            season == 0.75 ~ 'Fall')) %>% 
  mutate(decade = case_when(year >= 1930 & year < 1940 ~ '1930s',
                            year >= 1940 & year < 1950 ~ '1940s',
                            year >= 1950 & year < 1960 ~ '1950s',
                            year >= 1960 & year < 1970 ~ '1960s',
                            year >= 1970 & year < 1980 ~ '1970s',
                            year >= 1980 & year < 1990 ~ '1980s',
                            year >= 1990 & year < 2000 ~ '1990s',
                            year >= 2000 & year < 2010 ~ '2000s',
                            year >= 2010 & year < 2020 ~ '2010s',
                            TRUE ~ '')) %>% 
  filter(decade != '') %>% 
  group_by(season, decade) %>% 
  summarise(ave_event_precip_cm = mean(event_precip_cm, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(season = factor(season, levels = c('Spring', 'Summer', 'Fall', 'Winter')))

precip_events_season_pro_decade <- full_join(precip_events_season_pro_decade, precip_events_season_decade) %>% 
  mutate(percent_precip_events = (ave_event_precip_cm / (ave_precip_cm))*100)

ggplot(subset(precip_events_season_pro_decade),
       aes(x = decade, y = percent_precip_events, fill = percent_precip_events)) +
  geom_smooth(method = 'lm', se=F, color = 'blue') +
  facet_grid(season ~ .) +
  geom_bar(stat = 'identity') +
  labs(title = 'percent of average seasonal precipitation attributed to extreme events by decade',
       x = NULL,
       y = 'percent of average seasonal precipitation from extreme events') +
  final_theme +
  scale_fill_continuous(low = '#56B1F7', high = '#132B43') +
  theme(legend.position = 'none')


