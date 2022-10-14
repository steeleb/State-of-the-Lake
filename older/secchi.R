LMP_secchi <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/BIOLOGY.xlsx', 
                        col_types = c('text', 'text', 'numeric', 'date', 'numeric', 
                                      'numeric', 'text', 'numeric', 'text', 'numeric', 
                                      'text', 'numeric', 'numeric', 'numeric', 'date', 
                                      'text', 'numeric', 'text')) %>% 
  select_all(., 'tolower') %>% 
  rename(chla_ugl = 'chl',
         sd_m = 'sd') %>% 
  mutate(date = as.Date(date),
         year = as.numeric(format(date, '%Y')),
         month = as.numeric(format(date, '%m'))) %>%
  select(station, date, year, month, sd_m) %>% 
  filter(!is.na(sd_m))

range(LMP_secchi$sd_m)

#recode -99 as NA
LMP_secchi <- LMP_secchi %>% 
  mutate(sd_m = case_when(sd_m == -99.0 ~ NA_real_,
                          sd_m == 0 ~ NA_real_,
                          TRUE ~ sd_m)) %>% 
  filter(!is.na(sd_m))

range(LMP_secchi$sd_m)

station <- c(10, 20, 30, 60, 90, 110, 70 ,80, 100.1, 200, 210, 220, 230)
site_depth <-  c(10.8, 4.3, 10.8, 4.7, 7.3, 8.3, 6.4, 4.5, 7.7, 19.5, 30.5, 25.6, 21.5)
lmp_loc_depth <- data.frame(station, site_depth)

LMP_secchi <- full_join(LMP_secchi, lmp_loc_depth) %>% 
  mutate(group = case_when(site_depth >20 ~ '> 20m',
                           site_depth <20 & site_depth>10 ~ '10-20m',
                           site_depth <10 & site_depth>5 ~ '5-10m',
                           TRUE ~ ''))

LMP_secchi_210 <- LMP_secchi %>% 
  filter(station == 210) %>% 
  mutate(group = '210 only')

LMP_secchi_group <- full_join(LMP_secchi, LMP_secchi_210) %>% 
  filter(group != '') %>% 
  mutate(group = factor(group, levels = c('210 only', '> 20m', '10-20m', '5-10m'))) %>% 
  filter(month >=6 & month < 10)
  
LMP_secchi_deep <- LMP_secchi_group %>% 
  filter(group == '> 20m')

LMP_secchi_group %>% 
  select(-station) %>% 
  group_by(year, group) %>% 
  summarise(n_sd = length(sd_m),
            mean_sd = mean(sd_m)) %>% 
  ggplot(., aes(x=year, y=mean_sd)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(.~group) +
  geom_linerange(aes(x = year, ymax=mean_sd, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'mean summer secchi depth',
       x = NULL, 
       y='secchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi

LMP_secchi_ann <- LMP_secchi %>% 
  filter(month>=6 &month <10) %>% 
  group_by(station, year) %>% 
  summarise(mean_secchi = mean(sd_m),
            med_secchi = median(sd_m))

ggplot(subset(LMP_secchi_ann,
              subset = (station == 10)), 
       aes(x=year, y=mean_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=mean_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 10',
       x = NULL, 
       y='mean summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi

LMP_secchi_deep_ann <- LMP_secchi_deep %>% 
  filter(month>=6 &month <10) %>% 
  group_by(year) %>% 
  summarise(mean_secchi = mean(sd_m),
            med_secchi = median(sd_m))

ggplot(LMP_secchi_deep_ann,
       aes(x=year, y=mean_secchi)) +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  geom_linerange(aes(x = year, ymax=mean_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'mean summer secchi depth at deep locations',
       x = NULL, 
       y='mean summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi


ggplot(subset(LMP_secchi_ann,
              subset = (station == 20)), 
       aes(x=year, y=mean_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=mean_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 20',
       x = NULL, 
       y='mean summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi

ggplot(subset(LMP_secchi_ann,
              subset = (station == 30)), 
       aes(x=year, y=mean_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=mean_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 30',
       x = NULL, 
       y='mean summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi

ggplot(subset(LMP_secchi_ann,
              subset = (station == 60)), 
       aes(x=year, y=mean_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=mean_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 60',
       x = NULL, 
       y='mean summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi

ggplot(subset(LMP_secchi_ann,
              subset = (station == 70)), 
       aes(x=year, y=mean_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=mean_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 70',
       x = NULL, 
       y='mean summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi

ggplot(subset(LMP_secchi_ann,
              subset = (station == 80)), 
       aes(x=year, y=mean_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=mean_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 80',
       x = NULL, 
       y='mean summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi

ggplot(subset(LMP_secchi_ann,
              subset = (station == 90)), 
       aes(x=year, y=mean_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=mean_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 90',
       x = NULL, 
       y='mean summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi

ggplot(subset(LMP_secchi_ann,
              subset = (station == 100 | station == 100.1)), 
       aes(x=year, y=mean_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=mean_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 100.1',
       x = NULL, 
       y='mean summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi

ggplot(subset(LMP_secchi_ann,
              subset = (station == 110)), 
       aes(x=year, y=mean_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=mean_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 110',
       x = NULL, 
       y='mean summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi


ggplot(subset(LMP_secchi_ann,
              subset = (station == 10)), 
       aes(x=year, y=med_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=med_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 10',
       x = NULL, 
       y='median summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi


ggplot(subset(LMP_secchi_ann,
              subset = (station == 20)), 
       aes(x=year, y=med_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=med_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 20',
       x = NULL, 
       y='median summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi

ggplot(subset(LMP_secchi_ann,
              subset = (station == 30)), 
       aes(x=year, y=med_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=med_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 30',
       x = NULL, 
       y='median summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi

ggplot(subset(LMP_secchi_ann,
              subset = (station == 60)), 
       aes(x=year, y=med_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=med_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 60',
       x = NULL, 
       y='median summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi

ggplot(subset(LMP_secchi_ann,
              subset = (station == 70)), 
       aes(x=year, y=med_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=med_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 70',
       x = NULL, 
       y='median summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi

ggplot(subset(LMP_secchi_ann,
              subset = (station == 80)), 
       aes(x=year, y=med_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=med_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 80',
       x = NULL, 
       y='median summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi

ggplot(subset(LMP_secchi_ann,
              subset = (station == 90)), 
       aes(x=year, y=med_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=med_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 90',
       x = NULL, 
       y='median summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi

ggplot(subset(LMP_secchi_ann,
              subset = (station == 100 | station == 100.1)), 
       aes(x=year, y=med_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=med_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 100.1',
       x = NULL, 
       y='median summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi

ggplot(subset(LMP_secchi_ann,
              subset = (station == 110)), 
       aes(x=year, y=med_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=med_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 110',
       x = NULL, 
       y='median summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi



ggplot(subset(LMP_secchi_ann,
              subset = (station == 200)), 
       aes(x=year, y=mean_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=med_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 200',
       x = NULL, 
       y='mean summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi


ggplot(subset(LMP_secchi_ann,
              subset = (station == 200)), 
       aes(x=year, y=med_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=med_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 200',
       x = NULL, 
       y='median summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi

ggplot(subset(LMP_secchi_ann,
              subset = (station == 210)), 
       aes(x=year, y=mean_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=med_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 210',
       x = NULL, 
       y='mean summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi


ggplot(subset(LMP_secchi_ann,
              subset = (station == 210)), 
       aes(x=year, y=med_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=med_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 210',
       x = NULL, 
       y='median summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi


ggplot(subset(LMP_secchi_ann,
             subset = (station == 220)), 
      aes(x=year, y=mean_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=med_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 220',
       x = NULL, 
       y='mean summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi


ggplot(subset(LMP_secchi_ann,
              subset = (station == 220)), 
       aes(x=year, y=med_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=med_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 220',
       x = NULL, 
       y='median summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi


ggplot(subset(LMP_secchi_ann,
             subset = (station == 230)), 
      aes(x=year, y=mean_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=med_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 230',
       x = NULL, 
       y='mean summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi


ggplot(subset(LMP_secchi_ann,
              subset = (station == 230)), 
       aes(x=year, y=med_secchi)) +
  geom_smooth(method = 'lm', se=F, color = 'dark gray') +
  geom_point(shape = 21, size = 5, fill = 'white', color = 'black') +
  geom_point(shape = 10, size = 5) +
  facet_grid(station ~ .) +
  geom_linerange(aes(x = year, ymax=med_secchi, ymin = 0), linetype = 6) +
  scale_y_reverse(limits = c(15, 0), breaks = c(15, 10, 5, 0), labels = c('15', '10', '5', 'surface')) +
  scale_x_continuous(breaks = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(title = 'secchi depth at station 230',
       x = NULL, 
       y='median summer\nsecchi depth (meters)') +
  scale_color_colorblind() +
  final_theme_secchi
