DO_temp <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/DO.xlsx')


Epi220_temp <- DO_temp %>% 
  filter(DEPTH <= 4) %>% 
  filter(STATION == 220) %>% 
  select(DATE, DEPTH, TEMP) %>% 
  mutate(year = as.numeric(format(DATE, '%Y')),
         month = as.numeric(format(DATE, '%m'))) %>% 
  filter(month >= 6 & month <9) %>% 
  group_by(DATE) %>% 
  summarize(ave_epi_temp = mean(TEMP))

ggplot(Epi220_temp, aes(x=DATE, y=ave_epi_temp, color = ave_epi_temp)) +
  geom_point() +
  final_theme +
  scale_color_continuous()


Epi220_temp_annual <- DO_temp %>% 
  filter(DEPTH <= 4) %>% 
  filter(STATION == 220) %>% 
  select(DATE, DEPTH, TEMP) %>% 
  mutate(year = as.numeric(format(DATE, '%Y')),
         month = as.numeric(format(DATE, '%m'))) %>% 
  filter(month >= 6 & month <9) %>% 
  group_by(year) %>% 
  summarize(ave_epi_temp = mean(TEMP))

ggplot(Epi220_temp_annual, aes(x=year, y=ave_epi_temp, color = ave_epi_temp)) +
  geom_point() +
  final_theme +
  scale_color_continuous()


#temp and tp
epi220_tp <- lmp_tp %>% 
  filter(LAYER == 'E') %>% 
  filter(STATION == 220)

epi220_tp_temp <- full_join(epi220_tp, Epi220_temp)

ggplot(epi220_tp_temp, aes(x=DATE, y=ave_epi_temp, color = ave_epi_temp)) +
  geom_point() +
  geom_point(aes(x=DATE, y = TP * 1000, size=4))+
  final_theme +
  scale_color_continuous()


epi220_tp_annual <- lmp_tp %>% 
  filter(LAYER == 'E') %>% 
  filter(STATION == 220) %>% 
  mutate(year = as.numeric(format(DATE, '%Y')),
         month = as.numeric(format(DATE, '%m'))) %>% 
  filter(month >= 6 & month <9) %>% 
  group_by(year) %>% 
  summarize(ave_tp = mean(TP))

epi220_tp_temp_ann <- full_join(epi220_tp_annual, Epi220_temp_annual) %>% 
  mutate(ave_tp = ave_tp*1000) %>% 
  mutate(ave_epi_temp = ave_epi_temp * (9/5) + 32) %>% 
  gather(variable, value, -year) %>% 
  mutate(variable = case_when(variable == 'ave_epi_temp' ~ 'average temp deg F',
                              variable == 'ave_tp' ~ 'average TP ug/L',
                              TRUE ~ ''))

ggplot(epi220_tp_temp_ann, aes(x=year, y=value)) +
  geom_point() +
  facet_grid(variable ~. , scales = 'free_y') +
  labs(x=NULL,
       y=NULL,
       title = 'temperature and total phosphorus at site 220')+
  final_theme 


