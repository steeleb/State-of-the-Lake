lmp_tp <- read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/CHEMISTRY.xlsx') %>% 
  select(STATION, DATE, Depth, LAYER, TP) %>% 
  filter(TP >=0)

lmp_tp %>% 
  filter(STATION == 10 | STATION == 20 | STATION == 30 | STATION == 60 | STATION ==70 | STATION == 90 | STATION == 110) %>% 
  filter(DATE >= '1996-01-01') %>% 
  ggplot(., aes(x=DATE, y = TP *1000)) +
  geom_point() +
  facet_grid(STATION~.) +
  labs(title = 'Total Phosphorus at Littoral Locations',
       x = NULL,
       y = 'total phosphorus (ug/L)') +
  final_theme
  

tp_210 <- lmp_tp %>% 
  filter(STATION == 210) %>% 
  filter(LAYER == 'E') %>% 
  filter(DATE >= '1996-01-01') %>% 
  filter(TP !=-99.00)

ggplot(subset(lmp_tp, subset = (STATION == 210 & LAYER == 'E' & TP != -99.00)), 
       aes(x=DATE, y =TP*1000)) +
  geom_point() +
  labs(x=NULL,
       y='total phosphorus (ug/L)',
       title = 'Total Phosphorus at Sunapee Deep Spot') +
  final_theme

tp_DEEP_E_yrave <- lmp_tp %>% 
  filter(STATION == 200 | STATION == 210 | STATION == 220 | STATION == 230) %>% 
  filter(LAYER == 'E') %>% 
  filter(DATE >= '1996-01-01') %>%
  mutate(year = format(DATE, '%Y')) %>% 
  group_by(year) %>% 
  summarize(ave_yr_TP = mean(TP))

ggplot(tp_DEEP_E_yrave,
       aes(x=as.numeric(year), y =ave_yr_TP*1000, color = ave_yr_TP*1000)) +
  geom_point(size = 3) +
  labs(x=NULL,
       y='mean annual epilimnion total phosphorus (ug/L)',
       title = 'Total Phosphorus at Deep Locations') +
  coord_cartesian(ylim = c(0, 50)) +
  scale_color_continuous(limits = c(0, 50), low = '#BFECFF', high = '#7BAB82') +
  final_theme + 
  theme (legend.position = 'none')

tp_DEEP_H_yrave <- lmp_tp %>% 
  filter(STATION == 200 | STATION == 210 | STATION == 220 | STATION == 230) %>% 
  filter(LAYER == 'H') %>% 
  filter(DATE >= '1996-01-01') %>%
  mutate(year = format(DATE, '%Y')) %>% 
  group_by(year) %>% 
  summarize(ave_yr_TP = mean(TP))

ggplot(tp_DEEP_H_yrave,
       aes(x=as.numeric(year), y =ave_yr_TP*1000, color = ave_yr_TP*1000)) +
  geom_point(size = 3) +
  labs(x=NULL,
       y='mean annual hypolimnion total phosphorus (ug/L)',
       title = 'Total Phosphorus at Deep Locations') +
  coord_cartesian(ylim = c(0, 50)) +
  scale_color_continuous(limits = c(0, 50), low = '#BFECFF', high = '#7BAB82') +
  final_theme + 
  theme (legend.position = 'none')

tp_220 <- lmp_tp %>% 
  filter(STATION == 220) %>% 
  filter(LAYER == 'E')%>% 
  filter(DATE >= '1996-01-01')%>% 
  filter(TP !=-99.00) 

tp_110 <- lmp_tp %>% 
  filter(STATION == 110)%>% 
  filter(DATE >= '1996-01-01')%>% 
  filter(TP !=-99.00)

tp_10 <- lmp_tp %>% 
  filter(STATION == 10) %>% 
  filter(DATE >= '1996-01-01')%>% 
  filter(TP !=-99.00)

tp_20 <- lmp_tp %>% 
  filter(STATION == 20) %>% 
  filter(DATE >= '1996-01-01')%>% 
  filter(TP !=-99.00)

tp_30 <- lmp_tp %>% 
  filter(STATION == 30) %>% 
  filter(DATE >= '1996-01-01')%>% 
  filter(TP !=-99.00)

tp_60 <- lmp_tp %>% 
  filter(STATION == 60) %>% 
  filter(DATE >= '1996-01-01')%>% 
  filter(TP !=-99.00)

tp_90 <- lmp_tp %>% 
  filter(STATION == 90) %>% 
  filter(DATE >= '1996-01-01')%>% 
  filter(TP !=-99.00)

tp_110 <- lmp_tp %>% 
  filter(STATION == 110) %>% 
  filter(DATE >= '1996-01-01')%>% 
  filter(TP !=-99.00)

tp_70 <- lmp_tp %>% 
  filter(STATION == 70) %>% 
  filter(DATE >= '1996-01-01')%>% 
  filter(TP !=-99.00)

tp_200 <- lmp_tp %>% 
  filter(STATION == 200) %>% 
  filter(LAYER == 'E') %>% 
  filter(DATE >= '1996-01-01') %>% 
  filter(TP != -99.00)
 
tp_230 <- lmp_tp %>% 
  filter(STATION == 230) %>% 
  filter(LAYER == 'E') %>% 
  filter(DATE >= '1996-01-01') %>% 
  filter(TP != -99.00)


tp_selection <- full_join(tp_210, tp_220) %>% 
  full_join(., tp_110) %>% 
  full_join(., tp_10) %>% 
  full_join(., tp_20) %>% 
  full_join(., tp_30) %>% 
  full_join(., tp_60) %>% 
  full_join(., tp_90) %>% 
  full_join(., tp_110) %>% 
  full_join(., tp_70) %>% 
  full_join(., tp_200) %>% 
  full_join(., tp_230) %>% 
  group_by(STATION) %>% 
  summarize(TP = mean(TP))

