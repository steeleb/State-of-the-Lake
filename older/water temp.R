water_temp <-  read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/DO.xlsx', 
          sheet = 'DO') %>% 
  select_all(., 'tolower') %>% 
  select(station, date, depth, temp) %>% 
  rename(depth_m = 'depth',
         temp_degC = 'temp')

water_temp_h <- water_temp %>% 
  group_by(station, date, depth_m) %>% 
  summarise(temp_degC = mean(temp_degC)) %>% 
  group_by(station, date) %>% 
  spread(depth_m, temp_degC) %>% 
  filter(station !=999)
str(water_temp_h)
unique(water_temp_h$station)

# water_temp_100 <- water_temp_h %>% 
#   filter(station == 100) 
# only 2 observations, leave out



#### STATION 200 HEATMAPS ####
water_temp_200 <- water_temp_h %>% 
  ungroup() %>% 
  filter(station == 200) %>% 
  rename(wtr_0.1 = `0.1`,
         wtr_0.5 = `0.5`,
         wtr_1.0 = `1`,
         wtr_2.0 = `2`,
         wtr_3.0 = `3`,
         wtr_4.0 = `4`,
         wtr_5.0 = `5`,
         wtr_6.0 = `6`,
         wtr_7.0 = `7`,
         wtr_8.0 = `8`,
         wtr_9.0 = `9`,
         wtr_10.0 = `10`,
         wtr_11.0 = `11`,
         wtr_12.0 = `12`,
         wtr_13.0 = `13`,
         wtr_14.0 = `14`,
         wtr_16.0 = `16`,
         wtr_18.0 = `18`) %>% 
  select(date, wtr_0.1, wtr_0.5, wtr_1.0, wtr_2.0,
         wtr_3.0, wtr_4.0, wtr_0.5, wtr_6.0, wtr_7.0,
         wtr_8.0, wtr_9.0, wtr_10.0, wtr_11.0, wtr_12.0,
         wtr_13.0, wtr_14.0, wtr_16.0, wtr_18.0) %>% 
  filter(date != as.POSIXct('2011-08-17', tz='UTC')) %>% 
  filter(date != as.POSIXct('2011-09-19', tz='UTC')) %>% 
  filter(date != as.POSIXct('2004-08-24', tz='UTC')) %>% 
  filter(date != as.POSIXct('1998-09-27', tz='UTC')) %>% 
  filter(date != as.POSIXct('1993-10-17', tz='UTC')) %>% 
  filter(date != as.POSIXct('1993-05-30', tz='UTC'))

water_temp_200 %>%
  arrange(date) %>%
  filter(date>=as.POSIXct('2000-01-01', tz='UTC')) %>% 
  mutate(datetime = as.character(paste(date, '12:00', sep = ' '))) %>%
  select(-date) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_200.txt", delim='\t')

wtr.temp.200 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_200.txt")

wtr.heat.map(subset(wtr.temp.200,
                    subset=(datetime>=as.POSIXct('2000-01-01',tz='UTC') &
                              datetime < as.POSIXct('2001-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2000-05-15', tz='UTC'), as.POSIXct('2000-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2000'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.200,
                    subset=(datetime>=as.POSIXct('2001-01-01',tz='UTC') &
                              datetime < as.POSIXct('2002-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2001-05-15', tz='UTC'), as.POSIXct('2001-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2001'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.200,
                    subset=(datetime>=as.POSIXct('2002-01-01',tz='UTC') &
                              datetime < as.POSIXct('2003-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2002-05-15', tz='UTC'), as.POSIXct('2002-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2002'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.200,
                    subset=(datetime>=as.POSIXct('2003-01-01',tz='UTC') &
                              datetime < as.POSIXct('2004-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2003-05-15', tz='UTC'), as.POSIXct('2003-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2003'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.200,
                    subset=(datetime>=as.POSIXct('2004-01-01',tz='UTC') &
                              datetime < as.POSIXct('2005-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2004-05-15', tz='UTC'), as.POSIXct('2004-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2004'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.200,
                    subset=(datetime>=as.POSIXct('2005-01-01',tz='UTC') &
                              datetime < as.POSIXct('2006-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2005-05-15', tz='UTC'), as.POSIXct('2005-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2005'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.200,
                    subset=(datetime>=as.POSIXct('2006-01-01',tz='UTC') &
                              datetime < as.POSIXct('2007-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2006-05-15', tz='UTC'), as.POSIXct('2006-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2006'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.200,
                    subset=(datetime>=as.POSIXct('2007-01-01',tz='UTC') &
                              datetime < as.POSIXct('2008-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2007-05-15', tz='UTC'), as.POSIXct('2007-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2007'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.200,
                    subset=(datetime>=as.POSIXct('2008-01-01',tz='UTC') &
                              datetime < as.POSIXct('2009-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2008-05-15', tz='UTC'), as.POSIXct('2008-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2008'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.200,
                    subset=(datetime>=as.POSIXct('2009-01-01',tz='UTC') &
                              datetime < as.POSIXct('2010-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2009-05-15', tz='UTC'), as.POSIXct('2009-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2009'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.200,
                    subset=(datetime>=as.POSIXct('2010-01-01',tz='UTC') &
                              datetime < as.POSIXct('2011-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2010-05-15', tz='UTC'), as.POSIXct('2010-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2010'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.200,
                    subset=(datetime>=as.POSIXct('2011-01-01',tz='UTC') &
                              datetime < as.POSIXct('2012-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2011-05-15', tz='UTC'), as.POSIXct('2011-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2011'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.200,
                    subset=(datetime>=as.POSIXct('2012-01-01',tz='UTC') &
                              datetime < as.POSIXct('2013-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2012-05-15', tz='UTC'), as.POSIXct('2012-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2012'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.200,
                    subset=(datetime>=as.POSIXct('2013-01-01',tz='UTC') &
                              datetime < as.POSIXct('2014-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2013-05-15', tz='UTC'), as.POSIXct('2013-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2013'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.200,
                    subset=(datetime>=as.POSIXct('2014-01-01',tz='UTC') &
                              datetime < as.POSIXct('2015-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2014-05-15', tz='UTC'), as.POSIXct('2014-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2014'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.200,
                    subset=(datetime>=as.POSIXct('2015-01-01',tz='UTC') &
                              datetime < as.POSIXct('2016-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2015-05-15', tz='UTC'), as.POSIXct('2015-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2015'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.200,
                    subset=(datetime>=as.POSIXct('2016-01-01',tz='UTC') &
                              datetime < as.POSIXct('2017-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2016-05-15', tz='UTC'), as.POSIXct('2016-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2016'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.200,
                    subset=(datetime>=as.POSIXct('2017-01-01',tz='UTC') &
                              datetime < as.POSIXct('2018-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2017-05-15', tz='UTC'), as.POSIXct('2017-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2017'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

#### define epi and hypo over time ####
md_200 <- ts.meta.depths(wtr.temp.200,
               na.rm = T) %>% 
  mutate(date = as.POSIXct(format(datetime, '%Y-%m-%d'), tz='UTC')) %>% 
  select(-datetime)

water_temp_depth_200 <- water_temp %>% 
  filter(station == 200) %>% 
  full_join(., md_200) %>% 
  mutate(layer = case_when(depth_m < top ~ 'epi',
                           depth_m > top & depth_m < bottom ~ 'meta',
                           depth_m > bottom ~ 'hypo'))

#### MEAN SURFACE, BOTTOM JUN-SEPT SITE 200 ####
mean_hypo_200 <- water_temp_depth_200 %>%
  filter(layer == 'hypo') %>%
  mutate(year = as.numeric(format(date, '%Y')),
         month = as.numeric(format(date, '%m'))) %>% 
  filter(month >=6 & month<10) %>% 
  group_by(year) %>% 
  summarise(mean_temp_degC = mean(temp_degC, na.rm = T),
            n_temp = length(!is.na(temp_degC))) %>% 
  ungroup() %>% 
  filter(n_temp >= 0.75 * 30) %>% 
  mutate(layer = 'hypo',
         station = 200)

mean_epi_200 <- water_temp_depth_200 %>%
  filter(layer == 'epi') %>%
  mutate(year = as.numeric(format(date, '%Y')),
         month = as.numeric(format(date, '%m'))) %>% 
  filter(month >=6 & month<10) %>% 
  group_by(year) %>% 
  summarise(mean_temp_degC = mean(temp_degC, na.rm = T),
            n_temp = length(!is.na(temp_degC))) %>% 
  ungroup() %>% 
  filter(n_temp >= 0.75 * 30) %>% 
  mutate(layer = 'epi',
         station = 200)

mean_200 <- full_join(mean_epi_200, mean_hypo_200)

ggplot(mean_200, aes(x = year, y=mean_temp_degC)) +
  geom_smooth(method = 'lm', se=F, color = 'gray') +
  geom_point(size = 3) +
  facet_grid(layer ~ ., scales = 'free_y') +
  labs(title = 'mean summer temperature (Jun-Sept)',
       x=NULL,
       y='mean summer temperature') +
  final_theme



#### STATION 210 HEATMAPS ####
water_temp_210 <- water_temp_h %>% 
  ungroup() %>% 
  filter(station == 210) %>% 
  rename(wtr_0.1 = `0.1`,
         wtr_0.5 = `0.5`,
         wtr_1.0 = `1`,
         wtr_2.0 = `2`,
         wtr_3.0 = `3`,
         wtr_4.0 = `4`,
         wtr_5.0 = `5`,
         wtr_6.0 = `6`,
         wtr_7.0 = `7`,
         wtr_8.0 = `8`,
         wtr_9.0 = `9`,
         wtr_10.0 = `10`,
         wtr_11.0 = `11`,
         wtr_12.0 = `12`,
         wtr_13.0 = `13`,
         wtr_14.0 = `14`,
         wtr_16.0 = `16`,
         wtr_18.0 = `18`,
         wtr_20.0 = `20`,
         wtr_22.0 =`22`,
         wtr_25.0 = `25`) %>% 
  select(date, wtr_0.1, wtr_0.5, wtr_1.0, wtr_2.0,
         wtr_3.0, wtr_4.0, wtr_0.5, wtr_6.0, wtr_7.0,
         wtr_8.0, wtr_9.0, wtr_10.0, wtr_11.0, wtr_12.0,
         wtr_13.0, wtr_14.0, wtr_16.0, wtr_18.0, wtr_20.0, 
         wtr_22.0, wtr_25.0) %>% 
  filter(date != as.POSIXct('2011-08-17', tz='UTC')) %>% 
  filter(date != as.POSIXct('2011-09-19', tz='UTC')) %>% 
  filter(date != as.POSIXct('1998-09-27', tz='UTC')) 

water_temp_210 %>%
  arrange(date) %>%
  filter(date>=as.POSIXct('2000-01-01', tz='UTC')) %>% 
  mutate(datetime = as.character(paste(date, '12:00', sep = ' '))) %>%
  select(-date) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_210.txt", delim='\t')

wtr.temp.210 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_210.txt")

wtr.heat.map(subset(wtr.temp.210,
                    subset=(datetime>=as.POSIXct('2000-01-01',tz='UTC') &
                              datetime < as.POSIXct('2001-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2000-05-15', tz='UTC'), as.POSIXct('2000-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2000'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.210,
                    subset=(datetime>=as.POSIXct('2001-01-01',tz='UTC') &
                              datetime < as.POSIXct('2002-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2001-05-15', tz='UTC'), as.POSIXct('2001-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2001'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.210,
                    subset=(datetime>=as.POSIXct('2002-01-01',tz='UTC') &
                              datetime < as.POSIXct('2003-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2002-05-15', tz='UTC'), as.POSIXct('2002-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2002'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.210,
                    subset=(datetime>=as.POSIXct('2003-01-01',tz='UTC') &
                              datetime < as.POSIXct('2004-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2003-05-15', tz='UTC'), as.POSIXct('2003-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2003'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.210,
                    subset=(datetime>=as.POSIXct('2004-01-01',tz='UTC') &
                              datetime < as.POSIXct('2005-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2004-05-15', tz='UTC'), as.POSIXct('2004-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2004'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.210,
                    subset=(datetime>=as.POSIXct('2005-01-01',tz='UTC') &
                              datetime < as.POSIXct('2006-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2005-05-15', tz='UTC'), as.POSIXct('2005-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2005'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.210,
                    subset=(datetime>=as.POSIXct('2006-01-01',tz='UTC') &
                              datetime < as.POSIXct('2007-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2006-05-15', tz='UTC'), as.POSIXct('2006-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2006'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.210,
                    subset=(datetime>=as.POSIXct('2007-01-01',tz='UTC') &
                              datetime < as.POSIXct('2008-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2007-05-15', tz='UTC'), as.POSIXct('2007-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2007'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.210,
                    subset=(datetime>=as.POSIXct('2008-01-01',tz='UTC') &
                              datetime < as.POSIXct('2009-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2008-05-15', tz='UTC'), as.POSIXct('2008-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2008'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.210,
                    subset=(datetime>=as.POSIXct('2009-01-01',tz='UTC') &
                              datetime < as.POSIXct('2010-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2009-05-15', tz='UTC'), as.POSIXct('2009-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2009'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.210,
                    subset=(datetime>=as.POSIXct('2010-01-01',tz='UTC') &
                              datetime < as.POSIXct('2011-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2010-05-15', tz='UTC'), as.POSIXct('2010-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2010'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.210,
                    subset=(datetime>=as.POSIXct('2011-01-01',tz='UTC') &
                              datetime < as.POSIXct('2012-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2011-05-15', tz='UTC'), as.POSIXct('2011-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2011'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.210,
                    subset=(datetime>=as.POSIXct('2012-01-01',tz='UTC') &
                              datetime < as.POSIXct('2013-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2012-05-15', tz='UTC'), as.POSIXct('2012-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2012'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.210,
                    subset=(datetime>=as.POSIXct('2013-01-01',tz='UTC') &
                              datetime < as.POSIXct('2014-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2013-05-15', tz='UTC'), as.POSIXct('2013-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2013'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.210,
                    subset=(datetime>=as.POSIXct('2014-01-01',tz='UTC') &
                              datetime < as.POSIXct('2015-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2014-05-15', tz='UTC'), as.POSIXct('2014-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2014'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.210,
                    subset=(datetime>=as.POSIXct('2015-01-01',tz='UTC') &
                              datetime < as.POSIXct('2016-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2015-05-15', tz='UTC'), as.POSIXct('2015-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2015'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.210,
                    subset=(datetime>=as.POSIXct('2016-01-01',tz='UTC') &
                              datetime < as.POSIXct('2017-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2016-05-15', tz='UTC'), as.POSIXct('2016-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2016'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.210,
                    subset=(datetime>=as.POSIXct('2017-01-01',tz='UTC') &
                              datetime < as.POSIXct('2018-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2017-05-15', tz='UTC'), as.POSIXct('2017-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2017'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

#### define epi and hypo over time at 210 ####
md_210 <- ts.meta.depths(wtr.temp.210,
                         na.rm = T) %>% 
  mutate(date = as.POSIXct(format(datetime, '%Y-%m-%d'), tz='UTC')) %>% 
  select(-datetime)

water_temp_depth_210 <- water_temp %>% 
  filter(station == 210) %>% 
  full_join(., md_210) %>% 
  mutate(layer = case_when(depth_m < top ~ 'epi',
                           depth_m > top & depth_m < bottom ~ 'meta',
                           depth_m > bottom ~ 'hypo'))

md_210_vert <-  md_210 %>% 
  gather(variable, value, -date, - year)

#### MEAN SURFACE, BOTTOM JUN-SEPT SITE 210 ####
mean_hypo_210 <- water_temp_depth_210 %>%
  filter(layer == 'hypo') %>%
  mutate(year = as.numeric(format(date, '%Y')),
         month = as.numeric(format(date, '%m'))) %>% 
  filter(month >=6 & month<10) %>% 
  group_by(year) %>% 
  summarise(mean_temp_degC = mean(temp_degC, na.rm = T),
            n_temp = length(!is.na(temp_degC))) %>% 
  ungroup() %>% 
  filter(n_temp >= 0.75 * 30) %>% 
  mutate(layer = 'hypo',
         station = 210)

mean_epi_210 <- water_temp_depth_210 %>%
  filter(layer == 'epi') %>%
  mutate(year = as.numeric(format(date, '%Y')),
         month = as.numeric(format(date, '%m'))) %>% 
  filter(month >=6 & month<10) %>% 
  group_by(year) %>% 
  summarise(mean_temp_degC = mean(temp_degC, na.rm = T),
            n_temp = length(!is.na(temp_degC))) %>% 
  ungroup() %>% 
  filter(n_temp >= 0.75 * 30) %>% 
  mutate(layer = 'epi',
         station = 210)

mean_210 <- full_join(mean_epi_210, mean_hypo_210)

ggplot(mean_210, aes(x = year, y=mean_temp_degC)) +
  geom_smooth(method = 'lm', se=F, color = 'gray') +
  geom_point(size = 3) +
  facet_grid(layer ~ ., scales = 'free_y') +
  labs(title = 'mean summer temperature (Jun-Sept)',
       x=NULL,
       y='mean summer temperature') +
  final_theme


#### STATION 220 HEATMAPS ####
water_temp_220 <- water_temp_h %>% 
  ungroup() %>% 
  filter(station == 220) %>% 
  rename(wtr_0.5 = `0.5`,
         wtr_1.0 = `1`,
         wtr_2.0 = `2`,
         wtr_3.0 = `3`,
         wtr_4.0 = `4`,
         wtr_5.0 = `5`,
         wtr_6.0 = `6`,
         wtr_7.0 = `7`,
         wtr_8.0 = `8`,
         wtr_9.0 = `9`,
         wtr_10.0 = `10`,
         wtr_11.0 = `11`,
         wtr_12.0 = `12`,
         wtr_13.0 = `13`,
         wtr_14.0 = `14`,
         wtr_16.0 = `16`,
         wtr_18.0 = `18`,
         wtr_20.0 = `20`,
         wtr_22.0 =`22`) %>% 
  select(date, wtr_0.5, wtr_1.0, wtr_2.0,
         wtr_3.0, wtr_4.0, wtr_0.5, wtr_6.0, wtr_7.0,
         wtr_8.0, wtr_9.0, wtr_10.0, wtr_11.0, wtr_12.0,
         wtr_13.0, wtr_14.0, wtr_16.0, wtr_18.0, wtr_20.0, 
         wtr_22.0) %>% 
  filter(date != as.POSIXct('2011-08-17', tz='UTC')) %>% 
  filter(date != as.POSIXct('2011-09-19', tz='UTC')) %>%   
  filter(date != as.POSIXct('2005-07-02', tz='UTC')) %>% 
  filter(date != as.POSIXct('2002-09-23', tz='UTC')) %>% 
  filter(date != as.POSIXct('1998-09-28', tz='UTC')) 


water_temp_220 %>%
  arrange(date) %>%
  filter(date>=as.POSIXct('2000-01-01', tz='UTC')) %>% 
  mutate(datetime = as.character(paste(date, '12:00', sep = ' '))) %>%
  select(-date) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_220.txt", delim='\t')

wtr.temp.220 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_220.txt")

wtr.heat.map(subset(wtr.temp.220,
                    subset=(datetime>=as.POSIXct('2000-01-01',tz='UTC') &
                              datetime < as.POSIXct('2001-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2000-05-15', tz='UTC'), as.POSIXct('2000-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2000'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.220,
                    subset=(datetime>=as.POSIXct('2001-01-01',tz='UTC') &
                              datetime < as.POSIXct('2002-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2001-05-15', tz='UTC'), as.POSIXct('2001-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2001'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.220,
                    subset=(datetime>=as.POSIXct('2002-01-01',tz='UTC') &
                              datetime < as.POSIXct('2003-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2002-05-15', tz='UTC'), as.POSIXct('2002-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2002'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.220,
                    subset=(datetime>=as.POSIXct('2003-01-01',tz='UTC') &
                              datetime < as.POSIXct('2004-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2003-05-15', tz='UTC'), as.POSIXct('2003-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2003'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.220,
                    subset=(datetime>=as.POSIXct('2004-01-01',tz='UTC') &
                              datetime < as.POSIXct('2005-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2004-05-15', tz='UTC'), as.POSIXct('2004-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2004'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.220,
                    subset=(datetime>=as.POSIXct('2005-01-01',tz='UTC') &
                              datetime < as.POSIXct('2006-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2005-05-15', tz='UTC'), as.POSIXct('2005-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2005'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.220,
                    subset=(datetime>=as.POSIXct('2006-01-01',tz='UTC') &
                              datetime < as.POSIXct('2007-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2006-05-15', tz='UTC'), as.POSIXct('2006-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2006'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.220,
                    subset=(datetime>=as.POSIXct('2007-01-01',tz='UTC') &
                              datetime < as.POSIXct('2008-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2007-05-15', tz='UTC'), as.POSIXct('2007-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2007'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.220,
                    subset=(datetime>=as.POSIXct('2008-01-01',tz='UTC') &
                              datetime < as.POSIXct('2009-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2008-05-15', tz='UTC'), as.POSIXct('2008-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2008'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.220,
                    subset=(datetime>=as.POSIXct('2009-01-01',tz='UTC') &
                              datetime < as.POSIXct('2010-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2009-05-15', tz='UTC'), as.POSIXct('2009-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2009'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.220,
                    subset=(datetime>=as.POSIXct('2010-01-01',tz='UTC') &
                              datetime < as.POSIXct('2011-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2010-05-15', tz='UTC'), as.POSIXct('2010-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2010'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.220,
                    subset=(datetime>=as.POSIXct('2011-01-01',tz='UTC') &
                              datetime < as.POSIXct('2012-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2011-05-15', tz='UTC'), as.POSIXct('2011-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2011'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.220,
                    subset=(datetime>=as.POSIXct('2012-01-01',tz='UTC') &
                              datetime < as.POSIXct('2013-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2012-05-15', tz='UTC'), as.POSIXct('2012-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2012'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.220,
                    subset=(datetime>=as.POSIXct('2013-01-01',tz='UTC') &
                              datetime < as.POSIXct('2014-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2013-05-15', tz='UTC'), as.POSIXct('2013-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2013'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.220,
                    subset=(datetime>=as.POSIXct('2014-01-01',tz='UTC') &
                              datetime < as.POSIXct('2015-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2014-05-15', tz='UTC'), as.POSIXct('2014-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2014'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.220,
                    subset=(datetime>=as.POSIXct('2015-01-01',tz='UTC') &
                              datetime < as.POSIXct('2016-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2015-05-15', tz='UTC'), as.POSIXct('2015-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2015'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.220,
                    subset=(datetime>=as.POSIXct('2016-01-01',tz='UTC') &
                              datetime < as.POSIXct('2017-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2016-05-15', tz='UTC'), as.POSIXct('2016-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2016'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.220,
                    subset=(datetime>=as.POSIXct('2017-01-01',tz='UTC') &
                              datetime < as.POSIXct('2018-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2017-05-15', tz='UTC'), as.POSIXct('2017-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2017'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

#### define epi and hypo over time at 220 ####
md_220 <- ts.meta.depths(wtr.temp.220,
                         na.rm = T) %>% 
  mutate(date = as.POSIXct(format(datetime, '%Y-%m-%d'), tz='UTC')) %>% 
  select(-datetime)

water_temp_depth_220 <- water_temp %>% 
  filter(station == 220) %>% 
  full_join(., md_220) %>% 
  mutate(layer = case_when(depth_m < top ~ 'epi',
                           depth_m > top & depth_m < bottom ~ 'meta',
                           depth_m > bottom ~ 'hypo'))

#### MEAN SURFACE, BOTTOM JUN-SEPT SITE 220 ####
mean_hypo_220 <- water_temp_depth_220 %>%
  filter(layer == 'hypo') %>%
  mutate(year = as.numeric(format(date, '%Y')),
         month = as.numeric(format(date, '%m'))) %>% 
  filter(month >=6 & month<10) %>% 
  group_by(year) %>% 
  summarise(mean_temp_degC = mean(temp_degC, na.rm = T),
            n_temp = length(!is.na(temp_degC))) %>% 
  ungroup() %>% 
  filter(n_temp >= 0.75 * 30) %>% 
  mutate(layer = 'hypo',
         station = 220)

mean_epi_220 <- water_temp_depth_220 %>%
  filter(layer == 'epi') %>%
  mutate(year = as.numeric(format(date, '%Y')),
         month = as.numeric(format(date, '%m'))) %>% 
  filter(month >=6 & month<10) %>% 
  group_by(year) %>% 
  summarise(mean_temp_degC = mean(temp_degC, na.rm = T),
            n_temp = length(!is.na(temp_degC))) %>% 
  ungroup() %>% 
  filter(n_temp >= 0.75 * 30) %>% 
  mutate(layer = 'epi',
         station = 220)

mean_220 <- full_join(mean_epi_220, mean_hypo_220)

ggplot(mean_220, aes(x = year, y=mean_temp_degC)) +
  geom_smooth(method = 'lm', se=F, color = 'gray') +
  geom_point(size = 3) +
  facet_grid(layer ~ ., scales = 'free_y') +
  labs(title = 'mean summer temperature (Jun-Sept)',
       x=NULL,
       y='mean summer temperature') +
  final_theme


#### STATION 230 HEATMAPS ####
water_temp_230 <- water_temp_h %>% 
  ungroup() %>% 
  filter(station == 230) %>% 
  rename(wtr_0.1 = `0.1`,
         wtr_0.5 = `0.5`,
         wtr_1.0 = `1`,
         wtr_2.0 = `2`,
         wtr_3.0 = `3`,
         wtr_4.0 = `4`,
         wtr_5.0 = `5`,
         wtr_6.0 = `6`,
         wtr_7.0 = `7`,
         wtr_8.0 = `8`,
         wtr_9.0 = `9`,
         wtr_10.0 = `10`,
         wtr_11.0 = `11`,
         wtr_12.0 = `12`,
         wtr_13.0 = `13`,
         wtr_14.0 = `14`,
         wtr_16.0 = `16`,
         wtr_17.0 = `17`,
         wtr_18.0 = `18`,
         wtr_19.0 = `19`,
         wtr_20.0 = `20`) %>% 
  select(date, wtr_0.1, wtr_0.5, wtr_1.0, wtr_2.0,
         wtr_3.0, wtr_4.0, wtr_0.5, wtr_6.0, wtr_7.0,
         wtr_8.0, wtr_9.0, wtr_10.0, wtr_11.0, wtr_12.0,
         wtr_13.0, wtr_14.0, wtr_16.0, wtr_17.0, wtr_18.0, 
         wtr_19.0, wtr_20.0) %>% 
  filter(date != as.POSIXct('2011-08-17', tz='UTC')) %>% 
  filter(date != as.POSIXct('2006-05-31', tz='UTC')) %>% 
  filter(date != as.POSIXct('2005-07-02', tz='UTC')) %>% 
  filter(date != as.POSIXct('1998-09-29', tz='UTC')) 


water_temp_230 %>%
  arrange(date) %>%
  filter(date>=as.POSIXct('2000-01-01', tz='UTC')) %>% 
  mutate(datetime = as.character(paste(date, '12:00', sep = ' '))) %>%
  select(-date) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_230.txt", delim='\t')

wtr.temp.230 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_230.txt")

wtr.heat.map(subset(wtr.temp.230,
                    subset=(datetime>=as.POSIXct('2000-01-01',tz='UTC') &
                              datetime < as.POSIXct('2001-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2000-05-15', tz='UTC'), as.POSIXct('2000-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2000'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.230,
                    subset=(datetime>=as.POSIXct('2001-01-01',tz='UTC') &
                              datetime < as.POSIXct('2002-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2001-05-15', tz='UTC'), as.POSIXct('2001-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2001'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.230,
                    subset=(datetime>=as.POSIXct('2002-01-01',tz='UTC') &
                              datetime < as.POSIXct('2003-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2002-05-15', tz='UTC'), as.POSIXct('2002-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2002'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.230,
                    subset=(datetime>=as.POSIXct('2003-01-01',tz='UTC') &
                              datetime < as.POSIXct('2004-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2003-05-15', tz='UTC'), as.POSIXct('2003-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2003'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.230,
                    subset=(datetime>=as.POSIXct('2004-01-01',tz='UTC') &
                              datetime < as.POSIXct('2005-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2004-05-15', tz='UTC'), as.POSIXct('2004-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2004'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.230,
                    subset=(datetime>=as.POSIXct('2005-01-01',tz='UTC') &
                              datetime < as.POSIXct('2006-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2005-05-15', tz='UTC'), as.POSIXct('2005-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2005'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.230,
                    subset=(datetime>=as.POSIXct('2006-01-01',tz='UTC') &
                              datetime < as.POSIXct('2007-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2006-05-15', tz='UTC'), as.POSIXct('2006-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2006'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.230,
                    subset=(datetime>=as.POSIXct('2007-01-01',tz='UTC') &
                              datetime < as.POSIXct('2008-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2007-05-15', tz='UTC'), as.POSIXct('2007-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2007'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.230,
                    subset=(datetime>=as.POSIXct('2008-01-01',tz='UTC') &
                              datetime < as.POSIXct('2009-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2008-05-15', tz='UTC'), as.POSIXct('2008-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2008'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.230,
                    subset=(datetime>=as.POSIXct('2009-01-01',tz='UTC') &
                              datetime < as.POSIXct('2010-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2009-05-15', tz='UTC'), as.POSIXct('2009-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2009'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.230,
                    subset=(datetime>=as.POSIXct('2010-01-01',tz='UTC') &
                              datetime < as.POSIXct('2011-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2010-05-15', tz='UTC'), as.POSIXct('2010-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2010'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.230,
                    subset=(datetime>=as.POSIXct('2011-01-01',tz='UTC') &
                              datetime < as.POSIXct('2012-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2011-05-15', tz='UTC'), as.POSIXct('2011-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2011'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.230,
                    subset=(datetime>=as.POSIXct('2012-01-01',tz='UTC') &
                              datetime < as.POSIXct('2013-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2012-05-15', tz='UTC'), as.POSIXct('2012-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2012'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.230,
                    subset=(datetime>=as.POSIXct('2013-01-01',tz='UTC') &
                              datetime < as.POSIXct('2014-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2013-05-15', tz='UTC'), as.POSIXct('2013-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2013'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.230,
                    subset=(datetime>=as.POSIXct('2014-01-01',tz='UTC') &
                              datetime < as.POSIXct('2015-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2014-05-15', tz='UTC'), as.POSIXct('2014-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2014'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.230,
                    subset=(datetime>=as.POSIXct('2015-01-01',tz='UTC') &
                              datetime < as.POSIXct('2016-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2015-05-15', tz='UTC'), as.POSIXct('2015-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2015'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.230,
                    subset=(datetime>=as.POSIXct('2016-01-01',tz='UTC') &
                              datetime < as.POSIXct('2017-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2016-05-15', tz='UTC'), as.POSIXct('2016-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2016'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)

wtr.heat.map(subset(wtr.temp.230,
                    subset=(datetime>=as.POSIXct('2017-01-01',tz='UTC') &
                              datetime < as.POSIXct('2018-01-01', tz='UTC'))), 
             xlim=c(as.POSIXct('2017-05-15', tz='UTC'), as.POSIXct('2017-10-15', tz='UTC')), 
             zlim=c(5,30), 
             plot.title=title(main=NULL, 
                              ylab="depth (m)", 
                              xlab='2017'),
             key.title=title(main="water\ntemp\n(C)", 
                             font.main=1, 
                             cex.main=1)
)


#### define epi and hypo over time at 230 ####
md_230 <- ts.meta.depths(wtr.temp.230,
                         na.rm = T) %>% 
  mutate(date = as.POSIXct(format(datetime, '%Y-%m-%d'), tz='UTC')) %>% 
  select(-datetime)

water_temp_depth_230 <- water_temp %>% 
  filter(station == 230) %>% 
  full_join(., md_230) %>% 
  mutate(layer = case_when(depth_m < top ~ 'epi',
                           depth_m > top & depth_m < bottom ~ 'meta',
                           depth_m > bottom ~ 'hypo'))

#### MEAN SURFACE, BOTTOM JUN-SEPT SITE 230 ####
mean_hypo_230 <- water_temp_depth_230 %>%
  filter(layer == 'hypo') %>%
  mutate(year = as.numeric(format(date, '%Y')),
         month = as.numeric(format(date, '%m'))) %>% 
  filter(month >=6 & month<10) %>% 
  group_by(year) %>% 
  summarise(mean_temp_degC = mean(temp_degC, na.rm = T),
            n_temp = length(!is.na(temp_degC))) %>% 
  ungroup() %>% 
  filter(n_temp >= 0.75 * 30) %>% 
  mutate(layer = 'hypo',
         station = 230)

mean_epi_230 <- water_temp_depth_230 %>%
  filter(layer == 'epi') %>%
  mutate(year = as.numeric(format(date, '%Y')),
         month = as.numeric(format(date, '%m'))) %>% 
  filter(month >=6 & month<10) %>% 
  group_by(year) %>% 
  summarise(mean_temp_degC = mean(temp_degC, na.rm = T),
            n_temp = length(!is.na(temp_degC))) %>% 
  ungroup() %>% 
  filter(n_temp >= 0.75 * 30) %>% 
  mutate(layer = 'epi',
         station = 230)

mean_230 <- full_join(mean_epi_230, mean_hypo_230)

ggplot(mean_230, aes(x = year, y=mean_temp_degC)) +
  geom_smooth(method = 'lm', se=F, color = 'gray') +
  geom_point(size = 3) +
  facet_grid(layer ~ ., scales = 'free_y') +
  labs(title = 'mean summer temperature (Jun-Sept)',
       x=NULL,
       y='mean summer temperature') +
  final_theme


#### all hypo/epi data together ####
full_join(mean_200, mean_210) %>% 
  full_join(., mean_220) %>% 
  full_join(., mean_230) %>% 
  group_by(station, layer) %>% 
  ggplot(., aes(x = year, y=mean_temp_degC, color = layer, group=layer)) +
  geom_smooth(method = 'lm', color = 'dark gray', se = F) +
  geom_point(size = 3) +
  facet_grid(. ~ station) +
  labs(title = 'mean summer temperature (Jun-Sept)',
       x=NULL,
       y='mean summer temperature') +
  scale_color_economist() +
  scale_y_continuous(limits = c(5, 25)) +
  final_theme_temp


