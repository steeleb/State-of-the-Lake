# heatmaps for temp/do

library(tidyverse)
library(rLakeAnalyzer)

# 2007 temperature heatmap ####

buoy_2007 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2007_tempstring_L1_v2022.csv')

names(buoy_2007)

# check for flags - only clean data
unique(buoy_2007$flag_temp9p5m)
unique(buoy_2007$flag_temp10p5m)
unique(buoy_2007$flag_temp11p5m)
unique(buoy_2007$flag_temp13p5m)

# check locations - we only want data at loon
unique(buoy_2007$location)

buoy_2007_sub <- buoy_2007 %>% 
  mutate(waterTemperature_degC_9p5m = if_else(flag_temp9p5m == 'd', NA_real_, waterTemperature_degC_9p5m),
         waterTemperature_degC_10p5m = if_else(flag_temp9p5m == 'd', NA_real_, waterTemperature_degC_10p5m))
         

buoy_2007_sub <- buoy_2007_sub %>% 
  select(datetime,
         wtr_0.5 = waterTemperature_degC_0p5m,
         wtr_1.0 = waterTemperature_degC_1m,
         wtr_1.5 = waterTemperature_degC_1p5m,
         wtr_2.0 = waterTemperature_degC_2m,
         wtr_2.5 = waterTemperature_degC_2p5m,
         wtr_3.0 = waterTemperature_degC_3m,
         wtr_3.5 = waterTemperature_degC_3p5m,
         wtr_4.5 = waterTemperature_degC_4p5m,
         wtr_5.5 = waterTemperature_degC_5p5m,
         wtr_6.5 = waterTemperature_degC_6p5m,
         wtr_7.5 = waterTemperature_degC_7p5m,
         wtr_8.5 = waterTemperature_degC_8p5m) %>% 
  filter(if_all(wtr_0.5:wtr_8.5, ~!is.na(.)))

buoy_2007_sub %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, '%Y-%m-%d %H:%M'))) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2007.txt", delim='\t')

wtr.temp.2007 <- load.ts(fPath = "C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2007.txt",
                         tz = "UTC")

wtr.heat.map(wtr.temp.2007,
             xlim=c(as.POSIXct('2007-01-01', tz='UTC'), as.POSIXct('2008-01-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2007$datetime, 
                                        at = (seq(as.POSIXct('2007-01-01', tz='UTC'),
                                                  as.POSIXct('2008-01-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(-2,30),
             plot.title=title(main='2007',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)


# 2008 temperature heatmap ####

buoy_2008 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2008_tempstring_L1_v2022.csv')

names(buoy_2008)

# check for flags - only clean data
unique(buoy_2008$flag_temp9p5m)
unique(buoy_2008$flag_temp10p5m)
unique(buoy_2008$flag_temp11p5m)
unique(buoy_2008$flag_temp13p5m)

# check locations - we only want data at loon
unique(buoy_2008$location)

buoy_2008_sub <- buoy_2008 %>% 
  mutate(waterTemperature_degC_9p5m = if_else(flag_temp9p5m == 'd', NA_real_, waterTemperature_degC_9p5m),
         waterTemperature_degC_10p5m = if_else(flag_temp9p5m == 'd', NA_real_, waterTemperature_degC_10p5m))


buoy_2008_sub <- buoy_2008_sub %>% 
  select(datetime,
         wtr_0.5 = waterTemperature_degC_0p5m,
         wtr_1.0 = waterTemperature_degC_1m,
         wtr_1.5 = waterTemperature_degC_1p5m,
         wtr_2.0 = waterTemperature_degC_2m,
         wtr_2.5 = waterTemperature_degC_2p5m,
         wtr_3.0 = waterTemperature_degC_3m,
         wtr_3.5 = waterTemperature_degC_3p5m,
         wtr_4.5 = waterTemperature_degC_4p5m,
         wtr_5.5 = waterTemperature_degC_5p5m,
         wtr_6.5 = waterTemperature_degC_6p5m,
         wtr_7.5 = waterTemperature_degC_7p5m,
         wtr_8.5 = waterTemperature_degC_8p5m) %>% 
  filter(if_all(wtr_0.5:wtr_8.5, ~!is.na(.)))

buoy_2008_sub %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, '%Y-%m-%d %H:%M'))) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2008.txt", delim='\t')

wtr.temp.2008 <- load.ts(fPath = "C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2008.txt",
                         tz = "UTC")

wtr.heat.map(wtr.temp.2008,
             xlim=c(as.POSIXct('2008-01-01', tz='UTC'), as.POSIXct('2009-01-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2008$datetime, 
                                        at = (seq(as.POSIXct('2008-01-01', tz='UTC'),
                                                  as.POSIXct('2009-01-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(-2,30),
             plot.title=title(main='2008',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)

# 2009 tempearture heat map####

buoy_2009 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2009_tempstring_L1_v2022.csv')

names(buoy_2009)

# check for flags - only clean data
unique(buoy_2009$flag_alltemp)
unique(buoy_2009$flag_temp9p5m)
unique(buoy_2009$flag_temp10p5m)
unique(buoy_2009$flag_temp11p5m)
unique(buoy_2009$flag_temp13p5m)

# check locations - we only want data at loon
unique(buoy_2009$location)

buoy_2009_sub <- buoy_2009 %>% 
  filter(location == "loon") %>% 
  mutate(waterTemperature_degC_9p5m = if_else(flag_temp9p5m == 'd', NA_real_, waterTemperature_degC_9p5m),
         waterTemperature_degC_10p5m = if_else(flag_temp9p5m == 'd', NA_real_, waterTemperature_degC_10p5m))


buoy_2009_sub <- buoy_2009_sub %>% 
  select(datetime,
         wtr_0.5 = waterTemperature_degC_0p5m,
         wtr_1.0 = waterTemperature_degC_1m,
         wtr_1.5 = waterTemperature_degC_1p5m,
         wtr_2.0 = waterTemperature_degC_2m,
         wtr_2.5 = waterTemperature_degC_2p5m,
         wtr_3.0 = waterTemperature_degC_3m,
         wtr_3.5 = waterTemperature_degC_3p5m,
         wtr_4.5 = waterTemperature_degC_4p5m,
         wtr_5.5 = waterTemperature_degC_5p5m,
         wtr_6.5 = waterTemperature_degC_6p5m,
         wtr_7.5 = waterTemperature_degC_7p5m,
         wtr_8.5 = waterTemperature_degC_8p5m) 

buoy_2009_sub %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, '%Y-%m-%d %H:%M'))) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2009.txt", delim='\t')

wtr.temp.2009 <- load.ts(fPath = "C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2009.txt",
                         tz = "UTC")

wtr.heat.map(wtr.temp.2009,
             xlim=c(as.POSIXct('2009-01-01', tz='UTC'), as.POSIXct('2010-01-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2009$datetime, 
                                        at = (seq(as.POSIXct('2009-01-01', tz='UTC'),
                                                  as.POSIXct('2010-01-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(-2,30),
             plot.title=title(main='2009',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)


# 2010 temperature heatmaps####

buoy_2010 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2010_tempstring_L1_v2022.csv')

names(buoy_2010)

# check for flags - only clean data
unique(buoy_2010$flag_alltemp)
unique(buoy_2010$flag_temp11p5m)
unique(buoy_2010$flag_temp13p5m)

# check locations - we only want data at loon
unique(buoy_2010$location)

buoy_2010_sub <- buoy_2010 %>% 
  filter(location == "loon") %>% 
  mutate(across(waterTemperature_degC_0p5m:waterTemperature_degC_13p5m, 
                ~ if_else(!is.na(flag_alltemp), NA_real_, .))) 

buoy_2010_sub <- buoy_2010_sub %>% 
  select(datetime,
         wtr_0.5 = waterTemperature_degC_0p5m,
         wtr_1.0 = waterTemperature_degC_1m,
         wtr_1.5 = waterTemperature_degC_1p5m,
         wtr_2.0 = waterTemperature_degC_2m,
         wtr_2.5 = waterTemperature_degC_2p5m,
         wtr_3.0 = waterTemperature_degC_3m,
         wtr_3.5 = waterTemperature_degC_3p5m,
         wtr_4.5 = waterTemperature_degC_4p5m,
         wtr_5.5 = waterTemperature_degC_5p5m,
         wtr_6.5 = waterTemperature_degC_6p5m,
         wtr_7.5 = waterTemperature_degC_7p5m,
         wtr_8.5 = waterTemperature_degC_8p5m,
         wtr_9.5 = waterTemperature_degC_9p5m,
         wtr_10.5 = waterTemperature_degC_10p5m)

buoy_2010_sub %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, '%Y-%m-%d %H:%M'))) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2010.txt", delim='\t')

wtr.temp.2010 <- load.ts(fPath = "C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2010.txt",
                         tz = "UTC")

wtr.heat.map(wtr.temp.2010,
             xlim=c(as.POSIXct('2010-05-01', tz='UTC'), as.POSIXct('2010-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2010$datetime, 
                                        at = (seq(as.POSIXct('2010-01-01', tz='UTC'),
                                                  as.POSIXct('2011-01-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(-2,30),
             plot.title=title(main='2010',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)


# 2011 temperature heatmaps####

buoy_2011 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2011_tempstring_L1_v2022.csv')

names(buoy_2011)

# check locations - we only want data at loon
unique(buoy_2011$location)

buoy_2011_sub <- buoy_2011 %>% 
  filter(location == "loon") 

buoy_2011_sub <- buoy_2011_sub %>% 
  select(datetime,
         wtr_0.5 = waterTemperature_degC_0p5m,
         wtr_1.5 = waterTemperature_degC_1p5m,
         wtr_2.5 = waterTemperature_degC_2p5m,
         wtr_3.5 = waterTemperature_degC_3p5m,
         wtr_4.5 = waterTemperature_degC_4p5m,
         wtr_5.5 = waterTemperature_degC_5p5m,
         wtr_6.5 = waterTemperature_degC_6p5m,
         wtr_7.5 = waterTemperature_degC_7p5m,
         wtr_8.5 = waterTemperature_degC_8p5m,
         wtr_9.5 = waterTemperature_degC_9p5m)

buoy_2011_sub %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, '%Y-%m-%d %H:%M'))) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2011.txt", delim='\t')

wtr.temp.2011 <- load.ts(fPath = "C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2011.txt",
                         tz = "UTC")

wtr.heat.map(wtr.temp.2011,
             xlim=c(as.POSIXct('2011-05-01', tz='UTC'), as.POSIXct('2011-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2011$datetime, 
                                        at = (seq(as.POSIXct('2011-01-01', tz='UTC'),
                                                  as.POSIXct('2011-01-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(-2,30),
             
             plot.title=title(main='2011',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)



# 2012 temperature heatmaps####

buoy_2012 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2012_tempstring_L1_v2022.csv')

names(buoy_2012)

# check locations - we only want data at loon
unique(buoy_2012$location)

buoy_2012_sub <- buoy_2012 %>% 
  filter(location == "loon") 

buoy_2012_sub <- buoy_2012_sub %>% 
  select(datetime,
         wtr_0.5 = waterTemperature_degC_0p5m,
         wtr_1.5 = waterTemperature_degC_1p5m,
         wtr_2.5 = waterTemperature_degC_2p5m,
         wtr_3.5 = waterTemperature_degC_3p5m,
         wtr_4.5 = waterTemperature_degC_4p5m,
         wtr_5.5 = waterTemperature_degC_5p5m,
         wtr_6.5 = waterTemperature_degC_6p5m,
         wtr_7.5 = waterTemperature_degC_7p5m,
         wtr_8.5 = waterTemperature_degC_8p5m,
         wtr_9.5 = waterTemperature_degC_9p5m)

buoy_2012_sub %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, '%Y-%m-%d %H:%M'))) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2012.txt", delim='\t')

wtr.temp.2012 <- load.ts(fPath = "C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2012.txt",
                         tz = "UTC")

wtr.heat.map(wtr.temp.2012,
             xlim=c(as.POSIXct('2012-05-01', tz='UTC'), as.POSIXct('2012-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2012$datetime, 
                                        at = (seq(as.POSIXct('2012-05-01', tz='UTC'),
                                                  as.POSIXct('2012-11-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(-2,30),
             plot.title=title(main='2012',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)




# 2013 temperature heatmaps####

buoy_2013 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2013_tempstring_L1_v2022.csv')

names(buoy_2013)

# check locations - we only want data at loon
unique(buoy_2013$location)

buoy_2013_sub <- buoy_2013 %>% 
  filter(location == "loon") 

buoy_2013_sub <- buoy_2013_sub %>% 
  select(datetime,
         wtr_0.5 = waterTemperature_degC_0p5m,
         wtr_1.5 = waterTemperature_degC_1p5m,
         wtr_2.5 = waterTemperature_degC_2p5m,
         wtr_3.5 = waterTemperature_degC_3p5m,
         wtr_4.5 = waterTemperature_degC_4p5m,
         wtr_5.5 = waterTemperature_degC_5p5m,
         wtr_6.5 = waterTemperature_degC_6p5m,
         wtr_7.5 = waterTemperature_degC_7p5m,
         wtr_8.5 = waterTemperature_degC_8p5m,
         wtr_9.5 = waterTemperature_degC_9p5m)

buoy_2013_sub %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, '%Y-%m-%d %H:%M'))) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2013.txt", delim='\t')

wtr.temp.2013 <- load.ts(fPath = "C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2013.txt",
                         tz = "UTC")

wtr.heat.map(wtr.temp.2013,
             xlim=c(as.POSIXct('2013-05-01', tz='UTC'), as.POSIXct('2013-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2013$datetime, 
                                        at = (seq(as.POSIXct('2013-05-01', tz='UTC'),
                                                  as.POSIXct('2013-11-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(-2,30),
             plot.title=title(main='2013',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)


# 2014 temperature heatmaps####

buoy_2014 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2014_tempstring_L1_v2022.csv')

names(buoy_2014)

# check flags
unique(buoy_2014$flag_alltemp)
unique(buoy_2014$flag_temp10p5m)

# check locations - we only want data at loon
unique(buoy_2014$location)

buoy_2014_sub <- buoy_2014 %>% 
  filter(location == "loon") %>% 
  mutate(waterTemperature_degC_10p5m = if_else(flag_temp10p5m == 'd', 
                                               NA_real_, 
                                               waterTemperature_degC_10p5m))

buoy_2014_sub <- buoy_2014_sub %>% 
  select(datetime,
         wtr_0.5 = waterTemperature_degC_0p5m,
         wtr_1.5 = waterTemperature_degC_1p5m,
         wtr_2.5 = waterTemperature_degC_2p5m,
         wtr_3.5 = waterTemperature_degC_3p5m,
         wtr_4.5 = waterTemperature_degC_4p5m,
         wtr_5.5 = waterTemperature_degC_5p5m,
         wtr_6.5 = waterTemperature_degC_6p5m,
         wtr_7.5 = waterTemperature_degC_7p5m,
         wtr_8.5 = waterTemperature_degC_8p5m,
         wtr_9.5 = waterTemperature_degC_9p5m,
         wtr_10.5 = waterTemperature_degC_10p5m)

#winter hobo
winter1415 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/2014-2015_wintertempstring_L1_v2022.csv')

names(winter1415)
winter_end14 <- winter1415 %>% 
  select(datetime,
         # rename to match with summer
         wtr_1.5 = waterTemperature_degC_1m,
         wtr_2.5 = waterTemperature_degC_2m,
         wtr_3.5 = waterTemperature_degC_3m,
         wtr_4.5 = waterTemperature_degC_4m,
         wtr_5.5 = waterTemperature_degC_5m,
         wtr_6.5 = waterTemperature_degC_6m,
         wtr_7.5 = waterTemperature_degC_7m,
         wtr_8.5 = waterTemperature_degC_8m,
         wtr_9.5 = waterTemperature_degC_9m) %>% 
  filter(datetime < ymd('2015-01-01'))

buoy_hobo_2014 <- full_join(buoy_2014_sub, winter_end14) %>%
  select(datetime, wtr_0.5, wtr_1.5, wtr_2.5, wtr_3.5, wtr_4.5, wtr_6.5, wtr_7.5, wtr_8.5, wtr_9.5, wtr_10.5) %>% 
  filter(if_any(wtr_0.5:wtr_10.5, ~!is.na(.)))

buoy_hobo_2014 %>%
  group_by(datetime) %>% 
  slice(1) %>% 
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, "%Y-%m-%d %H:%M"))) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2014.txt", delim='\t')

wtr.temp.2014 <- load.ts(fPath = "C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2014.txt",
                         tz = "UTC")

wtr.heat.map(wtr.temp.2014,
             xlim=c(as.POSIXct('2014-05-01', tz='UTC'), as.POSIXct('2014-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2014$datetime, 
                                        at = (seq(as.POSIXct('2014-05-01', tz='UTC'),
                                                  as.POSIXct('2014-11-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(-2,30),
             plot.title=title(main='2014',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)


# 2015 temperature heatmaps ####

# early 2015
winter_start15 <- winter1415 %>% 
  select(datetime,
         # rename to match with summer
         wtr_1.5 = waterTemperature_degC_1m,
         wtr_2.5 = waterTemperature_degC_2m,
         wtr_3.5 = waterTemperature_degC_3m,
         wtr_4.5 = waterTemperature_degC_4m,
         wtr_5.5 = waterTemperature_degC_5m,
         wtr_6.5 = waterTemperature_degC_6m,
         wtr_7.5 = waterTemperature_degC_7m,
         wtr_8.5 = waterTemperature_degC_8m,
         wtr_9.5 = waterTemperature_degC_9m) %>% 
  filter(datetime >= ymd('2015-01-01'))


buoy_2015 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2015_tempstring_L1_v2022.csv')

names(buoy_2015)

# check locations (we only want loon)
unique(buoy_2015$location)

buoy_2015_sub <- buoy_2015 %>% 
  filter(location == 'loon') %>%  
  select(datetime,
         wtr_1.5 = waterTemperature_degC_1p5m,
         wtr_2.5 = waterTemperature_degC_2p5m,
         wtr_3.5 = waterTemperature_degC_3p5m,
         wtr_4.5 = waterTemperature_degC_4p5m,
         wtr_5.5 = waterTemperature_degC_5p5m,
         wtr_6.5 = waterTemperature_degC_6p5m,
         wtr_7.5 = waterTemperature_degC_7p5m,
         wtr_8.5 = waterTemperature_degC_8p5m,
         wtr_9.5 = waterTemperature_degC_9p5m) 

hobo_2015 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2015_hobotempstring_L1_v2022.csv')

names(hobo_2015)

unique(hobo_2015$location)

hobo_2015_sub <- hobo_2015 %>% 
  select(datetime,
         wtr_0.5 = waterTemperature_degC_0p5m,
         wtr_2.5 = waterTemperature_degC_2p5m,
         wtr_3.5 = waterTemperature_degC_3p5m,
         wtr_4.5 = waterTemperature_degC_4p5m,
         wtr_6.5 = waterTemperature_degC_6p5m,
         wtr_7.5 = waterTemperature_degC_7p5m,
         wtr_8.5 = waterTemperature_degC_8p5m)

#winter hobo
winter1516 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/2015-2016_wintertempstring_L1_v2022.csv')

names(winter1516)

winter_end15 <- winter1516 %>% 
  select(datetime,
         # rename to match with summer
         wtr_1.5 = waterTemperature_degC_1m,
         wtr_2.5 = waterTemperature_degC_2m,
         wtr_3.5 = waterTemperature_degC_3m,
         wtr_4.5 = waterTemperature_degC_4m,
         wtr_5.5 = waterTemperature_degC_5m,
         wtr_6.5 = waterTemperature_degC_6m,
         wtr_7.5 = waterTemperature_degC_7m,
         wtr_8.5 = waterTemperature_degC_8m,
         wtr_9.5 = waterTemperature_degC_9m) %>% 
  filter(datetime < ymd('2016-01-01'))

buoy_hobo_2015 <- full_join(winter_start15, buoy_2015_sub) %>% 
  full_join(., hobo_2015_sub) %>% 
  full_join(., winter_end15) %>% 
  select(datetime, wtr_0.5, wtr_1.5, wtr_2.5, wtr_3.5, wtr_4.5, wtr_6.5, wtr_7.5, wtr_8.5, wtr_9.5) %>% 
  filter(if_any(wtr_0.5:wtr_9.5, ~!is.na(.)))

buoy_hobo_2015 %>%
  group_by(datetime) %>% 
  slice(1) %>% 
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, "%Y-%m-%d %H:%M"))) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2015.txt", delim='\t')

wtr.temp.2015 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2015.txt")

wtr.heat.map(subset(wtr.temp.2015,
                    subset =(datetime>=as.POSIXct('2015-05-01', tz='UTC') & 
                               datetime<= as.POSIXct('2015-11-01', tz='UTC'))),
             xlim=c(as.POSIXct('2015-05-01', tz='UTC'), 
                    as.POSIXct('2015-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2015$datetime, 
                                        at = (seq(as.POSIXct('2015-05-01', tz='UTC'),
                                                  as.POSIXct('2015-11-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(5,30),
             plot.title=title(main='2015 summer',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)

wtr.heat.map(wtr.temp.2015,
             xlim=c(as.POSIXct('2015-01-01', tz='UTC'), as.POSIXct('2016-01-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2015$datetime, 
                                        at = (seq(as.POSIXct('2015-01-01', tz='UTC'),
                                                  as.POSIXct('2016-01-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(-2,30),
             plot.title=title(main='2015',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)



# 2016 tempearture heatmap ####

winter_start16 <- winter1516 %>% 
  filter(datetime >= as.POSIXct('2016-01-01', tz='UTC')) %>% 
  select(datetime,
         # rename to match with summer
         wtr_1.5 = waterTemperature_degC_1m,
         wtr_2.5 = waterTemperature_degC_2m,
         wtr_3.5 = waterTemperature_degC_3m,
         wtr_4.5 = waterTemperature_degC_4m,
         wtr_5.5 = waterTemperature_degC_5m,
         wtr_6.5 = waterTemperature_degC_6m,
         wtr_7.5 = waterTemperature_degC_7m,
         wtr_8.5 = waterTemperature_degC_8m,
         wtr_9.5 = waterTemperature_degC_9m) 

buoy_2016 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2016_tempstring_L1_v2022.csv')

names(buoy_2016)

# check flags
unique(buoy_2016$flag_temp0p5m)

# check locations
unique(buoy_2016$location)

buoy_2016_sub <- buoy_2016 %>% 
  filter(location == 'loon')  %>% 
  select(datetime,
    wtr_0.5 = waterTemperature_degC_0p5m,
    wtr_1.5 = waterTemperature_degC_1p5m,
    wtr_2.5 = waterTemperature_degC_2p5m,
    wtr_3.5 = waterTemperature_degC_3p5m,
    wtr_4.5 = waterTemperature_degC_4p5m,
    wtr_5.5 = waterTemperature_degC_5p5m,
    wtr_6.5 = waterTemperature_degC_6p5m,
    wtr_7.5 = waterTemperature_degC_7p5m,
    wtr_8.5 = waterTemperature_degC_8p5m,
    wtr_9.5 = waterTemperature_degC_9p5m) 

#winter hobo
winter1617 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/2016-2017_wintertempstring_L1_v2022.csv')

winter_end16 <- winter1617 %>% 
  select(datetime,
         # rename to match with summer
         wtr_1.5 = waterTemperature_degC_1m,
         wtr_2.5 = waterTemperature_degC_2m,
         wtr_3.5 = waterTemperature_degC_3m,
         wtr_4.5 = waterTemperature_degC_4m,
         wtr_5.5 = waterTemperature_degC_5m,
         wtr_6.5 = waterTemperature_degC_6m,
         wtr_7.5 = waterTemperature_degC_7m,
         wtr_8.5 = waterTemperature_degC_8m,
         wtr_9.5 = waterTemperature_degC_9m) %>% 
  filter(datetime < ymd('2017-01-01'))


buoy_hobo_2016 <- full_join(winter_start16, buoy_2016_sub) %>% 
  full_join(., winter_end16) %>% 
  select(datetime, wtr_0.5, wtr_1.5, wtr_2.5, wtr_3.5, wtr_4.5, wtr_6.5, wtr_7.5, wtr_8.5, wtr_9.5) %>% 
  filter(if_any(wtr_0.5:wtr_9.5, ~!is.na(.)))

buoy_hobo_2016 %>%
  group_by(datetime) %>% 
  slice(1) %>% 
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, "%Y-%m-%d %H:%M"))) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2016.txt", delim='\t')

wtr.temp.2016 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2016.txt")

wtr.heat.map(subset(wtr.temp.2016,
                    subset =(datetime>=as.POSIXct('2016-05-01', tz='UTC') & 
                               datetime< as.POSIXct('2016-11-01', tz='UTC'))),
             xlim=c(as.POSIXct('2016-05-01', tz='UTC'), as.POSIXct('2016-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2016$datetime, 
                                        at = (seq(as.POSIXct('2016-05-01', tz='UTC'),
                                                  as.POSIXct('2016-11-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(5,30),
             plot.title=title(main='Summer 2016',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)

wtr.heat.map(wtr.temp.2016,
             xlim=c(as.POSIXct('2016-01-01', tz='UTC'), as.POSIXct('2017-01-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2016$datetime, 
                                        at = (seq(as.POSIXct('2016-01-01', tz='UTC'),
                                                  as.POSIXct('2017-01-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(-2,30),
             plot.title=title(main='2016',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)

# 2017 temperature heatmaps ####

winter_start17 <- winter1617 %>% 
  filter(datetime >= as.POSIXct('2017-01-01', tz='UTC')) %>% 
  select(datetime,
         wtr_1 = waterTemperature_degC_1m,
         wtr_2 = waterTemperature_degC_2m,
         wtr_3 = waterTemperature_degC_3m,
         wtr_4 = waterTemperature_degC_4m,
         wtr_5 = waterTemperature_degC_5m,
         wtr_6 = waterTemperature_degC_6m,
         wtr_7 = waterTemperature_degC_7m,
         wtr_8 = waterTemperature_degC_8m,
         wtr_9 = waterTemperature_degC_9m) 

buoy_2017 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2017_tempstring_L1_v2022.csv')

names(buoy_2017)

# check flags
unique(buoy_2017$flag_temp0p85m)

# check locations
unique(buoy_2017$location)

buoy_2017_sub <- buoy_2017 %>% 
  filter(location == 'loon') %>% 
  select(datetime,
         # round for vis an joining
         wtr_1 = waterTemperature_degC_0p85m,,
         wtr_2 = waterTemperature_degC_1p85m,
         wtr_3 = waterTemperature_degC_2p85m,
         wtr_4 = waterTemperature_degC_3p85m,
         wtr_5 = waterTemperature_degC_4p85m,
         wtr_6 = waterTemperature_degC_5p85m,
         wtr_7 = waterTemperature_degC_6p85m,
         wtr_8 = waterTemperature_degC_7p85m,
         wtr_9 = waterTemperature_degC_8p85m,
         wtr_10 = waterTemperature_degC_9p85m) 

winter_1718 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/2017-2018_wintertempstringdo_L1_v2022.csv')

winter_end17 <- winter_1718 %>% 
  select(datetime, 
         wtr_2 = waterTemperature_degC_2m,
         wtr_3 = waterTemperature_degC_3m,
         wtr_4 = waterTemperature_degC_4m,
         wtr_5 = waterTemperature_degC_5m,
         wtr_6 = waterTemperature_degC_6m,
         wtr_8 = waterTemperature_degC_8m,
         wtr_9 = waterTemperature_degC_9m,
         wtr_10 = waterTemperature_degC_10m) %>% 
  filter(datetime < ymd('2018-01-01'))


buoy_hobo_2017 <- full_join(winter_start17, buoy_2017_sub) %>%
  full_join(., winter_end17) %>% 
  select(datetime, wtr_1, wtr_2, wtr_3, wtr_4, wtr_5, wtr_6, wtr_7, wtr_8, wtr_9, wtr_10) %>% 
  filter(if_any(wtr_1:wtr_10, ~!is.na(.)))

buoy_hobo_2017 %>%
  group_by(datetime) %>% 
  slice(1) %>% 
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, "%Y-%m-%d %H:%M"))) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2017.txt", delim='\t')

wtr.temp.2017 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2017.txt")

wtr.heat.map(subset(wtr.temp.2017,
                    subset =(datetime>=as.POSIXct('2017-05-01', tz='UTC') & 
                               datetime< as.POSIXct('2017-11-01', tz='UTC'))),
             xlim=c(as.POSIXct('2017-05-01', tz='UTC'), as.POSIXct('2017-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2017$datetime, 
                                        at = (seq(as.POSIXct('2017-05-01', tz='UTC'),
                                                  as.POSIXct('2017-11-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(5,30),
             plot.title=title(main='Summer 2017',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)

wtr.heat.map(wtr.temp.2017,
             xlim=c(as.POSIXct('2017-01-01', tz='UTC'), as.POSIXct('2018-01-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2017$datetime, 
                                        at = (seq(as.POSIXct('2017-01-01', tz='UTC'),
                                                  as.POSIXct('2018-01-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(-2,30),
             plot.title=title(main='2017',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)


# 2018 temperature heatmaps ####

winter_start18 <- winter_1718 %>% 
  filter(datetime >= as.POSIXct('2018-01-01', tz='UTC')) %>% 
  select(datetime,
         wtr_2 = waterTemperature_degC_2m,
         wtr_3 = waterTemperature_degC_3m,
         wtr_4 = waterTemperature_degC_4m,
         wtr_5 = waterTemperature_degC_5m,
         wtr_6 = waterTemperature_degC_6m,
         wtr_8 = waterTemperature_degC_8m,
         wtr_9 = waterTemperature_degC_9m,
         wtr_10 = waterTemperature_degC_10m) 

buoy_2018 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2018_tempstring_L1_v2022.csv')

names(buoy_2018)

# check flags
unique(buoy_2018$flag_temp0p85m)

# check locations
unique(buoy_2018$location)

buoy_2018_sub <- buoy_2018 %>% 
  filter(location == 'loon') %>% 
  select(datetime,
         # round for vis an joining
         wtr_1 = waterTemperature_degC_0p85m,,
         wtr_2 = waterTemperature_degC_1p85m,
         wtr_3 = waterTemperature_degC_2p85m,
         wtr_4 = waterTemperature_degC_3p85m,
         wtr_5 = waterTemperature_degC_4p85m,
         wtr_6 = waterTemperature_degC_5p85m,
         wtr_7 = waterTemperature_degC_6p85m,
         wtr_8 = waterTemperature_degC_7p85m,
         wtr_9 = waterTemperature_degC_8p85m,
         wtr_10 = waterTemperature_degC_9p85m) 

winter_1819 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/2018-2019_wintertempstringdo_L1_v2022.csv')

names(winter_1819)

winter_end18 <- winter_1819 %>% 
  select(datetime, 
         wtr_1 = waterTemperature_DO_degC_1m,
         wtr_2 = waterTemperature_degC_2m,
         wtr_3 = waterTemperature_degC_3m,
         wtr_4 = waterTemperature_degC_4m,
         wtr_5 = waterTemperature_degC_5m,
         wtr_6 = waterTemperature_degC_6m,
         wtr_7 = waterTemperature_degC_7m,
         wtr_8 = waterTemperature_degC_8m,
         wtr_9 = waterTemperature_degC_9m,
         wtr_10 = waterTemperature_degC_10m) %>% 
  filter(datetime < ymd('2019-01-01'))


buoy_hobo_2018 <- full_join(winter_start18, buoy_2018_sub) %>%
  full_join(., winter_end18) %>% 
  select(datetime, wtr_1, wtr_2, wtr_3, wtr_4, wtr_5, wtr_6, wtr_8, wtr_9, wtr_10) %>% 
  filter(if_any(wtr_1:wtr_10, ~!is.na(.)))

buoy_hobo_2018 %>%
  group_by(datetime) %>% 
  slice(1) %>% 
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, "%Y-%m-%d %H:%M"))) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2018.txt", delim='\t')

wtr.temp.2018 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2018.txt")

wtr.heat.map(subset(wtr.temp.2018,
                    subset =(datetime>=as.POSIXct('2018-05-01', tz='UTC') & 
                               datetime< as.POSIXct('2018-11-01', tz='UTC'))),
             xlim=c(as.POSIXct('2018-05-01', tz='UTC'), as.POSIXct('2018-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2018$datetime, 
                                        at = (seq(as.POSIXct('2018-05-01', tz='UTC'),
                                                  as.POSIXct('2018-11-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(5,30),
             plot.title=title(main='Summer 2018',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)

wtr.heat.map(wtr.temp.2018,
             xlim=c(as.POSIXct('2018-01-01', tz='UTC'), as.POSIXct('2019-01-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2018$datetime, 
                                        at = (seq(as.POSIXct('2018-01-01', tz='UTC'),
                                                  as.POSIXct('2019-01-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(-2,30),
             plot.title=title(main='2018',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)


# 2019 temperature heatmaps ####

winter_start19 <- winter_1819 %>% 
  filter(datetime >= as.POSIXct('2019-01-01', tz='UTC')) %>% 
  select(datetime,
         wtr_1 = waterTemperature_DO_degC_1m,
         wtr_2 = waterTemperature_degC_2m,
         wtr_3 = waterTemperature_degC_3m,
         wtr_4 = waterTemperature_degC_4m,
         wtr_5 = waterTemperature_degC_5m,
         wtr_6 = waterTemperature_degC_6m,
         wtr_8 = waterTemperature_degC_8m,
         wtr_9 = waterTemperature_degC_9m,
         wtr_10 = waterTemperature_degC_10m) 

buoy_2019 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2019_tempstring_L1_v2022.csv')

names(buoy_2019)

# check flags
unique(buoy_2019$flag_temp0p75m)

# check locations
unique(buoy_2019$location)

buoy_2019_sub <- buoy_2019 %>% 
  filter(location == 'loon') %>% 
  select(datetime,
         # round for vis an joining
         wtr_1 = waterTemperature_degC_0p75m,,
         wtr_2 = waterTemperature_degC_1p75m,
         wtr_3 = waterTemperature_degC_2p75m,
         wtr_4 = waterTemperature_degC_3p75m,
         wtr_5 = waterTemperature_degC_4p75m,
         wtr_6 = waterTemperature_degC_5p75m,
         wtr_7 = waterTemperature_degC_6p75m,
         wtr_8 = waterTemperature_degC_7p75m,
         wtr_9 = waterTemperature_degC_8p75m,
         wtr_10 = waterTemperature_degC_9p75m) 

winter_1920 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/2019-2020_wintertempstringdo_L1_v2022.csv')

names(winter_1920)

winter_end19 <- winter_1920 %>% 
  select(datetime, 
         wtr_1 = waterTemperature_DO_degC_1m,
         wtr_2 = waterTemperature_degC_2m,
         wtr_3 = waterTemperature_degC_3m,
         wtr_4 = waterTemperature_degC_4m,
         wtr_5 = waterTemperature_degC_5m,
         wtr_6 = waterTemperature_degC_6m,
         wtr_7 = waterTemperature_degC_7m,
         wtr_8 = waterTemperature_degC_8m,
         wtr_9 = waterTemperature_degC_9m,
         wtr_10 = waterTemperature_degC_10m) %>% 
  filter(datetime < ymd('2020-01-01'))


buoy_hobo_2019 <- full_join(winter_start19, buoy_2019_sub) %>%
  full_join(., winter_end19) %>% 
  select(datetime, wtr_1, wtr_2, wtr_3, wtr_4, wtr_5, wtr_6, wtr_7, wtr_8, wtr_9, wtr_10) %>% 
  filter(if_any(wtr_1:wtr_10, ~!is.na(.)))

buoy_hobo_2019 %>%
  group_by(datetime) %>% 
  slice(1) %>% 
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, "%Y-%m-%d %H:%M"))) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2019.txt", delim='\t')

wtr.temp.2019 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2019.txt")

wtr.heat.map(subset(wtr.temp.2019,
                    subset =(datetime>=as.POSIXct('2019-05-01', tz='UTC') & 
                               datetime< as.POSIXct('2019-11-01', tz='UTC'))),
             xlim=c(as.POSIXct('2019-05-01', tz='UTC'), as.POSIXct('2019-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2019$datetime, 
                                        at = (seq(as.POSIXct('2019-05-01', tz='UTC'),
                                                  as.POSIXct('2019-11-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(5,30),
             plot.title=title(main='Summer 2019',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)

wtr.heat.map(wtr.temp.2019,
             xlim=c(as.POSIXct('2019-01-01', tz='UTC'), as.POSIXct('2020-01-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2019$datetime, 
                                        at = (seq(as.POSIXct('2019-01-01', tz='UTC'),
                                                  as.POSIXct('2020-01-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(-2,30),
             plot.title=title(main='2019',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)


# 2020 temperature heatmaps ####

winter_start20 <- winter_1920 %>% 
  filter(datetime >= as.POSIXct('2020-01-01', tz='UTC')) %>% 
  select(datetime,
         wtr_1 = waterTemperature_DO_degC_1m,
         wtr_2 = waterTemperature_degC_2m,
         wtr_3 = waterTemperature_degC_3m,
         wtr_4 = waterTemperature_degC_4m,
         wtr_5 = waterTemperature_degC_5m,
         wtr_6 = waterTemperature_degC_6m,
         wtr_7 = waterTemperature_degC_7m,
         wtr_8 = waterTemperature_degC_8m,
         wtr_9 = waterTemperature_degC_9m,
         wtr_10 = waterTemperature_degC_10m) 

buoy_2020 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2020_tempstring_L1_v2022.csv')

names(buoy_2020)

# check flags
unique(buoy_2020$flag_temp0p75m)

# check locations
unique(buoy_2020$location)

buoy_2020_sub <- buoy_2020 %>% 
  filter(location == 'loon') %>% 
  select(datetime,
         # round for vis an joining
         wtr_1 = waterTemperature_degC_0p75m,
         wtr_2 = waterTemperature_degC_1p75m,
         wtr_3 = waterTemperature_degC_2p75m,
         wtr_4 = waterTemperature_degC_3p75m,
         wtr_5 = waterTemperature_degC_4p75m,
         wtr_6 = waterTemperature_degC_5p75m,
         wtr_7 = waterTemperature_degC_6p75m,
         wtr_8 = waterTemperature_degC_7p75m,
         wtr_9 = waterTemperature_degC_8p75m,
         wtr_10 = waterTemperature_degC_9p75m) 

winter_2021 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/2020-2021_wintertempstring_L1_v2022.csv')

names(winter_2021)

winter_end20 <- winter_2021 %>% 
  select(datetime, 
         wtr_2 = waterTemperature_degC_2m,
         wtr_3 = waterTemperature_degC_3m,
         wtr_4 = waterTemperature_degC_4m,
         wtr_5 = waterTemperature_degC_5m,
         wtr_6 = waterTemperature_degC_6m,
         wtr_7 = waterTemperature_degC_7m,
         wtr_8 = waterTemperature_degC_8m,
         wtr_9 = waterTemperature_degC_9m,
         wtr_10 = waterTemperature_degC_10m) %>% 
  filter(datetime < ymd('2021-01-01'))


buoy_hobo_2020 <- full_join(winter_start20, buoy_2020_sub) %>%
  full_join(., winter_end20) %>% 
  select(datetime, wtr_1, wtr_2, wtr_3, wtr_4, wtr_5, wtr_6, wtr_7, wtr_8, wtr_9, wtr_10) %>% 
  filter(if_any(wtr_1:wtr_10, ~!is.na(.)))

buoy_hobo_2020 %>%
  group_by(datetime) %>% 
  slice(1) %>% 
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, "%Y-%m-%d %H:%M"))) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2020.txt", delim='\t')

wtr.temp.2020 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2020.txt")

wtr.heat.map(subset(wtr.temp.2020,
                    subset =(datetime>=as.POSIXct('2020-05-01', tz='UTC') & 
                               datetime< as.POSIXct('2020-11-01', tz='UTC'))),
             xlim=c(as.POSIXct('2020-05-01', tz='UTC'), 
                    as.POSIXct('2020-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2019$datetime, 
                                        at = (seq(as.POSIXct('2020-05-01', tz='UTC'),
                                                  as.POSIXct('2020-11-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(5,30),
             plot.title=title(main='Summer 2020',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)

wtr.heat.map(wtr.temp.2020,
             xlim=c(as.POSIXct('2020-01-01', tz='UTC'), 
                    as.POSIXct('2021-01-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2019$datetime, 
                                        at = (seq(as.POSIXct('2020-01-01', tz='UTC'),
                                                  as.POSIXct('2021-01-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(-2,30),
             plot.title=title(main='2020',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)


# 2021 temperature heatmaps ####

winter_start21 <- winter_2021 %>% 
  filter(datetime >= as.POSIXct('2021-01-01', tz='UTC')) %>% 
  select(datetime,
         wtr_2 = waterTemperature_degC_2m,
         wtr_3 = waterTemperature_degC_3m,
         wtr_4 = waterTemperature_degC_4m,
         wtr_5 = waterTemperature_degC_5m,
         wtr_6 = waterTemperature_degC_6m,
         wtr_7 = waterTemperature_degC_7m,
         wtr_8 = waterTemperature_degC_8m,
         wtr_9 = waterTemperature_degC_9m,
         wtr_10 = waterTemperature_degC_10m) 

buoy_2021 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2021_tempstring_L1_v2022.csv')

names(buoy_2021)

# check locations
unique(buoy_2021$location)

buoy_2021_sub <- buoy_2021 %>% 
  filter(location == 'loon') %>% 
  select(datetime,
         wtr_0.1 = waterTemperature_degC_0p1m,
         wtr_1 = waterTemperature_degC_1m,
         wtr_2 = waterTemperature_degC_2m,
         wtr_3 = waterTemperature_degC_3m,
         wtr_4 = waterTemperature_degC_4m,
         wtr_5 = waterTemperature_degC_5m,
         wtr_6 = waterTemperature_degC_6m,
         wtr_7 = waterTemperature_degC_7m,
         wtr_8 = waterTemperature_degC_8m,
         wtr_9 = waterTemperature_degC_9m,
         wtr_10 = waterTemperature_degC_10m) 

winter_2122 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/2021-2022_wintertempstring_L1_v2022.csv')

names(winter_2122)

winter_end21 <- winter_2122 %>% 
  select(datetime, 
         wtr_2 = waterTemperature_degC_2m,
         wtr_3 = waterTemperature_degC_3m,
         wtr_4 = waterTemperature_degC_4m,
         wtr_5 = waterTemperature_degC_5m,
         wtr_7 = waterTemperature_degC_7m,
         wtr_8 = waterTemperature_degC_8m,
         wtr_9 = waterTemperature_degC_9m,
         wtr_10 = waterTemperature_degC_10m) %>% 
  filter(datetime < ymd('2022-01-01'))


buoy_hobo_2021 <- full_join(winter_start21, buoy_2021_sub) %>%
  full_join(., winter_end21) %>% 
  select(datetime, wtr_1, wtr_2, wtr_3, wtr_4, wtr_5, wtr_6, wtr_7, wtr_8, wtr_9, wtr_10) %>% 
  filter(if_any(wtr_1:wtr_10, ~!is.na(.)))

buoy_hobo_2021 %>%
  group_by(datetime) %>% 
  slice(1) %>% 
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, "%Y-%m-%d %H:%M"))) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2021.txt", delim='\t')

wtr.temp.2021 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2021.txt")

wtr.heat.map(subset(wtr.temp.2021,
                    subset =(datetime>=as.POSIXct('2021-05-01', tz='UTC') & 
                               datetime< as.POSIXct('2021-11-01', tz='UTC'))),
             xlim=c(as.POSIXct('2021-05-01', tz='UTC'), 
                    as.POSIXct('2021-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2019$datetime, 
                                        at = (seq(as.POSIXct('2021-05-01', tz='UTC'),
                                                  as.POSIXct('2021-11-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(5,30),
             plot.title=title(main='Summer 2021',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)

wtr.heat.map(wtr.temp.2021,
             xlim=c(as.POSIXct('2021-01-01', tz='UTC'), 
                    as.POSIXct('2022-01-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2019$datetime, 
                                        at = (seq(as.POSIXct('2021-01-01', tz='UTC'),
                                                  as.POSIXct('2022-01-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(-2,30),
             plot.title=title(main='2021',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)



# 2022 temperature heatmaps ####

winter_start22 <- winter_2122 %>% 
  filter(datetime >= as.POSIXct('2022-01-01', tz='UTC')) %>% 
  select(datetime,
         wtr_2 = waterTemperature_degC_2m,
         wtr_3 = waterTemperature_degC_3m,
         wtr_4 = waterTemperature_degC_4m,
         wtr_5 = waterTemperature_degC_5m,
         wtr_7 = waterTemperature_degC_7m,
         wtr_8 = waterTemperature_degC_8m,
         wtr_9 = waterTemperature_degC_9m,
         wtr_10 = waterTemperature_degC_10m) 

buoy_2022 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2022_tempstring_L1_v2022.csv')

names(buoy_2022)

# check locations
unique(buoy_2022$location)

buoy_2022_sub <- buoy_2022 %>% 
  filter(location == 'loon') %>% 
  select(datetime,
         wtr_0.1 = waterTemperature_degC_0p1m,
         wtr_1 = waterTemperature_degC_1m,
         wtr_2 = waterTemperature_degC_2m,
         wtr_3 = waterTemperature_degC_3m,
         wtr_4 = waterTemperature_degC_4m,
         wtr_5 = waterTemperature_degC_5m,
         wtr_6 = waterTemperature_degC_6m,
         wtr_7 = waterTemperature_degC_7m,
         wtr_8 = waterTemperature_degC_8m,
         wtr_9 = waterTemperature_degC_9m,
         wtr_10 = waterTemperature_degC_10m) 

winter_2223 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/winter/2022-2023_wintertempstring_L1_v2023.csv')

names(winter_2223)

winter_end22 <- winter_2223 %>% 
  select(datetime, 
         wtr_1 = waterTemperature_degC_1m,
         wtr_2 = waterTemperature_degC_2m,
         wtr_3 = waterTemperature_degC_3m,
         wtr_4 = waterTemperature_degC_4m,
         wtr_5 = waterTemperature_degC_5m,
         wtr_6 = waterTemperature_degC_6m,
         wtr_7 = waterTemperature_degC_7m,
         wtr_8 = waterTemperature_degC_8m,
         wtr_9 = waterTemperature_degC_9m,
         wtr_10 = waterTemperature_degC_10m) %>% 
  filter(datetime < ymd('2023-01-01'))


buoy_hobo_2022 <- full_join(winter_start22, buoy_2022_sub) %>%
  full_join(., winter_end22) %>% 
  select(datetime, wtr_1, wtr_2, wtr_3, wtr_4, wtr_5, wtr_6, wtr_7, wtr_8, wtr_9, wtr_10) %>% 
  filter(if_any(wtr_1:wtr_10, ~!is.na(.)))

buoy_hobo_2022 %>%
  group_by(datetime) %>% 
  slice(1) %>% 
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, "%Y-%m-%d %H:%M"))) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2022.txt", delim='\t')

wtr.temp.2022 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2022.txt")

wtr.heat.map(subset(wtr.temp.2022,
                    subset =(datetime>=as.POSIXct('2022-05-01', tz='UTC') & 
                               datetime< as.POSIXct('2022-11-01', tz='UTC'))),
             xlim=c(as.POSIXct('2022-05-01', tz='UTC'), 
                    as.POSIXct('2022-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2019$datetime, 
                                        at = (seq(as.POSIXct('2022-05-01', tz='UTC'),
                                                  as.POSIXct('2022-11-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(5,30),
             plot.title=title(main='Summer 2022',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)

wtr.heat.map(wtr.temp.2022,
             xlim=c(as.POSIXct('2022-01-01', tz='UTC'), 
                    as.POSIXct('2023-01-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2019$datetime, 
                                        at = (seq(as.POSIXct('2022-01-01', tz='UTC'),
                                                  as.POSIXct('2023-01-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(-2,30),
             plot.title=title(main='2022',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)

