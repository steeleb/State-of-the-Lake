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

buoy_2014_sub %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, '%Y-%m-%d %H:%M'))) %>% 
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



# 2015 temperature heatmaps####

buoy_2015 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2015_tempstring_L1_v2022.csv')

names(buoy_2015)

# check locations - we only want data at loon
unique(buoy_2015$location)

buoy_2015_sub <- buoy_2015 %>% 
  filter(location == "loon") 

buoy_2015_sub <- buoy_2015_sub %>% 
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

buoy_2015_sub %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, '%Y-%m-%d %H:%M'))) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2015.txt", delim='\t')

wtr.temp.2015 <- load.ts(fPath = "C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2015.txt",
                         tz = "UTC")

wtr.heat.map(wtr.temp.2015,
             xlim=c(as.POSIXct('2015-05-01', tz='UTC'), as.POSIXct('2015-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2015$datetime, 
                                        at = (seq(as.POSIXct('2015-05-01', tz='UTC'),
                                                  as.POSIXct('2015-11-01', tz='UTC'), 
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


# 2016 temperature heatmaps####

buoy_2016 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2016_tempstring_L1_v2022.csv')

names(buoy_2016)

# check flags
unique(buoy_2016$flag_temp0p5m)

# check locations - we only want data at loon
unique(buoy_2016$location)

buoy_2016_sub <- buoy_2016 %>% 
  filter(location == "loon") 

buoy_2016_sub <- buoy_2016_sub %>% 
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

buoy_2016_sub %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, '%Y-%m-%d %H:%M'))) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2016.txt", delim='\t')

wtr.temp.2016 <- load.ts(fPath = "C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2016.txt",
                         tz = "UTC")

wtr.heat.map(wtr.temp.2016,
             xlim=c(as.POSIXct('2016-05-01', tz='UTC'), as.POSIXct('2016-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2016$datetime, 
                                        at = (seq(as.POSIXct('2016-05-01', tz='UTC'),
                                                  as.POSIXct('2016-11-01', tz='UTC'), 
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


# 2017 temperature heatmaps####

buoy_2017 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/tempstring/2017_tempstring_L1_v2022.csv')

names(buoy_2017)

# check flags 
unique(buoy_2017$flag_temp0p85m)

# check locations - we only want data at loon
unique(buoy_2017$location)

buoy_2017_sub <- buoy_2017 %>% 
  filter(location == "loon") 

buoy_2017_sub <- buoy_2017_sub %>% 
  select(datetime,
         wtr_0.8 = waterTemperature_degC_0p85m,
         wtr_1.8 = waterTemperature_degC_1p85m,
         wtr_2.8 = waterTemperature_degC_2p85m,
         wtr_3.8 = waterTemperature_degC_3p85m,
         wtr_4.8 = waterTemperature_degC_4p85m,
         wtr_5.8 = waterTemperature_degC_5p85m,
         wtr_6.8 = waterTemperature_degC_6p85m,
         wtr_7.8 = waterTemperature_degC_7p85m,
         wtr_8.8 = waterTemperature_degC_8p85m,
         wtr_9.8 = waterTemperature_degC_9p85m)

buoy_2017_sub %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(format(datetime, '%Y-%m-%d %H:%M'))) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2017.txt", delim='\t')

wtr.temp.2017 <- load.ts(fPath = "C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_heatmaps/water_temp_buoy_2017.txt",
                         tz = "UTC")

wtr.heat.map(wtr.temp.2017,
             xlim=c(as.POSIXct('2017-05-01', tz='UTC'), as.POSIXct('2017-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2017$datetime, 
                                        at = (seq(as.POSIXct('2017-05-01', tz='UTC'),
                                                  as.POSIXct('2017-11-01', tz='UTC'), 
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











