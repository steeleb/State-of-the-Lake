####2007####

buoy_2007 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007_tempstring_L1.csv')

unique(buoy_2007$temp_flag)
unique(buoy_2007$location)

buoy_2007_sub <- buoy_2007 %>% 
  mutate_at(vars(TempC_9m, TempC_10m, TempC_11m),
            funs(case_when(temp_flag == '9.5d, 10.5d, 11.5d, 13.5d' ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(TempC_9m = case_when(temp_flag == '9.5d, 10.5d, 11.5d, 13.5d' ~ TempC_13m,
                              TRUE ~ TempC_9m)) %>% 
  mutate(TempC_13m = case_when(temp_flag == '9.5d, 10.5d, 11.5d, 13.5d' ~ NA_real_,
                                    TRUE ~ TempC_13m)) %>% 
  mutate_at(vars(TempC_0m:TempC_13m),
            funs(case_when(location == 'offline' ~ NA_real_,
                           TRUE ~ .)))
             

buoy_2007_sub <- buoy_2007_sub %>% 
  rename(wtr_0.5 = `TempC_0m`,
         wtr_1.0 = `TempC_0p5m`,
         wtr_1.5 = `TempC_1m`,
         wtr_2.0 = `TempC_1p5m`,
         wtr_2.5 = `TempC_2m`,
         wtr_3.0 = `TempC_2p5m`,
         wtr_3.5 = `TempC_3m`,
         wtr_4.5 = `TempC_4m`,
         wtr_5.5 = `TempC_5m`,
         wtr_6.5 = `TempC_6m`,
         wtr_7.5 = `TempC_7m`,
         wtr_8.5 = `TempC_8m`,
         wtr_9.5 = `TempC_9m`,
         wtr_10.5 = `TempC_10m`,
         wtr_11.5 = `TempC_11m`,
         wtr_13.5 = `TempC_13m`) %>% 
  select(-temp_flag, -location)  

buoy_2007_sub %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(datetime)) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2007.txt", delim='\t')

wtr.temp.2007 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2007.txt")

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

####2008####
buoy_2008 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2008_tempstring_L1.csv')

unique(buoy_2008$temp_flag)
unique(buoy_2008$location)

buoy_2008_sub <- buoy_2008 %>% 
  mutate_at(vars(TempC_9m, TempC_10m, TempC_11m),
            funs(case_when(temp_flag == '9.5d, 10.5d, 11.5d, 13.5d' ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(TempC_9m = case_when(temp_flag == '9.5d, 10.5d, 11.5d, 13.5d' ~ TempC_13m,
                              TRUE ~ TempC_9m)) %>% 
  mutate(TempC_13m = case_when(temp_flag == '9.5d, 10.5d, 11.5d, 13.5d' ~ NA_real_,
                               TRUE ~ TempC_13m)) %>% 
  mutate_at(vars(TempC_0m:TempC_13m),
            funs(case_when(location == 'offline' ~ NA_real_,
                           TRUE ~ .)))


buoy_2008_sub <- buoy_2008_sub %>% 
  rename(wtr_0.5 = `TempC_0m`,
         wtr_1.0 = `TempC_0p5m`,
         wtr_1.5 = `TempC_1m`,
         wtr_2.0 = `TempC_1p5m`,
         wtr_2.5 = `TempC_2m`,
         wtr_3.0 = `TempC_2p5m`,
         wtr_3.5 = `TempC_3m`,
         wtr_4.5 = `TempC_4m`,
         wtr_5.5 = `TempC_5m`,
         wtr_6.5 = `TempC_6m`,
         wtr_7.5 = `TempC_7m`,
         wtr_8.5 = `TempC_8m`,
         wtr_9.5 = `TempC_9m`,
         wtr_10.5 = `TempC_10m`,
         wtr_11.5 = `TempC_11m`,
         wtr_13.5 = `TempC_13m`) %>% 
  select(-temp_flag, -location)  

buoy_2008_sub %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(datetime)) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2008.txt", delim='\t')

wtr.temp.2008 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2008.txt")

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

####2009####
buoy_2009 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2009_tempstring_L1.csv')

unique(buoy_2009$temp_flag)
unique(buoy_2009$location)

buoy_2009_sub <- buoy_2009 %>% 
  mutate_at(vars(TempC_9m, TempC_10m, TempC_11m, TempC_13m),
            funs(case_when(temp_flag == 'i, 9.5d, 10.5d, 11.5d, 13.5d' ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(TempC_9m, TempC_10m, TempC_11m),
            funs(case_when(temp_flag == '9.5d, 10.5d, 11.5d, 13.5d' ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(TempC_9m = case_when(temp_flag == '9.5d, 10.5d, 11.5d, 13.5d' ~ TempC_13m,
                              TRUE ~ TempC_9m)) %>% 
  mutate(TempC_13m = case_when(temp_flag == '9.5d, 10.5d, 11.5d, 13.5d' ~ NA_real_,
                               TRUE ~ TempC_13m)) %>% 
  mutate_at(vars(TempC_0m:TempC_13m),
            funs(case_when(location == 'offline' ~ NA_real_,
                           temp_flag == 'q' ~ NA_real_,
                           TRUE ~ .)))


buoy_2009_sub <- buoy_2009_sub %>% 
  rename(wtr_0.5 = `TempC_0m`,
         wtr_1.0 = `TempC_0p5m`,
         wtr_1.5 = `TempC_1m`,
         wtr_2.0 = `TempC_1p5m`,
         wtr_2.5 = `TempC_2m`,
         wtr_3.0 = `TempC_2p5m`,
         wtr_3.5 = `TempC_3m`,
         wtr_4.5 = `TempC_4m`,
         wtr_5.5 = `TempC_5m`,
         wtr_6.5 = `TempC_6m`,
         wtr_7.5 = `TempC_7m`,
         wtr_8.5 = `TempC_8m`,
         wtr_9.5 = `TempC_9m`,
         wtr_10.5 = `TempC_10m`,
         wtr_11.5 = `TempC_11m`,
         wtr_13.5 = `TempC_13m`) %>% 
  select(-temp_flag, -location)  

buoy_2009_sub %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(datetime)) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2009.txt", delim='\t')

wtr.temp.2009 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2009.txt")

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


####2010####
buoy_2010 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2010_tempstring_L1.csv')

unique(buoy_2010$temp_flag)
unique(buoy_2010$location)

buoy_2010_sub <- buoy_2010 %>% 
  mutate(TempC_0p5m = case_when(temp_flag == 'q0.5m' ~ NA_real_,
                              TRUE ~ TempC_0p5m)) %>% 
  mutate_at(vars(TempC_0p5m:TempC_13m),
            funs(case_when(location == 'offline' ~ NA_real_,
                           location == 'in transit' ~ NA_real_,
                           location == 'harbor' ~ NA_real_,
                           temp_flag == 'q' ~ NA_real_,
                           TRUE ~ .)))


buoy_2010_sub <- buoy_2010_sub %>% 
  rename(wtr_1.0 = `TempC_0p5m`,
         wtr_1.5 = `TempC_1m`,
         wtr_2.0 = `TempC_1p5m`,
         wtr_2.5 = `TempC_2m`,
         wtr_3.0 = `TempC_2p5m`,
         wtr_3.5 = `TempC_3m`,
         wtr_4.5 = `TempC_4m`,
         wtr_5.5 = `TempC_5m`,
         wtr_6.5 = `TempC_6m`,
         wtr_7.5 = `TempC_7m`,
         wtr_8.5 = `TempC_8m`,
         wtr_9.5 = `TempC_9m`,
         wtr_10.5 = `TempC_10m`,
         wtr_11.5 = `TempC_11m`,
         wtr_13.5 = `TempC_13m`) %>% 
  select(-TempC_0m, -temp_flag, -location)  

buoy_2010_sub %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(datetime)) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2010.txt", delim='\t')

wtr.temp.2010 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2010.txt")

wtr.heat.map(wtr.temp.2010,
             xlim=c(as.POSIXct('2010-05-01', tz='UTC'), as.POSIXct('2010-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2010$datetime, 
                                        at = (seq(as.POSIXct('2010-05-01', tz='UTC'),
                                                  as.POSIXct('2010-11-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(5,30),
             plot.title=title(main='2010',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)

####2011####
buoy_2011 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2011_tempstring_L1.csv',
                      col_types = 'Tcnnnnnnnnnn')

unique(buoy_2011$location)

buoy_2011_sub <- buoy_2011 %>% 
  mutate_at(vars(TempC_0m:TempC_9m),
            funs(case_when(location == 'offline' ~ NA_real_,
                           location == 'in transit' ~ NA_real_,
                           location == 'harbor' ~ NA_real_,
                           TRUE ~ .)))


buoy_2011_sub <- buoy_2011_sub %>% 
  rename(wtr_0.5 = `TempC_0m`,
         wtr_1.5 = `TempC_1m`,
         wtr_2.5 = `TempC_2m`,
         wtr_3.5 = `TempC_3m`,
         wtr_4.5 = `TempC_4m`,
         wtr_5.5 = `TempC_5m`,
         wtr_6.5 = `TempC_6m`,
         wtr_7.5 = `TempC_7m`,
         wtr_8.5 = `TempC_8m`,
         wtr_9.5 = `TempC_9m`) %>% 
  select(-location)  

buoy_2011_sub %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(datetime)) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2011.txt", delim='\t')

wtr.temp.2011 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2011.txt")

wtr.heat.map(wtr.temp.2011,
             xlim=c(as.POSIXct('2011-05-01', tz='UTC'), as.POSIXct('2011-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2011$datetime, 
                                        at = (seq(as.POSIXct('2011-05-01', tz='UTC'),
                                                  as.POSIXct('2011-11-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(5,30),
             plot.title=title(main='2011',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)

####2013####
buoy_2013 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2013_tempstring_L1.csv',
                      col_types = 'Tcnnnnnnnnnn')

unique(buoy_2013$location)

buoy_2013_sub <- buoy_2013 %>% 
  mutate_at(vars(TempC_0m:TempC_9m),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           location == 'harbor' ~ NA_real_,
                           location == 'offline' ~ NA_real_,
                           TRUE ~ .)))


buoy_2013_sub <- buoy_2013_sub %>% 
  rename(wtr_0.5 = `TempC_0m`,
         wtr_1.5 = `TempC_1m`,
         wtr_2.5 = `TempC_2m`,
         wtr_3.5 = `TempC_3m`,
         wtr_4.5 = `TempC_4m`,
         wtr_5.5 = `TempC_5m`,
         wtr_6.5 = `TempC_6m`,
         wtr_7.5 = `TempC_7m`,
         wtr_8.5 = `TempC_8m`,
         wtr_9.5 = `TempC_9m`) %>% 
  select(-location)  

buoy_2013_sub %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(datetime)) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2013.txt", delim='\t')

wtr.temp.2013 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2013.txt")

wtr.heat.map(wtr.temp.2013,
             xlim=c(as.POSIXct('2013-05-01', tz='UTC'), as.POSIXct('2013-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2013$datetime, 
                                        at = (seq(as.POSIXct('2013-05-01', tz='UTC'),
                                                  as.POSIXct('2013-11-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(5,30),
             plot.title=title(main='2013',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)


####2014####
buoy_2014 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2014_tempstring_L1.csv',
                      col_types = 'Tcnnnnnnnnnnnc')

unique(buoy_2014$location)
unique(buoy_2014$temp_flag)

buoy_2014_sub <- buoy_2014 %>% 
  mutate_at(vars(TempC_0m:TempC_10m),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           location == 'harbor' ~ NA_real_,
                           location == 'offline' ~ NA_real_,
                           temp_flag == 'i' ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(TempC_10m = case_when(temp_flag == '10d, 10n' ~ NA_real_,
                               TRUE ~ TempC_10m))


buoy_2014_sub <- buoy_2014_sub %>% 
  rename(wtr_0.5 = `TempC_0m`,
         wtr_1.5 = `TempC_1m`,
         wtr_2.5 = `TempC_2m`,
         wtr_3.5 = `TempC_3m`,
         wtr_4.5 = `TempC_4m`,
         wtr_5.5 = `TempC_5m`,
         wtr_6.5 = `TempC_6m`,
         wtr_7.5 = `TempC_7m`,
         wtr_8.5 = `TempC_8m`,
         wtr_9.5 = `TempC_9m`) %>% 
  select(-location, -temp_flag, -TempC_10m)  

buoy_2014_sub %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(datetime)) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2014.txt", delim='\t')

wtr.temp.2014 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2014.txt")

wtr.heat.map(subset(wtr.temp.2014,
                    subset =(datetime>=as.POSIXct('2014-05-01', tz='UTC') & 
                               datetime< as.POSIXct('2014-11-01', tz='UTC'))),
             xlim=c(as.POSIXct('2014-05-01', tz='UTC'), as.POSIXct('2014-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2014$datetime, 
                                        at = (seq(as.POSIXct('2014-05-01', tz='UTC'),
                                                  as.POSIXct('2014-11-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(5,30),
             plot.title=title(main='2014',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)

####2015####
buoy_2015 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015_tempstring_L1.csv',
                      col_types = 'Tnnnnnnnnnncc')

unique(buoy_2015$location)
unique(buoy_2015$temp_flag)

buoy_2015_sub <- buoy_2015 %>% 
  mutate_at(vars(TempC_0m:TempC_9m),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           location == 'harbor' ~ NA_real_,
                           location == 'offline' ~ NA_real_,
                           temp_flag == 'i' ~ NA_real_,
                           TRUE ~ .))) 

buoy_2015_sub <- buoy_2015_sub %>% 
  rename(wtr_0.5 = `TempC_0m`,
         wtr_1.5 = `TempC_1m`,
         wtr_2.5 = `TempC_2m`,
         wtr_3.5 = `TempC_3m`,
         wtr_4.5 = `TempC_4m`,
         wtr_5.5 = `TempC_5m`,
         wtr_6.5 = `TempC_6m`,
         wtr_7.5 = `TempC_7m`,
         wtr_8.5 = `TempC_8m`,
         wtr_9.5 = `TempC_9m`) %>% 
  select(-location, -temp_flag) %>% 
  filter(datetime < as.POSIXct('2015-08-07 14:15', tz='UTC'))

hobo_2015 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015_hobotempstring_L1.csv',
                      col_types = 'Tnnnnnnnnn')

hobo_2015_sub <- hobo_2015 %>% 
  rename(wtr_1.5 = `TempC_1m`,
         wtr_2.5 = `TempC_2m`,
         wtr_3.5 = `TempC_3m`,
         wtr_4.5 = `TempC_4m`,
         wtr_5.5 = `TempC_5m`,
         wtr_6.5 = `TempC_6m`,
         wtr_7.5 = `TempC_7m`,
         wtr_8.5 = `TempC_8m`,
         wtr_9.5 = `TempC_9m`) %>% 
  select(-wtr_2.5, -wtr_6.5)  

#winter hobo
winter1516 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015-2016_hobotempstring_L1.csv',
                      col_types = 'Tnnnnnnnnn')

winter1516_sub <- winter1516 %>% 
  rename(wtr_1.5 = `TempC_1m`,
         wtr_2.5 = `TempC_2m`,
         wtr_3.5 = `TempC_3m`,
         wtr_4.5 = `TempC_4m`,
         wtr_5.5 = `TempC_5m`,
         wtr_6.5 = `TempC_6m`,
         wtr_7.5 = `TempC_7m`,
         wtr_8.5 = `TempC_8m`,
         wtr_9.5 = `TempC_9m`) 

buoy_hobo_2015 <- full_join(buoy_2015_sub, hobo_2015_sub) %>% 
  full_join(., winter1516_sub) %>% 
  select(-wtr_2.5, -wtr_6.5)

buoy_hobo_2015 %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(datetime)) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2015.txt", delim='\t')

wtr.temp.2015 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2015.txt")

wtr.heat.map(subset(wtr.temp.2015,
                    subset =(datetime>=as.POSIXct('2015-05-01', tz='UTC') & 
                               datetime< as.POSIXct('2015-11-01', tz='UTC'))),
             xlim=c(as.POSIXct('2015-05-01', tz='UTC'), as.POSIXct('2015-11-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2015$datetime, 
                                        at = (seq(as.POSIXct('2015-05-01', tz='UTC'),
                                                  as.POSIXct('2015-11-01', tz='UTC'), 
                                                  by = "month")), 
                                        format = "%b"); 
               axis(2) },
             zlim=c(5,30),
             plot.title=title(main='2015',
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



####2016####
early16 <- winter1516 %>% 
  filter(datetime >= as.POSIXct('2016-01-01', tz='UTC')) %>% 
  rename(wtr_1.5 = `TempC_1m`,
         wtr_2.5 = `TempC_2m`,
         wtr_3.5 = `TempC_3m`,
         wtr_4.5 = `TempC_4m`,
         wtr_5.5 = `TempC_5m`,
         wtr_6.5 = `TempC_6m`,
         wtr_7.5 = `TempC_7m`,
         wtr_8.5 = `TempC_8m`,
         wtr_9.5 = `TempC_9m`) 

#through 2016-05-03 11:00:00

buoy_2016 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2016_tempstring_L1.csv',
                      col_types = 'Tnnnnnnnnnnnc')

unique(buoy_2016$location)

buoy_2016_sub <- buoy_2016 %>% 
  mutate_at(vars(TempC_0m:TempC_9m),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           location == 'harbor' ~ NA_real_,
                           TRUE ~ .))) %>% 
  select(-Chlor_RFU) %>% 
  filter(datetime>as.POSIXct('2016-05-03 11:00:00', tz='UTC'))

buoy_2016_sub <- buoy_2016_sub %>% 
  rename(wtr_0.5 = `TempC_0m`,
         wtr_1.5 = `TempC_1m`,
         wtr_2.5 = `TempC_2m`,
         wtr_3.5 = `TempC_3m`,
         wtr_4.5 = `TempC_4m`,
         wtr_5.5 = `TempC_5m`,
         wtr_6.5 = `TempC_6m`,
         wtr_7.5 = `TempC_7m`,
         wtr_8.5 = `TempC_8m`,
         wtr_9.5 = `TempC_9m`) %>% 
  select(-location)  %>% 
  filter(datetime <= as.POSIXct('2016-10-12 12:00', tz='UTC'))

#winter hobo
winter1617 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2016-2017_hobotempstring_L1.csv',
                       col_types = 'Tnnnnnnnnn')

winter1617_sub <- winter1617 %>% 
  rename(wtr_1.5 = `TempC_1m`,
         wtr_2.5 = `TempC_2m`,
         wtr_3.5 = `TempC_3m`,
         wtr_4.5 = `TempC_4m`,
         wtr_5.5 = `TempC_5m`,
         wtr_6.5 = `TempC_6m`,
         wtr_7.5 = `TempC_7m`,
         wtr_8.5 = `TempC_8m`,
         wtr_9.5 = `TempC_9m`) 

buoy_hobo_2016 <- full_join(early16, buoy_2016_sub) %>% 
  full_join(., winter1617_sub) 

buoy_hobo_2016 %>%
  filter(datetime < as.POSIXct('2017-01-01', tz='UTC')) %>% 
  arrange(datetime) %>%
  mutate(datetime = as.character(datetime)) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2016.txt", delim='\t')

wtr.temp.2016 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2016.txt")

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
             plot.title=title(main='2016',
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

####2017####
early17 <- winter1617 %>% 
  filter(datetime >= as.POSIXct('2017-01-01', tz='UTC')) %>% 
  rename(wtr_1.5 = `TempC_1m`,
         wtr_2.5 = `TempC_2m`,
         wtr_3.5 = `TempC_3m`,
         wtr_4.5 = `TempC_4m`,
         wtr_5.5 = `TempC_5m`,
         wtr_6.5 = `TempC_6m`,
         wtr_7.5 = `TempC_7m`,
         wtr_8.5 = `TempC_8m`,
         wtr_9.5 = `TempC_9m`) 

#through 2017-05-17 10:15:00

buoy_2017 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2017_tempstring_L1.csv',
                      col_types = 'Tnnnnnnnnnnc')

unique(buoy_2017$location)

buoy_2017_sub <- buoy_2017 %>% 
  mutate_at(vars(TempC_0m:TempC_9m),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           location == 'harbor' ~ NA_real_,
                           location == 'offline' ~ NA_real_,
                           location == 'harbor, water sensors offline' ~ NA_real_,
                           TRUE ~ .))) #%>% 
  #filter(datetime>as.POSIXct('2017-05-17 10:15:00', tz='UTC'))

buoy_2017_sub <- buoy_2017_sub %>% 
  rename(wtr_1.5 = `TempC_1m`,
         wtr_2.5 = `TempC_2m`,
         wtr_3.5 = `TempC_3m`,
         wtr_4.5 = `TempC_4m`,
         wtr_5.5 = `TempC_5m`,
         wtr_6.5 = `TempC_6m`,
         wtr_7.5 = `TempC_7m`,
         wtr_8.5 = `TempC_8m`,
         wtr_9.5 = `TempC_9m`) %>% 
  select(-location, -TempC_0m)  

#no winter hobo yet


#errors when merging for some reason - just going to display buoy data

buoy_2017_sub %>%
  arrange(datetime) %>%
  mutate(datetime = as.character(datetime)) %>% 
  write_delim("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2017.txt", delim='\t')

wtr.temp.2017 <- load.ts("C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/heatmap files/water_temp_buoy_2017.txt")

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
             plot.title=title(main='2017',
                              ylab="depth (m)",
                              xlab=NULL),
             key.title=title(main="water\ntemp\n(C)",
                             font.main=1,
                             cex.main=1)
)

wtr.heat.map(wtr.temp.2017,
             xlim=c(as.POSIXct('2017-01-01', tz='UTC'), as.POSIXct('2017-01-01', tz='UTC')),
             plot.axes = { axis.POSIXct(side=1, 
                                        x=wtr.temp.2017$datetime, 
                                        at = (seq(as.POSIXct('2017-01-01', tz='UTC'),
                                                  as.POSIXct('2017-01-01', tz='UTC'), 
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
