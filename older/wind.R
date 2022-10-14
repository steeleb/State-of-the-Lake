#### 2007 ###

wind_2007 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2007_wind_L1.csv',
                      col_types = 'Tcnnc')

unique(wind_2007$wind_dir_flag)
unique(wind_2007$location)

wind_2007_sub <- wind_2007 %>% 
  mutate(WindDir_deg = case_when(wind_dir_flag == 'e' ~ NA_real_,
                                 TRUE ~ WindDir_deg)) %>% 
  mutate_at(vars(WindDir_deg, WindSp_ms),
            funs(case_when(location == 'offline' ~ NA_real_,
                           TRUE ~ .))) %>% 
  select(datetime, WindSp_ms, WindDir_deg) %>% 
  rename(date = 'datetime')

str(wind_2007_sub)

summaryPlot(wind_2007_sub)

#### 2008 ###

wind_2008 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2008_wind_L1.csv',
                      col_types = 'Tcnnc')

unique(wind_2008$wind_dir_flag)
unique(wind_2008$location)

wind_2008_sub <- wind_2008 %>% 
  mutate(WindDir_deg = case_when(wind_dir_flag == 'e' ~ NA_real_,
                                 TRUE ~ WindDir_deg)) %>% 
  mutate_at(vars(WindDir_deg, WindSp_ms),
            funs(case_when(location == 'offline' ~ NA_real_,
                           TRUE ~ .))) %>% 
  select(-location, -wind_dir_flag) %>% 
  rename(date = 'datetime')

str(wind_2008_sub)

summaryPlot(wind_2008_sub)

#### 2009 ###

wind_2009 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2009_wind_L1.csv',
                      col_types = 'Tcnnnnc')

unique(wind_2009$wind_dir_flag)
unique(wind_2009$location)

wind_2009_sub <- wind_2009 %>% 
  mutate_at(vars(WindDir_deg, AveWindDir_deg),
            funs(case_when(wind_dir_flag == 'e' ~ NA_real_,
                                 TRUE ~ .))) %>% 
  mutate_at(vars(WindDir_deg, WindSp_ms, AveWindDir_deg, AveWindSp_ms),
            funs(case_when(location == 'offline' ~ NA_real_,
                           TRUE ~ .))) %>% 
  rename(date = 'datetime')

str(wind_2009_sub)

summaryPlot(wind_2009_sub,
            main = '2009')

wind_2009_rose <- wind_2009_sub %>% 
  rename(ws = 'WindSp_ms',
         wd = 'WindDir_deg')

pollutionRose(wind_2009_rose, 
              pollutant = 'ws',
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2009 Instantaneous')

wind_2009_rose_ave <- wind_2009_sub %>% 
  rename(ws = 'AveWindSp_ms',
         wd = 'AveWindDir_deg')

pollutionRose(wind_2009_rose_ave, 
              pollutant = 'ws',
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2009 Average')


#### 2010 ###

wind_2010 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2010_wind_L1.csv',
                      col_types = 'Tcnnnn')

unique(wind_2010$location)

wind_2010_sub <- wind_2010 %>% 
  mutate_at(vars(WindDir_deg, WindSp_ms, AveWindDir_deg, AveWindSp_ms),
            funs(case_when(location == 'offline' ~ NA_real_,
                           location == 'in transit' ~ NA_real_,
                           TRUE ~ .))) %>% 
  rename(date = 'datetime')

str(wind_2010_sub)

summaryPlot(wind_2010_sub,
            main = '2010') #900x500

wind_2010_rose <- wind_2010_sub %>% 
  rename(ws = 'WindSp_ms',
         wd = 'WindDir_deg')

pollutionRose(wind_2010_rose, 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2010 Instantaneous') #500x500

pollutionRose(wind_2010_rose, 
              pollutant = 'ws', 
              type = 'month',
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2010 Instantaneous Values') #750x600

wind_2010_rose2 <- wind_2010_sub %>% 
  rename(ws = 'AveWindSp_ms',
         wd = 'AveWindDir_deg')

pollutionRose(wind_2010_rose2, 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2010 Average') #500x500

pollutionRose(wind_2010_rose2, 
              pollutant = 'ws', 
              type = 'month',
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2010 Average Values')

pollutionRose(subset(wind_2010_rose2,
                     subset = location == 'loon'), 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2010 Average Values, Loon') #500x500
pollutionRose(subset(wind_2010_rose2,
                     subset = (location == 'harbor')), 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2010 Average Values, Harbor') #500x500





#### 2011 ####

wind_2011 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2011_wind_L1.csv',
                      col_types = 'Tcnn')

unique(wind_2011$location)

wind_2011_sub <- wind_2011 %>% 
  mutate_at(vars(WindDir_deg, WindSp_ms),
            funs(case_when(location == 'offline' ~ NA_real_,
                           location == 'in transit' ~ NA_real_,
                           TRUE ~ .))) %>% 
  rename(date = 'datetime')

str(wind_2011_sub)

summaryPlot(wind_2011_sub,
            main = '2011') #900x350

wind_2011_rose <- wind_2011_sub %>% 
  rename(ws = 'WindSp_ms',
         wd = 'WindDir_deg')

pollutionRose(wind_2011_rose, 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2011 Instantaneous Values')

pollutionRose(wind_2011_rose, 
              pollutant = 'ws', 
              type = 'month',
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2011 Instantaneous Values') #750x600

pollutionRose(wind_2011_rose, 
              pollutant = 'ws', 
              type = 'season',
              main = '2011 Instantaneous Values')


pollutionRose(subset(wind_2011_rose,
                     subset = location == 'loon'), 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2011 Instantaneous Values, Loon') #500x500

pollutionRose(subset(wind_2011_rose,
                     subset = (location == 'harbor')), 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2011 Instantaneous Values, Harbor') #500x500


#### 2012 ####

wind_2012 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2012_wind_L1.csv',
                      col_types = 'Tcnn')

unique(wind_2012$location)

wind_2012_sub <- wind_2012 %>% 
  mutate_at(vars(WindDir_deg, WindSp_ms),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .))) %>% 
  rename(date = 'datetime')

str(wind_2012_sub)

summaryPlot(wind_2012_sub,
            main = '2012') #900x350

wind_2012_rose <- wind_2012_sub %>% 
  rename(ws = 'WindSp_ms',
         wd = 'WindDir_deg')

pollutionRose(wind_2012_rose, 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2012 Instantaneous Values') #500x500

pollutionRose(wind_2012_rose, 
              pollutant = 'ws', 
              type = 'month',
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2012 Instantaneous Values') #750x600

pollutionRose(subset(wind_2012_rose,
                     subset = location == 'loon'), 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2012 Instantaneous Values, Loon') #500x500
pollutionRose(subset(wind_2012_rose,
                     subset = (location == 'harbor')), 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2012 Instantaneous Values, Harbor') #500x500



#### 2013 ####

wind_2013 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2013_wind_L1.csv',
                      col_types = 'Tcnnnnnn')

unique(wind_2013$location)

wind_2013_sub <- wind_2013 %>% 
  mutate_at(vars(WindDir_deg, WindSp_ms, AveWindSp_ms, AveWindDir_deg, MaxWindDir_deg, MaxWindSp_ms),
            funs(case_when(location == 'offline' ~ NA_real_,
                           location == 'in transit' ~ NA_real_,
                           TRUE ~ .))) %>% 
  rename(date = 'datetime') %>% 
  select(-MaxWindDir_deg, -MaxWindSp_ms)

str(wind_2013_sub)

summaryPlot(wind_2013_sub,
            main = '2013') #900x5000

wind_2013_rose <- wind_2013_sub %>% 
  rename(ws = 'WindSp_ms',
         wd = 'WindDir_deg')

pollutionRose(wind_2013_rose, 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2013 Instantaneous Values') #500x500

pollutionRose(wind_2013_rose, 
              pollutant = 'ws', 
              type = 'month',
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2013 Instantaneous Values') #750x600


wind_2013_rose_ave <- wind_2013_sub %>% 
  rename(ws = 'AveWindSp_ms',
         wd = 'AveWindDir_deg')

pollutionRose(wind_2013_rose_ave, 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2013 Average Values') #500x500
pollutionRose(wind_2013_rose_ave, 
              pollutant = 'ws', 
              type = 'month',
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2013 Average Values') #750x600

pollutionRose(subset(wind_2013_rose_ave,
                     subset = location == 'loon'), 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2013 Average Values, Loon') #500x500
pollutionRose(subset(wind_2013_rose_ave,
                     subset = (location == 'harbor')), 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2013 Average Values, Harbor') #500x500



#### 2014 ###

wind_2014 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2014_wind_L1.csv',
                      col_types = 'Tcnnnnnn')

unique(wind_2014$location)

wind_2014_sub <- wind_2014 %>% 
  mutate_at(vars(InstWindDir, InstWindSp, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(location == 'offline' ~ NA_real_,
                           location == 'in transit' ~ NA_real_,
                           TRUE ~ .))) %>% 
  rename(date = 'datetime')%>% 
  select(-MaxWindDir, -MaxWindSp)

str(wind_2014_sub)

summaryPlot(wind_2014_sub,
            main = '2014') #900x650

wind_2014_rose <- wind_2014_sub %>% 
  rename(ws = 'InstWindSp',
         wd = 'InstWindDir')

pollutionRose(wind_2014_rose, 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2014 Instantaneous Values') #500x500
pollutionRose(wind_2014_rose, 
              pollutant = 'ws', 
              type = 'month',
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2014 Instantaneous Values') #750x600



wind_2014_rose_ave <- wind_2014_sub %>% 
  rename(ws = 'AveWindSp',
         wd = 'AveWindDir')

pollutionRose(wind_2014_rose_ave, 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2014 Average Values') #500x500
pollutionRose(wind_2014_rose_ave, 
              pollutant = 'ws', 
              type = 'month',
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2014 Average Values') #750x600

pollutionRose(subset(wind_2014_rose_ave,
                     subset = location == 'loon'), 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2014 Average Values, Loon') #500x500
pollutionRose(subset(wind_2014_rose_ave,
                     subset = (location == 'harbor')), 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2014 Average Values, Harbor') #500x500




#### 2015 ###

wind_2015 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2015_wind_L1.csv',
                      col_types = 'Tnnnnnnc')

unique(wind_2015$location)

wind_2015_sub <- wind_2015 %>% 
  mutate_at(vars(InstWindDir, InstWindSp, AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .))) %>% 
  rename(date = 'datetime')%>% 
  select(-MaxWindDir, -MaxWindSp)

str(wind_2015_sub)

summaryPlot(wind_2015_sub,
            main = '2015') #900x650

wind_2015_rose <- wind_2015_sub %>% 
  rename(ws = 'InstWindSp',
         wd = 'InstWindDir')

pollutionRose(wind_2015_rose, 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2015 Instantaneous Values') #500x500
pollutionRose(wind_2015_rose, 
              pollutant = 'ws', 
              type = 'month',
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2015 Instantaneous Values') #750x600



wind_2015_rose_ave <- wind_2015_sub %>% 
  rename(ws = 'AveWindSp',
         wd = 'AveWindDir')

pollutionRose(wind_2015_rose_ave, 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2015 Average Values') #500x500
pollutionRose(wind_2015_rose_ave, 
              pollutant = 'ws', 
              type = 'month',
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2015 Average Values') #750x600
pollutionRose(subset(wind_2015_rose_ave,
                     subset = location == 'loon'), 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2015 Average Values, Loon') #500x500
pollutionRose(subset(wind_2015_rose_ave,
                     subset = (location == 'harbor')), 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2015 Average Values, Harbor') #500x500



#### 2016 ###

wind_2016 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2016_wind_L1.csv',
                      col_types = 'Tnnnnc')

unique(wind_2016$location)

wind_2016_sub <- wind_2016 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           TRUE ~ .))) %>% 
  rename(date = 'datetime')%>% 
  select(-MaxWindDir, -MaxWindSp)

str(wind_2016_sub)

summaryPlot(wind_2016_sub,
            main = '2016') #900x650

wind_2016_rose_ave <- wind_2016_sub %>% 
  rename(ws = 'AveWindSp',
         wd = 'AveWindDir')

pollutionRose(wind_2016_rose_ave, 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2016 Average Values') #500x500
pollutionRose(wind_2016_rose_ave, 
              pollutant = 'ws', 
              type = 'month',
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2016 Average Values') #750x600
pollutionRose(subset(wind_2016_rose_ave,
                     subset = location == 'loon'), 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2016 Average Values, Loon') #500x500
pollutionRose(subset(wind_2016_rose_ave,
                     subset = (location == 'harbor')), 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2016 Average Values, Harbor') #500x500



#### 2017 ####

wind_2017 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/2017_wind_L1.csv',
                      col_types = 'Tnnnnc')

unique(wind_2017$location)

wind_2017_sub <- wind_2017 %>% 
  mutate_at(vars(AveWindDir, AveWindSp, MaxWindDir, MaxWindSp),
            funs(case_when(location == 'in transit' ~ NA_real_,
                           location == 'offline' ~ NA_real_,
                           TRUE ~ .))) %>% 
  rename(date = 'datetime')%>% 
  select(-MaxWindDir, -MaxWindSp)

str(wind_2017_sub)

summaryPlot(wind_2017_sub,
            main = '2017') #900x650

wind_2017_rose_ave <- wind_2017_sub %>% 
  rename(ws = 'AveWindSp',
         wd = 'AveWindDir')

pollutionRose(wind_2017_rose_ave, 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2017 Average Values') #500x500
pollutionRose(wind_2017_rose_ave, 
              pollutant = 'ws', 
              type = 'month',
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2017 Average Values') #750x600
pollutionRose(subset(wind_2017_rose_ave,
                     subset = location == 'loon'), 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2017 Average Values, Loon') #500x500
pollutionRose(subset(wind_2017_rose_ave,
                     subset = (location == 'harbor' | location == 'harbor, water sensors offline')), 
              pollutant = 'ws', 
              breaks = c(0, 2, 4, 6, 8, 10),
              main = '2017 Average Values, Harbor') #500x500

