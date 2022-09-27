# visualize affects of tropical storms on the temperature and DO of Sunapee via the buoy

library(tidyverse)
library(ggthemes)
library(rLakeAnalyzer)

#save final theme for ggplot
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom

#colorpallete for smears
color_palette <- colorRampPalette(c("darkred","red","orange", "#edf8b1","#41b6c4", "#225ea8","#253494", "#081d58","black"),bias = 2, space = "rgb")(n = 144)

#deg c to deg f function
deg_trans = function(x) ((x *9/5) + 32)

#dump dir
dumpdir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/tropical_storms/temp/'
figdir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/tropical_storms/'

## Henri August 2021 ----
### temp heatmap -----
#read in buoy data from EDI
buoy_2021 <- read_csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.499.3&entityid=e69f75756b69ae1b7a6d20f7c4370671')
head(buoy_2021)                 

#rename columns for Lake Anaylzer
colnames(buoy_2021)
buoy_2021 <- buoy_2021 %>%
  select(datetime, waterTemperature_degC_0p1m, waterTemperature_degC_1m, waterTemperature_degC_2m, waterTemperature_degC_3m,
         waterTemperature_degC_4m, waterTemperature_degC_5m, waterTemperature_degC_6m, waterTemperature_degC_7m, 
         waterTemperature_degC_8m, waterTemperature_degC_9m,waterTemperature_degC_10m) %>%
  rename(wtr_0.3 = 'waterTemperature_degC_0p1m',#convert to feet
         wtr_3.3 = 'waterTemperature_degC_1m',
         wtr_6.6 = 'waterTemperature_degC_2m',
         wtr_9.8 = 'waterTemperature_degC_3m',
         wtr_13.1 = 'waterTemperature_degC_4m',
         wtr_16.4 = 'waterTemperature_degC_5m',
         wtr_19.7 = 'waterTemperature_degC_6m',
         wtr_23 = 'waterTemperature_degC_7m',
         wtr_26.2 = 'waterTemperature_degC_8m',
         wtr_29.5 = 'waterTemperature_degC_9m',
         wtr_32.8 = 'waterTemperature_degC_10m') %>% 
  mutate_at(vars(c('wtr_0.3', 'wtr_3.3', 'wtr_6.6', 'wtr_9.8', 'wtr_13.1', 'wtr_16.4', 
                   'wtr_19.7', 'wtr_23', 'wtr_26.2', 'wtr_29.5', 'wtr_32.8')),
            ~ deg_trans(.))
head(buoy_2021)

#save text file in order to read in time series
buoy_2021 %>%
  arrange(datetime) %>%
  mutate(datetime=as.character(datetime)) %>%
  write_delim(paste0(dumpdir, "sun.ts.temp.f.2021.txt"), delim='\t')

#read in as timeseries
sun.temp.f.2021 <- load.ts(file.path(dumpdir, "sun.ts.temp.f.2021.txt"), tz='UTC')

#create heatmap
png(file.path(figdir, '2021_Henri_heatmap.png'),
    width = 8, height = 4, units = 'in', res = 300)
wtr.heat.map(sun.temp.f.2021, 
             xlim=c(as.POSIXct('2021-08-20', tz='UTC'), as.POSIXct('2021-08-25', tz='UTC')),
             zlim=c(52, 82), 
             plot.title=title(main="Temperature Heat Map Aug 20-25, 2021", 
                              ylab="Depth (ft)", 
                              xlab=''), 
             plot.axes = { axis.POSIXct(side=1, x=sun.temp.c.2021$datetime, 
                                        at = (seq(as.POSIXct('2021-08-20', tz='UTC'), as.POSIXct('2021-08-25', tz='UTC'), by = "day")), 
                                        format = "%b %d"); axis(2)},
             key.title=title(main="Water\nTemp\n(\u00B0F)", 
                             font.main=1, 
                             cex.main=1)
)
dev.off()

### weather ----
sun_met_2021 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.234.6&entityid=15dcd106aee68b11a4fd102166efda06')
head(sun_met_2021)

# air temp and windspeed Aug 20-25
sun_met_aug2021filt <- sun_met_2021 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  filter(datetime >= as.Date('2021-08-20') &
           datetime < as.Date('2021-08-25')) 
head(sun_met_aug2021filt)

sun_wind_aug2021 <- sun_met_aug2021filt %>% 
  select(datetime, windGustSpeed_mps) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = case_when(variable == 'windGustSpeed_mps' ~ 'wind gust speed\n(feet per second)')) %>% 
  mutate(value = value *3.28)

ggplot(sun_wind_aug2021, aes(x = datetime, y = value)) +
  geom_path() +
  labs(x = NULL,
       y = NULL) +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme +
  theme(strip.background = element_rect(color="black", fill="white", size=1.5, linetype="solid"),
        strip.text.y = element_text(size = 12, face = "bold.italic")) +
  scale_x_datetime(minor_breaks = '1 day')

ggsave(file.path(figdir, '2021_Henri_wind.png'),
       width = 8, height = 2.5)

## Isaias Aug 4 2020 ----

### temp heatmap -----
#read in buoy data from EDI
buoy_2020 <- read_csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.499.3&entityid=3ef7eaaea1fe6cb9ccbb9b7de173e790')
head(buoy_2020)                 

#rename columns for Lake Anaylzer
colnames(buoy_2020)
buoy_2020 <- buoy_2020 %>%
  select(datetime, waterTemperature_degC_0p75m, waterTemperature_degC_1p75m, waterTemperature_degC_2p75m, waterTemperature_degC_3p75m,
         waterTemperature_degC_4p75m, waterTemperature_degC_5p75m, waterTemperature_degC_6p75m, waterTemperature_degC_7p75m, 
         waterTemperature_degC_8p75m, waterTemperature_degC_9p75m) %>%
  rename(wtr_2.5 = 'waterTemperature_degC_0p75m', #convert to feet
         wtr_5.7 = 'waterTemperature_degC_1p75m',
         wtr_9 = 'waterTemperature_degC_2p75m',
         wtr_12.3 = 'waterTemperature_degC_3p75m',
         wtr_15.8 = 'waterTemperature_degC_4p75m',
         wtr_18.9 = 'waterTemperature_degC_5p75m',
         wtr_22.1= 'waterTemperature_degC_6p75m',
         wtr_25.4= 'waterTemperature_degC_7p75m',
         wtr_28.7= 'waterTemperature_degC_8p75m',
         wtr_32= 'waterTemperature_degC_9p75m') %>% 
  mutate_at(vars(c('wtr_2.5', 'wtr_5.7', 'wtr_9', 'wtr_12.3', 'wtr_15.8', 'wtr_18.9', 
                   'wtr_22.1', 'wtr_25.4', 'wtr_28.7', 'wtr_32')),
            ~ deg_trans(.))
head(buoy_2020)

#2020 has an issue of redundant datetimes in a few instances
dupes <- buoy_2020 %>% 
  group_by(datetime) %>% 
  filter(n()>1) %>% 
  slice(1)
buoy_2020 <- buoy_2020 %>% 
  group_by(datetime) %>% 
  filter(n()==1)
buoy_2020 <- full_join(buoy_2020, dupes) %>% 
  ungroup()

#save text file in order to read in time series
buoy_2020 %>%
  arrange(datetime) %>%
  mutate(datetime=as.character(datetime)) %>%
  write_delim(paste0(dumpdir, "sun.ts.temp.f.2020.txt"), delim='\t')

#read in as timeseries
sun.temp.f.2020 <- load.ts(file.path(dumpdir, "sun.ts.temp.f.2020.txt"), tz='UTC')

#create heatmap
png(file.path(figdir, '2020_Isaias_heatmap.png'),
       width = 8, height = 4, units = 'in', res = 300)
wtr.heat.map(sun.temp.f.2020, 
             xlim=c(as.POSIXct('2020-08-02', tz='UTC'), as.POSIXct('2020-08-08', tz='UTC')),
             zlim=c(52,82),
             plot.title=title(main="Temperature Heat Map Aug 2-8, 2020", 
                              ylab="Depth (ft)", 
                              xlab=''), 
             plot.axes = { axis.POSIXct(side=1, x=sun.temp.c.2020$datetime, 
                                        at = (seq(as.POSIXct('2020-05-01', tz='UTC'), 
                                                  as.POSIXct('2020-12-01', tz='UTC'), by = "day")), format = "%b %d"); axis(2) },
             key.title=title(main="Water\nTemp\n(\u00B0F)", 
                             font.main=1, 
                             cex.main=1)
)
dev.off()


### weather ----
sun_met = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.234.6&entityid=39ce71340197c34b8bad295fe83c85bf')
head(sun_met)

# air temp and windspeed Aug 2-Aug 8
sun_met_augfilt <- sun_met %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  filter(datetime >= as.Date('2020-08-02') &
           datetime < as.Date('2020-08-08')) 

sun_tempwind_aug <- sun_met_augfilt %>% 
  select(datetime, windGustSpeed_mps) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime)  %>% 
  mutate(variable = case_when(variable == 'windGustSpeed_mps' ~ 'wind gust speed\n(feet per second)')) %>% 
  mutate(value = value *3.28)

ggplot(sun_tempwind_aug, aes(x = datetime, y = value)) +
  geom_path() +
  labs(x = NULL,
         y = NULL) +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme +
  theme(strip.background = element_rect(color="black", fill="white", size=1.5, linetype="solid"),
        strip.text.y = element_text(size = 12, face = "bold.italic"))

ggsave(file.path(figdir, '2020_Isaias_met.png'),
       width = 8, height = 3)


## 2019 no sig tropical storms? ----

### temp heatmap -----
#read in buoy data from EDI
buoy_2019 <- read_csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.499.3&entityid=66da82b133c13591015006149d0a4576')
head(buoy_2019)                 

#rename columns for Lake Anaylzer
colnames(buoy_2019)
#2.75 is blank
buoy_2019 <- buoy_2019 %>%
  select(datetime, waterTemperature_degC_0p75m, waterTemperature_degC_1p75m, waterTemperature_degC_2p75m, waterTemperature_degC_3p75m,
         waterTemperature_degC_4p75m, waterTemperature_degC_5p75m, waterTemperature_degC_6p75m, waterTemperature_degC_7p75m, 
         waterTemperature_degC_8p75m, waterTemperature_degC_9p75m) %>%
  rename(wtr_0.75 = 'waterTemperature_degC_0p75m',
         wtr_1.75 = 'waterTemperature_degC_1p75m',
         wtr_2.75 = 'waterTemperature_degC_2p75m',
         wtr_3.75 = 'waterTemperature_degC_3p75m',
         wtr_4.75 = 'waterTemperature_degC_4p75m',
         wtr_5.75 = 'waterTemperature_degC_5p75m',
         wtr_6.75 = 'waterTemperature_degC_6p75m',
         wtr_7.75 = 'waterTemperature_degC_7p75m',
         wtr_8.75 = 'waterTemperature_degC_8p75m',
         wtr_9.75 = 'waterTemperature_degC_9p75m')

#save text file in order to read in time series
buoy_2019 %>%
  arrange(datetime) %>%
  mutate(datetime=as.character(datetime)) %>%
  write_delim(paste0(dumpdir, "sun.ts.temp.c.2019.txt"), delim='\t')

#read in as timeseries
sun.temp.c.2019 <- load.ts(file.path(dumpdir, "sun.ts.temp.c.2019.txt"), tz='UTC')

#create heatmap
wtr.heat.map(sun.temp.c.2019, 
             xlim=c(as.POSIXct('2019-05-01', tz='UTC'), as.POSIXct('2019-11-01', tz='UTC')),
             zlim=c(5,30), 
             plot.title=title(main="Temperature Heat Map 2019", 
                              ylab="Depth (m)", 
                              xlab=''), 
             plot.axes = { axis.POSIXct(side=1, x=sun.temp.c.2019$datetime, at = (seq(as.POSIXct('2019-05-01', tz='UTC'), as.POSIXct('2019-12-01', tz='UTC'), by = "month")), format = "%b"); axis(2) },
             key.title=title(main="Temp (C)", 
                             font.main=1, 
                             cex.main=1)
)


## Florence September 2018 ----

### temp heatmap ----
#read in buoy data from EDI
buoy_2018 <- read_csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.499.3&entityid=d6c8f8251e4f5873da2a3f69ad2d2d62')
head(buoy_2018)                 

#rename columns for Lake Anaylzer
colnames(buoy_2018)
#2.85 is blank
buoy_2018 <- buoy_2018 %>%
  select(datetime, waterTemperature_degC_0p85m, waterTemperature_degC_1p85m, waterTemperature_degC_3p85m,
         waterTemperature_degC_4p85m, waterTemperature_degC_5p85m, waterTemperature_degC_6p85m, waterTemperature_degC_7p85m, 
         waterTemperature_degC_8p85m, waterTemperature_degC_9p85m) %>%
  rename(wtr_0.85 = 'waterTemperature_degC_0p85m',
         wtr_1.85 = 'waterTemperature_degC_1p85m',
         wtr_3.85 = 'waterTemperature_degC_3p85m',
         wtr_4.85 = 'waterTemperature_degC_4p85m',
         wtr_5.85 = 'waterTemperature_degC_5p85m',
         wtr_6.85 = 'waterTemperature_degC_6p85m',
         wtr_7.85 = 'waterTemperature_degC_7p85m',
         wtr_8.85 = 'waterTemperature_degC_8p85m',
         wtr_9.85 = 'waterTemperature_degC_9p85m')

#save text file in order to read in time series
buoy_2018 %>%
  arrange(datetime) %>%
  mutate(datetime=as.character(datetime)) %>%
  write_delim(paste0(dumpdir, "sun.ts.temp.c.2018.txt"), delim='\t')

#read in as timeseries
sun.temp.c.2018 <- load.ts(file.path(dumpdir, "sun.ts.temp.c.2018.txt"), tz='UTC')

#create heatmap
wtr.heat.map(sun.temp.c.2018, 
             xlim=c(as.POSIXct('2018-09-01', tz='UTC'), as.POSIXct('2018-10-01', tz='UTC')),
             zlim=c(12,27), 
              plot.title=title(main="Temperature Heat Map September 2018", 
                               ylab="Depth (m)", 
                               xlab=''), 
              plot.axes = { axis.POSIXct(side=1, x=sun.temp.c.2018$datetime, 
                                         at = (seq(as.POSIXct('2018-09-01', tz='UTC'), as.POSIXct('2018-10-01', tz='UTC'), by = "day")), 
                                         format = "%b %d"); axis(2) },
              key.title=title(main="Temp (C)", 
                              font.main=1, 
                              cex.main=1)
)

### weather ----
sun_met_2018 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.234.6&entityid=15dcd106aee68b11a4fd102166efda06')
head(sun_met_2018)

# air temp and windspeed September
sun_met_sept2018filt <- sun_met_2018 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  filter(datetime >= as.Date('2018-09-01') &
           datetime < as.Date('2018-10-01')) 

#no met data in Sept

## Irene Aug 28 2011 ----

### temp heatmap -----
#read in buoy data from EDI
buoy_2011 <- read_csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.499.3&entityid=26fed3d0522dab567be961851f21dcb9')
head(buoy_2011)                 

#rename columns for Lake Anaylzer
colnames(buoy_2011)
buoy_2011 <- buoy_2011 %>%
  select(datetime, waterTemperature_degC_0p5m, waterTemperature_degC_1p5m, waterTemperature_degC_2p5m, waterTemperature_degC_3p5m,
         waterTemperature_degC_4p5m, waterTemperature_degC_5p5m, waterTemperature_degC_6p5m, waterTemperature_degC_7p5m, 
         waterTemperature_degC_8p5m, waterTemperature_degC_9p5m) %>%
  rename(wtr_0.3 = 'waterTemperature_degC_0p5m', #convert to feet
         wtr_4.9 = 'waterTemperature_degC_1p5m',
         wtr_8.2 = 'waterTemperature_degC_2p5m',
         wtr_11.5 = 'waterTemperature_degC_3p5m',
         wtr_14.8 = 'waterTemperature_degC_4p5m',
         wtr_18 = 'waterTemperature_degC_5p5m',
         wtr_21.3 = 'waterTemperature_degC_6p5m',
         wtr_24.6 = 'waterTemperature_degC_7p5m',
         wtr_27.9 = 'waterTemperature_degC_8p5m',
         wtr_31.2 = 'waterTemperature_degC_9p5m') %>% 
  mutate_at(vars(c('wtr_0.3', 'wtr_4.9', 'wtr_8.2', 'wtr_11.5', 'wtr_14.8', 'wtr_18', 
                   'wtr_21.3', 'wtr_24.6', 'wtr_27.9', 'wtr_31.2')),
            ~ deg_trans(.))

head(buoy_2011)

#save text file in order to read in time series
buoy_2011 %>%
  arrange(datetime) %>%
  mutate(datetime=as.character(datetime)) %>%
  write_delim(paste0(dumpdir, "sun.ts.temp.f.2011.txt"), delim='\t')

#read in as timeseries
sun.temp.f.2011 <- load.ts(file.path(dumpdir, "sun.ts.temp.f.2011.txt"), tz='UTC')

#create heatmap
png(file.path(figdir, '2011_Irene_heatmap.png'),
    width = 8, height = 4, units = 'in', res = 300)
wtr.heat.map(sun.temp.f.2011, 
             xlim=c(as.POSIXct('2011-08-25', tz='UTC'), as.POSIXct('2011-08-31', tz='UTC')),
             zlim=c(52,82), 
             plot.title=title(main="Temperature Heat Map Aug 25-31, 2011", 
                              ylab="Depth (ft)", 
                              xlab=''), 
             plot.axes = { axis.POSIXct(side=1, x=sun.temp.c.2011$datetime, 
                                        at = (seq(as.POSIXct('2011-08-25', tz='UTC'), as.POSIXct('2011-08-31', tz='UTC'), by = "day")), 
                                        format = "%b %d"); axis(2) },
             key.title=title(main="Water\nTemp\n(\u00B0F)", 
                             font.main=1, 
                             cex.main=1)
)
dev.off()

### weather ----
sun_met_2011 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.234.6&entityid=36891393a740d479f27c32ffd5854151')
head(sun_met_2011)

# air temp and windspeed Aug 20-25
sun_met_aug2011filt <- sun_met_2011 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  filter(datetime >= as.Date('2011-08-25') &
           datetime < as.Date('2011-08-31')) 
head(sun_met_aug2011filt)

sun_wind_aug2011 <- sun_met_aug2011filt %>% 
  select(datetime, windSpeedInstantaneous_mps) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime) %>% 
  mutate(variable = case_when(variable == 'windSpeedInstantaneous_mps' ~ 'instantaneous wind speed\n(feet per second)'))%>% 
  mutate(value = value *3.28)

ggplot(sun_wind_aug2011, aes(x = datetime, y = value)) +
  geom_path() +
  labs(x = NULL,
       y = NULL) +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme +
  theme(strip.background = element_rect(color="black", fill="white", size=1.5, linetype="solid"),
        strip.text.y = element_text(size = 12, face = "bold.italic")) +
  scale_x_datetime(minor_breaks = '1 day')

ggsave(file.path(figdir, '2011_Irene_wind.png'),
       width = 8, height = 2.5)
