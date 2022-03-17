# figure of rain storms and transducers for Sunapee SOTL

library(tidyverse)
library(ggthemes)
library(rnoaa)
library(cowplot)

#save final theme for ggplot
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom

figdir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/transducers/'

## read in transducer data from EDI ----

op_505 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.466.4&entityid=797a8fd504a0ef7b014bf2a096de1fc4')
cj_665 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.466.4&entityid=ae62a65a829d0500392bac93f8f648ce')
bs_788 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.466.4&entityid=4a5998ea80758ae0bba74c662de4f44a')
bn_790 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.466.4&entityid=16b0190ad87138ef9b521bc9891d0d89')
pb_800 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.466.4&entityid=e769fd3aa48e835ae5ef8873f383cd76')
kh_805 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.466.4&entityid=b6d137f0a87fec884c009b9b19203e3e')
hs_830 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.466.4&entityid=09712468e25bd9c59c80507272542850')

# limit to 2018 data
op_505_2018 = op_505 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  filter(datetime >= as.Date('2018-01-01'))

cj_665_2018 = cj_665 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  filter(datetime >= as.Date('2018-01-01'))

bs_788_2018 = bs_788 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  filter(datetime >= as.Date('2018-01-01'))

bn_790_2018 = bn_790 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  filter(datetime >= as.Date('2018-01-01'))

pb_800_2018 = pb_800 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  filter(datetime >= as.Date('2018-01-01'))

kh_805_2018 = kh_805 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  filter(datetime >= as.Date('2018-01-01'))

hs_830_2018 = hs_830 %>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC')) %>% 
  filter(datetime >= as.Date('2018-01-01')) %>% 
  filter(!is.na(depth_m))

# join all data
trans_2018 = full_join(op_505_2018, cj_665_2018) %>% 
  full_join(bs_788_2018) %>% 
  full_join(bn_790_2018) %>% 
  full_join(pb_800_2018) %>% 
  full_join(kh_805_2018) %>% 
  full_join(hs_830_2018) %>% 
  mutate(stream_no = factor(stream_no))

## read in precip data from NOAA ----
precip_sun <- ghcnd_search('US1NHSL0008', date_min = '2018-01-01', date_max = '2019-01-01', var = 'prcp')$prcp 

precip_sun <- precip_sun %>% 
  mutate(precip_mm = prcp/10)

## read in temp data from EDI buoy data ----
sun_met_2018 = read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.234.6&entityid=3cd9f7a61bc570314dc10c0c6bb35fb6')%>% 
  mutate(datetime = as.POSIXct(datetime, tz = 'UTC'))
head(sun_met_2018)

## visualization ----

#first quarter of year transducer data
trans <- ggplot(subset(trans_2018, 
              subset = datetime < as.Date('2018-04-01')),
       aes(x = datetime, y = depth_m, color = stream_no)) +
  geom_path(size = 1) +
  labs(x = NULL,
       y = 'stream depth\n(meters)') +
  final_theme +
  scale_color_colorblind() +
  theme(legend.position = 'none')
trans

#first quarter of year precip data
precip <- ggplot(subset(precip_sun, 
              subset = date < as.Date('2018-04-01')), 
       aes(x = date, y = precip_mm)) +
  geom_col(color = '#0009FF', fill = '#0009FF') +
  final_theme +
  labs(x = NULL,
       y = 'total daily\nprecipitation (mm)')
precip

#first quarter of year temperature data
airtemp <- ggplot(subset(sun_met_2018,
              subset = datetime < as.Date('2018-04-01')), 
       aes(x = datetime, y = airTemperature_degC)) +
  geom_abline(slope = 0, intercept = 0,
              lty = 2,
              color = 'grey',
              size = 1) +
  geom_path(size = 1,
            aes(color = airTemperature_degC)) +
  scale_color_gradient(low = '#96FAFD',
                       high = '#FCA597') +
  final_theme +
  theme(legend.position = 'none')+
  labs(x = NULL,
       y = 'air temperature\n(\u00B0C)')
airtemp

legend = get_legend(ggplot(subset(trans_2018, 
                                  subset = datetime < as.Date('2018-04-01')),
                           aes(x = datetime, y = depth_m, color = stream_no)) +
                      geom_path(size = 1) +
                      labs(x = NULL,
                           y = 'stream depth (meters)') +
                      final_theme +
                      scale_color_colorblind(name = 'stream\nnumber'))

legend = plot_grid(legend,NULL,NULL,
                   ncol = 1)

plot = plot_grid(trans, precip, airtemp,
          ncol = 1)

plot_leg = plot_grid(plot, legend,
                     rel_widths = c(0.9, 0.1))

title = ggdraw() + draw_label('Early 2018 Stream Depth and Weather Observations',
                              fontface = 'bold')

plot_grid(title, plot_leg,
          ncol = 1,
          rel_heights = c(0.05, 0.95))

ggsave(file.path(figdir, 'early2018_transducer_precip_temp.png'),
       width = 10,
       height = 7,
       units = 'in',
       dpi = 300)
