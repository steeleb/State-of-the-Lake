# script to make map of Davis Weather stations graphs for SOL ----

library(zoo)
library(tidyverse)
library(sf)
library(tmap)
library(ggthemes)
library(cowplot)
library(clifro)

# point to directories
gis_dir <- 'C:/Users/steeleb/Dropbox/travel/gis/project/Sunapee/'
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/davis'

# read in weather data from EDI ----
GM <- read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.736.2&entityid=9bcaed584cafe49d54fb7d0660cedad7')
HC <- read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.736.2&entityid=bc78e20295d5d876419703bf86fb8d01')
SF <- read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.736.2&entityid=910fe28ff7ee4c16ced5849fd6322f06')

locs <- read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.736.2&entityid=f0da7783c40aad1e01409c1f29d0a4c6')


# filter to 2021
GM_2021 <- GM %>% 
  mutate(instrument_datetime = as.POSIXct(instrument_datetime, tz = 'UTC')) %>% 
  filter(instrument_datetime >= as.Date('2021-01-01'))

HC_2021 <- HC %>% 
  mutate(instrument_datetime = as.POSIXct(instrument_datetime, tz = 'UTC')) %>% 
  filter(instrument_datetime >= as.Date('2021-01-01'))

SF_2021 <- SF %>% 
  mutate(instrument_datetime = as.POSIXct(instrument_datetime, tz = 'UTC')) %>% 
  filter(instrument_datetime >= as.Date('2021-01-01'))

# make map of locations ----


## load the spatial layers ####

#sunapee shoreline
sunapee_shore = st_read(file.path(gis_dir, 'hydrography/LS_shore_WGS.shp'))

#table to sf for med and max chla
locs <- st_as_sf(locs, coords = c('Longitude', 'Latitude'), crs = 'EPSG:4326')

#define bounding box fo vis
#get bounding box of bathy
bbox_sunapee <- st_bbox(sunapee_shore) # current bounding box

xrange <- bbox_sunapee$xmax - bbox_sunapee$xmin # range of x values
yrange <- bbox_sunapee$ymax - bbox_sunapee$ymin # range of y values

#create a new one and modify
bbox_sun_new <- st_bbox(sunapee_shore)
bbox_sun_new[1] <- bbox_sun_new[1] - (0.2 * xrange) # xmin - left
bbox_sun_new[3] <- bbox_sun_new[3] + (0.7 * xrange) # xmax - right
bbox_sun_new[2] <- bbox_sun_new[2] - (0.05 * yrange) # ymin - bottom
bbox_sun_new[4] <- bbox_sun_new[4] + (0.05 * yrange) # ymax - top

## visualize in paneled plots on the trophic scale ####
davis_locs <- tm_shape(sunapee_shore, bbox = bbox_sun_new) + 
  tm_borders() +
  tm_fill() +
  tm_shape(locs) +
  tm_dots(size = 1) +
  tm_text('StationName',
          # bg.color = 'white',
          just = 'left',
          xmod = 0.5) +
  tm_layout(main.title = 'Davis Weather\nStation Locations',
            main.title.position = 'center',
            main.title.size = 1.2)

davis_locs
tmap_save(davis_locs, filename = file.path(dump_dir, 'davis station locations.png'),
          width = 3,
          height = 6,
          units = 'in',
          dpi = 300)


# look at precip across sites ----
met2021 <- full_join(HC_2021, GM_2021) %>% 
  full_join(., SF_2021) %>% 
  select(-datetime_noDST) 

unique(met2021$rain_flag)
precip <- met2021 %>% 
  select(instrument_datetime, rain_flag, precipitation_mm, SiteName)

precip <- precip %>% 
  mutate(precipitation_mm = case_when(!is.na(rain_flag) ~ NA_real_,
                                   TRUE ~ precipitation_mm))
daily_precip <- precip %>% 
  mutate(date = as.Date(instrument_datetime)) %>% 
  group_by(SiteName, date) %>% 
  summarise(daily_precip_mm = sum(precipitation_mm, na.rm =  T))

daily_precip <- daily_precip %>% 
  mutate(month = as.numeric(format(date, '%m')),
         week = as.numeric(format(date, '%W')),
         quarter = as.yearqtr(date))

weekly_precip <- daily_precip %>% 
  group_by(SiteName, week, quarter) %>% 
  summarise(weekly_precip_mm = sum(daily_precip_mm, na.rm =  T))

ggplot(weekly_precip, aes(x = week, y = weekly_precip_mm)) +
  geom_col(aes(fill = SiteName), position = 'dodge')+
  labs(x = 'week of year (2021)',
       y = 'total weekly\nprecipitation (mm)') +
  theme_bw() +
  scale_fill_colorblind()

monthly_graph = function(m){
  df <- daily_precip %>% 
    filter(month == m)
  ggplot(df, aes(x = date, y = daily_precip_mm)) +
    geom_col(aes(fill = SiteName), position = 'dodge') +
    labs(x = NULL, y = 'total daily\nprecipitation (mm)')+
    theme_bw()
}

monthly_graph(2)


quarterly_graph = function(q){
  df <- weekly_precip %>% 
    filter(quarter == q)
  ggplot(df, aes(x = week, y = weekly_precip_mm)) +
    geom_col(aes(fill = SiteName), position = 'dodge') +
    labs(x = 'week of year (2021)', y = 'total weekly\nprecipitation (mm)', title = q)+
    theme_bw() +
    theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0.5)) +
    scale_fill_colorblind()
}

Q1_precip <- quarterly_graph('2021 Q1')
Q2_precip <- quarterly_graph('2021 Q2')
Q3_precip <- quarterly_graph('2021 Q3')
Q4_precip <- quarterly_graph('2021 Q4')

plot_grid(Q1_precip, Q2_precip, Q3_precip, Q4_precip,
          ncol = 1)

ggsave(file.path(dump_dir, 'quarterly precip 2021.png'),
       dpi = 300,
       units = 'in',
       width = 8,
       height = 6)

# look at wind across sites ----

wind <- met2021 %>% 
  select(windSpeed_mps, windDirection_SID, instrument_datetime, SiteName) %>% 
  mutate(windDirection_deg = case_when(windDirection_SID == 'N' ~ 0,
                                       windDirection_SID == 'NNE' ~ 0 + 45/2,
                                       windDirection_SID == 'NE' ~ 45,
                                       windDirection_SID == 'ENE' ~  45 + 45/2,
                                       windDirection_SID == 'E' ~ 90,
                                       windDirection_SID == 'ESE' ~ 90 + 45/2,
                                       windDirection_SID == 'SE' ~ 135,
                                       windDirection_SID == 'SSE' ~ 135 + 45/2,
                                       windDirection_SID == 'S' ~ 180,
                                       windDirection_SID == 'SSW' ~ 180 + 45/2,
                                       windDirection_SID == 'SW' ~ 225,
                                       windDirection_SID == 'WSW' ~ 225+ 45/2,
                                       windDirection_SID == 'W' ~ 270,
                                       windDirection_SID == 'WNW' ~ 270 + 45/2,
                                       windDirection_SID == 'NW' ~ 315,
                                       windDirection_SID == 'NNW' ~ 315 + 45/2,
                                       TRUE ~ NA_real_)) %>% 
  mutate(month = format(as.Date(instrument_datetime), '%m'),
         quarter = as.yearqtr(instrument_datetime))

quarterly_wind = function(q) {
  df <- wind %>% 
    filter(quarter == q)
  with(df, 
       windrose(windSpeed_mps, windDirection_deg,
                facet = SiteName,
                n_col = 3,
                ggtheme = 'minimal',
                col_pal = 'PRGn',
                legend_title = paste0(q, " Wind Speed\n(m/s)"),
                speed_cuts = c(1,2, 5, 8)))
}

q1wind <- quarterly_wind('2021 Q1')      
q2wind <- quarterly_wind('2021 Q2')      
q3wind <- quarterly_wind('2021 Q3')      
q4wind <- quarterly_wind('2021 Q4')      

plot_grid(q1wind, q2wind, q3wind, q4wind,
          nrow = 4
          )

ggsave(file.path(dump_dir, 'quarterly wind 2021.png'),
       dpi = 300,
       units = 'in',
       width = 8,
       height = 8)
