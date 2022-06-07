# code to visualize long term phosphorus in lake sunapee

library(sf)
library(tmap)
library(raster)
library(gifski)
library(tidyverse)
library(ggthemes)

# read in LMP record
lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/LSPALMP_1986-2020_v2021-03-29.csv')

#read in station locations
lmp_locs <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/master%20files/station_location_details.csv')

#point to local spatial files folder
gis_dir <- 'C:/Users/steeleb/Dropbox/travel/gis/project/Sunapee/'

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_chla/'

# create a color ramp to better match oligo-meso-eutro-hypereutro scale
chlaramp = c('#D0FAFB', '#D0FBF7', '#D0FBF1', '#B4F8D2', '#B4FAAF', '#B6E28D', '#A7CF82', '#9AC373', '#8EBB63','#559E12', '#448B02')
chlaval = c(0, 0.5, 1,  2, 4, 5, 10, 20, 50, 100, 250)


# filter and clean up chla for inlake chla ####
#filter for chla
unique(lmp$parameter)

lmp_chla <- lmp %>% 
  filter(parameter == 'chla_ugl') %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(year = format(date, '%Y')) %>% 
  mutate(month = as.numeric(format(date, '%m')))

#filter jun - sept
lmp_summer_chla = lmp_chla %>% 
  filter(month >=6 & month <=9)

#add location info
lmp_summer_chla <- lmp_summer_chla %>% 
  mutate(loc_type = case_when(station < 100 ~ 'near-shore',
                              station >=100 & station< 200 ~ 'other',
                              TRUE ~ 'deep')) %>% 
  filter(loc_type != 'other') %>% 
  filter(!is.na(value))

#no layer info to worry about

ggplot(lmp_summer_chla, aes(x = date, y = value, color = loc_type)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

#aggregate to median, max, mean, 3rd quartile value
lmp_agg_lake <- lmp_summer_chla %>% 
  group_by(station, year, loc_type) %>% 
  summarize(n = n(),
            med_chla_ugl = median(value),
            max_chla_ugl = max(value),
            mean_chla_ugl = mean(value),
            thquan_chla_ugl = quantile(value, 0.75))

ggplot(lmp_agg_lake, aes(x = as.numeric(year), y = med_chla_ugl, color = loc_type)) +
  geom_point() +
  geom_smooth()+
  theme_bw()

ggplot(lmp_agg_lake, aes(x = as.numeric(year), y = med_chla_ugl, color = loc_type)) +
  geom_point() +
  geom_smooth()+
  theme_bw()


## get station list and apply loc info ####
stationlist <- data.frame(unique(lmp_summer_chla$station))
colnames(stationlist) = 'station'

stationlist <- left_join(stationlist, lmp_locs)

# apply station info to 2 datasets
lmp_agg_lake <- full_join(lmp_agg_lake, stationlist) %>% 
  filter(!is.na(lat_dd))

## load the spatial layers ####

#sunapee shoreline
sunapee_shore = st_read(file.path(gis_dir, 'hydrography/LS_shore_WGS.shp'))
#bathy
sun_bathy <- raster(file.path(gis_dir, 'Sunapee Bathymetry/raster_files/originals/sun_ras_z_m'))
#sunpaee streams
sun_stream <- st_read(file.path(gis_dir, 'hydrography/streams.shp'))
sun_stream_wgs <- st_transform(sun_stream, crs = 'EPSG:4326')
#sunapee open water
sun_ws_water <- st_read(file.path(gis_dir, 'hydrography/waterbodies open water.shp'))
sun_ws_water_wgs <- st_transform(sun_ws_water, crs = 'EPSG:4326')

#table to sf for med and max chla
aggchla <- st_as_sf(lmp_agg_lake, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')

#define bounding box fo vis
#get bounding box of bathy
bbox_sunapee <- st_bbox(sun_bathy) # current bounding box

xrange <- bbox_sunapee$xmax - bbox_sunapee$xmin # range of x values
yrange <- bbox_sunapee$ymax - bbox_sunapee$ymin # range of y values

#create a new one and modify
bbox_sun_new <- st_bbox(sun_bathy) 
bbox_sun_new[1] <- bbox_sun_new[1] - (0.1 * xrange) # xmin - left
bbox_sun_new[3] <- bbox_sun_new[3] + (0.1 * xrange) # xmax - right
bbox_sun_new[2] <- bbox_sun_new[2] - (0.05 * yrange) # ymin - bottom
bbox_sun_new[4] <- bbox_sun_new[4] + (0.05 * yrange) # ymax - top

## visualize in paneled plots ####

paneled_maxchla = tm_shape(sun_bathy, bbox = bbox_sun_new) + 
    tm_raster(
      palette = 'Greys',
      title = 'lake depth\n(meters)',
      contrast = c(0, 0.5)) +
  tm_shape(sunapee_shore) + 
    tm_borders() +
  tm_shape(subset(aggchla, subset = year >=1997)) +
    tm_dots('max_chla_ugl',
            palette = chlaramp,
            breaks = chlaval,
            title = 'maximum summer\nchlorophyll-a\n(ug/L)',
            shape = 21,
            border.col = 'black',
            size = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color = 'white')
paneled_maxchla
tmap_save(paneled_maxchla, filename = file.path(dump_dir, 'max_chla_summer_paneled_1997_2020.png'),
          width = 9,
          height = 6,
          units = 'in',
          dpi = 300)

paneled_meanchla = tm_shape(sun_bathy, bbox = bbox_sun_new) + 
  tm_raster(
    palette = 'Greys',
    title = 'lake depth\n(meters)',
    contrast = c(0, 0.5)) +
  tm_shape(sunapee_shore) + 
  tm_borders() +
  tm_shape(subset(aggchla, subset = year >=1997)) +
  tm_dots('mean_chla_ugl',
          palette = chlaramp,
          breaks = chlaval,
          title = 'average summer\nchlorophyll-a\n(ug/L)',
          shape = 21,
          border.col = 'black',
          size = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color = 'white')
paneled_meanchla
tmap_save(paneled_meanchla, filename = file.path(dump_dir, 'mean_chla_summer_paneled_1997_2020.png'),
          width = 9,
          height = 6,
          units = 'in',
          dpi = 300)

paneled_medchla = tm_shape(sun_bathy, bbox = bbox_sun_new) + 
  tm_raster(
    palette = 'Greys',
    title = 'lake depth\n(meters)',
    contrast = c(0, 0.5)) +
  tm_shape(sunapee_shore) + 
  tm_borders() +
  tm_shape(subset(aggchla, subset = year >=1997)) +
  tm_dots('med_chla_ugl',
          palette = chlaramp,
          breaks = chlaval,
          title = 'median summer\nchlorophyll-a\n(ug/L)',
          shape = 21,
          border.col = 'black',
          size = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color = 'white')
paneled_medchla
tmap_save(paneled_medchla, filename = file.path(dump_dir, 'med_chla_summer_paneled_1997_2020.png'),
          width = 9,
          height = 6,
          units = 'in',
          dpi = 300)

paneled_thquanchla = tm_shape(sun_bathy, bbox = bbox_sun_new) + 
  tm_raster(
    palette = 'Greys',
    title = 'lake depth\n(meters)',
    contrast = c(0, 0.5)) +
  tm_shape(sunapee_shore) + 
  tm_borders() +
  tm_shape(subset(aggchla, subset = year >=1997)) +
  tm_dots('thquan_chla_ugl',
          palette = chlaramp,
          breaks = chlaval,
          title = 'third quantile summer\nchlorophyll-a\n(ug/L)',
          shape = 21,
          border.col = 'black',
          size = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color = 'white')
paneled_thquanchla
tmap_save(paneled_thquanchla, 
          filename = file.path(dump_dir, 'thquan_chla_summer_paneled_1997_2020.png'),
          width = 9,
          height = 6,
          units = 'in',
          dpi = 300)


# ## visualize in animated plot ####
# 
# #store faceted max chla - ncol and nrow must be 1
# chla_max_facet <- tm_shape(sun_stream_wgs, bbox = bbox_sun_new) + 
#     tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + 
#     tm_polygons(border.col = 'blue', col = '#D9F9FA') +
#   tm_shape(sun_bathy)+
#     tm_raster(palette = 'Blues',
#               title = 'lake depth\n(meters)',
#               contrast = c(0, 0.5)) +
#   tm_shape(sunapee_shore) + 
#     tm_borders() +
#   tm_shape(subset(aggchla, subset = year >=1997)) +
#     tm_bubbles('max_chla_ugl',
#                col = 'green',
#                title.size = 'maximum summer\nchlorophyll-a\n(mg/L)',
#                border.col = 'black',
#                scale = 3) +
#   tm_facets(by = 'year',
#             ncol = 1,
#             nrow = 1) +
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold')
# 
# #export gif
# tmap_animation(chla_max_facet,
#                filename = file.path(dump_dir, 'max_chla_summer_ani_1997_2020_v3.gif'),
#                fps = 1,
#                dpi = 300)
# 
# 
# #store faceted med chla - ncol and nrow must be 1
# chla_med_facet <- tm_shape(sun_stream_wgs, bbox = bbox_sun_new) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons(border.col = 'blue', col = '#D9F9FA') +
#   tm_shape(sun_bathy)+
#   tm_raster(palette = 'Blues',
#             title = 'lake depth\n(meters)',
#             contrast = c(0, 0.5)) +
#   tm_shape(sunapee_shore) + tm_borders() +
#   tm_shape(subset(aggchla, subset = year >=1997)) +
#   tm_bubbles('med_chla_ugl',
#              col = 'green',
#              title.size = 'median summer\nchlorophyll-a\n(mg/L)',
#              border.col = 'black',
#              scale = 3) +
#   tm_facets(by = 'year',
#             ncol = 1,
#             nrow = 1)+
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold')
# 
# 
# #export gif
# tmap_animation(chla_med_facet,
#                filename = file.path(dump_dir, 'med_chla_summer_ani_1997_2020_v3.gif'),
#                fps = 1,
#                dpi = 300)
# 
# 
# 
# #store faceted mean chla - ncol and nrow must be 1
# chla_mean_facet <- tm_shape(sun_stream_wgs, bbox = bbox_sun_new) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons(border.col = 'blue', col = '#D9F9FA') +
#   tm_shape(sun_bathy)+
#   tm_raster(palette = 'Blues',
#             title = 'lake depth\n(meters)',
#             contrast = c(0, 0.5)) +
#   tm_shape(sunapee_shore) + tm_borders() +
#   tm_shape(subset(aggchla, subset = year >=1997)) +
#   tm_bubbles('mean_chla_ugl',
#              col = 'green',
#              title.size = 'mean summer\nchlorophyll-a\n(mg/L)',
#              border.col = 'black',
#              scale = 3) +
#   tm_facets(by = 'year',
#             ncol = 1,
#             nrow = 1)+
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold')
# 
# 
# #export gif
# tmap_animation(chla_mean_facet,
#                filename = file.path(dump_dir, 'mean_chla_summer_ani_1997_2020_v3.gif'),
#                fps = 1,
#                dpi = 300)
# 
# 
# #store faceted thquan chla - ncol and nrow must be 1
# chla_thquan_facet <- tm_shape(sun_stream_wgs, bbox = bbox_sun_new) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons(border.col = 'blue', col = '#D9F9FA') +
#   tm_shape(sun_bathy)+
#   tm_raster(palette = 'Blues',
#             title = 'lake depth\n(meters)',
#             contrast = c(0, 0.5)) +
#   tm_shape(sunapee_shore) + tm_borders() +
#   tm_shape(subset(aggchla, subset = year >=1997)) +
#   tm_bubbles('thquan_chla_ugl',
#              col = 'green',
#              title.size = 'third quantile summer\nchlorophyll-a\n(mg/L)',
#              border.col = 'black',
#              scale = 3) +
#   tm_facets(by = 'year',
#             ncol = 1,
#             nrow = 1)+
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold')
# 
# 
# #export gif
# tmap_animation(chla_thquan_facet,
#                filename = file.path(dump_dir, 'thquan_chla_summer_ani_1997_2020_v3.gif'),
#                fps = 1,
#                dpi = 300)




# # single-panel 10-year average ----
# lmp_chla_loc <- left_join(lmp_chla, lmp_locs) %>% 
#   filter(year > 2010) %>% 
#   filter(!is.na(value))
# 
# unique(lmp_chla_loc$month)
# 
# chla_summary_2010 <- lmp_chla_loc %>% 
#   group_by(station, site_type, lat_dd, lon_dd) %>% 
#   summarize(n = n(),
#             mean_chla_ugl = mean(value),
#             med_chla_ugl = median(value),
#             max_chla_ugl = max(value),
#             thquan_chla_ugl = quantile(value, 0.75)) %>% 
#   filter(!is.na(lat_dd))
# 
# chla_summary_2010 <- st_as_sf(chla_summary_2010, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')
# 
# 
# lt_chla_ave <- tm_shape(sunapee_shore, bbox = bbox_sunapee) + tm_polygons() +
#   tm_shape(sun_ws_wgs) + tm_borders() +
#   tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons() +
#   tm_shape(chla_summary_2010) +
#   tm_symbols(size = 'mean_chla_ugl',
#              col = 'site_type',
#              shape = 21,
#              title.size = 'average summer\nchlorophyll-a\n(ug/L)',
#              border.col = 'black',
#              scale = 2,
#              shapes.legend.fill = 'white') +
#   tm_layout(legend.outside = T,
#             legend.title.fontface = 'bold',
#             legend.title.size = 1.5,
#             legend.text.size = 1,
#             title = 'Average Summer\nchlorophyll-a\n(Jun-Sept,\n2010-2020)\n ',
#             title.fontface = 'bold')
# lt_chla_ave
# tmap_save(lt_chla_ave, filename = file.path(dump_dir, 'average_longterm_chla_2010-2020.png'))
# 
# lt_chla_med <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
#   tm_shape(sun_ws_wgs) + tm_borders() +
#   tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons() +
#   tm_shape(chla_summary_2010) +
#   tm_symbols(size = 'med_chla_ugl',
#              col = 'site_type',
#              shape = 21,
#              title.size = 'median summer\nchlorophyll-a\n(ug/L)',
#              border.col = 'black',
#              scale = 2,
#              shapes.legend.fill = 'white') +
#   tm_layout(legend.outside = T,
#             legend.title.fontface = 'bold',
#             legend.title.size = 1.5,
#             legend.text.size = 1,
#             title = 'Median Summer\nchlorophyll-a\n(Jun-Sept,\n2010-2020)\n ',
#             title.fontface = 'bold')
# lt_chla_med
# tmap_save(lt_chla_med, filename = file.path(dump_dir, 'median_longterm_chla_2010-2020.png'))
# 
# # lt_chla_max <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
# #   tm_shape(sun_ws_wgs) + tm_borders() +
# #   tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
# #   tm_shape(sun_ws_water_wgs) + tm_polygons() +
# #   tm_shape(chla_summary_2010) +
# #   tm_symbols(size = 'max_chla_ugl',
# #              col = 'site_type',
# #              shape = 21,
# #              title.size = 'maximum summer\nchlorophyll-a\n(ug/L)',
# #              border.col = 'black',
# #              scale = 3,
# #              shapes.legend.fill = 'white') +
# #   tm_layout(legend.outside = T,
# #             legend.title.fontface = 'bold',
# #             legend.title.size = 1.5,
# #             legend.text.size = 1,
# #             title = 'Maximum Summer\nchlorophyll-a\n(Jun-Sept,\n2010-2020)\n ',
# #             title.fontface = 'bold')
# # lt_chla_max
# # tmap_save(lt_chla_max, filename = file.path(dump_dir, 'max_longterm_chla_2010-2020.png'))
# # 
# # lt_chla_thquan <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
# #   tm_shape(sun_ws_wgs) + tm_borders() +
# #   tm_shape(sun_stream_wgs) + tm_lines(col = 'blue') +
# #   tm_shape(sun_ws_water_wgs) + tm_polygons() +
# #   tm_shape(chla_summary_2010) +
# #   tm_symbols(size = 'thquan_chla_ugl',
# #              col = 'site_type',
# #              shape = 21,
# #              title.size = 'third quantile summer\nchlorophyll-a\n(ug/L)',
# #              border.col = 'black',
# #              scale = 3,
# #              shapes.legend.fill = 'white') +
# #   tm_layout(legend.outside = T,
# #             legend.title.fontface = 'bold',
# #             legend.title.size = 1.5,
# #             legend.text.size = 1,
# #             title = 'Third Quantile Summer\nchlorophyll-a\n(Jun-Sept,\n2010-2020)\n ',
# #             title.fontface = 'bold')
# # lt_chla_thquan
# # tmap_save(lt_chla_thquan, filename = file.path(dump_dir, 'thquan_longterm_chla_2010-2020.png'))
# 


# create scatterplot of in-lake deep/shallow and stream input (of those which inlet to lake) over time----

lmp_summer_chla_deep <- lmp_summer_chla %>% 
  filter((station == 200 |
            station == 210 |
            station == 220 |
            station == 230)
         & site_type == 'lake')

lmp_summer_chla_shallow <- lmp_summer_chla %>% 
  filter(station < 100 & site_type == 'lake')
unique(lmp_summer_chla_shallow$station)


## aggregate and join ----
lmp_chla_deep_agg <- lmp_summer_chla_deep %>% 
  group_by(year) %>% 
  summarize(n = n(),
            med_chla_ugl = median(value),
            max_chla_ugl = max(value),
            mean_chla_ugl = mean(value),
            thquan_chla_ugl = quantile(value, 0.75)) %>% 
  mutate(data = 'deep')

lmp_chla_shallow_agg <- lmp_summer_chla_shallow %>% 
  group_by(year) %>% 
  summarize(n = n(),
            med_chla_ugl = median(value),
            max_chla_ugl = max(value),
            mean_chla_ugl = mean(value),
            thquan_chla_ugl = quantile(value, 0.75)) %>% 
  mutate(data = 'shallow')

lmp_chla_aggyear <- full_join(lmp_chla_deep_agg, lmp_chla_shallow_agg) %>% 
  mutate(data = factor(data, levels = c('deep', 'shallow')))

## plot mean and median ----
ggplot(lmp_chla_aggyear, aes(x = as.numeric(year), y = mean_chla_ugl)) +
  geom_abline(slope = 0, intercept = 1, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = data)) +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual chlolorphyll-a (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_LT_chla.png'),
       height = 6,
       width = 9,
       dpi = 300,
       units ='in')

ggplot(lmp_chla_aggyear, aes(x = as.numeric(year), y = mean_chla_ugl)) +
  geom_abline(slope = 0, intercept = 1, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = data)) +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual total phosporus (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_LT_avechla.png'),
       height = 6,
       width = 9,
       dpi = 300,
       units ='in')

ggplot(lmp_chla_aggyear, aes(x = as.numeric(year), y = med_chla_ugl)) +
  geom_abline(slope = 0, intercept = 1, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = data)) +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual total phosporus (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_LT_medchla.png'),
       height = 6,
       width = 9,
       dpi = 300,
       units ='in')
