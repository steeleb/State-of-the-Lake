# code to visualize long term chla in lake sunapee

source('chla_summary.R')

library(sf)
library(tmap)
library(raster)
library(gifski)
library(ggthemes)

#point to local spatial files folder
gis_dir <- 'C:/Users/steeleb/Dropbox/travel/gis/project/Sunapee/'

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_chla/'

# create a color ramp to better match oligo-meso-eutro-hypereutro scale
chlaramp = c('#D0FAFB', '#D0FBF7', '#D0FBF1', '#B4F8D2', '#B4FAAF', '#B6E28D', '#A7CF82', '#9AC373', '#8EBB63','#559E12', '#448B02')
chlaval = c(0, 0.5, 1,  2, 4, 5, 10, 20, 50, 100, 250)

chlaramp2 = c('#feebe2','#fcc5c0','#fa9fb5','#f768a1','#c51b8a','#7a0177')

final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom

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

## visualize in paneled plots on the trophic scale ####
paneled_maxchla = tm_shape(sunapee_shore, bbox = bbox_sun_new) + 
    tm_borders() +
    tm_fill() +
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
          width = 8,
          height = 6,
          units = 'in',
          dpi = 300)

paneled_meanchla = tm_shape(sunapee_shore, bbox = bbox_sun_new) + 
  tm_borders() +
  tm_fill() +
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
          width = 8,
          height = 6,
          units = 'in',
          dpi = 300)

paneled_medchla = tm_shape(sunapee_shore, bbox = bbox_sun_new) + 
  tm_borders() +
  tm_fill()+
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
          width = 8,
          height = 6,
          units = 'in',
          dpi = 300)

## visualize in paneled plots in standalone scale ####
paneled_maxchla = tm_shape(sunapee_shore, bbox = bbox_sun_new) + 
  tm_borders() +
  tm_fill() +
  tm_shape(subset(aggchla, subset = year >=1997)) +
  tm_dots('max_chla_ugl',
          palette = chlaramp2,
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
tmap_save(paneled_maxchla, filename = file.path(dump_dir, 'max_chla_summer_paneled_1997_2020_standalone.png'),
          width = 8,
          height = 6,
          units = 'in',
          dpi = 300)

paneled_meanchla = tm_shape(sunapee_shore, bbox = bbox_sun_new) + 
  tm_borders() +
  tm_fill() +
  tm_shape(subset(aggchla, subset = year >=1997)) +
  tm_dots('mean_chla_ugl',
          palette = chlaramp2,
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
tmap_save(paneled_meanchla, filename = file.path(dump_dir, 'mean_chla_summer_paneled_1997_2020_standalone.png'),
          width = 8,
          height = 6,
          units = 'in',
          dpi = 300)

paneled_medchla = tm_shape(sunapee_shore, bbox = bbox_sun_new) + 
  tm_borders() +
  tm_fill()+
  tm_shape(subset(aggchla, subset = year >=1997)) +
  tm_dots('med_chla_ugl',
          palette = chlaramp2,
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
tmap_save(paneled_medchla, filename = file.path(dump_dir, 'med_chla_summer_paneled_1997_2020_standalone.png'),
          width = 8,
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


