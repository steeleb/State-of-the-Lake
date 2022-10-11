# code to visualize long term phosphorus in lake sunapee

source('phosphorus_summary.R')

library(sf)
library(tmap)
library(raster)
library(tidyverse)
library(ggthemes)

#point to local spatial files folder
gis_dir <- 'C:/Users/steeleb/Dropbox/travel/gis/project/Sunapee/'

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_tp/'


# create a color ramp to better match oligo-meso-eutro-hypereutro scale
tpramp = c('#D0FAFB', '#D0FBF7', '#D0FBF1', '#ADF8D6', '#B4F8D2', '#B4FAAF', '#B6E28D', '#A7CF82', '#9AC373', '#8EBB63','#559E12', '#448B02')
tpval = c(0, 3, 5, 7, 10, 15, 20, 30, 50, 80, 150, 250)

tpramp2 = c('#f6e8c3','#dfc27d','#bf812d', '#8c510a')
tpramp3 = c('#c7eae5','#80cdc1','#35978f','#01665e')

final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom


## subset aggregated data ----
lmp_agg_tp_lake <- lmp_tp_aggyearsite %>% 
  filter(site_type == 'lake')

agg_tp_trib <- lmp_tp_aggyearsite %>% 
  filter(site_type == 'stream')

## load the spatial layers ####

#sunapee shoreline
sunapee_shore = st_read(file.path(gis_dir, 'hydrography/LS_shore_WGS.shp'))
#bathy
sun_bathy <- raster(file.path(gis_dir, 'Sunapee Bathymetry/raster_files/originals/sun_ras_z_m'))
#sunpaee tribs
sun_trib <- st_read(file.path(gis_dir, 'hydrography/streams.shp'))
sun_trib_wgs <- st_transform(sun_trib, crs = 'EPSG:4326')
#sunapee open water
sun_ws_water <- st_read(file.path(gis_dir, 'hydrography/waterbodies open water.shp'))
sun_ws_water_wgs <- st_transform(sun_ws_water, crs = 'EPSG:4326')

#table to sf for med and max tp
aggtp <- st_as_sf(lmp_agg_tp_lake, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')

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

paneled_meantp = tm_shape(sunapee_shore, bbox = st_bbox(sunapee_shore)) + 
  tm_borders() +
  tm_fill() +
  tm_shape(subset(aggtp, subset = year >=1997)) +
  tm_dots('mean_tp_ugl',
          palette = tpramp3,
          breaks = tpval,
          title = 'average summer\ntotal phosphorus\n(ug/L)',
          shape = 24,
          border.col = 'black',
          size = 1.5) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color = 'white')
paneled_meantp
tmap_save(paneled_meantp, filename = file.path(dump_dir, 'mean_tp_summer_paneled_1997_2020.png'),
          width = 8,
          height = 6,
          units = 'in',
          dpi = 300)

paneled_medtp = tm_shape(sunapee_shore, bbox = st_bbox(sunapee_shore)) + 
  tm_borders() +
  tm_fill ()+
  tm_shape(subset(aggtp, subset = year >=1997)) +
  tm_dots('med_tp_ugl',
          palette = tpramp3,
          breaks = tpval,
          title = 'median summer\ntotal phosphorus\n(ug/L)',
          shape = 24,
          border.col = 'black',
          size = 1.5) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color = 'white')
paneled_medtp
tmap_save(paneled_medtp, filename = file.path(dump_dir, 'med_tp_summer_paneled_1997_2020.png'),
          width = 8,
          height = 6,
          units = 'in',
          dpi = 300)

## visualize in animated plot ####

# #store faceted max tp - ncol and nrow must be 1
# tp_max_facet <- tm_shape(sun_trib_wgs, bbox = bbox_sun_new) + 
#     tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + 
#     tm_polygons(border.col = 'blue', col = '#D9F9FA') +
#   tm_shape(sun_bathy)+
#     tm_raster(palette = 'Blues',
#               title = 'lake depth\n(meters)',
#               contrast = c(0, 0.5)) +
#   tm_shape(sunapee_shore) + 
#     tm_borders() +
#   tm_shape(subset(aggtp, subset = year >=1997)) +
#     tm_bubbles('max_tp_ugl',
#                col = 'green',
#                title.size = 'maximum summer\ntotal phosphorus\n(mg/L)',
#                border.col = 'black',
#                scale = 3) +
#   tm_facets(by = 'year',
#             ncol = 1,
#             nrow = 1) +
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold')
# 
# #export gif
# tmap_animation(tp_max_facet,
#                filename = file.path(dump_dir, 'max_tp_summer_ani_1997_2020_v3.gif'),
#                fps = 1,
#                dpi = 300)
# 
# 
# #store faceted med tp - ncol and nrow must be 1
# tp_med_facet <- tm_shape(sun_trib_wgs, bbox = bbox_sun_new) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons(border.col = 'blue', col = '#D9F9FA') +
#   tm_shape(sun_bathy)+
#   tm_raster(palette = 'Blues',
#             title = 'lake depth\n(meters)',
#             contrast = c(0, 0.5)) +
#   tm_shape(sunapee_shore) + tm_borders() +
#   tm_shape(subset(aggtp, subset = year >=1997)) +
#   tm_bubbles('med_tp_ugl',
#              col = 'green',
#              title.size = 'median summer\ntotal phosphorus\n(mg/L)',
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
# tmap_animation(tp_med_facet,
#                filename = file.path(dump_dir, 'med_tp_summer_ani_1997_2020_v3.gif'),
#                fps = 1,
#                dpi = 300)
# 
# 
# 
# #store faceted mean tp - ncol and nrow must be 1
# tp_mean_facet <- tm_shape(sun_trib_wgs, bbox = bbox_sun_new) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons(border.col = 'blue', col = '#D9F9FA') +
#   tm_shape(sun_bathy)+
#   tm_raster(palette = 'Blues',
#             title = 'lake depth\n(meters)',
#             contrast = c(0, 0.5)) +
#   tm_shape(sunapee_shore) + tm_borders() +
#   tm_shape(subset(aggtp, subset = year >=1997)) +
#   tm_bubbles('mean_tp_ugl',
#              col = 'green',
#              title.size = 'mean summer\ntotal phosphorus\n(mg/L)',
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
# tmap_animation(tp_mean_facet,
#                filename = file.path(dump_dir, 'mean_tp_summer_ani_1997_2020_v3.gif'),
#                fps = 1,
#                dpi = 300)
# 
# 
# #store faceted thquan tp - ncol and nrow must be 1
# tp_thquan_facet <- tm_shape(sun_trib_wgs, bbox = bbox_sun_new) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons(border.col = 'blue', col = '#D9F9FA') +
#   tm_shape(sun_bathy)+
#   tm_raster(palette = 'Blues',
#             title = 'lake depth\n(meters)',
#             contrast = c(0, 0.5)) +
#   tm_shape(sunapee_shore) + tm_borders() +
#   tm_shape(subset(aggtp, subset = year >=1997)) +
#   tm_bubbles('thquan_tp_ugl',
#              col = 'green',
#              title.size = 'third quantile summer\ntotal phosphorus\n(mg/L)',
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
# tmap_animation(tp_thquan_facet,
#                filename = file.path(dump_dir, 'thquan_tp_summer_ani_1997_2020_v3.gif'),
#                fps = 1,
#                dpi = 300)


## visualize trib TP in paneled plots ----

#table to sf for med and max tp
aggtp_trib <- st_as_sf(agg_tp_trib, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')

# sunapee watershed
sun_ws <- st_read(file.path(gis_dir, 'watersheds/NH_hydro_Sunapee/Lake_Sunapee_watershed.shp'))
sun_ws_wgs <- st_transform(sun_ws, crs = 'EPSG:4326')

bbox_sun_ws <- st_bbox(sun_ws_wgs)

## visualize in paneled plots ####

paneled_meantp_trib = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_trib_wgs) + tm_lines(col = 'grey') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggtp_trib, subset = year >=1997)) +
      tm_dots('max_tp_ugl',
                 palette = tpramp2,
                 title.size = 'maximum summer\ntotal phosphorus\n(ug/L)',
                 border.col = 'black',
                shape = 21,
              size = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color ='white')
paneled_meantp_trib

tmap_save(paneled_meantp_trib, filename = file.path(dump_dir, 'mean_tp_summer_trib_paneled_1997_2020.png'),
          width = 9,
          height = 6,
          units = 'in',
          dpi = 300)

paneled_medtp_trib = tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_trib_wgs) + tm_lines(col = 'grey') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(subset(aggtp_trib, subset = year >=1997)) +
  tm_dots('max_tp_ugl',
          palette = tpramp2,
          title.size = 'maximum summer\ntotal phosphorus\n(ug/L)',
          border.col = 'black',
          shape = 21,
          size = 2) +
  tm_facets(by = 'year',
            ncol = 8) +
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color ='white')
paneled_medtp_trib

tmap_save(paneled_medtp_trib, filename = file.path(dump_dir, 'med_tp_summer_trib_paneled_1997_2020.png'),
          width = 9,
          height = 6,
          units = 'in',
          dpi = 300)


## vis in animated plots ----

# #store faceted max tp - ncol and nrow must be 1
# tp_max_trib_facet <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
#   tm_shape(sun_ws_wgs) + tm_borders() +
#   tm_shape(sun_trib_wgs) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons() +
#   tm_shape(subset(aggtp_trib, subset = year >=1997)) +
#   tm_bubbles('max_tp_ugl',
#              col = 'green',
#              title.size = 'maximum summer\ntotal phosphorus\n(ug/L)',
#              border.col = 'black',
#              scale = 3) +
#   tm_facets(by = 'year',
#             ncol = 1,
#             nrow = 1) +
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold')
# 
# #export gif
# tmap_animation(tp_max_trib_facet,
#                filename = file.path(dump_dir, 'max_tp_summer_trib_ani_1997_2020.gif'),
#                fps = 1,
#                dpi = 300)
# 
# #store faceted med tp - ncol and nrow must be 1
# tp_med_trib_facet <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
#   tm_shape(sun_ws_wgs) + tm_borders() +
#   tm_shape(sun_trib_wgs) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons() +
#   tm_shape(subset(aggtp_trib, subset = year >=1997)) +
#   tm_bubbles('med_tp_ugl',
#              col = 'green',
#              title.size = 'median summer\ntotal phosphorus\n(ug/L)',
#              border.col = 'black',
#              scale = 3) +
#   tm_facets(by = 'year',
#             ncol = 1,
#             nrow = 1) +
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold')
# 
# #export gif
# tmap_animation(tp_med_trib_facet,
#                filename = file.path(dump_dir, 'med_tp_summer_trib_ani_1997_2020.gif'),
#                fps = 1,
#                dpi = 300)
# 
# #store faceted mean tp - ncol and nrow must be 1
# tp_mean_trib_facet <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
#   tm_shape(sun_ws_wgs) + tm_borders() +
#   tm_shape(sun_trib_wgs) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons() +
#   tm_shape(subset(aggtp_trib, subset = year >=1997)) +
#   tm_bubbles('mean_tp_ugl',
#              col = 'green',
#              title.size = 'mean summer\ntotal phosphorus\n(ug/L)',
#              border.col = 'black',
#              scale = 3) +
#   tm_facets(by = 'year',
#             ncol = 1,
#             nrow = 1) +
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold')
# 
# #export gif
# tmap_animation(tp_mean_trib_facet,
#                filename = file.path(dump_dir, 'mean_tp_summer_trib_ani_1997_2020.gif'),
#                fps = 1,
#                dpi = 300)
# 
# #store faceted thquan tp - ncol and nrow must be 1
# tp_thquan_trib_facet <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
#   tm_shape(sun_ws_wgs) + tm_borders() +
#   tm_shape(sun_trib_wgs) + tm_lines(col = 'blue') +
#   tm_shape(sun_ws_water_wgs) + tm_polygons() +
#   tm_shape(subset(aggtp_trib, subset = year >=1997)) +
#   tm_bubbles('thquan_tp_ugl',
#              col = 'green',
#              title.size = 'third quantile summer\ntotal phosphorus\n(ug/L)',
#              border.col = 'black',
#              scale = 3) +
#   tm_facets(by = 'year',
#             ncol = 1,
#             nrow = 1) +
#   tm_layout(panel.label.size = 1.5,
#             panel.label.fontface = 'bold')
# 
# #export gif
# tmap_animation(tp_thquan_trib_facet,
#                filename = file.path(dump_dir, 'thquan_tp_summer_trib_ani_1997_2020.gif'),
#                fps = 1,
#                dpi = 300)



# single-panel 10-year average ----
tp2010 <- lmp_summer_tp_select %>% 
  filter(year > 2010) 

unique(tp2010$month)

tp_summary_2010 <- tp2010 %>% 
  group_by(station, site_type, lat_dd, lon_dd) %>% 
  summarize(n = n(),
            mean_tp_ugl = mean(value)*1000,
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value*1000),
            thquan_tp_ugl = quantile(value, 0.75)*1000) %>% 
  filter(!is.na(lat_dd))

tp_summary_2010_trib = tp_summary_2010 %>% 
  filter(site_type == 'stream') %>% 
  mutate(site_type = 'tributary')
tp_summary_2010_lake = tp_summary_2010 %>% 
  filter(site_type == 'lake')

tp_summary_2010_trib <- st_as_sf(tp_summary_2010_trib, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')
tp_summary_2010_lake <- st_as_sf(tp_summary_2010_lake, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')


lt_tp_ave <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_trib_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(tp_summary_2010_trib) +
  tm_symbols(col = 'mean_tp_ugl',
             shape = 21,
             palette = tpramp2,
             title.col = 'average summer\ntotal phosphorus\n(trib)\n(ug/L)',
             border.col = 'black') +
  tm_shape(tp_summary_2010_lake) +
  tm_symbols(col = 'mean_tp_ugl',
             shape = 24,
             palette = tpramp3,
             title.col = 'average summer\ntotal phosphorus\n(lake)\n(ug/L)',
             border.col = 'black') +
  tm_layout(legend.outside = T,
            legend.title.fontface = 'bold',
            legend.title.size = 1,
            legend.text.size = 1,
            title = 'Average Summer\ntotal phosphorus\n(Jun-Sept,\n2011-2020)\n ',
            title.fontface = 'bold')
lt_tp_ave
tmap_save(lt_tp_ave, 
          filename = file.path(dump_dir, 'average_longterm_tp_2010-2020.png'),
          height = 6, width =5, dpi = 300)

lt_tp_med <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_trib_wgs) + tm_lines(col = 'blue') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(tp_summary_2010_trib) +
  tm_symbols(col = 'med_tp_ugl',
             shape = 21,
             palette = tpramp2,
             title.col = 'median summer\ntotal phosphorus\n(trib)\n(ug/L)',
             border.col = 'black') +
  tm_shape(tp_summary_2010_lake) +
  tm_symbols(col = 'med_tp_ugl',
             shape = 24,
             palette = tpramp3,
             title.col = 'median summer\ntotal phosphorus\n(lake)\n(ug/L)',
             border.col = 'black') +
  tm_layout(legend.outside = T,
            legend.title.fontface = 'bold',
            legend.title.size = 1,
            legend.text.size = 1,
            title = 'Median Summer\ntotal phosphorus\n(Jun-Sept,\n2011-2020)\n ',
            title.fontface = 'bold')
lt_tp_med
tmap_save(lt_tp_med, filename = file.path(dump_dir, 'median_longterm_tp_2010-2020.png'),
          height = 6, width =5, dpi = 300)


# both together (paneled only) ----
tp_summer_ls <- lmp_summer_tp_select %>% 
  filter(year >= 1997)

unique(tp_summer_ls$month)
unique(tp_summer_ls$site_type)

tp_summary_ls <- tp_summer_ls %>% 
  group_by(station, year, site_type, lat_dd, lon_dd) %>% 
  summarize(n = n(),
            mean_tp_ugl = mean(value)*1000,
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value*1000),
            thquan_tp_ugl = quantile(value, 0.75)*1000) %>% 
  filter(!is.na(lat_dd))

tp_summary_trib = tp_summary_ls %>% 
  filter(site_type == 'stream') %>% 
  mutate(site_type = 'tributary')

tp_summary_lake = tp_summary_ls %>% 
  filter(site_type == 'lake')

tp_summary_trib <- st_as_sf(tp_summary_trib, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')
tp_summary_lake <- st_as_sf(tp_summary_lake, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')

#average tp
lt_tp_ave_panel <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_trib_wgs) + tm_lines(col = 'grey') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(tp_summary_trib) +
  tm_symbols(col = 'mean_tp_ugl',
             pal = tpramp2,
             shape = 21,
             title.col = 'average summer\ntotal phosphorus (trib)\n(ug/L)',
             border.col = 'black') +
  tm_shape(tp_summary_lake) +
  tm_symbols(col = 'mean_tp_ugl',
             pal = tpramp3,
             shape = 24,
             title.col = 'average summer\ntotal phosphorus (lake)\n(ug/L)',
             border.col = 'black') +
  tm_layout(legend.outside = T,
            legend.title.fontface = 'bold',
            # legend.title.size = 1.5,
            # legend.text.size = 1,
            title = 'Average Summer\ntotal phosphorus\n(Jun-Sept,\n1997-2020)\n ',
            title.fontface = 'bold')+
  tm_facets(by = 'year',
            ncol =8)+
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color = 'white')
lt_tp_ave_panel

tmap_save(lt_tp_ave_panel,
          filename = file.path(dump_dir, 'paneled_ave_tp_laketrib.png'),
          width = 8,
          height = 6,
          units = 'in',
          dpi = 300)


#median tp
lt_tp_med_panel <- tm_shape(sunapee_shore, bbox = bbox_sun_ws) + tm_polygons() +
  tm_shape(sun_ws_wgs) + tm_borders() +
  tm_shape(sun_trib_wgs) + tm_lines(col = 'grey') +
  tm_shape(sun_ws_water_wgs) + tm_polygons() +
  tm_shape(tp_summary_trib) +
  tm_symbols(col = 'med_tp_ugl',
             pal = tpramp2,
             shape = 21,
             title.col = 'median summer\ntotal phosphorus (trib)\n(ug/L)',
             border.col = 'black') +
  tm_shape(tp_summary_lake) +
  tm_symbols(col = 'med_tp_ugl',
             pal = tpramp3,
             shape = 24,
             title.col = 'median summer\ntotal phosphorus (lake)\n(ug/L)',
             border.col = 'black') +  tm_layout(legend.outside = T,
            legend.title.fontface = 'bold',
            # legend.title.size = 1.5,
            # legend.text.size = 1,
            title = 'Median Summer\ntotal phosphorus\n(Jun-Sept,\n1997-2020)\n ',
            title.fontface = 'bold')+
  tm_facets(by = 'year',
            ncol =8)+
  tm_layout(panel.label.size = 1.5,
            panel.label.fontface = 'bold',
            panel.label.bg.color = 'white')
lt_tp_med_panel

tmap_save(lt_tp_med_panel, filename = file.path(dump_dir, 'paneled_med_tp_laketrib.png'),
          width = 8,
          height = 6,
          units = 'in',
          dpi = 300)

