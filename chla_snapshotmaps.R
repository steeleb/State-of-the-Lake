# chla snapshot maps ----

source('chla_summary.R')

library(sf)
library(ggspatial)
library(cowplot)

#features dir
feat_dir <- 'C:/Users/steeleb/Dropbox/travel/gis/project/Sunapee/'
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/maps/'

#read in station locations
lmp_shortlist <- read.csv('C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/lmp_shortlist.csv')

# bring in spatial layers ----
lake <- read_sf(file.path(feat_dir, 'hydrography/LS_shore_WGS.shp'))
watershed <- read_sf(file.path(feat_dir, 'watersheds/NH_hydro_Sunapee/Lake_Sunapee_watershed.shp'))
watershed <- st_transform(watershed, crs = 'epsg:4326')
streams <- read_sf(file.path(feat_dir, 'hydrography/streams.shp'))
streams <- st_transform(streams, crs = 'epsg:4326')
waterbodies <- read_sf(file.path(feat_dir, 'hydrography/waterbodies open water.shp'))
waterbodies <- st_transform(waterbodies, crs = 'epsg:4326')
roads <- read_sf(file.path(feat_dir, 'roads/roads_sun_wshed.shp'))
roads <- st_transform(roads, crs = 'epsg:4326')
roads <- st_zm(roads,drop = T)

lmp <- st_as_sf(lmp_shortlist, 
                coords = c('lon_dd', 'lat_dd'),
                crs = 'epsg:4326') 

# 2020 chlorophyll-a ----
lmp_chla_2020 <- lmp_chla %>% 
  right_join(lmp_shortlist) %>% 
  filter(year == 2020) %>% 
  group_by(station, lon_dd, lat_dd, site_type, sub_site_type, Name) %>% 
  filter(!is.na(value)) %>% 
  summarize(mean_chla_ugl = mean(value)) %>% 
  mutate(mean_name = round(mean_chla_ugl, digits = 1)) %>% 
  mutate(Name = paste0(Name, ' (', mean_name, ')'))

lmp_chla_2020 <- st_as_sf(lmp_chla_2020, 
                          coords = c('lon_dd', 'lat_dd'),
                          crs = 'epsg:4326') 

lmp_chla_2020_lake <- lmp_chla_2020 %>% 
  filter(site_type == 'lake')

lmp_chla_2020_lake_e = lmp_chla_2020_lake %>%
  filter(station == 110 | station == 200 | station == 220 | station == 230 |
           station == 90)
lmp_chla_2020_lake_w = lmp_chla_2020_lake %>%
  filter(station != 110 & station != 200 & station != 220 & station != 230 &
           station != 90)

ggplot() +
  geom_sf(watershed, mapping = aes(), fill = 'white') +
  geom_sf(streams, mapping = aes(), color = 'dark blue') +
  geom_sf(waterbodies, mapping = aes(), fill = 'light blue') +
  geom_sf(roads, mapping = aes(), color = 'light grey') +
  geom_sf(lmp_chla_2020_lake, mapping = aes(color = mean_chla_ugl), size = 3) +
  labs(color = 'average\nsummer\nchlorophyll-a\n(ug/L)') +
  scale_color_viridis_c(limits = c(0, 3)) +
  theme_void() +
  labs(x = NULL, y = NULL, title = '2020 in-lake chlorophyll-a') +
  geom_sf_label_repel(lmp_chla_2020_lake_w, mapping = aes(label = Name), nudge_x = -1,nudge_y = -0.01, size = 3) +
  geom_sf_label_repel(lmp_chla_2020_lake_e, mapping = aes(label = Name), nudge_x = 1,nudge_y = -0.001,  size =3) +
  # facet_grid(. ~ sub_site_type) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')) +
  # theme(strip.text.x = element_text(size = 12, face = "bold")) +
  theme(legend.position = 'bottom', legend.title = element_text(size = 10)) +
  scale_x_continuous(limits = c(as.numeric(st_bbox(watershed)[1])-0.04, as.numeric(st_bbox(watershed)[3])+0.04))
ggsave(file.path(dump_dir, 'chla2020_lake_map_labeled.jpg'),
       height = 6,
       width = 4,
       dpi = 600,
       units = 'in',
       bg = 'white')

# 2011-2020 chlorophyll-a ----
lmp_chla_10yr <- lmp_chla %>% 
  right_join(lmp_shortlist) %>% 
  filter(year >= 2011) %>% 
  group_by(station, lon_dd, lat_dd, site_type, sub_site_type, Name) %>% 
  filter(!is.na(value)) %>% 
  summarize(mean_chla_ugl = mean(value)) %>% 
  mutate(mean_name = round(mean_chla_ugl, digits = 1)) %>% 
  mutate(Name = paste0(Name, ' (', mean_name, ')'))

lmp_chla_10yr <- st_as_sf(lmp_chla_10yr, 
                          coords = c('lon_dd', 'lat_dd'),
                          crs = 'epsg:4326') 

lmp_chla_10yr_lake <- lmp_chla_10yr %>% 
  filter(site_type == 'lake')

lmp_chla_10yr_lake_e = lmp_chla_10yr_lake %>%
  filter(station == 110 | station == 200 | station == 220 | station == 230 |
           station == 90)
lmp_chla_10yr_lake_w = lmp_chla_10yr_lake %>%
  filter(station != 110 & station != 200 & station != 220 & station != 230 &
           station != 90)

ggplot() +
  geom_sf(watershed, mapping = aes(), fill = 'white') +
  geom_sf(streams, mapping = aes(), color = 'dark blue') +
  geom_sf(waterbodies, mapping = aes(), fill = 'light blue') +
  geom_sf(roads, mapping = aes(), color = 'light grey') +
  geom_sf(lmp_chla_10yr_lake, mapping = aes(color = mean_chla_ugl), size = 3) +
  labs(color = 'average\nchlorophyll-a\n2011-2020\n(ug/L)') +
  scale_color_viridis_c(limits = c(0,3)) +
  theme_void() +
  labs(x = NULL, y = NULL, title = '2011-2020 in-lake chlorophyll-a') +
  geom_sf_label_repel(lmp_chla_2020_lake_w, mapping = aes(label = Name), nudge_x = -1,nudge_y = -0.01, size = 3) +
  geom_sf_label_repel(lmp_chla_2020_lake_e, mapping = aes(label = Name), nudge_x = 1,nudge_y = -0.001,  size = 3) +
  # facet_grid(. ~ sub_site_type) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        legend.position = 'bottom',
        legend.title = element_text(size = 10)) +
  # theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_x_continuous(limits = c(as.numeric(st_bbox(watershed)[1])-0.04, as.numeric(st_bbox(watershed)[3])+0.04))
ggsave(file.path(dump_dir, 'chla10year_lake_map_labeled.jpg'),
       height = 6,
       width = 4,
       dpi = 600,
       units = 'in',
       bg = 'white')
