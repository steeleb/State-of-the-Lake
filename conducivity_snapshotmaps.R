# conductivity snapshot maps ---

source('conductivity_summary.R')

library(sf)
library(ggspatial)
library(ggsflabel)
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

# 2022 conductivity ----
lmp_cond_2022 <- lmp_cond %>% 
  right_join(lmp_shortlist) %>% 
  filter(year == 2022) %>% 
  group_by(station, lon_dd, lat_dd, site_type, sub_site_type, Name) %>% 
  filter(!is.na(value)) %>% 
  summarize(mean_cond_uScm = mean(value)) %>% 
  mutate(mean_name = as.integer(mean_cond_uScm)) %>% 
  mutate(Name = paste0(Name, ' (', mean_name, ')'))

lmp_cond_2022 <- st_as_sf(lmp_cond_2022, 
                          coords = c('lon_dd', 'lat_dd'),
                          crs = 'epsg:4326') 

lmp_cond_2022_stream <- lmp_cond_2022 %>% 
  filter(sub_site_type == 'tributary')
lmp_cond_2022_lake <- lmp_cond_2022 %>% 
  filter(site_type == 'lake')

lmp_cond_2022_stream_e = lmp_cond_2022_stream %>% 
  filter(station == 1415 | station == 1420 | station == 830 | station == 835 | 
           station == 805 | station == 800 | station == 788  | station == 760)
lmp_cond_2022_stream_w = lmp_cond_2022_stream %>% 
  filter(station != 1415 & station != 1420 &  station != 830 & station != 835 & 
           station != 805 & station != 800 & station != 788  & station != 760)

st_bbox(watershed)

ggplot() +
  geom_sf(watershed, mapping = aes(), fill = 'white') +
  geom_sf(streams, mapping = aes(), color = 'dark blue') +
  geom_sf(waterbodies, mapping = aes(), fill = 'light blue') +
  geom_sf(roads, mapping = aes(), color = 'light grey') +
  geom_sf(lmp_cond_2022_stream, mapping = aes(color = mean_cond_uScm), size = 3) +
  labs(color = 'average\nsummer\nconductivity\n(uS/cm)') +
  scale_color_viridis_c() +
  theme_void() +
  labs(x = NULL, y = NULL, title = '2022 Tributary Conductivity') +
  geom_sf_label_repel(lmp_cond_2022_stream_w, mapping = aes(label = Name), nudge_x = -1,nudge_y = -0.01, size = 1.75) +
  geom_sf_label_repel(lmp_cond_2022_stream_e, mapping = aes(label = Name), nudge_x = 1,nudge_y = -0.001,  size = 1.75) +
  # facet_grid(. ~ sub_site_type) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')) +
  # theme(strip.text.x = element_text(size = 12, face = "bold")) +
  theme(legend.position = 'bottom', legend.title = element_text(size = 10)) +
  scale_x_continuous(limits = c(as.numeric(st_bbox(watershed)[1])-0.04, as.numeric(st_bbox(watershed)[3])+0.04))
ggsave(file.path(dump_dir, 'conductivity2022_tribs_map_labeled.jpg'),
       height = 6,
       width = 4,
       dpi = 600,
       units = 'in',
       bg = 'white')

# 2013-2022 conductivity ----
lmp_cond_10year <- lmp_cond %>% 
  right_join(lmp_shortlist) %>% 
  filter(year >= 2013) %>% 
  group_by(station, lon_dd, lat_dd, site_type, sub_site_type, Name) %>% 
  filter(!is.na(value)) %>% 
  summarize(mean_cond_uScm = mean(value)) %>% 
  mutate(mean_name = as.integer(mean_cond_uScm)) %>% 
  mutate(Name = paste0(Name, ' (', mean_name, ')'))

lmp_cond_10year <- st_as_sf(lmp_cond_10year, 
                          coords = c('lon_dd', 'lat_dd'),
                          crs = 'epsg:4326') 

lmp_cond_10year_stream <- lmp_cond_10year %>% 
  filter(sub_site_type == 'tributary')
lmp_cond_10year_lake <- lmp_cond_10year %>% 
  filter(site_type == 'lake')

lmp_cond_10year_stream_e = lmp_cond_10year_stream %>% 
  filter(station == 1415 | station == 1420 | station == 830 | station == 835 | 
           station == 805 | station == 800 | station == 788  | station == 760)
lmp_cond_10year_stream_w = lmp_cond_10year_stream %>% 
  filter(station != 1415 & station != 1420 &  station != 830 & station != 835 & 
           station != 805 & station != 800 & station != 788  & station != 760)

st_bbox(watershed)

ggplot() +
  geom_sf(watershed, mapping = aes(), fill = 'white') +
  geom_sf(streams, mapping = aes(), color = 'dark blue') +
  geom_sf(waterbodies, mapping = aes(), fill = 'light blue') +
  geom_sf(roads, mapping = aes(), color = 'light grey') +
  geom_sf(lmp_cond_10year_stream, mapping = aes(color = mean_cond_uScm), size = 3) +
  labs(color = 'average\nconductivity\n2013-2022\n(uS/cm)') +
  scale_color_viridis_c() +
  theme_void() +
  labs(x = NULL, y = NULL, title = '2013-2022 Tributary Conductivity') +
  geom_sf_label_repel(lmp_cond_10year_stream_w, mapping = aes(label = Name), nudge_x = -1,nudge_y = -0.01, size = 1.75) +
  geom_sf_label_repel(lmp_cond_10year_stream_e, mapping = aes(label = Name), nudge_x = 1,nudge_y = -0.001,  size = 1.75) +
  # facet_grid(. ~ sub_site_type) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        legend.position = 'bottom',
        legend.title = element_text(size = 10)) +
  # theme(strip.text.x = element_text(size = 12, face = "bold")) +
  scale_x_continuous(limits = c(as.numeric(st_bbox(watershed)[1])-0.04, as.numeric(st_bbox(watershed)[3])+0.04))
ggsave(file.path(dump_dir, 'conductivity10year_tribs_map_labeled.jpg'),
       height = 6,
       width = 4,
       dpi = 600,
       units = 'in',
       bg = 'white')
