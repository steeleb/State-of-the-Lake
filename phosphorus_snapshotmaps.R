# phosphorus snapshot maps ---

library(sf)
library(ggspatial)
library(ggsflabel)
library(cowplot)

# point to dump and make if it doesn't exist
dump_dir <- file.path(gen_dump_dir, "maps")
if (!dir.exists(dump_dir)) {
  dir.create(dump_dir, recursive = T)
}

# add values to names for map ----
lmp_tp_aggyearsite <- lmp_tp_aggyearsite %>% 
  left_join(., lmp_shortlist) %>% 
  filter(!is.na(Name)) %>% 
  mutate(mean_name = round(mean_tp_ugl, digits = 0)) %>% 
  mutate(Name = paste0(Name, ' (', mean_name, ')')) %>% 
  filter(!is.na(lat_dd))

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

# 2023 phosphorus ----
lmp_tp_ys <- st_as_sf(lmp_tp_aggyearsite, 
                          coords = c('lon_dd', 'lat_dd'),
                          crs = 'epsg:4326') 

lmp_stream_2023 <- lmp_tp_ys %>% 
  filter(sub_site_type == 'tributary' & year == "2023") 
lmp_lake_2023 <- lmp_tp_ys %>% 
  filter(site_type == 'lake' & year == "2023")

east_labels = c(1415, 1420, 830, 835, 805, 800, 788, 760)

lmp_stream_2023_e = lmp_stream_2023 %>% 
  filter(station %in% east_labels)
lmp_stream_2023_w = lmp_stream_2023 %>% 
  filter(!(station %in% east_labels))

stream <- ggplot() +
  geom_sf(watershed, mapping = aes(), fill = 'white') +
  geom_sf(streams, mapping = aes(), color = 'dark blue') +
  geom_sf(waterbodies, mapping = aes(), fill = 'light blue') +
  geom_sf(roads, mapping = aes(), color = 'light grey') +
  geom_sf(lmp_stream_2023, mapping = aes(color = mean_tp_ugl), size = 3) +
  scale_color_viridis_c() +
  theme_void() +
  labs(x = NULL, y = NULL,
       color = 'average\nsummer\ntotal phosphorus\n(µg/L)') +
  geom_sf_label_repel(lmp_stream_2023_w, mapping = aes(label = Name), nudge_x = -0.07,nudge_y = -0.001, size = 3) +
  geom_sf_label_repel(lmp_stream_2023_e, mapping = aes(label = Name), nudge_x = 0.07,nudge_y = 0.001,  size = 3) +
  facet_grid(. ~ sub_site_type) +
  theme(strip.text.x = element_text(size = 12, face = "bold"))+
  theme(legend.position = 'bottom', legend.title = element_text(size = 10)) +
  scale_x_continuous(limits = c(as.numeric(st_bbox(watershed)[1])-0.07, as.numeric(st_bbox(watershed)[3])+0.07))


lake_east_labels <- c(110, 200, 220, 230, 90)

lmp_lake_2023_e = lmp_lake_2023 %>%
  filter(station %in% lake_east_labels)
lmp_lake_2023_w = lmp_lake_2023 %>%
  filter(!(station %in% lake_east_labels))

lake <-ggplot() +
  geom_sf(watershed, mapping = aes(), fill = 'white') +
  geom_sf(streams, mapping = aes(), color = 'dark blue') +
  geom_sf(waterbodies, mapping = aes(), fill = 'light blue') +
  geom_sf(roads, mapping = aes(), color = 'light grey') +
  geom_sf(lmp_lake_2023, mapping = aes(color = mean_tp_ugl), size = 3) +
  scale_color_viridis_c() +
  theme_void() +
  labs(x = NULL, y = NULL,
       color = 'average\nsummer\ntotal phosphorus\n(µg/L)') +
  geom_sf_label_repel(lmp_lake_2023_w, mapping = aes(label = Name), nudge_x = -0.07,nudge_y = -0.001, size = 3) +
  geom_sf_label_repel(lmp_lake_2023_e, mapping = aes(label = Name), nudge_x = 0.07,nudge_y = 0.001,  size = 3) +
  facet_grid(. ~ site_type) +
  theme(strip.text.x = element_text(size = 12, face = "bold"))+
  theme(legend.position = 'bottom', legend.title = element_text(size = 10)) +
  scale_x_continuous(limits = c(as.numeric(st_bbox(watershed)[1])-0.05, as.numeric(st_bbox(watershed)[3])+0.05))

plot_grid(stream, lake)

ggsave(file.path(dump_dir, 'tp2023_map_labeled.jpg'),
       height = 6,
       width = 9,
       dpi = 600,
       units = 'in',
       bg = 'white')


# 2014-2023 phosphorus ----
lmp_tp_10year <- lmp_tp %>% 
  right_join(., lmp_shortlist) %>% 
  filter(year >= start_10_year) %>% 
  group_by(station, lon_dd, lat_dd, site_type, sub_site_type, Name) %>% 
  filter(!is.na(value)) %>% 
  summarize(mean_tp_ugl = mean(value)*1000) %>% 
  mutate(mean_name = round(mean_tp_ugl, digits = 1)) %>% 
  mutate(Name = paste0(Name, ' (', mean_name, ')'))

lmp_tp_10year <- st_as_sf(lmp_tp_10year, 
                            coords = c('lon_dd', 'lat_dd'),
                            crs = 'epsg:4326') 

lmp_stream_10year <- lmp_tp_10year %>% 
  filter(sub_site_type == 'tributary')
lmp_lake_10year <- lmp_tp_10year %>% 
  filter(site_type == 'lake')

lmp_stream_10year_e = lmp_stream_10year %>% 
  filter(station %in% east_labels)
lmp_stream_10year_w = lmp_stream_10year %>% 
  filter(!(station %in% east_labels))

stream_10y <- ggplot() +
  geom_sf(watershed, mapping = aes(), fill = 'white') +
  geom_sf(streams, mapping = aes(), color = 'dark blue') +
  geom_sf(waterbodies, mapping = aes(), fill = 'light blue') +
  geom_sf(roads, mapping = aes(), color = 'light grey') +
  geom_sf(lmp_stream_10year, mapping = aes(color = mean_tp_ugl), size = 3) +
  scale_color_viridis_c(limits = c(0,max(lmp_stream_10year$mean_tp_ugl))) +
  theme_void() +
  labs(x = NULL, y = NULL,
       color = 'average\nsummer tributary\ntotal phosphorus\n(µg/L)') +
  geom_sf_label_repel(lmp_stream_10year_w, mapping = aes(label = Name), nudge_x = -0.07,nudge_y = -0.001, size =  2.2) +
  geom_sf_label_repel(lmp_stream_10year_e, mapping = aes(label = Name), nudge_x = 0.07,nudge_y = 0.001,  size = 2.2) +
  facet_grid(. ~ sub_site_type) +
  theme(strip.text.x = element_text(size = 12, face = "bold"))+
  theme(legend.position = 'bottom', legend.title = element_text(size = 10)) +
  scale_x_continuous(limits = c(as.numeric(st_bbox(watershed)[1])-0.05, as.numeric(st_bbox(watershed)[3])+0.05))

lmp_lake_10year_e = lmp_lake_10year %>%
  filter(station %in% lake_east_labels)
lmp_lake_10year_w = lmp_lake_10year %>%
  filter(!(station %in% lake_east_labels))

lake_10y <-ggplot() +
  geom_sf(watershed, mapping = aes(), fill = 'white') +
  geom_sf(streams, mapping = aes(), color = 'dark blue') +
  geom_sf(waterbodies, mapping = aes(), fill = 'light blue') +
  geom_sf(roads, mapping = aes(), color = 'light grey') +
  geom_sf(lmp_lake_10year, mapping = aes(color = mean_tp_ugl), size = 3) +
  scale_color_viridis_c(limits = c(0,max(lmp_lake_10year$mean_tp_ugl))) +
  theme_void() +
  labs(x = NULL, y = NULL,
       color = 'average\nsummer in-lake\ntotal phosphorus\n(µg/L)') +
  geom_sf_label_repel(lmp_lake_10year_w, mapping = aes(label = Name), nudge_x = -0.07,nudge_y = -0.001, size = 2.2) +
  geom_sf_label_repel(lmp_lake_10year_e, mapping = aes(label = Name), nudge_x = 0.07,nudge_y = 0.001,  size = 2.2) +
  facet_grid(. ~ site_type) +
  theme(strip.text.x = element_text(size = 12, face = "bold"))+
  theme(legend.position = 'bottom', legend.title = element_text(size = 10)) +
  scale_x_continuous(limits = c(as.numeric(st_bbox(watershed)[1])-0.05, as.numeric(st_bbox(watershed)[3])+0.05))

plot_grid(stream_10y, lake_10y)

ggsave(file.path(dump_dir, 'tp10year_map_labeled_v2023.jpg'),
       height = 6,
       width = 9,
       dpi = 600,
       units = 'in',
       bg = 'white')
