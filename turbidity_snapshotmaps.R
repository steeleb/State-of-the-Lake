# phosphorus snapshot maps ---

source('phosphorus_summary.R')

library(sf)
library(ggspatial)
library(cowplot)

dump_dir <- file.path(gen_dump_dir, "maps")

# add values to names for map ----
lmp_turb_aggyearsite <- lmp_turb_aggyearsite %>% 
  left_join(lmp_shortlist, .) %>% 
  mutate(mean_name = round(mean_turb_NTU, digits = 0)) %>% 
  mutate(Name = paste0(Name, ' (', mean_name, ')'))


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
lmp_turb_ys <- st_as_sf(lmp_turb_aggyearsite, 
                          coords = c('lon_dd', 'lat_dd'),
                          crs = 'epsg:4326') 

lmp_stream_2023 <- lmp_turb_ys %>% 
  filter(sub_site_type == 'tributary' & year == 2023)
lmp_lake_2023 <- lmp_turb_ys %>% 
  filter(site_type == 'lake' & year == 2023)

lmp_stream_2023_e = lmp_stream_2023 %>% 
  filter(station %in% east_labels)
lmp_stream_2023_w = lmp_stream_2023 %>% 
  filter(!(station %in% east_labels))

stream <- ggplot() +
  geom_sf(watershed, mapping = aes(), fill = 'white') +
  geom_sf(streams, mapping = aes(), color = 'dark blue') +
  geom_sf(waterbodies, mapping = aes(), fill = 'light blue') +
  geom_sf(roads, mapping = aes(), color = 'light grey') +
  geom_sf(lmp_stream_2023, mapping = aes(color = mean_turb_NTU), size = 3) +
  scale_color_viridis_c(limits = c(0,25)) +
  theme_void() +
  labs(x = NULL, y = NULL,
       color = 'average\nsummer tributary\nturbidity\n(NTU)') +
  geom_sf_label_repel(lmp_stream_2023_w, mapping = aes(label = Name), nudge_x = -0.07,nudge_y = -0.001, size = 2.2) +
  geom_sf_label_repel(lmp_stream_2023_e, mapping = aes(label = Name), nudge_x = 0.07,nudge_y = 0.001,  size = 2.2) +
  facet_grid(. ~ sub_site_type) +
  theme(strip.text.x = element_text(size = 12, face = "bold"))+
  theme(legend.position = 'bottom', legend.title = element_text(size = 10)) +
  scale_x_continuous(limits = c(as.numeric(st_bbox(watershed)[1])-0.05, as.numeric(st_bbox(watershed)[3])+0.05))

lmp_lake_2023_e = lmp_lake_2023 %>%
  filter(station %in% lake_east_labels)
lmp_lake_2023_w = lmp_lake_2023 %>%
  filter(!(station %in% lake_east_labels))

lake <-ggplot() +
  geom_sf(watershed, mapping = aes(), fill = 'white') +
  geom_sf(streams, mapping = aes(), color = 'dark blue') +
  geom_sf(waterbodies, mapping = aes(), fill = 'light blue') +
  geom_sf(roads, mapping = aes(), color = 'light grey') +
  geom_sf(lmp_lake_2023, mapping = aes(color = mean_turb_NTU), size = 3) +
  scale_color_viridis_c(limits = c(0,25)) +
  theme_void() +
  labs(x = NULL, y = NULL,
       color = 'average\nsummer in-lake\nturbidity\n(NTU)') +
  geom_sf_label_repel(lmp_lake_2023_w, mapping = aes(label = Name), nudge_x = -0.07,nudge_y = -0.001, size = 2) +
  geom_sf_label_repel(lmp_lake_2023_e, mapping = aes(label = Name), nudge_x = 0.07,nudge_y = 0.001,  size = 2) +
  facet_grid(. ~ site_type) +
  theme(strip.text.x = element_text(size = 12, face = "bold"))+
  theme(legend.position = 'bottom', legend.title = element_text(size = 10)) +
  scale_x_continuous(limits = c(as.numeric(st_bbox(watershed)[1])-0.05, as.numeric(st_bbox(watershed)[3])+0.05))

plot_grid(stream, lake)

ggsave(file.path(dump_dir, 'turb2023_map_labeled.jpg'),
       height = 6,
       width = 9,
       dpi = 600,
       units = 'in',
       bg = 'white')


# 10yr turbidity ----
lmp_turb_10year <- lmp_turb %>% 
  right_join(lmp_shortlist) %>% 
  filter(year >= start_10_year) %>% 
  group_by(station, lon_dd, lat_dd, site_type, sub_site_type, Name) %>% 
  filter(!is.na(value)) %>% 
  summarize(mean_turb_NTU = mean(value)) %>% 
  mutate(mean_name = round(mean_turb_NTU, digits = 0)) %>% 
  mutate(Name = paste0(Name, ' (', mean_name, ')'))

lmp_turb_10year <- st_as_sf(lmp_turb_10year, 
                            coords = c('lon_dd', 'lat_dd'),
                            crs = 'epsg:4326') 

lmp_stream_10year <- lmp_turb_10year %>% 
  filter(sub_site_type == 'tributary')
lmp_lake_10year <- lmp_turb_10year %>% 
  filter(site_type == 'lake')

lmp_stream_10year_e = lmp_stream_10year %>% 
  filter(station %in% east_labels)
lmp_stream_10year_w = lmp_stream_10year %>% 
  filter(!(station %in% east_labels))

stream <- ggplot() +
  geom_sf(watershed, mapping = aes(), fill = 'white') +
  geom_sf(streams, mapping = aes(), color = 'dark blue') +
  geom_sf(waterbodies, mapping = aes(), fill = 'light blue') +
  geom_sf(roads, mapping = aes(), color = 'light grey') +
  geom_sf(lmp_stream_10year, mapping = aes(color = mean_turb_NTU), size = 3) +
  scale_color_viridis_c(limits = c(0,10)) +
  theme_void() +
  labs(x = NULL, y = NULL,
       color = 'average\nsummer tributary\nturbidity\n(NTU)') +
  geom_sf_label_repel(lmp_stream_10year_w, mapping = aes(label = Name), nudge_x = -0.07,nudge_y = -0.001, size = 2) +
  geom_sf_label_repel(lmp_stream_10year_e, mapping = aes(label = Name), nudge_x = 0.07,nudge_y = 0.001,  size = 2) +
  facet_grid(. ~ sub_site_type) +
  theme(strip.text.x = element_text(size = 12, face = "bold"))+
  theme(legend.position = 'bottom', legend.title = element_text(size = 10)) +
  scale_x_continuous(limits = c(as.numeric(st_bbox(watershed)[1])-0.05, as.numeric(st_bbox(watershed)[3])+0.05))

lmp_lake_10year_e = lmp_lake_10year %>%
  filter(station %in% lake_east_labels)
lmp_lake_10year_w = lmp_lake_10year %>%
  filter(!(station %in% lake_east_labels))

lake <-ggplot() +
  geom_sf(watershed, mapping = aes(), fill = 'white') +
  geom_sf(streams, mapping = aes(), color = 'dark blue') +
  geom_sf(waterbodies, mapping = aes(), fill = 'light blue') +
  geom_sf(roads, mapping = aes(), color = 'light grey') +
  geom_sf(lmp_lake_10year, mapping = aes(color = mean_turb_NTU), size = 3) +
  scale_color_viridis_c(limits = c(0, 10)) +
  theme_void() +
  labs(x = NULL, y = NULL,
       color = 'average\nsummer in-lake\nturbidity\n(NTU)') +
  geom_sf_label_repel(lmp_lake_10year_w, mapping = aes(label = Name), nudge_x = -0.07,nudge_y = -0.001, size = 2) +
  geom_sf_label_repel(lmp_lake_10year_e, mapping = aes(label = Name), nudge_x = 0.07,nudge_y = 0.001,  size = 2) +
  facet_grid(. ~ site_type) +
  theme(strip.text.x = element_text(size = 12, face = "bold"))+
  theme(legend.position = 'bottom', legend.title = element_text(size = 10)) +
  scale_x_continuous(limits = c(as.numeric(st_bbox(watershed)[1])-0.05, as.numeric(st_bbox(watershed)[3])+0.05))

plot_grid(stream, lake)

ggsave(file.path(dump_dir, 'turb10year_map_labeled.jpg'),
       height = 6,
       width = 9,
       dpi = 600,
       units = 'in',
       bg = 'white')
