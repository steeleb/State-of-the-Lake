# phosphorus snapshot maps ---

library(sf)
library(ggspatial)
library(cowplot)

# point to dump and make if it doesn't exist
dump_dir <- file.path(gen_dump_dir, "maps")
if (!dir.exists(dump_dir)) {
  dir.create(dump_dir, recursive = T)
}

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

# 2024 phosphorus ----

# add values to names for map ----
lmp_tp_aggyearsite <- lmp_tp_aggyearsite %>% 
  left_join(., lmp_shortlist) %>% 
  filter(!is.na(Name)) %>% 
  mutate(mean_name = round(mean_tp_ugl, digits = 0)) %>% 
  mutate(map_name = paste0(Name, ' (', mean_name, ')')) %>% 
  filter(!is.na(lat_dd))

lmp_tp_ys <- lmp_tp_aggyearsite %>% 
  filter(!is.na(lon_dd)) %>% 
  st_as_sf(., 
           coords = c('lon_dd', 'lat_dd'),
           crs = 'epsg:4326', 
           remove = F) 

# 2024 Map ----

## streams ----
# Create point-on-surface for locations (you can safely ignore the warning)
east_labels = c(1415, 1420, 830, 835, 805, 800, 790, 788, 760) 
stream_locations_east <- lmp_tp_ys %>%
  filter(station %in% east_labels, 
         site_type == "tributary",
         as.numeric(year) == 2024) 

stream_locations_west <- lmp_tp_ys %>% 
  filter(!station %in% east_labels,
         site_type == "tributary",
         as.numeric(year) == 2024) 

#### east labels ----
# we need top/bottom/south labels
stream_locations_east_top <- stream_locations_east %>% 
  filter(station %in% c(835, 805, 790, 1415))

stream_locations_east_bottom <- stream_locations_east %>% 
  filter(station %in% c(830, 800, 1420))

stream_locations_east_more_south <- stream_locations_east %>% 
  filter(station %in% c(788, 760))

# Create points for label locations
stream_labels_east_top <- stream_locations_east_top %>%
  rowwise() %>% 
  mutate(lon = lon_dd + 0.05,
         lat = lat_dd + 0.003) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(stream_locations_east))
stream_labels_east_bottom <- stream_locations_east_bottom %>%
  rowwise() %>% 
  mutate(lon = lon_dd + 0.05,
         lat = lat_dd - 0.003) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(stream_locations_east))
stream_labels_east_more_south <- stream_locations_east_more_south %>%
  rowwise() %>% 
  mutate(lon = lon_dd + 0.05,
         lat = lat_dd - 0.01) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(stream_locations_east))

# Create lines between label locations and centroids
stream_lines_east_top <- rbind(stream_labels_east_top[,"station"], 
                               stream_locations_east_top[,"station"]) %>%
  group_by(station) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")
stream_lines_east_bottom <- rbind(stream_labels_east_bottom[,"station"], 
                               stream_locations_east_bottom[,"station"]) %>%
  group_by(station) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")
stream_lines_east_more_south <- rbind(stream_labels_east_more_south[,"station"], 
                                  stream_locations_east_more_south[,"station"]) %>%
  group_by(station) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")

#### west labels ----
# we need top/bottom labels
stream_locations_west_top <- stream_locations_west %>%
  filter(station %in% c(510, 670)) #chandler, muzzey
stream_locations_west_close <- stream_locations_west %>% 
  filter(station %in% c(610, 540)) # jobs, out
stream_locations_west_bottom <- stream_locations_west %>% 
  filter(station %in% c(505, 680))

# Create points for label locations
stream_labels_west_top <- stream_locations_west_top %>%
  rowwise() %>% 
  mutate(lon = lon_dd - 0.04,
         lat = lat_dd + 0.005) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(stream_locations_west))
stream_labels_west_close <- stream_locations_west_close %>%
  rowwise() %>%
  mutate(lon = lon_dd - 0.02,
         lat = lat_dd - 0.003) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(stream_locations_west))
stream_labels_west_bottom <- stream_locations_west_bottom %>%
  rowwise() %>%
  mutate(lon = lon_dd - 0.04,
         lat = lat_dd - 0.003) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(stream_locations_west))

# Create lines between label locations and centroids
stream_lines_west_top <- rbind(stream_labels_west_top[,"station"], 
                               stream_locations_west_top[,"station"]) %>%
  group_by(station) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")
stream_lines_west_bottom <- rbind(stream_labels_west_bottom[,"station"],
                                  stream_locations_west_bottom[,"station"]) %>%
  group_by(station) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")
stream_lines_west_close <- rbind(stream_labels_west_close[,"station"],
                                  stream_locations_west_close[,"station"]) %>%
  group_by(station) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")

## create stream map ----

stream <- ggplot() +
  geom_sf(watershed, mapping = aes(), fill = 'white') +
  geom_sf(streams, mapping = aes(), color = 'dark blue') +
  geom_sf(waterbodies, mapping = aes(), fill = 'light blue') +
  geom_sf(roads, mapping = aes(), color = 'light grey') +
  geom_sf(stream_lines_east_top, mapping = aes(), linewidth = 0.5) +
  geom_sf(stream_locations_east_top, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf(stream_lines_east_bottom, mapping = aes(), linewidth = 0.5) +
  geom_sf(stream_locations_east_bottom, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf(stream_lines_east_more_south, mapping = aes(), linewidth = 0.5) +
  geom_sf(stream_locations_east_more_south, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf(stream_lines_west_top, mapping = aes(), linewidth = 0.5) +
  geom_sf(stream_locations_west_top, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf(stream_lines_west_close, mapping = aes(), linewidth = 0.5) +
  geom_sf(stream_locations_west_close, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf(stream_lines_west_bottom, mapping = aes(), linewidth = 0.5) +
  geom_sf(stream_locations_west_bottom, mapping = aes(color = mean_tp_ugl), size = 3) +
  scale_color_viridis_c() +
  geom_sf_text(data = stream_labels_east_top,
               aes(label = map_name),
               size = 3,
               hjust = 0, # left align
               colour = "black")+
  geom_sf_text(data = stream_labels_east_bottom,
               aes(label = map_name),
               size = 3,
               hjust = 0, # left align
               colour = "black")+
  geom_sf_text(data = stream_labels_east_more_south,
               aes(label = map_name),
               size = 3,
               hjust = 0, # left align
               colour = "black")+
  geom_sf_text(data = stream_labels_west_top,
               aes(label = map_name),
               size = 3,
               hjust = 1, # right align
               colour = "black")+
  geom_sf_text(data = stream_labels_west_close,
               aes(label = map_name),
               size = 3,
               hjust = 1, # right align
               colour = "black")+
  geom_sf_text(data = stream_labels_west_bottom,
               aes(label = map_name),
               size = 3,
               hjust = 1, # right align
               colour = "black")+
  theme_void() +
  labs(x = NULL, y = NULL,
       color = 'average\nsummer\ntotal phosphorus\n(µg/L)') +
  # geom_sf_label(lmp_stream_10y, 
  #               mapping = aes(label = map_name), 
  #               nudge_x = -0.07,nudge_y = -0.001, size = 3) +
  facet_grid(. ~ sub_site_type) +
  theme(strip.text.x = element_text(size = 12, face = "bold"))+
  theme(legend.position = 'bottom', legend.title = element_text(size = 10)) +
  scale_x_continuous(limits = c(as.numeric(st_bbox(watershed)[1])-0.12, 
                                as.numeric(st_bbox(watershed)[3])+0.15))
stream
ggsave(file.path(dump_dir, 'tp2024_trib_map_labeled.jpg'),
       height = 5,
       width = 5.5,
       dpi = 600,
       units = 'in',
       bg = 'white')

## lakes ----

#### east labels ----

lake_east_labels <- c(110, 200, 220, 230, 90)

lake_locations_east_close = lmp_tp_ys %>%
  filter(station %in% c(110, 200, 230, 90),
         as.numeric(year) == 2024,
         site_type == "lake")
lake_locations_east_south <- lmp_tp_ys %>% 
  filter(station == 220,
         as.numeric(year) == 2024,
         site_type == "lake")

lake_labels_east_close <- lake_locations_east_close %>%
  rowwise() %>% 
  mutate(lon = lon_dd + 0.03,
         lat = lat_dd - 0.003) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(lake_locations_east_close))

lake_lines_east_close <- rbind(lake_labels_east_close[,"station"], 
                               lake_locations_east_close[,"station"]) %>%
  group_by(station) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")

lake_labels_east_south <- lake_locations_east_south %>%
  rowwise() %>% 
  mutate(lon = lon_dd + 0.045,
         lat = lat_dd - 0.02) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(lake_locations_east_close))

lake_lines_east_south <- rbind(lake_labels_east_south[,"station"], 
                               lake_locations_east_south[,"station"]) %>%
  group_by(station) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")

#### west labels ----
lake_locations_west = lmp_tp_ys %>%
  filter(station %in% c(10, 20, 30, 210, 60),
         as.numeric(year) == 2024,
         site_type == "lake")
lake_locations_west_out <- lmp_tp_ys %>% 
  filter(station %in% c(70, 80),
         as.numeric(year) == 2024,
         site_type == "lake")

lake_labels_west <- lake_locations_west %>%
  rowwise() %>% 
  mutate(lon = lon_dd - 0.035,
         lat = lat_dd - 0.003) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(lake_locations_west))

lake_lines_west <- rbind(lake_labels_west[,"station"], 
                         lake_locations_west[,"station"]) %>%
  group_by(station) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")

lake_labels_west_out <- lake_locations_west_out %>%
  rowwise() %>% 
  mutate(lon = lon_dd - 0.045,
         lat = lat_dd - 0.003) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(lake_locations_west))

lake_lines_west_out <- rbind(lake_labels_west_out[,"station"], 
                         lake_locations_west_out[,"station"]) %>%
  group_by(station) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")

## create lake map ----
lake <-ggplot() +
  geom_sf(watershed, mapping = aes(), fill = 'white') +
  geom_sf(streams, mapping = aes(), color = 'dark blue') +
  geom_sf(waterbodies, mapping = aes(), fill = 'light blue') +
  geom_sf(roads, mapping = aes(), color = 'light grey') +
  geom_sf(lake_lines_east_close, mapping = aes(), linewidth = 0.5) +
  geom_sf(lake_locations_east_close, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf_text(data = lake_labels_east_close,
               aes(label = map_name),
               size = 3,
               hjust = 0, # left align
               colour = "black")+
  geom_sf(lake_lines_east_south, mapping = aes(), linewidth = 0.5) +
  geom_sf(lake_locations_east_south, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf_text(data = lake_labels_east_south,
               aes(label = map_name),
               size = 3,
               hjust = 0, # left align
               colour = "black")+
  geom_sf(lake_lines_west, mapping = aes(), linewidth = 0.5) +
  geom_sf(lake_locations_west, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf_text(data = lake_labels_west,
               aes(label = map_name),
               size = 3,
               hjust = 1, # right align
               colour = "black")+
  geom_sf(lake_lines_west_out, mapping = aes(), linewidth = 0.5) +
  geom_sf(lake_locations_west_out, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf_text(data = lake_labels_west_out,
               aes(label = map_name),
               size = 3,
               hjust = 1, # right align
               colour = "black")+
  scale_color_viridis_c() +
  theme_void() +
  labs(x = NULL, y = NULL,
       color = 'average\nsummer\ntotal phosphorus\n(µg/L)') +
  facet_grid(. ~ site_type) +
  theme(strip.text.x = element_text(size = 12, face = "bold"))+
  theme(legend.position = 'bottom', legend.title = element_text(size = 10)) +
  scale_x_continuous(limits = c(as.numeric(st_bbox(watershed)[1])-0.1, 
                                as.numeric(st_bbox(watershed)[3])+0.1))
lake

ggsave(file.path(dump_dir, 'tp2024_lake_map_labeled.jpg'),
       height = 5,
       width = 4.5,
       dpi = 600,
       units = 'in',
       bg = 'white')


# 10-year maps ----


## streams ----

lmp_stream_10y <- lmp_tp_ys %>% 
  st_drop_geometry() %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(sub_site_type == 'tributary' & 
           between(year, start_10_year, end_10_year)) %>% 
  ungroup() %>% 
  summarize(mean_tp_ugl = mean(mean_tp_ugl),
            .by = c(station, Name, lat_dd, lon_dd)) %>% 
  mutate(mean_name = round(mean_tp_ugl, digits = 1),
         map_name = paste0(Name, ' (', mean_name, ')')) %>% 
  filter(!is.na(lat_dd)) %>% 
  st_as_sf(coords = c("lon_dd", "lat_dd"), 
           crs = "EPSG:4326",
           remove = F)

# Create point-on-surface for locations (you can safely ignore the warning)
east_labels = c(1415, 1420, 830, 835, 805, 800, 790, 788, 760) 
stream_locations_east <- lmp_stream_10y %>%
  filter(station %in% east_labels) 

stream_locations_west <- lmp_stream_10y %>% 
  filter(!station %in% east_labels) 

#### east labels ----
# we need top/bottom/south labels
stream_locations_east_short <- stream_locations_east %>% 
  filter(station == 1415)

stream_locations_east_top <- stream_locations_east %>% 
  filter(station %in% c(835, 805, 790))

stream_locations_east_bottom <- stream_locations_east %>% 
  filter(station %in% c(830, 800, 1420))

stream_locations_east_more_south <- stream_locations_east %>% 
  filter(station %in% c(788, 760))

# Create points for label locations
stream_labels_east_short <- stream_locations_east_short %>%
  rowwise() %>% 
  mutate(lon = lon_dd + 0.02,
         lat = lat_dd + 0.003) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(stream_locations_east))
stream_labels_east_top <- stream_locations_east_top %>%
  rowwise() %>% 
  mutate(lon = lon_dd + 0.05,
         lat = lat_dd + 0.003) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(stream_locations_east))
stream_labels_east_bottom <- stream_locations_east_bottom %>%
  rowwise() %>% 
  mutate(lon = lon_dd + 0.05,
         lat = lat_dd - 0.003) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(stream_locations_east))
stream_labels_east_more_south <- stream_locations_east_more_south %>%
  rowwise() %>% 
  mutate(lon = lon_dd + 0.05,
         lat = lat_dd - 0.01) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(stream_locations_east))

# Create lines between label locations and centroids
stream_lines_east_short <- rbind(stream_labels_east_short[,"station"], 
                               stream_locations_east_short[,"station"]) %>%
  group_by(station) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")
stream_lines_east_top <- rbind(stream_labels_east_top[,"station"], 
                               stream_locations_east_top[,"station"]) %>%
  group_by(station) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")
stream_lines_east_bottom <- rbind(stream_labels_east_bottom[,"station"], 
                                  stream_locations_east_bottom[,"station"]) %>%
  group_by(station) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")
stream_lines_east_more_south <- rbind(stream_labels_east_more_south[,"station"], 
                                      stream_locations_east_more_south[,"station"]) %>%
  group_by(station) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")

#### west labels ----
# we need top/bottom labels
stream_locations_west_top <- stream_locations_west %>%
  filter(station %in% c(510, 670)) #chandler, muzzey
stream_locations_west_close <- stream_locations_west %>% 
  filter(station %in% c(610, 540)) # jobs, out
stream_locations_west_bottom <- stream_locations_west %>% 
  filter(station %in% c(505, 680))

# Create points for label locations
stream_labels_west_top <- stream_locations_west_top %>%
  rowwise() %>% 
  mutate(lon = lon_dd - 0.04,
         lat = lat_dd + 0.005) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(stream_locations_west))
stream_labels_west_close <- stream_locations_west_close %>%
  rowwise() %>%
  mutate(lon = lon_dd - 0.02,
         lat = lat_dd - 0.003) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(stream_locations_west))
stream_labels_west_bottom <- stream_locations_west_bottom %>%
  rowwise() %>%
  mutate(lon = lon_dd - 0.04,
         lat = lat_dd - 0.003) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(stream_locations_west))

# Create lines between label locations and centroids
stream_lines_west_top <- rbind(stream_labels_west_top[,"station"], 
                               stream_locations_west_top[,"station"]) %>%
  group_by(station) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")
stream_lines_west_bottom <- rbind(stream_labels_west_bottom[,"station"],
                                  stream_locations_west_bottom[,"station"]) %>%
  group_by(station) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")
stream_lines_west_close <- rbind(stream_labels_west_close[,"station"],
                                 stream_locations_west_close[,"station"]) %>%
  group_by(station) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")

## create stream map ----

stream <- ggplot() +
  geom_sf(watershed, mapping = aes(), fill = 'white') +
  geom_sf(streams, mapping = aes(), color = 'dark blue') +
  geom_sf(waterbodies, mapping = aes(), fill = 'light blue') +
  geom_sf(roads, mapping = aes(), color = 'light grey') +
  geom_sf(stream_lines_east_short, mapping = aes(), linewidth = 0.5) +
  geom_sf(stream_locations_east_short, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf_text(data = stream_labels_east_short,
               aes(label = map_name),
               size = 3,
               hjust = 0, # left align
               colour = "black")+
  geom_sf(stream_lines_east_top, mapping = aes(), linewidth = 0.5) +
  geom_sf(stream_locations_east_top, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf_text(data = stream_labels_east_top,
               aes(label = map_name),
               size = 3,
               hjust = 0, # left align
               colour = "black")+
  geom_sf(stream_lines_east_bottom, mapping = aes(), linewidth = 0.5) +
  geom_sf(stream_locations_east_bottom, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf_text(data = stream_labels_east_bottom,
               aes(label = map_name),
               size = 3,
               hjust = 0, # left align
               colour = "black")+
  geom_sf(stream_lines_east_more_south, mapping = aes(), linewidth = 0.5) +
  geom_sf(stream_locations_east_more_south, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf_text(data = stream_labels_east_more_south,
               aes(label = map_name),
               size = 3,
               hjust = 0, # left align
               colour = "black")+
  geom_sf(stream_lines_west_top, mapping = aes(), linewidth = 0.5) +
  geom_sf(stream_locations_west_top, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf_text(data = stream_labels_west_top,
               aes(label = map_name),
               size = 3,
               hjust = 1, # right align
               colour = "black")+
  geom_sf(stream_lines_west_close, mapping = aes(), linewidth = 0.5) +
  geom_sf(stream_locations_west_close, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf_text(data = stream_labels_west_close,
               aes(label = map_name),
               size = 3,
               hjust = 1, # right align
               colour = "black")+
  geom_sf(stream_lines_west_bottom, mapping = aes(), linewidth = 0.5) +
  geom_sf(stream_locations_west_bottom, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf_text(data = stream_labels_west_bottom,
               aes(label = map_name),
               size = 3,
               hjust = 1, # right align
               colour = "black")+
  scale_color_viridis_c() +
  theme_void() +
  labs(x = NULL, y = NULL,
       color = 'average\nsummer\ntotal phosphorus\n(µg/L)') +
  # geom_sf_label(lmp_stream_10y, 
  #               mapping = aes(label = map_name), 
  #               nudge_x = -0.07,nudge_y = -0.001, size = 3) +
  theme(strip.text.x = element_text(size = 12, face = "bold"))+
  theme(legend.position = 'bottom', legend.title = element_text(size = 10)) +
  scale_x_continuous(limits = c(as.numeric(st_bbox(watershed)[1])-0.12, 
                                as.numeric(st_bbox(watershed)[3])+0.15))
stream
ggsave(file.path(dump_dir, 'tp_10y_trib_map_labeled.jpg'),
       height = 5,
       width = 5.5,
       dpi = 600,
       units = 'in',
       bg = 'white')

## lakes ----
lmp_lake_10y <- lmp_tp_ys %>% 
  st_drop_geometry() %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(site_type == 'lake' & 
           between(year, start_10_year, end_10_year)) %>% 
  ungroup() %>% 
  summarize(mean_tp_ugl = mean(mean_tp_ugl),
            .by = c(station, Name, lat_dd, lon_dd)) %>% 
  mutate(mean_name = round(mean_tp_ugl, digits = 1),
         map_name = paste0(Name, ' (', mean_name, ')')) %>% 
  filter(!is.na(lat_dd)) %>% 
  st_as_sf(coords = c("lon_dd", "lat_dd"), 
           crs = "EPSG:4326",
           remove = F)

#### east labels ----

lake_east_labels <- c(110, 200, 220, 230, 90)

lake_locations_east_close = lmp_lake_10y %>%
  filter(station %in% c(110, 200, 230, 90))
lake_locations_east_south <- lmp_lake_10y %>% 
  filter(station == 220)

lake_labels_east_close <- lake_locations_east_close %>%
  rowwise() %>% 
  mutate(lon = lon_dd + 0.03,
         lat = lat_dd - 0.003) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(lake_locations_east_close))

lake_lines_east_close <- rbind(lake_labels_east_close[,"station"], 
                               lake_locations_east_close[,"station"]) %>%
  group_by(station) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")

lake_labels_east_south <- lake_locations_east_south %>%
  rowwise() %>% 
  mutate(lon = lon_dd + 0.045,
         lat = lat_dd - 0.02) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(lake_locations_east_close))

lake_lines_east_south <- rbind(lake_labels_east_south[,"station"], 
                               lake_locations_east_south[,"station"]) %>%
  group_by(station) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")

#### west labels ----
lake_locations_west = lmp_lake_10y %>%
  filter(station %in% c(10, 20, 30, 210, 60))
lake_locations_west_out <- lmp_lake_10y %>% 
  filter(station %in% c(70, 80))

lake_labels_west <- lake_locations_west %>%
  rowwise() %>% 
  mutate(lon = lon_dd - 0.035,
         lat = lat_dd - 0.003) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(lmp_lake_10y))

lake_lines_west <- rbind(lake_labels_west[,"station"], 
                         lake_locations_west[,"station"]) %>%
  group_by(station) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")

lake_labels_west_out <- lake_locations_west_out %>%
  rowwise() %>% 
  mutate(lon = lon_dd - 0.045,
         lat = lat_dd - 0.003) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(lake_locations_west))

lake_lines_west_out <- rbind(lake_labels_west_out[,"station"], 
                             lake_locations_west_out[,"station"]) %>%
  group_by(station) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_cast("LINESTRING")

lake <-ggplot() +
  geom_sf(watershed, mapping = aes(), fill = 'white') +
  geom_sf(streams, mapping = aes(), color = 'dark blue') +
  geom_sf(waterbodies, mapping = aes(), fill = 'light blue') +
  geom_sf(roads, mapping = aes(), color = 'light grey') +
  geom_sf(lake_lines_east_close, mapping = aes(), linewidth = 0.5) +
  geom_sf(lake_locations_east_close, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf_text(data = lake_labels_east_close,
               aes(label = map_name),
               size = 3,
               hjust = 0, # left align
               colour = "black")+
  geom_sf(lake_lines_east_south, mapping = aes(), linewidth = 0.5) +
  geom_sf(lake_locations_east_south, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf_text(data = lake_labels_east_south,
               aes(label = map_name),
               size = 3,
               hjust = 0, # left align
               colour = "black")+
  geom_sf(lake_lines_west, mapping = aes(), linewidth = 0.5) +
  geom_sf(lake_locations_west, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf_text(data = lake_labels_west,
               aes(label = map_name),
               size = 3,
               hjust = 1, # right align
               colour = "black")+
  geom_sf(lake_lines_west_out, mapping = aes(), linewidth = 0.5) +
  geom_sf(lake_locations_west_out, mapping = aes(color = mean_tp_ugl), size = 3) +
  geom_sf_text(data = lake_labels_west_out,
               aes(label = map_name),
               size = 3,
               hjust = 1, # right align
               colour = "black")+
  scale_color_viridis_c() +
  theme_void() +
  labs(x = NULL, y = NULL,
       color = 'average\nsummer\ntotal phosphorus\n(µg/L)') +
  theme(strip.text.x = element_text(size = 12, face = "bold"))+
  theme(legend.position = 'bottom', legend.title = element_text(size = 10)) +
  scale_x_continuous(limits = c(as.numeric(st_bbox(watershed)[1])-0.1, 
                                as.numeric(st_bbox(watershed)[3])+0.1))
lake


ggsave(file.path(dump_dir, 'tp_10y_lake_map_labeled.jpg'),
       height = 5,
       width = 4.5,
       dpi = 600,
       units = 'in',
       bg = 'white')
