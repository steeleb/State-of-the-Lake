#map of limited sites referenced in figures

library(sf)
library(tmap)
library(tidyverse)
library(readxl)
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

lmp_stream <- lmp %>% 
  filter(sub_site_type == 'tributary')
lmp_lake <- lmp %>% 
  filter(site_type == 'lake')

stream <- ggplot() +
  geom_sf(watershed, mapping = aes(), fill = 'white') +
  geom_sf(streams, mapping = aes(), color = 'dark blue') +
  geom_sf(waterbodies, mapping = aes(), fill = 'light blue') +
  geom_sf(roads, mapping = aes(), color = 'light grey') +
  geom_sf(lmp_stream, mapping = aes(), size = 3) +
  scale_color_viridis_d() +
  theme_void() +
  labs(x = NULL, y = NULL) +
  geom_sf_label_repel(lmp_stream, mapping = aes(label = Name), size = 2) +
  facet_grid(. ~ sub_site_type) +
  theme(strip.text.x = element_text(size = 12, face = "bold"))
    

lake <- ggplot(lmp_lake) +
  geom_sf(watershed, mapping = aes(), fill = 'white') +
  geom_sf(streams, mapping = aes(), color = 'dark blue') +
  geom_sf(waterbodies, mapping = aes(), fill = 'light blue') +
  geom_sf(roads, mapping = aes(), color = 'light grey') +
  geom_sf(mapping = aes(shape = sub_site_type, color = sub_site_type), size = 3) +
  scale_color_manual(values = c('#e69f00', '#009e73')) +
  scale_shape_manual(values = c(17, 15)) +
  theme_void() +
  labs(x = NULL, y = NULL) +
  geom_sf_label_repel(lmp_lake, mapping = aes(label = Name), size = 3) +
  facet_grid(. ~ sub_site_type) +
  theme(strip.text.x = element_text(size = 12, face = "bold"),
        legend.position = 'none')

plot_grid(stream, lake,
          rel_widths = c(1, 2))

ggsave(file.path(dump_dir, 'labeled_locs_facet.jpg'),
       height = 6,
       width = 9,
       dpi = 600,
       units = 'in',
       bg = 'white')
