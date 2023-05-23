# code to visualize long term phosphorus in lake sunapee

source('turbidity_summary.R')

library(ggthemes)
library(gghighlight)

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_turb/'

# subset data ----
lmp_turb_shallow <- lmp_summer_turb_select %>% 
  filter(sub_site_type == 'cove')

lmp_turb_stream <- lmp_summer_turb_select %>% 
  filter(site_type == 'stream') 

## plot data (shallow) ----
ggplot(lmp_turb_shallow, aes(x = as.numeric(year), y = value)) +
  geom_point(shape = 17, color = '#E69F00') +
  facet_grid(station ~.) +
  gghighlight(value > 5, use_direct_label = FALSE, calculate_per_facet = TRUE) + #highlight the values above 20ug/L, the approximate average from the past few years
  labs(x = NULL,
       y = paste0('Turbidity (NTU)')) +
  theme_bw()
ggsave(filename = file.path(dump_dir, 'alldata_allsites_shallow_turbidity.png'),
          width = 4.5,
          height = 6,
          units = 'in',
          dpi = 300)
ggplot(lmp_turb_shallow, aes(x = as.numeric(year), y = value)) +
  geom_point(shape = 17, color = '#E69F00') +
  facet_grid(station ~., scales= 'free_y') +
  gghighlight(value > 5, use_direct_label = FALSE, calculate_per_facet = TRUE) + #highlight the values above 20ug/L, the approximate average from the past few years
  labs(x = NULL,
       y = paste0('Turbidity (NTU)')) +
  theme_bw()
ggsave(filename = file.path(dump_dir, 'alldata_allsites_shallow_turb_freey.png'),
       width = 4.5,
       height = 6,
       units = 'in',
       dpi = 300)


## plot data (stream) ----
ggplot(lmp_turb_stream, aes(x = as.numeric(year), y = value)) +
  geom_point() +
  facet_grid(station ~.) +
  gghighlight(value > 5, use_direct_label = FALSE, calculate_per_facet = TRUE) + #highlight the values above 20ug/L, the approximate average from the past few years
  labs(x = NULL,
       y = paste0('Turbidity (NTU)')) +
  theme_bw()
ggsave(filename = file.path(dump_dir, 'alldata_allsites_stream_turbidity.png'),
       width = 4.5,
       height = 8,
       units = 'in',
       dpi = 300)
ggplot(lmp_turb_stream, aes(x = as.numeric(year), y = value)) +
  geom_point() +
  facet_grid(station ~., scales = 'free_y') +
  gghighlight(value > 5, use_direct_label = FALSE, calculate_per_facet = TRUE) + #highlight the values above 20ug/L, the approximate average from the past few years
  labs(x = NULL,
       y = paste0('Turbidity (NTU)')) +
  theme_bw()
ggsave(filename = file.path(dump_dir, 'alldata_allsites_stream_turbidityfreey.png'),
       width = 4.5,
       height = 8,
       units = 'in',
       dpi = 300)

#filter outliers
lmp_turb_stream %>% 
  filter(value < 100) %>% 
  ggplot(., aes(x = as.numeric(year), y = value)) +
  geom_point() +
  facet_grid(station ~., scales = 'free_y') +
  gghighlight(value > 5, use_direct_label = FALSE, calculate_per_facet = TRUE) + #highlight the values above 20ug/L, the approximate average from the past few years
  labs(x = NULL,
       y = paste0('Turbidity (NTU)')) +
  theme_bw()
ggsave(filename = file.path(dump_dir, 'alldata_allsites_stream_turb_freey_lt100.png'),
       width = 4.5,
       height = 8,
       units = 'in',
       dpi = 300)

