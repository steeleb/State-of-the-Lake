# code to visualize long term phosphorus in lake sunapee

source('phosphorus_summary.R')

library(ggthemes)
library(gghighlight)

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_tp/'

# subset data ----
lmp_tp_shallow <- lmp_summer_tp_select %>% 
  filter(sub_site_type == 'cove') %>% 
  mutate(tp_ugl = value*1000)

lmp_tp_stream <- lmp_summer_tp_select %>% 
  filter(site_type == 'stream') %>% 
  mutate(tp_ugl = value*1000)

## plot data (shallow) ----
ggplot(lmp_tp_shallow, aes(x = as.numeric(year), y = tp_ugl)) +
  geom_point(shape = 17, color = '#E69F00') +
  facet_grid(station ~.) +
  gghighlight(tp_ugl > 30, use_direct_label = FALSE, calculate_per_facet = TRUE) + #highlight the values above 20ug/L, the approximate average from the past few years
  labs(x = NULL,
       y = paste0('Total Phosphorus (µg/L)')) +
  theme_bw()
ggsave(filename = file.path(dump_dir, 'alldata_allsites_shallow_tp.png'),
          width = 4.5,
          height = 6,
          units = 'in',
          dpi = 300)
ggplot(lmp_tp_shallow, aes(x = as.numeric(year), y = tp_ugl)) +
  geom_point(shape = 17, color = '#E69F00') +
  facet_grid(station ~., scales= 'free_y') +
  gghighlight(tp_ugl > 30,  use_direct_label = FALSE, calculate_per_facet = TRUE) + #highlight the values above 20ug/L, the approximate average from the past few years
  labs(x = NULL,
       y = paste0('Total Phosphorus (µg/L)')) +
  theme_bw()
ggsave(filename = file.path(dump_dir, 'alldata_allsites_shallow_tp_freey.png'),
       width = 4.5,
       height = 6,
       units = 'in',
       dpi = 300)
lmp_tp_shallow %>% 
  filter(tp_ugl < 80) %>% 
  ggplot(., aes(x = as.numeric(year), y = tp_ugl)) +
  geom_point(shape = 17, color = '#E69F00') +
  facet_grid(station ~., scales= 'free_y') +
  gghighlight(tp_ugl > 30,  use_direct_label = FALSE, calculate_per_facet = TRUE) + #highlight the values above 20ug/L, the approximate average from the past few years
  labs(x = NULL,
       y = paste0('Total Phosphorus (µg/L)')) +
  theme_bw()
ggsave(filename = file.path(dump_dir, 'alldata_allsites_shallow_tp_freey_lt80.png'),
       width = 4.5,
       height = 6,
       units = 'in',
       dpi = 300)
lmp_tp_shallow %>% 
  filter(tp_ugl < 80) %>% 
  ggplot(., aes(x = as.numeric(year), y = tp_ugl)) +
  geom_point(shape = 17, color = '#E69F00') +
  facet_grid(station ~.) +
  gghighlight(tp_ugl > 30,  use_direct_label = FALSE, calculate_per_facet = TRUE) + #highlight the values above 20ug/L, the approximate average from the past few years
  labs(x = NULL,
       y = paste0('Total Phosphorus (µg/L)')) +
  theme_bw()
ggsave(filename = file.path(dump_dir, 'alldata_allsites_shallow_tp_lt80.png'),
       width = 4.5,
       height = 6,
       units = 'in',
       dpi = 300)

## plot data (stream) ----
ggplot(lmp_tp_stream, aes(x = as.numeric(year), y = tp_ugl)) +
  geom_point() +
  facet_grid(station ~.) +
  gghighlight(tp_ugl > 25,  use_direct_label = FALSE, calculate_per_facet = TRUE) + #highlight the values above 25ug/L, the approximate average from the past few years
  labs(x = NULL,
       y = paste0('Total Phosphorus (µg/L)')) +
  theme_bw()
ggsave(filename = file.path(dump_dir, 'alldata_allsites_stream_tp.png'),
       width = 4.5,
       height = 8,
       units = 'in',
       dpi = 300)
ggplot(lmp_tp_stream, aes(x = as.numeric(year), y = tp_ugl)) +
  geom_point() +
  facet_grid(station ~., scales = 'free_y') +
  gghighlight(tp_ugl > 25,  use_direct_label = FALSE, calculate_per_facet = TRUE) + #highlight the values above 25ug/L, the approximate average from the past few years
  labs(x = NULL,
       y = paste0('Total Phosphorus (µg/L)')) +
  theme_bw()
ggsave(filename = file.path(dump_dir, 'alldata_allsites_stream_tpfreey.png'),
       width = 4.5,
       height = 8,
       units = 'in',
       dpi = 300)

#filter outliers
lmp_tp_stream %>% 
  filter(tp_ugl < 400) %>% 
  ggplot(., aes(x = as.numeric(year), y = tp_ugl)) +
  geom_point() +
  facet_grid(station ~., scales = 'free_y') +
  gghighlight(tp_ugl > 25,  use_direct_label = FALSE, calculate_per_facet = TRUE) + #highlight the values above 25ug/L, the approximate average from the past few years
  labs(x = NULL,
       y = paste0('Total Phosphorus (µg/L)')) +
  theme_bw()
ggsave(filename = file.path(dump_dir, 'alldata_allsites_stream_tp_freey_lt400.png'),
       width = 4.5,
       height = 8,
       units = 'in',
       dpi = 300)


## compare stream and in-lake where available ----
