# code to visualize long term phosphorus in lake sunapee - just timeseries plots

source('phosphorus_summary.R')

library(ggthemes)

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_tp/'


## aggregate and join ----
lmp_tp_deep_agg <- lmp_summer_tp_deep %>% 
  group_by(year, station) %>% 
  summarize(n = n(),
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value)*1000,
            mean_tp_ugl = mean(value)*1000,
            thquan_tp_ugl = quantile(value, 0.75)*1000) %>% 
  mutate(data = 'deep in-lake')

lmp_tp_shallow_agg <- lmp_summer_tp_shallow %>% 
  group_by(year, station) %>% 
  summarize(n = n(),
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value)*1000,
            mean_tp_ugl = mean(value)*1000,
            thquan_tp_ugl = quantile(value, 0.75)*1000) %>% 
  mutate(data = 'shallow in-lake')

lmp_tp_inlet_agg <- lmp_summer_tp_inlet %>% 
  group_by(year, station) %>% 
  summarize(n = n(),
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value)*1000,
            mean_tp_ugl = mean(value)*1000,
            thquan_tp_ugl = quantile(value, 0.75)*1000) %>% 
  mutate(data = 'tributary')

lmp_tp_aggyear <- full_join(lmp_tp_deep_agg, lmp_tp_shallow_agg) %>% 
  full_join(., lmp_tp_inlet_agg) %>% 
  mutate(data = factor(data, levels = c( 'tributary', 'shallow in-lake','deep in-lake')))


## aggregate and join by year----
lmp_tp_deep_agg_yr <- lmp_summer_tp_deep %>% 
  group_by(year) %>% 
  summarize(n = n(),
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value)*1000,
            mean_tp_ugl = mean(value)*1000,
            thquan_tp_ugl = quantile(value, 0.75)*1000) %>% 
  mutate(data = 'deep in-lake')

lmp_tp_shallow_agg_yr <- lmp_summer_tp_shallow %>% 
  group_by(year) %>% 
  summarize(n = n(),
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value)*1000,
            mean_tp_ugl = mean(value)*1000,
            thquan_tp_ugl = quantile(value, 0.75)*1000) %>% 
  mutate(data = 'shallow in-lake')

lmp_tp_inlet_agg_yr <- lmp_summer_tp_inlet %>% 
  group_by(year) %>% 
  summarize(n = n(),
            med_tp_ugl = median(value)*1000,
            max_tp_ugl = max(value)*1000,
            mean_tp_ugl = mean(value)*1000,
            thquan_tp_ugl = quantile(value, 0.75)*1000) %>% 
  mutate(data = 'tributary')

lmp_tp_aggyear_yr <- full_join(lmp_tp_deep_agg_yr, lmp_tp_shallow_agg_yr) %>% 
  full_join(., lmp_tp_inlet_agg_yr) %>% 
  mutate(data = factor(data, levels = c( 'tributary', 'shallow in-lake','deep in-lake')))

## plot mean and median ----
ggplot(lmp_tp_aggyear, aes(x = as.numeric(year), y = mean_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = data)) +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_aveTPpersite.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')
ggplot(lmp_tp_aggyear, aes(x = as.numeric(year), y = mean_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_aveTPpersite_noloess.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')

ggplot(lmp_tp_aggyear_yr, aes(x = as.numeric(year), y = mean_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = data)) +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_aveTPperyear.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')
ggplot(lmp_tp_aggyear_yr, aes(x = as.numeric(year), y = mean_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_aveTPperyear_noloess.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')

ggplot(lmp_tp_aggyear, aes(x = as.numeric(year), y = med_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = data)) +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_medTPpersite.png'),
       height = 6,
       width = 9,
       dpi = 300,
       units ='in')
ggplot(lmp_tp_aggyear, aes(x = as.numeric(year), y = med_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_medTPpersite_noloess.png'),
       height = 6,
       width = 9,
       dpi = 300,
       units ='in')

ggplot(lmp_tp_aggyear_yr, aes(x = as.numeric(year), y = med_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = data)) +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_medTPperyear.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')
ggplot(lmp_tp_aggyear_yr, aes(x = as.numeric(year), y = med_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_point(aes(color = data, shape = data), size = 2) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_medTPperyear_noloess.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')
