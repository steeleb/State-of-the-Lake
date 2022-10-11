# code to visualize long term phosphorus in lake sunapee

source('chla_summary.R')

library(ggthemes)

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_chla/'

## plot mean and median by site-year ----

ggplot(lmp_chla_aggyearsite, aes(x = as.numeric(year), y = mean_chla_ugl)) +
  geom_abline(slope = 0, intercept = 1, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = sub_site_type)) +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual chlorophyll-a per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_LT_avechla.png'),
       height = 4,
       width = 7,
       dpi = 300,
       units ='in')
ggplot(lmp_chla_aggyearsite, aes(x = as.numeric(year), y = mean_chla_ugl)) +
  geom_abline(slope = 0, intercept = 1, lty = 2, color = 'black') +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual chlorophyll-a per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_LT_avechla_noloess.png'),
       height = 4,
       width = 7,
       dpi = 300,
       units ='in')

ggplot(lmp_chla_aggyearsite, aes(x = as.numeric(year), y = med_chla_ugl)) +
  geom_abline(slope = 0, intercept = 1, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = sub_site_type)) +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual chlorophyll-a per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_LT_medchla.png'),
       height = 4,
       width = 7,
       dpi = 300,
       units ='in')
ggplot(lmp_chla_aggyearsite, aes(x = as.numeric(year), y = med_chla_ugl)) +
  geom_abline(slope = 0, intercept = 1, lty = 2, color = 'black') +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual chlorophyll-a per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_LT_medchla_noloess.png'),
       height = 4,
       width = 7,
       dpi = 300,
       units ='in')


# plot mean and median by year ----
ggplot(lmp_chla_aggyear, aes(x = as.numeric(year), y = mean_chla_ugl)) +
  geom_abline(slope = 0, intercept = 1, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = sub_site_type)) +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual chlorophyll-a per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_LT_avechla_peryear.png'),
       height = 4,
       width = 7,
       dpi = 300,
       units ='in')
ggplot(lmp_chla_aggyear, aes(x = as.numeric(year), y = mean_chla_ugl)) +
  geom_abline(slope = 0, intercept = 1, lty = 2, color = 'black') +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual chlorophyll-a per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_LT_avechla_peryear_noloess.png'),
       height = 4,
       width = 7,
       dpi = 300,
       units ='in')

ggplot(lmp_chla_aggyear, aes(x = as.numeric(year), y = med_chla_ugl)) +
  geom_abline(slope = 0, intercept = 1, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = sub_site_type)) +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual chlorophyll-a per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_LT_medchla_peryear.png'),
       height = 4,
       width = 7,
       dpi = 300,
       units ='in')
ggplot(lmp_chla_aggyear, aes(x = as.numeric(year), y = med_chla_ugl)) +
  geom_abline(slope = 0, intercept = 1, lty = 2, color = 'black') +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual chlorophyll-a per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_LT_medchla_peryear_noloess.png'),
       height = 4,
       width = 7,
       dpi = 300,
       units ='in')
