# code to visualize long term phosphorus in lake sunapee - just timeseries plots

source('phosphorus_summary.R')

library(ggthemes)

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_tp/'


## plot mean and median -- site-years ----
ggplot(lmp_tp_aggyearsite, aes(x = as.numeric(year), y = mean_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = sub_site_type)) +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = "year",
       y = 'average annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_aveTPpersite.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')

ggplot(lmp_tp_aggyearsite, aes(x = as.numeric(year), y = mean_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = "year",
       y = 'average annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_aveTPpersite_noloess.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')


ggplot(lmp_tp_aggyearsite, aes(x = as.numeric(year), y = med_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = sub_site_type)) +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = "year",
       y = 'median annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_medTPpersite.png'),
       height = 6,
       width = 9,
       dpi = 300,
       units ='in')
ggplot(lmp_tp_aggyearsite, aes(x = as.numeric(year), y = med_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = "year",
       y = 'median annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_medTPpersite_noloess.png'),
       height = 6,
       width = 9,
       dpi = 300,
       units ='in')

## plot mean and median -- years ----
ggplot(lmp_tp_aggyear, aes(x = as.numeric(year), y = mean_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = sub_site_type)) +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = "year",
       y = 'average annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_aveTPperyear.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')
ggplot(lmp_tp_aggyear, aes(x = as.numeric(year), y = mean_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = "year",
       y = 'average annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_aveTPperyear_noloess.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')

ggplot(lmp_tp_aggyear, aes(x = as.numeric(year), y = med_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_smooth(color = 'dark grey', se = F, aes(color = sub_site_type)) +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = "year",
       y = 'median annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_medTPperyear.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')
ggplot(lmp_tp_aggyear, aes(x = as.numeric(year), y = med_tp_ugl)) +
  geom_abline(slope = 0, intercept = 5, lty = 2, color = 'black') +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = "year",
       y = 'median annual total phosphorus per site per year (ug/L)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_medTPperyear_noloess.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')
