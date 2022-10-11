# code to visualize long term turbidity in lake sunapee - just timeseries plots

source('turbidity_summary.R')

library(ggthemes)

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_turb/'


## plot mean and median -- site-years ----
ggplot(lmp_turb_aggyearsite, aes(x = as.numeric(year), y = mean_turb_NTU)) +
  geom_smooth(color = 'dark grey', se = F, aes(color = sub_site_type)) +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual turbidity per site per year (NTU)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_aveturbpersite.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')
ggplot(lmp_turb_aggyearsite, aes(x = as.numeric(year), y = mean_turb_NTU)) +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual turbidity per site per year (NTU)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_aveturbpersite_noloess.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')


ggplot(lmp_turb_aggyearsite, aes(x = as.numeric(year), y = med_turb_NTU)) +
  geom_smooth(color = 'dark grey', se = F, aes(color = sub_site_type)) +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual turbidity per site per year (NTU)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_medturbpersite.png'),
       height = 6,
       width = 9,
       dpi = 300,
       units ='in')
ggplot(lmp_turb_aggyearsite, aes(x = as.numeric(year), y = med_turb_NTU)) +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual turbidity per site per year (NTU)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_medturbpersite_noloess.png'),
       height = 6,
       width = 9,
       dpi = 300,
       units ='in')

## plot mean and median -- years ----
ggplot(lmp_turb_aggyear, aes(x = as.numeric(year), y = mean_turb_NTU)) +
  geom_smooth(color = 'dark grey', se = F, aes(color = sub_site_type)) +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual turbidity per site per year (NTU)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_aveturbperyear.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')
ggplot(lmp_turb_aggyear, aes(x = as.numeric(year), y = mean_turb_NTU)) +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual turbidity per site per year (NTU)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_aveturbperyear_noloess.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')

ggplot(lmp_turb_aggyear, aes(x = as.numeric(year), y = med_turb_NTU)) +
  geom_smooth(color = 'dark grey', se = F, aes(color = sub_site_type)) +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual turbidity per site per year (NTU)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_medturbperyear.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')
ggplot(lmp_turb_aggyear, aes(x = as.numeric(year), y = med_turb_NTU)) +
  geom_point(aes(color = sub_site_type, shape = sub_site_type), size = 2) +
  facet_grid(sub_site_type ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual turbidity per site per year (NTU)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_inlet_LT_medturbperyear_noloess.png'),
       height = 5,
       width = 7,
       dpi = 300,
       units ='in')
