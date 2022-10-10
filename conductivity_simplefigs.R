
source('conductivity_summary.R')

library(ggthemes)

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_cond/'

## plot mean per station ----
ggplot(ws_cond, aes(x = as.numeric(year), y = mean_cond_uScm)) +
  geom_smooth(color = 'dark grey', se = F, aes(color = data)) +
  geom_point(aes(color = data, shape = data), size = 2.5) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual conductivity per site per year (uS/cm)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_stream_LT_avecond.png'),
       height = 6,
       width = 9,
       dpi = 300,
       units ='in')

ggplot(ws_cond, aes(x = as.numeric(year), y = med_cond_uScm)) +
  geom_smooth(color = 'dark grey', se = F, aes(color = data)) +
  geom_point(aes(color = data, shape = data), size = 2.5) +
  facet_grid(data ~ .) +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual conductivity per site per year (uS/cm)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_stream_LT_medcond.png'),
       height = 6,
       width = 9,
       dpi = 300,
       units ='in')

ggplot(ws_cond, aes(x = as.numeric(year), y = mean_cond_uScm)) +
  geom_smooth(color = 'dark grey', se = F, aes(color = data)) +
  geom_point(aes(color = data, shape = data), size = 2.5) +
  facet_grid(data ~ ., scales = 'free_y') +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'average annual conductivity per site per year (uS/cm)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_stream_LT_avecond_diffscale.png'),
       height = 6,
       width = 9,
       dpi = 300,
       units ='in')

ggplot(ws_cond, aes(x = as.numeric(year), y = med_cond_uScm)) +
  geom_smooth(color = 'dark grey', se = F, aes(color = data)) +
  geom_point(aes(color = data, shape = data), size = 2.5) +
  facet_grid(data ~ ., scales = 'free_y') +
  theme_bw() +
  theme(legend.position =  'none',
        strip.background =element_rect(fill="white"),
        strip.text = element_text(face = 'bold')) +
  scale_color_colorblind() +
  labs(x = NULL,
       y = 'median annual conductivity per site per year (uS/cm)')
ggsave(filename = file.path(dump_dir, 'deep_shallow_stream_LT_medcond_diffscale.png'),
       height = 6,
       width = 9,
       dpi = 300,
       units ='in')

