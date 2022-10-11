# deep dive into chlorophyll-a

source('chla_summary.R')

library(ggthemes)
library(gghighlight)

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_chla/'


# subset data ----
lmp_chla_shallow <- lmp_summer_chla_select %>% 
  filter(sub_site_type == 'cove') 

lmp_chla_deep <- lmp_summer_chla_select %>% 
  filter(sub_site_type == 'deep') 

## facet by location ----
lmp_chla_deep %>% 
  ggplot(., aes(x = as.numeric(year), y = value)) +
  geom_point(color = '#E69F00') +
  gghighlight(value>2) +
  facet_grid(station ~ .) +
  geom_abline(slope = 0, intercept = 1, lty= 2)+
  theme_bw() +
  labs(x = NULL, y = 'chlorophyll-a (µg/L)')
ggsave(filename = file.path(dump_dir, 'alldata_allsites_deep_chla.png'),
       width = 4.5,
       height = 6,
       units = 'in',
       dpi = 300)

lmp_chla_shallow %>% 
  ggplot(., aes(x = as.numeric(year), y = value)) +
  geom_point() +
  gghighlight(value>2) +
  facet_grid(station ~ .) +
  geom_abline(slope = 0, intercept = 1, lty= 2)+
  theme_bw() +
  labs(x = NULL, y = 'chlorophyll-a (µg/L)')
ggsave(filename = file.path(dump_dir, 'alldata_allsites_shallow_chla.png'),
       width = 4.5,
       height = 6,
       units = 'in',
       dpi = 300)
