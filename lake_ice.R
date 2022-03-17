# lake ice at Sunapee 

library(tidyverse)
library(ggthemes)
library(readxl)

#save final theme for ggplot
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom

datadir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/ice_out/'
figdir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/lake ice/'


sunapee_ice = read_xls(file.path(datadir, 'sunapee long term ice off dates.xls'),
                       sheet = 'ice out') %>% 
  mutate(date = as.Date(date, format = '%m/%d/%Y'),
         date_new = as.Date(paste(year, month, day, sep = '-')),
         jday = as.numeric(format(date_new, '%j')))

ggplot(sunapee_ice, aes(x = year, y = jday)) +
  geom_point() +
  coord_cartesian(ylim = c(59, 140)) +
  labs(x = NULL,
       y = 'day of year',
       title = 'day of ice out') +
  final_theme

ggsave(file.path(figdir, 'historical_iceout.png'), 
       width = 10,
       height = 7,
       units = 'in', 
       dpi = 300)
