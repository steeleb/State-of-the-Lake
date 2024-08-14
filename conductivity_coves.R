library(tidyverse)

# source the conductivity summary file
source("conductivity_summary.R")

# read in LMP record
lmp <- read.csv('https://raw.githubusercontent.com/Lake-Sunapee-Protective-Association/LMP/main/primary%20files/LSPALMP_1986-2023_v2024-01-20.csv')

#point to dump directory
dump_dir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_cond/'

#read in station locations
lmp_locs = read.csv('C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/lmp_shortlist.csv') %>% 
  mutate(site_type = if_else(site_type == "stream", "tributary", site_type))

# filter for conductivity in the coves/summer ----
unique(lmp$parameter)

lmp_cond <- lmp %>% 
  filter(parameter == 'specificConductance_uScm') %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(year = format(date, '%Y')) %>% 
  mutate(month = as.numeric(format(date, '%m')))

#filter jun - sept
lmp_summer_cond = lmp_cond %>% 
  filter(month >=6 & month <=9)

#grab only select locations
lmp_summer_cond_select <- lmp_summer_cond %>% 
  left_join(lmp_locs, .) 

#grab only cove data
lmp_summer_cond_cove <- lmp_summer_cond_select %>% 
  filter(sub_site_type == 'cove')

# aggregate and join by year/site ----
lmp_cond_aggyearsite_cove <- lmp_summer_cond_cove %>% 
  filter(!is.na(value)) %>% 
  group_by(year, station, site_type, sub_site_type, lat_dd, lon_dd) %>% 
  summarize(n = n(),
            med_cond_uScm = median(value),
            max_cond_uScm = max(value),
            mean_cond_uScm = mean(value),
            thquan_cond_uScm = quantile(value, 0.75)) %>% 
  mutate(year = as.numeric(year))


## facet conductivity per site ----
ggplot(lmp_cond_aggyearsite_cove, aes(x = year, y = med_cond_uScm)) +
  geom_smooth(color = 'dark grey', se = F) +
  geom_point() +
  facet_grid(station ~ .) +
  labs(y = 'average annual conductivity (uS/cm)',
       x = 'year') +
  theme_bw() +
  theme(axis.title = element_text(size = 12,face = "bold"))
ggsave(filename = file.path(dump_dir, 'coves_annual_mean_condiuctivity.png'),
       height = 9,
       width = 6,
       dpi = 300,
       units ='in')

