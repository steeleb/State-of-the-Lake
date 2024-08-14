# do figs

library(tidyverse)
library(ggthemes)
library(RcppRoll)
library(gganimate)
library(gifski)
library(av)

dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/buoy_do/'

final_theme=theme_bw() +
  theme(axis.text=element_text(size=8),
        plot.title=element_text(size=14, face='bold', hjust=0.5),
        strip.text=element_text(size=10))

make_do_plot <- function(data, year) {
  df <- data %>% 
    mutate(date = ymd(format(datetime, '%Y-%m-%d'))) %>% 
    group_by(date, parameter, depth) %>% 
    summarize(mean_value = mean(value, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(parameter = factor(parameter, 
                              levels = c('waterTemperature',
                                         'oxygenDissolved',
                                         'oxygenDissolvedPercentOfSaturation'),
                              labels = c('water\ntemperature\n(deg C)',
                                         'dissolved\noxygen\n(mg/L)',
                                         'dissolved\noxygen\nsaturation\n(%)')))
  
  ggplot(df, aes(x = date, y = mean_value, color = depth)) +
    geom_line() +
    facet_grid(parameter ~ ., scales = "free_y") +
    scale_color_colorblind() +
    scale_x_date(limits = c(ymd(paste0(year, "-05-01")),
                            ymd(paste0(year, "-11-01"))),
                 breaks = seq.Date(ymd(paste0(year, "-05-01")),
                                   ymd(paste0(year, "-11-01")),
                                   "1 month"),
                 labels = format(seq.Date(ymd(paste0(year, "-05-01")),
                                   ymd(paste0(year, "-11-01")),
                                   "1 month"), '%b')) +
    labs(x = NULL, y = NULL, 
         color = 'depth (m)',
         title = year) +
    final_theme +
    theme(legend.position = 'bottom')
  ggsave(file.path(dump_dir, paste0("dissolved_oxygen_facet_", year, ".png")),
         dpi = 200, width = 5, height = 4, units = 'in')
          
}

one_var_do_plot <- function(data, year, param, min, max) {
  df <- data %>% 
    mutate(date = ymd(format(datetime, '%Y-%m-%d'))) %>% 
    group_by(date, parameter, depth) %>% 
    summarize(mean_value = mean(value, na.rm = T)) %>% 
    ungroup() %>%
    filter(parameter == param) %>% 
    mutate(parameter = factor(parameter, 
                              levels = c('waterTemperature',
                                         'oxygenDissolved',
                                         'oxygenDissolvedPercentOfSaturation'),
                              labels = c('water\ntemperature\n(deg C)',
                                         'dissolved\noxygen\n(mg/L)',
                                         'dissolved\noxygen\nsaturation\n(%)')))
  
  ggplot(df, aes(x = date, y = mean_value, color = depth)) +
    geom_line() +
    facet_grid(parameter ~ ., scales = "free_y") +
    scale_color_colorblind() +
    scale_y_continuous(limits = c(min, max)) +
    scale_x_date(limits = c(ymd(paste0(year, "-05-01")),
                            ymd(paste0(year, "-11-01"))),
                 breaks = seq.Date(ymd(paste0(year, "-05-01")),
                                   ymd(paste0(year, "-11-01")),
                                   "1 month"),
                 labels = format(seq.Date(ymd(paste0(year, "-05-01")),
                                          ymd(paste0(year, "-11-01")),
                                          "1 month"), '%b')) +
    labs(x = NULL, y = NULL, 
         color = 'depth (m)',
         title = year) +
    final_theme +
    theme(legend.position = 'bottom')
  
  ggsave(file.path(dump_dir, paste0("do_", param, "_", year, ".png")),
         dpi = 200, width = 5, height = 2.5, units = 'in')
  
}


# 2016 do figures ####

buoy_2016 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2016_do_L1_v2022.csv')

names(buoy_2016)

# check locations
unique(buoy_2016$location)

# check flags
unique(buoy_2016_upper$flag_do1p5m)
unique(buoy_2016_lower$flag_do10p5m)

buoy_2016_vert <- buoy_2016 %>% 
  filter(location == 'loon') %>% 
  pivot_longer(cols = c(waterTemperature_DO_degC_1p5m,
                        oxygenDissolved_mgl_1p5m,
                        oxygenDissolvedPercentOfSaturation_pct_1p5m,
                        waterTemperature_DO_degC_10p5m, 
                        oxygenDissolved_mgl_10p5m, 
                        oxygenDissolvedPercentOfSaturation_pct_10p5m),
               names_to = 'parameter',
               values_to = 'value') %>% 
  group_by(parameter) %>% 
  mutate(depth = last(unlist(str_split(parameter, '_'))),
         depth = str_replace(depth, 'p', '.'),
         depth = str_remove(depth, 'm'),
         parameter = first(unlist(str_split(parameter, '_')))) %>% 
  ungroup()

make_do_plot(buoy_2016_vert, 2016)

one_var_do_plot(data = buoy_2016_vert, year = 2016, min = 5, max = 30, param = "waterTemperature")
one_var_do_plot(data = buoy_2016_vert, year = 2016, min = 6, max = 13, param = "oxygenDissolved")
one_var_do_plot(data = buoy_2016_vert, year = 2016, min = 65, max = 115, param = "oxygenDissolvedPercentOfSaturation")

# 2017 and 2018 have insufficient DO data  ####

# 2019 do figures ####

buoy_2019 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2019_do_L1_v2022.csv')

names(buoy_2019)

# check locations
unique(buoy_2019$location)

# check flags
unique(buoy_2019$flag_do0p25m)
unique(buoy_2019$flag_do10m)

buoy_2019_vert <- buoy_2019 %>% 
  filter(location == 'loon') %>% 
  pivot_longer(cols = c(waterTemperature_DO_degC_0p25m,
                        oxygenDissolved_mgl_0p25m,
                        oxygenDissolvedPercentOfSaturation_pct_0p25m,
                        waterTemperature_DO_degC_10m, 
                        oxygenDissolved_mgl_10m, 
                        oxygenDissolvedPercentOfSaturation_pct_10m),
               names_to = 'parameter',
               values_to = 'value') %>% 
  group_by(parameter) %>% 
  mutate(depth = last(unlist(str_split(parameter, '_'))),
         depth = str_replace(depth, 'p', '.'),
         depth = str_remove(depth, 'm'),
         parameter = first(unlist(str_split(parameter, '_')))) %>% 
  ungroup()

make_do_plot(buoy_2019_vert, 2019)

one_var_do_plot(data = buoy_2019_vert, year = 2019, min = 5, max = 30, param = "waterTemperature")
one_var_do_plot(data = buoy_2019_vert, year = 2019, min = 6, max = 13, param = "oxygenDissolved")
one_var_do_plot(data = buoy_2019_vert, year = 2019, min = 65, max = 115, param = "oxygenDissolvedPercentOfSaturation")


# 2020 do figures ####

buoy_2020 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2020_do_L1_v2022.csv')

names(buoy_2020)

# check locations
unique(buoy_2020$location)

# check flags
unique(buoy_2020$flag_do0p25m)
unique(buoy_2020$flag_do10m)

buoy_2020 <- buoy_2020 %>% 
  mutate(across(c(waterTemperature_DO_degC_10m, 
                  oxygenDissolved_mgl_10m, 
                  oxygenDissolvedPercentOfSaturation_pct_10m),
                ~ if_else(grepl("x", flag_do10m), NA_real_, .)))

buoy_2020_vert <- buoy_2020 %>% 
  filter(location == 'loon') %>% 
  pivot_longer(cols = c(waterTemperature_DO_degC_0p25m,
                        oxygenDissolved_mgl_0p25m,
                        oxygenDissolvedPercentOfSaturation_pct_0p25m,
                        waterTemperature_DO_degC_10m, 
                        oxygenDissolved_mgl_10m, 
                        oxygenDissolvedPercentOfSaturation_pct_10m),
               names_to = 'parameter',
               values_to = 'value') %>% 
  group_by(parameter) %>% 
  mutate(depth = last(unlist(str_split(parameter, '_'))),
         depth = str_replace(depth, 'p', '.'),
         depth = str_remove(depth, 'm'),
         parameter = first(unlist(str_split(parameter, '_')))) %>% 
  ungroup()

make_do_plot(buoy_2020_vert, 2020)

one_var_do_plot(data = buoy_2020_vert, year = 2020, min = 5, max = 30, param = "waterTemperature")
one_var_do_plot(data = buoy_2020_vert, year = 2020, min = 6, max = 13, param = "oxygenDissolved")
one_var_do_plot(data = buoy_2020_vert, year = 2020, min = 65, max = 115, param = "oxygenDissolvedPercentOfSaturation")


# 2021 do figures ####

do_2021 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2021_do_L1_v2022.csv')
exo_2021 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/exo/2021_exo_L1_v2022.csv')
names(do_2021)
names(exo_2021)

# check locations
unique(do_2021$location)
unique(exo_2021$location)

# check flags
unique(do_2021$flag_do10m)
unique(exo_2021$flag_exodo)

buoy_2021_vert <- full_join(do_2021, exo_2021) %>% 
  select(datetime, location, 
         waterTemperature_EXO_degC_1m,
         oxygenDissolved_mgl_1m,
         oxygenDissolvedPercentOfSaturation_pct_1m,
         waterTemperature_DO_degC_10m, 
         oxygenDissolved_mgl_10m, 
         oxygenDissolvedPercentOfSaturation_pct_10m) %>% 
  filter(location == 'loon') %>% 
  pivot_longer(cols = c(waterTemperature_EXO_degC_1m,
                        oxygenDissolved_mgl_1m,
                        oxygenDissolvedPercentOfSaturation_pct_1m,
                        waterTemperature_DO_degC_10m, 
                        oxygenDissolved_mgl_10m, 
                        oxygenDissolvedPercentOfSaturation_pct_10m),
               names_to = 'parameter',
               values_to = 'value') %>% 
  group_by(parameter) %>% 
  mutate(depth = last(unlist(str_split(parameter, '_'))),
         depth = str_replace(depth, 'p', '.'),
         depth = str_remove(depth, 'm'),
         parameter = first(unlist(str_split(parameter, '_')))) %>% 
  ungroup()

make_do_plot(buoy_2021_vert, 2021)

one_var_do_plot(data = buoy_2021_vert, year = 2021, min = 5, max = 30, param = "waterTemperature")
one_var_do_plot(data = buoy_2021_vert, year = 2021, min = 6, max = 13, param = "oxygenDissolved")
one_var_do_plot(data = buoy_2021_vert, year = 2021, min = 65, max = 115, param = "oxygenDissolvedPercentOfSaturation")



# 2022 do figures ####

do_2022 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/do/2022_do_L1_v2022.csv')
exo_2022 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/buoy data/data/all sensors/L1/exo/2022_exo_L1_v2022.csv')
names(do_2022)
names(exo_2022)

# check locations
unique(do_2022$location)
unique(exo_2022$location)

# check flags
unique(do_2022$flag_do10m)
unique(exo_2022$flag_exodo)

buoy_2022_vert <- full_join(do_2022, exo_2022) %>% 
  select(datetime, location, 
         waterTemperature_EXO_degC_1m,
         oxygenDissolved_mgl_1m,
         oxygenDissolvedPercentOfSaturation_pct_1m,
         waterTemperature_DO_degC_10m, 
         oxygenDissolved_mgl_10m, 
         oxygenDissolvedPercentOfSaturation_pct_10m) %>% 
  filter(location == 'loon') %>% 
  pivot_longer(cols = c(waterTemperature_EXO_degC_1m,
                        oxygenDissolved_mgl_1m,
                        oxygenDissolvedPercentOfSaturation_pct_1m,
                        waterTemperature_DO_degC_10m, 
                        oxygenDissolved_mgl_10m, 
                        oxygenDissolvedPercentOfSaturation_pct_10m),
               names_to = 'parameter',
               values_to = 'value') %>% 
  group_by(parameter) %>% 
  mutate(depth = last(unlist(str_split(parameter, '_'))),
         depth = str_replace(depth, 'p', '.'),
         depth = str_remove(depth, 'm'),
         parameter = first(unlist(str_split(parameter, '_')))) %>% 
  ungroup()

make_do_plot(buoy_2022_vert, 2022)

one_var_do_plot(data = buoy_2022_vert, year = 2022, min = 5, max = 30, param = "waterTemperature")
one_var_do_plot(data = buoy_2022_vert, year = 2022, min = 6, max = 13, param = "oxygenDissolved")
one_var_do_plot(data = buoy_2022_vert, year = 2022, min = 65, max = 115, param = "oxygenDissolvedPercentOfSaturation")


# 2023 do figures ####
# 
# do_2023 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/tempfiles/2023_do.csv')
# exo_2023 <- read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/tempfiles/2023_exo.csv')
# names(do_2023)
# names(exo_2023)
# 
# # check locations
# unique(do_2023$location)
# unique(exo_2023$location)
# 
# # check flags
# unique(do_2023$flag_do10m)
# unique(exo_2023$flag_exodo)
# 
# buoy_2023_vert <- full_join(do_2023, exo_2023) %>% 
#   select(datetime, location, 
#          waterTemperature_EXO_degC_1m,
#          oxygenDissolved_mgl_1m,
#          oxygenDissolvedPercentOfSaturation_pct_1m,
#          waterTemperature_DO_degC_10m, 
#          oxygenDissolved_mgl_10m, 
#          oxygenDissolvedPercentOfSaturation_pct_10m) %>% 
#   filter(location == 'loon') %>% 
#   pivot_longer(cols = c(waterTemperature_EXO_degC_1m,
#                         oxygenDissolved_mgl_1m,
#                         oxygenDissolvedPercentOfSaturation_pct_1m,
#                         waterTemperature_DO_degC_10m, 
#                         oxygenDissolved_mgl_10m, 
#                         oxygenDissolvedPercentOfSaturation_pct_10m),
#                names_to = 'parameter',
#                values_to = 'value') %>% 
#   group_by(parameter) %>% 
#   mutate(depth = last(unlist(str_split(parameter, '_'))),
#          depth = str_replace(depth, 'p', '.'),
#          depth = str_remove(depth, 'm'),
#          parameter = first(unlist(str_split(parameter, '_')))) %>% 
#   ungroup()
# 
# make_do_plot(buoy_2023_vert, 2023)
# 
# 
# 


## 2022 animations ####
df <- buoy_2022_vert %>% 
  mutate(date = ymd(format(datetime, '%Y-%m-%d'))) %>% 
  mutate(parameter = factor(parameter, 
                            levels = c('waterTemperature',
                                       'oxygenDissolved',
                                       'oxygenDissolvedPercentOfSaturation'),
                            labels = c('water\ntemperature\n(deg C)',
                                       'dissolved\noxygen\n(mg/L)',
                                       'dissolved\noxygen\nsaturation\n(%)'))) %>% 
  filter(!is.na(value)) %>% 
  filter(date >= '2022-07-18' & date <= '2022-08-01') %>% 
  filter(depth == 1) %>% 
  filter(parameter == "dissolved\noxygen\nsaturation\n(%)")


ggplot(df, aes(x = datetime, y = value)) +
  geom_line(color = 'black') +
  facet_grid(parameter ~ ., scales = "free_y") +
  labs(x = NULL, y = NULL) +
  final_theme +
  theme(legend.position = 'bottom') +
  scale_x_datetime(date_minor_breaks = '1 day')

ggsave(filename = file.path(dump_dir, "July 2022 saturation zoom.png"),
       width = 8,
       height = 4,
       units = "in",
       dpi = 200)
