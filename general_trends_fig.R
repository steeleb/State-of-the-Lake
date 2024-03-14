# script to make general trend graphs for all params

library(tidyverse)
library(ggthemes)

dump_dir <- file.path(gen_dump_dir, "summer_overview")

if (!dir.exists(dump_dir)) {
  dir.create(dump_dir, recursive = T)
}

# list the parameters of interest
selected <- c("oxygenDissolved_mgl",
              "phosphorusTotal_mgl",
              "specificConductance_uScm",
              "turbidity_NTU",
              "waterTemperature_degC")

# filter for those and summer/in shortlist, whatnot
lmp_select <- lmp %>% 
  mutate(year = year(date)) %>% 
  filter(station %in% lmp_shortlist$station) %>% 
  filter(parameter %in% selected) %>% 
  filter(between(month(date), 6, 9)) %>%
  filter(year >=1995) %>% 
  filter(!is.na(value))%>% 
  left_join(., lmp_shortlist) 

# do some high-level QAQC 
lmp_select_filt <- lmp_select %>% 
  filter(flag %in% c("", "BDL")) %>% 
  mutate(value = if_else(parameter == "phosphorusTotal_mgl",
                         value * 1000,
                         value),
         parameter = if_else(parameter == "phosphorusTotal_mgl",
                            "phosphorusTotal_ugl",
                            parameter))

# grab do and summarize surface 
lmp_do <- lmp_select_filt %>% 
  filter(parameter == "oxygenDissolved_mgl") %>% 
  filter(depth_m <= 1) %>% 
  mutate(parameter = "surface_oxygenDissolved_mgl")

# and surface temp
lmp_temp <- lmp_select_filt %>% 
  filter(parameter == 'waterTemperature_degC') %>% 
  filter(depth_m <= 1) %>% 
  mutate(parameter = "surface_waterTemperature_degC")

lmp_select_filt <- lmp_select_filt %>% 
  filter(!(parameter %in% c("oxygenDissolved_mgl",
                          "waterTemperature_degC"))) %>% 
  filter(sub_site_type == "cove" | 
           (sub_site_type == "deep" & layer == "E") |
           sub_site_type == "tributary") %>% 
  full_join(., lmp_do) %>% 
  full_join(., lmp_temp)

lmp_summary <- lmp_select_filt %>% 
  group_by(parameter, year, sub_site_type) %>% 
  summarize(mean_value = mean(value)) %>% 
  mutate(parameter = factor(parameter, 
                            levels = c("specificConductance_uScm",
                                       "phosphorusTotal_ugl",
                                       "turbidity_NTU",
                                       "surface_waterTemperature_degC",
                                       "surface_oxygenDissolved_mgl"),
                            labels = c("specific conductance (uS/cm)",
                                       "total phosphorus (ug/L)",
                                       "turbidity (NTU)",
                                       "surface water temp (C)",
                                       "surface dissolved oxygen (mg/L)")))


ggplot(lmp_summary, aes(x = year, y = mean_value, color = sub_site_type)) +
  facet_wrap("parameter", scales = 'free_y') +
  geom_point() +
  theme_bw() +
  labs(x = "year",
       y = "average value across sites",
       color = "site type") +
  scale_color_colorblind() +
  theme(legend.position = "bottom")
ggsave(file.path(dump_dir, "summer_overview_grid.png"),
       height = 6,
       width = 9,
       dpi = 600,
       units = "in")

ggplot(lmp_summary, aes(x = year, y = mean_value, color = sub_site_type)) +
  facet_grid(parameter ~ ., scales = 'free_y') +
  geom_point() +
  theme_bw() +
  labs(x = "year",
       y = "average value across sites",
       color = "site type") +
  scale_color_colorblind() +
  theme(legend.position = "bottom")
ggsave(file.path(dump_dir, "summer_overview_facet.png"),
       height = 6,
       width = 9,
       dpi = 600,
       units = "in")

lmp_summary %>% 
  filter(!(parameter %in% c("surface water temp (C)",
                            "surface dissolved oxygen (mg/L)"))) %>% 
  ggplot(., aes(x = year, y = mean_value, color = sub_site_type)) +
  facet_wrap("parameter", scales = 'free_y', nrow = 3) +
  geom_point() +
  theme_bw() +
  labs(x = "year",
       y = "average value across sites",
       color = "site type") +
  scale_color_colorblind() +
  theme(legend.position = "bottom")
ggsave(file.path(dump_dir, "summer_overview_notempDO.png"),
       height = 6,
       width = 9,
       dpi = 600,
       units = "in")

lmp_summary %>% 
  filter(!(parameter %in% c("surface water temp (C)",
                            "surface dissolved oxygen (mg/L)"))) %>% 
  ggplot(., aes(x = year, y = mean_value, color = sub_site_type)) +
  facet_wrap("parameter", scales = 'free_y') +
  geom_point() +
  theme_bw() +
  labs(x = "year",
       y = "average value across sites",
       color = "site type") +
  scale_color_colorblind() +
  theme(legend.position = "bottom")
ggsave(file.path(dump_dir, "summer_overview_notempDO_across.png"),
       height = 3,
       width = 9,
       dpi = 600,
       units = "in")
