# total phosphorus - connection between streams and coves?

source('phosphorus_summary.R')

library(ggthemes)

final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom

dump_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/figs/summer_tp/'


# herrick cove ----
HC <- lmp_tp_aggyearsite %>% 
  filter(station == 110| station == 830 | station ==835)

HC_mean <- HC %>% 
  pivot_wider(id_cols = year,
              values_from = mean_tp_ugl,
              names_from = station) %>% 
  select(year, `110`, `830`, `835`) %>% 
  rename(HC = `110`, 
         HerrickCoveSouth = `830`, 
         HerrickCoveNorth = `835`) %>% 
  pivot_longer(names_to = 'trib', 
               values_to = 'tp_ugl', 
               cols = c(HerrickCoveSouth, HerrickCoveNorth)) 

ggplot(HC_mean, aes(x = HC)) +
  geom_point(aes(y = tp_ugl, color = trib)) +
  scale_color_colorblind(name = 'tributary') +
  labs(title = 'Total Phosphorus: Herrick Cove',
       x = 'average in-lake total phosphorus (µg/L)',
       y = 'average tributary total phosphorus (µg/L)') +
  scale_x_continuous(limits = c(0, 10)) +
  final_theme

HC_med <- HC %>% 
  pivot_wider(id_cols = year,
              values_from = med_tp_ugl,
              names_from = station) %>% 
  select(year, `110`, `830`, `835`) %>% 
  rename(HC = `110`, 
         HerrickCoveSouth = `830`, 
         HerrickCoveNorth = `835`) %>% 
  pivot_longer(names_to = 'trib', 
               values_to = 'tp_ugl', 
               cols = c(HerrickCoveSouth, HerrickCoveNorth))

ggplot(HC_med, aes(x = HC)) +
  geom_point(aes(y = tp_ugl, color = trib)) +
  scale_color_colorblind(name = 'tributary') +
  labs(title = 'Total Phosphorus: Herrick Cove',
       x = 'median in-lake total phosphorus (µg/L)',
       y = 'median tributary total phosphorus (µg/L)') +
  scale_x_continuous(limits = c(0, 10)) +
  geom_smooth(method = 'lm', se = F, aes(x = HC, y = tp_ugl, color = trib), lty = 3) +
  final_theme +
  theme(legend.position = 'bottom')
ggsave(file.path(dump_dir, 'TP_herrick_stream_cove.jpg'), 
       height = 5, width = 6, units = 'in',
       dpi = 600)


# George's Mills ----
GM <- lmp_tp_aggyearsite %>% 
  filter(station == 10| station == 505 | station ==510)

GM_mean <- GM %>% 
  pivot_wider(id_cols = year,
              values_from = mean_tp_ugl,
              names_from = station) %>% 
  select(year, `10`, `505`, `510`) %>% 
  rename(GM = `10`, 
         OtterPondBrook = `505`, 
         MuzzeyBrook = `510`) %>% 
  pivot_longer(names_to = 'trib', 
               values_to = 'tp_ugl', 
               cols = c(OtterPondBrook, MuzzeyBrook))

ggplot(GM_mean, aes(x = GM)) +
  geom_point(aes(y = tp_ugl, color = trib)) +
  scale_color_colorblind() +
  theme_bw() +
  labs(x = 'average in-lake total phosphorus (µg/L)',
       y = 'average tributary total phosphorus (µg/L)')

GM_med <- GM %>% 
  pivot_wider(id_cols = year,
              values_from = med_tp_ugl,
              names_from = station) %>% 
  select(year, `10`, `505`, `510`) %>% 
  rename(GM = `10`, 
         OtterPondBrook = `505`, 
         MuzzeyBrook = `510`) %>% 
  pivot_longer(names_to = 'trib', 
               values_to = 'tp_ugl', 
               cols = c(OtterPondBrook, MuzzeyBrook))

ggplot(GM_med, aes(x = GM)) +
  geom_point(aes(y = tp_ugl, color = trib)) +
  scale_color_colorblind(name = 'tributary') +
  theme_bw() +
  labs(title = 'Total Phosphorus: George\'s Mills',
       x = 'median in-lake total phosphorus (µg/L)',
       y = 'median tributary total phosphorus (µg/L)')+
  geom_smooth(method = 'lm', se = F, aes(x = GM, y = tp_ugl, color = trib), lty = 3) +
  final_theme +
  theme(legend.position = 'bottom')
ggsave(file.path(dump_dir, 'TP_georges_stream_cove.jpg'), 
       height = 5, width = 6, units = 'in',
       dpi = 600)



# Jobs Creek ----
JC <- lmp_tp_aggyearsite %>% 
  filter(station == 20 | station == 540)

JC_mean <- JC %>% 
  pivot_wider(id_cols = year,
              values_from = mean_tp_ugl,
              names_from = station) %>% 
  select(year, `20`, `540`) %>% 
  rename(JCL = `20`, 
         JobsCreek = `540`) 

ggplot(JC_mean, aes(x = JCL)) +
  geom_point(aes(y = JobsCreek)) +
  scale_color_colorblind() +
  theme_bw() +
  labs(title = 'Total Phosphorus: Job\'s Creek', 
       x = 'average in-lake total phosphorus (µg/L)',
       y = 'average tributary total phosphorus (µg/L)')+
  geom_smooth(method = 'lm', se = F, aes(x = JCL, y = JobsCreek), lty = 3) +
  final_theme +
  theme(legend.position = 'bottom')
ggsave(file.path(dump_dir, 'aveTP_georges_stream_cove.jpg'), 
       height = 5, width = 6, units = 'in',
       dpi = 600)

JC_med <- JC %>% 
  pivot_wider(id_cols = year,
              values_from = med_tp_ugl,
              names_from = station) %>% 
  select(year, `20`, `540`) %>% 
  rename(JCL = `20`, 
         JobsCreek = `540`) 

ggplot(JC_med, aes(x = JCL)) +
  geom_point(aes(y = JobsCreek)) +
  scale_color_colorblind() +
  theme_bw() +
  labs(title = 'Total Phosphorus: Job\'s Creek', 
       x = 'median in-lake total phosphorus (µg/L)',
       y = 'median tributary total phosphorus (µg/L)')+
  geom_smooth(method = 'lm', se = F, aes(x = JCL, y = JobsCreek), lty = 3, color = 'black') +
  final_theme +
  theme(legend.position = 'bottom')
ggsave(file.path(dump_dir, 'medTP_georges_stream_cove.jpg'), 
       height = 5, width = 6, units = 'in',
       dpi = 600)



# Chandler's cove ----
CC <- lmp_tp_aggyearsite %>% 
  filter(station == 70 | station == 670 | station == 680)

CC_mean <- CC %>% 
  pivot_wider(id_cols = year,
              values_from = mean_tp_ugl,
              names_from = station) %>% 
  select(year, `70`, `670`, `680`) %>% 
  rename(CC = `70`,
         CB = `670`, 
         BB = `680`) %>% 
  pivot_longer(names_to = 'trib', 
               values_to = 'tp_ugl', 
               cols = c(CB, BB))

ggplot(CC_mean, aes(x = CC)) +
  geom_point(aes(y = tp_ugl, color = trib)) +
  scale_color_colorblind() +
  theme_bw() +
  labs(x = 'average in-lake total phosphorus (µg/L)',
       y = 'average tributary total phosphorus (µg/L)')

CC_med<- CC %>% 
  pivot_wider(id_cols = year,
              values_from = med_tp_ugl,
              names_from = station) %>% 
  select(year, `70`, `670`, `680`) %>% 
  rename(CC = `70`,
         ChandlerBrook = `670`, 
         BeckBrook = `680`) %>% 
  pivot_longer(names_to = 'trib', 
               values_to = 'tp_ugl', 
               cols = c(ChandlerBrook, BeckBrook))

ggplot(CC_med, aes(x = CC)) +
  geom_point(aes(y = tp_ugl, color = trib)) +
  scale_color_colorblind(name = 'tributary') +
  labs(title = 'Total Phosphorus: Chandler Cove', 
       x = 'median in-lake total phosphorus (µg/L)',
       y = 'median tributary total phosphorus (µg/L)') +
  geom_smooth(method = 'lm', se = F, aes(x = CC, y = tp_ugl, color = trib), lty = 3) +
  final_theme +
  theme(legend.position = 'bottom')
ggsave(file.path(dump_dir, 'medTP_chandler_stream_cove.jpg'), 
       height = 5, width = 6, units = 'in',
       dpi = 600)

  

