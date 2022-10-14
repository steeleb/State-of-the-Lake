####tributary conductivity ####
trib_cond_all <-read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/CHEMISTRY.xlsx', 
                      sheet = 'CHEMISTRY') %>% 
  select_all(., 'tolower') %>% 
  select(station, date, cond) %>% 
  mutate(year = as.numeric(format(date, '%Y')))

trib_cond_all <- trib_cond_all %>% 
  mutate(cond = case_when(cond == -99.99 ~ NA_real_,
                          cond == -99 ~ NA_real_,
                          TRUE ~ cond)) %>% 
  filter(!is.na(cond))
range(trib_cond_all$cond, na.rm = T)
str(trib_cond_all)

trib_cond_since2010 <- trib_cond_all %>% 
  filter(year >= 2010) %>% 
  # ggplot(., aes(x=date, y=cond)) +
  #   geom_point() +
  #   facet_grid(station~., scales = 'free_y') %>% 
  group_by(station) %>% 
  summarize(mean_cond = mean(cond))

write_csv(trib_cond_since2010, 'C:/Users/steeleb/Dropbox/Lake Sunapee/misc/state of the lake/r program sotl/2018 figures/cond/trib_cond_since2010.csv')

trib_cond <-read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/CHEMISTRY.xlsx', 
          sheet = 'CHEMISTRY') %>% 
  select_all(., 'tolower') %>% 
  filter(station == 505 | station == 510 | station == 540 | station == 670 | station == 760 | station == 788 | station == 790 | station == 800 | station == 805 | station == 830 | station == 835) %>% 
  select(station, date, cond) %>% 
  mutate(year = as.numeric(format(date, '%Y')))

trib_cond <- trib_cond %>% 
  mutate(cond = case_when(cond == -99.99 ~ NA_real_,
                          cond == -99 ~ NA_real_,
                          TRUE ~ cond)) %>% 
  filter(!is.na(cond))
range(trib_cond$cond, na.rm = T)
str(trib_cond)

ggplot(trib_cond,
       aes(x = year, y = cond, group = year)) +
  geom_boxplot() +
  facet_grid(station ~ .) +
  labs(title = 'conductivity at the tributaries',
       x= NULL, 
       y = 'conductivity (uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(trib_cond,
       aes(x = year, y = cond, group = year)) +
  geom_boxplot() +
  facet_grid(station ~ ., scales = 'free_y') +
  labs(title = 'conductivity at the tributaries',
       x= NULL, 
       y = 'conductivity (uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(trib_cond,
              subset = date >= as.POSIXct('2010-01-01', tz='UTC')),
       aes(x = date, y = cond)) +
  geom_point() +
  facet_grid(station ~ .) +
  labs(title = 'summer patterns of conductivity',
       x= NULL, 
       y = 'conductivity (uS/cm)') +
  final_theme

ggplot(subset(trib_cond,
              subset = date >= as.POSIXct('2010-01-01', tz='UTC')),
       aes(x = date, y = cond)) +
  geom_point() +
  facet_grid(station ~ ., scales = 'free_y') +
  labs(title = 'summer patterns of conductivity',
       x= NULL, 
       y = 'conductivity (uS/cm)') +
  final_theme


ggplot(subset(trib_cond,
              subset=station == 505),
       aes(x = year, y = cond, group = year)) +
  geom_boxplot() +
  labs(title = 'conductivity at 505',
       x= NULL, 
       y = 'conductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme
  

ggplot(subset(trib_cond,
              subset=station == 510),
       aes(x = year, y = cond, group = year)) +
  geom_boxplot() +
  labs(title = 'conductivity at 510',
       x= NULL, 
       y = 'conductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme

ggplot(subset(trib_cond,
              subset=station == 540),
       aes(x = year, y = cond, group = year)) +
  geom_boxplot() +
  labs(title = 'conductivity at 540',
       x= NULL, 
       y = 'conductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(trib_cond,
              subset=station == 670),
       aes(x = year, y = cond, group = year)) +
  geom_boxplot() +
  labs(title = 'conductivity at 670',
       x= NULL, 
       y = 'conductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(trib_cond,
              subset=station == 760),
       aes(x = year, y = cond, group = year)) +
  geom_boxplot() +
  labs(title = 'conductivity at 760',
       x= NULL, 
       y = 'conductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(trib_cond,
              subset=station == 788),
       aes(x = year, y = cond, group = year)) +
  geom_boxplot() +
  labs(title = 'conductivity at 788',
       x= NULL, 
       y = 'conductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(trib_cond,
              subset=station == 790),
       aes(x = year, y = cond, group = year)) +
  geom_boxplot() +
  labs(title = 'conductivity at 790',
       x= NULL, 
       y = 'conductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(trib_cond,
              subset=(station==800 & cond <200)),
       aes(x = year, y = cond, group = year)) +
  geom_boxplot() +
  labs(title = 'conductivity at 800',
       x= NULL, 
       y = 'conductivity\n(uS/cm)') +
  annotate(geom='point', x = 2008, y = 155, shape = 8, color = 'red', size = 2) +
  annotate(geom='point', x = 2004, y = 155, shape = 8, color = 'red', size = 2) +
  annotate(geom='point', x = 1997, y = 155, shape = 8, color = 'red', size = 2) +
  scale_x_continuous() +
  final_theme


ggplot(subset(trib_cond,
              subset=(station == 805 & cond < 700)),
       aes(x = year, y = cond, group = year)) +
  geom_boxplot() +
  labs(title = 'conductivity at 805',
       x= NULL, 
       y = 'conductivity\n(uS/cm)') +
  annotate(geom='point', x = 1998, y = 210, shape = 8, color = 'red', size = 2) +
  scale_x_continuous() +
  final_theme


ggplot(subset(trib_cond,
              subset=station == 830),
       aes(x = year, y = cond, group = year)) +
  geom_boxplot() +
  labs(title = 'conductivity at 830',
       x= NULL, 
       y = 'conductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(trib_cond,
              subset=station == 835),
       aes(x = year, y = cond, group = year)) +
  geom_boxplot() +
  labs(title = 'conductivity at 835',
       x= NULL, 
       y = 'conductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ann_trib_cond <- trib_cond %>% 
  mutate(month = as.numeric(format(date, '%m'))) %>% 
  filter(month >= 6 & month <10) %>% 
  group_by(station, year) %>% 
  summarise(mean_cond = mean(cond))

ggplot(ann_trib_cond,
       aes(x = year, y = mean_cond)) +
  geom_point() +
  facet_grid(station ~ .) +
  labs(title = 'conductivity at the tributaries',
       x= NULL, 
       y = 'average summer conductivity (uS/cm)') +
  final_theme

ggplot(ann_trib_cond,
       aes(x = year, y = mean_cond)) +
  geom_point() +
  facet_grid(station ~ ., scales = 'free_y') +
  labs(title = 'conductivity at the tributaries',
       x= NULL, 
       y = 'average summer conductivity (uS/cm)') +
  final_theme

ggplot(subset(ann_trib_cond,
              subset=station == 505),
       aes(x = year, y =mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at 505',
       x= NULL, 
       y = 'summer mean\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(ann_trib_cond,
              subset=station == 510),
       aes(x = year, y =mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at 510',
       x= NULL, 
       y = 'summer mean\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme

ggplot(subset(ann_trib_cond,
              subset=station == 540),
       aes(x = year, y =mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at 540',
       x= NULL, 
       y = 'summer mean\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(ann_trib_cond,
              subset=station == 670),
       aes(x = year, y =mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at 670',
       x= NULL, 
       y = 'summer mean\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(ann_trib_cond,
              subset=station == 760),
       aes(x = year, y =mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at 760',
       x= NULL, 
       y = 'summer mean\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(ann_trib_cond,
              subset=station == 788),
       aes(x = year, y =mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at 788',
       x= NULL, 
       y = 'summer mean\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(ann_trib_cond,
              subset=station == 790),
       aes(x = year, y =mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at 790',
       x= NULL, 
       y = 'summer mean\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(ann_trib_cond,
              subset=(station==800)),
       aes(x = year, y =mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at 800',
       x= NULL, 
       y = 'summer mean\nconductivity\n(uS/cm)') +
  final_theme


ggplot(subset(ann_trib_cond,
              subset=(station == 805)),
       aes(x = year, y =mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at 805',
       x= NULL, 
       y = 'summer mean\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(ann_trib_cond,
              subset=station == 830),
       aes(x = year, y =mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at 830',
       x= NULL, 
       y = 'summer mean\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(ann_trib_cond,
              subset=station == 835),
       aes(x = year, y =mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at 835',
       x= NULL, 
       y = 'summer mean\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


med_trib_cond <- trib_cond %>% 
  mutate(month = as.numeric(format(date, '%m'))) %>% 
  filter(month >= 6 & month <10) %>% 
  group_by(station, year) %>% 
  summarise(med_cond = median(cond))

ggplot(med_trib_cond,
       aes(x = year, y = med_cond)) +
  geom_point() +
  facet_grid(station ~ .) +
  labs(title = 'conductivity at the tributaries',
       x= NULL, 
       y = 'median summer conductivity (uS/cm)') +
  final_theme

ggplot(med_trib_cond,
       aes(x = year, y = med_cond)) +
  geom_point() +
  facet_grid(station ~ ., scales = 'free_y') +
  labs(title = 'conductivity at the tributaries',
       x= NULL, 
       y = 'median summer conductivity (uS/cm)') +
  final_theme

ggplot(subset(med_trib_cond,
              subset=station == 505),
       aes(x = year, y =med_cond)) +
  geom_point() +
  labs(title = 'conductivity at 505',
       x= NULL, 
       y = 'summer median\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(med_trib_cond,
              subset=station == 510),
       aes(x = year, y =med_cond)) +
  geom_point() +
  labs(title = 'conductivity at 510',
       x= NULL, 
       y = 'summer median\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme

ggplot(subset(med_trib_cond,
              subset=station == 540),
       aes(x = year, y =med_cond)) +
  geom_point() +
  labs(title = 'conductivity at 540',
       x= NULL, 
       y = 'summer median\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(med_trib_cond,
              subset=station == 670),
       aes(x = year, y =med_cond)) +
  geom_point() +
  labs(title = 'conductivity at 670',
       x= NULL, 
       y = 'summer median\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(med_trib_cond,
              subset=station == 760),
       aes(x = year, y =med_cond)) +
  geom_point() +
  labs(title = 'conductivity at 760',
       x= NULL, 
       y = 'summer median\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(med_trib_cond,
              subset=station == 788),
       aes(x = year, y =med_cond)) +
  geom_point() +
  labs(title = 'conductivity at 788',
       x= NULL, 
       y = 'summer median\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(med_trib_cond,
              subset=station == 790),
       aes(x = year, y =med_cond)) +
  geom_point() +
  labs(title = 'conductivity at 790',
       x= NULL, 
       y = 'summer median\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(med_trib_cond,
              subset=(station==800)),
       aes(x = year, y =med_cond)) +
  geom_point() +
  labs(title = 'conductivity at 800',
       x= NULL, 
       y = 'summer median\nconductivity\n(uS/cm)') +
  final_theme


ggplot(subset(med_trib_cond,
              subset=(station == 805)),
       aes(x = year, y =med_cond)) +
  geom_point() +
  labs(title = 'conductivity at 805',
       x= NULL, 
       y = 'summer median\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(med_trib_cond,
              subset=station == 830),
       aes(x = year, y =med_cond)) +
  geom_point() +
  labs(title = 'conductivity at 830',
       x= NULL, 
       y = 'summer median\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme


ggplot(subset(med_trib_cond,
              subset=station == 835),
       aes(x = year, y =med_cond)) +
  geom_point() +
  labs(title = 'conductivity at 835',
       x= NULL, 
       y = 'summer median\nconductivity\n(uS/cm)') +
  scale_x_continuous() +
  final_theme

#### littoral conductivity ####
lit_cond <-read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/CHEMISTRY.xlsx', 
                      sheet = 'CHEMISTRY') %>% 
  select_all(., 'tolower') %>% 
  filter(station == 10 | station == 20 | station == 30 | station == 60 | station == 70 | station == 80 | station == 90 | station ==100 | station == 100.1 | station == 110) %>% 
  select(station, date, cond) %>% 
  mutate(year = as.numeric(format(date, '%Y')),
         month = as.numeric(format(date, '%m'))) %>% 
  filter(month >=6 & month < 10)

lit_cond <- lit_cond %>% 
  mutate(cond = case_when(cond == -99.99 ~ NA_real_,
                          cond == -99 ~ NA_real_,
                          TRUE ~ cond)) %>% 
  filter(!is.na(cond))
range(lit_cond$cond, na.rm = T)
str(lit_cond)


# ggplot(lit_cond,
#        aes(x = date, y = cond)) +
#   geom_point() +
#   facet_grid(station ~ .) +
#   labs(title = 'conductivity at the littoral stations 2010-2018',
#        x= NULL, 
#        y = 'conductivity (uS/cm)') +
#   final_theme
# 
# ggplot(lit_cond,
#        aes(x = year, y = cond, group = year)) +
#   geom_boxplot() +
#   facet_grid(station ~ .) +
#   labs(title = 'conductivity at the littoral stations',
#        x= NULL, 
#        y = 'conductivity (uS/cm)') +
#   scale_x_continuous() +
#   final_theme
# 
# ggplot(lit_cond,
#        aes(x = year, y = cond, group = year)) +
#   geom_boxplot() +
#   facet_grid(station ~ ., scales = 'free_y') +
#   labs(title = 'conductivity at the littoral stations',
#        x= NULL, 
#        y = 'conductivity (uS/cm)') +
#   scale_x_continuous() +
#   final_theme
# 
# ggplot(subset(lit_cond,
#               subset=station==10),
#        aes(x = year, y = cond, group = year)) +
#   geom_boxplot() +
#   labs(title = 'conductivity at station 10',
#        x= NULL, 
#        y = 'conductivity (uS/cm)') +
#   final_theme
# 
# ggplot(subset(lit_cond,
#               subset=station==20),
#        aes(x = year, y = cond, group = year)) +
#   geom_boxplot() +
#   labs(title = 'conductivity at station 20',
#        x= NULL, 
#        y = 'conductivity (uS/cm)') +
#   final_theme
# 
# ggplot(subset(lit_cond,
#               subset=station==30),
#        aes(x = year, y = cond, group = year)) +
#   geom_boxplot() +
#   labs(title = 'conductivity at station 30',
#        x= NULL, 
#        y = 'conductivity (uS/cm)') +
#   final_theme
# 
# ggplot(subset(lit_cond,
#               subset=station==60),
#        aes(x = year, y = cond, group = year)) +
#   geom_boxplot() +
#   labs(title = 'conductivity at station 60',
#        x= NULL, 
#        y = 'conductivity (uS/cm)') +
#   final_theme
# 
# ggplot(subset(lit_cond,
#               subset=station==70),
#        aes(x = year, y = cond, group = year)) +
#   geom_boxplot() +
#   labs(title = 'conductivity at station 70',
#        x= NULL, 
#        y = 'conductivity (uS/cm)') +
#   final_theme
# 
# ggplot(subset(lit_cond,
#               subset=station==80),
#        aes(x = year, y = cond, group = year)) +
#   geom_boxplot() +
#   labs(title = 'conductivity at station 80',
#        x= NULL, 
#        y = 'conductivity (uS/cm)') +
#   final_theme
# 
# ggplot(subset(lit_cond,
#               subset=station==90),
#        aes(x = year, y = cond, group = year)) +
#   geom_boxplot() +
#   labs(title = 'conductivity at station 90',
#        x= NULL, 
#        y = 'conductivity (uS/cm)') +
#   final_theme
# 
# ggplot(subset(lit_cond,
#               subset=(station==100 | station==100.1)),
#        aes(x = year, y = cond, group = year)) +
#   geom_boxplot() +
#   labs(title = 'conductivity at station 100 and 100.1',
#        x= NULL, 
#        y = 'conductivity (uS/cm)') +
#   final_theme
# 
# ggplot(subset(lit_cond,
#               subset=station==110),
#        aes(x = year, y = cond, group = year)) +
#   geom_boxplot() +
#   labs(title = 'conductivity at station 110',
#        x= NULL, 
#        y = 'conductivity (uS/cm)') +
#   final_theme


ann_lit_cond <- lit_cond %>% 
  group_by(station, year) %>% 
  summarize(mean_cond = mean(cond))

ggplot(ann_lit_cond,
       aes(x = year, y = mean_cond)) +
  geom_smooth(method = 'lm', se =F, color = 'dark gray')+
  geom_point() +
  facet_grid(station ~ .) +
  labs(title = 'conductivity at littoral sites',
       x= NULL,
       y = 'average summer conductivity (uS/cm)') +
  final_theme

ggplot(subset(ann_lit_cond,
              subset=station==10),
       aes(x = year, y = mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at station 10',
       x= NULL,
       y = 'average summer conductivity (uS/cm)') +
  final_theme

ggplot(subset(ann_lit_cond,
              subset=station==20),
       aes(x = year, y = mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at station 20',
       x= NULL,
       y = 'average summer conductivity (uS/cm)') +
  final_theme

ggplot(subset(ann_lit_cond,
              subset=station==30),
       aes(x = year, y = mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at station 30',
       x= NULL,
       y = 'average summer conductivity (uS/cm)') +
  final_theme

ggplot(subset(ann_lit_cond,
              subset=station==60),
       aes(x = year, y = mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at station 60',
       x= NULL,
       y = 'average summer conductivity (uS/cm)') +
  final_theme

ggplot(subset(ann_lit_cond,
              subset=station==70),
       aes(x = year, y = mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at station 70',
       x= NULL,
       y = 'average summer conductivity (uS/cm)') +
  final_theme

ggplot(subset(ann_lit_cond,
              subset=station==80),
       aes(x = year, y = mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at station 80',
       x= NULL,
       y = 'average summer conductivity (uS/cm)') +
  final_theme

ggplot(subset(ann_lit_cond,
              subset=station==90),
       aes(x = year, y = mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at station 90',
       x= NULL,
       y = 'average summer conductivity (uS/cm)') +
  final_theme

ggplot(subset(ann_lit_cond,
              subset=(station==100 | station==100.1)),
       aes(x = year, y = mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at station 100 and 100.1',
       x= NULL,
       y = 'average summer conductivity (uS/cm)') +
  final_theme

ggplot(subset(ann_lit_cond,
              subset=station==110),
       aes(x = year, y = mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at station 110',
       x= NULL,
       y = 'average summer conductivity (uS/cm)') +
  final_theme

#### pelagic conductivity ####
pel_cond <-read_xlsx('C:/Users/steeleb/Dropbox/Lake Sunapee/long term Sunapee data/raw data/LMP files/LMP 2017/CHEMISTRY.xlsx', 
                     sheet = 'CHEMISTRY') %>% 
  select_all(., 'tolower') %>% 
  filter(station == 200 | station == 210 | station == 220 | station == 230) %>% 
  select(station, date, cond) %>% 
  mutate(year = as.numeric(format(date, '%Y')),
         month = as.numeric(format(date, '%m'))) %>% 
  filter(month >=6 & month < 10)

pel_cond <- pel_cond %>% 
  mutate(cond = case_when(cond == -99.99 ~ NA_real_,
                          cond == -99 ~ NA_real_,
                          cond >= 7500 ~ NA_real_,
                          TRUE ~ cond)) %>% 
  filter(!is.na(cond))
range(pel_cond$cond, na.rm = T)
str(pel_cond)
 
ann_pel_cond <- pel_cond %>% 
  group_by(station, year) %>% 
  summarise(mean_cond = mean(cond),
            med_cond = median(cond))

ggplot(subset(ann_pel_cond,
              subset=(station == 200)),
       aes(x = year, y = mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at station 200',
       x= NULL,
       y = 'average summer\nconductivity\n(uS/cm)') +
  final_theme

ggplot(subset(ann_pel_cond,
              subset=(station == 210)),
       aes(x = year, y = mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at station 210',
       x= NULL,
       y = 'average summer\nconductivity\n(uS/cm)') +
  final_theme


ggplot(subset(ann_pel_cond,
              subset=(station == 220)),
       aes(x = year, y = mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at station 220',
       x= NULL,
       y = 'average summer\nconductivity\n(uS/cm)') +
  final_theme

ggplot(subset(ann_pel_cond,
              subset=(station == 230)),
       aes(x = year, y = mean_cond)) +
  geom_point() +
  labs(title = 'conductivity at station 230',
       x= NULL,
       y = 'average summer\nconductivity\n(uS/cm)') +
  final_theme

ggplot(subset(ann_pel_cond,
              subset=(station == 200)),
       aes(x = year, y = med_cond)) +
  geom_point() +
  labs(title = 'conductivity at station 200',
       x= NULL,
       y = 'median summer\nconductivity\n(uS/cm)') +
  final_theme

ggplot(subset(ann_pel_cond,
              subset=(station == 210)),
       aes(x = year, y = med_cond)) +
  geom_point() +
  labs(title = 'conductivity at station 210',
       x= NULL,
       y = 'median summer\nconductivity\n(uS/cm)') +
  final_theme


ggplot(subset(ann_pel_cond,
              subset=(station == 220)),
       aes(x = year, y = med_cond)) +
  geom_point() +
  labs(title = 'conductivity at station 220',
       x= NULL,
       y = 'median summer\nconductivity\n(uS/cm)') +
  final_theme

ggplot(subset(ann_pel_cond,
              subset=(station == 230)),
       aes(x = year, y = med_cond)) +
  geom_point() +
  labs(title = 'conductivity at station 230',
       x= NULL,
       y = 'median summer\nconductivity\n(uS/cm)') +
  final_theme


#### merge lit and pel ####
inlake_cond <- full_join(pel_cond, lit_cond) 

inlake_cond %>% 
  group_by(station, year) %>% 
  summarise(mean_ann_cond = mean(cond)) %>% 
  ggplot(., aes(x=year, y = mean_ann_cond, color = station)) +
  geom_point()


inlake_cond %>% 
  group_by(year) %>% 
  summarise(mean_ann_cond = mean(cond)) %>% 
  ggplot(., aes(x=year, y = mean_ann_cond, color=mean_ann_cond)) +
  geom_point(size = 4) +
  labs(title = 'mean summer conductivity at all in-lake stations',
       x= NULL,
       y = 'mean summer conductivity (uS/cm)') +
  final_theme +
  theme(legend.position = 'none')
