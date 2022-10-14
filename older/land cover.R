#land cover analysis#

land_cover <- read_xlsx('F:/GIS_data_general/US_data/project_data/Sunapee/land_cover/sun_nlcd_01-16_summary.xlsx') %>% 
  mutate(`YYYY NLCD` = as.factor(`YYYY NLCD`)) %>% 
  mutate(`Alternate Name` = factor(`Alternate Name`, levels = c('forest', 'developed + transportation', 'wetland', 'water', 'open', 'agriculture'), ordered=T))

ggplot(land_cover, (aes(x=`Alternate Name`, y = `Percent WS only`))) +
  geom_bar(stat = 'identity', position = 'dodge', aes(fill = `YYYY NLCD`)) +
  labs(x=NULL,
       y='land cover as\npercent of watershed',
       fill = NULL) +
  scale_x_discrete(labels = c('forest', 'developed +\ntransportation', 'wetland', 'water', 'open', 'agriculture')) +
  scale_fill_colorblind() +
  final_theme +
  theme(legend.position = 'bottom')
