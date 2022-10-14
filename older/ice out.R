sun_ice <- read_xlsx('C:/Users/steeleb/Dropbox/gloeo_sediment_cores/ice data/raw data/newenglandlakes_iceout.xlsx', 
                     sheet = 'sunapee') %>% 
  mutate(date = as.Date(paste(year, month, day, sep = '-')),
         jday = as.numeric(format(date, format = '%j')))

ggplot(sun_ice, aes(x=year, y = jday, fill = jday)) +
  geom_smooth(method = 'lm', se=F, color = 'black', linetype = 5, size= .5) +
  geom_point(size = 4, shape = 21, color='black') +
  final_theme +
  scale_y_reverse(limits = c(140, 70), breaks = c(74, 91, 105, 121, 135), labels = c('March 15', 'April 1', 'April 15', 'May 1', 'May 15')) +
  scale_x_continuous(limits = c(1860, 2020), breaks = c(1860, 1900, 1940, 1980, 2020)) +
  scale_fill_gradient(low='#9dfbff', high='#0032ba') +
  labs(title = 'ice out date at Lake Sunapee',
       x=NULL,
       y='date of ice out') +
  theme(legend.position = 'none',
        panel.grid.major.x = element_line(color = 'white'),
        panel.grid.minor.x = element_line(color = 'white'),
        panel.grid.minor.y = element_line(color = 'white'))


