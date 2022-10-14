# gloeo visualization for LSPA State of the Lake #
# R Version 3.5.2
# R Studio version 1.1.463

# last modified 08August 2019
# by B. Steele

library(tidyverse)
library(ggthemes)
library(readxl)

gloeo <-  read_csv('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/gloeo/allGloeosurf0516.csv')

gloeo_summ <- gloeo %>% 
  group_by(year)

ggplot(gloeo, aes(x=year, y = coloniesperL, group = year)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 10))


