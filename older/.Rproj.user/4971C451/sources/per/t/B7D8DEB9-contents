library(tidyverse)
library(readxl)
library(ggthemes)
library(rLakeAnalyzer)
library(openair)

##ggplot grom of final theme
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5))

final_theme_secchi=theme_economist() +
  theme(axis.text=element_text(size=12),
        axis.text.y = element_text(hjust = 1.0),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.9),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5),
        plot.background = element_rect(fill='white'),
        strip.background = element_blank(),
        axis.line.x = element_line(color = 'white'))

final_theme_temp=theme_economist() +
  theme(axis.text=element_text(size=12),
        axis.text.y = element_text(hjust = 1.0),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5),
        plot.background = element_rect(fill='white'),
        axis.line.x = element_line(color = 'white'),
        panel.grid.major.y = element_line(color = 'white', size = 0.2),
        strip.background = element_blank(),
        legend.position = 'bottom')
