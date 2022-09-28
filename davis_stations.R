# script to make map of Davis Weather stations graphs for SOL ----

library(tidyverse)
library(sf)
library(tmap)

# point to directories


# read in weather data from EDI ----
GM <- read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.736.2&entityid=9bcaed584cafe49d54fb7d0660cedad7')
HC <- read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.736.2&entityid=bc78e20295d5d876419703bf86fb8d01')
SF <- read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.736.2&entityid=910fe28ff7ee4c16ced5849fd6322f06')

locs <- read.csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.736.2&entityid=f0da7783c40aad1e01409c1f29d0a4c6')


# filter to 2021
GM_2021 <- GM %>% 
  mutate(instrument_datetime = as.POSIXct(instrument_datetime, tz = 'UTC')) %>% 
  filter(instrument_datetime >= as.Date('2021-01-01'))

HC_2021 <- HC %>% 
  mutate(instrument_datetime = as.POSIXct(instrument_datetime, tz = 'UTC')) %>% 
  filter(instrument_datetime >= as.Date('2021-01-01'))

SF_2021 <- SF %>% 
  mutate(instrument_datetime = as.POSIXct(instrument_datetime, tz = 'UTC')) %>% 
  filter(instrument_datetime >= as.Date('2021-01-01'))

# make map of locations ----
