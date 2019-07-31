
install.packages("ggthemes")
install.packages("stringi")
install.packages('gifski')
install.packages('transformr')

require(stringi)
require(transformr)
require(gifski)
library(tidyverse)
library(sf)
library(rvest)
library(stringr)
library(scales)
library(viridis)
library(maps)
library(ggthemes)
require(magick)

devtools::install_github("thomasp85/gganimate")
require(gganimate)


uis<- read.csv("C:/Users/EMCB-PC/Desktop/rdir/Projects/HDX/UNESCO_UIS/oosc_global.csv")

uis_data <- uis %>% select(-contains('notes')) %>% 
  rename(country = Education...Out.of.school.rate.for.children.of.primary.school.age..both.sexes....)

uis_data$X1970 <- as.numeric(as.character(uis_data$X1970))

uis_data$X2018 <- as.numeric(as.character(uis_data$X2018))

uis_data_em <- uis_data %>% gather(X1970:X2018, key = 'year', value = 'rate')

#don't knnow how to avoid the leading X on numerical variable names here is how to remove them. 

uis_data_em$year <- sub("X","", uis_data_em$year)

uis_data_em$year <- as.numeric(uis_data_em$year)

uis_data_em$year <- as.Date(uis_data_em$year)

#map data

map_world <- map_data('world')

uis_map_data <- left_join(map_world, uis_data_em, by = c('region' = 'country'))

drop.cols <- c('order', 'subregion')

uis_map_data <- uis_map_data %>% select(-drop.cols)

uis_map_data_clip <- uis_map_data %>% filter(year > 2009)

uis_map_data_clip$year <- as.integer(uis_map_data_clip$year)

uismap <- ggplot(uis_map_data_clip, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = rate)) +
  scale_colour_manual(na.value = 'white') +
  scale_x_continuous(limits = c(-40, 65)) +
  scale_y_continuous(limits = c(-14, 35)) +
  transition_time(year) +
  ease_aes('linear')

animate(uismap)

?gg_animate

?transition_time

