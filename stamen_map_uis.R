install.packages("magick")
library(gganimate)
library(ggmap)
library(maps)
library(magick)
require(stringi)
require(transformr)
require(gifski)
library(tidyverse)
library(sf)
library(rvest)
library(stringr)
library(scales)
library(viridis)
library(ggthemes)
require(httr)
require(RColorBrewer)

devtools::install_github("thomasp85/gganimate")
require(gganimate)

devtools::install_github("dkahle/ggmap")
require(ggmap)

#read datasheet
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

uis_map_data_clip <- uis_map_data %>% select(-drop.cols)

uis_map_data_clip$year <- as.integer(uis_map_data_clip$year)

#choose final countries

#scrape Africa countries

#rm(africa_df)
#rm(africa)
?as.data.frame

url <- "http://www.worldometers.info/geography/how-many-countries-in-africa/"

africa <- read_html(url) %>% 
  html_nodes("tr :nth-child(2)") %>% 
  html_text()


africa_names <- as.data.frame(africa, col.names = names('country'))

africa_df <- africa_names %>% rename(country = africa) %>% 
  filter(africa != "Country")


#changedata frame
##df.oil <- df.oil %>%  mutate(country = recode(country, `United States` = 'USA'


africa_df <- africa_df %>% mutate( country = recode(country, `DR Congo` = 'Democratic Republic of the Congo',
                                                    `Côte d'Ivoire` = "Ivory Coast",
                                                    `Congo` = 'Republic of Congo')) %>% 
  select(country)






##you can filter by multiple values but dont change to list or DF keep as character vector

africa_df <- as.character(africa_df$country)

uis_final <- uis_map_data_clip %>% filter(region %in% africa_df)

uis_final$region <- as.character(uis_final$region)



#Mapping with alt method from ggmap
#I DID IT
# Define lon/lat boundary box of Africa
bbox <- c(-20.873904, -38.053422, 52.367006, 38.779947)

#Get map
map <- get_stamenmap(c(bbox[1], bbox[2], bbox[3], bbox[4]), zoom = 4)

# Build plot
g <- ggmap(map)

anim <- g + geom_polygon(data = uis_final, aes(x = long, y = lat, group = group, fill = rate), 
               alpha = 0.8, color = "black") + 
  theme(plot.title = element_text(family = 'Arial', size = 20),
        plot.subtitle = element_text(family = 'Arial', size = 16),
        plot.background = element_rect(colour = 'black')) +
  labs(title = "National Rate of Out of School Children", subtitle = "Year: {current_frame}") +
  scale_fill_gradientn(colours = c("palegreen", "orangered2")) +
# Here come the gganimate specific bits <- changes result in erros usually mean the type of animation needs to change
  transition_manual(year) + 
  ease_aes('linear')

anim

anim_save("uis_anim.gif",anim)



  