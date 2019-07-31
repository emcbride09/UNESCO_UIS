install.packages("magick")
install.packages('extrafont')
install.packages("grid")
install.packages("gridExtra")
library(extrafont)
library(gganimate)
library(ggmap)
library(maps)
library(magick)
require(stringi)
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
require(ggrepel)
require(cowplot)
require(haven)
library(grid)
library(gridExtra)
devtools::install_github("thomasp85/transformr")

require(transformr)

devtools::install_github('thomasp85/tweenr')

require(tweenr)

devtools::install_github("thomasp85/gganimate")
require(gganimate)

devtools::install_github("dkahle/ggmap")
require(ggmap)

install.packages("reprex")
require(reprex)

install.packages("installr")
install.ImageMagick()

#install fonts
font_import()
fonts <- fonttable()
View(fonts)
loadfonts(device="win")


#read datasheet
uis<- read.csv("C:/Users/EMCB-PC/Desktop/rdir/Projects/HDX/UNESCO_UIS/recoded_uis.csv", na.strings = "")

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


url <- "http://www.worldometers.info/geography/how-many-countries-in-africa/"

africa <- read_html(url) %>% 
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()


africa_names <-  africa %>% select(- `#`)

africa_df <- africa_names %>% rename(country = Country,
                                     pop = `Population(2019)`,
                                     subregion = Subregion)


#changedata frame
##df.oil <- df.oil %>%  mutate(country = recode(country, `United States` = 'USA'


africa_df <- africa_df %>% mutate(country = recode(country, `DR Congo` = 'Democratic Republic of the Congo',
                                                    `Côte d'Ivoire` = "Ivory Coast",
                                                    `Congo` = 'Republic of Congo'))






##you can filter by multiple values but dont change to list or DF keep as character vector

africa_char <- as.character(africa_df$country)

uis_final <- uis_map_data_clip %>% filter(region %in% africa_char)


?transition_manual

#Mapping with alt method from ggmap
#I DID IT
# Define lon/lat boundary box of Africa
bbox <- c(-20.873904, -38.053422, 52.367006, 38.779947)

#Get map
map <- get_stamenmap(c(bbox[1], bbox[2], bbox[3], bbox[4]), zoom = 4)

# Build plot
g <- ggmap(map)

uis_no_na <- na.omit(uis_final)

anim <- g + 
  labs(title = "National Rate of Out of School Children", subtitle = "Year: {current_frame}", x = "",
       y = "") +
  geom_polygon(data = uis_no_na, aes(x = long, y = lat, group = group, fill = rate), 
               alpha = 0.8, color = "black") + 
  theme(plot.title = element_text(family = 'Arial', size = 20),
        plot.subtitle = element_text(family = 'Arial', size = 16),
        plot.background = element_rect(colour = 'black')) +
  scale_fill_gradientn(colours = c("palegreen", "orangered2")) +
# Here come the gganimate specific bits <- changes result in erros usually mean the type of animation needs to change
  transition_manual(year, cumulative = TRUE) +
  theme(plot.background = element_rect(fill = 'white', colour = NA),
        panel.background = element_rect(fill = 'white', color = NA),
        legend.background = element_blank(),
        legend.position = 'topright',
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
        )


anim

anim_save("uis_anim.gif",anim)

#####line chart

url <- "http://www.worldometers.info/geography/how-many-countries-in-africa/"

africa <- read_html(url) %>% 
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()

africa_names <-  africa %>% select(- `#`)

africa_df <- africa_names %>% rename(country = Country,
                                     pop = `Population(2019)`,
                                     subregion = Subregion)

#f.oil <- df.oil %>% mutate(oil_bbl_per_day = oil_bbl_per_day %>% str_replace_all(',','') %>% as.integer())

africa_df_w <- africa_df %>% mutate(pop = pop %>% str_replace_all(',',"") %>% as.numeric())

uis_final_line <- left_join(uis_final, africa_df_w, by = c('region' = 'country'))

write.csv(uis_final_line, "uis_africa.csv")




uis_final_line$rate <- as.numeric(uis_final_line$rate)

uis_final_line$year <- as.numeric(uis_final_line$year)

df1 <- uis_final_line %>% select(year, rate, region, pop, subregion) %>% 
  as.data.frame()



subs_anim <- df1 %>% filter(!is.na(rate)) %>%
  filter(!is.na(subregion)) %>% 
  group_by(subregion, year) %>% 
  summarise(regional_ave = sum(rate)/length(rate))

###logo parse
hdxlogo <- image_read('C:/Users/EMCB-PC/Desktop/rdir/Projects/HDX/UNESCO_UIS/logo-hdx-gray.png')

logo <- hdxlogo %>%
  image_scale("100") %>% 
  image_background("white", flatten = TRUE)
  
image_write(logo, path = "hdxlogo.png", format = "png")

logo
##gglineplotanimation

reg <- ggplot(subs_anim, aes(year, regional_ave, group = subregion, color = subregion)) +
                labs(title = "Rates of Out-of-School Children \nin the African Continent",
                     subtitle = "% aggregated to the regional level - {round(frame_along)}", x = "Year", 
                     y = "Regional Average (%)") +
  geom_line(lineend = 'butt') +
  geom_segment(aes(xend = 2020, yend = regional_ave)) +
  geom_point() + 
  geom_label(aes(x = 2015, label = subregion, family = 'Source Sans Pro', size = 12)) +
  transition_reveal(year) + 
  #ease_aes('linear') +
  enter_fade() +
  exit_fade() + 
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0, 100)) +
  theme(plot.background = element_rect(fill = 'white', color = NA),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.background = element_blank(),
        legend.position = 'topright',
        axis.text.x = element_text(colour =  "black", size = 13, family = ),
        axis.text.y = element_text(colour =  "black", size = 13, family = 'Source Sans Pro'),
        axis.title.x = element_text(colour =  "black", size = 16, family = 'Source Sans Pro'),
        axis.title.y = element_text(colour =  "black", size = 16, family = 'Source Sans Pro'),
        plot.title = element_text(colour = "black", size = 24, family = 'Gotham'),
        plot.subtitle = element_text(colour =  "black", size = 16, hjust = 0.5),
        legend.text = element_text(colour =  "black", size = 16)) +
          annotate("text", x = 1989, y = 100, label = "Powered by", family = "Source Sans Pro", size = 6, 
                   color = "grey40", alpha = 0.8) + 
  coord_cartesian(clip = "off") + 
  annotation_raster(hdxlogo, xmin = 1996, xmax = 2006, ymin = 96, ymax = 103)
        

reg



anim_save("uis_reg_anim.gif",reg, nframe = 200)

#reg_ready <- animate(reg, width = 440, height = 470)

#anim_ready <- animate(anim, width = 440, height = 470, fps = 2)

#reg_gif <- image_read(reg_ready)
# <- image_read(anim_ready)

#combined_gif <- image_append(c(reg_gif[1], anim_gif[1]))
#for (i in 2:100) {
#  combined <- image_append(c(reg_gif[i], anim_gif[i]))
#  combined_gif <- c(combined_gif, combined)
  
#}

#combined_gif 

anim <- image_read("C:/Users/EMCB-PC/Desktop/rdir/Projects/HDX/UNESCO_UIS/uis_reg_anim.gif")

anim

logo <- image_read("C:/Users/EMCB-PC/Desktop/rdir/Projects/HDX/UNESCO_UIS/logo-hdx-gray.png")

double <- c(anim, logo)

final <- image_append(double)

final
