
library(tidytuesdayR)
library(skimr)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(spData)
library(ggpubr)
library(patchwork)
library(ggplot2)
library(extrafont)
library(ggtext)

loadfonts(device="win")

# Data import
tuesdata <- tidytuesdayR::tt_load('2020-08-18')
plants <- tuesdata$plants

#Explore data
skim(plants)

#Extinct plants in Africa after 1900

plantsAfrica <- plants %>% 
  dplyr::filter(continent == "Africa", year_last_seen != "Before 1900") %>% 
  group_by(country) %>% 
  dplyr::summarise(n = n()) %>% 
  arrange(desc(n)) 


#Map of Africa combined with plant data
africa <- world %>% 
  filter(continent == "Africa", !is.na(iso_a2)) %>% 
  left_join(plantsAfrica, by=c("name_long" = "country")) 

#Plot 1

p1 <- ggplot(africa) + 
  geom_sf(aes(geometry = geom, fill= n)) + 
  theme_void() + scale_fill_viridis_c() 

#Annotation
p1 <- p1 + 
  annotate("text", x = 45, y = -32, label = "Madagascar:\n91 plants\nextinct", color = "white", size=3.5, family = "Gadugi") + 
  theme(plot.title = element_text(family = "Gadugi"),plot.background = element_rect(fill = "#3f3f3f", colour = "#3f3f3f"))


#Top 3 threats in Africa - data

top3threatsAfrica <- plants %>% 
  filter(continent == "Africa", year_last_seen != "Before 1900") %>% 
  select(starts_with("threat")) %>% summarise_all(sum) %>% 
  tidyr::pivot_longer(starts_with("threat"), names_to = "threats", values_to = "count") %>% 
  arrange(desc(count)) %>% top_n(3)

#Preparation 

top3threatsAf2 <-as.matrix(top3threatsAfrica[,-1])
rownames(top3threatsAf2)<-top3threatsAfrica$threats

#Plot 2
p2 <- ggballoonplot(top3threatsAf2, fill="value")+ 
  scale_fill_viridis_c() + theme_void()  +  
  scale_size_area(max_size=15) + guides(size = FALSE) + 
  theme(legend.position = "none")  + 
  theme(plot.title = element_text(family = "Gadugi", color="white"),plot.background = element_rect(fill = "#3f3f3f", colour = "#3f3f3f"))

#Annotation

p2 <- p2 + 
  annotate("text", x = 1, y = 3.3, label ="Agriculture & Aquaculture", color="white", family = "Gadugi", size = 5) + 
  annotate("text", x = 1, y = 2.3, label ="Biological Resource Use", color = "white",  family = "Gadugi", size = 5) + 
  annotate("text", x = 1, y = 1.3, label ="Natural System Modifications", color = "white", family = "Gadugi", size = 5) 


#Combining plots
patchwork <- (p1 + p2) 
patchwork + plot_annotation(
  title = bquote('Number of plants' ~ bold('extinct') ~ 'in Africa from 1900 till 2020 and' ~ bold('three major') ~ 'threats'),
  caption = 'Data source: IUCN', 
  theme = theme(plot.title = element_text(family = "Gadugi", color="white", size = 20),plot.background = element_rect(fill = "#3f3f3f", colour = "#3f3f3f"), plot.caption = element_text(colour = "white", family = "Gadugi"))
  )

#Saving plot
ggsave("TTw34_plot.png", width = 10, height = 6)
