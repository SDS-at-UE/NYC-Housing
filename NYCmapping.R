library(tidyverse)
library(tmap)
library(sf)
library(RColorBrewer)


zscore <- read.csv("z-scores.csv")
nyc <- st_read("nyc/nyc.shp")
plot(nyc)

nyc <- nyc[,c(2,3,35)]
zscore$"Borough.and.Sub.Borough.Area" -> zscore$code
nyc <- right_join(nyc, zscore)
name <- nyc$name

# Bronx
nyc %>% filter(`Borough and Sub-Borough Area` < 200) %>% tm_shape() +
  tm_fill("Average",title="Quality Index", palette="BuGn")  +
  tm_borders() +
  tm_text("code") +
  tm_add_legend(type = "text", labels = name[c(1:10)], text = c(101,102,103,104,105,106,107,108,109,110)) +
  tm_layout(title = "Bronx",legend.outside = TRUE, legend.text.size = 1) 


# Brooklyn
nyc %>% filter(`Borough and Sub-Borough Area` < 300 & `Borough and Sub-Borough Area` > 200) %>% tm_shape() +
  tm_fill("Average",title="Quality Index", palette="BuGn")  +
  tm_borders() +
  tm_text("name") +
  tm_layout(title = "Brooklyn", title.position = c("right","bottom"))

# Manhattan
nyc %>% filter(`Borough and Sub-Borough Area` < 200) %>% tm_shape() +
  tm_fill("Average",title="Quality Index", palette="BuGn")  +
  tm_borders() +
  tm_text("name") +
  tm_layout(title = "Bronx", title.position = c("right","bottom"))

# Queens
nyc %>% filter(`Borough and Sub-Borough Area` < 200) %>% tm_shape() +
  tm_fill("Average",title="Quality Index", palette="BuGn")  +
  tm_borders() +
  tm_text("name") +
  tm_layout(title = "Bronx", title.position = c("right","bottom"))

# Staten Island
nyc %>% filter(`Borough and Sub-Borough Area` < 200) %>% tm_shape() +
  tm_fill("Average",title="Quality Index", palette="BuGn")  +
  tm_borders() +
  tm_text("name") +
  tm_layout(title = "Bronx", title.position = c("right","bottom"))

display.brewer.pal(7,"Blues")

