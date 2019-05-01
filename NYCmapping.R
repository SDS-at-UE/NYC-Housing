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

# Bronx
nyc %>% filter(`Borough and Sub-Borough Area` < 200) %>% tm_shape() +
  tm_fill("Average",title="Quality Index", palette="BuGn")  +
  tm_borders() +
  tm_text("code") +
  tm_layout(title = "Bronx", title.position = c("right","bottom")) +
  tm_add_legend(type = "symbol", labels = c("a","b","c","d","e","f","g","h","i","j"))

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

