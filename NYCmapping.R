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
  tm_borders(col = "black") +
  tm_text("code") +
  tm_add_legend(type = "text", labels = name[c(1:10)], text = c(101:110), col = "black") +
  tm_layout(title = "Bronx",title.size = 1.7, legend.outside = TRUE, legend.text.size = 1, legend.title.size = 1.2) 


# Brooklyn
nyc %>% filter(`Borough and Sub-Borough Area` < 300 & `Borough and Sub-Borough Area` > 200) %>% tm_shape() +
  tm_fill("Average",title="Quality Index", palette="BuGn")  +
  tm_borders(col = "black") +
  tm_text("code") +
  tm_add_legend(type = "text", labels = name[c(11:28)], text = c(201:218), col = "black") +
  tm_layout(title = "Brooklyn",title.size = 1.7, legend.outside = TRUE, legend.text.size = 1,legend.title.size = 1.2) 

# Manhattan
nyc %>% filter(`Borough and Sub-Borough Area` < 400 & `Borough and Sub-Borough Area` > 300) %>% tm_shape() +
  tm_fill("Average",title="Quality Index", palette="BuGn")  +
  tm_borders(col = "black") +
  tm_text("code") +
  tm_add_legend(type = "text", labels = name[c(29:38)], text = c(301:310), col = "black") +
  tm_layout(title = "Manhattan",title.size = 1.7, legend.outside = TRUE, legend.text.size = 1,legend.title.size = 1.2)

# Queens
nyc %>% filter(`Borough and Sub-Borough Area` < 500 & `Borough and Sub-Borough Area` > 400) %>% tm_shape() +
  tm_fill("Average",title="Quality Index", palette="BuGn")  +
  tm_borders(col = "black") +
  tm_text("code") +
  tm_add_legend(type = "text", labels = name[c(39:52)], text = c(401:414), col = "black") +
  tm_layout(title = "Queens",title.size = 1.7, legend.outside = TRUE, legend.text.size = 1,legend.title.size = 1.2)

# Staten Island
nyc %>% filter(`Borough and Sub-Borough Area` > 500) %>% tm_shape() +
  tm_fill("Average",title="Quality Index", palette="BuGn")  +
  tm_borders(col = "black") +
  tm_text("code") +
  tm_add_legend(type = "text", labels = name[c(53:55)], text = c(501:503), col = "black") +
  tm_layout(title = "Staten Island",title.size = 1.7, legend.outside = TRUE, legend.text.size = 1,legend.title.size = 1.2)

# NYC
nyc %>% tm_shape() +
  tm_fill("Average",title="Quality Index", palette="BuGn")  +
  tm_borders(col = "black") +
  tm_layout( legend.text.size = 1.5,legend.title.size = 2.5, frame = F)


