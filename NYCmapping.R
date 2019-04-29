library(tidyverse)
library(tmap)
library(sf)
library(RColorBrewer)

nycmap <- read.csv("imputed_NYC2.csv")
nyc <- st_read("nyc/nyc.shp")
plot(nyc)
nyc <- nyc[,c(3,35)]
nyc$code -> nyc$`Borough and Sub-Borough Area`
nyc <- right_join(nyc, sub)
tm_shape(nyc) +
  tm_polygons("ex")

display.brewer.pal(7,"Blues")

