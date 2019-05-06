library(readr)
library(tidyverse)
library(sf)
library(tmap)
library(RColorBrewer)
select <- dplyr::select
set.seed(6969)

NYC <- read_csv("immigration.csv", col_types = cols(`Householder's Race` = col_character()))
data <- read_csv("Index_Data.csv")
NYC$index <- data$final_index
nyc <- st_read("nyc/nyc.shp")
plot(nyc)

NYC$`Household Sampling Weight (5 implied decimal places)` <-
  NYC$`Household Sampling Weight (5 implied decimal places)`/10000

NYC %>% group_by(`Borough and Sub-Borough Area`) %>% 
  summarise(Population = sum(`Household Sampling Weight (5 implied decimal places)`)) -> stuff

stuff$`Borough and Sub-Borough Area` -> stuff$code

nyc <- nyc[,c(2,3,35)]
nyc <- right_join(nyc, stuff)

nyc %>% tm_shape() +
  tm_fill("Population",title="Population", palette="BuGn")  +
  tm_borders(col = "black") +
  tm_text("code") +
  tm_add_legend(type = "text", labels = name[c(1:10)], text = c(101:110), col = "black") +
  tm_layout(legend.outside = FALSE, legend.text.size = 1, legend.title.size = 1.2) 
