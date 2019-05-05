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

### Make sampling weights proper weight
NYC$`Household Sampling Weight (5 implied decimal places)` <-
  NYC$`Household Sampling Weight (5 implied decimal places)`/10000

### Subset to proper years
selfid <- NYC %>% filter(`Year Identifier` %in% c(2002,2005,2008,2011,2014,2017))


NYC %>% filter(`Moved to the U.S. as immigrant` == 1) %>% 
  group_by(`Borough and Sub-Borough Area`) %>% 
  summarise(total = n(), sumz = sum(index),average = sumz/total) %>% 
  select(`Borough and Sub-Borough Area`, average) -> immigration_hqi

stuff <- read_csv("Immigration_HQI.csv")


nyc <- nyc[,c(2,3,35)]
stuff$`Borough and Sub-Borough Area` -> stuff$code
nyc <- right_join(nyc, stuff)
name <- nyc$name

# Bronx
nyc %>% filter(`Borough and Sub-Borough Area` < 200) %>% tm_shape() +
  tm_fill("average",title="Quality Index", 
          breaks = c(.48,.50,.52,.54,.56,.58,.60,.62,.64), palette="BuGn")  +
  tm_borders(col = "black") +
  tm_text("code") +
  tm_add_legend(type = "text", labels = name[c(1:10)], text = c(101:110), col = "black") +
  tm_layout(legend.outside = TRUE, legend.text.size = 1, legend.title.size = 1.2) 

# Brooklyn
nyc %>% filter(`Borough and Sub-Borough Area` < 300 & `Borough and Sub-Borough Area` > 200) %>% tm_shape() +
  tm_fill("average",title="Quality Index", 
          breaks = c(.46,.48,.50,.52,.54,.56,.58,.60), palette="BuGn")  +
  tm_borders(col = "black") +
  tm_text("code") +
  tm_add_legend(type = "text", labels = name[c(11:28)], text = c(201:218), col = "black") +
  tm_layout(legend.outside = TRUE, legend.text.size = 1,legend.title.size = 1.2) 

# Manhattan
nyc %>% filter(`Borough and Sub-Borough Area` < 400 & `Borough and Sub-Borough Area` > 300) %>% tm_shape() +
  tm_fill("average",title="Quality Index", 
          breaks = c(.48,.50,.52,.54,.56,.58,.60,.62), palette="BuGn")  +
  tm_borders(col = "black") +
  tm_text("code") +
  tm_add_legend(type = "text", labels = name[c(29:38)], text = c(301:310), col = "black") +
  tm_layout(legend.outside = TRUE, legend.text.size = 1,legend.title.size = 1.2)

# Queens
nyc %>% filter(`Borough and Sub-Borough Area` < 500 & `Borough and Sub-Borough Area` > 400) %>% tm_shape() +
  tm_fill("average",title="Quality Index", 
          breaks = c(.46,.47,.48,.49,.50,.51), palette="BuGn")  +
  tm_borders(col = "black") +
  tm_text("code") +
  tm_add_legend(type = "text", labels = name[c(39:52)], text = c(401:414), col = "black") +
  tm_layout(legend.outside = TRUE, legend.text.size = 1,legend.title.size = 1.2)

# Staten Island
nyc %>% filter(`Borough and Sub-Borough Area` > 500) %>% tm_shape() +
  tm_fill("average",title="Quality Index", 
          breaks = c(.44,.45,.46,.47,.48,.49,.50,.51), palette="BuGn")  +
  tm_borders(col = "black") +
  tm_text("code") +
  tm_add_legend(type = "text", labels = name[c(53:55)], text = c(501:503), col = "black") +
  tm_layout(legend.outside = TRUE, legend.text.size = 1,legend.title.size = 1.2)

