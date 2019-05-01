library(readr)
library(tidyverse)
library(sf)
library(tmap)
select <- dplyr::select
set.seed(6969)

NYC <- read_csv("immigration.csv", col_types = cols(`Householder's Race` = col_character()))
data <- read_csv("Index_Data.csv")
NYC$index <- data$final_index
nyc_map <- st_read("nyc/nyc.shp")

### Make sampling weights proper weight
NYC$`Household Sampling Weight (5 implied decimal places)` <-
  NYC$`Household Sampling Weight (5 implied decimal places)`/10000

### Subset to proper years
selfid <- NYC %>% filter(`Year Identifier` %in% c(2002,2005,2008,2011,2014,2017))


NYC %>% filter(`Moved to the U.S. as immigrant` == 1) %>% 
  group_by(`Borough and Sub-Borough Area`) %>% 
  summarise(total = n(), sumz = sum(index),average = sumz/total) %>% 
  select(`Borough and Sub-Borough Area`, average) -> immigration_hqi

write_csv(immigration_hqi, "Immigration_HQI.csv")

