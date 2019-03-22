library(readr)
library(tidyverse)
set.seed(6969)

years <- c(1991,1993,1996,1999,2002,2005,2008,2011,2014,2017)
dta <- list()
for (i in 1:10) {
  name <- str_c("DATA/NYCHVS ", years[i]," Occupied File for ASA Challenge.csv")
  dta[[i]] <- read_csv(name, skip = 1)
}

NYC <- dta[[1]] 
for (i in 2:10) {
  dta[[i]] %>% 
    bind_rows(NYC) -> 
    NYC
}

NYC <- NYC %>% mutate(Borough = ifelse(Borough == 1, "Bronx", Borough))
NYC <- NYC %>% mutate(Borough = ifelse(Borough == 2, "Brooklyn", Borough))
NYC <- NYC %>% mutate(Borough = ifelse(Borough == 3, "Manhattan", Borough))
NYC <- NYC %>% mutate(Borough = ifelse(Borough == 4, "Queens", Borough))
NYC <- NYC %>% mutate(Borough = ifelse(Borough == 5, "Staten Island", Borough))

## Need to combine the columns from 91-99 and 02-17
NYC %>% mutate(waterleakage = ifelse(`Year Identifier` > 2000, 
                                     `Water leakage inside apartment`,
                                     `Water leakage inside apartment (house)`)) %>%
  select(waterleakage, `Year Identifier`) %>% View()

newdatarent <- NYC %>% select(`Monthly contract rent`,`Year Identifier`) %>% 
  filter(!(`Monthly contract rent` %in% c(99998, 99999))) %>% 
  na.exclude()

newdataooprent <- NYC %>% select(`Out of pocket rent`, `Year Identifier`) %>%
  filter(!(`Out of pocket rent` %in% c(99998, 99999))) %>%
  na.exclude()

newdatavalue <- NYC %>% select(Value,`Year Identifier`) %>%
  filter(!(Value %in% c(9999998, 9999999)), Value < 3990000) %>% na.exclude()

newdatarent %>% group_by(`Year Identifier`) %>% count()
newdataooprent %>% group_by(`Year Identifier`) %>% count()
newdatavalue %>% group_by(`Year Identifier`) %>% count()


### Divide data into own and rent
own <- NYC %>% filter(`Tenure 1` == 1)
rent <- NYC %>% filter(`Tenure 1` == 9)

