library(readr)
library(tidyverse)
set.seed(6969)

years <- c(1991,1993,1996,1999,2002,2005,2008,2011,2014,2017)
dta <- list()
for (i in 1:10) {
  name <- str_c("DATA/NYCHVS ", years[i]," Occupied File for ASA Challenge.csv")
  dta[[i]] <- read_csv(name, skip = 1)
}

years <- c(1991,1993,1996,1999,2002,2005,2008,2011,2014,2017)
dta <- list()
for (i in 1:10) {
  name <- str_c("DATA/NYCHVS ", years[i]," Occupied File for ASA Challenge.csv")
  dta[[i]] <- read_csv(name, skip = 1)
}

NYC <- dta[[1]]
for (i in 2:3) {
  dta[[i]] %>% 
    bind_rows(NYC) -> 
    NYC
}
for (i in 4:9) {
  dta[[i]] %>% dplyr::select(-`Floor of unit`) %>%
    bind_rows(NYC) -> 
    NYC
}
dta[[10]] %>% 
  bind_rows(NYC) -> 
  NYC

NYC <- NYC %>% mutate(Borough = ifelse(Borough == 1, "Bronx", Borough))
NYC <- NYC %>% mutate(Borough = ifelse(Borough == 2, "Brooklyn", Borough))
NYC <- NYC %>% mutate(Borough = ifelse(Borough == 3, "Manhattan", Borough))
NYC <- NYC %>% mutate(Borough = ifelse(Borough == 4, "Queens", Borough))
NYC <- NYC %>% mutate(Borough = ifelse(Borough == 5, "Staten Island", Borough))

## Need to combine the columns from 91-99 and 02-17
NYC %>% select(`Water leakage inside apartment`,
               `Water leakage inside apartment (house)`,
               `Year Identifier`) %>% View()

NYC <- NYC %>% mutate(waterleakage = ifelse(`Year Identifier` > 2000, 
                                     `Water leakage inside apartment`,
                                     `Water leakage inside apartment (house)`))

#### All 10 years of data for water leakage
NYC %>% select(waterleakage, `Year Identifier`) %>% 
  filter(waterleakage %in% c(1,2)) %>% count()

#### All 10 years of data for heating equip breakdowns
NYC %>% select(`Heating equipment breakdown`, `Year Identifier`) %>% 
  filter(`Heating equipment breakdown` %in% c(0,1)) %>% count()

#### Only 2014 and 2017 for functioning air conditioning
NYC %>% select(`Functioning Air Conditioning`, `Year Identifier`) %>% 
  filter(`Functioning Air Conditioning` %in% c(1,2,3)) %>% table()

#### All 10 years of data for presence of mice or rats
NYC %>% select(`Presence of mice or rats`, `Year Identifier`) %>% 
  filter(`Presence of mice or rats` %in% c(1,2)) %>% table()

#### Only 2008-2017 for # of cockroaches
NYC %>% select(`Number of Cockroaches`, `Year Identifier`) %>% 
  filter(`Number of Cockroaches` %in% c(1,2,3,4)) %>% table()

#### Looking at best variables to judge prices on
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




