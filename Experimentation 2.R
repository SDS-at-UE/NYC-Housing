library(readr)
library(tidyverse)
library(plm)
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
               `Year Identifier`)

NYC <- NYC %>% mutate(waterleakage = ifelse(`Year Identifier` > 2000, 
                                     `Water leakage inside apartment`,
                                     `Water leakage inside apartment (house)`))

#### All 10 years of data for water leakage
NYC %>% select(waterleakage, `Year Identifier`) %>% 
  filter(waterleakage %in% c(1,2)) %>% count()

#### All 10 years of data for heating equip breakdowns
NYC %>% select(`Heating equipment breakdown`, `Year Identifier`) %>% 
  filter(`Heating equipment breakdown` %in% c(0,1)) %>% table()

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


#### Adding CPI to the data
cpidata <- read.csv("CPI by Year.csv")
NYC <- NYC %>% 
  left_join(cpidata, by = c("Year Identifier" = "Year.Identifier"))

NYC$Value2017 <- NYC$Value/NYC$CPI
NYC$`Monthly contract rent` <- NYC$`Monthly contract rent`/NYC$CPI

NYC$waterleakage <- ifelse(NYC$waterleakage == 2, 0, NYC$waterleakage)
NYC$`Presence of mice or rats` <- 
  ifelse(NYC$`Presence of mice or rats` == 2, 0, NYC$`Presence of mice or rats`)
NYC$`Heating equipment breakdown` <-
  ifelse(NYC$`Heating equipment breakdown` == 1, 2, NYC$`Heating equipment breakdown`)
NYC$`Heating equipment breakdown` <-
  ifelse(NYC$`Heating equipment breakdown` == 0, 1, NYC$`Heating equipment breakdown`)
NYC$`Heating equipment breakdown` <-
  ifelse(NYC$`Heating equipment breakdown` == 2, 0, NYC$`Heating equipment breakdown`)

### Divide data into own and rent
own <- NYC %>% filter(`Tenure 1` == 1)
own %>% select(waterleakage) %>% filter(waterleakage %in% c(0,1)) %>% count()
own %>% select(`Heating equipment breakdown`) %>% 
  filter(`Heating equipment breakdown` %in% c(0,1)) %>% count()
own %>% select(`Presence of mice or rats`) %>% 
  filter(`Presence of mice or rats` %in% c(0,1)) %>% count()
own <- own %>% filter(waterleakage %in% c(0,1), 
                        `Heating equipment breakdown` %in% c(0,1),
                        `Presence of mice or rats` %in% c(0,1)) 


rent <- NYC %>% filter(`Tenure 1` == 9)
rent %>% select(waterleakage) %>% filter(waterleakage %in% c(0,1)) %>% count()
rent %>% select(`Heating equipment breakdown`) %>% 
  filter(`Heating equipment breakdown` %in% c(0,1)) %>% count()
rent %>% select(`Presence of mice or rats`) %>% 
  filter(`Presence of mice or rats` %in% c(0,1)) %>% count()
rent <- rent %>% filter(waterleakage %in% c(0,1), 
                        `Heating equipment breakdown` %in% c(0,1),
                        `Presence of mice or rats` %in% c(0,1)) 


#### Calculate problems index column
own <- own %>% mutate(problems = waterleakage + `Heating equipment breakdown` +
                        `Presence of mice or rats`)
rent <- rent %>% mutate(problems = waterleakage + `Heating equipment breakdown` +
                        `Presence of mice or rats`)

#### Regression on owners
own$`Borough and Sub-Borough Area` <- as.factor(own$`Borough and Sub-Borough Area`)
own$waterleakage <- as.factor(own$waterleakage)
own$`Heating equipment breakdown` <- as.factor(own$`Heating equipment breakdown`)
own$`Presence of mice or rats` <- as.factor(own$`Presence of mice or rats`)
own$year <- own$`Year Identifier`

mod <- lm(Value2017 ~ `Borough and Sub-Borough Area` + `Year Identifier` + 
             `Number of rooms` + `Number of bedrooms` + waterleakage + 
             `Heating equipment breakdown` + `Presence of mice or rats`, 
          data = own, weights = `Replicate Weight 27 (5 implied decimal places)`)
summary(mod)


#### Regression on renters
rent$`Borough and Sub-Borough Area` <- as.factor(rent$`Borough and Sub-Borough Area`)
rent$waterleakage <- as.factor(rent$waterleakage)
rent$`Heating equipment breakdown` <- as.factor(rent$`Heating equipment breakdown`)
rent$`Presence of mice or rats` <- as.factor(rent$`Presence of mice or rats`)
rent$year <- rent$`Year Identifier`


mod2 <- lm(`Monthly contract rent` ~  `Borough and Sub-Borough Area` + 
             `Number of rooms` + `Year Identifier` + waterleakage + 
             `Heating equipment breakdown` + `Presence of mice or rats`, 
           data = rent, weights = `Replicate Weight 27 (5 implied decimal places)`)
summary(mod2)
