library(tidyverse)
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

NYC <- NYC %>% mutate(waterleakage = ifelse(`Year Identifier` > 2000, 
                                            `Water leakage inside apartment`,
                                            `Water leakage inside apartment (house)`))

NYC <- NYC %>% filter(`Toilet breakdowns` %in% c(1,2), 
                            `Complete plumbing facilities` %in% c(1,2),
                            `Kitchen facilities functioning` %in% c(1,2),
                            `Condition of building` %in% c(1,2,3)) 

NYC$`Toilet breakdowns` <- ifelse(NYC$`Toilet breakdowns` == 2, 0, NYC$`Toilet breakdowns`)
NYC$`Complete plumbing facilities` <- 
  ifelse(NYC$`Complete plumbing facilities` == 2, 0, NYC$`Complete plumbing facilities`)
NYC$`Kitchen facilities functioning` <- 
  ifelse(NYC$`Kitchen facilities functioning` == 2, 0, NYC$`Kitchen facilities functioning`)


CPI <- read_csv("CPI by Year.csv")
NYC %>% left_join(CPI) -> NYC
NYC$monthly_rent <- NYC$`Monthly contract rent`/NYC$CPI
NYC$value_own <- NYC$Value/NYC$CPI
NYC %>% mutate(value_own = Value/CPI)
NYC %>% filter(`Tenure 1` == 1) -> Owned
NYC %>% filter(`Tenure 1` == 9) -> Rented



Rented$`Borough and Sub-Borough Area` <- as.factor(Rented$`Borough and Sub-Borough Area`)
Rented$`Toilet breakdowns` <- as.factor(Rented$`Toilet breakdowns`)
Rented$`Complete plumbing facilities` <- as.factor(Rented$`Complete plumbing facilities`)
Rented$`Kitchen facilities functioning` <- as.factor(Rented$`Kitchen facilities functioning`)
Rented$`Condition of building` <- as.factor(Rented$`Condition of building`)
Rented$`Year Identifier` <- as.factor(Rented$`Year Identifier`)

mod <- lm(monthly_rent ~ `Borough and Sub-Borough Area` + `Number of bedrooms` + 
            `Number of rooms` +
            `Toilet breakdowns` + `Complete plumbing facilities` + 
            `Kitchen facilities functioning` + `Condition of building` + 
            `Year Identifier`, data = Rented, 
          weights = `Household Sampling Weight (5 implied decimal places)`)
summary(mod)

Owned$`Borough and Sub-Borough Area` <- as.factor(Owned$`Borough and Sub-Borough Area`)
Owned$waterleakage <- as.factor(Owned$waterleakage)
Owned$`Heating equipment breakdown` <- as.factor(Owned$`Heating equipment breakdown`)
Owned$`Presence of mice or rats` <- as.factor(Owned$`Presence of mice or rats`)

Owned$`Borough and Sub-Borough Area` <- as.factor(Owned$`Borough and Sub-Borough Area`)
Owned$`Toilet breakdowns` <- as.factor(Owned$`Toilet breakdowns`)
Owned$`Complete plumbing facilities` <- as.factor(Owned$`Complete plumbing facilities`)
Owned$`Kitchen facilities functioning` <- as.factor(Owned$`Kitchen facilities functioning`)
Owned$`Condition of building` <- as.factor(Owned$`Condition of building`)
Owned$`Year Identifier` <- as.factor(Owned$`Year Identifier`)

mod_owned <- lm(value_own ~ `Borough and Sub-Borough Area` + `Number of bedrooms` + `Number of rooms`+ 
            `Toilet breakdowns` + `Complete plumbing facilities` + 
            `Kitchen facilities functioning` + `Condition of building` + 
              `Year Identifier` + `Heating equipment breakdown` + waterleakage +
              `Presence of mice or rats`, data = Owned)
summary(mod_owned)
