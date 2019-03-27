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
NYC %>% filter(`Tenure 1` == 1) -> Owned
NYC %>% filter(`Tenure 1` == 9) -> Rented

Rented <- Rented %>% filter(`Toilet breakdowns` %in% c(1,2), 
                      `Complete plumbing facilities` %in% c(1,2),
                      `Kitchen facilities functioning` %in% c(1,2),
                      `Condition of building` %in% c(1,2,3)) 




Rented$`Borough and Sub-Borough Area` <- as.factor(Rented$`Borough and Sub-Borough Area`)
Rented$`Toilet breakdowns` <- as.factor(Rented$`Toilet breakdowns`)
Rented$`Complete plumbing facilities` <- as.factor(Rented$`Complete plumbing facilities`)
Rented$`Kitchen facilities functioning` <- as.factor(Rented$`Kitchen facilities functioning`)
Rented$`Condition of building` <- as.factor(Rented$`Condition of building`)


mod <- lm(`Monthly contract rent` ~ `Borough and Sub-Borough Area` + `Number of bedrooms` + 
            `Toilet breakdowns` + `Complete plumbing facilities` + 
            `Kitchen facilities functioning` + `Condition of building`, data = Rented)
summary(mod)
