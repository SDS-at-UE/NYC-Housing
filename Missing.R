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
NYC %>% select(`Toilet breakdowns`) %>% summarise(missing = sum(`Toilet breakdowns` == 8), total = n(), pct_missing = missing/total)
NYC %>% select(`Heating equipment breakdown`) %>% summarise(missing = sum(`Heating equipment breakdown` == 8), total = n(), pct_missing = missing/total)
names(NYC)
NYC %>% select(`Out of pocket rent`) %>% summarise(missing = sum(`Out of pocket rent` == 99998), total = n(), pct_mising = missing/total)
NYC %>% select(`Out of pocket rent`) %>% View()
NYC %>% select(`Out of pocket rent`, `Year Identifier`) %>% group_by(`Year Identifier`) %>% summarise(missing = sum(is.na(`Out of pocket rent`)))
NYC %>% dplyr::filter(starts_with("Y"))

names(dta[[10]]) -> nms
vars_select(nms, starts_with("Floor"))
nms <- names(NYC)                                     
vars_select(nms, starts_with("Y"))
NYC %>% count(`Year Identifier`)


NYC %>% select(`Toilet breakdowns`, `Year Identifier`) %>% group_by(`Year Identifier`) %>%
  summarise(missing = sum(`Toilet breakdowns` == 8),
            total = n(), missing_pct = missing/total)

NYC %>% select(`Heating equipment breakdown`, `Year Identifier`) %>% group_by(`Year Identifier`) %>%
  summarise(missing = sum(`Heating equipment breakdown` == 8),
            total = n(), missing_pct = missing/total)

NYC %>% select(`Out of pocket rent`, `Year Identifier`) %>% group_by(`Year Identifier`) %>% summarise(missing = sum(is.na(`Out of pocket rent`)))
NYC %>% select(`Monthly contract rent`, `Year Identifier`) %>% group_by(`Year Identifier`) %>% summarise(missing = sum(is.na(`Monthly contract rent`)))
NYC %>% select(`Monthly contract rent`, `Year Identifier`) %>% group_by(`Year Identifier`) %>% summarise(missing = sum(`Monthly contract rent` == 99999),
                                                                                                         total = n(),
                                                                                                         missing_pct = missing/total)

NYC <- NYC %>% mutate(waterleakage = ifelse(`Year Identifier` > 2000, 
                                            `Water leakage inside apartment`,
                                            `Water leakage inside apartment (house)`))
NYC %>% filter(`Tenure 1` == 1) -> Owned
NYC %>% filter(`Tenure 1` == 9) -> Rented


## Rented ####################################################################

# A lot of toilet missing in 2008
Rented %>% select(`Toilet breakdowns`, `Year Identifier`) %>% group_by(`Year Identifier`) %>%
  summarise(missing = sum(`Toilet breakdowns` == 8),
            total = n(), missing_pct = missing/total)
# A lot of heating missing in 2008
Rented %>% select(`Heating equipment breakdown`, `Year Identifier`) %>% group_by(`Year Identifier`) %>%
  summarise(missing = sum(`Heating equipment breakdown` == 8),
            total = n(), missing_pct = missing/total)

# Very little missing for monthly contract rent
Rented %>% select(`Monthly contract rent`, `Year Identifier`) %>% 
  group_by(`Year Identifier`) %>% 
  summarise(missing = sum(`Monthly contract rent` == 99999),
            total = n(),
            missing_pct = missing/total)

# No missing plumbing
Rented %>% select(`Complete plumbing facilities`, `Year Identifier`) %>% 
  group_by(`Year Identifier`) %>% 
  summarise(missing = sum(is.na(`Complete plumbing facilities`)),
            total = n(),
            missing_pct = missing/total)

# A lot of missing kitchen in 2008
Rented %>% select(`Kitchen facilities functioning`, `Year Identifier`) %>% 
  group_by(`Year Identifier`) %>% 
  summarise(missing = sum(`Kitchen facilities functioning` == 8),
            total = n(),
            missing_pct = missing/total)

# A lot of missing water leakage in 2008
Rented %>% select(waterleakage, `Year Identifier`) %>% 
  group_by(`Year Identifier`) %>% 
  summarise(missing = sum(waterleakage == 8),
            total = n(),
            missing_pct = missing/total)

# No missing number of rooms
Rented %>% select(`Number of rooms`, `Year Identifier`) %>% 
  group_by(`Year Identifier`) %>% 
  summarise(missing = sum(is.na(`Number of rooms`)),
            total = n(),
            missing_pct = missing/total)

# No missing number of bedrooms
Rented %>% select(`Number of bedrooms`, `Year Identifier`) %>% 
  group_by(`Year Identifier`) %>% 
  summarise(missing = sum(is.na(`Number of bedrooms`)),
            total = n(),
            missing_pct = missing/total)

# No missing condition of building
Rented %>% select(`Condition of building`, `Year Identifier`) %>% 
  group_by(`Year Identifier`) %>% 
  summarise(missing = sum(`Condition of building` == 8),
            total = n(),
            missing_pct = missing/total)

# Floor conditions missing at really high rates
Rented %>% select(`Condition of floors: Sagging or sloping floors`, `Condition of floors: Sagging/sloping floors OR slanted/shifted doorsills/frames`, `Year Identifier`) %>% 
  group_by(`Year Identifier`) %>% 
  summarise(missing = sum(`Condition of floors: Sagging or sloping floors` == 8 | 
                            `Condition of floors: Sagging or sloping floors` == 9),
            missing2 = sum(`Condition of floors: Sagging/sloping floors OR slanted/shifted doorsills/frames` == 8 |
                             `Condition of floors: Sagging/sloping floors OR slanted/shifted doorsills/frames` == 9),
                          total = n(), misssing_pct = missing/total, missing_pct2 = missing2/total)
## Owned #########################################################

# A lot of toilet missing in 2008
Owned %>% select(`Toilet breakdowns`, `Year Identifier`) %>% group_by(`Year Identifier`) %>%
  summarise(missing = sum(`Toilet breakdowns` == 8),
            total = n(), missing_pct = missing/total)

# A lot of missing in 2008
Owned %>% select(`Heating equipment breakdown`, `Year Identifier`) %>% group_by(`Year Identifier`) %>%
  summarise(missing = sum(`Heating equipment breakdown` == 8),
            total = n(), missing_pct = missing/total)

# Only missing value in 1991
Owned %>% select(Value, `Year Identifier`) %>% 
  group_by(`Year Identifier`) %>% 
  summarise(missing = sum(Value == 9999998 | Value == 9999999),
            total = n(),
            missing_pct = missing/total)

# No missing plumbing
Owned %>% select(`Complete plumbing facilities`, `Year Identifier`) %>% 
  group_by(`Year Identifier`) %>% 
  summarise(missing = sum(is.na(`Complete plumbing facilities`)),
            total = n(),
            missing_pct = missing/total)

# A lot of missing kitchen in 2008
Owned %>% select(`Kitchen facilities functioning`, `Year Identifier`) %>% 
  group_by(`Year Identifier`) %>% 
  summarise(missing = sum(`Kitchen facilities functioning` == 8),
            total = n(),
            missing_pct = missing/total)

# A lot of water leakage missing in 2008
Owned %>% select(waterleakage, `Year Identifier`) %>% 
  group_by(`Year Identifier`) %>% 
  summarise(missing = sum(waterleakage == 8),
            total = n(),
            missing_pct = missing/total)

# No missing number of rooms
Owned %>% select(`Number of rooms`, `Year Identifier`) %>% 
  group_by(`Year Identifier`) %>% 
  summarise(missing = sum(is.na(`Number of rooms`)),
            total = n(),
            missing_pct = missing/total)

# No missing number of bedrooms
Owned %>% select(`Number of bedrooms`, `Year Identifier`) %>% 
  group_by(`Year Identifier`) %>% 
  summarise(missing = sum(is.na(`Number of bedrooms`)),
            total = n(),
            missing_pct = missing/total)

# No missing condition of building
Owned %>% select(`Condition of building`, `Year Identifier`) %>% 
  group_by(`Year Identifier`) %>% 
  summarise(missing = sum(`Condition of building` == 8),
            total = n(),
            missing_pct = missing/total)
