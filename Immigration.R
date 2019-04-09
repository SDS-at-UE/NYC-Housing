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

### Add graphs about percentage of immigrants year over year, borough to borough

### Puerto Ricans added to data set?
### Percentage of 1st and 2nd generation immigrants, compare and contrast
### Make data set, add flags for 1st and 2nd generations
NYC %>% filter(`Moved to the U.S. as immigrant` == 1) %>% 
  select(`Moved to the U.S. as immigrant`) %>% table()

NYC %>% filter(!(`Place of Householder's Father's Birth` %in% c(7,9,10,98))) %>% 
  select(`Place of Householder's Father's Birth`) %>% count()

NYC %>% filter(!(`Place of Householder's Mother's Birth` %in% c(7,9,10,98))) %>% 
  select(`Place of Householder's Mother's Birth`) %>% count()

## Split into rent versus own
own <- NYC %>% filter(`Tenure 1` == 1)
rent <- NYC %>% filter(`Tenure 1` == 9)

## Bar charts of rent vs own



## Age, income, how long you've stayed in the same unit, how long been in NYC



#impute housing quality index for immigrants
NYC <- NYC %>% mutate(waterleakage = ifelse(`Year Identifier` > 2000, 
                                            `Water leakage inside apartment`,
                                            `Water leakage inside apartment (house)`))


#### Relevel pertinent variables
NYC$waterleakage <- ifelse(NYC$waterleakage == 2, 0, NYC$waterleakage)
NYC$`Presence of mice or rats` <- 
  ifelse(NYC$`Presence of mice or rats` == 2, 0, NYC$`Presence of mice or rats`)
NYC$`Heating equipment breakdown` <-
  ifelse(NYC$`Heating equipment breakdown` == 1, 2, NYC$`Heating equipment breakdown`)
NYC$`Heating equipment breakdown` <-
  ifelse(NYC$`Heating equipment breakdown` == 0, 1, NYC$`Heating equipment breakdown`)
NYC$`Heating equipment breakdown` <-
  ifelse(NYC$`Heating equipment breakdown` == 2, 0, NYC$`Heating equipment breakdown`)

internal <- NYC %>% select(waterleakage, `Presence of mice or rats`,
                           `Heating equipment breakdown`,
                           `Number of Cockroaches`, `Functioning Air Conditioning`,
                           `Year Identifier`)

### Air Conditioning starts in 2014
### Number of Cockroaches starts in 2008

internal_imputed <- internal
for (i in c(1:3)) {
  int <- subset(internal[[i]],internal[[i]] != 8)
  p <- mean(int)
  q <- 1 - p
  miss <- which(internal[[i]]==8)
  impute_1 <- c(1:length(miss)) %>% sample(length(miss)*p)
  impute_2 <- setdiff(1:length(miss),impute_1)
  
  inn <- case_when(internal[[i]]== 1 ~ 1,
                   internal[[i]]== 8 ~ 8,
                   internal[[i]]== 0 ~ 0) 
  
  inn[miss[impute_1]] <- 1 
  inn[miss[impute_2]] <- 0
  internal_imputed[i] <- inn
}

internal_imputed <- internal_imputed %>% 
  mutate(QIndex = waterleakage + `Presence of mice or rats` +
           `Heating equipment breakdown`)


# Formatting year
internal_imputed[[6]] <- case_when(internal_imputed[[6]] == 91 ~ 1991,
                                   internal_imputed[[6]] == 93 ~ 1993,
                                   internal_imputed[[6]] == 96 ~ 1996,
                                   internal_imputed[[6]] == 99 ~ 1999,
                                   TRUE ~ as.double(internal_imputed[[6]]))
internal_imputed[[6]] <- factor(internal_imputed[[6]])


# Formatting borough
internal_imputed$Borough <- NYC$Borough
internal_imputed[[8]] <- case_when(internal_imputed[[8]] == 1 ~ "Bronx",
                                   internal_imputed[[8]] == 2 ~ "Brooklyn",
                                   internal_imputed[[8]] == 3 ~ "Manhattan",
                                   internal_imputed[[8]] == 4 ~ "Queens",
                                   internal_imputed[[8]] == 5 ~ "Staten Island")



# Select external variables
external <- dta[[1]] %>% select(contains("Window"),contains("Exterior Walls"),contains("Stairways"),contains("Condition of building"),contains("Number of Units"),contains("Stories"),contains("identifier"),Borough,`Tenure 1`) 
for (i in 2:10) {
  dta[[i]] %>% select(contains("Window"),contains("Exterior Walls"),contains("Stairways"),contains("Condition of building"),contains("Number of Units"),contains("Stories"),contains("identifier"),Borough,`Tenure 1`) %>%
    bind_rows(external) -> 
    external
}

# Clean & formating
external <- external %>% select(-`Condition of Stairways (Exterior and Interior): No interior steps or stairways`,-`Condition of Stairways (Exterior and Interior): No exterior steps or stairways`,-`Condition of Stairways (Exterior and Interior): No stairways`)
external$`Number of Units in Building` <- factor(external$`Number of Units in Building`)
external$`Stories in building` <- factor(external$`Stories in building`)
external <- external[,-c(7,22,23)]

# Impute columns with 1,8,and 9
external_imputed <- external
for (i in c(1:14)) {
  ex <- subset(external[[i]],external[[i]] != 8)
  ex <- ifelse(ex == 1, 1, 0)
  p <- mean(ex)
  q <- 1 - p
  miss <- which(external[[i]]==8)
  impute_1 <- c(1:length(miss)) %>% sample(length(miss)*p)
  impute_2 <- setdiff(1:length(miss),impute_1)
  
  exx <- case_when(external[[i]]== 1 ~ 1,
                   external[[i]]== 9 ~ 0,
                   external[[i]]== 8 ~ 8,
                   external[[i]]== 2 ~ 0)
  exx[miss[impute_1]] <- 1 
  exx[miss[impute_2]] <- 0
  external_imputed[i] <- exx
}

# Impute column with 1, 2, 3, and 8 (condition of building)
ex <- subset(external[[15]], external[[15]] != 8) %>% factor()
p1 <- summary(ex)[[1]]/length(ex)
p2 <- summary(ex)[[2]]/length(ex)
p3 <- 1-p1-p2
miss <- which(external[[15]] == 8)
impute_1 <- c(1:length(miss)) %>% sample(length(miss)*p1)
impute_2 <- setdiff(1:length(miss), impute_1)
impute_25 <- impute_2 %>% sample(length(impute_2)*p2/(p2+p3))
impute_3 <- setdiff(impute_2, impute_25)

external_imputed[[15]][miss[impute_1]] <- 1
external_imputed[[15]][miss[impute_25]] <- 2
external_imputed[[15]][miss[impute_3]] <- 3
external_imputed[[15]] <- case_when(external_imputed[[15]] == 1 ~ 3,
                                    external_imputed[[15]] == 2 ~ 1,
                                    external_imputed[[15]] == 3 ~ 2)
# check NA's
external_imputed %>% mutate_all(., funs(factor(.))) %>% summary()

# Computing score
external_imputed <- external_imputed %>% mutate(score = external_imputed[[1]] + external_imputed[[2]]  
                                                +external_imputed[[3]] + external_imputed[[7]] 
                                                +external_imputed[[8]] + external_imputed[[11]] 
                                                +external_imputed[[12]] + external_imputed[[15]])   
# Formating year
external_imputed[[18]] <- case_when(external_imputed[[18]] == 91 ~ 1991,
                                    external_imputed[[18]] == 93 ~ 1993,
                                    external_imputed[[18]] == 96 ~ 1996,
                                    external_imputed[[18]] == 99 ~ 1999,
                                    TRUE ~ as.double(external_imputed[[18]]))
external_imputed[[18]] <- factor(external_imputed[[18]])

# Renaming borough
external_imputed[[19]] <- case_when(external_imputed[[19]] == 1 ~ "Bronx",
                                    external_imputed[[19]] == 2 ~ "Brooklyn",
                                    external_imputed[[19]] == 3 ~ "Manhattan",
                                    external_imputed[[19]] == 4 ~ "Queens",
                                    external_imputed[[19]] == 5 ~ "Staten Island")

# Renaming status
external_imputed[[20]] <- ifelse(external_imputed[[20]]==1,"Own","Rent")

### Select internal structural variables
internal2 <- dta[[1]] %>% select(contains("bedrooms"), 
                                 contains("rooms"),contains("Interior"),
                                 contains("Condition of building"),
                                 contains("Kitchen"),contains("Plumbing"), 
                                 contains("Toilet"), contains("holes")) 
for (i in 2:10) {
  dta[[i]] %>% select(contains("bedrooms"), contains("rooms"),contains("Interior"),
                      contains("Condition of building"),contains("Kitchen"),
                      contains("Plumbing"), contains("Toilet"), 
                      contains("holes")) %>%
    bind_rows(internal2) -> 
    internal2
}

summary(factor(internal2[[17]]))
# Impute columns with 1,2 and 8 (Crack holes walls)
internal2_imputed <- internal2
for (i in c(9, 16)) {
  int <- subset(internal2[[i]],internal2[[i]] != 8)
  int <- ifelse(int == 1, 1, 0)
  p <- mean(int)
  q <- 1 - p
  miss <- which(internal2[[i]]==8)
  impute_1 <- c(1:length(miss)) %>% sample(length(miss)*p)
  impute_2 <- setdiff(1:length(miss),impute_1)
  
  int <- case_when(internal2[[i]]== 1 ~ 1,
                   internal2[[i]]== 8 ~ 8,
                   internal2[[i]]== 2 ~ 0) 
  int[miss[impute_1]] <- 1 
  int[miss[impute_2]] <- 0
  internal2_imputed[i] <- int
}

# We can use cracks of holes in interior walls,
# number of bedrooms, and number of rooms
internal2_imputed %>% select(`Number of bedrooms`, `Number of rooms`,
                             `Cracks of holes in interior walls`, 
                             `Holes in floors`) -> 
  internal2_imputed

### Add Year and Borough to the data set
internal2_imputed$`Year Identifier` <- NYC$`Year Identifier`
internal2_imputed$Borough <- NYC$Borough

# Formatting year
internal2_imputed[[5]] <- case_when(internal2_imputed[[5]] == 91 ~ 1991,
                                    internal2_imputed[[5]] == 93 ~ 1993,
                                    internal2_imputed[[5]] == 96 ~ 1996,
                                    internal2_imputed[[5]] == 99 ~ 1999,
                                    TRUE ~ as.double(internal2_imputed[[5]]))
internal2_imputed[[5]] <- factor(internal2_imputed[[5]])


# Formatting borough
internal2_imputed$Borough <- NYC$Borough
internal2_imputed[[6]] <- case_when(internal2_imputed[[6]] == 1 ~ "Bronx",
                                    internal2_imputed[[6]] == 2 ~ "Brooklyn",
                                    internal2_imputed[[6]] == 3 ~ "Manhattan",
                                    internal2_imputed[[6]] == 4 ~ "Queens",
                                    internal2_imputed[[6]] == 5 ~ "Staten Island")

# Create an index
internal2_imputed <- internal2_imputed %>% 
  mutate(Index = `Cracks of holes in interior walls` + `Holes in floors`)


## Combine the 3 data sets
immigrant <- bind_cols(external_imputed, internal_imputed, internal2_imputed)
immigrant <- immigrant %>% select(Borough, `Year Identifier`, `Tenure 1`, 
                              score, QIndex, Index)

## Compute the combined quality index
immigrant <- immigrant %>% mutate(QualityIndex = score + QIndex + Index)