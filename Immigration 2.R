library(readr)
library(tidyverse)
library(plm)
select <- dplyr::select
set.seed(6969)

NYC <- read_csv("immigration.csv")

### Make sampling weights proper weight
selfid <- NYC %>% filter(`Year Identifier` %in% c(2002,2005,2008,2011,2014,2017))
selfid$`Household Sampling Weight (5 implied decimal places)` <-
  selfid$`Household Sampling Weight (5 implied decimal places)`/100000

### Immigrants by borough
selfid %>% group_by(Borough) %>% summarise(total = sum(`Household Sampling Weight (5 implied decimal places)`),
                                           total_immigrant = sum((`Moved to the U.S. as immigrant` == 1) * `Household Sampling Weight (5 implied decimal places)`), 
                                           total_native = sum(`Moved to the U.S. as immigrant` %in% c(2,9) * `Household Sampling Weight (5 implied decimal places)`),
                                           total_na = sum((`Moved to the U.S. as immigrant` == 8) * `Household Sampling Weight (5 implied decimal places)`),
                                           Immigrant = total_immigrant/total, Native = total_native/total,
                                           Unidentified = total_na/total) %>% 
  dplyr::select(-starts_with("total")) %>%
  gather(key = "type", value = "percent", Immigrant, Native, Unidentified) %>%
  ggplot(aes(x = Borough, y = percent)) + 
  geom_bar(aes(fill = type), position = position_dodge(width = 1), stat = "identity") + 
  labs(title = "Natives vs. Immigrants", x = "Borough", y = "Percent", legend = "Type") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 15),
        title = element_text(size = 20))

### Immigrants by year
selfid %>% group_by(`Year Identifier`) %>% summarise(total = sum(`Household Sampling Weight (5 implied decimal places)`),
                                                     total_immigrant = sum((`Moved to the U.S. as immigrant` == 1) * `Household Sampling Weight (5 implied decimal places)`), 
                                                     total_native = sum(`Moved to the U.S. as immigrant` %in% c(2,9) * `Household Sampling Weight (5 implied decimal places)`),
                                                     total_na = sum((`Moved to the U.S. as immigrant` == 8) * `Household Sampling Weight (5 implied decimal places)`),
                                                     Immigrant = total_immigrant/total, Native = total_native/total,
                                                     Unidentified = total_na/total) %>% 
  dplyr::select(-starts_with("total")) %>%
  gather(key = "type", value = "percent", Immigrant, Native, Unidentified) %>%
  ggplot(aes(x = `Year Identifier`, y = percent)) + 
  geom_bar(aes(fill = type), stat = "identity") + 
  labs(title = "Citizen vs. Non-Citizens", x = "Years", y = "Percent", legend = "Type") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 15),
        title = element_text(size = 20))

### Percentage of 1st and 2nd generation immigrants, compare and contrast
### Make data set, add flags for 1st and 2nd generations
immigrant <- NYC %>% filter(`Moved to the U.S. as immigrant` == 1 | 
                              !(`Place of Householder's Father's Birth` %in% c(7,9,10,98)) |
                              !(`Place of Householder's Mother's Birth` %in% c(7,9,10,98)))
immigrant <- immigrant %>% filter(`Year Identifier` %in% c(1999,2002,2005,2008,2011,2014,2017))
immigrant <- immigrant %>% mutate(selfidflag = ifelse(`Moved to the U.S. as immigrant` == 1, 1, 0))
immigrant <- immigrant %>% 
  mutate(fatherflag = ifelse(!(`Place of Householder's Father's Birth`) %in% c(7,9,10,98) , 1, 0))
immigrant <- immigrant %>% 
  mutate(motherflag = ifelse(!(`Place of Householder's Mother's Birth`) %in% c(7,9,10,98) , 1, 0))
immigrant <- immigrant %>% mutate(parentsflag = ifelse(fatherflag == 1 & motherflag == 1, 1, 0))

### Calculate percents of self-id'd 1st gen immigrants and second generation immigrants
#### Add weights to this!!!!
immigrant %>% select(selfidflag, `Household Sampling Weight (5 implied decimal places)`) %>% 
  mutate(weight = selfidflag*`Household Sampling Weight (5 implied decimal places)`) %>%
  summarise(immi = sum(weight),
            total = sum(`Household Sampling Weight (5 implied decimal places)`),
            percent = immi/total)
immigrant %>% select(fatherflag) %>% summarise(father = sum(fatherflag), total = n(), 
                                                      percent = father/total)
immigrant %>% select(motherflag) %>% summarise(mother = sum(motherflag), total = n(), 
                                                      percent = mother/total)
immigrant %>% select(parentsflag) %>% summarise(parents = sum(parentsflag), total = n(), 
                                                       percent = parents/total)

## Name rent versus own
immigrant$`Tenure 1` <- ifelse(immigrant$`Tenure 1` == 1,"Own","Rent")
immigrant$`Tenure 1` <- as.factor(immigrant$`Tenure 1`)

## Bar charts of own vs rent
selfid %>% group_by(`Tenure 1`) %>% summarise(total = sum(`Household Sampling Weight (5 implied decimal places)`),
                                              total_immigrant = sum((`Moved to the U.S. as immigrant` == 1) * `Household Sampling Weight (5 implied decimal places)`), 
                                              total_native = sum(`Moved to the U.S. as immigrant` %in% c(2,9) * `Household Sampling Weight (5 implied decimal places)`),
                                              total_na = sum((`Moved to the U.S. as immigrant` == 8) * `Household Sampling Weight (5 implied decimal places)`),
                                              Immigrant = total_immigrant/total, Native = total_native/total,
                                              Unidentified = total_na/total) %>% 
  gather(key = "type", value = "percent", Immigrant, Native, Unidentified) %>%
  ggplot(aes(x = `Tenure 1`, y = percent)) + 
  geom_bar(aes(fill = type), stat = "identity") + 
  labs(title = "Citizen vs. Non-Citizens", x = "Own vs. Rent", y = "Percent", legend = "Type") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 15),
        title = element_text(size = 20)) +
  scale_x_continuous(breaks = c(1,9), labels = c("Own", "Rent"))

## Age, income, how long you've stayed in the same unit, how long been in NYC
selfid %>% select(`Householder's Age Recode`, `Year Identifier`) %>% table()
selfid %>% select(`Householder's Race`, `Year Identifier`) %>% table()
selfid %>% select(`Year Householder Moved into Unit`, `Year Identifier`) %>% table()
selfid %>% select(`Year Built Recode`, `Year Identifier`) %>% table()
### Flip sign on income for negatives
selfid %>% select(`Total Household Income Recode`, `Year Identifier`) %>% table()

##### Try 10 year increments
##### Do for every year individually
### Householder's Age
selfid <- selfid %>% mutate(agerecode = case_when(`Householder's Age Recode` < 35 ~ 0,
                                        `Householder's Age Recode` < 55 ~ 1,
                                        `Householder's Age Recode` < 75 ~ 2,
                                        TRUE ~ 3))
selfid %>% group_by(agerecode) %>% summarise(total = sum(`Household Sampling Weight (5 implied decimal places)`),
                                             total_immigrant = sum((`Moved to the U.S. as immigrant` == 1) * `Household Sampling Weight (5 implied decimal places)`), 
                                             total_native = sum(`Moved to the U.S. as immigrant` %in% c(2,9) * `Household Sampling Weight (5 implied decimal places)`),
                                             total_na = sum((`Moved to the U.S. as immigrant` == 8) * `Household Sampling Weight (5 implied decimal places)`),
                                             Immigrant = total_immigrant/total, Native = total_native/total,
                                             Unidentified = total_na/total) %>% 
  gather(key = "type", value = "percent", Immigrant, Native, Unidentified) %>%
  ggplot(aes(x = agerecode, y = percent)) + 
  geom_bar(aes(fill = type), stat = "identity") + 
  labs(title = "Citizen vs. Non-Citizens", x = "House Age", y = "Percent", legend = "Type") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 15),
        title = element_text(size = 20)) +
  scale_x_continuous(breaks = seq(0,3, by = 1), labels = c("15-35", "35-55", "55-75", "75+"))

### Householder's Race
#### Large case when, end with mixed race
str_sub(`Householder's Race`, 1, 2) == "0100000000000000000000"

### Year moved into unit
## Match years to results contained in 2014
#### Count number of years difference between current year and year moved in
#### Do individual years...facet wrap
selfid <- selfid %>% mutate(yearmoved = case_when(`Year Householder Moved into Unit` >= 2012 ~ 1,
                                                  `Year Householder Moved into Unit` < 2012 & `Year Householder Moved into Unit`>= 2009 ~ 2,
                                                  `Year Householder Moved into Unit` < 2009 & `Year Householder Moved into Unit` >= 2006 ~ 3,
                                                  `Year Householder Moved into Unit` < 2006 & `Year Householder Moved into Unit` >= 2003 ~ 4,
                                                  `Year Householder Moved into Unit` < 2003 & `Year Householder Moved into Unit` >= 2000 ~ 5,
                                                  `Year Householder Moved into Unit` < 2000 & `Year Householder Moved into Unit` >= 1995 ~ 6,
                                                  `Year Householder Moved into Unit` < 1995 & `Year Householder Moved into Unit` >= 1990 ~ 7,
                                                  `Year Householder Moved into Unit` < 1990 & `Year Householder Moved into Unit` >= 1985 ~ 8,
                                                  `Year Householder Moved into Unit` < 1985 & `Year Householder Moved into Unit` >= 1980 ~ 9,
                                                  `Year Householder Moved into Unit` < 1980 & `Year Householder Moved into Unit` >= 1970 ~ 10,
                                                  `Year Householder Moved into Unit` < 1970 ~ 11))

selfid %>% group_by(yearmoved) %>% summarise(total = sum(`Household Sampling Weight (5 implied decimal places)`),
                                             total_immigrant = sum((`Moved to the U.S. as immigrant` == 1) * `Household Sampling Weight (5 implied decimal places)`), 
                                             total_native = sum(`Moved to the U.S. as immigrant` %in% c(2,9) * `Household Sampling Weight (5 implied decimal places)`),
                                             total_na = sum((`Moved to the U.S. as immigrant` == 8) * `Household Sampling Weight (5 implied decimal places)`),
                                             Immigrant = total_immigrant/total, Native = total_native/total,
                                             Unidentified = total_na/total) %>% 
  gather(key = "type", value = "percent", Immigrant, Native, Unidentified) %>%
  ggplot(aes(x = yearmoved, y = percent)) + 
  geom_bar(aes(fill = type), stat = "identity") + 
  labs(title = "Citizen vs. Non-Citizens", x = "Year Moved In", y = "Percent", legend = "Type") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 15),
        title = element_text(size = 20)) + 
  scale_x_continuous(breaks = seq(1,11, by = 1), labels = c("<'69", "'70-\n'79", "'80-\n'84", "'85-\n'89", 
                                                            "'90-\n'94", "'95-\n'99", "'00-\n'02", "'03-\n'05", 
                                                            "'06-\n'08", "'09-\n'11", ">'12"))

### Year Dwelling Built
#### Start with 2002, group by decade
#### Be careful with labels
#### Individual years, facet wrap
selfid %>% group_by(`Year Built Recode`) %>% summarise(total = sum(`Household Sampling Weight (5 implied decimal places)`),
                                                       total_immigrant = sum((`Moved to the U.S. as immigrant` == 1) * `Household Sampling Weight (5 implied decimal places)`), 
                                                       total_native = sum(`Moved to the U.S. as immigrant` %in% c(2,9) * `Household Sampling Weight (5 implied decimal places)`),
                                                       total_na = sum((`Moved to the U.S. as immigrant` == 8) * `Household Sampling Weight (5 implied decimal places)`),
                                                       Immigrant = total_immigrant/total, Native = total_native/total,
                                                       Unidentified = total_na/total) %>% 
  gather(key = "type", value = "percent", Immigrant, Native, Unidentified) %>%
  ggplot(aes(x = `Year Built Recode`, y = percent)) + 
  geom_bar(aes(fill = type), stat = "identity") + 
  labs(title = "Citizen vs. Non-Citizens", x = "House Age", y = "Percent", legend = "Type") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 15),
        title = element_text(size = 20)) +
  scale_x_continuous(breaks = seq(1,10, by = 3), labels = c("Old", "Not So \nOld", "Not So \nNew", "New"))


### Household income


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