library(tidyverse)
# Reading in all data
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

# Select external variables
internal <- dta[[1]] %>% select(contains("bedrooms"), contains("rooms"),contains("Interior"),contains("Condition of building"),contains("Kitchen"),contains("Plumbing"), contains("Toilet"), contains("holes")) 
for (i in 2:10) {
  dta[[i]] %>% select(contains("bedrooms"), contains("rooms"),contains("Interior"),contains("Condition of building"),contains("Kitchen"),contains("Plumbing"), contains("Toilet"), contains("holes")) %>%
    bind_rows(internal) -> 
    internal
}

summary(factor(internal[[17]]))
# Impute columns with 1,2 and 8 (Crack holes walls)
internal_imputed <- internal
for (i in c(9, 16)) {
  int <- subset(internal[[i]],internal[[i]] != 8)
  int <- ifelse(int == 1, 1, 0)
  p <- mean(int)
  q <- 1 - p
  miss <- which(internal[[i]]==8)
  impute_1 <- c(1:length(miss)) %>% sample(length(miss)*p)
  impute_2 <- setdiff(1:length(miss),impute_1)
  
  int <- case_when(internal[[i]]== 1 ~ 1,
                   internal[[i]]== 8 ~ 8,
                   internal[[i]]== 2 ~ 0) 
  int[miss[impute_1]] <- 1 
  int[miss[impute_2]] <- 0
  internal_imputed[i] <- int
}



# Impute columns with 1,2,8, and 9 (Toilet breakdowns and Kitchen facilities functioning)
for (i in c(12, 14)) {
  int <- subset(internal[[i]],internal[[i]] != 8)
  int <- ifelse(int == 1, 1, 0)
  p <- mean(int)
  q <- 1 - p
  miss <- which(internal[[i]]==8)
  impute_1 <- c(1:length(miss)) %>% sample(length(miss)*p)
  impute_2 <- setdiff(1:length(miss),impute_1)
  
  internal <- case_when(internal[[i]]== 1 ~ 1,
                   internal[[i]]== 9 ~ 0,
                   internal[[i]]== 8 ~ 8,
                   internal[[i]]== 2 ~ 0,
                   internal[[i]]== 3 ~ 8) 
  int[miss[impute_1]] <- 1 
  int[miss[impute_2]] <- 0
  internal_imputed[i] <- int
}

# Impute Columns with 0,1,2,3,8 (Complete Kitchen Facilities) Refering to whether or not unit has their own kitchen facility
for (i in c()) {
  int <- subset(internal[[i]],internal[[i]] != 8)
  int <- ifelse(int == 1, 1, 0)
  p <- mean(int)
  q <- 1 - p
  miss <- which(internal[[i]]==8)
  impute_1 <- c(1:length(miss)) %>% sample(length(miss)*p)
  impute_2 <- setdiff(1:length(miss),impute_1)
  
  internal <- case_when(internal[[i]]== 0 ~ 1,
                        internal[[i]]== 1 ~ 0,
                        internal[[i]]== 8 ~ 8,
                        internal[[i]]== 2 ~ 0,
                        internal[[i]]== 3 ~ 0) 
  int[miss[impute_1]] <- 1 
  int[miss[impute_2]] <- 0
  internal_imputed[i] <- int
}

internal_imputed %>% filter(`Number of bedrooms` != 98) -> internal_imputed

# We can use cracks of holes in interior walls, kitchen facilities/kitchen facilities functioning,
# Toilet Breakdown, number of bedrooms, and number of rooms


internal_imputed %>% select(`Number of bedrooms`, `Number of rooms`,
                            `Kitchen facilities functioning`, `Complete plumbing facilities`, 
                            `Cracks of holes in interior walls`, `Holes in floors`, 
                            `Toilet breakdowns`, `Kitchen facilities`) -> imputed_data
