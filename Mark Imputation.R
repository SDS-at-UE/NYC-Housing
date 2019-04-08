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

#### Imputation for # of Cockroaches
int <- subset(internal[[4]],internal[[4]] != 8) %>% factor()
p1 <- summary(int)[[1]]/length(int)
p2 <- summary(int)[[2]]/length(int)
p3 <- summary(int)[[3]]/length(int)
p4 <- 1-p1-p2-p3
miss <- which(internal[[4]]==8)
impute_1 <- c(1:length(miss)) %>% sample(length(miss)*p1) %>% floor()
impute_2 <- setdiff(1:length(miss), impute_1)
impute_25 <- impute_2 %>% sample(length(impute_2)*p2/(p2+p3)) %>% floor()
impute_3 <- setdiff(impute_2, impute_25)
impute_35 <- impute_3 %>% sample(length(impute_3)*p3/(p3+p4))
impute_4 <- setdiff(impute_3, impute_35)
  
inn <- case_when(internal[[4]]== 1 ~ 1,
                   internal[[4]]== 2 ~ 2,
                   internal[[4]]== 3 ~ 3,
                   internal[[4]]== 4 ~ 4,
                   internal[[4]]== 5 ~ 1,
                   internal[[4]]== 8 ~ 8) 
  
inn[miss[impute_1]] <- 1
inn[miss[impute_25]] <- 2
inn[miss[impute_3]] <- 3
inn[miss[impute_4]] <- 4
inn <- inn - 1
internal_imputed[4] <- inn

#### Imputation for Functioning Air Conditioning
int <- subset(internal[[5]], internal[[5]] != 8) %>% factor()
p1 <- summary(int)[[1]]/length(int)
p2 <- summary(int)[[2]]/length(int)
p3 <- 1-p1-p2
miss <- which(internal[[5]] == 8)
impute_1 <- c(1:length(miss)) %>% sample(length(miss)*p1)
impute_2 <- setdiff(1:length(miss), impute_1)
impute_25 <- impute_2 %>% sample(length(impute_2)*p2/(p2+p3))
impute_3 <- setdiff(impute_2, impute_25)

inn <- case_when(internal[[5]]== 1 ~ 1,
                 internal[[5]]== 2 ~ 2,
                 internal[[5]]== 3 ~ 3,
                 internal[[5]]== 4 ~ 3,
                 internal[[5]]== 8 ~ 8) 

inn[miss[impute_1]] <- 1
inn[miss[impute_25]] <- 2
inn[miss[impute_3]] <- 3
internal_imputed[5] <- inn

internal_imputed$`Functioning Air Conditioning` <-
  ifelse(internal_imputed$`Functioning Air Conditioning` == 1 | 
           internal_imputed$`Functioning Air Conditioning` == 2, 0, 
         internal_imputed$`Functioning Air Conditioning`)
internal_imputed$`Functioning Air Conditioning` <-
  ifelse(internal_imputed$`Functioning Air Conditioning` == 3, 1, 
         internal_imputed$`Functioning Air Conditioning`)

internal_imputed <- internal_imputed %>% 
  mutate(QIndex = waterleakage + `Presence of mice or rats` +
                              `Heating equipment breakdown`)

internal_imputed %>% 
  mutate(QIndeXtra = ifelse(`Year Identifier` >= 2014, 
                            waterleakage + `Presence of mice or rats` +
                              `Heating equipment breakdown` + 
                              .25*`Number of Cockroaches` + 
                              `Functioning Air Conditioning`,
                            NA))


# Formating year
internal_imputed[[6]] <- case_when(internal_imputed[[6]] == 91 ~ 1991,
                                    internal_imputed[[6]] == 93 ~ 1993,
                                    internal_imputed[[6]] == 96 ~ 1996,
                                    internal_imputed[[6]] == 99 ~ 1999,
                                    TRUE ~ as.double(internal_imputed[[6]]))
internal_imputed[[6]] <- factor(internal_imputed[[6]])

by_year <- internal_imputed %>% 
  group_by(`Year Identifier`) %>% summarise(Score = mean(QIndex))
ggplot(by_year,aes(x = `Year Identifier`, y = Score, group = 0)) + 
  geom_point() + geom_line()

