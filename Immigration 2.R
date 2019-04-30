library(readr)
library(tidyverse)
library(plm)
select <- dplyr::select
set.seed(6969)

NYC <- read_csv("immigration.csv", col_types = cols(`Householder's Race` = col_character()))

### Make sampling weights proper weight
NYC$`Household Sampling Weight (5 implied decimal places)` <-
  NYC$`Household Sampling Weight (5 implied decimal places)`/10000

### Subset to proper years
selfid <- NYC %>% filter(`Year Identifier` %in% c(2002,2005,2008,2011,2014,2017))


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
  labs(title = "Natives vs. Immigrants", x = "Years", y = "Percent", legend = "Type") +
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
immigrant %>% select(fatherflag, `Household Sampling Weight (5 implied decimal places)`) %>% 
  mutate(weight = fatherflag*`Household Sampling Weight (5 implied decimal places)`) %>%
  summarise(immi = sum(weight),
            total = sum(`Household Sampling Weight (5 implied decimal places)`),
            percent = immi/total)
immigrant %>% select(motherflag, `Household Sampling Weight (5 implied decimal places)`) %>% 
  mutate(weight = motherflag*`Household Sampling Weight (5 implied decimal places)`) %>%
  summarise(immi = sum(weight),
            total = sum(`Household Sampling Weight (5 implied decimal places)`),
            percent = immi/total)
immigrant %>% select(parentsflag, `Household Sampling Weight (5 implied decimal places)`) %>% 
  mutate(weight = parentsflag*`Household Sampling Weight (5 implied decimal places)`) %>%
  summarise(immi = sum(weight),
            total = sum(`Household Sampling Weight (5 implied decimal places)`),
            percent = immi/total)

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
  labs(title = "Natives vs. Immigrants", x = "Own vs. Rent", y = "Percent", legend = "Type") +
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
selfid <- selfid %>% mutate(agerecode = case_when(`Householder's Age Recode` < 35 ~ 1,
                                        `Householder's Age Recode` < 55 ~ 2,
                                        `Householder's Age Recode` < 75 ~ 3,
                                        TRUE ~ 4))
selfid <- selfid %>% mutate(agerecode2 = case_when(`Householder's Age Recode` < 25 ~ 1,
                                                   `Householder's Age Recode` < 35 ~ 2,
                                                   `Householder's Age Recode` < 45 ~ 3,
                                                   `Householder's Age Recode` < 55 ~ 4,
                                                   `Householder's Age Recode` < 65 ~ 5,
                                                   `Householder's Age Recode` < 75 ~ 6,
                                                   TRUE ~ 7))
#### Age brackets of 20 years
selfid %>% group_by(agerecode) %>% summarise(total = sum(`Household Sampling Weight (5 implied decimal places)`),
                                             total_immigrant = sum((`Moved to the U.S. as immigrant` == 1) * `Household Sampling Weight (5 implied decimal places)`), 
                                             total_native = sum(`Moved to the U.S. as immigrant` %in% c(2,9) * `Household Sampling Weight (5 implied decimal places)`),
                                             total_na = sum((`Moved to the U.S. as immigrant` == 8) * `Household Sampling Weight (5 implied decimal places)`),
                                             Immigrant = total_immigrant/total, Native = total_native/total,
                                             Unidentified = total_na/total) %>% 
  gather(key = "type", value = "percent", Immigrant, Native, Unidentified) %>%
  ggplot(aes(x = agerecode, y = percent)) + 
  geom_bar(aes(fill = type), stat = "identity") + 
  labs(title = "Natives vs. Immigrants", x = "House Age", y = "Percent", legend = "Type") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 15),
        title = element_text(size = 20)) +
  scale_x_continuous(breaks = seq(1,4, by = 1), labels = c("15-35", "35-55", "55-75", "75+"))

#### Age brackets of 10 years
selfid %>% group_by(agerecode2) %>% summarise(total = sum(`Household Sampling Weight (5 implied decimal places)`),
                                             total_immigrant = sum((`Moved to the U.S. as immigrant` == 1) * `Household Sampling Weight (5 implied decimal places)`), 
                                             total_native = sum(`Moved to the U.S. as immigrant` %in% c(2,9) * `Household Sampling Weight (5 implied decimal places)`),
                                             total_na = sum((`Moved to the U.S. as immigrant` == 8) * `Household Sampling Weight (5 implied decimal places)`),
                                             Immigrant = total_immigrant/total, Native = total_native/total,
                                             Unidentified = total_na/total) %>% 
  gather(key = "type", value = "percent", Immigrant, Native, Unidentified) %>%
  ggplot(aes(x = agerecode2, y = percent)) + 
  geom_bar(aes(fill = type), stat = "identity") + 
  labs(title = "Natives vs. Immigrants", x = "House Age", y = "Percent", legend = "Type") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 15),
        title = element_text(size = 20)) +
  scale_x_continuous(breaks = seq(1,7, by = 1), labels = c("15-25", "25-35", "35-45", "45-55", 
                                                           "55-65", "65-75", "75+"))

### Householder's Race
#### Large case when, end with mixed race
selfid <- selfid %>% mutate(racerecode = case_when(`Householder's Race` == "1e20" ~ 1,
                                         `Householder's Race` == "2e18" ~ 2,
                                         `Householder's Race` == "3e16" ~ 3,
                                         `Householder's Race` == "4e14" ~ 4,
                                         `Householder's Race` == "5e12" ~ 5,
                                         `Householder's Race` == "6e10" ~ 6,
                                         `Householder's Race` == "7e8" ~ 8,
                                         `Householder's Race` == "8e6" ~ 7,
                                         `Householder's Race` == "9e4" ~ 8,
                                         `Householder's Race` == "1e3" ~ 9,
                                         `Householder's Race` == "1" ~ 1,
                                         `Householder's Race` == "2" ~ 2,
                                         `Householder's Race` == "3" ~ 3,
                                         `Householder's Race` == "4" ~ 4,
                                         `Householder's Race` == "5" ~ 5,
                                         `Householder's Race` == "6" ~ 6,
                                         `Householder's Race` == "7" ~ 7,
                                         `Householder's Race` == "8" ~ 8,
                                         `Householder's Race` == "9" ~ 9,
                                         `Householder's Race` == "10" ~ 10,
                                         `Householder's Race` == "11" ~ 10,
                                         TRUE ~ 10))

selfid %>% group_by(racerecode) %>% summarise(total = sum(`Household Sampling Weight (5 implied decimal places)`),
                                             total_immigrant = sum((`Moved to the U.S. as immigrant` == 1) * `Household Sampling Weight (5 implied decimal places)`), 
                                             total_native = sum(`Moved to the U.S. as immigrant` %in% c(2,9) * `Household Sampling Weight (5 implied decimal places)`),
                                             total_na = sum((`Moved to the U.S. as immigrant` == 8) * `Household Sampling Weight (5 implied decimal places)`),
                                             Immigrant = total_immigrant/total, Native = total_native/total,
                                             Unidentified = total_na/total) %>% 
  gather(key = "type", value = "percent", Immigrant, Native, Unidentified) %>%
  ggplot(aes(x = racerecode, y = percent)) + 
  geom_bar(aes(fill = type), stat = "identity") + 
  labs(title = "Natives vs. Immigrants", x = "Race", y = "Percent", legend = "Type") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 15),
        title = element_text(size = 20)) +
  scale_x_continuous(breaks = seq(1,10, by = 1), labels = c("White", "Black", "American Indian", 
                                                            "Chinese", "Filipino", "Korean",
                                                            "Indian", "Other Asian", "Islander", "Mixed Race"))

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

#### Year householder moved into unit by decade
## Relevel 2017
selfid2017 <- selfid %>% filter(`Year Identifier` == 2017) %>% 
  mutate(yearmovedrecode = case_when(`Year Householder Moved into Unit` >= 2015 ~ 1,
                                     `Year Householder Moved into Unit` >= 2012 ~ 2,
                                     `Year Householder Moved into Unit` >= 2009 ~ 3,
                                     `Year Householder Moved into Unit` >= 2006 ~ 4,
                                     `Year Householder Moved into Unit` >= 2003 ~ 5,
                                     `Year Householder Moved into Unit` >= 1998 ~ 6,
                                     `Year Householder Moved into Unit` >= 1993 ~ 7,
                                     `Year Householder Moved into Unit` >= 1988 ~ 8,
                                     `Year Householder Moved into Unit` >= 1983 ~ 9,
                                     `Year Householder Moved into Unit` >= 1973 ~ 10,
                                     TRUE ~ 11))

## Relevel 2011
selfid2011 <- selfid %>% filter(`Year Identifier` == 2011) %>% 
  mutate(yearmovedrecode = case_when(`Year Householder Moved into Unit` >= 2009 ~ 1,
                                     `Year Householder Moved into Unit` >= 2006 ~ 2,
                                     `Year Householder Moved into Unit` >= 2003 ~ 3,
                                     `Year Householder Moved into Unit` >= 2000 ~ 4,
                                     `Year Householder Moved into Unit` >= 1997 ~ 5,
                                     `Year Householder Moved into Unit` >= 1992 ~ 6,
                                     `Year Householder Moved into Unit` >= 1987 ~ 7,
                                     `Year Householder Moved into Unit` >= 1982 ~ 8,
                                     `Year Householder Moved into Unit` >= 1977 ~ 9,
                                     `Year Householder Moved into Unit` >= 1967 ~ 10,
                                     TRUE ~ 11))

## Relevel 2008
selfid2008 <- selfid %>% filter(`Year Identifier` == 2008) %>% 
  mutate(yearmovedrecode = case_when(`Year Householder Moved into Unit` >= 2006 ~ 1,
                                     `Year Householder Moved into Unit` >= 2003 ~ 2,
                                     `Year Householder Moved into Unit` >= 2000 ~ 3,
                                     `Year Householder Moved into Unit` >= 1997 ~ 4,
                                     `Year Householder Moved into Unit` >= 1994 ~ 5,
                                     `Year Householder Moved into Unit` >= 1989 ~ 6,
                                     `Year Householder Moved into Unit` >= 1984 ~ 7,
                                     `Year Householder Moved into Unit` >= 1979 ~ 8,
                                     `Year Householder Moved into Unit` >= 1974 ~ 9,
                                     `Year Householder Moved into Unit` >= 1964 ~ 10,
                                     TRUE ~ 11))

## Relevel 2005
selfid2005 <- selfid %>% filter(`Year Identifier` == 2005) %>% 
  mutate(yearmovedrecode = case_when(`Year Householder Moved into Unit` >= 2003 ~ 1,
                                     `Year Householder Moved into Unit` >= 2000 ~ 2,
                                     `Year Householder Moved into Unit` >= 1997 ~ 3,
                                     `Year Householder Moved into Unit` >= 1994 ~ 4,
                                     `Year Householder Moved into Unit` >= 1991 ~ 5,
                                     `Year Householder Moved into Unit` >= 1986 ~ 6,
                                     `Year Householder Moved into Unit` >= 1981 ~ 7,
                                     `Year Householder Moved into Unit` >= 1976 ~ 8,
                                     `Year Householder Moved into Unit` >= 1971 ~ 9,
                                     `Year Householder Moved into Unit` >= 1961 ~ 10,
                                     TRUE ~ 11))

## Relevel 2002
selfid2002 <- selfid %>% filter(`Year Identifier` == 2002) %>% 
  mutate(yearmovedrecode = case_when(`Year Householder Moved into Unit` >= 2000 ~ 1,
                                     `Year Householder Moved into Unit` >= 1997 ~ 2,
                                     `Year Householder Moved into Unit` >= 1994 ~ 3,
                                     `Year Householder Moved into Unit` >= 1991 ~ 4,
                                     `Year Householder Moved into Unit` >= 1988 ~ 5,
                                     `Year Householder Moved into Unit` >= 1983 ~ 6,
                                     `Year Householder Moved into Unit` >= 1978 ~ 7,
                                     `Year Householder Moved into Unit` >= 1973 ~ 8,
                                     `Year Householder Moved into Unit` >= 1968 ~ 9,
                                     `Year Householder Moved into Unit` >= 1958 ~ 10,
                                     TRUE ~ 11))

selfid2014 <- selfid %>% filter(`Year Identifier` == 2014) %>% 
  mutate(yearmovedrecode = `Year Householder Moved into Unit`)

selfid2000s <- bind_rows(selfid2002, selfid2005, selfid2008, selfid2011, selfid2014, selfid2017)
selfid2000s <- selfid2000s %>% mutate(newyear = case_when(`Year Identifier` == 2017 ~ "2010s",
                                           `Year Identifier` == 2014 ~ "2010s",
                                           `Year Identifier` == 2011 ~ "2010s",
                                          TRUE ~ "2000s"))

selfid2000s %>% group_by(yearmovedrecode, newyear) %>% summarise(total = sum(`Household Sampling Weight (5 implied decimal places)`),
                                             total_immigrant = sum((`Moved to the U.S. as immigrant` == 1) * `Household Sampling Weight (5 implied decimal places)`), 
                                             total_native = sum(`Moved to the U.S. as immigrant` %in% c(2,9) * `Household Sampling Weight (5 implied decimal places)`),
                                             total_na = sum((`Moved to the U.S. as immigrant` == 8) * `Household Sampling Weight (5 implied decimal places)`),
                                             Immigrant = total_immigrant/total, Native = total_native/total,
                                             Unidentified = total_na/total) %>% 
  gather(key = "type", value = "percent", Immigrant, Native, Unidentified) %>%
  ggplot(aes(x = yearmovedrecode, y = percent)) + 
  geom_bar(aes(fill = type), stat = "identity") + 
  labs(title = "Citizen vs. Non-Citizens", x = "Years Spent in Current Dwelling", y = "Percent", legend = "Type") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 15),
        title = element_text(size = 20)) +
  facet_wrap(~newyear) + 
  scale_x_continuous(breaks = seq(1,11, by = 1), labels = c("<2", "2-5", "5-8", "8-11", "11-14", "14-19",
                                                            "19-24", "24-29", "29-34", "34-44", ">44"))

### Year Dwelling Built
#### Start with 2002, group by decade
#### Be careful with labels
#### Individual years, facet wrap
selfid <- selfid %>% mutate(newyear = case_when(`Year Identifier` == 2017 ~ "2010s",
                                                          `Year Identifier` == 2014 ~ "2010s",
                                                          `Year Identifier` == 2011 ~ "2010s",
                                                          TRUE ~ "2000s"))
selfid %>% group_by(`Year Built Recode`, newyear) %>% summarise(total = sum(`Household Sampling Weight (5 implied decimal places)`),
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
  facet_wrap(~newyear) +
  scale_x_continuous(breaks = seq(1,10, by = 3), labels = c("Old", "Not So \nOld", "Not So \nNew", "New"))


### Household income
selfid <- selfid %>% mutate(newincome = ifelse(`Total Household Income Recode` < 0, -`Total Household Income Recode`, 
                                     `Total Household Income Recode`))
selfid <- selfid %>% mutate(newincomerecode = case_when(newincome < 25000 ~ 1,
                                              newincome < 50000 ~ 2,
                                              newincome < 75000 ~ 3,
                                              newincome < 100000 ~ 4,
                                              newincome < 250000 ~ 5,
                                              newincome < 500000 ~ 6,
                                              newincome < 1000000 ~ 7,
                                              newincome < 5000000 ~ 8,
                                              TRUE ~ 9))

selfid %>% group_by(newincomerecode, newyear) %>% summarise(total = sum(`Household Sampling Weight (5 implied decimal places)`),
                                                                total_immigrant = sum((`Moved to the U.S. as immigrant` == 1) * `Household Sampling Weight (5 implied decimal places)`), 
                                                                total_native = sum(`Moved to the U.S. as immigrant` %in% c(2,9) * `Household Sampling Weight (5 implied decimal places)`),
                                                                total_na = sum((`Moved to the U.S. as immigrant` == 8) * `Household Sampling Weight (5 implied decimal places)`),
                                                                Immigrant = total_immigrant/total, Native = total_native/total,
                                                                Unidentified = total_na/total) %>% 
  gather(key = "type", value = "percent", Immigrant, Native, Unidentified) %>%
  ggplot(aes(x = newincomerecode, y = percent)) + 
  geom_bar(aes(fill = type), stat = "identity") + 
  labs(title = "Citizen vs. Non-Citizens", x = "House Age", y = "Percent", legend = "Type") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 15),
        title = element_text(size = 20)) +
  facet_wrap(~newyear)

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