library(readr)
library(tidyverse)
set.seed(6969)
NYCHVS_1991_Occupied_File_for_ASA_Challenge <- read_csv("DATA/NYCHVS 1991 Occupied File for ASA Challenge.csv", skip = 1)
View(NYCHVS_1991_Occupied_File_for_ASA_Challenge)

samp <- sample_frac(NYCHVS_1991_Occupied_File_for_ASA_Challenge, .25)

## All households in 1991 paid their electricity bill included with rent
elec <-NYCHVS_1991_Occupied_File_for_ASA_Challenge %>% 
  filter(`Electricity paid separately` == 3)

## 9233 of 14899 observations in 1991 paid gas separately
gas <-NYCHVS_1991_Occupied_File_for_ASA_Challenge %>% 
  filter(`Gas paid separately`== 1) 

hist(gas$`Monthly cost (gas)`)

## 1374 households have gas bills over $100
gas %>% filter(!(`Monthly cost (gas)` %in% c(998, 999))) %>% 
  ggplot(aes(x = `Monthly cost (gas)`)) +   geom_histogram(binwidth = 10) + 
    coord_cartesian(xlim = c(0, 200))
## 35 households have gas bills over $500
gas %>% filter(!(`Monthly cost (gas)` %in% c(998, 999)),
               `Monthly cost (gas)` >= 500) %>% nrow()

## 2241 households with combined gas and electricity bills
gaselec <- NYCHVS_1991_Occupied_File_for_ASA_Challenge %>% 
  filter(!(`Combined gas and electric` %in% c(998, 999)))

## 1977 of 14899 households who pay water bills
water <-NYCHVS_1991_Occupied_File_for_ASA_Challenge %>% 
  filter(`Water and sewer paid separately` == 1) 

## Distribution of water bill amount
water %>% filter(!(`Yearly cost (water sewer)` %in% c(998,999))) %>%
  ggplot(aes(x = `Yearly cost (water sewer)`)) + geom_histogram(binwidth = 50)

## Difference in average cost of water bill compared by borough
water %>% filter(!(`Yearly cost (water sewer)` %in% c(998,999))) %>%
  ggplot(aes(x = factor(Borough), y = `Yearly cost (water sewer)`)) + geom_boxplot()
### Queens has lots of outliers (#4)  

## Difference in average cost of gas bill compared by borough
gas %>% filter(!(`Monthly cost (gas)` %in% c(998, 999))) %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + geom_boxplot() + scale_y_log10()

## Difference in average cost of gas/electricity bill compared by borough
gaselec %>% 
  ggplot(aes(x = factor(Borough), y = `Combined gas and electric`)) + geom_boxplot() + scale_y_log10()



##### Separation into sub-boroughs
water %>% filter(!(`Yearly cost (water sewer)` %in% c(998,999)), Borough == 1) %>%
  ggplot(aes(x = factor(`Sub-Borough Area`), y = `Yearly cost (water sewer)`)) + geom_boxplot()

water %>% filter(!(`Yearly cost (water sewer)` %in% c(998,999))) %>%
  count(Borough, `Sub-Borough Area`) %>% View()
