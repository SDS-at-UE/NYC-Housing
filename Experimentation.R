library(readr)
library(tidyverse)
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

hist(gaselec$`Combined gas and electric`)
