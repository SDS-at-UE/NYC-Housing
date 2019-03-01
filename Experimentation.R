library(readr)
library(tidyverse)
set.seed(6969)
NYCHVS_1991_Occupied_File_for_ASA_Challenge <- read_csv("DATA/NYCHVS 1991 Occupied File for ASA Challenge.csv", skip = 1)
View(NYCHVS_1991_Occupied_File_for_ASA_Challenge)

NYCHVS_1993_Occupied_File_for_ASA_Challenge <- read_csv("DATA/NYCHVS 1993 Occupied File for ASA Challenge.csv", skip = 1)
NYCHVS_1996_Occupied_File_for_ASA_Challenge <- read_csv("DATA/NYCHVS 1996 Occupied File for ASA Challenge.csv", skip = 1)
NYCHVS_1999_Occupied_File_for_ASA_Challenge <- read_csv("DATA/NYCHVS 1999 Occupied File for ASA Challenge.csv", skip = 1)
NYCHVS_2002_Occupied_File_for_ASA_Challenge <- read_csv("DATA/NYCHVS 2002 Occupied File for ASA Challenge.csv", skip = 1)
NYCHVS_2005_Occupied_File_for_ASA_Challenge <- read_csv("DATA/NYCHVS 2005 Occupied File for ASA Challenge.csv", skip = 1)
NYCHVS_2008_Occupied_File_for_ASA_Challenge <- read_csv("DATA/NYCHVS 2008 Occupied File for ASA Challenge.csv", skip = 1)
NYCHVS_2011_Occupied_File_for_ASA_Challenge <- read_csv("DATA/NYCHVS 2011 Occupied File for ASA Challenge.csv", skip = 1)
NYCHVS_2014_Occupied_File_for_ASA_Challenge <- read_csv("DATA/NYCHVS 2014 Occupied File for ASA Challenge.csv", skip = 1)
NYCHVS_2017_Occupied_File_for_ASA_Challenge <- read_csv("DATA/NYCHVS 2017 Occupied File for ASA Challenge.csv", skip = 1)

samp <- sample_frac(NYCHVS_1991_Occupied_File_for_ASA_Challenge, .25)

## Electricity
elec91 <-NYCHVS_1991_Occupied_File_for_ASA_Challenge %>% 
  filter(`Electricity paid separately` == 3, !(`Monthly cost (electric)` %in% c(998, 999))) 

elec93 <-NYCHVS_1993_Occupied_File_for_ASA_Challenge %>% 
  filter(`Electricity paid separately` == 3, !(`Monthly cost (electric)` %in% c(998, 999))) 

elec93 %>% 
  ggplot(aes(x = factor(Borough), y = `Monthly cost (electric)`)) + geom_boxplot() 


## Gas
#### 1991
gas91 <-NYCHVS_1991_Occupied_File_for_ASA_Challenge %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999))) 

gas %>% 
  ggplot(aes(x = `Monthly cost (gas)`)) +   geom_histogram(binwidth = 10) + 
    coord_cartesian(xlim = c(0, 200))

## Difference in average cost of gas bill compared by borough
gas91  %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10()
### Manhattan is the lowest on average, but lots of big outliers

#### 1993
gas93 <-NYCHVS_1993_Occupied_File_for_ASA_Challenge %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999))) 

gas93 %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10()
### Manhattan and the Bronx are lowest, both have lots of high outliers

#### 1996
gas96 <- NYCHVS_1996_Occupied_File_for_ASA_Challenge %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999))) 

gas96 %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10()
### Manhattan the lowest, Bronx and Manhattan high outliers
### Staten Island highest, outliers on both sides

#### 1999
gas99 <-NYCHVS_1999_Occupied_File_for_ASA_Challenge %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999))) 

gas99 %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10()
### Similar to 1996

#### 2002
gas02 <-NYCHVS_2002_Occupied_File_for_ASA_Challenge %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999))) 

gas02 %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10()
### Similar

#### 2005
gas05 <-NYCHVS_2005_Occupied_File_for_ASA_Challenge %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999))) 

gas05 %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10()
### Similar

#### 2008
gas08 <-NYCHVS_2008_Occupied_File_for_ASA_Challenge %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999))) 

gas08 %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10()
### Similar

#### 2011
gas11 <-NYCHVS_2011_Occupied_File_for_ASA_Challenge %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999))) 

gas11 %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10()
### Similar

#### 2014
gas14 <-NYCHVS_2014_Occupied_File_for_ASA_Challenge %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999))) 

gas14 %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10()
### Similar

#### 2017
gas17 <-NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999))) 

gas17 %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10()



## Gas/electric
#### 1991
gaselec91 <- NYCHVS_1991_Occupied_File_for_ASA_Challenge %>% 
  filter(!(`Combined gas and electric` %in% c(998, 999)))
### Similar, but Manhattan's average increased relative to other boroughs

## Difference in average cost of gas/electricity bill compared by borough
gaselec91 %>% 
  ggplot(aes(x = factor(Borough), y = `Combined gas and electric`)) + 
  geom_boxplot() + scale_y_log10()

#### 1993
gaselec93 <- NYCHVS_1993_Occupied_File_for_ASA_Challenge %>% 
  filter(!(`Combined gas and electric` %in% c(998, 999)))


gaselec93 %>% 
  ggplot(aes(x = factor(Borough), y = `Combined gas and electric`)) + 
  geom_boxplot() + scale_y_log10()


## Water
water91 <-NYCHVS_1991_Occupied_File_for_ASA_Challenge %>% 
  filter(`Water and sewer paid separately` == 1, !(`Yearly cost (water sewer)` %in% c(998,999))) 

## Distribution of water bill amount
water91 %>%
  ggplot(aes(x = `Yearly cost (water sewer)`)) + geom_histogram(binwidth = 50)

## Difference in average cost of water bill compared by borough
water91 %>%
  ggplot(aes(x = factor(Borough), y = `Yearly cost (water sewer)`)) + geom_boxplot()
### Queens has lots of outliers (#4)  

## Separation into sub-boroughs
water91 %>% filter(Borough == 1) %>%
  ggplot(aes(x = factor(`Sub-Borough Area`), y = `Yearly cost (water sewer)`)) + geom_boxplot()

water91 %>%
  count(Borough, `Sub-Borough Area`) %>% View()

#### 1993
water93 <-NYCHVS_1993_Occupied_File_for_ASA_Challenge %>% 
  filter(`Water and sewer paid separately` == 1, !(`Yearly cost (water sewer)` %in% c(998,999))) 

## Distribution of water bill amount
water93 %>%
  ggplot(aes(x = `Yearly cost (water sewer)`)) + geom_histogram(binwidth = 50)

## Difference in average cost of water bill compared by borough
water93 %>%
  ggplot(aes(x = factor(Borough), y = `Yearly cost (water sewer)`)) + geom_boxplot()

## Separation into sub-boroughs
water93 %>% filter(Borough == 1) %>%
  ggplot(aes(x = factor(`Sub-Borough Area`), y = `Yearly cost (water sewer)`)) + 
  geom_boxplot()

water93 %>%
  count(Borough, `Sub-Borough Area`) %>% View()