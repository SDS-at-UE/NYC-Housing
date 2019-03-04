library(readr)
library(tidyverse)
set.seed(6969)

years <- c(1991,1993,1996,1999,2002,2005,2008,2011,2014,2017)
dta <- list()
for (i in 1:10) {
  name <- str_c("DATA/NYCHVS ", years[i]," Occupied File for ASA Challenge.csv")
  dta[[i]] <- read_csv(name, skip = 1)
}

### full_data <- bind_rows(dta[[1]], 
#                       dta[[2]],
#                       dta[[3]],
#                       dta[[4]],
#                       dta[[5]],
#                       dta[[6]],
#                       dta[[7]],
#                       dta[[8]],
#                       dta[[9]],
#                       dta[[10]]
#                      )

NYC <- dta[[1]] %>% 
  select(`Borough`, `Electricity paid separately`, `Gas paid separately`,
         `Monthly cost (gas)`, `Combined gas and electric`, 
         `Water and sewer paid separately`, `Yearly cost (water sewer)`, 
         `Sub-Borough Area`, `Borough and Sub-Borough Area`,
         `Year Identifier`, `Number of rooms`) 
for (i in 2:10) {
  dta[[i]] %>% 
    select(`Borough`, `Electricity paid separately`, `Gas paid separately`,
           `Monthly cost (gas)`, `Combined gas and electric`, 
           `Water and sewer paid separately`, `Yearly cost (water sewer)`, 
           `Sub-Borough Area`, `Borough and Sub-Borough Area`,
           `Year Identifier`, `Number of rooms`) %>%
    bind_rows(NYC) -> 
    NYC
}

NYC <- NYC %>% mutate(Borough = ifelse(Borough == 1, "Bronx", Borough))
NYC <- NYC %>% mutate(Borough = ifelse(Borough == 2, "Brooklyn", Borough))
NYC <- NYC %>% mutate(Borough = ifelse(Borough == 3, "Manhattan", Borough))
NYC <- NYC %>% mutate(Borough = ifelse(Borough == 4, "Queens", Borough))
NYC <- NYC %>% mutate(Borough = ifelse(Borough == 5, "Staten Island", Borough))

NYC %>% ggplot(aes(x = `Number of rooms`, fill = Borough)) + geom_bar()
total <- NYC %>% count(Borough, `Number of rooms`) 
total %>% ggplot(aes(fill = `Number of rooms`, y = percent, x = Borough)) + 
  geom_bar(stat = "identity") + 
  scale_fill_gradientn(colors = c("pink", "red", "purple", "blue", "black"))


for (i in 1:10) {
  dta[[i]] <- dta[[i]] %>% 
    mutate(Borough = ifelse(Borough == 1, "Bronx", Borough),
           Borough = ifelse(Borough == 2, "Brooklyn", Borough),
           Borough = ifelse(Borough == 3, "Manhattan", Borough),
           Borough = ifelse(Borough == 4, "Queens", Borough),
           Borough = ifelse(Borough == 5, "Staten Island", Borough))
}


## Electricity
elec91 <-dta[[1]] %>% 
  filter(`Electricity paid separately` == 3, 
         !(`Monthly cost (electric)` %in% c(998, 999))) 

elec93 <-dta[[2]] %>% 
  filter(`Electricity paid separately` == 3, 
         !(`Monthly cost (electric)` %in% c(998, 999))) 

elec93 %>% 
  ggplot(aes(x = factor(Borough), y = `Monthly cost (electric)`)) + geom_boxplot() 


## Gas
#### 1991
gas91 <-dta[[1]] %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999, 9999))) 

gas91 %>% 
  ggplot(aes(x = `Monthly cost (gas)`)) + geom_histogram(binwidth = 10) + 
    coord_cartesian(xlim = c(0, 200))

## Difference in average cost of gas bill compared by borough
gas91  %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas)")
### Manhattan is the lowest on average, but lots of big outliers

#### 1993
gas93 <-dta[[2]] %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999, 9999))) 

gas93 %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas)")
### Manhattan and the Bronx are lowest, both have lots of high outliers

#### 1996
gas96 <- dta[[3]] %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999, 9999))) 

gas96 %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas)")
### Manhattan the lowest, Bronx and Manhattan high outliers
### Staten Island highest, outliers on both sides

#### 1999
gas99 <-dta[[4]] %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999, 9999))) 

gas99 %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas)")
### Similar to 1996

#### 2002
gas02 <-dta[[5]] %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999, 9999))) 

gas02 %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas)")
### Similar

#### 2005
gas05 <-dta[[6]] %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999, 9999))) 

gas05 %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas)")
### Similar

#### 2008
gas08 <-dta[[7]] %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999, 9999))) 

gas08 %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas)")
### Similar

#### 2011
gas11 <-dta[[8]] %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999, 9999))) 

gas11 %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas)")
### Similar

#### 2014
gas14 <-dta[[9]] %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999, 9999))) 

gas14 %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas)")
### Similar

#### 2017
gas17 <-dta[[10]] %>% 
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999, 9999))) 

gas17 %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas)")
### Similar

#### Entire Data Set
NYC %>%
  filter(`Gas paid separately`== 1, !(`Monthly cost (gas)` %in% c(998, 999, 9999))) %>%
  ggplot(aes(x = factor(Borough), y = `Monthly cost (gas)`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas)")



## Gas/electric
#### 1991
gaselec91 <- dta[[1]] %>% 
  filter(!(`Combined gas and electric` %in% c(998, 999)))
### Similar, but Manhattan's average increased relative to other boroughs

## Difference in average cost of gas/electricity bill compared by borough
gaselec91 %>% 
  ggplot(aes(x = factor(Borough), y = `Combined gas and electric`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas & Electric)")
### Staten Island most expensive by far, other 4 relatively equal
### high outliers for Bronx, high and low outliers for Manhattan


#### 1993
gaselec93 <- dta[[2]] %>% 
  filter(!(`Combined gas and electric` %in% c(998, 999)))

gaselec93 %>% 
  ggplot(aes(x = factor(Borough), y = `Combined gas and electric`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas & Electric)")
### Staten Island still most expensive barely
### Manhattan still with outliers on both sides
### All relatively close


#### 1996
gaselec96 <- dta[[3]] %>% 
  filter(!(`Combined gas and electric` %in% c(998, 999, 9999)))

gaselec96 %>% 
  ggplot(aes(x = factor(Borough), y = `Combined gas and electric`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas & Electric)")
### All relatively close, some outliers for each

#### 1999
gaselec99 <- dta[[4]] %>% 
  filter(!(`Combined gas and electric` %in% c(998, 999, 9999)))

gaselec99 %>% 
  ggplot(aes(x = factor(Borough), y = `Combined gas and electric`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas & Electric)")
### Similar

#### 2002
gaselec02 <- dta[[5]] %>% 
  filter(!(`Combined gas and electric` %in% c(998, 999, 9999)))

gaselec02 %>% 
  ggplot(aes(x = factor(Borough), y = `Combined gas and electric`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas & Electric)")
### Staten Island and Brooklyn average almost the same
### Manhattan still with weird outliers

#### 2005
gaselec05 <- dta[[6]] %>% 
  filter(!(`Combined gas and electric` %in% c(998, 999, 9999)))

gaselec05 %>% 
  ggplot(aes(x = factor(Borough), y = `Combined gas and electric`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas & Electric)")
### Similar story

#### 2008
gaselec08 <- dta[[7]] %>% 
  filter(!(`Combined gas and electric` %in% c(998, 999, 9999)))

gaselec08 %>% 
  ggplot(aes(x = factor(Borough), y = `Combined gas and electric`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas & Electric)")
### Similar story, Staten Island's spread becomes huge for some reason

#### 2011
gaselec11 <- dta[[8]] %>% 
  filter(!(`Combined gas and electric` %in% c(998, 999, 9999)))

gaselec11 %>% 
  ggplot(aes(x = factor(Borough), y = `Combined gas and electric`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas & Electric)")
### Queens jumps into second, Manhattan with a ton of outliers

#### 2014
gaselec14 <- dta[[9]] %>% 
  filter(!(`Combined gas and electric` %in% c(998, 999, 9999)))

gaselec14 %>% 
  ggplot(aes(x = factor(Borough), y = `Combined gas and electric`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas & Electric)")
### Similar

#### 2017
gaselec17 <- dta[[10]] %>% 
  filter(!(`Combined gas and electric` %in% c(998, 999, 9999)))

gaselec17 %>% 
  ggplot(aes(x = factor(Borough), y = `Combined gas and electric`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas & Electric)")
### Similar

#### Whole data set
NYC %>%
  filter(!(`Combined gas and electric` %in% c(998, 999, 9999))) %>%
  ggplot(aes(x = factor(Borough), y = `Combined gas and electric`)) + 
  geom_boxplot() + scale_y_log10() +
  xlab("Boroughs") + ylab("Monthly Cost (Gas & Electric)")



## Water
water91 <-dta[[1]] %>% 
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
water93 <-dta[[2]] %>% 
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






Own <- NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% 
  filter(`Mortgage Status` == 1 | `Mortgage Status` == 2)