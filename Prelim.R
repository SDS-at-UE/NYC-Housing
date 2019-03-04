library(tidyverse)
NYCHVS_1991_Occupied_File_for_ASA_Challenge <- read_csv("NYCHVS 1991 Occupied File for ASA Challenge.csv", 
                                                        skip = 1)
samp <- sample_n(NYCHVS_1991_Occupied_File_for_ASA_Challenge, 1000)

glimpse(NYCHVS_1991_Occupied_File_for_ASA_Challenge)


# Nothing
NYCHVS_1991_Occupied_File_for_ASA_Challenge %>% ggplot(aes(x = `Reason for Moving`)) + geom_histogram()


NYCHVS_2017_Occupied_File_for_ASA_Challenge <- read_csv("NYCHVS 2017 Occupied File for ASA Challenge.csv", 
                                                        skip = 1)

# People live in one place for a long time
NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% ggplot(aes(x = `Reason for Moving`)) + geom_histogram()

# More females than male tenants
NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% ggplot(aes(x = `Householder's Sex`)) + geom_histogram()
                        
# A lot of non applicable             
NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% ggplot(aes(x = `Mortgage Status`)) + geom_histogram()


# Owners less likely to live in the apartment building
NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% ggplot(aes(x = `Owner in building`)) + geom_histogram()

# Quality of housing for people who rent vs. own

Own <- NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% filter(`Mortgage Status` == 1 | `Mortgage Status` == 2)
#Own %>% select(`Mortgage Status`) %>% View()

Rent <- NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% filter(`Mortgage Status` == 9)
 # More people rent vs own

# Surprising: Owner's have more breakdowns of heating equipment than renters do
# Conditions: 1=Dilapidated, 2=Sound, 3=Deteriorating, 8=Not reported
# First Occupant: 1=Yes, first occupants, 2=No, previously occupied, 3=Don't know, 8=Not reported

Rent %>% group_by(`Condition of building`, `First Occupants of Unit`) %>% summarise(heat_break = sum(`Heating equipment breakdown` == 0), total = n(), ratio = heat_break/total) %>% ggplot(aes(x = `Condition of building`, y = ratio)) + geom_point(aes(color = factor(`First Occupants of Unit`)))
Own %>% group_by(`Condition of building`, `First Occupants of Unit`) %>% summarise(heat_break = sum(`Heating equipment breakdown`== 0), total = n(), ratio = heat_break/total)  %>% ggplot(aes(x = `Condition of building`, y = ratio)) + geom_point(aes(size = factor(`First Occupants of Unit`)))

Rent %>% group_by(`Condition of building`) %>% summarise(toilet_break = sum(`Toilet breakdowns` == 1), total = n(), ratio = toilet_break/total)
Own %>% group_by(`Condition of building`) %>% summarise(toilet_break = sum(`Toilet breakdowns` == 1), total = n(), ratio = toilet_break/total)

# On average people own at higher ages
Rent %>% summarise(average = mean(`Householder's Age Recode`))
Own %>% summarise(average = mean(`Householder's Age Recode`))

names(NYCHVS_2017_Occupied_File_for_ASA_Challenge)[3] <- "Slope_walls"
NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% ggplot(aes(x = `Mortgage Status`)) + geom_bar(aes(fill = factor(Slope_walls)))

NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% ggplot(aes(x = `Mortgage Status`)) + geom_bar(aes(fill = factor(Slope_walls)))


# Owner/Renter Status across boroughs
NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% ggplot(aes(x = `Tenure 1`)) + geom_bar(aes(fill = factor(Borough)))
NYCHVS_2017_Occupied_File_for_ASA_Challenge  %>% group_by(Borough) %>% summarise(total = n(), 
                                total_own = sum(`Tenure 1` == 1), total_rent = sum(`Tenure 1` == 9),
                                Own = total_own/total, Rent = total_rent/total) %>% 
  select(-starts_with("total")) %>%
  gather(key = "type", value = "percent", Own, Rent) %>%
  ggplot(aes(x = Borough, y = percent)) + geom_bar(aes(fill = type), stat = "identity") + geom_hline(yintercept = 2/3, linetype = 2, size = 2)
# Staten Island is least populated out of the 5 Boroughs. The Demand in the housing market is therefore less
# Resulting in lower house costs



# Combining data sets

years <- c(1991,1993,1996,1999,2002,2005,2008,2011,2014,2017)
dta <- list()
for (i in 1:10) {
  name <- str_c("DATA/NYCHVS ", years[i]," Occupied File for ASA Challenge.csv")
  dta[[i]] <- read_csv(name, skip = 1)
}

NYC <- dta[[1]] %>% select(`Reason for Moving`, `Mortgage Status`, 
                           `Year Identifier`, `Tenure 1`, 
                           `Condition of building`, 
                           `Heating equipment breakdown`, 
                           `Toilet breakdowns`, `First Occupants of Unit`) 
for (i in 2:10) {
  dta[[i]] %>% select(`Reason for Moving`, `Mortgage Status`, 
                      `Year Identifier`, `Tenure 1`, 
                      `Condition of building`, 
                      `Heating equipment breakdown`, 
                      `Toilet breakdowns`, `First Occupants of Unit`, 
                      `Number of rooms`, `Number of bedrooms`, 
                      `Presence of mice or rats`,
                      Borough, `Borough and Sub-Borough Area`) %>%
    bind_rows(NYC) -> 
    NYC
}



NYC %>% group_by(Borough) %>% summarise(total = n(),
       total_own = sum(`Tenure 1` == 1), total_rent = sum(`Tenure 1` == 9),
       Own = total_own/total, Rent = total_rent/total) %>% 
  select(-starts_with("total")) %>%
  gather(key = "type", value = "percent", Own, Rent) %>%
  ggplot(aes(x = Borough, y = percent)) + geom_bar(aes(fill = type), stat = "identity") + geom_hline(yintercept = 2/3, linetype = 2, size = 2)
