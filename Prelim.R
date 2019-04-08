library(tidyverse)
NYCHVS_1991_Occupied_File_for_ASA_Challenge <- read_csv("NYCHVS 1991 Occupied File for ASA Challenge.csv", 
                                                        skip = 1)
samp <- sample_n(NYCHVS_1991_Occupied_File_for_ASA_Challenge, 1000)

glimpse(NYCHVS_1991_Occupied_File_for_ASA_Challenge)


# Nothing
NYCHVS_1991_Occupied_File_for_ASA_Challenge %>% ggplot(aes(x = `Reason for Moving`)) + geom_histogram()


NYCHVS_2017_Occupied_File_for_ASA_Challenge <- read_csv("DATA/NYCHVS 2017 Occupied File for ASA Challenge.csv", 
                                                        skip = 1) %>% View()

NYCHVS_2017_Occupied_File_for_ASA_Challenge <- NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% mutate(Borough = ifelse(Borough == 1, "Bronx", Borough))
NYCHVS_2017_Occupied_File_for_ASA_Challenge <- NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% mutate(Borough = ifelse(Borough == 2, "Brooklyn", Borough))
NYCHVS_2017_Occupied_File_for_ASA_Challenge <- NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% mutate(Borough = ifelse(Borough == 3, "Manhattan", Borough))
NYCHVS_2017_Occupied_File_for_ASA_Challenge <- NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% mutate(Borough = ifelse(Borough == 4, "Queens", Borough))
NYCHVS_2017_Occupied_File_for_ASA_Challenge <- NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% mutate(Borough = ifelse(Borough == 5, "Staten Island", Borough))


# People live in one place for a long time
NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% ggplot(aes(x = `Reason for Moving`)) + geom_histogram()

# More females than male tenants
NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% ggplot(aes(x = `Householder's Sex`)) + geom_histogram()
                        
# A lot of non applicable             
NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% ggplot(aes(x = `Mortgage Status`)) + geom_histogram()


# Owners less likely to live in the apartment building
NYCHVS_2017_Occupied_File_for_ASA_Challenge %>% ggplot(aes(x = `Owner in building`)) + geom_histogram()

library(tidyverse)
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



# Quality of housing for people who rent vs. own

Own <- NYC %>% filter(`Tenure 1` == 1)
#Own %>% select(`Mortgage Status`) %>% View()

Rent <- NYC %>% filter(`Tenure 1` == 9)
 # More people rent vs own

# Surprising: Owner's have more breakdowns of heating equipment than renters do
# Conditions: 1=Dilapidated, 2=Sound, 3=Deteriorating, 8=Not reported
# First Occupant: 1=Yes, first occupants, 2=No, previously occupied, 3=Don't know, 8=Not reported

Rent %>% group_by(`Condition of building`, `First Occupants of Unit`) %>% summarise(heat_break = sum(`Heating equipment breakdown` == 0), total = n(), ratio = heat_break/total) %>% ggplot(aes(x = `Condition of building`, y = ratio)) + geom_point(aes(color = factor(`First Occupants of Unit`)))
Own %>% group_by(`Condition of building`, `First Occupants of Unit`) %>% summarise(heat_break = sum(`Heating equipment breakdown`== 0), total = n(), ratio = heat_break/total)  %>% ggplot(aes(x = `Condition of building`, y = ratio)) + geom_point(aes(size = factor(`First Occupants of Unit`)))

Rent %>% group_by(`Condition of building`, `First Occupants of Unit`) %>% 
  summarise(heat_break = sum(`Heating equipment breakdown` == 0), total = n(), 
            ratio = heat_break/total) %>% ggplot(aes(x = `Condition of building`, 
                                                     y = ratio)) + 
  geom_point(aes(color = factor(`First Occupants of Unit`)))

Own %>% group_by(`Condition of building`, `First Occupants of Unit`) %>% 
  summarise(heat_break = sum(`Heating equipment breakdown`== 0), total = n(), 
            ratio = heat_break/total)  %>% ggplot(aes(x = `Condition of building`, 
                                                      y = ratio)) + 
  geom_point(aes(size = factor(`First Occupants of Unit`)))


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
                           `Toilet breakdowns`, `First Occupants of Unit`, 
                           `Number of rooms`, `Number of bedrooms`, 
                           `Presence of mice or rats`,
                           Borough, `Borough and Sub-Borough Area`) 
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

NYC <- NYC %>% mutate(Borough = ifelse(Borough == 1, "Bronx", Borough))
NYC <- NYC %>% mutate(Borough = ifelse(Borough == 2, "Brooklyn", Borough))
NYC <- NYC %>% mutate(Borough = ifelse(Borough == 3, "Manhattan", Borough))
NYC <- NYC %>% mutate(Borough = ifelse(Borough == 4, "Queens", Borough))
NYC <- NYC %>% mutate(Borough = ifelse(Borough == 5, "Staten Island", Borough))

NYC <- NYC %>% mutate(`First Occupants of Unit` = case_when(`First Occupants of Unit` == 1 ~ "Yes",
                                                            `First Occupants of Unit` == 2 ~ "No",
                                                            `First Occupants of Unit` == 3 ~ "Don't Know",
                                                            `First Occupants of Unit` == 8 ~ "Not Reported"))
NYC <- NYC %>% mutate(`Tenure 1` = case_when(`Tenure 1` == 1 ~ "Owned",
                                             `Tenure 1` == 9 ~ "Rented"))



NYC[[3]] <- case_when(NYC[[3]] == 91 ~ 1991,
                       NYC[[3]] == 93 ~ 1993,
                       NYC[[3]] == 96 ~ 1996,
                       NYC[[3]] == 99 ~ 1999,
                                    TRUE ~ as.double(NYC[[3]]))
NYC[[3]] <- factor(NYC[[3]])

# Renaming borough
NYC[[12]] <- case_when(NYC[[12]] == 1 ~ "Bronx",
                       NYC[[12]] == 2 ~ "Brooklyn",
                       NYC[[12]] == 3 ~ "Manhattan",
                       NYC[[12]] == 4 ~ "Queens",
                       NYC[[12]] == 5 ~ "Staten Island")



# Borough Breakdown RENT VS OWN
NYC %>% group_by(Borough) %>% summarise(total = n(),
       total_own = sum(`Tenure 1` == "Owned"), total_rent = sum(`Tenure 1` == "Rented"),
       Own = total_own/total, Rent = total_rent/total) %>% 
  select(-starts_with("total")) %>%
  gather(key = "type", value = "percent", Own, Rent) %>%
  ggplot(aes(x = Borough, y = percent)) + geom_bar(aes(fill = type), stat = "identity") + geom_hline(yintercept = 2/3, linetype = 2, size = 2)

NYC  %>% group_by(`Year Identifier`) %>% summarise(total = n(),
      total_own = sum(`Tenure 1` == "Owned"), total_rent = sum(`Tenure 1` == "Rented"),
      Own = total_own/total, Rent = total_rent/total) %>% 
  select(-starts_with("total")) %>%
  gather(key = "type", value = "percent", Own, Rent) %>%
  ggplot(aes(x = `Year Identifier`, y = percent)) + geom_bar(aes(fill = type),
         stat = "identity", position = position_dodge(width = 1)) + geom_hline(yintercept = 2/3, linetype = 2, size = 2) +
  labs(title = "Own VS. Rent", x = "Years", y = "Percent", legend = "Type") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.text = element_text(size = 15), legend.title = element_text(size = 15),
        title = element_text(size = 20))





# Heating Equipment Breakdowns
NYC %>% group_by(`Tenure 1`, `First Occupants of Unit`) %>% 
  summarise(heat_break = sum(`Heating equipment breakdown` == 0), total = n(), 
            ratio = heat_break/total)


NYC %>% group_by(`Tenure 1`, `First Occupants of Unit`) %>% 
  summarise(heat_break = sum(`Heating equipment breakdown` == 0), total = n(), 
            ratio = heat_break/total) %>% ggplot(aes(x = `Tenure 1`, 
                                                     y = ratio)) + 
  geom_point(aes(color = `First Occupants of Unit`, size = total)) + 
  ggtitle("Heating Equipment Breakdowns")

# Toilet Breakdowns
NYC %>% group_by(`Tenure 1`, `First Occupants of Unit`) %>% 
  summarise(toilet_break = sum(`Toilet breakdowns` == 1), total = n(), 
            ratio = toilet_break/total)


NYC %>% group_by(`Tenure 1`, `First Occupants of Unit`) %>% 
  summarise(toilet_break = sum(`Toilet breakdowns` == 1), total = n(), 
            ratio = toilet_break/total) %>% ggplot(aes(x = `Tenure 1`, 
                                                       y = ratio)) + 
  geom_point(aes(color = `First Occupants of Unit`, size = total)) + 
  ggtitle("Toilet Breakdowns")



# Number of Rooms
NYC %>% group_by(`Tenure 1`, Borough) %>% 
  summarise(number_rooms = mean(`Number of rooms`), total = n()) -> Rooms

library(reshape)
combined <- unite(Rooms, "Tenure Region", c("Tenure 1", "Borough"))

library(ggfittext)
combined %>% ggplot(aes(x = `Tenure Region`, y = number_rooms)) + geom_bar(stat = "identity", fill = "blue") + 
  theme(axis.text.x = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"), 
        axis.title.y = element_text(color = "grey20", size = 15))


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
NYC %>% select(`Toilet breakdowns`) %>% summarise(missing = sum(`Toilet breakdowns` == 8), total = n(), pct_missing = missing/total)
NYC %>% select(`Heating equipment breakdown`) %>% summarise(missing = sum(`Heating equipment breakdown` == 8), total = n(), pct_missing = missing/total)
names(NYC)
NYC %>% select(`Out of pocket rent`) %>% summarise(missing = sum(`Out of pocket rent` == 99998), total = n(), pct_mising = missing/total)
NYC %>% select(`Out of pocket rent`) %>% View()
NYC %>% select(`Out of pocket rent`, `Year Identifier`) %>% group_by(`Year Identifier`) %>% summarise(missing = sum(is.na(`Out of pocket rent`)))
NYC %>% dplyr::filter(starts_with("Y"))
              
names(dta[[10]]) -> nms
vars_select(nms, starts_with("Floor"))
nms <- names(NYC)                                     
vars_select(nms, starts_with("Y"))
NYC %>% count(`Year Identifier`)


NYC %>% select(`Toilet breakdowns`, `Year Identifier`) %>% group_by(`Year Identifier`) %>%
  summarise(missing = sum(`Toilet breakdowns` == 8),
            total = n(), missing_pct = missing/total)

NYC %>% select(`Heating equipment breakdown`, `Year Identifier`) %>% group_by(`Year Identifier`) %>%
  summarise(missing = sum(`Heating equipment breakdown` == 8),
            total = n(), missing_pct = missing/total)

NYC %>% select(`Out of pocket rent`, `Year Identifier`) %>% group_by(`Year Identifier`) %>% summarise(missing = sum(is.na(`Out of pocket rent`)))
NYC %>% select(`Monthly contract rent`, `Year Identifier`) %>% group_by(`Year Identifier`) %>% summarise(missing = sum(is.na(`Monthly contract rent`)))
NYC %>% select(`Monthly contract rent`, `Year Identifier`) %>% group_by(`Year Identifier`) %>% summarise(missing = sum(`Monthly contract rent` == 99999),
                                                                                                         total = n(),
                                                                                             missing_pct = missing/total)
NYC %>% filter(`Tenure 1` == 1) -> Owned
NYC %>% filter(`Tenure 1` == 9) -> Rented


## Rented
Rented %>% select(`Toilet breakdowns`, `Year Identifier`) %>% group_by(`Year Identifier`) %>%
  summarise(missing = sum(`Toilet breakdowns` == 8),
            total = n(), missing_pct = missing/total)

Rented %>% select(`Heating equipment breakdown`, `Year Identifier`) %>% group_by(`Year Identifier`) %>%
  summarise(missing = sum(`Heating equipment breakdown` == 8),
            total = n(), missing_pct = missing/total)

Rented %>% select(`Monthly contract rent`, `Year Identifier`) %>% 
  group_by(`Year Identifier`) %>% 
  summarise(missing = sum(`Monthly contract rent` == 99999),
            total = n(),
            missing_pct = missing/total)

Rented %>% select(`Complete plumbing facilities`, `Year Identifier`) %>% 
  group_by(`Year Identifier`) %>% 
  summarise(missing = sum(is.na(`Complete plumbing facilities`)),
            total = n(),
            missing_pct = missing/total)

Rent %>% select(`Complete plumbing facilities`, `Year Identifier`) %>% View()
## Owned
Owned %>% select(`Toilet breakdowns`, `Year Identifier`) %>% group_by(`Year Identifier`) %>%
  summarise(missing = sum(`Toilet breakdowns` == 8),
            total = n(), missing_pct = missing/total)

Owned %>% select(`Heating equipment breakdown`, `Year Identifier`) %>% group_by(`Year Identifier`) %>%
  summarise(missing = sum(`Heating equipment breakdown` == 8),
            total = n(), missing_pct = missing/total)

Owned %>% select(Value, `Year Identifier`) %>% 
  group_by(`Year Identifier`) %>% 
  summarise(missing = sum(Value == 9999998 | Value == 9999999),
            total = n(),
            missing_pct = missing/total)
