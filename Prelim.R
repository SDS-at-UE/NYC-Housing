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



