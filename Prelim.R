library(tidyverse)
NYCHVS_1991_Occupied_File_for_ASA_Challenge <- read_csv("NYCHVS 1991 Occupied File for ASA Challenge.csv", 
                                                        skip = 1)
samp <- sample_n(NYCHVS_1991_Occupied_File_for_ASA_Challenge, 1000)

glimpse(NYCHVS_1991_Occupied_File_for_ASA_Challenge)
