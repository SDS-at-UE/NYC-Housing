years <- c(1991,1993,1996,1999,2002,2005,2008,2011,2014,2017)
dta <- list()
for (i in 1:10) {
  name <- str_c("DATA/NYCHVS ", years[i]," Occupied File for ASA Challenge.csv")
  dta[[i]] <- read_csv(name, skip = 1)
}

NYC <- dta[[1]] %>% select(`Reason for Moving`, `Mortgage Status`, `Year Identifier`) 
for (i in 2:10) {
  dta[[i]] %>% select(`Reason for Moving`, `Mortgage Status`, `Year Identifier`) %>%
    bind_rows(NYC) -> 
    NYC
}
