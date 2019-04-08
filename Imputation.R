library(tidyverse)
library(ggplot2)

# Reading in all data
years <- c(1991,1993,1996,1999,2002,2005,2008,2011,2014,2017)
dta <- list()
for (i in 1:10) {
  name <- str_c("DATA/NYCHVS ", years[i]," Occupied File for ASA Challenge.csv")
  dta[[i]] <- read_csv(name, skip = 1)
  dta[[i]] <- dta[[i]] #%>% mutate_all(., funs(factor(.)))
}

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

# Comouting score
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

# By year
by_year <- external_imputed %>% group_by(`Year Identifier`, `Tenure 1`) %>% summarise(Score = mean(score))
by_year <- by_year %>% mutate(Score1 = format(Score, digits = 3))
ggplot(by_year) + 
  geom_bar(aes(x = `Year Identifier`, y = Score, fill = `Tenure 1`),stat="identity",position="dodge") + 
  geom_point(aes(x = `Year Identifier`, y = Score,shape=`Tenure 1`), position = position_dodge(width = 0.9)) + 
  geom_line(aes(x = `Year Identifier`, y = Score,group = `Tenure 1`), position = position_dodge(width = 0.9)) + 
  geom_text(aes(label=Score1, x = `Year Identifier`, y = Score, vjust=-1.5))+
  ylim(0.0, 1.5) 
  

# By borough
<<<<<<< HEAD
by_borough <- external_imputed %>% group_by(Borough,`Tenure 1`) %>% summarise(Score = mean(score))
by_borough <- by_borough %>% mutate(Score1 = format(Score, digits = 3))
ggplot(by_borough,aes(x = Borough, y = Score, fill = `Tenure 1`)) + 
  geom_bar(stat="identity",position="dodge") + 
  geom_text(aes(label=Score1, x = Borough, y = Score, vjust=-1.5), position = position_dodge(width = 0.9)) +
  ylim(0.0,1.5)




=======
by_borough <- external_imputed %>% group_by(Borough) %>% summarise(Score = mean(score))
ggplot(by_borough,aes(x = Borough, y = Score, group = 0)) + geom_point() + geom_line()
>>>>>>> 7c99bd92ba90904d4873ee3e05f5cdc398a10486
