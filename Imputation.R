library(tidyverse)
library(ggplot2)
library(ltm)
library(sf)
library(tmap)
library(raster)
library(maptools)

# Reading in all data
years <- c(1991,1993,1996,1999,2002,2005,2008,2011,2014,2017)
dta <- list()
for (i in 1:10) {
  name <- str_c("DATA/NYCHVS ", years[i]," Occupied File for ASA Challenge.csv")
  dta[[i]] <- read_csv(name, skip = 1)
  dta[[i]] <- dta[[i]] #%>% mutate_all(., funs(factor(.)))
}

# Select external variables
external <- dta[[1]] %>% select(contains("Window"),contains("Exterior Walls"),contains("Stairways"),contains("Condition of building"),contains("Number of Units"),contains("Stories"),contains("identifier"),Borough,`Tenure 1`,`Borough and Sub-Borough Area`) 
for (i in 2:10) {
  dta[[i]] %>% select(contains("Window"),contains("Exterior Walls"),contains("Stairways"),contains("Condition of building"),contains("Number of Units"),contains("Stories"),contains("identifier"),Borough,`Tenure 1`,`Borough and Sub-Borough Area`) %>%
    bind_rows(external) -> 
    external
}

# Clean & formating
external <- external %>% select(-`Condition of Stairways (Exterior and Interior): No interior steps or stairways`,-`Condition of Stairways (Exterior and Interior): No exterior steps or stairways`,-`Condition of Stairways (Exterior and Interior): No stairways`)
external$`Number of Units in Building` <- factor(external$`Number of Units in Building`)
external$`Stories in building` <- factor(external$`Stories in building`)
external <- external[,-c(7,23,24)]

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

# Computing score
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
by_borough <- external_imputed %>% group_by(Borough,`Tenure 1`) %>% summarise(Score = mean(score))
by_borough <- by_borough %>% mutate(Score1 = format(Score, digits = 3))
ggplot(by_borough,aes(x = Borough, y = Score, fill = `Tenure 1`)) + 
  geom_bar(stat="identity",position="dodge") + 
  geom_text(aes(label=Score1, x = Borough, y = Score, vjust=-1.5), position = position_dodge(width = 0.9)) +
  ylim(0.0,1.5)

# IRT
external_a <- external_imputed[,c(1:3,7,8,11,12)]
mod_rasch <- rasch(external_a,start.val = "random")
summary(mod_rasch)

mod_2pl <- ltm(external_a ~ z1)
summary(mod_2pl)

theta.rasch <- ltm::factor.scores(mod_2pl)

irt <- theta.rasch$score.dat 
irt <- irt %>% mutate(Total = irt[[1]] + irt[[2]] + irt[[3]] + irt[[4]] + irt[[5]] + irt[[6]] + irt[[7]]) 
ggplot(irt, aes(x = z1 , y = Total)) + geom_point()

irt %>% filter(Total ==1)
  # 1. Rotton/loose windows  1.659920
  # 2. Major cracks in outside walls  1.598761
  # 3. Broken or missing windows 1.491062
  # 4. Loose, broken, or missing steps 1.464491
  # 5. Loose, broken, or missing stair 1.429216
  # 6. Boarded up windows 1.426477
  # 7. Loose or hanging cornice, roofing, or other materia 1.331211
for (i in 1:7) {
  a <- c(1.659920, 1.598761, 1.491062, 1.464491, 1.429216, 1.426477, 1.331211)
  external_a[[i]] <- ifelse(external_a[[i]] == 0, 0, a[i])
}


external_a <- external_a %>% mutate(Score = rowSums(.))
external_b <- cbind(external_a,external_imputed) 
external_b <- external_b[,c(8,29)]
sub <- external_b %>% group_by(`Borough and Sub-Borough Area`) %>% summarise(ex = mean(Score))

