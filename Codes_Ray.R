library(readr)
library(tidyverse)
library(ggplot2)


dta1991 <- read.csv("DATA/NYCHVS 1991 Occupied File for ASA Challenge.csv", skip = 1)
dta1993 <- read.csv("DATA/NYCHVS 1993 Occupied File for ASA Challenge.csv", skip = 1)
dta <- read_csv("DATA/NYCHVS 2017 Occupied File for ASA Challenge.csv", skip = 1)

NYC <- dta[[1]] %>% select(`Householder's Sex`,`Mortgage Status`,Borough,`Householder's Age Recode`,`Monthly Gross Rent`,`Householder's Race`) 
for (i in 2:3) {
  dta[[i]] %>% select(`Householder's Sex`,`Mortgage Status`,Borough,`Householder's Age Recode`,`Monthly Gross Rent`,`Householder's Race`) %>%
    bind_rows(NYC) -> 
    NYC
}
NYC$`Householder's Age Recode` <- NYC$`Householder's Age Recode` %>% as.numeric()
NYC$`Monthly Gross Rent` <- NYC$`Monthly Gross Rent`%>% as.numeric()
NYC <- NYC %>% filter(`Monthly Gross Rent`<9900) 


tmp <- dta1991 %>% select(Borough, starts_with("Householder"), starts_with("Place of Householder"),`Mortgage Status`)
tmp <- tmp[,-c(6:8)] %>% mutate_all(., funs(factor(.)))
tmp$`Householder's Age Recode` <-  as.integer(tmp$`Householder's Age Recode`)
summary(tmp)


ggplot(NYC, aes(x = `Mortgage Status`,fill = Borough)) + geom_bar()

ggplot(NYC, aes(x = `Mortgage Status`,fill = `Householder's Sex`)) + geom_bar()+scale_fill_manual(values = c("orange","green4","blue2"),labels=c("Male","Female","Not reported"))

ggplot(NYC, aes(x = `Householder's Age Recode`)) + geom_histogram() + xlim(0,90)
summary(NYC$`Householder's Age Recode`)

ggplot(NYC, aes(x = `Mortgage Status`,fill = `Householder's Race`)) + geom_bar()
ggplot(NYC,aes(x=`Householder's Race`))+geom_bar()

ggplot(NYC,aes(x = `Householder's Sex`,y = `Monthly Gross Rent`,fill=`Householder's Sex`)) +geom_boxplot()
ggplot(NYC,aes(x = Borough,y = `Monthly Gross Rent`,fill=Borough)) +geom_boxplot()
ggplot(NYC,aes(x = `Householder's Age Recode`,y = `Monthly Gross Rent`)) + geom_jitter()
ggplot(NYC,aes(x = `Householder's Race`,y = `Monthly Gross Rent`,fill=`Householder's Race`)) +geom_boxplot()

ggplot(tmp, aes(x = `Mortgage Status`,fill =`Place of Householder's Birth`)) + geom_bar()
ggplot(NYC,aes(x=Borough))+geom_bar()

ggplot(NYC,aes(`Mortgage Status`,y=`Householder's Age Recode`,fill=`Mortgage Status`))+geom_boxplot()


sex <- data.frame("Year" = 1:10, "Male" = 1:10,"Male_p" = 1:10, "Female" = 1:10, "Female_p" = 1:10)
borough <- data.frame("Year"=1:10,"Bronx"=1:10,"Bronx_p"=1:10,"Brooklyn"=1:10,"Brooklyn_p"=1:10,"Manhattan"=1:10,"Manhattan_p"=1:10,"Queens"=1:10,"Queens_p"=1:10,"Staten_Island"=1:10,"Staten_Island_p"=1:10)
race <- data.frame("Year"=1:10,"White"=1:10,"White_p"=1:10,"Black"=1:10,"Black_p"=1:10,"Asian"=1:10,"Asian_p"=1:10,"Others"=1:10,"Others_p"=1:10)

# Reading in all data
years <- c(1991,1993,1996,1999,2002,2005,2008,2011,2014,2017)
dta <- list()
for (i in 1:10) {
  name <- str_c("DATA/NYCHVS ", years[i]," Occupied File for ASA Challenge.csv")
  dta[[i]] <- read_csv(name, skip = 1)
  dta[[i]] <- dta[[i]] #%>% mutate_all(., funs(factor(.)))
}

# Sex proportions
sex[,1] <- years %>% factor()
for (i in 1:10) {
  sex[i,2] = summary(dta[[i]]$`Householder's Sex`)[1]
  sex[i,4] = summary(dta[[i]]$`Householder's Sex`)[2]
  sex[i,3] = summary(dta[[i]]$`Householder's Sex`)[1]/(summary(dta[[i]]$`Householder's Sex`)[1] + summary(dta[[i]]$`Householder's Sex`)[2])
  sex[i,5] = summary(dta[[i]]$`Householder's Sex`)[2]/(summary(dta[[i]]$`Householder's Sex`)[1] + summary(dta[[i]]$`Householder's Sex`)[2])
}

# borough proportions
borough[,1] <- years %>% factor()
for (i in 1:10) {
  total <- sum(summary(dta[[i]]$Borough))
  borough[i,2] = summary(dta[[i]]$Borough)[1]
  borough[i,4] = summary(dta[[i]]$Borough)[2]
  borough[i,6] = summary(dta[[i]]$Borough)[3]
  borough[i,8] = summary(dta[[i]]$Borough)[4]
  borough[i,10] = summary(dta[[i]]$Borough)[5]
  borough[i,3] = summary(dta[[i]]$Borough)[1]/total
  borough[i,5] = summary(dta[[i]]$Borough)[2]/total
  borough[i,7] = summary(dta[[i]]$Borough)[3]/total
  borough[i,9] = summary(dta[[i]]$Borough)[4]/total
  borough[i,11] = summary(dta[[i]]$Borough)[5]/total
}

# Race proportions
race[,1] <- years %>% factor()
for (i in 1) {
  total <- sum(summary(dta[[i]]$`Householder's Race`))
  race[i,2] <- summary(dta[[i]]$`Householder's Race`)[1]
  race[i,4] <- summary(dta[[i]]$`Householder's Race`)[2]
  race[i,6] <- sum(summary(dta[[i]]$`Householder's Race`)[3:9])
  race[i,8] <- summary(dta[[i]]$`Householder's Race`)[10]
  race[i,3] <- summary(dta[[i]]$`Householder's Race`)[1]/total
  race[i,5] <- summary(dta[[i]]$`Householder's Race`)[2]/total
  race[i,7] <- sum(summary(dta[[i]]$`Householder's Race`)[3:9])/total
  race[i,9] <- summary(dta[[i]]$`Householder's Race`)[10]/total
}

for (i in 2:4) {
  total <- sum(summary(dta[[i]]$`Householder's Race`))
  race[i,2] <- summary(dta[[i]]$`Householder's Race`)[1]
  race[i,4] <- summary(dta[[i]]$`Householder's Race`)[2]
  race[i,6] <- sum(summary(dta[[i]]$`Householder's Race`)[3:9])
  race[i,3] <- summary(dta[[i]]$`Householder's Race`)[1]/total
  race[i,5] <- summary(dta[[i]]$`Householder's Race`)[2]/total
  race[i,7] <- sum(summary(dta[[i]]$`Householder's Race`)[3:9])/total
}

for (i in 5:10) {
  total <- sum(summary(dta[[i]]$`Race Recode 1 (Householder)`))
  race[i,2] <- summary(dta[[i]]$`Race Recode 1 (Householder)`)[1]
  race[i,4] <- summary(dta[[i]]$`Race Recode 1 (Householder)`)[2]
  race[i,6] <- summary(dta[[i]]$`Race Recode 1 (Householder)`)[4]
  race[i,8] <- sum(summary(dta[[i]]$`Race Recode 1 (Householder)`)[c(3,5,6)])
  race[i,3] <- summary(dta[[i]]$`Race Recode 1 (Householder)`)[1]/total
  race[i,5] <- summary(dta[[i]]$`Race Recode 1 (Householder)`)[2]/total
  race[i,7] <- sum(summary(dta[[i]]$`Race Recode 1 (Householder)`)[4])/total
  race[i,9] <- sum(summary(dta[[i]]$`Race Recode 1 (Householder)`)[c(3,5,6)])/total
}


samp <- dta %>% select(contains("race"))


ggplot(sex) + geom_point(aes(x=Year,y=Male, color = "blue",size=1)) + 
  geom_point(aes(x=Year,y=Female,color = "red",size=1)) +
  ylab("Amount") + scale_color_manual(labels = c("Male", "Female"), values = c("blue", "red")) 

ggplot(sex) + geom_point(aes(x=Year,y=Male_p, color = "blue", size = 1)) + 
  geom_point(aes(x=Year,y=Female_p,color = "red", size = 1)) +
  ylab("Percentage") + scale_color_manual(labels = c("Male", "Female"), values = c("blue", "red")) 

ggplot(borough) + geom_point(aes(x=Year,y=Bronx, color = "red",size=1)) + 
  geom_point(aes(x=Year,y=Brooklyn, color = "green",size=1)) +
  geom_point(aes(x=Year,y=Manhattan, color = "blue",size=1)) +
  geom_point(aes(x=Year,y=Queens, color = "purple",size=1)) +
  geom_point(aes(x=Year,y=Staten_Island, color = "yellow",size=1)) +ylab("Amount") +
  scale_color_manual(labels = c("Manhattan", "Brooklyn","Queens","Bronx","Staten_Island"), values = c("red","green","blue","purple","orange")) +
  guides(color=guide_legend("Borough"))
  
ggplot(borough) + geom_point(aes(x=Year,y=Bronx_p, color = "red",size=1)) + 
  geom_point(aes(x=Year,y=Brooklyn_p, color = "green",size=1)) +
  geom_point(aes(x=Year,y=Manhattan_p, color = "blue",size=1)) +
  geom_point(aes(x=Year,y=Queens_p, color = "purple",size=1)) +
  geom_point(aes(x=Year,y=Staten_Island_p, color = "yellow",size=1)) +ylab("Percentage") +
  scale_color_manual(labels = c("Manhattan", "Brooklyn","Queens","Bronx","Staten_Island"), values = c("red","green","blue","purple","orange")) +
  guides(color=guide_legend("Borough"))
# 1991
# Exterior walls 3:8
# Window 9:13
# stairways 14:18
# condition of building 25
# number of units 62
# stories 64

external <- dta[[1]] %>% select(contains("Window"),contains("Exterior Walls"),contains("Stairways"),contains("Condition of building"),contains("Number of Units"),contains("Stories")) 
for (i in 2:3) {
  dta[[i]] %>% select(contains("Window"),contains("Exterior Walls"),contains("Stairways"),contains("Condition of building"),contains("Number of Units"),contains("Stories")) %>%
    bind_rows(external) -> 
    external
}
external <- external %>% select(-`Condition of Stairways (Exterior and Interior): No interior steps or stairways`,-`Condition of Stairways (Exterior and Interior): No exterior steps or stairways`,-`Condition of Stairways (Exterior and Interior): No stairways`)
external$`Number of Units in Building` <- factor(external$`Number of Units in Building`)
external$`Stories in building` <- factor(external$`Stories in building`)
external <- data.frame(external)


external_imputed <- external
for (i in c(1:16)) {
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

ex <- subset(external[[17]], external[[17]] != 8) %>% factor()
p1 <- summary(ex)[[1]]/length(ex)
p2 <- summary(ex)[[2]]/length(ex)
p3 <- 1-p1-p2
miss <- which(external[[17]] == 8)
impute_1 <- c(1:length(miss)) %>% sample(length(miss)*p1)
impute_2 <- setdiff(1:length(miss), impute_1)
impute_25 <- impute_2 %>% sample(length(impute_2)*p2/(p2+p3))
impute_3 <- setdiff(impute_2, impute_25)

external_imputed[[17]][miss[impute_1]] <- 1
external_imputed[[17]][miss[impute_25]] <- 2
external_imputed[[17]][miss[impute_3]] <- 3
summary(external_imputed)
