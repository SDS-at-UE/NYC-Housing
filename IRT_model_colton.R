library(readr)
library(tidyverse)
library(plm)
set.seed(6969)

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


#### Relevel pertinent variables
NYC$waterleakage <- ifelse(NYC$waterleakage == 2, 0, NYC$waterleakage)
NYC$`Presence of mice or rats` <- 
  ifelse(NYC$`Presence of mice or rats` == 2, 0, NYC$`Presence of mice or rats`)
NYC$`Heating equipment breakdown` <-
  ifelse(NYC$`Heating equipment breakdown` == 1, 2, NYC$`Heating equipment breakdown`)
NYC$`Heating equipment breakdown` <-
  ifelse(NYC$`Heating equipment breakdown` == 0, 1, NYC$`Heating equipment breakdown`)
NYC$`Heating equipment breakdown` <-
  ifelse(NYC$`Heating equipment breakdown` == 2, 0, NYC$`Heating equipment breakdown`)

internal <- NYC %>% select(waterleakage, `Presence of mice or rats`,
                           `Heating equipment breakdown`,
                           `Number of Cockroaches`, `Functioning Air Conditioning`,
                           `Year Identifier`)

### Air Conditioning starts in 2014
### Number of Cockroaches starts in 2008

internal_imputed <- internal
for (i in c(1:3)) {
  int <- subset(internal[[i]],internal[[i]] != 8)
  p <- mean(int)
  q <- 1 - p
  miss <- which(internal[[i]]==8)
  impute_1 <- c(1:length(miss)) %>% sample(length(miss)*p)
  impute_2 <- setdiff(1:length(miss),impute_1)
  
  inn <- case_when(internal[[i]]== 1 ~ 1,
                   internal[[i]]== 8 ~ 8,
                   internal[[i]]== 0 ~ 0) 
  
  inn[miss[impute_1]] <- 1 
  inn[miss[impute_2]] <- 0
  internal_imputed[i] <- inn
}

internal_imputed <- internal_imputed %>% 
  mutate(QIndex = waterleakage + `Presence of mice or rats` +
           `Heating equipment breakdown`)


# Formatting year
internal_imputed[[6]] <- case_when(internal_imputed[[6]] == 91 ~ 1991,
                                   internal_imputed[[6]] == 93 ~ 1993,
                                   internal_imputed[[6]] == 96 ~ 1996,
                                   internal_imputed[[6]] == 99 ~ 1999,
                                   TRUE ~ as.double(internal_imputed[[6]]))
internal_imputed[[6]] <- factor(internal_imputed[[6]])


# Formatting borough
internal_imputed$Borough <- NYC$Borough
internal_imputed[[8]] <- case_when(internal_imputed[[8]] == 1 ~ "Bronx",
                                   internal_imputed[[8]] == 2 ~ "Brooklyn",
                                   internal_imputed[[8]] == 3 ~ "Manhattan",
                                   internal_imputed[[8]] == 4 ~ "Queens",
                                   internal_imputed[[8]] == 5 ~ "Staten Island")



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

### Select internal structural variables
internal2 <- dta[[1]] %>% select(contains("bedrooms"), 
                                 contains("rooms"),contains("Interior"),
                                 contains("Condition of building"),
                                 contains("Kitchen"),contains("Plumbing"), 
                                 contains("Toilet"), contains("holes")) 
for (i in 2:10) {
  dta[[i]] %>% select(contains("bedrooms"), contains("rooms"),contains("Interior"),
                      contains("Condition of building"),contains("Kitchen"),
                      contains("Plumbing"), contains("Toilet"), 
                      contains("holes")) %>%
    bind_rows(internal2) -> 
    internal2
}

summary(factor(internal2[[17]]))
# Impute columns with 1,2 and 8 (Crack holes walls)
internal2_imputed <- internal2
for (i in c(9, 16)) {
  int <- subset(internal2[[i]],internal2[[i]] != 8)
  int <- ifelse(int == 1, 1, 0)
  p <- mean(int)
  q <- 1 - p
  miss <- which(internal2[[i]]==8)
  impute_1 <- c(1:length(miss)) %>% sample(length(miss)*p)
  impute_2 <- setdiff(1:length(miss),impute_1)
  
  int <- case_when(internal2[[i]]== 1 ~ 1,
                   internal2[[i]]== 8 ~ 8,
                   internal2[[i]]== 2 ~ 0) 
  int[miss[impute_1]] <- 1 
  int[miss[impute_2]] <- 0
  internal2_imputed[i] <- int
}

# We can use cracks of holes in interior walls,
# number of bedrooms, and number of rooms
internal2_imputed %>% select(`Number of bedrooms`, `Number of rooms`,
                             `Cracks of holes in interior walls`, 
                             `Holes in floors`) -> 
  internal2_imputed

### Add Year and Borough to the data set
internal2_imputed$`Year Identifier` <- NYC$`Year Identifier`
internal2_imputed$Borough <- NYC$Borough

# Formatting year
internal2_imputed[[5]] <- case_when(internal2_imputed[[5]] == 91 ~ 1991,
                                    internal2_imputed[[5]] == 93 ~ 1993,
                                    internal2_imputed[[5]] == 96 ~ 1996,
                                    internal2_imputed[[5]] == 99 ~ 1999,
                                    TRUE ~ as.double(internal2_imputed[[5]]))
internal2_imputed[[5]] <- factor(internal2_imputed[[5]])


# Formatting borough
internal2_imputed$Borough <- NYC$Borough
internal2_imputed[[6]] <- case_when(internal2_imputed[[6]] == 1 ~ "Bronx",
                                    internal2_imputed[[6]] == 2 ~ "Brooklyn",
                                    internal2_imputed[[6]] == 3 ~ "Manhattan",
                                    internal2_imputed[[6]] == 4 ~ "Queens",
                                    internal2_imputed[[6]] == 5 ~ "Staten Island")

# Create an index
internal2_imputed <- internal2_imputed %>% 
  mutate(Index = `Cracks of holes in interior walls` + `Holes in floors`)

## Combine the 3 data sets
imputed <- bind_cols(external_imputed, internal_imputed, internal2_imputed)

write_csv(imputed, "imputed_NYC.csv")

imputed <- imputed %>% select(Borough, `Year Identifier`, `Tenure 1`, 
                              score, QIndex, Index)

## Compute the combined quality index
imputed <- imputed %>% mutate(QualityIndex = score + QIndex + Index)



# Building a model for zscore
library(ltm)
NYC <- read_csv("imputed_NYC2.csv")





NYC <- NYC %>% filter(`Year Identifier2` == 2017)
NYC <- NYC %>% mutate(`Quality Index` = score + QIndex + Index)
NYC <- NYC[,-c(4:6, 9:10, 13:21, 25:31, 34:37)]
NYC_int <- NYC[,c(32:33)]

mod <- rasch(NYC_int)

plot(mod, type = "IIC")
plot(mod, type = "ICC")


mod_2pl <- ltm(NYC ~ z1)
summary(mod_2pl)

plot(mod_2pl, type = "IIC")
plot(mod_2pl, type = "ICC")


theta.rasch <- ltm::factor.scores(mod_2pl)

theta.rasch$score.dat %>% View()
  
theta.rasch$score.dat %>%
  mutate(Total = rowSums(theta.rasch$score.dat[,c(1:13)])) %>%
  ggplot(aes(x = z1 , y = Total)) + geom_point()

theta.rasch$score.dat %>% select(-c(13:14,16)) %>% right_join(NYC) -> NYCZ
NYCZ %>% View()


###########

NYC2 <- NYC %>% mutate(`Quality Index` = score + QIndex + Index)
NYC -> NYC_temp
years <- c(1991,1993,1996,1999,2002,2005,2008,2011,2014,2017)
results <- NULL
results2 <- NULL
for (i in years) {
  NYC2 %>% filter(`Year Identifier2` == i) -> NYC3
  NYC4 <- NYC3[,-c(4:6, 9:10, 13:23, 27:35, 38:43)]
  
  mod_2pl <- ltm(NYC4 ~ z1)
  coefficients(mod_2pl) -> coeff
  data.frame(coeff) %>% mutate(year = i, problem = rownames(coeff)) %>% 
    dplyr::select(problem, year, Dffclt, Dscrmn) %>% bind_rows(results)  -> results
  factor.scores(mod_2pl) -> theta.2pl
  NYC3 %>% left_join(theta.2pl$score.dat, 
                         by = c("Condition of Windows: Broken or missing windows", 
                                "Condition of Windows: Rotten/loose windows", 
                                "Condition of Windows: Boarded up windows", 
                                "Condition of Exterior Walls: Major cracks in outside walls", 
                                "Condition of Exterior Walls: Loose or hanging cornice, roofing, or other materia", 
                                "Condition of Stairways (Exterior and Interior): Loose, broken, or missing stair", 
                                "Condition of Stairways (Exterior and Interior): Loose, broken, or missing steps", 
                                "waterleakage", "Presence of mice or rats", "Heating equipment breakdown", 
                                "Cracks of holes in interior walls", "Holes in floors")) %>% 
    dplyr::select(-Obs, -Exp, -se.z1) -> NYC3
  NYC3 %>% bind_rows(results2) -> results2
}

#library(scales)
#results2 %>% mutate(final_index = rescale(z1)) -> results2

cal_z <- function(x){
  exp(x)/(1+exp(x))
}

results2 %>% mutate(final_index = cal_z(z1)) -> results2
write_csv(results2, "Index_Data.csv")

results2 %>% group_by(Borough) %>% summarise(Average = mean(final_index)) %>% 
  ggplot(aes(y = Average, x = Borough)) + geom_bar(stat = "identity")

results2 %>% group_by(`Borough1`, `Sub-Borough Area1`) %>%
  summarise(Average = mean(final_index)) %>% 
  ggplot(aes(y = Average, x = `Sub-Borough Area1`)) + 
  geom_bar(stat = "identity")


#########################################################################################
cal_prob <- function(x, a, b){
  exp(a*(x-b))/(1 +exp(a*(x-b)))
}

cal_prob(seq(-3,3, by = .1), Dscrmn, Dffclt)

### Difficulty parameter over years #### 
results %>% ggplot(aes(x = year, y = Dffclt, group = problem, color = problem)) + 
  geom_line() + labs(title = "Difficulty Over Years")

results %>% filter(str_detect(problem, "^Condition")) %>% ggplot(aes(x = year, y = Dffclt, group = problem, color = problem)) + 
  geom_line(size = 1.5) + labs(title = "External Structure Problems", y = "Difficulty", x = "Years") + 
  scale_color_discrete(labels = c("Loose or hanging cornice,\n roofing, or other material",
                                  "Major cracks in outside walls", "Loose, broken, or missing stair",
                                  "Loose, broken, or missing steps",
                                  "Boarded up windows", "Broken or missing windows",
                                  "Rotten or loose windows")) +  
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.title = element_text(size = 15),
        title = element_text(size = 15), legend.position = c(.72,.14), 
        legend.text = element_text(size =10)) +
  ylim(1,9.5) -> graph1
graph1

results %>% filter(str_detect(tolower(problem), "holes")) %>% ggplot(aes(x = year, y = Dffclt, group = problem, color = problem)) + 
  geom_line(size = 1.5) + 
  labs(title = "Internal Structure Problems", y = "Difficulty", x = "Years") +  
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.title = element_text(size = 15),
        title = element_text(size = 15), legend.position = c(.72,.95), 
        legend.text = element_text(size =10)) + 
  ylim(1,9.5) -> graph2
graph2

results %>% filter(str_detect(problem, "equipment|mice|waterleakage")) %>% ggplot(aes(x = year, y = Dffclt, group = problem, color = problem)) + 
  geom_line(size =1.5) + labs(title = "Internal Environment Problems", y = "Difficulty", x = "Years") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.title = element_text(size = 15),
        title = element_text(size = 15), legend.position = c(.72,.925),
        legend.text = element_text(size =10)) + ylim(1,9.5) -> graph3
graph3
library(ggplot2)
library(gridExtra)
grid.arrange(graph1, graph2, graph3, ncol = 3)


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(graph1, graph2, graph3, cols=3)
