library(readr)
library(tidyverse)
library(plm)
library(ltm)
select <- dplyr::select
set.seed(6969)

imputed <- read_csv("imputed_data.csv")
imputed$WeightedQI <- imputed$QualityIndex * imputed$Weights
averageline <- sum(imputed$WeightedQI)/sum(imputed$Weights)

imputed %>% group_by(`Year Identifier`) %>% summarise(AverageQI = mean(WeightedQI)) %>%
  ggplot(x = `Year Identifier`) +
  geom_point(aes(x = `Year Identifier`, y = AverageQI)) + 
  geom_abline(a = averageline, b = 0)


imputed %>% geom_boxplot(x = `Year Identifier`, y = WeightedQI)



