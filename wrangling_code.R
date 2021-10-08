library(tidyverse) 
library(tidyr)
library(dplyr)     # library a tidyr package

# upload two individual indicators
## Adult literacy rate is the percentage of people ages 15 and above who can, 
## with understanding, read and write a short, simple statement on their 
## everyday life.

Literacy_org <- read.csv("/Users/odd/Desktop/FALL2021/MA615/MA615-HW-2/data/literacy_rate_adult_total_percent_of_people_ages_15_and_above.csv", 
                         header = T)

## Percentage of total population, age group 15+, that has been employed 
## during the given year.

employment_org <- read.csv("/Users/odd/Desktop/FALL2021/MA615/MA615-HW-2/data/aged_15plus_employment_rate_percent.csv", 
                     header = T)

## select the adult literacy rate and adult employment rate
Literacy1 <- Literacy_org[, c(1,28)]
sum(is.na(Literacy1$X2000))       # see how many NAs are on the dataset
Literacy_final <- na.omit(Literacy1)
Literacy_final$literacy <- Literacy_final$X2000
# list the literacy rate in 2000 for each country
literacy <- Literacy_final[, c(1,3)]
head(literacy)
employment_final <- employment_org[, c(1,12)]
sum(is.na(employment_final$X2000))       # see how many NAs are on the dataset
employment_final$employment <- employment_final$X2000
# list the employment rate in 2000 for each country

Employment <- employment_final[, c(1,3)]
head(Employment)

## create tibbles
as_tibble(literacy)
as_tibble(Employment)

## combine two tibbles
trend_org <- left_join(literacy, Employment)
trend <- na.omit(trend_org)
trend


