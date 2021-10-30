library(ggplot2)
library(tidyverse)
library(gapminder)



# library(printr) 
library(RColorBrewer) ## to chose different colors for the graph


# Let’s turn this code into a reusable template for making graphs with ggplot2. 
# ggplot(data = <DATA>) +
#  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))



## To plot trend, run this code to put literacy on the x-axis and 
## Employment on the y-axis
ggplot(data = trend) +
  geom_point(mapping = aes(x = literacy, y = employment))
## The plot shows a positive relationship between literacy rate (literacy) and 
## employment rate (employment) with each country. In other words, high literacy 
## rate leads high employment rate.


ggplot(data = trend) +
  geom_point(mapping = aes(x = literacy, y = employment, color = country))
## map the colors of points to the class variable to reveal the relationship 
## between literacy rate (literacy) and employment rate (employment) 
## for each country.

## exploring continuous variales  -- distributions
ggplot(data=trend, aes(x=literacy)) + 
  geom_density()

ggplot(data=trend, aes(x=literacy)) + 
  geom_density(size=1.5, fill="pink", alpha=0.3)

ggplot(data=trend, aes(x=literacy)) + 
  geom_density(size=1.5, fill="pink", alpha=0.5) +
  geom_histogram(aes(y=..density..), binwidth=4, color="black", fill="lightblue", alpha=0.5)

geom_histogram(aes(y=..density..), binwidth=4, color="black", fill="lightblue", alpha=0.5)

##  using layers

plt <- ggplot(data=trend,
              aes(x=literacy, y=employment))
plt

plt + geom_point()

plt + geom_point(aes(color=country))

plt + geom_point(mapping = aes(color=country)) +
  geom_smooth(method="loess")

plt + geom_point(aes(color=country)) +
  geom_smooth(mapping = aes(x=literacy, y=employment), method="loess") +
  scale_x_log10()


## use these mappings to extend or overwrite the global mappings


## World Map
## Loading required package: sp
library(sp)
library(lattice)
library(survival)
library(Formula)

library(dplyr)
library(rworldmap) ## plotting the data on World Map
library(countrycode) ## Converting the country name to Country code
library(Hmisc)

dim(trend)


colnames(trend)


sum(complete.cases(trend)) ## No missing values found

describe(trend)  ## see Hmisc

trend$countrycode <- countrycode(trend$country, 'country.name', 'iso3c')

sPDF <- joinCountryData2Map(trend 
                            ,joinCode = "ISO3"
                            ,nameJoinColumn = "countrycode"
                            ,suggestForFailedCodes = FALSE
                            , verbose = T)




colourPalette <- brewer.pal(7,'GnBu')

mapParams <- mapCountryData(sPDF,
                            nameColumnToPlot="literacy",
                            addLegend=FALSE,
                            colourPalette=colourPalette )

do.call(addMapLegend
        ,c(mapParams
           ,legendLabels="all"
           ,legendWidth=0.5
           ,legendIntervals="data"
           ,legendMar = 2))



colourPalette <- brewer.pal(7,'GnBu')

mapParams1 <- mapCountryData(sPDF,
                            nameColumnToPlot="employment",
                            addLegend=FALSE,
                            colourPalette=colourPalette )

do.call(addMapLegend
        ,c(mapParams1
           ,legendLabels="all"
           ,legendWidth=0.5
           ,legendIntervals="data"
           ,legendMar = 2))





#############################################################
# The following message might help you to improve your visualization and presentation. I hope this way can give you a direction for the plotting.

# First I would recommend when read the data, using current work direction instead of local disk

# data <- read_csv(data/"file name")

# REMEMBER!!!! Well organize your code. 


library(ggplot2)
library(tidyverse)
library(gapminder)
library(RColorBrewer)
library(sp)
library(lattice)
library(survival)
library(Formula)

library(dplyr)
library(rworldmap)
library(countrycode)
library(Hmisc)

# Next time try to use "read_csv", "read_csv" is under tidyverse, using this function can easily tidy into tibble form, for instance: 
Literacy_org <- read.csv("data/literacy_rate_adult_total_percent_of_people_ages_15_and_above.csv", header = T)
employment_org <- read.csv("data/aged_15plus_employment_rate_percent.csv", header = T)

# I will create a function for you to use for the presentation. 

# The following function I created might help you to select the way you want to use, either map or data explore. 
# In addition, I include some warning if you input the incorrect type, type number and title.
# type: either you explore the data or making a map
# type number: related to type, 1 indicate the histogram you made preciously, 2 indicate the dot plot and regression.
# title: relate to map, either plot with literacy or employment.

plt_fuc <- function(type, `type number`, title) {
   # The following code only for debug, once the function work notice them
   # type <- "explor"
   # title <- "liter"
   # `type number` <- 1 
  
   # type condition: explor or map and type number
  if (type == "explor") {
    if (`type number` == 1) {
      ggplot(data=trend, aes(x=literacy)) + 
        geom_density(size=1.5, fill="pink", alpha=0.5) +
        geom_histogram(aes(y=..density..), binwidth=4, color="black", fill="lightblue", alpha=0.5)
    } else if (`type number` == 2) {
      ggplot(data=trend, aes(x=literacy, y=employment))+
        geom_point(aes(color=country)) +
        geom_smooth(mapping = aes(x=literacy, y=employment), method="loess") +
        scale_x_log10()
    } else if (`type number` >= 3) {
      stop("Warning: Type number does not exist")  #make sure remind if you type the wrong type number
    }
  } else if (type == "map") {
   # indicate the title condition:
     if (title == "liter") {
      trend$countrycode <- countrycode(trend$country, 'country.name', 'iso3c')
      sPDF <- joinCountryData2Map(trend 
                                  ,joinCode = "ISO3"
                                  ,nameJoinColumn = "countrycode"
                                  ,suggestForFailedCodes = FALSE
                                  , verbose = T)
      colourPalette <- brewer.pal(7,'GnBu')
      mapParams <- mapCountryData(sPDF,
                                  nameColumnToPlot="literacy",
                                  addLegend=FALSE,
                                  colourPalette=colourPalette )
      do.call(addMapLegend
              ,c(mapParams
                 ,legendLabels="all"
                 ,legendWidth=0.5
                 ,legendIntervals="data"
                 ,legendMar = 2))}
     else if (title == "employ") {
      colourPalette <- brewer.pal(7,'GnBu')
      mapParams1 <- mapCountryData(sPDF,
                                   nameColumnToPlot="employment",
                                   addLegend=FALSE,
                                   colourPalette=colourPalette )
      do.call(addMapLegend
              ,c(mapParams1
                 ,legendLabels="all"
                 ,legendWidth=0.5
                 ,legendIntervals="data"
                 ,legendMar = 2))
     } else if (title != c("empply", "liter")) {
       stop("Warning: Incorrect title.") #same as type number condition
     }
  } else {stop("Warning: Please ensure you input the correct type, type number and title.\
               Type: explor|map; Type number: 1|2; Title: liter|empoly.")}
}

# debugging the function: 
plt_fuc(type = "map", title = "liter")

# Last thing I would like to mentioned, you can add the title for your legend, it might help other peopel to understand what it is that for. 




