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



















