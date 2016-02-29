# Gandrud // Session 03 // 2016-02-29

# working directory

try(setwd("/Users/Lukas/Documents/Git/PairAssignment3"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/PairAssignment3"),silent=TRUE)
getwd()

## download URL

URL <- "http://real-chart.finance.yahoo.com/table.csv?s=%5EGDAXI&d=1&e=29&f=2016&g=d&a=10&b=26&c=1990&ignore=.csv"
dax <- read.csv(URL)

## character vector

country <- c('Germany')
country <- data.frame(country)
cc <- country$country
country$iso2c <- countrycode(cc, "country.name", "iso2c")

## get data from the World Bank

WDI <- WDI(iso, indicator = c(

