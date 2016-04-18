# Collaborative Social Sience Data - Pair Assignment 3
# Variables

library(DataCombine)
library(plm)

# Set Working Directory
try(setwd("/Users/Lukas/Documents/Git/PairAssignment3/"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/PairAssignment3/"),silent=TRUE)
getwd()


# Dynamical Link to first R script file
source("PairAssignment3_Code/Y2_SS_Collaborative_Session03_b.R")

# define year as numeric
merge10$year <- as.numeric(merge10$year)

# creating time lags
# lag variable by one time period (one quarter year)
lag <- slide(merge10, Var = 'GDPq.gr', TimeVar = 'Date', GroupVar = 'iso3c', NewVar = 'L.GDPq.gr', slideBy = -1,
             keepInvalid = FALSE, reminder = TRUE)

# panel data to order data set by country and time
panel <- pdata.frame(lag, index=c("iso3c", "Date")) #setting dataframe to panel data

# lag variable by one time period (one quarter year)
lag <- slide(lag, Var = 'unempl', TimeVar = 'Date', GroupVar = 'iso3c', NewVar = 'L.unempl', slideBy = -1,
             keepInvalid = FALSE, reminder = TRUE)

# panel data to order data set by country and time
panel <- pdata.frame(lag, index=c("iso3c", "Date")) #setting dataframe to panel data

# lag variable by one time period (one quarter year)
lag <- slide(lag, Var = 'consumption.spending', TimeVar = 'Date', GroupVar = 'iso3c', NewVar = 'L.cons.spend', slideBy = -1,
             keepInvalid = FALSE, reminder = TRUE)

# panel data to order data set by country and time
panel <- pdata.frame(lag, index=c("iso3c", "Date")) #setting dataframe to panel data

# lag variable by one time period (one quarter year)
lag <- slide(lag, Var = 'Brent.dollar.change', TimeVar = 'Date', GroupVar = 'iso3c', NewVar = 'L.Brent', slideBy = -1,
             keepInvalid = FALSE, reminder = TRUE)

# panel data to order data set by country and time
panel <- pdata.frame(lag, index=c("iso3c", "Date")) #setting dataframe to panel data

# lag variable by one time period (one quarter year)
lag <- slide(lag, Var = 'WTI.dollar.change', TimeVar = 'Date', GroupVar = 'iso3c', NewVar = 'L.WTI', slideBy = -1,
             keepInvalid = FALSE, reminder = TRUE)

# panel data to order data set by country and time
panel <- pdata.frame(lag, index=c("iso3c", "Date")) #setting dataframe to panel data

# lag variable by one time period (one quarter year)
lag <- slide(lag, Var = 'ECB.MRO.change', TimeVar = 'Date', GroupVar = 'iso3c', NewVar = 'L.ECB.MRO.ch', slideBy = -1,
             keepInvalid = FALSE, reminder = TRUE)

# panel data to order data set by country and time
panel <- pdata.frame(lag, index=c("iso3c", "Date")) #setting dataframe to panel data

# lag variable by one time period (one quarter year)
lag <- slide(lag, Var = 'ECB.dep.change', TimeVar = 'Date', GroupVar = 'iso3c', NewVar = 'L.ECB.dep.ch', slideBy = -1,
             keepInvalid = FALSE, reminder = TRUE)

# panel data to order data set by country and time
panel <- pdata.frame(lag, index=c("iso3c", "Date")) #setting dataframe to panel data
rm(merge10, lag)
