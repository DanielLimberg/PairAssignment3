# Collaborative Social Sience Data - Pair Assignment 3
# Variables

library(DataCombine)

# Set Working Directory
try(setwd("/Users/Lukas/Documents/Git/PairAssignment3/"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/PairAssignment3/"),silent=TRUE)
getwd()


# Dynamical Link to first R script file
source("PairAssignment3_Code/Y2_SS_Collaborative_Session03_b.R")


merge10$iso3c <- NULL
merge10$iso3c.x <- NULL
merge10$iso3c.y <- NULL
merge10$V4 <- NULL
merge10$V5 <- NULL

# define year and Date as numeric
merge10$year <- as.factor(merge10$year)
merge10$Date <- gsub("Q",".",merge10$Date)
merge10$Date <- as.numeric(merge10$Date)

sel <- which(merge10$Date>1998.4) 
merge10$ECB.MRO.change[sel][is.na(merge10$ECB.MRO.change[sel])] <- 0
merge10$ECB.dep.change[sel][is.na(merge10$ECB.dep.change[sel])] <- 0

rm(sel)

# creating time lags
# lag variables by one time period (one quarter year)

var <- c("USA.GDP", "JPN.GDP", "GBR.GDP", "FRA.GDP", "DEU.GDP", "USA.unempl", "JPN.unempl", "GBR.unempl",
         "FRA.unempl", "DEU.unempl", "USA.prvconsm", "JPN.prvconsm", "GBR.prvconsm", "FRA.prvconsm",
         "DEU.prvconsm", "WTI.dollar.change", "Brent.dollar.change", "ECB.MRO.change", "ECB.dep.change")

for (i in var) {
  merge10[, paste0("L.", i)] <- DataCombine::shift(merge10[, i], shiftBy = -1)
}

rm(i, var)


merge10 <- merge10[ ,c(1,3,2,4:64)]
