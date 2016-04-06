# Pair Assignment 3
# Data 

library(countrycode)
library(WDI)
library(plyr)

# Set Working Directory
try(setwd("/Users/Lukas/Documents/Git/PairAssignment3"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/PairAssignment3/PairAssignment3_Data"),silent=TRUE)
getwd()

#get data on natural disasters
dis <- read.csv("disaster19912015.csv", header = FALSE, sep = ",", ".", stringsAsFactors = FALSE, na.strings = c("", "NA"))

names(dis)[1] <- 'year'
names(dis)[2] <- 'disaster'
names(dis)[3] <- 'iso3c'
names(dis)[4] <- 'country'
names(dis)[5] <- 'occurrence'
names(dis)[6] <- 'deaths'
names(dis)[7] <- 'affected'
names(dis)[8] <- 'injured'
names(dis)[9] <- 'homeless'
names(dis)[10] <- 'total.affected'
names(dis)[11] <- 'total.damage'

dis <- dis[-1,]
dis <- dis[-1,]

dis$iso3c <- NULL
dis$deaths <- NULL
dis$affected <- NULL
dis$injured <- NULL
dis$homeless <- NULL
dis$total.affected <- NULL
dis$total.damage <- NULL

dis$country <- gsub("[^a-zA-Z0-9]","",dis$country) #get rid of special characters
dis$country[dis$country=="AzoresIslands"] <- "Azores"
dis$country[dis$country=="CanaryIs"] <- "Canary Islands"
dis$country[dis$country=="CentralAfricanRepublic"] <- "Central African Republic"
dis$country[dis$country=="LaoPeoplesDemocraticRepublicthe"] <- "Laos"
dis$country[dis$country=="Runion"] <- "Reunion"
dis$country[dis$country=="SaintLucia"] <- "Saint Lucia"
dis$country[dis$country=="SerbiaMontenegro"] <- "Serbia"
dis$country[dis$country=="VirginIslandUS"] <- "Virgin Island US"




dis$disaster <- gsub("[^a-zA-Z0-9]","",dis$disaster) #get rid of special characters
dis$occurrence <- as.numeric(dis$occurrence)

dis <- dis[,c(1,3,2,4)]

dis[794, ]
dis <- dis[-c(794), ]

aggrtdis <- dcast(dis, country + year ~ disaster, sum) #p317 R for Dummies
disastercc <- aggrtdis$country
aggrtdis$iso2c <- countrycode(disastercc, "country.name", "iso2c")
aggrtdis <- aggrtdis[complete.cases(aggrtdis),]

aggrtdis$Animalaccident <- NULL
aggrtdis$Extremetemperature <- NULL
aggrtdis$Insectinfestation <- NULL
aggrtdis$Massmovementdry <- NULL
aggrtdis$Volcanicactivity <- NULL
aggrtdis$Impact <- NULL
aggrtdis$Disaster <- (aggrtdis$Drought + aggrtdis$Earthquake + aggrtdis$Epidemic + aggrtdis$Flood + aggrtdis$Landslide + aggrtdis$Storm + aggrtdis$Wildfire)
rm(dis, disastercc)

# Germany
URL <- "http://real-chart.finance.yahoo.com/table.csv?s=%5EGDAXI&a=00&b=01&c=1991&d=02&e=21&f=2016&g=d&ignore=.csv"
DAX <- read.csv(URL)
colnames(DAX) <- paste("DAX", colnames(DAX), sep = ".")
names(DAX)[1] <- 'year'
DAX$year <- as.character(DAX$year)
DAX$year <- substring(DAX$year,1,nchar(DAX$year)-6)
DAX <- ddply(DAX, .(year), function(DAX) c(DAX.Open=mean(DAX$DAX.Open), DAX.High=mean(DAX$DAX.High), DAX.Low=mean(DAX$DAX.Low), DAX.Close=mean(DAX$DAX.Close), DAX.Volume=mean(DAX$DAX.Volume), DAX.Adj.Close=mean(DAX$DAX.Adj.Close)))
DAX$year <- as.numeric(DAX$year)

# Japan
URL <- "http://real-chart.finance.yahoo.com/table.csv?s=%5EN225&a=00&b=01&c=1991&d=02&e=21&f=2016&g=d&ignore=.csv"
NIKKEI <- read.csv(URL)
colnames(NIKKEI) <- paste("NIK", colnames(NIKKEI), sep = ".")
names(NIKKEI)[1] <- 'year'
NIKKEI$year <- as.character(NIKKEI$year)
NIKKEI$year <- substring(NIKKEI$year,1,nchar(NIKKEI$year)-6)
NIKKEI <- ddply(NIKKEI, .(year), function(NIKKEI) c(NIK.Open=mean(NIKKEI$NIK.Open), NIK.High=mean(NIKKEI$NIK.High), NIK.Low=mean(NIKKEI$NIK.Low), NIK.Close=mean(NIKKEI$NIK.Close), NIK.Volume=mean(NIKKEI$NIK.Volume), NIK.Adj.Close=mean(NIKKEI$NIK.Adj.Close)))
NIKKEI$year <- as.numeric(NIKKEI$year)

# UK
URL <- "http://real-chart.finance.yahoo.com/table.csv?s=%5EFTSE&a=00&b=01&c=1991&d=02&e=21&f=2016&g=d&ignore=.csv"
FTSE <- read.csv(URL)
colnames(FTSE) <- paste("FTSE", colnames(FTSE), sep = ".")
names(FTSE)[1] <- 'year'
FTSE$year <- as.character(FTSE$year)
FTSE$year <- substring(FTSE$year,1,nchar(FTSE$year)-6)
FTSE <- ddply(FTSE, .(year), function(FTSE) c(FTSE.Open=mean(FTSE$FTSE.Open), FTSE.High=mean(FTSE$FTSE.High), FTSE.Low=mean(FTSE$FTSE.Low), FTSE.Close=mean(FTSE$FTSE.Close), FTSE.Volume=mean(FTSE$FTSE.Volume), FTSE.Adj.Close=mean(FTSE$FTSE.Adj.Close)))
FTSE$year <- as.numeric(FTSE$year)

# France
URL <- "http://real-chart.finance.yahoo.com/table.csv?s=%5EFCHI&a=00&b=1&c=1991&d=02&e=21&f=2016&g=d&ignore=.csv"
CAC <- read.csv(URL)
colnames(CAC) <- paste("CAC", colnames(CAC), sep = ".")
names(CAC)[1] <- 'year'
CAC$year <- as.character(CAC$year)
CAC$year <- substring(CAC$year,1,nchar(CAC$year)-6)
CAC <- ddply(CAC, .(year), function(CAC) c(CAC.Open=mean(CAC$CAC.Open), CAC.High=mean(CAC$CAC.High), CAC.Low=mean(CAC$CAC.Low), CAC.Close=mean(CAC$CAC.Close), CAC.Volume=mean(CAC$CAC.Volume), CAC.Adj.Close=mean(CAC$CAC.Adj.Close)))
CAC$year <- as.numeric(CAC$year)

# Brasil
URL <- "http://real-chart.finance.yahoo.com/table.csv?s=%5EBVSP&a=00&b=01&c=1991&d=02&e=21&f=2016&g=d&ignore=.csv"
IBOV <- read.csv(URL)
colnames(IBOV) <- paste("IBOV", colnames(IBOV), sep = ".")
names(IBOV)[1] <- 'year'
IBOV$year <- as.character(IBOV$year)
IBOV$year <- substring(IBOV$year,1,nchar(IBOV$year)-6)
IBOV <- ddply(IBOV, .(year), function(IBOV) c(IBOV.Open=mean(IBOV$IBOV.Open), IBOV.High=mean(IBOV$IBOV.High), IBOV.Low=mean(IBOV$IBOV.Low), IBOV.Close=mean(IBOV$IBOV.Close), IBOV.Volume=mean(IBOV$IBOV.Volume), IBOV.Adj.Close=mean(IBOV$IBOV.Adj.Close)))
IBOV$year <- as.numeric(IBOV$year)

# Russia
URL <- "http://real-chart.finance.yahoo.com/table.csv?s=OMRU.EX&a=00&b=01&c=1992&d=02&e=21&f=2016&g=d&ignore=.csv"
OMRU <- read.csv(URL)
colnames(OMRU) <- paste("OMRU", colnames(OMRU), sep = ".")
names(OMRU)[1] <- 'year'
OMRU$year <- as.character(OMRU$year)
OMRU$year <- substring(OMRU$year,1,nchar(OMRU$year)-6)
OMRU <- ddply(OMRU, .(year), function(OMRU) c(OMRU.Open=mean(OMRU$OMRU.Open), OMRU.High=mean(OMRU$OMRU.High), OMRU.Low=mean(OMRU$OMRU.Low), OMRU.Close=mean(OMRU$OMRU.Close), OMRU.Volume=mean(OMRU$OMRU.Volume), OMRU.Adj.Close=mean(OMRU$OMRU.Adj.Close)))
OMRU$year <- as.numeric(OMRU$year)
rm(URL)



WDI <- WDI(country = "all", indicator = c("CM.MKT.TRAD.GD.ZS", #Stocks traded, total value (% of GDP) (4th column)
                                          "BN.KLT.DINV.CD.ZS", #Foreign direct investment (% of GDP)
                                          "CM.MKT.TRNR", #Stocks traded, turnover ratio (%)
                                          "NY.GDP.PCAP.KD.ZG", #GDP per capita growth (annual %)
                                          "NEGDIKSTKKD", #Estimated Capital stock (real 2005 US$)
                                          "NY.GDP.PCAP.KD"), #GDP per capita (constant 2005 US$)
           start=1991, end=2015)

summary(WDI$CM.MKT.TRAD.GD.ZS) #4339 NA's
summary(WDI$BN.KLT.DINV.CD.ZS) #5996
summary(WDI$CM.MKT.TRNR) # 4313 NA's
summary(WDI$NY.GDP.PCAP.KD.ZG) # 945 NA's
summary(WDI$NY.GDP.PCAP.KD) # 972 NA's

names(WDI)[4] <- 'stocks'
names(WDI)[5] <- 'fdi'
names(WDI)[6] <- 'sturnover'
names(WDI)[7] <- 'gdp.pc.gr'
names(WDI)[8] <- 'gdp.pc'

merge1 <- merge(WDI,aggrtdis,by=c("iso2c", "year"), all.x = TRUE) #16 observations too much
merge2 <- merge(merge1,CAC,by=c("year"), all.x = TRUE)
merge3 <- merge(merge2,DAX,by=c("year"), all.x = TRUE)
merge4 <- merge(merge3,FTSE,by=c("year"), all.x = TRUE)
merge5 <- merge(merge4,IBOV,by=c("year"), all.x = TRUE)
merge6 <- merge(merge5,NIKKEI,by=c("year"), all.x = TRUE)
merge7 <- merge(merge6,OMRU,by=c("year"), all.x = TRUE)
rm(aggrtdis, CAC, DAX, FTSE, IBOV, OMRU, NIKKEI)