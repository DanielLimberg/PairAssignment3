# Collaborative Social Sience Data - Pair Assignment 3
# Data

library(countrycode)
library(WDI)
library(plyr)
library(reshape2)
library(zoo)

# Set Working Directory
try(setwd("/Users/Lukas/Documents/Git/PairAssignment3"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/PairAssignment3/PairAssignment3_Data"),silent=TRUE)
getwd()


# natural disasters
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
dis$country[dis$country=="CongotheDemocraticRepublicofthe"] <- "Democratic republic of the Congo"
dis$country[dis$country=="Congothe"] <- "Republic of the Congo"

dis$disaster <- gsub("[^a-zA-Z0-9]","",dis$disaster) #get rid of special characters
dis$occurrence <- as.numeric(dis$occurrence)

dis <- dis[,c(1,3,2,4)]

dis[91, ] #delete GermanyFedRep
dis <- dis[-c(91), ]
dis[793, ] #delete NetherlandsAntilles
dis <- dis[-c(793), ]

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


# main refinancing operation (ECB)
MRO <- read.csv("MainRefinancingOperations.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))
MRO <- MRO[-1,]
MRO <- MRO[-1,]
MRO <- MRO[-1,]
MRO <- MRO[-1,]
MRO <- MRO[-1,]

names(MRO)[1] <- 'time'
names(MRO)[2] <- 'ECB.MRO'

MRO$ECB.MRO <- as.numeric(MRO$ECB.MRO)

MRO$Date <- as.yearqtr(MRO$time, format = "%Y-%m-%d")
format(MRO$Date, format = "%y/0%q")
MRO$Date <- gsub("[^a-zA-Z0-9]","",MRO$Date) #get rid of special characters


# deposit facility (ECB)
deposit <- read.csv("DepositFacility.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))
deposit <- deposit[-1,]
deposit <- deposit[-1,]
deposit <- deposit[-1,]
deposit <- deposit[-1,]
deposit <- deposit[-1,]

names(deposit)[1] <- 'time'
names(deposit)[2] <- 'ECB.depofacil'

deposit$ECB.depofacil <- as.numeric(deposit$ECB.depofacil)

deposit$Date <- as.yearqtr(deposit$time, format = "%Y-%m-%d")
format(deposit$Date, format = "%y/0%q")
deposit$Date <- gsub("[^a-zA-Z0-9]","",deposit$Date) #get rid of special characters


# quarterly GDP growth (OECD)
GDPq <- read.csv("QNA_06042016113157540.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))
GDPq <- GDPq[-1,]
GDPq$GPSA <- 0
GDPq$GPSA[which(GDPq$V5=="GPSA")] <- 1
GDPq$GDP <- 0
GDPq$GDP[which(GDPq$V4=="Gross domestic product - expenditure approach")] <- 1
GDPq$year <- GDPq$V9
GDPq$year <- gsub("\\-.*","",GDPq$year)

names(GDPq)[1] <- 'iso3c'
names(GDPq)[2] <- 'country'

GDPq$V3 <- NULL
GDPq$V6 <- NULL
GDPq$V7 <- NULL
GDPq$V8 <- NULL
GDPq$V10 <- NULL
GDPq$V11 <- NULL
GDPq$V12 <- NULL
GDPq$V13 <- NULL
GDPq$V14 <- NULL
GDPq$V15 <- NULL
GDPq$V16 <- NULL
GDPq$V18 <- NULL
GDPq$V19 <- NULL

names(GDPq)[5] <- 'Date'
GDPq$Date <- gsub("[^a-zA-Z0-9]","",GDPq$Date) #get rid of special characters
names(GDPq)[6] <- 'GDPq.gr'

sub <- subset(GDPq, GPSA > 0)
GPSA <- subset(sub, GDP > 0)
rm(GDPq, sub)


# consumption spending
consume <- read.csv("QNA_06042016174850637.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))
consume <- consume[-1,]

names(consume)[1] <- 'iso3c'
names(consume)[2] <- 'country'
consume$type <- 0
consume$type[which(consume$V4=="Private final consumption expenditure by durability")] <- 1
consume$computation <- 0
consume$computation[which(consume$V5=="CQRSA")] <- 1

consume$V3 <- NULL
consume$V5 <- NULL
consume$V6 <- NULL
consume$V7 <- NULL
consume$V8 <- NULL
consume$V10 <- NULL
consume$V12 <- NULL
consume$V13 <- NULL
consume$V14 <- NULL
consume$V15 <- NULL
consume$V16 <- NULL
consume$V18 <- NULL
consume$V19 <- NULL

names(consume)[3] <- 'consumption.type'
names(consume)[4] <- 'Date'
consume$Date <- gsub("[^a-zA-Z0-9]","",consume$Date) #get rid of special characters
names(consume)[5] <- 'currency'
names(consume)[6] <- 'consumption.spending'

sub <- subset(consume,  type > 0)
prvconsm <- subset(sub, computation > 0)
rm(consume, sub)


# total rate of unemployment (OECD)
unempl <- read.csv("Unemployment.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))
unempl <- unempl[-1,]

names(unempl)[1] <- 'iso3c'

unempl$V2 <- NULL
unempl$V3 <- NULL
unempl$V4 <- NULL
unempl$V5 <- NULL
unempl$V8 <- NULL

names(unempl)[2] <- 'Date'
unempl$Date <- gsub("[^a-zA-Z0-9]","",unempl$Date) #get rid of special characters
names(unempl)[3] <- 'unempl'


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


#World Bank Development Indicators
WDI <- WDI(country = "all", indicator = c("CM.MKT.TRAD.GD.ZS", #Stocks traded, total value (% of GDP) (4th column)
                                          "BN.KLT.DINV.CD.ZS", #Foreign direct investment (% of GDP)
                                          "CM.MKT.TRNR", #Stocks traded, turnover ratio (%)
                                          "NY.GDP.PCAP.KD.ZG", #GDP per capita growth (annual %)
                                          "NEGDIKSTKKD", #Estimated Capital stock (real 2005 US$)
                                          "NY.GDP.MKTP.KD.ZG"), #GDP growth (annual %)
           start=1991, end=2015)

summary(WDI$CM.MKT.TRAD.GD.ZS) #4339 NA's
summary(WDI$BN.KLT.DINV.CD.ZS) #5996
summary(WDI$CM.MKT.TRNR) # 4313 NA's
summary(WDI$NY.GDP.PCAP.KD.ZG) # 945 NA's
summary(WDI$NY.GDP.MKTP.KD.ZG) # 942 NA's

names(WDI)[4] <- 'stocks'
names(WDI)[5] <- 'fdi'
names(WDI)[6] <- 'sturnover'
names(WDI)[7] <- 'gdp.pc.gr'
names(WDI)[8] <- 'gdp.gr'

WDI[1:213, ] #delete non-countries (a.k.a. regions)
WDI <- WDI[-c(1:213), ]
WDI[1126:1147, ] #delete Cape Verde, it's called Cabo Verde now (both have same iso2c)
WDI <- WDI[-c(1126:1147), ]
sub <- subset(WDI, iso2c != "B8")
sub <- subset(sub, iso2c != "F1")
sub <- subset(sub, iso2c != "S1")
sub <- subset(sub, iso2c != "S2")
sub <- subset(sub, iso2c != "S3")
sub <- subset(sub, iso2c != "S4")
sub <- subset(sub, iso2c != "Z4")
WDI <- subset(sub, iso2c != "Z7")
rm(sub)


# merge the data sets
merge1 <- merge(WDI,aggrtdis,by=c("iso2c", "year"), all.x = TRUE)
merge2 <- merge(merge1,CAC,by=c("year"), all.x = TRUE)
merge3 <- merge(merge2,DAX,by=c("year"), all.x = TRUE)
merge4 <- merge(merge3,FTSE,by=c("year"), all.x = TRUE)
merge5 <- merge(merge4,IBOV,by=c("year"), all.x = TRUE)
merge6 <- merge(merge5,NIKKEI,by=c("year"), all.x = TRUE)
merge7 <- merge(merge6,OMRU,by=c("year"), all.x = TRUE)
rm(WDI, aggrtdis, CAC, DAX, FTSE, IBOV, OMRU, NIKKEI, merge1, merge2, merge3, merge4, merge5, merge6)

merge7$country.y <- NULL
names(merge7)[3] <- 'country'

merge8 <- merge(GPSA,merge7,by=c("country", "year"), all.x = TRUE)
rm(merge7, GPSA)

merge9 <- merge(merge8,unempl,by=c("iso3c", "Date"), all.x = TRUE)
rm(merge8, unempl)

merge10 <- merge(merge9,prvconsm,by=c("iso3c", "Date"), all.x = TRUE)
merge10$country.y <- NULL
names(merge10)[3] <- 'country'
rm(merge9, prvconsm)

#merge11 <- merge(merge10,deposit,by=c("Date"), all.x = TRUE)
#rm(deposit, merge10)

#merge12 <- merge(merge11,MRO,by=c("Date"), all.x = TRUE) #396 observations too much
#rm(MRO, merge11)


# creating percentage changes
Out <- change(merge12, Var = 'B',
              type = 'proportion',
              NewVar = 'PercentChange',
              slideBy = -2)
