# Descriptive & Inferential Statistics

library(ggplot2)


# Set Working Directory
try(setwd("/Users/Lukas/Documents/Git/PairAssignment3/"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/PairAssignment3/"),silent=TRUE)
getwd()


# Dynamical Link to first R script file
source("PairAssignment3_Code/PairAssignment3_Variables_LagsPercChanges.R")

# Subsets for Country
DEU <- subset(panel, iso3c == "DEU") 
GBR <- subset(panel, iso3c == "GBR")
JPN <- subset(panel, iso3c == "JPN")
FRA <- subset(panel, iso3c == "FRA")

# Distribution of Dependent Variables
par(mfrow=c(2,2))
hist(DEU$DAX.Close,
     main="DAX Closing Point", 
     col="red", 
     breaks = 15,
     xlab = "DAX Closing Points",
     ylab = "Frequency")
hist(GBR$FTSE.Close,
     main="FTSE Closing Point", 
     col="blue", 
     breaks = 15,
     xlab = "FTSE Closing Points",
     ylab = "Frequency")
hist(JPN$NIK.Close,
     main="Nikkei Closing Point", 
     col="green", 
     breaks = 15,
     xlab = "Nikkei Closing Points",
     ylab = "Frequency")
hist(FRA$CAC.Close,
     main="CAC Closing Point", 
     col="orange", 
     breaks = 15,
     xlab = "CAC Closing Points",
     ylab = "Frequency")
par(mfrow=c(1,1))


# Correlation Plots of DV and Key IV (lag)
# Germany
ggplot(DEU, aes(x = L.GDPq.gr, y = DAX.Close)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("Germany: Correlation GDP growth (lag) | DAX Closing") +
  xlab("GDP Growth") +
  ylab("DAX Closing Point")
# UK
ggplot(GBR, aes(x = L.GDPq.gr, y = FTSE.Close)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("UK: Correlation GDP growth (lag) | FTSE Closing") +
  xlab("GDP Growth") +
  ylab("FTSE Closing Point")
# Japan
ggplot(JPN, aes(x = L.GDPq.gr, y = NIK.Close)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("Japan: Correlation GDP growth (lag) | Nikkei Closing") +
  xlab("GDP Growth") +
  ylab("Nikkei Closing Point")
# France
ggplot(FRA, aes(x = L.GDPq.gr, y = CAC.Close)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("France: Correlation GDP growth (lag) | CAC Closing") +
  xlab("GDP Growth") +
  ylab("CAC Closing Point")

# Correlation Plots of DV and Key IV (no lag)
ggplot(DEU, aes(x = GDPq.gr, y = DAX.Close)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("Germany: Correlation GDP growth (no lag) | DAX Closing") +
  xlab("GDP Growth") +
  ylab("DAX Closing Point")
# UK
ggplot(GBR, aes(x = GDPq.gr, y = FTSE.Close)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("UK: Correlation GDP growth (no lag) | FTSE Closing") +
  xlab("GDP Growth") +
  ylab("FTSE Closing Point")
# Japan
ggplot(JPN, aes(x = GDPq.gr, y = NIK.Close)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("Japan: Correlation GDP growth (no lag) | Nikkei Closing") +
  xlab("GDP Growth") +
  ylab("Nikkei Closing Point")
# France
ggplot(FRA, aes(x = GDPq.gr, y = CAC.Close)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("France: Correlation GDP growth (lag) | CAC Closing") +
  xlab("GDP Growth") +
  ylab("CAC Closing Point")