# Descriptive & Inferential Statistics

library(Rmisc)
library(ggplot2)
library(magrittr)
library(grid)


# Set Working Directory
try(setwd("/Users/Lukas/Documents/Git/PairAssignment3/"),silent=TRUE)
try(setwd("C:/Users/Dani/Documents/GitHub2/PairAssignment3/"),silent=TRUE)
getwd()


# Dynamical Link to first R script file
source("PairAssignment3_Code/PairAssignment3_Variables_LagsPercChanges.R")

# Subset Data
data <- as.data.frame(panel)
USA <- subset(data, iso3c == "USA")
DEU <- subset(data, iso3c == "DEU")
GBR <- subset(data, iso3c == "GBR")
JPN <- subset(data, iso3c == "JPN")
FRA <- subset(data, iso3c == "FRA")
ALL <- subset(data, iso3c == "JPN" | iso3c == "DEU" | iso3c == "GBR" | iso3c == "FRA" | iso3c == "USA")
ALL <- ALL[ ,c(1,2,8,14,20,26,2:7,9:13,15:19,21:25,27:41)]


ALL::scatterplotMatrix(GDPq.gr)

# Dependent Variable (Equity Prices): Plotting the course over time
# DEU
p1 <- ggplot(data = DEU, aes(x = Date, y = DAX.Close, group = iso3c, color = iso3c)) + geom_line() + ggtitle("DAX") + labs(x = "Quarters", y = "Av. Closing Value")
p1 + scale_colour_discrete(name  ="Country", labels=c("Germany"))
# JPN
p2 <- ggplot(data = JPN, aes(x = Date, y = NIK.Close, group = iso3c, color = iso3c)) + geom_line() + ggtitle("NIKKEI") + labs(x = "Quarters", y = "Av. Closing Value")
p2 + scale_colour_discrete(name  ="Country", labels=c("Japan"))
# FRA
p3 <- ggplot(data = FRA, aes(x = Date, y = CAC.Close, group = iso3c, color = iso3c)) + geom_line() + ggtitle("CAC40") + labs(x = "Quarters", y = "Av. Closing Value")
p3 + scale_colour_discrete(name  ="Country", labels=c("France"))
# UK
p4 <- ggplot(data = GBR, aes(x = Date, y = FTSE.Close, group = iso3c, color = iso3c)) + geom_line() + ggtitle("FTSE") + labs(x = "Quarters", y = "Av. Closing Value")
p4 + scale_colour_discrete(name  ="Country", labels=c("Britain"))
multiplot(p1, p2, p3, p4, row=4)

# Key Independent Variable: Quarterly GDP growth (incl. USA)
p5 <- ggplot(data = DEU, aes(x = Date, y = GDPq.gr, group, group = iso3c, color = iso3c)) + geom_line() + ggtitle("Quarterly GDP growth") + labs(x = "Quarters", y = "Percentage Change to prev. Q.")
p5 + scale_colour_discrete(name  ="Country", labels=c("Germany"))

p6 <- ggplot(data = JPN, aes(x = Date, y = GDPq.gr, group, group = iso3c, color = iso3c)) + geom_line() + ggtitle("Quarterly GDP growth") + labs(x = "Quarters", y = "Percentage Change to prev. Q.")
p6 + scale_colour_discrete(name  ="Country", labels=c("Japan"))

p7 <- ggplot(data = FRA, aes(x = Date, y = GDPq.gr, group, group = iso3c, color = iso3c)) + geom_line() + ggtitle("Quarterly GDP growth") + labs(x = "Quarters", y = "Percentage Change to prev. Q.")
p7 + scale_colour_discrete(name  ="Country", labels=c("France"))

p8 <- ggplot(data = GBR, aes(x = Date, y = GDPq.gr, group, group = iso3c, color = iso3c)) + geom_line() + ggtitle("Quarterly GDP growth") + labs(x = "Quarters", y = "Percentage Change to prev. Q.")
p8 + scale_colour_discrete(name  ="Country", labels=c("Britain"))

p9 <- ggplot(data = USA, aes(x = Date, y = GDPq.gr, group, group = iso3c, color = iso3c)) + geom_line() + ggtitle("Quarterly GDP growth") + labs(x = "Quarters", y = "Percentage Change to prev. Q.")
p9 + scale_colour_discrete(name  ="Country", labels=c("USA"))

p10 <- ggplot(data = ALL, aes(x = Date, y = GDPq.gr, group = iso3c, color = iso3c)) + geom_line() + ggtitle("Quarterly GDP growth") + labs(x = "Quarters", y = "Percentage Change to prev. Q.")
p10 + scale_colour_discrete(name  ="Country", labels=c("Germany", "France", "Britain", "Japan", "USA"))

multiplot(p5, p6, p7, p8, p9, p10, row=6)

# Dependent Variable: loop for Mean
for (i in 3:6) {
  ALL[, i] %>% 
    mean() %>%
    paste(names(ALL)[i], ., "\n") %>%
    cat()
}

# Dependent Variable: loop for Median
for (i in 3:6) {
  ALL[, i] %>% 
    median() %>%
    paste(names(ALL)[i], ., "\n") %>%
    cat()
}

# Dependent Variable: loop for St. Dev. 
for (i in 3:6) {
  ALL[, i] %>% #Error: could not find function "%>%"
    sd() %>%
    paste(names(ALL)[i], ., "\n") %>%
    cat()
}

# Distribution of Dependent Variables
par(mfrow=c(2,2))
hist(DEU$DAX.Close,
     main="DAX Av. Closing Value", 
     col="red", 
     breaks = 15,
     xlab = "DAX Closing Values",
     ylab = "Frequency")
hist(JPN$NIK.Close,
     main="Nikkei Closing Value", 
     col="green", 
     breaks = 15,
     xlab = "Nikkei Closing Values",
     ylab = "Frequency")
hist(FRA$CAC.Close,
     main="CAC Closing Value", 
     col="orange", 
     breaks = 15,
     xlab = "CAC Closing Values",
     ylab = "Frequency")
hist(GBR$FTSE.Close,
     main="FTSE Closing Value", 
     col="blue", 
     breaks = 15,
     xlab = "FTSE Closing Values",
     ylab = "Frequency")

# Correlation Plots of DV and Key IV (lag)
# DEU
p11 <- ggplot(DEU, aes(x = L.GDPq.gr, y = DAX.Close)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("Germany: Correlation GDP growth (lag) | DAX Av. Closing Value") +
  xlab("GDP Growth") +
  ylab("DAX Av. Closing Value")
# JPN
p12 <- ggplot(JPN, aes(x = L.GDPq.gr, y = NIK.Close)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("Japan: Correlation GDP growth (lag) | Nikkei Av. Closing Value") +
  xlab("GDP Growth") +
  ylab("Nikkei Av. Closing Value")
# FRA
p13 <- ggplot(FRA, aes(x = L.GDPq.gr, y = CAC.Close)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("France: Correlation GDP growth (lag) | CAC Av. Closing Value") +
  xlab("GDP Growth") +
  ylab("CAC Av. Closing Value")
# UK
p14 <- ggplot(GBR, aes(x = L.GDPq.gr, y = FTSE.Close)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("UK: Correlation GDP growth (lag) | FTSE Av. Closing Value") +
  xlab("GDP Growth") +
  ylab("FTSE Av. Closing Value")
multiplot(p11, p12, p13, p14, cols = 2)

# Correlation Plots of DV and Key IV (no lag)
# DEU
p15 <- ggplot(DEU, aes(x = GDPq.gr, y = DAX.Close)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("Germany: Correlation GDP growth (no lag) | DAX Av. Closing Value") +
  xlab("GDP Growth") +
  ylab("DAX Av. Closing Value")
# JPN
p16 <- ggplot(JPN, aes(x = GDPq.gr, y = NIK.Close)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("Japan: Correlation GDP growth (no lag) | Nikkei Av. Closing Value") +
  xlab("GDP Growth") +
  ylab("Nikkei Av. Closing Value")
# FRA
p17 <- ggplot(FRA, aes(x = GDPq.gr, y = CAC.Close)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("France: Correlation GDP growth (lag) | CAC Av. Closing Value") +
  xlab("GDP Growth") +
  ylab("CAC Av. Closing Value")
# UK
p18 <- ggplot(GBR, aes(x = GDPq.gr, y = FTSE.Close)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("UK: Correlation GDP growth (no lag) | FTSE Av. Closing Value") +
  xlab("GDP Growth") +
  ylab("FTSE Av. Closing Value")
multiplot(p15, p16, p17, p18, cols = 2)

# Inferential Statistics

# Basic Models 
M1 <- lm(DAX.Close ~ GDPq.gr, data = DEU)
summary(M1)
# Heteroscedasticity Diagnose
plot(M1, which = 1) # Basically does the same as correlation matrix above
M2 <- lm(NIK.Close ~ GDPq.gr, data = JPN)
summary(M2)

M3 <- lm(CAC.Close ~ GDPq.gr, data = FRA)
summary(M3)

M4 <- lm(FTSE.Close ~ GDPq.gr, data = GBR)
summary(M4)



