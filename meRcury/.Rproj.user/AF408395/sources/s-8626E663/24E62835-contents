# Create DF.ML for ML Analysis
library(data.table)
library(plyr)
library(dplyr)
library(devtools)
# install_github('abresler/gdeltr2')
library(gdeltr2)
# We start with GSR.CU, GSR.MA, Weather, weekend, holidays & GDelt Data, filtered for country and date range
# fread...
GDELT <-fread('GitHub/GiveMercuryAMoon/Data/GDELT_EGYPT.csv')
GSR.CU <- fread('GitHub/GiveMercuryAMoon/Data/GSR_CivilUnrest_Full_thru20180930.csv')
GSR.MA <- fread('GitHub/GiveMercuryAMoon/Data/GSR_MilitaryAction_Full_thru20180930.csv')
weather <-fread('GitHub/GiveMercuryAMoon/Data/Egypt_historical_weather.csv')
date_info <- fread('GitHub/GiveMercuryAMoon/Data/weekends_and_holidays.csv')

GDELT$Date <- as.Date(GDELT$Date, format = "%Y-%m-%d")
GSR.CU$Event_Date <- as.Date(GSR.CU$Event_Date, format = "%m/%d/%Y")
GSR.MA$Event_Date <- as.Date(GSR.MA$Event_Date, format = "%m/%d/%Y")


# Each row is one day...
all.days <- seq.Date(from = min(GSR.CU$Event_Date), to = max(GSR.CU$Event_Date), by = 1)
DF.ML <- data.frame("DATE" = all.days)

# Get CU count for each day...
cu.dt <-count(GSR.CU$Event_Date)
DF.ML <- merge(x = DF.ML, y = cu.dt, by.x = "DATE", by.y = "x", all.x = T)
DF.ML[is.na(DF.ML$freq),]$freq <- 0
names(DF.ML) <- c("DATE", "CU.Count")

# Get MA count for each day...
ma.dt <-count(GSR.MA$Event_Date)
DF.ML <- merge(x = DF.ML, y = ma.dt, by.x = "DATE", by.y = "x", all.x = T)
DF.ML[is.na(DF.ML$freq),]$freq <- 0
names(DF.ML)[3] <- "MA.Count"

# Get GDELT doc count for each day
gd.dt <- count(GDELT$Date)
DF.ML <- merge(x = DF.ML, y = gd.dt, by.x = "DATE", by.y = "x", all.x = T)
DF.ML[is.na(DF.ML$freq),]$freq <- 0
names(DF.ML)[4] <- "GDELT.Doc.Count"

rm(cu.dt, ma.dt, gd.dt, all.days)

# Parse tone and get daily average
library(gdeltr2)
tones <- cbind(GDELT$Date, as.data.frame(parse_gkg_mentioned_article_tone(gdelt_data = GDELT)) )
names(tones) <- c("Date", "idGKG", "Tone", "Pos.Tone", "Neg.Tone", "Polarity", "Active.Refs", "Self.Ref", "Word.Count")
attach(tones)
agg <- aggregate(x = tones, by = list(Date), FUN = mean)[,-c(1,3)]
detach(tones)

DF.ML <- merge(x = DF.ML, y = agg, by.x = "DATE", by.y = "Date", all.x = T)

# Find days with "radical tone" (e.g., negative tone for a day outside 2 sd of mean)
mu <- mean(DF.ML$Neg.Tone, na.rm = T)
sigma <- sd(DF.ML$Neg.Tone, na.rm = T)
# Write assigment function for this later...
DF.ML$SD1.Neg.Tone <- DF.ML$Neg.Tone >= ( mu + sigma )
DF.ML$SD2.Neg.Tone <- DF.ML$Neg.Tone >= ( mu + 2*sigma )
DF.ML$SD3.Neg.Tone <- DF.ML$Neg.Tone >= ( mu + 3*sigma )

# Tone
mu <- mean(DF.ML$Tone, na.rm = T)
sigma <- sd(DF.ML$Tone, na.rm = T)
#
DF.ML$SD1.Tone <- DF.ML$Tone >= ( mu + sigma )
DF.ML$SD2.Tone <- DF.ML$Tone >= ( mu + 2*sigma )
DF.ML$SD3.Tone <- DF.ML$Tone >= ( mu + 3*sigma )

# Polarity
mu <- mean(DF.ML$Polarity, na.rm = T)
sigma <- sd(DF.ML$Polarity, na.rm = T)
# 
DF.ML$SD1.Polarity <- DF.ML$Polarity >= ( mu + sigma )
DF.ML$SD2.Polarity <- DF.ML$Polarity >= ( mu + 2*sigma )
DF.ML$SD3.Polarity <- DF.ML$Polarity >= ( mu + 3*sigma )


# CU and MA Moving Averages
CU <- DF.ML$CU.Count
DF.ML$CU.mavg.2day <- numeric(length = nrow(DF.ML))
DF.ML$CU.mavg.3day <- numeric(length = nrow(DF.ML))
DF.ML$CU.mavg.5day <- numeric(length = nrow(DF.ML))
DF.ML$CU.mavg.8day <- numeric(length = nrow(DF.ML))
for ( i in 1:nrow(DF.ML) ) {
  DF.ML$CU.mavg.2day[i] <- ifelse(test = ( (i-1) <= 0 ), 
                            yes = NA, no = ( sum(CU[(i-1):i], na.rm = T) / 2 ) )
  DF.ML$CU.mavg.3day[i] <- ifelse(test = ( (i-2) <= 0 ), 
                            yes = NA, no = ( sum(CU[(i-2):i], na.rm = T) / 3 ) )
  DF.ML$CU.mavg.5day[i] <- ifelse(test = ( (i-4) <= 0 ), 
                            yes = NA, no = ( sum(CU[(i-4):i], na.rm = T) / 5 ) )
  DF.ML$CU.mavg.8day[i] <- ifelse(test = ( (i-7) <= 0 ), 
                            yes = NA, no = ( sum(CU[(i-7):i], na.rm = T) / 8 ) )
}
# MA...
MA <- DF.ML$MA.Count
DF.ML$MA.mavg.2day <- numeric(length = nrow(DF.ML))
DF.ML$MA.mavg.3day <- numeric(length = nrow(DF.ML))
DF.ML$MA.mavg.5day <- numeric(length = nrow(DF.ML))
DF.ML$MA.mavg.8day <- numeric(length = nrow(DF.ML))
for ( i in 1:nrow(DF.ML) ) {
  DF.ML$MA.mavg.2day[i] <- ifelse(test = ( (i-1) <= 0 ), 
                               yes = NA, no = ( sum(MA[i:(i+1)], na.rm = T) / 2 ) )
  DF.ML$MA.mavg.3day[i] <- ifelse(test = ( (i-2) <= 0 ), 
                               yes = NA, no = ( sum(MA[i:(i+2)], na.rm = T) / 3 ) )
  DF.ML$MA.mavg.5day[i] <- ifelse(test = ( (i-4) <= 0 ), 
                               yes = NA, no = ( sum(MA[i:(i+4)], na.rm = T) / 5 ) )
  DF.ML$MA.mavg.8day[i] <- ifelse(test = ( (i-7) <= 0 ), 
                               yes = NA, no = ( sum(MA[i:(i+7)], na.rm = T) / 8 ) )
}

# Re-ordering columns :)
my.order <- c(1,2,21:24,3,25:28,4,5,15:17,6,7,12:14,8,18:20,9:11)
DF.ML <- DF.ML[,my.order]

rm(tones, agg, CU, MA, i, mu, sigma, my.order)

# Plotting CU Moving Averages
plot(x = DF.ML$DATE, y = DF.ML$CU.Count, type = "l", 
     xlim = c( as.Date("2018-01-01"), as.Date("2018-10-16") ) )
# Ex.
lines(x = DF.ML$DATE, y = DF.ML$CU.mavg.5day, type = "l", col = "red")

