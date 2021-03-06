# call libraries
library(dplyr)
library(data.table)
library(zoo)

data <- fread("~/GitHub/GiveMercuryAMoon/Data/Mercury_ML_20181201.csv")
data$DATE <- as.Date(data$DATE, format = "%Y-%m-%d")
data$DayOfWeek <- as.factor(weekdays(data$DATE))
data$is_weekend <- ifelse(as.character(data$DayOfWeek) %in% c("Friday", "Saturday"), 1,0)

data$Neg.Tone.t1 <- na.approx(data$Neg.Tone.t1)
data$Neg.Tone.t2 <- na.approx(data$Neg.Tone.t2)
data$Neg.Tone.t3 <- na.approx(data$Neg.Tone.t3)
data$Neg.Tone.t4 <- na.approx(data$Neg.Tone.t4)
data$Neg.Tone.t5 <- na.approx(data$Neg.Tone.t5)
data$Neg.Tone.t6 <- na.approx(data$Neg.Tone.t6)
data$Neg.Tone.t7 <- na.approx(data$Neg.Tone.t7)
data$Neg.Tone.t8 <- na.approx(data$Neg.Tone.t8)
data$Neg.Tone.t9 <- na.approx(data$Neg.Tone.t9)
data$Neg.Tone.t10 <- na.approx(data$Neg.Tone.t10)
data$Neg.Tone.t11 <- na.approx(data$Neg.Tone.t11)
data$Neg.Tone.t12 <- na.approx(data$Neg.Tone.t12)
data$Neg.Tone.t13 <- na.approx(data$Neg.Tone.t13)
data$Neg.Tone.t14 <- na.approx(data$Neg.Tone.t14)
data$Neg.Tone.t15 <- na.approx(data$Neg.Tone.t15)
data$Neg.Tone.t16 <- na.approx(data$Neg.Tone.t16)

# data$prcpdummy <- ifelse(data$prcp != 0,"rain","norain")
# data$prcpdummy <- as.factor(data$prcpdummy)
# summary(data$prcpdummy)
# table(data$prcpdummy)


#data[] <- lapply(data, na.approx)
data$avgnegtone <- (data$Neg.Tone.t7+ data$Neg.Tone.t6+ data$Neg.Tone.t5+ data$Neg.Tone.t4+ data$Neg.Tone.t3+ data$Neg.Tone.t2+ data$Neg.Tone.t1) / 7


# create linear regression
data2 <- select(data, CU.Count,temp, is_weekend, avgnegtone)
# data2$DATE < NULL
# data2$MA.Count <- NULL

linreg1 <- lm(CU.Count~., data = data2)

summary(linreg1)

predictions <- predict.lm(linreg1,data2)

GDELT <- Aggregate_GDELT()

tones <- cbind(GDELT$Date, as.data.frame(parse_gkg_mentioned_article_tone(gdelt_data = GDELT)) )
names(tones) <- c("Date", "idGKG", "Tone", "Pos.Tone", "Neg.Tone", "Polarity", "Active.Refs", "Self.Ref", "Word.Count")
attach(tones)
agg <- aggregate(x = tones, by = list(Date), FUN = mean)[,-c(1,3)]
detach(tones)
negTone <- mean(agg$Neg.Tone[(length(agg$Neg.Tone)-6):length(agg$Neg.Tone)], na.rm = T)


ModelInput <- data.frame(
  temp = get_weather()[3,2],
  is_weekend = ifelse(weekdays(Sys.Date() + 3) %in% c("Friday", "Saturday"),1,0),
  avgnegtone = negTone)

PREDICTION <- predict.lm(linreg1, ModelInput)
sd()



MSE <- sum((predictions - data2$CU.Count)^2, na.rm = T)/nrow(data2)
MSE1 <- sum((lag(data2$CU.Count) - data2$CU.Count)^2, na.rm = T)/nrow(data2)

error <- (predictions-data2$CU.Count)
plot(error)
plot(predictions)
