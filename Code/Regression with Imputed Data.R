# call libraries
library(dplyr)
library(data.table)
library(zoo)

data <- fread("C:/Users/pprasad001/Documents/GitHub/GiveMercuryAMoon/Data/Mercury_ML_20181201.csv")
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
data$avgnegtone <- (data$Neg.Tone.t10+ data$Neg.Tone.t9+ data$Neg.Tone.t8+ data$Neg.Tone.t7+ data$Neg.Tone.t6+ data$Neg.Tone.t5+ data$Neg.Tone.t4) / 7


# create linear regression
data2 <- select(data, CU.Count,temp, is_holiday,avgnegtone)

linreg1 <- lm(CU.Count~., data = data2)

summary(linreg1) 
      
      
predictions <- predict.lm(linreg1,data2)

MSE <- sum((predictions - data2$CU.Count)^2, na.rm = T)/nrow(data2)
MSE1 <- sum((lag(data2$CU.Count) - data2$CU.Count)^2, na.rm = T)/nrow(data2)

error <- (predictions-data2$CU.Count)
plot(error)
plot(predictions)
