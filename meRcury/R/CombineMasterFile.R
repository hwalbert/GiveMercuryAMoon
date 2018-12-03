# EgyptHolidays <- fread(file = "~/GitHub/GiveMercuryAMoon/Data/weekends_and_holidays.csv", colClasses = "character")
# EgyptHolidays$Date <- as.Date(EgyptHolidays$Date, format = "%Y-%m-%d")

Combine_MasterFile <- function(){
  AllEgyptGDELTData <- Aggregate_GDELT()
  #Connect Egypt GDELT data to Holiday and weekend data

  # Each row is one day...
  all.days <- seq.Date(from = min(AllEgyptGDELTData$Date), to = max(AllEgyptGDELTData$Date), by = 1)
  DF.ML <- data.frame("DATE" = all.days)

















  MasterFile <- merge(AllEgyptGDELTData, EgyptHolidays, by = "Date", all = T)


  #Connect to GSR Civil Unrest counts
  CUData <- Get_GSR_CU_Data()
  MAData <- Get_GSR_MA_Data()

  #Filter CU and MA data for only Egypt
  CUDataEgypt <- subset(CUData, Country == "Egypt")
  MADataEgypt <- subset(MAData, Country == "Egypt")

  CUDataEgypt <- summarize(group_by(CUDataEgypt, Event_Date), COUNT = n())
  CUDataEgypt$Event_Date <- as.Date(CUDataEgypt$Event_Date, format = "%Y-%m-%d")
  names(CUDataEgypt)[1] <- "Date"

  MADataEgypt <- summarize(group_by(MADataEgypt, Event_Date), COUNT = n())
  MADataEgypt$Event_Date <- as.Date(MADataEgypt$Event_Date, format = "%Y-%m-%d")
  names(MADataEgypt)[1] <- "Date"


  MasterFile <- merge(MasterFile, CUDataEgypt, by = "Date", all = T)
  MasterFile <- merge(MasterFile, MADataEgypt, by = "Date", all = T)
  return(MasterFile)

}
