Check_GDELT_Dates <- function(){
  #Read in the current historical data for GDELT
  AllGDELT <- fread(file = "~/GitHub/GiveMercuryAMoon/Data/GDELT_EGYPT.csv")
  #Fix the Date column
  AllGDELT$Date <- as.Date(AllGDELT$Date, format = "%Y-%m-%d")
  #Get the list of all unique dates in the dataset
  dates <- unique(AllGDELT$Date)
  #We want the list of current dates not in the dataset

  if(max(dates) == Sys.Date()){
    NewDatesNeeded <- ""
  }else{

  NewDatesNeeded <- seq.Date(from=as_date(max(dates))+days(1),
                             to=Sys.Date(),by = 'day')

  }
  return(NewDatesNeeded)
}


#GetGDELT(SpecificDates = NewDatesNeeded, Engine = "Egypt")
