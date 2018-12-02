
GetGDELT <- function(DaysBack = 1, writeResults = T, SpecificDates = NULL){

  if(is.null(SpecificDates)){
    dates = DatesForGDELT(daysBack = DaysBack)
  }else{
    dates = SpecificDates
  }


  #This code grabs the Global Knowledge Graph data for the dates specified by the DatesForGDELT() function
  GDELTData <-
    get_data_gkg_days_detailed(
      dates = dates,
      #dates = c("2018-09-08"),
      table_name = 'gkg',
      return_message = T
    )

  dates <<- dates
  GDELTData <<- GDELTData

  #dates <- gsub("-", "_", dates)
  if(writeResults==T){
    FolderForResults <- choose.dir(caption = "Select where to store GDELT Results")
    fwrite(GDELTData, file = paste(FolderForResults, "/GDELTDataDate", dates, ".csv", sep = ""))
  }

}


