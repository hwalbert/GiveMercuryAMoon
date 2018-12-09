#' @param DaysBack
#'
#' @param writeResults
#' @param SpecificDates
#' @param location
#'
#' @title ETL Egypt GDELT data
#' @description A giant loop that downloads gdelt data for a given month, extracts the egypt related info, combines it, saves it to a master folder, and deletes them if all of the above is done successfully
#'
#' @return Returns a data frame to the global environment showing the debit/credit results.
#' @examples DebitCreditCompare(MasterFile)


GetGDELT <- function(DaysBack = NULL, writeResults = T, location = "~/GitHub/GiveMercuryAMoon/Data", Engine = "Egypt"){
cat("Preparing the Full Global Knowledge Graph \n")

  if(!is.null(DaysBack)){
    dates = DatesForGDELT(daysBack = DaysBack)
  }else{
  dates <- Check_GDELT_Dates()
  if(dates == ""){return(data.frame())}
  }

  #This code grabs the Global Knowledge Graph data for the dates specified by the DatesForGDELT() function
  GDELTData <-
    suppressMessages(get_data_gkg_days_detailed(
      dates = dates,
      # dates = c("2018-09-08"),
      table_name = 'gkg',
      return_message = T
    ))

  #if(nrow(GDELTData) < 1){stop("The pulled GDELT Data has no rows.")}

  if(Engine == "Egypt"){
    GDELTData<-ExtractEGYPT(GDELTData)

    #dates <- gsub("-", "_", dates)
    if(writeResults==T){
      FolderForResults <- location
      fwrite(GDELTData, file = paste(FolderForResults, "/GDELTDataDate", dates, ".csv", sep = ""))
    }
  }


return(GDELTData)

}
