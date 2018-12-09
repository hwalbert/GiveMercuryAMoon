#' Title
#'
#' @return
#' @export
#'
#' @examples
Aggregate_GDELT <- function(){
  HistoricalData <- fread(file = "~/GitHub/GiveMercuryAMoon/Data/GDELT_EGYPT.csv")
  HistoricalData$Date <- NULL

  GDELTData <- GetGDELT()

  GDELTData[] <- lapply(GDELTData[], as.character)

  AllEgyptGDELTData <- rbind(HistoricalData, GDELTData)


  AllEgyptGDELTData$Date <- substr(AllEgyptGDELTData$dateTimeDocument, 1, 10)
  AllEgyptGDELTData$Date <- as.Date(AllEgyptGDELTData$Date, format = "%Y-%m-%d")
  #AllEgyptGDELTData <<- AllEgyptGDELTData
  return(AllEgyptGDELTData)

}
