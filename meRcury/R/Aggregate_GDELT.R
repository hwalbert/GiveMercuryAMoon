#' Title
#'
#' @return
#' @export
#'
#' @examples
Aggregate_GDELT <- function(){
  filelocation <- "~/GitHub/GiveMercuryAMoon/Data/GDELT_EGYPT.csv"
  HistoricalData <- fread(file = filelocation)
  HistoricalData$Date <- NULL

  GDELTData <- GetGDELT(writeResults = F)

  GDELTData[] <- lapply(GDELTData[], as.character)

  AllEgyptGDELTData <- rbind(HistoricalData, GDELTData)


  AllEgyptGDELTData$Date <- substr(AllEgyptGDELTData$dateTimeDocument, 1, 10)
  AllEgyptGDELTData$Date <- as.Date(AllEgyptGDELTData$Date, format = "%Y-%m-%d")
  #AllEgyptGDELTData <<- AllEgyptGDELTData
  fwrite(AllEgyptGDELTData, file = filelocation)
  return(AllEgyptGDELTData)

}
