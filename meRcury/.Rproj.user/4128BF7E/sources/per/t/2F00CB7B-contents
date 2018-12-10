#' Title
#'
#' @return
#' @export
#'
#' @examples

library(jsonlite)
library(data.table)
library(curl)

update.CU.GSR <- function() {
  file <- "~\\GitHub\\GiveMercuryAMoon\\Data\\GSR_Civil_Unrest.csv"
  cu.data <- fread(file.CU)
  n.old <- nrow(cu.data)
  
  latest <- max( as.Date( x = cu.data$Event_Date, format = "%m/%d/%Y" ) )
  today <- Sys.Date()
  
  diff.year <- year(today) - year(latest)
  diff.month <- month(today) - month(latest)
  n.months <- (12*diff.year) + diff.month
  
  # Pull in GSR data from Github
  for ( i  in 0:n.months ) {
    # Set URL for the given month & year
    i.month <- month.name[( ( month(latest) + i ) %% 12 )]
    if( ( ( month(latest) + i ) %% 12 ) == 0 ) {
      i.month <- month.name[12]
    }
    i.year <- year(latest) + floor( ( month(latest) + i ) / 12.001)
    i.url <- paste0("https://raw.githubusercontent.com/planetmercury/mercury-challenge/master/data/gsr/cu_gsr/CU_", 
                    i.month, "_", i.year, ".json")
    # Grab new data & check for any returned
    new.data <- fromJSON(i.url)
    
    output <- unique.data.frame( rbind(cu.data, new.data) ) 
    n.rows.added <- nrow(output) - n.old
    print(paste0("There were ", n.rows.added, " records to the GSR Civil Unrest dataset.") )
    return(output)  
  }
}
