#' Title
#'
#' @return
#' @export
#'
#' @examples


update.MA.GSR <- function(writeResults = F) {
  file.MA <- "~\\GitHub\\GiveMercuryAMoon\\Data\\GSR_Military_Activity.csv"
  ma.data <- fread(file.MA)
  n.old <- nrow(ma.data)

  latest <- max( as.Date( x = ma.data$Event_Date, format = "%Y-%m-%d" ) )
  today <- Sys.Date()

  diff.year <- year(today) - year(latest)
  diff.month <- month(today) - month(latest)
  n.months <- (12*diff.year) + diff.month
  #cat(as.Date(latest, origin = "1970-01-01"), "\n")

  # Pull in GSR data from Github
  for ( i  in 0:n.months ) {
    # Set URL for the given month & year
    i.month <- month.name[( ( month(latest) + i ) %% 12 )]
    if( ( ( month(latest) + i ) %% 12 ) == 0 ) {
      i.month <- month.name[12]
    }
    i.year <- year(latest) + floor( ( month(latest) + i ) / 12.001)
    i.url <- paste0("https://raw.githubusercontent.com/planetmercury/mercury-challenge/master/data/gsr/ma_gsr/MA_",
                    i.month, "_", i.year, ".json")
    # Grab new data & check for any returned
    new.data <- fromJSON(i.url)

    output <- unique.data.frame( rbind(ma.data, new.data) )
    n.rows.added <- nrow(output) - n.old
    print(paste0("There were ", n.rows.added, " records to the GSR Military Activity dataset.") )
    if(writeResults == T){write.csv(file = file.MA, output, row.names = F)}

    return(output)
  }
}
