#' Title
#'
#' @return
#' @export
#'
#' @examples

library(jsonlite)
library(data.table)

update.GSR <- function(CU.Update = F, MA.Update = F, today = NA) {
  # File locations are fixed w/i this directory:
  # setwd("~")
  file.CU <- "~\\GitHub\\GiveMercuryAMoon\\Data\\GSR_Civil_Unrest.csv"
  file.MA <- "~\\GitHub\\GiveMercuryAMoon\\Data\\GSR_Military_Activity.csv"
  
  # Setup Variables
  today <- ifelse(test = is.na(today), yes = Sys.Date(), no = today) 
  
  
  # Update Civil Unrest
  if (CU.Update) {
    cu.data <- fread(file.CU)
    n.old <- nrow(cu.data)
    # Get latest date from existing data
    last.dt <- max( as.Date( cu.data$Event_Date, format = "%m/%d/%Y") )
    # Set up months to be pulled
    diff.year <- year(today, origin = as.Date("2018-01-01")) - year(last.dt, origin = as.Date("2018-01-01"))
    diff.month <- month(today) - month(last.dt)
    n.months <- (12*diff.year) + diff.month
    # Pull in GSR data from Github
    for ( i  in 0:n.months ) {
      # Set URL for the given month & year
      i.month <- month.name[( ( month(last.dt) + i ) %% 12 )]
      if( ( ( month(last.dt) + i ) %% 12 ) == 0 ) {
        i.month <- month.name[12]
      }
      i.year <- year(last.dt) + floor( ( month(last.dt) + i ) / 12.001)
      i.url <- paste0("https://raw.githubusercontent.com/planetmercury/mercury-challenge/master/data/gsr/cu_gsr/CU_", 
                      i.month, "_", i.year, ".json")
      # Grab new data & check for any returned
      new.data <- fromJSON(i.url)
      if (any(new.data)) {
        cu.data <- rbind(cu.data, new.data)
      } else {
        print(paste0("There is no CU GSR data for ", i.month, " ", i.year ) ) 
      }
    }
    n.rows.added <- nrow(cu.data) - n.old
    print(paste0("There were ", n.rows.added, " to the GSR Civil Unrest dataset.") )
    return(cu.data)
  }
  
  # Update Military Activity
  if (MA.Update) {
    ma.data <- fread(file.MA)
    n.old <- nrow(ma.data)
    # Get latest date from existing data
    last.dt <- max( as.Date( ma.data$Event_Date, format = "%Y-%m-%d" ) )
    # Set up months to be pulled
    diff.year <- year(today, origin = as.Date("2018-01-01")) - year(last.dt, origin = as.Date("2018-01-01"))
    diff.month <- month(today) - month(last.dt)
    n.months <- (12*diff.year) + diff.month
    # Pull in GSR data from Github
    for ( i  in 0:n.months ) {
      # Set URL for the given month & year
      i.month <- month.name[( ( month(last.dt) + i ) %% 12 )]
      if( ( ( month(last.dt) + i ) %% 12 ) == 0 ) {
        i.month <- month.name[12]
      }
      i.year <- year(last.dt) + floor( ( month(last.dt) + i ) / 12.001)
      i.url <- paste0("https://raw.githubusercontent.com/planetmercury/mercury-challenge/master/data/gsr/ma_gsr/MA_", 
                      i.month, "_", i.year, ".json")
      # Grab new data & check for any returned
      new.data <- fromJSON(i.url)
      if (any(new.data)) {
        cu.data <- rbind(ma.data, new.data)
      } else {
        print(paste0("There is no MA GSR data for ", i.month, " ", i.year ) ) 
      }
    }
    n.rows.added <- nrow(ma.data) - n.old
    print(paste0("There were ", n.rows.added, " to the GSR Military dataset.") )
    return(ma.data)
  }
  
}
