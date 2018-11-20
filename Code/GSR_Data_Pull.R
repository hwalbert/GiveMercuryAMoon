###   GSR Data Pull ###
library(jsonlite)
library(data.table)

# Date setup
start.dt <- as.Date("2015-05-01")
end.dt <- as.Date("2018-10-31")
d.year <- year(end.dt) - year(start.dt)
d.month <- month(end.dt) - month(start.dt)
n.month <- (12*d.year) + d.month


#################   CIVIL UNREST   #################


# Pull from Github Mercury Page
# ~ 1 minute for full pull (3 years ofdata)
v.names <- character(1)
for ( i  in 0:n.month ) {
  i.month <- month.name[( ( month(start.dt) + i ) %% 12 )]
  if( ( ( month(start.dt) + i ) %% 12 ) == 0 ) {
    i.month <- month.name[12]
  }
  i.year <- year(start.dt) + floor( ( month(start.dt) + i ) / 12.001)
  i.url <- paste0("https://raw.githubusercontent.com/planetmercury/mercury-challenge/master/data/gsr/cu_gsr/CU_", 
                         i.month, "_", i.year, ".json")
  #print(i.url)
  v.names[i+1] <- paste0("GSR.CU.", i.year, i.month)
  assign(x = v.names[i+1], value = fromJSON(i.url))
}

# Combine each month of GSR Data
df.GSR.CU.Full <- get(x = v.names[1])
for (name in v.names[-1] ) {
  df.GSR.CU.Full <- rbind(df.GSR.CU.Full, get(x = name) )
}

# Remove each month's df
rm(list = v.names)
rm(v.names)


#####################    MILITARY ACTION    ##########################

# PUll from Github Mercury Page
  # ~ 1 minute for full pull (3 years ofdata)
v.names <- character(1)
for ( i  in 0:n.month ) {
  i.month <- month.name[( ( month(start.dt) + i ) %% 12 )]
  if( ( ( month(start.dt) + i ) %% 12 ) == 0 ) {
    i.month <- month.name[12]
  }
  i.year <- year(start.dt) + floor( ( month(start.dt) + i ) / 12.001)
  i.url <- paste0("https://raw.githubusercontent.com/planetmercury/mercury-challenge/master/data/gsr/ma_gsr/MA_", 
                  i.month, "_", i.year, ".json")
  #print(i.url)
  v.names[i+1] <- paste0("GSR.MA.", i.year, i.month)
  assign(x = v.names[i+1], value = fromJSON(i.url))
}

# Combine each month of GSR Data
df.GSR.MA.Full <- get(x = v.names[1])
for (name in v.names[-1] ) {
  df.GSR.MA.Full <- rbind(df.GSR.MA.Full, get(x = name) )
}

# Remove each month's df
rm(list = v.names)
rm(c(v.names, d.year, d.month, end.dt, i, i.month, i.year, i.url, n.month, start.dt, end.dt, name) )


