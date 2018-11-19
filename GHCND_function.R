library(rnoaa)
library(data.table)
library(lubridate)
library(devtools)
library(stringr)
library(rNOMADS)
# EGYPT stations
dt.station<-fread('Mercury Challenge/code and data/Data/ghcnd-stations.csv')
vec.egy_stations<-dt.station[substr(ID,start = 1,stop = 2)=='EG']$ID


#' User friendly Daily Global Historical Climatology Network (GHCN-DAILY) function 
#' please refer to this link for more information: https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt
#' @param station.id      Station id from ghcnd-stations.csv 
#' @param start.dt        start date of the weather dataset
#' @param end.dt          end date of the weather dataset
#' @param str.measurement optional parameter that specifies the type of measurement wanted (eg TMAX, TMIN, TAVG, PRCP)
#'
#' @return data table object w/ station ID, date, element, value, MFLAG, QFLAG, SFLAG
#' @export 
#'
#' @examples fn.read.ghcnd(station.id='EG000062306', start.dt='2015-05-01', end.dt='2018-11-15', str.measurement='TMAX')
fn.read.ghcnd<-function(station.id,start.dt,end.dt,str.measurement=NA){
  dt<-data.table(ghcnd(station.id))[between(year,year(as_date(start.dt)),year(as_date(end.dt)))]
  if(nrow(dt)==0){
    return(data.table())
  }
  #break it down into rows
  lst.weather<-
    lapply(X = 1:31,FUN = function(X){
      vec.range=(1+X*4):(4+X*4)
      df<-data.frame(dt)[,c(1:4,vec.range)]
      colnames(df)<-c('ID','year','month','element','value','MFLAG','QFLAG','SFLAG')
      df$day<-X
      data.table(df)[,list(ID,date=as_date(paste0(year,"-",month,"-",day)),element,value,MFLAG,QFLAG,SFLAG)]
    })
  dt.weather<-rbindlist(lst.weather)[!is.na(date)][order(date)][between(date,as_date(start.dt),as_date(end.dt))]
  if(!is.na(str.measurement)){
    return(dt.weather[element==str.measurement])
  }
  return(dt.weather)
}

dt.egy_weather<-rbindlist(lapply(X=vec.egy_stations,FUN=fn.read.ghcnd,start.dt='2015-05-01',end.dt=Sys.Date()))
