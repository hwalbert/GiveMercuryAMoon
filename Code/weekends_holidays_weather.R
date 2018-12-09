library(lubridate)
library(data.table)
# install.packages('GSODR')
library(GSODR)
holidays<-fread('GitHub/GiveMercuryAMoon/Data/holidays.csv',header = T)[,1:3]
holidays[1,]$Date<-'1/3/2015'
holidays$Date<-as.Date(holidays$Date,format='%m/%d/%Y')
holidays<-unique(holidays[,list(Date,is_holiday=1)])
weekends_and_holidays<-
  data.table(Date=seq.Date(from=as_date('2015-01-01'),to=as_date('2019-12-31'),by='days'))
weekends_and_holidays[,is_weekend:=ifelse(wday(Date)%in% c(6,7),1,0)]
weekends_and_holidays<-merge(weekends_and_holidays,holidays,
                              by=c('Date'),all.x = T)
weekends_and_holidays$is_holiday<-
  ifelse(is.na(weekends_and_holidays$is_holiday),
        0,1)

weather.egypt<-data.table(GSODR::get_GSOD(years = c(2015,2016,2017,2018),
                                          station = c('623660-99999'),
                                          country = 'Egypt'))
summary(data.table(weather.egypt))
weather.cairo<-
  merge(data.table(dt=seq.Date(from=as_date('2015-01-01'),
                               to=Sys.Date(),
                               by='day')),
        weather.egypt[STNID=='623660-99999',
                      list(dt=as_date(YEARMODA),
                           temp=TEMP,
                           prcp=PRCP)],
        by=c('dt'),all.x = T)
fwrite(x = weekends_and_holidays,'GitHub/GiveMercuryAMoon/Data/weekends_and_holidays.csv')

data.table(GSODR::get_GSOD(years = c(2018)))

dt<-.Last.value
summary(dt[CTRY=='US'][STATE=='VA'][grepl('RONALD REAGAN WASHINGTON NATL AP',STN_NAME)]$PRCP)
