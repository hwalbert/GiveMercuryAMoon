#' Weather pull uses rvest
#'
#' @param when
#' @param where
#'
#' @return
#' @export
#'
#' @examples
get_weather<-function(where='CAI:9:EG'){
  library(rvest)

  # pull the days from weather.com's 5-day weather forecast
  days<-html_text(html_nodes(read_html(paste0('https://weather.com/weather/5day/l/',where)),
                             '.clearfix.day-detail'))
  vec.days<-days[(length(days)-4):length(days)]
  # for the week where dec and jan overlaps
  if(grepl('DEC',vec.days)&grepl('JAN',vec.days)){
    yr<-ifelse(grepl('DEC',vec.days),'2018','2019')
  }else{
    yr<-year(Sys.Date())
  }
  vec.date<-as.Date(paste0(yr," ",Hmisc::capitalize(tolower(vec.days))),
                    format="%Y %b %d")

  # pull the temperatures
  vec.temp<-
    sapply(X = 2:6,
           FUN = function(X){
             temp<-html_text(html_nodes(read_html(paste0('https://weather.com/weather/5day/l/',where)),
                                        paste0('tr.closed.clickable:nth-of-type(',X,') > .temp')))
             return(temp)
           }
    )
  # average and transform to Celsius
  vec.temp.celsius<-
    unlist(lapply(X = vec.temp,
                  FUN = function(X){
                    avg<-sum(as.numeric(unlist(strsplit(X,'°'))))/2
                    avg.celsius<- (avg - 32) * 5/9
                    return(avg.celsius)
                  }))
  return(data.table(date=vec.date,temp=vec.temp.celsius))
}


