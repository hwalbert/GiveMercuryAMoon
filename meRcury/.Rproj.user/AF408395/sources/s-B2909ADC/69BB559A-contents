# MercuryCountries <- c("eg" = "Egypt", "sa" = "Saudia Arabia", "iq" = "Iraq", "sy" = "Syria")
# EgyptPop <- data.frame()
# for (i in 1:4) {
#   countrySample <- read.csv("http://pplapi.com/batch/500/country/eg/sample.csv")
#   EgyptPop <- rbind(EgyptPop, countrySample)
# }
#
# #Convert date of birth data from factor into date
# EgyptPop$date_of_birth <- as.Date(EgyptPop$date_of_birth)
#
#
#
# # Install and Activate Packages
# install.packages("twitteR", "RCurl", "RJSONIO", "stringr")
# library(twitteR)
# library(RCurl)
# library(RJSONIO)
# library(stringr)
#
# # Declare Twitter API Credentials
# api_key <- "API KEY" # From dev.twitter.com
# api_secret <- "API SECRET" # From dev.twitter.com
# token <- "TOKEN" # From dev.twitter.com
# token_secret <- "TOKEN SECRET" # From dev.twitter.com
#
# # Create Twitter Connection
# setup_twitter_oauth(api_key, api_secret, token, token_secret)
#
# # Run Twitter Search. Format is searchTwitter("Search Terms", n=100, lang="en", geocode="lat,lng", also accepts since and until).
#
# tweets <- searchTwitter("Obamacare OR ACA OR 'Affordable Care Act' OR #ACA", n=100, lang="en", since="2014-08-20")
#
# # Transform tweets list into a data frame
# tweets.df <- twListToDF(tweets)
#
# # Use the searchTwitter function to only get tweets within 50 miles of Los Angeles
# tweets_geolocated <- searchTwitter("Obamacare OR ACA OR 'Affordable Care Act' OR #ACA", n=100, lang="en", geocode='34.04993,-118.24084,50mi', since="2014-08-20")
# tweets_geoolocated.df <- twListToDF(tweets_geolocated)
