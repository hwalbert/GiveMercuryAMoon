#You'll need these - Get the packages####
library(dplyr)
library(digest)
library(devtools)
library(stringi)
library(purrrlyr)
library(rJava)
library(NLP)
library(openNLP)
# devtools::install_github("hafen/trelliscopejs")
# install_github("abresler/gdeltr2")
library(gdeltr2)
library(trelliscopejs)

#Date Specification Function####
#This functions takes the current date and returns a list of todays date plus the dates for the previous number of days specified
#This return will be put into the code that pulls the latest GDELT data
DatesForGDELT <- function(daysBack = 2){
  CurrentDate <- as.Date(format(Sys.time(), "%Y-%m-%d"))
  daysPrevious <- 1:daysBack
  DatesPrevious <- CurrentDate-daysPrevious
  DatesForReview <- c(DatesPrevious)
  return(DatesForReview)
}

#This code grabs the Global Knowledge Graph data for the dates specified by the DatesForGDELT() function
LatestPull <-
  get_data_gkg_days_detailed(
    dates = DatesForGDELT(daysBack = 1),
    #dates = c("2018-09-16", "2018-09-15", "2018-09-14"),
    table_name = 'gkg',
    return_message = T
  )

#Stack multiple pulls of GDELT data in one data frame
GDELTFolderLocation <- choose.dir()
MonthOfGDELTData <- list.files(GDELTFolderLocation, full.names = T)
MonthOfGDELTDataNames <- list.files(GDELTFolderLocation, full.names = F)
for(i in 1:length(MonthOfGDELTData)){
  cat("Loading", MonthOfGDELTDataNames[i], "\n")
  load(MonthOfGDELTData[i])
  assign(paste(MonthOfGDELTDataNames[i],"GDELTMONTH", sep="", collapse = ""), LatestPull, envir = globalenv())
  rm(LatestPull);gc()
  }
rm(LatestPull)


EntireMonth <- data.frame()
ImportedData <- c(grep("GDELTMONTH", ls(), value = T))
for (i in ImportedData){
  EntireMonth <- rbind(get(i), EntireMonth)
}

#show dates and articles table
table(substr(EntireMonth$dateTimeDocument,1,10))

#Look for Organizations:####
EgyptOrganizations <- c("muslim brotherhood", "egyptian police", "egyptian military", "egyptian army", "egyptian", "Harakat Sawa'd Misr", "Harakat Sawad Misr", "HASM", "ISIS-Sinai", "ISIS Sinai", "Liwa al-Thawra")
AllOrganizations <- data.frame()
for (i in EgyptOrganizations){
  Result <- filter(LatestPull, grepl(tolower(i), tolower(organizations)))
  AllOrganizations <- rbind(Result, AllOrganizations)
}
AllOrganizations <- unique(AllOrganizations)

#Look for Themes:####
#Grab the GDELT Codebook
GDELT_Codebook <- get_gdelt_codebook_ft_api(code_book = "gkg")
#Look at some of the Egypt themes in the GDELT Codebook:
EgyptThemes <- c(grep("EGYPT", GDELT_Codebook$idGKGTheme, value = T))

AllThemes <- data.frame()
for (i in EgyptThemes){
  Result <- filter(LatestPull, grepl(tolower(i), tolower(themes)))
  AllThemes <- rbind(Result, AllThemes)
}
AllThemes <- unique(AllThemes)


AllDomains <- data.frame()
EgyptDomains <- c("almasryalyoum.com", "ahram.org.eg", "alwafd.org", "youm7.com", "egyptindependent.com", "dailynewsegypt.com")
for (i in EgyptDomains){
  Result <- filter(LatestPull, grepl(tolower(i), tolower(domainSource)))
  AllDomains <- rbind(Result, AllDomains)
}
AllDomains <- unique(AllDomains)



AllData <- rbind(AllOrganizations, AllThemes, AllDomains)
AllData <- unique(AllData)
ToneData <- data.frame(t(data.frame(strsplit(AllData$tone, ","))))
names(ToneData) <- c("Tone","PositiveScore", "NegativeScore", "Polarity", "ActivityReferenceDensity", "Self_GroupReferenceDensity", "WordCount")
ToneData[] <- lapply(ToneData, as.character)
ToneData[] <- lapply(ToneData, as.numeric)
AllData <- cbind(AllData, ToneData)

#dates <- data.frame(parse_gkg_mentioned_dates(AllData))
locations <- data.frame(parse_gkg_mentioned_locations(AllData))
names <- data.frame(parse_gkg_mentioned_names(AllData))
organizations <- data.frame(parse_gkg_mentioned_organizations(AllData))
#people <- data.frame(parse_gkg_mentioned_people(AllData))

AllData <- left_join(AllData, locations, by = "idGKG")
AllData <- left_join(AllData, names, by = "idGKG")
AllData <- left_join(AllData, organizations, by = "idGKG")
sort(table(AllData$idCountry))
#Other data to look at:####
EventsCodes <- get_codes_cameo_events()
CountryCodes <- get_codes_cameo_country()

table(substr(AllData$dateTimeDocument,1,10))

GetAllValues <- function(FieldType = c("longitude", "latitude")){

  VariableFieldsData <-AllData[,grep(FieldType, names(AllData))]
  
  TallData <- data.frame()
  for (i in 1:ncol(VariableFieldsData)) {
    column <- data.frame(VariableFieldsData[,i])
    TallData <- bind_cols(column, TallData)
  }
  assign(paste(FieldType, "_List", sep = ""), TallData, envir = globalenv())
}


library(ggmap)
map <- get_map(location = 'Cairo, Egypt', zoom = 12)
mapPoints <- ggmap(map) + geom_point(aes(x = longitude, y = latitude, size = 1), data = AllData2, alpha = .5)
plot(mapPoints)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-122, 142), ylim = c(-25, 62), asp = 1)
