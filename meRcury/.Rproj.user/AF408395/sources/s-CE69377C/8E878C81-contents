

ExpandEgypt <- function(EgyptData, writeResults = T){

AllData <- EgyptData
cat("Parsing Tone, Names, Locations, and Organizations... \n")

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

EgyptDataExpanded <<- AllData

if(writeResults==T){
  FolderForResults <- choose.dir(caption = "Select where to store Egypt specific GDELT Results")
  fwrite(EgyptDataExpanded, file = paste(FolderForResults, "/EgyptExpandedDataDate", dates, ".csv", sep = ""))
}


}
