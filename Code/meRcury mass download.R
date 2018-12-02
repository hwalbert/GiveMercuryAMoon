# USER INPUT
# set directory
location<-'/Mercury Challenge/code and data/Data/'


# install.packages('lubridate')
# install.packages('icesTAF')
# install.packages('purrr')
library("meRcury", lib.loc="C:/Program Files/R/R-3.5.0/library")
library(lubridate)
library(icesTAF)
library(purrr)
library(stringr)
library(tidyr)
library(gdeltr2)
# rewrite some functions
ExtractEGYPT <- function(GDELTData){
  #Location filter
  vec.EgyptLocation <-
    c("egypt")
  #Look for Organizations:####
  vec.EgyptOrganizations <- 
    c("muslim brotherhood", "egyptian", "Harakat Sawa'd Misr", "Harakat Sawad Misr",
      "HASM", "ISIS-Sinai", "ISIS Sinai", "Liwa al-Thawra","sisi")
  #Look for Themes:####
  vec.EgyptThemes <- c('revolution','strike','econ_inflation','grievance','arrest','rebellion',
                       'corruption','conflict','violence','protest','terror','unrest')
  #Look for domains:
  vec.EgyptDomains <- 
    c("almasryalyoum.com", "ahram.org.eg", "alwafd.org", "youm7.com",
      "egyptindependent.com", "dailynewsegypt.com","alrai.com",
      'wsj.com','al-monitor.com')
  filtered.gdelt<-
    (data.table(GDELTData)
     [grepl(paste0(vec.EgyptLocation,collapse = "|"), tolower(locations))]
     [grepl(paste0(vec.EgyptOrganizations,collapse = "|"), tolower(organizations))]
     [grepl(paste0(vec.EgyptThemes,collapse = "|"), tolower(themes))]
     [grepl(paste0(vec.EgyptDomains,collapse = "|"), tolower(domainSource))])
  return(filtered.gdelt)
}
GetGDELT <- function(DaysBack = 1, writeResults = T, SpecificDates = NULL, location){
  
  if(is.null(SpecificDates)){
    dates = DatesForGDELT(daysBack = DaysBack)
  }else{
    dates = SpecificDates
  }
  
  
  #This code grabs the Global Knowledge Graph data for the dates specified by the DatesForGDELT() function
  GDELTData <-
    get_data_gkg_days_detailed(
      dates = dates,
      # dates = c("2018-09-08"),
      table_name = 'gkg',
      return_message = T
    )
  if(nrow(GDELTData)>0){
    GDELTData<-ExtractEGYPT(GDELTData)
    
    #dates <- gsub("-", "_", dates)
    if(writeResults==T){
      FolderForResults <- location
      fwrite(GDELTData, file = paste(FolderForResults, "/GDELTDataDate", dates, ".csv", sep = ""))
    }
  }
}
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
  # names <- data.frame(parse_gkg_mentioned_names(AllData))
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

# folder structure
icesTAF::mkdir(paste0(getwd(),location,'/Master'))

# a giant for loop that downloads gdelt data for a given month,
# extracts the egypt related info, combines it, saves it to a master folder,
# and deletes them if all of the above is done successfully
# Enter in start date and end date

for (Month in seq.Date(from=as.Date('2015-04-01'),to=as.Date('2015-04-01'),by='month')){
  #create folder
  icesTAF::mkdir(paste0(getwd(),location,as_date(Month)))
  for (i in seq.Date(from=as_date(Month),as_date(Month)+months(1)-days(1),by='day')){
    print(as_date(i))
    suppressWarnings(suppressMessages(GetGDELT(SpecificDates = as_date(i),
             location=paste0(getwd(),location,as_date(Month)))))
    
  }
  #once downloaded, let's combine
  lst.egypt<-
    lapply(X = dir(paste0(getwd(),location,as_date(Month))),
           FUN = function(X){
             print(paste0("Read GDELT data from ",X))
             egypt<-fread(paste0(paste0(getwd(),location,as_date(Month)),'/',X))
             #filter broken entries
             egypt<-egypt[is.Date(as_date(substr(dateTimeDocument,0,10)))]
           }
    )
  egypt_gdelt <- rbindlist(lst.egypt)
  
  #perform sanity checks
  message<-c()
  #check filter outputed non-zero items
  if(nrow(egypt_gdelt)!=0){
    message<-c(message,'Output check is good')

    #check # of columns (needs 29 columns)
    if(ncol(egypt_gdelt)==29){
      message<-c(message,'Column check is good')
      
      #check dates on dateTimeDocument are in deed from that month
      if(sum(format(as_date(substr(egypt_gdelt$dateTimeDocument,0,10)),
                    '%Y-%m-01')!=as_date(Month))==0){
        message<-c(message,'Date check is good')
        #write to master folder
        fwrite(egypt_gdelt, file = paste0(getwd(),location,'/Master/',as_date(Month),'-filtered_gdelt.csv'))
        #delete original month (for size reasons)
        sapply(X = paste0(paste0(getwd(),location,as_date(Month)),'/',
                          dir(paste0(getwd(),location,as_date(Month)))),FUN = file.remove)
      }else{
        message<-c(message,'Date check failed')
      }
    }else{
      message<-c(message,'Column check failed')
    }
  }else{
    message<-c(message,'Output check failed')
  }
  # write messages
  message<-paste0(message,collapse='\n')
  writeLines(message, paste0(getwd(),location,as_date(Month),'.txt'))
  # delete temp files or the hard drive will explode
  unlink(paste0(tempdir(),"\\",dir(tempdir())),recursive=T)
}

# once everything is downloaded, let's combine, expand, and date each article
lst.master <- lapply(X = paste0(paste0(getwd(),location,'/Master/'),grep('.csv',dir(paste0(getwd(),location,'/Master/')),value=T)),
                     FUN = fread)
dt.master <- rbindlist(lst.master)

dt.master <- ExpandEgypt(dt.master,writeResults = F)
dt.master<-cbind(Date=as_date(substr(dt.master$dateTimeDocument,1,10)),dt.master)

parse_gkg_mentioned_names<-function (gdelt_data, filter_na = T, return_wide = T) {
  parse_mentioned_names_counts <- function(field = "Interior Minister Chaudhry Nisar Ali Khan,47;Mullah Mansour,87;Afghan Taliban,180;Mullah Mansour,382;Mullah Mansor,753;Mullah Mansour,815;Mullah Mansour,1025", 
                                           return_wide = return_wide) {
    options(scipen = 99999)
    if (field %>% is.na()|field=="") {
      if (return_wide) {
        field_data <- data_frame(nameMentionedName1 = NA, 
                                 charLoc1 = NA)
      }
      else {
        field_data <- data_frame(nameMentionedName = NA, 
                                 charLoc = NA, idArticleMentionedName = 1)
      }
    }
    else {
      fields <- field %>% str_split("\\;") %>% flatten_chr() %>% 
        .[!. %in% ""]
      fields_df <- data_frame(field = fields) %>% dplyr::mutate(idArticleMentionedName = 1:n()) %>% 
        separate(field, into = c("nameMentionedName", 
                                 "charLoc"), sep = "\\,") %>% mutate(charLoc = charLoc %>% 
                                                                       as.numeric()) %>% suppressMessages() %>% suppressWarnings()
      if (return_wide) {
        fields_df <- fields_df %>% gather(item, value, 
                                          -c(idArticleMentionedName, charLoc)) %>% arrange(idArticleMentionedName) %>% 
          unite(item, item, idArticleMentionedName, sep = "")
        order_fields <- fields_df$item
        field_data <- fields_df %>% dplyr::select(-matches("charLoc")) %>% 
          spread(item, value) %>% dplyr::select_(.dots = order_fields)
        field_data <- field_data %>% mutate_at(field_data %>% 
                                                 dplyr::select(matches("charLoc")) %>% names(), 
                                               funs(. %>% as.character() %>% readr::parse_number()))
      }
      else {
        field_data <- fields_df
        field_data <- field_data %>% dplyr::select(idArticleMentionedName, 
                                                   charLoc, nameMentionedName)
      }
    }
    return(field_data)
  }
  if (!"mentionedNamesCounts" %in% names(gdelt_data)) {
    stop("Sorry missing metioned name column")
  }
  counts_data <- gdelt_data %>% dplyr::select(idGKG, mentionedNamesCounts)
  all_counts <- 1:length(counts_data$mentionedNamesCounts) %>% 
    purrr::map_df(function(x) {
      parse_mentioned_names_counts(field = counts_data$mentionedNamesCounts[x], 
                                   return_wide = F) %>% dplyr::mutate(idGKG = counts_data$idGKG[x])
    }) %>% dplyr::select(idGKG, everything())
  if (filter_na) {
    if ("nameMentionedName" %in% names(all_counts)) {
      all_counts <- all_counts %>% dplyr::filter(!nameMentionedName %>% 
                                                   is.na())
    }
  }
  all_counts <- all_counts %>% gdeltr2::get_clean_count_data(count_col = "idArticleMentionedName", 
                                                    return_wide = F) %>% separate(idGKG, into = c("GKG", 
                                                                                                  "dateTime"), sep = "\\-", remove = F) %>% mutate(dateTime = dateTime %>% 
                                                                                                                                                     as.numeric()) %>% select(-matches("charLoc")) %>% arrange(dateTime) %>% 
    dplyr::select(-c(dateTime, GKG)) %>% suppressWarnings()
  if (return_wide) {
    all_counts <- all_counts %>% spread(item, value)
  }
  if (!return_wide) {
    all_counts <- all_counts %>% .resolve_long_names()
  }
  return(all_counts)
}
ExpandEgypt
