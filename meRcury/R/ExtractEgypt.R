#
#
ExtractEGYPT <- function(GDELTData, writeResults = T){
  #Look for Organizations:####
  cat("Finding Egypt Specific Organizations... \n")
  EgyptOrganizations <- c("muslim brotherhood", "egyptian police", "egyptian military", "egyptian army", "egyptian", "Harakat Sawa'd Misr", "Harakat Sawad Misr", "HASM", "ISIS-Sinai", "ISIS Sinai", "Liwa al-Thawra")
  AllOrganizations <- data.frame()
  for (i in EgyptOrganizations){
    Result <- filter(GDELTData, grepl(tolower(i), tolower(organizations)))
    AllOrganizations <- rbind(Result, AllOrganizations)
  }
  AllOrganizations <- unique(AllOrganizations)

  #Look for Themes:####
  #Grab the GDELT Codebook
  #GDELT_Codebook <- get_gdelt_codebook_ft_api(code_book = "gkg")
  #Look at some of the Egypt themes in the GDELT Codebook:
  cat("Finding Egypt Specific Themes... \n")
  EgyptThemes <- c(grep("EGYPT", GDELT_Codebook$idGKGTheme, value = T))

  AllThemes <- data.frame()
  for (i in EgyptThemes){
    Result <- filter(GDELTData, grepl(tolower(i), tolower(themes)))
    AllThemes <- rbind(Result, AllThemes)
  }
  AllThemes <- unique(AllThemes)

  cat("Finding Egypt Specific Domain Names... \n")
  AllDomains <- data.frame()
  EgyptDomains <- c("almasryalyoum.com", "ahram.org.eg", "alwafd.org", "youm7.com", "egyptindependent.com", "dailynewsegypt.com")
  for (i in EgyptDomains){
    Result <- filter(GDELTData, grepl(tolower(i), tolower(domainSource)))
    AllDomains <- rbind(Result, AllDomains)
  }
  AllDomains <- unique(AllDomains)


  AllData <- rbind(AllOrganizations, AllThemes, AllDomains)
  AllData <- unique(AllData)



  EgyptData <<- AllData

  if(writeResults==T){
    FolderForResults <- choose.dir(caption = "Select where to store Egypt specific GDELT Results")
    fwrite(EgyptData, file = paste(FolderForResults, "/EgyptDataDate", dates, ".csv", sep = ""))
  }
}
