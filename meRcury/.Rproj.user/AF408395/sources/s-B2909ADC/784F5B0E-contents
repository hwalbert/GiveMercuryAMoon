# rewrite some functions
ExtractEGYPT <- function(GDELTData, location = '~/GitHub/GiveMercuryAMoon/Data/'){
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
