# hehe
#Create  Mercury NLP DF
library(plyr)
# Set days
all.days <- seq.Date(from = min(GDELT$Date), to = max(GDELT$Date), by = 1)  

# Combine themes from all docs for each day
day.docs <- character(length = length(all.days))
for ( i in 1:length(all.days)) {
  s <- ( GDELT$Date == all.days[i] )
  day.docs[i] <- ifelse(test = ( sum(s, na.rm = T) > 0 ), 
                        yes = paste0(GDELT[s,]$themes, collapse = ""),
                        no = "QQQQ")
}

# Create DF.NLP
DF.NLP <- data.frame("DATE" = all.days, "FULL.DAY.THEMES" = day.docs)

# Get GDELT doc count for each day
gd.dt <- count(GDELT$Date)
DF.NLP <- merge(x = DF.NLP, y = gd.dt, by.x = "DATE", by.y = "x", all.x = T)
DF.NLP[is.na(DF.NLP$freq),]$freq <- 0
names(DF.NLP)[3] <- "GDELT.Doc.Count"

#Re-order
DF.NLP <- DF.NLP[,c(1,3,2)]

rm(i, s, all.days, day.docs, gd.dt)

# NLP
library(tm)

VS <- VectorSource(DF.NLP$FULL.DAY.THEMES)
CO <- Corpus(VS)
tdm <- TermDocumentMatrix(CO, control = list(removePunctuation = TRUE, 
                                             stopwords = c( stopwords('english') ), 
                                             removeNumbers = TRUE, tolower = TRUE) 
)
tdm <- as.data.frame( t( as.matrix(tdm) ) )

DF.NLP <- cbind(DF.NLP, tdm)

rm(VS, CO, tdm)

# Word cloud
library(wordcloud)
library(RColorBrewer)

w <- names(tdm)
c <- colSums(tdm)

set.seed(1234)
wordcloud(words = w, freq = c, min.freq = 10,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
