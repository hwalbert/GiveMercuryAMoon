

CreateEgyptPrediction <- function(){

  GDELT <- Aggregate_GDELT()

  tones <- cbind(GDELT$Date, as.data.frame(parse_gkg_mentioned_article_tone(gdelt_data = GDELT)) )
  names(tones) <- c("Date", "idGKG", "Tone", "Pos.Tone", "Neg.Tone", "Polarity", "Active.Refs", "Self.Ref", "Word.Count")
  attach(tones)
  agg <- aggregate(x = tones, by = list(Date), FUN = mean)[,-c(1,3)]
  detach(tones)
  negTone <- mean(agg$Neg.Tone[(length(agg$Neg.Tone)-6):length(agg$Neg.Tone)], na.rm = T)


  ModelInput <- data.frame(
    temp = get_weather()[3,2],
    is_weekend = ifelse(weekdays(Sys.Date() + 3) %in% c("Friday", "Saturday"),1,0),
    avgnegtone = negTone)

  PREDICTION <- round(predict.lm(linreg1, ModelInput))
  Warning_ID <- paste0("GMAM-CU-", as.numeric(Sys.Date()))

  # Create the submission as a data.frame
  my.submission <- data.frame(
    "Warning_ID" = Warning_ID,
    "Event_Type" = "Civil Unrest Daily",
    "Event_Date" = Sys.Date()+3,
    "Country" = "Egypt",
    "Count" = PREDICTION
  )

  # Write the submission to a .json file
  write_json(x = my.submission,
             path = paste0("~/GitHub/GiveMercuryAMoon/Data/",Warning_ID, ".json")
  )



  return(round(PREDICTION))

}
