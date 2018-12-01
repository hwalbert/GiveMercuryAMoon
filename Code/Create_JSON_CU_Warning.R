library(jsonlite)
library(data.table)

# Read in the submission file (.csv)
model.input <- fread(choose.files())

# Create the submission as a data.frame
my.submission <- data.frame(
  "Warning_ID" = paste0("GH-CU-", as.numeric(Sys.Date())),
  "Event_Type" = "Civil Unrest Daily",
  "Event_Date" = model.input$Date,
  "Country" = "Egypt",
  "Count" = model.input$CU.Count
)

# Write the submission to a .json file
toJSON(my.submission)
