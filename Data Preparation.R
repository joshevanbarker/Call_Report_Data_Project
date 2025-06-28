#Download "Call Reports -- Balance Sheet, Income Statement, Past Due -- Four Periods" from CDR 2019 - 2023
#Link: https://cdr.ffiec.gov/public/PWS/DownloadBulkData.aspx
#Save to the directory, then use this to convert to CSVs that can be easily uploaded into a SQL database

library(tidyverse) 

getwd()
setwd("/Users/joshbarker/Documents/GitHub/FinStat/Call Report Project/FFIEC CDR Call Data/Raw")

data_1 <- read.delim("FFIEC CDR Call Subset of Schedules 2019(1 of 2).txt", header = TRUE, sep = "\t")
cdr_file_1_def <- head(data_1, 2)
write.csv(cdr_file_1_def, "cdr_file_1_def.csv")

data_2 <- read.delim("FFIEC CDR Call Subset of Schedules 2019(2 of 2).txt", header = TRUE, sep = "\t")
cdr_file_2_def <- head(data_2, 2)
write.csv(cdr_file_2_def, "cdr_file_2_def.csv")

cdr_files <- list.files(path = path, pattern = "FFIEC CDR Call Subset of Schedules*", full.names = TRUE)

for (file in cdr_files) {
  data <- read.delim(file, header = TRUE, sep = "\t")
  data <- data |> 
    select(-last_col())
  data <- data[-1,]
  data[data == ""] <- NA
  data[data == "N/A"] <- NA
  data[data == "true"] <- TRUE
  data[data == "false"] <- FALSE
  file_name <- substring(file, 1, nchar(file) - 4) #to remove the .txt from the file name
  write.csv(data, paste0(file_name, ".csv"), row.names = FALSE)
  
}
