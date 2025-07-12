#Call Report Data Project Data Preparation

#Download "Call Reports -- Balance Sheet, Income Statement, Past Due -- Four Periods" from CDR 2019 - 2023
#Link: https://cdr.ffiec.gov/public/PWS/DownloadBulkData.aspx
#Save to the directory, then use this to convert to CSVs that can be easily uploaded into a SQL database

library(tidyverse) 
library(tibble)

getwd()
setwd("/FFIEC CDR Call Data")

#Get the MDRM Descriptions in their own file
data_1 <- read.delim("FFIEC CDR Call Subset of Schedules 2019(1 of 2).txt", header = TRUE, sep = "\t")
cdr_file_1_def <- head(data_1, 1)[,14:254] |> 
  t() |> 
  as.data.frame() |> 
  rownames_to_column()

colnames(cdr_file_1_def) <- c("MDRM", "Description")

data_2 <- read.delim("FFIEC CDR Call Subset of Schedules 2019(2 of 2).txt", header = TRUE, sep = "\t")
cdr_file_2_def <- head(data_2, 1)[,14:226] |> 
  t() |> 
  as.data.frame() |> 
  rownames_to_column()
colnames(cdr_file_2_def) <- c("MDRM", "Description")

mdrm_definitions <- bind_rows(cdr_file_1_def, cdr_file_2_def)

#Clear the environment of the large dataframes
remove(data_1, data_2)

#Identify all the .txt files from CDR
cdr_files <- list.files(path = path, pattern = "FFIEC CDR Call Subset of Schedules*", full.names = TRUE)

#For loop to convert them to CSV files ready for SQL
for (file in cdr_files) {
  data <- read.delim(file, header = TRUE, sep = "\t")
  data <- data |> 
    select(-last_col()) #Removing the entirely Null last column
  data <- data[-1,] #Removing the first row that is the text descriptions we've already saved--that won't fit our numeric datatypes in SQL
  data[data == ""] <- NA #Empty characters represent Null values
  data[data == "N/A"] <- NA #NA represents NUll values
  data[data == "true"] <- TRUE #So that SQL correctly intrepets "true" and "false" as Booleans
  data[data == "false"] <- FALSE
  file_name <- substring(file, 1, nchar(file) - 4) #to remove the .txt from the file name
  write.csv(data, paste0(file_name, ".csv"), row.names = FALSE)
  
}

#Now, go to PostgreSQL to create the database and input these CSVs into two staging tables
