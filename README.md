# Call Report Database and Shiny App Project
<b> Josh Barker </b>

The Federal Financial Institutions Examination Council (FFIEC) requires all United States banks to file the <u>Consolidated Report of Condition and Income</u> quarterly. Dependent upon the institution's size, each institution files one of three variants of this report (FFIEC 031/041/051). The data is used by federal bank regulators for supervisory and regulatory functions. 

In this project, I create an R Shiny application to visualize and analyze Call Report data. I pull publicly-available Call Report data, prepare it for staging, and pull it into SQL. After restaging tables in SQL, I create an RShiny app that queries data from this newly created database. Note: all SQL scripts use PostgreSQL syntax. 

## What's in this repository
### Call Report Data (2019-2023)
* Five years of key Call Report variables in tab-delimited txt files, as downloaded directly from the FFIEC's Central Data Repository are available in the `FFIEC CDR Call Data` folder.
### SQL Database
* `Data Preparation.R` prepares the raw txt files to be sent into a SQL server and creates 11 CSV files: 10 of the Call Report data and 1 that contains only the raw column names and descriptions.
* `Create SQL Database.sql` creates a database called `FFIEC_CDR_CALL_PUBLIC`.
* `Create Staging Tables.sql` pulls the 11 CSV files created in the Data Preparation file into the SQL Database.
* `Prepare Final Tables.sql` takes the data from the two staging tables and turns it into 7 tables for easier analysis and querying
### R Shiny App
* `Josh Barker Call Report Shiny App.R` is the meat of the project containing both the user interface and server functions to allow users to easily query, graph, and analyze the data directly from the SQL Database. 
