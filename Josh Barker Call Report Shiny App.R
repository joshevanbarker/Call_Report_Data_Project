# Josh Barker Call Report Analysis Application
## Import Libraries ====
library(tidyverse)
library(DBI)
library(RPostgres)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(bslib)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(ggplot2)
library(glue)
library(shinycssloaders)


## Preparation ====

con <- dbConnect(RPostgres::Postgres(),
                 dbname = "FFIEC_CDR_CALL_PUBLIC",
                 host = "localhost",
                 port = 5433,
                 user = .rs.AskForPassword("Please enter your PostgreSQL username"),
                 password = .rs.askForPassword("Please enter your PostgreSQL password"))

comparison_options <- c("Multiple MDRMs", "Multiple Institutions")
mdrm_description_info <- dbGetQuery(con, "SELECT * FROM mdrm_descriptions") |> 
  mutate(options = paste(mdrm, description, sep = " - "))

institutions <- dbGetQuery(con, "SELECT id_rssd, inst_name, first_dt, last_dt FROM institutions ORDER BY inst_name") |> 
  mutate(options = paste(inst_name, id_rssd, sep = " - "))

mdrm_options <- mdrm_description_info$options
institution_options <- institutions$options
date_options <- dbGetQuery(con, "SELECT DISTINCT as_of_dt FROM schedule_ri ORDER BY as_of_dt DESC")
output_options <- c("Dollars", "Dollar Change", "Percent Change")
schedule_options <- c("Schedule RI - Income Statement", "Schedule RC - Balance Sheet", "Schedule RC-N - Past Due and Nonaccrual Loans, Leases, and Other Assets")


## User Interface ====
ui <- fluidPage(theme = shinytheme("sandstone"),
page_navbar(
                  title = "Call Report Analysis Application",
                  nav_panel(title = "Home",
                            titlePanel("Home"),
                            p("This application is designed to work with a PostgreSQL database the maintains Call Report Data.
                              The data used in developing this application consisted of Schedules RI, RC, and RC-N for all FFIEC 031/041/051 filings from 2019 Q1 to 2023 Q4.
                              To download the data yourself, visit The Federal Financial Institutions Examination Council (FFIEC)'s Central Data Repository's ", 
                              tags$a(href = "https://cdr.ffiec.gov/public/PWS/DownloadBulkData.aspx", "Public Data Distribution website. "),
                              "To understand what each MDRM means, visit the Federal Reserve's", tags$a(href = "https://www.federalreserve.gov/apps/mdrm/data-dictionary", "Micro Data Reference Manual.")),
                            br(),
                            p("The following tools are available for use:"),
                            headerPanel(tags$h3("MDRM Analysis")),
                            p("This tool allows users to compare several line items at an institution as well as the same line item between multiple institutions.
                              Users can graph or view a table with raw amounts, dollar changes, or percentage changes for each line item."),
                            headerPanel(tags$h3("Schedule Viewer")),
                            p("This tool allows users to query a single schedule for a single institution for a single as-of date to view the raw numbers."),
                            headerPanel(tags$h3("Schedule Analysis")),
                            p("This tool allows users to query the prior year's worth of Call Report data for a single institution and a single schedule.
                              It highlights percentage and dollar amount changes between the most recent quarter. It also has a merger tool that allows 
                              a user to view the same data, but with prior quarters summed between the two merged institutions prior to the merger, for 
                              a more accurate quarter-over-quarter and year-over-year comparison."),
                            hr(),
                            p("This application was created by Josh Barker in the summer of 2025.")),
                  nav_panel(title = "MDRM Analysis", sidebarLayout(  
                            sidebarPanel(
                                radioButtons("mdrmcomparisontype", "Comparison:", comparison_options, selected = character(0)),
                                selectizeInput("mdrms", "MDRM(s):", mdrm_options, options = list(maxOptions = 8), multiple = TRUE),
                                selectizeInput("rssds", "RSSD(s):", institution_options, options = list(maxOptions = 8), multiple = TRUE),
                                selectInput("start_date", "Start Date:", date_options, selected = "2019-03-31"),
                                selectInput("end_date", "End Date:", date_options),
                                selectInput("output_options", "Output:", output_options),
                                actionButton("mdrm_submit", "Submit")
                                ),
                              mainPanel(
                                span(textOutput("mdrm_error_message"), style = "font-family:verdanal font-size: 400%, color: red; font-weight: bold"),
                                span(textOutput("mdrm_title"), style = "font-family:verdanal font-size: 200%; color: navy; font-weight: bold"),
                                span(textOutput("mdrm_description"), style = "font-family:verdanal font-size: 50%"),
                                tabsetPanel(type = "tabs",
                                                    tabPanel("Graph", plotlyOutput("mdrm_graph")),
                                                    tabPanel("Data", DTOutput("mdrm_table"))
                                                    )))),
                  nav_panel(title = "Schedule Viewer", sidebarLayout(  
                    sidebarPanel(
                      selectizeInput("viewer_rssd", "RSSD", institution_options, options = list(maxOptions = 8)),
                      selectInput("viewer_as_of_dt", "As-Of Date:", date_options, selected = "2023-12-31"),
                      selectInput("viewer_schedule", "Schedule:", schedule_options),
                      actionButton("viewer_submit", "Submit")
                    ),
                    mainPanel(
                      span(textOutput("viewer_title"), style = "font-family:verdanal font-size: 200%; color: navy; font-weight: bold"),
                      span(textOutput("viewer_description"), style = "font-family:verdanal font-size: 50%"),
                      span(textOutput("viewer_error_message"), style = "font-family:verdanal font-size: 400%, color: red; font-weight: bold"),
                      DTOutput("viewer_table")
                      ))),
                  nav_panel(title = "Schedule Analysis", sidebarLayout(  
                    sidebarPanel(
                      selectizeInput("analysis_rssd", "RSSD", institution_options, options = list(maxOptions = 8)),
                      selectInput("analysis_as_of_dt", "As-Of Date:", date_options, selected = "2023-12-31"),
                      selectInput("analysis_schedule", "Schedule:", schedule_options),
                      selectInput("analysis_merger", "Is this the first quarter after a merger?", c("Yes", "No"), selected = "No"),
                      conditionalPanel(
                        condition = "input.analysis_merger == 'Yes'",
                        selectizeInput("analysis_merger_rssd", "Merged Institution:", institution_options, options = list(maxOptions = 8))
                      ),
                      actionButton("analysis_submit", "Submit")
                    ),
                    mainPanel(
                      span(textOutput("analysis_title"), style = "font-family:verdanal font-size: 200%; color: navy; font-weight: bold"),
                      span(textOutput("analysis_description"), style = "font-family:verdanal font-size: 50%"),
                      span(textOutput("analysis_error_message"), style = "font-family:verdanal font-size: 400%, color: red; font-weight: bold"),
                      DTOutput("analysis_table")
                    ))), 
                  nav_panel(title = "About the Creator",
                              mainPanel(
                              p("This application was developed and created by Josh Barker. Josh is a Financial Statistics Analyst at
                                     the Federal Reserve Bank of Philadelphia where he works with bank regulatory data like this."),
                              hr(),
                              p("Josh received his B.A. in Economics and Politics from Hillsdale College and M.S. in Data Science 
                              from Eastern University. For more on him and his projects, visit his", 
                                tags$a(href = "https://joshevanbarker.github.io/", "Github Site"), ".")
                    ))
  ))

## Server ==== 
server <- function(input, output, session) {

### MDRM Analysis Panel: Comparison Function ====
  observeEvent(input$mdrmcomparisontype, 
               {
                 if (input$mdrmcomparisontype == "Multiple MDRMs"){
                   updateSelectizeInput(session, "mdrms", options = list(maxOptions = 8))
                   updateSelectizeInput(session, "rssds", options = list(maxOptions = 1))
                   
                 } else {
                   updateSelectizeInput(session, "mdrms", options = list(maxOptions = 1))
                   updateSelectizeInput(session, "rssds", options = list(maxOptions = 8))
                 }
               })

### MDRM Analysis Panel: Submit Function ====    
  observeEvent(input$mdrm_submit,
               {
                 showPageSpinner(caption = tags$strong("I'm querying your data!"))
                 
                 if (length(input$rssds) > 1 & length(input$mdrms) > 1) {
                   output$mdrm_error_message <- renderText({"I'm sorry. I am only designed to accept either multiple line items or multiple institutions, but not both. Please limit your search and try again."})
                   output$mdrm_title <- NULL
                   output$mdrm_description <- NULL
                   output$mdrm_graph <- NULL
                   output$mdrm_table <- NULL
                   hidePageSpinner()
                 } else {
                 
                   output$mdrm_error_message <- NULL 
                   
                 selected_rssds <- institutions |> 
                   filter(options %in% input$rssds) |> 
                   select(id_rssd)
                 
                 
                 selected_mdrm <- substring(input$mdrms, 1, 8)
                 
                if (input$mdrmcomparisontype == "Multiple MDRMs"){
                  
                  bank_name <- institutions |> 
                    filter(options %in% input$rssds) |> 
                    select(inst_name) |> 
                    as.character()
                  
                  output$mdrm_title <- renderText({bank_name})
                  output$mdrm_description <- renderText({paste0(input$mdrms, collapse = " , ")})
                  
                  selected_mdrms <- paste0(sprintf("'%s'", selected_mdrm), collapse = ",")
                  
                  raw_data <- dbGetQuery(con, paste0("SELECT as_of_dt, id_rssd, UPPER(mdrm) AS mdrm, amount
                                                     FROM call_data_long
                                                     WHERE as_of_dt >= CAST('", input$start_date, "' AS DATE)
                                                     AND as_of_dt <= CAST('", input$end_date, "' AS DATE)
                                                     AND id_rssd = '", selected_rssds,"'
                                                     AND UPPER(mdrm) IN (", selected_mdrms,")
                                                     ORDER BY as_of_dt;")) |>
                    as.data.frame() |> 
                    mutate(amount = as.numeric(amount))
                  
                  output_data <- raw_data |> 
                    pivot_wider(names_from = mdrm, values_from = amount)
                  
                  if (input$output_options == "Dollar Change") {
                    
                    for (x in 1:length(selected_mdrm)) {
                      col <- selected_mdrm[x]
                      
                      output_data <- output_data |> 
                        mutate("{col}" := !!sym(glue("{col}")) - lag(!!sym(glue("{col}"))))
                    }}
                  
                  if (input$output_options == "Percent Change") {
                    
                    for (x in 1:length(selected_mdrm)) {
                      col <- selected_mdrm[x]
                      
                      output_data <- output_data |> 
                        mutate("{col}_lag" := lag(!!sym(glue("{col}"))),
                               "{col}_change" := !!sym(glue("{col}")) - !!sym(glue("{col}_lag")),
                               "{col}" := round((!!sym(glue("{col}_change")) / !!sym(glue("{col}_lag"))) * 100, 2)
                               )
                      
                      output_data <- output_data |> 
                        select(-contains("_lag")) |> 
                        select(-contains("_change"))
                      
                    }} 
                  
                  if (input$output_options != "Dollars") {
                    
                    output_data <- output_data |> 
                      na.omit()
                    
                    update_table <- reactive({output_data})
                    output$mdrm_table <- renderDT({update_table()})
                    
                    raw_data <- output_data |> 
                      pivot_longer(cols = selected_mdrm, names_to = "mdrm", values_to = "amount")
                    
                  } #else {} input$output_options == 'Dollars'
                  
                  update_table <- reactive({output_data})
                  output$mdrm_table <- renderDT({update_table()})
                  
                  update_graph <- reactive ({raw_data})
                  
                  hidePageSpinner()
                  
                  if (input$output_options == "Percent Change") {
                    output$mdrm_graph <- renderPlotly({
                      graph <- plot_ly(data = update_graph(), 
                                       x = ~as_of_dt, y = ~amount, color = ~mdrm, 
                                       type = 'scatter', 
                                       mode = 'lines+markers') |> 
                        layout(separators = '.,',
                               xaxis = list(title = 'Date'),
                               yaxis = list(title = 'Percent Change', ticksuffix = '%', nticks = 5, tickformat = ",.0f", hoverformat = ",."))
                    }) 
                  } else {
                  
                  output$mdrm_graph <- renderPlotly({
                    graph <- plot_ly(data = update_graph(), 
                                     x = ~as_of_dt, y = ~amount, color = ~mdrm, 
                                     type = 'scatter', 
                                     mode = 'lines+markers') |> 
                      layout(separators = '.,',
                             xaxis = list(title = 'Date'),
                             yaxis = list(title = 'Thousands of Dollars', tickprefix = '$', nticks = 5, tickformat = ".0f", hoverformat = ",."))
                  }) }
                  
                } else #input$mdrmcomparisontype == 'Multiple RSSDs'
                {
                  mdrm_name <- mdrm_description_info |> 
                    filter(options %in% input$mdrms) |> 
                    select(description) |> 
                    as.character()
                  
                  output$mdrm_title <- renderText({mdrm_name})
                  output$mdrm_description <- renderText({paste0(input$rssds, collapse = " , ")})
                  
                  selected_rssd <- institutions |> 
                    filter(options %in% input$rssds) |> 
                    select(id_rssd) |> 
                    distinct()
                  
                  selected_rssds <- paste0(sprintf("'%s'", unlist(selected_rssd)), collapse = ",")
                  
                  raw_data <- dbGetQuery(con, paste0("SELECT as_of_dt, id_rssd, UPPER(mdrm) AS mdrm, amount
                                                     FROM call_data_long
                                                     WHERE as_of_dt >= CAST('", input$start_date, "' AS DATE)
                                                     AND as_of_dt <= CAST('", input$end_date, "' AS DATE)
                                                     AND id_rssd IN (", selected_rssds,")
                                                     AND UPPER(mdrm) = '", substring(input$mdrms, 1, 8),"'
                                                     ORDER BY as_of_dt;")) |>
                    as.data.frame() |> 
                    mutate(amount = as.numeric(amount))
                  
                  output_data <- raw_data |>
                    pivot_wider(names_from = id_rssd, values_from = amount)
                  
                  if (input$output_options == "Dollar Change") {
                    
                    id_rssd_cols <- c()

                    for (x in 1:nrow(selected_rssd)) {
                      col <- selected_rssd[x,]

                      output_data <- output_data |>
                        mutate("{col}" := !!sym(glue("{col}")) - lag(!!sym(glue("{col}"))))
                      
                      id_rssd_cols <- append(id_rssd_cols, col)
                    }}
                  
                  if (input$output_options == "Percent Change") {
                    
                    id_rssd_cols <- c()
                    
                    for (x in 1:nrow(selected_rssd)) {
                      col <- selected_rssd[x,]
                      
                      output_data <- output_data |> 
                        mutate("{col}_lag" := lag(!!sym(glue("{col}"))),
                               "{col}_change" := !!sym(glue("{col}")) - !!sym(glue("{col}_lag")),
                               "{col}" := round((!!sym(glue("{col}_change")) / !!sym(glue("{col}_lag"))) * 100, 2)
                        )
                      
                      id_rssd_cols <- append(id_rssd_cols, col)
                      
                      output_data <- output_data |> 
                        select(-contains("_lag")) |> 
                        select(-contains("_change"))
                    }} 
                  
                  if (input$output_options != "Dollars") {
                    
                    output_data <- output_data |> 
                      na.omit()

                    update_table <- reactive({output_data})
                    output$mdrm_table <- renderDT({update_table()})

                    raw_data <- output_data |>
                      pivot_longer(cols = id_rssd_cols, names_to = "id_rssd", values_to = "amount")

                  } #else {} input$output_options == 'Dollars'
                  
                  update_table <- reactive({output_data})
                  output$mdrm_table <- renderDT({update_table()})
                  
                  update_graph <- reactive ({raw_data})
                  
                  hidePageSpinner()
                  
                  if (input$output_options == "Percent Change") {
                    output$mdrm_graph <- renderPlotly({
                      graph <- plot_ly(data = update_graph(), 
                                       x = ~as_of_dt, y = ~amount, color = ~id_rssd, 
                                       type = 'scatter', 
                                       mode = 'lines+markers') |> 
                        layout(separators = '.,',
                               xaxis = list(title = 'Date'),
                               yaxis = list(title = 'Percent Change', ticksuffix = '%', nticks = 5, tickformat = ".0f", hoverformat = ",."))
                      
                    })
                  } else {
                  
                  output$mdrm_graph <- renderPlotly({
                    graph <- plot_ly(data = update_graph(), 
                                     x = ~as_of_dt, y = ~amount, color = ~id_rssd, 
                                     type = 'scatter', 
                                     mode = 'lines+markers') |> 
                      layout(separators = '.,',
                             xaxis = list(title = 'Date'),
                             yaxis = list(title = 'Thousands of Dollars', tickprefix = '$', nticks = 5, tickformat = ",.0f", hoverformat = ",.0f"))
                    
                  })
                  }
                } 
              }
               }
                   )

### Schedule Viewer Panel: Submit Function ====
      
  observeEvent(input$viewer_submit,
               {
                 showPageSpinner(caption = tags$strong("I'm looking for that Call Report filing for you."))
                 viewer_rssd <- institutions |> 
                   filter(options == input$viewer_rssd) |> 
                   select(id_rssd)
                 
                 valid_check <- nrow(dbGetQuery(con, paste0("SELECT * FROM call_data_long WHERE id_rssd = '", viewer_rssd, "' AND as_of_dt = '", input$viewer_as_of_dt, "';")))
                 if (valid_check == 0){
                   min_date <- institutions |> 
                     filter(id_rssd == viewer_rssd[[1,1]]) |> 
                     select(first_dt) |> 
                     format("%B %d, %Y")
                   
                   max_date <- institutions |> 
                     filter(id_rssd == viewer_rssd[[1,1]]) |> 
                     select(last_dt) |> 
                     format("%B %d, %Y")
                   
                   hidePageSpinner()
                   output$viewer_error_message <- renderText({paste("We're sorry, but there is no report for this institution and this As-Of Date. Try a date between", min_date,"and", max_date,".")})
                   output$viewer_table <- NULL
                   output$viewer_title <- NULL
                   output$viewer_description <- NULL
                   
                 } else{output$viewer_error_message <- NULL
                                     
                 bank_info <- dbGetQuery(con, paste0("SELECT DISTINCT inst_name, filing_type FROM raw_call_data_part_1
                                                     WHERE as_of_dt = '", input$viewer_as_of_dt, "'
                                                     AND id_rssd = '", viewer_rssd, "';"))
                   
                 bank_name <- as.character(bank_info$inst_name)
                 filing_type <- bank_info$filing_type

                 output$viewer_title <- renderText({bank_name})
                 output$viewer_description <- renderText({paste0("FFIEC 0", as.character(filing_type), " ", input$viewer_schedule, " for ",input$viewer_as_of_dt)})
                 
                 if (input$viewer_schedule == "Schedule RI - Income Statement") {table <- "schedule_ri"}
                 else{
                   if (input$viewer_schedule == "Schedule RC - Balance Sheet") {table <- "schedule_rc"}
                   else{ #if (input$viewer_schedule == "Schedule RC-N...")
                     table <- "schedule_rcn"}
                   }

                 if (table == "schedule_ri") {table = table} #since the table does not depend on the filing type, continue to pull it.
                 else{ #for their schedule_rc or schedule_rcn
                   if (filing_type == "31") {table <- paste0(table, "_031")}
                   else{ #for schedule_rc or rcn, and filing_type is 041 or 051
                     table <- paste0(table, "_041_51")
                   }
                 }

                 data <- dbGetQuery(con, paste0("SELECT * FROM ", table, "
                                                WHERE id_rssd = '", viewer_rssd, "'
                                                AND as_of_dt = '", input$viewer_as_of_dt,"';")) |>
                   select(-c(id_rssd, as_of_dt, filing_type))

                 data <-  as.data.frame(t(data)) |>
                   rownames_to_column() |>
                   setNames(c("mdrm", "value")) |>
                   mutate(mdrm = toupper(mdrm)) |>
                   drop_na() |>
                   left_join(mdrm_description_info |> select(mdrm, description), join_by(mdrm)) |>
                   select(description, value, mdrm)

                 update_viewing_table <- reactive({data})
                 hidePageSpinner()
                 output$viewer_table <- renderDT({update_viewing_table() |> 
                     datatable() |> 
                     formatCurrency(columns = "value", currency = "$", interval = 3, mark = ",", digits = 0) })
               
               }
               })
  
### Schedule Analysis Panel: Submit Function ====  
  
  observeEvent(input$analysis_submit,
               {
                 showPageSpinner(caption = tags$strong("One moment while I search for your report and conduct some calculations."))
                 analysis_rssd <- institutions |> 
                   filter(options == input$analysis_rssd) |> 
                   select(id_rssd)
                 
                 analysis_merger_rssd <- institutions |> 
                   filter(options == input$analysis_merger_rssd) |> 
                   select(id_rssd)
                 
                 valid_check <- nrow(dbGetQuery(con, paste0("SELECT * FROM call_data_long WHERE id_rssd = '", analysis_rssd, "' AND as_of_dt = '", input$analysis_as_of_dt, "';")))
                 if (valid_check == 0){
                   min_date <- institutions |> 
                     filter(id_rssd == analysis_rssd[[1,1]]) |> 
                     select(first_dt) |> 
                     format("%B %d, %Y")
                   
                   max_date <- institutions |> 
                     filter(id_rssd == analysis_rssd[[1,1]]) |> 
                     select(last_dt) |> 
                     format("%B %d, %Y")
                   
                   hidePageSpinner()
                   
                   output$analysis_error_message <- renderText({paste("We're sorry, but there is no report for this institution and this As-Of Date. Try a date between", min_date,"and", max_date,".")})
                   output$analysis_table <- NULL
                   output$analysis_title <- NULL
                   output$analysis_description <- NULL
                   
                 } else {output$analysis_error_message <- NULL
                 
                 bank_info <- dbGetQuery(con, paste0("SELECT DISTINCT inst_name, filing_type FROM raw_call_data_part_1
                                                     WHERE as_of_dt = '", input$analysis_as_of_dt, "'
                                                     AND id_rssd = '", analysis_rssd, "';"))
                 
                 bank_name <- as.character(bank_info$inst_name)
                 filing_type <- bank_info$filing_type
                 last_as_of_dt <- input$analysis_as_of_dt
                 last_year <- as.numeric(substr(last_as_of_dt, 1, 4)) - 1
                 start_as_of_dt <- paste0(last_year, substr(last_as_of_dt, 5, 10))
                 
                 output$analysis_title <- renderText({bank_name})
                 
                 if (input$analysis_schedule == "Schedule RI - Income Statement") {table <- "schedule_ri"}
                 else{
                   if (input$analysis_schedule == "Schedule RC - Balance Sheet") {table <- "schedule_rc"}
                   else{ #if (input$viewer_schedule == "Schedule RC-N...")
                     table <- "schedule_rcn"}
                 }
                 
                 if (table == "schedule_ri") {table = table} #since the table does not depend on the filing type, continue to pull it.
                 else{ #for their schedule_rc or schedule_rcn
                   if (filing_type == "31") {table <- paste0(table, "_031")}
                   else{ #for schedule_rc or rcn, and filing_type is 041 or 051
                     table <- paste0(table, "_041_51")
                   }
                 }
                 
                 if (input$analysis_merger == "Yes") {
                   output$analysis_description <- renderText({paste0("FFIEC 0", as.character(filing_type), " ", input$analysis_schedule, " for ",input$analysis_as_of_dt, " for Recent Merger Analysis")})
                   
                   data <- dbGetQuery(con, paste0("SELECT * FROM ", table, "
                                                  WHERE ID_RSSD IN ('", analysis_rssd, "', '", analysis_merger_rssd,"')
                                                  AND as_of_dt >='", start_as_of_dt,"'
                                                  AND as_of_dt <= '", last_as_of_dt, "'
                                                  ORDER BY as_of_dt DESC;"))
                   
                   data <- data |> 
                     group_by(as_of_dt) |> 
                     summarise(across(where(is.numeric), sum, na.rm = TRUE)) |> 
                     select(-where(is.character))
                   
                 } else {
                   output$analysis_description <- renderText({paste0("FFIEC 0", as.character(filing_type), " ", input$analysis_schedule, " for ",input$analysis_as_of_dt)})
                   
                   data <- dbGetQuery(con, paste0("SELECT * FROM ", table, "
                                                  WHERE ID_RSSD = '", analysis_rssd, "'
                                                  AND as_of_dt >='", start_as_of_dt,"'
                                                  AND as_of_dt <= '", last_as_of_dt, "'
                                                  ORDER BY as_of_dt DESC;")) |> 
                     select(-c(id_rssd, filing_type)) |> 
                     select(-where(is.character))
                 }
                 
                 mdrms <- colnames(data)
                 mdrms <- mdrms[mdrms != "as_of_dt"]
                 
                 data <- data |> 
                   pivot_longer(cols = all_of(mdrms), names_to = "mdrm", values_to = "amount") |> 
                   pivot_wider(names_from = as_of_dt, values_from = amount) |> 
                   mutate(mdrm = toupper(mdrm))
                 
                 dates <- colnames(data)
                 dates <- dates[dates != "mdrm"]
                 dates <- data.frame(as_of = dates) |> 
                   mutate(as_of = as.Date(as_of)) |> 
                   arrange(desc(as_of)) |> 
                   mutate(as_of = as.character(as_of))
                 
                 data <- data |> 
                   select(c("mdrm", dates$as_of))
                 
                 Q1 <- dates$as_of[1]
                 Q2 <- dates$as_of[2]
                 Q3 <- dates$as_of[3]
                 Q4 <- dates$as_of[4]
                 Q5 <- dates$as_of[5]
                 
                 
                 if (table == "schedule_ri" & substr(Q1,6,7) == "03") {#For Schedule RI, our first quarter change will be different since the values are already since the start of the last quarter for an income statement
                
                 data <- data |> 
                   mutate(Q1oQ1_Dollar = data[[Q1]] - data[[Q5]],
                          Q1oQ1_Percent = if_else(Q1oQ1_Dollar == 0, 0, round((data[[Q5]] - data[[Q5]])/data[[Q5]], 4))) |> 
                   left_join(mdrm_description_info |> select(mdrm, description), join_by(mdrm)) |> 
                   select(c("mdrm", "description", Q1, Q1oQ1_Dollar, Q1oQ1_Percent, Q2, Q3, Q4, Q5))
                 
                 col_currency_list <- c(Q1, "Q1oQ1_Dollar", Q2, Q3, Q4, Q5)
                 col_percent_list <- c("Q1oQ1_Percent")
                 
                 } else { 
                   
                   data <- data |> 
                     mutate(QoQ_Dollar = data[[Q1]] - data[[Q2]],
                            QoQ_Percent = if_else(QoQ_Dollar == 0, 0, round((data[[Q1]] - data[[Q2]])/data[[Q2]], 4)),
                            YoY_Percent = if_else(data[[Q1]] - data[[Q5]] == 0, 0, round((data[[Q1]] - data[[Q5]])/data[[Q5]], 4))) |> 
                     left_join(mdrm_description_info |> select(mdrm, description), join_by(mdrm)) |> 
                     select(c("mdrm", "description", Q1, QoQ_Dollar, QoQ_Percent, YoY_Percent, Q2, Q3, Q4, Q5)) 
                   
                   col_currency_list <- c(Q1, "QoQ_Dollar", Q2, Q3, Q4, Q5)
                   col_percent_list <- c("QoQ_Percent", "YoY_Percent")
                   
                 }
                 
                 update_table <- reactive({data})
                 hidePageSpinner()
                 output$analysis_table <- renderDT({update_table() |>
                     datatable() |> 
                     formatCurrency(columns = col_currency_list, currency = "$", interval = 3, mark = ",", digits = 0) |> 
                     formatPercentage(col_percent_list, 2)}, 
                     rownames = FALSE, options = list(paging = FALSE))

                 } 
               })
  
  
  observeEvent(input$ksubmit,
               {
                 
               })
}

## Application Running ====

shinyApp(ui, server)
