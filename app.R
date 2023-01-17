library(tidyverse)
library(lubridate)
library(openxlsx)
library(readxl)
library(reactable)
library(icons)
library(shiny)
library(htmltools)
library(zuericssstyle)

# Source Prepared Data
source("R/get_data.R")
data <- get_data()

# Source Export Excel
source("R/ssz_download_excel.R",)

# source functions to create reactables
source("R/get_main_reactable.R")
source("R/get_second_reactable.R")

# Set the Icon path
icons_ssz <- icon_set("icons/")

# if data load didn't work show message
if(is.null(data)) {
  
  # Define UI
  ui <- fluidPage(
    
    # Include CSS
    includeCSS("www/sszThemeShiny.css"),
    
    h1("Fehler"),
    p("Aufgrund momentaner Wartungsarbeiten ist die Applikation zur Zeit nicht verfügbar.")
  )
  
  # Server function
  server <- function(input, output) {}
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  
}else{
  
  # Define UI for application that draws a histogram
  ui <- fluidPage(
    
    # Application title
    # titlePanel("Abstimmungsresultate App"),
    
    # CSS
    includeCSS("www/sszThemeShiny.css"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      
      # Sidebar Layout
      sidebarPanel(
        
        # Text input to facilitate search
        sszTextInput("suchfeld",
                     "Suchtext:"),
        
        # Select Date Range
        sszDateRange("date_range",
                     "Datum:",
                     start = "1933-01-01",
                     min = "1933-01-01",
                     end = Sys.Date(),
                     max = Sys.Date(),
                     format = "dd.mm.yyyy",
                     language = "de",
                     separator = icons_ssz("calendar")),
        
        # Select level of vote/referendum
        sszRadioButtons("abstimmungsebene",
                        "Politische Ebene der Abstimmung:",
                        choices = c("Alle Vorlagen", 
                                    "Eidgenössische Vorlagen", 
                                    "Kantonale Vorlagen", 
                                    "Städtische Vorlagen"),
                        selected = "Alle Vorlagen"),
        
        # Action Button
        conditionalPanel(
          condition = 'input.abfragestart==0',
          
          sszActionButton("abfragestart",
                          "Abfrage starten")
        ),
        
        br(),
        
        # Downloads
        conditionalPanel(
          condition = 'output.selected_vote',
          h3("Daten herunterladen"),
          
          # Download Panel
          tags$div(
            id = "downloadWrapperId",
            class = "downloadWrapperDiv",
            
            sszDownload("csv_download",
                        label = "csv"
            ),
            sszDownload("excel_download",
                        label = "xlsx"
            ),
            sszOgdDownload(inputId = "ogd_download",
                           label = "OGD",
                           onclick ="window.open('https://data.stadt-zuerich.ch/dataset/politik_abstimmungen_seit1933', '_blank')"
            )
          )
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        conditionalPanel(
          condition = 'input.abfragestart>0',
          
          # Title for table
          h1("Die untenstehenden Vorlagen entsprechen Ihren Suchkriterien"),
          hr(),
          # Define subtitle
          tags$div(
            class = "infoDiv",
            p("Für Detailinformationen zur Stimmbeteiligung und zum Ergebnis einer Abstimmung wählen Sie eine Zeile aus.")
          ),
          
          # Table Output to select vote
          reactableOutput("vote_list"),
          
          # Details: show only if one row selected _________
          
          # initialise hidden variable for row selection, to be used with JS function
          conditionalPanel("false",
                           numericInput(label = NULL, inputId = 'show_details', value = 0)),
          
          # Name of selected vote
          htmlOutput("title_vote"),
          
          # Details about selected vote
          reactableOutput("selected_vote")
          
        ),
      )
    )
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    
    # First button click to activate search, after not necessary anymore
    global <- reactiveValues(active_button = FALSE)
    
    observeEvent(input$abfragestart, {
      req(input$abfragestart)
      global$active_button <- TRUE
    })
    
    ## Test with Date
    date_range <- reactive({
      req(global$active_button == TRUE)
      date_range <- input$date_range
      date_range
    })
    
    data_date <- reactive({
      req(global$active_button == TRUE)
      datum <- data %>%
        dplyr::filter(
          `Politische Ebene` %in% input$abstimmungsebene,
          Datum >= input$date_range[1],
          Datum <= input$date_range[2]) %>%
        pull(Datum) %>%
        unique()
      datum
    })
    
    ## Get Data for Download
    filtered_data <- reactive({
      req(global$active_button == TRUE)
      # Filter: No Search
      if(input$suchfeld == "") {
        filtered <- data %>%
          dplyr::filter(Datum >= input$date_range[1] & Datum <= input$date_range[2]) %>%
          mutate(Datum = as.character(as.Date(Datum, "%d.%m.%Y")),
                 Stimmberechtigte = as.integer(Stimmberechtigte))  %>%
          select(Datum, `Politische Ebene`, Abstimmungstext, Gebiet, NrGebiet, Wahlkreis, Stimmberechtigte, `Ja-Stimmen`, `Nein-Stimmen`, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
        
        # Filter the level of vote
        if(input$abstimmungsebene == "Alle Vorlagen"){
          filtered
        }else{
          filtered <- filtered %>%
            filter(`Politische Ebene` %in% input$abstimmungsebene)
          filtered
        }
        
        # Filter: With Search
      } else {
        filtered <- data %>%
          filter(grepl(input$suchfeld, Abstimmungstext, ignore.case=TRUE)) %>%
          dplyr::filter(Datum >= input$date_range[1] & Datum <= input$date_range[2]) %>%
          mutate(Datum = as.character(as.Date(Datum, "%d.%m.%Y")),
                 Stimmberechtigte = as.integer(Stimmberechtigte))  %>%
          select(Datum, `Politische Ebene`, Abstimmungstext, Gebiet, NrGebiet, Wahlkreis, Stimmberechtigte, `Ja-Stimmen`, `Nein-Stimmen`, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
        
        # Filter the level of vote
        if(input$abstimmungsebene == "Alle Vorlagen"){
          filtered
        }else{
          filtered <- filtered %>%
            filter(`Politische Ebene` %in% input$abstimmungsebene)
          filtered
        }
      }
    })
    
    # Captions
    # Reactive Title
    title_reactive <- reactive({
      req(global$active_button == TRUE)
      if(input$suchfeld == ""){
        title <- "Ohne Suchtext"
      } else {
        title <- paste0("Suche: ", input$suchfeld)
      }
    })
    output$title <- renderText({
      title_reactive()
    })
    
    # Reactive Subtitle
    subtitle_reactive <- reactive({
      req(global$active_button == TRUE)
      subtitle <- input$abstimmungsebene
    })
    output$subtitle <- renderText({
      subtitle_reactive()
    })
    
    # Reactive Sub-Subtitle
    subsubtitle_reactive <- reactive({
      req(global$active_button == TRUE)
      subSubtitle <- paste0("Zeitraum: ", input$date_range[1], " bis ", input$date_range[2])
    })
    output$subSubtitle <- renderText({
      subsubtitle_reactive()
    })
    
    
    output$vote_list <- renderReactable({
      get_main_reactable(filtered_data())
    })
    
    rowNumber <- reactive( {
      print(input$show_details)
      input$show_details
    })
    
    observeEvent(eventExpr = list(input$suchfeld,input$date_range,input$abstimmungsebene),
                 handlerExpr = {
                   print("setting to zero")
                   updateNumericInput(session, "show_details", value = 0)},
                 ignoreNULL = FALSE)
    
    
    
    name_vote <- reactive({
      req(rowNumber())
      
      vote <- filtered_data() %>%
        select(Datum, `Politische Ebene`, Abstimmungstext) %>%
        unique() %>%
        mutate(ID = row_number()) %>%
        filter(ID == rowNumber())
      
      print(glue::glue("name_vote, row number: {rowNumber()}"))
      vote$Abstimmungstext
    })
    
    
    date_vote <- reactive({
      req(rowNumber())
      
      vote <- filtered_data() %>%
        select(Datum, `Politische Ebene`, Abstimmungstext) %>%
        unique() %>%
        mutate(ID = row_number()) %>%
        filter(ID == rowNumber())
      
      print("date_vote")
      vote$Datum
    })
    
    vote_data <- reactive({
      req(name_vote())
      
      vote <- filtered_data() %>%
        filter(Abstimmungstext == name_vote()) %>%
        rename(`Beteiligung (in %)` = `Stimmbeteiligung (in %)`,
               `Stimm-berechtigte` = `Stimmberechtigte`) %>% 
        arrange(NrGebiet) %>% 
        select(Gebiet, `Stimm-berechtigte`, `Ja-Stimmen`, `Nein-Stimmen`, `Beteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
      print("vote_data")
      vote
    })
    
    
    
    
    ## Write Download Table
    # CSV
    output$csv_download <- downloadHandler(
      filename = function(vote) {
        
        suchfeld <- gsub(" ", "-", name_vote(), fixed = TRUE) 
        time <- gsub(" ", "-", date_vote(), fixed = TRUE)
        paste0("Abstimmungsresultate_", suchfeld, "_", time, ".csv")
        
      },
      content = function(file) {
        write.csv(vote_data(), file, fileEncoding = "UTF-8", row.names = FALSE, na = " ")
      }
    )
    
    # Excel
    output$excel_download <- downloadHandler(
      filename = function(vote) {
        
        suchfeld <- gsub(" ", "-",  name_vote(), fixed = TRUE)
        time <- gsub(" ", "-", date_vote(), fixed = TRUE)
        paste0("Abstimmungsresultate_", suchfeld, "_", time, ".xlsx")
        
      },
      content = function(file) {
        ssz_download_excel(vote_data(), file, name_vote())
      }
    )
    
    output$title_vote <- renderText({
      req(name_vote())
      print("title_vote")
      
      paste("<br><h2>", name_vote(), "</h2><hr>")
    })
    
    output$selected_vote <- renderReactable({
      req(name_vote())
      get_second_reactable(filtered_data(), name_vote())
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}       
