library(tidyverse)
library(lubridate)
library(openxlsx)
library(readxl)
library(reactable)
library(icons)
library(shiny)
library(htmltools)
library(zuericssstyle)
library(zuericolors)

# Source Prepared Data
source("R/get_data.R")
data <- get_data()

# Source Export Excel
source("R/ssz_download_excel.R",)

# source functions to create reactables
source("R/get_main_reactable.R")
source("R/get_second_reactable.R")

# Set the Icon path
icons_ssz <- icon_set("www/icons/")

# if data load didn't work show message
if (is.null(data)) {
  
  # Define UI
  ui <- fluidPage(
    
    # Include CSS
    includeCSS("www/sszThemeShiny.css"),
    includeCSS("www/ABDATheme.css"),
    
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
    
    # CSS
    includeCSS("www/sszThemeShiny.css"),
    includeCSS("www/ABDATheme.css"),
    
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
            
            sszDownloadButton("csv_download",
                              label = "csv",
                              image = img(icons_ssz("download"))
            ),
            sszDownloadButton("excel_download",
                              label = "xlsx",
                              image = img(icons_ssz("download"))
            ),
            sszOgdDownload(outputId = "ogd_download",
                           label = "OGD",
                           href = "https://data.stadt-zuerich.ch/dataset/politik_abstimmungen_seit1933",
                           image = img(icons_ssz("external-link"))
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
          p(paste0("Für Detailinformationen zur Stimmbeteiligung und zum Ergebnis ",
                   "einer Abstimmung wählen Sie eine Zeile aus.")),
          
          # Table Output to select vote
          shinycssloaders::withSpinner(
            reactableOutput("vote_list"),
            type = 7,
            color = "#0F05A0"
          ),
          
          # Details: show only if one row selected _________
          
          # initialise hidden variable for row selection, to be used with JS function
          conditionalPanel("false",
                           numericInput(label = NULL, inputId = 'show_details', value = 0)),
          
          # Name of selected vote
          htmlOutput("title_vote"),
          
          # Details about selected vote
          shinycssloaders::withSpinner(
            reactableOutput("selected_vote"),
            type = 7,
            color = "#0F05A0"
          ),
        ),
      )
    )
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    
    ## Get Data for Download
    filtered_data <- reactive({
      req(input$abfragestart > 0)
      
      # Filter what needs to be filtered in any case
      filtered <- data %>%
        dplyr::filter(Datum >= input$date_range[1] & Datum <= input$date_range[2]) %>%
        mutate(Datum = as.character(as.Date(Datum, "%d.%m.%Y")),
               Stimmberechtigte = as.integer(Stimmberechtigte))  %>%
        select(Datum, `Politische Ebene`, Abstimmungstext, Gebiet, NrGebiet, Wahlkreis, Stimmberechtigte, `Ja-Stimmen`, `Nein-Stimmen`, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
      
      # Filter: Some search term entered
      if (input$suchfeld != "") {
        filtered <- filtered %>%
          filter(grepl(input$suchfeld, Abstimmungstext, ignore.case = TRUE))
      }
      
      # Filter the level of vote
      if (input$abstimmungsebene != "Alle Vorlagen") {
        filtered <- filtered %>%
          filter(`Politische Ebene` %in% input$abstimmungsebene)
      }
      
      filtered
    })
    
    output$vote_list <- renderReactable({
      get_main_reactable(filtered_data())
    })
    
    # once an input is changed, update input$show_details to zero so the extra
    # table etc. is not shown anymore
    observeEvent(eventExpr = list(input$suchfeld,input$date_range,input$abstimmungsebene),
                 handlerExpr = {
                   #print("setting to zero")
                   updateNumericInput(session, "show_details", value = 0)},
                 ignoreNULL = FALSE)
    
    # Name, date and data of selected vote as reactive, as they are used 
    # for csv and excel
    
    name_date_vote <- reactive({
      req(input$show_details > 0)
      
      vote <- filtered_data() %>%
        select(Datum, `Politische Ebene`, Abstimmungstext) %>%
        unique() %>%
        mutate(ID = row_number()) %>%
        filter(ID == input$show_details)
      #print(glue::glue("name_vote, row number: {input$show_details}"))
      vote
    })
    
    vote_data <- reactive({
      req(name_date_vote())
      
      vote <- filtered_data() %>%
        filter(Abstimmungstext == name_date_vote()$Abstimmungstext) %>%
        rename(`Beteiligung (in %)` = `Stimmbeteiligung (in %)`,
               `Stimm-berechtigte` = `Stimmberechtigte`) %>% 
        arrange(NrGebiet) %>% 
        select(Gebiet, `Stimm-berechtigte`, `Ja-Stimmen`, `Nein-Stimmen`, 
               `Beteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
      vote
    })
    
    ## Write Download Table
    # CSV
    output$csv_download <- downloadHandler(
      filename = function(vote) {
        
        suchfeld <- name_date_vote()$Abstimmungstext %>% 
          stringr::str_replace_all(" ", "-") %>% 
          stringr::str_replace_all("[:punct:]", "") 
        
        time <- stringr::str_replace(name_date_vote()$Datum, " ", "-") 
        
        filename <- paste0("Abstimmungsresultate_", suchfeld, "_", time, ".csv")
        
        filename
      },
      content = function(file) {
        write.csv(vote_data(), file, fileEncoding = "UTF-8", row.names = FALSE, na = " ")
      }
    )
    
    # Excel
    output$excel_download <- downloadHandler(
      filename = function(vote) {
        suchfeld <- name_date_vote()$Abstimmungstext %>% 
          stringr::str_replace_all(" ", "-") %>% 
          stringr::str_replace_all("[:punct:]", "") 
        
        time <- stringr::str_replace(name_date_vote()$Datum, " ", "-") 
        
        filename <- paste0("Abstimmungsresultate_", suchfeld, "_", time, ".xlsx")
        
        filename
      },
      content = function(file) {
        ssz_download_excel(vote_data(), file, name_date_vote()$Abstimmungstext)
      }
    )
    
    output$title_vote <- renderText({
      req(name_date_vote())
      paste("<br><h2>", name_date_vote()$Abstimmungstext, "</h2><hr>")
    })
    
    output$selected_vote <- renderReactable({
      req(name_date_vote())
      get_second_reactable(filtered_data(), name_date_vote()$Abstimmungstext)
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}       
