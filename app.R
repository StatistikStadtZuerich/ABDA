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
source("R/get_data.R", encoding = "UTF-8")
data <- get_data()

# Source Export Excel
source("R/ssz_download_excel.R", encoding = "UTF-8")


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
      tableOutput1 <- reactable(filtered_data() %>%
                                  select(Datum, `Politische Ebene`, Abstimmungstext) %>%
                                  unique()
                                ,
                                paginationType = "simple",
                                language = reactableLang(
                                  noData = "Keine Einträge gefunden",
                                  pageNumbers = "{page} von {pages}",
                                  pageInfo = "{rowStart} bis {rowEnd} von {rows} Einträgen",
                                  pagePrevious = "\u276e",
                                  pageNext = "\u276f",
                                  pagePreviousLabel = "Vorherige Seite",
                                  pageNextLabel = "Nächste Seite"
                                  
                                ),
                                theme = reactableTheme(
                                  borderColor = "#DEDEDE"
                                ),
                                columns = list(
                                  Datum = colDef(minWidth = 80, align = "left", cell = function(value) strftime(value, "%d.%m.%Y")),   # 12,5% width, 50px minimum
                                  `Politische Ebene` = colDef(minWidth = 100, align = "left"),   # 25% width, 100px minimum
                                  Abstimmungstext = colDef(minWidth = 225, align = "left") # 62,5% width, 250px minimum
                                ),
                                outlined = TRUE,
                                highlight = TRUE,
                                defaultPageSize = 5,
                                onClick = JS("function(rowInfo, column) {
    
    // Send the click event to Shiny, which will be available in input$show_details
    // Note that the row index starts at 0 in JavaScript, so we add 1
    if (window.Shiny) {
      Shiny.setInputValue('show_details', rowInfo.index + 1, { priority: 'event' })
    }
  }")
      )
      tableOutput1
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
      
      # Render a bar chart with a label on the left
      bar_chart <- function(label, width = "100%", height = "2rem", fill = "#00bfc4", background = NULL) {
        bar <- div(style = list(background = fill, width = width, height = height))
        chart <- div(style = list(flexGrow = 1, marginLeft = "0rem", background = background), bar)
        div(style = list(display = "flex"), chart)
      }
      
      # always have one decimal
      specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
      
      # Prepare dfs
      data_vote <- filtered_data() %>%
        filter(Abstimmungstext == name_vote()) %>%
        mutate(Chart_Anteil = specify_decimal(`Ja-Anteil (in %)`, 1),
               `Stimmbeteiligung (in %)` = as.numeric(`Stimmbeteiligung (in %)`),
               `Ja-Anteil (in %)` = specify_decimal(`Ja-Anteil (in %)`, 1),
               `Nein-Anteil (in %)` = specify_decimal(`Nein-Anteil (in %)`, 1)) %>%
        arrange(NrGebiet) %>% 
        select(Gebiet, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, Chart_Anteil,`Nein-Anteil (in %)`)
      
      data_detail <-filtered_data() %>%
        filter(Abstimmungstext == name_vote()) %>%
        select(Gebiet, Stimmberechtigte, `Ja-Stimmen`, `Nein-Stimmen`) %>% 
        pivot_longer(!Gebiet) %>% 
        # When row is empty or 0 (maily Stimmberechtigt is empty or 0 because of old data) then delete
        filter(!is.na(value) & value != 0) %>% 
        mutate(Test1 = " ",
               Test2 = " ", 
               Test3 = " ")
      
      tableOutput2 <- reactable(data_vote,
                                paginationType = "simple",
                                language = reactableLang(
                                  noData = "Keine Einträge gefunden",
                                  pageNumbers = "{page} von {pages}",
                                  pageInfo = "{rowStart} bis {rowEnd} von {rows} Einträgen",
                                  pagePrevious = "\u276e",
                                  pageNext = "\u276f",
                                  pagePreviousLabel = "Vorherige Seite",
                                  pageNextLabel = "Nächste Seite"
                                ),
                                theme = reactableTheme(
                                  borderColor = "#DEDEDE"
                                ),
                                outlined = TRUE,
                                highlight = TRUE,
                                columns = list(
                                  Gebiet =  colDef(minWidth = 40,
                                                   sortable = FALSE),
                                  `Stimmbeteiligung (in %)` = colDef(html = TRUE,
                                                                     name = "Beteiligung<br>(in %)",
                                                                     minWidth = 30,
                                                                     align = "left",
                                                                     cell = function(value) {
                                                                       if(!is.na(value)){
                                                                         return(specify_decimal(value, 1))
                                                                       } else {
                                                                         "–"
                                                                       }
                                                                     }), 
                                  `Ja-Anteil (in %)` = colDef(
                                    minWidth = 20,
                                    html = TRUE,
                                    name = "Ja-Anteil<br>(in %)",
                                    align = "right",
                                    headerClass = "barHeadershares"),
                                  Chart_Anteil = colDef(
                                    minWidth = 70,
                                    html = TRUE,
                                    name = "Ja-/Nein-<br>Anteil (in %)",
                                    align = "center",
                                    cell = function(value) {
                                      width <- paste0(value, "%")
                                      bar_chart(value, width = width, fill = "#0f05a0", background = "#ea4f61")
                                    },
                                    class = "bar",
                                    headerClass = "barHeader"),
                                  `Nein-Anteil (in %)` = colDef(
                                    minWidth = 20,
                                    name = " ",
                                    align = "left",
                                    class = "bar",
                                    headerClass = "barHeader")
                                ),
                                details = function(index) {
                                  det <- filter(data_detail, Gebiet == data_vote$Gebiet[index]) %>% select(-Gebiet)
                                  htmltools::div(
                                    class = "Details",
                                    reactable(det, 
                                              class = "innerTable",
                                              outlined = TRUE,
                                              fullWidth = TRUE,
                                              borderless = TRUE,
                                              theme = reactableTheme(
                                                borderColor = "#DEDEDE"
                                              ),
                                              columns = list(
                                                name = colDef(
                                                  name = "Details",
                                                  align = "left",
                                                  minWidth = 40,
                                                  sortable = FALSE
                                                ),
                                                value = colDef(
                                                  name = "Wert",
                                                  align = "left",
                                                  minWidth = 30,
                                                  sortable = FALSE,
                                                  cell = function(value) {
                                                    if (is.numeric(value)) {
                                                      format(value, big.mark = " ")
                                                    } else
                                                    {
                                                      return(value)
                                                    }
                                                  }
                                                ),
                                                # braucht leere Spalten für die ausklappbare Tabelle, damit die gleiche Spaltenanzahl
                                                Test1 = colDef(
                                                  minWidth = 20,
                                                  name = " ",
                                                  align = "center",
                                                  sortable = FALSE
                                                ),
                                                Test2 = colDef(
                                                  name = "",
                                                  align = "left",
                                                  minWidth = 70,
                                                  sortable = FALSE,
                                                  class = "spacer",
                                                  headerClass = "spacerHeader"),
                                                Test3 = colDef(
                                                  minWidth = 20,
                                                  name = " ",
                                                  align = "center",
                                                  sortable = FALSE,
                                                  class = "bar",
                                                  headerClass = "barHeader"
                                                )
                                              )
                                    )
                                  )
                                },
                                onClick = "expand",
                                defaultPageSize = 13
      )
      tableOutput2
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}       
