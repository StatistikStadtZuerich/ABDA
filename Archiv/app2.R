### Required packages
library(tidyverse)
library(xlsx)
library(reactable)
library(icons)
library(shiny)
library(htmltools)

# Source Download Function
source("sszDownload.R", local = TRUE)

# Source Prepared Data
source("prepareData.R", local = TRUE, encoding = "UTF-8")

# Set the Icon path
icon <- icon_set("icons/")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Abstimmungsresultate App"),
    
    # Horizontal Ruler
    tags$hr(),
    
    # CSS
    includeCSS("sszTheme.css"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        # Sidebar Layout
        sidebarPanel(
          
          # Define subtitle
          # h2("Abfrage definieren:"),
            
          # Text input to facilitate search
          textInput("suchfeld",
                    "Suchtext:"),
          
          # Select Date Range
          dateRangeInput("DateRange",
                         "Datum:",
                         start = "1993-01-01",
                         end = Sys.Date(),
                         format = "dd.mm.yyyy",
                         language = "de",
                         separator = icon("calendar")),
          
          # Select level of vote/referendum
          tags$div(
            class = "radioDiv",
            radioButtons("ButtonGroupLabel",
                         "Politische Ebene der Abstimmung:",
                         choices = c("Alle Vorlagen", 
                                     "Eidgenössische Vorlagen", 
                                     "Kantonale Vorlagen", 
                                     "Städtische Vorlagen"),
                         selected = "Alle Vorlagen")
          ),
          
          
          # Action Button
          conditionalPanel(
            condition = 'input.ActionButtonId==0',
            
            actionButton("ActionButtonId",
                         "Abfrage starten")
          ),
          conditionalPanel(
            condition = 'input.ActionButtonId>0',
         
          ),
          
          br(),
          
          # Downloads
          conditionalPanel(
              condition = 'output.selectedVote',
              h3("Daten herunterladen"),
              
              # Download Panel
              tags$div(
                id = "downloadWrapperId",
                class = "downloadWrapperDiv",
                
                sszDownload("csvDownload",
                            label = "csv"
                ),
                sszDownload("excelDownload",
                            label = "xlsx"
                ),
                actionButton(inputId = "ogdDown",
                             label = "OGD",
                             onclick ="window.open('https://data.stadt-zuerich.ch/dataset/politik_abstimmungen_seit1933', '_blank')"
                )
              )
            )
          ),
        
        # Show a plot of the generated distribution
        mainPanel(
          
          conditionalPanel(
            condition = 'output.voteList',
            
            # Define subtitle
            h3("Folgende Abstimmungen entsprechen Ihren Suchergebnissen:")
          ),
   
          # Table Output to select vote
          reactableOutput("voteList"),
          
          # Name of selected vote
          htmlOutput("titleVote"),
          
          # Details about selected vote
          reactableOutput("selectedVote")

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # First button click to activate search, after not necessary anymore
    global <- reactiveValues(activeButton = FALSE)
    
    observeEvent(input$ActionButtonId, {
      req(input$ActionButtonId)
      global$activeButton <- TRUE
    })  
  
  
    ## Test with Date
    dataRange <- reactive({
      req(global$activeButton == TRUE)
        dateRange <- input$DateRange
        dateRange
    })
    
    dataDate <- reactive({
      req(global$activeButton == TRUE)
        datum <- data %>%
            dplyr::filter(
                `Politische Ebene` %in% input$ButtonGroupLabel,
                Datum >= input$DateRange[1],
                Datum <= input$DateRange[2]) %>% 
            pull(Datum) %>% 
            unique()
        datum
    })
        
    ## Get Data for Download
    filteredData <- reactive({ 
      req(global$activeButton == TRUE)
        # Filter: No Search
        if(input$suchfeld == "") {
            filtered <- data %>%
                dplyr::filter(Datum >= input$DateRange[1] & Datum <= input$DateRange[2]) %>% 
                mutate(Datum = as.character(as.Date(Datum, "%d.%m.%Y")),
                       Stimmberechtigte = as.integer(Stimmberechtigte))  %>% 
                select(Datum, `Politische Ebene`, Abstimmungstext, Gebiet, Wahlkreis, Stimmberechtigte, `Ja-Stimmen`, `Nein-Stimmen`, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
            
            # Filter the level of vote
            if(input$ButtonGroupLabel == "Alle Vorlagen"){
                filtered
            }else{
              filtered <- filtered %>% 
                  filter(`Politische Ebene` %in% input$ButtonGroupLabel)
              filtered
            }
            
        # Filter: With Search   
        } else {
            filtered <- data %>%
                filter(grepl(input$suchfeld, Abstimmungstext, ignore.case=TRUE)) %>%
                dplyr::filter(Datum >= input$DateRange[1] & Datum <= input$DateRange[2]) %>% 
                mutate(Datum = as.character(as.Date(Datum, "%d.%m.%Y")),
                       Stimmberechtigte = as.integer(Stimmberechtigte))  %>% 
                select(Datum, `Politische Ebene`, Abstimmungstext, Gebiet, Wahlkreis, Stimmberechtigte, `Ja-Stimmen`, `Nein-Stimmen`, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
            
            # Filter the level of vote
            if(input$ButtonGroupLabel == "Alle Vorlagen"){
                filtered
            }else{
                filtered <- filtered %>% 
                    filter(`Politische Ebene` %in% input$ButtonGroupLabel)
                filtered
            }
        }
    })
    
    # Captions
    # Reactive Title
    titleReactive <- reactive({
      req(global$activeButton == TRUE)
        if(input$suchfeld == ""){
            title <- "Ohne Suchtext"
        } else {
            title <- paste0("Suche: ", input$suchfeld)
        }
    })
    output$title <- renderText({
        titleReactive()
    })
    
    # Reactive Subtitle
    subtitleReactive <- reactive({
      req(global$activeButton == TRUE)
        subtitle <- input$ButtonGroupLabel
    })
    output$subtitle <- renderText({
        subtitleReactive()
    })
    
    # Reactive Sub-Subtitle
    subSubtitleReactive <- reactive({
      req(global$activeButton == TRUE)
        subSubtitle <- paste0("Zeitraum: ", input$DateRange[1], " bis ", input$DateRange[2])
    })
    output$subSubtitle <- renderText({
        subSubtitleReactive()
    })
    
    
    output$voteList <- renderReactable({
      tableOutput1 <- reactable(filteredData() %>% 
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
                                  Datum = colDef(minWidth = 80, cell = function(value) strftime(value, "%d.%m.%Y")),   # 12,5% width, 50px minimum
                                  `Politische Ebene` = colDef(minWidth = 100),   # 25% width, 100px minimum
                                  Abstimmungstext = colDef(minWidth = 225) # 62,5% width, 250px minimum
                                ),
                                outlined = TRUE,
                                highlight = FALSE,
                                defaultPageSize = 5,
                                onClick = "select",
                                selection = "single",
                                rowClass = JS("function(rowInfo) {return rowInfo.selected ? 'selected' : ''}"),
                                rowStyle = JS("function(rowInfo) {if (rowInfo.selected) { return { backgroundColor: '#F2F2F2'}}}")
      )
      tableOutput1
    })
    
    rowNumber <- reactive( {
      getReactableState("voteList", "selected")
    })
    
    
    nameVote <- reactive({
      req(rowNumber())
      
      vote <- filteredData() %>% 
        select(Datum, `Politische Ebene`, Abstimmungstext) %>% 
        unique() %>% 
        mutate(ID = row_number()) %>% 
        filter(ID == rowNumber())
      
      print(vote$Abstimmungstext)
    })
    
    
    dateVote <- reactive({
      req(rowNumber())
      
      vote <- filteredData() %>% 
        select(Datum, `Politische Ebene`, Abstimmungstext) %>% 
        unique() %>% 
        mutate(ID = row_number()) %>% 
        filter(ID == rowNumber())
      
      print(vote$Datum)
    })
    
    voteData <- reactive({
      req(nameVote())
      
      vote <- filteredData() %>%
        filter(Abstimmungstext == nameVote()) %>% 
        select(Gebiet, `Stimmberechtigte`, `Ja-Stimmen`, `Nein-Stimmen`, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
    })
    
    
    
    
    ## Write Download Table
    # CSV
    output$csvDownload <- downloadHandler(
        filename = function(vote) {

            suchfeld <- gsub(" ", "-", nameVote(), fixed = TRUE)
            time <- gsub(" ", "-", dateVote(), fixed = TRUE)
            paste0("Abstimmungsresultate_", suchfeld, "_", time, ".csv")
            
        },
        content = function(file) {
            write.csv(voteData(), file, row.names = FALSE, na = " ")
        }
    )
    
    # Excel
    output$excelDownload <- downloadHandler(
      filename = function(vote) {
        
        suchfeld <- gsub(" ", "-", nameVote(), fixed = TRUE)
        time <- gsub(" ", "-", dateVote(), fixed = TRUE)
        paste0("Abstimmungsresultate_", suchfeld, "_", time, ".xlsx")
        
      },
        content = function(file) {
            xlsx::write.xlsx(voteData(), file, row.names = FALSE, showNA = FALSE)
        }
    )
  
    
    output$titleVote <- renderText({
      req(nameVote())
      
      paste("<h3>Resultat für:</h3>", "<h2>", print(nameVote()), "</h2>")
    })
    
    output$selectedVote <- renderReactable({
        req(nameVote())
      
        # Render a bar chart with a label on the left
        bar_chart <- function(label, width = "100%", height = "2rem", fill = "#00bfc4", background = NULL) {
          bar <- div(style = list(background = fill, width = width, height = height))
          chart <- div(style = list(flexGrow = 1, marginLeft = "1.5rem", background = background), bar)
          div(style = list(display = "flex", alignItems = "center"), label, chart)
        }
      
        # always have one decimal
        specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
        
        # Prepare dfs
        data_vote <- filteredData() %>%
          filter(Abstimmungstext == nameVote()) %>% 
          mutate(`Stimmbeteiligung (in %)` = specify_decimal(`Stimmbeteiligung (in %)`, 1),
                 `Ja-Anteil (in %)` = specify_decimal(`Ja-Anteil (in %)`, 1),
                 `Nein-Anteil (in %)` = specify_decimal(`Nein-Anteil (in %)`, 1)) %>% 
          select(Gebiet, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
        
        data_detail <-filteredData() %>%
          filter(Abstimmungstext == nameVote()) %>% 
          select(Gebiet, Stimmberechtigte, `Ja-Stimmen`, `Nein-Stimmen`)

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
                                  columns = list(
                                    Gebiet =  colDef(minWidth = 30),
                                    `Stimmbeteiligung (in %)` = colDef(minWidth = 30),
                                    `Ja-Anteil (in %)` = colDef(
                                      minWidth = 50,
                                      name = "Abstimmungsergebnis (in %)",
                                      align = "left", 
                                      cell = function(value) {
                                      width <- paste0(value, "%")
                                      bar_chart(value, width = width, fill = "#A5C0BE", background = "#E0AAB2")
                                    }),
                                    `Nein-Anteil (in %)` = colDef(
                                      minWidth = 15,
                                      name = "", 
                                      align = "left") 
                                  ),
                                  details = function(index) {
                                    det <- filter(data_detail, Gebiet == data_vote$Gebiet[index]) %>% select(-Gebiet)
                                    tbl <- reactable(det, 
                                                     outlined = TRUE,
                                                     fullWidth = FALSE, 
                                                     defaultColDef = reactable::colDef(
                                                       minWidth = 150,
                                                       cell = function(value) {
                                        
                                                          # Format only numeric columns with thousands separators
                                                          if (is.numeric(value)) {
                                                            format(value, big.mark = " ")
                                                          } else
                                                          {
                                                            return(value)
                                                          }
                                                        }
                                                      ))
                                    htmltools::div(tbl)
                                  },
                                  onClick = "expand",
                                  defaultPageSize = 13
        )
        tableOutput2
    })
    }
    
# Run the application 
shinyApp(ui = ui, server = server)
    