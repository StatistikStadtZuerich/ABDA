### Required packages
library(tidyverse)
library(httr)
library(parallel)
library(data.table)
library(knitr)
library(kableExtra)
library(ggplot2)
library(xlsx)
library(lubridate)
library(DT)
library(shinydashboard)
library(reactable)


### Load Data
## URLS
URLs <- c("https://data.stadt-zuerich.ch/dataset/politik_abstimmungen_seit1933/download/abstimmungen_seit1933.csv")

## Download function
dataDownload <- function(link) {
    data <- data.table::fread(link,
                              encoding = "UTF-8")
}

## Download & Prepare Data
cl <- makeCluster(detectCores())
clusterExport(cl, "URLs")
data <- parLapply(cl, URLs, dataDownload)

df <- data.frame(Reduce(rbind, data))

data <- df %>%  
    mutate(Abstimmungs_Datum = as.Date(Abstimmungs_Datum, "%d.%m.%Y")) %>% 
    mutate(Name_Resultat_Gebiet = case_when(
        Name_Resultat_Gebiet == "Stadt Zürich" & !is.na(Nr_Wahlkreis_StZH) ~ "Stadtkreise",
        TRUE ~ Name_Resultat_Gebiet
    )) %>% 
    mutate(Name_Politische_Ebene = case_when(
        Name_Politische_Ebene == "Eidgenossenschaft" ~ "Eidgenössische Vorlagen",
        Name_Politische_Ebene == "Stadt Zürich" ~ "Städtische Vorlagen",
        Name_Politische_Ebene == "Kanton Zürich" ~ "Kantonale Vorlagen",
    )) %>% 
    mutate(Name_Resultat_Gebiet = case_when(
      Name_Resultat_Gebiet == "Stadtkreise" ~ Name_Wahlkreis_StZH,
      TRUE ~ Name_Resultat_Gebiet
    )) %>% 
    # ToDo:
    # Annäherung am Stimmbeteiligte dort wo sie fehlen?! Oder besser rausnehmen?
    mutate(Stimmberechtigt = case_when(
      is.na(Stimmberechtigt) ~ round((Ja + Nein)*(100/Stimmbeteiligung....), 0),
      TRUE ~ Stimmberechtigt
    )) %>%
    # Rename variables
    rename(Abstimmungstext = Abstimmungs_Text,
           Datum = Abstimmungs_Datum,
           'Politische Ebene' = Name_Politische_Ebene,
           'Wahlkreis' = Name_Wahlkreis_StZH,
           'Gebiet' = Name_Resultat_Gebiet,
           'Stimmberechtigte' = Stimmberechtigt,
           'Ja-Stimmen' = Ja,
           'Nein-Stimmen' = Nein,
           'Stimmbeteiligung (in %)' = Stimmbeteiligung....,
           'Ja-Anteil (in %)' = Ja....,
           'Nein-Anteil (in %)' = Nein....,
           'Stände Ja' = StaendeJa,
           'Stände Nein' = StaendeNein)

# Shiny App
library(shiny)

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
            
            br(),
            
            # Action Button
            actionButton("ActionButtonId",
                         "Abfrage starten"),
            
            br(),
            br(),
            
            # Downloads
            
            conditionalPanel(
                condition = 'output.selectedVote',
                h4("Daten herunterladen"),
                
                # Download Panel
                tags$div(
                  id = "downloadWrapperId",
                  class = "downloadWrapperDiv",
                  
                  actionButton(inputId = "csvDown",
                               label = "csv"
                  ),
                  actionButton(inputId = "excelDown",
                               label = "xlsx"
                  ),
                  actionButton(inputId = "ogdDown",
                               label = "OGD",
                               onclick ="window.open('https://data.stadt-zuerich.ch/dataset/politik_abstimmungen_seit1933', '_blank')"
                  )
                )
              )
            ),
        #         tags$div(
        #             id = "downloadLinkCSV",
        #             class = "downloadLink",
        #             icon("file-csv"),
        #             tags$div(
        #                 id = "downloadLinkCSVText",
        #                 class = "downloadLinkIcon",
        #                 downloadLink("downloadDataCSV", " .csv herunterladen")
        #             )
        #         ),
        #         tags$div(
        #             id = "downloadLinkEXCEL",
        #             class = "downloadLink",
        #             icon("file-excel"),
        #             tags$div(
        #                 id = "downloadLinkEXCELText",
        #                 class = "downloadLinkIcon",
        #                 downloadLink("downloadDataEXCEL", " .xlsx herunterladen")
        #             )
        #         ),
        #         tags$div(
        #             id = "linkOGD",
        #             class = "downloadLink",
        #             icon("database"),
        #             tags$div(
        #                 id = "downloadLinkOGDText",
        #                 class = "downloadLinkIcon",
        #                 tags$a(
        #                     class = "downloadLinkOGD",
        #                     href = "https://data.stadt-zuerich.ch/dataset/politik_abstimmungen_seit1933",
        #                     target="_blank",
        #                     " im OGD-Portal herunterladen"
        #                 )
        #             )
        #         )
        #     )
        # ),
        # 
        
        # Show a plot of the generated distribution
        mainPanel(
            
            #Table Title (prices)
            tags$div(
                id = "title_id",
                class = "title_div",
                textOutput("title")
            ),
            
            # Table Subtitle (prices)
            tags$div(
                id = "subtitle_id",
                class = "subtitle_div",
                textOutput("subtitle")
            ),
            
            # Table Subsubtitle (prices)
            tags$div(
                id = "subSubtitle_id",
                class = "subSubtitle_div",
                textOutput("subSubtitle")
            ),
            
            # Table for BZO 99 (prices)
            # htmlOutput("resultsPrice99")
            
            
            br(),
            reactableOutput("voteList"),
            br(),
            br(),
            htmlOutput("titleVote"),
            br(),
            reactableOutput("selectedVote")
                
            # # Verschiedene Tabs für Gebiete
            # conditionalPanel(
            #     condition = "input.ActionButtonId && input.ButtonGroupLabel == 'Alle Vorlagen' | input.ButtonGroupLabel == 'Eidgenössische Vorlagen'",
            #     tabsetPanel(
            #         id= "ttabs",
            #         # Select geographic context
            #         tabPanel("Resultat für Schweiz", value = 1, DT::dataTableOutput("voteListCH")),
            #         tabPanel("Resultat für Kanton Zürich",value = 2, DT::dataTableOutput("voteListKtZH")),
            #         tabPanel("Resultat für Stadt Zürich", value = 3, DT::dataTableOutput("voteListStZH")),
            #         tabPanel("Resultat für Stadtkreise", value = 4, DT::dataTableOutput("voteListZHkr"))
            #     )
            # ),
            # conditionalPanel(
            #     condition = "input.ActionButtonId && input.ButtonGroupLabel == 'Kantonale Vorlagen'",
            #     tabsetPanel(
            #         id= "ttabs",
            #         # Select geographic context
            #         tabPanel("Resultat für Kanton Zürich", value = 5, reactableOutput("voteListKtZH2")),
            #         tabPanel("Resultat für Stadt Zürich", value = 6, reactableOutput("voteListStZH2")),
            #         tabPanel("Resultat für Stadtkreise", value = 7, reactableOutput("voteListZHkr2"))
            #     )
            # ),
            # conditionalPanel(
            #     condition = "input.ActionButtonId && input.ButtonGroupLabel == 'Städtische Vorlagen'",
            #     tabsetPanel(
            #         id= "ttabs",
            #         # Select geographic context
            #         tabPanel("Resultat für Stadt Zürich", value = 8,reactableOutput("voteListStZH3")),
            #         tabPanel("Resultat für Stadtkreise", value = 9, reactableOutput("voteListZHkr3"))
            #     )
            #     # https://stackoverflow.com/questions/38797646/hyperlink-from-one-datatable-to-another-in-shiny
            # )
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
                                # %>% 
                                #   mutate(details = "")
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
                                columns = list(
                                  Datum = colDef(minWidth = 80, cell = function(value) strftime(value, "%d.%m.%Y")),   # 12,5% width, 50px minimum
                                  `Politische Ebene` = colDef(minWidth = 100),   # 25% width, 100px minimum
                                  Abstimmungstext = colDef(minWidth = 225) # 62,5% width, 250px minimum
                                  # details = colDef(
                                  #   name = "",
                                  #   cell = function() htmltools::tags$button("Details")
                                  # )
                                ),
                                # onClick = "select",
                                # onClick = JS("function(rowInfo, column) {
                                #   // Only handle click events on the 'details' column
                                #   if (column.id !== 'details') {
                                #     return
                                #   }
                                # 
                                #   // Display an alert dialog with details for the row
                                #   window.alert('Details for row ' + rowInfo.index + ':\\n' + JSON.stringify(rowInfo.values, null, 2))
                                # 
                                #   // Send the click event to Shiny, which will be available in input$show_details
                                #   // Note that the row index starts at 0 in JavaScript, so we add 1
                                #   if (window.Shiny) {
                                #     Shiny.setInputValue('show_details', { index: rowInfo.index + 1 }, { priority: 'event' })
                                #   }
                                # }"),
                                highlight = TRUE,
                                defaultPageSize = 5,
                                selection = "single", onClick = "select"
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
    output$csvDown <- downloadHandler(
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
    output$excelDown <- downloadHandler(
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
      
      paste("Resultate für: <br>", "<b>", print(nameVote()), "</b>")
    })
    
    output$selectedVote <- renderReactable({
        req(nameVote())

        tableOutput2 <- reactable(filteredData() %>%
                                      filter(Abstimmungstext == nameVote()) %>% 
                                      select(Gebiet, `Stimmberechtigte`, `Ja-Stimmen`, `Nein-Stimmen`, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`),
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
                                    Gebiet = colDef(minWidth = 50),   # 12,5% width, 50px minimum
                                    `Stimmberechtigte` = colDef(minWidth = 30),   # 25% width, 100px minimum
                                    `Ja-Stimmen` = colDef(minWidth = 30),  # 62,5% width, 250px minimum
                                    `Nein-Stimmen` = colDef(minWidth = 30),  # 62,5% width, 250px minimum
                                    `Stimmbeteiligung (in %)` = colDef(minWidth = 30),  # 62,5% width, 250px minimum
                                    `Ja-Anteil (in %)` = colDef(minWidth = 30),  # 62,5% width, 250px minimum
                                    `Nein-Anteil (in %)` = colDef(minWidth = 30)  # 62,5% width, 250px minimum
                                  ),
                                  defaultPageSize = 13,
        )
        tableOutput2
    })

    
    # # Verschiedene Tabs für Gebiete
    # output$voteListCH <- DT::renderDataTable({
    #     tableOutput1 <- filteredData() %>% filter(Gebiet == "Eidgenossenschaft")
    #     tableOutput1
    #     })
    # output$voteListKtZH <- DT::renderDataTable({
    #     tableOutput1 <- filteredData() %>% filter(Gebiet == "Kanton Zürich")
    #     tableOutput1
    # })
    # output$voteListStZH <- DT::renderDataTable({
    #     tableOutput1 <- filteredData() %>% filter(Gebiet == "Stadt Zürich")
    #     tableOutput1
    # })
    # output$voteListZHkr <- DT::renderDataTable({
    #     tableOutput1 <- filteredData() %>% filter(Gebiet == "Stadtkreise")
    #     tableOutput1
    # })
    # 
    # output$voteListKtZH2 <- renderReactable({
    #     tableOutput1 <- reactable(filteredData() %>% filter(Gebiet == "Kanton Zürich"))
    #     tableOutput1
    # })
    # output$voteListStZH2 <- renderReactable({
    #     tableOutput1 <- reactable(filteredData() %>% filter(Gebiet == "Stadt Zürich"))
    #     tableOutput1
    # })
    # output$voteListZHkr2 <- renderReactable({
    #     tableOutput1 <- reactable(filteredData() %>% filter(Gebiet == "Stadtkreise"))
    #     tableOutput1
    # })
    # 
    # output$voteListStZH3 <- renderReactable({
    #     tableOutput1 <- reactable(filteredData() %>% filter(Gebiet == "Stadt Zürich"))
    #     tableOutput1
    # })
    # output$voteListZHkr3 <- renderReactable({
    #     tableOutput1 <- reactable(filteredData() %>% filter(Gebiet == "Stadtkreise"))
    #     tableOutput1
    # })
 
          
    ## Change Action Query Button when first selected
    # observe({
    #     req(input$ActionButtonId)
    #     updateActionButton(session, "ActionButtonId",
    #                        label = "Erneute Abfrage",
    #                        icon = icon("refresh"))
    # })
    # 
    # observe({
    #   if(input$ActionButtonId == 0) return()
    #   shinyjs::disable("ActionButtonId")
    #   
    #   tryCatch(
    #     foo(),          
    #     error = function(e) return(),
    #     finally = shinyjs::enable("ActionButtonId")
    #   )
    # })

    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)
    