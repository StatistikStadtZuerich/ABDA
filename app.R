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
    # CSS
    includeCSS("sszTheme.css"),
    
    br(),
    
    # Application title
    titlePanel("Abstimmungsresultate App"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        # Sidebar Layout
        sidebarPanel(
            
            # Text input to facilitate search
            textInput("textSearch",
                      "Suchtext:"),
            
            # Select Date Range
            dateRangeInput("selectDateRange",
                           "Datum (z.B. 25.01.2004):",
                           start = "1993-01-01",
                           end = Sys.Date(),
                           format = "dd.mm.yyyy",
                           startview = "month",
                           weekstart = 0,
                           language = "de",
                           separator = " bis "),
            
            # Select level of vote/referendum
            radioButtons("selectPolLevel",
                         "Politische Ebene der Abstimmung:",
                         choices = c("Alle Vorlagen", "Eidgenössische Vorlagen", "Kantonale Vorlagen", "Städtische Vorlagen"),
                         selected = "Alle Vorlagen"),
            
            
            # Action Button
            actionButton("buttonStart",
                         "Abfrage starten", 
                         icon = icon("database")),
            br(),
            
            # Downloads
            conditionalPanel(
                condition = 'input.buttonStart',
                h5("Daten herunterladen"),
                tags$div(
                    id = "downloadLinkCSV",
                    class = "downloadLink",
                    icon("file-csv"),
                    tags$div(
                        id = "downloadLinkCSVText",
                        class = "downloadLinkIcon",
                        downloadLink("downloadDataCSV", " .csv herunterladen")
                    )
                ),
                tags$div(
                    id = "downloadLinkEXCEL",
                    class = "downloadLink",
                    icon("file-excel"),
                    tags$div(
                        id = "downloadLinkEXCELText",
                        class = "downloadLinkIcon",
                        downloadLink("downloadDataEXCEL", " .xlsx herunterladen")
                    )
                ),
                tags$div(
                    id = "linkOGD",
                    class = "downloadLink",
                    icon("database"),
                    tags$div(
                        id = "downloadLinkOGDText",
                        class = "downloadLinkIcon",
                        tags$a(
                            class = "downloadLinkOGD",
                            href = "https://data.stadt-zuerich.ch/dataset/politik_abstimmungen_seit1933",
                            target="_blank",
                            " im OGD-Portal herunterladen"
                        )
                    )
                )
            )
        ),
        
        
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
            #     condition = "input.buttonStart && input.selectPolLevel == 'Alle Vorlagen' | input.selectPolLevel == 'Eidgenössische Vorlagen'",
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
            #     condition = "input.buttonStart && input.selectPolLevel == 'Kantonale Vorlagen'",
            #     tabsetPanel(
            #         id= "ttabs",
            #         # Select geographic context
            #         tabPanel("Resultat für Kanton Zürich", value = 5, reactableOutput("voteListKtZH2")),
            #         tabPanel("Resultat für Stadt Zürich", value = 6, reactableOutput("voteListStZH2")),
            #         tabPanel("Resultat für Stadtkreise", value = 7, reactableOutput("voteListZHkr2"))
            #     )
            # ),
            # conditionalPanel(
            #     condition = "input.buttonStart && input.selectPolLevel == 'Städtische Vorlagen'",
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
    
    ## Test with Date
    dataRange <- eventReactive(input$buttonStart, {
        dateRange <- input$selectDateRange
        dateRange
    })
    
    dataDate <- eventReactive(input$buttonStart, {
        datum <- data %>%
            dplyr::filter(
                `Politische Ebene` %in% input$selectPolLevel,
                Datum >= input$selectDateRange[1],
                Datum <= input$selectDateRange[2]) %>% 
            pull(Datum) %>% 
            unique()
        datum
    })
        
    ## Get Data for Download
    filteredData <- eventReactive(input$buttonStart, {
        
        # Filter: No Search
        if(input$textSearch == "") {
            filtered <- data %>%
                dplyr::filter(Datum >= input$selectDateRange[1] & Datum <= input$selectDateRange[2]) %>% 
                mutate(Datum = as.character(as.Date(Datum, "%d.%m.%Y")),
                       Stimmberechtigte = as.integer(Stimmberechtigte))  %>% 
                select(Datum, `Politische Ebene`, Abstimmungstext, Gebiet, Wahlkreis, Stimmberechtigte, `Ja-Stimmen`, `Nein-Stimmen`, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
            
            # Filter the level of vote
            if(input$selectPolLevel == "Alle Vorlagen"){
                filtered
            }else{
              filtered <- filtered %>% 
                  filter(`Politische Ebene` %in% input$selectPolLevel)
              filtered
            }
            
        # Filter: With Search   
        } else {
            filtered <- data %>%
                filter(grepl(input$textSearch, Abstimmungstext, ignore.case=TRUE)) %>%
                dplyr::filter(Datum >= input$selectDateRange[1] & Datum <= input$selectDateRange[2]) %>% 
                mutate(Datum = as.character(as.Date(Datum, "%d.%m.%Y")),
                       Stimmberechtigte = as.integer(Stimmberechtigte))  %>% 
                select(Datum, `Politische Ebene`, Abstimmungstext, Gebiet, Wahlkreis, Stimmberechtigte, `Ja-Stimmen`, `Nein-Stimmen`, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
            
            # Filter the level of vote
            if(input$selectPolLevel == "Alle Vorlagen"){
                filtered
            }else{
                filtered <- filtered %>% 
                    filter(`Politische Ebene` %in% input$selectPolLevel)
                filtered
            }
        }
    })
    
    # Captions
    # Reactive Title
    titleReactive <- eventReactive(input$buttonStart, {
        if(input$textSearch == ""){
            title <- "Ohne Suchtext"
        } else {
            title <- paste0("Suche: ", input$textSearch)
        }
    })
    output$title <- renderText({
        titleReactive()
    })
    
    # Reactive Subtitle
    subtitleReactive <- eventReactive(input$buttonStart, {
        subtitle <- input$selectPolLevel
    })
    output$subtitle <- renderText({
        subtitleReactive()
    })
    
    # Reactive Sub-Subtitle
    subSubtitleReactive <- eventReactive(input$buttonStart, {
        subSubtitle <- paste0("Zeitraum: ", input$selectDateRange[1], " bis ", input$selectDateRange[2])
    })
    output$subSubtitle <- renderText({
        subSubtitleReactive()
    })
    
    
    ## Write Download Table
    # CSV
    output$downloadDataCSV <- downloadHandler(
        filename = function(price) {
            textSearch <- input$textSearch
            if(textSearch == "") {
                textSearch <- gsub(" ", "-", "Alle Abstimmungen", fixed = TRUE)
                level <- gsub(" ", "-", input$selectPolLevel, fixed = TRUE)
                time1 <- gsub(" ", "-", input$selectDateRange[1], fixed = TRUE)
                time2 <- gsub(" ", "-", input$selectDateRange[2], fixed = TRUE)
                paste0("Abstimmungsresultate_", textSearch, "_", level, "_", time1, "_bis_", time2, ".csv")
            } else {
                textSearch <- gsub(" ", "-", input$price, fixed = TRUE)
                level <- gsub(" ", "-", input$selectPolLevel, fixed = TRUE)
                time1 <- gsub(" ", "-", input$selectDateRange[1], fixed = TRUE)
                time2 <- gsub(" ", "-", input$selectDateRange[2], fixed = TRUE)
                paste0("Abstimmungsresultate_", textSearch, "_", level, "_", time1, "_bis_", time2, ".csv")
            }
        },
        content = function(file) {
            write.csv(filteredData(), file, row.names = FALSE, na = " ")
        }
    )
    
    # Excel
    output$downloadDataEXCEL <- downloadHandler(
        filename = function(price) {
            textSearch <- input$textSearch
            if(textSearch == "") {
                textSearch <- gsub(" ", "-", "Alle Abstimmungen", fixed = TRUE)
                level <- gsub(" ", "-", input$selectPolLevel, fixed = TRUE)
                time1 <- gsub(" ", "-", input$selectDateRange[1], fixed = TRUE)
                time2 <- gsub(" ", "-", input$selectDateRange[2], fixed = TRUE)
                paste0("Abstimmungsresultate_", textSearch, "_", level, "_", time1, "_bis_", time2, ".xlsx")
            } else {
                textSearch <- gsub(" ", "-", input$price, fixed = TRUE)
                level <- gsub(" ", "-", input$selectPolLevel, fixed = TRUE)
                time1 <- gsub(" ", "-", input$selectDateRange[1], fixed = TRUE)
                time2 <- gsub(" ", "-", input$selectDateRange[2], fixed = TRUE)
                paste0("Abstimmungsresultate_", textSearch, "_", level, "_", time1, "_bis_", time2, ".xlsx")
            }
        },
        content = function(file) {
            xlsx::write.xlsx(filteredData(), file, row.names = FALSE, showNA = FALSE)
        }
    )
  
    
    output$voteList <- renderReactable({
        tableOutput1 <- reactable(filteredData() %>% 
                                      select(Datum, `Politische Ebene`, Abstimmungstext) %>% 
                                      unique(),
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
                                      Datum = colDef(minWidth = 50),   # 12,5% width, 50px minimum
                                      `Politische Ebene` = colDef(minWidth = 100),   # 25% width, 100px minimum
                                      Abstimmungstext = colDef(minWidth = 250)  # 62,5% width, 250px minimum
                                      # Abstimmungstext = colDef(cell = function(value) {
                                      #     htmltools::tags$a(href = value, target = "_blank", value)
                                      # })
                                      ),
                                  defaultPageSize = 5,
                                  selection = "single", onClick = "select"
                                  )
        tableOutput1
        })
    
    rowNumber <- reactive({
        getReactableState("voteList", "selected")
    })
    
    nameVote <- reactive({
        req(rowNumber())
        
        vote <- filteredData() %>% 
            filter(row_number() == rowNumber())
        
        print(vote$Abstimmungstext)
    })
    
    output$titleVote <- renderText({
        req(nameVote())
        
        paste("Resultate für: <br>", "<b>", nameVote(), "</b>")
    })
    
    output$selectedVote <- renderReactable({
        req(nameVote())

        tableOutput2 <- reactable(filteredData() %>%
                                      filter(Abstimmungstext == nameVote()) %>% 
                                      select(Gebiet, `Stimmberechtigte`, `Ja-Stimmen`, `Nein-Stimmen`, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
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
    observe({
        req(input$buttonStart)
        updateActionButton(session, "buttonStart",
                           label = "Erneute Abfrage",
                           icon = icon("refresh"))
    })
    
    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)
    