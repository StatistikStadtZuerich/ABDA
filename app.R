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
            textInput("suchfeld",
                      "Suchtext:"),
            
            # Select Date Range
            dateRangeInput("selectZeitraum",
                           "Datum (z.B. 25.01.2004):",
                           start = "1993-01-01",
                           end = Sys.Date(),
                           format = "dd.mm.yyyy",
                           startview = "month",
                           weekstart = 0,
                           language = "de",
                           separator = " bis "),
            
            # Select level of vote/referendum
            radioButtons("selectEbene",
                         "Politische Ebene der Abstimmung:",
                         choices = c("Alle Vorlagen", "Eidgenössische Vorlagen", "Kantonale Vorlagen", "Städtische Vorlagen"),
                         selected = "Alle Vorlagen"),
            
            
            # # Ebene 1: 
            # conditionalPanel(
            #     condition = 'input.selectEbene == "Alle politischen Ebenen"',
            #     
            #     # Select geographic context
            #     selectInput("selectArea",
            #                 "Resultat für Gebiet:",
            #                 choices = c("Alle Gebiete", "Eidgenossenschaft", "Kanton Zürich", "Stadt Zürich", "Stadtkreise"),
            #                 selected = "Stadt Zürich")
            #     
            # ),
            # 
            # 
            # # Ebene 2: 
            # conditionalPanel(
            #     condition = 'input.selectEbene == "Eidgenossenschaft"',
            #     
            #     # Select geographic context
            #     selectInput("selectArea",
            #                 "Resultat für Gebiet:",
            #                 choices = c("Alle Gebiete", "Eidgenossenschaft", "Kanton Zürich", "Stadt Zürich", "Stadtkreise"),
            #                 selected = "Stadt Zürich")
            #     
            # ),
            # 
            # # Ebene 3: 
            # conditionalPanel(
            #     condition = 'input.selectEbene == "Kanton Zürich"',
            #     
            #     # Select geographic context
            #     selectInput("selectArea",
            #                 "Resultat für Gebiet:",
            #                 choices = c("Alle Gebiete", "Kanton Zürich", "Stadt Zürich", "Stadtkreise"),
            #                 selected = "Stadt Zürich")
            #     
            # ),
            # 
            # # Ebene 4: 
            # conditionalPanel(
            #     condition = 'input.selectEbene == "Stadt Zürich"',
            #     
            #     # Select geographic context
            #     selectInput("selectArea",
            #                 "Resultat für Gebiet:",
            #                 choices = c("Alle Gebiete", "Stadt Zürich", "Stadtkreise"),
            #                 selected = "Stadt Zürich")
            #     
            # ),
            
            
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
            DT::dataTableOutput("Resultateliste")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    ## Test with Date
    dataRange <- eventReactive(input$buttonStart, {
        dateRange <- input$selectZeitraum
        dateRange
    })
    
    dataDate <- eventReactive(input$buttonStart, {
        datum <- data %>%
            dplyr::filter(
                `Politische Ebene` %in% input$selectEbene,
                Datum >= input$selectZeitraum[1],
                Datum <= input$selectZeitraum[2]) %>% 
            pull(Datum) %>% 
            unique()
        datum
    })
        
    ## Get Data for Download
    dataDownload <- eventReactive(input$buttonStart, {
        
        # Filter: No Search
        if(input$suchfeld == "") {
            filtered <- data %>%
                dplyr::filter(Datum >= input$selectZeitraum[1] & Datum <= input$selectZeitraum[2]) %>% 
                mutate(Datum = as.character(as.Date(Datum, "%d.%m.%Y")),
                       Stimmberechtigte = as.integer(Stimmberechtigte))  %>% 
                select(Datum, `Politische Ebene`, Abstimmungstext, Gebiet, Wahlkreis, Stimmberechtigte, `Ja-Stimmen`, `Nein-Stimmen`, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
            
            # Filter the level of vote
            if(input$selectEbene == "Alle Vorlagen"){
                filtered
            }else{
              filtered <- filtered %>% 
                  filter(`Politische Ebene` %in% input$selectEbene)
              filtered
            }
            
        # Filter: With Search   
        } else {
            filtered <- data %>%
                filter(grepl(input$suchfeld, Abstimmungstext, ignore.case=TRUE)) %>%
                dplyr::filter(Datum >= input$selectZeitraum[1] & Datum <= input$selectZeitraum[2]) %>% 
                mutate(Datum = as.character(as.Date(Datum, "%d.%m.%Y")),
                       Stimmberechtigte = as.integer(Stimmberechtigte))  %>% 
                select(Datum, `Politische Ebene`, Abstimmungstext, Gebiet, Wahlkreis, Stimmberechtigte, `Ja-Stimmen`, `Nein-Stimmen`, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
            
            # Filter the level of vote
            if(input$selectEbene == "Alle Vorlagen"){
                filtered
            }else{
                filtered <- filtered %>% 
                    filter(`Politische Ebene` %in% input$selectEbene)
                filtered
            }
        }
    })
    
    # Captions
    # Reactive Title
    titleReactive <- eventReactive(input$buttonStart, {
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
    subtitleReactive <- eventReactive(input$buttonStart, {
        subtitle <- input$selectEbene
    })
    output$subtitle <- renderText({
        subtitleReactive()
    })
    
    # Reactive Sub-Subtitle
    subSubtitleReactive <- eventReactive(input$buttonStart, {
        subSubtitle <- paste0("Zeitraum: ", input$selectZeitraum[1], " bis ", input$selectZeitraum[2])
    })
    output$subSubtitle <- renderText({
        subSubtitleReactive()
    })
    
    
        ## Write Download Table
        # CSV
        output$downloadDataCSV <- downloadHandler(
            filename = function(price) {
                suchfeld <- input$suchfeld
                if(suchfeld == "") {
                    suchfeld <- gsub(" ", "-", "Alle Abstimmungen", fixed = TRUE)
                    level <- gsub(" ", "-", input$selectEbene, fixed = TRUE)
                    time1 <- gsub(" ", "-", input$selectZeitraum[1], fixed = TRUE)
                    time2 <- gsub(" ", "-", input$selectZeitraum[2], fixed = TRUE)
                    paste0("Abstimmungsresultate_", suchfeld, "_", level, "_", time1, "_bis_", time2, ".csv")
                } else {
                    suchfeld <- gsub(" ", "-", input$price, fixed = TRUE)
                    level <- gsub(" ", "-", input$selectEbene, fixed = TRUE)
                    time1 <- gsub(" ", "-", input$selectZeitraum[1], fixed = TRUE)
                    time2 <- gsub(" ", "-", input$selectZeitraum[2], fixed = TRUE)
                    paste0("Abstimmungsresultate_", suchfeld, "_", level, "_", time1, "_bis_", time2, ".csv")
                }
            },
            content = function(file) {
                write.csv(dataDownload(), file, row.names = FALSE, na = " ")
            }
        )
        
        # Excel
        output$downloadDataEXCEL <- downloadHandler(
            filename = function(price) {
                suchfeld <- input$suchfeld
                if(suchfeld == "") {
                    suchfeld <- gsub(" ", "-", "Alle Abstimmungen", fixed = TRUE)
                    level <- gsub(" ", "-", input$selectEbene, fixed = TRUE)
                    time1 <- gsub(" ", "-", input$selectZeitraum[1], fixed = TRUE)
                    time2 <- gsub(" ", "-", input$selectZeitraum[2], fixed = TRUE)
                    paste0("Abstimmungsresultate_", suchfeld, "_", level, "_", time1, "_bis_", time2, ".xlsx")
                } else {
                    suchfeld <- gsub(" ", "-", input$price, fixed = TRUE)
                    level <- gsub(" ", "-", input$selectEbene, fixed = TRUE)
                    time1 <- gsub(" ", "-", input$selectZeitraum[1], fixed = TRUE)
                    time2 <- gsub(" ", "-", input$selectZeitraum[2], fixed = TRUE)
                    paste0("Abstimmungsresultate_", suchfeld, "_", level, "_", time1, "_bis_", time2, ".xlsx")
                }
            },
            content = function(file) {
                xlsx::write.xlsx(dataDownload(), file, row.names = FALSE, showNA = FALSE)
            }
        )
        
        
        
        output$Resultateliste <- DT::renderDataTable({
            dataDownload() 
        })

        
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
    