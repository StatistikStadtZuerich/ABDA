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
df2 <- df %>% 
    mutate(Name_Politische_Ebene = "Alle politischen Ebenen")

data <- df2 %>% 
    bind_rows(df) %>% 
    mutate(Abstimmungs_Datum = as.Date(Abstimmungs_Datum, "%d.%m.%Y")) %>% 
    mutate(Name_Resultat_Gebiet = case_when(
        Name_Resultat_Gebiet == "Stadt Zürich" & !is.na(Nr_Wahlkreis_StZH) ~ "Stadtkreise",
        TRUE ~ Name_Resultat_Gebiet
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
                           end = NULL,
                           format = "dd.mm.yyyy",
                           startview = "month",
                           weekstart = 0,
                           language = "de",
                           separator = " bis "),
            
            # Select level of vote/referendum
            radioButtons("selectEbene",
                         "Politische Ebene der Abstimmung:",
                         choices = unique(data$`Politische Ebene`),
                         selected = "Stadt Zürich"),
            
            
            # Ebene 1: 
            conditionalPanel(
                condition = 'input.selectEbene == "Alle politischen Ebenen"',
                
                # Select geographic context
                selectInput("selectArea",
                            "Resultat für Gebiet:",
                            choices = c("Alle Gebiete", "Eidgenossenschaft", "Kanton Zürich", "Stadt Zürich", "Stadtkreise"),
                            selected = "Stadt Zürich")
                
            ),
            
            
            # Ebene 2: 
            conditionalPanel(
                condition = 'input.selectEbene == "Eidgenossenschaft"',
                
                # Select geographic context
                selectInput("selectArea",
                            "Resultat für Gebiet:",
                            choices = c("Alle Gebiete", "Eidgenossenschaft", "Kanton Zürich", "Stadt Zürich", "Stadtkreise"),
                            selected = "Stadt Zürich")
                
            ),
            
            # Ebene 3: 
            conditionalPanel(
                condition = 'input.selectEbene == "Kanton Zürich"',
                
                # Select geographic context
                selectInput("selectArea",
                            "Resultat für Gebiet:",
                            choices = c("Alle Gebiete", "Kanton Zürich", "Stadt Zürich", "Stadtkreise"),
                            selected = "Stadt Zürich")
                
            ),
            
            # Ebene 4: 
            conditionalPanel(
                condition = 'input.selectEbene == "Stadt Zürich"',
                
                # Select geographic context
                selectInput("selectArea",
                            "Resultat für Gebiet:",
                            choices = c("Alle Gebiete", "Stadt Zürich", "Stadtkreise"),
                            selected = "Stadt Zürich")
                
            ),
            
            
            # Action Button
            actionButton("buttonStart",
                         " Abfrage starten", 
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
            textOutput("Suchmaschine"),
            DT::dataTableOutput("Resultateliste"),
            textOutput("DatumTest"),
            textOutput("RangeTest")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
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
        dataDownload <- eventReactive(input$suchfeld, {
            
            # Filter: No Search
            if(is.null(input$suchfeld)) {
                filtered <- data %>%
                    dplyr::filter(`Politische Ebene` %in% input$selectEbene,
                                  Datum >= input$selectZeitraum[1],
                                  Datum <= input$selectZeitraum[2]) %>% 
                    mutate(Datum = as.character(as.Date(Datum, "%d.%m.%Y")),
                           Stimmberechtigte = as.integer(Stimmberechtigte)) 
                
                # Filter Area for Results of Vote/Referendum
                if(input$selectArea == "Alle Gebiete") {
                    filtered <- filtered %>% 
                        select(Datum, `Politische Ebene`, Abstimmungstext, Gebiet, Wahlkreis, Stimmberechtigte, `Ja-Stimmen`, `Nein-Stimmen`, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
                } else if(input$selectArea == "Stadtkreise") {
                    filtered <- filtered %>% 
                        filter(Gebiet == input$selectArea) %>% 
                        select(Datum, `Politische Ebene`, Abstimmungstext, Gebiet, Wahlkreis, Stimmberechtigte, `Ja-Stimmen`, `Nein-Stimmen`, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
                } else {
                    filtered <- filtered %>% 
                        filter(Gebiet == input$selectArea) %>% 
                        select(Datum, `Politische Ebene`, Abstimmungstext, Gebiet, Stimmberechtigte, `Ja-Stimmen`, `Nein-Stimmen`, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
                }
                filtered
                
                # Filter: With Search   
                # hier müssen wir case sensitivity rausnehmen!!!
            } else {
                filtered <- data %>%
                    filter(grepl(input$suchfeld, Abstimmungstext)) %>%
                    filter(`Politische Ebene` %in% input$selectEbene,
                           Datum >= input$selectZeitraum[1],
                           Datum <= input$selectZeitraum[2]) %>% 
                    mutate(Datum = as.character(as.Date(Datum, "%d.%m.%Y")),
                           Stimmberechtigte = as.integer(Stimmberechtigte)) 
                
                # Filter Area for Results of Vote/Referendum
                if(input$selectArea == "Alle Gebiete") {
                    filtered <- filtered %>% 
                        select(Datum, `Politische Ebene`, Abstimmungstext, Gebiet, Wahlkreis, Stimmberechtigte, `Ja-Stimmen`, `Nein-Stimmen`, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
                } else if(input$selectArea == "Stadtkreise") {
                    filtered <- filtered %>% 
                        filter(Gebiet == input$selectArea) %>% 
                        select(Datum, `Politische Ebene`, Abstimmungstext, Gebiet, Wahlkreis, Stimmberechtigte, `Ja-Stimmen`, `Nein-Stimmen`, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
                } else {
                    filtered <- filtered %>% 
                        filter(Gebiet == input$selectArea) %>% 
                        select(Datum, `Politische Ebene`, Abstimmungstext, Gebiet, Stimmberechtigte, `Ja-Stimmen`, `Nein-Stimmen`, `Stimmbeteiligung (in %)`, `Ja-Anteil (in %)`, `Nein-Anteil (in %)`)
                }
                filtered
            }
        })
        
        
        ## Write Download Table
        # CSV
        output$downloadDataCSV <- downloadHandler(
            filename = function(price) {
                suchfeld <- input$suchfeld
                if(suchfeld == "") {
                    suchfeld <- gsub(" ", "-", "Alle Abstimmungen", fixed = TRUE)
                    level <- gsub(" ", "-", input$selectEbene, fixed = TRUE)
                    area <- gsub(" ", "-", input$selectArea, fixed = TRUE)
                    time1 <- gsub(" ", "-", input$selectZeitraum[1], fixed = TRUE)
                    time2 <- gsub(" ", "-", input$selectZeitraum[2], fixed = TRUE)
                    paste0("Abstimmungsresultate_", suchfeld, "_", level, "_", area, "_", time1, "_bis_", time2, ".csv")
                } else {
                    suchfeld <- gsub(" ", "-", input$price, fixed = TRUE)
                    level <- gsub(" ", "-", input$selectEbene, fixed = TRUE)
                    area <- gsub(" ", "-", input$selectArea, fixed = TRUE)
                    time1 <- gsub(" ", "-", input$selectZeitraum[1], fixed = TRUE)
                    time2 <- gsub(" ", "-", input$selectZeitraum[2], fixed = TRUE)
                    paste0("Abstimmungsresultate_", suchfeld, "_", level, "_", area, "_", time1, "_bis_", time2, ".csv")
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
        }
        
        output$DatumTest <- renderText({
            dataDate()
        })
        
        output$RangeTest <- renderText({
            dataRange()
        })
        
        
    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)
    