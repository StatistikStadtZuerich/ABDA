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
          
            # tabsetPanel(
            #     conditionalPanel(
            #         condition = "input.buttonStart && input.selectEbene == 'Alle Vorlagen' | input.selectEbene == 'Eidgenössische Vorlagen'",
            #             # Select geographic context
            #             tabPanel("Resultat für Schweiz", DT::dataTableOutput("ResultatelisteCH")), 
            #             tabPanel("Resultat für Kanton Zürich",DT::dataTableOutput("ResultatelisteKtZH")), 
            #             tabPanel("Resultat für Stadt Zürich", DT::dataTableOutput("ResultatelisteStZH")), 
            #             tabPanel("Resultat für Stadtkreise", DT::dataTableOutput("ResultatelisteZHkr")) 
            #     ),
            #     conditionalPanel(
            #         condition = "input.buttonStart && input.selectEbene == 'Kantonale Vorlagen'",
            #             # Select geographic context
            #             tabPanel("Resultat für Kanton Zürich"),#, DT::dataTableOutput("ResultatelisteKtZH2")),
            #             tabPanel("Resultat für Stadt Zürich"),#, DT::dataTableOutput("ResultatelisteStZH2")),
            #             tabPanel("Resultat für Stadtkreise"),#, DT::dataTableOutput("ResultatelisteZHkr2"))
            #     ),
            #     conditionalPanel(
            #         condition = "input.buttonStart && input.selectEbene == 'Städtische Vorlagen'",
            #             # Select geographic context
            #             tabPanel("Resultat für Stadt Zürich"),#, DT::dataTableOutput("ResultatelisteStZH3")),
            #             tabPanel("Resultat für Stadtkreise"),#, DT::dataTableOutput("ResultatelisteZHkr3"))
            #     )
            # )
                
            conditionalPanel(
                condition = "input.buttonStart && input.selectEbene == 'Alle Vorlagen' | input.selectEbene == 'Eidgenössische Vorlagen'",
                tabsetPanel(
                    id= "ttabs",
                    # Select geographic context
                    tabPanel("Resultat für Schweiz", value=1,DT::dataTableOutput("ResultatelisteCH")),
                    tabPanel("Resultat für Kanton Zürich",value=2,DT::dataTableOutput("ResultatelisteKtZH")),
                    tabPanel("Resultat für Stadt Zürich", value=3, DT::dataTableOutput("ResultatelisteStZH")),
                    tabPanel("Resultat für Stadtkreise", value=4, DT::dataTableOutput("ResultatelisteZHkr"))
                )
            ),
            conditionalPanel(
                condition = "input.buttonStart && input.selectEbene == 'Kantonale Vorlagen'",
                tabsetPanel(
                    id= "ttabs",
                    # Select geographic context
                    tabPanel("Resultat für Kanton Zürich", value=5, DT::dataTableOutput("ResultatelisteKtZH2")),
                    tabPanel("Resultat für Stadt Zürich", value=6, DT::dataTableOutput("ResultatelisteStZH2")),
                    tabPanel("Resultat für Stadtkreise", value=7, DT::dataTableOutput("ResultatelisteZHkr2"))
                )
            ),
            conditionalPanel(
                condition = "input.buttonStart && input.selectEbene == 'Städtische Vorlagen'",
                tabsetPanel(
                    id= "ttabs",
                    # Select geographic context
                    tabPanel("Resultat für Stadt Zürich", value=8, DT::dataTableOutput("ResultatelisteStZH3")),
                    tabPanel("Resultat für Stadtkreise", value=9, DT::dataTableOutput("ResultatelisteZHkr3"))
                )
            )

            
            # # Option 3
            # # Ebene 1:
            # conditionalPanel(
            #     condition = 'input.selectEbene == "Alle Vorlagen"',
            #     
            #     uiOutput("tabCH")
            # ),
            # # Ebene 2:
            # conditionalPanel(
            #     condition = 'input.selectEbene == "Eidgenössische Vorlagen"',
            #     
            #     uiOutput("tabCH")
            # ),
            # # Ebene 3:
            # conditionalPanel(
            #     condition = 'input.selectEbene == "Kantonale Vorlagen',
            #     
            #     uiOutput("tabKtZH")
            # ),
            # # Ebene 4:
            # conditionalPanel(
            #     condition = 'input.selectEbene == "Städtische Vorlagen',
            #     
            #     uiOutput("tabStZH")
            # )
            
            
            # Option 4
           #uiOutput("tabs"),
           #uiOutput("table")
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
    
    # # Option1
    # output$Resultateliste <- DT::renderDataTable({
    #     dataDownload()
    # })
    
    # # Option 2
    output$ResultatelisteCH <- DT::renderDataTable( {
        dataDownload() %>% filter(Gebiet == "Eidgenossenschaft")
        })

    output$ResultatelisteKtZH <- DT::renderDataTable( {
        dataDownload() %>% filter(Gebiet == "Kanton Zürich")
    })
    output$ResultatelisteStZH <- DT::renderDataTable({
        dataDownload() %>% filter(Gebiet == "Stadt Zürich")
    })
    output$ResultatelisteZHkr <- DT::renderDataTable( {
        dataDownload() %>% filter(Gebiet == "Stadtkreise")
    })
    
    output$ResultatelisteKtZH2 <- DT::renderDataTable( {
        dataDownload() %>% filter(Gebiet == "Kanton Zürich")
    })
    output$ResultatelisteStZH2 <- DT::renderDataTable({
        dataDownload() %>% filter(Gebiet == "Stadt Zürich")
    })
    output$ResultatelisteZHkr2 <- DT::renderDataTable( {
        dataDownload() %>% filter(Gebiet == "Stadtkreise")
    })
    
    output$ResultatelisteStZH3 <- DT::renderDataTable({
        dataDownload() %>% filter(Gebiet == "Stadt Zürich")
    })
    output$ResultatelisteZHkr3 <- DT::renderDataTable( {
        dataDownload() %>% filter(Gebiet == "Stadtkreise")
    })
    
    # # Option 3
    # output$tabCH <- renderUI({
    #     tabsetPanel(
    #         # Select geographic context
    #         tabPanel("Resultat für Schweiz", DT::dataTableOutput("ResultatelisteCH")),
    #         tabPanel("Resultat für Kanton Zürich", DT::dataTableOutput("ResultatelisteKtZH")),
    #         tabPanel("Resultat für Stadt Zürich", DT::dataTableOutput("ResultatelisteStZH")),
    #         tabPanel("Resultat für Stadtkreise", DT::dataTableOutput("ResultatelisteZHkr"))
    #     )
    # })
    # 
    # output$tabKtZH <- renderUI({
    #     tabsetPanel(
    #         # Select geographic context
    #         tabPanel("Resultat für Schweiz", DT::dataTableOutput("ResultatelisteCH")),
    #         tabPanel("Resultat für Kanton Zürich", DT::dataTableOutput("ResultatelisteKtZH")),
    #         tabPanel("Resultat für Stadt Zürich", DT::dataTableOutput("ResultatelisteStZH")),
    #         tabPanel("Resultat für Stadtkreise", DT::dataTableOutput("ResultatelisteZHkr"))
    #     )
    # })
    # 
    # output$tabStZH <- renderUI({
    #     tabsetPanel(
    #         # Select geographic context
    #         tabPanel("Resultat für Schweiz", DT::dataTableOutput("ResultatelisteCH")),
    #         tabPanel("Resultat für Kanton Zürich", DT::dataTableOutput("ResultatelisteKtZH")),
    #         tabPanel("Resultat für Stadt Zürich", DT::dataTableOutput("ResultatelisteStZH")),
    #         tabPanel("Resultat für Stadtkreise", DT::dataTableOutput("ResultatelisteZHkr"))
    #     )
    # })
    
    
    # Option 4
    
    # whichTab <- reactive({
    #     req(input$selectEbene)
    #     if(input$selectEbene == "Alle Vorlagen"){
    #         check <- "check1"
    #     } else if(input$selectEbene == "Eidgenössische Vorlagen"){
    #         check <- "check2"
    #     }else if(input$selectEbene == "Kantonale Vorlagen"){
    #         check <- "check3"
    #     }else if(input$selectEbene == "Städtische Vorlagen"){
    #         check <- "check4"
    #     }
    #     check
    # })
   
    # 
    # output$ResultatelisteCH <- DT::renderDataTable({
    #     if(input$selectEbene == "Alle Vorlagen" | input$selectEbene == "Eidgenössische Vorlagen"){
    #         dataDownload() %>% filter(Gebiet == "Eidgenossenschaft")
    #     } else {
    #         return(NULL)
    #     }
    # })
    # 
    # output$ResultatelisteCH <- DT::renderDataTable({
    #     if(input$selectEbene == "Alle Vorlagen" | input$selectEbene == "Eidgenössische Vorlagen" | input$selectEbene == "Kantonale Vorlagen"){
    #         dataDownload() %>% filter(Gebiet == "Kanton Zürich")
    #     } else {
    #         return(NULL)
    #     }
    # })
    # 
    # output$ResultatelisteStZH <- DT::renderDataTable({
    #     dataDownload() %>% filter(Gebiet == "Stadt Zürich")
    # })
    # 
    # output$ResultatelisteZHKr <- DT::renderDataTable( {
    #     dataDownload() %>% filter(Gebiet == "Stadtkreise")
    # })

    # output$ResultatelisteCH <- eventReactive(input$buttonStart, {
    #     data <- dataDownload() %>% filter(Gebiet == "Eidgenossenschaft") %>% 
    #         DT::datatable()
    #     data
    # })
    # 
    # ResultatelisteKtZH <- eventReactive(input$buttonStart, {
    #     data <- dataDownload() %>% filter(Gebiet == "Kanton Zürich") %>% 
    #         DT::datatable()
    #     data
    # })
    # ResultatelisteStZH <- eventReactive(input$buttonStart, {
    #     data <- dataDownload() %>% filter(Gebiet == "Stadt Zürich") %>% 
    #         DT::datatable()
    #     data
    # })
    # ResultatelisteZHkr <- eventReactive(input$buttonStart, {
    #     data <- dataDownload() %>% filter(Gebiet == "Stadtkreise") %>% 
    #         DT::datatable()
    #     data
    # })
    # output$table <- renderUI({

        # if(whichTab() == "check1"){
        #     output$ResultatelisteCH <- DT::renderDataTable( {
        #         dataDownload() %>% filter(Gebiet == "Eidgenossenschaft")
        #     })
        #     
        #     output$ResultatelisteKtZH <- DT::renderDataTable( {
        #         dataDownload() %>% filter(Gebiet == "Kanton Zürich")
        #     })
        #     output$ResultatelisteStZH <- DT::renderDataTable({
        #         dataDownload() %>% filter(Gebiet == "Stadt Zürich")
        #     })
        #     output$ResultatelisteZHKr <- DT::renderDataTable( {
        #         dataDownload() %>% filter(Gebiet == "Stadtkreise")
        #     })
        # }
        # else if(whichTab() == "check2"){
        #     output$ResultatelisteCH <- DT::renderDataTable( {
        #         dataDownload() %>% filter(Gebiet == "Eidgenossenschaft")
        #     })
        #     
        #     output$ResultatelisteKtZH <- DT::renderDataTable( {
        #         dataDownload() %>% filter(Gebiet == "Kanton Zürich")
        #     })
        #     output$ResultatelisteStZH <- DT::renderDataTable({
        #         dataDownload() %>% filter(Gebiet == "Stadt Zürich")
        #     })
        #     output$ResultatelisteZHKr <- DT::renderDataTable( {
        #         dataDownload() %>% filter(Gebiet == "Stadtkreise")
        #     })
        # }
        # else if(whichTab() == "check3"){
        #     output$ResultatelisteKtZH <- DT::renderDataTable( {
        #         dataDownload() %>% filter(Gebiet == "Kanton Zürich")
        #     })
        #     output$ResultatelisteStZH <- DT::renderDataTable({
        #         dataDownload() %>% filter(Gebiet == "Stadt Zürich")
        #     })
        #     output$ResultatelisteZHKr <- DT::renderDataTable( {
        #         dataDownload() %>% filter(Gebiet == "Stadtkreise")
        #     })
        # }
        # else if(whichTab() == "check4"){
        #     output$ResultatelisteStZH <- DT::renderDataTable({
        #         dataDownload() %>% filter(Gebiet == "Stadt Zürich")
        #     })
        #     output$ResultatelisteZHKr <- DT::renderDataTable( {
        #         dataDownload() %>% filter(Gebiet == "Stadtkreise")
        #     })
        # }
        # else{return(NULL)}
    # })
    
    # observeEvent(input$buttonStart, {
    #     output$ResultCH <- renderText({
    #         ResultCHout <- ResultatelisteCH()
    #         ResultCHout
    #     })
    # })
    # 
    # output$tabs <- renderUI({
    # 
    #     
    #     if(whichTab() == "check1"){
    #         tabsetPanel(id= "ttabs",
    #                tabPanel("Resultat für Schweiz", ResultCH()),
    #                tabPanel("Resultat für Kanton Zürich", DT::dataTableOutput("ResultatelisteKtZH")),
    #                tabPanel("Resultat für Stadt Zürich", DT::dataTableOutput("ResultatelisteStZH")),
    #                tabPanel("Resultat für Stadtkreise", DT::dataTableOutput("ResultatelisteZHkr"))
    #         )
    #     }
    #     else if(whichTab() == "check2"){
    #         tabsetPanel(id= "ttabs",
    #                tabPanel("Resultat für Schweiz", DT::dataTableOutput("ResultatelisteCH")),
    #                tabPanel("Resultat für Kanton Zürich", DT::dataTableOutput("ResultatelisteKtZH")),
    #                tabPanel("Resultat für Stadt Zürich", DT::dataTableOutput("ResultatelisteStZH")),
    #                tabPanel("Resultat für Stadtkreise", DT::dataTableOutput("ResultatelisteZHkr"))
    #         )
    #     }
    #     else if(whichTab() == "check3"){
    #         tabsetPanel(id= "ttabs",
    #                tabPanel("Resultat für Kanton Zürich", DT::dataTableOutput("ResultatelisteKtZH")),
    #                tabPanel("Resultat für Stadt Zürich", DT::dataTableOutput("ResultatelisteStZH")),
    #                tabPanel("Resultat für Stadtkreise", DT::dataTableOutput("ResultatelisteZHkr"))
    #         )
    #     }
    #     else if(whichTab() == "check4"){
    #         tabsetPanel(id= "ttabs",
    #                tabPanel("Resultat für Stadt Zürich", DT::dataTableOutput("ResultatelisteStZH")),
    #                tabPanel("Resultat für Stadtkreise", DT::dataTableOutput("ResultatelisteZHkr"))
    #         )
    #     }
    #     else{return(NULL)}
    #     })
        
        
       
    
   

        
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
    