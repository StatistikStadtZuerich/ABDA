

ui <- fluidPage(
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


server <- function(input, output, session) {
  
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