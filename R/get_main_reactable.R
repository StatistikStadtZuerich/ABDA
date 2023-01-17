#' get_main_reactable
#' 
#' @description function to create the main reactable for ABDA, with row selection (affects input$show_details)
#'
#' @param filtered_data 
#'
#' @return reactable
get_main_reactable <- function(filtered_data) {
  reactable(filtered_data %>%
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
              Datum = colDef(minWidth = 80, 
                             align = "left", 
                             cell = function(value) strftime(value, "%d.%m.%Y")),   # 12,5% width, 50px minimum
              `Politische Ebene` = colDef(minWidth = 100, 
                                          align = "left"),   # 25% width, 100px minimum
              Abstimmungstext = colDef(minWidth = 225, 
                                       align = "left") # 62,5% width, 250px minimum
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
}