#' bar_chart
#'
#' @description function to render bar chart within a reactable, with label on the left
#'
#' @param label label to be shown on the left
#' @param width width of the bar chart, defaults to "100%"
#' @param height defaults to "2rem"
#' @param fill fill color, defaults to "#00bfc4"
#' @param background background color, defaults to NULL
#'
#' @return div with label and chart
bar_chart <- function(label, width = "100%", height = "2rem", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "0rem", background = background), bar)
  div(style = list(display = "flex"), chart)
}


#' get_second_reactable
#' 
#' @description function to prepare data and create secondary, expandable reactable for ABDA
#'
#' @param filtered_data 
#' @param name_vote 
#'
#' @return reactable
get_second_reactable <- function(filtered_data, name_vote) {
  
  # always have one decimal
  specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall = k))
  
  # Prepare dfs
  data_vote <- filtered_data %>%
    filter(Abstimmungstext == name_vote) %>%
    mutate(
      Chart_Anteil = specify_decimal(`Ja-Anteil (in %)`, 1),
      `Stimmbeteiligung (in %)` = as.numeric(`Stimmbeteiligung (in %)`),
      `Ja-Anteil (in %)` = specify_decimal(`Ja-Anteil (in %)`, 1),
      `Nein-Anteil (in %)` = specify_decimal(`Nein-Anteil (in %)`, 1)
      ) %>%
    arrange(NrGebiet) %>% 
    select(Gebiet, 
           `Stimmbeteiligung (in %)`, 
           `Ja-Anteil (in %)`, 
           Chart_Anteil,
           `Nein-Anteil (in %)`)
  
  data_detail <- filtered_data %>%
    filter(Abstimmungstext == name_vote) %>%
    select(Gebiet, Stimmberechtigte, `Ja-Stimmen`, `Nein-Stimmen`) %>% 
    pivot_longer(!Gebiet) %>% 
    # When row is empty or 0 (maily Stimmberechtigt is empty or 0 because of old data) then delete
    filter(!is.na(value) & value != 0) %>% 
    mutate(Test1 = " ",
           Test2 = " ", 
           Test3 = " ")
  
  
  
  reactable(data_vote,
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
              `Stimmbeteiligung (in %)` = colDef(
                html = TRUE,
                name = "Beteiligung<br>(in %)",
                minWidth = 30,
                align = "left",
                cell = function(value) {
                  if (!is.na(value)) {
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
                headerClass = "barHeadershares"
                ),
              Chart_Anteil = colDef(
                minWidth = 70,
                html = TRUE,
                name = "Ja-/Nein-<br>Anteil (in %)",
                align = "center",
                cell = function(value) {
                  width <- paste0(value, "%")
                  bar_chart(value, 
                            width = width, 
                            fill = "#0f05a0", 
                            background = "#ea4f61")
                },
                class = "bar",
                headerClass = "barHeader"
                ),
              `Nein-Anteil (in %)` = colDef(
                minWidth = 20,
                name = " ",
                align = "left",
                class = "bar",
                headerClass = "barHeader")
            ),
            details = function(index) {
              det <- filter(data_detail, 
                            Gebiet == data_vote$Gebiet[index]) %>% select(-Gebiet)
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
}