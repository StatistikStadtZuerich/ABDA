ssz_download_excel <- function(filtered_data, file, name_vote){
    
    # Data Paths
    hauptPfad <- "www/Titelblatt.xlsx"
    imagePfad <- "www/logo_stzh_stat_sw_pos_1.png"
    
    # Read Data
    data <- read_excel(hauptPfad, sheet = 1)
    
    # Manipulate Data
    # Data Sheet 1
    data <- data %>%
      mutate(
        Date = ifelse(is.na(Date), 
                      NA, 
                      paste0(format(Sys.Date(), "%d"), ".", 
                             format(Sys.Date(), "%m"), ".", 
                             format(Sys.Date(), "%Y"))),
        Titel = ifelse(is.na(Titel), 
                       NA, 
                       paste0("Abstimmungsresultate für Ihre Auswahl: ", 
                              name_vote))
        )
    
    selected <- c("T_1", 
                  "Abstimmungsresultate für Ihre Auswahl:", 
                  name_vote, 
                  " ", 
                  " ",
                  "Quelle: Statistik Stadt Zürich, Präsidialdepartement") %>% 
      as.data.frame()
      
    # Data Sheet 2
    # Styling
    sty <- createStyle(fgFill = "#ffffff")
    sty_concept <- createStyle(textDecoration = c("bold"),
                            valign = "top",
                            wrapText = TRUE)
    sty_definition <- createStyle(valign = "top",
                                 wrapText = TRUE)
    sty_title <- createStyle(fontName = "Arial Black")
    
    # Create Workbook
    wb <- createWorkbook()
    
    # Add Sheets
    addWorksheet(wb, sheetName = "Inhalt", gridLines = FALSE)
    addWorksheet(wb, sheetName = "T_1", gridLines = TRUE)
    
    # Write Table Sheet 1
    writeData(wb, sheet = 1, x = data,
                 colNames = FALSE, rowNames = FALSE,
                 startCol = 2,
                 startRow = 7,
                 withFilter = FALSE)
    
    # Write Table Sheet 2
    writeData(wb, sheet = 2, x = selected,
              colNames = FALSE, rowNames = FALSE,
              startCol = 1,
              startRow = 1,
              withFilter = FALSE)
    writeData(wb, sheet = 2, x = filtered_data,
            colNames = TRUE, rowNames = FALSE,
            startCol = 1,
            startRow = 9,
            withFilter = FALSE)
    
    # Insert Logo on Sheet 1
    insertImage(wb, imagePfad, sheet = 1, startRow = 2, startCol = 2, 
                width = 1.75 , height = 0.35)

    # Add Styling
    addStyle(wb, 1, style = sty, row = 1:19, cols = 1:6, gridExpand = TRUE)
    addStyle(wb, 1, style = sty_title, row = 14, cols = 2, gridExpand = TRUE)
    addStyle(wb, 2, style = sty_concept, row = 9, cols = 1:50, gridExpand = TRUE)
    modifyBaseFont(wb, fontSize = 8, fontName = "Arial")
    
    # Set Column Width
    setColWidths(wb, sheet = 1, cols = "A", widths = 1)
    setColWidths(wb, sheet = 1, cols = "B", widths = 4)
    setColWidths(wb, sheet = 1, cols = "D", widths = 40)
    setColWidths(wb, sheet = 1, cols = "5", widths = 8)
    
    
    # Save Excel
    saveWorkbook(wb, file, overwrite = TRUE) ## save to working directory
}
