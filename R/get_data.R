### Required packages
library(dplyr)
library(httr)
library(data.table)
library(lubridate)

get_data <- function() {
  
  # By default the data frame is empty
  data <- NULL
  
  URL <- "https://data.stadt-zuerich.ch/dataset/politik_abstimmungen_seit1933/download/abstimmungen_seit1933.csv"
  
  tryCatch(                       # Applying tryCatch
    
    expr = {                      # Specifying expression
      ### 
      
      
      df <- data.table::fread(URL, encoding = "UTF-8")
      
      # Prepare Data for analysis
      data <- df %>%  
        dplyr::mutate(Abstimmungs_Datum = as.Date(Abstimmungs_Datum)) %>% 
        dplyr::mutate(Name_Resultat_Gebiet = case_when(
          Name_Resultat_Gebiet == "Stadt Zürich" & !is.na(Nr_Wahlkreis_StZH) ~ "Stadtkreise",
          TRUE ~ Name_Resultat_Gebiet
        )) %>% 
        dplyr::mutate(Name_Politische_Ebene = case_when(
          Name_Politische_Ebene == "Eidgenossenschaft" ~ "Eidgenössische Vorlagen",
          Name_Politische_Ebene == "Stadt Zürich" ~ "Städtische Vorlagen",
          Name_Politische_Ebene == "Kanton Zürich" ~ "Kantonale Vorlagen",
        )) %>% 
        dplyr::mutate(Name_Resultat_Gebiet = case_when(
          Name_Resultat_Gebiet == "Stadtkreise" ~ Name_Wahlkreis_StZH,
          Name_Resultat_Gebiet == "Eidgenossenschaft" ~ "Gesamte Schweiz",
          TRUE ~ Name_Resultat_Gebiet
        )) %>% 
        dplyr::mutate(NrGebiet = case_when(
          !is.na(Nr_Wahlkreis_StZH) ~ as.numeric(Nr_Wahlkreis_StZH*10),
          TRUE ~ as.numeric(Nr_Resultat_Gebiet)
        )) %>%
        
        # Auslandschweizer/-innen streichen
        filter(Name_Resultat_Gebiet != "Auslandschweizer/-innen") %>% 
        
        # Rename variables
        dplyr::rename(Abstimmungstext = Abstimmungs_Text,
                      Datum = Abstimmungs_Datum,
                      'Politische Ebene' = Name_Politische_Ebene,
                      'Wahlkreis' = Name_Wahlkreis_StZH,
                      'Gebiet' = Name_Resultat_Gebiet,
                      'Stimmberechtigte' = Stimmberechtigt,
                      'Ja-Stimmen' = Ja,
                      'Nein-Stimmen' = Nein,
                      'Stimmbeteiligung (in %)' = "Stimmbeteiligung (%)",
                      'Ja-Anteil (in %)' = "Ja (%)",
                      'Nein-Anteil (in %)' = "Nein (%)",
                      'Stände Ja' = StaendeJa,
                      'Stände Nein' = StaendeNein)
      rm(df)
      rm(cl)
      
    },
    
    error = function(e){          # Specifying error message
      message("Error in Data Load")
    },
    
    warning = function(w){        # Specifying warning message
      message("Warning in Data Load")
    }
  )
  
  return(data)
}
