### Required packages
packages <- c("tidyverse",
              "httr",
              "parallel",
              "data.table",
              "lubridate")

### Load packages
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

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
    TRUE ~ Name_Resultat_Gebiet
  )) %>% 
  
  # ToDo:
  # Annäherung am Stimmbeteiligte dort wo sie fehlen?! Oder besser rausnehmen?
  dplyr::mutate(Stimmberechtigt = case_when(
    is.na(Stimmberechtigt) ~ round((Ja + Nein)*(100/Stimmbeteiligung....), 0),
    TRUE ~ Stimmberechtigt
  )) %>%
  
  # Da Auslandschweizer/-innen nur von ZH streichen, um keine Verwirrung zu schaffen
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
         'Stimmbeteiligung (in %)' = Stimmbeteiligung....,
         'Ja-Anteil (in %)' = Ja....,
         'Nein-Anteil (in %)' = Nein....,
         'Stände Ja' = StaendeJa,
         'Stände Nein' = StaendeNein)

rm(df)
rm(cl)