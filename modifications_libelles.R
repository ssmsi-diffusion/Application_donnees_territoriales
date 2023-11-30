library("shiny")
library(tidyverse)
repertoire = "C:/Users/ssmsi/Documents/AppTerritoires/donneesTerritoriales"
setwd(repertoire)

load("./libelles.Rdata")

# Ajout de "sur personnes de 15 ans ou plus

libelles$lIndicateurs = libelles$lIndicateurs %>%
  mutate(lib.indicateur = 
           case_when(
             lib.indicateur == "Coups et blessures volontaires" ~ "Coups et blessures volontaires sur personnes de 15 ans ou plus",
             lib.indicateur == "Coups et blessures volontaires intrafamiliaux" ~ "Coups et blessures volontaires intrafamiliaux sur personnes de 15 ans ou plus",
             lib.indicateur == "Autres coups et blessures volontaires" ~ "Autres coups et blessures volontaires sur personnes de 15 ans ou plus",
             TRUE ~ lib.indicateur 
           )
         )

libelles$lIndicateurs = libelles$lIndicateurs %>%
  mutate(lib.indicateur = 
           case_when(
             lib.indicateur == "Coups et blessures volontaires sur personnes de 15 ans ou plus" ~ "Coups et blessures volontaires",
             lib.indicateur == "Coups et blessures volontaires intrafamiliaux sur personnes de 15 ans ou plus" ~ "Coups et blessures volontaires intrafamiliaux",
             lib.indicateur == "Autres coups et blessures volontaires sur personnes de 15 ans ou plus" ~ "Autres coups et blessures volontaires",
             TRUE ~ lib.indicateur 
           )
  )

# Retrait de "sur personnes de 15 ans ou plus

libelles$lIndicateurs = libelles$lIndicateurs %>%
  mutate(lib.indicateur = 
           case_when(
             lib.indicateur == "Coups et blessures volontaires sur personnes de 15 ans ou plus" ~ "Coups et blessures volontaires",
             lib.indicateur == "Coups et blessures volontaires intrafamiliaux sur personnes de 15 ans ou plus" ~ "Coups et blessures volontaires intrafamiliaux",
             lib.indicateur == "Autres coups et blessures volontaires sur personnes de 15 ans ou plus" ~ "Autres coups et blessures volontaires",
             TRUE ~ lib.indicateur 
           )
  )

# Modification des "Autres coups et blessure

libelles$lIndicateurs = libelles$lIndicateurs %>%
  mutate(lib.indicateur = 
           case_when(
             lib.indicateur == "Autres coups et blessures volontaires" ~ "Coups et blessures volontaires en dehors du cadre familial",
             TRUE ~ lib.indicateur 
           )
  )

save(libelles,file = "./libelles.Rdata")
