# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sys.setenv(OPENSSL_CONF="/dev/null")

# Packages

library(shinythemes)
library(shinyjs)
library(htmltools)
library(fresh)
library(sf)
library(tidyverse)
library(leaflet)
library(shinyBS)
library(htmlwidgets)
library(shinycssloaders)
library(webshot)
library(mapview)

# Thème de l'application

monTheme = create_theme(
  theme = "united",
  bs_vars_navbar(
    default_bg = "#2C4F9E",
    default_color = "#FFFFFF",
    default_link_color = "#FFFFFF",
    default_link_active_color = "#FFFFFF",
    default_link_active_bg = "#F4983A",
    default_link_hover_color = "#FFFFFF"
  ),
  output_file = NULL
)

# Données

load("./cartes.16.22.RData")
load("./uu.RData")

# Restriction du jeu de données aux donnéesmises à disposition sur data.gouv.fr

# for(aaaa in names(cartes)){
#   
#   for(uu in names(cartes[[aaaa]])){
#     
#     cartes[[aaaa]][[uu]]["carte.ref.nat"] = NULL
#     cartes[[aaaa]][[uu]]["carte.ref.uu"] = NULL
#     
#   }
#   
# } 

# Contours géographiques des communes

com.shp = sf::st_read("./shp/communes/communes-met-drom-20220101-simpl04.shp", stringsAsFactors = FALSE) %>% rename_with(~tolower(.x))

if(!webshot::is_phantomjs_installed()){
  webshot::install_phantomjs()
}

# Pour la gestion des fonds de cartes

fondsCartes = tibble(
  "fonds" = c(providers$OpenStreetMap.Mapnik, 
              providers$Esri.WorldImagery, 
              providers$Stamen.Toner, 
              providers$CartoDB.Positron),
  "groupes" = c("Plan","Vue satellite","Vue noir et blanc","Gris")
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
