################################################################################
##    Ce script contient les packages et les fonctions nécessaires pour la    ##
##    création et l'import des cartes/styles vers geoserver.                 ##
################################################################################

# Packages----------------------------------------------------------------------
library(shinythemes)
library(shinyWidgets)
library(DT)
library(shinyBS)
library(htmltools)
library(ggplot2)
library(ggfortify)
library(ggthemes)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(htmltools)
library(RColorBrewer)
library(stringdist)
library(shinycssloaders)
library(fresh)
library(XML)
library(tidyverse)
library(stringi)
library(geosapi)
library(highcharter)
library(shinyjs)
library(mapview)

## Espace de travail 
setwd("M:/Commun/Territoires/Diffusion/Application/v0/baseCommunale")

# Données-----------------------------------------------------------------------
## Données de délinquance communale, départementale, régionale, métropolitaine, 
## par unité urbaineet et leurs différents libélles
load("donneesGrTbCartes.RData")
load("libelles.RData")

indicateurs = (libelles$lIndicateurs %>% filter(carte))$indicateur

## Contours géographiques communaux et départementaux 
contoursCommunes = st_read("M:/Perso/KMilin/References/contours géographiques/communes/2021/communes-20210101-shp/communes-20210101-simpl04.shp") %>%
  mutate(insee = ifelse(insee == "27676","27058",insee)) %>%
  select(insee) %>%
  filter(substr(insee,1,2)<"96")

contoursDep = st_read("M:/Perso/KMilin/References/contours géographiques/departements/2021/departements-2021-simpl04.shp") %>%
  filter(dep<"96")

contoursReg = st_read("M:/Perso/KMilin/References/contours géographiques/regions/2021/regions-simpl-2021.shp") 

frontieresDep = st_cast(contoursDep, "MULTILINESTRING")

# Fonctions---------------------------------------------------------------------
## Fonction qui permet de mettre en page des données décimales 
echelleAvecVirgule = function(x, ...) {
  format(x, decimal.mark = ",", trim = TRUE, scientific = FALSE, ...)
}

## Fonction qui prend en entrée une liste de label (issue de l'interface) et 
## renvoie un code ou label (selon 'k1'/'k2')
renvoiCodes = function(listeCodes,listeLabelsChoisis,dek1,versk2){
  codesChoisis = NULL
  listeCodes = listeCodes %>% as.data.frame()
  for(labelsChoisis in listeLabelsChoisis){
    codesChoisis = c(codesChoisis,listeCodes[which(listeCodes[,dek1]==labelsChoisis),versk2])
  }
  return(codesChoisis) 
}

## Fonction permettant de simplifier les caractères spéciaux d'une 
## chaîne de caractères 
modifieTexteRecherche = function(texte.a.modifier) {
  
  return(
    
    texte.a.modifier %>%
      
      str_replace(pattern = "à",replacement = "a") %>%
      
      str_replace(pattern = "ê",replacement = "e") %>%
      str_replace(pattern = "é",replacement = "e") %>%
      str_replace(pattern = "è",replacement = "e") %>%
      str_replace(pattern = "ë",replacement = "e") %>%
      str_replace(pattern = "ê",replacement = "e") %>%
      
      str_replace(pattern = "î",replacement = "i") %>%
      str_replace(pattern = "ï",replacement = "i") %>%
      
      str_replace(pattern = "ô",replacement = "o") %>%
      str_replace(pattern = "ö",replacement = "o") %>%
      
      str_replace(pattern = "ù",replacement = "u") %>%
      str_replace(pattern = "ü",replacement = "u") %>%
      
      str_replace(pattern = "'",replacement = " ") %>%
      
      tolower()
    
  )
  
}

## Doubler les noms des colonnes des DataTable
ajoutNomsColonnesDiff = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Année'),
      th(rowspan = 2, 'Nombre de faits constatés dans la commune'),
      th(colspan = 5, 'Nombre de faits constatés pour 1 000 habitants')
    ),
    tr(
      lapply(c("commune","département","même type d'aggloméraiton/ndans la région",
               "même type d'agglomération","France métropolitaine"), th)
    )
  )
))

ajoutNomsColonnesNdiff = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Année'),
      th(colspan = 2, 'Nombre de faits constatés dans la commune'),
      th(colspan = 6, 'Nombre de faits constatés pour 1 000 habitants')
    ),
    tr(
      lapply(c("commune","moyenne sur les communes non diffusées du département",
               "commune","moyenne sur les communes non diffusées du département",
               "département","même type d'aggloméraiton/ndans la région",
               "même type d'agglomération","France métropolitaine"), th)
    )
  )
))

# Paramètres--------------------------------------------------------------------
## Charte graphique de l'application
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

## Charte graphique des séries temporelles
themeGraph = hc_theme(
  chart = list(
    backgroundColor = "#f5f5f5",
    style = list(
      fontFamily = "Noto Sans JP Regular"
    )))

## Format d'export des graphiques
export = list(
  list(text = "PNG",
       onclick = JS("function () { 
                  this.exportChart({ type: 'image/png' }); }")),
  list(text = "JPEG",
       onclick = JS("function () { 
                  this.exportChart({ type: 'image/jpeg' }); }")),
  list(text = "PDF",
       onclick = JS("function () { 
                  this.exportChart({ type: 'application/pdf' }); }")),
  list(separator = TRUE),
  list(text = "CSV",
       onclick = JS("function () { this.downloadCSV(); }")),
  list(text = "XLS",
       onclick = JS("function () { this.downloadXLS(); }")))

## Manipulation des series temporelles
  deselectionIndicateurs = JS("function(event) {
  if (!this.visible )
  return true;
  
  var seriesIndex = this.index;
  var series = this.chart.series;
  
  for (var i = 0; i < series.length; i++)
  {
  if (series[i].index != seriesIndex)
  {
  series[i].visible ? series[i].hide() : series[i].show();
  } 
  }
  return false;
  }")



