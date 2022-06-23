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
library(highcharter)
library(shinyjs)

# Données-----------------------------------------------------------------------
## Données de délinquance communale, départementale, régionale, métropolitaine, 
## par unité urbaineet et leurs différents libélles
load("donneesGrTbCartes.RData")
load("libelles.RData")

indicateurs = (libelles$lIndicateurs %>% filter(carte))$indicateur

## Contours géographiques communaux et départementaux 
contoursCommunes = st_read("contoursGeographiques/communes-20210101-shp/communes-20210101-simpl04.shp") %>%
  mutate(insee = ifelse(insee == "27676","27058",insee)) %>%
  select(insee) %>%
  filter(substr(insee,1,2)<"96")

contoursDep = st_read("contoursGeographiques/departements-20210101-shp/departements-20210101-simpl04.shp") %>%
  filter(dep<"96")

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

# --------------------------------------------------------------------------------------------------------------------------------------------------

shinyUI(fluidPage(
  
  useShinyjs(),
  
  theme = shinytheme("united"),
  
  tags$head(includeScript("google-analytics.js")),
  tags$head(includeHTML("google-analytics.html")),
  
  tags$li(a(href = 'https://www.interieur.gouv.fr/Interstats/Actualites',
            img(src = "logossmsi.png", title = "Vers le site du SSM-SI", height = "50px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          tags$style(".main-header .logo {height: 70px ; line-height: 60px;}"),
          tags$style(".sidebar-toggle {height: 70px; padding-top: 20px !important;}"),
          class = "dropdown"
  ),
  
  navbarPage(title = "Délinquance enregistrée, en lieu de commission",
             header = tagList(
               use_theme(monTheme)
             ),
             # position = "fixed-top",
 
             tabPanel("Carte et graphiques",
                      
                      sidebarPanel(
                        # style = "position:fixed;width:inherit;width:30%;margin-top: 25px",
                        style = "margin-top: 25px",
                        
                        textInput("barreRechercheCommune", label = h5("Rechercher une commune :  ",bsButton(inputId="popCommuneRechercheGrTb", label = "", icon = icon("question"), style = "default", size = "extra-small", type="toggle")), placeholder = "Veuillez entrer le nom d'une commune"),
                        
                        selectInput("choixCommuneGrTb",
                                    label = h5("Résultats de la recherche :", bsButton(inputId="popCommuneChoixGrTb", label = "", icon = icon("question"), style = "default", size = "extra-small", type="toggle")),
                                    choices = libelles[["lCommunes"]]$code.lib.comm %>% base::sample(1),
                                    multiple = FALSE
                        ),
                        
                        selectInput("choixIndicateur", label = h5("Indicateur"), libelles[["lIndicateurs"]][["lib.indicateur"]] %>% sort(decreasing=FALSE), multiple = FALSE, selected = (libelles[["lIndicateurs"]] %>% filter(carte))$lib.indicateur),
                        
                        selectInput("choixAnneeCarte", label = h5("Année"), paste0("20", donnees[["dMet"]][["2"]][["annee"]]), multiple = FALSE, selected = "2021"),
                        
                        bsPopover(id = "popCommuneRechercheGrTb", title = NULL,
                                  content = "Veuillez entrer le nom de la commune pour laquelle vous souhaitez visualiser les données. Les informations apparaissent dans les graphiques situés sous la carte. </br></br> Si aucune recherche n&#39est effectuée, une commune est sélectionnée aléatoirement. </br></br> La recherche par numéro de département n&#39est pas reconnue.",
                                  placement = "right",
                                  trigger = "hover",
                                  options = list(container = "body") 
                        ),
                        
                        bsPopover(id = "popCommuneChoixGrTb", title = NULL,
                                  content = "Veuillez confirmer votre choix de commune parmi les 100 meilleurs résultats de la recherche. Ces derniers sont composés du numéro de département et du libellé de la commune",
                                  placement = "right",
                                  trigger = "hover",
                                  options = list(container = "body")
                        ),
                        
                        textOutput("indicateursFiltres")
                        
                      ),
                      
                      mainPanel(

                        leafletOutput(outputId = "carte"),
                        
                        conditionalPanel("isNaN(input.carte_shape_click)", uiOutput("controls")),

                        tags$style(type = "text/css", "#carte {height: calc(95vh - 80px) !important; margin-top: 25px}"),
                        tags$style(type = "text/css", "#comparaisonTerritoires {height: calc(100vh - 80px) !important;}"),
                        tags$style(type = "text/css", "#comparaisonIndicateursNiveau {height: calc(100vh - 80px) !important;}"),
                        tags$style(type = "text/css", "#comparaisonIndicateursEvol {height: calc(100vh - 80px) !important;}"),
                        
                        br(),
                        br(),
                        
                        highchartOutput("comparaisonTerritoires") %>%
                          withSpinner(type = 3,color = rgb(244,152,58, maxColorValue = 255), color.background = rgb(44,79,158, maxColorValue = 255)),
                        
                        br(),
                        br(),
                        
                        tabsetPanel(type = "tabs",
                                    tabPanel("En niveau", highchartOutput("comparaisonIndicateursNiveau") %>% 
                                               withSpinner(type = 3, color = rgb(244,152,58, maxColorValue = 255), color.background = rgb(44,79,158, maxColorValue = 255))),
                                    tabPanel("En évolution", highchartOutput("comparaisonIndicateursEvol") %>% 
                                               withSpinner(type = 3, color = rgb(244,152,58, maxColorValue = 255), color.background = rgb(44,79,158, maxColorValue = 255)))
                        ),
                        
                        br(),
                        br()
                        
                      )
                      
             ),
             
             tabPanel("Documentation",  value="tabDoc", h3("Description"), 
                      p(
                        HTML("Dans le cadre de leur activité judiciaire, les services de police et de gendarmerie rédigent des procédures relatives à des infractions avant de les transmettre à l’autorité judiciaire qui est susceptible de les requalifier, pendant ou après l’enquête. Ces infractions ont pu être constatées suite à une plainte, à un signalement, à un témoignage, à un flagrant délit, à une dénonciation ou encore sur l’initiative des forces de sécurité. Les informations recueillies <i> via </i> une main courante n’y sont pas intégrées. Enfin, les infractions relevées par d’autres services (douanes, offices environnementaux, etc.) n’y figurent pas non plus."),
                        br(), br(), "Depuis 1972, les services de sécurité (police et gendarmerie) se sont dotées d’un outil standardisé de mesure de l’activité judiciaire des services basé sur des comptages mensuels, appelé « État 4001 ». Ce document administratif porte sur les crimes et les délits (à l’exclusion des contraventions et des délits routiers), enregistrés pour la première fois par les forces de sécurité et portés à la connaissance de l’institution judiciaire. Les infractions y sont classées en 107 catégories (appelés « index »), très hétérogènes par la nature et la gravité des faits, mais aussi par le nombre d’infractions constatées chaque mois. Selon l’index, l’unité de compte retenue peut varier : elle peut être la victime par exemple en matière de violence sexuelle, l’auteur par exemple en matière d’usage ou revente de stupéfiants ou encore le véhicule en matière de vol de véhicule, etc. Cela implique qu’il n’est pas pertinent de constituer des agrégats regroupant des index n’ayant pas la même unité de compte.",
                        br(), br(), "Depuis sa création fin 2014, le Service statistique ministériel de la sécurité intérieure (",a("SSMSI", href='https://www.interieur.gouv.fr/Interstats/Qui-sommes-nous'),") expertise et fiabilise ces différents index et diffuse des indicateurs choisis et construits à partir de regroupements de ces index. Par ailleurs, depuis 2016, l’exploitation de données plus détaillées que les index de l’Etat 4001 ont permis au SSMSI de construire des indicateurs supplémentaires, par exemple sur le champ des violences physiques intrafamiliales, etc… "
                      ),
                      br(), strong("Afin de favoriser l’ouverture des données sur la délinquance et l’insécurité, le SSMSI met à disposition deux bases de données annuelles sur les principaux indicateurs des crimes et délits enregistrés par la police et la gendarmerie nationales, depuis 2016 : l’une à l’échelle communale et l’autre à l’échelle départementale, toutes deux selon le lieu de commission. Ce sont les données de ces deux bases qui sont datavisualisées dans cet outil."), "Ces bases de données ont vocation à être enrichies au fur et à mesure que les données pour d’autres indicateurs seront fiabilisées (escroqueries, destructions/dégradations volontaires, infractions à la législation sur les stupéfiants…).",
                      br(), br(), strong("Les données diffusées sont limitées aux communes pour lesquelles plus de 5 faits ont été enregistrés pendant 3 années successives."), "Cette précaution est motivée : d’une part par la fragilité des estimations sur des communes qui enregistrent peu de faits de délinquance", a("(Interstats Analyse n°44, mars 2022)",href = "https://www.interieur.gouv.fr/Interstats/Publications/Interstats-Analyse/Geographie-de-la-delinquance-a-l-echelle-communale-Interstats-Analyse-N-44"), "d’autre part par le secret statistique qui ne doit pas permettre, par le croisement de multiples sources, de déduire des informations individuelles sur les personnes concernées dans ces procédures. Cette précaution doit notamment être appréciée au regard de la sensibilité de certaines atteintes comme les violences sexuelles. La base de données diffusée fournit également l'information sur l'absence de faits enregistrés lorsqu’elle se reproduit sur 3 années successives.",
                      # br(), br(), "Outre les indicateurs des crimes et délits, la base communale comporte des données informatives relatives à la commune où se sont produits les faits, telles que les codes officiels géographiques de la commune, du département et de la région d’appartenance, leurs libellés respectifs, les populations résidentes correspondantes… ",
                      br(), br(), "Les données sont librement accessibles depuis le site  ", a("data.gouv.fr", href="https://www.data.gouv.fr/fr/datasets/bases-communale-et-departementale-des-principaux-indicateurs-des-crimes-et-delits-enregistres-par-la-police-et-la-gendarmerie-nationales/"),
                      br(), br(), h3("Précision sur les types d'agglomérations utilisées dans les graphiques"),
                      "Afin de situer le niveau d’une de délinquance sur une commune, le nombre de crimes et délits par habitant est comparé au même taux calculé à l’échelon de la France métropolitaine et à l’échelon du département où se situe la commune. Le niveau de délinquance étant particulièrement corrélé avec le degré d’urbanisation et la taille des communes, les taux par habitant de la commune sont également comparés aux taux moyens des communes qui appartiennent à la même tranche de taille d’unité urbaine (appelé « type d’agglomération » dans l’application), que ce soit au niveau de la France métropolitaine ou de la région d’appartenance de la commune d’intérêt. Plus précisément, la notion d’unité urbaine est définie par l’Insee comme étant une commune ou un ensemble de communes présentant une zone de bâti continu (pas de coupure de plus de 200 mètres entre deux constructions) et comptant au moins 2 000 habitants. Les unités urbaines se basent donc sur le maillage communal, et peuvent être regroupées selon leur taille :",
                      br(), br(), " - ", "Les communes situées en dehors d’une unité urbaine, auparavant appelées « communes rurales » ;",
                      br(), " - ", "De 2 000 à 4 999 habitants ;",
                      br(), " - ", "De 5 000 à 9 999 habitants ;",
                      br(), " - ", "De 10 000 à 19 999 habitants ;",
                      br(), " - ", "De 20 000 à 49 999 habitants ;",
                      br(), " - ", "De 20 000 à 49 999 habitants ;",
                      br(), " - ", "De 50 000 à 99 999 habitants ;",
                      br(), " - ", "De 100 000 à 199 999 habitants ;",
                      br(), " - ", "De 200 000 à 1 999 999 habitants (hors Paris) ;",
                      br(), " - ", "L’unité urbaine de Paris.",
                      br(),
                      br(), "Pour cette application, les unités urbaines corses recensant entre 10 000 et 49 999 forment une seule et même tranche de taille, afin de garantir un nombre suffisant de communes dans chaque tranche de taille d’unité urbaine de la région.",
                      br(), br(), h3("Publication"),
                      HTML("Retrouvez l’analyse de la géographie de la délinquance à l’échelle communale sur le site Interstats du SSMSI <i> via </i> le lien suivant : "),
                      br(), a("https://www.interieur.gouv.fr/Interstats/Publications/Interstats-Analyse/Geographie-de-la-delinquance-a-l-echelle-communale-Interstats-Analyse-N-44", href = 'https://www.interieur.gouv.fr/Interstats/Publications/Interstats-Analyse/Geographie-de-la-delinquance-a-l-echelle-communale-Interstats-Analyse-N-44')),
             br()),
  
              includeHTML("www/css/SSMSIFooter.html"),
              includeCSS("www/css/footer.css")
  )

)

# --------------------------------------------------------------------------------------------------------------------------------------------------
