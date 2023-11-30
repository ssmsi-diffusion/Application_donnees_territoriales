# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Packages, paramètres généraux et fonctions

source("C:/Users/Administrateur/Documents/Datavisualisation/applicationCartesChaleur/cambriolagesLogementsEchelleInfracommunale/environnement.R", encoding = "UTF-8")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

shinyUI(fluidPage(

  theme = shinytheme("united"),
  
  tags$li(a(href = 'https://www.interieur.gouv.fr/Interstats/Actualites',
            img(src = "logossmsi.png", title = "Vers le site du SSMSI", height = "70px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          tags$style(".main-header .logo {height: 70px ; line-height: 60px;}"),
          tags$style(".sidebar-toggle {height: 70px; padding-top: 20px !important;}"),
          class = "dropdown"
  ),

  tags$head(
    tags$style(HTML("
      .leaflet-left .leaflet-control{
        visibility: hidden;
      }
    "))
  ),

  navbarPage(title = "Cambriolages de logements à l'échelle infracommunale",
             header = tagList(
               use_theme(monTheme)
             ),

             tabPanel("Cartes de chaleur", value="tabCartes",
                      
                      strong("Cambriolages et tentatives de cambriolages de logements enregistrés, en lieu de commission pour 1000 logements"), 
                      br(), br(),
                      
                      fluidRow(
                        column(3,
                               selectInput(inputId = "choixUU", 
                                           label = strong("Choix d'une unité urbaine :",
                                                          bsButton(inputId="popChoixUu", label = "", icon = icon("question"), style = "default", size = "extra-small",type="toggle")
                                           ),
                                           choices = uu$libuu2020 %>% sort(decreasing=FALSE), multiple = FALSE, selectize = TRUE, selected = uu %>% slice(sample(size = 1,1:nrow(uu))) %>% select(libuu2020) %>% pull()
                                           )
                               ),
                        column(2,
                               selectInput(inputId = "choixAnnee", 
                                           label = strong("Choix d'une année :"),
                                           choices = 2016:2022 %>% as.character(), multiple = FALSE, selectize = FALSE, selected = "2022"
                                          )
                              ),
                        column(5,
                               selectInput(inputId = "choixClassif",
                                            label = strong("Choix de la classification :",
                                                           bsButton(inputId="popChoixClassif", label = "", icon = icon("question"), style = "default", size = "extra-small",type="toggle")
                                          ),
                                           choices = c("Spécifique à l'unité urbaine et l'année sélectionnées" = "cl.uu"
                                                       # , "Référence de l'unité urbaine sélectionnée (moyenne 2016-2022)" = "cl.ref.uu","Référence nationale pour l'année sélectionnée" = "cl.ref.nat"
                                                       ),
                                           multiple = FALSE, selectize = FALSE, selected = "cl.uu", width = "100%")
                                          )
                        ),
                      
                      bsPopover(id = "popChoixUu", title = NULL,
                                content = "Unité urbaine de 2020 recensant plus de 100000 habitants en 2017 (cf. onglet &#171 Documentation &#187).",
                                placement = "right",
                                trigger = "hover",
                                options = list(container = "body")
                      ),
                      
                      bsPopover(id = "popChoixClassif", title = NULL,
                                content = "La classification de référence pour l&#39unité urbaine sélectionnée se base sur les valeurs enregistrées sur ses carreaux entre 2016 et 2022. La référence nationale est quant à elle construite en considérant – pour l&#39année sélectionnée – l&#39ensemble des carreaux diffusés pour les 59 unités urbaines disponibles. Enfin, la classification spécifique à l&#39unité urbaine et l&#39année sélectionnées se restreint aux cambriolages de logements commis dans l&#39urbaine urbaine, et enregistrés sur l&#39année retenue.",
                                placement = "right",
                                trigger = "hover",
                                options = list(container = "body")
                      ),
                      
                      mainPanel(
                        textOutput("texteRayonLissage"),
                        downloadButton(outputId = "telechargeCarte", label = "Enregistrer la carte"),
                        leafletOutput("carte",width = 1500, height = 1000) %>% withSpinner(type = 3,color = rgb(249,196,143,maxColorValue = 255), color.background = rgb(44,79,158,maxColorValue = 255))
                      )

             ),

             tabPanel("Documentation", value="tabDoc",
                      
                      
                      h3(strong("Définitions")),br(),
                      
                      HTML("<ul> 
                        <li> <strong> Le cambriolage de logement </strong>  est un vol dans un local d’habitation, aggravé quand il est commis par effraction, ruse ou escalade. L’usage de fausses clefs pour entrer dans les lieux est assimilé à une effraction. <strong> La tentative de cambriolage </strong> (acte manqué, interrompu, etc.) est considérée par la justice comme une infraction caractérisée, elle sera donc jugée au même titre qu’un cambriolage « abouti ». </li>
                        <li> Le zonage en <a href = 'https://www.insee.fr/fr/information/4802589'> <strong> unité urbaine </strong>  de 2020 </a> est construit par l’Insee et repose sur la continuité du bâti et sur le nombre d’habitants. Les unités urbaines sont constituées « d’une commune ou d’un ensemble de communes présentant une zone de bâti continu (pas de coupure de plus de 200 mètres entre deux constructions) qui compte au moins 2 000 habitants ». </li>
                        </ul> <br>"
                      ),
                      
                      h3(strong("Champ et sources")),br(),
                      
                      HTML(
                        "Dans cette application, les taux de cambriolages de logements lissés à l’échelle infracommunale sont disponibles sur les 59 unités urbaines recensant plus de 100000 habitants en 2017 (selon le recensement de la population de l’Insee), hors Guadeloupe, Guyane et Mayotte. Ces taux sont construits en suivant une méthodologie décrite dans 
                        <a href = https://www.interieur.gouv.fr/content/download/123316/988561/file/IM15_Les%20cartes%20de%20chaleur%20appliqu%C3%A9es%20aux%20taux%20de%20cambriolages.pdf '>  [Pramil, 2019] </a>. 
                        En particulier, ils sont issus d’un lissage – ou moyenne – spatial des nombres d’infractions et de logements, dans un rayon qui varie pour chaque unité urbaine et dépend de leur surface.
                        
                        <br><br>
                        
                        Les nombres de cambriolages et tentatives de cambriolage de logements (principaux ou secondaires) en lieu de commission valorisés dans cette application sont issus de la base statistique géolocalisée des infractions enregistrés par la police et la gendarmerie nationale entre 2016 et 2022 (SSMSI). Seuls les cambriolages portés à la connaissance des forces de sécurité intérieure sont inclus dans ces données. Selon l’enquête de victimation  <i> Cadre de vie et sécurité </i>, en moyenne en 2017, 2018 et 2020, 69 % des victimes de cambriolages (de résidences principales) déposent plainte auprès des forces de l’ordre, contre 33 % pour les tentatives de cambriolage. 
                        
                        <br><br>
                        
                        Les nombres de logements sont issus du <a href = 'https://www.insee.fr/fr/statistiques/6215217'> <i>Fichier Localisé social et fiscal </i> </a>2017 (Filosofi) de l’Insee. 
                        
                        <br><br>
                        
                        Ainsi, les taux annuels de cambriolages de logements infracommunaux sont obtenus à partir du nombre de cambriolages lissés – pour une année donnée –, et du nombre de logements lissés en 2017 (dernier millésime disponible). Les données carroyées de Filosofi n’étant pas disponibles pour la Guadeloupe, la Guyane et Mayotte, les données sont présentées pour les plus grandes unités urbaines de France métropolitaine, de Martinique et de La Réunion.
                        
                        <br><br>"
                      ),
                      
                      h3(strong("Références")),br(),
                      
                      h4(strong("Études contextualisant les cambriolages de logements à l’échelle communale")),br(),
                      
                      HTML("<ul>
                        <li> <a href='https://www.interieur.gouv.fr/Interstats/Actualites/davantage-de-cambriolages-de-logements-enregistres-dans-les-communes-urbaines-aisees-et-voisines-de-fortes-inegalites-sociales-Interstats-Analyse-n-60'> Milin K., « Davantage de cambriolages de logements enregistrés dans les communes urbaines, aisées et voisines de fortes inégalités sociales », <i> Interstats Analyse </i> n°60, SSMSI, mai 2023. </a> </li>
                        <li> <a href='https://www.insee.fr/fr/statistiques/7617747'> Milin K., Silhol J., « Moins de cambriolages de logements depuis la crise sanitaire sur une majeure partie du territoire », <i> Insee Focus </i> n°299, Insee/SSMSI, mai 2023 </a> </li>
                      </ul><br>"),
                      
                      h4(strong("Études à l’échelle infracommunale")),br(),
                      
                      HTML("<ul>
                        <li> <a href='https://www.interieur.gouv.fr/Interstats/Analyses-territoriales/Des-risques-de-cambriolages-de-logements-eleves-dans-les-centres-villes-des-agglomerations-de-Paris-Lyon-Marseille-mais-plus-faibles-dans-leurs-quartiers-de-grands-ensembles-Interstats-Analyse-N-27'> Pramil J., « Des risques de cambriolages de logements élevés dans les centres-villes des agglomérations de Paris-Lyon-Marseille, mais plus faibles dans leurs quartiers de 'grands-ensembles' », <i> Interstats Analyse </i> n°027, SSMSI, juin 2020. </a> </li>
                        <li> <a href='https://www.interieur.gouv.fr/Interstats/Actualites/Info-rapide-n-10-Les-cambriolages-de-logements-a-Paris-Lyon-et-Marseille-sont-concentres-dans-les-coeurs-d-agglomerations'> Pramil J., « Les cambriolages de logements à Paris, Lyon et Marseille sont concentrés dans les coeurs d'agglomérations », <i> Interstats Info Rapide </i> n°010, SSMSI, mai 2019. </a> </li>
                      </ul><br>"),
                      
                      h4(strong("Guides méthodologiques ")),br(),
                      
                      HTML("<ul>
                        <li> <a href='https://www.interieur.gouv.fr/content/download/123316/988561/file/IM15_Les%20cartes%20de%20chaleur%20appliqu%C3%A9es%20aux%20taux%20de%20cambriolages.pdf'> Pramil J., « Les cartes de chaleur appliquées aux taux de cambriolages », <i> Interstats Méthode </i> n°015, SSMSI, septembre 2019. </a> </li>
                        <li> <a href='https://www.interieur.gouv.fr/content/download/123320/988581/file/IM11.pdf'> Pramil J., « Le lissage spatial de la délinquance enregistrée et la représentation sous forme de cartes de chaleur », <i> Interstats Méthode </i> n°011, SSMSI, mai 2019. </a> </li>
                      </ul> <br>"
                           )
                      
             )
            
  ),
  includeHTML("www/css/SSMSIFooter.html"),
  includeCSS("www/css/footer.css")
  )
  
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
