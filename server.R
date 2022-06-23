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

shinyServer(function(input,output,session) { 
  
  listeCommuneRecherche = reactive({

    if(input$barreRechercheCommune != ""){
      libelles[["lCommunes"]] %>%
        mutate(score.recherche = stringdist::stringsim(libgeo %>% modifieTexteRecherche(), input$barreRechercheCommune %>% modifieTexteRecherche(), method = "jw")) %>%
        arrange(desc(score.recherche)) %>% slice(1:100)
    }else{
      libelles[["lCommunes"]] %>% slice(1:nrow(libelles[["lCommunes"]]) %>% sample(1))
    }

  })

  observe({
    updateSelectInput(session, "choixCommuneGrTb",choices = listeCommuneRecherche()$code.lib.comm)
  })

  ### Passage du code-libellé de la commune au code commune

  codeCommuneChoisie = reactive({
    renvoiCodes(libelles[["lCommunes"]] %>% as.data.frame(), input$choixCommuneGrTb, "code.lib.comm","code.commune")
  })

  ### Passage du libellé de l'indicateur de délinquance au code d'indicateur

  codeIndicateurChoisiGrTb = reactive({
    renvoiCodes(libelles[["lIndicateurs"]] %>% as.data.frame(),input$choixIndicateur,"lib.indicateur","indicateur")
  })

  codeIndicateurChoisiCarte = reactive({
    renvoiCodes(libelles[["lIndicateurs"]] %>% as.data.frame(),input$choixIndicateur,"lib.indicateur","indicateur")
  })

  ### Sélection onglet graphique/tableau

  selectionInit = reactive({

    donnees[["dCommunesGrTb"]][[codeIndicateurChoisiGrTb()]][[codeCommuneChoisie()]] %>%
    left_join(donnees[["dDepGrTb"]][[codeIndicateurChoisiGrTb()]] %>% select(-FC) %>% rename("département" = tx),by=c("annee"="annee","indicateur"="indicateur","dep"="dep")) %>%
    left_join(donnees[["dUu"]][[codeIndicateurChoisiGrTb()]] %>% select(-FC) %>% rename("même type d'agglomération" = tx),by=c("annee"="annee","indicateur"="indicateur","tuu"="tuu")) %>%
    left_join(donnees[["dRegUu"]][[codeIndicateurChoisiGrTb()]] %>% select(-FC) %>% rename("même type d'agglomération\ndans la région" = tx),by=c("annee"="annee","indicateur"="indicateur","reg"="reg","tuu.reg"="tuu.reg")) %>%
    left_join(donnees[["dMet"]][[codeIndicateurChoisiGrTb()]] %>% select(-FC) %>% rename("France métropolitaine" = tx),by=c("annee"="annee","indicateur"="indicateur"))

    })
  
  selectionInitIndicateurs = reactive({
    
    do.call("rbind", lapply(donnees[["dCommunesGrTb"]], "[[", codeCommuneChoisie())) %>% 
      arrange(annee) %>% 
      rename("année" = annee) %>% 
      mutate(année = paste0("20", année)) %>% 
      select(année, code.commune, indicateur, bool.pub, comp.tx, diff.tx, diff.fc) %>% 
      mutate(taux.infraction = ifelse(bool.pub == FALSE, comp.tx, diff.tx)) %>%
      mutate(indicateur = renvoiCodes(libelles$lIndicateurs, indicateur, "indicateur", "lib.indicateur")) %>% 
      arrange(indicateur) %>%
      rename("faits constatés" = diff.fc) %>% group_by(indicateur) %>%
      mutate(tx_evol_ann_pct = (`faits constatés`/ lag(`faits constatés`) - 1) * 100)  %>% 
      mutate(`Évolutions (en %)` = ifelse(is.na(tx_evol_ann_pct), 0, tx_evol_ann_pct)) %>% 
      ungroup() %>% 
      mutate_if(is.numeric, round, 2)
    
  })

  ind.diff = reactive({

    if(FALSE %in% selectionInit()$bool.pub){
      FALSE
    }else{
      TRUE
    }

  })
  
  ind.diff.indicateurs = reactive({
    
    if(FALSE %in% selectionInitIndicateurs()$bool.pub){
      FALSE
    }else{
      TRUE
    }
    
  })

  selection = reactive({

    if(!ind.diff()){
      selectionInit() %>% select("année" = annee,diff.fc,comp.fc,diff.tx,comp.tx,`département`,`même type d'agglomération\ndans la région`,`même type d'agglomération`,`France métropolitaine`)
    }else if(ind.diff()){
      selectionInit() %>% select("année" = annee,diff.fc,diff.tx,`département`,`même type d'agglomération\ndans la région`,`même type d'agglomération`,`France métropolitaine`)
    }

  })
  
  ### texte accompagnant les graphiques et les tableaux

  libelleCommuneChoisie = reactive({
    renvoiCodes(libelles[["lCommunes"]] %>% as.data.frame(),input$choixCommuneGrTb,"code.lib.comm","libgeo")
  })

  poplibelleCommuneChoisie = reactive({
    renvoiCodes(libelles[["lCommunes"]] %>% as.data.frame(),input$choixCommuneGrTb,"code.lib.comm","pop") %>% format(big.mark = " ")
  })

  libelleDepCommuneChoisie = reactive({
    renvoiCodes(libelles[["lDep"]] %>% as.data.frame(),selectionInit()$dep %>% unique(),"dep","lib.dep")
  })

  libelleRegCommuneChoisie = reactive({
    renvoiCodes(libelles[["lReg"]] %>% as.data.frame(),selectionInit()$reg %>% unique(),"reg","lib.reg")
  })

  libelleTuuTxtCommuneChoisie = reactive({
    renvoiCodes(libelles[["lUu"]] %>% as.data.frame(),selectionInit()$tuu.reg %>% unique(),"tuu","lib.tuu.txt")
  })

  libelleTuuCommuneChoisie = reactive({
    renvoiCodes(libelles[["lUu"]] %>% as.data.frame(),selectionInit()$tuu.reg %>% unique(),"tuu","lib.tuu")
  })

  texteCommune = reactive({

    paste0(libelleCommuneChoisie()," (",libelleDepCommuneChoisie(),", région ",libelleRegCommuneChoisie(),")"," est une commune qui recense ", poplibelleCommuneChoisie()," habitants en 2019,"," elle est située ",libelleTuuTxtCommuneChoisie(),".")

  })

  texteIndicateur = reactive({

    if(ind.diff()){
      paste0("")
    }else if(!ind.diff()){
      paste0("Pour cet indicateur, le nombre de faits constatés à ",libelleCommuneChoisie()," est inférieur au seuil d'interprétabilité et de respect du secret statistique. Dans ce cas, les données communales ne sont pas diffusées sur l'ensemble de la période et sont complétées par la moyenne des communes du département (",
             libelleDepCommuneChoisie(),") recensant des nombres de faits constatés non significatifs. Cette précaution est nécessaire pour garantir l'anonymat des personnes concernées et la pertinence de l'analyse territoriale de la délinquance.")
    }

  })
  
  texteIndicateur = reactive({
    
    if(ind.diff()){
      paste0("")
    }else if(!ind.diff()){ 
      paste0("Pour cet indicateur, le nombre de faits constatés à ",libelleCommuneChoisie()," est inférieur au seuil d'interprétabilité et de respect du secret statistique. Dans ce cas, les données communales ne sont pas diffusées sur l'ensemble de la période et sont complétées par la moyenne des communes du département (",
             libelleDepCommuneChoisie(),") recensant des nombres de faits constatés non significatifs. Cette précaution est nécessaire pour garantir l'anonymat des personnes concernées et la pertinence de l'analyse territoriale de la délinquance.")
    }
    
  })
  
  texteComparaisonIndicateur = reactive({
    
    if(ind.diff.indicateurs()){
      paste0("")
    }else if(!ind.diff.indicateurs()){
      paste0("Pour certain(s) indicateur(s), le nombre de faits constatés à ",libelleCommuneChoisie()," est inférieur au seuil d'interprétabilité et de respect du secret statistique pour qu'il soit diffusé sur l'ensemble de la période.")
    }
    
  })
  
  indicateursFiltres = reactive({
    if(!input$choixIndicateur %in% (libelles[["lIndicateurs"]] %>% filter(carte) %>% select(lib.indicateur) %>% pull()))
      {paste0("Les ", tolower(input$choixIndicateur), " ne sont pas ", ifelse(grepl("sexuelles", input$choixIndicateur), "cartographiées", "cartographiés"), " car les écarts entre les classes ne permettent pas de distinguer des disparités entre les territoires.")
    }else{
      NULL
    }
    
   })
  

  ### Adaptation de la sélection de données à une sortie de type graphe
  
  selectionGrapheAtteintesNiveau = reactive({
    
    selectionInitIndicateurs() %>% 
      rename(`Faits constatés` = `faits constatés`) %>% 
      hchart("line", hcaes(x = année, y = `Faits constatés`, group = indicateur)) %>%
      hc_add_theme(themeGraph) %>% 
      hc_plotOptions(series = list(events = list(legendItemClick = deselectionIndicateurs))) %>%
      hc_yAxis(labels = list(format = "{value:,.0f}")) %>% 
      hc_credits(
        enabled = TRUE, text = "Source : SSMSI",
        href = "https://www.data.gouv.fr/fr/datasets/bases-communale-et-departementale-des-principaux-indicateurs-des-crimes-et-delits-enregistres-par-la-police-et-la-gendarmerie-nationales/",
        style = list(fontSize = "12px")) %>% 
      hc_subtitle(
        text = str_c("<i>",texteCommune(),"<br/><br/>",texteComparaisonIndicateur(),"</i>"),
        style = list(fontWeight = "normal"),
        align = "left",verticalAlign = 'bottom') %>%
      hc_exporting(enabled = TRUE,sourceWidth=1300,sourceHeight=700,formAttributes = list(target = "_blank"),
                   buttons=list(
                     contextButton=list(
                       text= "Télécharger",
                       menuItems=export,
                       symbol='', y=10)))
      
    })
  
  selectionGrapheAtteintesEvol = reactive({
    
    selectionInitIndicateurs() %>% 
      filter(année != "2016") %>% 
      mutate(tx_evol_ann_pct = ifelse(is.na(tx_evol_ann_pct), 0, `faits constatés`/ lag(`faits constatés` ))) %>% 
      hchart("line", hcaes(x = année, y = `Évolutions (en %)`, group = indicateur)) %>%
      hc_add_theme(themeGraph) %>% 
      hc_plotOptions(series = list(events = list(legendItemClick = deselectionIndicateurs))) %>%
      hc_yAxis(labels = list(format = "{value:,.0f}")) %>% 
      hc_credits(
        enabled = TRUE, text = "Source : SSMSI",
        href = "https://www.data.gouv.fr/fr/datasets/bases-communale-et-departementale-des-principaux-indicateurs-des-crimes-et-delits-enregistres-par-la-police-et-la-gendarmerie-nationales/",
        style = list(fontSize = "12px")) %>% 
      hc_subtitle(
        text = str_c("<i>",texteCommune(),"<br/><br/>",texteComparaisonIndicateur(),"</i>"),
        style = list(fontWeight = "normal"),
        align = "left",verticalAlign = 'bottom') %>% 
      hc_exporting(enabled = TRUE,sourceWidth=1300,sourceHeight=700,formAttributes = list(target = "_blank"),
                   buttons=list(
                     contextButton=list(
                       text= "Télécharger",
                       menuItems=export,
                       symbol='', y=10)))
    
  })
  
  
  selectionGrapheTerritoires = reactive({
    
    if(ind.diff()){
      
      selection() %>%
        mutate_if(is.numeric, round, 2) %>%
        arrange(année) %>%
        mutate(année = paste0("20", année)) %>% 
        select(-c(diff.fc)) %>%
        rename("commune" = diff.tx)  %>%
        pivot_longer(cols = c("commune", "département","même type d'agglomération","même type d'agglomération\ndans la région", "France métropolitaine"), names_to = "territoire", values_to = "Nombre de faits constatés pour 1000 habitants") %>% 
        hchart("line", hcaes (x = année, y = `Nombre de faits constatés pour 1000 habitants`, group = territoire)) %>% 
        hc_add_theme(themeGraph) %>% 
        hc_plotOptions(series = list(events = list(legendItemClick = deselectionIndicateurs))) %>% 
        hc_yAxis(labels = list(format = "{value:,,2f}")) %>% 
        hc_title(
          text = paste0(libelleCommuneChoisie()," (",libelleDepCommuneChoisie(),")"),
          margin = 20,
          align = "center",
          style = list(useHTML = TRUE)) %>% 
        hc_credits(
          enabled = TRUE, text = "Source : SSMSI",
          href = "https://www.data.gouv.fr/fr/datasets/bases-communale-et-departementale-des-principaux-indicateurs-des-crimes-et-delits-enregistres-par-la-police-et-la-gendarmerie-nationales/",
          style = list(fontSize = "12px")
        ) %>% 
        hc_subtitle(
          text = str_c("<i>",texteCommune(),"<br/><br/>",texteIndicateur(),"</i>"),
          style = list(fontWeight = "normal"),
          align = "left",verticalAlign = 'bottom') %>% 
        hc_exporting(enabled = TRUE,sourceWidth=1300,sourceHeight=700,formAttributes = list(target = "_blank"),
                     buttons=list(
                       contextButton=list(
                         text= "Télécharger",
                         menuItems=export,
                         symbol='', y=10)))
      
    }else if(!ind.diff()){
      
      selection() %>%
        mutate_if(is.numeric, round, 2) %>%
        arrange(année) %>%
        mutate(année = paste0("20", année)) %>% 
        select(-c(diff.fc, comp.fc)) %>%
        rename("commune" = diff.tx)  %>%
        rename("moyenne sur les communes non diffusées du département" = comp.tx) %>%
        pivot_longer(cols = c("commune", "moyenne sur les communes non diffusées du département", "département", "même type d'agglomération", "même type d'agglomération\ndans la région", "France métropolitaine"), names_to = "territoire", values_to = "Nombre de faits constatés pour 1000 habitants") %>%
        hchart("line", hcaes(x = année, y = `Nombre de faits constatés pour 1000 habitants`, group = territoire)) %>% 
        hc_add_theme(themeGraph) %>% 
        hc_plotOptions(series = list(events = list(legendItemClick = deselectionIndicateurs))) %>% 
        hc_yAxis(labels = list(format = "{value:,,2f}")) %>% 
        hc_title(
          text = paste0(libelleCommuneChoisie()," (",libelleDepCommuneChoisie(),")"),
          margin = 20,
          align = "center",
          style = list(useHTML = TRUE)) %>% 
        hc_credits(
          enabled = TRUE, text = "Source : SSMSI",
          href = "https://www.data.gouv.fr/fr/datasets/bases-communale-et-departementale-des-principaux-indicateurs-des-crimes-et-delits-enregistres-par-la-police-et-la-gendarmerie-nationales/",
          style = list(fontSize = "12px")
        ) %>% 
        hc_subtitle(
          text = str_c("<i>",texteCommune(),"<br/><br/>",texteIndicateur(),"</i>"),
          style = list(fontWeight = "normal"),
          align = "left", verticalAlign = 'bottom') %>% 
        hc_exporting(enabled = TRUE,sourceWidth=1300,sourceHeight=700,formAttributes = list(target = "_blank"),
                     buttons=list(
                       contextButton=list(
                         text= "Télécharger",
                         menuItems=export,
                         symbol='', y=10
                     )))
      
    }
      
  })

  # ### Création de la carte

  choixAnneeCarte = reactive({input$choixAnneeCarte %>% substr(3,4)})
  
  selectionCarteDep = reactive({
    contoursDep  %>%
      left_join(donnees[["dDepCartes"]][[codeIndicateurChoisiCarte()]][[choixAnneeCarte()]],by = "dep")
  })
  
  selectionCarteComPub = reactive({
    contoursCommunes %>%   
      right_join(donnees[["dCommunesCartes"]][[codeIndicateurChoisiCarte()]][[paste0(choixAnneeCarte(),TRUE)]],by=c("insee"="code.commune"))
  })
  
  selectionCarteComNPub = reactive({
    contoursCommunes %>% 
      right_join(donnees[["dCommunesCartes"]][[codeIndicateurChoisiCarte()]][[paste0(choixAnneeCarte(),FALSE)]], by=c("insee"="code.commune"))
  })
  
  titreLegendeCarte = reactive({
    if(grepl("Cambriolages", input$choixIndicateur))
    {paste0("Nombre d'infractions</br> pour 1 000 logements en</br>", input$choixAnneeCarte)}
    else{
    paste0("Nombre d'infractions</br> pour 1 000 habitants en</br>", input$choixAnneeCarte)
    }
    
  })
  
  observeEvent(input$choixIndicateur, {
    if(input$choixIndicateur %in% (libelles[["lIndicateurs"]] %>% filter(carte))$lib.indicateur){
      show("carte")
      show("choixAnneeCarte")
    }else{
      hide("carte")
      hide("choixAnneeCarte")

    }

  })
  
  creerCarte = reactive({

    if(input$choixIndicateur %in% (libelles[["lIndicateurs"]] %>% filter(carte) %>% select(lib.indicateur) %>% pull())){
    shinyjs::hide("controls")
    
    pal = colorFactor(palette = "Purples", selectionCarteComPub()$label.tx.q, reverse = FALSE)

    labelCarteDep = paste("<strong> Département : </strong>", renvoiCodes(libelles[["lDep"]] %>% as.data.frame(), selectionCarteDep()$dep, "dep", "lib.dep"),
                          "</br> <strong> Nombre de faits constatés : </strong>", selectionCarteDep()$FC %>% format(big.mark = " "),
                          ifelse(grepl("Cambriolages", input$choixIndicateur),"</br> <strong> Taux par logement : </strong>", "</br> <strong> Taux par habitant : </strong>"), selectionCarteDep()$tx %>% round(1) %>% format(decimal.mark = ",")," ‰"

    ) %>%
      lapply(htmltools::HTML)

    leaflet() %>%
      addPolygons(data = selectionCarteDep(),
                  fillColor = ~pal(selectionCarteDep()$label.tx.q),
                  opacity = 1,
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(fillColor = "#F4983A", bringToFront = FALSE),
                  weight = 1,
                  color = "white",
                  label = labelCarteDep,
                  layerId = selectionCarteDep()$dep
                      
      ) %>%
      addSearchOSM(options = searchOptions(autoCollapse = FALSE, minLength = 2, zoom = 11, hideMarkerOnCollapse = TRUE)) %>%
      addLegend("topright", pal = pal, values = selectionCarteComPub()$label.tx.q,
                title = titreLegendeCarte(),opacity = 1) %>%
      addLegend("topright", colors = "#CECECE", labels = "Données non diffusées")
    
    }else{
      leaflet() %>% addPolygons(data = contoursDep)
    }

  })

    retourBoutonCarte = reactive({

    req(input$carte_shape_click)
    absolutePanel(id = "controls", top = 150, left = 22,
                  right = "auto", bottom = "auto", width = "auto", height = "auto",
                  actionButton(inputId = "reset", label = "Retour", class = "btn btn-primary"))
  })

  observeEvent(input$carte_shape_click, {
    

    click = input$carte_shape_click

    codeCommuneChoisieCarte = "00000"

    if(!is.null(click$id)){

      if(nchar(click$id) < 3){
        shinyjs::show("controls")

      communesPub = selectionCarteComPub() %>% filter(startsWith(insee, click$id))

      communesNPub = selectionCarteComNPub() %>% filter(startsWith(insee, click$id))

      frontieresDepClick = frontieresDep %>% filter(dep %in% click$id)

      labelCarteCommunePub = paste("<strong> Commune : </strong>", renvoiCodes(libelles[["lCommunes"]] %>% as.data.frame, communesPub$insee,"code.commune","libgeo"),
                                   "</br> <strong> Nombre de faits constatés : </strong>", communesPub$diff.fc %>% format(big.mark = " "),
                                   ifelse(grepl("Cambriolages", input$choixIndicateur),"</br> <strong> Taux par logement : </strong>", "</br> <strong> Taux par habitant : </strong>"), communesPub$diff.tx %>% round(1) %>% format(decimal.mark = ",")," ‰"
      ) %>%
        lapply(htmltools::HTML)

      labelCarteCommuneNPub = paste("<strong> Commune : </strong>", renvoiCodes(libelles[["lCommunes"]] %>% as.data.frame,communesNPub$insee,"code.commune","libgeo"),
                                    "</br> <strong> Statut : </strong> données non diffusées",
                                    "</br> <strong> Nombre moyen de faits constatés pour les communes non diffusées du département : </strong>", communesNPub$comp.fc %>% round(1) %>% format(decimal.mark = ","),
                                    ifelse(grepl("Cambriolages", input$choixIndicateur),"</br> <strong> Taux moyen par logement pour les communes non diffusées du département : </strong>", "</br> <strong> Taux moyen par habitant pour les communes non diffusées du département : </strong>"), communesNPub$comp.tx %>% round(1) %>% format(decimal.mark = ",")," ‰"
      ) %>%
        lapply(htmltools::HTML)

      pal = colorFactor(palette = "Purples", selectionCarteComPub()$label.tx.q, reverse = FALSE)

      leafletProxy("carte") %>%
        clearControls() %>%
        addPolylines(data = frontieresDepClick,
                     color = "black",
                     weight = 3) %>%
        addPolygons(data = communesPub,
                    fillColor = ~pal(communesPub$label.tx.q),
                    fillOpacity = 1,
                    opacity = 1,
                    weight = 0.5,
                    layerId = communesPub$insee,
                    color = "#CECECE",
                    highlightOptions = highlightOptions(fillColor = "#F4983A", bringToFront = FALSE),
                    label = labelCarteCommunePub) %>%
        addPolygons(data = communesNPub,
                    fillColor = "#CECECE",
                    fillOpacity = 1,
                    opacity = 1,
                    weight = 0.5,
                    color = "white",
                    layerId = communesNPub$insee,
                    highlightOptions = highlightOptions(fillColor = "#F4983A", bringToFront = FALSE),
                    label = labelCarteCommuneNPub) %>%
        setView(lng = click$lng, lat = click$lat, zoom = 9) %>%
        addLegend("topright", pal = pal, values = selectionCarteComPub()$label.tx.q,
                  title = titreLegendeCarte(),opacity = 1) %>%
        addLegend("topright", colors = "#CECECE", labels = "Données non diffusées")


      }else if(nchar(click$id) > 2 & codeCommuneChoisieCarte != click$id){
        

      updateTextInput(session,
                      "barreRechercheCommune",
                      value = as.character(libelles$lCommunes %>% filter(code.commune %in% click$id) %>% select(libgeo)))

      codeCommuneChoisieCarte = click$id

      communeChoisieCartePub = selectionCarteComPub() %>% filter(insee %in% click$id)
      communeChoisieCarteNPub = selectionCarteComNPub() %>% filter(insee %in% click$id)

      pal = colorFactor(palette = "Purples", selectionCarteComPub()$label.tx.q, reverse = FALSE)

      labelCarteCommunePub = paste("<strong> Commune : </strong>", renvoiCodes(libelles[["lCommunes"]] %>% as.data.frame, communeChoisieCartePub$insee,"code.commune","libgeo"),
                                   "</br> <strong> Nombre de faits constatés : </strong>", communeChoisieCartePub$diff.fc %>% format(big.mark = " "),
                                   ifelse(grepl("Cambriolages", input$choixIndicateur),"</br> <strong> Taux par logement : </strong>", "</br> <strong> Taux par habitant : </strong>"), communeChoisieCartePub$diff.tx %>% round(1) %>% format(decimal.mark = ",")," ‰"
      ) %>%
      lapply(htmltools::HTML)

      labelCarteCommuneNPub = paste("<strong> Commune : </strong>", renvoiCodes(libelles[["lCommunes"]] %>% as.data.frame,communeChoisieCarteNPub$insee,"code.commune","libgeo"),
                                    "</br> <strong> Statut : </strong> données non diffusées",
                                    "</br> <strong> Nombre moyen de faits constatés pour les communes non diffusées du département : </strong>", communeChoisieCarteNPub$comp.fc %>% round(1) %>% format(decimal.mark = ","),
                                    ifelse(grepl("Cambriolages", input$choixIndicateur),"</br> <strong> Taux moyen par logement pour les communes non diffusées du département : </strong>", "</br> <strong> Taux moyen par habitant pour les communes non diffusées du département : </strong>"), communeChoisieCarteNPub$comp.tx %>% round(1) %>% format(decimal.mark = ",")," ‰"
      ) %>%
      lapply(htmltools::HTML)

      leafletProxy("carte") %>%
        clearControls() %>%
        clearGroup("communeChoisie") %>%
        addPolygons(data = communeChoisieCartePub,
                     fillColor = "",
                     color = "#F4983A",
                     weight = 3,
                     opacity = 1,
                     fillOpacity = 0,
                     highlightOptions = highlightOptions(fillColor = "#F4983A", bringToFront = FALSE),
                     label = labelCarteCommunePub,
                     group = "communeChoisie") %>%
        addPolygons(data = communeChoisieCarteNPub,
                    fillColor = "",
                    color = "#F4983A",
                    weight = 3,
                    opacity = 1,
                    fillOpacity = 0,
                    highlightOptions = highlightOptions(fillColor = "#F4983A", bringToFront = FALSE),
                    label = labelCarteCommuneNPub,
                    group = "communeChoisie") %>%
        addLegend("topright", pal = pal, values = selectionCarteComPub()$label.tx.q,
                  title = titreLegendeCarte(),opacity = 1) %>%
        addLegend("topright", colors = "#CECECE", labels = "Données non diffusées")
      }
    }
    
  })

  observeEvent(input$reset, {

    shinyjs::hide("controls")

    labelCarteDep = paste("<strong> Département : </strong>", renvoiCodes(libelles[["lDep"]] %>% as.data.frame(), selectionCarteDep()$dep, "dep", "lib.dep"),
                          "</br> <strong> Nombre de faits constatés : </strong>", selectionCarteDep()$FC %>% format(big.mark = " "),
                          ifelse(grepl("Cambriolages", input$choixIndicateur),"</br> <strong> Taux par logement : </strong>", "</br> <strong> Taux par habitant : </strong>"), selectionCarteDep()$tx %>% round(1) %>% format(decimal.mark = ",")," ‰"

    ) %>%
      lapply(htmltools::HTML)

    pal = colorFactor(palette = "Purples", selectionCarteComPub()$label.tx.q, reverse = FALSE)

    leafletProxy("carte") %>%
      setView(3, 47, zoom = 6) %>%
      clearControls() %>%
      clearShapes() %>%
      addPolygons(data = selectionCarteDep(),
                  fillColor = ~pal(selectionCarteDep()$label.tx.q),
                  opacity = 1,
                  fillOpacity = 1,
                  highlightOptions = highlightOptions(fillColor = "#F4983A", bringToFront = FALSE),
                  color = "white",
                  weight = 1,
                  layerId = selectionCarteDep()$dep,
                  label = labelCarteDep) %>%
      addSearchOSM(options = searchOptions(autoCollapse = FALSE, minLength = 2, zoom = 11, hideMarkerOnCollapse = TRUE)) %>%
      addLegend("topright", pal = pal, values = selectionCarteComPub()$label.tx.q,
                title = titreLegendeCarte(),opacity = 1) %>%
      addLegend("topright", colors = "#CECECE", labels = "Données non diffusées")


  })
  
  output$comparaisonTerritoires = renderHighchart({
    selectionGrapheTerritoires()
  })
  
  output$comparaisonIndicateursNiveau = renderHighchart({
    selectionGrapheAtteintesNiveau()
  })
  
  output$comparaisonIndicateursEvol = renderHighchart({
    selectionGrapheAtteintesEvol()
  })

  output$indicateursFiltres = renderText({
    indicateursFiltres()
  })

output$controls = renderUI({
  retourBoutonCarte()
  })

output$carte = renderLeaflet({
  creerCarte()
  })

})

