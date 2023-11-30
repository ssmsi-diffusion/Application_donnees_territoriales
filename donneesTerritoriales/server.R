source("./environnement.R", encoding = "UTF-8")

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
  
  axeYComparaisonTerritoires = reactive({
      ifelse(input$choixIndicateur == "Cambriolages de logement",
             paste0("Nombre ", uni.com %>% filter(classe == input$choixIndicateur) %>% select(uni.com) %>%  pull()," pour 1000 logements"),
             paste0("Nombre ", uni.com %>% filter(classe == input$choixIndicateur) %>% select(uni.com) %>% pull()," pour 1000 habitants")
      )
  })
  

  ### Adaptation de la sélection de données à une sortie de type graphe
  
  selectionGrapheAtteintesNiveau = reactive({
    
    selectionInitIndicateurs() %>% 
      rename(`Nombre de crimes et délits` = `faits constatés`) %>% 
      hchart("line", hcaes(x = année, y = `Nombre de crimes et délits`, group = indicateur)) %>%
      hc_add_theme(themeGraph) %>% 
      hc_plotOptions(series = list(events = list(legendItemClick = deselectionIndicateurs))) %>%
      hc_yAxis(labels = list(format = "{value:,.0f}")) %>% 
      hc_title(
        text = paste0(libelleCommuneChoisie()," (",libelleDepCommuneChoisie(),")"),
        margin = 20,
        align = "center",
        style = list(useHTML = TRUE)) %>% 
      # hc_credits(
      #   enabled = TRUE, text = "Source : SSMSI, base communale des crimes et délits enregistrés par la police et la gendarmerie.",
      #   href = "https://www.data.gouv.fr/fr/datasets/bases-communale-et-departementale-des-principaux-indicateurs-des-crimes-et-delits-enregistres-par-la-police-et-la-gendarmerie-nationales/",
      #   style = list(fontSize = "11px")) %>%
      hc_subtitle(
        text = str_c("<i>",texteCommune(),"<br/><br/>",
                     texteComparaisonIndicateur(),"</i><br/><br/>",
                     "Source : SSMSI, base communale des crimes et délits enregistrés par la police et la gendarmerie."
                     # "<br/><br/><div style'='text_align: right;'><i><ul><a href = https://www.data.gouv.fr/fr/datasets/bases-communale-et-departementale-des-principaux-indicateurs-des-crimes-et-delits-enregistres-par-la-police-et-la-gendarmerie-nationales/>Source : SSMSI, base communale des crimes et délits ","</a></ul>","enregistrés par la police et la gendarmerie.","</i></div>"
                     ),
        style = list(useHTML = TRUE,fontWeight = "normal"),
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
      hc_title(
        text = paste0(libelleCommuneChoisie()," (",libelleDepCommuneChoisie(),")"),
        margin = 20,
        align = "center",
        style = list(useHTML = TRUE)) %>% 
      # hc_credits(
      #   enabled = TRUE, text = "Source : SSMSI, base communale des crimes et délits enregistrés par la police et la gendarmerie.",
      #   href = "https://www.data.gouv.fr/fr/datasets/bases-communale-et-departementale-des-principaux-indicateurs-des-crimes-et-delits-enregistres-par-la-police-et-la-gendarmerie-nationales/",
      #   style = list(fontSize = "11px")) %>% 
      hc_subtitle(
        text = str_c("<i>",texteCommune(),"<br/><br/>",texteComparaisonIndicateur(),"</i><br/><br/>",
                     "Source : SSMSI, base communale des crimes et délits enregistrés par la police et la gendarmerie."),
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
        hc_yAxis(title = list(text = axeYComparaisonTerritoires()),labels = list(format = "{value:,,2f}")) %>% 
        hc_title(
          text = paste0(libelleCommuneChoisie()," (",libelleDepCommuneChoisie(),")"),
          margin = 20,
          align = "center",
          style = list(useHTML = TRUE)) %>% 
        # hc_credits(
        #   enabled = TRUE, text = "Source : SSMSI, base communale des crimes et délits enregistrés par la police et la gendarmerie ; Insee, recensement de la population.",
        #   href = "https://www.data.gouv.fr/fr/datasets/bases-communale-et-departementale-des-principaux-indicateurs-des-crimes-et-delits-enregistres-par-la-police-et-la-gendarmerie-nationales/",
        #   style = list(fontSize = "11px")
        # ) %>% 
        hc_subtitle(
          text = str_c("<i>",texteCommune(),"<br/><br/>",texteIndicateur(),"</i><br/><br/>",
                       "Source : SSMSI, base communale des crimes et délits enregistrés par la police et la gendarmerie ; Insee, recensement de la population."),
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
        hc_yAxis(title = list(text = axeYComparaisonTerritoires()), labels = list(format = "{value:,,2f}")) %>% 
        hc_title(
          text = paste0(libelleCommuneChoisie()," (",libelleDepCommuneChoisie(),")"),
          margin = 20,
          align = "center",
          style = list(useHTML = TRUE)) %>% 
        # hc_credits(
        #   enabled = TRUE, text = "Source : SSMSI, base communale des crimes et délits enregistrés par la police et la gendarmerie ; Insee, recensement de la population.",
        #   href = "https://www.data.gouv.fr/fr/datasets/bases-communale-et-departementale-des-principaux-indicateurs-des-crimes-et-delits-enregistres-par-la-police-et-la-gendarmerie-nationales/",
        #   style = list(fontSize = "11px")
        # ) %>% 
        hc_subtitle(
          text = str_c("<i>",texteCommune(),"<br/><br/>",texteIndicateur(),"</i><br/><br/>",
                       "Source : SSMSI, base communale des crimes et délits enregistrés par la police et la gendarmerie ; Insee, recensement de la population."),
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
  
  # titreLegendeCarte = reactive({
  #   if(grepl("Cambriolages", input$choixIndicateur))
  #   {paste0("Nombre ", ,"</br> pour 1 000 logements en</br>", input$choixAnneeCarte)}
  #   else{
  #   paste0("Nombre ", ,"</br> pour 1 000 habitants en</br>", input$choixAnneeCarte)
  #   }
  #   
  # })
  
  titreLegendeCarte = reactive({
    
    ifelse(input$choixIndicateur == "Cambriolages de logement",
           paste0("Nombre ", uni.com %>% filter(classe == input$choixIndicateur) %>% select(uni.com) %>% 
                    pull(),"</br>pour 1 000 logements"),
           paste0("Nombre ", uni.com %>% filter(classe == input$choixIndicateur) %>% select(uni.com) %>% 
                    pull(),"</br>pour 1 000 habitants")
    )
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
      addLegend("topright", colors = "#CECECE", labels = "Données non diffusées </br> (effectifs trop faibles)")
    
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
                                    "</br> <strong> Statut : </strong> données non diffusées (effectifs trop faibles)",
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
        addLegend("topright", colors = "#CECECE", labels = "Données non diffusées </br> (effectifs trop faibles)")


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
                                    "</br> <strong> Statut : </strong> données non diffusées (effectifs trop faibles)",
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
        addLegend("topright", colors = "#CECECE", labels = "Données non diffusées </br> (effectifs trop faibles)")
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
      addLegend("topright", colors = "#CECECE", labels = "Données non diffusées </br> (effectifs trop faibles)")


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

