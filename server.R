# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Packages, paramètres généraux et fonctions

source("C:/Users/Administrateur/Documents/Datavisualisation/applicationCartesChaleur/cambriolagesLogementsEchelleInfracommunale/environnement.R", encoding = "UTF-8")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

shinyServer(function(input,output,session){ 
  
# Reactive ---- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Code l'unité urbaine sélectionnée
  
  labelUuChoisie = reactive({
    input$choixUU
  })
  
  # Code l'unité urbaine sélectionnée

  codeUuChoisie = reactive({
      uu %>% filter(libuu2020 == input$choixUU) %>% select(uu2020) %>% pull()
    })

  # Année sélectionnée
  
  anneeChoisie = reactive({
    input$choixAnnee
  })
  
  # Classification sélectionnée 
  
  classifChoisie = reactive({ 
    input$choixClassif
  })
  
  # Rayon de lissage

  rayonLissage = reactive({
    paste0("Rayon de lissage : ",cartes[[anneeChoisie()]][[codeUuChoisie()]][["rayon"]]," mètres")
    })

  # Retour des départements/communes

  communesUUChoisieShp = reactive({
    com.shp %>% filter(insee %in% cartes[[anneeChoisie()]][[codeUuChoisie()]][["communes"]])
    })

  # Carte pour l'affichage
  
  carteChoisie = reactive({
    if(classifChoisie() == "cl.uu"){
      cartes[[anneeChoisie()]][[codeUuChoisie()]][["carte"]]
    }else if(classifChoisie() == "cl.ref.uu"){
      cartes[[anneeChoisie()]][[codeUuChoisie()]][["carte.ref.uu"]] %>% rename("lbl.tx.q" = lbl.tx.q.ref.uu)
    }else if(classifChoisie() == "cl.ref.nat"){
      cartes[[anneeChoisie()]][[codeUuChoisie()]][["carte.ref.nat"]] %>% rename("lbl.tx.q" = lbl.tx.q.ref.nat)
    }
   })
  
  carteAffiche = reactive({
    
    pal = colorFactor(palette = "RdYlBu", carteChoisie()$lbl.tx.q,reverse = TRUE)
    
    leaflet(data = carteChoisie()) %>%
      onRender( # ne pas oublier de cacher le bouton de zoom sur le côté gauche de la carte depuis le UI
        "function(el, x) {
          L.control.zoom({
            position:'bottomright'
          }).addTo(this);
        }") %>%
      addTiles(group = "Plan",options = providerTileOptions(opacity = 1)) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Plan") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Vue satellite") %>%
      # addProviderTiles(providers$Stamen.Toner, group = "Vue noir et blanc") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Gris") %>%
      addLayersControl(position = "bottomright",
                       # baseGroups = c("Plan", "Vue noir et blanc", "Vue satellite", "Gris"),
                       baseGroups = c("Plan", "Vue satellite", "Gris"),
                       options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addPolygons(color = ~pal(carteChoisie()$lbl.tx.q), fillOpacity=0.5, stroke = FALSE) %>%
      addPolygons(data = communesUUChoisieShp(),fillColor = NULL, fillOpacity = 0, color = "black",weight = 1, label = communesUUChoisieShp()$nom) %>%
      addLegend("topright", pal = pal, values = ~carteChoisie()$lbl.tx.q,
                title = paste0("Taux pour 1 000 logements"),
                opacity = 0.3
      )
    
  })
  
  # Carte pour le téléchargement
  
  groupeFondSelectionne = reactive({
    (req(input$carte_groups))
  })
  
  fondSelectionne = reactive({
    fondsCartes %>% filter(groupes == groupeFondSelectionne()) %>% select(fonds) %>% pull()
  })
  
  carteTelecharge = reactive({
    carteAffiche() %>%
      setView(lng = input$carte_center$lng,
              lat = input$carte_center$lat,
              zoom = input$carte_zoom) %>%
      addProviderTiles(providers[[fondSelectionne()]]) 
    
  })
  
# Output ---- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Rayon de lissage

  output$texteRayonLissage = renderText({
    rayonLissage()
    })

  # Carte leaflet

  output$carte = renderLeaflet({
      carteAffiche()
    })

  # Enregistrement de la carte
  
  output$telechargeCarte = downloadHandler(
    filename = function() {
      paste0("Cambriolages.logements.", labelUuChoisie(),".", anneeChoisie(),".png")
    },
    content = function(file){
      mapview::mapshot(x = carteTelecharge(),
                       file = file,
                       cliprect = "viewport",
                       selfcontained = FALSE
      )
    }
  )
  
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
