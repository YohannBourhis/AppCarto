############################ Paramétrage de l'onglet 1 #########################
###############################################################################

#############
## côté UI ##
#############

map1UI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = "visualisation_generale",  fluidRow(
    
    column(width = 3, box_input_ui(id)),
    
    column(width = 6,
           
           box(id = ns("map1ui"),
               title = "Proportion des logements sociaux en fonction des ménages",
               status = "info",
               solidHeader = TRUE,
               collapsible = FALSE,
               width = NULL,
               leafletOutput(ns("map1"))))),
    
    
    fluidRow(
      valueBoxOutput(ns("box_nom")),
      valueBoxOutput(ns("box_pop"))),
    fluidRow(
    valueBoxOutput(ns("box_surface")),
    valueBoxOutput(ns("box_pieces")),
    valueBoxOutput(ns("box_construction"))),
    fluidRow(
    valueBoxOutput(ns("box_pauvrete_tot")),
    valueBoxOutput(ns("box_pauvrete_locataires")),
    valueBoxOutput(ns("box_niveau_vie")))
    
    
  )
  
}

##################
## Coté serveur ##
##################

map1 <- function(input, output, session, df_com, df_dep, df_reg, map_vars) {
  
  ns <- session$ns
  
  filtre1 <- filter_year_pop(input, output, session, df_com, map_vars )
  filtre2 <- filter_year(input, output, session, df_dep, map_vars )
  filtre3 <- filter_year(input, output, session, df_reg, map_vars )
  
  output$map1<- renderLeaflet({
    
    labels <-sprintf(
      "<strong>%s</strong> (%g habitants)<br/>%g logements sociaux<br/>%g foyers fiscaux",
      filtre1()$NOM_COMMUNE,
      filtre1()$POPULATION,
      filtre1()$nbr_logements_ville,
      filtre1()$nb_menages_fiscaux_com
    ) %>%
      lapply(htmltools::HTML)
    
    labels2 <- sprintf(
      "<strong>%s</strong> (%g habitants)<br/>%g logements sociaux<br/>%g foyers fiscaux",
      filtre2()$DEPARTEMENT,
      filtre2()$POPULATION_dep,
      filtre2()$nbr_logements_dep,
      filtre2()$nb_menages_fiscaux_dep) %>%
      lapply(htmltools::HTML)
    
    labels3 <- sprintf(
      "<strong>%s</strong> (%g habitants)<br/>%g logements sociaux<br/>%g foyers fiscaux",
      filtre3()$REGION, 
      filtre3()$POPULATION_reg,
      filtre3()$nbr_logements_reg,
      filtre3()$nb_menages_fiscaux_reg) %>%
      lapply(htmltools::HTML)
    
    
    color1 <- colorBin("viridis", domain = filtre1()$proportion_logements, bins = bins_com)
    
    color2 <- colorBin("viridis", domain = filtre2()$proportion_logements_dep, bins = bins_dep) 
    
    color3 <- colorBin("viridis", domain = filtre3()$proportion_logements_reg, bins = bins_reg) 
    
    
    map1 <- baseMap(input, output, session) %>%
      
      addPolygons(data = filtre1(),
                  weight = 1,
                  color = "#999999",
                  fillColor=~color1(proportion_logements),
                  fillOpacity=0.7,
                  group = "Commune",
                  layerId = filtre1()$NOM_COMMUNE,
                  highlightOptions = highlightOptions(
                    color = "#000000", opacity = 1, weight = 2, fillOpacity = 1,
                    bringToFront = TRUE, sendToBack = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))%>%
      
      addPolygons(data = filtre2(),
                  weight = 1,
                  color = "#999999",
                  fillColor=~color2(proportion_logements_dep),
                  fillOpacity=0.7,
                  group = "Département",
                  layerId = filtre2()$DEPARTEMENT,
                  highlightOptions = highlightOptions(
                    color = "#000000", opacity = 1, weight = 2, fillOpacity = 1,
                    bringToFront = TRUE, sendToBack = TRUE),
                  label = labels2,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      
      addPolygons(data = filtre3(),
                  weight = 1,
                  color = "#999999",
                  fillColor=~color3(proportion_logements_reg),
                  fillOpacity=0.7,
                  group = "Région",
                  layerId = filtre3()$REGION,
                  highlightOptions = highlightOptions(
                    color = "#000000", opacity = 1, weight = 2, fillOpacity = 1,
                    bringToFront = TRUE, sendToBack = TRUE),
                  label = labels3,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))   %>%
      
      addLegend("bottomright",
                pal = color1,
                group = "Commune",
                values = filtre1()$proportion_logements,
                title = "Taux de logements sociaux<br/>par rapport aux foyers fiscaux",
                opacity = 1) %>%
      
      addLegend("bottomright",
                pal = color2,
                group =  "Département",
                values = filtre2()$proportion_logements_dep,
                title = "Taux de logements sociaux<br/>par rapport aux foyers fiscaux",
                opacity = 1) %>%
      
      addLegend("bottomright",
                pal = color3,
                group = "Région",
                values = filtre3()$proportion_logements_reg,
                title = "Taux de logements sociaux<br/>par rapport aux foyers fiscaux",
                opacity = 1) %>%
      
      addLayersControl(overlayGroups = c("Région", "Département","Commune"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      
      hideGroup(c("Département","Commune"))
    
    return(map1)
    
  })
}
############################################ Valuebox de proportion de logements sociaux #####################################

box_pop <- function(input, output, session, df_com, df_dep, df_reg, map_vars) {
  
  ns <- session$ns
  
  filtre1 <- filter_year_pop(input, output, session, df_com, map_vars )
  filtre2 <- filter_year(input, output, session, df_dep, map_vars )
  filtre3 <- filter_year(input, output, session, df_reg, map_vars )
  
  
  pop_map1 <- eventReactive(input$map1_shape_click,{
    
    click <- input$map1_shape_click
    
    if (is.null(click$group)) {return(NULL)}
    
    else if (click$group == "Région") {
      proportion_logements <- filtre3() %>%
        filter(REGION == click$id) %>%
        select(proportion_logements_reg)
      proportion_logements_reg <- proportion_logements[[1]]
      return(proportion_logements_reg)
    }
    
    else if (click$group == "Département") {
      proportion_logements <- filtre2() %>%
        filter(DEPARTEMENT == click$id) %>%
        select(proportion_logements_dep)
      proportion_logements_dep <- proportion_logements[[1]]
      return(proportion_logements_dep)
    }
    
    else if (click$group == "Commune") {
      proportion_logements <- filtre1() %>%
        filter(NOM_COMMUNE == click$id) %>%
        select(proportion_logements)
      proportion_logements_com <- proportion_logements[[1]]
      return(proportion_logements_com)
    }
  })
  output$box_pop<- renderValueBox({
    valueBox(paste(pop_map1(),'%'),
             "de logements sociaux par rapport au nombre de foyers fiscaux",
             icon = icon("home"),
             color = "olive", 
             width = 6)
  })
}

############################################ Valuebox de surface moyenne par logement #####################################

box_surface <- function(input, output, session, df_com, df_dep, df_reg, map_vars) {
  
  ns <- session$ns
  
  filtre1 <- filter_year_pop(input, output, session, df_com, map_vars )
  filtre2 <- filter_year(input, output, session, df_dep, map_vars )
  filtre3 <- filter_year(input, output, session, df_reg, map_vars )
  
  
  surface_map1 <- eventReactive(input$map1_shape_click,{
    
    click <- input$map1_shape_click
    
    if (is.null(click$group)) {return(NULL)}
    
    else if (click$group == "Région") {
      surface_logements <- filtre3() %>%
        filter(REGION == click$id) %>%
        select(surface_moyenne_reg)
      surface_logements_reg <- surface_logements[[1]]
      return(surface_logements_reg)
    }
    
    else if (click$group == "Département") {
      surface_logements <- filtre2() %>%
        filter(DEPARTEMENT == click$id) %>%
        select(surface_moyenne_dep)
      surface_logements_dep <- surface_logements[[1]]
      return(surface_logements_dep)
    }
    
    else if (click$group == "Commune") {
      surface_logements <- filtre1() %>%
        filter(NOM_COMMUNE == click$id) %>%
        select(moyenne_surface_habitable_logement)
      surface_logements_com <- surface_logements[[1]]
      return(surface_logements_com)
    }
  })
  output$box_surface<- renderValueBox({
    valueBox(paste(surface_map1(),'m2'),
             "de surface moyenne dans les logements sociaux",
             icon = icon("home"),
             color = "olive", 
             width = 6)
  })
}

############################################ Valuebox de nombre de pieces par logement #####################################

box_pieces <- function(input, output, session, df_com, df_dep, df_reg, map_vars) {
  
  ns <- session$ns
  
  filtre1 <- filter_year_pop(input, output, session, df_com, map_vars )
  filtre2 <- filter_year(input, output, session, df_dep, map_vars )
  filtre3 <- filter_year(input, output, session, df_reg, map_vars )
  
  
  pieces_map1 <- eventReactive(input$map1_shape_click,{
    
    click <- input$map1_shape_click
    
    if (is.null(click$group)) {return(NULL)}
    
    else if (click$group == "Région") {
      pieces_logements <- filtre3() %>%
        filter(REGION == click$id) %>%
        select(nb_pieces_moyen_reg)
      pieces_logements_reg <- pieces_logements[[1]]
      return(pieces_logements_reg)
    }
    
    else if (click$group == "Département") {
      pieces_logements <- filtre2() %>%
        filter(DEPARTEMENT == click$id) %>%
        select(nb_pieces_moyen_dep)
      pieces_logements_dep <- pieces_logements[[1]]
      return(pieces_logements_dep)
    }
    
    else if (click$group == "Commune") {
      pieces_logements <- filtre1() %>%
        filter(NOM_COMMUNE == click$id) %>%
        select(moyenne_nombre_de_pieces_logement)
      pieces_logements_com <- pieces_logements[[1]]
      return(pieces_logements_com)
    }
  })
  output$box_pieces<- renderValueBox({
    valueBox(paste(pieces_map1(),'pièces'),
             "en moyenne dans les logements sociaux",
             icon = icon("home"),
             color = "olive", 
             width = 6)
  })
}

############################################ Valuebox d'année moyenne de construction du batiment #####################################

box_construction <- function(input, output, session, df_com, df_dep, df_reg, map_vars) {
  
  ns <- session$ns
  
  filtre1 <- filter_year_pop(input, output, session, df_com, map_vars )
  filtre2 <- filter_year(input, output, session, df_dep, map_vars )
  filtre3 <- filter_year(input, output, session, df_reg, map_vars )
  
  
  construction_map1 <- eventReactive(input$map1_shape_click,{
    
    click <- input$map1_shape_click
    
    if (is.null(click$group)) {return(NULL)}
    
    else if (click$group == "Région") {
      construction_logements <- filtre3() %>%
        filter(REGION == click$id) %>%
        select(annee_construction_moyenne_reg)
      construction_logements_reg <- construction_logements[[1]]
      return(construction_logements_reg)
    }
    
    else if (click$group == "Département") {
      construction_logements <- filtre2() %>%
        filter(DEPARTEMENT == click$id) %>%
        select(annee_construction_moyenne_dep)
      construction_logements_dep <- construction_logements[[1]]
      return(construction_logements_dep)
    }
    
    else if (click$group == "Commune") {
      construction_logements <- filtre1() %>%
        filter(NOM_COMMUNE == click$id) %>%
        select(moyenne_annee_achevement_construction)
      construction_logements_com <- construction_logements[[1]]
      return(construction_logements_com)
    }
  })
  output$box_construction<- renderValueBox({
    valueBox(construction_map1(),
             "année moyenne de construction des logements",
             icon = icon("home"),
             color = "olive", 
             width = 6)
  })
}

############################################ Valuebox de nom du géom #####################################

box_nom <- function(input, output, session, df_com, df_dep, df_reg, map_vars) {
  
  ns <- session$ns
  
  filtre1 <- filter_year_pop(input, output, session, df_com, map_vars )
  filtre2 <- filter_year(input, output, session, df_dep, map_vars )
  filtre3 <- filter_year(input, output, session, df_reg, map_vars )
  
  
  nom_map1 <- eventReactive(input$map1_shape_click,{
    
    click <- input$map1_shape_click
    
    if (is.null(click$group)) {return(NULL)}
    
    else if (click$group == "Région") {
      layer <- "Région"
      
      return(list(click$id, layer))
    }
    
    else if (click$group == "Département") {
      layer <- "Département"
      
      return(list(click$id, layer))
    }
    
    else if (click$group == "Commune") {
      layer <- "Commune"
      
      return(list(click$id, layer))
    }
  })
  output$box_nom<- renderValueBox({
    valueBox(
      nom_map1()[[1]],
      nom_map1()[[2]],
      icon = icon("map-marked-alt"),
      color = "olive", 
      width = 6)
  })
}
############################################ Valuebox de taux de pauvreté général #####################################

box_pauvrete_tot <- function(input, output, session, df_com, df_dep, df_reg, map_vars) {
  
  ns <- session$ns
  
  filtre1 <- filter_year_pop(input, output, session, df_com, map_vars )
  filtre2 <- filter_year(input, output, session, df_dep, map_vars )
  filtre3 <- filter_year(input, output, session, df_reg, map_vars )
  
  
  pauvrete_tot_map1 <- eventReactive(input$map1_shape_click,{
    
    click <- input$map1_shape_click
    
    if (is.null(click$group)) {return(NULL)}
    
    else if (click$group == "Région") {
      pauvrete_tot <- filtre3() %>%
        filter(REGION == click$id) %>%
        select(tx_pauvrete_total_reg)
      pauvrete_tot_reg <- pauvrete_tot[[1]]
      return(pauvrete_tot_reg)
    }
    
    else if (click$group == "Département") {
      pauvrete_tot <- filtre2() %>%
        filter(DEPARTEMENT == click$id) %>%
        select(tx_pauvrete_total_dep)
      pauvrete_tot_dep <- pauvrete_tot[[1]]
      return(pauvrete_tot_dep)
    }
    
    else if (click$group == "Commune") {
      pauvrete_tot <- filtre1() %>%
        filter(NOM_COMMUNE == click$id) %>%
        select(tx_pauvrete_total_com)
      pauvrete_tot_com <- pauvrete_tot[[1]]
      return(pauvrete_tot_com)
    }
  })
  output$box_pauvrete_tot<- renderValueBox({
    valueBox(paste(pauvrete_tot_map1(),"%"),
             "de personnes sous le seuil de pauvreté",
             icon = icon("arrow-alt-circle-down"),
             color = "olive", 
             width = 6)
  })
}

############################################ Valuebox de taux de pauvreté locataires #####################################

box_pauvrete_locataires <- function(input, output, session, df_com, df_dep, df_reg, map_vars) {
  
  ns <- session$ns
  
  filtre1 <- filter_year_pop(input, output, session, df_com, map_vars )
  filtre2 <- filter_year(input, output, session, df_dep, map_vars )
  filtre3 <- filter_year(input, output, session, df_reg, map_vars )
  
  
  pauvrete_locataires_map1 <- eventReactive(input$map1_shape_click,{
    
    click <- input$map1_shape_click
    
    if (is.null(click$group)) {return(NULL)}
    
    else if (click$group == "Région") {
      pauvrete_locataires <- filtre3() %>%
        filter(REGION == click$id) %>%
        select(tx_pauvrete_locataires_reg)
      pauvrete_locataires_reg <- pauvrete_locataires[[1]]
      return(pauvrete_locataires_reg)
    }
    
    else if (click$group == "Département") {
      pauvrete_locataires <- filtre2() %>%
        filter(DEPARTEMENT == click$id) %>%
        select(tx_pauvrete_locataires_dep)
      pauvrete_locataires_dep <- pauvrete_locataires[[1]]
      return(pauvrete_locataires_dep)
    }
    
    else if (click$group == "Commune") {
      pauvrete_locataires <- filtre1() %>%
        filter(NOM_COMMUNE == click$id) %>%
        select(tx_pauvrete_locataires_com)
      pauvrete_locataires_com <- pauvrete_locataires[[1]]
      return(pauvrete_locataires_com)
    }
  })
  output$box_pauvrete_locataires<- renderValueBox({
    valueBox(paste(pauvrete_locataires_map1(),"%"),
             "de locataires sous le seuil de pauvreté",
             icon = icon("arrow-alt-circle-down"),
             color = "olive", 
             width = 6)
  })
}

############################################ Valuebox de Médiane de niveau de vie #####################################

box_niveau_vie <- function(input, output, session, df_com, df_dep, df_reg, map_vars) {
  
  ns <- session$ns
  
  filtre1 <- filter_year_pop(input, output, session, df_com, map_vars )
  filtre2 <- filter_year(input, output, session, df_dep, map_vars )
  filtre3 <- filter_year(input, output, session, df_reg, map_vars )
  
  
  niveau_vie_map1 <- eventReactive(input$map1_shape_click,{
    
    click <- input$map1_shape_click
    
    if (is.null(click$group)) {return(NULL)}
    
    else if (click$group == "Région") {
      niveau_vie <- filtre3() %>%
        filter(REGION == click$id) %>%
        select(Mediane_niv_vie_reg)
      niveau_vie_reg <- niveau_vie[[1]]
      return(niveau_vie_reg)
    }
    
    else if (click$group == "Département") {
      niveau_vie <- filtre2() %>%
        filter(DEPARTEMENT == click$id) %>%
        select(Mediane_niv_vie_dep)
      niveau_vie_dep <- niveau_vie[[1]]
      return(niveau_vie_dep)
    }
    
    else if (click$group == "Commune") {
      niveau_vie <- filtre1() %>%
        filter(NOM_COMMUNE == click$id) %>%
        select(Mediane_niv_vie_com)
      niveau_vie_com <- niveau_vie[[1]]
      return(niveau_vie_com)
    }
  })
  output$box_niveau_vie<- renderValueBox({
    valueBox(paste(niveau_vie_map1(),"€"),
             "de revenu médian",
             icon = icon("euro-sign"),
             color = "olive", 
             width = 6)
  })
}