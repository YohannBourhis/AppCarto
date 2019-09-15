############################ Paramétrage de l'onglet 1 #########################
###############################################################################

###################################################################################
##côté UI##
###################################################################################

map2UI <- function(id) {
  ns <- NS(id)

tabItem(tabName = "Cartogram",   fluidRow(valueBoxOutput(ns("box_nom2")),
                                          valueBoxOutput(ns("box_niveau_vie2")),
                                          valueBoxOutput(ns("box_pop2"))),
        fluidRow(column(width = 4,box_input_ui(id))),
        
                                 fluidRow(column(width = 6,
                                                 
                                                 box(id = ns("map2ui"),
                                                     title = "Cartogramme croisant le seuil de pauvreté avec proportion de logements sociaux",
                                                     status = "info",
                                                     solidHeader = TRUE,
                                                     collapsible = FALSE,
                                                     width = NULL,
                                                     leafletOutput(ns("map2")))
                                          ),
                                            column(width = 6,box(
                                              
                                                          id = ns("map2ui"),
                                                          title = "Comparaison avec les niveaux geographiques superieurs",
                                                          status = "info",
                                                          solidHeader = TRUE,
                                                          collapsible = FALSE,
                                                          width = NULL,
                                                          dataTableOutput(ns("tableau")))
                                                          
                                                          ))

)
}

########################################################################################################
## Coté serveur ##
########################################################################################################

map2 <- function(input, output, session, df_com, df_dep, df_reg,df_com_cartogram, df_dep_cartogram, df_reg_cartogram, map_vars) {
  
  ns <- session$ns
  
  filtre1_contours <- filter_year_pop(input, output, session, df_com, map_vars )
  filtre2_contours <- filter_year(input, output, session, df_dep, map_vars )
  filtre3_contours <- filter_year(input, output, session, df_reg, map_vars )
  
  
  
  filtre1 <- filter_year_pop(input, output, session, df_com_cartogram, map_vars )
  filtre2 <- filter_year(input, output, session, df_dep_cartogram, map_vars )
  filtre3 <- filter_year(input, output, session, df_reg_cartogram, map_vars )
  
  
  
  output$map2<- renderLeaflet({
    
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
    
    
    map2 <- baseMap(input, output, session) %>%
      
      addPolygons(data = filtre1_contours(),
                  weight = 1,
                  color = "#999999",
                  group = "Commune")%>%

      
      addPolygons(data = filtre2_contours(),
                  weight = 1,
                  color = "#999999",
                  group = "Département") %>%

      addPolygons(data = filtre3_contours(),
                  weight = 1,
                  color = "#999999",
                  group = "Région")   %>%
      
      addPolygons(data = filtre1(),
                  weight = 1,
                  color = "#999999",
                  fillColor=~color1(proportion_logements),
                  fillOpacity=0.7,
                  group = "Commune",
                  layerId = filtre1()$NOM_COMMUNE,
                  highlightOptions = highlightOptions(
                    color = "#000000", opacity = 1, weight = 2, fillOpacity = 1,
                    bringToFront = TRUE, sendToBack = FALSE),
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
                    bringToFront = TRUE, sendToBack = FALSE),
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
                    bringToFront = TRUE, sendToBack = FALSE),
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
      
      addLayersControl(overlayGroups = c("Région", "Département", "Commune"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      
      hideGroup(c("Département","Commune")) %>%
      
      addMiniMap(position = "bottomleft", toggleDisplay = TRUE)
    
    return(map2)
  })
}

# Implantation du DT

tableau <- function(input, output, session, df_com, df_dep, df_reg, map_vars) {
  
  ns <- session$ns
  
  filtre1 <- filter_year(input, output, session, df_com, map_vars )
  filtre2 <- filter_year(input, output, session, df_dep, map_vars )
  filtre3 <- filter_year(input, output, session, df_reg, map_vars )

  
  df <- eventReactive(input$map2_shape_click,{
    
    click <- input$map2_shape_click
    
    if (is.null(click$group)) {return(NULL)}
    
    else if (click$group == "Région") {
      df <- filtre3() %>%
        filter(REGION == click$id) %>% 
        select(indicateur, valeur)
      return(df)
    }
    

    else if (click$group == "Département") {
      df <- filtre2() %>%
        filter(DEPARTEMENT == click$id) %>% 
        select(indicateur, valeur, ecart_region)
      return(df)
    }

    else if (click$group == "Commune") {
      df <- filtre1() %>%
        filter(NOM_COMMUNE == click$id) %>% 
        select(indicateur, valeur, ecart_region, ecart_departement)
      return(df)
    }

  })
  
  output$tableau <- renderDataTable({
    
    click <- input$map2_shape_click
    
    if (is.null(click$group)) {return(NULL)}
    
    else if (click$group == "Région") {
      datatable(df(), 
                options = list(dom = 't')) 
    }
    
    
    else if (click$group == "Département") {
    datatable(df(), 
              options = list(dom = 't')) %>%
      
      formatStyle('ecart_region',
                   color = styleInterval(0, c('red', 'green')))
    }
    
    else if (click$group == "Commune") {
      datatable(df(),
                options = list(dom = 't')) %>%

        formatStyle('ecart_region',
                     color = styleInterval(0, c('red', 'green')))%>%

        formatStyle('ecart_departement',
                    color = styleInterval(0, c('red', 'green')))

    }
    
    

  })
}



box_pop2 <- function(input, output, session, df_com, df_dep, df_reg, map_vars) {
  
  ns <- session$ns
  
  filtre1 <- filter_year_pop(input, output, session, df_com, map_vars )
  filtre2 <- filter_year(input, output, session, df_dep, map_vars )
  filtre3 <- filter_year(input, output, session, df_reg, map_vars )
  
  
  pop_map2 <- eventReactive(input$map2_shape_click,{
    
    click <- input$map2_shape_click
    
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
  
  output$box_pop2<- renderValueBox({
    valueBox(paste(pop_map2(),'%'),
             "de logements sociaux par rapport au nombre de foyers fiscaux",
             icon = icon("list"),
             color = "olive", 
             width = 12)
  })
}

############################################ Valuebox de Médiane de niveau de vie #####################################

box_niveau_vie2 <- function(input, output, session, df_com, df_dep, df_reg, map_vars) {
  
  ns <- session$ns
  
  filtre1 <- filter_year_pop(input, output, session, df_com, map_vars )
  filtre2 <- filter_year(input, output, session, df_dep, map_vars )
  filtre3 <- filter_year(input, output, session, df_reg, map_vars )
  
  
  niveau_vie_map2 <- eventReactive(input$map2_shape_click,{
    
    click <- input$map2_shape_click
    
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
  
  output$box_niveau_vie2<- renderValueBox({
    valueBox(paste(niveau_vie_map2(),"euros"),
             "de revenu median",
             icon = icon("list"),
             color = "olive", 
             width = 12)
  })
}

############################################ Valuebox de nom du géom #####################################

box_nom2 <- function(input, output, session, df_com, df_dep, df_reg, map_vars) {
  
  ns <- session$ns
  
  filtre1 <- filter_year_pop(input, output, session, df_com, map_vars )
  filtre2 <- filter_year(input, output, session, df_dep, map_vars )
  filtre3 <- filter_year(input, output, session, df_reg, map_vars )
  
  
  nom_map2 <- eventReactive(input$map2_shape_click,{
    
    click <- input$map2_shape_click
    
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
  
  output$box_nom2<- renderValueBox({
    valueBox(nom_map2()[[1]],
             nom_map2()[[2]],
             icon = icon("list"),
             color = "olive", 
             width = 12)
  })
}
