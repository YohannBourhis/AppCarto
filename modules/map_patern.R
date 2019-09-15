library(shiny)
library(shinydashboard)
library(rmapshaper)
library(data.table)
library(sf)
library(dplyr)
library(leaflet)
library(shinyWidgets)
library(viridisLite)

###################################################################################
                        ##côté UI##
###################################################################################

map1UI <- function(id) {
  ns <- NS(id)

  fluidRow(
  
    column(width = 3,
         box( ns("filtretest"), 
             title = "Filtres", 
             status = "info", 
             solidHeader = TRUE, 
             collapsible = FALSE, 
             width = NULL,
             
             numericInput( inputId =ns("pop_min"), 
                          label = "Population minimum de la commune", 
                          value = 3500,
                          min = 0, 
                          max = max(df$POPULATION, na.rm = T)),
             
             selectInput( inputId =ns("filtre_annee"), 
                         label = "Annee", 
                         choices = df$annee %>% unique(), 
                         selected = 2016),
             
             pickerInput( inputId =ns("filtre_region"),
                         label = "Region", 
                         choices = df$REGION %>% sort() %>% unique(),
                         selected = df$REGION %>% unique(),
                         multiple = TRUE,
                         options =  pickerOptions(actionsBox = TRUE, 
                                                  liveSearch = TRUE,
                                                  liveSearchStyle = 'startsWith')),
             
             pickerInput( inputId =ns("filtre_dep"),
                         label = "Departement", 
                         choices = df$DEPARTEMENT %>% sort() %>% unique(),
                         multiple = TRUE,
                         options =  pickerOptions(actionsBox = TRUE, 
                                                  liveSearch = TRUE,
                                                  liveSearchStyle = 'startsWith')),
             
             pickerInput(inputId = ns("filtre_com"),
                         label = "Commune", 
                         choices = df$NOM_COMMUNE %>% sort() %>% unique(),
                         multiple = TRUE,
                         options = pickerOptions(actionsBox = TRUE, 
                                                 liveSearch = TRUE,
                                                 liveSearchStyle = 'startsWith'))
         )
         
  ),
  column(width = 9,
         
         box(id = ns("maptest"), title = "Croisement seuil de pauvreté, proportion de logements sociaux en fonction des ménages en 2016", 
             status = "info", solidHeader = TRUE, collapsible = FALSE, 
             width = NULL, 
             leafletOutput(ns("map"))
         )
         )
  )
         
}

########################################################################################################
                                  ## Coté serveur ##
########################################################################################################

map_one <- function(input, output, session,df,df_com, df_dep, df_reg) {

  ns <- session$ns
  
  filtre1 <- reactive({
    
    if (is.null(input$filtre_region) |
        is.null(input$filtre_dep)  |
        is.null(input$filtre_com)) 
    {return(df_com() %>% 
              filter(
                (annee == input$filtre_annee | is.na(annee)) &
                  (POPULATION >= input$pop_min ))
    )}
    
    df_com() %>% 
      filter(
        (annee == input$filtre_annee | is.na(annee)) &
          (POPULATION >= input$pop_min ) &
          (REGION %in% input$filtre_region) &
          (DEPARTEMENT %in% input$filtre_dep) &        
          (NOM_COMMUNE %in% input$filtre_com)) })
  
  
  
  filtre2 <- reactive({
    
    if (is.null(input$filtre_region)|
        is.null(input$filtre_dep)) 
    {return(df_dep() %>% filter(
      (annee == input$filtre_annee | is.na(annee)) &
        (POPULATION >= input$pop_min )))}
    
    df_dep() %>% filter(
      (annee == input$filtre_annee | is.na(annee)) &
        (REGION %in% input$filtre_region) &
        (DEPARTEMENT %in% input$filtre_dep)) })
  
  filtre3 <- reactive({
    
    if (is.null(input$filtre_region))
    {return(df_reg() %>% filter(
      (annee == input$filtre_annee | is.na(annee)) &
        (POPULATION >= input$pop_min )))}
    
    df_reg() %>% filter(
      (annee == input$filtre_annee | is.na(annee)) &
        (REGION %in% input$filtre_region))})  
  
  observe({ 
    
    if (is.null(input$filtre_region))
      
    {return(NULL)}
    
    dep <- df()$DEPARTEMENT[df()$REGION %in% input$filtre_region]
    updatePickerInput(session, 'filtre_dep', choices = unique(sort(dep)))})
  
  observe({ 
    if (is.null(input$filtre_dep))
      
    {return(NULL)}
    
    com <- df()$NOM_COMMUNE[(df()$DEPARTEMENT %in% input$filtre_dep) & (df()$POPULATION >= input$pop_min) ]
    updatePickerInput(session, 'filtre_com', choices = unique(sort(com)))})  
  
  
  output$map<- renderLeaflet({
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g logements sociaux<br/>qui représentent %g pourcents de la totalité des foyers",
      filtre1()$NOM_COMMUNE, 
      filtre1()$nbr_logements_ville,
      filtre1()$prop_log) %>%
      lapply(htmltools::HTML)
    
    labels2 <- sprintf(
      "<strong>%s</strong><br/>%g logements sociaux<br/>qui représentent %g pourcents de la totalité des foyers",
      filtre2()$DEPARTEMENT, 
      filtre2()$nbr_logements_dep,
      filtre2()$prop_log_dep) %>%
      lapply(htmltools::HTML)
    
    labels3 <- sprintf(
      "<strong>%s</strong><br/>%g logements sociaux<br/>qui représentent %g pourcents de la totalité des foyers",
      filtre3()$REGION, 
      filtre3()$nbr_logements_reg,
      filtre3()$prop_log_reg) %>%
      lapply(htmltools::HTML)
    
    
    color1 <- colorBin("viridis", domain = 
                         filtre1()$prop_log,
                       bins = bins_com)
    
    color2 <- colorBin("viridis", domain = 
                         filtre2()$prop_log_dep,
                       bins = bins_dep) 
    
    color3 <- colorBin("viridis", domain = 
                         filtre3()$prop_log_reg,
                       bins = bins_reg) 
    
    
    map <- leaflet() %>%
      
      fitBounds(-4,51,7,42) %>% 
      
      addProviderTiles(providers$OpenStreetMap.France) %>%
      
      addPolygons(data = filtre1(),
                  weight = 1, 
                  color = "#999999",
                  fillColor=~color1(prop_log),
                  fillOpacity=0.7, 
                  group = "Communes",
                  highlightOptions = highlightOptions(
                    color = "#000000", opacity = 1, weight = 2, fillOpacity = 1,
                    bringToFront = TRUE, sendToBack = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addPolygons(data = filtre2(),
                  weight = 1, 
                  color = "#999999",
                  fillColor=~color2(prop_log_dep),
                  fillOpacity=0.7, 
                  group = "Departement",
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
                  fillColor=~color3(prop_log_reg),
                  fillOpacity=0.7, 
                  group = "Regions",
                  highlightOptions = highlightOptions(
                    color = "#000000", opacity = 1, weight = 2, fillOpacity = 1,
                    bringToFront = TRUE, sendToBack = TRUE),
                  label = labels3,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      
      addLayersControl(baseGroups = c("Regions", "Departement","Communes"))  %>%
      addLegend("bottomright", 
                pal = color1,
                values = filtre1()$prop_log,
                title = "Taux de logements sociaux<br/>par rapport aux foyers fiscaux",
                opacity = 1)
    
    return(map)
    
  })
  
}



  
