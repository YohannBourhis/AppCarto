############################ Utilitaires pour les maps #########################
###############################################################################



############################# Fonctions ui #####################################

#creation de box contenant les filtres de selection de population et d'annee

box_input_ui <- function(id) {
  ns <- NS(id)

box <- box(title = 'Filtres', 
       status = "info", 
       solidHeader = TRUE, 
       collapsible = FALSE, 
       width = NULL,
       
       numericInput( inputId =ns("pop_min"), 
                     label = "Population minimum de la commune", 
                     value = 3500,
                     min = 0, 
                     max = 2500000),
       
       selectInput( inputId =ns("filtre_annee"), 
                    label = "Année", 
                    choices = c(2016, 2017, 2018), 
                    selected = 2016)
       )
  return(box)
}

########################## Fonctions server ########################################

############## df filtrée réactive ##############################

## Filtre population + année

filter_year_pop <- function(input, output, session, df, map_vars) {
  ns <- session$ns
  
  filter_year <- reactive({map_vars$filter_year()})
  filter_pop <- reactive({map_vars$filter_pop()})
  
  filtre <- reactive({
    df() %>% 
      filter((annee == filter_year() | is.na(annee)) &
               (POPULATION >= filter_pop()) ) 
    })
    return(filtre)
}

## Filtre année

filter_year <- function(input, output, session, df, map_vars) {
  ns <- session$ns
  
  filter_year <- reactive({map_vars$filter_year()})

  filtre <- reactive({
    df() %>% 
      filter((annee == filter_year() | is.na(annee)) )
    })
    return(filtre)
}


############## Création de map de base ######################

baseMap <- function(input, output, session){
  ns <- session$ns
  
  leaflet() %>%
    fitBounds(-4,51,7,42) %>% 
    addProviderTiles(providers$CartoDB)
  
}


############# Création d'une valubox ############################
vbox <- function(input, output, session, var) {
  output$box <- renderValueBox({
    valueBox(
      paste0(input$var," logements"),
      icon = icon("list"),
      color = "olive")
  })
  return(box)
}





