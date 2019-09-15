##################### UI #####################
##############################################
# Le script ui.R correspond à la disposition #
# des éléments au sein du dashboard.         #
##############################################

dashboardPage(
  
  # Couleur du thème du dashboard ("black", "blue", "purple"...)
  skin = "black",
  
  # Barre d'en-tête
  dashboardHeader(title = "Logements sociaux"),
              
  # Barre de menu
  dashboardSidebar(
    
    # Contenu du menu
    sidebarMenu(
      
      menuItem(text = "Accueil", tabName = "Accueil", icon = icon("home")),
      
      menuItem(text = "Analyse descriptive", tabName = "analyse_descriptive", icon = icon("atlas"),
               
               menuSubItem(
                 text = "Visualisation générale", 
                 tabName = "visualisation_generale"),
               menuSubItem(
                 text = "Caractéristiques des logements", 
                 tabName = "variables_qualitatives")
               
               ),
                  
      menuItem(text = "Logements et Richesse", tabName = "info_sup", icon = icon("book"),
               
               menuSubItem(
                 text = "Cartogrammes non contigus", 
                 tabName = "Cartogram"),
               menuSubItem(
                 text = "Cartogrammes contigus", 
                 tabName = "n_cont")
               
               
               )
      ) #Fin du sidebarMenu()
    
  ), #Fin du dashboardSidebar()
              
  #Corps du dashboard, équivalent d'un <main> sous HTML
  dashboardBody(
    #On reprend chaque menuItem
    tabItems(
      #Ainsi que chaque menuSubItem
      tabItem(tabName = "Accueil",
              
              withMathJax(includeMarkdown("welcome.md"))
              
      ),
      
      #Ici, chaque tabName défini dans la sideBar peut-être complété d'un élément
      #qui s'affichera dans le main. Il suffit pour cela de rappeler le namespace
      #du menuItem/menuSubItem avec tabName, puis d'ajouter un output qui prendra
      #une fonction serveur comme argument.
      
              
              map1UI("map1")
              ,
      
      
      
      tabItem(tabName = "variables_qualitatives",
              
              fluidRow(
                
                column(6, textInput("filtre_commune_graph",
                                    "Filtrer les données pour la commune de : ", NULL)),
                
                column(6, selectInput("filtre_annee", "Année : ", 
                                      choices = list(
                                        "2016" = 2016,
                                        "2017" = 2017,
                                        "2018" = 2018),
                      selected = 2018)
                      )
                ),
              
              plotlyOutput("plot_energie", height = "150px"),
              plotlyOutput("plot_qvp", height = "150px"),
              plotlyOutput("plot_construction", height = "150px"),
              plotlyOutput("plot_surface", height = "150px"),
              
              textOutput("text_graph")
              
              ),
      
      map2UI("map2"),
      
      tabItem(tabName = "n_cont", fluidRow(leafletOutput("cartogram_contigu_region")),
                                  fluidRow(leafletOutput("cartogram_contigu_departement")))
       
      ) #Fin des tabItems
    
    ) #Fin du dashboardBody
  
  ) #Fin du dashboardPage

