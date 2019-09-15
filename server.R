############################ SERVER ###########################################
###############################################################################

shinyServer <- function(input, output, session) {
  
  # Transformation des dataframes en réactive pour être appelé dans les modules  
  df_com_rec1 <-  reactive({df_com})
  df_dep_rec1 <-  reactive({df_dep }) 
  df_reg_rec1 <-  reactive({df_reg})
  
  ################ Paramétrage du premier onglet ##############################
  
  # Appel de la fonction dynamiques_input_annee_pop du module dynamiques_input pour rendre les inputs recquis pour la map réactifs
  map1_vars <- callModule(dynamiques_input_annee_pop, 'map1')
  
  # Appel du module mpa1 avec en argument les variables dynamiques et les dataframes dynamiques
  map1<-  callModule(map1,"map1", df_com_rec1, df_dep_rec1, df_reg_rec1, map1_vars)
  
  # Appel des modules de value box
  
  box_pop <- callModule(box_pop, 'map1', df_com_rec1, df_dep_rec1, df_reg_rec1, map1_vars)
  
  box_surface <- callModule(box_surface, 'map1', df_com_rec1, df_dep_rec1, df_reg_rec1, map1_vars)
  
  box_pieces <- callModule(box_pieces, 'map1', df_com_rec1, df_dep_rec1, df_reg_rec1, map1_vars)
  
  box_construction <- callModule(box_construction, 'map1', df_com_rec1, df_dep_rec1, df_reg_rec1, map1_vars)
  
  box_nom <- callModule(box_nom, 'map1', df_com_rec1, df_dep_rec1, df_reg_rec1, map1_vars)
  
  box_pauvrete_tot <- callModule(box_pauvrete_tot, 'map1', df_com_rec1, df_dep_rec1, df_reg_rec1, map1_vars)
  
  box_pauvrete_locataires <- callModule(box_pauvrete_locataires, 'map1', df_com_rec1, df_dep_rec1, df_reg_rec1, map1_vars)
  
  box_niveau_vie <- callModule(box_niveau_vie, 'map1', df_com_rec1, df_dep_rec1, df_reg_rec1, map1_vars)
  
  ################ Paramétrage du deuxième onglet #####################################
  
  # Transformation des dataframes en réactive pour être appelé dans les modules  
  df_com_rec_cartogram <-  reactive({df_com_cartogram})
  df_dep_rec_cartogram <-  reactive({df_dep_cartogram}) 
  df_reg_rec_cartogram <-  reactive({df_reg_cartogram})
  
  communes_indicateurs_reac <-  reactive({communes_indicateurs})
  departements_indicateurs_reac <-  reactive({departements_indicateurs})
  regions_indicateurs_reac <-  reactive({regions_indicateurs})
  
  # Appel de la fonction dynamiques_input_annee_pop du module dynamiques_input pour rendre les inputs recquis pour la map réactifs
  map2_vars <- callModule(dynamiques_input_annee_pop, 'map2')
  
  # Appel du module mpa1 avec en argument les variables dynamiques et les dataframes dynamiques
  map2 <- callModule(map2,
                     "map2", 
                     df_com_rec1, 
                     df_dep_rec1, 
                     df_reg_rec1,
                     df_com_rec_cartogram,
                     df_dep_rec_cartogram,
                     df_reg_rec_cartogram,
                     map2_vars)
  
  tableau<-  callModule(tableau,
                         "map2", 
                        communes_indicateurs_reac , 
                        departements_indicateurs_reac,
                        regions_indicateurs_reac,
                        map2_vars)
  
  box_pop2 <- callModule(box_pop2, 'map2', df_com_rec1, df_dep_rec1, df_reg_rec1, map2_vars)
  
  box_niveau_vie2 <- callModule(box_niveau_vie2, 'map2', df_com_rec1, df_dep_rec1, df_reg_rec1, map2_vars)
  
  box_nom2 <- callModule(box_nom2, 'map2', df_com_rec1, df_dep_rec1, df_reg_rec1, map2_vars)
  
  ################ Paramétrage du troisième onglet #####################################  
  #region
  output$cartogram_contigu_region = renderLeaflet({
    tm <- tm_shape(df_reg_cartogram_contigu) + tm_polygons("proportion_logements_reg",palette=viridis(5, alpha = 1, begin = 0.3, end = 1, direction = 1, option = "D"),
                                       title="Proportion de logements sociaux en %") +
      
      tm_layout(main.title="Croisement à l'échelle région de la médiane de niveau de vie en cartogramme et de la proportion de logements sociaux en choroplèthe en 2016",
                main.title.size=0.6,
                bg.color = "skyblue",
                legend.outside = F,
                main.title.position = "center",
                frame = FALSE)
    tmap_leaflet(tm)
  })
  
  # departement
  
  output$cartogram_contigu_departement = renderLeaflet({
    tm <- tm_shape(df_dep_cartogram_contigu) + tm_polygons("proportion_logements_dep",palette=viridis(5, alpha = 1, begin = 0.3, end = 1, direction = 1, option = "D"),
                                                           title="Proportion de logements sociaux en %") +
      
      tm_layout(main.title="Croisement à l'échelle département de la médiane de niveau de vie en cartogramme et de la proportion de logements sociaux en choroplèthe en 2016",
                main.title.size=0.6,
                bg.color = "skyblue",
                legend.outside = F,
                main.title.position = "center",
                frame = FALSE)
    tmap_leaflet(tm)
  })
  
  # communes enlevées par défaut de lisibilité
  # 
  # output$cartogram_contigu_commune = renderLeaflet({
  #   tm <- tm_shape(df_com_cartogram_contigu) + tm_polygons("proportion_logements",palette=viridis(5, alpha = 1, begin = 0.3, end = 1, direction = 1, option = "D"),
  #                                                          title="Proportion de logements sociaux en %") +
  #     
  #     tm_layout(main.title="Croisement à l'échelle département de la médiane de niveau de vie en cartogramme et de la proportion de logements sociaux en choroplèthe en 2016",
  #               main.title.size=0.6,
  #               bg.color = "skyblue",
  #               legend.position=c("left","bottom"),
  #               legend.outside = F,
  #               main.title.position = "center",
  #               frame = FALSE)
  #   tmap_leaflet(tm)
  # })
  
  ####################### Paramétrage des graphiques #######################
  
  qualitative_data <- reactive({
    
    # Ajout du filtre commune
    filtre_commune_graph <- input$filtre_commune_graph
    
    # Ajout du filtre année
    filtre_annee <- input$filtre_annee
    
    if(filtre_commune_graph != ""){
      
      # Filtrage des données en fonction des inputs, puis sélection des colonnes pertinentes
      # suivie du renommage des niveaux de facteur en niveaux plus lisible, résumé des données
      # par villes et création de 4 dataframes pour chaque type de facteur
      df_datavis_temp <- df_datavis %>% 
        filter(Reduce(`&`, lapply(strsplit(filtre_commune_graph,' ')[[1]], grepl, nom_commune,ignore.case=T))) %>%
        filter(annee == filtre_annee) %>%
        select(consommation_energie_nbr_logements_classe_A, consommation_energie_nbr_logements_classe_B,
               consommation_energie_nbr_logements_classe_C, consommation_energie_nbr_logements_classe_D,
               consommation_energie_nbr_logements_classe_E, consommation_energie_nbr_logements_classe_F,
               consommation_energie_nbr_logements_classe_G, consommation_energie_nbr_logements_NA,
               
               QVP_nbr_logements_quartier_prioritaire, QVP_nbr_logements_quartier_non_prioritaire,
               
               classe_annee_construction_nbr_logements_avant_1900, classe_annee_construction_nbr_logements_1901_1950,
               classe_annee_construction_nbr_logements_1951_1970, classe_annee_construction_nbr_logements_1971_1990,
               classe_annee_construction_nbr_logements_1991_2010, classe_annee_construction_nbr_logements_2011_2017,
               classe_annee_construction_nbr_logements_NA,
               
               surface_habitable_nbr_logements_moins_de_15m2, surface_habitable_nbr_logements_16_30m2,
               surface_habitable_nbr_logements_31_60m2, surface_habitable_nbr_logements_61_90m2,
               surface_habitable_nbr_logements_plus_de_90m2) %>%
        summarise_all(mean) %>%
        ungroup() %>%
        `colnames<-`(gsub('consommation_energie_nbr_logements_classe_A', 'Classe A', names(.))) %>%
        `colnames<-`(gsub('consommation_energie_nbr_logements_classe_B', 'Classe B', names(.))) %>%
        `colnames<-`(gsub('consommation_energie_nbr_logements_classe_C', 'Classe C', names(.))) %>%
        `colnames<-`(gsub('consommation_energie_nbr_logements_classe_D', 'Classe D', names(.))) %>%
        `colnames<-`(gsub('consommation_energie_nbr_logements_classe_E', 'Classe E', names(.))) %>%
        `colnames<-`(gsub('consommation_energie_nbr_logements_classe_F', 'Classe F', names(.))) %>%
        `colnames<-`(gsub('consommation_energie_nbr_logements_classe_G', 'Classe G', names(.))) %>%
        `colnames<-`(gsub('consommation_energie_nbr_logements_NA', 'Classe non renseignée', names(.))) %>%
        `colnames<-`(gsub('QVP_nbr_logements_quartier_prioritaire', 'Quartier Prioritaire', names(.))) %>%
        `colnames<-`(gsub('QVP_nbr_logements_quartier_non_prioritaire', 'Quartier Non Prioritai.', names(.))) %>%
        `colnames<-`(gsub('classe_annee_construction_nbr_logements_avant_1900', 'Construc. < 1900', names(.))) %>%
        `colnames<-`(gsub('classe_annee_construction_nbr_logements_1901_1950', 'Construc. 1901-1950', names(.))) %>%
        `colnames<-`(gsub('classe_annee_construction_nbr_logements_1951_1970', 'Construc. 1951-1970', names(.))) %>%
        `colnames<-`(gsub('classe_annee_construction_nbr_logements_1971_1990', 'Construc. 1971-1990', names(.))) %>%
        `colnames<-`(gsub('classe_annee_construction_nbr_logements_1991_2010', 'Construc. 1991-2010', names(.))) %>%
        `colnames<-`(gsub('classe_annee_construction_nbr_logements_2011_2017', 'Construc. 2011-2017', names(.))) %>%
        `colnames<-`(gsub('classe_annee_construction_nbr_logements_NA', 'Année non renseignée', names(.))) %>%
        `colnames<-`(gsub('surface_habitable_nbr_logements_moins_de_15m2', 'Surface 15m² et inf.', names(.))) %>%
        `colnames<-`(gsub('surface_habitable_nbr_logements_16_30m2', 'Surface 16 à 30m²', names(.))) %>%
        `colnames<-`(gsub('surface_habitable_nbr_logements_31_60m2', 'Surface 31 à 60m²', names(.))) %>%
        `colnames<-`(gsub('surface_habitable_nbr_logements_61_90m2', 'Surface 61 à 90m²', names(.))) %>%
        `colnames<-`(gsub('surface_habitable_nbr_logements_plus_de_90m2', 'Surface sup. à 90m²', names(.))) %>%
        gather("subset_variable", "value")
      
      sub_consommation <- df_datavis_temp %>%
        filter(str_detect(subset_variable, "Classe"))
      sub_qvp <- df_datavis_temp %>%
        filter(str_detect(subset_variable, "Quartier"))
      sub_construction <- df_datavis_temp %>%
        filter(str_detect(subset_variable, "Construc.|construction"))
      sub_surface <- df_datavis_temp %>%
        filter(str_detect(subset_variable, "Surface"))
      
      # Création d'une liste des 4 dataframes
      combo <- list(sub_consommation = sub_consommation, sub_qvp = sub_qvp, sub_construction = sub_construction, sub_surface = sub_surface)
      
      return(combo)
      
    } else {
      
      #Similaire à au-dessus si pas de filtre commune
      df_datavis_temp <- df_datavis %>%
        filter(annee == filtre_annee) %>%
        select(consommation_energie_nbr_logements_classe_A, consommation_energie_nbr_logements_classe_B,
               consommation_energie_nbr_logements_classe_C, consommation_energie_nbr_logements_classe_D,
               consommation_energie_nbr_logements_classe_E, consommation_energie_nbr_logements_classe_F,
               consommation_energie_nbr_logements_classe_G, consommation_energie_nbr_logements_NA,
               
               QVP_nbr_logements_quartier_prioritaire, QVP_nbr_logements_quartier_non_prioritaire,
               
               classe_annee_construction_nbr_logements_avant_1900, classe_annee_construction_nbr_logements_1901_1950,
               classe_annee_construction_nbr_logements_1951_1970, classe_annee_construction_nbr_logements_1971_1990,
               classe_annee_construction_nbr_logements_1991_2010, classe_annee_construction_nbr_logements_2011_2017,
               classe_annee_construction_nbr_logements_NA,
               
               surface_habitable_nbr_logements_moins_de_15m2, surface_habitable_nbr_logements_16_30m2,
               surface_habitable_nbr_logements_31_60m2, surface_habitable_nbr_logements_61_90m2,
               surface_habitable_nbr_logements_plus_de_90m2) %>%
        summarise_all(mean) %>%
        ungroup() %>%
        `colnames<-`(gsub('consommation_energie_nbr_logements_classe_A', 'Classe A', names(.))) %>%
        `colnames<-`(gsub('consommation_energie_nbr_logements_classe_B', 'Classe B', names(.))) %>%
        `colnames<-`(gsub('consommation_energie_nbr_logements_classe_C', 'Classe C', names(.))) %>%
        `colnames<-`(gsub('consommation_energie_nbr_logements_classe_D', 'Classe D', names(.))) %>%
        `colnames<-`(gsub('consommation_energie_nbr_logements_classe_E', 'Classe E', names(.))) %>%
        `colnames<-`(gsub('consommation_energie_nbr_logements_classe_F', 'Classe F', names(.))) %>%
        `colnames<-`(gsub('consommation_energie_nbr_logements_classe_G', 'Classe G', names(.))) %>%
        `colnames<-`(gsub('consommation_energie_nbr_logements_NA', 'Classe non renseignée', names(.))) %>%
        `colnames<-`(gsub('QVP_nbr_logements_quartier_prioritaire', 'Quartier Prioritaire', names(.))) %>%
        `colnames<-`(gsub('QVP_nbr_logements_quartier_non_prioritaire', 'Quartier Non Prioritai.', names(.))) %>%
        `colnames<-`(gsub('classe_annee_construction_nbr_logements_avant_1900', 'Construc. < 1900', names(.))) %>%
        `colnames<-`(gsub('classe_annee_construction_nbr_logements_1901_1950', 'Construc. 1901-1950', names(.))) %>%
        `colnames<-`(gsub('classe_annee_construction_nbr_logements_1951_1970', 'Construc. 1951-1970', names(.))) %>%
        `colnames<-`(gsub('classe_annee_construction_nbr_logements_1971_1990', 'Construc. 1971-1990', names(.))) %>%
        `colnames<-`(gsub('classe_annee_construction_nbr_logements_1991_2010', 'Construc. 1991-2010', names(.))) %>%
        `colnames<-`(gsub('classe_annee_construction_nbr_logements_2011_2017', 'Construc. 2011-2017', names(.))) %>%
        `colnames<-`(gsub('classe_annee_construction_nbr_logements_NA', 'Année non renseignée', names(.))) %>%
        `colnames<-`(gsub('surface_habitable_nbr_logements_moins_de_15m2', 'Surface 15m² et inf.', names(.))) %>%
        `colnames<-`(gsub('surface_habitable_nbr_logements_16_30m2', 'Surface 16 à 30m²', names(.))) %>%
        `colnames<-`(gsub('surface_habitable_nbr_logements_31_60m2', 'Surface 31 à 60m²', names(.))) %>%
        `colnames<-`(gsub('surface_habitable_nbr_logements_61_90m2', 'Surface 61 à 90m²', names(.))) %>%
        `colnames<-`(gsub('surface_habitable_nbr_logements_plus_de_90m2', 'Surface sup. à 90m²', names(.))) %>%
        gather("subset_variable", "value")
      
      sub_consommation <- df_datavis_temp %>%
        filter(str_detect(subset_variable, "Classe"))
      sub_qvp <- df_datavis_temp %>%
        filter(str_detect(subset_variable, "Quartier"))
      sub_construction <- df_datavis_temp %>%
        filter(str_detect(subset_variable, "Construc.|construction"))
      sub_surface <- df_datavis_temp %>%
        filter(str_detect(subset_variable, "Surface"))
      
      combo <- list(sub_consommation = sub_consommation, sub_qvp = sub_qvp, sub_construction = sub_construction, sub_surface = sub_surface)
      
      return(combo)
      
    }
    
  }) 
  
  output$plot_energie <- renderPlotly({
    
    combo <- qualitative_data()
    
    df_energie <- combo$sub_consommation
    
    plot_energie <- ggplot(data = df_energie, 
                           aes(x = "Consommation Energétique", y = value, 
                               group = subset_variable, fill = subset_variable,
                               text = paste("Classe énergétique : ", subset_variable, "<br>",
                                            "Nombre de logements : ", round(value*100), "<br>",
                                            "Pourcentage de logements : ", round(value/sum(value)*100, 2), "%")
                           )) +
      geom_bar(stat = "identity", position = "fill", width = 0.8) +
      coord_flip() +
      scale_fill_viridis_d(direction = -1) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme(line = element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y = element_blank(),
            legend.text = element_text(size = 10),
            panel.background = element_blank()) +
      ggtitle("Répartition des foyers par consommation énergétique") +
      labs(fill = "Catégorie énergétique")
    
    ggplotly(plot_energie, tooltip = "text")
    
  })
  
  output$plot_qvp <- renderPlotly({
    
    combo <- qualitative_data()
    
    df_qvp <- combo$sub_qvp
    
    plot_qvp <- ggplot(data = df_qvp, 
                       aes(x = "Type de Quartier", y = value, 
                           group = subset_variable, fill = subset_variable,
                           text = paste("Type de Quartier : ", subset_variable, "<br>",
                                        "Nombre de logements : ", round(value*100), "<br>",
                                        "Pourcentage de logements : ", round(value/sum(value)*100, 2), "%")
                       )) +
      geom_bar(stat = "identity", position = "fill", width = 0.8) +
      coord_flip() +
      scale_fill_viridis_d(direction = -1) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme(line = element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y = element_blank(),
            legend.text = element_text(size = 10),
            panel.background = element_blank()) +
      ggtitle("Répartition des foyers par type de quartier") +
      labs(fill = "Type de Quartier")
    
    ggplotly(plot_qvp, tooltip = "text") 
    
  })
  
  output$plot_construction <- renderPlotly({
    
    combo <- qualitative_data()
    
    df_construction <- combo$sub_construction
    
    plot_construction <- ggplot(data = df_construction, 
                                aes(x = "Année de Construction", y = value, 
                                    group = fct_rev(subset_variable), fill = fct_rev(subset_variable),
                                    text = paste("Année de construction : ", subset_variable, "<br>",
                                                 "Nombre de logements : ", round(value*100), "<br>",
                                                 "Pourcentage de logements : ", round(value/sum(value)*100, 2), "%")
                                )) +
      geom_bar(stat = "identity", position = "fill", width = 0.8) +
      coord_flip() +
      scale_fill_viridis_d(direction = -1) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme(line = element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y = element_blank(),
            legend.text = element_text(size = 10),
            panel.background = element_blank()) +
      ggtitle("Répartition des foyers par année de construction") +
      labs(fill = "Année de construction")
    
    ggplotly(plot_construction, tooltip = "text") 
    
  })
  
  output$plot_surface <- renderPlotly({
    
    combo <- qualitative_data()
    
    df_surface <- combo$sub_surface
    
    plot_surface <- ggplot(data = df_surface, 
                           aes(x = "Surface du logement", y = value, 
                               group = fct_rev(subset_variable), fill = fct_rev(subset_variable),
                               text = paste("Surface logement : ", subset_variable, "<br>",
                                            "Nombre de logements : ", round(value*100), "<br>",
                                            "Pourcentage de logements : ", round(value/sum(value)*100, 2), "%")
                           )) +
      geom_bar(stat = "identity", position = "fill", width = 0.8) +
      coord_flip() +
      scale_fill_viridis_d(direction = -1) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme(line = element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y = element_blank(),
            legend.text = element_text(size = 10),
            panel.background = element_blank()) +
      ggtitle("Répartition des foyers par surface de logement") +
      labs(fill = "Surface logement")
    
    ggplotly(plot_surface, tooltip = "text")
    
  })
 
  output$text_graph <- renderText("nb : Si aucune ville n'est filtrée, les données représentées correspondront
                        à la moyenne française")
   
}
