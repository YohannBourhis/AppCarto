############################ Inputs dynamiques ###################################
##################################################################################

########################### Historique ########################################

# Créé le 24/08/19 par Yohann 

## Création de la fonction server dynamiques_input_annee_pop pour rendre les inputs dynamiques

##################################################################################
##################################################################################

##### Fonction serveur : création des input dynamiques
 
dynamiques_input_annee_pop <- function(input, output, session) {
 
 return(
   list(
     filter_year = reactive({ input$filtre_annee }),
     filter_pop = reactive({ input$pop_min })
   )
 )
}

