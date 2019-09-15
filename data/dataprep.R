############################ Data prep ###########################################
##################################################################################

########################### Historique ########################################
############### Créé le 24/08/19 par Yohann ##############################

# Traitement des données brutes avant utilisation dans shiny

# Indicateurs ajoutés : 

## Niveau commune :     proportion_logements
## Niveau département : proportion_logements_dep, POPULATION_dep, nbr_logements_dep, nb_menages_fiscaux_dep
## Niveau région :      proportion_logements_reg POPULATION_reg, nbr_logements_reg, nb_menages_fiscaux_reg

############### Ajout le 05/09/19 par Yohann ##############################

# Correction des groupby en ajoutant l'année

# Indicateurs ajoutés :

## Niveau département : surface_moyenne_dep, nb_pieces_moyen_dep, annee_construction_moyenne_dep
## Niveau région :      surface_moyenne_reg, nb_pieces_moyen_reg, annee_construction_moyenne_reg

############### Ajout le 07/09/19 par Yohann ##############################

# débugage variable année sur 2017 et 2018

##################################################################################
##################################################################################

#Librairies nécessaires
library(rmapshaper)
library(data.table)
library(sf)
library(dplyr)
library(cartogram)
library(tidyr)



#################### Chargement des données #####################################

# Géoms des différentes couches
## source : IGN ADMIN EXPRESS

communes <- st_read('data/shapefiles', layer = 'COMMUNE_CARTO')

departements <- st_read(dsn = 'data/shapefiles',layer = 'DEPARTEMENT_CARTO')

regions <- st_read('data/shapefiles',layer = 'REGION_CARTO')

#Données du projet précédent

df <- data.table::fread('data/df_finale.csv', header = TRUE, sep = ";", dec = ",") 

df$code_INSEE <- ifelse(nchar(df$code_INSEE) <5, paste(0,df$code_INSEE,sep=''),df$code_INSEE)

df$NUM_DEP <- ifelse(nchar(df$NUM_DEP) <2, paste(0,df$NUM_DEP,sep=''),df$NUM_DEP)


############# Traitement des géoms pour diminuer le volume de données ########### 

communes <- communes %>% 
  select(INSEE_COM,geometry)%>%
  rename(code_INSEE = INSEE_COM) %>%
  rmapshaper::ms_simplify(keep = 0.01) %>% 
  inner_join(df, by = "code_INSEE")
  
departements <-departements %>% 
  select(INSEE_DEP, NOM_DEP_M, geometry) %>% 
  rename(NUM_DEP = INSEE_DEP, DEPARTEMENT=NOM_DEP_M)%>%
  rmapshaper::ms_simplify(keep = 0.01)


regions <-regions %>% 
  select(INSEE_REG,NOM_REG_M,geometry) %>% 
  rename(NUM_REGION = INSEE_REG, REGION = NOM_REG_M)%>%
  rmapshaper::ms_simplify(keep = 0.01) 

departements_drop <- departements %>% st_drop_geometry()

regions_drop <- regions %>% st_drop_geometry()
regions_drop$NUM_REGION <-  as.numeric(as.character(regions_drop$NUM_REGION))

communes <- communes %>% 
  inner_join(departements_drop, by = "NUM_DEP")%>% 
  inner_join(regions_drop, by = "NUM_REGION") %>% 
  filter(NUM_DEP != "974" & NUM_DEP != "973" & NUM_DEP != "972" & NUM_DEP != "971")

departements <-departements %>% 
  select(NUM_DEP, geometry) %>% 
  filter(NUM_DEP != "974" & NUM_DEP != "973" & NUM_DEP != "972" & NUM_DEP != "971")

regions <-regions %>% 
  select(NUM_REGION,geometry) %>% 
  filter(NUM_REGION != "01" & NUM_REGION != "02" & NUM_REGION != "03" & NUM_REGION != "04")



######## Création des indicateurs de proportion de logement sociaux ###########################

communes$moyenne_surface_habitable_logement <- as.numeric(communes$moyenne_surface_habitable_logement)
communes$moyenne_nombre_de_pieces_logement <- as.numeric(communes$moyenne_nombre_de_pieces_logement)
communes$moyenne_annee_achevement_construction <- as.numeric(communes$moyenne_annee_achevement_construction)

communes <-communes %>% 
  mutate(proportion_logements = round(nbr_logements_ville/nb_menages_fiscaux_com*100,2)) %>% 
  group_by(REGION, annee)%>%
  mutate(POPULATION_reg = sum(POPULATION, na.rm = T),
         nbr_logements_reg = sum(nbr_logements_ville, na.rm = T),
         nb_menages_fiscaux_reg = sum(nb_menages_fiscaux_com, na.rm = T),
         proportion_logements_reg = round(nbr_logements_reg/nb_menages_fiscaux_reg*100,2)) %>%
  ungroup() %>%
  group_by(DEPARTEMENT, annee) %>% 
  mutate(POPULATION_dep = sum(POPULATION, na.rm = T),
         nbr_logements_dep = sum(nbr_logements_ville, na.rm = T),
         nb_menages_fiscaux_dep = sum(nb_menages_fiscaux_com, na.rm = T),
         proportion_logements_dep = round(nbr_logements_dep/nb_menages_fiscaux_dep*100,2)) %>% ungroup()

communes$proportion_logements_reg = na_if(communes$proportion_logements_reg, Inf)

communes$proportion_logements_dep = na_if(communes$proportion_logements_dep,Inf)

######## Création des indicateurs de moyenne de surface habitable ###########################

communes <-communes %>% 
  group_by(REGION, annee)%>%
  mutate(surface_totale_reg = sum((moyenne_surface_habitable_logement*nbr_logements_ville), na.rm = T),
         surface_moyenne_reg = round(surface_totale_reg/nbr_logements_reg ,2)) %>%
  ungroup() %>%
  group_by(DEPARTEMENT, annee) %>% 
  mutate(surface_totale_dep = sum((moyenne_surface_habitable_logement*nbr_logements_ville), na.rm = T),
         surface_moyenne_dep = round(surface_totale_dep/nbr_logements_dep ,2)) %>%
  ungroup() %>% 
  mutate(moyenne_surface_habitable_logement = round(moyenne_surface_habitable_logement,2))

######## Création des indicateurs de moyenne de nombre de pièces ###########################

communes <-communes %>% 
  group_by(REGION, annee)%>%
  mutate(nb_pieces_totale_reg = sum((moyenne_nombre_de_pieces_logement*nbr_logements_ville), na.rm = T),
         nb_pieces_moyen_reg = round(nb_pieces_totale_reg/nbr_logements_reg ,2)) %>%
  ungroup() %>%
  group_by(DEPARTEMENT, annee) %>% 
  mutate(nb_pieces_totale_dep = sum((moyenne_nombre_de_pieces_logement*nbr_logements_ville), na.rm = T),
         nb_pieces_moyen_dep = round(nb_pieces_totale_dep/nbr_logements_dep ,2)) %>%
  ungroup() %>% 
  mutate(moyenne_nombre_de_pieces_logement = round(moyenne_nombre_de_pieces_logement,2))

######## Création des indicateurs de moyenne de date de construction ###########################

communes <-communes %>% 
  group_by(REGION, annee)%>%
  mutate(nbr_batiments_reg = sum(nbr_batiments, na.rm = T),
         annee_construction_totale_reg = sum((moyenne_annee_achevement_construction*nbr_batiments), na.rm = T),
         annee_construction_moyenne_reg = round(annee_construction_totale_reg/nbr_batiments_reg)) %>%
  ungroup() %>%
  group_by(DEPARTEMENT, annee) %>% 
  mutate(nbr_batiments_dep = sum(nbr_batiments, na.rm = T),
         annee_construction_totale_dep = sum((moyenne_annee_achevement_construction*nbr_batiments), na.rm = T),
         annee_construction_moyenne_dep = round(annee_construction_totale_dep/nbr_batiments_dep)) %>%
  ungroup() %>% 
  mutate(moyenne_annee_achevement_construction = round(moyenne_annee_achevement_construction))

######## Création des indicateurs pour le tableau de l'onglet 2 ###########################

# Commune

communes_indicateurs <-communes %>% gather("proportion_logements",
                                           "moyenne_surface_habitable_logement",
                                           "moyenne_nombre_de_pieces_logement",
                                           "moyenne_annee_achevement_construction",
                                           "tx_pauvrete_total_com",
                                           "tx_pauvrete_locataires_com",
                                           "Mediane_niv_vie_com",
                                           key = indicateur, value = valeur)  

communes_indicateurs$valeur <- as.numeric(communes_indicateurs$valeur)

  
communes_indicateurs$ecart_departement <- ifelse(communes_indicateurs$indicateur == "proportion_logements", 
                                             communes_indicateurs$valeur - communes_indicateurs$proportion_logements_dep,
                 
                                            ifelse(communes_indicateurs$indicateur == "moyenne_surface_habitable_logement", 
                                                  communes_indicateurs$valeur - communes_indicateurs$surface_moyenne_dep,
                                                   
                                                ifelse(communes_indicateurs$indicateur == "moyenne_nombre_de_pieces_logement", 
                                                          communes_indicateurs$valeur - communes_indicateurs$nb_pieces_moyen_dep,
                                                       
                                                       ifelse(communes_indicateurs$indicateur == "moyenne_annee_achevement_construction", 
                                                              communes_indicateurs$valeur - communes_indicateurs$annee_construction_moyenne_dep,
                                                              
                                                              ifelse(communes_indicateurs$indicateur == "tx_pauvrete_total_com", 
                                                                    communes_indicateurs$valeur - communes_indicateurs$tx_pauvrete_total_dep,
                                                                     
                                                                     ifelse(communes_indicateurs$indicateur == "tx_pauvrete_locataires_com", 
                                                                            communes_indicateurs$valeur - communes_indicateurs$tx_pauvrete_locataires_dep,
                                                                              communes_indicateurs$valeur - communes_indicateurs$Mediane_niv_vie_dep
                                                        ))))))

communes_indicateurs$ecart_region <- ifelse(communes_indicateurs$indicateur == "proportion_logements", 
                                                 communes_indicateurs$valeur - communes_indicateurs$proportion_logements_reg,
                                                 
                                                 ifelse(communes_indicateurs$indicateur == "moyenne_surface_habitable_logement", 
                                                        communes_indicateurs$valeur - communes_indicateurs$surface_moyenne_reg,
                                                        
                                                        ifelse(communes_indicateurs$indicateur == "moyenne_nombre_de_pieces_logement", 
                                                               communes_indicateurs$valeur - communes_indicateurs$nb_pieces_moyen_reg,
                                                               
                                                               ifelse(communes_indicateurs$indicateur == "moyenne_annee_achevement_construction", 
                                                                      communes_indicateurs$valeur - communes_indicateurs$annee_construction_moyenne_reg,
                                                                      
                                                                      ifelse(communes_indicateurs$indicateur == "tx_pauvrete_total_com", 
                                                                             communes_indicateurs$valeur - communes_indicateurs$tx_pauvrete_total_reg,
                                                                             
                                                                             ifelse(communes_indicateurs$indicateur == "tx_pauvrete_locataires_com", 
                                                                                    communes_indicateurs$valeur - communes_indicateurs$tx_pauvrete_locataires_reg,
                                                                                    communes_indicateurs$valeur - communes_indicateurs$Mediane_niv_vie_reg
                                                                             ))))))

communes_indicateurs <- communes_indicateurs %>%select("annee",
                                                       "NOM_COMMUNE",
                                                       "indicateur",
                                                       "valeur", 
                                                       "ecart_departement",
                                                       "ecart_region") %>%
                                                st_drop_geometry() %>%  
                                                unique() %>%
                                                na.omit() %>% 
    tibble::rowid_to_column() %>% 
    spread(key = indicateur, value = valeur) %>% 
    rename(part_logements_sociaux_en_pourcentage = proportion_logements,
          surface_habitable_moyenne_en_m2  = moyenne_surface_habitable_logement,
          nombre_de_pieces_moyen  = moyenne_nombre_de_pieces_logement,
          moyenne_d_annee_de_construction  = moyenne_annee_achevement_construction,
          taux_de_pauvrete_de_la_population_en_pourcentage = tx_pauvrete_total_com,
          taux_de_pauvrete_des_locataires_en_pourcentage = tx_pauvrete_locataires_com,
          revenu_median_en_euros = Mediane_niv_vie_com)%>% gather("part_logements_sociaux_en_pourcentage",
                                                                  "surface_habitable_moyenne_en_m2",
                                                                  "nombre_de_pieces_moyen",
                                                                  "moyenne_d_annee_de_construction",
                                                                  "taux_de_pauvrete_de_la_population_en_pourcentage",
                                                                  "taux_de_pauvrete_des_locataires_en_pourcentage",
                                                                  "revenu_median_en_euros",
                                                                  key = indicateur, value = valeur)%>%
  na.omit() %>% 
  mutate(ecart_region = round(ecart_region,2),
         ecart_departement = round(ecart_departement,2))
  
  

  
# Departement

departements_indicateurs <-communes %>% gather("proportion_logements_dep",
                                           "surface_moyenne_dep",
                                           "nb_pieces_moyen_dep",
                                           "annee_construction_moyenne_dep",
                                           "tx_pauvrete_total_dep",
                                           "tx_pauvrete_locataires_dep",
                                           "Mediane_niv_vie_dep",
                                           key = indicateur, value = valeur)  

departements_indicateurs$valeur <- as.numeric(departements_indicateurs$valeur)

departements_indicateurs$ecart_region <- ifelse(departements_indicateurs$indicateur == "proportion_logements_dep", 
                                                departements_indicateurs$valeur - departements_indicateurs$proportion_logements_reg,
                                            
                                            ifelse(departements_indicateurs$indicateur == "surface_moyenne_dep", 
                                                   departements_indicateurs$valeur - departements_indicateurs$surface_moyenne_reg,
                                                   
                                                   ifelse(departements_indicateurs$indicateur == "nb_pieces_moyen_dep", 
                                                          departements_indicateurs$valeur - departements_indicateurs$nb_pieces_moyen_reg,
                                                          
                                                          ifelse(departements_indicateurs$indicateur == "annee_construction_moyenne_dep", 
                                                                 departements_indicateurs$valeur - departements_indicateurs$annee_construction_moyenne_reg,
                                                                 
                                                                 ifelse(departements_indicateurs$indicateur == "tx_pauvrete_total_dep", 
                                                                        departements_indicateurs$valeur - departements_indicateurs$tx_pauvrete_total_reg,
                                                                        
                                                                        ifelse(departements_indicateurs$indicateur == "tx_pauvrete_locataires_dep", 
                                                                               departements_indicateurs$valeur - departements_indicateurs$tx_pauvrete_locataires_reg,
                                                                               departements_indicateurs$valeur - departements_indicateurs$Mediane_niv_vie_reg
                                                                        ))))))

departements_indicateurs <- departements_indicateurs %>%select("annee",
                                                       "DEPARTEMENT",
                                                       "indicateur",
                                                       "valeur", 
                                                       "ecart_region") %>%
  st_drop_geometry() %>%  
  unique() %>%
  na.omit()%>% 
  tibble::rowid_to_column() %>% 
  spread(key = indicateur, value = valeur) %>% 
  rename(part_logements_sociaux_en_pourcentage = proportion_logements_dep,
         surface_habitable_moyenne_en_m2  = surface_moyenne_dep,
         nombre_de_pieces_moyen  = nb_pieces_moyen_dep,
         moyenne_d_annee_de_construction  = annee_construction_moyenne_dep,
         taux_de_pauvrete_de_la_population_en_pourcentage = tx_pauvrete_total_dep,
         taux_de_pauvrete_des_locataires_en_pourcentage = tx_pauvrete_locataires_dep,
         revenu_median_en_euros = Mediane_niv_vie_dep)%>% gather("part_logements_sociaux_en_pourcentage",
                                                                 "surface_habitable_moyenne_en_m2",
                                                                 "nombre_de_pieces_moyen",
                                                                 "moyenne_d_annee_de_construction",
                                                                 "taux_de_pauvrete_de_la_population_en_pourcentage",
                                                                 "taux_de_pauvrete_des_locataires_en_pourcentage",
                                                                 "revenu_median_en_euros",
                                                                 key = indicateur, value = valeur)%>%
  na.omit() %>% 
  mutate(ecart_region = round(ecart_region,2))

# Regions

regions_indicateurs <-communes %>%
  st_drop_geometry() %>%  
  rename(part_logements_sociaux_en_pourcentage = proportion_logements_reg,
         surface_habitable_moyenne_en_m2  = surface_moyenne_reg,
         nombre_de_pieces_moyen  = nb_pieces_moyen_reg,
         moyenne_d_annee_de_construction  = annee_construction_moyenne_reg,
         taux_de_pauvrete_de_la_population_en_pourcentage = tx_pauvrete_total_reg,
         taux_de_pauvrete_des_locataires_en_pourcentage = tx_pauvrete_locataires_reg,
         revenu_median_en_euros = Mediane_niv_vie_reg)%>% gather("part_logements_sociaux_en_pourcentage",
                                                                 "surface_habitable_moyenne_en_m2",
                                                                 "nombre_de_pieces_moyen",
                                                                 "moyenne_d_annee_de_construction",
                                                                 "taux_de_pauvrete_de_la_population_en_pourcentage",
                                                                 "taux_de_pauvrete_des_locataires_en_pourcentage",
                                                                 "revenu_median_en_euros",
                                                                 key = indicateur, value = valeur) %>%select("annee",
                                                                                                             "REGION",
                                                                                                             "indicateur",
                                                                                                             "valeur")%>%  
  unique() %>%
  na.omit()


############################# Préparation des données de sortie ################################

# Dataprep

communes_drop <- communes %>% st_drop_geometry()

regions$NUM_REGION <-  as.numeric(as.character(regions$NUM_REGION))

# Communes
communes <- communes %>%
  select(annee,
         NOM_COMMUNE,
         POPULATION,
         proportion_logements, 
         nbr_logements_ville,
         moyenne_surface_habitable_logement,
         moyenne_nombre_de_pieces_logement,
         moyenne_annee_achevement_construction,
         nb_menages_fiscaux_com,
         tx_pauvrete_total_com,
         tx_pauvrete_locataires_com,
         Mediane_niv_vie_com,
         geometry)

# Departements

departements <- communes_drop %>% 
  select(annee,
         DEPARTEMENT,
         NUM_DEP,
         POPULATION_dep,
         proportion_logements_dep, 
         nbr_logements_dep,
         surface_moyenne_dep,
         nb_pieces_moyen_dep,
         annee_construction_moyenne_dep,
         nb_menages_fiscaux_dep,
         tx_pauvrete_total_dep,
         Mediane_niv_vie_dep,
         tx_pauvrete_locataires_dep) %>%
  unique() %>%
  na.omit() %>% 
  inner_join(departements)


# Regions

regions <- communes_drop %>%
  select(annee,
         REGION,
         NUM_REGION,
         POPULATION_reg,
         proportion_logements_reg,
         nbr_logements_reg,
         surface_moyenne_reg,
         nb_pieces_moyen_reg,
         annee_construction_moyenne_reg,
         nb_menages_fiscaux_reg,
         tx_pauvrete_total_reg,
         Mediane_niv_vie_reg,
         tx_pauvrete_locataires_reg) %>% 
  unique() %>%
  na.omit() %>% 
  inner_join(regions)

########################### Ecriture des fichiers de sortie ####################################


st_write(communes, 'data/communes.gpkg', 
         layer_options = "OVERWRITE=YES",
         layer = "geometry")

st_write(departements, 'data/departements.gpkg', 
         layer_options = "OVERWRITE=YES",
         layer = "geometry")

st_write(regions, 'data/regions.gpkg', 
         layer_options = "OVERWRITE=YES",
         layer = "geometry")

fwrite(communes_indicateurs, 'data/communes_indicateurs.csv')
fwrite(departements_indicateurs, 'data/departements_indicateurs.csv')
fwrite(regions_indicateurs, 'data/regions_indicateurs.csv')

#Création des df cartogram

df_com <- st_read('data/communes.gpkg', layer = "geometry")

df_dep <- st_read('data/departements.gpkg', layer = "geometry")

df_reg <- st_read('data/regions.gpkg', layer = "geometry")

################################################################################### Cartogram non contigu

# traitement commune

df_com_carto <- df_com %>% filter(annee == 2016)%>%
  mutate(log_Mediane_niv_vie_com = log(Mediane_niv_vie_com)) %>%
  select(NOM_COMMUNE,
         log_Mediane_niv_vie_com,
                            geom) %>% 
  st_transform(2154)%>% 
  as_Spatial() %>%
  cartogram_ncont( "log_Mediane_niv_vie_com") %>%
  st_as_sf() %>% 
  st_transform(4326)

df_com_f <- df_com %>%
  st_drop_geometry() %>% 
  inner_join(df_com_carto) %>%  st_as_sf() %>%
  st_set_crs(4326)


  
# traitement dep

df_dep_carto <- df_dep %>% filter(annee == 2016)%>%
  mutate(log_Mediane_niv_vie_dep = log(Mediane_niv_vie_dep)) %>%
  select(DEPARTEMENT,
         log_Mediane_niv_vie_dep,
         geom) %>% 
  st_transform(2154)%>% 
  as_Spatial() %>%
  cartogram_ncont( "log_Mediane_niv_vie_dep",2) %>%
  st_as_sf() %>% 
  st_transform(4326)

df_dep_f <- df_dep %>%
  st_drop_geometry() %>% 
  inner_join(df_dep_carto)%>%  st_as_sf() %>%
  st_set_crs(4326)

# traitement reg

df_reg_carto <- df_reg %>% filter(annee == 2016) %>%
  mutate(log_Mediane_niv_vie_reg = log(Mediane_niv_vie_reg)) %>%
  select(REGION,
         log_Mediane_niv_vie_reg,
         geom) %>% 
  st_transform(2154)%>% 
  as_Spatial() %>%
  cartogram_ncont( "log_Mediane_niv_vie_reg") %>%
  st_as_sf() %>% 
  st_transform(4326)

df_reg_f <- df_reg %>%
  st_drop_geometry() %>% 
  inner_join(df_reg_carto)%>%  st_as_sf() %>%
  st_set_crs(4326)

# Ecriture des df cartogram

st_write(df_com_f, 'data/communes_cartogram.gpkg', 
         layer_options = "OVERWRITE=YES",
         layer = "geometry")

st_write(df_dep_f, 'data/departements_cartogram.gpkg', 
         layer_options = "OVERWRITE=YES",
         layer = "geometry")

st_write(df_reg_f, 'data/regions_cartogram.gpkg', 
         layer_options = "OVERWRITE=YES",
         layer = "geometry")

################################################################################### Cartogram continu

# traitement commune

df_com_carto_c <- df_com %>% filter(annee == 2016)%>%
  select(NOM_COMMUNE,
         Mediane_niv_vie_com,
         geom) %>% 
  st_transform(2154) %>%
  cartogram_cont( "Mediane_niv_vie_com") 

df_com_c_f <- df_com %>%
  st_drop_geometry() %>% 
  inner_join(df_com_carto_c)%>%  st_as_sf() %>%
  st_set_crs(2154)

# traitement dep

df_dep_carto_c <- df_dep %>% filter(annee == 2016)%>%
  select(DEPARTEMENT,
         Mediane_niv_vie_dep,
         geom) %>% 
  st_transform(2154)%>% 
  cartogram_cont("Mediane_niv_vie_dep") 


df_dep_c_f <- df_dep %>%
  st_drop_geometry() %>% 
  inner_join(df_dep_carto_c)%>%  st_as_sf() %>%
  st_set_crs(2154)

# traitement reg

df_reg_carto_c <- df_reg %>% filter(annee == 2016) %>%
  select(REGION,
         Mediane_niv_vie_reg,
         geom) %>% 
  st_transform(2154)%>%
  cartogram_cont("Mediane_niv_vie_reg")

df_reg_c_f <- df_reg %>%
  st_drop_geometry() %>% 
  inner_join(df_reg_carto_c)%>%  st_as_sf() %>%
  st_set_crs(2154)

# Ecriture des df cartogram

st_write(df_com_c_f, 'data/communes_cartogram_contigu.gpkg', 
         layer_options = "OVERWRITE=YES",
         layer = "geometry")

st_write(df_dep_c_f, 'data/departements_cartogram_contigu.gpkg', 
         layer_options = "OVERWRITE=YES",
         layer = "geometry")

st_write(df_reg_c_f, 'data/regions_cartogram_contigu.gpkg', 
         layer_options = "OVERWRITE=YES",
         layer = "geometry")

################################################################################### Cartogram dorling

# traitement commune

df_com_carto_d <- df_com %>% filter(annee == 2016)%>%
  select(NOM_COMMUNE,
         Mediane_niv_vie_com,
         geom) %>% 
  st_transform(2154) %>%
  na.omit() %>% 
  cartogram_dorling( "Mediane_niv_vie_com") 

df_com_d_f <- df_com %>%
  st_drop_geometry() %>% 
  inner_join(df_com_carto_d)%>%  st_as_sf() %>%
  st_set_crs(2154)

# traitement dep

df_dep_carto_d <- df_dep %>% filter(annee == 2016)%>%
  select(DEPARTEMENT,
         Mediane_niv_vie_dep,
         geom) %>% 
  st_transform(2154)%>% 
  cartogram_dorling("Mediane_niv_vie_dep") 


df_dep_d_f <- df_dep %>%
  st_drop_geometry() %>% 
  inner_join(df_dep_carto_d)%>%  st_as_sf() %>%
  st_set_crs(2154)

# traitement reg

df_reg_carto_d <- df_reg %>% filter(annee == 2016) %>%
  select(REGION,
         Mediane_niv_vie_reg,
         geom) %>% 
  st_transform(2154)%>%
  cartogram_dorling("Mediane_niv_vie_reg")

df_reg_d_f <- df_reg %>%
  st_drop_geometry() %>% 
  inner_join(df_reg_carto_d)%>%  st_as_sf() %>%
  st_set_crs(2154)

# Ecriture des df cartogram

st_write(df_com_d_f, 'data/communes_cartogram_dorling.gpkg', 
         layer_options = "OVERWRITE=YES",
         layer = "geometry")

st_write(df_dep_d_f, 'data/departements_cartogram_dorling.gpkg', 
         layer_options = "OVERWRITE=YES",
         layer = "geometry")

st_write(df_reg_d_f, 'data/regions_cartogram_dorling.gpkg', 
         layer_options = "OVERWRITE=YES",
         layer = "geometry")



tm_shape(df_com_d_f)+ tm_borders()
+ tm_polygons("proportion_logements",palette=viridis(5, alpha = 1, begin = 0.3, end = 1, direction = 1, option = "D"),
                                   title="Proportion de logements sociaux en %") +
  
  tm_layout(frame = FALSE)

str(df_reg_c_f)
