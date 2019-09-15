############################ GLOBAL ###########################################

# Le fichier global.R permet de partager des ressources entre le fichier ui.R #
# et server.R, évitant ainsi de répéter du code.                              

###############################################################################

options(encoding = "utf-8")

# Chargement des librairies 
library(shiny)
library(shinydashboard)
library(sf)
library(dplyr)
library(leaflet)
library(shinyWidgets)
library(viridisLite)
library(ggplot2)
library(plotly)
library(tidyr)
library(data.table)
library(stringr)
library(forcats)
library(ggpubr)
library(scales)
library(cartogram)
library(DT)
library(tmap)
library(maptools)
library(markdown)
#library(rsconnect)

#Importation des modules
source("modules/map_functions.R")
source("modules/onglet1.R")
source("modules/onglet2.R")
source("modules/dynamiques_input.R")

welcome <- c("welcome.Rmd")
sapply(welcome, knitr::knit, quiet = T)

# Importation des données des différentes couches géographiques

df_com <- st_read('data/communes.gpkg', layer = "geometry")

df_dep <- st_read('data/departements.gpkg', layer = "geometry")

df_reg <- st_read('data/regions.gpkg', layer = "geometry")

df_com_cartogram <- st_read('data/communes_cartogram.gpkg', layer = "geometry")

df_dep_cartogram <- st_read('data/departements_cartogram.gpkg', layer = "geometry")

df_reg_cartogram <- st_read('data/regions_cartogram.gpkg', layer = "geometry")

communes_indicateurs <- fread('data/communes_indicateurs.csv')

departements_indicateurs <- fread('data/departements_indicateurs.csv')

regions_indicateurs <- fread('data/regions_indicateurs.csv')

df_com_cartogram_contigu <- st_read('data/communes_cartogram_contigu.gpkg', layer = "geometry")

df_dep_cartogram_contigu  <- st_read('data/departements_cartogram_contigu.gpkg', layer = "geometry")

df_reg_cartogram_contigu  <- st_read('data/regions_cartogram_contigu.gpkg', layer = "geometry")

# Paramètres pour cartes

bins_com <- c(0,12.5,25,37.5,50,90)

bins_dep <- c(0,10,20,30,40)

bins_reg <- c(10,15,20,25,30)

df_datavis <- data.table::fread('data/df_agrege.csv', header = TRUE, sep = ",") 
