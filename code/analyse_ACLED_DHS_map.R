# Netoyer et indiquer le repertoire de travail
# install.packages("here")
library(here)

rm(list = ls()) 
set.seed(3)


# install.packages("htmltools")
library(htmltools)
help(htmltools)

# Installer l'ensemble des packages
library(devtools)
options(timeout = 600)
install_github("PPgp/wpp2022")
devtools::install_github("mrc-ide/demogsurv")


library(readstata13)
library(rdhs)
# library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(khroma)    # Pour les coleurs de graphique ggplot
library(questionr)
library(testthat) # Pour les tests de coherence
library(survival) 
library(esquisse) # Pour les graphiques interatifs
library(labelled)
library(data.table)
library(haven)
library(demogsurv)
library(gdata)
library(survey)
library(anesrake)
library(gtsummary)
library(wpp2022)
library(gridExtra)
library(magick)
library(pdftools)
library(readr)
# Parametres pour avoir acces aux donnees de DHS
# install.packages(c("ggplot2", "dplyr", "tidyr", "sf"))
library(sf) # Pour gérer les données géospatiales
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)
library(ggrepel) # Pour les labels
library(patchwork) # Pour combiner les graphiques
library(readxl)
library(scales)


# Télécharger les données -------------------------------------------------

gedevents_2025_01_17 <- read_csv("data", "gedevents-2025-01-17.csv")

ucdp <- read_delim("data", "2016-02-01-2025-01-16-Western_Africa-Burkina_Faso.csv", 
                                                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

head(ucdp)
table(ucdp$year)
table(ucdp$admin1)
table(ucdp$admin2)
table(ucdp$admin3)


table(ucdp$admin1, ucdp$year)


# Graphique 2 -------------------------------------------------------------

# Charger la base de données
# Remplacez 'votre_base_donnees.csv' par le fichier contenant vos données.
data = ucdp 

# Exemple de format attendu pour les données :
# Region, Province, Commune, Annee, Nombre_evenements

# Agréger les données par région, année, et compter les événements
heatmap_data <- data %>%
  group_by(admin1, year) %>%
  summarise(Nombre_evenements = sum(fatalities, na.rm = TRUE)) %>%
  ungroup()

# Transformation pour le format "long" pour ggplot
heatmap_data <- heatmap_data %>%
  pivot_wider(names_from = year, values_from = Nombre_evenements, values_fill = 0) %>%
  pivot_longer(-admin1, names_to = "Annee", values_to = "Nombre_evenements")

table(heatmap_data$Nombre_evenements)
heatmap_data$Nombre_evenements = (heatmap_data$Nombre_evenements/10)


# 1. Créer un ordre des régions basé sur le nombre de fatalités en 2023
ordre_regions <- heatmap_data %>%
  filter(Annee == 2023) %>%
  arrange(desc(Nombre_evenements)) %>%
  pull(admin1)

# 2. Convertir admin1 en facteur ordonné
heatmap_data$admin1 <- factor(heatmap_data$admin1, levels = ordre_regions)

pdf('../figures/figure2_trend.pdf', width = 7*2*0.8, height =7*1*0.8)
# 3. Graphique avec l'ordre imposé
ggplot(heatmap_data[heatmap_data$Annee < 2025,], 
       aes(x = Annee, y = admin1, fill = Nombre_evenements)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = c("#f7fbff", "#6baed6", "#08306b"),
                       name = "Fatalities") +
  labs(
    title = "",
    x = "Year",
    y = "Region"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
graphics.off()



# Graphique1 --------------------------------------------------------------

# Cartes régional Burkina Faso --------------------------------------------

# gadm41_BFA_1
burkina_regions <- st_read("data", "gadm41_BFA_1.shp")

# Afficher la structure pour voir les colonnes disponibles
print(burkina_regions)

# Calculer un point à l'intérieur de chaque région pour le placement du label
burkina_centroids <- st_point_on_surface(burkina_regions)

# Ajouter le nom des régions si pas déjà dedans
burkina_centroids$NAME_1 <- burkina_regions$NAME_1


# Graphique avec les proportions des zones enquêtées et non  --------------

dhs_surv <- read_excel("data", "prop_surveyed.xlsx")

dhs_surv <- dhs_surv %>%
  mutate(prop_ns_pct = round(prop_ns * 100, 1))

colnames(burkina_regions)[4] = "region"

burkina_regions <-  burkina_regions %>%
  left_join(dhs_surv , by = c("region" = "region"))


# Ajouter les centroids pour les labels des régions
burkina_regions_centroids <- burkina_regions %>%
  group_by(region) %>%
  summarise(geometry = st_centroid(st_union(geometry))) %>%
  st_as_sf()

burkina_regions_centroids <-  burkina_regions_centroids %>%
  left_join(dhs_surv , by = c("region" = "region"))


# Calcul des centroïdes avec projection adaptée
burkina_centroids <- burkina_regions %>%
  st_transform(crs = 32630) %>%
  st_centroid() %>%
  st_transform(crs = st_crs(burkina_regions))  # revenir au CRS initial si besoin

# Ajouter les données DHS (si ce n’est pas déjà fait)
burkina_centroids <- burkina_centroids %>%
  left_join(dhs_surv, by = c("region" = "region"))

# Exporter en PDF
pdf( '../figures/Figure1_Map_dhs.pdf', width = 7*2*0.8, height = 7*2*0.8)


# Créer la carte
ggplot(data = burkina_regions) +
  geom_sf(aes(fill = prop_ns_pct), color = "black") +
  
  # Noms des régions avec geom_text_repel
  geom_text_repel(
    data = burkina_centroids,
    aes(label = region, geometry = geometry),
    stat = "sf_coordinates",
    size = 3,
    color = "blue"
  ) +
  
  # Valeurs de prop_ns_pct au centre
  geom_sf_text(
    data = burkina_centroids,
    aes(label = percent(prop_ns_pct.x / 100)),
    size = 3,
    color = "red"
  ) +
  
  # Dégradé de couleurs
  scale_fill_gradient(low = "white", high = "gray40", name = "Proportion (%)") +
  
  labs(
    title = "Carte administrative des régions du Burkina Faso",
    caption = "Source : DHS"
  ) +
  theme_minimal() +
  labs(
    title = "",
    subtitle = "",
    caption = "*The proportions indicate the percentage of clusters that were not surveyed in the 2021 Demographic and Health Survey (DHS)",
    fill = "Groupes",
    x = NULL,
    y = NULL
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(face = "bold"),
    # plot.caption = element_text(hjust = 0.5, size = 8, face = "italic"),
    axis.text = element_blank(),         # Supprimer les étiquettes des axes
    axis.ticks = element_blank()         # Supprimer les ticks des axes
  )
graphics.off()
