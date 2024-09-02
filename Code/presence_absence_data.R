# Presence/absence data
# 02/09/2024

library(tidyr)
library(sf)
library(dplyr)
library(car)

#### Dados de ocorrência ####
# 1. Ler o arquivo shapefile
sp <- st_read("C:/Users/Bianca/Documents/USP/Projeto/eBird/Data/1km/ebd_total_1km_urb_f1km_30min_10spp.shp")

# 2. Criar uma tabela de espécies únicas e IDs de grade
unique_species <- unique(sp$scntfc_)
unique_ids <- unique(sp$id)

# 3. Ordenar IDs e espécies
unique_species <- sort(unique_species)
unique_ids <- sort(unique_ids)

# 4. Inicializar a matriz de presença-ausência com zeros
presence_absence <- matrix(0, nrow = length(unique_ids), ncol = length(unique_species))
colnames(presence_absence) <- unique_species
rownames(presence_absence) <- unique_ids

# 5. Preencher a matriz de presença-ausência
for (i in 1:nrow(sp)) {
  grid_id <- sp$id[i]
  species <- sp$scntfc_[i]
  presence_absence[as.character(grid_id), as.character(species)] <- 1
}

# 6. Converter a matriz para um data frame para melhor legibilidade
pa <- as.data.frame(presence_absence)
