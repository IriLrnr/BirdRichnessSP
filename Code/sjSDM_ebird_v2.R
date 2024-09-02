# Joint Species Distribution Modeling with bird species
# Bianca Neves e Irina Lerner
# 25/06/2024

library(sjSDM)
library(tidyr)
library(sf)
library(dplyr)
library(car)

#### Dados de ocorrência ####
# 1. Ler o arquivo shapefile
sp <- st_read("Z:/Bianca/eBird/Data/1km_urb_f1km/ebd_total_v6_1km_urb_f1km_30min.shp")

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

#### Dados ambientais ####
# 1. Ler o arquivo shapefile
shp <- st_read("Z:/Bianca/eBird/Data/1km_urb_f1km/richness_effort_v10_total_1km_urb_f1km_30min.shp")

# 2. Selecionar apenas as colunas de interesse
colunas_interesse <- c("id", "effort", "Bvol", "Vvol", "area_mn", "np", "shdi")
amb <- shp[, colunas_interesse]

# 3. Ordenar o data frame pelo ID
amb <- amb[order(amb$id), ]

# 4. Transformar NA em 0
amb[is.na(amb)] <- 0

#### Dados espaciais ####
# 1. Calcular os centroides dos polígonos
centroids <- st_centroid(shp)

# 2. Extrair as coordenadas centroides
coords <- st_coordinates(centroids)

# 3. Criar um data frame com as colunas id, coord_x e coord_y
coord <- data.frame(
  id = shp$id, 
  coord_x = coords[, 1], 
  coord_y = coords[, 2]
)

# 4. Ordenar o data frame pelo id
coord <- coord[order(coord$id), ]

#### Verificar colinearidade ####
verif <- lm(richness ~ effort + Bvol + Vvol + area_mn + np + shdi, data = shp)
vif(verif)

#### Verificações e Ajustes ####
Env <- amb %>% select(-id)
Occ <- pa
Occ <- as.matrix(Occ)
SP <- coord %>% select(-id)

# Remover variáveis constantes do Env e SP
Env <- Env[, sapply(Env, function(x) length(unique(x)) > 1)]
SP <- SP[, sapply(SP, function(x) length(unique(x)) > 1)]

# Verificar e remover coordenadas duplicadas
if (sum(duplicated(SP)) > 0) {
  SP <- SP[!duplicated(SP), ]
}

# Garantir a remoção completa de NAs
Env[is.na(Env)] <- 0
SP[is.na(SP)] <- 0

# Remover colunas com variância zero
Env <- Env[, apply(Env, 2, function(x) var(x, na.rm = TRUE) != 0)]
SP <- SP[, apply(SP, 2, function(x) var(x, na.rm = TRUE) != 0)]

#### sjSDM model ####
model <- sjSDM(
  Y = Occ,
  env = linear(data = Env, formula = ~ effort + Bvol + Vvol + area_mn + np + shdi),
  spatial = linear(data = SP, formula = ~0 + coord_x:coord_y),
  se = TRUE,
  family = binomial("probit"),
  sampling = 419L
)

summary(model)

# Model
plot(model)
