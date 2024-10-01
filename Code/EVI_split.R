library(raster)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(purrr)
library(tibble)
library(patchwork) 

#### EVI do município ####
# Carregar o raster inicial
EVI <- raster("Z:/Bianca/eBird/Data/EVI/EVI_SP_final.tif")

# Calcular intensidade urbana (SP)
SP <- EVI * 1

# Criar um data frame a partir dos dados do raster, removendo NA
SP_values <- na.omit(as.data.frame(raster::values(SP), stringsAsFactors = FALSE))
colnames(SP_values) <- "green_intensity"

# Configurar paleta de cores
minhascores <- c('#d7191c', '#fdae61', '#ffffc0', '#a6d96a', '#77c35c', '#1a9641')
minha_color <- colorRampPalette(colors = minhascores, interpolate = 'spline', space = 'rgb')

# Criar o gráfico de frequência usando ggplot
ggplot(SP_values, aes(x = green_intensity)) +
  geom_histogram(aes(y = ..count..),  # Usar count para frequência absoluta
                 bins = 1000, 
                 fill = minha_color(1000)[1], 
                 color = "black", 
                 alpha = 0.7) +
  geom_vline(xintercept = 0.15, col = "#d7191c", lwd = 1, lty = 2) +  
  geom_vline(xintercept = 0.55, col = '#1a9641', lwd = 1, lty = 2) +  
  labs(x = "Green intensity", y = "Frequency") + 
  ggtitle("Green intensity in São Paulo") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  )

#### EVI do grid ####
# Função para processar cada raster
process_raster <- function(file) {
  grid <- raster(file)
  
  # Calcular proporções A, B, e C
  A <- sum(grid[grid <= 0.15 & grid >= 0], na.rm = TRUE) / sum(grid[grid >= 0], na.rm = TRUE)
  B <- sum(grid[grid >= 0.15 & grid <= 0.55], na.rm = TRUE) / sum(grid[grid >= 0], na.rm = TRUE)
  C <- sum(grid[grid >= 0.55], na.rm = TRUE) / sum(grid[grid >= 0], na.rm = TRUE)
  
  # Retornar os resultados como uma lista
  list(A = A, B = B, C = C)
}

# Função para gerar gráficos de frequência
create_histogram <- function(data, scale_name, color_palette) {
  ggplot(data, aes(x = green_intensity)) +
    geom_histogram(aes(y = ..count..), bins = 1000, 
                   fill = color_palette(1000)[1], color = "black", alpha = 0.7) +
    geom_vline(xintercept = 0.15, col = "#d7191c", lwd = 1, lty = 2) +  
    geom_vline(xintercept = 0.55, col = '#1a9641', lwd = 1, lty = 2) +  
    labs(x = NULL, y = NULL) + 
    theme_classic() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    ) +
    ggtitle(paste0("Scale: ", scale_name))
}

# Definir os diretórios das três escalas
directories <- c("Z:/Bianca/eBird/Data/EVI/EVI_sp_1km", 
                 "Z:/Bianca/eBird/Data/EVI/EVI_sp_4km", 
                 "Z:/Bianca/eBird/Data/EVI/EVI_sp_9km")

# Configurar paleta de cores
minhascores <- c('#d7191c', '#fdae61', '#ffffc0', '#a6d96a', '#77c35c', '#1a9641')
minha_color <- colorRampPalette(colors = minhascores, interpolate = 'spline', space = 'rgb')

# Função principal para processar rasters e gerar gráficos e CSVs
process_and_visualize <- function(directories) {
  for (dir in directories) {
    # Listar arquivos raster na pasta correspondente
    rastlist <- list.files(path = dir, pattern = '.tif$', full.names = TRUE)
    
    # Processar rasters
    sharing_mat <- rastlist %>%
      map(process_raster) %>%
      bind_rows() %>%
      mutate(nome = basename(rastlist),
             id = as.numeric(gsub(".tif$", "", nome)))
    
    # Nome da escala (última parte do caminho)
    scale_name <- basename(dir)
    
    # Salvar CSV na mesma pasta de onde vieram os rasters
    output_path <- dirname(rastlist[1])  # Usar o diretório do primeiro raster
    write_csv(sharing_mat, file.path(output_path, paste0("sharing_sparing_", scale_name, ".csv")))
    
    # Criar gráficos de frequência para cada raster
    for (i in seq_along(rastlist)) {
      grid <- raster(rastlist[i])
      grid_values <- na.omit(as.data.frame(raster::values(grid), stringsAsFactors = FALSE))
      colnames(grid_values) <- "green_intensity"
      
      # Gerar e salvar gráficos de frequência
      plot <- create_histogram(grid_values, scale_name, minha_color)
      ggsave(filename = paste0(output_path, "/histogram_", scale_name, "_", i, ".png"), plot = plot)
    }
  }
}


process_and_visualize(directories)

### EVI exemplo ####
# Diretório dos rasters
directory <- "Z:/Bianca/eBird/Data/EVI/EVI_exemplo"

# Listar arquivos raster na pasta
rastlist_1km <- list.files(path = directory, pattern = '1km.tif$', full.names = TRUE)
rastlist_4km <- list.files(path = directory, pattern = '4km.tif$', full.names = TRUE)
rastlist_9km <- list.files(path = directory, pattern = '9km.tif$', full.names = TRUE)

# Função para criar dataframe de intensidade de verde a partir de raster
process_raster <- function(file) {
  grid <- raster(file)
  SP_values <- na.omit(as.data.frame(raster::values(grid), stringsAsFactors = FALSE))
  colnames(SP_values) <- "green_intensity"
  SP_values$file <- basename(file)  # Adicionar nome do arquivo para identificação
  return(SP_values)
}

# Processar rasters para cada escala
data_1km <- rastlist_1km %>% map_df(process_raster)
data_4km <- rastlist_4km %>% map_df(process_raster)
data_9km <- rastlist_9km %>% map_df(process_raster)

# Função para criar histogramas com limites de eixos fixos
create_histogram_clean <- function(data, x_limits, y_limits) {
  ggplot(data, aes(x = green_intensity)) +
    geom_histogram(aes(y = ..count..), 
                   bins = 1000, 
                   fill = minha_color(1000)[1], 
                   color = "black", 
                   alpha = 0.7) +
    geom_vline(xintercept = 0.15, col = "red", lwd = 1, lty = 2) +
    geom_vline(xintercept = 0.55, col = "green", lwd = 1, lty = 2) +
    scale_x_continuous(limits = x_limits) +  # Definir limites x
    scale_y_continuous(limits = y_limits) +   # Definir limites y
    theme_classic() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_blank(),
      legend.position = "none"
    )
}

# Limites fixos para cada escala
limits_list <- list(
  km_1 = list(x = c(0, 1), y = c(0, 20)),
  km_4 = list(x = c(0, 1), y = c(0, 35)),
  km_9 = list(x = c(0, 1), y = c(0, 60))
)

# Criar histogramas e combinar para cada escala
hist_1km <- data_1km %>% 
  group_by(file) %>% 
  nest() %>% 
  mutate(plot = map(data, ~ create_histogram_clean(.x, limits_list$km_1$x, limits_list$km_1$y)))

hist_4km <- data_4km %>% 
  group_by(file) %>% 
  nest() %>% 
  mutate(plot = map(data, ~ create_histogram_clean(.x, limits_list$km_4$x, limits_list$km_4$y)))

hist_9km <- data_9km %>% 
  group_by(file) %>% 
  nest() %>% 
  mutate(plot = map(data, ~ create_histogram_clean(.x, limits_list$km_9$x, limits_list$km_9$y)))

# Combinar gráficos
combined_1km <- wrap_plots(hist_1km$plot) + plot_annotation(title = "1km² Rasters")
combined_4km <- wrap_plots(hist_4km$plot) + plot_annotation(title = "4km² Rasters")
combined_9km <- wrap_plots(hist_9km$plot) + plot_annotation(title = "9km² Rasters")

# Exibir gráficos
combined_1km
combined_4km
combined_9km
