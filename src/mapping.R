# Gráfico estratificação municípios 

library(sf)          # Manipulação de dados geoespaciais
library(ggplot2)     # Visualização
library(geobr)       # Obter shapes oficiais do Brasil
library(dplyr)       # Manipulação de dados
library(lwgeom)      # Operações geométricas (simplificação de polígonos)
library(viridis)     # Escalas de cores amigáveis
library(ggspatial)
library(Cairo)

setores_pr <- st_read("PR_setores_CD2022.gpkg") #23899 linhas



mapa <- setores %>% 
  left_join(setores_pr, by = "cd_setor") %>% 
  select(cd_setor, codigo_upa, situacao.x, est_geo1, est_geo2, est_geo3, var_estrat_imp, nm_mun.y, cd_mun.y, geom )
mapa$est23 <- interaction(mapa$est_geo2, mapa$est_geo3)
mapa$est12 <- interaction(mapa$est_geo2, mapa$situacao.x)
mapa <- st_as_sf(mapa)
mapa <- st_set_crs(mapa, 4674)  



#Fazer o Mapa bonito 
CairoPNG(filename = "mapa_estratificacao1.png", 
         width = 3000, height = 2000, res = 300) 
# Mapa Estratificação 
mapa_1 <- ggplot(mapa) +
  geom_sf(aes(geometry = geom, fill = est12), color = NA) +
  scale_fill_manual(
    values = c(
      "Entorno de RM de capital.Rural" = "#386641",
      "Interior.Rural" = "#8ecae6",
      "Capital.Urbana" = "#e63946",
      "Entorno de RM de capital.Urbana" = "#e9c46a",
      "Interior.Urbana" =  "#003049"
    ),
    guide = guide_legend(nrow = 2)
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(
    fill = "Estratificação por tipo de município\n e situação dos domicílios"
    #    title = "Mapa de Estratificação Geográfica",
    #    subtitle = "Por setores censitários"
  ) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +
  annotate("text", 
           x = Inf, y = -Inf, 
           label = "Projeção Policônica, Datum: SIRGAS2000",
           hjust = 1.1, vjust = -3, 
           size = 3.5)
mapa_1
dev.off()

# Mapa estratificação


CairoPNG(filename = "mapa_estratificacao2.png", 
         width = 3000, height = 2000, res = 300) 
# Mapa Estratificação 
mapa_2 <- ggplot(mapa) +
  geom_sf(aes(geometry = geom, fill = est23), color = NA) +
  scale_fill_manual(
    values = c(
      "Entorno de RM de capital.Rural" = "#386641",
      "Interior.Rural" = "#8ecae6",
      "Capital.Urbano-favela e comunidade" = "#e63946",
      "Entorno de RM de capital.Urbano-favela e comunidade" = "#e9c46a",
      "Interior.Urbano-favela e comunidade" =  "#003049",
      "Capital.Urbano-não especial" =  "#ff99c8",
      "Entorno de RM de capital.Urbano-não especial" = "#6a4c93",
      "Interior.Urbano-não especial" = "#220901"
    ),
    guide = guide_legend(nrow = 2)
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(
    fill = "Estratificação por tipo de município\n e situação dos domicílios"
    #    title = "Mapa de Estratificação Geográfica",
    #    subtitle = "Por setores censitários"
  ) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +
  annotate("text", 
           x = Inf, y = -Inf, 
           label = "Projeção Policônica, Datum: SIRGAS2000",
           hjust = 1.1, vjust = -3, 
           size = 3.5)
mapa_2
dev.off()

# Mapa municipio 


CairoPNG(filename = "mapa_municipios.png", 
         width = 3000, height = 2000, res = 300) 
# Mapa Estratificação 
mapa_3 <- ggplot(mapa) +
  geom_sf(aes(geometry = geom, fill = est_geo2), color = NA) +
  scale_fill_manual(
    values = c(
      "Interior" = "#8ecae6",
      "Capital" = "#e63946",
      "Entorno de RM de capital" = "#6a4c93"
    ),
    guide = guide_legend(nrow = 2)
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(
    fill = "Estratificação por tipo de município"
    #    title = "Mapa de Estratificação Geográfica",
    #    subtitle = "Por setores censitários"
  ) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +
  annotate("text", 
           x = Inf, y = -Inf, 
           label = "Projeção Policônica, Datum: SIRGAS2000",
           hjust = 1.1, vjust = -3, 
           size = 3.5)
mapa_3
dev.off()

table(mapa$est23)
##############

pol1 = cadastro_mestre[425,]
pol2 = cadastro_mestre[90,]
pol3 = cadastro_mestre[2490,]
pol4 = cadastro_mestre[4890,]


# # Fazendo o plot da geometria
# plot(pol1$geom, main = paste("UPA:", pol1$codigo_upa))
# plot(pol1$centroide, add = TRUE, col = "red", pch = 16)
# text(
#   x = pol1$coord_x, 
#   y = pol1$coord_y, 
#   labels = paste0("(", round(pol1$coord_x, 5), ", ", round(pol1$coord_x, 5), ")"),
#   pos = 3,  # posição acima do ponto
#   cex = 0.8,
#   col = "blue"
# )
# Configura o layout da janela gráfica: 2 linhas, 2 colunas
CairoPNG(filename = "UPAs_centroide.png",  width = 6, height = 5, units = "in", dpi = 300)
par(mfrow = c(2, 2))
# Função auxiliar para plotar um polígono com centróide e coordenadas
plot_pol <- function(pol) {
  plot(pol$geom, main = paste("UPA", pol$codigo_upa))
  plot(pol$centroide, add = TRUE, col = "red", pch = 16)
  
  coords <- sf::st_coordinates(pol$centroide)
  
  text(
    x = pol$coord_x,
    y = pol$coord_y,
    labels = paste0("(", round( pol$coord_x, 5), ", ", round( pol$coord_y, 5), ")"),
    pos = 3,
    cex = 0.8,
    col = "blue"
  )
}

# Aplica a função para cada polígono
plot_pol(pol1)
plot_pol(pol2)
plot_pol(pol3)
plot_pol(pol4)
dev.off()

