rm(list=ls()) # Limpa área de trabalho e Console

#  PACOTES ------------------------------------------------------
suppressPackageStartupMessages(suppressWarnings(library(BalancedSampling)))
suppressPackageStartupMessages(suppressWarnings(library(purrr)))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))
suppressPackageStartupMessages(suppressWarnings(library(VIM)))
suppressPackageStartupMessages(suppressWarnings(library(sf)))

# CARREGAR E PREPARAR DADOS ------------------------------------------
# Carregamento dos arquivos

cadastro_mestre <- readRDS("upas_estratificadas.rds") #
setores_censo2022 <- readRDS("setores_pr_3.rds") 

# Tratamento de valores faltantes na variável rede_distribuicao
vars_alvo <- c( "rede_distribuicao", "agua_encanada_casa", "rede_geral_esgoto", 
               "fossa_sem_ligacao",  "servico_limpeza", 
               "lixo_queimado")

setores_censo2022 <- setores_censo2022 %>%
  mutate(across(all_of(vars_alvo), ~ ifelse(.x == "X", NA, .x))) %>%
  mutate(across(all_of(vars_alvo), as.numeric)) %>%
  mutate(n_domic = as.numeric(n_domic))



sum(is.na(setores_censo2022$rede_distribuicao)) #verificar quantos dados precisa imputar

  # Imputação 
setores_censo2022 <- VIM::hotdeck(
    data = setores_censo2022,
    variable = vars_alvo,
    ord_var = c("n_domic"),
    domain_var = c("cd_mun", "est_geo2", "est_geo3"),
    imp_var = TRUE
  )
  
  sum(is.na(setores_censo2022$rede_distribuicao)) #se for 0 a imputação foi feita
  
  #corrige imputação 
  
  corrigir_imputacao <- function(dados, variaveis) {
    
    # Verifica e corrige cada variável
    for(var in variaveis) {
      
      # Verifica se imputação funcionou adequadamente
      verif <- dados |>
        filter(.data[[var]] > n_domic)
      
      if(nrow(verif) > 0) {
        cat( var, "tinha", nrow(verif), "valores inadequados \n")
      }
      
      # Trata valores imputados inadequados
      dados <- dados |>
        mutate(!!var := pmin(.data[[var]], n_domic))
    }
    
    return(dados)
  }
  
  
  setores_censo2022 <- corrigir_imputacao(setores_censo2022, vars_alvo)
  sum(is.na(setores_censo2022$rede_distribuicao)) #imputacao feita
  
  
  # Agregação por UPA (Y_hi = total de domicílios com rede por UPA)
  # totais_por_upa <- setores_censo2022 %>% 
  #   group_by(codigo_upa) %>% 
  #   summarize(Y1_hi = sum(rede_distribuicao),
  #             Y2_hi = sum(lixo_queimado), 
  #             Y3_hi = sum(n_criancas), 
  #             Y4_hi = sum(sem_agua_encanada),
  #             Y5_hi = sum(servico_limpeza))
  
  totais_por_upa <- setores_censo2022 %>%
    group_by(codigo_upa) %>%
    summarize(across(all_of(vars_alvo), ~ sum(.x, na.rm = TRUE), .names = "Y{col}_hi"))
  
  # CONSTRUÇÃO DO CADASTRO MESTRE FINAL ------------------------------------------
  # Junção com cadastro mestre
  cadastro_mestre <- cadastro_mestre |>
    left_join(totais_por_upa, 
              join_by(codigo_upa))
  
  nomes <- names(totais_por_upa)[-1]
  # Verifica se junção funcionou adequadamente
  verif2 <- cadastro_mestre %>%
    rowwise() %>%
    filter(any(c_across(all_of(nomes)) > n_domic)) %>%
    ungroup()
  
  # Trata valores imputados inadequados (nao teve nenhum)
  # cadastro_mestre <- cadastro_mestre |>
  #   mutate(Y1_hi = pmin(Y1_hi, n_domic), 
  #          Y2_hi = pmin(Y2_hi, n_domic),
  #          Y3_hi = pmin(Y3_hi, n_domic),
  #          Y4_hi = pmin(Y4_hi, n_domic),
  #          Y5_hi = pmin(Y5_hi, n_domic))
  
  # Padronização de nomes e criação de variáveis necessárias 
  cadastro_mestre <- cadastro_mestre %>%
    rename(h = estrato_final, N_hi = n_domic) %>%
    mutate(n_hi = 14)
  
  
  
  # Totais do denominador e numerador da proporção 
  N <- sum(cadastro_mestre$N_hi)
  
  Totais <- cadastro_mestre %>%
    summarize(across(all_of(nomes), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) %>%
    rename_with(~ gsub("_hi", "", .x))  # remove o "_hi" do nome da variável
  
  
  Totais
  
  
  # Proporção na população
  
prop = Totais/N
prop = prop * 100
prop
# CÁLCULO DE PARÂMETROS POPULACIONAIS --------------------------
# Estratos
H <- unique(cadastro_mestre$h)
  # Totais de UPAs na população e na amostra por estrato
UPAs_h <- cadastro_mestre %>% 
    group_by(h) %>% 
    summarize(M_h = n()) |>
    ungroup() |>
    mutate(m_h = case_when(
      h == 1.1 ~ 11, h == 2.1 ~ 25, h == 2.2 ~ 29, h == 2.3 ~ 26, h == 2.4 ~ 26,
      h == 2.5 ~ 27, h == 3.1 ~ 27, h == 4.1 ~ 43, h == 4.2 ~ 37, h == 4.3 ~ 43,
      h == 4.4 ~ 37, h == 4.5 ~ 37, h == 5.1 ~ 6, h == 5.2 ~ 6, h == 5.3 ~ 5
    ))
  # Total de USAs por estrato
Nh <- cadastro_mestre |>
    group_by(h) |>
    summarise(N_h = sum(N_hi)) |>
    ungroup()
  
  # Junção de informações por estrato
cadastro_mestre <- cadastro_mestre %>% 
    left_join(UPAs_h, join_by(h)) %>% 
    left_join(Nh, join_by(h)) %>% 
    mutate(total = "Todos") %>%
    # select(total, h, codigo_upa, Y1_hi, Y2_hi, Y3_hi, Y4_hi,Y5_hi, M_h, m_h, N_h, N_hi, n_hi) %>%
    select(-situacao, -cd_uf, -sigla_uf, -nm_uf, -cd_mun, -n_morador, -area, -var_est_upa, -estrato_geografico) %>% 
    arrange(total, h, codigo_upa)
names(cadastro_mestre)

  # Cálculo das probabilidades de inclusão
cadastro_mestre <- cadastro_mestre %>%
    mutate(pi_hi = m_h * (N_hi / N_h))
  # select(total, h, codigo_upa, Y1_hi, Y2_hi, Y3_hi, Y4_hi,Y5_hi, M_h, m_h, N_h, N_hi, n_hi) %>%

#Quantidade de amostras
start <- Sys.time()
qtd_amostras = 1000 

######### Amostra SCPS ############
st_crs(cadastro_mestre$geom)
str(cadastro_mestre)
# Extrai os centroides de cada polígono

cadastro_mestre <- cadastro_mestre %>%  mutate(
  centroide = st_centroid(geom)) %>% 
  mutate(
    coord_x = st_coordinates(centroide)[, 1],
    coord_y = st_coordinates(centroide)[, 2]
  )

names(totais_por_upa)
#lista com os totais por estrato 
Yh_hat = list()
upas_amostra = c()
resultado = list()
cadastro_mestre <-  cadastro_mestre %>% mutate(
  Y_hi = Yfossa_sem_ligacao_hi) 

# Loop para gerar as 10 amostras
# s representa a replicação
for (s in 1:qtd_amostras) { 
  semente_global = 123 + s
  set.seed(semente_global)
  
  # Vetores temporários com os resultados pro estrato
  totais = numeric(length(H))
  names(totais) = H
  upas_amostra_h = c()
  
  
  # Loop por cada estrato
  for (estrato in H) {
    h_df <- cadastro_mestre %>%
      filter(h == estrato)
    
    matriz_auxiliar_estrato = scale(cbind(h_df$coord_x, h_df$coord_y, h_df$pi_hi))
    pik_estrato = h_df$pi_hi
    
    scps_indices = scps(pik_estrato, matriz_auxiliar_estrato)
    
    Yh_hat_estrato = sum(h_df$Y_hi[scps_indices] / h_df$pi_hi[scps_indices])
    upas_amostra_h[[as.character(estrato)]] =  h_df$codigo_upa[scps_indices]
    
    # Armazena a estimativa Yh_hat na posição correta do vetor para esta amostra
    totais[[as.character(estrato)]] = Yh_hat_estrato
  }
  
  #salvando resultados por replicação
  
  #upas selecionadas na amostra
  
  # armazena o vetor de estimativas Yh_hat na lista de colunas para a matriz Yh
  resultado[[s]] = totais
}

#estimativas por estrato (linhas) por replicação (colunas)
Yh_hat = do.call(cbind, resultado)
Yh_hat

#soma as estimativas por estrato 
Y_hat = colSums(Yh_hat)


#Estimativas da proporção 


proporcao_scps =  Y_hat/N
proporcao_scps
Y = sum(cadastro_mestre$Y_hi)
p =Y/N

## EQM

eqm_prop = (1/qtd_amostras) * sum((proporcao_scps - p)^2)

#Vies
vies = (1/qtd_amostras) * sum((proporcao_scps - p))


# CV 
cv_p = 100 * sqrt(eqm_prop)/p


#Erro Padrao do vies 

EP_Vies = sqrt((1/(qtd_amostras*(qtd_amostras - 1))) * sum((proporcao_scps - p)^2))

#teste 
#h0: Vies  == 0, 
#h1: Vies != 0

t = vies/EP_Vies
p_valor <- 2 * pt(abs(t), df = 999, lower.tail = FALSE)
p_valor

Y
p
eqm_prop
vies
cv_p
end <- Sys.time()
end - start
 