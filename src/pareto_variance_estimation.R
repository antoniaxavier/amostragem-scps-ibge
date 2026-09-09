cat("\014") ; 
rm(list=ls()) # Limpa área de trabalho e Console
#  PACOTES ------------------------------------------------------
suppressPackageStartupMessages(suppressWarnings(library(purrr)))
suppressPackageStartupMessages(suppressWarnings(library(dplyr)))
suppressPackageStartupMessages(suppressWarnings(library(VIM)))
suppressPackageStartupMessages(suppressWarnings(library(sf)))

# CARREGAR E PREPARAR DADOS ------------------------------------------
# Carregamento dos arquivos
cadastro_mestre <- readRDS("upas_estratificadas.rds") |>
  select(-geom)
setores_censo2022<- readRDS("setores_pr_3.rds")
names(setores_censo2022)
# Tratamento de valores faltantes na variável rede_distribuicao
vars_alvo <- c("rede_distribuicao",  
               "agua_encanada_casa", "rede_geral_esgoto", 
               "fossa_sem_ligacao", "servico_limpeza", 
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
  summarize(across(all_of(vars_alvo), ~ sum(.x, na.rm = TRUE), .names = "{col}_hi"))

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

prop * 100
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

# Cálculo das probabilidades de inclusão
cadastro_mestre <- cadastro_mestre %>%
  mutate(pi_hi = m_h * (N_hi / N_h))
start = Sys.time()
# Cálculo da variância sem usar 'for' -------------------------- 
# Calcula termos das parcelas da variância, UPA por UPA
# cadastro_mestre <- cadastro_mestre |>
#   mutate(p_hi = Y_hi / N_hi)
#   mutate(S2_hi = N_hi * p_hi * (1 - p_hi) / (N_hi - 1),
#          parcela1 = Y_hi^2 * (1 - pi_hi) / pi_hi,
#          parcela3 = N_hi^2 * ((1 /n_hi) - (1/N_hi)) * S2_hi) 
# 
# # Calcula resumos que precisamos por estrato
# resumos_por_estrato <- cadastro_mestre |>
#   group_by(total, h) |>
#   summarise(M_h = mean(M_h),
#             m_h = mean(m_h),
#             soma_parc1 = sum(parcela1),
#             num_parc2 = sum(Y_hi * (1 - pi_hi)),
#             den_parc2 = sum(pi_hi^2), 
#             soma_parc3 = sum(parcela3)) |>
#   ungroup() 
# 
# # Calcula parcelas da variância por estrato
# resumos_por_estrato <- resumos_por_estrato |>
#   mutate(soma_parc1 = M_h * soma_parc1 / (M_h - 1),
#          soma_parc2 = M_h * (num_parc2^2 / (m_h - den_parc2)) / (M_h - 1)) |>
#   mutate(var_y_hat_h = soma_parc1 - soma_parc2 + soma_parc3)
# 
# # Calcula variância do total no mumerador
# var_numerador <- resumos_por_estrato |>
#   group_by(total) |>
#   summarise(var_y_hat = sum(var_y_hat_h)) |>
#   ungroup()
# # Calcula variância da proporção estimada e seu CV
# var_prop <- var_numerador |>
#   mutate(var_prop = var_y_hat / N^2,
#          cv_prop = 100 * sqrt(var_prop) / p)
# 
# # Conferindo cv do estimador do total
# var_numerador <- var_numerador |>
#   mutate(cv_total = 100 * sqrt(var_y_hat) / Y)
# 
# print(var_prop)

# Cálculo da variância por função 


var_cv_prop <- function(Y_hi_col, p, Y) {
  
  cadastro_mestre_mod <- cadastro_mestre |>
    mutate(
      Y_hi = .data[[Y_hi_col]],
      p_hi = Y_hi / N_hi,
      S2_hi = N_hi * p_hi * (1 - p_hi) / (N_hi - 1),
      parcela1 = Y_hi^2 * (1 - pi_hi) / pi_hi,
      parcela3 = N_hi^2 * ((1 / n_hi) - (1 / N_hi)) * S2_hi
    )
  
  # Calcula resumos que precisamos por estrato
  resumos_por_estrato <- cadastro_mestre_mod |>
    group_by(total, h) |>
    summarise(M_h = mean(M_h),
              m_h = mean(m_h),
              soma_parc1 = sum(parcela1),
              num_parc2 = sum(Y_hi * (1 - pi_hi)),
              den_parc2 = sum(pi_hi^2), 
              soma_parc3 = sum(parcela3)) |>
    ungroup()   
  
  # Calcula parcelas da variância por estrato
resumos_por_estrato <- resumos_por_estrato |>
    mutate(soma_parc1 = M_h * soma_parc1 / (M_h - 1),
           soma_parc2 = M_h * (num_parc2^2 / (m_h - den_parc2)) / (M_h - 1)) |>
    mutate(var_y_hat_h = soma_parc1 - soma_parc2 + soma_parc3)
  
  # Calcula variância do total no mumerador
var_numerador <- resumos_por_estrato |>
    group_by(total) |>
    summarise(var_y_hat = sum(var_y_hat_h)) |>
    ungroup()
  # Calcula variância da proporção estimada e seu CV
  var_prop <- var_numerador |>
    mutate(var_prop =(var_y_hat / N^2),
           cv_prop = 100 * sqrt(var_prop) / p) 
  
  # Conferindo cv do estimador do total
  var_numerador <- var_numerador |>
    mutate(cv_total = 100 * sqrt(var_y_hat) / Y)
  
  return(var_prop)
}


names(Totais)
resultados <- map2(
  nomes,
  names(Totais),
  ~ var_cv_prop(.x, prop[[.y]], Totais[[.y]])
)
names(resultados) <- nomes  # nomeia a lista com os nomes do vetor `nomes`

resultados 
Totais
prop * 100

end = Sys.time()

end - start

# CÁLCULO DA VARIÂNCIA ---------------------------------------------------------
var_yh <-  c()


for (i in seq_along(H)) {
  h <- cadastro_mestre %>% filter(h == H[i])
  
  Mh <- max(h$M_h) #so tem um valor entao tanto faz
  m_h <- max(h$m_h) #so tem um valor entao tanto faz
  Nh <- sum(h$n_hi)
  
  
  # Primeiro termo: variância entre UPAs
  termo1 <- Mh * (
    sum(
      (h$Y_hi^2 / h$pi_hi) * (1 - h$pi_hi)
    ) -
      (sum(
        h$Y_hi * (1 - h$pi_hi))
      )^2 / (m_h - sum(h$pi_hi^2))
  )/(Mh - 1) 
  
  # Segundo termo: variância dentro das UPAs
  termo2 <- sum( h$N_hi^2 * (1 / h$n_hi - 1 / h$N_hi) * h$S2_hi
  )
  
  # Soma total
  var_yh[i] <- termo1 + termo2
}

var_yh

# Variância total e da proporção
var_y <- sum(var_yh)
var_p <- var_y / N^2
cv_p <- sqrt(var_p) / p

var_y
var_p
cv_p

var_prop

# RESULTADOS FINAIS ------------------------------------------------------------


tabelinha <- map_dfr(names(resultados), function(nome) {
  tibble(
    variavel = nome,
    var_prop = resultados[[nome]]$var_prop,
    cv_prop = resultados[[nome]]$cv_prop
  )
})

tabelinha <- map_dfr(names(resultados), function(nome_hi) {
  nome_base <- gsub("_hi$", "", nome_hi)  # remove "_hi"
  
  tibble(
    Variavel = nome_hi,
    Y = Totais[[nome_base]],
    p = prop[[nome_base]],
    var_prop = resultados[[nome_hi]]$var_prop,
    cv_prop = resultados[[nome_hi]]$cv_prop
  )
})
library(kableExtra)
# Exibir a tabela em LaTeX
kable(tabelinha, format = "latex", booktabs = TRUE, digits = 6,
      col.names = c("Variável", "Var(p)", "CV(p)"))
