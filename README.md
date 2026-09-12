# Integração de coordenadas geográficas na seleção de UPAs — Amostra Mestra do IBGE

Código e resultados da monografia de graduação em Estatística (ENCE/IBGE) que testa o uso de informação geográfica explícita na seleção de Unidades Primárias de Amostragem (UPAs) da Amostra Mestra, comparando o método atual com uma alternativa espacialmente balanceada.

**Pergunta de pesquisa:** incorporar coordenadas geográficas no plano amostral da Amostra Mestra traria ganho de eficiência?

## O que foi feito

1. **Construção do cadastro mestre.** Malha de setores censitários e agregados por setor do Censo 2022, restritos à Região Metropolitana de Curitiba. Depois dos filtros (massas d'água, quartéis, unidades prisionais, aldeias indígenas e demais setores especiais), sobraram 6.100 setores, agregados em **5.851 UPAs**, **1.303.003 domicílios** e **15 estratos finais**, com amostra de **385 UPAs** e 14 domicílios por UPA.
2. **Replicação do plano atual (PPT de Pareto)**, que é o usado na Amostra Mestra.
3. **Aplicação do PPT de Poisson Correlacionado Espacialmente (SCPS)** de Grafström (2012), que usa os centroides das UPAs para induzir dispersão espacial na amostra.
4. **Comparação de eficiência** entre os dois planos via variância teórica, EQM e coeficiente de variação, com 1.000 réplicas de simulação para 6 variáveis de saneamento.

## Os scripts

| Arquivo | O que faz |
|---|---|
| `src/scps_sampling.R` | Núcleo do trabalho. Imputa os dados faltantes por *hot deck* (VIM) dentro de domínios de município e estrato, agrega os totais dos setores para UPAs, calcula as probabilidades de inclusão `π_hi = m_h · N_hi / N_h`, extrai os centroides das UPAs com `sf` e roda `BalancedSampling::scps()` estrato a estrato, 1.000 vezes com semente fixa. Ao final estima a proporção por réplica e calcula EQM, viés, CV e o teste t para viés nulo. |
| `src/pareto_variance_estimation.R` | Mesma preparação de dados, mas calcula a **variância teórica** do estimador sob o plano conglomerado em dois estágios, decomposta em componente entre UPAs e componente dentro das UPAs. A função `var_cv_prop()` faz isso para cada variável de interesse e devolve `Var(p̂)` e `CV(p̂)`; a versão em `for` sobre os estratos serve de conferência. Saída final em tabela LaTeX via `kableExtra`. |
| `src/mapping.R` | Gera os mapas da estratificação (tipo de município × situação do domicílio, com e sem o recorte de favela/comunidade) com `ggplot2` + `sf`, e a figura com exemplos de UPAs e seus centroides. |

O SCPS não tem suporte nativo a estratificação, por isso a função é chamada separadamente em cada um dos 15 estratos. Também não tem estimador não viesado de variância — daí a necessidade das simulações repetidas.

## Resultados

Seis variáveis de saneamento, proporções na população da RM de Curitiba: rede geral de distribuição (93,5%), água encanada em casa (74,1%), esgoto ligado à rede (84,8%), fossa séptica não ligada à rede (7,6%), lixo coletado por serviço de limpeza (93,3%) e lixo queimado na propriedade (1,2%).

Coeficientes de variação estimados (K = 1.000 réplicas):

| Variável | Pareto (variância) | Pareto (simulação) | SCPS |
|---|---|---|---|
| Rede geral de distribuição | 0,7% | 0,7% | 0,7% |
| Água encanada em casa | 2,2% | 2,3% | **1,8%** |
| Esgoto ligado à rede | 1,1% | 1,1% | **1,0%** |
| Fossa séptica não ligada | 10,4% | 10,8% | **9,9%** |
| Lixo coletado por serviço | 0,9% | 0,9% | **0,8%** |
| Lixo queimado na propriedade | 26,9% | 28,1% | **24,1%** |

O SCPS produziu CVs menores ou iguais aos do Pareto em todas as variáveis, ou seja, há ganho de precisão sem aumentar o tamanho da amostra. Os ganhos são discretos e não foram testados quanto à significância estatística. Os CVs altos das duas últimas variáveis refletem proporções populacionais muito baixas, o que infla o CV por construção.

Do lado dos custos: o SCPS levou cerca de 3 minutos por variável, contra 2 min 10s do Pareto em loop e ~2 segundos na versão vetorizada. Somado à ausência de estimador não viesado da variância, isso coloca o método em desvantagem prática apesar do ganho de eficiência.

**Limitações e próximos passos:** o `BalancedSampling` só permite pesos maximais com distância euclidiana, o que pode não servir para regiões extensas. O balanceamento espacial em si não foi medido — o índice de Voronoi seria o caminho natural. Também ficam em aberto pesos gaussianos e distâncias geodésicas.

## Dados

Nenhum dado está versionado aqui. As fontes são públicas e as instruções de download estão em [`data/README.md`](data/README.md):

- Malha de setores censitários do Censo 2022 (GPKG, por UF)
- Agregados por setores censitários do Censo 2022 (CSV)

A agregação dos setores em UPAs (algoritmo HAGM, mínimo de 60 DPPOs por UPA e contiguidade) e a estratificação estatística foram feitas pelo professor José André de Moura Brito (ENCE) e fornecidas para este trabalho.

Os scripts esperam dois arquivos `.rds` no diretório de trabalho: `upas_estratificadas.rds` (cadastro mestre com geometria e estrato) e `setores_pr_3.rds` (setores com as variáveis de saneamento).

## Dependências

```r
install.packages(c("sf", "dplyr", "purrr", "VIM", "BalancedSampling",
                   "ggplot2", "geobr", "lwgeom", "viridis", "ggspatial",
                   "Cairo", "kableExtra"))
```

Desenvolvido em R no Positron.

## Estrutura

```
src/        scripts de seleção, estimação de variância e mapas
data/       instruções de download dos dados públicos do IBGE
results/    figuras (mapas de estratificação, fluxograma, centroides)
monografia/ texto final e apresentação de resultados
```

## Autoria

Antonia Xavier — ENCE, 2025.
Orientação: Alinne de Carvalho Veiga. Coorientação: Pedro Luis do Nascimento Silva.

Licença MIT.
