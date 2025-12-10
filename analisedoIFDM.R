library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(gt)
library(ggplot2)
library(forcats)
library(stringr)
library(knitr)
library(kableExtra)
library(ggridges)
library(viridis)
library(geobr)


Sys.setlocale("LC_ALL", "en_US.UTF-8")

##### Base #####
# Importando base de dados
IFDM_empregoerenda <- read_xlsx("C:/Users/dinoe/Downloads/IFDM/IFDMEmprego&Renda.xlsx")

# Verificando estrutura dos dados
str(IFDM_empregoerenda)

# Corrigindo Variáveis 
IFDM_empregoerenda <- IFDM_empregoerenda %>%
  mutate(across(starts_with("Ranking"), ~as.numeric(.)),
         across(starts_with("IFDM"), ~as.numeric(.)))
 
# renomeando colunas
IFDM_empregoerenda <- IFDM_empregoerenda %>%
  clean_names()

#### Estatísticas Decritivas ####

# mudando estrutura dos dados
ifdm_lgpt <- IFDM_empregoerenda %>%
  pivot_longer(
    cols = starts_with("ifdm_emprego_"),
    names_to = "ano",
    values_to = "ifdm"
  ) %>%
  mutate(ano = readr::parse_number(ano)) %>% 
  select(cod_munic,
         sigla_uf,
         nome_munic, 
         ano,
         ifdm
         )

# Estatísticas
tabela_estats <- ifdm_lgpt %>%
  group_by(ano) %>%
  summarise(
    media   = mean(ifdm, na.rm = TRUE),
    mediana = median(ifdm, na.rm = TRUE),
    sd      = sd(ifdm, na.rm = TRUE),
    minimo  = min(ifdm, na.rm = TRUE),
    maximo  = max(ifdm, na.rm = TRUE)
  )

# Tabela de visualização Estatísticas Descritivas 
tabela_descritiva <- tabela_estats %>%
  mutate(across(media:maximo, ~ round(.x, 3))) %>%   
  kable(
    format = "html",
    caption = "Estatísticas Descritivas do IFDM Emprego (2013–2023)",
    align = "c",
    col.names = c("Ano", "Média", "Mediana", "Desvio-padrão", "Mínimo", "Máximo")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center",
    font_size = 13
  ) %>%
  row_spec(0, bold = TRUE, background = "#E5E5E5") %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2:6, width = "10em")
tabela_descritiva


# Grafico distribuição para o IFDM 
grafico_distribuicao <- ggplot(ifdm_lgpt, aes(x = factor(ano), y = ifdm, fill = factor(ano))) +
  geom_violin(alpha = 0.9) +
  geom_boxplot(width = 0.12, fill = "white", alpha = 0.5, color = "black") +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  labs(
    title = "Distribuição do IFDM por Ano (2013–2023)",
    x = "Ano",
    y = "IFDM"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  )
grafico_distribuicao


#### Raking dos Melhores e Piores #####
### Top 10 melhores municípios por ano 
top10_melhores <- ifdm_lgpt %>%
  group_by(ano) %>%
  arrange(desc(ifdm)) %>%
  slice(1:10) %>%
  ungroup()

# Tabela Top 10 melhores
top10_melhores <- top10_melhores %>%
  group_by(ano) %>%
  mutate(Ranking = row_number()) %>%   
  ungroup() %>%
  select(Ranking, ano, nome_munic) %>%  
  pivot_wider(
    names_from = ano,
    values_from = nome_munic
  ) %>%
  arrange(Ranking)

# tabela para visualização 
tabela_top10melhores <- top10_melhores %>%
  kable(
    format = "html",
    caption = "Top 10 Municípios com Maior IFDM por Ano",
    align = "c",
    col.names = c("Ranking", names(top10_melhores)[-1])
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 13,
    position = "center"
  ) %>%
  column_spec(1, bold = TRUE, width = "5em", color = "black") %>%
  column_spec(2:ncol(top10_melhores), width = "12em") %>%
  row_spec(0, bold = TRUE, background = "#E5E5E5") %>%
  add_header_above(c(" " = 1, "Municípios por Ano" = ncol(top10_melhores)-1))
tabela_top10melhores


### Top 10 piores municípios por ano 
top10_piores <- ifdm_lgpt %>%
  group_by(ano) %>%
  arrange(ifdm) %>%
  slice(1:10) %>%
  ungroup()

# Tabela Top 10 piores
top10_piores <- top10_piores %>%
  group_by(ano) %>%
  mutate(Ranking = row_number()) %>%   
  ungroup() %>%
  select(Ranking, ano, nome_munic) %>%  
  pivot_wider(
    names_from = ano,
    values_from = nome_munic
  ) %>%
  arrange(Ranking)

# tabela para visualização 
tabela_top10piores <- top10_piores %>%
  kable(
    format = "html",
    caption = "Top 10 Municípios com Menor IFDM por Ano",
    align = "c",
    col.names = c("Ranking", names(top10_piores)[-1])
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 13,
    position = "center"
  ) %>%
  column_spec(1, bold = TRUE, width = "5em", color = "black") %>%
  column_spec(2:ncol(top10_piores), width = "12em") %>%
  row_spec(0, bold = TRUE, background = "#E5E5E5") %>%
  add_header_above(c(" " = 1, "Municípios por Ano" = ncol(top10_piores)-1))
tabela_top10piores


##### Evolução dos Municípios #####

# Calcular IFDM inicial e final
df_variacao <- ifdm_lgpt %>%
  filter(ano %in% c(2013, 2023)) %>%
  pivot_wider(
    names_from = ano,
    values_from = ifdm,
    names_prefix = "ano_"
  ) %>%
  mutate(variacao = ano_2023 - ano_2013)

# Top 10 Cresceram
top10_crescimento <- df_variacao %>%
  arrange(desc(variacao)) %>%
  slice(1:10) %>%
  mutate(variacao = round(variacao, 3))
# tabela 
tabela_crescimento <- top10_crescimento %>%
  select(nome_munic, sigla_uf, ano_2013, ano_2023, variacao) %>%
  kable(
    format = "html",
    caption = "Top 10 Municípios com Maior Crescimento no IFDM (2013–2023)",
    col.names = c("Município", "UF", "IFDM 2013", "IFDM 2023", "Variação"),
    align = "c"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE, background = "#E5E5E5") %>%
  column_spec(1, bold = TRUE, width = "15em") %>%
  column_spec(5, bold = TRUE)
tabela_crescimento

# Top 10 Cresceram
top10_queda <- df_variacao %>%
  arrange(variacao) %>%     
  slice(1:10) %>%
  mutate(variacao = round(variacao, 3))
# tabela 
tabela_queda <- top10_queda %>%
  select(nome_munic, sigla_uf, ano_2013, ano_2023, variacao) %>%
  kable(
    format = "html",
    caption = "Top 10 Municípios com Maior Queda no IFDM (2013–2023)",
    col.names = c("Município", "UF", "IFDM 2013", "IFDM 2023", "Variação"),
    align = "c"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE, background = "#E5E5E5") %>%
  column_spec(1, bold = TRUE, width = "15em") %>%
  column_spec(5, bold = TRUE, color = "red")
tabela_queda


##### Dispersão dos municipios IFDM 2013 x 2023 #####

df_disp <- ifdm_lgpt %>%
  filter(ano %in% c(2013, 2023)) %>%
  pivot_wider(
    names_from = ano,
    values_from = ifdm,
    names_prefix = "ano_"
  )

# Grafico
grafico_dispersao <- ggplot(df_disp, aes(x = ano_2013, y = ano_2023)) +
  geom_point(
    aes(color = ano_2023),
    alpha = 0.7,
    size = 2.2
  ) +
  scale_color_viridis(
    option = "plasma",
    name = "IFDM 2023"
  ) +
  geom_abline(
    intercept = 0, slope = 1,
    linetype = "dashed",
    color = "black",
    size = 0.8
  ) +
  labs(
    title = "Dispersão do IFDM: 2013 × 2023",
    x = "IFDM em 2013",
    y = "IFDM em 2023"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85")
  )
grafico_dispersao

##### Evolução da Média do Indice por Estado #####

media_estado <- ifdm_lgpt %>%
  group_by(sigla_uf, ano) %>%
  summarise(media_ifdm = mean(ifdm, na.rm = TRUE), .groups = "drop")

# grafico
grafico_evo <- ggplot(media_estado, aes(x = media_ifdm, y = fct_rev(fct_inorder(sigla_uf)), fill = sigla_uf)) +
  geom_density_ridges(
    scale = 1.4,
    alpha = 0.85,
    rel_min_height = 0.01,
    color = "black",      
    size = 0.4
  ) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "Distribuição da Média do IFDM por Estado (2013–2023)",
    x = "IFDM Médio",
    y = "Estados"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 12),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  )
grafico_evo

# Media do Periodo para os Estados
media_periodo_estados <- ifdm_lgpt %>%
  group_by(sigla_uf) %>%
  summarise(media_ifdm_periodo = mean(ifdm, na.rm = TRUE)) %>%
  arrange(desc(media_ifdm_periodo))
# Tabela
media_periodo_estados %>%
  mutate(media_ifdm_periodo = round(media_ifdm_periodo, 3)) %>%
  kable("html", col.names = c("UF", "IFDM Médio (2013–2023)"),
        align = "c",
        caption = "Média do IFDM de Emprego e Renda por Estado (2013–2023)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  column_spec(1, width = "10em", bold = TRUE) %>%
  column_spec(2, width = "20em", bold = TRUE)


##### Região #######

df_regiao <- ifdm_lgpt %>%
  mutate(
    regiao = case_when(
      sigla_uf %in% c("AC","AP","AM","PA","RO","RR","TO") ~ "Norte",
      sigla_uf %in% c("AL","BA","CE","MA","PB","PE","PI","RN","SE") ~ "Nordeste",
      sigla_uf %in% c("DF","GO","MT","MS") ~ "Centro-Oeste",
      sigla_uf %in% c("ES","MG","RJ","SP") ~ "Sudeste",
      sigla_uf %in% c("PR","RS","SC") ~ "Sul"
    )
  )
# grafico 
grafico_regi <- ggplot(df_regiao, aes(x = regiao, y = ifdm, fill = regiao)) +
  geom_boxplot(
    alpha = 0.9,
    color = "black",        
    size = 0.6,
    outlier.color = viridis::viridis(1, option = "plasma"), 
    outlier.size = 2.2
  ) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "Distribuição do IFDM por Região (2013–2023)",
    x = "Região",
    y = "IFDM"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "gray25"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_blank(),  
    panel.grid.major.y = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(15, 15, 15, 15)
  )
grafico_regi

##### Mapa Completo #####

# Carregar mapa municipal
mapa <- read_municipality(year = 2023) %>%
  mutate(
    code_muni_7 = as.character(code_muni),
    code_muni = substr(code_muni_7, 1, 6)
  )

# Preparar
df_map <- ifdm_lgpt %>%
  filter(ano %in% c(2013, 2023)) %>%
  select(ano, cod_munic, ifdm) %>%
  mutate(code_muni = as.character(cod_munic)) %>%
  select(-cod_munic)

# Join
mapa_ifdm <- mapa %>%
  left_join(df_map, by = "code_muni")

# Grafico 2013 
mapa_2013 <- ggplot(mapa_ifdm %>% filter(ano == 2013)) +
  geom_sf(aes(fill = ifdm), color = NA) +
  scale_fill_viridis(
    name = "IFDM 2013",
    option = "plasma",
    na.value = "grey90"
  ) +
  labs(
    title = "Mapa do IFDM (Emprego e Renda) – 2013",
    caption = "Fonte: Elaboração própria com dados do IBEGE / FIRJAN"
  ) +
  coord_sf(datum = NA, expand = FALSE) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_text(face = "bold")
  )
mapa_2013    

# Grafico 2023
mapa_2023 <- ggplot(mapa_ifdm %>% filter(ano == 2023)) +
  geom_sf(aes(fill = ifdm), color = NA) +
  scale_fill_viridis(
    name = "IFDM 2023",
    option = "plasma",
    na.value = "grey90"
  ) +
  labs(
    title = "Mapa do IFDM (Emprego e Renda) – 2023",
    caption = "Fonte: Elaboração própria com dados do IBEGE / FIRJAN"
  ) +
  coord_sf(datum = NA, expand = FALSE) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_text(face = "bold")
  )
mapa_2023








