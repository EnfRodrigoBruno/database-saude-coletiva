# ============================================================
# 12_indicadores_sinasc.R — Indicadores materno-infantis
# ============================================================
# Gera:
#   - Taxa de natalidade
#   - Perfil materno (idade, escolaridade, raça)
#   - Perfil do recém-nascido (peso, prematuridade, Apgar)
#   - Mortalidade infantil (cruzando SINASC + SIM)
#   - Mortalidade materna
# ============================================================

if (!exists("raiz")) raiz <- "/home/inominado/Documentos/BancoEpidemio"
source(file.path(raiz, "scripts", "00_setup.R"))
source(file.path(raiz, "scripts", "01_utils.R"))

# ------------------------------------------------------------
# 1. Carregar dados
# ------------------------------------------------------------
cat("Carregando dados...\n")

nasc     <- read_parquet(file.path(dir_tratado, "fato_nascimentos.parquet"))
obitos   <- read_parquet(file.path(dir_tratado, "fato_obitos.parquet"))
pop      <- read_parquet(file.path(dir_bruto_pop, "populacao_araruama_1996_2024.parquet"))

cat("Nascimentos:", nrow(nasc), "\n")
cat("Óbitos:", nrow(obitos), "\n")

# ------------------------------------------------------------
# 2. Taxa de natalidade (por 1.000 hab)
# ------------------------------------------------------------
natalidade <- nasc %>%
  count(ano_nasc, name = "nascidos") %>%
  filter(!is.na(ano_nasc)) %>%
  left_join(pop, by = c("ano_nasc" = "year")) %>%
  mutate(taxa_1000 = nascidos / pop * 1000) %>%
  arrange(ano_nasc)

cat("\n--- Taxa de natalidade (por 1.000 hab) ---\n")
print(natalidade, n = 30)

write_parquet(natalidade,
              file.path(dir_analytics, "ind_natalidade.parquet"))

# ------------------------------------------------------------
# 3. Perfil materno
# ------------------------------------------------------------

# Idade da mãe (por década)
mae_idade_decada <- nasc %>%
  mutate(decada = case_when(
    ano_nasc < 2005 ~ "1996-2004",
    ano_nasc < 2015 ~ "2005-2014",
    TRUE            ~ "2015-2024"
  )) %>%
  count(decada, faixa_etaria_mae, name = "n") %>%
  group_by(decada) %>%
  mutate(prop = n / sum(n) * 100) %>%
  ungroup()

cat("\n--- Idade da mãe por década (%) ---\n")
print(mae_idade_decada, n = 30)

write_parquet(mae_idade_decada,
              file.path(dir_analytics, "ind_sinasc_idade_mae_decada.parquet"))

# Gravidez na adolescência por ano
gravidez_adolesc <- nasc %>%
  mutate(adolescente = !is.na(idade_mae) & idade_mae < 20) %>%
  group_by(ano_nasc) %>%
  summarise(
    total     = n(),
    adolescentes = sum(adolescente, na.rm = TRUE),
    prop_adolescente = adolescentes / total * 100,
    .groups = "drop"
  ) %>%
  arrange(ano_nasc)

write_parquet(gravidez_adolesc,
              file.path(dir_analytics, "ind_sinasc_gravidez_adolesc.parquet"))

cat("\n--- Gravidez na adolescência (%) ---\n")
print(gravidez_adolesc, n = 30)

# ------------------------------------------------------------
# 4. Perfil do recém-nascido
# ------------------------------------------------------------

# Peso ao nascer por ano
peso_por_ano <- nasc %>%
  group_by(ano_nasc) %>%
  summarise(
    total       = n(),
    baixo_peso  = sum(baixo_peso, na.rm = TRUE),
    prop_baixo_peso = baixo_peso / total * 100,
    peso_medio  = mean(peso_g, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ano_nasc)

write_parquet(peso_por_ano,
              file.path(dir_analytics, "ind_sinasc_peso_por_ano.parquet"))

cat("\n--- Baixo peso ao nascer (%) por ano ---\n")
print(peso_por_ano, n = 30)

# Prematuridade por ano
premat_por_ano <- nasc %>%
  group_by(ano_nasc) %>%
  summarise(
    total            = n(),
    prematuros       = sum(prematuro, na.rm = TRUE),
    prop_prematuros  = prematuros / total * 100,
    .groups = "drop"
  ) %>%
  arrange(ano_nasc)

write_parquet(premat_por_ano,
              file.path(dir_analytics, "ind_sinasc_prematuridade_por_ano.parquet"))

# Tipo de parto por ano
parto_por_ano <- nasc %>%
  filter(tipo_parto %in% c("Vaginal", "Cesáreo")) %>%
  count(ano_nasc, tipo_parto, name = "n") %>%
  group_by(ano_nasc) %>%
  mutate(prop = n / sum(n) * 100) %>%
  ungroup() %>%
  arrange(ano_nasc)

write_parquet(parto_por_ano,
              file.path(dir_analytics, "ind_sinasc_parto_por_ano.parquet"))

cat("\n--- Cesáreo (%) por ano ---\n")
print(parto_por_ano %>% filter(tipo_parto == "Cesáreo") %>% select(ano_nasc, prop), n = 30)

# Apgar por ano
apgar_por_ano <- nasc %>%
  group_by(ano_nasc) %>%
  summarise(
    total        = n(),
    apgar5_baixo = sum(apgar5_baixo, na.rm = TRUE),
    prop_apgar_baixo = apgar5_baixo / total * 100,
    .groups = "drop"
  ) %>%
  arrange(ano_nasc)

write_parquet(apgar_por_ano,
              file.path(dir_analytics, "ind_sinasc_apgar_por_ano.parquet"))

# ------------------------------------------------------------
# 5. Mortalidade infantil
# ------------------------------------------------------------
# Óbitos de <1 ano (usando o decodificador do SIM)
# A coluna `idade` no SIM usa códigos: <1 ano = 000-499 (horas/dias/meses/semanas)
obitos_infantis <- obitos %>%
  mutate(
    idade_anos_dec = decodificar_idade_sim(idade),
    eh_infantil     = !is.na(idade_anos_dec) & idade_anos_dec < 1
  ) %>%
  filter(eh_infantil)

cat("\nÓbitos infantis totais:", nrow(obitos_infantis), "\n")

# Componentes: neonatal precoce (0-6 dias), neonatal tardio (7-27d), pós-neonatal (28d-11m)
obitos_infantis <- obitos_infantis %>%
  mutate(
    dias_vida = suppressWarnings(as.integer(idade)) - 100,   # dias (código 100-199)
    componente = case_when(
      # Horas (<100) = neonatal precoce
      suppressWarnings(as.integer(idade)) < 100 ~ "Neonatal precoce (0-6 dias)",
      # Dias (100-199)
      suppressWarnings(as.integer(idade)) >= 100 &
      suppressWarnings(as.integer(idade)) < 200 &
      (suppressWarnings(as.integer(idade)) - 100) <= 6 ~ "Neonatal precoce (0-6 dias)",
      suppressWarnings(as.integer(idade)) >= 100 &
      suppressWarnings(as.integer(idade)) < 200 &
      (suppressWarnings(as.integer(idade)) - 100) <= 27 ~ "Neonatal tardio (7-27 dias)",
      suppressWarnings(as.integer(idade)) >= 100 &
      suppressWarnings(as.integer(idade)) < 200 ~ "Pós-neonatal (28d-11m)",
      # Meses (200-299) = pós-neonatal (1-11 meses)
      suppressWarnings(as.integer(idade)) >= 200 &
      suppressWarnings(as.integer(idade)) < 300 ~ "Pós-neonatal (28d-11m)",
      # Semanas (300-399) = neonatal
      suppressWarnings(as.integer(idade)) >= 300 &
      suppressWarnings(as.integer(idade)) < 400 ~ "Neonatal precoce (0-6 dias)",
      # Anos = 400 (menos de 1 ano)
      suppressWarnings(as.integer(idade)) == 400 ~ "Pós-neonatal (28d-11m)",
      TRUE ~ "Ignorado"
    )
  )

# Taxa de mortalidade infantil por ano
mort_infantil <- obitos_infantis %>%
  count(ano_obito, name = "obitos_infantis") %>%
  filter(!is.na(ano_obito)) %>%
  left_join(natalidade %>% select(ano_nasc, nascidos),
            by = c("ano_obito" = "ano_nasc")) %>%
  mutate(taxa_1000 = obitos_infantis / nascidos * 1000) %>%
  arrange(ano_obito)

cat("\n--- Mortalidade infantil (por 1.000 NV) ---\n")
print(mort_infantil, n = 30)

write_parquet(mort_infantil,
              file.path(dir_analytics, "ind_mortalidade_infantil.parquet"))

# Componentes por ano
mort_infantil_comp <- obitos_infantis %>%
  count(ano_obito, componente, name = "n") %>%
  left_join(natalidade %>% select(ano_nasc, nascidos),
            by = c("ano_obito" = "ano_nasc")) %>%
  mutate(taxa_1000 = n / nascidos * 1000) %>%
  arrange(ano_obito, componente)

write_parquet(mort_infantil_comp,
              file.path(dir_analytics, "ind_mortalidade_infantil_componentes.parquet"))

# ------------------------------------------------------------
# 6. Mortalidade materna
# ------------------------------------------------------------
# Óbitos maternos: capítulo XV do CID-10 (O00-O99)
obitos_maternos <- obitos %>%
  mutate(
    cid3 = normalizar_cid3(causabas),
    eh_materno = substr(cid3, 1, 1) == "O",
    idade_faixa = decodificar_idade_sim(idade)
  ) %>%
  filter(eh_materno, !is.na(idade_faixa),
         idade_faixa >= 10, idade_faixa <= 49)

cat("\nÓbitos maternos totais:", nrow(obitos_maternos), "\n")

mort_materna <- obitos_maternos %>%
  count(ano_obito, name = "obitos_maternos") %>%
  filter(!is.na(ano_obito)) %>%
  left_join(natalidade %>% select(ano_nasc, nascidos),
            by = c("ano_obito" = "ano_nasc")) %>%
  mutate(razao_100k = obitos_maternos / nascidos * 100000) %>%
  arrange(ano_obito)

cat("\n--- Razão de mortalidade materna (por 100 mil NV) ---\n")
print(mort_materna, n = 30)

write_parquet(mort_materna,
              file.path(dir_analytics, "ind_mortalidade_materna.parquet"))

# ------------------------------------------------------------
# 7. Gráficos
# ------------------------------------------------------------

# Gráfico 1: Taxa de natalidade
g_natal <- ggplot(natalidade, aes(ano_nasc, taxa_1000)) +
  geom_line(color = "#2E7EC0", linewidth = 1) +
  geom_point(color = "#2E7EC0", size = 2) +
  labs(title = "Taxa de natalidade — Araruama (RJ)",
       subtitle = "Nascidos vivos por 1.000 habitantes • Fonte: SINASC e IBGE",
       x = "Ano", y = "Nascidos por 1.000 hab.") +
  theme_minimal(base_size = 13)

ggsave(file.path(dir_analytics, "grafico_natalidade.png"),
       g_natal, width = 11, height = 5, dpi = 120)

# Gráfico 2: Mortalidade infantil
g_mort_inf <- ggplot(mort_infantil, aes(ano_obito, taxa_1000)) +
  geom_line(color = "#C0392B", linewidth = 1) +
  geom_point(color = "#C0392B", size = 2) +
  geom_hline(yintercept = 12, linetype = "dashed", color = "#F5A623", alpha = 0.7) +
  annotate("text", x = min(mort_infantil$ano_obito),
           y = 12.5, hjust = 0, size = 3,
           label = "Referência nacional ~12/1.000") +
  labs(title = "Mortalidade infantil — Araruama (RJ)",
       subtitle = "Óbitos em <1 ano por 1.000 nascidos vivos • Fonte: SIM e SINASC",
       x = "Ano", y = "Óbitos por 1.000 NV") +
  theme_minimal(base_size = 13)

ggsave(file.path(dir_analytics, "grafico_mortalidade_infantil.png"),
       g_mort_inf, width = 11, height = 5, dpi = 120)

# Gráfico 3: Cesárea por ano
g_cesarea <- parto_por_ano %>%
  filter(tipo_parto == "Cesáreo") %>%
  ggplot(aes(ano_nasc, prop)) +
  geom_col(fill = "#8E44AD", alpha = 0.85) +
  geom_hline(yintercept = 15, linetype = "dashed", color = "#F5A623") +
  annotate("text", x = min(parto_por_ano$ano_nasc),
           y = 16, hjust = 0, size = 3,
           label = "Meta OMS ~15%") +
  labs(title = "Proporção de partos cesáreos — Araruama (RJ)",
       subtitle = "Fonte: SINASC",
       x = "Ano", y = "% de cesáreas") +
  theme_minimal(base_size = 13)

ggsave(file.path(dir_analytics, "grafico_cesarea.png"),
       g_cesarea, width = 11, height = 5, dpi = 120)

# Gráfico 4: Gravidez na adolescência
g_adolesc <- ggplot(gravidez_adolesc, aes(ano_nasc, prop_adolescente)) +
  geom_line(color = "#E91E63", linewidth = 1) +
  geom_point(color = "#E91E63", size = 2) +
  labs(title = "Gravidez na adolescência — Araruama (RJ)",
       subtitle = "% de nascidos vivos de mães com menos de 20 anos • Fonte: SINASC",
       x = "Ano", y = "% de nascimentos") +
  theme_minimal(base_size = 13)

ggsave(file.path(dir_analytics, "grafico_gravidez_adolesc.png"),
       g_adolesc, width = 11, height = 5, dpi = 120)

# Gráfico 5: Baixo peso e prematuridade
g_peso_prem <- peso_por_ano %>%
  select(ano_nasc, prop_baixo_peso) %>%
  left_join(premat_por_ano %>% select(ano_nasc, prop_prematuros),
            by = "ano_nasc") %>%
  pivot_longer(-ano_nasc, names_to = "indicador", values_to = "prop") %>%
  mutate(indicador = recode(indicador,
                            "prop_baixo_peso"  = "Baixo peso (<2500g)",
                            "prop_prematuros"  = "Prematuros (<37sem)")) %>%
  ggplot(aes(ano_nasc, prop, color = indicador)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Baixo peso (<2500g)" = "#C0392B",
                                 "Prematuros (<37sem)" = "#2E7EC0")) +
  labs(title = "Baixo peso e prematuridade — Araruama (RJ)",
       subtitle = "Fonte: SINASC",
       x = "Ano", y = "% dos nascidos vivos", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(dir_analytics, "grafico_peso_prematuridade.png"),
       g_peso_prem, width = 11, height = 5, dpi = 120)

# ------------------------------------------------------------
# 8. Sumário final
# ------------------------------------------------------------
cat("\n✅ Indicadores materno-infantis gerados:\n")
cat("  Parquets:\n")
cat("    - ind_natalidade.parquet\n")
cat("    - ind_sinasc_idade_mae_decada.parquet\n")
cat("    - ind_sinasc_gravidez_adolesc.parquet\n")
cat("    - ind_sinasc_peso_por_ano.parquet\n")
cat("    - ind_sinasc_prematuridade_por_ano.parquet\n")
cat("    - ind_sinasc_parto_por_ano.parquet\n")
cat("    - ind_sinasc_apgar_por_ano.parquet\n")
cat("    - ind_mortalidade_infantil.parquet\n")
cat("    - ind_mortalidade_infantil_componentes.parquet\n")
cat("    - ind_mortalidade_materna.parquet\n")
cat("  Gráficos:\n")
cat("    - grafico_natalidade.png\n")
cat("    - grafico_mortalidade_infantil.png\n")
cat("    - grafico_cesarea.png\n")
cat("    - grafico_gravidez_adolesc.png\n")
cat("    - grafico_peso_prematuridade.png\n")