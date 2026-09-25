# ============================================================
# 06_indicadores_causas.R — Causas de morte e APVP
# ============================================================
# Gera:
#   - Top 10 causas CID por ano (1996-2024)
#   - Top 10 causas do período completo
#   - Mortalidade por capítulo CID-10 por ano
#   - APVP (Anos Potenciais de Vida Perdidos) — limite 70 anos
#   - Gráficos correspondentes
# ============================================================

if (!exists("raiz")) raiz <- "/home/inominado/Documentos/BancoEpidemio"
source(file.path(raiz, "scripts", "00_setup.R"))
source(file.path(raiz, "scripts", "01_utils.R"))

# ------------------------------------------------------------
# 0. Dicionário CID (categorias + subcategorias como fallback)
# ------------------------------------------------------------
cat_cid <- cid10::cid_categorias %>%
  select(cat, descricao) %>%
  mutate(cid = toupper(trimws(as.character(cat)))) %>%
  select(cid, descricao)

sub_cid <- cid10::cid_subcat %>%
  select(cid, descricao) %>%
  mutate(cid = toupper(trimws(substr(as.character(cid), 1, 3)))) %>%
  select(cid, descricao)

dic_cid <- bind_rows(cat_cid, sub_cid) %>%
  distinct(cid, .keep_all = TRUE)

cat("Dicionário CID carregado:", nrow(dic_cid), "categorias\n")

# ------------------------------------------------------------
# 1. Carregar dados redistribuídos
# ------------------------------------------------------------
fato <- read_parquet(file.path(dir_tratado, "fato_obitos_redistribuido.parquet"))

cat("Registros agregados:", nrow(fato), "\n")
cat("Total de óbitos:", round(sum(fato$obitos), 2), "\n")

fato <- fato %>%
  left_join(dic_cid, by = c("cid3" = "cid")) %>%
  mutate(
    capitulo_nome = atribuir_capitulo_cid(cid3),
    causa_nome    = ifelse(is.na(descricao), cid3, descricao)
  )

# ------------------------------------------------------------
# 2. Top 10 causas — período completo
# ------------------------------------------------------------
top10_total <- fato %>%
  filter(!is.na(cid3), cid3 != "") %>%
  group_by(cid3, causa_nome) %>%
  summarise(obitos = sum(obitos), .groups = "drop") %>%
  slice_max(order_by = obitos, n = 10) %>%
  mutate(ranking = row_number()) %>%
  arrange(ranking)

cat("\n--- Top 10 causas (1996-2024) ---\n")
print(top10_total, n = 10)

write_parquet(top10_total,
              file.path(dir_analytics, "ind_top10_causas_total.parquet"))

# ------------------------------------------------------------
# 3. Top 10 causas por ano
# ------------------------------------------------------------
top10_por_ano <- fato %>%
  filter(!is.na(ano_obito), !is.na(cid3), cid3 != "") %>%
  group_by(ano_obito, cid3, causa_nome) %>%
  summarise(obitos = sum(obitos), .groups = "drop") %>%
  group_by(ano_obito) %>%
  slice_max(order_by = obitos, n = 10, with_ties = FALSE) %>%
  mutate(ranking = row_number()) %>%
  ungroup() %>%
  arrange(ano_obito, ranking)

write_parquet(top10_por_ano,
              file.path(dir_analytics, "ind_top10_causas_por_ano.parquet"))

# ------------------------------------------------------------
# 4. Mortalidade por capítulo CID-10
# ------------------------------------------------------------
mort_por_capitulo <- fato %>%
  filter(!is.na(ano_obito), !is.na(capitulo_nome)) %>%
  group_by(ano_obito, capitulo_nome) %>%
  summarise(obitos = sum(obitos), .groups = "drop")

write_parquet(mort_por_capitulo,
              file.path(dir_analytics, "ind_mortalidade_capitulo_cid.parquet"))

# ------------------------------------------------------------
# 5. APVP — Anos Potenciais de Vida Perdidos
# ------------------------------------------------------------
apvp_por_ano <- fato %>%
  mutate(idade_media = mapa_faixa_etaria(faixa_etaria)) %>%
  filter(!is.na(ano_obito), !is.na(idade_media),
         idade_media < LIMITE_APVP) %>%
  mutate(apvp = obitos * (LIMITE_APVP - idade_media)) %>%
  group_by(ano_obito) %>%
  summarise(
    obitos_prematuros = sum(obitos),
    apvp_total        = sum(apvp),
    .groups = "drop"
  ) %>%
  arrange(ano_obito)

cat("\n--- APVP por ano (primeiros 3) ---\n")
print(head(apvp_por_ano, 3))
cat("\n--- APVP por ano (últimos 3) ---\n")
print(tail(apvp_por_ano, 3))

write_parquet(apvp_por_ano,
              file.path(dir_analytics, "ind_apvp_por_ano.parquet"))

apvp_por_causa <- fato %>%
  mutate(idade_media = mapa_faixa_etaria(faixa_etaria)) %>%
  filter(!is.na(idade_media), idade_media < LIMITE_APVP,
         !is.na(cid3), cid3 != "") %>%
  mutate(apvp = obitos * (LIMITE_APVP - idade_media)) %>%
  group_by(cid3, causa_nome) %>%
  summarise(
    apvp_total = sum(apvp),
    obitos     = sum(obitos),
    .groups = "drop"
  ) %>%
  slice_max(order_by = apvp_total, n = 10) %>%
  mutate(ranking = row_number()) %>%
  arrange(ranking)

cat("\n--- Top 10 causas por APVP ---\n")
print(apvp_por_causa, n = 10)

write_parquet(apvp_por_causa,
              file.path(dir_analytics, "ind_apvp_top_causas.parquet"))

# ------------------------------------------------------------
# 6. Gráficos
# ------------------------------------------------------------

# Gráfico 1: Top 10 causas período completo
g_top10 <- top10_total %>%
  mutate(
    causa_wrap = str_wrap(causa_nome, width = 50),
    causa_wrap = reorder(causa_wrap, obitos)
  ) %>%
  ggplot(aes(obitos, causa_wrap)) +
  geom_col(fill = "#2a7ab9", alpha = 0.85) +
  labs(title = "Top 10 causas de morte — Araruama (RJ)",
       subtitle = "1996-2024 (após redistribuição) • Fonte: SIM/DATASUS",
       x = "Óbitos", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.margin = margin(5.5, 15, 5.5, 5.5, unit = "pt"),
    axis.text.y = element_text(size = 10)
  )

ggsave(file.path(dir_analytics, "grafico_top10_causas_total.png"),
       g_top10, width = 13, height = 7, dpi = 120)

# Gráfico 2: Top 5 causas ao longo do tempo
top5_cids <- top10_total %>% slice_head(n = 5) %>% pull(cid3)
serie_top5 <- top10_por_ano %>% filter(cid3 %in% top5_cids)

g_top5 <- ggplot(serie_top5, aes(ano_obito, obitos, color = causa_nome)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(title = "Evolução das 5 principais causas de morte — Araruama (RJ)",
       subtitle = "1996-2024 (após redistribuição) • Fonte: SIM/DATASUS",
       x = "Ano", y = "Óbitos", color = "Causa") +
  theme_minimal(base_size = 12) +
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(face = "bold"))

ggsave(file.path(dir_analytics, "grafico_top5_causas_serie.png"),
       g_top5, width = 13, height = 6, dpi = 120)

# Gráfico 3: Capítulos CID por ano
g_capitulos <- mort_por_capitulo %>%
  filter(ano_obito >= 2000) %>%
  ggplot(aes(ano_obito, obitos, fill = capitulo_nome)) +
  geom_area(alpha = 0.85) +
  labs(title = "Óbitos por capítulo CID-10 — Araruama (RJ)",
       subtitle = "2000-2024 (após redistribuição) • Fonte: SIM/DATASUS",
       x = "Ano", y = "Óbitos", fill = "Capítulo CID-10") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right",
        legend.text = element_text(size = 8))

ggsave(file.path(dir_analytics, "grafico_capitulos_cid.png"),
       g_capitulos, width = 13, height = 6, dpi = 120)

# Gráfico 4: APVP por ano
g_apvp <- ggplot(apvp_por_ano, aes(ano_obito, apvp_total)) +
  geom_col(fill = "#8e44ad", alpha = 0.8) +
  labs(title = "Anos Potenciais de Vida Perdidos (APVP) — Araruama (RJ)",
       subtitle = paste0("Óbitos antes dos ", LIMITE_APVP,
                         " anos (após redistribuição) • Fonte: SIM/DATASUS"),
       x = "Ano", y = "APVP (anos)") +
  theme_minimal(base_size = 12)

ggsave(file.path(dir_analytics, "grafico_apvp_serie.png"),
       g_apvp, width = 10, height = 5, dpi = 120)

# Gráfico 5: Top 10 causas por APVP
g_apvp_causa <- apvp_por_causa %>%
  mutate(
    causa_wrap = str_wrap(causa_nome, width = 55),
    causa_wrap = reorder(causa_wrap, apvp_total)
  ) %>%
  ggplot(aes(apvp_total, causa_wrap)) +
  geom_col(fill = "#c0392b", alpha = 0.85) +
  labs(title = "Top 10 causas por Anos Potenciais de Vida Perdidos",
       subtitle = paste0("Araruama (RJ), 1996-2024 • Limite: ",
                         LIMITE_APVP, " anos • Após redistribuição"),
       x = "APVP (anos)", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.margin = margin(5.5, 20, 5.5, 5.5, unit = "pt"),
    axis.text.y = element_text(size = 10)
  )

ggsave(file.path(dir_analytics, "grafico_apvp_top10.png"),
       g_apvp_causa, width = 13, height = 7, dpi = 120)

cat("\n✅ Indicadores e gráficos gerados.\n")