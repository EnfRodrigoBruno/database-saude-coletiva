# ============================================================
# 05_indicadores_mortalidade.R — Taxa de mortalidade geral
# ============================================================

if (!exists("raiz")) raiz <- "/home/inominado/Documentos/BancoEpidemio"
source(file.path(raiz, "scripts", "00_setup.R"))
source(file.path(raiz, "scripts", "01_utils.R"))

fato_obitos <- read_parquet(file.path(dir_tratado, "fato_obitos.parquet"))
pop         <- read_parquet(file.path(dir_bruto_pop,
                                      "populacao_araruama_1996_2024.parquet"))

taxa_mortalidade <- fato_obitos %>%
  count(ano_obito, name = "obitos") %>%
  left_join(pop, by = c("ano_obito" = "year")) %>%
  mutate(taxa_1000 = obitos / pop * 1000)

write_parquet(taxa_mortalidade,
              file.path(dir_analytics, "ind_mortalidade_geral.parquet"))

# --- Gráfico 1: série absoluta ---
g1 <- ggplot(filter(taxa_mortalidade, !is.na(obitos)),
             aes(ano_obito, obitos)) +
  geom_line(color = "#2a7ab9", linewidth = 1) +
  geom_point(color = "#2a7ab9", size = 2) +
  labs(title = "Óbitos em Araruama (RJ) — 1996 a 2024",
       subtitle = "Fonte: SIM/DATASUS",
       x = "Ano", y = "Óbitos") +
  theme_minimal(base_size = 13)

ggsave(file.path(dir_analytics, "grafico_serie_obitos.png"),
       g1, width = 10, height = 5, dpi = 120)

# --- Gráfico 2: taxa ---
serie_taxa <- filter(taxa_mortalidade, !is.na(taxa_1000))

g2 <- ggplot(serie_taxa, aes(ano_obito, taxa_1000)) +
  geom_line(color = "#c0392b", linewidth = 1) +
  geom_point(color = "#c0392b", size = 2) +
  labs(title = "Taxa de mortalidade geral — Araruama (RJ)",
       subtitle = "Por 1.000 habitantes • Fonte: SIM e IBGE",
       x = "Ano", y = "Óbitos por 1.000 hab.") +
  theme_minimal(base_size = 13)

ggsave(file.path(dir_analytics, "grafico_taxa_mortalidade.png"),
       g2, width = 10, height = 5, dpi = 120)

# --- Gráfico 3: combinado ---
escala <- max(serie_taxa$obitos) / max(serie_taxa$taxa_1000)

g3 <- ggplot(serie_taxa, aes(ano_obito)) +
  geom_col(aes(y = obitos), fill = "#a8c8e8", alpha = 0.7) +
  geom_line(aes(y = taxa_1000 * escala), color = "#c0392b", linewidth = 1.2) +
  geom_point(aes(y = taxa_1000 * escala), color = "#c0392b", size = 2.5) +
  scale_y_continuous(
    name = "Óbitos (contagem)",
    sec.axis = sec_axis(~ . / escala, name = "Taxa por 1.000 hab.")
  ) +
  labs(title = "Mortalidade geral em Araruama (RJ) — 2000 a 2024",
       subtitle = "Barras: óbitos • Linha: taxa • Fonte: SIM e IBGE",
       x = "Ano") +
  theme_minimal(base_size = 13) +
  theme(axis.title.y.right = element_text(color = "#c0392b"),
        axis.text.y.right  = element_text(color = "#c0392b"))

ggsave(file.path(dir_analytics, "grafico_obitos_e_taxa.png"),
       g3, width = 10, height = 5, dpi = 120)

cat("Indicadores e gráficos gerados.\n")