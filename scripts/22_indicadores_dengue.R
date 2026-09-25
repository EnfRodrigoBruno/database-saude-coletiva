# ============================================================
# 22_indicadores_dengue.R — Indicadores de dengue em Araruama
# ============================================================

if (!exists("raiz")) raiz <- "/home/inominado/Documentos/BancoEpidemio"
source(file.path(raiz, "scripts", "00_setup.R"))
source(file.path(raiz, "scripts", "01_utils.R"))

# ------------------------------------------------------------
# 1. Carregar dados
# ------------------------------------------------------------
cat("Carregando dados...\n")

dengue <- read_parquet(file.path(dir_tratado, "fato_dengue.parquet"))
pop    <- read_parquet(file.path(dir_bruto_pop, "populacao_araruama_1996_2024.parquet"))

dengue_conf <- dengue %>% filter(confirmado)
cat("Registros confirmados:", nrow(dengue_conf), "\n")

# ------------------------------------------------------------
# 2. Série de incidência por ano (por 100.000 hab)
# ------------------------------------------------------------
incid_anual <- dengue_conf %>%
  count(ano_sintoma, name = "casos") %>%
  filter(!is.na(ano_sintoma)) %>%
  left_join(pop, by = c("ano_sintoma" = "year")) %>%
  mutate(taxa_100k = casos / pop * 100000) %>%
  arrange(ano_sintoma)

cat("\n--- Incidência anual (por 100 mil hab) ---\n")
print(incid_anual, n = 20)

write_parquet(incid_anual,
              file.path(dir_analytics, "ind_dengue_incidencia_anual.parquet"))

# ------------------------------------------------------------
# 3. Série por semana epidemiológica (2020-2023)
# ------------------------------------------------------------
incid_semanal <- dengue_conf %>%
  filter(!is.na(ano_sintoma), !is.na(sem_sintoma),
         ano_sintoma >= 2020) %>%
  count(ano_sintoma, sem_sintoma, name = "casos") %>%
  arrange(ano_sintoma, sem_sintoma)

write_parquet(incid_semanal,
              file.path(dir_analytics, "ind_dengue_incidencia_semanal.parquet"))

# ------------------------------------------------------------
# 4. Perfil demográfico
# ------------------------------------------------------------
por_sexo <- dengue_conf %>%
  count(sexo_label, name = "casos") %>%
  mutate(prop = casos / sum(casos) * 100)

por_faixa <- dengue_conf %>%
  filter(!is.na(faixa_etaria)) %>%
  count(faixa_etaria, name = "casos")

por_raca <- dengue_conf %>%
  count(raca_label, name = "casos")

cat("\n--- Perfil por sexo ---\n"); print(por_sexo)
cat("\n--- Perfil por faixa etária ---\n"); print(por_faixa)
cat("\n--- Perfil por raça/cor ---\n"); print(por_raca)

write_parquet(por_sexo,  file.path(dir_analytics, "ind_dengue_por_sexo.parquet"))
write_parquet(por_faixa, file.path(dir_analytics, "ind_dengue_por_faixa.parquet"))
write_parquet(por_raca,  file.path(dir_analytics, "ind_dengue_por_raca.parquet"))

# ------------------------------------------------------------
# 5. Sazonalidade (por mês) — a partir da data aproximada
# ------------------------------------------------------------
por_mes <- dengue_conf %>%
  filter(!is.na(dt_sin_aprox)) %>%
  mutate(mes_sintoma = as.integer(format(dt_sin_aprox, "%m"))) %>%
  count(mes_sintoma, name = "casos") %>%
  arrange(mes_sintoma)

cat("\n--- Sazonalidade por mês ---\n")
print(por_mes)

write_parquet(por_mes, file.path(dir_analytics, "ind_dengue_por_mes.parquet"))

# ------------------------------------------------------------
# 6. Indicadores de gravidade
# ------------------------------------------------------------
gravidade <- tibble(
  indicador = c("Dengue com sinais de alarme",
                "Dengue grave",
                "Hospitalizações (onde registrado)",
                "Óbitos por dengue"),
  n = c(sum(dengue_conf$classi_label == "Dengue com sinais de alarme", na.rm = TRUE),
        sum(dengue_conf$classi_label == "Dengue grave", na.rm = TRUE),
        sum(dengue_conf$hospitalizado, na.rm = TRUE),
        sum(dengue_conf$obito_dengue, na.rm = TRUE))
)

cat("\n--- Indicadores de gravidade ---\n")
print(gravidade)

write_parquet(gravidade,
              file.path(dir_analytics, "ind_dengue_gravidade.parquet"))

# ------------------------------------------------------------
# 7. Gráficos
# ------------------------------------------------------------

# Gráfico 1: Série de incidência anual
g_incid <- ggplot(incid_anual, aes(ano_sintoma, taxa_100k)) +
  geom_col(fill = "#2a7ab9", alpha = 0.85) +
  geom_text(aes(label = round(taxa_100k, 0)),
            vjust = -0.5, size = 3, color = "#0c3f6a") +
  labs(title = "Incidência de dengue em Araruama (RJ)",
       subtitle = "Casos confirmados por 100.000 habitantes • Fonte: SINAN/DATASUS",
       x = "Ano", y = "Casos por 100 mil hab.") +
  theme_minimal(base_size = 13)

ggsave(file.path(dir_analytics, "grafico_dengue_incidencia_anual.png"),
       g_incid, width = 11, height = 5, dpi = 120)

# Gráfico 2: Série semanal (2020-2023)
g_sem <- incid_semanal %>%
  mutate(ano_sem = paste0(ano_sintoma, " S", sprintf("%02d", sem_sintoma))) %>%
  ggplot(aes(reorder(ano_sem, as.integer(ano_sintoma) * 100 + sem_sintoma),
             casos)) +
  geom_col(fill = "#c0392b", alpha = 0.85) +
  labs(title = "Dengue por semana epidemiológica — Araruama (RJ)",
       subtitle = "2020 a 2023 • Fonte: SINAN/DATASUS",
       x = "Semana epidemiológica", y = "Casos confirmados") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 1, size = 7))

ggsave(file.path(dir_analytics, "grafico_dengue_semanal.png"),
       g_sem, width = 14, height = 6, dpi = 120)

# Gráfico 3: Faixa etária × sexo
g_faixa_sexo <- dengue_conf %>%
  filter(!is.na(faixa_etaria), sexo_label %in% c("Masculino", "Feminino")) %>%
  count(faixa_etaria, sexo_label, name = "casos") %>%
  ggplot(aes(faixa_etaria, casos, fill = sexo_label)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Masculino" = "#2E7EC0", "Feminino" = "#E91E63")) +
  labs(title = "Dengue por faixa etária e sexo — Araruama (RJ)",
       subtitle = "2007 a 2023 • Fonte: SINAN/DATASUS",
       x = "Faixa etária", y = "Casos confirmados", fill = "Sexo") +
  theme_minimal(base_size = 13)

ggsave(file.path(dir_analytics, "grafico_dengue_faixa_sexo.png"),
       g_faixa_sexo, width = 11, height = 5, dpi = 120)

# Gráfico 4: Sazonalidade (mês)
g_mes <- por_mes %>%
  mutate(mes_nome = factor(mes_sintoma,
                           levels = 1:12,
                           labels = c("Jan","Fev","Mar","Abr","Mai","Jun",
                                      "Jul","Ago","Set","Out","Nov","Dez"))) %>%
  ggplot(aes(mes_nome, casos)) +
  geom_col(fill = "#F5A623", alpha = 0.9) +
  labs(title = "Sazonalidade da dengue — Araruama (RJ)",
       subtitle = "Distribuição mensal acumulada (2007-2023) • Fonte: SINAN/DATASUS",
       x = "Mês", y = "Casos confirmados") +
  theme_minimal(base_size = 13)

ggsave(file.path(dir_analytics, "grafico_dengue_sazonalidade.png"),
       g_mes, width = 11, height = 5, dpi = 120)

# ------------------------------------------------------------
# 8. Sumário final
# ------------------------------------------------------------
cat("\n✅ Indicadores de dengue gerados:\n")
cat("  Parquets: 7 arquivos ind_dengue_*.parquet\n")
cat("  Gráficos: 4 arquivos grafico_dengue_*.png\n")