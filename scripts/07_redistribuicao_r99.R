# ============================================================
# 07_redistribuicao_r99.R — Redistribuição de causas mal definidas
# Versão 3 — Lista conservadora (GBD Classes 1 e 2 restritas)
# ============================================================

if (!exists("raiz")) raiz <- "/home/inominado/Documentos/BancoEpidemio"
source(file.path(raiz, "scripts", "00_setup.R"))
source(file.path(raiz, "scripts", "01_utils.R"))

# ------------------------------------------------------------
# 1. Lista conservadora de causas mal definidas
# ------------------------------------------------------------
garbage_codes <- c(
  "R99", "R95", "P95", "B34", "X59", "V89"
)

# ------------------------------------------------------------
# 2. Carregar e preparar os dados
# ------------------------------------------------------------
fato <- read_parquet(file.path(dir_tratado, "fato_obitos.parquet"))

cat("Total de óbitos carregados:", nrow(fato), "\n")

fato <- fato %>%
  mutate(
    idade_anos = decodificar_idade_sim(idade),
    cid3       = normalizar_cid3(causabas),
    faixa_etaria = cut(idade_anos,
                       breaks = c(0, 1, 5, 15, 30, 45, 60, 70, Inf),
                       labels = c("0", "1-4", "5-14", "15-29",
                                  "30-44", "45-59", "60-69", "70+"),
                       right = FALSE),
    sexo = case_when(
      sexo == "1" ~ "Masculino",
      sexo == "2" ~ "Feminino",
      TRUE        ~ "Ignorado"
    )
  )

# ------------------------------------------------------------
# 3. Separar causas mal definidas e definidas
# ------------------------------------------------------------
mal_definidos <- fato %>% filter(cid3 %in% garbage_codes)
definidos     <- fato %>% filter(!cid3 %in% garbage_codes, !is.na(cid3), cid3 != "")

cat("Óbitos com causa definida:", nrow(definidos), "\n")
cat("Óbitos mal definidos (a redistribuir):", nrow(mal_definidos), "\n")
cat("Percentual mal definidos:",
    round(100 * nrow(mal_definidos) / nrow(fato), 2), "%\n")

# ------------------------------------------------------------
# 4. Calcular pesos de redistribuição
# ------------------------------------------------------------
pesos <- definidos %>%
  count(ano_obito, sexo, faixa_etaria, cid3, name = "peso") %>%
  group_by(ano_obito, sexo, faixa_etaria) %>%
  mutate(prop = peso / sum(peso)) %>%
  ungroup()

# ------------------------------------------------------------
# 5. Redistribuir proporcionalmente
# ------------------------------------------------------------
mal_definidos_base <- mal_definidos %>%
  select(ano_obito, sexo, faixa_etaria)

redistribuidos <- mal_definidos_base %>%
  left_join(pesos,
            by = c("ano_obito", "sexo", "faixa_etaria"),
            relationship = "many-to-many") %>%
  mutate(obitos = 1 * prop) %>%
  group_by(ano_obito, sexo, faixa_etaria, cid3) %>%
  summarise(obitos = sum(obitos, na.rm = TRUE), .groups = "drop")

# ------------------------------------------------------------
# 6. Juntar definidos + redistribuídos
# ------------------------------------------------------------
definidos_agregados <- definidos %>%
  count(ano_obito, sexo, faixa_etaria, cid3, name = "obitos")

fato_final <- bind_rows(definidos_agregados, redistribuidos) %>%
  group_by(ano_obito, sexo, faixa_etaria, cid3) %>%
  summarise(obitos = sum(obitos), .groups = "drop")

# ------------------------------------------------------------
# 7. Validação e salvamento
# ------------------------------------------------------------
total_original  <- nrow(fato)
total_redistrib <- sum(fato_final$obitos)

cat("\n--- Validação ---\n")
cat("Óbitos originais:          ", total_original, "\n")
cat("Óbitos após redistribuição:", round(total_redistrib, 2), "\n")

if (abs(total_original - total_redistrib) < 1) {
  cat("✅ Totais batem (diferença < 1 óbito).\n")
} else {
  cat("⚠️  Diferença:", round(total_original - total_redistrib, 2), "óbitos.\n")
}

write_parquet(fato_final,
              file.path(dir_tratado, "fato_obitos_redistribuido.parquet"))

cat("\n✅ Arquivo salvo em: tratado/fato_obitos_redistribuido.parquet\n")
cat("Colunas:", paste(names(fato_final), collapse = ", "), "\n")