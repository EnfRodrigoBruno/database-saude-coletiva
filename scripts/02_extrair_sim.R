# ============================================================
# 02_extrair_sim.R — Baixa SIM-DO para Araruama (1996–2024)
# ============================================================
# Salva um arquivo por ano em bruto/sim/sim_araruama_YYYY.parquet
# Usa datasus como padrão e microdatasus como fallback.
# ============================================================

if (!exists("raiz")) raiz <- "/home/inominado/Documentos/BancoEpidemio"
source(file.path(raiz, "scripts", "00_setup.R"))
source(file.path(raiz, "scripts", "01_utils.R"))

# --- Função: baixa um ano via datasus ---
processar_sim_datasus <- function(ano, uf = UF_PROJETO) {
  cat("=== SIM", ano, "(datasus) ===\n")
  dados <- sim_microdados(ano = ano, uf = uf, tipo = "DO")
  cat("  RJ:", nrow(dados), "registros\n")

  obitos <- dados %>%
    filter(normalizar_cod_municipio(codmunres) == COD_IBGE_ARARUAMA_6) %>%
    mutate(ano_arquivo = ano)

  cat("  Araruama:", nrow(obitos), "óbitos\n")
  write_parquet(obitos, file.path(dir_bruto_sim,
                                  paste0("sim_araruama_", ano, ".parquet")))
  rm(dados, obitos); gc(verbose = FALSE)
}

# --- Função: baixa um ano via microdatasus (fallback) ---
processar_sim_microdatasus <- function(ano, uf = UF_PROJETO) {
  cat("=== SIM", ano, "(microdatasus) ===\n")
  dados <- fetch_datasus(year_start = ano, year_end = ano,
                         uf = uf, information_system = "SIM-DO")
  cat("  RJ:", nrow(dados), "registros\n")

  obitos <- dados %>%
    rename_with(tolower) %>%
    filter(normalizar_cod_municipio(codmunres) == COD_IBGE_ARARUAMA_6) %>%
    mutate(ano_arquivo = ano)

  cat("  Araruama:", nrow(obitos), "óbitos\n")
  write_parquet(obitos, file.path(dir_bruto_sim,
                                  paste0("sim_araruama_", ano, ".parquet")))
  rm(dados, obitos); gc(verbose = FALSE)
}

# --- Loop principal ---
for (ano in ANOS_SIM) {
  alvo <- file.path(dir_bruto_sim, paste0("sim_araruama_", ano, ".parquet"))
  if (file.exists(alvo)) {
    cat("SIM", ano, "já existe, pulando.\n"); next
  }

  ok <- tryCatch({
    processar_sim_datasus(ano)
    TRUE
  }, error = function(e) {
    cat("  Falha (datasus):", conditionMessage(e), "\n")
    FALSE
  })

  if (!ok) {
    tryCatch(
      processar_sim_microdatasus(ano),
      error = function(e) cat("  Falha (microdatasus):", conditionMessage(e), "\n")
    )
  }
}

cat("\nArquivos em bruto/sim:", length(list.files(dir_bruto_sim)), "\n")