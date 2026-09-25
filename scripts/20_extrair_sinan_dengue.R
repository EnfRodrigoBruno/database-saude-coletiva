# ============================================================
# 20_extrair_sinan_dengue.R — Série completa (2007-2024)
# ============================================================
# Baixa, filtra e salva ano a ano, liberando memória.
# Pula anos já existentes (retomável).
# ============================================================

if (!exists("raiz")) raiz <- "/home/inominado/Documentos/BancoEpidemio"
source(file.path(raiz, "scripts", "00_setup.R"))
source(file.path(raiz, "scripts", "01_utils.R"))

if (!requireNamespace("lobstr", quietly = TRUE)) install.packages("lobstr")
library(lobstr)

dir_bruto_sinan_dengue <- file.path(dir_bruto, "sinan", "dengue")
dir.create(dir_bruto_sinan_dengue, recursive = TRUE, showWarnings = FALSE)

# --- Anos disponíveis para dengue no DATASUS ---
ANOS_DENGUE <- 2007:2024

# --- Função: baixa, filtra e salva 1 ano ---
processar_dengue_ano <- function(ano) {

  arquivo_saida <- file.path(dir_bruto_sinan_dengue,
                              paste0("dengue_araruama_", ano, ".parquet"))
  if (file.exists(arquivo_saida)) {
    cat("Dengue", ano, "já existe, pulando.\n")
    return(invisible(NULL))
  }

  cat("\n=== Dengue", ano, "===\n")
  t0 <- Sys.time()

  dados <- tryCatch(
    healthbR::sinan_data(year = ano, disease = "DENG"),
    error = function(e) {
      cat("  ERRO no download:", conditionMessage(e), "\n")
      NULL
    }
  )
  if (is.null(dados)) return(invisible(NULL))

  cat("  Brasil:", nrow(dados), "linhas\n")

  araruama <- dados %>%
    filter(
      ID_MN_RESI == COD_IBGE_ARARUAMA_6 |
      ID_MUNICIP == COD_IBGE_ARARUAMA_6
    ) %>%
    mutate(
      flag_residencia  = ID_MN_RESI == COD_IBGE_ARARUAMA_6,
      flag_notificacao = ID_MUNICIP == COD_IBGE_ARARUAMA_6
    )

  cat("  Araruama:",
      sum(araruama$flag_residencia, na.rm = TRUE), "residentes •",
      sum(araruama$flag_notificacao, na.rm = TRUE), "notificados\n")

  rm(dados); invisible(gc(verbose = FALSE))

  if (nrow(araruama) > 0) {
    write_parquet(araruama, arquivo_saida)
    cat("  Salvo:", basename(arquivo_saida),
        "|", round(file.size(arquivo_saida) / 1024^2, 2), "MB\n")
  }

  rm(araruama); invisible(gc(verbose = FALSE))

  duracao <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  cat("  Tempo:", duracao, "s • Memória R:",
      round(lobstr::mem_used() / 1024^3, 2), "GB\n")
}

# --- Loop completo ---
for (ano in ANOS_DENGUE) {
  tryCatch(
    processar_dengue_ano(ano),
    error = function(e) cat("  ERRO em", ano, ":", conditionMessage(e), "\n")
  )
}

# --- Resumo final ---
cat("\n========================================\n")
cat("Resumo — Dengue Araruama\n")
cat("========================================\n")

arquivos <- list.files(dir_bruto_sinan_dengue,
                        pattern = "\\.parquet$", full.names = TRUE)

resumo <- purrr::map_dfr(arquivos, function(f) {
  d <- read_parquet(f)
  tibble(
    ano         = as.integer(gsub("\\D", "", basename(f))),
    residentes  = sum(d$flag_residencia, na.rm = TRUE),
    notificados = sum(d$flag_notificacao, na.rm = TRUE)
  )
}) %>% arrange(ano)

print(resumo, n = 20)
cat("\nTotal de residentes (2007-2024):", sum(resumo$residentes), "\n")
cat("Total de notificados (2007-2024):", sum(resumo$notificados), "\n")
cat("Arquivos:", length(arquivos), "\n")