# ============================================================
# 10_extrair_sinasc.R — Série completa (1996-2024)
# ============================================================
# Versão 2 — Normaliza códigos de município (6 ou 7 dígitos)
# ============================================================

if (!exists("raiz")) raiz <- "/home/inominado/Documentos/BancoEpidemio"
source(file.path(raiz, "scripts", "00_setup.R"))
source(file.path(raiz, "scripts", "01_utils.R"))

dir_bruto_sinasc <- file.path(dir_bruto, "sinasc")
dir.create(dir_bruto_sinasc, recursive = TRUE, showWarnings = FALSE)

ANOS_SINASC <- 1996:2024

# --- Função: baixa, filtra e salva 1 ano ---
processar_sinasc_ano <- function(ano) {

  arquivo_saida <- file.path(dir_bruto_sinasc,
                              paste0("sinasc_araruama_", ano, ".parquet"))
  if (file.exists(arquivo_saida)) {
    cat("SINASC", ano, "já existe, pulando.\n")
    return(invisible(NULL))
  }

  cat("\n=== SINASC", ano, "===\n")
  t0 <- Sys.time()

  dados <- tryCatch(
    healthbR::sinasc_data(year = ano, uf = UF_PROJETO),
    error = function(e) {
      cat("  ERRO:", conditionMessage(e), "\n")
      NULL
    }
  )
  if (is.null(dados) || nrow(dados) == 0) {
    cat("  Sem dados\n")
    return(invisible(NULL))
  }

  cat("  RJ:", nrow(dados), "nascidos\n")

  # CORREÇÃO: normalizar códigos antes de filtrar
  nascidos <- dados %>%
    mutate(
      codres_norm = normalizar_cod_municipio(CODMUNRES),
      codnas_norm = normalizar_cod_municipio(CODMUNNASC)
    ) %>%
    filter(codres_norm == COD_IBGE_ARARUAMA_6) %>%
    mutate(
      flag_residencia_araruama = TRUE,
      flag_nascimento_araruama = codnas_norm == COD_IBGE_ARARUAMA_6,
      ano_arquivo              = ano
    ) %>%
    select(-codres_norm, -codnas_norm)

  cat("  Araruama:",
      nrow(nascidos), "residentes •",
      sum(nascidos$flag_nascimento_araruama, na.rm = TRUE), "nascidos no município\n")

  rm(dados); invisible(gc(verbose = FALSE))

  if (nrow(nascidos) > 0) {
    write_parquet(nascidos, arquivo_saida)
    cat("  Salvo:", basename(arquivo_saida),
        "|", round(file.size(arquivo_saida) / 1024^2, 2), "MB\n")
  }

  rm(nascidos); invisible(gc(verbose = FALSE))

  duracao <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  cat("  Tempo:", duracao, "s\n")
}

# --- Loop completo ---
for (ano in ANOS_SINASC) {
  tryCatch(
    processar_sinasc_ano(ano),
    error = function(e) cat("  ERRO em", ano, ":", conditionMessage(e), "\n")
  )
}

# --- Resumo final ---
cat("\n========================================\n")
cat("Resumo — SINASC Araruama\n")
cat("========================================\n")

arquivos <- list.files(dir_bruto_sinasc,
                        pattern = "\\.parquet$", full.names = TRUE)

resumo <- purrr::map_dfr(arquivos, function(f) {
  d <- read_parquet(f)
  tibble(
    ano      = as.integer(gsub("\\D", "", basename(f))),
    nascidos = nrow(d),
    no_munic = sum(d$flag_nascimento_araruama, na.rm = TRUE)
  )
}) %>% arrange(ano)

print(resumo, n = 30)
cat("\nTotal de nascidos vivos (residentes):", sum(resumo$nascidos), "\n")
cat("Arquivos:", length(arquivos), "\n")