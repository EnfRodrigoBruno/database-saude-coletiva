# ============================================================
# 08_previas_tabnet.R — Prévia de óbitos do TabNet (2025-2026)
# ============================================================

if (!exists("raiz")) raiz <- "/home/inominado/Documentos/BancoEpidemio"
source(file.path(raiz, "scripts", "00_setup.R"))

dir_bruto_prelim <- file.path(dir_bruto, "sim_preliminar")
dir.create(dir_bruto_prelim, recursive = TRUE, showWarnings = FALSE)

ANOS_PREVIA <- 2025:2026
MUN_FILTRO  <- "330020 ARARUAMA"
CONTEUDO    <- 1

baixar_previa <- function(ano, linha, sufixo) {
  cat("=== ", ano, "| linha:", linha, "===\n")

  dados <- NULL
  for (tentativa in 1:3) {
    dados <- tryCatch(
      datasus::sim(
        conjunto    = "obitos",
        abrangencia = "municipio",
        uf          = UF_PROJETO,
        linha       = linha,
        coluna      = "--Não-Ativa--",
        conteudo    = CONTEUDO,
        periodo     = as.character(ano),
        filtros     = list(municipio = MUN_FILTRO)
      ),
      error = function(e) {
        cat("  Tentativa", tentativa, "falhou:", conditionMessage(e), "\n")
        NULL
      }
    )
    if (!is.null(dados)) break
    Sys.sleep(5)
  }

  if (is.null(dados) || nrow(dados) == 0) {
    cat("  Sem dados após 3 tentativas.\n\n")
    return(invisible(NULL))
  }

  primeira_col <- names(dados)[1]
  dados <- dados %>%
    filter(.data[[primeira_col]] != "TOTAL")

  if (nrow(dados) == 0) {
    cat("  Sem dados após remover TOTAL.\n\n")
    return(invisible(NULL))
  }

  dados <- dados %>%
    mutate(
      ano_obito = ano,
      status    = "preliminar",
      fonte     = "TabNet/DATASUS",
      dimensao  = sufixo
    )

  caminho <- file.path(dir_bruto_prelim,
                       paste0("sim_araruama_", ano, "_", sufixo, ".parquet"))
  write_parquet(dados, caminho)
  cat("  Salvo:", basename(caminho), "|", nrow(dados), "linhas\n\n")
  invisible(dados)
}

for (ano in ANOS_PREVIA) {
  baixar_previa(ano, "Município", "total");         Sys.sleep(3)
  baixar_previa(ano, "Sexo", "sexo");                Sys.sleep(3)
  baixar_previa(ano, "Faixa_Etária", "faixa_etaria"); Sys.sleep(3)
  baixar_previa(ano, "Capítulo_CID-10", "capitulo_cid"); Sys.sleep(3)
  baixar_previa(ano, "Mês_do_Óbito", "mes");         Sys.sleep(3)
}

arquivos <- list.files(dir_bruto_prelim, pattern = "\\.parquet$")
cat("\n✅ Prévia baixada. Arquivos em bruto/sim_preliminar/:\n")
for (f in arquivos) cat("   -", f, "\n")