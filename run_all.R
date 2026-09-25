# ============================================================
# run_all.R — Orquestrador do banco epidemiológico de Araruama
# ============================================================
# Executa todos os scripts na ordem correta.
# Detecta a raiz automaticamente (funciona em qualquer PC).
#
# Uso:
#   source("run_all.R")               # roda tudo
#   run_all(apenas_pendentes = TRUE)  # só o que falta
# ============================================================

# ------------------------------------------------------------
# 1. Detecção automática da raiz
# ------------------------------------------------------------
encontrar_raiz <- function(inicio = getwd(), sentinela = "run_all.R") {
  dir <- normalizePath(inicio)
  for (i in 1:10) {
    if (file.exists(file.path(dir, sentinela))) return(dir)
    pai <- dirname(dir)
    if (pai == dir) break
    dir <- pai
  }
  stop("Raiz não encontrada. Rode este script a partir da pasta do projeto ",
       "ou defina `raiz` manualmente antes de chamar run_all().")
}

if (!exists("raiz")) {
  raiz <- tryCatch(
    encontrar_raiz(),
    error = function(e) {
      # Fallback: caminho do usuário atual
      file.path(path.expand("~"), "Documentos", "BancoEpidemio")
    }
  )
}

cat("\n")
cat("============================================================\n")
cat("  PIPELINE DO BANCO EPIDEMIOLÓGICO DE ARARUAMA\n")
cat("============================================================\n")
cat("  Raiz:", raiz, "\n")
cat("  Início:", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), "\n")
cat("============================================================\n\n")

# ------------------------------------------------------------
# 2. Definição dos scripts e ordem de execução
# ------------------------------------------------------------
# A ordem importa! Alguns scripts dependem do resultado de outros.
# Estrutura de numeração:
#   00-09: infra + SIM
#   10-19: SINASC
#   20-29: SINAN
#   30-39: SIH
#   40-49: CNES
#   50-59: SIA
#   60-69: PNI
#   99   : status

pipeline <- list(
  # --- Infraestrutura ---
  list(script = "00_setup.R",
       descricao = "Configuração global",
       categoria = "infra",
       essencial = TRUE),

  list(script = "01_utils.R",
       descricao = "Funções utilitárias",
       categoria = "infra",
       essencial = TRUE),

  # --- SIM (óbitos) ---
  list(script = "02_extrair_sim.R",
       descricao = "Extrair SIM 1996-2024",
       categoria = "sim",
       essencial = FALSE),

  list(script = "03_extrair_populacao.R",
       descricao = "Extrair população IBGE",
       categoria = "sim",
       essencial = FALSE),

  list(script = "04_tratar_sim.R",
       descricao = "Consolidar SIM",
       categoria = "sim",
       essencial = FALSE),

  list(script = "07_redistribuicao_r99.R",
       descricao = "Redistribuir garbage codes",
       categoria = "sim",
       essencial = FALSE),

  list(script = "06_indicadores_causas.R",
       descricao = "Indicadores de causas e APVP",
       categoria = "sim",
       essencial = FALSE),

  list(script = "05_indicadores_mortalidade.R",
       descricao = "Indicadores de mortalidade geral",
       categoria = "sim",
       essencial = FALSE),

  list(script = "08_previas_tabnet.R",
       descricao = "Prévias TabNet 2025-2026",
       categoria = "sim",
       essencial = FALSE),

  # --- SINASC (nascimentos) ---
  list(script = "10_extrair_sinasc.R",
       descricao = "Extrair SINASC 1996-2024",
       categoria = "sinasc",
       essencial = FALSE),

  list(script = "11_tratar_sinasc.R",
       descricao = "Consolidar SINASC",
       categoria = "sinasc",
       essencial = FALSE),

  list(script = "12_indicadores_sinasc.R",
       descricao = "Indicadores materno-infantis",
       categoria = "sinasc",
       essencial = FALSE),

  # --- SINAN dengue ---
  list(script = "20_extrair_sinan_dengue.R",
       descricao = "Extrair SINAN dengue 2007-2024",
       categoria = "sinan",
       essencial = FALSE),

  list(script = "21_tratar_sinan_dengue.R",
       descricao = "Consolidar SINAN dengue",
       categoria = "sinan",
       essencial = FALSE),

  list(script = "22_indicadores_dengue.R",
       descricao = "Indicadores de dengue",
       categoria = "sinan",
       essencial = FALSE)
)

# ------------------------------------------------------------
# 3. Função auxiliar: verifica se um script já foi executado
# ------------------------------------------------------------
# Estratégia: cada script é considerado "pendente" se algum arquivo
# esperado de saída ainda não existe. Se não soubermos os produtos,
# sempre executa.
produtos_esperados <- list(
  # --- SIM ---
  "02_extrair_sim.R"             = "bruto/sim/sim_araruama_2024.parquet",
  "03_extrair_populacao.R"       = "bruto/populacao/populacao_araruama_1996_2024.parquet",
  "04_tratar_sim.R"              = "tratado/fato_obitos.parquet",
  "07_redistribuicao_r99.R"      = "tratado/fato_obitos_redistribuido.parquet",
  "06_indicadores_causas.R"      = "analytics/ind_top10_causas_total.parquet",
  "05_indicadores_mortalidade.R" = "analytics/ind_mortalidade_geral.parquet",
  "08_previas_tabnet.R"          = "bruto/sim_preliminar/sim_araruama_2025_total.parquet",
  
  # --- SINASC ---
  "10_extrair_sinasc.R"          = "bruto/sinasc/sinasc_araruama_2024.parquet",
  "11_tratar_sinasc.R"           = "tratado/fato_nascimentos.parquet",
  "12_indicadores_sinasc.R"      = "analytics/ind_mortalidade_infantil.parquet",
  
  # --- SINAN dengue ---
  "20_extrair_sinan_dengue.R"    = "bruto/sinan/dengue/dengue_araruama_2023.parquet",
  "21_tratar_sinan_dengue.R"     = "tratado/fato_dengue.parquet",
  "22_indicadores_dengue.R"      = "analytics/ind_dengue_incidencia_anual.parquet"
)

# ------------------------------------------------------------
# 4. Execução cronometrada
# ------------------------------------------------------------
run_all <- function(apenas_pendentes = FALSE) {

  tempos <- list()
  status <- list()
  pulados <- 0

  t0_geral <- Sys.time()

  for (i in seq_along(pipeline)) {
    passo <- pipeline[[i]]
    caminho <- file.path(raiz, "scripts", passo$script)

    # Verifica se deve pular (apenas_pendentes)
    produto <- produtos_esperados[[passo$script]]
    if (apenas_pendentes && !is.null(produto)) {
      if (file.exists(file.path(raiz, produto))) {
        cat(sprintf("[%2d/%d] %-30s ⊘ (já existe)\n",
                    i, length(pipeline), passo$script))
        pulados <- pulados + 1
        status[[passo$script]] <- "pulado"
        next
      }
    }

    # Verifica se o script existe
    if (!file.exists(caminho)) {
      cat(sprintf("[%2d/%d] %-30s ✗ (não encontrado)\n",
                  i, length(pipeline), passo$script))
      status[[passo$script]] <- "não encontrado"
      next
    }

    cat(sprintf("[%2d/%d] %-30s ", i, length(pipeline), passo$script))

    t_inicio <- Sys.time()

    resultado <- tryCatch({
      suppressWarnings(
        source(caminho, local = FALSE, echo = FALSE, print.eval = FALSE)
      )
      "✓"
    }, error = function(e) {
      cat("\n     ❌ ERRO:", conditionMessage(e), "\n")
      paste0("erro: ", conditionMessage(e))
    })

    duracao <- round(as.numeric(difftime(Sys.time(), t_inicio, units = "secs")), 1)
    tempos[[passo$script]] <- duracao
    status[[passo$script]] <- resultado

    if (resultado == "✓") {
      cat(sprintf("✓ (%.1fs)\n", duracao))
    }

    # Coleta lixo entre scripts
    invisible(gc(verbose = FALSE))
  }

  # ------------------------------------------------------------
  # 5. Sumário final
  # ------------------------------------------------------------
  t_total <- round(as.numeric(difftime(Sys.time(), t0_geral, units = "mins")), 1)

  cat("\n")
  cat("============================================================\n")
  cat("  PIPELINE CONCLUÍDO\n")
  cat("============================================================\n\n")

  # Contagem
  n_ok      <- sum(unlist(status) == "✓")
  n_pulados <- sum(unlist(status) == "pulado")
  n_erros   <- sum(grepl("^erro", unlist(status)))
  n_falta   <- sum(unlist(status) == "não encontrado")

  cat("Total de scripts:      ", length(pipeline), "\n")
  cat("  ✓ Executados com sucesso:", n_ok, "\n")
  cat("  ⊘ Pulados (já existiam):", n_pulados, "\n")
  cat("  ✗ Erros:                 ", n_erros, "\n")
  cat("  - Não encontrados:       ", n_falta, "\n\n")

  cat("Tempo total:", t_total, "minutos\n")
  cat("Fim:", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), "\n")

  # Lista erros se houver
  if (n_erros > 0) {
    cat("\n⚠ Scripts com erro:\n")
    erros <- names(status)[grepl("^erro", unlist(status))]
    for (s in erros) {
      cat("  -", s, "::", status[[s]], "\n")
    }
  }

  # Tabela de tempos
  if (length(tempos) > 0) {
    cat("\nTempos por script (segundos):\n")
    for (s in names(tempos)) {
      cat(sprintf("  %-30s %6.1f\n", s, tempos[[s]]))
    }
  }

  cat("\n============================================================\n")
}

# ------------------------------------------------------------
# 6. Execução automática
# ------------------------------------------------------------
# Descomente para rodar automaticamente ao dar source():
# run_all()

# Ou rode manualmente com o modo desejado:
cat("\n💡 Para executar o pipeline:\n")
cat("   run_all()                     # roda tudo\n")
cat("   run_all(apenas_pendentes = TRUE)  # pula o que já existe\n\n")