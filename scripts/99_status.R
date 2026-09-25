# ============================================================
# 99_status.R — Inventário do banco de dados
# ============================================================
# Mostra o que temos, o que falta e o tamanho do banco.
# ============================================================

if (!exists("raiz")) raiz <- "/home/inominado/Documentos/BancoEpidemio"

cat("\n")
cat("============================================================\n")
cat("  INVENTÁRIO DO BANCO EPIDEMIOLÓGICO DE ARARUAMA\n")
cat("============================================================\n\n")
cat("Raiz:", raiz, "\n")
cat("Data da consulta:", format(Sys.time(), "%d/%m/%Y %H:%M"), "\n\n")

# ------------------------------------------------------------
# Função auxiliar: tamanho de uma pasta em MB
# ------------------------------------------------------------
tamanho_pasta <- function(p) {
  if (!dir.exists(p)) return(0)
  files <- list.files(p, recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) return(0)
  round(sum(file.size(files)) / 1024^2, 1)
}

# ------------------------------------------------------------
# Camada BRUTA
# ------------------------------------------------------------
cat("============================================================\n")
cat("  CAMADA BRUTA (dados originais do DATASUS)\n")
cat("============================================================\n\n")

sistemas_brutos <- list(
  list(nome = "SIM (óbitos)",           pasta = "sim",             esperado = 29),
  list(nome = "SIM prévia 2025-2026",   pasta = "sim_preliminar",  esperado = 10),
  list(nome = "SINASC (nascimentos)",   pasta = "sinasc",          esperado = 29),
  list(nome = "População IBGE",         pasta = "populacao",       esperado = 1),
  list(nome = "SINAN dengue",           pasta = "sinan/dengue",    esperado = 17),
  list(nome = "SINAN chikungunya",      pasta = "sinan/chikungunya", esperado = NA),
  list(nome = "SINAN zika",             pasta = "sinan/zika",      esperado = NA),
  list(nome = "SINAN tuberculose",      pasta = "sinan/tuberculose", esperado = NA),
  list(nome = "SIH (internações)",      pasta = "sih",             esperado = NA),
  list(nome = "CNES",                   pasta = "cnes",            esperado = NA),
  list(nome = "SIA",                    pasta = "sia",             esperado = NA),
  list(nome = "PNI",                    pasta = "pni",             esperado = NA)
)

cat(sprintf("%-30s %8s %8s %8s\n", "Sistema", "Arqs", "Esperado", "MB"))
cat(strrep("-", 60), "\n")

total_bruto_mb <- 0
for (s in sistemas_brutos) {
  pasta_full <- file.path(raiz, "bruto", s$pasta)
  n_arqs <- length(list.files(pasta_full, pattern = "\\.(parquet|dbc|dbf)$",
                              recursive = TRUE))
  mb <- tamanho_pasta(pasta_full)
  total_bruto_mb <- total_bruto_mb + mb

  status <- if (is.na(s$esperado)) {
    if (n_arqs > 0) "✓" else "—"
  } else if (n_arqs == s$esperado) {
    "✓"
  } else {
    "⚠"
  }

  esp <- if (is.na(s$esperado)) "—" else as.character(s$esperado)
  cat(sprintf("%-30s %8d %8s %7.1f %s\n", s$nome, n_arqs, esp, mb, status))
}

cat(strrep("-", 60), "\n")
cat(sprintf("%-30s %8s %8s %7.1f\n", "TOTAL BRUTO", "", "", total_bruto_mb))

# ------------------------------------------------------------
# Camada TRATADA
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("  CAMADA TRATADA (dados limpos e enriquecidos)\n")
cat("============================================================\n\n")

arquivos_tratado <- list.files(file.path(raiz, "tratado"),
                                pattern = "\\.parquet$",
                                full.names = TRUE)

esperados_tratado <- c(
  "fato_obitos.parquet",
  "fato_obitos_redistribuido.parquet",
  "fato_nascimentos.parquet",
  "fato_dengue.parquet"
)

cat(sprintf("%-45s %8s\n", "Arquivo", "MB"))
cat(strrep("-", 55), "\n")

total_tratado_mb <- 0
for (arq in arquivos_tratado) {
  mb <- round(file.size(arq) / 1024^2, 2)
  total_tratado_mb <- total_tratado_mb + mb
  cat(sprintf("%-45s %7.2f\n", basename(arq), mb))
}
cat(strrep("-", 55), "\n")
cat(sprintf("%-45s %7.2f\n", "TOTAL", total_tratado_mb))

# Verificar faltantes
faltando <- setdiff(esperados_tratado, basename(arquivos_tratado))
if (length(faltando) > 0) {
  cat("\n⚠ Arquivos esperados que estão faltando:\n")
  for (f in faltando) cat("   -", f, "\n")
} else {
  cat("\n✓ Todos os arquivos esperados estão presentes.\n")
}

# ------------------------------------------------------------
# Camada ANALYTICS
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("  CAMADA ANALYTICS (indicadores prontos)\n")
cat("============================================================\n\n")

arquivos_ana <- list.files(file.path(raiz, "analytics"),
                            pattern = "\\.parquet$")
arquivos_png <- list.files(file.path(raiz, "analytics"),
                            pattern = "\\.png$")

cat("Parquets de indicadores:", length(arquivos_ana), "\n")
cat("Gráficos (PNG):          ", length(arquivos_png), "\n")
cat("Tamanho total:           ", tamanho_pasta(file.path(raiz, "analytics")), "MB\n\n")

cat("Por sistema:\n")
por_sistema <- case_when(
  grepl("^ind_mortalidade|^ind_top10|^ind_apvp", arquivos_ana) ~ "Mortalidade",
  grepl("^ind_natalidade|^ind_sinasc|^ind_mortalidade_infantil|^ind_mortalidade_materna", arquivos_ana) ~ "Materno-infantil",
  grepl("^ind_dengue", arquivos_ana) ~ "Dengue",
  TRUE ~ "Outros"
)
print(table(por_sistema))

# ------------------------------------------------------------
# Resumo geral
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("  RESUMO GERAL\n")
cat("============================================================\n\n")

total_geral <- total_bruto_mb + total_tratado_mb +
               tamanho_pasta(file.path(raiz, "analytics"))

cat("Tamanho total do banco:", total_geral, "MB\n\n")

cat("Sistemas COMPLETOS:\n")
cat("  ✓ SIM (óbitos) 1996-2024\n")
cat("  ✓ População IBGE 2000-2024\n")
cat("  ✓ SINASC (nascimentos) 1996-2024\n")
cat("  ✓ SINAN dengue 2007-2023\n")
cat("  ✓ Prévia TabNet 2025-2026\n")

cat("\nSistemas PENDENTES:\n")
cat("  ○ SINAN outros agravos (chikungunya, zika, tuberculose, hanseníase, etc.)\n")
cat("  ○ SIH (internações hospitalares)\n")
cat("  ○ CNES (estabelecimentos de saúde)\n")
cat("  ○ SIA (produção ambulatorial)\n")
cat("  ○ PNI (imunizações)\n")

cat("\n============================================================\n")