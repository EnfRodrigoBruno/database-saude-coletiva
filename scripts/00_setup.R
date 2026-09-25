# ============================================================
# 00_setup.R — Configuração central do projeto
# ============================================================
# Carregado por todos os outros scripts. Define:
#   - Pacotes usados
#   - Caminho raiz do banco
#   - Diretórios derivados
#   - Opções globais
#   - Constantes do projeto
# ============================================================

# --- Caminho raiz ---
raiz <- "/home/inominado/Documentos/BancoEpidemio"

# --- Diretórios ---
dir_bruto       <- file.path(raiz, "bruto")
dir_bruto_sim   <- file.path(dir_bruto, "sim")
dir_bruto_prelim <- file.path(dir_bruto, "sim_preliminar")
dir_bruto_pop   <- file.path(dir_bruto, "populacao")
dir_tratado     <- file.path(raiz, "tratado")
dir_analytics   <- file.path(raiz, "analytics")
dir_scripts     <- file.path(raiz, "scripts")
dir_dashboard   <- file.path(raiz, "dashboard")

# --- Garante que os diretórios existem ---
for (d in c(dir_bruto, dir_bruto_sim, dir_bruto_prelim, dir_bruto_pop,
            dir_tratado, dir_analytics, dir_scripts, dir_dashboard)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# --- Pacotes do projeto ---
pacotes <- c(
  "datasus", "microdatasus", "healthbR", "brpop",
  "dplyr", "tidyr", "purrr", "stringr", "lubridate", "tibble",
  "forcats", "arrow", "ggplot2", "cid10"
)

for (p in pacotes) {
  if (!requireNamespace(p, quietly = TRUE)) {
    message("Pacote ausente: ", p, " — instale com install.packages('", p, "')")
  } else {
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }
}

# --- Opções globais ---
options(timeout = 300)

# --- Constantes do projeto ---
COD_IBGE_ARARUAMA_6 <- "330020"    # SIM, SINASC, SINAN (6 dígitos)
COD_IBGE_ARARUAMA_7 <- "3300209"   # IBGE, brpop, SIDRA (7 dígitos)
UF_PROJETO          <- "RJ"
ANOS_SIM            <- 1996:2024
ANOS_POP            <- 2000:2024
LIMITE_APVP         <- 70

message("Setup carregado. Raiz: ", raiz)