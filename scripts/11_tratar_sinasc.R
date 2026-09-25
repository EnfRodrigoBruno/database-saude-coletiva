# ============================================================
# 11_tratar_sinasc.R — Consolida e enriquece a base de nascidos vivos
# ============================================================
# Gera: tratado/fato_nascimentos.parquet
# ============================================================

if (!exists("raiz")) raiz <- "/home/inominado/Documentos/BancoEpidemio"
source(file.path(raiz, "scripts", "00_setup.R"))
source(file.path(raiz, "scripts", "01_utils.R"))

dir_bruto_sinasc <- file.path(dir_bruto, "sinasc")

cat("Consolidando arquivos...\n")

nasc <- list.files(dir_bruto_sinasc,
                    pattern = "\\.parquet$", full.names = TRUE) %>%
  purrr::map_dfr(read_parquet)

cat("Total bruto:", nrow(nasc), "registros\n")

# ------------------------------------------------------------
# Funções auxiliares
# ------------------------------------------------------------
parse_data <- function(x) {
  if (inherits(x, "Date")) return(x)
  s <- as.character(x)
  if (all(nchar(s[!is.na(s)]) == 8)) {
    return(as.Date(s, format = "%d%m%Y"))
  }
  return(as.Date(s))
}

faixa_idade_mae <- function(idade) {
  cut(idade,
      breaks = c(-Inf, 15, 20, 25, 30, 35, 40, 45, Inf),
      labels = c("<15", "15-19", "20-24", "25-29", "30-34",
                 "35-39", "40-44", "45+"),
      right = FALSE)
}

classif_peso <- function(p) {
  dplyr::case_when(
    is.na(p)          ~ NA_character_,
    p < 1500          ~ "Muito baixo peso (<1500g)",
    p < 2500          ~ "Baixo peso (1500-2499g)",
    p < 4000          ~ "Peso adequado (2500-3999g)",
    TRUE              ~ "Macrossomia (>=4000g)"
  )
}

# CORREÇÃO: GESTACAO é código, não semanas
classif_gestacao <- function(codigo) {
  dplyr::case_when(
    codigo == "1" ~ "Prematuro extremo (<22sem)",
    codigo == "2" ~ "Prematuro (22-27sem)",
    codigo == "3" ~ "Prematuro (28-31sem)",
    codigo == "4" ~ "Prematuro tardio (32-36sem)",
    codigo == "5" ~ "Termo (37-41sem)",
    codigo == "6" ~ "Pós-termo (>=42sem)",
    codigo == "9" ~ "Ignorado",
    TRUE          ~ NA_character_
  )
}

# ------------------------------------------------------------
# Enriquecer
# ------------------------------------------------------------
nasc_tratado <- nasc %>%
  mutate(
    dt_nasc     = parse_data(DTNASC),
    ano_nasc    = as.integer(format(dt_nasc, "%Y")),
    mes_nasc    = as.integer(format(dt_nasc, "%m")),

    idade_mae        = as.numeric(IDADEMAE),
    faixa_etaria_mae = faixa_idade_mae(idade_mae),

    peso_g       = as.numeric(PESO),
    peso_classif = classif_peso(peso_g),

    gestacao_codigo = as.character(GESTACAO),
    prematuridade   = classif_gestacao(GESTACAO),
    prematuro       = GESTACAO %in% c("1", "2", "3", "4"),

    apgar1_num   = suppressWarnings(as.numeric(APGAR1)),
    apgar5_num   = suppressWarnings(as.numeric(APGAR5)),
    apgar5_baixo = !is.na(apgar5_num) & apgar5_num < 7,

    tipo_parto = case_when(
      PARTO == "1" ~ "Vaginal",
      PARTO == "2" ~ "Cesáreo",
      TRUE         ~ "Ignorado"
    ),

    sexo_nasc = case_when(
      SEXO == "1" ~ "Masculino",
      SEXO == "2" ~ "Feminino",
      TRUE        ~ "Ignorado"
    ),

    consultas_prenatal = suppressWarnings(as.numeric(CONSULTAS)),
    prenatal_adequado  = !is.na(consultas_prenatal) & consultas_prenatal >= 6,

    baixo_peso          = !is.na(peso_g) & peso_g < 2500,
    nasceu_no_municipio = flag_nascimento_araruama
  ) %>%
  select(
    year, ano_nasc, mes_nasc, dt_nasc,
    flag_residencia_araruama, nasceu_no_municipio,
    CODMUNRES, CODMUNNASC,
    sexo_nasc,
    peso_g, peso_classif, baixo_peso,
    gestacao_codigo, prematuridade, prematuro,
    apgar1_num, apgar5_num, apgar5_baixo,
    tipo_parto,
    idade_mae, faixa_etaria_mae,
    consultas_prenatal, prenatal_adequado,
    RACACORMAE, ESCMAE
  )

cat("Registros tratados:", nrow(nasc_tratado), "\n")

write_parquet(nasc_tratado,
              file.path(dir_tratado, "fato_nascimentos.parquet"))

cat("Salvo em: tratado/fato_nascimentos.parquet\n")

# ------------------------------------------------------------
# Sumários
# ------------------------------------------------------------
cat("\n--- Prematuridade (corrigida) ---\n")
print(table(nasc_tratado$prematuridade, useNA = "ifany"))

cat("\n--- Tipo de parto ---\n")
print(table(nasc_tratado$tipo_parto, useNA = "ifany"))

cat("\n--- Faixa etária da mãe ---\n")
print(table(nasc_tratado$faixa_etaria_mae, useNA = "ifany"))

cat("\n--- Peso ao nascer ---\n")
print(table(nasc_tratado$peso_classif, useNA = "ifany"))

cat("\n--- Apgar 5' < 7 ---\n")
print(table(nasc_tratado$apgar5_baixo, useNA = "ifany"))