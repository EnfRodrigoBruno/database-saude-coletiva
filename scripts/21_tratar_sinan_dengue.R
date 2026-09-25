# ============================================================
# 21_tratar_sinan_dengue.R — Consolida e enriquece a base de dengue
# ============================================================

if (!exists("raiz")) raiz <- "/home/inominado/Documentos/BancoEpidemio"
source(file.path(raiz, "scripts", "00_setup.R"))
source(file.path(raiz, "scripts", "01_utils.R"))

dir_bruto_sinan_dengue <- file.path(dir_bruto, "sinan", "dengue")

# ------------------------------------------------------------
# 1. Consolida os 17 arquivos
# ------------------------------------------------------------
cat("Consolidando arquivos...\n")

dengue <- list.files(dir_bruto_sinan_dengue,
                      pattern = "\\.parquet$", full.names = TRUE) %>%
  purrr::map_dfr(read_parquet)

cat("Total bruto:", nrow(dengue), "registros\n")

dengue_res <- dengue %>% filter(flag_residencia)
cat("Residentes em Araruama:", nrow(dengue_res), "\n")

# ------------------------------------------------------------
# 2. Decodificadores específicos do SINAN
# ------------------------------------------------------------

# Idade SINAN: 4 dígitos (1=hora, 2=dia, 3=mês, 4=ano)
decodificar_idade_sinan <- function(cod) {
  i <- suppressWarnings(as.integer(cod))
  dplyr::case_when(
    is.na(i)                              ~ NA_real_,
    substr(as.character(i), 1, 1) == "1"  ~ as.numeric(substr(as.character(i), 2, 4)) / 8760,
    substr(as.character(i), 1, 1) == "2"  ~ as.numeric(substr(as.character(i), 2, 4)) / 365,
    substr(as.character(i), 1, 1) == "3"  ~ as.numeric(substr(as.character(i), 2, 4)) / 12,
    substr(as.character(i), 1, 1) == "4"  ~ as.numeric(substr(as.character(i), 2, 4)),
    TRUE                                  ~ NA_real_
  )
}

decod_sexo <- function(x) dplyr::case_when(
  x == "M" ~ "Masculino", x == "F" ~ "Feminino", TRUE ~ "Ignorado"
)

# CLASSI_FIN: códigos antigos (1-4) + novos (8, 10-12)
#   "1" e "8" e "10"  → Dengue
#   "2" e "11"        → Dengue com sinais de alarme
#   "3", "4", "12"    → Dengue grave
decod_classi <- function(x) dplyr::case_when(
  x %in% c("1", "8", "10") ~ "Dengue",
  x %in% c("2", "11")      ~ "Dengue com sinais de alarme",
  x %in% c("3", "4", "12") ~ "Dengue grave",
  x == "5"                 ~ "Descartado",
  x == "6"                 ~ "Inconclusivo",
  TRUE                     ~ "Outros/Ignorado"
)

decod_evolucao <- function(x) dplyr::case_when(
  x == "1" ~ "Cura",
  x == "2" ~ "Óbito por dengue",
  x == "3" ~ "Óbito por outras causas",
  x == "4" ~ "Óbito em investigação",
  x == "9" ~ "Ignorado",
  TRUE     ~ "Ignorado"
)

decod_raca <- function(x) dplyr::case_when(
  x == "1" ~ "Branca", x == "2" ~ "Preta", x == "3" ~ "Amarela",
  x == "4" ~ "Parda",  x == "5" ~ "Indígena", TRUE ~ "Ignorado"
)

decod_autoctone <- function(x) dplyr::case_when(
  x == "1" ~ "Sim", x == "2" ~ "Não", x == "3" ~ "Indeterminado", TRUE ~ "Ignorado"
)

faixa_etaria_cut <- function(idade) {
  cut(idade,
      breaks = c(-0.01, 1, 5, 15, 30, 45, 60, 70, Inf),
      labels = c("0", "1-4", "5-14", "15-29", "30-44", "45-59", "60-69", "70+"),
      right  = FALSE)
}

# ------------------------------------------------------------
# 3. Enriquecer
# ------------------------------------------------------------
dengue_tratado <- dengue_res %>%
  mutate(
    ano_sintoma = as.integer(substr(SEM_PRI, 1, 4)),
    sem_sintoma = as.integer(substr(SEM_PRI, 5, 6)),

    dt_sin_aprox = as.Date(paste0(ano_sintoma, "-01-01")) +
                   (sem_sintoma - 1) * 7,

    idade_anos   = decodificar_idade_sinan(NU_IDADE_N),
    faixa_etaria = faixa_etaria_cut(idade_anos),

    sexo_label      = decod_sexo(CS_SEXO),
    classi_label    = decod_classi(CLASSI_FIN),
    evolucao_label  = decod_evolucao(EVOLUCAO),
    raca_label      = decod_raca(CS_RACA),
    autoctone_label = decod_autoctone(TPAUTOCTO),

    # Confirmado agora inclui todos os códigos históricos
    confirmado    = CLASSI_FIN %in% c("1", "2", "3", "4", "8", "10", "11", "12"),
    hospitalizado = HOSPITALIZ == "1",
    obito_dengue  = EVOLUCAO == "2",
    autoctone     = TPAUTOCTO == "1"
  ) %>%
  select(
    year, ano_sintoma, sem_sintoma, dt_sin_aprox,
    ID_MUNICIP, ID_MN_RESI,
    sexo_label, idade_anos, faixa_etaria,
    raca_label, CS_GESTANT,
    classi_label, evolucao_label,
    confirmado, hospitalizado, obito_dengue,
    autoctone, autoctone_label,
    SOROTIPO, CRITERIO,
    DT_ENCERRA, DT_OBITO
  )

cat("Registros tratados:", nrow(dengue_tratado), "\n")

write_parquet(dengue_tratado,
              file.path(dir_tratado, "fato_dengue.parquet"))

cat("Salvo em: tratado/fato_dengue.parquet\n")
cat("Tamanho:",
    round(file.size(file.path(dir_tratado, "fato_dengue.parquet")) / 1024^2, 2),
    "MB\n")

# ------------------------------------------------------------
# 4. Sumário
# ------------------------------------------------------------
cat("\n--- Casos confirmados por ano ---\n")
dengue_tratado %>%
  filter(confirmado) %>%
  count(ano_sintoma, name = "casos") %>%
  arrange(ano_sintoma) %>%
  print(n = 20)

cat("\n--- Idade média ---\n")
cat(round(mean(dengue_tratado$idade_anos, na.rm = TRUE), 1), "anos\n")

cat("\n--- Distribuição por sexo ---\n")
print(table(dengue_tratado$sexo_label))

cat("\n--- Distribuição por classificação ---\n")
print(table(dengue_tratado$classi_label))

cat("\n--- Desfechos ---\n")
print(table(dengue_tratado$evolucao_label))

cat("\n--- Hospitalizações ---\n")
print(table(dengue_tratado$hospitalizado))

cat("\n--- Autóctones ---\n")
print(table(dengue_tratado$autoctone_label))