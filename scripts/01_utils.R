# ============================================================
# 01_utils.R — Funções auxiliares reutilizáveis
# ============================================================

# --- Garante que o setup está carregado ---
if (!exists("raiz")) {
  raiz <- "/home/inominado/Documentos/BancoEpidemio"
}
if (!exists("COD_IBGE_ARARUAMA_6")) {
  source(file.path(raiz, "scripts", "00_setup.R"))
}

# --- Operador auxiliar ---
`%||%` <- function(a, b) if (is.null(a)) b else a

# --- Formata número no padrão brasileiro (vetorizada) ---
fmt_num <- function(x, dec = 0) {
  if (length(x) == 0) return(character(0))
  ifelse(
    is.na(x),
    "—",
    prettyNum(round(x, dec),
              big.mark     = ".",
              decimal.mark = ",",
              scientific   = FALSE,
              nsmall       = dec)
  )
}

# --- Formata percentual com sinal ---
fmt_pct <- function(x, dec = 1) {
  if (length(x) == 0 || is.na(x)) return("—")
  s <- sprintf(paste0("%+.", dec, "f"), x)
  s <- gsub("\\.", ",", s)
  paste0(s, "%")
}

# --- Normaliza código de município para 6 dígitos ---
normalizar_cod_municipio <- function(x) {
  x <- trimws(as.character(x))
  x[nchar(x) == 7] <- substr(x[nchar(x) == 7], 1, 6)
  x
}

# --- Extrai dia/mês/ano de DTOBITO (DDMMAAAA) ---
extrair_data_sim <- function(x) {
  s <- as.character(x)
  list(
    dia = as.integer(substr(s, 1, 2)),
    mes = as.integer(substr(s, 3, 4)),
    ano = as.integer(substr(s, 5, 8))
  )
}

# --- Adiciona colunas derivadas ao data.frame de óbitos ---
adicionar_derivadas_sim <- function(df) {
  d <- extrair_data_sim(df$dtobito)
  df$dia_obito    <- d$dia
  df$mes_obito    <- d$mes
  df$ano_obito    <- d$ano
  df$capitulo_cid <- substr(as.character(df$causabas), 1, 1)
  df
}

# --- Decodifica a idade codificada do SIM ---
decodificar_idade_sim <- function(idade_cod) {
  i <- suppressWarnings(as.integer(idade_cod))
  dplyr::case_when(
    is.na(i)                      ~ NA_real_,
    i < 100                       ~ 0,
    i >= 100 & i < 200            ~ (i - 100) / 365,
    i >= 200 & i < 300            ~ (i - 200) / 12,
    i >= 300 & i < 400            ~ (i - 300) / 52,
    i >= 400 & i < 500            ~ (i - 400),
    i >= 500                      ~ 100,
    TRUE                          ~ NA_real_
  )
}

# --- Normaliza CID para 3 caracteres ---
normalizar_cid3 <- function(cid) {
  cid <- toupper(trimws(as.character(cid)))
  cid <- gsub("\\.", "", cid)
  substr(cid, 1, 3)
}

# --- Atribui capítulo CID-10 ---
atribuir_capitulo_cid <- function(cid) {
  cid <- toupper(as.character(cid))
  letra  <- substr(cid, 1, 1)
  numero <- suppressWarnings(as.integer(substr(cid, 2, 3)))

  dplyr::case_when(
    letra %in% c("A", "B")            ~ "I - Infecciosas e parasitárias",
    letra == "C"                      ~ "II - Neoplasias",
    letra == "D" & numero <= 48       ~ "II - Neoplasias",
    letra == "D" & numero >= 50       ~ "III - Sangue e imunológicas",
    letra == "E"                      ~ "IV - Endócrinas e metabólicas",
    letra == "F"                      ~ "V - Transtornos mentais",
    letra == "G"                      ~ "VI - Sistema nervoso",
    letra == "H" & numero <= 59       ~ "VII - Olho e anexos",
    letra == "H" & numero >= 60       ~ "VIII - Ouvido",
    letra == "I"                      ~ "IX - Aparelho circulatório",
    letra == "J"                      ~ "X - Aparelho respiratório",
    letra == "K"                      ~ "XI - Aparelho digestivo",
    letra == "L"                      ~ "XII - Pele e tecido subcutâneo",
    letra == "M"                      ~ "XIII - Sistema osteomuscular",
    letra == "N"                      ~ "XIV - Aparelho geniturinário",
    letra == "O"                      ~ "XV - Gravidez, parto e puerpério",
    letra == "P"                      ~ "XVI - Afecções perinatais",
    letra == "Q"                      ~ "XVII - Malformações congênitas",
    letra == "R"                      ~ "XVIII - Sintomas e sinais",
    letra %in% c("S", "T")            ~ "XIX - Lesões e envenenamentos",
    letra %in% c("V", "W", "X", "Y")  ~ "XX - Causas externas",
    letra == "Z"                      ~ "XXI - Fatores de saúde",
    TRUE                              ~ "Outros"
  )
}

# --- Abrevia nomes longos de causas ---
abreviar_causa <- function(x) {
  x <- as.character(x)
  subs <- c(
    "Agressão por meio de disparo de outra arma de fogo ou de arma não especificada" = "Agressão por arma de fogo",
    "Agressão por meio de disparo de arma de fogo de mão"                            = "Agressão por arma de fogo de mão",
    "Agressão por meios não especificados"                                            = "Agressão (meio NE)",
    "Agressão por outros meios especificados"                                         = "Agressão (outros meios)",
    "Diabetes mellitus não especificado"                                              = "Diabetes mellitus NE",
    "Broncopneumonia não especificada"                                                = "Broncopneumonia NE",
    "Pneumonia por microorganismo não especificado"                                   = "Pneumonia NE",
    "Insuficiência cardíaca congestiva"                                               = "Insuficiência cardíaca",
    "Hipertensão essencial (primária)"                                                = "Hipertensão essencial",
    "Acidente vascular cerebral, não especificado como hemorrágico ou isquêmico"      = "AVC não especificado",
    "Hemorragia intracerebral hemisférica subcortical"                                = "Hemorragia intracerebral",
    "Dissecção de artérias cerebrais, sem ruptura"                                    = "Dissecção de artérias cerebrais",
    "Neoplasia maligna da mama"                                                       = "Neopl. maligna da mama",
    "Neoplasia maligna do colo do útero"                                              = "Neopl. maligna colo do útero",
    "Neoplasia maligna dos brônquios e dos pulmões"                                   = "Neopl. brônquios/pulmões",
    "Neoplasia maligna da traqueia, dos brônquios e dos pulmões"                      = "Neopl. traqueia/brônquios",
    "Neoplasia maligna do estômago"                                                   = "Neopl. maligna do estômago",
    "Neoplasia maligna do fígado e vias biliares intra-hepáticas"                     = "Neopl. fígado/vias biliares",
    "Neoplasia maligna do cólon"                                                      = "Neopl. maligna do cólon",
    "Neoplasia maligna do pâncreas"                                                   = "Neopl. maligna do pâncreas",
    "Neoplasia maligna do esôfago"                                                    = "Neopl. maligna do esôfago",
    "Pedestre traumatizado em um acidente não-de-trânsito, envolvendo outros veículos a motor e os não especificados" = "Pedestre atropelado",
    "Pedestre traumatizado em um acidente de transporte"                              = "Pedestre atropelado",
    "Ocupante de automóvel traumatizado em um acidente de trânsito"                   = "Ocupante de automóvel",
    "Motociclista traumatizado em um acidente de trânsito"                            = "Motociclista",
    "Outras septicemias"                                                              = "Outras septicemias",
    "Septicemia não especificada"                                                     = "Septicemia NE",
    "Septicemia do recém-nascido devida a estreptococo do grupo B"                    = "Septicemia neonatal (grupo B)",
    "Lesão autoprovocada intencionalmente por enforcamento, estrangulamento e sufocação" = "Autoagressão por enforcamento",
    "Lesão autoprovocada intencionalmente por outros meios"                           = "Autoagressão (outros meios)",
    "Lesão autoprovocada intencionalmente"                                            = "Autoagressão",
    "Fatos ou eventos não especificados e intenção não determinada"                   = "Eventos NE / intenção indeterminada",
    "Exposição a fator não especificado"                                              = "Exposição a fator NE",
    "Doenças pelo HIV, resultando em doenças infecciosas e parasitárias"              = "Doenças pelo HIV",
    "Doença pelo HIV resultando em doenças infecciosas e parasitárias"                = "Doenças pelo HIV",
    "Infecção do trato urinário de localização não especificada"                      = "Infecção trato urinário NE",
    "Doença pulmonar obstrutiva crônica com infecção respiratória aguda do trato respiratório inferior" = "DPOC com infecção respiratória",
    "Doença pulmonar obstrutiva crônica"                                              = "DPOC",
    "Síndrome da imunodeficiência adquirida"                                          = "AIDS",
    "Síndrome da angústia respiratória do adulto"                                     = "SARA",
    "Síndrome da angústia respiratória aguda"                                         = "SARA",
    "Morte instantânea"                                                               = "Morte instantânea",
    "Morte sem assistência"                                                           = "Morte sem assistência",
    "Outras causas mal definidas e as não especificadas de mortalidade"               = "Outras causas mal definidas"
  )
  for (padrao in names(subs)) {
    x <- gsub(padrao, subs[[padrao]], x, fixed = TRUE)
  }
  x
}

# --- Gera mapa de faixas etárias ---
mapa_faixa_etaria <- function(faixa) {
  dplyr::case_when(
    faixa == "0"     ~ 0.5,
    faixa == "1-4"   ~ 2.5,
    faixa == "5-14"  ~ 9.5,
    faixa == "15-29" ~ 22,
    faixa == "30-44" ~ 37,
    faixa == "45-59" ~ 52,
    faixa == "60-69" ~ 64.5,
    faixa == "70+"   ~ 75,
    TRUE             ~ NA_real_
  )
}

message("Funções utilitárias carregadas.")