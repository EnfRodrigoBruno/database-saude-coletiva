# ============================================================
# global.R — Configuração global, carregamento de dados e helpers
# ============================================================

library(shiny)
library(bslib)
library(plotly)
library(arrow)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(cid10)

raiz <- "/home/inominado/Documentos/BancoEpidemio"

# --- Paleta oficial de Araruama ---
COR_AZUL_ESCURO <- "#0c3f6a"
COR_AZUL        <- "#2E7EC0"
COR_AZUL_CLARO  <- "#5FA8DC"
COR_VERDE       <- "#3DAA5A"
COR_LARANJA     <- "#F5A623"
COR_VERMELHO    <- "#C0392B"
COR_ROXO        <- "#8E44AD"
COR_ROSA        <- "#E91E63"
COR_FUNDO       <- "#F5F7FA"
COR_TEXTO       <- "#0c3f6a"
COR_CINZA       <- "#6B7280"

# --- Função: formata número no padrão brasileiro (vetorizada) ---
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

# --- Mapa de faixas etárias ---
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

# --- Verificação da raiz ---
if (!dir.exists(raiz)) stop("Diretório raiz não encontrado: ", raiz)

# --- Carregamento dos dados ---
cat("Carregando dados do banco...\n")

fato_obitos    <- read_parquet(file.path(raiz, "tratado/fato_obitos.parquet"))
populacao      <- read_parquet(file.path(raiz, "bruto/populacao/populacao_araruama_1996_2024.parquet"))
fato_redistrib <- read_parquet(file.path(raiz, "tratado/fato_obitos_redistribuido.parquet"))
ind_apvp_ano   <- read_parquet(file.path(raiz, "analytics/ind_apvp_por_ano.parquet"))
ind_capitulos  <- read_parquet(file.path(raiz, "analytics/ind_mortalidade_capitulo_cid.parquet"))

# --- Dicionário CID ---
cat_cid <- cid10::cid_categorias %>%
  select(cat, descricao) %>%
  mutate(cid = toupper(trimws(as.character(cat)))) %>%
  select(cid, descricao)

sub_cid <- cid10::cid_subcat %>%
  select(cid, descricao) %>%
  mutate(cid = toupper(trimws(substr(as.character(cid), 1, 3)))) %>%
  select(cid, descricao)

dic_cid <- bind_rows(cat_cid, sub_cid) %>% distinct(cid, .keep_all = TRUE)

fato_redistrib <- fato_redistrib %>%
  left_join(dic_cid, by = c("cid3" = "cid")) %>%
  mutate(causa_nome = ifelse(is.na(descricao), cid3, descricao)) %>%
  select(-descricao)

serie_mortalidade <- fato_obitos %>%
  count(ano_obito, name = "obitos") %>%
  filter(!is.na(ano_obito)) %>%
  left_join(populacao, by = c("ano_obito" = "year")) %>%
  mutate(taxa_1000 = obitos / pop * 1000) %>%
  arrange(ano_obito)

# --- Carregamento das prévias ---
cat("Carregando prévias 2025-2026...\n")
dir_prelim <- file.path(raiz, "bruto/sim_preliminar")

ler_previa <- function(ano, dim) {
  f <- file.path(dir_prelim, paste0("sim_araruama_", ano, "_", dim, ".parquet"))
  if (!file.exists(f)) return(NULL)
  d <- read_parquet(f)
  if (ncol(d) >= 2) {
    names(d)[1] <- "categoria"
    names(d)[2] <- "obitos"
  }
  d
}

previa <- list(
  total_2025 = ler_previa(2025, "total"),
  total_2026 = ler_previa(2026, "total"),
  mes_2025   = ler_previa(2025, "mes"),
  mes_2026   = ler_previa(2026, "mes"),
  cap_2025   = ler_previa(2025, "capitulo_cid"),
  cap_2026   = ler_previa(2026, "capitulo_cid")
)

cat("Dados carregados com sucesso.\n")