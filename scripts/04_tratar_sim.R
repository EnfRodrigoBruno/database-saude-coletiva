# ============================================================
# 04_tratar_sim.R — Consolida SIM e gera fato_obitos.parquet
# ============================================================

if (!exists("raiz")) raiz <- "/home/inominado/Documentos/BancoEpidemio"
source(file.path(raiz, "scripts", "00_setup.R"))
source(file.path(raiz, "scripts", "01_utils.R"))

arquivos <- list.files(dir_bruto_sim, pattern = "\\.parquet$", full.names = TRUE)

fato_obitos <- arquivos %>%
  map_dfr(read_parquet) %>%
  adicionar_derivadas_sim()

cat("Total de óbitos:", nrow(fato_obitos), "\n")
cat("Colunas:", ncol(fato_obitos), "\n")

write_parquet(fato_obitos, file.path(dir_tratado, "fato_obitos.parquet"))
cat("Salvo em tratado/fato_obitos.parquet\n")