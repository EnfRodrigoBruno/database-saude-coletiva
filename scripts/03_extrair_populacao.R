# ============================================================
# 03_extrair_populacao.R — População de Araruama (2000–2024)
# ============================================================

if (!exists("raiz")) raiz <- "/home/inominado/Documentos/BancoEpidemio"
source(file.path(raiz, "scripts", "00_setup.R"))

pop_araruama <- ibge_pop() %>%
  filter(code_muni == as.numeric(COD_IBGE_ARARUAMA_7),
         year %in% ANOS_POP) %>%
  arrange(year)

write_parquet(
  pop_araruama,
  file.path(dir_bruto_pop, "populacao_araruama_1996_2024.parquet")
)

cat("População salva:", nrow(pop_araruama), "anos\n")
print(pop_araruama)