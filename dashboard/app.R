# ============================================================
# app.R — Ponto de entrada
# ============================================================

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui, server)