# ============================================================
# ui.R — Interface do usuário (responsiva)
# ============================================================

tema_araruama <- bs_theme(
  version      = 5,
  bootswatch   = "flatly",
  primary      = COR_AZUL_ESCURO,
  secondary    = COR_AZUL,
  success      = COR_VERDE,
  warning      = COR_LARANJA,
  bg           = COR_FUNDO,
  fg           = COR_TEXTO,
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter"),
  font_scale   = 1
)

css_linhas <- c(
  ".irs-bar, .irs-bar-edge { background: ", COR_AZUL_CLARO, " !important; border-color: ", COR_AZUL_CLARO, " !important; }",
  ".irs-slider { background: ", COR_LARANJA, " !important; border-color: ", COR_LARANJA, " !important; width: 18px; height: 18px; top: 22px; }",
  ".irs-from, .irs-to, .irs-single { background: ", COR_AZUL_CLARO, " !important; font-weight: 600; }",
  ".irs-from::before, .irs-to::before, .irs-single::before { border-top-color: ", COR_AZUL_CLARO, " !important; }",
  ".irs-line { background: rgba(255,255,255,0.25) !important; border-color: rgba(255,255,255,0.15) !important; }",
  ".irs-min, .irs-max { color: rgba(255,255,255,0.85) !important; background: rgba(255,255,255,0.1) !important; font-size: 11px; }",
  ".irs-grid-text { color: rgba(255,255,255,0.6) !important; }",
  ".card-body { padding: 1rem 1.25rem !important; }",
  ".card-header { padding: 0.6rem 1.25rem !important; font-weight: 600; font-size: 13px; color: ", COR_AZUL_ESCURO, "; }",
  ".card { overflow: hidden; }",
  ".navbar-brand img { border-radius: 4px; }",
  ".card-header { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }"
)

css_custom <- tags$head(tags$style(HTML(paste(css_linhas, collapse = " "))))

card_kpi <- function(titulo, valor_ui, sub_ui) {
  card(
    card_header(titulo),
    div(
      style = "padding: 4px 0;",
      valor_ui,
      p(sub_ui,
        style = paste0("color: ", COR_CINZA,
                       "; font-size: 12px; margin: 6px 0 0 0;"))
    )
  )
}

ui <- page_navbar(
  title = tags$span(
    style = paste0("display: flex; align-items: center; gap: 12px; ",
                   "color: white; font-weight: 700; letter-spacing: 0.5px;"),
    tags$img(src = "logo.png", height = "42px"),
    "Araruama — Painel Epidemiológico"
  ),
  theme = tema_araruama,
  fillable = TRUE,
  header = css_custom,

  sidebar = sidebar(
    width = 260,
    bg = COR_AZUL_ESCURO,
    fg = "#FFFFFF",
    open = "always",

    h5("Filtros", style = "color: white; font-weight: 600; margin-bottom: 20px;"),

    sliderInput(
      inputId = "anos",
      label   = tags$span("Período de análise",
                          style = "color: white; font-weight: 500;"),
      min = 1996, max = 2024, value = c(2000, 2024),
      step = 1, sep = "", ticks = FALSE
    ),

    tags$div(
      style = "color: rgba(255,255,255,0.75); font-size: 12px; margin-top: 20px; line-height: 1.5;",
      tags$p(icon("info-circle"), " O período se aplica ",
             strong("a todas as abas"), "."),
      tags$p("Taxa de mortalidade a partir de 2000.")
    ),

    tags$hr(style = "border-color: rgba(255,255,255,0.2); margin: 20px 0;"),

    tags$div(
      style = "color: rgba(255,255,255,0.7); font-size: 11px;",
      tags$p(strong("Fontes:"), "SIM/DATASUS, IBGE"),
      tags$p(strong("Dados consolidados:"), "até 2024")
    )
  ),

  # ============================================================
  # ABA 1: VISÃO GERAL
  # ============================================================
  nav_panel(
    title = "Visão Geral",
    icon  = icon("gauge-high"),

    # Cards responsivos (mínimo 240px, auto-quebram)
    layout_column_wrap(
      width = "240px",
      gap = "0.75rem",
      heights_equal = "row",
      card_kpi("População",
               h3(textOutput("card_pop"),
                  style = paste0("color: ", COR_AZUL_ESCURO, "; margin: 0; font-weight: 700;")),
               textOutput("card_pop_ano")),
      card_kpi(textOutput("card_obitos_label"),
               h3(textOutput("card_obitos"),
                  style = paste0("color: ", COR_AZUL, "; margin: 0; font-weight: 700;")),
               textOutput("card_obitos_var")),
      card_kpi(textOutput("card_taxa_label"),
               h3(textOutput("card_taxa"),
                  style = paste0("color: ", COR_VERDE, "; margin: 0; font-weight: 700;")),
               textOutput("card_taxa_var")),
      card_kpi(textOutput("card_apvp_label"),
               h3(textOutput("card_apvp"),
                  style = paste0("color: ", COR_ROXO, "; margin: 0; font-weight: 700;")),
               textOutput("card_apvp_var"))
    ),

    # Destaques responsivos (mínimo 380px)
    layout_column_wrap(
      width = "380px",
      gap = "1rem",
      heights_equal = "row",
      card(card_header(icon("list"), " Principais causas de morte"),
           uiOutput("destaque_top3_causas")),
      card(card_header(icon("hourglass-half"), " Principais causas em APVP"),
           uiOutput("destaque_top3_apvp"))
    )
  ),

  # ============================================================
  # ABA 2: MORTALIDADE
  # ============================================================
  nav_panel(
    title = "Mortalidade", icon = icon("heart-pulse"),
    card(card_header("Série histórica — óbitos e taxa por 1.000 habitantes"),
         plotlyOutput("grafico_mortalidade", height = "450px")),
    card(card_header("Pirâmide de mortalidade — distribuição por sexo e faixa etária"),
         plotlyOutput("piramide_mortalidade", height = "550px"))
  ),

  # ============================================================
  # ABA 3: CAUSAS
  # ============================================================
  nav_panel(
    title = "Causas", icon = icon("list"),

    # 2 cards lado a lado, mín. 450px
    layout_column_wrap(
      width = "450px",
      gap = "1rem",
      heights_equal = "row",
      card(card_header("Top 10 causas de morte"),
           plotlyOutput("grafico_top10", height = "600px")),
      card(card_header("Distribuição por capítulo CID-10"),
           plotlyOutput("grafico_capitulos", height = "600px"))
    ),

    # Novo gráfico: evolução das top 5 causas
    card(
      card_header("Evolução das 5 principais causas de morte"),
      plotlyOutput("grafico_top5_serie", height = "500px")
    )
  ),

  # ============================================================
  # ABA 4: APVP
  # ============================================================
  nav_panel(
    title = "APVP", icon = icon("hourglass-half"),

    layout_column_wrap(
      width = "380px",
      gap = "0.75rem",
      heights_equal = "row",
      card_kpi(tags$span(icon("person"), " Homens — média de anos perdidos por óbito prematuro"),
               h3(textOutput("apvp_media_homens"),
                  style = paste0("color: ", COR_AZUL, "; margin: 0; font-weight: 700;")),
               textOutput("apvp_media_homens_sub")),
      card_kpi(tags$span(icon("person-dress"), " Mulheres — média de anos perdidos por óbito prematuro"),
               h3(textOutput("apvp_media_mulheres"),
                  style = paste0("color: ", COR_ROSA, "; margin: 0; font-weight: 700;")),
               textOutput("apvp_media_mulheres_sub"))
    ),

    card(card_header("Série histórica — APVP por sexo (limite: 70 anos)"),
         plotlyOutput("grafico_apvp_serie_sexo", height = "450px")),

    layout_column_wrap(
      width = "450px",
      gap = "1rem",
      heights_equal = "row",
      card(card_header(icon("person"), " Top 10 causas por APVP — Homens"),
           plotlyOutput("grafico_apvp_causas_homens", height = "800px")),
      card(card_header(icon("person-dress"), " Top 10 causas por APVP — Mulheres"),
           plotlyOutput("grafico_apvp_causas_mulheres", height = "800px"))
    )
  ),

  # ============================================================
  # ABA 5: PRÉVIA
  # ============================================================
  nav_panel(
    title = tags$span(icon("triangle-exclamation"), " Prévia 2025-2026"),

    tags$div(
      style = paste0("background-color: ", COR_LARANJA, "; color: white; ",
                     "padding: 14px 18px; border-radius: 8px; margin-bottom: 16px; ",
                     "font-size: 13px; line-height: 1.5;"),
      tags$div(style = "display: flex; align-items: center; gap: 10px;",
               tags$span(icon("triangle-exclamation"), style = "font-size: 18px;"),
               tags$strong("Atenção: dados preliminares")),
      tags$div(style = "margin-top: 6px;",
               "Dados provenientes das prévias do TabNet/DATASUS. ",
               "Estão sujeitos a revisões substanciais nos próximos meses.",
               tags$br(),
               tags$small("Última consulta: 2025 (3ª prévia, ano completo) e 2026 (1ª prévia, até maio)."))
    ),

    layout_column_wrap(
      width = "380px",
      gap = "0.75rem",
      heights_equal = "row",
      card_kpi("Óbitos 2025 — ano completo",
               h3(textOutput("previa_2025_total"),
                  style = paste0("color: ", COR_LARANJA, "; margin: 0; font-weight: 700;")),
               "Fonte: TabNet/DATASUS"),
      card_kpi("Óbitos 2026 — parcial (até maio)",
               h3(textOutput("previa_2026_total"),
                  style = paste0("color: ", COR_LARANJA, "; margin: 0; font-weight: 700;")),
               "Fonte: TabNet/DATASUS")
    ),

    card(card_header("Distribuição mensal — prévia"),
         plotlyOutput("previa_mensal", height = "420px")),

    card(card_header("Distribuição por capítulo CID-10 — prévia"),
         plotlyOutput("previa_capitulos", height = "600px"))
  )
)