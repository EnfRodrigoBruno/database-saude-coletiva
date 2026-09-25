# ============================================================
# server.R — Lógica reativa
# ============================================================

server <- function(input, output, session) {

  anos_sel <- reactive(seq(input$anos[1], input$anos[2]))

  serie_filtrada          <- reactive(serie_mortalidade %>% filter(ano_obito %in% anos_sel()))
  fato_redistrib_filtrado <- reactive(fato_redistrib %>% filter(ano_obito %in% anos_sel()))
  ind_apvp_filtrado       <- reactive(ind_apvp_ano %>% filter(ano_obito %in% anos_sel()))
  ind_capitulos_filtrado  <- reactive(ind_capitulos %>% filter(ano_obito %in% anos_sel()))

  # --- Cards KPI ---
  output$card_pop <- renderText({ "129.671" })
  output$card_pop_ano <- renderText({ "Censo 2022 / IBGE" })

  output$card_obitos_label <- renderText(paste0("Óbitos (", max(anos_sel()), ")"))
  output$card_obitos <- renderText({
    n <- serie_filtrada() %>% filter(ano_obito == max(anos_sel())) %>% pull(obitos)
    if (length(n) == 0) "—" else fmt_num(n)
  })
  output$card_obitos_var <- renderText({
    d <- serie_filtrada()
    atual    <- d %>% filter(ano_obito == max(anos_sel()))     %>% pull(obitos)
    anterior <- d %>% filter(ano_obito == max(anos_sel()) - 1) %>% pull(obitos)
    if (length(atual) == 0 || length(anterior) == 0) return("vs ano anterior")
    paste0(fmt_pct((atual - anterior) / anterior * 100), " vs ano anterior")
  })

  output$card_taxa_label <- renderText(paste0("Taxa mortalidade (", max(anos_sel()), ")"))
  output$card_taxa <- renderText({
    t <- serie_filtrada() %>% filter(ano_obito == max(anos_sel())) %>% pull(taxa_1000)
    if (length(t) == 0 || is.na(t)) "—" else fmt_num(t, dec = 2)
  })
  output$card_taxa_var <- renderText({
    d <- serie_filtrada() %>% filter(!is.na(taxa_1000))
    atual    <- d %>% filter(ano_obito == max(anos_sel()))     %>% pull(taxa_1000)
    anterior <- d %>% filter(ano_obito == max(anos_sel()) - 1) %>% pull(taxa_1000)
    if (length(atual) == 0 || length(anterior) == 0) return("por 1.000 hab.")
    paste0(fmt_pct((atual - anterior) / anterior * 100), " vs ano anterior • por 1.000 hab.")
  })

  output$card_apvp_label <- renderText(paste0("APVP (", max(anos_sel()), ")"))
  output$card_apvp <- renderText({
    d <- ind_apvp_filtrado() %>% filter(ano_obito == max(anos_sel())) %>% pull(apvp_total)
    if (length(d) == 0) "—" else fmt_num(d)
  })
  output$card_apvp_var <- renderText({
    d <- ind_apvp_filtrado()
    atual    <- d %>% filter(ano_obito == max(anos_sel()))     %>% pull(apvp_total)
    anterior <- d %>% filter(ano_obito == max(anos_sel()) - 1) %>% pull(apvp_total)
    if (length(atual) == 0 || length(anterior) == 0) return("anos de vida")
    paste0(fmt_pct((atual - anterior) / anterior * 100), " vs ano anterior")
  })

  # --- Destaques ---
  output$destaque_top3_causas <- renderUI({
    d <- fato_redistrib_filtrado() %>%
      filter(!is.na(cid3), cid3 != "") %>%
      group_by(cid3, causa_nome) %>%
      summarise(obitos = sum(obitos), .groups = "drop") %>%
      slice_max(obitos, n = 3) %>%
      arrange(desc(obitos))
    if (nrow(d) == 0) return(p("Sem dados para o período selecionado."))
    tagList(lapply(seq_len(nrow(d)), function(i) {
      borda <- if (i < nrow(d)) "border-bottom: 1px solid #eee;" else ""
      tags$div(
        style = paste0("display: flex; justify-content: space-between; ",
                       "align-items: center; padding: 14px 0; ", borda),
        tags$div(
          style = "display: flex; align-items: center; gap: 12px; flex: 1; min-width: 0;",
          tags$span(paste0(i, "º"),
                    style = paste0("color: ", COR_AZUL_ESCURO, "; ",
                                   "font-weight: 700; font-size: 20px; min-width: 32px;")),
          tags$span(d$causa_nome[i], style = "font-size: 14px; line-height: 1.3;")
        ),
        tags$span(fmt_num(d$obitos[i]),
                  style = paste0("color: ", COR_AZUL, "; font-weight: 700; ",
                                 "font-size: 20px; margin-left: 12px; white-space: nowrap;"))
      )
    }))
  })

  output$destaque_top3_apvp <- renderUI({
    d <- fato_redistrib_filtrado() %>%
      mutate(idade_media = mapa_faixa_etaria(faixa_etaria)) %>%
      filter(!is.na(idade_media), idade_media < LIMITE_APVP,
             !is.na(cid3), cid3 != "") %>%
      mutate(apvp = obitos * (LIMITE_APVP - idade_media)) %>%
      group_by(cid3, causa_nome) %>%
      summarise(apvp_total = sum(apvp), .groups = "drop") %>%
      slice_max(apvp_total, n = 3) %>%
      arrange(desc(apvp_total))
    if (nrow(d) == 0) return(p("Sem dados para o período selecionado."))
    tagList(lapply(seq_len(nrow(d)), function(i) {
      borda <- if (i < nrow(d)) "border-bottom: 1px solid #eee;" else ""
      tags$div(
        style = paste0("display: flex; justify-content: space-between; ",
                       "align-items: center; padding: 14px 0; ", borda),
        tags$div(
          style = "display: flex; align-items: center; gap: 12px; flex: 1; min-width: 0;",
          tags$span(paste0(i, "º"),
                    style = paste0("color: ", COR_AZUL_ESCURO, "; ",
                                   "font-weight: 700; font-size: 20px; min-width: 32px;")),
          tags$span(d$causa_nome[i], style = "font-size: 14px; line-height: 1.3;")
        ),
        tags$span(fmt_num(d$apvp_total[i]),
                  style = paste0("color: ", COR_VERMELHO, "; font-weight: 700; ",
                                 "font-size: 20px; margin-left: 12px; white-space: nowrap;"))
      )
    }))
  })

  # --- Mortalidade ---
  output$grafico_mortalidade <- renderPlotly({
    d <- serie_filtrada() %>% filter(!is.na(taxa_1000))
    plot_ly(d) %>%
      add_bars(x = ~ano_obito, y = ~obitos, name = "Óbitos",
               marker = list(color = "#a8c8e8"),
               hovertemplate = "<b>%{x}</b><br>Óbitos: %{y:,}<extra></extra>") %>%
      add_trace(x = ~ano_obito, y = ~taxa_1000, name = "Taxa por 1.000 hab.",
                yaxis = "y2", type = "scatter", mode = "lines+markers",
                line = list(color = COR_VERMELHO, width = 2.5),
                marker = list(color = COR_VERMELHO, size = 8),
                hovertemplate = "<b>%{x}</b><br>Taxa: %{y:.2f}<extra></extra>") %>%
      layout(
        xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
        yaxis = list(title = "Óbitos", showgrid = TRUE, gridcolor = "#eee",
                     zeroline = FALSE, titlefont = list(color = COR_AZUL)),
        yaxis2 = list(title = "Taxa por 1.000 hab.", overlaying = "y", side = "right",
                      showgrid = FALSE, zeroline = FALSE,
                      titlefont = list(color = COR_VERMELHO),
                      tickfont  = list(color = COR_VERMELHO)),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
        hovermode = "x unified",
        margin = list(l = 70, r = 80, t = 20, b = 60),
        paper_bgcolor = "white", plot_bgcolor = "white"
      ) %>% config(displayModeBar = FALSE)
  })

  output$piramide_mortalidade <- renderPlotly({
    faixas_ordem <- c("0", "1-4", "5-14", "15-29", "30-44", "45-59", "60-69", "70+")
    d <- fato_obitos %>%
      filter(ano_obito %in% anos_sel(), !is.na(idade)) %>%
      mutate(
        idade_num = decodificar_idade_sim(idade),
        fx = cut(idade_num, breaks = c(-0.01, 1, 5, 15, 30, 45, 60, 70, Inf),
                 labels = faixas_ordem, right = FALSE),
        sexo_label = case_when(sexo == "1" ~ "Masculino",
                               sexo == "2" ~ "Feminino",
                               TRUE ~ "Ignorado")
      ) %>%
      filter(!is.na(fx), sexo_label != "Ignorado") %>%
      count(fx, sexo_label, name = "obitos")

    d_completo <- expand.grid(fx = factor(faixas_ordem, levels = faixas_ordem),
                              sexo_label = c("Masculino", "Feminino"),
                              stringsAsFactors = FALSE) %>%
      left_join(d, by = c("fx", "sexo_label")) %>%
      mutate(obitos = ifelse(is.na(obitos), 0, obitos))

    d_masc <- d_completo %>% filter(sexo_label == "Masculino") %>%
      mutate(fx = factor(fx, levels = faixas_ordem)) %>% arrange(desc(fx))
    d_fem <- d_completo %>% filter(sexo_label == "Feminino") %>%
      mutate(fx = factor(fx, levels = faixas_ordem)) %>% arrange(desc(fx))

    max_valor <- max(c(d_masc$obitos, d_fem$obitos), na.rm = TRUE) * 1.15
    ticks <- pretty(c(0, max_valor), n = 5)

    plot_ly() %>%
      add_trace(data = d_masc, x = ~(-obitos), y = ~fx, type = "bar",
                orientation = "h", name = "Masculino",
                marker = list(color = COR_AZUL), customdata = ~obitos,
                hovertemplate = "<b>Masculino</b><br>%{y} anos<br>%{customdata:,} óbitos<extra></extra>") %>%
      add_trace(data = d_fem, x = ~obitos, y = ~fx, type = "bar",
                orientation = "h", name = "Feminino",
                marker = list(color = COR_ROSA), customdata = ~obitos,
                hovertemplate = "<b>Feminino</b><br>%{y} anos<br>%{customdata:,} óbitos<extra></extra>") %>%
      layout(
        barmode = "overlay", bargap = 0.15,
        xaxis = list(title = "",
                     tickvals = c(-rev(ticks[-1]), ticks[-1]),
                     ticktext = c(rev(ticks[-1]), ticks[-1]),
                     range = c(-max_valor, max_valor),
                     zeroline = TRUE, zerolinecolor = "#999", zerolinewidth = 1.5,
                     showgrid = TRUE, gridcolor = "#f0f0f0"),
        yaxis = list(title = "Faixa etária", showgrid = FALSE, titlefont = list(size = 12)),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.08,
                      font = list(size = 12)),
        annotations = list(
          list(x = 0.22, y = -0.13, xref = "paper", yref = "paper",
               text = "<b>◀ Homens</b>", showarrow = FALSE,
               font = list(color = COR_AZUL, size = 13), xanchor = "center"),
          list(x = 0.78, y = -0.13, xref = "paper", yref = "paper",
               text = "<b>Mulheres ▶</b>", showarrow = FALSE,
               font = list(color = COR_ROSA, size = 13), xanchor = "center")
        ),
        margin = list(l = 80, r = 40, t = 60, b = 70),
        paper_bgcolor = "white", plot_bgcolor = "white"
      ) %>% config(displayModeBar = FALSE)
  })

  # --- Causas ---
  output$grafico_top10 <- renderPlotly({
    d <- fato_redistrib_filtrado() %>%
      filter(!is.na(cid3), cid3 != "") %>%
      group_by(cid3, causa_nome) %>%
      summarise(obitos = sum(obitos), .groups = "drop") %>%
      slice_max(obitos, n = 10) %>%
      arrange(obitos) %>%
      mutate(causa_nome_abrev = abreviar_causa(causa_nome),
             causa_wrap = str_wrap(causa_nome_abrev, width = 42),
             causa_wrap = factor(causa_wrap, levels = causa_wrap),
             obitos_fmt = fmt_num(obitos, dec = 0),
             tooltip_txt = paste0("<b>", causa_nome, "</b><br>Óbitos: ", obitos_fmt))

    plot_ly(d, x = ~obitos, y = ~causa_wrap, type = "bar", orientation = "h",
            marker = list(color = COR_AZUL),
            text = ~obitos_fmt, textposition = "outside",
            textfont = list(size = 11, color = COR_AZUL_ESCURO),
            hoverinfo = "text", hovertext = ~tooltip_txt) %>%
      layout(
        xaxis = list(title = "Óbitos", showgrid = TRUE, gridcolor = "#eee",
                     zeroline = FALSE, range = c(0, max(d$obitos) * 1.15)),
        yaxis = list(title = "", showgrid = FALSE, tickfont = list(size = 11), automargin = TRUE),
        margin = list(l = 10, r = 40, t = 20, b = 60),
        paper_bgcolor = "white", plot_bgcolor = "white"
      ) %>% config(displayModeBar = FALSE)
  })

  output$grafico_capitulos <- renderPlotly({
    d <- ind_capitulos_filtrado() %>%
      group_by(capitulo_nome) %>%
      summarise(obitos = sum(obitos), .groups = "drop") %>%
      arrange(desc(obitos)) %>%
      mutate(obitos_fmt = fmt_num(obitos, dec = 0),
             tooltip_txt = paste0("<b>", capitulo_nome, "</b><br>Óbitos: ", obitos_fmt))

    plot_ly(d, labels = ~capitulo_nome, values = ~obitos, type = "pie", hole = 0.5,
            textinfo = "percent", textposition = "inside",
            textfont = list(size = 10, color = "white"),
            hoverinfo = "text", hovertext = ~tooltip_txt,
            marker = list(colors = c(
              COR_AZUL_ESCURO, COR_AZUL, COR_AZUL_CLARO,
              "#7FB3D5", "#A9CCE3", "#D4E6F1",
              COR_VERDE, "#7DCEA0", "#ABEBC6",
              COR_LARANJA, "#FAD7A0", "#FDEBD0",
              COR_VERMELHO, "#F1948A", "#F5B7B1",
              COR_ROXO, "#C39BD3", "#D7BDE2",
              COR_ROSA, "#F5B7D0", "#FADBD8"
            ))) %>%
      layout(showlegend = TRUE,
             legend = list(orientation = "v", font = list(size = 10),
                           x = 1.02, xanchor = "left", y = 0.5, yanchor = "middle"),
             margin = list(l = 20, r = 200, t = 20, b = 20),
             paper_bgcolor = "white") %>%
      config(displayModeBar = FALSE)
  })

  # --- APVP ---
  calcular_apvp_sexo <- function(df, agrupar_por = NULL) {
    d <- df %>%
      mutate(idade_media = mapa_faixa_etaria(faixa_etaria)) %>%
      filter(!is.na(idade_media), idade_media < LIMITE_APVP,
             sexo %in% c("Masculino", "Feminino"))

    if (!is.null(agrupar_por)) d <- d %>% group_by(across(all_of(agrupar_por)))
    else d <- d %>% group_by(sexo)

    d %>% summarise(
      apvp_total        = sum(obitos * (LIMITE_APVP - idade_media)),
      obitos_prematuros = sum(obitos),
      .groups = "drop"
    ) %>% mutate(media_anos = apvp_total / obitos_prematuros)
  }

  apvp_media_sexo <- reactive(calcular_apvp_sexo(fato_redistrib_filtrado()))
  apvp_serie_sexo <- reactive(calcular_apvp_sexo(fato_redistrib_filtrado(),
                                                 agrupar_por = c("ano_obito", "sexo")))
  apvp_causas_sexo <- reactive({
    fato_redistrib_filtrado() %>%
      mutate(idade_media = mapa_faixa_etaria(faixa_etaria)) %>%
      filter(!is.na(idade_media), idade_media < LIMITE_APVP,
             !is.na(cid3), cid3 != "",
             sexo %in% c("Masculino", "Feminino")) %>%
      mutate(apvp = obitos * (LIMITE_APVP - idade_media)) %>%
      group_by(sexo, cid3, causa_nome) %>%
      summarise(apvp_total = sum(apvp), .groups = "drop") %>%
      group_by(sexo) %>%
      slice_max(apvp_total, n = 10) %>%
      ungroup() %>%
      mutate(causa_abrev = abreviar_causa(causa_nome),
             causa_wrap = str_wrap(causa_abrev, width = 35),
             apvp_fmt = fmt_num(apvp_total, dec = 0),
             tooltip_txt = paste0("<b>", causa_nome, "</b><br>APVP: ", apvp_fmt, " anos"))
  })

  output$apvp_media_homens <- renderText({
    d <- apvp_media_sexo() %>% filter(sexo == "Masculino")
    if (nrow(d) == 0) return("—")
    paste0(fmt_num(d$media_anos, dec = 1), " anos")
  })
  output$apvp_media_homens_sub <- renderText({
    d <- apvp_media_sexo() %>% filter(sexo == "Masculino")
    if (nrow(d) == 0) return("sem dados")
    paste0(fmt_num(d$obitos_prematuros, dec = 0), " óbitos prematuros • APVP total: ",
           fmt_num(d$apvp_total, dec = 0))
  })
  output$apvp_media_mulheres <- renderText({
    d <- apvp_media_sexo() %>% filter(sexo == "Feminino")
    if (nrow(d) == 0) return("—")
    paste0(fmt_num(d$media_anos, dec = 1), " anos")
  })
  output$apvp_media_mulheres_sub <- renderText({
    d <- apvp_media_sexo() %>% filter(sexo == "Feminino")
    if (nrow(d) == 0) return("sem dados")
    paste0(fmt_num(d$obitos_prematuros, dec = 0), " óbitos prematuros • APVP total: ",
           fmt_num(d$apvp_total, dec = 0))
  })

  output$grafico_apvp_serie_sexo <- renderPlotly({
    d <- apvp_serie_sexo() %>%
      mutate(sexo = factor(sexo, levels = c("Masculino", "Feminino")),
             apvp_fmt = fmt_num(apvp_total, dec = 0),
             tooltip_txt = paste0("<b>", ano_obito, " — ", sexo, "</b><br>APVP: ",
                                  apvp_fmt, " anos<br>Óbitos prematuros: ",
                                  fmt_num(obitos_prematuros, dec = 0),
                                  "<br>Média por óbito: ", fmt_num(media_anos, dec = 1), " anos"))
    plot_ly(d, x = ~ano_obito, y = ~apvp_total, color = ~sexo, type = "bar",
            colors = c(COR_AZUL, COR_ROSA), hoverinfo = "text", hovertext = ~tooltip_txt) %>%
      layout(barmode = "group", bargap = 0.2,
             xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
             yaxis = list(title = "APVP (anos)", showgrid = TRUE, gridcolor = "#eee", zeroline = FALSE),
             legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
             margin = list(l = 80, r = 30, t = 20, b = 60),
             paper_bgcolor = "white", plot_bgcolor = "white") %>%
      config(displayModeBar = FALSE)
  })

  render_apvp_causas_sexo <- function(sexo_alvo, cor) {
    renderPlotly({
      d <- apvp_causas_sexo() %>% filter(sexo == sexo_alvo) %>%
        arrange(apvp_total) %>% mutate(causa_wrap = factor(causa_wrap, levels = causa_wrap))
      if (nrow(d) == 0) return(plotly_empty(type = "bar") %>%
                                 layout(title = "Sem dados no período selecionado"))
      plot_ly(d, x = ~apvp_total, y = ~causa_wrap, type = "bar", orientation = "h",
              marker = list(color = cor), text = ~apvp_fmt, textposition = "outside",
              textfont = list(size = 10, color = COR_AZUL_ESCURO),
              hoverinfo = "text", hovertext = ~tooltip_txt) %>%
        layout(
          xaxis = list(title = "APVP (anos)", showgrid = TRUE, gridcolor = "#eee",
                       zeroline = FALSE, range = c(0, max(d$apvp_total) * 1.20),
                       tickfont = list(size = 10)),
          yaxis = list(title = "", showgrid = FALSE, tickfont = list(size = 10), automargin = TRUE),
          margin = list(l = 10, r = 60, t = 20, b = 60),
          paper_bgcolor = "white", plot_bgcolor = "white"
        ) %>% config(displayModeBar = FALSE)
    })
  }

  output$grafico_apvp_causas_homens   <- render_apvp_causas_sexo("Masculino", COR_AZUL)
  output$grafico_apvp_causas_mulheres <- render_apvp_causas_sexo("Feminino",  COR_ROSA)

  # --- Prévia ---
  output$previa_2025_total <- renderText({
    if (!is.null(previa$total_2025)) fmt_num(previa$total_2025$obitos[1]) else "—"
  })
  output$previa_2026_total <- renderText({
    if (!is.null(previa$total_2026)) fmt_num(previa$total_2026$obitos[1]) else "—"
  })

  output$previa_mensal <- renderPlotly({
    meses_ordem <- c("Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho",
                     "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
    df <- bind_rows(
      if (!is.null(previa$mes_2025)) previa$mes_2025 %>% mutate(ano = "2025") else NULL,
      if (!is.null(previa$mes_2026)) previa$mes_2026 %>% mutate(ano = "2026") else NULL
    ) %>%
      mutate(categoria = factor(categoria, levels = meses_ordem),
             ano = factor(ano, levels = c("2025", "2026")),
             obitos_fmt = fmt_num(obitos, dec = 0),
             tooltip_txt = paste0("<b>", categoria, "</b><br>Ano: ", ano,
                                  "<br>Óbitos: ", obitos_fmt))
    plot_ly(df, x = ~categoria, y = ~obitos, color = ~ano, type = "bar",
            colors = c(COR_LARANJA, "#FAD7A0"),
            hoverinfo = "text", hovertext = ~tooltip_txt) %>%
      layout(barmode = "group", bargap = 0.25,
             xaxis = list(title = "", tickangle = -35, tickfont = list(size = 11), showgrid = FALSE),
             yaxis = list(title = "Óbitos", showgrid = TRUE, gridcolor = "#eee"),
             legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.35),
             margin = list(l = 60, r = 20, t = 20, b = 100),
             paper_bgcolor = "white", plot_bgcolor = "white") %>%
      config(displayModeBar = FALSE)
  })

  output$previa_capitulos <- renderPlotly({
    df <- bind_rows(
      if (!is.null(previa$cap_2025)) previa$cap_2025 %>% mutate(ano = "2025") else NULL,
      if (!is.null(previa$cap_2026)) previa$cap_2026 %>% mutate(ano = "2026") else NULL
    ) %>%
      mutate(categoria_abrev = str_trunc(categoria, 60, side = "right"),
             ano = factor(ano, levels = c("2025", "2026")),
             obitos_fmt = fmt_num(obitos, dec = 0),
             tooltip_txt = paste0("<b>", categoria, "</b><br>Ano: ", ano,
                                  "<br>Óbitos: ", obitos_fmt))
    plot_ly(df, x = ~obitos, y = ~categoria_abrev, color = ~ano, type = "bar",
            orientation = "h", colors = c(COR_LARANJA, "#FAD7A0"),
            hoverinfo = "text", hovertext = ~tooltip_txt) %>%
      layout(barmode = "group", bargap = 0.2,
             xaxis = list(title = "Óbitos", showgrid = TRUE, gridcolor = "#eee"),
             yaxis = list(title = "", showgrid = FALSE, tickfont = list(size = 11), automargin = TRUE),
             legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1),
             margin = list(l = 10, r = 30, t = 20, b = 60),
             paper_bgcolor = "white", plot_bgcolor = "white") %>%
      config(displayModeBar = FALSE)
  })
  
  # ============================================================
  # CAUSAS — Série das 5 principais (nova)
  # ============================================================
  output$grafico_top5_serie <- renderPlotly({
    
    # Top 5 causas do período filtrado
    top5_cids <- fato_redistrib_filtrado() %>%
      filter(!is.na(cid3), cid3 != "") %>%
      group_by(cid3, causa_nome) %>%
      summarise(obitos = sum(obitos), .groups = "drop") %>%
      slice_max(obitos, n = 5) %>%
      pull(cid3)
    
    # Série ano a ano dessas 5
    d <- fato_redistrib_filtrado() %>%
      filter(cid3 %in% top5_cids, !is.na(ano_obito)) %>%
      group_by(ano_obito, cid3, causa_nome) %>%
      summarise(obitos = sum(obitos), .groups = "drop") %>%
      mutate(
        causa_abrev = abreviar_causa(causa_nome),
        obitos_fmt = fmt_num(obitos, dec = 0),
        tooltip_txt = paste0("<b>", causa_abrev, "</b><br>",
                             ano_obito, ": ", obitos_fmt, " óbitos")
      )
    
    plot_ly(
      d,
      x = ~ano_obito,
      y = ~obitos,
      color = ~causa_abrev,
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      hovertext = ~tooltip_txt,
      marker = list(size = 7)
    ) %>%
      layout(
        xaxis = list(title = "Ano", showgrid = FALSE),
        yaxis = list(title = "Óbitos", showgrid = TRUE, gridcolor = "#eee"),
        legend = list(
          orientation = "h",
          x = 0.5, xanchor = "center",
          y = -0.25,
          font = list(size = 11)
        ),
        hovermode = "x unified",
        margin = list(l = 60, r = 30, t = 20, b = 100),
        paper_bgcolor = "white",
        plot_bgcolor  = "white"
      ) %>%
      config(displayModeBar = FALSE)
  })
}