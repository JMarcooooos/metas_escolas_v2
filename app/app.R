library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(sf)
library(DT)
library(bslib)
library(bsicons)
library(shinyWidgets)
library(htmltools)

# ==============================================================================
# 1. CARREGAMENTO DE DADOS
# ==============================================================================

dados_mapa <- readRDS("dados_para_o_mapa.rds")
shapes     <- readRDS("mapa_shapes.rds")

shape_estado <- shapes$estado
shape_munis  <- shapes$municipios

lista_regionais <- sort(unique(dados_mapa$NM_REGIONAL))
lista_status    <- sort(unique(dados_mapa$CLASSIFICACAO))

# ==============================================================================
# 2. TEMA FUTURISTA CUSTOMIZADO
# ==============================================================================

tema_futurista <- bs_theme(
  version = 5,
  bg = "#0a0e27",
  fg = "#e0e7ff",
  primary = "#6366f1",
  secondary = "#8b5cf6",
  success = "#10b981",
  danger = "#ef4444",
  warning = "#f59e0b",
  info = "#06b6d4",
  base_font = font_google("Space Grotesk"),
  heading_font = font_google("Orbitron"),
  code_font = font_google("Fira Code")
)

# ==============================================================================
# 3. CSS INLINE FUTURISTA
# ==============================================================================

css_futurista <- "
/* Background animado com gradiente */
body {
  background: linear-gradient(135deg, #0a0e27 0%, #1a1f3a 50%, #0f172a 100%);
  background-attachment: fixed;
  font-family: 'Space Grotesk', sans-serif;
}

/* Efeito glassmorphism nos cards */
.card {
  background: rgba(255, 255, 255, 0.03) !important;
  backdrop-filter: blur(20px) !important;
  border: 1px solid rgba(255, 255, 255, 0.1) !important;
  border-radius: 20px !important;
  box-shadow: 0 8px 32px rgba(0, 0, 0, 0.4) !important;
  transition: all 0.3s ease !important;
}

.card:hover {
  transform: translateY(-5px);
  box-shadow: 0 12px 40px rgba(99, 102, 241, 0.3) !important;
}

.card-header {
  background: linear-gradient(135deg, rgba(99, 102, 241, 0.2), rgba(139, 92, 246, 0.2)) !important;
  border-bottom: 1px solid rgba(255, 255, 255, 0.1) !important;
  font-family: 'Orbitron', sans-serif !important;
  font-weight: 700 !important;
  letter-spacing: 1px !important;
  color: #e0e7ff !important;
  padding: 1rem 1.5rem !important;
  border-radius: 20px 20px 0 0 !important;
}

/* Sidebar futurista */
.bslib-sidebar-layout > .sidebar {
  background: linear-gradient(180deg, rgba(15, 23, 42, 0.95), rgba(30, 41, 59, 0.95)) !important;
  backdrop-filter: blur(20px) !important;
  border-right: 2px solid rgba(99, 102, 241, 0.3) !important;
  box-shadow: 4px 0 20px rgba(0, 0, 0, 0.3) !important;
}

.sidebar-title {
  font-family: 'Orbitron', sans-serif !important;
  font-size: 1.3rem !important;
  background: linear-gradient(135deg, #6366f1, #8b5cf6);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
  font-weight: 800 !important;
  letter-spacing: 2px !important;
  text-transform: uppercase;
  margin-bottom: 1.5rem !important;
}

/* Inputs modernos */
.form-control, .form-select {
  background: rgba(15, 23, 42, 0.6) !important;
  border: 1px solid rgba(99, 102, 241, 0.3) !important;
  border-radius: 12px !important;
  color: #e0e7ff !important;
  padding: 0.75rem 1rem !important;
  transition: all 0.3s ease !important;
}

.form-control:focus, .form-select:focus {
  background: rgba(15, 23, 42, 0.8) !important;
  border-color: #6366f1 !important;
  box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.2) !important;
  outline: none !important;
}

/* Labels dos inputs */
label {
  color: #cbd5e1 !important;
  font-weight: 600 !important;
  font-size: 0.9rem !important;
  margin-bottom: 0.5rem !important;
  letter-spacing: 0.5px !important;
}

/* Value boxes futuristas */
.bslib-value-box {
  background: linear-gradient(135deg, rgba(99, 102, 241, 0.15), rgba(139, 92, 246, 0.15)) !important;
  border: 1px solid rgba(99, 102, 241, 0.3) !important;
  border-radius: 16px !important;
  backdrop-filter: blur(10px) !important;
  transition: all 0.3s ease !important;
  position: relative;
  overflow: hidden;
}

.bslib-value-box::before {
  content: '';
  position: absolute;
  top: 0;
  left: -100%;
  width: 100%;
  height: 100%;
  background: linear-gradient(90deg, transparent, rgba(255, 255, 255, 0.1), transparent);
  transition: left 0.5s;
}

.bslib-value-box:hover::before {
  left: 100%;
}

.bslib-value-box:hover {
  transform: scale(1.05);
  box-shadow: 0 8px 30px rgba(99, 102, 241, 0.4) !important;
}

.bslib-value-box .value-box-value {
  font-family: 'Orbitron', sans-serif !important;
  font-size: 2.5rem !important;
  font-weight: 800 !important;
  background: linear-gradient(135deg, #6366f1, #a78bfa);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
}

.bslib-value-box .value-box-title {
  color: #cbd5e1 !important;
  font-size: 0.85rem !important;
  text-transform: uppercase;
  letter-spacing: 1.5px !important;
  font-weight: 600 !important;
}

/* Tema danger (vermelho) */
.bslib-value-box.bg-danger {
  background: linear-gradient(135deg, rgba(239, 68, 68, 0.15), rgba(220, 38, 38, 0.15)) !important;
  border-color: rgba(239, 68, 68, 0.4) !important;
}

.bg-danger .value-box-value {
  background: linear-gradient(135deg, #ef4444, #f87171) !important;
  -webkit-background-clip: text !important;
  -webkit-text-fill-color: transparent !important;
}

/* Tema success (verde) */
.bslib-value-box.bg-success {
  background: linear-gradient(135deg, rgba(16, 185, 129, 0.15), rgba(5, 150, 105, 0.15)) !important;
  border-color: rgba(16, 185, 129, 0.4) !important;
}

.bg-success .value-box-value {
  background: linear-gradient(135deg, #10b981, #34d399) !important;
  -webkit-background-clip: text !important;
  -webkit-text-fill-color: transparent !important;
}

/* Bootstrap-select (pickerInput) customizado */
.bootstrap-select .dropdown-toggle {
  background: rgba(15, 23, 42, 0.6) !important;
  border: 1px solid rgba(99, 102, 241, 0.3) !important;
  border-radius: 12px !important;
  color: #e0e7ff !important;
  padding: 0.75rem 1rem !important;
}

.bootstrap-select .dropdown-toggle:focus {
  border-color: #6366f1 !important;
  box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.2) !important;
}

.bootstrap-select .dropdown-menu {
  background: rgba(15, 23, 42, 0.98) !important;
  border: 1px solid rgba(99, 102, 241, 0.3) !important;
  border-radius: 12px !important;
  backdrop-filter: blur(20px) !important;
  box-shadow: 0 10px 40px rgba(0, 0, 0, 0.5) !important;
}

.bootstrap-select .dropdown-menu li a {
  color: #e0e7ff !important;
  transition: all 0.2s ease !important;
}

.bootstrap-select .dropdown-menu li a:hover {
  background: rgba(99, 102, 241, 0.2) !important;
}

.bootstrap-select .dropdown-menu li.selected a {
  background: linear-gradient(135deg, rgba(99, 102, 241, 0.3), rgba(139, 92, 246, 0.3)) !important;
}

/* Título principal */
.bslib-page-title {
  font-family: 'Orbitron', sans-serif !important;
  font-size: 1.8rem !important;
  font-weight: 900 !important;
  background: linear-gradient(135deg, #6366f1, #8b5cf6, #06b6d4);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
  letter-spacing: 2px !important;
  text-transform: uppercase;
  text-shadow: 0 0 30px rgba(99, 102, 241, 0.5);
  padding: 1rem 0 !important;
}

/* Separadores */
hr {
  border-color: rgba(99, 102, 241, 0.3) !important;
  margin: 1.5rem 0 !important;
  opacity: 0.5 !important;
}

/* Tabela DataTables */
.dataTables_wrapper {
  color: #e0e7ff !important;
}

table.dataTable {
  background: transparent !important;
  color: #e0e7ff !important;
}

table.dataTable thead th {
  background: linear-gradient(135deg, rgba(99, 102, 241, 0.2), rgba(139, 92, 246, 0.2)) !important;
  color: #e0e7ff !important;
  border-bottom: 2px solid rgba(99, 102, 241, 0.4) !important;
  font-weight: 700 !important;
  text-transform: uppercase;
  font-size: 0.85rem !important;
  letter-spacing: 1px !important;
}

table.dataTable tbody tr {
  background: rgba(15, 23, 42, 0.3) !important;
  border-bottom: 1px solid rgba(99, 102, 241, 0.1) !important;
  transition: all 0.2s ease !important;
}

table.dataTable tbody tr:hover {
  background: rgba(99, 102, 241, 0.15) !important;
  transform: scale(1.01);
}

table.dataTable tbody tr.selected {
  background: rgba(99, 102, 241, 0.3) !important;
}

.dataTables_info, .dataTables_paginate {
  color: #94a3b8 !important;
}

.paginate_button {
  background: rgba(99, 102, 241, 0.2) !important;
  border: 1px solid rgba(99, 102, 241, 0.3) !important;
  color: #e0e7ff !important;
  border-radius: 8px !important;
  margin: 0 3px !important;
}

.paginate_button:hover {
  background: rgba(99, 102, 241, 0.4) !important;
  border-color: #6366f1 !important;
}

.paginate_button.current {
  background: linear-gradient(135deg, #6366f1, #8b5cf6) !important;
  border-color: #6366f1 !important;
}

/* Leaflet popup customizado */
.leaflet-popup-content-wrapper {
  background: rgba(15, 23, 42, 0.95) !important;
  backdrop-filter: blur(20px) !important;
  border: 1px solid rgba(99, 102, 241, 0.4) !important;
  border-radius: 12px !important;
  color: #e0e7ff !important;
  box-shadow: 0 8px 32px rgba(0, 0, 0, 0.5) !important;
}

.leaflet-popup-tip {
  background: rgba(15, 23, 42, 0.95) !important;
}

/* Botão de reset do mapa */
.leaflet-control-easyButton {
  background: rgba(15, 23, 42, 0.9) !important;
  border: 1px solid rgba(99, 102, 241, 0.4) !important;
  border-radius: 8px !important;
  transition: all 0.3s ease !important;
}

.leaflet-control-easyButton:hover {
  background: rgba(99, 102, 241, 0.3) !important;
  transform: scale(1.1);
}

/* Controles do zoom */
.leaflet-control-zoom a {
  background: rgba(15, 23, 42, 0.9) !important;
  border: 1px solid rgba(99, 102, 241, 0.3) !important;
  color: #e0e7ff !important;
}

.leaflet-control-zoom a:hover {
  background: rgba(99, 102, 241, 0.3) !important;
}

/* Animação de pulse nos ícones */
@keyframes pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.7; }
}

.bslib-value-box .value-box-showcase {
  animation: pulse 2s infinite;
}

/* Texto muted */
.text-muted {
  color: #64748b !important;
  font-size: 0.8rem !important;
  font-style: italic;
}

/* Scrollbar customizada */
::-webkit-scrollbar {
  width: 10px;
  height: 10px;
}

::-webkit-scrollbar-track {
  background: rgba(15, 23, 42, 0.5);
  border-radius: 10px;
}

::-webkit-scrollbar-thumb {
  background: linear-gradient(135deg, #6366f1, #8b5cf6);
  border-radius: 10px;
}

::-webkit-scrollbar-thumb:hover {
  background: linear-gradient(135deg, #8b5cf6, #a78bfa);
}
"

# ==============================================================================
# 4. INTERFACE (UI)
# ==============================================================================

ui <- page_sidebar(
  title = tags$span(
    tags$i(class = "bi bi-stars", style = "margin-right: 10px;"),
    "SEDUC GO | Monitoramento Estratégico",
    tags$i(class = "bi bi-stars", style = "margin-left: 10px;")
  ),
  theme = tema_futurista,
  
  tags$head(
    tags$style(HTML(css_futurista))
  ),
  
  sidebar = sidebar(
    title = tags$div(
      class = "sidebar-title",
      tags$i(class = "bi bi-sliders", style = "margin-right: 8px;"),
      "Filtros de Análise"
    ),
    width = 340,
    
    selectInput(
      "filtro_etapa", 
      tags$span(tags$i(class = "bi bi-mortarboard-fill"), " Etapa de Ensino:"),
      choices = unique(dados_mapa$ETAPA)
    ),
    
    pickerInput(
      inputId = "filtro_regional",
      label = tags$span(tags$i(class = "bi bi-geo-alt-fill"), " Regionais (Multi-seleção):"),
      choices = lista_regionais,
      selected = lista_regionais,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 3",
        `count-selected-text` = "{0} Regionais",
        `none-selected-text` = "Nenhuma selecionada"
      )
    ),
    
    pickerInput(
      inputId = "filtro_classificacao",
      label = tags$span(tags$i(class = "bi bi-flag-fill"), " Status da Escola:"),
      choices = lista_status,
      selected = lista_status,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 2"
      )
    ),
    
    hr(),
    
    selectizeInput(
      "busca_escola", 
      tags$span(tags$i(class = "bi bi-search"), " Buscar Escola Específica:"),
      choices = NULL, 
      options = list(placeholder = 'Digite o nome da escola...')
    ),
    
    hr(),
    
    div(
      class = "text-muted", 
      style = "font-size: 0.75rem; text-align: center; padding: 10px;",
      tags$i(class = "bi bi-lightbulb-fill", style = "margin-right: 5px;"),
      "Use 'Select All' para seleção rápida"
    )
  ),
  
  layout_columns(
    fill = FALSE, 
    height = "140px",
    value_box(
      title = "Total Selecionado", 
      value = textOutput("kpi_total"), 
      showcase = bs_icon("buildings-fill"), 
      theme = "primary"
    ),
    value_box(
      title = "Atenção", 
      value = textOutput("kpi_risco"), 
      showcase = bs_icon("exclamation-triangle-fill"), 
      theme = "danger"
    ),
    value_box(
      title = "Sucesso", 
      value = textOutput("kpi_sucesso"), 
      showcase = bs_icon("check-circle-fill"), 
      theme = "success"
    )
  ),
  
  layout_columns(
    col_widths = c(12, 12), 
    row_heights = c(600, 400),
    card(
      full_screen = TRUE, 
      card_header(
        tags$i(class = "bi bi-map-fill", style = "margin-right: 8px;"),
        "Mapa Interativo de Previsões"
      ), 
      leafletOutput("mapa", height = "100%")
    ),
    card(
      card_header(
        tags$i(class = "bi bi-table", style = "margin-right: 8px;"),
        "Detalhamento das Escolas"
      ), 
      DTOutput("tabela")
    )
  )
)

# ==============================================================================
# 5. SERVIDOR
# ==============================================================================

server <- function(input, output, session) {
  
  # --- A. Filtragem Reativa ---
  dados_filtrados <- reactive({
    if (is.null(input$filtro_regional) || is.null(input$filtro_classificacao)) {
      return(dados_mapa[0,]) 
    }
    
    df <- dados_mapa %>% 
      filter(ETAPA == input$filtro_etapa) %>%
      filter(NM_REGIONAL %in% input$filtro_regional) %>%
      filter(CLASSIFICACAO %in% input$filtro_classificacao)
    
    return(df)
  })
  
  # --- B. Atualizar Busca ---
  observe({
    req(input$filtro_etapa)
    escolas <- unique(dados_mapa %>% filter(ETAPA == input$filtro_etapa) %>% pull(NM_ESCOLA))
    updateSelectizeInput(session, "busca_escola", choices = c("", sort(escolas)), server = TRUE)
  })
  
  # --- C. KPIs ---
  output$kpi_total   <- renderText({ format(nrow(dados_filtrados()), big.mark=".") })
  output$kpi_risco   <- renderText({ format(sum(grepl("VERMELHO", dados_filtrados()$CLASSIFICACAO)), big.mark=".") })
  output$kpi_sucesso <- renderText({ format(sum(grepl("VERDE", dados_filtrados()$CLASSIFICACAO)), big.mark=".") })
  
  # --- D. Mapa Base (Dark Mode) ---
  paleta <- colorNumeric(palette = "RdYlGn", domain = c(0, 1))
  
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%  # Tema escuro para combinar
      fitBounds(lng1 = -53.5, lat1 = -19.5, lng2 = -45.5, lat2 = -12.5) %>%
      addResetMapButton()
  })
  
  # --- E. Polígonos Inteligentes ---
  observeEvent(input$filtro_regional, {
    proxy <- leafletProxy("mapa")
    proxy %>% clearShapes()
    
    total_regionais_possiveis <- length(lista_regionais)
    n_selecionados <- length(input$filtro_regional)
    
    if (n_selecionados == total_regionais_possiveis || n_selecionados == 0) {
      proxy %>% addPolygons(
        data = shape_estado, 
        fillColor = "#1e293b", 
        color = "#6366f1", 
        weight = 3, 
        fillOpacity = 0.2,
        dashArray = "5, 5"
      )
    } else {
      munis_selecionados <- shape_munis %>% 
        filter(NM_REGIONAL %in% input$filtro_regional)
      
      if(nrow(munis_selecionados) > 0){
        bbox <- st_bbox(munis_selecionados)
        proxy %>% 
          flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>%
          addPolygons(
            data = munis_selecionados, 
            fillColor = "#6366f1", 
            color = "#8b5cf6", 
            weight = 2, 
            fillOpacity = 0.15,
            label = ~name_muni,
            highlightOptions = highlightOptions(
              weight = 3,
              color = "#a78bfa",
              fillOpacity = 0.3,
              bringToFront = TRUE
            )
          )
      }
    }
  }, ignoreNULL = FALSE)
  
  # --- F. Pontos (Escolas) com estilo neon ---
  observe({
    df <- dados_filtrados()
    if(nrow(df) == 0) {
      leafletProxy("mapa") %>% clearMarkers()
      return()
    }
    
    tem_cresc <- "CRESCIMENTO_LP" %in% names(df)
    popup_content <- paste0(
      "<div style='font-family: Space Grotesk, sans-serif; padding: 5px;'>",
      "<h4 style='margin: 0 0 8px 0; color: #a78bfa; font-family: Orbitron, sans-serif;'>", 
      df$NM_ESCOLA, "</h4>",
      "<span style='color: #94a3b8; font-size: 0.9rem;'>📍 ", df$NM_MUNICIPIO, "</span>",
      "<hr style='border-color: rgba(99, 102, 241, 0.3); margin: 10px 0;'>",
      "<div style='background: linear-gradient(135deg, rgba(99, 102, 241, 0.2), rgba(139, 92, 246, 0.2)); ",
      "padding: 10px; border-radius: 8px; margin-bottom: 8px;'>",
      "<b style='font-size: 1.3rem; color: #a78bfa;'>", round(df$Prob_Media * 100, 1), "%</b>",
      "<span style='color: #cbd5e1; font-size: 0.85rem;'> Probabilidade</span>",
      "</div>",
      "<div style='color: #e0e7ff;'><b>Status:</b> ", df$CLASSIFICACAO, "</div>",
      if(tem_cresc) paste0("<div style='color: #94a3b8; font-size: 0.85rem; margin-top: 8px;'>",
                           "<i>📈 Crescimento LP: ", round(df$CRESCIMENTO_LP, 2), "</i></div>") else "",
      "</div>"
    )
    
    leafletProxy("mapa", data = df) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~LONGITUDE, lat = ~LATITUDE,
        radius = 8, 
        color = "#ffffff", 
        weight = 2,
        fillColor = ~paleta(Prob_Media), 
        fillOpacity = 0.9,
        popup = popup_content, 
        layerId = ~CD_ESCOLA,
        label = ~paste0("🏫 ", NM_ESCOLA, " • ", round(Prob_Media*100,0), "%")
      )
  })
  
  # --- G. Interações ---
  observeEvent(input$busca_escola, {
    req(input$busca_escola)
    alvo <- dados_mapa %>% filter(NM_ESCOLA == input$busca_escola)
    if(nrow(alvo) > 0) {
      leafletProxy("mapa") %>%
        setView(lng = alvo$LONGITUDE, lat = alvo$LATITUDE, zoom = 16) %>%
        addPopups(
          lng = alvo$LONGITUDE, 
          lat = alvo$LATITUDE, 
          popup = paste0(
            "<div style='font-family: Orbitron, sans-serif; font-size: 1.1rem; color: #a78bfa;'>",
            "🎯 <b>", alvo$NM_ESCOLA, "</b></div>"
          )
        )
    }
  })
  
  observeEvent(input$tabela_rows_selected, {
    idx <- input$tabela_rows_selected
    escola <- dados_filtrados()[idx, ]
    leafletProxy("mapa") %>%
      setView(lng = escola$LONGITUDE, lat = escola$LATITUDE, zoom = 15) %>%
      addPopups(
        lng = escola$LONGITUDE, 
        lat = escola$LATITUDE, 
        popup = paste0(
          "<div style='font-family: Orbitron, sans-serif; color: #a78bfa;'>",
          "📍 <b>", escola$NM_ESCOLA, "</b></div>"
        )
      )
  })
  
  output$tabela <- renderDT({
    dados_filtrados() %>%
      select(NM_ESCOLA, NM_MUNICIPIO, NM_REGIONAL, CLASSIFICACAO, Prob_Media) %>%
      mutate(Prob_Media = paste0(round(Prob_Media * 100, 1), "%")) %>%
      datatable(
        selection = 'single', 
        options = list(
          pageLength = 10, 
          scrollX = TRUE,
          dom = 'frtip',
          language = list(
            search = "Buscar:",
            lengthMenu = "Mostrar _MENU_ registros",
            info = "Mostrando _START_ a _END_ de _TOTAL_ escolas",
            paginate = list(previous = "Anterior", `next` = "Próximo")
          )
        ), 
        rownames = FALSE
      ) %>%
      formatStyle(
        'CLASSIFICACAO',
        backgroundColor = styleEqual(
          c('VERDE', 'AMARELO', 'VERMELHO'),
          c('rgba(16, 185, 129, 0.2)', 'rgba(245, 158, 11, 0.2)', 'rgba(239, 68, 68, 0.2)')
        ),
        color = styleEqual(
          c('VERDE', 'AMARELO', 'VERMELHO'),
          c('#10b981', '#f59e0b', '#ef4444')
        ),
        fontWeight = 'bold'
      )
  })
}

shinyApp(ui, server)