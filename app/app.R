library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(sf)
library(DT)
library(bslib)
library(shinyWidgets)
library(htmltools)
library(shinyjs)
library(bsicons)

# ==============================================================================
# 1. CARREGAMENTO DE DADOS (MANTIDO INTACTO)
# ==============================================================================

if (file.exists("dados_para_o_mapa.rds")) {
  dados_mapa <- readRDS("dados_para_o_mapa.rds")
  shapes     <- readRDS("mapa_shapes.rds")
} else {
  dados_mapa <- readRDS("app/dados_para_o_mapa.rds")
  shapes     <- readRDS("app/mapa_shapes.rds")
}

shape_estado <- shapes$estado
shape_munis  <- shapes$municipios

lista_regionais <- sort(unique(dados_mapa$NM_REGIONAL))
lista_status    <- sort(unique(dados_mapa$CLASSIFICACAO))

# ==============================================================================
# 2. CSS DINÂMICO (COM CORREÇÃO DE SCROLLBAR)
# ==============================================================================

css_dinamico <- "
:root {
  --transition-speed: 0.3s;
}

/* --- DEFINIÇÃO DAS CORES (MODO ESCURO) --- */
body.dark-mode {
  --bg-body: #0f172a;
  --bg-sidebar: rgba(15, 23, 42, 0.95);
  --bg-card: rgba(30, 41, 59, 0.6);
  --border-color: rgba(255, 255, 255, 0.15);
  --text-primary: #f8fafc;        
  --text-secondary: #94a3b8;      
  --input-bg: rgba(15, 23, 42, 0.8);
  --accent: #a78bfa;              
  --table-hover: rgba(139, 92, 246, 0.15);
  
  /* Cores da Scrollbar Dark */
  --sb-track: #0f172a;
  --sb-thumb: #475569;
  --sb-thumb-hover: #a78bfa;
}

/* --- DEFINIÇÃO DAS CORES (MODO CLARO) --- */
body.light-mode {
  --bg-body: #f8fafc;
  --bg-sidebar: #ffffff;
  --bg-card: #ffffff;
  --border-color: #cbd5e1;
  --text-primary: #1e293b;        
  --text-secondary: #475569;      
  --input-bg: #f1f5f9;
  --accent: #6366f1;              
  --table-hover: rgba(99, 102, 241, 0.1);
  
  /* Cores da Scrollbar Light */
  --sb-track: #e2e8f0;
  --sb-thumb: #94a3b8;
  --sb-thumb-hover: #6366f1;
}

/* --- APLICAÇÃO GLOBAL --- */
body {
  background-color: var(--bg-body) !important;
  color: var(--text-primary) !important;
  font-family: 'Inter', sans-serif;
  transition: background-color var(--transition-speed), color var(--transition-speed);
}

/* --- SCROLLBARS (Forçando aplicação em tudo) --- */
/* Funciona no Chrome, Edge, Safari */
*::-webkit-scrollbar {
  width: 10px;
  height: 10px;
  background-color: var(--sb-track);
}

*::-webkit-scrollbar-track {
  background-color: var(--sb-track);
  border-radius: 4px;
}

*::-webkit-scrollbar-thumb {
  background-color: var(--sb-thumb);
  border-radius: 6px;
  border: 2px solid var(--sb-track);
}

*::-webkit-scrollbar-thumb:hover {
  background-color: var(--sb-thumb-hover);
}

/* --- FORÇAR CORES NOS TÍTULOS E LABELS --- */
.control-label, label, .shiny-input-container label {
  color: var(--text-primary) !important;
  font-weight: 700 !important;
  transition: color var(--transition-speed);
}

h1, h2, h3, h4, h5, h6 {
  color: var(--text-primary) !important;
  transition: color var(--transition-speed);
}
.dynamic-subtitle {
  color: var(--accent) !important;
  font-weight: 600;
  letter-spacing: 1px;
}

/* Legenda da Caixa de Modo Escuro */
.material-switch label {
  color: var(--text-primary) !important;
}

/* --- SIDEBAR --- */
.bslib-sidebar-layout > .sidebar {
  background-color: var(--bg-sidebar) !important;
  border-right: 1px solid var(--border-color) !important;
}

.sidebar-title {
  background: linear-gradient(90deg, var(--accent), #06b6d4);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  font-family: 'Outfit', sans-serif !important;
  font-weight: 800 !important;
}

/* --- CARDS --- */
.card, .bslib-value-box {
  background-color: var(--bg-card) !important;
  border: 1px solid var(--border-color) !important;
  backdrop-filter: blur(10px);
  color: var(--text-primary) !important;
}

.card-header {
  border-bottom: 1px solid var(--border-color) !important;
  color: var(--text-primary) !important;
  font-weight: 700;
}

/* --- INPUTS E BOTÕES --- */
.form-control, .selectize-input, .btn-light {
  background-color: var(--input-bg) !important;
  color: var(--text-primary) !important;
  border: 1px solid var(--border-color) !important;
}

.filter-option-inner-inner, .dropdown-toggle {
  color: var(--text-primary) !important;
}

.dropdown-menu {
  background-color: var(--bg-sidebar) !important;
  border: 1px solid var(--border-color) !important;
}
.dropdown-menu li a {
  color: var(--text-primary) !important;
}
.dropdown-menu li a:hover {
  background-color: var(--accent) !important;
  color: #fff !important;
}

/* --- TABELA (DATATABLES) --- */
.dataTables_wrapper {
  color: var(--text-primary) !important;
}
table.dataTable tbody tr {
  background-color: transparent !important;
  color: var(--text-primary) !important;
}
table.dataTable tbody tr:hover {
  background-color: var(--table-hover) !important;
}
.dataTables_length select, .dataTables_filter input {
  background-color: var(--input-bg) !important;
  color: var(--text-primary) !important;
  border: 1px solid var(--border-color) !important;
}
.paginate_button {
  color: var(--text-primary) !important;
}
.paginate_button.current {
  background: var(--accent) !important;
  color: #fff !important;
  border: none;
}

/* --- VALUE BOXES ICONS --- */
.bslib-value-box .value-box-showcase svg {
  fill: var(--accent) !important;
}
.bslib-value-box .value-box-title {
  color: var(--text-secondary) !important;
}
.bslib-value-box .value-box-value {
  color: var(--text-primary) !important;
}
"

# ==============================================================================
# 3. INTERFACE (UI)
# ==============================================================================

ui <- page_fillable(
  title = "SEDUC GO | Intelligence Platform",
  theme = bs_theme(version = 5, primary = "#8b5cf6"),
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.0/font/bootstrap-icons.css"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Outfit:wght@400;700;800&family=Inter:wght@400;600&display=swap"),
    tags$style(HTML(css_dinamico)),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggle_theme_js', function(mode) {
        if (mode === 'dark') {
          document.body.classList.remove('light-mode');
          document.body.classList.add('dark-mode');
        } else {
          document.body.classList.remove('dark-mode');
          document.body.classList.add('light-mode');
        }
      });
      // Inicia em Dark Mode
      $(document).ready(function() {
        document.body.classList.add('dark-mode');
      });
    "))
  ),
  
  useShinyjs(),
  
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      
      div(
        class = "sidebar-title",
        style = "margin-bottom: 20px; font-size: 1.3rem;",
        tags$i(class = "bi bi-grid-fill", style = "margin-right: 10px;"),
        "CONTROL CENTER"
      ),
      
      selectInput("filtro_etapa", "Etapa de Ensino", choices = unique(dados_mapa$ETAPA)),
      
      pickerInput(
        inputId = "filtro_regional",
        label = "Regionais",
        choices = lista_regionais,
        selected = lista_regionais,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      
      pickerInput(
        inputId = "filtro_classificacao",
        label = "Status",
        choices = lista_status,
        selected = lista_status,
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      
      hr(style="border-color: var(--border-color); opacity: 0.5;"),
      
      selectizeInput("busca_escola", "Buscar Unidade", choices = NULL, options = list(placeholder = 'Nome da escola...')),
      
      hr(style="border-color: var(--border-color); opacity: 0.5;"),
      
      # --- CAIXA DE MODO ESCURO (MODIFICADA: Sem ícone, sem texto "Aparência") ---
      div(
        style = "background: var(--bg-card); padding: 15px; border-radius: 12px; border: 1px solid var(--border-color);",
        # O switch ocupa toda a largura agora para ficar elegante
        materialSwitch(inputId = "dark_mode", label = "Modo Escuro", value = TRUE, status = "primary", right = TRUE, width = "100%")
      ),
      
      div(style = "height: 20px;"),
      
      actionButton("reset_filters", "Resetar Filtros", icon = icon("rotate-right"), class = "btn-outline-primary w-100")
    ),
    
    # Conteúdo Principal
    div(
      style = "padding: 25px; height: 100vh; overflow-y: auto;",
      
      div(
        style = "display: flex; justify-content: space-between; align-items: flex-end; margin-bottom: 30px;",
        div(
          h1("Monitoramento Estratégico", style = "margin: 0; font-weight: 800; font-size: 2rem; font-family: 'Outfit';"),
          div(class = "dynamic-subtitle", "Inteligência de Dados • SEDUC GO")
        ),
        div(
          style = "color: var(--text-secondary); font-size: 0.8rem;",
          tags$i(class = "bi bi-clock-history"), 
          paste("Atualizado em:", format(Sys.Date(), "%d/%m/%Y"))
        )
      ),
      
      layout_columns(
        col_widths = c(4, 4, 4),
        heights = "130px",
        value_box(title = "Total Unidades", value = textOutput("kpi_total"), showcase = bsicons::bs_icon("buildings", size = "3rem"), theme = "primary"),
        value_box(title = "Ponto de Atenção", value = textOutput("kpi_risco"), showcase = bsicons::bs_icon("exclamation-triangle", size = "3rem"), theme = "danger"),
        value_box(title = "Desempenho Alto", value = textOutput("kpi_sucesso"), showcase = bsicons::bs_icon("check-circle", size = "3rem"), theme = "success")
      ),
      
      navset_card_pill(
        height = "800px",
        nav_panel("Geoanálise", icon = icon("map"), leafletOutput("mapa", height = "100%")),
        nav_panel("Dados Tabulares", icon = icon("table"), div(style = "padding: 10px;", DTOutput("tabela")))
      )
    )
  )
)

# ==============================================================================
# 4. SERVIDOR
# ==============================================================================

server <- function(input, output, session) {
  
  observeEvent(input$dark_mode, {
    modo <- if(input$dark_mode) "dark" else "light"
    session$sendCustomMessage("toggle_theme_js", modo)
    
    proxy <- leafletProxy("mapa")
    if(input$dark_mode) {
      proxy %>% clearTiles() %>% addProviderTiles(providers$CartoDB.DarkMatter)
    } else {
      proxy %>% clearTiles() %>% addProviderTiles(providers$CartoDB.Positron)
    }
  })
  
  observeEvent(input$reset_filters, {
    updatePickerInput(session, "filtro_regional", selected = lista_regionais)
    updatePickerInput(session, "filtro_classificacao", selected = lista_status)
    updateSelectizeInput(session, "busca_escola", selected = "")
  })
  
  dados_filtrados <- reactive({
    req(input$filtro_regional, input$filtro_classificacao)
    dados_mapa %>% 
      filter(ETAPA == input$filtro_etapa) %>%
      filter(NM_REGIONAL %in% input$filtro_regional) %>%
      filter(CLASSIFICACAO %in% input$filtro_classificacao)
  })
  
  observe({
    req(input$filtro_etapa)
    escolas <- unique(dados_mapa %>% filter(ETAPA == input$filtro_etapa) %>% pull(NM_ESCOLA))
    updateSelectizeInput(session, "busca_escola", choices = c("", sort(escolas)), server = TRUE)
  })
  
  output$kpi_total   <- renderText({ format(nrow(dados_filtrados()), big.mark = ".") })
  output$kpi_risco   <- renderText({ format(sum(grepl("VERMELHO", dados_filtrados()$CLASSIFICACAO)), big.mark = ".") })
  output$kpi_sucesso <- renderText({ format(sum(grepl("VERDE", dados_filtrados()$CLASSIFICACAO)), big.mark = ".") })
  
  paleta <- colorNumeric(palette = c("#ef4444", "#f59e0b", "#10b981"), domain = c(0, 1))
  
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>% 
      fitBounds(lng1 = -53.5, lat1 = -19.5, lng2 = -45.5, lat2 = -12.5) %>%
      addResetMapButton() %>%
      htmlwidgets::onRender("function(el, x) { 
        var map = this; 
        map.createPane('polygons'); map.getPane('polygons').style.zIndex = 400;
        map.createPane('markers'); map.getPane('markers').style.zIndex = 600;
      }")
  })
  
  observeEvent(input$filtro_regional, {
    proxy <- leafletProxy("mapa")
    proxy %>% clearShapes()
    
    if (length(input$filtro_regional) == length(lista_regionais) || length(input$filtro_regional) == 0) {
      proxy %>% addPolygons(data = shape_estado, fillColor = "#1e293b", color = "#6366f1", weight = 2, fillOpacity = 0.1, options = pathOptions(pane = "polygons"))
    } else {
      munis <- shape_munis %>% filter(NM_REGIONAL %in% input$filtro_regional)
      if(nrow(munis) > 0){
        bbox <- st_bbox(munis)
        proxy %>% flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>%
          addPolygons(data = munis, fillColor = "#8b5cf6", color = "#0ea5e9", weight = 1, fillOpacity = 0.1, label = ~name_muni, options = pathOptions(pane = "polygons"))
      }
    }
  }, ignoreNULL = FALSE)
  
  observe({
    df <- dados_filtrados()
    if(nrow(df) == 0) { leafletProxy("mapa") %>% clearMarkers(); return() }
    
    popup_content <- paste0(
      "<div style='font-family: Inter, sans-serif; min-width: 220px;'>",
      "<div style='background: linear-gradient(135deg, #8b5cf6, #06b6d4); padding: 12px; color: white; border-radius: 8px 8px 0 0;'>",
      "<div style='font-size: 0.7rem; opacity: 0.9; text-transform: uppercase;'>Escola</div>",
      "<div style='font-weight: 700; font-size: 1.1rem; line-height: 1.2;'>", df$NM_ESCOLA, "</div>",
      "</div>",
      "<div style='padding: 15px; background: white; color: #334155;'>",
      "<div><small style='color: #64748b;'>Município</small><br><strong>", df$NM_MUNICIPIO, "</strong></div>",
      "<div style='margin-top: 10px; display: flex; justify-content: space-between; align-items: center;'>",
      "<div><small style='color: #64748b;'>Probabilidade</small><br><span style='font-size: 1.5rem; font-weight: 800; color: #7c3aed;'>", round(df$Prob_Media * 100, 1), "%</span></div>",
      "<span style='padding: 4px 10px; border-radius: 15px; font-size: 0.8rem; font-weight: 700; background: ",
      ifelse(df$CLASSIFICACAO == "VERDE", "#dcfce7; color: #166534", 
             ifelse(df$CLASSIFICACAO == "AMARELO", "#fef3c7; color: #b45309", "#fee2e2; color: #991b1b")),
      ";'>", df$CLASSIFICACAO, "</span>",
      "</div>",
      "</div>",
      "</div>"
    )
    
    leafletProxy("mapa", data = df) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~LONGITUDE, lat = ~LATITUDE, radius = 7, color = "#ffffff", weight = 1.5,
        fillColor = ~paleta(Prob_Media), fillOpacity = 0.9,
        popup = popup_content, options = pathOptions(pane = "markers")
      )
  })
  
  observeEvent(input$busca_escola, {
    req(input$busca_escola, input$busca_escola != "")
    alvo <- dados_mapa %>% filter(NM_ESCOLA == input$busca_escola)
    if(nrow(alvo) > 0) leafletProxy("mapa") %>% setView(lng = alvo$LONGITUDE[1], lat = alvo$LATITUDE[1], zoom = 15)
  })
  
  observeEvent(input$tabela_rows_selected, {
    idx <- input$tabela_rows_selected
    if(length(idx) > 0) {
      escola <- dados_filtrados()[idx, ]
      leafletProxy("mapa") %>% setView(lng = escola$LONGITUDE, lat = escola$LATITUDE, zoom = 15)
    }
  })
  
  # --- Tabela (COLUNAS E DADOS INTACTOS) ---
  output$tabela <- renderDT({
    df <- dados_filtrados()
    colunas <- c("NM_ESCOLA", "NM_MUNICIPIO", "NM_REGIONAL", "CLASSIFICACAO", "Prob_Media")
    if("Prob_Min_Credivel" %in% names(df)) colunas <- c(colunas, "Prob_Min_Credivel")
    if("Prob_Max_Credivel" %in% names(df)) colunas <- c(colunas, "Prob_Max_Credivel")
    if("CRESCIMENTO_LP" %in% names(df)) colunas <- c(colunas, "CRESCIMENTO_LP")
    
    df_tab <- df %>% select(any_of(colunas))
    if("Prob_Media" %in% names(df_tab)) df_tab <- mutate(df_tab, Prob_Media = paste0(round(Prob_Media * 100, 1), "%"))
    
    datatable(df_tab, selection = 'single', rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE, dom = 'frtp', 
                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'))) %>%
      formatStyle('CLASSIFICACAO',
                  color = styleEqual(c('VERDE', 'AMARELO', 'VERMELHO'), c('#10b981', '#f59e0b', '#f43f5e')),
                  fontWeight = 'bold')
  })
}

shinyApp(ui, server)
