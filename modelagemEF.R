# modelagemEF.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(brms, tidyverse, tidybayes, recipes, readxl, cmdstanr, openxlsx)

# --- BLOCO DE CONFIGURAÇÃO DO CMDSTAN ---

# Verifica se estamos rodando no GitHub Actions (Ambiente CI)
if (Sys.getenv("CI") == "true") {
  set_cmdstan_path("/home/runner/.cmdstan/cmdstan-2.34.1") 
  
} else {
  # Se você estiver rodando no seu computador (Z:/...), ele tenta achar sozinho
  # ou você define seu caminho local aqui se precisar
  check_cmdstan_toolchain() 
}

message("CmdStan path: ", cmdstan_path())

message("--- Carregando Dados ---")

load("bases_para_modelagem.RData") 

# Ensino Fundamental ----

dados_bayes_ef <- dados_treino_ef %>%
  filter(!is.na(Y)) %>%
  mutate(Y_num = ifelse(Y == "Sim" | Y == "1", 1, 0)) %>%
  select(-Y) %>%
  na.omit()

# Receita (Feature Engineering)
receita_escala <- recipe(Y_num ~ ., data = dados_bayes_ef) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors(), -CD_ESCOLA) %>%
  step_dummy(all_nominal_predictors(),-NM_ESCOLA, -NM_MUNICIPIO, -NM_REGIONAL) %>%
  step_rename_at(all_predictors(), fn = ~ make.names(.)) %>% 
  prep()

dados_prontos_stan <- bake(receita_escala, new_data = dados_bayes_ef)
# Tratamento para o teste (garantindo colunas iguais)
dados_teste_proc_ef <- bake(receita_escala, new_data = dados_teste_ef %>% mutate(Y_num=0))

message("--- Iniciando Modelagem Bayesiana (CmdStanR) ---")

# Modelo BRMS Otimizado para GitHub Actions
modelo_bayes_ef_final <- brm(
  formula = bf(Y_num ~ . -CD_ESCOLA -NM_ESCOLA -NM_REGIONAL -NM_MUNICIPIO + (1 | NM_REGIONAL) + (1 | NM_MUNICIPIO), 
               family = bernoulli(link = "logit")),
  data = dados_prontos_stan,
  backend = "cmdstanr", # <--- Mais rápido e leve
  chains = 4,
  cores = 2,  # <--- Limite seguro para o GitHub Actions Free
  threads = threading(2), # <--- Paralelismo intra-chain
  iter = 2000,
  warmup = 1000,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  seed = 2026,
  file = "modelo_bayes_ef_final" # Salva automaticamente como .rds
)

message("--- Gerando Previsões e Intervalos de Credibilidade ---")

previsoes_finais_ef <- dados_teste_proc_ef %>%
  add_epred_draws(
    modelo_bayes_ef_final, 
    ndraws = 1000, 
    allow_new_levels = TRUE
  ) %>% 
  group_by(CD_ESCOLA) %>%
  summarise(
    Prob_Media = mean(.epred, na.rm = TRUE),
    Prob_Min_Credivel = quantile(.epred, 0.05, na.rm = TRUE), 
    Prob_Max_Credivel = quantile(.epred, 0.95, na.rm = TRUE),
    Certeza_Alta = mean(.epred > 0.6, na.rm = TRUE) 
  ) %>%
  arrange(desc(Prob_Media))

resultado_calibrado_ef <- previsoes_finais_ef %>%
  mutate(
    CLASSIFICACAO = case_when(
      # CERTEZA DE SUCESSO: Mesmo no pior cenário (Min), ela ainda ganha (> 50%)
      Prob_Min_Credivel > 0.50 ~ "VERDE: Alta Probabilidade (Consolidado)",
      
      # CERTEZA DE FRACASSO: Mesmo no melhor cenário (Max), ela perde (< 50%)
      Prob_Max_Credivel < 0.50 ~ "VERMELHO: Baixa Probabilidade (Risco Crítico)",
      
      # INCERTEZA: O intervalo cruza os 50% (Pode dar cara ou coroa)
      TRUE ~ "AMARELO: Incerto (Depende de Gestão)"
    )
  ) %>%
  left_join(
    dados_teste_ef %>% select(CD_ESCOLA, NM_ESCOLA, NM_REGIONAL, NM_MUNICIPIO),
    by = "CD_ESCOLA"
  ) %>%
  select(CD_ESCOLA, NM_ESCOLA, NM_REGIONAL, NM_MUNICIPIO, Prob_Media, Prob_Min_Credivel, Prob_Max_Credivel, CLASSIFICACAO)


message("--- Salvando Resultados ---")

# 1. Salvar a Tabela para Excel (CSV universal)

CONFIG <- list(
  fonte_padrao = "Calibri",
  cor_primaria = "#1f4e78", 
  pasta_projeto = "."
)

estilo_titulo <- createStyle(fontName = CONFIG$fonte_padrao, fontSize = 16, textDecoration = "bold", halign = "center", valign = "center")
estilo_cabecalho <- createStyle(fontName = CONFIG$fonte_padrao, fontColour = "white", textDecoration = "bold", fgFill = CONFIG$cor_primaria, halign = "center", valign = "center", border = "TopBottomLeftRight")
estilo_corpo <- createStyle(fontName = CONFIG$fonte_padrao, border = "TopBottomLeftRight", borderStyle = "thin", halign = "center")
estilo_rodape <- createStyle(fontName = CONFIG$fonte_padrao, fontSize = 10, textDecoration = "italic", halign = "left")

estilo_numero <- createStyle(fontName = CONFIG$fonte_padrao, numFmt = "#,##0", border = "TopBottomLeftRight", borderStyle = "thin", halign = "center")
estilo_porcentagem <- createStyle(fontName = CONFIG$fonte_padrao, numFmt = "0.0%", border = "TopBottomLeftRight", borderStyle = "thin", halign = "center")

agora <- Sys.time()
attr(agora, "tzone") <- "America/Sao_Paulo"
texto_fonte <- paste0("Fonte: Dados extraídos e calculados via Modelo Bayesiano em ", format(agora, "%d/%m/%Y às %Hh%Mmin"), ".")

caminho_logo <- "logos/logo.png" 

adicionar_aba_formatada <- function(workbook, df, nome_aba, titulo_texto) {
  
  addWorksheet(workbook, nome_aba, gridLines = FALSE)
  n_cols <- ncol(df)
  n_rows <- nrow(df)

  cols_prob <- grep("Prob_", names(df)) 
  
  if (file.exists(caminho_logo)) {
    insertImage(workbook, nome_aba, caminho_logo, startRow = 1, startCol = 1, width = 19.66/2.54, height = 1.87/2.54)
  }
  
  # --- TÍTULO ---
  mergeCells(workbook, nome_aba, cols = 1:n_cols, rows = 5)
  setRowHeights(workbook, nome_aba, rows = 5, heights = 25)
  writeData(workbook, nome_aba, titulo_texto, startRow = 5)
  addStyle(workbook, nome_aba, style = estilo_titulo, rows = 5, cols = 1:n_cols)
  
  # --- DADOS E CABEÇALHO ---
  writeData(workbook, nome_aba, df, startRow = 6)
  addStyle(workbook, nome_aba, style = estilo_cabecalho, rows = 6, cols = 1:n_cols)
  addStyle(workbook, nome_aba, style = estilo_corpo, rows = 7:(n_rows + 6), cols = 1:n_cols, gridExpand = TRUE)
  
  # --- RODAPÉ ---
  linha_fonte_dinamica <- n_rows + 8
  writeData(workbook, nome_aba, texto_fonte, startRow = linha_fonte_dinamica)
  addStyle(workbook, nome_aba, style = estilo_rodape, rows = linha_fonte_dinamica, cols = 1)
  
  # --- FORMATAÇÃO ESPECÍFICA (PORCENTAGEM) ---
  if(length(cols_prob) > 0) {
    addStyle(workbook, nome_aba, style = estilo_porcentagem, 
             rows = 7:(n_rows + 6), 
             cols = cols_prob, 
             gridExpand = TRUE, stack = TRUE)
  }
  
  # --- FORMATAÇÃO CONDICIONAL (COR NA CLASSIFICAÇÃO) ---
  col_class <- which(names(df) == "CLASSIFICACAO")
  if(length(col_class) > 0) {
    # Verde
    conditionalFormatting(workbook, nome_aba, cols = col_class, rows = 7:(n_rows + 6), 
                          rule = "VERDE", type = "contains", 
                          style = createStyle(fontColour = "#006100", bgFill = "#C6EFCE"))
    # Vermelho
    conditionalFormatting(workbook, nome_aba, cols = col_class, rows = 7:(n_rows + 6), 
                          rule = "VERMELHO", type = "contains", 
                          style = createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE"))
    # Amarelo
    conditionalFormatting(workbook, nome_aba, cols = col_class, rows = 7:(n_rows + 6), 
                          rule = "AMARELO", type = "contains", 
                          style = createStyle(fontColour = "#9C5700", bgFill = "#FFEB9C"))
  }
  
  freezePane(workbook, nome_aba, firstActiveRow = 7)
  setColWidths(workbook, nome_aba, cols = 1:n_cols, widths = "auto")
  setColWidths(workbook, nome_aba, cols = which(names(df) %in% c("NM_MUNICIPIO", "NM_REGIONAL", "CLASSIFICACAO")), widths = 25)
}

wb <- createWorkbook()

if (exists("resultado_calibrado_ef")) {
  adicionar_aba_formatada(wb, resultado_calibrado_ef, "Ensino Fund.", "Previsão de Metas - Ensino Fundamental")
}

saveWorkbook(wb, "resultado_previsao_escolas.xlsx", overwrite = TRUE)

# 2. O modelo já foi salvo pelo brm no argumento 'file', mas garantindo extensão:
# (O brms cria modelo_bayes_ef_final.rds)

message("--- Concluído com Sucesso ---")