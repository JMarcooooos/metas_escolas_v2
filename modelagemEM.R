# modelagemEM.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(brms, tidyverse, tidybayes, recipes, readxl, cmdstanr, openxlsx)

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

# Ensino Médio ----

dados_bayes_em <- dados_treino_em %>%
  filter(!is.na(Y)) %>%
  mutate(Y_num = ifelse(Y == "Sim" | Y == "1", 1, 0)) %>%
  select(-Y) %>%
  na.omit()

# Receita (Feature Engineering)
receita_escala <- recipe(Y_num ~ ., data = dados_bayes_em) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors(),-CD_ESCOLA) %>%
  step_dummy(all_nominal_predictors(),-NM_ESCOLA, -NM_MUNICIPIO, -NM_MUNICIPIO, -NM_REGIONAL) %>%
  step_rename_at(all_predictors(), fn = ~ make.names(.)) %>% 
  prep()

dados_prontos_stan_em <- bake(receita_escala, new_data = dados_bayes_em)
# Tratamento para o teste (garantindo colunas iguais)
dados_teste_proc_em <- bake(receita_escala, new_data = dados_teste_em %>% mutate(Y_num=0))

message("--- Iniciando Modelagem Bayesiana (CmdStanR) ---")

modelo_bayes_em_final <- brm(
    formula = bf(
      Y_num ~ 0 + Intercept + CRESCIMENTO_GERAL * RELACAO_DESAFIO_NOTA + IP + CRESCIMENTO_GERAL:IP +
        (1 | NM_REGIONAL / NM_MUNICIPIO),
      family = bernoulli(link = "logit")),
  data = dados_prontos_stan_em,
  prior = c(
    prior(normal(0.2, 0.5), class = "b", coef = "Intercept"), # media 0.2 pois quero ser mais otimista com as escolas que ficarem como incertas
    prior(normal(0, 1), class = "b"),                         # Para termos lineares
    prior(exponential(3), class = "sd")                       # Para os grupos hierárquicos
  ),
  backend = "cmdstanr",
  chains = 4,
  cores = 2,  
  threads = threading(2), 
  iter = 3000,
  warmup = 1000,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  seed = 2026,
  file = "modelo_bayes_em_final" 
)

message("--- Gerando Previsões e Intervalos de Credibilidade ---")
previsoes_finais_em <- dados_teste_proc_em %>%
  add_epred_draws(
    modelo_bayes_em_final, 
    ndraws = 1000, 
    allow_new_levels = TRUE
  ) %>% 
  group_by(CD_ESCOLA) %>%
  summarise(
    Prob_Media = mean(.epred, na.rm = TRUE),
    Prob_Min_Credivel = quantile(.epred, 0.05, na.rm = TRUE), 
    Prob_Max_Credivel = quantile(.epred, 0.95, na.rm = TRUE)
  ) %>%
  mutate(
    Amplitude_IC = Prob_Max_Credivel - Prob_Min_Credivel
  ) %>%
  arrange(desc(Prob_Media))

summary(previsoes_finais_em$Amplitude_IC)

corte_incerteza <- 0.25 
corte_sucesso <- 0.50

resultado_calibrado_em <- previsoes_finais_em %>%
  mutate(
    CLASSIFICACAO = case_when(

      # O modelo crava que vai passar (Intervalo curto e Média Alta)
      Amplitude_IC <= corte_incerteza & Prob_Media >= corte_sucesso ~ "VERDE SÓLIDO: Alta Chance (Consolidado)",
      
      # O modelo crava que vai reprovar (Intervalo curto e Média Baixa)
      Amplitude_IC <= corte_incerteza & Prob_Media < corte_sucesso  ~ "VERMELHO SÓLIDO: Baixa Chance (Crítico)",
      
      # A média é boa, mas o intervalo é gigante (Pode dar zebra)
      Amplitude_IC > corte_incerteza & Prob_Media >= corte_sucesso  ~ "AZUL: Tendência Positiva (Mas Volátil/Arriscado)",
      
      # A média é ruim, mas o intervalo é gigante (Gestão pode salvar!)
      Amplitude_IC > corte_incerteza & Prob_Media < corte_sucesso   ~ "AMARELO: Tendência Negativa (Mas Reversível/Incerto)"
    )
  ) %>%
  left_join(
    dados_teste_em %>% select(CD_ESCOLA, NM_ESCOLA, NM_REGIONAL, NM_MUNICIPIO),
    by = "CD_ESCOLA"
  ) %>%
  select(NM_REGIONAL, NM_MUNICIPIO, CD_ESCOLA, NM_ESCOLA, Prob_Media, Amplitude_IC, Prob_Min_Credivel, Prob_Max_Credivel,CLASSIFICACAO)

message("--- Salvando Resultados ---")

CONFIG <- list(
  fonte_padrao = "Calibri",
  cor_primaria = "#275317", 
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

caminho_logo <- "logo.png"

adicionar_aba_formatada <- function(workbook, df, nome_aba, titulo_texto) {
  
  addWorksheet(workbook, nome_aba, gridLines = FALSE)
  n_cols <- ncol(df)
  n_rows <- nrow(df)
  
  cols_prob <- grep("Prob_|Amplitude_", names(df))
  
  if (file.exists(caminho_logo)) {
    insertImage(workbook, nome_aba, caminho_logo, startRow = 1, startCol = 1, width = 19.66/2.54, height = 1.87/2.54)
  }
  
  mergeCells(workbook, nome_aba, cols = 1:n_cols, rows = 5)
  setRowHeights(workbook, nome_aba, rows = 5, heights = 25)
  writeData(workbook, nome_aba, titulo_texto, startRow = 5)
  addStyle(workbook, nome_aba, style = estilo_titulo, rows = 5, cols = 1:n_cols)
  
  writeData(workbook, nome_aba, df, startRow = 6)
  addStyle(workbook, nome_aba, style = estilo_cabecalho, rows = 6, cols = 1:n_cols)
  addStyle(workbook, nome_aba, style = estilo_corpo, rows = 7:(n_rows + 6), cols = 1:n_cols, gridExpand = TRUE)
  
  linha_fonte_dinamica <- n_rows + 8
  writeData(workbook, nome_aba, texto_fonte, startRow = linha_fonte_dinamica)
  addStyle(workbook, nome_aba, style = estilo_rodape, rows = linha_fonte_dinamica, cols = 1)
  
  if(length(cols_prob) > 0) {
    addStyle(workbook, nome_aba, style = estilo_porcentagem, 
             rows = 7:(n_rows + 6), 
             cols = cols_prob, 
             gridExpand = TRUE, stack = TRUE)
  }
  
  col_class <- which(names(df) == "CLASSIFICACAO")
  
  if(length(col_class) > 0) {

    conditionalFormatting(workbook, nome_aba, cols = col_class, rows = 7:(n_rows + 6), 
                          rule = "VERDE", type = "contains", 
                          style = createStyle(fontColour = "#006100", bgFill = "#C6EFCE"))
    
    conditionalFormatting(workbook, nome_aba, cols = col_class, rows = 7:(n_rows + 6), 
                          rule = "VERMELHO", type = "contains", 
                          style = createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE"))
    
    conditionalFormatting(workbook, nome_aba, cols = col_class, rows = 7:(n_rows + 6), 
                          rule = "AMARELO", type = "contains", 
                          style = createStyle(fontColour = "#9C5700", bgFill = "#FFEB9C"))
    
    conditionalFormatting(workbook, nome_aba, cols = col_class, rows = 7:(n_rows + 6), 
                          rule = "AZUL", type = "contains", 
                          style = createStyle(fontColour = "#003366", bgFill = "#BDD7EE"))
  }
  
  freezePane(workbook, nome_aba, firstActiveRow = 7)
  setColWidths(workbook, nome_aba, cols = 1:n_cols, widths = "auto")
  
  cols_largas <- which(names(df) %in% c("NM_MUNICIPIO", "NM_REGIONAL", "CLASSIFICACAO", "NM_ESCOLA"))
  if(length(cols_largas) > 0){
    setColWidths(workbook, nome_aba, cols = cols_largas, widths = 35)
  }
}

wb <- createWorkbook()

if (exists("resultado_calibrado_em")) {
  adicionar_aba_formatada(wb, resultado_calibrado_em, "Ensino Médio", "Previsão de Metas - Ensino Médio")
}

saveWorkbook(wb, "resultado_previsao_escolas_em.xlsx", overwrite = TRUE)

message("--- Concluído com Sucesso ---")