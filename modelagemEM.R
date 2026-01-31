# modelagemEM.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(brms, tidyverse, tidybayes, recipes, readxl, cmdstanr)

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

dados_bayes_em <- dados_treino_em %>%
  select(-CD_ESCOLA) %>%
  filter(!is.na(Y)) %>%
  mutate(Y_num = ifelse(Y == "Sim" | Y == "1", 1, 0)) %>%
  select(-Y) %>%
  na.omit()

# Receita (Feature Engineering)
receita_escala <- recipe(Y_num ~ ., data = dados_bayes_em) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors(), -NM_MUNICIPIO, -NM_REGIONAL) %>%
  step_rename_at(all_predictors(), fn = ~ make.names(.)) %>% 
  prep()

dados_prontos_stan <- bake(receita_escala, new_data = dados_bayes_em)
# Tratamento para o teste (garantindo colunas iguais)
dados_teste_proc   <- bake(receita_escala, new_data = dados_teste_em %>% mutate(Y_num=0))

message("--- Iniciando Modelagem Bayesiana (CmdStanR) ---")

# Modelo BRMS Otimizado para GitHub Actions
modelo_bayes <- brm(
  formula = bf(Y_num ~ . -NM_REGIONAL -NM_MUNICIPIO + (1 | NM_REGIONAL) + (1 | NM_MUNICIPIO), 
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
  file = "modelo_bayes_em_final" # Salva automaticamente como .rds
)

message("--- Gerando Previsões e Intervalos de Credibilidade ---")

previsoes_finais <- dados_teste_proc %>%
  mutate(CD_ESCOLA = dados_teste_em$CD_ESCOLA) %>% 
  add_epred_draws(
    modelo_bayes, 
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

# Lógica de Classificação (Calibração)
corte_verde <- quantile(previsoes_finais$Prob_Media, probs = (1 - 0.43))
corte_vermelho <- quantile(previsoes_finais$Prob_Media, probs = 0.25)

resultado_calibrado <- previsoes_finais %>%
  mutate(
    CLASSIFICACAO = case_when(
      Prob_Media >= corte_verde ~ "VERDE: Tendência Alta",
      Prob_Media <= corte_vermelho ~ "VERMELHO: Risco Crítico",
      TRUE ~ "AMARELO: Incerto / Batalha"
    )
  ) %>%
  left_join(
    dados_teste_em %>% select(CD_ESCOLA, NM_REGIONAL, NM_MUNICIPIO),
    by = "CD_ESCOLA"
  ) %>%
  select(CD_ESCOLA, NM_REGIONAL, NM_MUNICIPIO, Prob_Media, Prob_Min_Credivel, Prob_Max_Credivel, CLASSIFICACAO)

message("--- Salvando Resultados ---")

# 1. Salvar a Tabela para Excel (CSV universal)
write_csv(resultado_calibrado, "resultado_previsao_escolas_EM.csv")

# 2. O modelo já foi salvo pelo brm no argumento 'file', mas garantindo extensão:
# (O brms cria modelo_bayes_em_final.rds)

message("--- Concluído com Sucesso ---")