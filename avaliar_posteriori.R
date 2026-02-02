# avaliar posteriori

modelo_bayes_ef_final <- readRDS("C:\\Users\\05681371166\\Downloads\\resultados\\modelo_bayes_ef_final.rds")

fixef(modelo_bayes_ef_final)



# EM

ggplot(dados_prontos_stan_em, aes(x = CRESCIMENTO_GERAL, y = Y_num)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.4, color = "gray50") +
  geom_smooth(method = "loess", color = "blue", fill = "lightblue") +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +
  labs(title = "Linearidade: Crescimento vs. Meta Batida",
       subtitle = "Azul = Tendência Real (Loess) | Vermelho = Tendência Linear",
       y = "Probabilidade de Bater a Meta (0 ou 1)",
       x = "Crescimento Geral (Padronizado)") +
  theme_minimal()


ggplot(dados_prontos_stan_em, aes(x = CRESCIMENTO_MEDIO_ANUAL_IDEB, y = Y_num)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.4, color = "gray50") +
  geom_smooth(method = "loess", color = "blue", fill = "lightblue") +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +
  labs(title = "Linearidade: Crescimento MEDIO IDEB vs. Meta Batida",
       subtitle = "Azul = Tendência Real (Loess) | Vermelho = Tendência Linear",
       y = "Probabilidade de Bater a Meta (0 ou 1)",
       x = "Crescimento Medio Aual IDEB Geral") +
  theme_minimal()


ggplot(dados_prontos_stan_em, aes(x = IP, y = Y_num)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.4, color = "gray50") +
  geom_smooth(method = "loess", color = "blue", fill = "lightblue") +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +
  labs(title = "Linearidade: IP vs. Meta Batida",
       subtitle = "Azul = Tendência Real (Loess) | Vermelho = Tendência Linear",
       y = "Probabilidade de Bater a Meta (0 ou 1)",
       x = "IP") +
  theme_minimal()


ggplot(dados_prontos_stan_em, aes(x = RELACAO_DESAFIO_NOTA, y = Y_num)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.4, color = "gray50") +
  geom_smooth(method = "loess", color = "blue", fill = "lightblue") +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +
  labs(title = "Linearidade: RELAÇÃO DESAFIO NOTA vs. Meta Batida",
       subtitle = "Azul = Tendência Real (Loess) | Vermelho = Tendência Linear",
       y = "Probabilidade de Bater a Meta (0 ou 1)",
       x = "RELACAO DESAFIO NOTA)") +
  theme_minimal()


ggplot(dados_prontos_stan_em, aes(x = IDEB_ANTERIOR, y = Y_num)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.4, color = "gray50") +
  geom_smooth(method = "loess", color = "blue", fill = "lightblue") +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +
  labs(title = "Linearidade: IDEB ANTERIOR vs. Meta Batida",
       subtitle = "Azul = Tendência Real (Loess) | Vermelho = Tendência Linear",
       y = "Probabilidade de Bater a Meta (0 ou 1)",
       x = "IDEB ANTERIOR") +
  theme_minimal()

ggplot(dados_prontos_stan_em, aes(x = DESAFIO_EM, y = Y_num)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.4, color = "gray50") +
  geom_smooth(method = "loess", color = "blue", fill = "lightblue") +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +
  labs(title = "Linearidade: DESAFIO vs. Meta Batida",
       subtitle = "Azul = Tendência Real (Loess) | Vermelho = Tendência Linear",
       y = "Probabilidade de Bater a Meta (0 ou 1)",
       x = "DESAFIO") +
  theme_minimal()

interacao_heatmap <- dados_prontos_stan_em %>%
  mutate(
    # Dividindo em 5 grupos (quintis) para criar um grid 5x5
    Bin_Crescimento = cut_number(CRESCIMENTO_GERAL, 5),
    Bin_Relacao = cut_number(RELACAO_DESAFIO_NOTA, 5) 
  ) %>%
  group_by(Bin_Crescimento, Bin_Relacao) %>%
  summarise(
    Prob_Sucesso = mean(Y_num), # % de escolas que bateram a meta nesse quadrado
    n = n()
  )

ggplot(interacao_heatmap, aes(x = Bin_Crescimento, y = Bin_Relacao, fill = Prob_Sucesso)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "red", high = "green") +
  labs(title = "Mapa de Interação: Crescimento x Dificuldade",
       subtitle = "Mudança diagonal = Interação forte",
       x = "Faixas de Crescimento (Baixo -> Alto)",
       y = "Faixas de Relação Desafio/Nota (Fácil -> Difícil)",
       fill = "Chance de Sucesso") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(rpart)
library(rpart.plot)

arvore <- rpart(Y_num ~ CRESCIMENTO_GERAL + RELACAO_DESAFIO_NOTA + IP + IDEB_ANTERIOR, 
                data = dados_prontos_stan_em, 
                method = "class",
                cp = 0.015)
rpart.plot(arvore, 
           main = "Como as variáveis interagem naturalmente?",
           extra = 106) 


modelo_bayes_em <- readRDS("C:\\Users\\05681371166\\Downloads\\resultados\\modelo_bayes_em_final.rdhttp://127.0.0.1:19253/graphics/e6c6190f-6bba-4891-8530-96d933045bcf.pngs")
fixef(modelo_bayes_em_final)
summary(modelo_bayes_em_final)

summary(dados_treino_em$RELACAO_DESAFIO_NOTA) # Treino
summary(dados_teste_em$RELACAO_DESAFIO_NOTA)   # Teste

summary(dados_treino_em$CRESCIMENTO_GERAL)    # Treino
summary(dados_teste_em$CRESCIMENTO_GERAL)      # Teste

mean(dados_prontos_stan_em$Y_num)

matriz_correlacao <- dados_prontos_stan_em %>% 
  select(where(is.numeric)) %>%  
  select(-Y_num,-CD_ESCOLA,-IDEB_ANTERIOR,-DESAFIO_EM,-CRESCIMENTO_LP,-CRESCIMENTO_MT) %>%             
  cor(use = "pairwise.complete.obs") 

ggcorrplot::ggcorrplot(matriz_correlacao,
           method = "square",   
           type = "lower",         
           lab = TRUE,            
           lab_size = 3,           
           colors = c("red", "white", "blue"), 
           title = "Matriz de Correlação das Variáveis",
           ggtheme = theme_minimal())


previsoes_finais_em <- readxl::read_excel("C:\\Users\\05681371166\\Downloads\\resultados\\resultado_previsao_escolas_em.xlsx",skip=5)


ggplot(resultado_calibrado_em, aes(x = Prob_Media)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  labs(title = "Distribuição das Probabilidades Calculadas",
       subtitle = "O modelo está travado em valores baixos?",
       x = "Probabilidade de Bater a Meta", y = "Qtd Escolas")



pp_check(modelo_bayes_em_final,)