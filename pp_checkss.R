# averiguar modelo com sample_prior = "only"


# ENSINO FUNDAMENTAL
table(dados_treino_ef$Y)/nrow(dados_treino_ef)

log(0.4350133/(1-0.4350133))

modelo_bayes_ef_final <- brm(
  formula = bf(
    Y_num ~ CRESCIMENTO_LP + CRESCIMENTO_MT + IP +
      s(CRESCIMENTO_MEDIO_ANUAL_IDEB, k = 4) + 
      s(IDEB_ANTERIOR, k = 4) +
      t2(IDEB_ANTERIOR, DESAFIO_AF) +
      (1 | NM_REGIONAL / NM_MUNICIPIO),
    family = bernoulli(link = "logit")),
  data = dados_prontos_stan,
  prior = c(
    prior(normal(-0.2614256, 1), class = "Intercept"), # logit(-0.26...) faz ficar centrado em 43% de prob. de alcançar a meta (que é próximo da proporção real de escolas que bateram a ultima meta), diminui o sd pra ficar mais concentrado nisso
    prior(normal(0, 1), class = "b"),            # Para termos lineares
    prior(exponential(3), class = "sds"),        # Para controlar a "wiggliness" dos Splines
    prior(exponential(2), class = "sd")          # Para os grupos hierárquicos
  ),
  backend = "cmdstanr",
  chains = 4,
  cores = 2,  
  threads = threading(2), 
  iter = 3000,
  warmup = 1000,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  seed = 2026,
  sample_prior = "only"
)

pp_check(modelo_bayes_ef_final,ndraws = 1000)
pp_check(modelo_bayes_ef_final, type = "stat", stat = "mean", prefix = "ppd")

conditional_effects(modelo_bayes_ef_final, 
                    effects = "CRESCIMENTO_LP", 
                    method = "predict", 
                    spaghetti = TRUE, 
                    ndraws = 100) 

conditional_effects(modelo_bayes_ef_final, 
                    effects = "CRESCIMENTO_MT", 
                    method = "predict", 
                    spaghetti = TRUE, 
                    ndraws = 100) 
conditional_effects(modelo_bayes_ef_final, 
                    effects = "IDEB_ANTERIOR", 
                    method = "predict", 
                    spaghetti = TRUE, 
                    ndraws = 100) 
conditional_effects(modelo_bayes_ef_final, 
                    effects = "CRESCIMENTO_MEDIO_ANUAL_IDEB", 
                    method = "predict", 
                    spaghetti = TRUE, 
                    ndraws = 100) 

plot(density(as.vector(posterior_linpred(modelo_bayes_ef_final))), main = "Escala Log-Odds (Latente)", xlim = c(-15, 15))
abline(v = c(-5, 5), col = "red", lty = 2)

# ENSINO MEDIO

table(dados_treino_em$Y)/nrow(dados_treino_em)

log(0.4260163 /(1-0.4260163))

modelo_bayes_em_final <- brm(
  formula = bf(
    Y_num ~ CRESCIMENTO_LP + CRESCIMENTO_MT + IP +
      s(CRESCIMENTO_MEDIO_ANUAL_IDEB, k = 4) + 
      s(IDEB_ANTERIOR, k = 4) +
      t2(IDEB_ANTERIOR, DESAFIO_EM) +
      (1 | NM_REGIONAL / NM_MUNICIPIO),
    family = bernoulli(link = "logit")),
  data = dados_prontos_stan_em,
  prior = c(
    prior(normal(-0.2981234, 1), class = "Intercept"), # logit(-0.2981234) faz ficar centrado em 42% de prob. de alcançar a meta (que é próximo da proporção real de escolas que bateram a ultima meta), diminui o sd pra ficar mais concentrado nisso
    prior(normal(0, 1), class = "b"),            # Para termos lineares
    prior(exponential(3), class = "sds"),        # Para controlar a "wiggliness" dos Splines
    prior(exponential(2), class = "sd")          # Para os grupos hierárquicos
  ),
  backend = "cmdstanr",
  chains = 4,
  cores = 2,  
  threads = threading(2), 
  iter = 3000,
  warmup = 1000,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  seed = 2026,
  sample_prior = "only"
)

pp_check(modelo_bayes_em_final,ndraws = 1000)
pp_check(modelo_bayes_em_final, type = "stat", stat = "mean", prefix = "ppd")
pp_check(modelo_bayes_ef_final,type = "stat_grouped",  stat = "mean", group = "NM_REGIONAL", ndraws = 200)
pp_check(modelo_bayes_ef_final,type="scatter_avg_grouped",group = "NM_REGIONAL")
pp_check(modelo_bayes_ef_final,type="pit_ecdf_grouped",group = "NM_REGIONAL")

pp_check(modelo_bayes_ef_final,type="error_binned")
pp_check(modelo_bayes_ef_final,type="bars_grouped",group = "NM_REGIONAL")
pp_check(modelo_bayes_ef_final,type="ecdf_overlay")



conditional_effects(modelo_bayes_em_final, 
                    effects = "CRESCIMENTO_LP", 
                    method = "predict", 
                    spaghetti = TRUE, 
                    ndraws = 100) 

conditional_effects(modelo_bayes_em_final, 
                    effects = "CRESCIMENTO_MT", 
                    method = "predict", 
                    spaghetti = TRUE, 
                    ndraws = 100) 
conditional_effects(modelo_bayes_em_final, 
                    effects = "IDEB_ANTERIOR", 
                    method = "predict", 
                    spaghetti = TRUE, 
                    ndraws = 100) 
conditional_effects(modelo_bayes_em_final, 
                    effects = "CRESCIMENTO_MEDIO_ANUAL_IDEB", 
                    method = "predict", 
                    spaghetti = TRUE, 
                    ndraws = 100) 

plot(density(as.vector(posterior_linpred(modelo_bayes_em_final))), main = "Escala Log-Odds (Latente)", xlim = c(-15, 15))
abline(v = c(-5, 5), col = "red", lty = 2)
