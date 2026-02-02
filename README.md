# Modelo Bayesiano CI/CD: Escolas que vão alcançar a META

Os objetivos são:

-   [ ] Reunir informação, limpar e construir as Bases de Dados necessárias

-   [ ] Montar uma modelagem bayesiana para prever a probabilidade de cada escola bater a meta do IDEB 2025

-   [ ] Exportar os resultados como tabelas em formato `.xlsx`

-   [ ] Montar um painel interativo com esses resultados, identificando no mapa escolas que vão alcançar a meta, regionais/municipios que tenham maior peso e averiguar a confiabilidade na previsão feita.

## Variáveis do Modelo

-   Crescimento_LP e Crescimento_MT é a nota no SAEGO em LP e MT da escola na edição de 2025 - nota na edição de 2024. É esperado que se a escola melhorou de um ano para o outro, ela alcançe a meta certo?

-   O crescimento medio anual do ideb é a média de crescimento na nota do IDEB da escola ao longo das edições (Peguei todas as edições que a escola fez do IDEB, sua nota e fiz uma regressão disso e esse valor é o coeficiente "coef(lm(IDEB \~ Ano))[2]"). É esperado que se ela vem crescendo o coeficiente vai ser positivo e ela alcançe a meta né?

-   IP é o indice de participação no SAEB, que é um componente do calculo do IDEB, se muita gente for, vai aumentar a nota portanto melhora a chance de bater a meta.

-   DESAFIO_AF_MENOR é ver se o Desafio (Meta - IDEB anterior) é menor que 0.4 (vai de 0 a 1 o desafio), escolas com desafios menores que 0.4 tem desafio baixo correto? Se elas tem desafio baixo é pra ser mais "dificil" pra elas bater a meta.

## Pensamentos

-   Acho que essa variavel (DESAFIO_XX_MENOR) deveria ser apenas DESAFIO e nao tratada de forma discretizada, assim eu posso colocar ela com um termo suavizador e modelar melhor a relação de menores desafios exigem mais esforço.

-   Acho que devo acrescentar a variavel NOTA NO IDEB ANTERIOR.

-   CRESCIMENTO_MEDIO_ANUAL_IDEB faz sentido pra mim, mas tbm me deixa confuso. Se historicamente a escola tem um crescimento forte no IDEB é pro coeficiente da regressão linear ficar positivo (o valor da variavel vai ser positivo e grande). Se a escola vem crescendo lentamente, vai aumentar levemente a chance de bater a meta. Se a escola vem em média decrescendo, vai diminuir as chances dela bater a meta. Entretanto, a escola com maior valor de crescimento médio no ideb, NAO bateu a meta no passado!? Eu preciso de uma prior suave aqui pois não parece ser uma relação linear ? Ou eu preciso de uma interação dessa variavel com outra?

## Sugestões do GEMINI (TODO)

-   [x] Acrescentar variáveis vitais como: IDEB_ANTERIOR ; DESAFIO_CONTINUO = META_2025 - IDEB_ANTERIOR ; RELACAO_DESAFIO_NOTA = DESAFIO_CONTINUO / (IDEB_ANTERIOR + 0.01)

-   [ ] Interação Tensor Product (Sofisticado): t2() cria uma interação 3D entre ONDE a escola está e o TAMANHO do desafio. Isso resolve seu problema: "Desafio 0.2 na nota 7 é diferente de 0.2 na nota 4" `t2(IDEB_ANTERIOR, DESAFIO_CONTINUO)`

-   [ ] Efeito Teto: O modelo vai aprender que nota muito alta diminui a chance de bater meta `s(IDEB_ANTERIOR, k = 4)`

-   [ ] O Paradoxo do Crescimento Histórico: O modelo vai aprender que crescimento moderado é bom, mas crescimento exagerado pode não aumentar mais a chance (saturação) `s(CRESCIMENTO_MEDIO_ANUAL_IDEB, k = 4)`

### Após arrumar as bases de treino e teste

1.  `conditional_effects(modelo_gam_final, effects = "CRESCIMENTO_MEDIO_ANUAL_IDEB")` 1.1 O que esperar: Se sua intuição estiver certa, você verá uma curva que sobe e depois achata (ou até cai) para valores muito altos de crescimento.

2.  `conditional_effects(modelo_gam_final, effects = "IDEB_ANTERIOR:DESAFIO_CONTINUO")` 2.1 O que esperar: Isso vai gerar um gráfico de calor ou linhas coloridas. Você verá que para `IDEB_ANTERIOR` alto, a inclinação da linha do `DESAFIO` é muito mais íngreme (pequenos aumentos no desafio derrubam a probabilidade drasticamente) do que para `IDEB_ANTERIOR` baixo.

3.  O modelo final utilizará `s()` para curvas de saturação e `t2()` para interação 3D entre ponto de partida e tamanho do desafio:

```{r}
# Fórmula refinada com Splines e Interações
formula_gam_final <- bf(
  Y_num ~ 
    # Variáveis Lineares (Impacto direto)
    CRESCIMENTO_LP + 
    CRESCIMENTO_MT + 
    IP +
    
    # Curva de Saturação: Crescimento histórico tem limite de impacto
    s(CRESCIMENTO_MEDIO_ANUAL_IDEB, k = 4) +
    
    # Interação Não-Linear: O peso do Desafio depende da Nota Anterior
    # (Captura o "Efeito Teto" e a "Armadilha da Pobreza")
    t2(IDEB_ANTERIOR, DESAFIO_CONTINUO) +
    
    # Hierarquia: Regionais e Municípios (Partial Pooling)
    (1 | NM_REGIONAL / NM_MUNICIPIO),
    
  family = bernoulli(link = "logit")
)

priors_gam <- c(
  prior(student_t(3, 0, 2.5), class = "Intercept"),
  prior(normal(0, 1), class = "b"),            # Para termos lineares
  prior(student_t(3, 0, 2.5), class = "sds"),  # Para controlar a "wiggliness" dos Splines
  prior(exponential(2), class = "sd")          # Para os grupos hierárquicos
)

modelo_bayes_ef_final <- brm(
  formula = formula_gam_final,
  data = dados_prontos_stan, # Lembrar de ter as cols DESAFIO_CONTINUO e IDEB_ANTERIOR 
  prior = priors_gam,
  backend = "cmdstanr",
  chains = 4,
  cores = 2,
  threads = threading(2),
  iter = 3000,
  warmup = 1000,
  control = list(adapt_delta = 0.99, max_treedepth = 15), # Aumentado para lidar com t2()
  seed = 2026,
  file = "modelo_bayes_ef_final"
)
```

Eu espero muito que dê certo
