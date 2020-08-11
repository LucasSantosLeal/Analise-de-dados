Exercicio 7
================
Lucas Santos Leal de Albuquerque

### No exercício anterior foram feitos alguns modelos bivariados. Agora faça uma regressão multivariada mostrando como a nota atribuída a Jair Bolsonaro (variável Q1607) pode ser explicada pelas variáveis idade (D1A\_ID), educação (D3\_ESCOLA), renda (D9), nota atribuída ao PT (Q1501) e auto-atribuição ideológica (Q18) dos respondentes. Interprete o resultado a partir das informações dadas pelo sumário da regressão.

#### Os resultados da regressão múltipla indicaram coeficientes significativos para três variáveis, possibilitando a exclusão da hipótese de que seus efeitos sejam nulos. A variável D1A\_ID obteve um coeficiente de 0,005, para cada aumento de uma unidade na idade. No entanto o p-valor não significativo impossibilita a rejeição da hipótese de que o coeficiente seja diferente de zero. A escolaridade (D3\_ESCOLA) teve efeito negativo, ou seja, para cada ano adicional de educação, a nota atribuída ao político Jair Bolsonaro é reduzida em 0,15. A variável D9 teve impacto negativo de 0,06 na nota ao político para cada variação de 1% no nível de renda. A nota ao PT (Q1501) teve impacto negativo de 0,41. Por fim, a escala ideológica (Q18) teve coeficiente de 0,31, indicando que quanto mais à direita na escala ideológica, maior a nota atribuída a Jair Bolsonaro. O modelo, embora inclua diversas variáveis, tem baixa capacidade explicativa mediana (28,3%).

``` r
reg_1 <- lm(Q1607~D1A_ID+D3_ESCOLA+D9+Q1501+Q18, data = banco)
summary(reg_1)
```

    ## 
    ## Call:
    ## lm(formula = Q1607 ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18, data = banco)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.0176 -2.5841  0.4915  2.1784  9.0477 
    ## 
    ## Coefficients:
    ##                Estimate  Std. Error t value             Pr(>|t|)    
    ## (Intercept)  5.74253138  0.47681279  12.044 < 0.0000000000000002 ***
    ## D1A_ID       0.00581551  0.00621645   0.936              0.34968    
    ## D3_ESCOLA   -0.15431384  0.04468547  -3.453              0.00057 ***
    ## D9          -0.00003067  0.00002793  -1.098              0.27230    
    ## Q1501       -0.41538765  0.02359018 -17.608 < 0.0000000000000002 ***
    ## Q18          0.32439404  0.02626794  12.349 < 0.0000000000000002 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.336 on 1456 degrees of freedom
    ## Multiple R-squared:  0.2831, Adjusted R-squared:  0.2806 
    ## F-statistic:   115 on 5 and 1456 DF,  p-value: < 0.00000000000000022

### Em que medida os resultados se mantém ou se alteram quando comparados com os resultados do exercício anterior, quando foram utilizadas apenas regressões bivariadas?

#### Para a variável idade (D1A\_ID), o resutado foi uma redução no coeficiente de 0,02 para 0,005 na regressão múltipla, neste último caso, o p-valor do coeficiente deixou de ser significativo. O coeficiente da variável de escolaridade teve sua magnitude ampliada de -0,11 para -0,15. A variável renda mudou o sentido do coeficiente, no entanto, ambos coeficientes obtiveram p-valores não significativos. A nota atribuída ao PT teve desempenho semelhante ao caso bivariado, com pequeno aumento na magnitude do coeficiente da regressão múltipla de -0.411 para -0,415. Por fim, a escala ideológica também teve o coeficiente menor na regressão múltipla do que na simples, passando de 0,40 para 0,32, mas mantendo-se significativa. É importante ressaltar que o número de observações entre as regressões bivariadas e a regressão mútlipla não foi o mesmo devido aos filtros utilizados.

### A partir da exposição de gráficos e testes, avalie se o modelo se adequa aos pressupostos que uma regressão linear exige.

#### O teste de Shapiro para a normalidade dos resíduos foi significativo, portato a hipótese nula da normalidade deve ser rejeitada. A hipótese de homoscedasticidade também pode ser rejeitada rejeitada através do teste de Breusch Pagan.

``` r
require(MASS)
layout(matrix(c(1,2,3,4),2,2))
plot(reg_1)
```

![](exercicio_7_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
resid <- studres(reg_1)
shapiro.test(resid) ## normalidade dos resíduos
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  resid
    ## W = 0.98814, p-value = 0.000000001522

``` r
ncvTest(reg_1) ## heteroscedasticidade
```

    ## Non-constant Variance Score Test 
    ## Variance formula: ~ fitted.values 
    ## Chisquare = 23.31391, Df = 1, p = 0.000001376

``` r
psych::pairs.panels(banco)
```

![](exercicio_7_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

### Caso algum pressuposto não seja satisfeito, quais são as consequências específicas para o modelo estimado?

### Considerando o 4o hurdle do livro *Fundamentals…*, faça um modelo de regressão extra adicionando uma variável **numérica** que foi omitida do modelo anterior, mas que possa ter relação causal com a variável dependente (e talvez alguma associação com as independentes anteriores). Justifique a variável extra e analise o resultado.

#### Uma possível variável explicativa é a nota atribuída à Polícia Federal por conta de seu papel na operação Lava Jato. Eleitores alinhados com a operação podem considerar que Jair Bolsonaro seja um bom político por conter em seus discursos a luta contra a corrupção. A regressão mostra que de fato a variável se adequa ao modelo, com coeficiente de 0.24 e p-valor significativo. O R2 do modelo passou de 28% para 30%, indicando uma capacidade explicativa adicionada da segunda regressão.

``` r
banco_2 <- read_sav('C:\\Users\\Lucas Albuquerque\\Downloads\\04622.sav') %>%
  mutate(Q1607 = as.numeric(Q1607),
         D9 = as.numeric(D9),
         D3_ESCOLA = as.numeric(D3_ESCOLA),
         Q1501 = as.numeric(Q1501),
         Q18 = as.numeric(Q18),
         P1601 = as.numeric(P1601)) %>%
  filter(Q1607 <= 10 &
           D9 != 9999998 & D9 != 9999999 &
           Q1501 <= 10 &
           Q18 <= 10 &
           P1601 <= 10) %>%
  dplyr::select(Q1607, D1A_ID, D3_ESCOLA,D9, Q1501, Q18, P1601)

reg_2 <- lm(Q1607~D1A_ID+D3_ESCOLA+log(D9)+Q1501+Q18 + P1601, data = banco_2)
summary(reg_2)
```

    ## 
    ## Call:
    ## lm(formula = Q1607 ~ D1A_ID + D3_ESCOLA + log(D9) + Q1501 + Q18 + 
    ##     P1601, data = banco_2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.5088 -2.4515  0.4299  2.2948  9.8182 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept)  5.291817   0.948824   5.577  0.00000002918935994 ***
    ## D1A_ID       0.002002   0.006222   0.322              0.74764    
    ## D3_ESCOLA   -0.146953   0.045996  -3.195              0.00143 ** 
    ## log(D9)     -0.116657   0.119248  -0.978              0.32811    
    ## Q1501       -0.410906   0.023889 -17.201 < 0.0000000000000002 ***
    ## Q18          0.280220   0.026505  10.572 < 0.0000000000000002 ***
    ## P1601        0.240267   0.030160   7.967  0.00000000000000331 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.264 on 1427 degrees of freedom
    ## Multiple R-squared:  0.3126, Adjusted R-squared:  0.3097 
    ## F-statistic: 108.2 on 6 and 1427 DF,  p-value: < 0.00000000000000022

``` r
psych::pairs.panels(banco_2)
```

![](exercicio_7_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Compare o resultado obtido com o modelo e conclusões anteriores.
