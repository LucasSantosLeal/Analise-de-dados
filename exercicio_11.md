Exercicio 11
================
Lucas Santos Leal

``` r
library(tidyverse)
library(haven)

link <- "https://github.com/MartinsRodrigo/Analise-de-dados/blob/master/04622.sav?raw=true"

download.file(link, "04622.sav", mode = "wb")

banco <- read_spss("04622.sav") 

banco <- banco %>%
  mutate(D10 = as_factor(D10)) %>%
  filter(Q1607 < 11, 
         Q18 < 11,
         D9 < 9999998,
         Q1501 < 11)


Outras <- levels(banco$D10)[-c(3,5,13)]

banco <- banco %>%
  mutate(religiao = case_when(D10 %in% Outras ~ "Outras",
                              D10 == "Católica" ~ "Católica",
                              D10 == "Evangélica" ~ "Evangélica",
                              D10 == "Não tem religião" ~ "Não tem religião"))
```

### Faça uma regressão linear avaliando em que medida as variáveis independentes utilizadas nos exercícios 7 e 8, idade(D1A\_ID), educação (D3\_ESCOLA), renda (D9), nota atribuída ao PT (Q1501), auto-atribuição ideológica (Q18), sexo (D2\_SEXO) e religião (variável criada no passo anterior) explicam a avaliação de Bolsonaro (Q1607)

``` r
modelo_1 <- lm(Q1607~D1A_ID+
                 D3_ESCOLA +
                 D9+
                 Q1501+
                 Q18+
                 D2_SEXO +
                 religiao, data = banco)


summary(modelo_1)
```

    ## 
    ## Call:
    ## lm(formula = Q1607 ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + 
    ##     D2_SEXO + religiao, data = banco)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.0608 -2.5654  0.4179  2.3268  8.9954 
    ## 
    ## Coefficients:
    ##                             Estimate  Std. Error t value             Pr(>|t|)
    ## (Intercept)               6.21603337  0.53652155  11.586 < 0.0000000000000002
    ## D1A_ID                    0.01040331  0.00623408   1.669             0.095376
    ## D3_ESCOLA                -0.11159236  0.04486448  -2.487             0.012982
    ## D9                       -0.00003620  0.00002764  -1.309             0.190576
    ## Q1501                    -0.39463597  0.02367397 -16.670 < 0.0000000000000002
    ## Q18                       0.31608133  0.02603275  12.142 < 0.0000000000000002
    ## D2_SEXO                  -0.68735722  0.17457327  -3.937            0.0000863
    ## religiaoEvangélica        0.66853691  0.19839649   3.370             0.000772
    ## religiaoNão tem religião -0.07564700  0.34847549  -0.217             0.828177
    ## religiaoOutras           -0.83255901  0.30807160  -2.702             0.006963
    ##                             
    ## (Intercept)              ***
    ## D1A_ID                   .  
    ## D3_ESCOLA                *  
    ## D9                          
    ## Q1501                    ***
    ## Q18                      ***
    ## D2_SEXO                  ***
    ## religiaoEvangélica       ***
    ## religiaoNão tem religião    
    ## religiaoOutras           ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.296 on 1452 degrees of freedom
    ## Multiple R-squared:  0.3018, Adjusted R-squared:  0.2975 
    ## F-statistic: 69.75 on 9 and 1452 DF,  p-value: < 0.00000000000000022

### Faça o teste de homoscedasticidade do modelo e corrija as estimações dos coeficientes caso seja necessário.

#### A análise gráfica indica a presença de heteroscedasticidade. O gráfico dos resíduos vs valores preditos indica uma relação negativa entre os dois, ainda que a média seja zero. O gráfico dos resíduos padronizados mostra que há uma relação não linear entre estes e os valores preditos. O teste de Breusch-Pagan corrobora a análise gráfica. O p-valor significativo indica a possibilidade de rejeição da hipótese nula de que a variância é constante ao longo dos valores das variáveis independentes.

``` r
par(mfrow=c(2,2))
plot(modelo_1)
```

![](exercicio_11_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
car::ncvTest(modelo_1)
```

    ## Non-constant Variance Score Test 
    ## Variance formula: ~ fitted.values 
    ## Chisquare = 22.48512, Df = 1, p = 0.0000021178

#### Para corrigir as estimativas, utilizarei a abordagem dos Mínimos quadrados factíveis o teste indicou uma estatística T de 0.07 e um p-valor de 0.77, não significativo aos níveis razoáveis de .5 e .1, indicando que o modelo\_3 reduziu a heteroscedasticidade.

``` r
u <- modelo_1$residuals
ln_u2 <- log(u^2)
modelo_u <- lm(ln_u2~D1A_ID+
                 D3_ESCOLA +
                 D9+
                 Q1501+
                 Q18+
                 D2_SEXO +
                 religiao, data = banco)
g <- modelo_u$fitted.values
h <-  exp(g)

modelo_2 <- lm(Q1607~D1A_ID+
                 D3_ESCOLA +
                 D9+
                 Q1501+
                 Q18+
                 D2_SEXO +
                 religiao, weights = 1/h,data = banco)
summary(modelo_2)
```

    ## 
    ## Call:
    ## lm(formula = Q1607 ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + 
    ##     D2_SEXO + religiao, data = banco, weights = 1/h)
    ## 
    ## Weighted Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -6.111 -1.198  0.216  1.195  4.231 
    ## 
    ## Coefficients:
    ##                             Estimate  Std. Error t value             Pr(>|t|)
    ## (Intercept)               5.99920270  0.50884236  11.790 < 0.0000000000000002
    ## D1A_ID                    0.00488756  0.00596909   0.819             0.413029
    ## D3_ESCOLA                -0.08846137  0.04368114  -2.025             0.043033
    ## D9                       -0.00004685  0.00002632  -1.780             0.075345
    ## Q1501                    -0.38402386  0.02523841 -15.216 < 0.0000000000000002
    ## Q18                       0.34391767  0.02535871  13.562 < 0.0000000000000002
    ## D2_SEXO                  -0.58856324  0.17266109  -3.409             0.000670
    ## religiaoEvangélica        0.70879523  0.18633782   3.804             0.000148
    ## religiaoNão tem religião -0.34210079  0.34124256  -1.003             0.316262
    ## religiaoOutras           -0.83677396  0.29806140  -2.807             0.005061
    ##                             
    ## (Intercept)              ***
    ## D1A_ID                      
    ## D3_ESCOLA                *  
    ## D9                       .  
    ## Q1501                    ***
    ## Q18                      ***
    ## D2_SEXO                  ***
    ## religiaoEvangélica       ***
    ## religiaoNão tem religião    
    ## religiaoOutras           ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.681 on 1452 degrees of freedom
    ## Multiple R-squared:  0.302,  Adjusted R-squared:  0.2977 
    ## F-statistic:  69.8 on 9 and 1452 DF,  p-value: < 0.00000000000000022

``` r
car::ncvTest(modelo_2)
```

    ## Non-constant Variance Score Test 
    ## Variance formula: ~ fitted.values 
    ## Chisquare = 0.07871593, Df = 1, p = 0.77905

``` r
par(mfrow = c(1,2))
plot(modelo_1,3)
plot(modelo_2,3)
```

![](exercicio_11_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### Avalie a multicolinearidade entre as variáveis

#### O teste de VIF mostra que todas as variáveis incluidas no modelo\_1 estão abaixo do limite de 4 no valor do VIF indicando que a colinearidade entre as variáveis é baixa.

``` r
if(!require(PerformanceAnalytics)){install.packages("PerformanceAnalytics")};require(PerformanceAnalytics)

dat <- banco[c('Q1607', 'D1A_ID', 'D3_ESCOLA', 'D9', 'Q1501','Q18')]

chart.Correlation(dat, method = 'pearson', histogram = TRUE, pch = 10)
```

![](exercicio_11_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
olsrr::ols_vif_tol(modelo_1)
```

    ##                  Variables Tolerance      VIF
    ## 1                   D1A_ID 0.8200747 1.219401
    ## 2                D3_ESCOLA 0.7477374 1.337368
    ## 3                       D9 0.9133681 1.094849
    ## 4                    Q1501 0.8930021 1.119818
    ## 5                      Q18 0.9531118 1.049195
    ## 6                  D2_SEXO 0.9775164 1.023001
    ## 7       religiaoEvangélica 0.8626773 1.159182
    ## 8 religiaoNão tem religião 0.9261817 1.079702
    ## 9           religiaoOutras 0.8985630 1.112888

### Verifique a presença de outilier ou observações influentes no modelo

#### Embora algumas observações chamem atenção, nenhuma está além das linhas tracejadas vermelhas da distância de Cook, não sendo portando influentes de forma diferente das outras.

``` r
par(mfrow = c(1,2))
plot(modelo_1, 5)
plot(modelo_2, 5)
```

![](exercicio_11_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Faça a regressao linear sem a observação mais influente e avalie a alteração do resultad

#### De acordo com o gráfico da distância de Cook, deve-se retirar a observação 553. Após a retirada da observação mais influentes, pôde-se observar uma leve aleração nos coeficientes da regressão, mas com efeitos praticamente imperceptíveis. No mesmo sentido, os sinais dos coeficiêntes permaneceu inalterado.

### A remoção das três variáveis mais influentes causou alterações mais perceptíveis nos coeficientes da regressão, no entando, seus sinais foram mantidos, assim como a proporção dos impactos na variável dependente.

``` r
modelo_3 <- lm(Q1607~D1A_ID+
                 D9+
                 D3_ESCOLA +
                 Q1501+
                 Q18+
                 D2_SEXO +
                 religiao,data = banco[-553,]) ### modelo sem uma variável

summary(modelo_3)
```

    ## 
    ## Call:
    ## lm(formula = Q1607 ~ D1A_ID + D9 + D3_ESCOLA + Q1501 + Q18 + 
    ##     D2_SEXO + religiao, data = banco[-553, ])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.0675 -2.5701  0.4203  2.3307  8.9970 
    ## 
    ## Coefficients:
    ##                             Estimate  Std. Error t value             Pr(>|t|)
    ## (Intercept)               6.21459610  0.53664514  11.580 < 0.0000000000000002
    ## D1A_ID                    0.01057403  0.00624194   1.694             0.090474
    ## D9                       -0.00003637  0.00002765  -1.315             0.188578
    ## D3_ESCOLA                -0.11226608  0.04488840  -2.501             0.012494
    ## Q1501                    -0.39486194  0.02368218 -16.673 < 0.0000000000000002
    ## Q18                       0.31643016  0.02604497  12.149 < 0.0000000000000002
    ## D2_SEXO                  -0.68988941  0.17466271  -3.950             0.000082
    ## religiaoEvangélica        0.66935007  0.19844484   3.373             0.000763
    ## religiaoNão tem religião -0.07397330  0.34856345  -0.212             0.831963
    ## religiaoOutras           -0.81644869  0.30930647  -2.640             0.008389
    ##                             
    ## (Intercept)              ***
    ## D1A_ID                   .  
    ## D9                          
    ## D3_ESCOLA                *  
    ## Q1501                    ***
    ## Q18                      ***
    ## D2_SEXO                  ***
    ## religiaoEvangélica       ***
    ## religiaoNão tem religião    
    ## religiaoOutras           ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.297 on 1451 degrees of freedom
    ## Multiple R-squared:  0.302,  Adjusted R-squared:  0.2977 
    ## F-statistic: 69.76 on 9 and 1451 DF,  p-value: < 0.00000000000000022

``` r
plot(modelo_3, 4)
```

![](exercicio_11_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
modelo_4 <- lm(Q1607~D1A_ID+
                 D9+
                 D3_ESCOLA +
                 Q1501+
                 Q18+
                 D2_SEXO +
                 religiao,data = banco[-c(1442,553,491),])

summary(modelo_4)
```

    ## 
    ## Call:
    ## lm(formula = Q1607 ~ D1A_ID + D9 + D3_ESCOLA + Q1501 + Q18 + 
    ##     D2_SEXO + religiao, data = banco[-c(1442, 553, 491), ])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.1560 -2.4705  0.3664  2.3193  8.9859 
    ## 
    ## Coefficients:
    ##                             Estimate  Std. Error t value             Pr(>|t|)
    ## (Intercept)               6.24243051  0.53537325  11.660 < 0.0000000000000002
    ## D1A_ID                    0.01151591  0.00624189   1.845             0.065250
    ## D9                       -0.00006485  0.00003067  -2.114             0.034650
    ## D3_ESCOLA                -0.10755671  0.04504756  -2.388             0.017085
    ## Q1501                    -0.39959525  0.02367503 -16.878 < 0.0000000000000002
    ## Q18                       0.31732675  0.02598410  12.212 < 0.0000000000000002
    ## D2_SEXO                  -0.69793394  0.17458385  -3.998            0.0000672
    ## religiaoEvangélica        0.67981753  0.19801767   3.433             0.000614
    ## religiaoNão tem religião -0.06113731  0.34768765  -0.176             0.860444
    ## religiaoOutras           -0.74179536  0.30970869  -2.395             0.016740
    ##                             
    ## (Intercept)              ***
    ## D1A_ID                   .  
    ## D9                       *  
    ## D3_ESCOLA                *  
    ## Q1501                    ***
    ## Q18                      ***
    ## D2_SEXO                  ***
    ## religiaoEvangélica       ***
    ## religiaoNão tem religião    
    ## religiaoOutras           *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.288 on 1449 degrees of freedom
    ## Multiple R-squared:  0.3054, Adjusted R-squared:  0.3011 
    ## F-statistic:  70.8 on 9 and 1449 DF,  p-value: < 0.00000000000000022

``` r
plot(modelo_4,4)
```

![](exercicio_11_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->
