Exercicio 10
================

### Continuaremos com a utilização dos dados do ESEB2018. Carregue o banco da mesma forma que nos exercicios anteriores

``` r
library(tidyverse)
library(haven)
require(margins)
require(dotwhisker)
#install.packages('caret')
#install.packages('e1071')
require(e1071)
require(caret)


link <- "https://github.com/MartinsRodrigo/Analise-de-dados/blob/master/04622.sav?raw=true"

download.file(link, "04622.sav", mode = "wb")

banco <- read_spss("04622.sav") 

banco <- banco %>%
  mutate(D10 = as_factor(D10)) %>%
  filter(Q18 < 11,
         D9 < 9999998,
         Q1501 < 11,
         Q12P2_B < 3) %>%
  mutate(Q12P2_B = case_when(Q12P2_B == 1 ~ 0,  # Quem votou em Haddad = 0
                             Q12P2_B == 2 ~ 1)) # Quem votou em Bolsonaro = 1
```

### Crie a mesma variável de religião utilizada no exercício anterior

``` r
Outras <- levels(banco$D10)[-c(3,5,13)]

banco <- banco %>%
  mutate(religiao = case_when(D10 %in% Outras ~ "Outras",
                              D10 == "Católica" ~ "Católica",
                              D10 == "Evangélica" ~ "Evangélica",
                              D10 == "Não tem religião" ~ "Não tem religião"))
```

### Faça uma regressão linear utilizando as mesmas variáveis do exercício 9 - idade(D1A\_ID), educação (D3\_ESCOLA), renda (D9), nota atribuída ao PT (Q1501), auto-atribuição ideológica (Q18), sexo (D2\_SEXO) e religião (variável criada no passo anterior) - explicam o voto em Bolsonaro (Q12P2\_B).

``` r
modelo_1 <- lm(Q12P2_B~D1A_ID+D3_ESCOLA+D9+Q1501+Q18+D2_SEXO+religiao, data = banco)
summary(modelo_1)
```

    ## 
    ## Call:
    ## lm(formula = Q12P2_B ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + 
    ##     D2_SEXO + religiao, data = banco)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.05532 -0.19854  0.01565  0.16182  0.96682 
    ## 
    ## Coefficients:
    ##                               Estimate    Std. Error t value
    ## (Intercept)               0.7066528237  0.0646893413  10.924
    ## D1A_ID                    0.0011401012  0.0007538896   1.512
    ## D3_ESCOLA                 0.0055466625  0.0052257983   1.061
    ## D9                       -0.0000009837  0.0000031963  -0.308
    ## Q1501                    -0.0772824015  0.0027990853 -27.610
    ## Q18                       0.0265094568  0.0030932523   8.570
    ## D2_SEXO                  -0.0528631872  0.0208943372  -2.530
    ## religiaoEvangélica        0.0768358985  0.0236336987   3.251
    ## religiaoNão tem religião -0.0027459861  0.0423769173  -0.065
    ## religiaoOutras           -0.0726267771  0.0367795766  -1.975
    ##                                      Pr(>|t|)    
    ## (Intercept)              < 0.0000000000000002 ***
    ## D1A_ID                                0.13074    
    ## D3_ESCOLA                             0.28873    
    ## D9                                    0.75832    
    ## Q1501                    < 0.0000000000000002 ***
    ## Q18                      < 0.0000000000000002 ***
    ## D2_SEXO                               0.01154 *  
    ## religiaoEvangélica                    0.00118 ** 
    ## religiaoNão tem religião              0.94835    
    ## religiaoOutras                        0.04855 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3489 on 1138 degrees of freedom
    ## Multiple R-squared:  0.5028, Adjusted R-squared:  0.4989 
    ## F-statistic: 127.9 on 9 and 1138 DF,  p-value: < 0.00000000000000022

### Interprete o resultado dos coeficientes

#### Como a variável dependente pode assumir dois valores, 0 e 1, pode-se interpretar a regressão como um modelo linear de probabilidade. Os resultados indicam que idade, escolaridade, escala ideológica e religião evangélica estão associadas a uma probabilidade maior de votar em bolsonaro em relação às observações de referência (homens católicos). O aumento de uma no de idade está associado a um aumento de 0.001 na probabilidade do voto em Bolsonaro, o aumento em um ano de escolaridade está associado a um aumento de 0.005 na probabilidade, a escala ideológica está associada a um aumento na probabilidade de 0.026 e por fim, ser evangélico está associado a um aumento de 0.07 na probabilidade, em relação à religião católica. No sentido contrário, renda, nota atribuída ao PT, sexo, sem religião e outras religiões estão associadas à probabilidades menores de voto em bolsonaro. Para cada aumento de uma unidade na variável de nota atribuida ao PT está associada uma queda na probabilidade de voto em Bolsonaro de 0.07, ser do sexo feminino está associado a uma queda de 0.05 na probabilidade de voto, não ter religião e religiões outras que não católica e evangélica estão associadas a quedas de 0.002 e 0.07, respectivamente. Idade, escolaridade, renda e nenhuma relgião tiveram coeficientes não significantes estatísticamente, o que impede a rejeição da hipótese de que sejam zero. Nesse contexto, o R2 não possui interpretação relevante, e deve-se estar atento para a necessidade de erros padrão robustos, devido à heteroscedasticidade.

### O resultado difere dos encontrados anteriormente, quando a variavel dependente era a aprovação de Bolsonaro?

#### A direção esperada das relações entre as variáveis foi semelhante à dos exercícios anteriores, apenas a variável escolaridade (D3\_ESCOLA) que teve o sentido invertido no modelo de probabilidade linear.

``` r
### Faça uma regressão logistica com as mesmas variaveis

modelo_2 <- glm(Q12P2_B~D1A_ID+
                  D3_ESCOLA+
                  D9+
                  Q1501+
                  Q18+
                  D2_SEXO+
                  religiao, family = binomial(link = 'logit'), data = banco)

summary(modelo_2)
```

    ## 
    ## Call:
    ## glm(formula = Q12P2_B ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + 
    ##     D2_SEXO + religiao, family = binomial(link = "logit"), data = banco)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7529  -0.5625   0.2518   0.4744   2.5830  
    ## 
    ## Coefficients:
    ##                              Estimate   Std. Error z value             Pr(>|z|)
    ## (Intercept)               0.820905327  0.529763447   1.550              0.12124
    ## D1A_ID                    0.010013686  0.006336810   1.580              0.11405
    ## D3_ESCOLA                 0.056341787  0.043575753   1.293              0.19602
    ## D9                       -0.000004635  0.000023959  -0.193              0.84660
    ## Q1501                    -0.467805560  0.026663935 -17.545 < 0.0000000000000002
    ## Q18                       0.224213882  0.027479605   8.159 0.000000000000000337
    ## D2_SEXO                  -0.449713328  0.173903438  -2.586              0.00971
    ## religiaoEvangélica        0.621655696  0.198470380   3.132              0.00173
    ## religiaoNão tem religião -0.021056111  0.347756068  -0.061              0.95172
    ## religiaoOutras           -0.673554187  0.312177200  -2.158              0.03096
    ##                             
    ## (Intercept)                 
    ## D1A_ID                      
    ## D3_ESCOLA                   
    ## D9                          
    ## Q1501                    ***
    ## Q18                      ***
    ## D2_SEXO                  ** 
    ## religiaoEvangélica       ** 
    ## religiaoNão tem religião    
    ## religiaoOutras           *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1557.84  on 1147  degrees of freedom
    ## Residual deviance:  862.45  on 1138  degrees of freedom
    ## AIC: 882.45
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
### Transforme os coeficientes estimados em probabilidade



summary(margins(modelo_2))
```

    ##                    factor     AME     SE        z      p   lower   upper
    ##                    D1A_ID  0.0012 0.0007   1.5849 0.1130 -0.0003  0.0026
    ##                   D2_SEXO -0.0526 0.0202  -2.6078 0.0091 -0.0921 -0.0131
    ##                 D3_ESCOLA  0.0066 0.0051   1.2949 0.1953 -0.0034  0.0166
    ##                        D9 -0.0000 0.0000  -0.1935 0.8466 -0.0000  0.0000
    ##                     Q1501 -0.0547 0.0009 -57.9079 0.0000 -0.0566 -0.0529
    ##                       Q18  0.0262 0.0030   8.8434 0.0000  0.0204  0.0320
    ##        religiaoEvangélica  0.0735 0.0235   3.1280 0.0018  0.0274  0.1195
    ##  religiaoNão tem religião -0.0025 0.0417  -0.0605 0.9517 -0.0842  0.0791
    ##            religiaoOutras -0.0817 0.0379  -2.1574 0.0310 -0.1560 -0.0075

``` r
efeito_marginal <- summary(margins(modelo_2)) %>%
  rename(term = factor,
         estimate = AME,
         std.error = SE,
         statistic = z,
         p.value = p) %>%
  arrange(estimate)

dwplot(efeito_marginal,
       order_vars = c('Q18', 'Q1501',
                                       'D2_SEXO', 'D9',
                                       'D1A_ID', 'D3_ESCOLA',
                                       'religiaoOutras', 'religiaoNão tem religião',
                                       'religiaoEvangélica'),
       vline = geom_vline(xintercept = 0))
```

![](exercicio_10_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### Quais foram as diferenças no resultado entre usar a regressão linear e a logistica?

#### As probabilidades estimadas pelos dois modelos foram semelhantes e para todas as variáveis o sentido da relação com a variável dependente foi o mesmo.

### Verifique a quantidade de classificações corretas da regressao logistica e avalie o resultado

#### a matriz de confusão mostra que o modelo tem uma precisão de 83% dos valores da variável de voto para presidente. Dos respondentes que votaram em Haddad, o modelo previu corretamente 376 e errou 96, ou seja acertou 79%. Dos que votaram em Bolsonaro, o modelo previu corretamente 577 e errou 100, ou seja 85% de precisão. A precisão do modelo foi maior que a taxa de não informação, indicando que houve acréscimo na capacidade preditiva ao adicionar as variáveis explicativas.

``` r
preditas_3 <- as.numeric(modelo_2$fitted.values >= 0.5)
caret::confusionMatrix(as.factor(banco$Q12P2_B), as.factor(preditas_3))
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 376 100
    ##          1  95 577
    ##                                              
    ##                Accuracy : 0.8301             
    ##                  95% CI : (0.8071, 0.8514)   
    ##     No Information Rate : 0.5897             
    ##     P-Value [Acc > NIR] : <0.0000000000000002
    ##                                              
    ##                   Kappa : 0.6495             
    ##                                              
    ##  Mcnemar's Test P-Value : 0.7745             
    ##                                              
    ##             Sensitivity : 0.7983             
    ##             Specificity : 0.8523             
    ##          Pos Pred Value : 0.7899             
    ##          Neg Pred Value : 0.8586             
    ##              Prevalence : 0.4103             
    ##          Detection Rate : 0.3275             
    ##    Detection Prevalence : 0.4146             
    ##       Balanced Accuracy : 0.8253             
    ##                                              
    ##        'Positive' Class : 0                  
    ##
