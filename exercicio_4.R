
## Fa�a todos os gr�ficos utilizando um tema que voc� ache mais adequado
## e nomeie os eixos x e y da maneira adequada

## Carregue o banco world do pacote poliscidata

library(poliscidata)
require(tidyverse)
banco <- world

## Observe o banco de dados com as fun��es adequadas
glimpse(banco)


## A vari�vel democ_regime08 indica se um pa�s � democr�tico.
## Usando as ferramentas de manipulacao de bancos de dados, verifique
## quantos paises sao democraticos ou nao, e apresente esta vari�vel 
## graficamente


table(banco$democ_regime08)  ## 69 pa�ses n�o democr�ticos e 95 pa�ses democr�ticos

ggplot(banco, aes(democ_regime08)) + geom_bar()

## Teste a rela��o entre a vari�vel democ_regime08 e a vari�vel
## muslim (que indica se um pa�s � mu�ulmano ou n�o). E represente
## visualmente as vari�veis para visualizar se esta religi�o
## aumenta ou diminui a chance de um pa�s ser democr�tico
## Qual seria sua conclus�o com rela��o a associa��o destas duas
## vari�veis?

tabela <- table(banco$democ_regime08, banco$muslim)
prop.table(tabela)
chisq.test(tabela)
library(graphics)
mosaicplot(tabela, shade = TRUE)

library(vcd)
assoc(tabela, shade = TRUE)

## ser isl�mico aumenta a probabilidade de um pa�s n�o ser democr�tico, assim como
## n�o ser isl�mico aumenta a probabilidade de um pa�s ser democr�tico.


## A vari�vel gdppcap08 possui informa��o sobre o PIB per capta
## dos pa�ses. Fa�a uma representa��o gr�fica desta vari�vel

ggplot(banco, aes(x = "", y = gdppcap08)) + geom_boxplot()



## Fa�a um sumario com a m�dia, mediana e desvio padr�o do pib per capta
## para cada tipo de regime politico, represente a associa��o destas
## vari�veis graficamente, e fa�a o teste estat�stico adequado para
## chegar a uma conclus�o. Existe associa��o entre as vari�veis?

banco %>%
  filter(!is.na(gdppcap08),
         !is.na(democ_regime08)) %>%
  group_by(democ_regime08) %>%
  summarise(mediana = median(gdppcap08),
            media = mean(gdppcap08),
            desvio = sd(gdppcap08),
            n = n())

t.test(gdppcap08~democ_regime08, data = banco)

## o teste t indica que h� diferenca nas m�dias do PIB per capta dos pa�ses de acordo com
## o tipo de regime. Os pa�ses n�o democr�ticos t�m uma 
## m�dia de 9.243 enquanto os n�o isl�micos t�m m�dia 16.351 O teste 
## � estatisticamente significativo no n�vel de 0.05.


## Por fim, ao inv�s de utilizar uma vari�vel bin�ria de democracia,
## utilize a vari�vel dem_score14 para avaliar se existe uma associa��o
## entre regime pol�tico e desenvolvimento econ�mico. Represente
## a associa��o graficamente, fa�a o teste estat�stico e explica sua
## conclus�o

cor.test(banco$dem_score14, banco$gdppcap08)
ggplot(banco, aes(dem_score14, log(gdppcap08))) + 
  geom_point() + geom_smooth(method = lm, se = FALSE)

## o teste de correla��o indica que as vari�veis dem_score14 e gdpppcap possuem uma associa��o
## mediana, com coeficiente de 0.505. O p-valor significativo refor�a a hip�tese de que esta 
## associa��o � diferente de zero.

## Teste a associa��o entre renda perca capta e religiao (com a vari�vel
## muslim) e represente graficamente. Qual � sua conclus�o? 

t.test(gdppcap08~muslim, data = banco)

ggplot(banco, aes(gdppcap08, fill = muslim)) +
  geom_density(alpha = 0.3) +
  labs(x = "Frequ�ncia",
       y = "gdppcap")


ggplot(banco, aes(muslim, gdppcap08)) + geom_boxplot()

## o teste t indica que h� diferenca nas m�dias do PIB per capta dos pa�ses de acordo com
## sua religi�o. Os pa�ses isl�micos t�m uma m�dia de 9.061 enquanto os n�o isl�micos t�m 
## m�dia 15.325. O teste � estatisticamente significante no n�vel de 0.05.


## Comparando suas conclus�es anteriores, � poss�vel afirmar qual
## das duas vari�veis possui maior impacto no desenvolvimento economico?
## Por que? 

## Enquanto a diferen�a de renda entre os pa�ses democr�ticos e n�o democr�ticos � de 7.107
## a diferen�a de renda entre pa�ses isl�micos e n�o isl�micos � de 6.264. Isso indica que
## ser um pa�s n�o democr�tico t�m maior influ�ncia na renda per capta, quando essas vari�veis
## s�o consideradas isoladamente.

##########################################################################

## Exerc�cio te�rico
## Levando em considera��o as vari�veis de seu trabalho final,
## qual dos 3 testes estat�sticos utilizados seria adequado utilizar?
