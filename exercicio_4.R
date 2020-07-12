
## Faça todos os gráficos utilizando um tema que você ache mais adequado
## e nomeie os eixos x e y da maneira adequada

## Carregue o banco world do pacote poliscidata

library(poliscidata)
require(tidyverse)
banco <- world

## Observe o banco de dados com as funções adequadas
glimpse(banco)


## A variável democ_regime08 indica se um país é democrático.
## Usando as ferramentas de manipulacao de bancos de dados, verifique
## quantos paises sao democraticos ou nao, e apresente esta variável 
## graficamente


table(banco$democ_regime08)  ## 69 países não democráticos e 95 países democráticos

ggplot(banco, aes(democ_regime08)) + geom_bar()

## Teste a relação entre a variável democ_regime08 e a variável
## muslim (que indica se um país é muçulmano ou não). E represente
## visualmente as variáveis para visualizar se esta religião
## aumenta ou diminui a chance de um país ser democrático
## Qual seria sua conclusão com relação a associação destas duas
## variáveis?

tabela <- table(banco$democ_regime08, banco$muslim)
prop.table(tabela)
chisq.test(tabela)
library(graphics)
mosaicplot(tabela, shade = TRUE)

library(vcd)
assoc(tabela, shade = TRUE)

## ser islãmico aumenta a probabilidade de um país não ser democrático, assim como
## não ser islãmico aumenta a probabilidade de um país ser democrático.


## A variável gdppcap08 possui informação sobre o PIB per capta
## dos países. Faça uma representação gráfica desta variável

ggplot(banco, aes(x = "", y = gdppcap08)) + geom_boxplot()



## Faça um sumario com a média, mediana e desvio padrão do pib per capta
## para cada tipo de regime politico, represente a associação destas
## variáveis graficamente, e faça o teste estatístico adequado para
## chegar a uma conclusão. Existe associaçào entre as variáveis?

banco %>%
  filter(!is.na(gdppcap08),
         !is.na(democ_regime08)) %>%
  group_by(democ_regime08) %>%
  summarise(mediana = median(gdppcap08),
            media = mean(gdppcap08),
            desvio = sd(gdppcap08),
            n = n())

t.test(gdppcap08~democ_regime08, data = banco)

## o teste t indica que há diferenca nas médias do PIB per capta dos países de acordo com
## o tipo de regime. Os países não democráticos têm uma 
## média de 9.243 enquanto os não islâmicos têm média 16.351 O teste 
## é estatisticamente significativo no nível de 0.05.


## Por fim, ao invés de utilizar uma variável binária de democracia,
## utilize a variável dem_score14 para avaliar se existe uma associação
## entre regime político e desenvolvimento econômico. Represente
## a associação graficamente, faça o teste estatístico e explica sua
## conclusão

cor.test(banco$dem_score14, banco$gdppcap08)
ggplot(banco, aes(dem_score14, log(gdppcap08))) + 
  geom_point() + geom_smooth(method = lm, se = FALSE)

## o teste de correlação indica que as variáveis dem_score14 e gdpppcap possuem uma associação
## mediana, com coeficiente de 0.505. O p-valor significativo reforça a hipótese de que esta 
## associação é diferente de zero.

## Teste a associação entre renda perca capta e religiao (com a variável
## muslim) e represente graficamente. Qual é sua conclusão? 

t.test(gdppcap08~muslim, data = banco)

ggplot(banco, aes(gdppcap08, fill = muslim)) +
  geom_density(alpha = 0.3) +
  labs(x = "Frequência",
       y = "gdppcap")


ggplot(banco, aes(muslim, gdppcap08)) + geom_boxplot()

## o teste t indica que há diferenca nas médias do PIB per capta dos países de acordo com
## sua religião. Os países islâmicos têm uma média de 9.061 enquanto os não islâmicos têm 
## média 15.325. O teste é estatisticamente significante no nível de 0.05.


## Comparando suas conclusões anteriores, é possível afirmar qual
## das duas variáveis possui maior impacto no desenvolvimento economico?
## Por que? 

## Enquanto a diferença de renda entre os países democráticos e não democráticos é de 7.107
## a diferença de renda entre países islãmicos e não islãmicos é de 6.264. Isso indica que
## ser um país não democrático têm maior influência na renda per capta, quando essas variáveis
## são consideradas isoladamente.

##########################################################################

## Exercício teórico
## Levando em consideração as variáveis de seu trabalho final,
## qual dos 3 testes estatísticos utilizados seria adequado utilizar?
