require(tidyverse)
#
# Suponha que tenhamos o dataframe df abaixo
#
# x     y
# A     5
# A     3
# B     8
# B    12
#
# Complete o código que obtém o seguinte resultado:
#
#        z
#        7
#
df %>%
summarise(z = mean(y))

#######################################################################

# Suponha que tenhamos o dataframe df abaixo
#
# y1    y2    y3    y4
# 8.04  9.14  7.46  6.58
# 6.95  8.14  6.77  5.76
# 7.58  8.74  12.74 7.71
#
# Complete o código que obtém o seguinte resultado:
#
# y1    
# 8.04  
# 6.95  
# 7.58 

df %>% select(y1)
  
#######################################################################

# Suponha que tenhamos o dataframe df abaixo
#
#    x  y
#   1  10
#   6  8
#   2  3
#   4  5
#
# Complete o código que obtém o seguinte resultado, fazendo uma operação
# entre x e y
#
#    x  y   z
#   1  10  -9
#   6  8   -2
#   2  3   -1
#   4  5   -1
#

df %>%
mutate(z = x-y)
  
########################################################################

#
# Suponha que tenhamos o dataframe df abaixo
#
#    city sales
# Boston   220
# Boston   125
#    NYC   150
#    NYC   250
#
# Complete o código que obtém o seguinte resultado:
#
# city   avg_sales
# Boston      172
# NYC         200 


df %>%
  group_by(city) %>%
  summarise(avg_sales = mean(sales))
  
########################################################################

# Suponha que tenhamos o dataframe df abaixo
#
#week   min   max
#  3    55    60
#  2    52    56
#  1    60    63
#  4    65    67
#
# Complete o código que obtém o seguinte resultado:
#
#week   min   max
#  1    60    63
#  2    52    56
#  3    55    60
#  4    65    67


df %>% arrange(week)

########################################################################

# Suponha que tenhamos o dataframe df abaixo
#
# x_b_1  x_b_2  y_c_1  y_c_2
#  A      2      W1     25
#  A      4      W2     21
#  B      6      W1     26
#  B      8      W2     30
#
# Complete o código que obtém o seguinte resultado:
#
# y_c_1  y_c_2
#  W1     25
#  W2     21
#  W1     26
#  W2     30

df %>%
select(starts_with(y))

#########################################################################
iris 
# Suponha que tenhamos o dataframe df abaixo
#
# Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
# 78           6.7         3.0          5.0         1.7 versicolor
# 121          6.9         3.2          5.7         2.3  virginica
# 11           5.4         3.7          1.5         0.2     setosa
# 92           6.1         3.0          4.6         1.4 versicolor
# 146          6.7         3.0          5.2         2.3  virginica
# 62           5.9         3.0          4.2         1.5 versicolor
# 50           5.0         3.3          1.4         0.2     setosa
# 17           5.4         3.9          1.3         0.4     setosa
# 69           6.2         2.2          4.5         1.5 versicolor
# 143          5.8         2.7          5.1         1.9  virginica
#
# Complete o código que obtém o seguinte resultado:
#
#Species      Sepal.Area
#versicolor      20.10
#virginica       22.08
#setosa          19.98
#versicolor      18.30
#virginica       20.10
#versicolor      17.70
#setosa          16.50
#setosa          21.06
#versicolor      13.64
#virginica      15.66


df %>%
  mutate(sepal.area = Sepal.Length*Sepal.Width) %>% select(Species, sepal.area)

########################################################################

# Suponha que tenhamos o dataframe df abaixo
#
#name         start       end         party     
#Eisenhower   1953-01-20  1961-01-20  Republican
#Kennedy      1961-01-20  1963-11-22  Democratic
#Johnson      1963-11-22  1969-01-20  Democratic
#Nixon        1969-01-20  1974-08-09  Republican
#Ford         1974-08-09  1977-01-20  Republican
#Carter       1977-01-20  1981-01-20  Democratic
#Reagan       1981-01-20  1989-01-20  Republican
#Bush         1989-01-20  1993-01-20  Republican
#Clinton      1993-01-20  2001-01-20  Democratic
#Bush         2001-01-20  2009-01-20  Republican
#Obama        2009-01-20  2017-01-20  Democratic
#
#Crie um código abaixo para que se altere a variável party
#deixando apenas a primeira letra dos partidos


df %>% mutate(party = str_sub(party, end = 1))


###############################################################################

# No pacote poliscidata existe um banco de dados chamado nes, com informações 
# do American National Election Survey. Para os exerícicios a seguir, instale 
# o pacote poliscidata e tidyverse, carregue-os e crie um objeto chamado
# df com os dados do nes. 
require(poliscidata)
require(tidyverse)
df <- nes

# Faça uma primeira exploração do banco de dados com todos os comandos
# passados até aqui que possuem esse objetivo
glimpse(df)
summary(df)
head(df)
str(df)

# Quantos respondentes possui na pesquisa?

## 5916 respondentes

# Caso queiram ter mais informações sobre as variáveis do nes, basta rodar
# o código `?nes`, que no canto inferior direito aparecerá uma descrição.
# Como temos muitas variáveis, deixe apenas as colunas
# ftgr_cons, dem_raceeth, voted2012, science_use, preknow3, obama_vote
# income5, gender.

df <- nes %>% select(ftgr_cons, dem_raceeth, voted2012, science_use,
                     preknow3, obama_vote, income5, gender)



# Se quisermos ter informações apenas de pessoas que votaram na
# eleição de 2012, podemos usar a variável voted2012. Tire do banco
# os respondentes que não votaram


df <- df %>% filter(voted2012 == 'Voted')


# Quantos respondentes sobraram?

## 4404 respondentes

# Crie uma variável chamada white que indica se o respondente é branco
# ou não a partir da variável dem_raceeth, crie a variável ideology a
# partir da variável ftgr_cons (0-33 como liberal, 34 a 66 como centro,
# e 67 a 100 como conservador), ao mesmo tempo em que muda
# a variável obama_vote para trocar o 1 por "Sim" e 2 por "não"


table(df$dem_raceeth)
df$White <- ifelse(df$dem_raceeth == '1. White non-Hispanic', 'Yes', 'No')
df <- df %>% mutate(ideology = case_when(ftgr_cons <= 33 ~'liberal',
                                         ftgr_cons <= 66 ~ 'centro',
                                         ftgr_cons > 66 ~'conservador'),
                    obama_vote = case_when(obama_vote == 0 ~ 'Não', obama_vote == 1~'Sim'))

# Demonstre como observar a quantidade de pessoas em cada uma das
# categorias de science_use
table(df$science_use)


# Demonstre como observar a média de conservadorismo (variável 
# ftgr_cons) para cada categoria de science_use

df %>% group_by(science_use) %>% filter(ideology == 'conservador') %>% summarise(mean(ftgr_cons))
  
###############################################################################

# Responder as questões teóricas da aula abaixo



