############# Distribuição do dado e GLM #############

# Dica de atalhos do teclado:
# Para fazer o pipe (%>%) = ctrl + shif + M


######### Distribuição do Dado #########

# Qual distribuição é mais próxima do meu dado? 

# Referência online
# https://stats.stackexchange.com/questions/132652/how-to-determine-which-distribution-fits-my-data-best

# Acho que não devo usar o dado transformado, pois a transformação já é uma
# tentativa de tentar fazer o dado caber em algo. Além disso, seria ideal não 
# transformar pois, para descobrir a distribuição, seria interessante ter "a cara 
# original do dado".
# No script do github "Normalidade e Homocedasticidade" tem a comparação dos 
# histogramas e da normalidade com dados transformados e não transformados. É
# possível ver que a transformação dá uma compensanda, fazendo com que o  extremo 
# fique não tão extremo. 


install.packages("fitdistrplus")
library(fitdistrplus)
library(readxl)
library(dplyr)

rm(list = ls())


## Função para transformar o dado e caber dentro do da distribuição beta
# (que só aceita valores entre 0 e 1 com intervalos abertos).
# O objetivo é amaciar quem é exatamente 1 e 0.

y.transf.betareg <- function(y){ # função aplicada só no vetor y (não aplicada no x, só no y).
  n.obs <- sum(!is.na(y)) # número de observações recebe soma diferente de NA = quantos vazios temos no nosso vetor y
  (y * (n.obs - 1) + 0.5) / n.obs # y * número de observacoes - 1 + 0.5 / número de observações.
}

# Entendendo a função:
#  Quando se faz função, se diz o que ela recebe como argumento. O que está em 
# parenteses é o que a função vai receber (que é y no caso, mas poderia  colocar 
# qualquer outra coisa).
# Lá dentro da função, y é o cara que entra, se eu tivesse uma variável y fora do
# código, não deveria afetar. O nome y vale no escopo da função (as coisas que
# estão fora das funções são globais, ou seja, valem pro todo. Logo y fora do função 
# não existe). 
# Aí depois, quando eu for chamar a função, eu vou especificar pra ela que y são 
# as colunas dos grupos funcionais.
# O y é um vetor e vai ser minhas observações (portanto, os grupos funcionais) e
# aí a função vai olhar do y quem é NA e então vai somar todo mundo que não é NA 
# (pois as vezes o vetor tem vazios e eu quero saber quantos números tem lá dentro, 
# então eu não posso confiar que não há NAs inclusos no comprimento do vetor (acho 
# que a função beta não briga com NA, logo não seria absurdo ele estar ali).


setwd("C:\\Users/manoe/Desktop/TCC/Atual/Estatística/Análises")
dir()

ilhas <- read_excel("data_Noronha_Rocas_final.xlsx")
ilhas_transf <- read_excel("data_transformed_Noronha_Rocas_final.xlsx")


## Descobrir a distribuição:
# Essa função mostra em um gráfico onde se econtraria meu dado dentre os tipos 
# de ditribuição.
# O 'skewness' é uma medida de estatística descritiva.
# Testei a distribuição separando por grupo funcional e misturando de ambas
# ilhas, portanto, tudo que se encontra na coluna do determinado grupo funcional.

# Começando com o grupo funcional dos calcificadores:

descdist(ilhas$calcificadores, # com os dados não transformados.
         discrete = F) # para falar que o valor é contínuo (não é inteiro).

descdist(ilhas_transf$calcificadores, # com os dados transformaos.
         discrete = F)

# Dá pra ver que transformar aproxima meu dado da distribuição normal.
# Mas fazendo o shapiro fica evidente que não é normal:
shapiro.test(ilhas_transf$calcificadores)


## Aplicar a descoberta da distribuição para os demais grupos funcionais:
# Usando 'apply': função para aplicar uma certa função para todas as colunas de 
# uma determinada tabela.
# Detalhe que não quero todas colunas dos grupos funcionais pois já fiz
# para calcificadores, logo, quero de calcificadores para frente.

apply(ilhas[8:13], # número das colunas desejadas.
      MARGIN = 2, # especificando que farei isso por coluna e não por linhas.
      descdist) # específicando a função que vou rodar. 
# No 'descdist' também é possível colocar shapiro para testar a normalidade de cada 
# grupo funcional (trocar a função por 'shapiro.test'). Tambem dá para fazer isso 
# com os histogramas (trocando por 'hist').

# Para saber a distribuição dos dados pelos grupos funcionais, abrir 'ilhas' no
# environment (clicando na bolinha azul) e ver qual a ordem dos grupos funcionais,
# os plots na área de gráficos estarão na mesma ordem dos grupos funcionais que 
# está em ilhas.

# Aparentemente todos se encaixam na distribuição beta. Além disso, alguns se
# aproximam da gamma.

# Sobre a distribuição beta:
# Ela é meio que pensada em probabilidade, então serve pra prever valores entre 
# 0 e 1 (estatística bayesiana). Como meu dado é porcentagem, é facil de 
# transformar entre 0 e 1.
# Contudo, essa distribuição não aceita valores exatamente igual a 0 ou a 1 (é de
# 0 a 1 mas com intervalos abertos). 
# Outro detalhe é que ela é sensível ao N, então não pode ser usada para comparar 
# modelo de amostras diferentes. Tem de ser os mesmos dados ou mesmo número de 
# dados que se está comparando.
# A distribuição beta tem 2 parâmetros, o alfa e o beta (assim como a distribuição
# normal é definida pelos dois parâmetros que são média e desvio). Aí dependendo
# dos valores de alfa ou de beta, ela vai ter uma cara diferente. Quando os 2 são 
# iguais a 0,5 ela tem cara de U; quando o alfa é 5 e o beta é 1, é uma cara mais 
# exponencial (curva azul); quando o alfa é menor que o beta ou o beta menor que 
# o alfa, ela muda o lado da curva. O CDF é a densidade da probabilidade, então 
# se eu andar um pouquinho, o quanto de probabilidade eu já expliquei (é 
# acumulativo). Link: https://pt.wikipedia.org/wiki/Distribui%C3%A7%C3%A3o_beta
# Pesquisando vi que a distribuição beta é uma otima opção mesmo para esse tipo 
# de dado de porcentagem.

#_______________________________________________________________________________

### Testando a ditribuição Beta (e Gamma)

## Converter minha tabela de porcentagem para decimal:
# O que não é exatamente transformar os dados, é voltar eles a decimal pois eles
# estão em porcentagem (entre 0 e 100 para 0 e 1).
ilhas_dec <- ilhas %>%
  mutate(calcificadores = calcificadores/100,
         macroalgas = macroalgas/100,
         MAE = MAE/100,
         cianobacterias = cianobacterias/100,
         suspensivoros.filtradores = suspensivoros.filtradores/100,
         zoantideo = zoantideo/100)


## Transformação do dado para não ter valores 0 ou 1:
# Utilizando a função criada anteriormente para isso (ela está acima do library).
# (Essa transformação é em cima dos decimais já).
# Aplicando a função e usando apply. 
# O valor que seria 0 fica muito próximo de zero em casas decimais (mas sem ser
# zero) e o 1 fica bem próximo de 1 (mas sem ser 1). Há uma alteração também nos
# demais valores, que ficam um pouquinho diferente. Porém, o tanto que essa função 
# muda meus dados é sensível ao número de elementos, então quanto maior o número 
# de elementos, menos meus dados são alterados. 

ilhas_beta <- apply(ilhas_dec[8:13],
                    MARGIN = 2,
                    y.transf.betareg) %>% 
  data.frame

ilhas_beta <- cbind(ilhas[1:7], # colocando as colunas descritivas na tabela transformada.
                    ilhas_beta)


## Testando como meus dados se encaixam dentro dessas distribuições:
# O mesmo pacote que tem o gráfico para descobrir a distribuição também tem uma 
# função pra testar como o meu modelo se encaixa dentro de alguma dessas distribuições.

# Fazendo com modelo beta
modelo_beta <- fitdist(data = ilhas_beta$calcificadores, # um objeto com esse modelo.
                       distr = "beta",
                       method = "mle") # maximum likelihood estimation (método 
# que é "golden standard" pra esse tipo
# de distribuição).

plot(modelo_beta)
# Gráficos falando como foi esse modelo (Q-Q plot indica que essa distribuição
# parece ser boa).
# O Q-Q plot tem o empírico, que é o que observei, e o teorico, que é o ideal 
# do modelo. O desejo é que o empírico caia sobre o teorico, ou pelo menos que o 
# empirico seja bem parecido com o teorico.

# Fazendo com modelo gamma
modelo_gamma <- fitdist(data = ilhas_beta$calcificadores,
                        distr = "gamma",
                        method = "mle")

plot(modelo_gamma)
# Deu estranho o Q-Q plot pois ele previu o teórico dando 1.5, e nunca pode ser
# maior do que 1 a porcentagem.


## Confirmando a escolha do modelo:
# Para confirmar qual distribuição de dado meu modelo melhor se encaixa, olhar o
# aic.
# O aic é um indice/uma medida para comparar. Seu valor absoluto não importa, 
# geralmente o que interessa é quão pequeno ele é. Quanto menor, melhor. Isso porque
# o valor baixo indicaria o modelo com menos complexidade que mais explica o meu dado.
modelo_beta$aic
modelo_gamma$aic
# O beta dá menor que o gamma. 



######### GLM (Generalized Linear Models) #########

# Referência online
# https://stats.stackexchange.com/questions/169391/beta-distribution-glm-with-categorical-independents-and-proportional-response
# https://stats.stackexchange.com/q/167159/246889 

?glm # (generalized linear models); family é o segundo argumento.
# No GLM que vi a pessoa usou uma função própria para GLMs de função beta.


#install.packages("betareg")
#install.packages("multcomp")
library(betareg) # é uma modificação do GLM; é a função betareg, mas é um GLM "disfarçado".
library(readxl)
library(dplyr)
library(car)
library(multcomp)


## Mudar as colunas ilha e ano para fator (com o R base):
ilhas_beta$ano <- factor(ilhas_beta$ano) # (tabela$coluna); transformando ano em 
# fator (não precisa separar aqui pois pra
# Rocas e Noronha será usado o mesmo).

ilhas_beta_noronha <- ilhas_beta # cópia do data frame inteiro para separar a entrada 
# pra Noronha e Rocas por conta do baseline na hora de fazer o GLM.
ilhas_beta_noronha$ilha <- factor(ilhas_beta_noronha$ilha) # transformando ilha em fator.

ilhas_beta_rocas <- ilhas_beta
ilhas_beta_rocas$ilha <- factor(ilhas_beta_rocas$ilha,
                                levels = c("rocas", "noronha")) # mudando o baseline de noronha 2013 p/ rocas 2013.

# ilhas_beta sem especificar a ilha para o baseline não será usado na análise
# final (pois dupliquei e dividi entre as ilhas), portanto pode apagar se quiser:
# rm(ilhas_beta)
# Mas eu ainda uso na parte que estava entendendo e testando o GLM.


### Agora sim o GLM propriamente dito (usando a função betareg):

# Sobre o GLM: 
# Quando cria fator, por padrão, ele organiza por ordem alfabetica. E o primeiro
# fator é o "baseline". então tudo vai ser visto e comparado contra noronha e 2013.
# Para comparar com Rocas vai ter que inverter o baseline.

## 1o. Modelo em relação a calcificadores de Noronha:
glm_Noronha <- betareg(formula = calcificadores ~ ano + ilha,
                       data = ilhas_beta)
summary(glm_Noronha)
# Lembrando que o baseline é Noronha 2013.


## 2o. Modelo em relação a Noronha com interação entre os dois fatores (ano e ilha):
glm_Noronha_interac <- betareg(formula = calcificadores ~ ano + ilha + ano:ilha,
                               data = ilhas_beta)
summary(glm_Noronha_interac)
# Noronha 2013 ainda é o baseline (sempre será a não ser que mude).
# 'Intercept' = em alguns casos faz sentido interpretar o intercept e em outros 
# casos não. No meu caso não faz muito sentido. Um caso que faria sentido seria, 
# por exemplo, tempo de estudo e nota da prova "se o fulaninho estudar zero horas, 
# quanto ele tira na prova?". Ou seja, o intercept é o zero (o lugar que o modelo 
# toca o zero do eixo y). O intercept indica onde começa o modelo.
# 'ilharocas' = comparação de Noronha 2013 com Rocas 2013.


## 3o. Modelo em relação a Rocas com interação (mudando o baseline pra ao invés de 
# 'Noronha2013' ser 'Rocas2013'):
ilhas_beta$ilha <- factor(ilhas_beta$ilha,
                          levels = c("rocas", "noronha")) # colocar os fatores na ordem que eu quero pra mudar o baseline.
ilhas_beta$ano <- factor(ilhas_beta$ano) # ano deixar normal pois 2013 é meu baseline.

glm_Rocas_interac <- betareg(formula = calcificadores ~ ano + ilha + ano:ilha,
                             data = ilhas_beta)
summary(glm_Rocas_interac)


## 4o. Modelo testando mudar a ordem dos anos no baseline (mudei para Rocas 2014):
ilhas_beta$ilha <- factor(ilhas_beta$ilha,
                          levels = c("rocas", "noronha"))
ilhas_beta$ano <- factor(ilhas_beta$ano,
                         levels = c(2014, 2013, 2015, 2016, 2017, 2018, 2019)) # trocando a ordem dos anos pra o baseline ser 2014.

glm_Rocas2014_interac <- betareg(formula = calcificadores ~ ano + ilha + ano:ilha,
                                 data = ilhas_beta)
summary(glm_Rocas2014_interac)


# Usando a função do multcomp para fazer as comparações específicar entre os
# anos e as ilhas:

## 5o. Fazendo padronizado em relação a Noronha (para calcificadores):
summary(glht(model = glm_Noronha_interac,
             linfct = c("ano2014 = 0", # de 2014 p/ 2013
                        "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                        "ano2016 - ano2015 = 0",
                        "ano2017 - ano2016 = 0",
                        "ano2018 - ano2017 = 0",
                        "ano2019 - ano2018 = 0")))

# O 'glht' é mto poderoso pois cabe um vetor (várias strings).
# Sempre usar o summary que possua o mesmo baseline como referência para copiar
# e colar os nomes entre as aspas, pois lá ele já deu como está registrado no GLM.

# O primeiro de cada linha tem q ser o último da próxima linha. 
# Colocar o futuro menos o passado pra saber se diminuiu ou aumentou (no "Estimated"
# sinal positivo quer dizer que aumentou aquele grupo funcional, se for negativo, 
# diminuiu). Apesar de saber como interpretar os sinais positivos e negativos, o 
# valor em si do estimated não sei exatamente como interpretar. Exemplo dos 
# imóveis: quero prever o preço dos imoveis, se eu aumentar 1 metro do quarto, 
# quanto do valor do imóvel aumenta? Esse seria o "estimated", o valor
# do imóvel. Mas sei que somar uma unidade não faz sentido pois é uma regressao beta.

# Entendendo melhor os sinais:
# Por exemplo, 2013 = 5 e 2014 = 3
# 2014 - 2013 = 3 - 5 = -2 (a noção de que diminuiu)
# 2013 - 2014 = 5 - 3 = +2 (a noção erronea que aumentou, sendo que na verdade
# diminuiu)


## 6o. Modelo em relação a Rocas (para calcificadores):

glm_Rocas_interac <- betareg(formula = calcificadores ~ ano + ilha + ano:ilha,
                             data = ilhas_beta_rocas)
summary(glm_Rocas_interac)

summary(glht(model = glm_Rocas_interac,
             linfct = c("ano2014 = 0", # de 2014 p/ 2013
                        "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                        "ano2016 - ano2015 = 0",
                        "ano2017 - ano2016 = 0",
                        "ano2018 - ano2017 = 0",
                        "ano2019 - ano2018 = 0")))

## 7o. Em relação a Rocas (para MAE):
glm_Rocas_interac <- betareg(formula = MAE ~ ano + ilha + ano:ilha,
                             data = ilhas_beta_rocas)
summary(glm_Rocas_interac)

summary(glht(model = glm_Rocas_interac,
             linfct = c("ano2014 = 0", # de 2014 p/ 2013
                        "ano2015 - ano2014 = 0", # de 2015 p/ 2014
                        "ano2016 - ano2015 = 0",
                        "ano2017 - ano2016 = 0",
                        "ano2018 - ano2017 = 0",
                        "ano2019 - ano2018 = 0")))


## 8o. Misturando as duas ilhas:

summary(glht(model = glm_Rocas_interac,
             linfct = c("ilhanoronha = 0",
                        "ano2014 - ano2014:ilhanoronha = 0", 
                        "ano2015 - ano2015:ilhanoronha = 0", 
                        "ano2016 - ano2016:ilhanoronha = 0",
                        "ano2017 - ano2017:ilhanoronha = 0",
                        "ano2018 - ano2018:ilhanoronha = 0",
                        "ano2019 - ano2019:ilhanoronha = 0")))
# Entendendo o sinal (se diminuiu ou aumentou aquele grupo funcional): quem manda 
# no sinal é o da esquerda (aqui, no caso, é Rocas).