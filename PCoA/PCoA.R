############# PCoA (Principal Coordinate Analysis) #############

#### Sobre o cálculo da PCoA.
### Com dados não transformados e com dados transformados arcoseno da raíz quadrada.
### Achatado por sítio + ano e chamando o ponto pela ilha: cada ponto representa a
# PCoA da mediana por ano de cada grupo funcional de todos transectos amostrados 
# para determinado sítio (todos transectos de um sítio, não separei por transecto).
### Cada ponto, identificador por ilha (formato do ponto) e ano (cor do ponto)
# representa uma coordenada do eixo 1 e 2 criado pela PCoA.
### Duas opções de gráficos finais: polígonos criados unindo os pontos por ano
# resultando em 7 polígonos sobrepostos (os que correspondem ao mesmo ano, tanto 
# de Noronha quanto de Rocas, são ligados) ou polígonos criados unindo os pontos 
# por ilha resultando em 2 polígonos sobrepostos (os pontos correspondentes a mesma
# ilha são ligados).

## Problema de todas linhas dos grupos funcionais com zero:
# Inicialmente pensei em remover as linhas que possuiam zero em todos grupos 
# funcionais. Contudo, achatando o dado (seja ele transformado ou não) da forma 
# que fiz "group_by(ilha, ano, sítio) + medianas dos grupos funcionais" os valores 
# zeros somem, eles ficam diluídos.
# Já se o dado for achatado por transecto, fica com linhas todas zero (talvez esse
# seja mais um motivo para eu achatar por sítio, pois assim não jogo dados fora
# já que tudo zerao também é dado).
# Mas lembrar que, caso eu removesse as linhas, haveria de descrever em material
# e métodos que eu removi as linhas que correspondiam a zero em todos grupos 
# funcionais (isso porque o NMDS não aceita e a PCoA aceita porém não gosta, pois
# "distorce" a análise).

## Dicas de atalho de teclado que eu descobri:
# Apertar ctrl e apertar nas setinhas vai percorrendo o texto através das palavras inteiras.
# Apertar ctrl + shift e apertar as setinhas vai selecionando por palavra.
# Apertar ctrl + S salva em boa parte dos programas (a não ser word e excel pois
# esse comando é de sublinhar).

## Livro referência (PCA):
# https://www.statlearning.com/
# James G. et al, 2013 - An Introduction to Statistical Learning with Applications in R
# O livro está disponível para baixar e tem um curso online do livro, com vídeos
# falando sobre os tópicos (como da PCA).

## Sobre os eixos da PCoA 
# A PCoA cria um eixo para explicar meus valores/meus dados, ou seja, quanto que 
# cada eixo está explicando da variação dos meus dados e isso vai acumulando.
# O objetivo é achar um eixo que explique boa parte da variância dos meus dados 
# quando vamos pra essa dimensão do gráfico. Queremos que essa dimensão separe os dados.
# O gráfico ao todo explica a % da soma da porcentagem dos eixos. Por exemplo, o da
# Ju é PCoA2=16.15% e PCoA1=21.17%, isso quer dizer que o gráfico ao todo explica
# menos que 40% da variação do dado (ou seja, o gráfico captura menos de 40% do
# que o dado é). O desejo é ter eixos que expliquem bastante a variação desse dado.
# A PCoA dá vários eixos como resultado da análise. Mas queremos só os dois primeiros
# eixos, que são os que mais explicam e, logo, são os mais importantes. Até porque
# não é humanamente vizualizável um gráfico com muitos eixos (tipo 3 já complicaria). 
# O eixo x é sempre o maior e é o melhor separador do dado, ou seja, o que variar 
# horizontalmente vai separ melhor meu dado do que o que variar pra cima/baixo.

## Acredito que a diferença entre o PCA e o PCoA é só o jeito que são calculadas 
# as distâncias, mas o mecânismo é o mesmo.

## Ordem da montagem do PCoA: é feito em camadas
# 1o. plotar os eixos do gráfico
# 2o. plotar os polígonos
# 3o. plotar os pontos
# 4o. plotar os vetores dos grupos funcionais
# 5o. plotar as siglas correspondentem aos grupos funcionais que identificam os vetores.
# OBS.: como é em camadas, se plotasse os pontos antes dos polígonos, os
# polígonos sobrepoem os pontos (mesmo que seja os pontos que "liga" os polígonos).
# OBS.: sempre que adicionar algo no gráfico zerar e rodar desde o começo pois
# se não vai sobrepondo no gráfico antigo e fica uma meleca.


library(readxl)
library(dplyr)
library(vegan)
library(ape)

setwd("C:\\Users/manoe/Desktop/TCC/Atual/Estatística/Analises_TCC")
dir()

## Comando que limpa tudo do environment
rm(list = ls())

## Pegar o dado (transformado ou não)
ilhas <- read_excel("data_transformed_Noronha_Rocas_final.xlsx")
# ilhas <- read_excel("data_Noronha_Rocas_final.xlsx")


## Primeiro passo
# Achatar/agrupar meu dado da forma que eu achar melhor com 'group_by'. 
# Fazer as medianas com o 'summarise': colocar o nome da coluna que eu quero 
# fazer a mediana e depois sobreescrever na tabela original.
# Colocar siglas como identificação de cada coluna de grupo funcional com objetivo 
# de na última tarefa de plotagem do gráfico, que é o texto de identificação dos 
# vetores, ficar bonito (o que for colocado aqui aparecerá exatamente da mesma
# forma no gráfico final).
med_ilhas_sitio <- ilhas %>%
  group_by(ilha, ano, sitio) %>%
  summarise(CAL = median(calcificadores),
            MAC = median(macroalgas),
            MAE = median(MAE),
            CIANO = median(cianobacterias),
            SF = median(suspensivoros.filtradores),
            ZOA = median(zoantideo)) %>%
  data.frame()

class(med_ilhas_sitio)
# O formato tbl é "queridinho" do dplyr, então não tem problema não transformar inicialmente
# 'ilha' pra data frame e só transformar depois que fizer as medianas. Essa transformação 
# de medianas é "tbl friendly". 
# E detalhe que, quando se coloca um 'group_by', a parada vira tbl automaticamente. 


## Colocar na legenda do gráfico o nome das ilhas com letra maiúscula.
# Fazendo essa alteração na tabela "med_ilhas_sitio".
# O que for "noronha" na tabela se torna "Noronha", e o que não for "noronha" se
# torna "Rocas".
med_ilhas_sitio$ilha <- ifelse(med_ilhas_sitio$ilha == "noronha",
                               yes = "Noronha",
                               no = "Rocas")


## Pegar só os grupos funcionais da minha tabela para fazer as distâncias entre
# os pontos pra plotar no gráfico.
ilhas_community_sitio <- med_ilhas_sitio %>%
  select(CAL, MAC, MAE, CIANO, SF, ZOA)


## Transformar 'ano' e 'ilha' em fatores.
# Criando as variáveis fator 1 e 2, que correspondem à ano e ilha.
# Fazer separado os fatores, que é uma coluna com os anos de cada amostra
# e outro fator com as ilhas (fator 1 e fator 2).
fator1 <- as.factor(med_ilhas_sitio$ano)
length(fator1) # 47 observações dentro do vetor, então está certo (igual minha tabela).
f_ano <- data.frame(fator1)
# Cria-se um data frame em que o fator 1 é o nome e o dado são as linhas.

fator2 <- as.factor(med_ilhas_sitio$ilha)
length(fator2) # 47 também.
f_ilha <- data.frame(fator2)

#_______________________________________________________________________________
## Saber quantos de cada fator possuo e atribuir a forma do ponto (que vai aparecer 
# no gráfico) para elas.
# Com objetivo de evitar hardcoding: criar um vetor que possua o número 21 - que
# equivale a um formato específico de ponto - o tanto de vezes que aparece 'noronha' 
# no meu dataset. E fazer respectivamente o mesmo para Rocas.

# Escolher o tipo de ponto
?pch # 21 é bolinha pintável e 22 é o quadradinho pintável.

# Função 'table' para contagem do que eu desejo
# Função 'rep' para repetir 

# Para Noronha:
contagem_ilhas <- table(fator2)
contagem_ilhas[1] # pegando o primeiro elemento, que é 20 de noronha.
rep_noronha <- rep(x = 21, # '21' quer dizer ponto com aspecto "bolinha pintável".
                   times = contagem_ilhas[1]) # 'times' é quantas vezes eu desejo
# repetir o ponto com aspecto '21'.
length(rep_noronha) # para confirmar o 20x formato '21' do ponto para Noronha.

# Para Rocas:
contagem_ilhas[2] # pegando o segundo elemento que é 27 de rocas.
rep_rocas <- rep(x = 22, # '22' quer dizer ponto com aspecto "quadradinho pintável".
                 times = contagem_ilhas[2])
length(rep_rocas)

# Concatenando Noronha e Rocas para chamar só um vetor com ambas informações quando 
# for plotar o gráfico da PCoA. 
shapes <- c(rep_noronha, rep_rocas) # 'shapes' que vai ter o número de observações
# de Noronha correspondendo a um formato e o número de Rocas correspondendo a outro formato.
length(shapes)
# OBS: A cor dos pontos só coloca no momento em que for plotar o gráfico da PCoA.
#_______________________________________________________________________________


## Fazendo o cálculo da PCoA:
# Fazer a distância com o pacote vegan 'vegdist'.
# Matriz de distância entre os pontos (ela é a entrada da PCoA):
dist_ilhas_community_sitio <- vegdist(x = ilhas_community_sitio,
                                      method = "bray")

ilhas_PCoA <- pcoa(D = dist_ilhas_community_sitio, # 'D' é a matriz de distância.
                   correction = "none")  


## Porcentagem de quanto meus 2 primeiros eixos explicam a variação dos meus dados:
ilhas_PCoA$values$Relative_eig[1:2]
sum(ilhas_PCoA$values$Relative_eig) # aqui tem que somar 1 (pois é a soma total,
# ou seja, de todos os eixos criados na PCoA... não é só dos meus dois eixos que
# usarei para plotar o gráfico).
sum(ilhas_PCoA$values$Relative_eig[1:2]) # aqui é a soma da explicação do dado
# proporcionada só pelos meus 2 primeiros eixos; a soma deu 0.79 para a tabela de
# entrada com dados transformados e 0.81 para a tabela com dados não transformados, 
# ou seja, estou explicando bastante pois essa é a soma dos meus eixos 1 e 2 (logo, 
# não precisa ser 1 o total da soma, mas quanto mais próximo de 1 melhor é).

# Colocando em uma variável o quanto os eixos x e y explicam meu dado:
# Isso para colocar no plot como identificação dos eixos.
expl_x <- ilhas_PCoA$values$Relative_eig[1]
expl_y <- ilhas_PCoA$values$Relative_eig[2]
# Aí colocar no gráfico na hora que for fazer a chamada do plot.


## Determinando a posição dos meus grupos funcionais nesse espaço criado pela minha PCoA:
# (Criando as setas dos vetores/linhas com os vetores).
# Calcular as posições que os grupos funcionais caíram na PCoA.

# As setas são desenhadas com base em "score" que é uma função chamada 'wascores'.
# A entrada no 'x' da função pode ser 'environmental variables' ou 'ordination
# scores'. No meu caso é o 'ordination scores' da PCoA (coordenadas da PCoA, que 
# correspondem ao eixo 1 e 2 dela). E 'environmental variables' seria usado para, 
# por exemplo, medidas de sal/oxigênio na água, etc. 
# A entrada de 'w', que é weight (= peso), é a tabela só dos organismos (ilhas_community_sitio)
# que transformou e tirou os dados e me devolve a coordenada de cada grupo funcional
# na PCoA. Assim, pra cada coluna (grupo funcional) da minha matriz da comunidade, será 
# posionado um vetor na PCoA (portanto, 6 vetores).

#wascores #f1: para entender melhor o que essa função faz.
ilhas_wascores <- wascores(x = ilhas_PCoA$vectors[,1:2], # o eixo 1 e 2 do resultado da minha PCoA.
                           w = ilhas_community_sitio) # a matriz da comunidade.


## Definindo no gráfico a posição das siglas indicadoras dos vetores:
# Criando um data frame, fazer minha própria tabela na mão para cada sigla.
# 'x' é o que cada sigla irá andar no eixo x (horizontal).
# 'y' é o que cada sigla irá andar no eixo y (vertical).
passitos_letritas <- 0.012 #variável de quanto tem que andar a sigla no gráfico 
# (pois aí se tiver que trocar, troca só a variável).
wascores_text_position <- data.frame(x = c(ilhas_wascores["CAL", "Axis.1"] - passitos_letritas,
                                           ilhas_wascores["MAC", "Axis.1"] - passitos_letritas,
                                           ilhas_wascores["MAE", "Axis.1"] + passitos_letritas,
                                           ilhas_wascores["CIANO", "Axis.1"], # tem que ter a posição
                                           # mesmo que eu não queira alterar.
                                           ilhas_wascores["SF", "Axis.1"] - passitos_letritas,
                                           ilhas_wascores["ZOA", "Axis.1"] + passitos_letritas),
                                     y = c(ilhas_wascores["CAL", "Axis.2"] + passitos_letritas,
                                           ilhas_wascores["MAC", "Axis.2"],
                                           ilhas_wascores["MAE", "Axis.2"],
                                           ilhas_wascores["CIANO", "Axis.2"] + passitos_letritas,
                                           ilhas_wascores["SF", "Axis.2"],
                                           ilhas_wascores["ZOA", "Axis.2"]))
# Pronto!


##### PLOTANDO O GRÁFICO:

## Criar um vetor para as cores dos polígonos
cores <- rainbow(nlevels(f_ano$fator1)) # colocar entre parenteses o número de
# fatores que eu possuo.

## Gráfico propriamente dito:
plot(ilhas_PCoA$vectors[,1:2], # pegando quais eixos calculados na PCoA eu quero,
     # no caso, só os 2 que explique mais meu dado, logo,
     # coluna 1 e 2 e todas suas linhas.
     las = 1, # parâmetro que deixa o eixo x e y horizontais.
     type = "n", # parâmetro que tira os pontos para os poligonos serem plotados antes.
     xlab = paste0("PCoA1 = ", # legendas dos eixos 1 e 2.
                   round(expl_x,
                         digits = 4) * 100,  # multiplicando por 100 pra ficar em porcentagem.
                   "%"),
     ylab = paste0("PCoA2 = ",
                   round(expl_y,
                         digits = 4) * 100,
                   "%"),
     main = "")
# O 'paste0' é uma função melhor que 'paste', pois, no primeiro, o separador da
# legenda já é vazio. No 'paste' precisa especificar que o separador é vazio 
# colocando "".
# O 'round' é uma função para arredondar o valor, já que não quero colocar o 
# número original que está com muitas casas decimais. Aí seleciono quantos dígitos
# desejo de casas decimais.

## Fazer as formas/polígonos ligando as amostras (com a função 'ordihull' e 
# usando o 'for')

# Como funciona o 'for': ele é um laço (em inglê "loop"); um jeito de automatizar coisas.

# A utilização do 'for' é com objetivo de fazer e sobrepor cada polígono 
# separadamente no gráfico.

# Quando se quer percorrer alguma coisa usa o 'i' como índice/index (no inglês).

# O 'i' recebe o valor da primeira "coisa" do vetor.
# Seu formato sempre será 'for(variável in vetor)'.
# O 'i' primeiro vai receber o valor de 1 e aí vai dazer o que estiver dentro
# do laço, que no caso é "imprimir 'i'". Aí ele vai imprimir esse primeiro 'i' e
# aí, no segundo passo, ele pergunta "tem mais coisa dentro no meu vetor? Se sim,
# qual essa coisa?". A segunda coisa é o número 2, então meu 'i' agora vale 2. Aí
# faz tudo que tem dentro do laço e volta pra pergunta "Tem mais coisa? O que é?".
# A terceira coisa é o número 3, então 'i' recebe 3 e faz tudo que tem dentro do 
# laço e volta. Isso até acabar, que no meu caso vai chegar no último e será o 7
# (pois são 7 anos). Nesse momento "Tem mais alguma coisa?", a resposta sendo "não."
# saí do laço e se encerra. Ou seja, quando você já percorreu todas as coisas do 
# vetor, ele sai do laço e termina.

# Em qualquer uma das linhas em que tem o curly brackets pode dar o ctrl + enter
# que ele roda o looping inteiro. Se der ctrl + enter só em alguma linha de dentro
# do 'for', ele vai rodar só aquela linha de dentro.

# Aqui eu optei por criar os polígonos sendo formados em duas opções, unindo os
# pontos por ano (os que correspondem ao mesmo ano) e unindo os pontos por ilha 
# (os pontos que correspondem a mesma ilha, logo, todos sítios da determinada ilha
# se encontram no polígono). 
# Basta ligar e desligar qual conjunto de polígos eu desejo plotar comentando e 
# descomentando o script.

# Fazendo os polígonos por ano (um pra cada ano, portanto, 7 polígonos):
for(i in 1:nlevels(f_ano$fator1)){
  ordihull(ord = ilhas_PCoA$vectors[,1:2], # os vetores da PCoA (os dois eixos/
           # colunas e todas as linhas).
           groups = f_ano$fator1, # um dos dois fatores (aqui no caso 'anos').
           draw = "polygon", # para desenhar os polígonos ligando os pontos.
           border = cores, # a cor da borda do polígono; fica preta se não específicar aqui.
           show.groups = levels(f_ano$fator1)[i], # 'i' para cada ano fazer um polígono.
           col = cores) # a cor do preenchimento do polígono.
}

# Fazendo os polígonos por ilha (um pra cada ilha, portanto, 2 polígonos):
# for(i in 1:nlevels(f_ilha$fator2)){
#   ordihull(ord = ilhas_PCoA$vectors[,1:2],
#            groups = f_ilha$fator2, # um dos dois fatores (aqui no caso 'ilha')
#            draw = "polygon",
#            border = c("gray", "darkgreen"),
#            show.groups = levels(f_ilha$fator2)[i],
#            col = c("gray", "darkgreen"))
# }

## Colocar os pontos no gráfico (sobre os polígonos):
points(x = ilhas_PCoA$vectors[,1:2], # a matriz de dados é a mesma; o que ele vai
       # plotar vem da mesma coisa.
       pch = shapes, # já preparado anteriormente quando transformei ano e ilha em fator.
       bg = cores[c(f_ano$fator1)])

## Fazendo as legendas:
legend("topright", # local que a legenda será escrita no gráfico.
       legend = levels(f_ano$fator1), # a função e o argumento tem o mesmo nome.
       text.col = cores, # cada escrito de ano ficar da cor do respectivo polígono.
       cex = 0.9, # tamanho da legenda
       bty = "n") # tirar a caixa de enquadramento da legenda.

legend("topleft",
       legend = levels(f_ilha$fator2),
       text.col = "black",
       cex = 1,
       pch = unique(shapes), # tomar cuidado se os pchs vem na ordem dos locais mesmo.
       bty = "n")

## Colocando a linha horizontal e vertical cruzando o zero dos eixos:
abline(h = 0, # hotizontal.
       lty = 3) # line type.
abline(v = 0, # vertical.
       lty = 3)

## Colocando as setas dos vetores:
# (o cálculo foi feito préviamente, mas só se usa o já calculado previamente
# aqui no final na hora de plotar o gráfico pois esse gráfico é feito em camadas 
# e as últimas duas coisas que eu quero que vá são os vetores e seus respectivos
# nomes).
arrows(x0 = 0, # a origem é o zero; todas começam no zero e de lá faz a seta.
       y0 = 0,
       x1 = ilhas_wascores[,1], # primeira coluna do 'ilhas_wascores' e todas linhas.
       y1 = ilhas_wascores[,2], # segunda coluna do 'ilhas_wascores' e todas linhas.
       length = 0.05, # a espessura da seta.
       col = "black",
       lty = 1)

## Colocando nome nas setas dos vetores:
text(wascores_text_position, # coordenada de onde ficará o texto no gráfico (que
     # eu yambém fiz previamente antes de plotar o gráfico).
     rownames(ilhas_wascores),
     cex = 0.8,
     col = "black")


#### Analisando os gráficos:
## Com os polígonos formados ligando os pontos por ano de amostragem:
# São lugares de coleta que o ano não importou muito, pois os polígonos de 
# diversas cores que são de anos diferentes estão sobrepostos. Só 2019 que é o
# ano mais discrepante e apresenta diferença visível. Contudo, o ano de 2019 é
# puxado principalmente por Noronha.
# Calcificadores, macroalgas, MAE e zoantídeos caem dentro da maior porção de
# sobreposição dos polígonos. 

## Com os polígonos formados ligando os pontos por ilha:
# Noronha e Rocas ficaram bem separados (exceto calcificadores, macroalgas e MAE
# que caem dentro da sobreposição dos polígonos).
# Dá pra falar de ciano que varia diferente em Noronha. Enquanto que o vetor de 
# zoantídeos puxou mais a ilha de Rocas e o vetor de suspensívoros e filtradores
# puxou mais a ilha de Noronha. Contudo, isso já era esperado e faz sentido pois
# Rocas tem mais zoantídeos e Noronha tem mais suspensívoros e filtradores na composição
# da comunidade (isso já podia ser visto nos box-plots).

## Observei que...
# Na PCoA dá Ju, os eixos 1 e 2 explicaram pouco a variação do dado (21.17 % +
# 16.15% = 37.32% = 0.37) e ela separou por mais próximo de espécie possível.
# Na minha PCoA, os eixos 1 e 2 explicaram bem a variação do dado (soma deu 0.79
# para a tabela de entrada com dados transformados e 0.81 para a tabela com dados
# não transformados). E eu separei por grupos funcionais, ou seja, pegar por grupos 
# funcionais "separa bem o role".


#### Legendas para o gráfico:

# 1.
# Para facilitar a vizualização, utilizamos as medianas dos grupos funcionais por 
# sítio. Cada ponto, identificado por ilha (formato do ponto) e ano (cor do ponto),
# reprensenta uma coordenada do eixo 1 e 2 criado pela PCoA.

# 2.
# Gráfico da PCoA criada com base na mediana dos grupos funcionais divididos por 
# sítios. Cada ponto, identificado por ilha (formato do ponto) e ano (cor do ponto),
# reprensenta o coordenada do eixo 1 e 2 criado pela PCoA.

# 3.
# Cálculo da PCoA com as medianas de cada sítio. Cada ponto representa ilha no 
# formato e o ano na cor.

# 4.
# Se utilizar o gráfico com os polígonos sendo formados ligando por ilha, descrever
# na legenda do gráfico que o polígono de tal cor é tal ilha e o polígono de tal
# cor é a outra ilha. Ex: O polígono verde é o agrupamento de todos os anos de Rocas 
# e o polígono cinza é Noronha.
# Pra facilitar o leitor de ver todos os dados referentes a Noronha/Rocas.