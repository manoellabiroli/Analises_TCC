############# NMDS (todos dados, sem fazer medianas) #############

## Sobre:

# O MDS é realmente muito parecido com um PCoA.
# Mas no PCoA, é possível ver que variáveis da sua tabela original contribuiram
# para cada eixo, não sei se o NMDS possibilita fazer isso. 

# Estamos, basicamente, tentando diminuir a dimensionalidade do dado. Há jeitos
# matemáticos diferentes de fazer isso. Não sei a exatamente a diferença deles.

# Diferença entre MDS e NMDS:
# Todos MDS que vi não são de ecologia. Existe o tal de NMDS, que é muito bom,
# pois ele é feito do jeito de ranking, como se fosse um "teste não paramétrico"
# do MDS (acho que o MDS leva mais em conta o valor e não o ranking). Em mais de
# um lugar achei o NMDS como exemplo de uso para a ecologia.
# Do MDS vi exemplos sobre distância entre coisas, como cidades. Fazem um modelo
# que avaliam distância entre cidades, reproduzindo um mapa a partir do 
# achatamento dessas distâncias, onde cada linha é uma cidade. No meu caso, cada
# linha é uma coleta de um único lugar. Então, não entendi como transpor pra MDS
# os meus dados.

# Diferença entre ranking e valor:
# O ranking não é exatamente um valor, pois não estou olhando se um é 1 e outro
# é 100, estou olhando se o 1 é antes do 100. 
# Exemplo: tenho 3 valores, o 1, o 5 e o 1000. O 1 é o menor, o 5 é o próximo e 
# 1000 é o último. Entre o 5 e o 1000 há 995 de distância, mas se fizer por 
# ranking, a diferença entre esses dois número é 1. O MDS considera o 995, e o 
# NMDS considera inteiros de diferença.


# install.packages("vegan")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("ape")
# install.packages("graphics")
library(vegan)
library(ggplot2)
library(dplyr)
library(readxl)
library(graphics)
library(ape)

setwd("C:\\Users/manoe/Desktop/TCC/atual/Estatística/Analises_TCC/")
dir()

ilhas <- read_excel(path = "data_Noronha_Rocas_final.xlsx",
                    sheet = 1,
                    range = "A1:M1879")

ilhas <- data.frame(ilhas)
class(ilhas)

### Tenho que dar para o NMDS a tabela só com os dados das comunidades, ou seja, 
# só as colunas dos organismos (grupos funcionais).
# 'ilhas [linha,coluna]'
names(ilhas) # Pra ver os nomes das colunas da planilha e preencher ali na função
ilhas[, c("calcificadores", "macroalgas", "MAE", "cianobacterias",
            "suspensivoros.filtradores", "zoantideo")]
# OU
# sem usar os nomes, escolhendo só o número das colunas que quero:
# ilhas[,8:13]

# Jogar isso pra algum lugar, pois rodando a função ali de cima aparece só no 
# console. Então criar um novo objeto com as variáveis númericas para colocar no
# NMDS.
ilhas_community = ilhas[, c("calcificadores", "macroalgas", "MAE",
                                "cianobacterias", "suspensivoros.filtradores",
                                "zoantideo")]
# OU
# ilhas_community <- ilhas[,8:13]


### 'NA' e zero não são permitidos na tabela para fazer o MDS, portanto, é preciso remover
# de 'ilhas_community' tudo o que está com as zero ou 'NA', que são linhas 
# com todos os valores de grupos funcionais igual a zero. Se tentar fazer o NMDS
# com esses valores ali, dá um erro.
ilhas_community <-  ilhas_community %>% 
  filter_all(any_vars(. != 0))
# Atribuir a saida disso para ele mesmo.
# '!=' é igual 'diferente' no R.

linhas_sem_zero_all <- as.numeric(row.names(ilhas_community))

ilhas <- ilhas[linhas_sem_zero_all,]
# Agora 'ilhas' e 'ilhas_community' tem o mesmo número de linhas.


### Rodar o NMDS, que vai achatar meus dados, criando um objeto/variável no 
# environment pra salvar:
ilhas_community_NMDS <- metaMDS(comm = ilhas_community,
                                k = 2,
                                distance = "bray",
                                trace = F,
                                autotransform = F)
# Usar o control espaço para ver o cardário de parâmetros:

# 'comm =' posso usar a dissimilaridade do dado filtrado ou o data.frame
# com o meu dado da minha comunidade. 

# 'k =' número de vetores que explica minha amostra; quero dois, um gráfico 2D, 
# já que humanos entendem melhor nessa dimensão. 

# 'try =' algorítimo ("receita de alguma coisa") iterativo; é o número de vezes
# que ele vai rodar o NMDS; quantas vezes ele vai tentar "supor coisas"/"fazer
# esse gráfico pra mim"; falar o número de iterações, pois quanto mais, mais ele
# demora; não vou colocar ali descrevendo entre os parenteses pois usarei o
# deafault do R, logo, não precisa específicar; usaremos o padrão já proposto 
# pelo R; número de interações pra cair no "melhor ótimo".

# Pintar a função/argumento e clicar 'f1' o R mostra o 'help'; dentro da ajuda,
# o 'usage' mostra os parâmetros padrão que são usados automaticamente pelo R e,
# algumas vezes, uma lista de opções do que pode ser usado; os argumentos não
# específicados será usado automaticamente o padrão.

# 'distance =' é o que ele vai usar pra calcular a distância entre os pontos;
# quero supor quão parecidos ou diferentes os dados são (geralmente nisso as
# pessoas utilizam distância euclidiana, é uma distância muito básica, mas pra
# ecologia as pessoas usam outro); usar Bray-Curtis.

# 'trace =' usar 'FALSE' no argumento ajuda o output a ficar mais clean, pois
# cada vez que ele tá fazendo a iteração ele deve cuspir umas groselhas no
# console e é só pro R não fazer isso e ficar mais limpo. 

# 'autotransform =' argumento para fazer uma transformação no dado; o meu dado
# da comunidade está em porcentagem, então não precisa transormar, logo usamos o
# 'FALSE'.

# Jeito feio de ver os meus pontos já distribuídos nos meus dois eixos que foram
# criados:
plot(ilhas_community_NMDS$points)


### Criar um objeto/variável com o valor achatado produzido pelo NMDS:
# Criar um novo data.frame (uma nova tabela) mais completinho, que vai ter esses
# novos pontos criados no NMDS e os labels originais. Combinando então os rótulos
# de ilhas com a saída do NMDS.
MDS_ilhas_xy <- data.frame(ilhas_community_NMDS$points)
# 'xy' porque queremos os labels dos pontos.
# A planilha dele aparece só os eixos que ele criou achatando os dados em duas
# dimensões. São as minhas novas duas dimensões de dados.

# Adicionar colunas na tabela que foi gerada com os valores achatados criados
# pelo NMDS, colando do lado do dado que já tenho ali o que quiser:
# Pra ano:
MDS_ilhas_xy$ano <- ilhas$ano
# Pra sítio:
MDS_ilhas_xy$sitio <- ilhas$sitio
# Pro nome de cada ilha (se é Rocas ou Noronha):
MDS_ilhas_xy$ilha <- ilhas$ilha
# Olhar como ficou minha tabela 'MDS_ilhas_xy' e ver se foi adicionado mesmo.


### PLOTANDO OS GRÁFICOS (com ggplot)

# O primeiro eixo é o que mais separa as amostras, o eixo x. É o que é o mais 
# importante em separar esses dados. Observar que a maior parte dos dados eles
# não são iguaizinhos no eixo x. A segunda dimensão, eixo y, é o que vai explicar
# um pouco mais meus dados. Se eu cortar meu gráfico na vertical, estou cortando
# mas informações do que se eu cortar ele na horizontal (o eixo x explica mais).
# Quanto mais eixos, menos importantes eles vão ser. O primeiro é mais 
# importante, o segundo depois, o terceiro seguinte e por aí vai.

# Cada pontinho desses é uma linha da minha tabela.
# O grupo funcional é o dado que eu achatei/reduzi a dimensão em 1 pontinho. 

# Instalar e rodar pacotes (coloquei lá em cima com os demais); ele tem uma coisa
# que cria camadas no meu plot; conceito de camadas quando estou pendindo
# coisas pra ele.

# Como a partir daqui já coloquei data como MDS_noronha_xy não preciso mais
# chamar a coluna com o '$', posso usar só o nome. Se eu falar 'ano' ele vai 
# procurar no MDS_noronha_xy qual que é a coluna 'ano', logo não preciso 
# falar pra ele onde procurar 'ano'. 

# 'aes' é a parte mais estatística, "o que eu quero que vá aonde do meu gráfico";
# coloco os dados do eixo x e y dentro desse argumento, coloco a variável que
# eu quero que pinte as coisas (pode ser sítio ou ano).

# No ggplot não é vírgula depois do parenteses, é '+'; o conceito de somar 
# coisas / criar camadas.

# 'geom_point' é o tipo de gráfico que eu quero (as famosas camadas); no caso é
# gráfico de pontos, se fossem de linhas seria 'geom_line'; dentro desse 
# argumento posso colocar mais um detalhe da nossa amostra que é o 
# o sitio evidenciado pelo formato dos pontos 'aes(shape = sitio)'.

# O padrão do ggplot é fazer o fundo cinza, então uso o argumento 'theme_bw'
# para mudar o fundo; somando um pre-set de temas ao fim da função.

## GRÁFICO 1: Noronha e Rocas
ggplot(data = MDS_ilhas_xy,
       aes(x = MDS1, y = MDS2, color = ilha)) + 
  geom_point() +
  theme_bw()
# Há uma certa sobreposição desses pontos.
# O eixo x é o mais importante pra separar, e é possível observar que as cores
# não estão distribuindas verticalmente separando Rocas e Noronha no eixo x, 
# Rocas e Noronha estão mais separados pelo eixo y.

## GRÁFICO 2: ano de Noronha e Rocas
# Mudando 'color = ilha' pra 'ano' no gráfico
ggplot(data = MDS_ilhas_xy,
       aes(x = MDS1, y = MDS2, color = ano)) + 
  geom_point() +
  theme_bw()
# Os pontos são identicos, mas dá pra ver que 2019 tem mais pra baixo do que pra
# cima.

## GRÁFICO 3: ilhas e anos (anos em azul)
ggplot(data = MDS_ilhas_xy,
       aes(x = MDS1, y = MDS2, color = ano)) + 
  geom_point(aes(shape = ilha), size = 3) +
  theme_bw()

## GRÁFICO 4: ilhas e anos (anos coloridos)
# Salvando a imagem com resolução:
png(filename = "C:\\Users/manoe/Desktop/plots/Finais/NMDS.png", 
    width = 6*300, 
    height = 5*300, 
    res = 300, 
    pointsize = 8)
# Gráfico propriamente dito:
ggplot(data = MDS_ilhas_xy,
       aes(x = MDS1, 
           y = MDS2, 
           color = as.character(ano))) + 
  geom_point(aes(shape = ilha), 
             size = 2) +
  scale_color_brewer(palette = "Set1", 
                     name = "Ano") +
  labs(title = 'Rocas e Noronha') +
  annotate(geom = "text",
           x = -1.5,
           y = 1,
           label = paste0("Stress = ",
                          round(ilhas_community_NMDS$stress,
                                digits = 3)),
           color = "black") +
  theme_bw()
# Finalizando exportação da imagem:
dev.off()


### Parâmetros do NMDS

# Quando se apresenta NMDS como resultado deve-se registrar e descrever alguns
# parâmetros na minha figura:

# 1 - a figura é tal gráfico.

# 2 - descrever qual foi a medida de dissimilaridade utilizada: Bray-Curtis.

# 3 - falar se o dado foi transformado ou não (que não fiz standarlization): o 
# padrão do meu dado é a porcentagem; isso é relativo a fazer alguma
# padronização dos dados, como colocar tudo em log; não foi necessário faze-la
# pois todos meus dados estão em porcentagem, então não tem perigo de um em uma
# dimensão querer dizer algo muito diferente do que outro em outra dimensão;
# por exemplo, para compor o valor de um imóvel, um metro na fachada do prédio
# compõe mais o preço do que um metro dentro do apartamento.

# 4 - falar que eliminei os pontos que eram só areia e outros.invertebrados: 
# falar o que eu considerei (os grupos funcionais).

# 5 - A medida denominada estresse:
# Para olhar o estresse:
ilhas_community_NMDS$stress
# O de ilhas foi 0,1541673, então foi aceitável.
# O estresse é uma medida que descreve quão bem essa redução de dimensões foi 
# boa para o meu dado. Quanto menor seu valor, melhor. O melhor seria que o 
# estresse fosse próximo ou menor que 0,1, mas até 0,2 é aceito. Depois disso
# a interpretação é que o número de dimensões que escolhi não foi bom pra 
# separar meu dado.

# Os eixos definidos para o gráfico vão conter essa medida de dissimilaridde, 
# então a distância entre os pontos é a similaridade deles. Então quanto menos
# a distância mais similar.Temos a tabela montada (MDS_ilhas_xy) mostrando a 
# dissimilaridade por linha. 



############# NMDS com Medianas #############

# É mediana ao invés de média pois a mediana leva menos em consideração os 
# outliers do que a média. A mediana é menos sensível a eles, pois quando 
# fazemos as distribuição vemos que é não normal, logo, o mais correto é fazer 
# a mediana.

# 'mean' = média
# 'median' = mediana

setwd("C:\\Users/manoe/Desktop/TCC/atual/Estatística/Analises_TCC/")
dir()

# Para fazer com os dados transformados ou normais (não transformados) basta 
# trocar o nome da planilha selecionada aqui:
ilhas <- read_excel(path = "data_transformed_Noronha_Rocas_final.xlsx",
                    sheet = 1,
                    range = "A1:M1879")

ilhas <- data.frame(ilhas)


## Mediana por transectos e profundidade.piscina
med_ilhas <- ilhas %>%
  group_by(ilha, ano, sitio, profundidade.piscina, transecto) %>%
  summarise(med_calcificadores = median(calcificadores),
            med_macroalgas = median(macroalgas),
            med_MAE = median(MAE),
            med_ciano = median(cianobacterias),
            med_susp = median(suspensivoros.filtradores),
            med_zoantideos = median(zoantideo)) %>%
  data.frame()

ilhas_community <- med_ilhas %>%
  select(med_calcificadores, med_macroalgas, med_MAE, med_ciano, med_susp,
         med_zoantideos) %>%
  filter_all(any_vars(. != 0)) %>%
  data.frame() # AQUI ESTA DANDO ERRADO. SE EU TIRO ESSE 'DATA.FRAME' UM OU 
# OUTRO PONTO FICA MUDANDO DE LUGAR. ALGO QUE NÃO DEVERIA.


linhas_sem_zero_all <- as.numeric(row.names(ilhas_community))

med_ilhas <- med_ilhas[linhas_sem_zero_all,]


ilhas_community_NMDS <- metaMDS(comm = ilhas_community,
                                k = 2,
                                distance = "bray",
                                trace = F,
                                autotransform = F)

MDS_ilhas_xy <- data.frame(ilhas_community_NMDS$points)
MDS_ilhas_xy$ano <- med_ilhas$ano
MDS_ilhas_xy$ilha <- med_ilhas$ilha

# ggplot(data = MDS_ilhas_xy,
#       aes(x = MDS1, y = MDS2, color = ilha)) + 
#    geom_point(size = 3) +
#    theme_bw()

# Salvando a imagem com resolução:
png(filename = "C:\\Users/manoe/Desktop/plots/Finais/NMDS/transformado_transecto e profundidade.tipo de piscina.png",
    width = 6*300,
    height = 5*300,
    res = 300,
    pointsize = 8)

# Gráfico propriamente dito:
ggplot(data = MDS_ilhas_xy,
       aes(x = MDS1, 
           y = MDS2, 
           color = as.character(ano))) + 
  geom_point(aes(shape = ilha), 
             size = 3) +
  scale_color_brewer(palette = "Set1", 
                     name = "Ano") +
  labs(title = 'Rocas e Noronha por transecto com profundidade/tipo de piscina') +
  annotate(geom = "text",
           x = - 0.5,
           y = 1,
           label = paste0("Stress = ",
                          round(ilhas_community_NMDS$stress,
                                digits = 3)),
           color = "black") +
  theme_bw()

# Finalizando a exportação da imagem:
dev.off()


## Mediana por transecto (sem profundidade.piscina):
med_ilhas_transect <- ilhas %>%
  group_by(ilha, ano, sitio, transecto) %>%
  summarise(med_calcificadores = median(calcificadores),
            med_macroalgas = median(macroalgas),
            med_MAE = median(MAE),
            med_ciano = median(cianobacterias),
            med_susp = median(suspensivoros.filtradores),
            med_zoantideos = median(zoantideo)) %>%
  data.frame()

ilhas_community_transect <- med_ilhas_transect %>%
  select(med_calcificadores, med_macroalgas, med_MAE, med_ciano, med_susp,
         med_zoantideos)%>%
  filter_all(any_vars(. != 0)) %>%
  data.frame() # AQUI ESTA DANDO ERRADO. SE EU TIRO ESSE 'DATA.FRAME' UM OU 
# OUTRO PONTO FICA MUDANDO DE LUGAR. ALGO QUE NÃO DEVERIA.


linhas_sem_zero_all <- as.numeric(row.names(ilhas_community_transect))

med_ilhas_transect <- med_ilhas_transect[linhas_sem_zero_all,]



ilhas_community_NMDS_transect <- metaMDS(comm = ilhas_community_transect,
                                         k = 2,
                                         distance = "bray",
                                         trace = F,
                                         autotransform = F)

MDS_ilhas_xy_transect <- data.frame(ilhas_community_NMDS_transect$points)
MDS_ilhas_xy_transect$ano <- med_ilhas_transect$ano
MDS_ilhas_xy_transect$ilha <- med_ilhas_transect$ilha

# ggplot(data = MDS_ilhas_xy_transect,
#        aes(x = MDS1, y = MDS2, color = ilha)) + 
#     geom_point(size = 3) +
#     theme_bw()

# Salvando a imagem com resolução:
png(filename = "C:\\Users/manoe/Desktop/plots/Finais/NMDS/transformado_só transecto.png",
    width = 6*300,
    height = 5*300,
    res = 300,
    pointsize = 8)

# Gráfico propriamente dito:
ggplot(data = MDS_ilhas_xy_transect,
       aes(x = MDS1, 
           y = MDS2, 
           color = as.character(ano))) + 
  geom_point(aes(shape = ilha), 
             size = 3) +
  scale_color_brewer(palette = "Set1", 
                     name = "Ano") +
  labs(title = 'Rocas e Noronha por transecto (sem profundidade/tipo de piscina)') +
  annotate(geom = "text",
           x = -0.5,
           y = 1,
           label = paste0("Stress = ",
                          round(ilhas_community_NMDS_transect$stress,
                                digits = 3)),
           color = "black") +
  theme_bw()

# Finalizando a exportação da imagem:
dev.off()


## Médianas por sítios:

# Tive que remover transecto do argumento 'group_by' para fazer.

med_ilhas_sitio <- ilhas %>%
  group_by(ilha, ano, sitio) %>%
  summarise(med_calcificadores = median(calcificadores),
            med_macroalgas = median(macroalgas),
            med_MAE = median(MAE),
            med_ciano = median(cianobacterias),
            med_susp = median(suspensivoros.filtradores),
            med_zoantideos = median(zoantideo)) %>%
  data.frame()

ilhas_community_sitio <- med_ilhas_sitio %>%
  select(med_calcificadores, med_macroalgas, med_MAE, med_ciano, med_susp,
         med_zoantideos) %>%
  filter_all(any_vars(. != 0)) %>%
  data.frame() # AQUI ESTA DANDO ERRADO. SE EU TIRO ESSE 'DATA.FRAME' UM OU 
# OUTRO PONTO FICA MUDANDO DE LUGAR. ALGO QUE NÃO DEVERIA.


linhas_sem_zero_all <- as.numeric(row.names(ilhas_community_sitio))

med_ilhas_sitio <- med_ilhas_sitio[linhas_sem_zero_all,]


ilhas_community_NMDS_sitio <- metaMDS(comm = ilhas_community_sitio,
                                      k = 2,
                                      distance = "bray",
                                      trace = F,
                                      autotransform = F)

MDS_ilhas_xy_sitio <- data.frame(ilhas_community_NMDS_sitio$points)
MDS_ilhas_xy_sitio$ano <- med_ilhas_sitio$ano
MDS_ilhas_xy_sitio$ilha <- med_ilhas_sitio$ilha

# ggplot(data = MDS_ilhas_xy_sitio,
#        aes(x = MDS1, y = MDS2, color = ilha)) + 
#     geom_point(size = 3) +
#     theme_bw()

# Salvando a imagem com resolução:
png(filename = "C:\\Users/manoe/Desktop/plots/Finais/NMDS/transformado_sítio.png",
    width = 6*300,
    height = 5*300,
    res = 300,
    pointsize = 8)

ggplot(data = MDS_ilhas_xy_sitio,
       aes(x = MDS1, 
           y = MDS2, 
           color = as.character(ano))) + 
  geom_point(aes(shape = ilha), 
             size = 3) +
  scale_color_brewer(palette = "Set1", 
                     name = "Ano") +
  labs(title = 'Rocas e Noronha por sítio') +
  annotate(geom = "text",
           x = -0.3,
           y = 1,
           label = paste0("Stress = ",
                          round(ilhas_community_NMDS_sitio$stress,
                                digits = 3)),
           color = "black") +
  theme_bw()

# Finalizando a exportação da imagem:
dev.off()


